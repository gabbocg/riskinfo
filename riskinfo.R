# ======================================================== #
#
#                Replicates Smith (2021, JAR)
#                "Measuring Risk Information"
#
#                 Gabriel E. Cabrera Guzmán
#                The University of Manchester
#
#                       Spring, 2026
#
#                https://gcabrerag.rbind.io
#
# ------------------------------ #
# email: gabriel.cabreraguzman@postgrad.manchester.ac.uk
# ======================================================== #
#
# Expects in the calling environment (set by load.R):
#   wrds      — active DBI connection to WRDS PostgreSQL
#   beg_date  — sample start (Date)
#   end_date  — sample end   (Date)
#
# Produces: 05_smithsoJAR.csv

# Read auxiliary functions 
source(file.path("R", "wd-diff.R"))
source(file.path("R", "vol-to.R"))
source(file.path("R", "run-ff.R"))

# ==========================================
# STEPS 1–3: Build SSBase (event–firm panel)
# ------------------------------------------

# Compustat quarterly identifiers
isdcon <- dbGetQuery(wrds, "
    SELECT DISTINCT
      substr(cusip, 1, 8)  AS cusip,
      gvkey,
      cik,
      rdq                  AS compdate,
      datadate             AS fqe,
      fyearq               AS fiscaly,
      fqtr                 AS fiscalq
    FROM comp.fundq
    WHERE rdq  > '2010-01-01'
      AND rdq  IS NOT NULL
      AND cusip <> ''
      AND consol  = 'C'
      AND indfmt  = 'INDL'
      AND datafmt = 'STD'
      AND popsrc  = 'D'
    ORDER BY gvkey
  ") |>
  mutate(across(c(compdate, fqe), as.Date))

# Capital IQ Analyst/Investor Day events
ciq_keydev <- dbGetQuery(wrds, "
    SELECT
      keydevid,
      companyid,
      announcedate,
      announcetime,
      mostimportantdateutc::date AS mostimportantdateutc,
      gvkey
    FROM ciq.wrds_keydev
    WHERE eventtype = 'Analyst/Investor Day'
  ") |>
  mutate(
    mostimportantdateutc = as.Date(mostimportantdateutc),
    announcedate         = as.Date(announcedate)
  )

ciq_company_meta <- dbGetQuery(wrds, "
    SELECT a.companyid, b.companytypename, c.isocountry3
    FROM ciq.ciqcompany    a
    LEFT JOIN ciq.ciqcompanytype b ON a.companytypeid = b.companytypeid
    LEFT JOIN ciq.ciqcountrygeo  c ON a.countryid     = c.countryid
    WHERE a.companyid IN (
      SELECT DISTINCT companyid FROM ciq.wrds_keydev
      WHERE eventtype = 'Analyst/Investor Day'
    )
  ")

# Filter to US public companies; create datafqtr key
ciq_aiday <- ciq_keydev |>
  left_join(ciq_company_meta, by = "companyid") |>
  filter(companytypename == "Public Company", isocountry3 == "USA")

# Match events to most recently completed fiscal quarter before the event
ssbase <- ciq_aiday |>
  inner_join(
    isdcon |> select(gvkey, cusip, cik, compdate, fqe, fiscaly, fiscalq),
    by = "gvkey",
    relationship = "many-to-many"
  ) |>
  filter(
    !is.na(cusip),
    fqe <= mostimportantdateutc,
    mostimportantdateutc >= beg_date,
    mostimportantdateutc <= end_date
  ) |>
  rename(actdate = mostimportantdateutc) |>
  arrange(gvkey, actdate, desc(fqe)) |>
  distinct(gvkey, actdate, .keep_all = TRUE)

# ==========================================
# STEP 4: Link Compustat → CRSP; S&P 500 flag; NYSE size decile
# ------------------------------------------

lnk_raw <- dbGetQuery(wrds, sprintf("
    SELECT gvkey, lpermno AS permno, linktype, linkdt, linkenddt
    FROM crsp.ccmxpf_lnkhist
    WHERE linkprim IN ('P','C')
      AND linktype IN ('LC','LU','LX','LD','LS','LN')
      AND (extract(year FROM linkdt) <= %d OR linkdt IS NULL)
  ", year(end_date) + 1L)) |>
  mutate(across(c(linkdt, linkenddt), as.Date))

ssbase <- ssbase |>
  inner_join(lnk_raw, by = "gvkey", relationship = "many-to-many") |>
  filter(
    is.na(linkdt)    | linkdt    <= actdate,
    is.na(linkenddt) | actdate   <= linkenddt
  )

# S&P 500 membership flag
snp500 <- dbGetQuery(
  wrds, "SELECT permno, start, ending FROM crsp.msp500list"
) |>
  mutate(across(c(start, ending), as.Date))

ssbase <- ssbase |>
  left_join(
    snp500 |> mutate(snp = 1L),
    by = join_by(permno, actdate >= start, actdate <= ending)
  ) |>
  mutate(snp = replace_na(snp, 0L))

# NYSE size decile (prior year)
mport1 <- dbGetQuery(wrds, "SELECT permno, year, capn AS nysd FROM crsp.mport1")

ssbase <- ssbase |>
  mutate(join_year = year(actdate) - 1L) |>
  left_join(mport1, by = c("permno", "join_year" = "year")) |>
  mutate(nysq = floor(nysd / 2L), .keep = "unused")

# ==========================================
# STEP 5: Option implied volatility (OptionMetrics)
# ------------------------------------------

# Call options at maturities 30/60/122/182/365 days
# Event window: td in [-10, 10] and td == -32

cusips_ev <- unique(ssbase$cusip)
opt_years <- as.integer(format(beg_date, "%Y")):
  as.integer(format(end_date, "%Y"))
opt_years <- unique(c(min(opt_years) - 1L, opt_years))   # include prior year

opt_raw <- lapply(opt_years, function(yr) {
  
  dbGetQuery(wrds, sprintf("
        SELECT d.cusip, o.date, o.days, o.impl_volatility AS iv
        FROM optionm.stdopd%d o
        JOIN optionm.securd   d ON o.secid = d.secid
        WHERE d.cusip = ANY(ARRAY[%s])
          AND o.cp_flag = 'C'
          AND ABS(o.impl_volatility) < 10
          AND o.days IN (30, 60, 122, 182, 365)
          AND o.date BETWEEN '%s' AND '%s'
      ",
                           yr,
                           paste0("'", cusips_ev, "'", collapse = ","),
                           format(beg_date - 60, "%Y-%m-%d"),
                           format(end_date + 60, "%Y-%m-%d")
  ))
  
}) |>
  bind_rows() |>
  mutate(date = as.Date(date))

# Join with events; compute weekday offset; keep within window
opt_joined <- ssbase |>
  select(cusip, actdate) |>
  inner_join(opt_raw, by = "cusip", relationship = "many-to-many") |>
  mutate(td = wd_diff(actdate, date)) |>
  filter((td >= -10L & td <= 10L) | td == -32L) |>
  arrange(cusip, actdate, td, days, desc(date)) |>
  distinct(cusip, actdate, td, days, .keep_all = TRUE)

# Pivot wide on maturity for each (cusip, actdate, td)
iv_wide <- opt_joined |>
  pivot_wider(
    id_cols      = c(cusip, actdate, td),
    names_from   = days,
    names_prefix = "iv",
    values_from  = iv
  )

# Pre-event (td = -2): short-horizon variance decomposition
# SIGQ = slope of IV^2/252 w.r.t. 1/maturity at 30 and 60 days
# SIGB = IV30 - SIGQ / 30  (baseline diffusion vol)
ssiv_pre <- iv_wide |>
  filter(td == td_pre) |>
  mutate(
    sigq_pre1 = (iv30 - iv60) / (252 / 30 - 252 / 60),
    sigb_pre1 = iv30 - sigq_pre1 / 30
  ) |>
  filter(!is.na(sigq_pre1), !is.na(sigb_pre1)) |>
  select(cusip, actdate, sigq_pre1, sigb_pre1)

# Post-event (td = 1)
ssiv_post <- iv_wide |>
  filter(td == td_post) |>
  mutate(
    sigq_post1 = (iv30 - iv60) / (252 / 30 - 252 / 60),
    sigb_post1 = iv30 - sigq_post1 / 30
  ) |>
  filter(!is.na(sigq_post1), !is.na(sigb_post1)) |>
  select(cusip, actdate, sigq_post1, sigb_post1)

# IV levels at td_pre and td_post for DeltaIV* in Step 9
iv_at_tdm2 <- iv_wide |>
  filter(td == td_pre) |>
  select(
    cusip, actdate,
    iv30_m2  = iv30,  iv60_m2  = iv60,
    iv122_m2 = iv122, iv182_m2 = iv182, iv365_m2 = iv365
  )

iv_at_tdp2 <- iv_wide |>
  filter(td == td_post) |>
  select(
    cusip, actdate,
    iv30_p2  = iv30,  iv60_p2  = iv60,
    iv122_p2 = iv122, iv182_p2 = iv182, iv365_p2 = iv365
  )

ssiv1 <- ssiv_pre |>
  left_join(ssiv_post,  by = c("cusip", "actdate")) |>
  left_join(iv_at_tdm2, by = c("cusip", "actdate")) |>
  left_join(iv_at_tdp2, by = c("cusip", "actdate"))

# VIX 2 weekdays before/after each event
vix_raw <- dbGetQuery(wrds, "SELECT date, vix FROM cboe.cboe") |>
  mutate(date = as.Date(date))

vix_pre <- ssbase |>
  select(permno, actdate) |>
  left_join(
    vix_raw |> rename(vix_date = date, vixpre = vix), by = character()
  ) |>
  mutate(td = wd_diff(vix_date, actdate)) |>
  filter(td == 2L) |>
  select(permno, actdate, vixpre)

vix_post <- ssbase |>
  select(permno, actdate) |>
  left_join(
    vix_raw |> rename(vix_date = date, vixpost = vix), by = character()
  ) |>
  mutate(td = wd_diff(actdate, vix_date)) |>
  filter(td == -2L) |>
  select(permno, actdate, vixpost)

# ==========================================
# STEP 6: Returns, turnover, and liquidity (CRSP daily)
# ------------------------------------------

permnos_ev <- unique(ssbase$permno)

crsp_daily <- dbGetQuery(wrds, sprintf("
    SELECT s.permno,
           s.dlycaldt   AS date,
           s.dlyret     AS ret,
           s.dlyvol     AS vol,
           s.shrout,
           s.dlyprc     AS prc,
           s.dlyask     AS ask,
           s.dlybid     AS bid,
           i.ewretd
    FROM crsp.dsf_v2 s
    JOIN crsp.dsi i ON s.dlycaldt = i.date
    WHERE s.permno = ANY(ARRAY[%s])
      AND s.dlycaldt BETWEEN '%s' AND '%s'",
                                       paste(permnos_ev, collapse = ","),
                                       format(beg_date - 400L, "%Y-%m-%d"),
                                       format(end_date + 400L, "%Y-%m-%d")
)) |>
  mutate(date = as.Date(date))

crsp_ev <- ssbase |>
  select(permno, actdate) |>
  inner_join(crsp_daily, by = "permno", relationship = "many-to-many") |>
  mutate(td = wd_diff(actdate, date))

ssv1 <- vol_to(crsp_ev, -257L, -5L,   "257_5")
ssv2 <- vol_to(crsp_ev,    5L, 257L,  "5_257")
ssv3 <- vol_to_liq(crsp_ev, 5L, 26L,  "5_26")
ssv4 <- vol_to(crsp_ev,   27L, 257L,  "27_257")
ssv6 <- vol_to(crsp_ev,   27L, 127L,  "27_127")
ssv7 <- vol_to_liq(crsp_ev, -26L, -5L, "26_5")
ssv8 <- vol_to(crsp_ev, -127L, -5L,   "127_5")
ssv9 <- vol_to(crsp_ev,    5L, 127L,  "5_127")

# Event window [-1, 1]: CAR, turnover, VIX
ssv5 <- crsp_ev |>
  filter(td >= -1L, td <= 1L) |>
  left_join(vix_raw, by = "date") |>
  group_by(permno, actdate) |>
  summarise(
    eanret = 100 * (prod(1 + ret, na.rm = TRUE) - prod(1 + ewretd, na.rm = TRUE)),
    eanrr  = 100 * (prod(1 + ret, na.rm = TRUE) - 1),
    eanto  = mean(vol / shrout, na.rm = TRUE),
    eavix  = mean(vix, na.rm = TRUE),
    .groups = "drop"
  )

# ==========================================
# STEP 7: Fama-French 4-factor betas and idiosyncratic volatility
# ------------------------------------------

ff_daily <- dbGetQuery(wrds, "
    SELECT date, rf, mktrf, smb, hml, umd
    FROM ff.factors_daily
    ORDER BY date
  ") |>
  mutate(date = as.Date(date))

ff_ret <- crsp_ev |>
  inner_join(ff_daily, by = "date") |>
  mutate(rrf = ret - rf)

# Window pair 1: pre = [-26, -5), post = [5, 26)
biv_pre1  <- ff_ret |> filter(td >= -26L,  td < -5L)  |> run_ff("pre",  "pre")
biv_post1 <- ff_ret |> filter(td >=   5L,  td < 26L)  |> run_ff("post", "post")
betaivol  <- full_join(biv_pre1, biv_post1, by = c("permno", "actdate"))

# Window pair 2: pre = [-127, -5), post = [5, 127)
biv_pre2  <- ff_ret |> filter(td >= -127L, td < -5L)  |> run_ff("pre2",  "pre2")
biv_post2 <- ff_ret |> filter(td >=    5L, td < 127L)  |> run_ff("post2", "post2")
betaivol2 <- full_join(biv_pre2, biv_post2, by = c("permno", "actdate"))

# ==========================================
# STEP 9: Final merge and variable construction
# ------------------------------------------

smithso_jar <- ssbase |>
  inner_join(ssiv1,    by = c("cusip",  "actdate")) |>
  inner_join(ssv1,     by = c("permno", "actdate")) |>
  left_join(ssv2,      by = c("permno", "actdate")) |>
  left_join(ssv3,      by = c("permno", "actdate")) |>
  left_join(ssv4,      by = c("permno", "actdate")) |>
  left_join(ssv5,      by = c("permno", "actdate")) |>
  left_join(ssv6,      by = c("permno", "actdate")) |>
  left_join(ssv7,      by = c("permno", "actdate")) |>
  left_join(ssv8,      by = c("permno", "actdate")) |>
  left_join(ssv9,      by = c("permno", "actdate")) |>
  left_join(betaivol,  by = c("permno", "actdate")) |>
  left_join(betaivol2, by = c("permno", "actdate")) |>
  left_join(
    vix_pre,  by = c("permno", "actdate"), relationship = "many-to-many"
  ) |>
  left_join(
    vix_post, by = c("permno", "actdate"), relationship = "many-to-many"
  ) |>
  mutate(
    # ── Calendar variables ────────────────────────────────────────────────
    period     = year(actdate) * 10L + quarter(actdate),
    ayear      = year(actdate),
    amonth     = month(actdate),
    ayearmonth = ayear * 100L + amonth,
    
    # ── Short-horizon risk slope (SIGQP) ──────────────────────────────────
    # d(IV^2/252) / d(1/maturity) evaluated at 30 and 60 days, td = -2
    sigqp = ((iv30_m2 / sqrt(252)) ^ 2 - (iv60_m2 / sqrt(252)) ^ 2) / (1 / 30 - 1 / 60),
    
    # ── Changes in option-implied variance: maturity * d(IV^2/252) ────────
    # Compares td = 2 (post) vs td = -2 (pre)
    delta_iv30  =  30 * ((iv30_p2  / sqrt(252)) ^ 2 - (iv30_m2  / sqrt(252)) ^ 2),
    delta_iv60  =  60 * ((iv60_p2  / sqrt(252)) ^ 2 - (iv60_m2  / sqrt(252)) ^ 2),
    delta_iv122 = 122 * ((iv122_p2 / sqrt(252)) ^ 2 - (iv122_m2 / sqrt(252)) ^ 2),
    delta_iv182 = 182 * ((iv182_p2 / sqrt(252)) ^ 2 - (iv182_m2 / sqrt(252)) ^ 2),
    delta_iv365 = 365 * ((iv365_p2 / sqrt(252)) ^ 2 - (iv365_m2 / sqrt(252)) ^ 2),
    
    # ── Risk information measures: RI = DeltaIV + SIGQP ──────────────────
    ri30  = delta_iv30  + sigqp,
    ri60  = delta_iv60  + sigqp,
    ri122 = delta_iv122 + sigqp,
    ri182 = delta_iv182 + sigqp,
    ri365 = delta_iv365 + sigqp,
    
    abri30  = abs(ri30),
    abri122 = abs(ri122),
    abri182 = abs(ri182),
    abri365 = abs(ri365),
    
    # ── Scaled RI: divide by baseline diffusion variance ──────────────────
    # DiffVol = maturity * (SIGB_pre1 / sqrt(252))^2
    diffvol30  =  30 * (sigb_pre1 / sqrt(252)) ^ 2,
    diffvol182 = 182 * (sigb_pre1 / sqrt(252)) ^ 2,
    sari30  = if_else(diffvol30  > 0, abri30  / diffvol30,  NA_real_),
    sri30   = if_else(diffvol30  > 0, ri30    / diffvol30,  NA_real_),
    sari182 = if_else(diffvol182 > 0, abri182 / diffvol182, NA_real_),
    sri182  = if_else(diffvol182 > 0, ri182   / diffvol182, NA_real_),
    
    # ── Volatility changes ────────────────────────────────────────────────
    ppvma = if_else(vma_26_5 > 0 & vma_5_26   > 0, 100 * (vma_5_26  - vma_26_5),  NA_real_),
    dvma  = if_else(vma_5_26 > 0 & vma_27_127 > 0, 100 * (vma_5_26 - vma_27_127), NA_real_),
    ppalr = if_else(alr_26_5 > 0 & alr_5_26 > 0, log(alr_5_26 / alr_26_5), NA_real_),
    ppbas = if_else(bas_26_5 > 0 & bas_5_26 > 0, log(bas_5_26 / bas_26_5), NA_real_),
    
    # ── Idiosyncratic volatility changes (window pair 1) ──────────────────
    delta_siv  = 100 * (siv_post - siv_pre),
    dsiv       = if_else(siv_post > 0 & siv_pre > 0, log(siv_post / siv_pre), NA_real_),
    delta_ivol = 100 * (idiovol_post - idiovol_pre),
    divol      = if_else(idiovol_post > 0 & idiovol_pre > 0, log(idiovol_post / idiovol_pre), NA_real_),
    
    # ── Beta changes (window pair 1, levels) ──────────────────────────────
    delta_mktbeta_sq = mktbeta_post ^ 2 - mktbeta_pre ^ 2,
    delta_smbbeta_sq = smbbeta_post ^ 2 - smbbeta_pre ^ 2,
    delta_hmlbeta_sq = hmlbeta_post ^ 2 - hmlbeta_pre ^ 2,
    delta_umdbeta_sq = umdbeta_post ^ 2 - umdbeta_pre ^ 2,
    
    # ── Idiosyncratic volatility changes (window pair 2) ──────────────────
    delta_ivol2 = 100 * (idiovol_post2 - idiovol_pre2),
    divol2      = if_else(idiovol_post2 > 0 & idiovol_pre2 > 0, log(idiovol_post2 / idiovol_pre2), NA_real_),
    
    # ── Beta changes (window pair 2, log of squared betas) ────────────────
    delta_mktbeta_sq2 = if_else(mktbeta_post ^ 2 > 0 & mktbeta_pre ^ 2 > 0, log(mktbeta_post ^ 2 / mktbeta_pre ^ 2), NA_real_),
    delta_smbbeta_sq2 = if_else(smbbeta_post ^ 2 > 0 & smbbeta_pre ^ 2 > 0, log(smbbeta_post ^ 2 / smbbeta_pre ^ 2), NA_real_),
    delta_hmlbeta_sq2 = if_else(hmlbeta_post ^ 2 > 0 & hmlbeta_pre ^ 2 > 0, log(hmlbeta_post ^ 2 / hmlbeta_pre ^ 2), NA_real_),
    delta_umdbeta_sq2 = if_else(umdbeta_post ^ 2 > 0 & umdbeta_pre ^ 2 > 0, log(umdbeta_post ^ 2 / umdbeta_pre ^ 2), NA_real_),
    
    # ── VIX change ────────────────────────────────────────────────────────
    delta_vix = vixpost - vixpre
  ) |>
  distinct_all() |>
  select(
    permno, gvkey, companyid, keydevid, actdate, announcedate,
    ri30, ri60, ri182, ri365, sri30, sri182
  )

# ── Export ────────────────────────────────────────────────────────────────────
write.csv(smithso_jar, file.path("data", "smithsoJAR.csv"), row.names = FALSE)
cat("Exported", nrow(smithso_jar), "observations to data/smithsoJAR.csv\n")
