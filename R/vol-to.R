# Volatility, turnover, and liquidity summaries over event windows
vol_to <- function(df, lo, hi, sfx) {
  
  df |>
    filter(td >= lo, td <= hi) |>
    group_by(permno, actdate) |>
    summarise(
      "vma_{sfx}" := sd(ret - ewretd, na.rm = TRUE),
      "vrw_{sfx}" := sd(ret,          na.rm = TRUE),
      "to_{sfx}"  := mean(vol / shrout, na.rm = TRUE),
      .groups = "drop"
    )
  
}

vol_to_liq <- function(df, lo, hi, sfx) {
  
  df |>
    filter(td >= lo, td <= hi) |>
    group_by(permno, actdate) |>
    summarise(
      "vma_{sfx}" := sd(ret - ewretd, na.rm = TRUE),
      "vrw_{sfx}" := sd(ret,          na.rm = TRUE),
      "to_{sfx}"  := mean(vol / shrout, na.rm = TRUE),
      "alr_{sfx}" := mean(
        abs(ret) / (abs(prc) * vol / 1e6), na.rm = TRUE
      ),
      "bas_{sfx}" := sum(
        vol * (ask - bid) / (0.5 * ask + 0.5 * bid), na.rm = TRUE
      ) / sum(vol, na.rm = TRUE),
      .groups = "drop"
    )
  
}
