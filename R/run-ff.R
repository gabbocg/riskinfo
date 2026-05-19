# Fama-French 4-factor OLS for each (permno, actdate) group
# Returns betas and idiosyncratic volatility with suffixes b_sfx / v_sfx.
run_ff <- function(df, b_sfx, v_sfx) {
  
  df |>
    group_by(permno, actdate) |>
    group_modify(~ {
      if (nrow(.x) < 5L) return(tibble())
      m       <- lm(
        rrf ~ mktrf + smb + hml + umd, data = .x, na.action = na.omit
      )
      cf      <- coef(m)
      res_var <- mean(resid(m) ^ 2, na.rm = TRUE)
      tot_var <- mean(.x$rrf ^ 2,   na.rm = TRUE)
      tibble(
        !!paste0("mktbeta_", b_sfx) := cf["mktrf"],
        !!paste0("smbbeta_", b_sfx) := cf["smb"],
        !!paste0("hmlbeta_", b_sfx) := cf["hml"],
        !!paste0("umdbeta_", b_sfx) := cf["umd"],
        !!paste0("idiovol_", v_sfx) := res_var,
        !!paste0("siv_",     v_sfx) := res_var / tot_var
      )
    }) |>
    ungroup()
  
}
