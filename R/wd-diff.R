# Helper: weekday distance equivalent to SAS intck('WeekDay', d1, d2)
# Counts Mon-Fri days from d1 to d2; negative when d2 < d1.
create.calendar(
  "wkdays",
  weekdays   = c("saturday", "sunday"),
  start.date = as.Date("2005-01-01"),
  end.date   = as.Date("2025-12-31")
)

wd_diff <- function(d1, d2) {
  
  d1  <- as.Date(d1)
  d2  <- as.Date(d2)
  lo  <- pmin(d1, d2)
  hi  <- pmax(d1, d2)
  biz <- as.integer(bizdays::bizdays(lo, hi, "wkdays"))
  
  ifelse(d2 >= d1, biz, -biz)
  
}
