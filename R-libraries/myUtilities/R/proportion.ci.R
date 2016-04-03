proportion.ci <-
function(r, n, ci = 0.95)
{
# uses exact F distribution to determine the exact confidence intervals
# r can be a proportion or a number
  r <- ifelse(r < 1, round(r * n), r)
  t1 <- 1 - (1 - ci)/2
  old.warn <- options()$warn
  options(warn = -1)
  F1 <- qf(t1, 2 * n - 2 * r + 2, 2 * r)
  F2 <- qf(t1, 2 * r + 2, 2 * n - 2 * r)
  options(warn = old.warn)
  lower.ci <- r/(r + (n - r + 1) * F1)
  upper.ci <- (r + 1)/(r + 1 + (n - r)/F2)
  lower.ci <- ifelse(is.na(lower.ci) & !is.na(n) & !is.na(r), 0, lower.ci)
  upper.ci <- ifelse(is.na(upper.ci) & !is.na(n) & !is.na(r), 1,upper.ci)
  RES <- data.frame(r, n, p = r/n, lower.ci, upper.ci)
  return(RES)
}
