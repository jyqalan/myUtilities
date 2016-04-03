extractCPUE <-
function(fit, scale = 0, ...)
{
  n <- length(fit$residuals)
  edf <- n - fit$df.residual
  r.squared <- (fit$null.deviance - fit$deviance)/fit$null.deviance
  c(edf, r.squared)
}
