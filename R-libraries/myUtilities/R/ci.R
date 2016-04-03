ci <-
function(data, z.value = 1.96) {
  data.mean <- mean(data, na.rm = T)
  data.se <- sqrt(Var(data)/length(data[!is.na(data)]))
  upper.ci <- data.mean + z.value * data.se
  lower.ci <- data.mean - z.value * data.se
  return(c(mean = data.mean, std.error = data.se, CV = round(sqrt(Var(data))/data.mean * 100, 2), lower.ci = lower.ci, upper.ci = upper.ci))
}
