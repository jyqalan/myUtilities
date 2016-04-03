predictCPUE <-
function(variable, data, cpue.fit = NULL, nonzero.fit = NULL, xcts = F,
             fixed = list(), nmin = 0, plotit = T, se.fit = F, nobs = (nmin > 0),
             incl.fixed = F, log.scale = F, type = "l", ylab = NULL, xlab = NULL,
             plotvalues=T,xlim,...)
{
# Written by RICC Francis, and fiddled around with by Alistair Dunn.
# Calculates (and plots, if requested) the predicted value of the predictand
# for: a range of values of the predictors named in variable, and fixed values
# for all other predictors.
#
# The predictand will be either:
#    expected non-zero catch rate (if is.null(nonzero.fit) & log.scale=F),
#    expected log(non-zero catch rte) (if is.null(nonzero.fit) & log.scale=T),
#    expected probability of non-zero catch (if is.null(cpue.fit)), or
#    expected catch rate (otherwise)
#
# WARNING: This does not work properly for nested factors
#
# Default output is a vector of predicted values
# (or a matrix, if len(variable) = 2).
# If either se.fit or nobs is true then the predicted
# values are out$fit, their s.e.s are out$se.fit [omitted if(se.fit==F)],
# and the number of observations for each predicted value is in out$nobs
# [omitted if(nobs==F)]
#
# If predictions are plotted the x-axis will contain all levels of variable[1]
# (if it is a factor) or the range of its values of variable (if not). The
# fixed values of all other predictors are either as given in fixed or are
# "median" values.  For predictors that are factors the median level is
# that associated with the median regression coefficient (ignoring
# interactions); for those that are not it is the median value of the
# predictor in the associated data frame.
#
# variable - a character vector of length 1 or 2 naming the predictors
#        that should be varied.  If it is of length 2 then the first
#        predictand will occur on the x-axis, and a curve will be plotted
#        for each level of the second predictand (which must be a factor)
#
# data - data frame for variables in cpue.fit &/or nonzero.fit - normally
#        output from function Mkcpue.dat
#        (should be the same as was used in constructing glm.fit - I should be
#         able to get this info from glm.fit but I don't know how!!)
#
# cpue.fit - fit (output from glm or stepCPUE) of model whose dependent
#        variable is either data$cpue or data$lcpue (for non-zero catches)
#
# nonzero.fit - fit (output from glm or stepCPUE) of model whose dependent
#        variable is data$nonzero
#
# fixed - a list (e.g., list(yr=1986,area='h2')) specifying the fixed
#         values of other predictors.
#
# xcts - if T then the levels of variable[1] (which form the x-axis) will
#        be treated as a continuous variable
#        If variable[1] is not a factor then xcts is ignored
#
# nmin - if nmin>0 then predicted values will be plotted
#        only for those combinations of factor levels
#        from variable in which the number of observations >= nmin
#        (ignored if variable[1] is not a factor)
#
# plotit - if T plot predicted values
#
# se.fit - if T include component se.fit in output
#
# nobs - if T include component nobs in outpt (ignored if variable[1] is not
#        a factor)
#
# incl.fixed - if T include component fixed in output (detailing the levels
#             of predictors that were held fixed)
#
# log.scale - return log(non-zero catch rate) if T, non-zero catch rate if F
#             (log.scale is ignored if !is.null(nonzero.fit))
#
# type,ylab, ... -  plot arguments
#

is.in<-function(x,y)!is.na(match(x,y))
 if(!is.in("catch",names(data))) stop("The data set must contain a catch variable")
 if(!is.in("cpue",names(data)) & !is.in("lcpue",names(data))) stop("The data set must contain either a cpue or lcpue variable")
 nonz <- data$catch != 0
 cpue.coefnam <- if(!is.null(cpue.fit)) names(coef(cpue.fit)) else ""
 nonzero.coefnam <-if(!is.null(nonzero.fit)) names(coef(nonzero.fit)) else ""
 varnam <- names(data)
 fixnam <- names(fixed)
 multiple <- length(variable) > 1
 if(multiple) {
  if(!is.factor(data[[variable[2]]]))
   stop(paste(variable[2], "is not a factor"))
  variable <- variable[1:2]
 # ignore additional members of variable
 }
 if(length(variable) == 2 & !is.factor(data[[variable[2]]])) {
  nmin <- 0
  nobs <- F
 }
#
# Check for errors in variable and names(fixed)
#
 for(vr in c(variable, fixnam)) {
  tmp <- match(vr, varnam)
  if(is.na(tmp)) stop(paste(vr, "not in data frame"))
 }
#
# Find median values for unnamed fixed predictors
#
 for(vr in varnam[!is.in(varnam, c(variable, fixnam))]) {
  in.cpue <- length(string.match(vr, cpue.coefnam)) > 0
  in.nonzero <- length(string.match(vr, nonzero.coefnam)) > 0
  if(in.cpue) {
   if(is.factor(data[[vr]])) {
    levs <- levels(data[[vr]])
    cf <- coef(cpue.fit)[paste(vr, levs, sep ="")]
    cf[1] <- 0
    fixed[[vr]] <-levs[order(cf)][floor(length(cf)/2)]
   }
   else {
    fixed[[vr]] <- median(data[[vr]][nonz])
   }
  }
  else if(in.nonzero) {
   if(is.factor(data[[vr]])) {
    levs <- levels(data[[vr]])
    cf <- coef(nonzero.fit)[paste(vr, levs,sep = "")]
    cf[1] <- 0
    fixed[[vr]] <-levs[order(cf)][floor(length(cf)/2)]
   }
   else {
    fixed[[vr]] <- median(data[[vr]])
   }
  }
 }
#
# Find values for first variable
#
 xaxt <- "s"
 if(is.factor(data[[variable[1]]])) {
  xlev <- levels(data[[variable[1]]])
  xf <- as.factor(xlev)
  if(xcts)
   xvar <- as.numeric(xlev)
  else {
   xvar <- 1:length(xlev)
   xaxt <- "n"
  }
 }
 else {
  xrng <-if(!is.null(cpue.fit)) range(data[[variable[1]]][nonz])
    else range(data[[variable[1]]])
  xf <- xvar <- seq(xrng[1], xrng[2], length = 20)
  xlev <- paste(xf)
 }
 if(multiple) {
  zlev <- levels(data[[variable[2]]])
  newfram <- data.frame(rep(xf, length(zlev)),rep(as.factor(zlev),rep(length(xf), length(zlev))))
 }
 else newfram <- data.frame(xf, row.names = xf)
 names(newfram) <- variable
 nrw <- nrow(newfram)
 for(vr in names(fixed)) {
  if(is.factor(data[[vr]]))
   newfram[[vr]] <- factor(rep(fixed[[vr]], nrw),levels(data[[vr]]))
  else newfram[[vr]] <- rep(fixed[[vr]], nrw)
 }
#
# Calculate predicted values (and s.e.s, if requested)
#
print(newfram)

if(!is.null(cpue.fit)) {
  predval <- predict.glm(cpue.fit, newfram, type ="response", se.fit = se.fit)
  if(!se.fit)
   predval <- list(fit = predval)
  cpue.is.log <- as.character(formula(cpue.fit))[2] =="lcpue"
  if(!log.scale & cpue.is.log) {
   predval$fit <- exp(predval$fit)
   if(se.fit) predval$se.fit <- predval$se.fit *predval$fit
  }
  else if(log.scale & is.null(nonzero.fit) & !cpue.is.log)
   stop("No code written for this possibility")
  if(!is.null(nonzero.fit)) {
   tmp <- predict.glm(nonzero.fit, newfram, type = "response", se.fit = se.fit)
   if(!se.fit)
    tmp <- list(fit = tmp)
   if(se.fit)
    predval$se.fit <- sqrt((predval$fit * tmp$se.fit)^2 + (tmp$fit * predval$se.fit)^2)
   predval$fit <- predval$fit * tmp$fit
  }
 }
 else {
  predval <- predict.glm(nonzero.fit, newfram, type ="response", se.fit = se.fit)
  if(!se.fit)
   predval <- list(fit = predval)
 }
 if(multiple) {
  predval$fit <- matrix(predval$fit, length(xvar), dimnames =list( xlev, zlev))
  if(se.fit) predval$se.fit <- matrix(predval$se.fit,length(xvar),dimnames = list(xlev, zlev))
 }
#
# Calculate predval$nobs - number of observations per level of variables -
# if variable[1] is a factor, and set predicted values to NA when
# predval$nobs==0
#
 if(is.factor(data[[variable[1]]])) {
  v1 <- if(is.null(nonzero.fit)) data[[variable[1]]][nonz] else data[[variable[1]]]
  if(multiple) {
   v2 <- if(is.null(nonzero.fit)) data[[variable[2]]][nonz]  else data[[variable[2]]]
   predval$nobs <- if(is.factor(v1)) table(v1,v2)
   else matrix(rep(table(v2), length(xvar)),length(xvar),byrow = T)
  }
  else {
   predval$nobs <- if(is.factor(v1)) table(v1) else
      predval$nobs <- rep(nmin, length(xvar))
  }
  sel <- predval$nobs == 0
  if(any(sel)) {
   predval$fit[sel] <- NA
   if(se.fit) predval$se.fit[sel] <- NA
  }
 }
#
# Plot predicted values (if required)
#
 if(plotit) {
  y <- predval$fit
  if(nmin > 0)
   y[predval$nobs < nmin] <- NA
  if(is.null(ylab)) {
   ylab <- if(is.null(cpue.fit)) "Expected probability of non-zero catch"
     else if(is.null(nonzero.fit)) "Expected non-zero catch rate"
   else "Expected catch rate"
   if(multiple) ylab <- paste(ylab, "by", variable[2])
  }
  if(is.null(xlab)) xlab <- variable[1]
  if(multiple) {
    plot(xvar, y[, 1], type = type, ylim = c(0, max(y,na.rm = T)), yaxs = "r", xlab = xlab, ylab= ylab, xaxt = xaxt, xlim = range(xvar), pch= "1", ...)
    for(i in 2:ncol(y))
      lines(xvar, y[, i], type = type, pch =paste(i), col = i)
  } else {
    plot(xvar, y, type = type, ylim = c(0, max(y, na.rm =T)), yaxs = "r", xlab = xlab, ylab = ylab, xlim = range(xvar), xaxt = xaxt, ...)
  }
  if(xaxt == "n")
   axis(1, xvar, xlev)
  if(plotvalues) {
    vals<-lapply(fixed,function(x) if(is.numeric(x)) round(x,1) else x)
    text(min(xvar)+0.02*diff(range(xvar)),max(y,na.rm=T)*0.98,paste(paste(names(fixed), "=",as.vector(unlist(vals))), collapse = "\n"),adj=0)
  }
  cat("\n\n")
  cat(paste(paste(names(fixed), "=",as.vector(unlist(fixed))), collapse = "\n"))
  cat("\n\n")
 }
#
# Return predicted values (with s.e.s and/or numbers of observations
# and/or fixed, if required)
#
 if(incl.fixed)
  predval$fixed <- unlist(fixed)
 predval <- predval[c("fit", "se.fit", "nobs", "fixed")[c(T,se.fit,nobs, incl.fixed)]]
 if(length(predval) == 1)
  predval[[1]]
 else predval
}
