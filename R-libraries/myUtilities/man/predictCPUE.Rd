\name{predictCPUE}
\alias{predictCPUE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predictCPUE(variable, data, cpue.fit = NULL, nonzero.fit = NULL, xcts = F, fixed = list(), nmin = 0, plotit = T, se.fit = F, nobs = (nmin > 0), incl.fixed = F, log.scale = F, type = "l", ylab = NULL, xlab = NULL, plotvalues = T, xlim, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variable}{
%%     ~~Describe \code{variable} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{cpue.fit}{
%%     ~~Describe \code{cpue.fit} here~~
}
  \item{nonzero.fit}{
%%     ~~Describe \code{nonzero.fit} here~~
}
  \item{xcts}{
%%     ~~Describe \code{xcts} here~~
}
  \item{fixed}{
%%     ~~Describe \code{fixed} here~~
}
  \item{nmin}{
%%     ~~Describe \code{nmin} here~~
}
  \item{plotit}{
%%     ~~Describe \code{plotit} here~~
}
  \item{se.fit}{
%%     ~~Describe \code{se.fit} here~~
}
  \item{nobs}{
%%     ~~Describe \code{nobs} here~~
}
  \item{incl.fixed}{
%%     ~~Describe \code{incl.fixed} here~~
}
  \item{log.scale}{
%%     ~~Describe \code{log.scale} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{plotvalues}{
%%     ~~Describe \code{plotvalues} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (variable, data, cpue.fit = NULL, nonzero.fit = NULL, 
    xcts = F, fixed = list(), nmin = 0, plotit = T, se.fit = F, 
    nobs = (nmin > 0), incl.fixed = F, log.scale = F, type = "l", 
    ylab = NULL, xlab = NULL, plotvalues = T, xlim, ...) 
{
    is.in <- function(x, y) !is.na(match(x, y))
    if (!is.in("catch", names(data))) 
        stop("The data set must contain a catch variable")
    if (!is.in("cpue", names(data)) & !is.in("lcpue", names(data))) 
        stop("The data set must contain either a cpue or lcpue variable")
    nonz <- data$catch != 0
    cpue.coefnam <- if (!is.null(cpue.fit)) 
        names(coef(cpue.fit))
    else ""
    nonzero.coefnam <- if (!is.null(nonzero.fit)) 
        names(coef(nonzero.fit))
    else ""
    varnam <- names(data)
    fixnam <- names(fixed)
    multiple <- length(variable) > 1
    if (multiple) {
        if (!is.factor(data[[variable[2]]])) 
            stop(paste(variable[2], "is not a factor"))
        variable <- variable[1:2]
    }
    if (length(variable) == 2 & !is.factor(data[[variable[2]]])) {
        nmin <- 0
        nobs <- F
    }
    for (vr in c(variable, fixnam)) {
        tmp <- match(vr, varnam)
        if (is.na(tmp)) 
            stop(paste(vr, "not in data frame"))
    }
    for (vr in varnam[!is.in(varnam, c(variable, fixnam))]) {
        in.cpue <- length(string.match(vr, cpue.coefnam)) > 0
        in.nonzero <- length(string.match(vr, nonzero.coefnam)) > 
            0
        if (in.cpue) {
            if (is.factor(data[[vr]])) {
                levs <- levels(data[[vr]])
                cf <- coef(cpue.fit)[paste(vr, levs, sep = "")]
                cf[1] <- 0
                fixed[[vr]] <- levs[order(cf)][floor(length(cf)/2)]
            }
            else {
                fixed[[vr]] <- median(data[[vr]][nonz])
            }
        }
        else if (in.nonzero) {
            if (is.factor(data[[vr]])) {
                levs <- levels(data[[vr]])
                cf <- coef(nonzero.fit)[paste(vr, levs, sep = "")]
                cf[1] <- 0
                fixed[[vr]] <- levs[order(cf)][floor(length(cf)/2)]
            }
            else {
                fixed[[vr]] <- median(data[[vr]])
            }
        }
    }
    xaxt <- "s"
    if (is.factor(data[[variable[1]]])) {
        xlev <- levels(data[[variable[1]]])
        xf <- as.factor(xlev)
        if (xcts) 
            xvar <- as.numeric(xlev)
        else {
            xvar <- 1:length(xlev)
            xaxt <- "n"
        }
    }
    else {
        xrng <- if (!is.null(cpue.fit)) 
            range(data[[variable[1]]][nonz])
        else range(data[[variable[1]]])
        xf <- xvar <- seq(xrng[1], xrng[2], length = 20)
        xlev <- paste(xf)
    }
    if (multiple) {
        zlev <- levels(data[[variable[2]]])
        newfram <- data.frame(rep(xf, length(zlev)), rep(as.factor(zlev), 
            rep(length(xf), length(zlev))))
    }
    else newfram <- data.frame(xf, row.names = xf)
    names(newfram) <- variable
    nrw <- nrow(newfram)
    for (vr in names(fixed)) {
        if (is.factor(data[[vr]])) 
            newfram[[vr]] <- factor(rep(fixed[[vr]], nrw), levels(data[[vr]]))
        else newfram[[vr]] <- rep(fixed[[vr]], nrw)
    }
    print(newfram)
    if (!is.null(cpue.fit)) {
        predval <- predict.glm(cpue.fit, newfram, type = "response", 
            se.fit = se.fit)
        if (!se.fit) 
            predval <- list(fit = predval)
        cpue.is.log <- as.character(formula(cpue.fit))[2] == 
            "lcpue"
        if (!log.scale & cpue.is.log) {
            predval$fit <- exp(predval$fit)
            if (se.fit) 
                predval$se.fit <- predval$se.fit * predval$fit
        }
        else if (log.scale & is.null(nonzero.fit) & !cpue.is.log) 
            stop("No code written for this possibility")
        if (!is.null(nonzero.fit)) {
            tmp <- predict.glm(nonzero.fit, newfram, type = "response", 
                se.fit = se.fit)
            if (!se.fit) 
                tmp <- list(fit = tmp)
            if (se.fit) 
                predval$se.fit <- sqrt((predval$fit * tmp$se.fit)^2 + 
                  (tmp$fit * predval$se.fit)^2)
            predval$fit <- predval$fit * tmp$fit
        }
    }
    else {
        predval <- predict.glm(nonzero.fit, newfram, type = "response", 
            se.fit = se.fit)
        if (!se.fit) 
            predval <- list(fit = predval)
    }
    if (multiple) {
        predval$fit <- matrix(predval$fit, length(xvar), dimnames = list(xlev, 
            zlev))
        if (se.fit) 
            predval$se.fit <- matrix(predval$se.fit, length(xvar), 
                dimnames = list(xlev, zlev))
    }
    if (is.factor(data[[variable[1]]])) {
        v1 <- if (is.null(nonzero.fit)) 
            data[[variable[1]]][nonz]
        else data[[variable[1]]]
        if (multiple) {
            v2 <- if (is.null(nonzero.fit)) 
                data[[variable[2]]][nonz]
            else data[[variable[2]]]
            predval$nobs <- if (is.factor(v1)) 
                table(v1, v2)
            else matrix(rep(table(v2), length(xvar)), length(xvar), 
                byrow = T)
        }
        else {
            predval$nobs <- if (is.factor(v1)) 
                table(v1)
            else predval$nobs <- rep(nmin, length(xvar))
        }
        sel <- predval$nobs == 0
        if (any(sel)) {
            predval$fit[sel] <- NA
            if (se.fit) 
                predval$se.fit[sel] <- NA
        }
    }
    if (plotit) {
        y <- predval$fit
        if (nmin > 0) 
            y[predval$nobs < nmin] <- NA
        if (is.null(ylab)) {
            ylab <- if (is.null(cpue.fit)) 
                "Expected probability of non-zero catch"
            else if (is.null(nonzero.fit)) 
                "Expected non-zero catch rate"
            else "Expected catch rate"
            if (multiple) 
                ylab <- paste(ylab, "by", variable[2])
        }
        if (is.null(xlab)) 
            xlab <- variable[1]
        if (multiple) {
            plot(xvar, y[, 1], type = type, ylim = c(0, max(y, 
                na.rm = T)), yaxs = "r", xlab = xlab, ylab = ylab, 
                xaxt = xaxt, xlim = range(xvar), pch = "1", ...)
            for (i in 2:ncol(y)) lines(xvar, y[, i], type = type, 
                pch = paste(i), col = i)
        }
        else {
            plot(xvar, y, type = type, ylim = c(0, max(y, na.rm = T)), 
                yaxs = "r", xlab = xlab, ylab = ylab, xlim = range(xvar), 
                xaxt = xaxt, ...)
        }
        if (xaxt == "n") 
            axis(1, xvar, xlev)
        if (plotvalues) {
            vals <- lapply(fixed, function(x) if (is.numeric(x)) 
                round(x, 1)
            else x)
            text(min(xvar) + 0.02 * diff(range(xvar)), max(y, 
                na.rm = T) * 0.98, paste(paste(names(fixed), 
                "=", as.vector(unlist(vals))), collapse = "\n"), 
                adj = 0)
        }
        cat("\n\n")
        cat(paste(paste(names(fixed), "=", as.vector(unlist(fixed))), 
            collapse = "\n"))
        cat("\n\n")
    }
    if (incl.fixed) 
        predval$fixed <- unlist(fixed)
    predval <- predval[c("fit", "se.fit", "nobs", "fixed")[c(T, 
        se.fit, nobs, incl.fixed)]]
    if (length(predval) == 1) 
        predval[[1]]
    else predval
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
