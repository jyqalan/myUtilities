\name{plot.predictCPUE}
\alias{plot.predictCPUE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.predictCPUE(variable, data, cpue.fit = NULL, nonzero.fit = NULL, xcts = F, fixed = list(), nmin = 0, plotit = F, se.fit = T, nobs = (nmin > 0), incl.fixed = F, log.scale = F, type = "l", ylab = NULL, xlab = NULL, axis.labels = TRUE, at = NULL, labels = NULL, plot.se = T, se.lty = 1, plotvalues = T, xlim = NULL, ylim = NULL, scale = F, ...)
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
  \item{axis.labels}{
%%     ~~Describe \code{axis.labels} here~~
}
  \item{at}{
%%     ~~Describe \code{at} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{plot.se}{
%%     ~~Describe \code{plot.se} here~~
}
  \item{se.lty}{
%%     ~~Describe \code{se.lty} here~~
}
  \item{plotvalues}{
%%     ~~Describe \code{plotvalues} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
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
    xcts = F, fixed = list(), nmin = 0, plotit = F, se.fit = T, 
    nobs = (nmin > 0), incl.fixed = F, log.scale = F, type = "l", 
    ylab = NULL, xlab = NULL, axis.labels = TRUE, at = NULL, 
    labels = NULL, plot.se = T, se.lty = 1, plotvalues = T, xlim = NULL, 
    ylim = NULL, scale = F, ...) 
{
    RESULTS <- predictCPUE(variable, data, cpue.fit, nonzero.fit, 
        xcts, fixed, nmin, plotit, se.fit, nobs, incl.fixed, 
        log.scale, type, ylab, xlab, plotvalues, xlim, ...)
    tmp.x <- if (type == "l") {
        as.numeric(names(RESULTS$fit))
    }
    else {
        1:length(RESULTS$fit)
    }
    tmp.y <- RESULTS$fit
    tmp.se <- RESULTS$se.fit
    if (scale) {
        if (plot.se) 
            stop("can not plot std when fits are scaled!")
        tmp.y = tmp.y/mean(tmp.y)
    }
    if (is.null(xlim) == TRUE) {
        xlim <- if (type == "l") {
            c(min(pretty(tmp.x)), max(pretty(tmp.x)))
        }
        else {
            c(1, length(tmp.x))
        }
    }
    if (is.null(ylim) == TRUE) {
        ylim <- c(floor(min(tmp.y - 2 * tmp.se, na.rm = TRUE)), 
            ceiling(max(tmp.y + 2 * tmp.se, na.rm = TRUE)))
    }
    plot(tmp.x, tmp.y, type = type, xlim = xlim, ylim = ylim, 
        xlab = xlab, ylab = ylab, xaxt = "n", ...)
    if (plot.se == TRUE) {
        if (type == "l") {
            points(tmp.x, tmp.y + (2 * tmp.se), type = "l", lty = se.lty)
            points(tmp.x, tmp.y - (2 * tmp.se), type = "l", lty = se.lty)
        }
        else {
            segments(tmp.x, tmp.y, tmp.x, tmp.y + (2 * tmp.se))
            segments(tmp.x, tmp.y, tmp.x, tmp.y - (2 * tmp.se))
        }
    }
    if (axis.labels == TRUE) {
        if (is.null(labels)) {
            labels = names(RESULTS$fit)
        }
        if (is.null(at)) {
            at = tmp.x
        }
        axis(1, at = at, labels = labels)
    }
    return(RESULTS)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
