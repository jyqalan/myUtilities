\name{plotCI}
\alias{plotCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotCI(x, y = NULL, uiw, liw = uiw, aui = NULL, ali = aui, err = "y", ylim = NULL, xlim = NULL, sfrac = 0.01, gap = 0, add = FALSE, col = par("col"), lwd = par("lwd"), slty = par("lty"), xlab = NULL, ylab = NULL, main = "", pt.bg = NA, scol = col, axes = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{uiw}{
%%     ~~Describe \code{uiw} here~~
}
  \item{liw}{
%%     ~~Describe \code{liw} here~~
}
  \item{aui}{
%%     ~~Describe \code{aui} here~~
}
  \item{ali}{
%%     ~~Describe \code{ali} here~~
}
  \item{err}{
%%     ~~Describe \code{err} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{sfrac}{
%%     ~~Describe \code{sfrac} here~~
}
  \item{gap}{
%%     ~~Describe \code{gap} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{slty}{
%%     ~~Describe \code{slty} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{pt.bg}{
%%     ~~Describe \code{pt.bg} here~~
}
  \item{scol}{
%%     ~~Describe \code{scol} here~~
}
  \item{axes}{
%%     ~~Describe \code{axes} here~~
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
function (x, y = NULL, uiw, liw = uiw, aui = NULL, ali = aui, 
    err = "y", ylim = NULL, xlim = NULL, sfrac = 0.01, gap = 0, 
    add = FALSE, col = par("col"), lwd = par("lwd"), slty = par("lty"), 
    xlab = NULL, ylab = NULL, main = "", pt.bg = NA, scol = col, 
    axes = TRUE, ...) 
{
    if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (is.null(y)) {
        if (is.null(x)) 
            stop("both x and y NULL")
        y <- as.numeric(x)
        x <- seq(along = x)
    }
    if (missing(xlab)) 
        xlab <- deparse(substitute(x))
    if (missing(ylab)) 
        ylab <- deparse(substitute(y))
    if (missing(uiw)) {
        ui <- aui
        li <- ali
    }
    else {
        if (err == "y") 
            z <- y
        else z <- x
        ui <- z + uiw
        li <- z - liw
    }
    if (err == "y" & is.null(ylim)) 
        ylim <- range(c(y, ui, li), na.rm = TRUE)
    else if (err == "x" & is.null(xlim)) 
        xlim <- range(c(x, ui, li), na.rm = TRUE)
    if (!add) 
        plot(x, y, ylim = ylim, xlim = xlim, col = col, lwd = lwd, 
            xlab = xlab, ylab = ylab, main = main, type = "n", 
            axes = axes, ...)
    if (gap == TRUE) 
        gap <- 0.01
    ul <- c(li, ui)
    if (err == "y") {
        gap <- rep(gap, length(x)) * diff(par("usr")[3:4])
        segments(x, li, x, pmax(y - gap, li), col = scol, lwd = lwd, 
            lty = slty)
        segments(x, ui, x, pmin(y + gap, ui), col = scol, lwd = lwd, 
            lty = slty)
        x2 <- c(x, x)
        smidge <- diff(par("usr")[1:2]) * sfrac
        segments(x2 - smidge, ul, x2 + smidge, ul, col = scol, 
            lwd = lwd)
    }
    else if (err == "x") {
        gap <- rep(gap, length(x)) * diff(par("usr")[1:2])
        segments(li, y, pmax(x - gap, li), y, col = scol, lwd = lwd, 
            lty = slty)
        segments(ui, y, pmin(x + gap, ui), y, col = scol, lwd = lwd, 
            lty = slty)
        y2 <- c(y, y)
        smidge <- diff(par("usr")[3:4]) * sfrac
        segments(ul, y2 - smidge, ul, y2 + smidge, col = scol, 
            lwd = lwd)
    }
    points(x, y, col = col, lwd = lwd, bg = pt.bg, ...)
    invisible(list(x = x, y = y))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
