\name{draw.dplot}
\alias{draw.dplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw.dplot(..., name = T, quantiles = c(0.5), plot.mean = F, main = "Density Plot", xlab = "", ylab = "", ylim = NULL, srtx = 0, draw.axis = "xy", log = "", plot.levels = FALSE, levels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{quantiles}{
%%     ~~Describe \code{quantiles} here~~
}
  \item{plot.mean}{
%%     ~~Describe \code{plot.mean} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{srtx}{
%%     ~~Describe \code{srtx} here~~
}
  \item{draw.axis}{
%%     ~~Describe \code{draw.axis} here~~
}
  \item{log}{
%%     ~~Describe \code{log} here~~
}
  \item{plot.levels}{
%%     ~~Describe \code{plot.levels} here~~
}
  \item{levels}{
%%     ~~Describe \code{levels} here~~
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
function (..., name = T, quantiles = c(0.5), plot.mean = F, main = "Density Plot", 
    xlab = "", ylab = "", ylim = NULL, srtx = 0, draw.axis = "xy", 
    log = "", plot.levels = FALSE, levels = NULL) 
{
    dlines <- function(y, offset, Q) {
        y <- y[!is.na(y)]
        if (min(y) == max(y)) {
            ans <- list(x = min(y), y = 1)
            y1 <- ans$x
            x1 <- ans$y/max(ans$y) * 0.8
            q.y1 <- quantile(y, probs = c(0, Q, 1))
            q.x1 <- offset + x1
            q.x2 <- offset
            mean.y1 <- mean(y)
            mean.x1 <- offset + x1
            mean.x2 <- offset
        }
        else {
            ans <- density(y, from = min(y), to = max(y))
            y1 <- ans$x
            x1 <- ans$y/max(ans$y) * 0.8
            q.y1 <- quantile(y, probs = c(0, Q, 1))
            q.x1 <- offset + approx(y1, x1, xout = q.y1)$y
            q.x2 <- offset
            mean.y1 <- mean(y)
            mean.x1 <- offset + approx(y1, x1, xout = mean.y1)$y
            mean.x2 <- offset
        }
        return(list(x1 = offset + x1, y1 = y1, q.x1 = q.x1, q.y1 = q.y1, 
            q.x2 = q.x2, mean.x1 = mean.x1, mean.x2 = mean.x2, 
            mean.y1 = mean.y1))
    }
    all.x <- list(...)
    nam <- character(0)
    if (is.list(all.x[[1]])) {
        all.x <- all.x[[1]]
        if (is.logical(name) && name) 
            name <- names(...)
    }
    if (plot.levels) {
        n <- length(all.x)
        centers <- (match(names(all.x), levels) - 1) + 0.1
        centers.all <- (seq(1, length(levels), 1) - 1) + 0.1
        ymax <- max(sapply(all.x, max, na.rm = T), na.rm = T)
        if (is.na(ymax)) 
            stop("Error: list of empty vectors")
        ymin <- min(sapply(all.x, min, na.rm = T), na.rm = T)
        xmax <- (length(levels) - 1) + 0.95
        xmin <- 0
    }
    else {
        n <- length(all.x)
        centers <- seq(from = 0, by = 1, length = n) + 0.1
        ymax <- max(sapply(all.x, max, na.rm = T), na.rm = T)
        if (is.na(ymax)) 
            stop("Error: list of empty vectors")
        ymin <- min(sapply(all.x, min, na.rm = T), na.rm = T)
        xmax <- max(centers) + 0.95
        xmin <- 0
    }
    if (!missing(ylim)) {
        plot(c(xmin, xmax), c(ymin, ymax), type = "n", main = main, 
            xlab = xlab, ylab = ylab, xaxt = "n", yaxt = "n", 
            ylim = ylim, log = log)
        if (draw.axis == "xy" || draw.axis == "yx" || draw.axis == 
            "y") {
            if (is.null(levels)) {
                axis(2)
            }
            else {
                axis(2, at = 1:length(levels), labels = levels, 
                  las = 1)
            }
        }
    }
    else {
        plot(c(xmin, xmax), c(ymin, ymax), type = "n", main = main, 
            xlab = xlab, ylab = ylab, xaxt = "n", yaxt = "n", 
            ylim = ylim, log = log)
        if (draw.axis == "xy" || draw.axis == "yx" || draw.axis == 
            "y") {
            axis(2)
        }
    }
    for (i in 1:n) {
        if (length(all.x[[i]][!is.na(all.x[[i]])]) > 0) {
            plot.values <- dlines(all.x[[i]], centers[i], quantiles)
            lines(plot.values$x1, plot.values$y1)
            segments(centers[i], min(plot.values$y1), centers[i], 
                max(plot.values$y1))
            segments(plot.values$q.x1, plot.values$q.y1, plot.values$q.x2, 
                plot.values$q.y1)
            if (plot.mean) 
                segments(plot.values$mean.x1, plot.values$mean.y1, 
                  plot.values$mean.x2, plot.values$mean.y1)
        }
    }
    if (is.logical(name)) {
        if (name) 
            if (draw.axis == "xy" || draw.axis == "yx" || draw.axis == 
                "x") {
                if (plot.levels) {
                  axis(1, centers.all, levels, srt = srtx, adj = if (srtx == 
                    0) 
                    0.5
                  else 1)
                }
                else {
                  axis(1, centers, sapply(substitute(list(...)), 
                    deparse)[2:(n + 1)], srt = srtx, adj = if (srtx == 
                    0) 
                    0.5
                  else 1)
                }
            }
    }
    else if (draw.axis == "xy" || draw.axis == "yx" || draw.axis == 
        "x") {
        if (plot.levels) {
            axis(1, centers.all, levels, srt = srtx, adj = if (srtx == 
                0) 
                0.5
            else 1)
        }
        else {
            axis(1, centers, name, srt = srtx, adj = if (srtx == 
                0) 
                0.5
            else 1)
        }
    }
    invisible(centers)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
