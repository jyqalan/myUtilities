\name{SW}
\alias{SW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SW(std = NULL, width = NULL, height = NULL, size = NULL, pointsize = NULL, rescale = NULL, bg = NULL, mar = NULL, oma = NULL, xaxs = NULL, yaxs = NULL, las = NULL, mfrow = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{std}{
%%     ~~Describe \code{std} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{pointsize}{
%%     ~~Describe \code{pointsize} here~~
}
  \item{rescale}{
%%     ~~Describe \code{rescale} here~~
}
  \item{bg}{
%%     ~~Describe \code{bg} here~~
}
  \item{mar}{
%%     ~~Describe \code{mar} here~~
}
  \item{oma}{
%%     ~~Describe \code{oma} here~~
}
  \item{xaxs}{
%%     ~~Describe \code{xaxs} here~~
}
  \item{yaxs}{
%%     ~~Describe \code{yaxs} here~~
}
  \item{las}{
%%     ~~Describe \code{las} here~~
}
  \item{mfrow}{
%%     ~~Describe \code{mfrow} here~~
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
function (std = NULL, width = NULL, height = NULL, size = NULL, 
    pointsize = NULL, rescale = NULL, bg = NULL, mar = NULL, 
    oma = NULL, xaxs = NULL, yaxs = NULL, las = NULL, mfrow = NULL, 
    ...) 
{
    if (is.null(pointsize)) 
        pointsize <- 10
    if (is.null(rescale)) 
        rescale = "fixed"
    if (is.null(bg)) 
        bg = "white"
    if (is.null(size)) 
        size = 1
    if (is.null(xaxs)) 
        xaxs = "r"
    if (is.null(yaxs)) 
        yaxs = "r"
    if (is.null(las)) 
        las = 0
    if (is.null(mar)) 
        mar = c(5.1, 4.1, 4.1, 2.1)
    if (is.null(oma)) 
        oma = c(0, 0, 0, 0)
    if (is.null(std)) {
        if (is.null(width)) 
            width = (21/2.54) * 0.8
        if (is.null(height)) 
            height = (29.7/2.54) * 0.8 * size
        if (is.null(mfrow)) 
            mfrow = c(1, 1)
    }
    else if (std == 0) {
        if (is.null(width)) 
            width = 7
        if (is.null(height)) 
            height = 7
        if (is.null(mfrow)) 
            mfrow = c(1, 1)
    }
    else if (std == 1) {
        width = 5.75
        height = 7.4
        if (is.null(mfrow)) 
            mfrow = c(2, 1)
    }
    else {
        stop(paste("std=", std, ",not supported!", sep = ""))
    }
    windows.options(reset = TRUE)
    windows(width = width, height = height, pointsize = pointsize, 
        rescale = rescale, bg = bg)
    par(mar = mar, oma = oma, mfrow = mfrow, xaxs = xaxs, yaxs = yaxs, 
        las = las, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
