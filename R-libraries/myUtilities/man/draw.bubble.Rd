\name{draw.bubble}
\alias{draw.bubble}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw.bubble(mat, xat = 1:ncol(mat), yat = 1:nrow(mat), xnames.arg = colnames(mat), ynames.arg = rownames(mat), rescale = FALSE, fg = "black", bg = "lightgrey", main = NULL, xlab = NULL, ylab = NULL, inches = TRUE, plot.NA = F, add = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{xat}{
%%     ~~Describe \code{xat} here~~
}
  \item{yat}{
%%     ~~Describe \code{yat} here~~
}
  \item{xnames.arg}{
%%     ~~Describe \code{xnames.arg} here~~
}
  \item{ynames.arg}{
%%     ~~Describe \code{ynames.arg} here~~
}
  \item{rescale}{
%%     ~~Describe \code{rescale} here~~
}
  \item{fg}{
%%     ~~Describe \code{fg} here~~
}
  \item{bg}{
%%     ~~Describe \code{bg} here~~
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
  \item{inches}{
%%     ~~Describe \code{inches} here~~
}
  \item{plot.NA}{
%%     ~~Describe \code{plot.NA} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
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
function (mat, xat = 1:ncol(mat), yat = 1:nrow(mat), xnames.arg = colnames(mat), 
    ynames.arg = rownames(mat), rescale = FALSE, fg = "black", 
    bg = "lightgrey", main = NULL, xlab = NULL, ylab = NULL, 
    inches = TRUE, plot.NA = F, add = F, ...) 
{
    nx <- ncol(mat)
    ny <- nrow(mat)
    xlim = c(1, nx)
    ylim = c(1, ny)
    tmp.x <- 1:ncol(mat)
    tmp.y <- 1:nrow(mat)
    tmp.z <- as.vector(mat)
    tmp.x <- unlist(lapply(tmp.x, rep, ny))
    tmp.y <- rep(tmp.y, nx)
    tmp.z <- sqrt(tmp.z/pi)
    if (rescale) {
        inches.mat <- inches * sqrt(mat/rescale)
        inches <- inches * sqrt(Max(mat)/rescale)
    }
    else {
        inches.mat <- inches * sqrt(mat)
        inches <- inches * sqrt(Max(mat))
    }
    mat <- sqrt(mat/pi)
    if (add == F) {
        plot.new()
        plot.window(xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n")
    }
    if (plot.NA) {
        tmp.z <- ifelse(is.na(tmp.z), 0, tmp.z)
        symbols(tmp.x, tmp.y, circles = tmp.z, inches = inches, 
            add = TRUE, fg = fg, bg = bg, ...)
    }
    else {
        for (j in 1:ny) {
            for (i in 1:nx) {
                if (!is.na(mat[j, i])) 
                  symbols(i, j, circles = mat[j, i], inches = inches.mat[j, 
                    i], add = TRUE, fg = fg, bg = bg, ...)
            }
        }
    }
    box()
    axis(1, at = xat, labels = xnames.arg)
    axis(2, at = yat, labels = ynames.arg)
    title(main = main, xlab = xlab, ylab = ylab)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
