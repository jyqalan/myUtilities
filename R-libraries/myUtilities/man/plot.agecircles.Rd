\name{plot.agecircles}
\alias{plot.agecircles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.agecircles(mat, b4spawn, expand = 1, xlab = "Year", ylab = "Year class", plotit = T, ylim = NULL, xlim = NULL, only5thcohorts = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{b4spawn}{
%%     ~~Describe \code{b4spawn} here~~
}
  \item{expand}{
%%     ~~Describe \code{expand} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{plotit}{
%%     ~~Describe \code{plotit} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{only5thcohorts}{
%%     ~~Describe \code{only5thcohorts} here~~
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
function (mat, b4spawn, expand = 1, xlab = "Year", ylab = "Year class", 
    plotit = T, ylim = NULL, xlim = NULL, only5thcohorts = T) 
{
    original.mat <- mat
    mat <- sqrt(abs(mat))
    yrs <- as.numeric(dimnames(mat)[[2]])
    nyr <- dim(mat)[2]
    ages <- as.numeric(dimnames(mat)[[1]])
    nage <- dim(mat)[1]
    yrcl <- matrix(0, nage, nyr)
    for (i in 1:nyr) yrcl[, i] <- yrs[i] - ages - ifelse(b4spawn, 
        1, 0)
    ylim <- if (is.null(ylim)) 
        range(-yrcl) + c(-1, 1)
    else rev(-ylim)
    xlim <- if (is.null(xlim)) 
        range(yrs) + c(-1, 1)
    else xlim
    if (plotit) {
        plot(0, 0, type = "n", xlim = xlim, ylim = ylim, xlab = xlab, 
            ylab = ylab, yaxt = "n")
        uyrcl <- unique(yrcl)
        uyrcl <- uyrcl[uyrcl\%\%5 == 0]
        axis(2, -uyrcl, paste(uyrcl))
    }
    pin <- par()$pin
    usr <- par()$usr
    xyin = c(pin[1]/(usr[2] - usr[1]), pin[2]/(usr[4] - usr[3]))
    yrinc <- ifelse(length(yrs) > 1, min(diff(yrs)), 1)
    maxradxin <- 0.5 * expand * yrinc * xyin[1]
    maxradyin <- 0.5 * expand * xyin[2]
    maxradin <- min(maxradxin, maxradyin)
    tmp <- cos(seq(0, pi, len = 13))
    max.circle <- list(x = (tmp[c(1:13, 12:1)] * maxradin)/(xyin[1] * 
        max(mat)), y = (tmp[c(7:1, 2:13, 12:7)] * maxradin)/(xyin[2] * 
        max(mat)))
    if (plotit) {
        for (i in 1:nyr) {
            for (j in 1:nage) {
                polygon(max.circle$x * mat[j, i] + yrs[i], max.circle$y * 
                  mat[j, i] - yrcl[j, i], col = ifelse(original.mat[j, 
                  i] > 0, "white", "black"))
            }
        }
        invisible()
    }
    else list(max.circle = max.circle, mat = mat, yrs = yrs, 
        yrcl = yrcl)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
