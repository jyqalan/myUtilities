\name{plot.lf}
\alias{plot.lf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.lf(data, lgth, cv, xlim, ylim, ylim2, xat, yat, yat2, xnames, ynames, ynames2, line = 0.5, cex = 0.8, col = 0, col2 = 0, cex2 = 0.8, border = "black", offset = 0, tck = -0.025, outer = F, add = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{lgth}{
%%     ~~Describe \code{lgth} here~~
}
  \item{cv}{
%%     ~~Describe \code{cv} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{ylim2}{
%%     ~~Describe \code{ylim2} here~~
}
  \item{xat}{
%%     ~~Describe \code{xat} here~~
}
  \item{yat}{
%%     ~~Describe \code{yat} here~~
}
  \item{yat2}{
%%     ~~Describe \code{yat2} here~~
}
  \item{xnames}{
%%     ~~Describe \code{xnames} here~~
}
  \item{ynames}{
%%     ~~Describe \code{ynames} here~~
}
  \item{ynames2}{
%%     ~~Describe \code{ynames2} here~~
}
  \item{line}{
%%     ~~Describe \code{line} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{col2}{
%%     ~~Describe \code{col2} here~~
}
  \item{cex2}{
%%     ~~Describe \code{cex2} here~~
}
  \item{border}{
%%     ~~Describe \code{border} here~~
}
  \item{offset}{
%%     ~~Describe \code{offset} here~~
}
  \item{tck}{
%%     ~~Describe \code{tck} here~~
}
  \item{outer}{
%%     ~~Describe \code{outer} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
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
function (data, lgth, cv, xlim, ylim, ylim2, xat, yat, yat2, 
    xnames, ynames, ynames2, line = 0.5, cex = 0.8, col = 0, 
    col2 = 0, cex2 = 0.8, border = "black", offset = 0, tck = -0.025, 
    outer = F, add = F) 
{
    par(xaxs = "i", yaxs = "i")
    if (length(data) != length(lgth)) {
        stop("lengthof data not equal to that of lgth")
    }
    if (missing(xlim)) {
        xlim <- c(0, max(lgth))
    }
    if (missing(ylim)) {
        ylim <- c(0, max(pretty(data)))
    }
    if (missing(xat)) 
        xat = lgth[lgth\%\%20 == 0]
    if (missing(yat)) 
        yat = ylim
    if (missing(xnames)) 
        xnames = xat
    if (missing(ynames)) 
        ynames = yat
    if (!missing(cv)) {
        if (missing(ylim2)) {
            ylim2 = c(0, max(pretty(cv)))
        }
        if (missing(yat2)) {
            yat2 = ylim2
        }
        if (missing(ynames2)) {
            ynames2 = yat2
        }
        if (length(yat2) != length(ynames2)) 
            stop("yat2 and ynames2 not of the same length")
    }
    if (length(xat) != length(xnames)) 
        stop("xat and xnames not of the same length")
    if (length(yat) != length(ynames)) 
        stop("yat and ynames not of the same length")
    offset = ylim[2] * offset
    ylim_prime = ylim
    yat_prime = yat
    ylim_prime[2] = ylim_prime[2] + offset
    yat_prime = yat_prime + offset
    if (!missing(cv)) {
        yat2 = yat2/ylim2[2] * ylim[2]
        ylim2_prime = ylim2
        yat2_prime = yat2
        ylim2_prime[2] = ylim2_prime[2] + offset
        yat2_prime = yat2_prime + offset
    }
    xx <- barplot(data, xlim = xlim, ylim = ylim_prime, space = 0, 
        density = -1, err = -1, xlab = "", ylab = "", yaxt = "n", 
        xaxt = "n", col = col, border = border, offset = offset, 
        add = add)
    if (outer) {
        zz <- axis(1, at = xat + (diff(xx)/2)[1], labels = xnames, 
            tck = tck, outer = T)
    }
    else {
        zz <- axis(1, at = xat + (diff(xx)/2)[1], labels = F, 
            tck = tck, outer = F)
        mtext(side = 1, at = xat + (diff(xx)/2)[1], text = xnames, 
            line = line, cex = cex, outer = F)
    }
    if (outer) {
        axis(2, at = yat_prime, labels = ynames, tck = tck, outer = T)
    }
    else {
        axis(2, at = yat_prime, labels = F, tck = tck, outer = F)
        mtext(side = 2, at = yat_prime, text = ynames, line = line, 
            cex = cex, outer = F)
    }
    if (!missing(cv)) {
        points(xx, (cv)/ylim2[2] * ylim[2] + offset, pch = 16, 
            cex = cex2, col = col2)
        lines(xx, (cv)/ylim2[2] * ylim[2] + offset, col = col2)
        if (outer) {
            axis(4, at = yat2_prime, labels = ynames2, tck = tck, 
                outer = T)
        }
        else {
            axis(4, at = yat2_prime, labels = F, tck = tck, outer = F)
            mtext(side = 4, at = yat2_prime, text = ynames2, 
                line = line, cex = cex, outer = F)
        }
    }
    abline(h = offset, col = "gray")
    box()
    invisible()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
