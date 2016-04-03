\name{hexbinpie}
\alias{hexbinpie}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hexbinpie(x, y, z, kat, xbnds = range(x), ybnds = range(y), xbin = 20, shape = 1, pal = terrain.colors(length(levels(as.factor(kat)))), hex = "gray", circ = NA, cnt = "black", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{kat}{
%%     ~~Describe \code{kat} here~~
}
  \item{xbnds}{
%%     ~~Describe \code{xbnds} here~~
}
  \item{ybnds}{
%%     ~~Describe \code{ybnds} here~~
}
  \item{xbin}{
%%     ~~Describe \code{xbin} here~~
}
  \item{shape}{
%%     ~~Describe \code{shape} here~~
}
  \item{pal}{
%%     ~~Describe \code{pal} here~~
}
  \item{hex}{
%%     ~~Describe \code{hex} here~~
}
  \item{circ}{
%%     ~~Describe \code{circ} here~~
}
  \item{cnt}{
%%     ~~Describe \code{cnt} here~~
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
function (x, y, z, kat, xbnds = range(x), ybnds = range(y), xbin = 20, 
    shape = 1, pal = terrain.colors(length(levels(as.factor(kat)))), 
    hex = "gray", circ = NA, cnt = "black", ...) 
{
    require(hexbin)
    hb <- hexbin(x, y, xbnds = xbnds, ybnds = ybnds, IDs = TRUE, 
        xbin = xbin, shape = shape)
    hbc <- hcell2xy(hb)
    rx <- diff(hb@xbnds)/(2 * hb@xbins)
    ry <- diff(hb@ybnds)/(2 * hb@xbins * hb@shape)
    hexC <- hexcoords(dx = rx, dy = ry/sqrt(3), n = 1)
    nl <- length(levels(as.factor(kat)))
    ztab <- tapply(z, hb@cID, Sum)
    zbnds <- quantile(ztab, prob = c(0.05, 0.95), na.rm = TRUE)
    maxztab <- max(ztab)
    zz <- pmax(pmin(sqrt(ztab/zbnds[2]), 1), 0.2)
    tt <- tapply(z, list(kat, hb@cID), Sum)
    tt <- ifelse(is.na(tt), 0, tt)
    for (i in seq(along = zz)) {
        if (!is.na(hex)) 
            polygon(hbc$x[i] + hexC$x, hbc$y[i] + hexC$y, col = NA, 
                border = hex)
        if (sum(tt[, i]) == 0) 
            next
        tp <- pi/2 - 2 * pi * c(0, cumsum(tt[, i])/sum(tt[, i]))
        for (j in 1:nl) {
            if (tp[j + 1] == tp[j]) 
                next
            pp <- seq(tp[j], tp[j + 1], length = floor((tp[j] - 
                tp[j + 1]) * 4) + 2)
            xi <- hbc$x[i] + c(0, zz[i] * rx * cos(pp))
            yi <- hbc$y[i] + c(0, zz[i] * ry * sin(pp))
            polygon(xi, yi, col = pal[j], border = NA)
        }
        if (!is.na(circ)) 
            polygon(hbc$x[i] + rx * zz[i] * cos((1:18) * pi/9), 
                hbc$y[i] + ry * zz[i] * sin((1:18) * pi/9), col = NA, 
                border = circ)
    }
    return(maxztab)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
