\name{geo.dist}
\alias{geo.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
geo.dist(x, y, x2 = NULL, y2 = NULL, r = 1, f = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{x2}{
%%     ~~Describe \code{x2} here~~
}
  \item{y2}{
%%     ~~Describe \code{y2} here~~
}
  \item{r}{
%%     ~~Describe \code{r} here~~
}
  \item{f}{
%%     ~~Describe \code{f} here~~
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
function (x, y, x2 = NULL, y2 = NULL, r = 1, f = 0) 
{
    if (missing(y)) 
        y <- NULL
    if (is.matrix(x) | is.data.frame(x)) {
        if (all(c("long", "lat") \%in\% colnames(x))) {
            long1 <- x[, "long"]
            lat1 <- x[, "lat"]
        }
        else {
            long1 <- x[, 1]
            lat1 <- x[, 2]
        }
        if (is.matrix(y) | is.data.frame(y)) {
            if (all(c("long", "lat") \%in\% colnames(y))) {
                long2 <- y[, "long"]
                lat2 <- y[, "lat"]
            }
            else {
                long2 <- y[, 1]
                lat2 <- y[, 2]
            }
        }
        else {
            if (!(is.vector(x2) & is.vector(y2))) 
                stop("Coordinates of second points are not in vector, matrix or data frame format")
            long2 <- x2
            lat2 <- y2
        }
    }
    else {
        if (!(is.vector(x) & is.vector(y) & is.vector(x2) & is.vector(y2))) 
            stop("Coordinates of points are not in vector, matrix or data frame format")
        long1 <- x
        lat1 <- y
        long2 <- x2
        lat2 <- y2
    }
    llg1 <- length(long1)
    llg2 <- length(long2)
    if (llg1 != length(lat1)) 
        stop("vectors of latitude and longititude for first points are NOT equal")
    if (llg2 != length(lat2)) 
        stop("vectors of latitude and longititude for second points are NOT equal")
    if (llg2 < llg1) {
        l0 <- numeric(llg1)
        long2 <- long2 + l0
        lat2 <- lat2 + l0
        if (llg2 > 1) 
            warning("second points are being recycled")
    }
    if (llg1 < llg2) {
        l0 <- numeric(llg2)
        long1 <- long1 + l0
        lat1 <- lat1 + l0
        if (llg1 > 1) 
            warning("first points are being recycled")
    }
    if (any(abs(lat1)[!is.na(lat1)] >= 90)) 
        stop("Some latitudes for the first points are out of range")
    if (any(abs(lat2)[!is.na(lat2)] >= 90)) 
        stop("Some latitudes for the second points are out of range")
    long1 <- (long1\%\%360) * pi/180
    long2 <- (long2\%\%360) * pi/180
    lat1 <- lat1 * pi/180
    lat2 <- lat2 * pi/180
    if (abs(f) > 1e-06) {
        warning(paste("Oblate spheroid Earth not implemented,", 
            "assuming Earth is a sphere"))
    }
    return(r * acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * 
        cos(long1 - long2)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
