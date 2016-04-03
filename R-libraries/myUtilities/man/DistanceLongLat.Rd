\name{DistanceLongLat}
\alias{DistanceLongLat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DistanceLongLat(long1, long2, lat1, lat2, metres = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long1}{
%%     ~~Describe \code{long1} here~~
}
  \item{long2}{
%%     ~~Describe \code{long2} here~~
}
  \item{lat1}{
%%     ~~Describe \code{lat1} here~~
}
  \item{lat2}{
%%     ~~Describe \code{lat2} here~~
}
  \item{metres}{
%%     ~~Describe \code{metres} here~~
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
function (long1, long2, lat1, lat2, metres = F) 
{
    if (missing(long1) | missing(long2) | missing(lat1) | missing(lat2)) {
        cat("Using function \"locator(2)\" to locate end points\n")
        x <- nz.locator(2)
        long1 <- x$x[1]
        long2 <- x$x[2]
        lat1 <- x$y[1]
        lat2 <- x$y[2]
        print(unlist(c(long1, long2, lat1, lat2)))
    }
    long1 <- (long1 * pi)/180
    long2 <- (long2 * pi)/180
    lat1 <- ifelse(lat1 > 180, 360 - lat1, -lat1)
    lat2 <- ifelse(lat2 > 180, 360 - lat2, -lat2)
    lat1 <- (lat1 * pi)/180
    lat2 <- (lat2 * pi)/180
    d <- 2 * asin(sqrt((sin((lat1 - lat2)/2))^2 + cos(lat1) * 
        cos(lat2) * (sin((long1 - long2)/2))^2))
    nm <- (d * 180 * 60)/pi
    if (metres) 
        nm <- nm * 1.852 * 1000
    return(nm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
