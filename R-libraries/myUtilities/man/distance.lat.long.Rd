\name{distance.lat.long}
\alias{distance.lat.long}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
distance.lat.long(x1, y1, x2, y2, units = "metres")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{y1}{
%%     ~~Describe \code{y1} here~~
}
  \item{x2}{
%%     ~~Describe \code{x2} here~~
}
  \item{y2}{
%%     ~~Describe \code{y2} here~~
}
  \item{units}{
%%     ~~Describe \code{units} here~~
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
function (x1, y1, x2, y2, units = "metres") 
{
    radians <- function(xin) {
        tmp.rad <- (pi * xin)/180
        return(tmp.rad)
    }
    if (units == "metres") {
        r <- 6378388
    }
    else if (units == "km") {
        r <- 6378.388
    }
    else if (is.numeric(units) == TRUE) {
        r <- units
    }
    else {
        stop("Not a valid unit type...")
    }
    tmp.trig <- (sin(radians(y1)) * sin(radians(y2))) + (cos(radians(y1)) * 
        cos(radians(y2)) * cos(radians(x1) - radians(x2)))
    tmp.d <- r * acos(tmp.trig)
    return(tmp.d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
