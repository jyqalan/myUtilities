\name{AreaLongLat}
\alias{AreaLongLat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AreaLongLat(Long, Lat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Long}{
%%     ~~Describe \code{Long} here~~
}
  \item{Lat}{
%%     ~~Describe \code{Lat} here~~
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
function (Long, Lat) 
{
    if (missing(Lat)) 
        Poly <- Long
    else Poly <- list(x = Long, y = Lat)
    Tna <- !is.na(Poly$x)
    Poly$x[Tna] <- Poly$x[Tna] * (111.41 * cos(pi/180 * Poly$y[Tna]) - 
        0.01 * cos(3 * pi/180 * Poly$y[Tna]))
    Tna <- !is.na(Poly$y)
    Poly$y[Tna] <- Poly$y[Tna] * (111.14 - 0.56 * cos(2 * pi/180 * 
        Poly$y[Tna]))
    return(Area(Poly))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
