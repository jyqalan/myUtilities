\name{match.up}
\alias{match.up}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
match.up(Dx, Ix, Iy, nomatch = NA, incomparables = FALSE, zap.NAs = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Dx}{
%%     ~~Describe \code{Dx} here~~
}
  \item{Ix}{
%%     ~~Describe \code{Ix} here~~
}
  \item{Iy}{
%%     ~~Describe \code{Iy} here~~
}
  \item{nomatch}{
%%     ~~Describe \code{nomatch} here~~
}
  \item{incomparables}{
%%     ~~Describe \code{incomparables} here~~
}
  \item{zap.NAs}{
%%     ~~Describe \code{zap.NAs} here~~
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
function (Dx, Ix, Iy, nomatch = NA, incomparables = FALSE, zap.NAs = FALSE) 
{
    if (!all(is.vector(Dx) || is.vector(Ix) || is.vector(Iy))) {
        stop("at least one of Dx, Ix, or Iy is not a vector")
    }
    if (!any(unique(Ix) \%in\% unique(Iy))) {
        stop("Ix and Iy contain no values in common")
    }
    if (length(Dx) != length(Ix)) {
        stop("the length of your data vector (", deparse(substitute(Dx)), 
            ") does not equal the length of the corresponding index (", 
            deparse(substitute(Ix)), ")")
    }
    OUT <- match(as.character(Iy), as.character(Ix), nomatch = nomatch, 
        incomparables = incomparables)
    Dy <- Dx[OUT]
    if (zap.NAs) {
        Dy[is.na(Dy)] <- rep(0, length(Dy[is.na(Dy)]))
    }
    return(Dy)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
