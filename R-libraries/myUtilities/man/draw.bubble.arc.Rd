\name{draw.bubble.arc}
\alias{draw.bubble.arc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw.bubble.arc(mat, proportions, rescale, inches, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{proportions}{
%%     ~~Describe \code{proportions} here~~
}
  \item{rescale}{
%%     ~~Describe \code{rescale} here~~
}
  \item{inches}{
%%     ~~Describe \code{inches} here~~
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
function (mat, proportions, rescale, inches, ...) 
{
    nx <- ncol(mat)
    ny <- nrow(mat)
    if (rescale) {
        inches.mat <- inches * sqrt(mat/rescale)
        inches <- inches * sqrt(Max(mat)/rescale)
    }
    else {
        inches.mat <- inches * sqrt(mat)
        inches <- inches * sqrt(Max(mat))
    }
    mat <- sqrt(mat/pi)
    for (j in 1:ny) {
        for (i in 1:nx) {
            if (!is.na(proportions[j, i]) & !is.na(mat[j, i])) {
                rx <- xinch(inches.mat[j, i])
                ry <- yinch(inches.mat[j, i])
                angles <- seq(-round(proportions[j, i] * 180), 
                  round(proportions[j, i] * 180), by = 1)/360 * 
                  2 * pi
                x <- i + rx * cos(angles)
                y <- j + ry * sin(angles)
                polygon(c(i, x), c(j, y), ...)
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
