\name{get.top.n}
\alias{get.top.n}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.top.n(x, y, FUN = sum, n = 10, na.rm = TRUE, zap.zeros = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{zap.zeros}{
%%     ~~Describe \code{zap.zeros} here~~
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
function (x, y, FUN = sum, n = 10, na.rm = TRUE, zap.zeros = TRUE, 
    ...) 
{
    TAB <- tapply(y, x, FUN, na.rm = TRUE, ...)
    TAB <- sort(TAB, decreasing = TRUE)
    if (zap.zeros) {
        TAB <- TAB[TAB > 0]
        if (length(TAB) < n) {
            N <- length(TAB)
            index <- 1:N
            warning(paste("You have fewer non-zero names available than your specified n, ", 
                N, " names only will (can) be returned", sep = ""))
        }
        else {
            index <- 1:n
        }
    }
    else {
        index <- 1:n
    }
    out <- names(TAB)[index]
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
