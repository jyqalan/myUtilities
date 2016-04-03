\name{qqDist}
\alias{qqDist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
qqDist(x, standardise = F, add.median = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{standardise}{
%%     ~~Describe \code{standardise} here~~
}
  \item{add.median}{
%%     ~~Describe \code{add.median} here~~
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
function (x, standardise = F, add.median = F, ...) 
{
    n <- length(x)
    seq.length <- min(1000, n)
    if (standardise) {
        SEQ <- seq(1, 2 * n + 1, length = seq.length)/2
        U <- qnorm(qbeta(0.975, SEQ, rev(SEQ)))
        L <- qnorm(qbeta(0.025, SEQ, rev(SEQ)))
        if (add.median) 
            M <- qnorm(qbeta(0.5, SEQ, rev(SEQ)))
    }
    else {
        SD <- sqrt(var(x) * (n + 1)/n)
        SEQ <- seq(1, 2 * n + 1, length = seq.length)/2
        U <- mean(x) + SD * qt(qbeta(0.975, SEQ, rev(SEQ)), n - 
            1)
        L <- mean(x) + SD * qt(qbeta(0.025, SEQ, rev(SEQ)), n - 
            1)
        if (add.median) 
            M <- mean(x) + SD * qt(qbeta(0.5, SEQ, rev(SEQ)), 
                n - 1)
    }
    X <- qnorm((SEQ - 0.25)/(n + 0.5))
    qqnorm(x, main = "", ...)
    lines(X, U, type = "l")
    lines(X, L, type = "l")
    if (add.median) 
        lines(X, M, type = "l")
    invisible()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
