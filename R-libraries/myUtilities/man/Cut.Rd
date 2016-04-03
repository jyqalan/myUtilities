\name{Cut}
\alias{Cut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Cut(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, dig.lab = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{include.lowest}{
%%     ~~Describe \code{include.lowest} here~~
}
  \item{right}{
%%     ~~Describe \code{right} here~~
}
  \item{dig.lab}{
%%     ~~Describe \code{dig.lab} here~~
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
function (x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, 
    dig.lab = 3, ...) 
{
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    if (length(breaks) == 1) {
        if (is.na(breaks) | breaks < 2) 
            stop("invalid number of intervals")
        nb <- as.integer(breaks + 1)
        dx <- diff(rx <- range(x, na.rm = TRUE))
        if (dx == 0) 
            dx <- rx[1]
        breaks <- seq(rx[1] - dx/1000, rx[2] + dx/1000, len = nb)
    }
    else nb <- length(breaks <- sort(breaks))
    if (any(duplicated(breaks))) 
        stop("'breaks' are not unique")
    codes.only <- FALSE
    if (is.null(labels)) {
        for (dig in dig.lab:max(12, dig.lab)) {
            ch.br <- formatC(breaks, digits = dig, wid = 1)
            if (ok <- all(ch.br[-1] != ch.br[-nb])) 
                break
        }
        labels <- if (ok) 
            (breaks[-1] + breaks[-nb])/2
        else (1:(nb - 1) + 2:nb)/2
    }
    else if (is.logical(labels) && !labels) 
        codes.only <- TRUE
    else if (length(labels) != nb - 1) 
        stop("labels/breaks length conflict")
    code <- .bincode(x = as.double(x), breaks = as.double(breaks), 
        right = as.logical(right), include.lowest = as.logical(include.lowest))
    if (codes.only) 
        code
    else factor(code, seq(labels), labels)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
