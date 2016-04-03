\name{proportion.ci}
\alias{proportion.ci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
proportion.ci(r, n, ci = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
%%     ~~Describe \code{r} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{ci}{
%%     ~~Describe \code{ci} here~~
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
function (r, n, ci = 0.95) 
{
    r <- ifelse(r < 1, round(r * n), r)
    t1 <- 1 - (1 - ci)/2
    old.warn <- options()$warn
    options(warn = -1)
    F1 <- qf(t1, 2 * n - 2 * r + 2, 2 * r)
    F2 <- qf(t1, 2 * r + 2, 2 * n - 2 * r)
    options(warn = old.warn)
    lower.ci <- r/(r + (n - r + 1) * F1)
    upper.ci <- (r + 1)/(r + 1 + (n - r)/F2)
    lower.ci <- ifelse(is.na(lower.ci) & !is.na(n) & !is.na(r), 
        0, lower.ci)
    upper.ci <- ifelse(is.na(upper.ci) & !is.na(n) & !is.na(r), 
        1, upper.ci)
    RES <- data.frame(r, n, p = r/n, lower.ci, upper.ci)
    return(RES)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
