\name{Area}
\alias{Area}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Area(corners)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corners}{
%%     ~~Describe \code{corners} here~~
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
function (corners) 
{
    if (length(corners$x) < 3 | length(corners$y) < 3) {
        print("ERROR Area(): need 3 or more points ************")
        return(NULL)
    }
    if (length(corners$x) != length(corners$y)) {
        print("ERROR Area(): points in corners$y not equal to corners$x ************")
        return(NULL)
    }
    TotArea <- 0
    SubArea <- 0
    n <- length(corners$x)
    I0 <- 0
    for (j in c(1:(n))) {
        if (!is.na(corners$y[j]) & !is.na(corners$x[j]) & I0 == 
            0) 
            I0 <- j
    }
    for (j in c(I0:(n))) {
        i <- j
        i1 <- j + 1
        if (i1 > n) 
            i1 <- I0
        if (is.na(corners$y[i1]) | is.na(corners$x[i1])) 
            i1 <- I0
        if (!is.na(corners$y[i]) & !is.na(corners$x[i])) {
            SubArea <- SubArea + corners$x[i] * corners$y[i1] - 
                corners$x[i1] * corners$y[i]
        }
        else {
            TotArea <- TotArea + abs(SubArea)
            SubArea <- 0
            I0 <- i1
        }
    }
    TotArea <- (TotArea + abs(SubArea))/2
    return(TotArea)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
