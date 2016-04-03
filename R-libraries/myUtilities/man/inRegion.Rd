\name{inRegion}
\alias{inRegion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inRegion(corners, rx, ry)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corners}{
%%     ~~Describe \code{corners} here~~
}
  \item{rx}{
%%     ~~Describe \code{rx} here~~
}
  \item{ry}{
%%     ~~Describe \code{ry} here~~
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
function (corners, rx, ry) 
{
    In <- rep(F, length(rx))
    a1 <- rep(0, length(rx))
    n <- length(corners$x)
    for (j in c(1:(n + 1))) {
        i <- j
        if (j > n) 
            i <- 1
        Inn <- ((corners$y[i] == ry) & (corners$x[i] == rx))
        In[Inn] <- T
        Atan <- atan2(corners$y[i] - ry, corners$x[i] - rx)
        if (j == 1) 
            prevAtan <- Atan
        a0 <- Atan - prevAtan
        a0[!is.finite(a0)] <- 0
        a0[a0 > pi] <- a0[a0 > pi] - 2 * pi
        a0[a0 < -pi] <- a0[a0 < -pi] + 2 * pi
        a1 <- a1 + a0
        prevAtan <- Atan
    }
    In[!In] <- abs(abs(a1[!In]) - 2 * pi) < 0.001
    if (length(In[!In]) > 0) {
        Inn <- abs(a1[!In]) > 0.001
        if (length(Inn[Inn]) > 0) {
            print("Queer points .. CHECK them")
            print(t(matrix(c(a1[!In][Inn]/2/pi * 360, c(1:n)[!In][Inn]), 
                ncol = 2, dimnames = list(NULL, c("Sum angles", 
                  "Index of point")))))
        }
    }
    return(In)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
