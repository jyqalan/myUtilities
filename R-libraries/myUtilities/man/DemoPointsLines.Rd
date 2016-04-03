\name{DemoPointsLines}
\alias{DemoPointsLines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DemoPointsLines()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    Pex <- 3
    ipch <- 1:(np <- 25 + 11)
    k <- floor(sqrt(np))
    dd <- c(-1, 1)/2
    rx <- dd + range(ix <- (ipch - 1)\%/\%k)
    ry <- dd + range(iy <- 3 + (k - 1) - (ipch - 1)\%\%k)
    pch <- as.list(ipch)
    pch[25 + 1:11] <- as.list(c("*", ".", "o", "O", "0", "+", 
        "-", ":", "|", "\%", "#"))
    plot(rx, ry, type = "n", axes = FALSE, xlab = "", ylab = "")
    for (i in 1:np) {
        pc <- pch[[i]]
        points(ix[i], iy[i] + 0.5, pch = pc, cex = Pex)
        text(ix[i] - 0.5, iy[i] + 0.5, pc, cex = 1)
    }
    ilty = 1:6
    lty = as.list(ilty)
    for (i in 1:6) {
        lines(x = c(ix[i] - 0.5, ix[i] + 0.5), y = c(iy[i], iy[i]), 
            lty = lty[[i]], lwd = 2)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
