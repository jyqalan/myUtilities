\name{new.graph}
\alias{new.graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
new.graph(size = 1, pointsize = 10, xaxs = "i", yaxs = "i", las = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{pointsize}{
%%     ~~Describe \code{pointsize} here~~
}
  \item{xaxs}{
%%     ~~Describe \code{xaxs} here~~
}
  \item{yaxs}{
%%     ~~Describe \code{yaxs} here~~
}
  \item{las}{
%%     ~~Describe \code{las} here~~
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
function (size = 1, pointsize = 10, xaxs = "i", yaxs = "i", las = 1, 
    ...) 
{
    windows.options(reset = TRUE)
    width <- (21/2.54) * 0.8
    height <- (29.7/2.54) * 0.8
    windows(width = width, height = height * size, pointsize = pointsize, 
        bg = "white", rescale = "fixed")
    par(xaxs = xaxs, yaxs = yaxs, las = las, ...)
    .par <<- par(no.readonly = TRUE)
    .par$size <<- size
    .par$pointsize <<- pointsize
    invisible()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
