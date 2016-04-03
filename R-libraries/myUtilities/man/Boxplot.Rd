\name{Boxplot}
\alias{Boxplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Boxplot(x, name = names(x), medlwd = 2, staplewex = 0, staplehex = 0, outpch = 1, outline = F, whisklty = 1, range = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{medlwd}{
%%     ~~Describe \code{medlwd} here~~
}
  \item{staplewex}{
%%     ~~Describe \code{staplewex} here~~
}
  \item{staplehex}{
%%     ~~Describe \code{staplehex} here~~
}
  \item{outpch}{
%%     ~~Describe \code{outpch} here~~
}
  \item{outline}{
%%     ~~Describe \code{outline} here~~
}
  \item{whisklty}{
%%     ~~Describe \code{whisklty} here~~
}
  \item{range}{
%%     ~~Describe \code{range} here~~
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
function (x, name = names(x), medlwd = 2, staplewex = 0, staplehex = 0, 
    outpch = 1, outline = F, whisklty = 1, range = 0, ...) 
{
    old.par <- par("tcl")
    par(tcl = 0)
    if (missing(name) || is.null(name)) 
        name <- F
    centers <- boxplot(x, names = name, medlwd = medlwd, staplewex = staplewex, 
        staplehex = staplehex, outpch = outpch, outline = outline, 
        whisklty = whisklty, range = range, ...)
    par(tcl = old.par)
    invisible(1:length(x))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
