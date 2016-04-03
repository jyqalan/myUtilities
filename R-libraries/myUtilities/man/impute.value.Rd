\name{impute.value}
\alias{impute.value}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
impute.value(VAR, fix.index, group.index, FUN = median, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{VAR}{
%%     ~~Describe \code{VAR} here~~
}
  \item{fix.index}{
%%     ~~Describe \code{fix.index} here~~
}
  \item{group.index}{
%%     ~~Describe \code{group.index} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
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
function (VAR, fix.index, group.index, FUN = median, ...) 
{
    VAR[fix.index] <- NA
    group.use <- group.index \%in\% unique(group.index[fix.index])
    temp <- tapply(VAR[group.use], group.index[group.use], FUN, 
        ...)
    temp.index <- names(temp)
    VAR[fix.index] <- temp[match(group.index[fix.index], temp.index)]
    return(VAR)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
