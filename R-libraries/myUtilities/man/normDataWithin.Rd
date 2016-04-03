\name{normDataWithin}
\alias{normDataWithin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
normDataWithin(data = NULL, idvar, measurevar, betweenvars = NULL, na.rm = FALSE, .drop = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{idvar}{
%%     ~~Describe \code{idvar} here~~
}
  \item{measurevar}{
%%     ~~Describe \code{measurevar} here~~
}
  \item{betweenvars}{
%%     ~~Describe \code{betweenvars} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{.drop}{
%%     ~~Describe \code{.drop} here~~
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
function (data = NULL, idvar, measurevar, betweenvars = NULL, 
    na.rm = FALSE, .drop = TRUE) 
{
    require(plyr)
    data.subjMean <- ddply(data, c(idvar, betweenvars), .drop = .drop, 
        .fun = function(xx, col, na.rm) {
            c(subjMean = mean(xx[, col], na.rm = na.rm))
        }, measurevar, na.rm)
    data <- merge(data, data.subjMean)
    measureNormedVar <- paste(measurevar, "Normed", sep = "")
    data[, measureNormedVar] <- data[, measurevar] - data[, "subjMean"] + 
        mean(data[, measurevar], na.rm = na.rm)
    data$subjMean <- NULL
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
