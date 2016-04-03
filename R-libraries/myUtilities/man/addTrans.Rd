\name{addTrans}
\alias{addTrans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addTrans(color, trans)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{color}{
%%     ~~Describe \code{color} here~~
}
  \item{trans}{
%%     ~~Describe \code{trans} here~~
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
function (color, trans) 
{
    if (length(color) != length(trans) & !any(c(length(color), 
        length(trans)) == 1)) 
        stop("Vector lengths not correct")
    if (length(color) == 1 & length(trans) > 1) 
        color <- rep(color, length(trans))
    if (length(trans) == 1 & length(color) > 1) 
        trans <- rep(trans, length(color))
    num2hex <- function(x) {
        hex <- unlist(strsplit("0123456789ABCDEF", split = ""))
        return(paste(hex[(x - x\%\%16)/16 + 1], hex[x\%\%16 + 1], 
            sep = ""))
    }
    rgb <- rbind(col2rgb(color), trans)
    res <- paste("#", apply(apply(rgb, 2, num2hex), 2, paste, 
        collapse = ""), sep = "")
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
