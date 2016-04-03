\name{SavePlot}
\alias{SavePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SavePlot(filename = "", path = "", size = 1, pointsize = 12, res = 300, type = "jpg")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{pointsize}{
%%     ~~Describe \code{pointsize} here~~
}
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (filename = "", path = "", size = 1, pointsize = 12, 
    res = 300, type = "jpg") 
{
    width <- (21/2.54) * 0.8
    height <- (29.7/2.54) * 0.8
    if (is.null(filename)) 
        stop("Please specify a filename")
    this.image <- recordPlot()
    valid <- c("jpg", "png", "pdf", "wmf")
    Type <- valid[match(tolower(type), valid)]
    if (is.na(Type)) {
        stop(paste(type, "is an invalid file type"))
    }
    else if (Type == "png") {
        filename <- paste(make.filename(filename, path), ".png", 
            sep = "")
        png(filename = filename, width = width, height = height * 
            size, units = "in", pointsize = pointsize, bg = "white", 
            res = res, restoreConsole = TRUE)
        replayPlot(this.image)
        dev.off()
    }
    else if (type == "jpg") {
        filename <- paste(make.filename(filename, path), ".jpg", 
            sep = "")
        jpeg(filename = filename, width = width, height = height * 
            size, units = "in", quality = 100, pointsize = pointsize, 
            bg = "white", res = res, restoreConsole = TRUE)
        replayPlot(this.image)
        dev.off()
    }
    else if (type == "pdf") {
        filename <- paste(make.filename(filename, path), ".pdf", 
            sep = "")
        pdf(file = filename, width = width, height = height * 
            size, pointsize = pointsize, bg = "white")
        replayPlot(this.image)
        dev.off()
    }
    else if (type == "wmf") {
        filename <- paste(make.filename(filename, path), ".wmf", 
            sep = "")
        savePlot(filename, type = "wmf")
    }
    return(filename)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
