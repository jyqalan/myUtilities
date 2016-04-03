\name{image.scale}
\alias{image.scale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
image.scale(z, col, x, y = NULL, size = NULL, digits = 2, labels = c("breaks", "ranges"), values = TRUE, breaks = NULL, label.text = NULL, title.add = TRUE, title.text = NULL, title.adj = c(0.5, 0), title.cex = 0.8, box.scale = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{values}{
%%     ~~Describe \code{values} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{label.text}{
%%     ~~Describe \code{label.text} here~~
}
  \item{title.add}{
%%     ~~Describe \code{title.add} here~~
}
  \item{title.text}{
%%     ~~Describe \code{title.text} here~~
}
  \item{title.adj}{
%%     ~~Describe \code{title.adj} here~~
}
  \item{title.cex}{
%%     ~~Describe \code{title.cex} here~~
}
  \item{box.scale}{
%%     ~~Describe \code{box.scale} here~~
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
function (z, col, x, y = NULL, size = NULL, digits = 2, labels = c("breaks", 
    "ranges"), values = TRUE, breaks = NULL, label.text = NULL, 
    title.add = TRUE, title.text = NULL, title.adj = c(0.5, 0), 
    title.cex = 0.8, box.scale = FALSE) 
{
    n <- length(col)
    usr <- par("usr")
    mx <- mean(usr[1:2])
    my <- mean(usr[3:4])
    dx <- diff(usr[1:2])
    dy <- diff(usr[3:4])
    if (missing(x)) 
        x <- mx + 1.05 * dx/2
    else if (is.list(x)) {
        if (length(x$x) == 2) 
            size <- c(diff(x$x), -diff(x$y)/n)
        y <- x$y[1]
        x <- x$x[1]
    }
    else x <- x[1]
    if (is.null(size)) 
        if (is.null(y)) {
            size <- 0.618 * dy/n
            y <- my + 0.618 * dy/2
        }
        else size <- (y - my) * 2/n
    if (length(size) == 1) 
        size <- rep(size, 2)
    if (is.null(y)) 
        y <- my + n * size[2]/2
    i <- seq(along = col)
    if (box.scale == TRUE) {
        rect(x, y - i * size[2], x + size[1], y - (i - 1) * size[2], 
            col = rev(col), xpd = TRUE, lty = 1)
    }
    else {
        rect(x, y - i * size[2], x + size[1], y - (i - 1) * size[2], 
            col = rev(col), xpd = TRUE, lty = 0)
        rect(x, y - max(i) * size[2], x + size[1], y, lty = 1)
    }
    if (!values) {
        if (is.null(breaks)) {
            stop("values=FALSE but no breaks supplied")
        }
        if (!(length(breaks) == (n + 1))) {
            stop("Specified number of breaks does not match n+1 specified colours")
        }
        if (any(diff(breaks) < 0)) {
            stop("breaks are not a vector of increasing numbers")
        }
        bks <- breaks
    }
    else {
        rng <- range(z, na.rm = TRUE)
        bks <- seq(from = rng[2], to = rng[1], length = n + 1)
    }
    bks <- formatC(bks, format = "f", digits = digits)
    labels <- match.arg(labels)
    if (labels == "breaks") 
        ypts <- y - c(0, i) * size[2]
    else {
        bks <- paste(bks[-1], bks[-(n + 1)], sep = " - ")
        ypts <- y - (i - 0.5) * size[2]
    }
    if (is.null(label.text)) {
        label.text <- bks
    }
    else {
        label.text <- label.text
    }
    text(x = x + 1.4 * size[1], y = ypts, labels = label.text, 
        adj = ifelse(size[1] > 0, 0, 1), xpd = TRUE, cex = 0.7)
    if (title.add) {
        if (!is.null(title.text)) {
            TITLE <- title.text
        }
        else {
            TITLE <- "Key"
        }
        text(x - 0.5 * size[2], y + ((diff(i) * size[2])), labels = TITLE, 
            adj = title.adj, cex = title.cex, pos = 4, offset = 0.5)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
