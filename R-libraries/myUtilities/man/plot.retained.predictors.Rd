\name{plot.retained.predictors}
\alias{plot.retained.predictors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.retained.predictors(object, labels = NULL, log.scale = TRUE, plotvalues = FALSE, se.fit = TRUE, sort.vars = TRUE, do.layout = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{log.scale}{
%%     ~~Describe \code{log.scale} here~~
}
  \item{plotvalues}{
%%     ~~Describe \code{plotvalues} here~~
}
  \item{se.fit}{
%%     ~~Describe \code{se.fit} here~~
}
  \item{sort.vars}{
%%     ~~Describe \code{sort.vars} here~~
}
  \item{do.layout}{
%%     ~~Describe \code{do.layout} here~~
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
function (object, labels = NULL, log.scale = TRUE, plotvalues = FALSE, 
    se.fit = TRUE, sort.vars = TRUE, do.layout = TRUE) 
{
    DATA <- object$data
    ATTR <- attributes(object$terms)$term.labels
    if (length(grep(":", ATTR)) > 0) {
        ATTRnoI <- ATTR[-grep(":", ATTR)]
    }
    else {
        ATTRnoI <- ATTR
    }
    DRAW <- names(unlist(sapply(names(DATA), grep, ATTRnoI)))
    if (sort.vars) {
        DRAW <- sort(DRAW)
    }
    N <- length(DRAW)
    newN <- if (((N/2) - floor(N/2)) > 0) {
        N + 1
    }
    else {
        N
    }
    matN <- matrix(1:newN, ncol = 2, byrow = TRUE)
    if (do.layout) {
        par(mai = par("mai")/2)
        par(oma = c(3, 3, 1, 1))
        layout(matN)
    }
    if (is.null(labels)) {
        labels <- DRAW
    }
    if (!is.null(labels)) {
        if (length(labels) != length(DRAW)) {
            stop("The length of your labels vector does not equal the number of retained predictors in your model")
        }
    }
    print(data.frame(Retained.variables = DRAW, Axis.labels = labels))
    cat("\n")
    for (i in 1:length(DRAW)) {
        PRETTY <- if (is.factor(DATA[[DRAW[i]]])) {
            levels(DATA[[DRAW[i]]])
        }
        else {
            pretty(DATA[[DRAW[i]]])
        }
        plot.type <- if (is.factor(DATA[[DRAW[i]]])) {
            "p"
        }
        else {
            "l"
        }
        axis.at <- if (is.factor(DATA[[DRAW[i]]])) {
            if (DRAW[i] == "vessel_key") {
                if (length(levels(DATA[[DRAW[i]]])) <= 10) 
                  1:length(PRETTY)
                else seq(1, length(levels(DATA[[DRAW[i]]])), 
                  5)
            }
            else {
                1:length(PRETTY)
            }
        }
        else {
            PRETTY
        }
        axis.las <- if (is.factor(DATA[[DRAW[i]]])) {
            3
        }
        else {
            1
        }
        axis.labels <- if (is.factor(DATA[[DRAW[i]]])) {
            PRETTY[axis.at]
        }
        else {
            PRETTY
        }
        this.label <- labels[i]
        ans <- plot.predictCPUE(DRAW[i], DATA, object, log.scale = log.scale, 
            plotvalues = plotvales, se.fit = se.fit, type = plot.type, 
            xlab = "", ylab = "", axis.labels = FALSE)
        axis(side = 1, at = axis.at, labels = axis.labels, las = axis.las)
        tmp.x <- if (plot.type == "l") {
            as.numeric(names(ans$fit))
        }
        else {
            1:length(ans$fit)
        }
        text(tmp.x[1], ceiling(max(ans$fit + 2 * ans$se.fit, 
            na.rm = TRUE)), this.label, font = 2, adj = c(0, 
            1))
    }
    mtext(side = 2, "Expected catch rate", outer = TRUE, line = 1)
    mtext(side = 1, "Levels or values of retained predictor variables", 
        outer = TRUE, line = 1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
