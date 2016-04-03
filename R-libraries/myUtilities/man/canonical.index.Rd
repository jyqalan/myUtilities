\name{canonical.index}
\alias{canonical.index}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
canonical.index(GLM, year, base = BASEYR, year.name = "year", plots = F, diags = F, line.type = "o", pch = 19, xlim = NULL, ylim = NULL, rescale = FALSE, add = FALSE, one.line = TRUE, one.line.lty = 2, one.line.col = "black", xlab = "Fishing year", ylab = "Index", digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GLM}{
%%     ~~Describe \code{GLM} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
}
  \item{base}{
%%     ~~Describe \code{base} here~~
}
  \item{year.name}{
%%     ~~Describe \code{year.name} here~~
}
  \item{plots}{
%%     ~~Describe \code{plots} here~~
}
  \item{diags}{
%%     ~~Describe \code{diags} here~~
}
  \item{line.type}{
%%     ~~Describe \code{line.type} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{rescale}{
%%     ~~Describe \code{rescale} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{one.line}{
%%     ~~Describe \code{one.line} here~~
}
  \item{one.line.lty}{
%%     ~~Describe \code{one.line.lty} here~~
}
  \item{one.line.col}{
%%     ~~Describe \code{one.line.col} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
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
function (GLM, year, base = BASEYR, year.name = "year", plots = F, 
    diags = F, line.type = "o", pch = 19, xlim = NULL, ylim = NULL, 
    rescale = FALSE, add = FALSE, one.line = TRUE, one.line.lty = 2, 
    one.line.col = "black", xlab = "Fishing year", ylab = "Index", 
    digits = 4) 
{
    res <- as.data.frame(summary(GLM)$coefficients)
    index <- regexpr(year.name, row.names(res)) > 0
    X <- res[index, 1]
    V <- summary(GLM)$cov.unscaled[index, ][, index]
    n <- length(X) + 1
    A <- matrix(-1/n, n, n - 1)
    A[-base, ] <- A[-base, ] + diag(rep(1, n - 1))
    CPUE <- A \%*\% X
    COV <- sqrt(diag((A \%*\% V) \%*\% t(A)))
    index <- exp(CPUE)
    lower.CI <- exp(CPUE - 2 * COV)
    upper.CI <- exp(CPUE + 2 * COV)
    ans <- data.frame(year, index, lower.CI, upper.CI)
    RSQ <- round((GLM$null.deviance - GLM$deviance)/GLM$null.deviance, 
        digits = digits)
    if (is.null(ylim)) {
        ylim <- c(0, ceiling(max(pretty(ans$index))))
    }
    else {
        ylim <- ylim
    }
    if (plots) {
        if (rescale == TRUE) {
            MEAN <- mean(ans$index)
            INDEX <- ans$index/MEAN
            CI.L <- ans$lower.CI/MEAN
            CI.U <- ans$upper.CI/MEAN
        }
        else {
            INDEX <- ans$index
            CI.L <- ans$lower.CI
            CI.U <- ans$upper.CI
        }
        if (!add) {
            plot(ans$year, INDEX, ylim = ylim, xlim = xlim, type = "n", 
                xaxs = "i", yaxs = "i", xlab = xlab, ylab = ylab)
            if (one.line) {
                abline(h = 1, lty = one.line.lty, col = one.line.col)
            }
        }
        points(ans$year, INDEX, pch = pch, type = line.type, 
            bg = one.line.col)
        segments(ans$year, CI.L, ans$year, CI.U)
        if (diags) {
            qqnorm(GLM$residuals)
            qqline(GLM$residuals)
        }
    }
    ans$se <- COV
    ans$cv <- sqrt(exp(COV^2) - 1)
    ans <- list(index = ans, R_squared = RSQ)
    .ans <<- ans
    return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
