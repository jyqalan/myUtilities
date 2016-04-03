\name{stepCPUE}
\alias{stepCPUE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stepCPUE(object, scope, r2.change = 0.005, scale = 0, direction = c("both", "backward", "forward"), trace = 1, keep = deviance, steps = 1000, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{scope}{
%%     ~~Describe \code{scope} here~~
}
  \item{r2.change}{
%%     ~~Describe \code{r2.change} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
  \item{direction}{
%%     ~~Describe \code{direction} here~~
}
  \item{trace}{
%%     ~~Describe \code{trace} here~~
}
  \item{keep}{
%%     ~~Describe \code{keep} here~~
}
  \item{steps}{
%%     ~~Describe \code{steps} here~~
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
function (object, scope, r2.change = 0.005, scale = 0, direction = c("both", 
    "backward", "forward"), trace = 1, keep = deviance, steps = 1000, 
    ...) 
{
    cut.string <- function(string) {
        if (length(string) > 1) 
            string[-1] <- paste("\n", string[-1], sep = "")
        string
    }
    AIC.drop1 <- function(fit, Terms, scale, trace, ...) {
        n <- length(Terms)
        ans <- matrix(nrow = n + 1, ncol = 2)
        dimnames(ans) <- list(c("<none>", paste("-", Terms, sep = "")), 
            c("df", "AIC"))
        ans[1, ] <- extractCPUE(fit, scale, ...)
        if (n == 0) 
            return(ans)
        i <- 2
        for (tt in Terms) {
            if (trace > 1) 
                cat("trying -", tt, "\n")
            else cat(".")
            nfit <- update(fit, paste("~ . -", tt))
            ans[i, ] <- extractCPUE(nfit, scale, ...)
            if (trace > 2) 
                print(ans[i, ])
            i <- i + 1
        }
        ans
    }
    AIC.add1 <- function(fit, Terms, scale, trace, screen, ...) {
        n <- length(Terms)
        ans <- matrix(nrow = n + 1, ncol = 2)
        t2 <- if (length(Terms)) 
            paste("+", Terms, sep = "")
        else NULL
        dimnames(ans) <- list(c("<none>", t2), c("df", "AIC"))
        ans[1, ] <- extractCPUE(fit, scale, ...)
        if (n == 0) 
            return(ans)
        i <- 2
        for (tt in Terms) {
            if (trace > 1) 
                cat("trying +", tt, "\n")
            else cat(".")
            nfit <- update(fit, paste("~ . +", tt))
            ans[i, ] <- extractCPUE(nfit, scale, ...)
            if (trace > 2) 
                print(ans[i, ])
            i <- i + 1
        }
        ans
    }
    re.arrange <- function(keep) {
        namr <- names(k1 <- keep[[1]])
        namc <- names(keep)
        nc <- length(keep)
        nr <- length(k1)
        array(unlist(keep, recursive = F), c(nr, nc), list(namr, 
            namc))
    }
    make.step <- function(models, fit, object) {
        change <- sapply(models, "[[", "change")
        rd <- sapply(models, "[[", "deviance")
        dd <- c(NA, diff(rd))
        rdf <- sapply(models, "[[", "df.resid")
        ddf <- c(NA, diff(rdf))
        AIC <- sapply(models, "[[", "AIC")
        print(AIC)
        aic <- sapply(models, "[[", "aic")
        heading <- c("Stepwise Model Path \nAnalysis of Deviance Table", 
            "\nInitial Model:", deparse(as.vector(formula(object))), 
            "\nFinal Model:", deparse(as.vector(formula(fit))), 
            "\n")
        aod <- data.frame(Step = change, Df = ddf, Deviance = dd, 
            `Resid. Df` = rdf, `Resid. Dev` = rd, r.squared = AIC, 
            aic = aic, check.names = F)
        attr(aod, "heading") <- heading
        attr(aod, "class") <- c("anova", "data.frame")
        fit$anova <- aod
        fit
    }
    if (missing(direction)) 
        direction <- "both"
    else direction <- match.arg(direction)
    backward <- direction == "both" | direction == "backward"
    forward <- direction == "both" | direction == "forward"
    if (missing(scope)) {
        fdrop <- numeric(0)
        fadd <- NULL
    }
    else {
        if (is.list(scope)) {
            fdrop <- if (!is.null(fdrop <- scope$lower)) 
                attr(terms(update.formula(object, fdrop)), "factor")
            else numeric(0)
            fadd <- if (!is.null(fadd <- scope$upper)) 
                attr(terms(update.formula(object, fadd)), "factor")
        }
        else {
            fadd <- if (!is.null(fadd <- scope)) 
                attr(terms(update.formula(object, scope)), "factor")
            fdrop <- numeric(0)
        }
    }
    if (is.null(fadd)) {
        backward <- T
        forward <- F
    }
    models <- vector("list", steps)
    if (!is.null(keep)) {
        keep.list <- vector("list", steps)
        nv <- 1
    }
    n <- length(object$residuals)
    fit <- object
    cf <- attributes(coef(object))
    if (!is.null(cf$singular) && cf$singular > 0) {
        TT <- !match(TL <- attr(object$terms, "term.labels"), 
            names(cf$assign), F)
        if (any(TT)) {
            upd <- eval(parse(text = paste(c(".~.", TL[TT]), 
                collapse = "-")))
            fit <- update(fit, upd)
        }
    }
    bAIC <- extractCPUE(fit, scale, ...)
    edf <- bAIC[1]
    bAIC <- bAIC[2]
    aic <- AIC(fit)
    nm <- 1
    Terms <- fit$terms
    cat("\n\nTesting for a change in r.squared of less than ", 
        format(round(r2.change, 4)), "\n")
    models[[nm]] <- list(deviance = deviance(fit), df.resid = n - 
        edf, change = "", AIC = bAIC, aic = aic)
    if (!is.null(keep)) 
        keep.list[[nm]] <- keep(fit, bAIC)
    AIC <- 0
    count.steps <- 0
    while (steps > 0) {
        steps <- steps - 1
        AIC <- bAIC
        bfit <- fit
        ffac <- attr(Terms, "factor")
        scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
        aod.drop <- NULL
        aod.add <- NULL
        aod <- NULL
        change <- NULL
        if (backward && (ndrop <- length(scope$drop))) {
            aod.drop <- AIC.drop1(fit, scope$drop, scale = scale, 
                trace = trace, ...)
        }
        if (forward && (nadd <- length(scope$add))) {
            aod.add <- AIC.add1(fit, scope$add, scale = scale, 
                trace = trace, screen = screen, ...)
        }
        if (is.null(aod.drop) & is.null(aod.add)) 
            break
        if (!is.null(aod.drop) && nrow(aod.drop) > 1) {
            o <- rev(order(aod.drop[, "AIC"]))
            if ((aod.drop[1, "AIC"] - aod.drop[o[2], "AIC"]) < 
                r2.change) {
                change <- dimnames(aod.drop)[[1]][o[2]]
                cat(paste("\nDrop term", change, "\n"))
                if (is.null(aod.add)) {
                  aod <- aod.drop
                }
                else {
                  aod <- rbind(aod.drop, aod.add[-1, ])
                }
            }
        }
        if (is.null(aod)) {
            o <- order(-aod.add[, "AIC"])
            if ((aod.add[o[1], "AIC"] - aod.add[1, "AIC"]) >= 
                r2.change) {
                change <- dimnames(aod.add)[[1]][o[1]]
                cat(paste("\nAdd term", change, "\n"))
                if (is.null(aod.drop)) {
                  aod <- aod.add
                }
                else {
                  aod <- rbind(aod.drop, aod.add[-1, ])
                }
            }
        }
        if (is.null(aod)) {
            if (is.null(aod.drop)) 
                aod <- aod.add
            else if (is.null(aod.add)) 
                aod <- aod.drop
            else aod <- rbind(aod.add, aod.drop[-1, ])
            cat("\n")
            print(data.frame(df = aod[, 1], r.squared = aod[, 
                2]))
            cat("\n")
            break
        }
        if (trace > 1) {
            print(data.frame(df = aod[, 1], r.squared = aod[, 
                2]))
        }
        fit <- update(fit, eval(parse(text = paste("~ .", change))))
        Terms <- fit$terms
        bAIC <- extractCPUE(fit, scale, ...)
        edf <- bAIC[1]
        aic <- bAIC[3]
        bAIC <- bAIC[2]
        aic <- AIC(fit)
        nm <- nm + 1
        edf <- models[[nm]] <- list(deviance = fit$deviance, 
            df.resid = n - edf, change = change, AIC = bAIC, 
            aic = aic)
        if (!is.null(keep)) 
            keep.list[[nm]] <- keep(fit, bAIC)
    }
    if (!is.null(keep)) 
        fit$keep <- re.arrange(keep.list[seq(nm)])
    fit <- make.step(models = models[seq(nm)], fit, object)
    print(fit$anova)
    fit
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
