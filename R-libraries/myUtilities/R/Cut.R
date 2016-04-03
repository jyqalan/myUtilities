Cut <-
function (x, breaks, labels = NULL,  include.lowest = FALSE, right = TRUE,  dig.lab = 3, ...) 
{
    # Same as cut but using midpoints as labels
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    if (length(breaks) == 1) {
        if (is.na(breaks) | breaks < 2) 
            stop("invalid number of intervals")
        nb <- as.integer(breaks + 1)
        dx <- diff(rx <- range(x, na.rm = TRUE))
        if (dx == 0) 
            dx <- rx[1]
        breaks <- seq(rx[1] - dx/1000, rx[2] + dx/1000, len = nb)
    }
    else nb <- length(breaks <- sort(breaks))
    if (any(duplicated(breaks))) 
        stop("'breaks' are not unique")
    codes.only <- FALSE
    if (is.null(labels)) {
        for (dig in dig.lab:max(12, dig.lab)) {
            ch.br <- formatC(breaks, digits = dig, wid = 1)
            if (ok <- all(ch.br[-1] != ch.br[-nb])) 
                break
        }
        labels <- if (ok) 
            (breaks[-1]+breaks[-nb])/2
        else (1:(nb - 1)+2:nb)/2
    }
    else if (is.logical(labels) && !labels) 
        codes.only <- TRUE
    else if (length(labels) != nb - 1) 
        stop("labels/breaks length conflict")
    #code <- .C("bincode", x = as.double(x), n = as.integer(length(x)), 
    #    breaks = as.double(breaks), as.integer(nb), code = integer(length(x)), 
    #    right = as.logical(right), include = as.logical(include.lowest), 
    #    naok = TRUE, NAOK = TRUE, DUP = FALSE, PACKAGE = "base")$code
    code <- .bincode(x = as.double(x), breaks = as.double(breaks), right = as.logical(right), include.lowest = as.logical(include.lowest))
    if (codes.only) 
        code
    else factor(code, seq(labels), labels)
}
