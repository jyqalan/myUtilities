match.up <-
function(Dx, Ix, Iy, nomatch=NA, incomparables=FALSE, zap.NAs=FALSE)
{
    # A function to match up values in one vector with those in
    # another using some common index

    if(!all(is.vector(Dx) || is.vector(Ix) || is.vector(Iy)))
    {
        stop("at least one of Dx, Ix, or Iy is not a vector")
    }

    if(!any(unique(Ix) %in% unique(Iy)))
    {
        stop("Ix and Iy contain no values in common")
    }

    if(length(Dx) != length(Ix))
    {
        stop("the length of your data vector (", deparse(substitute(Dx)), ") does not equal the length of the corresponding index (", deparse(substitute(Ix)), ")")
    }

    OUT <- match(as.character(Iy), as.character(Ix), nomatch=nomatch, incomparables=incomparables)
    Dy <- Dx[OUT]

    if(zap.NAs)
    {Dy[is.na(Dy)] <- rep(0, length(Dy[is.na(Dy)]))}

    return(Dy)
}
