image.scale <-
function (z, col, x, y = NULL, size = NULL, digits = 2, labels = c("breaks", "ranges"),values=TRUE, breaks=NULL, label.text=NULL, title.add=TRUE,title.text=NULL,title.adj=c(0.5,0),title.cex=0.8,box.scale=FALSE)
{
    # Credit to Jonathan Rougier (J.C.Rougier@durham.ac.uk)
    # Post to R-Help mailing list Tue, 21 Sep 1999 08:56:30 +0100 (BST)

    # MJM 1 Oct 2004: Added arguments "values" and "breaks" : if "values"=TRUE, use equidistant points from min(z) to max(z) where n=length(col), otherwise a specified vector of "breaks"\
    # MJM 4 Oct 2004: Added "add.title" and "title.text"
    # MJM 2006-08-30: Added bks as an argument and (slightly) restructured the labelling code to allow user-specified break labels

    # --------------------------------------------------------------------------------
    # sort out the location
    n <- length(col)
    usr <- par("usr")
    mx <- mean(usr[1:2]); my <- mean(usr[3:4])
    dx <- diff(usr[1:2]); dy <- diff(usr[3:4])
    if (missing(x))
        x <- mx + 1.05*dx/2 # default x to right of image
    else if (is.list(x)) {
        if (length(x$x) == 2)
          size <- c(diff(x$x), -diff(x$y)/n)
        y <- x$y[1]
        x <- x$x[1]
    } else x <- x[1]
    if (is.null(size))
        if (is.null(y)) {
          size <- 0.618*dy/n # default size, golden ratio
          y <- my + 0.618*dy/2 # default y to give centred scale
        } else size <- (y-my)*2/n
    if (length(size)==1)
        size <- rep(size, 2) # default square boxes
    if (is.null(y))
        y <- my + n*size[2]/2

    # --------------------------------------------------------------------------------
    # draw the image scale
    i <- seq(along = col)
    if(box.scale==TRUE)
    {
        rect(x, y - i * size[2], x + size[1], y - (i - 1) * size[2],col = rev(col), xpd = TRUE, lty=1)
        #rect(x, y - i * size[2], x + size[1], y - (i - 1) * size[2],col = col, xpd = TRUE, lty=1)

    }
    else
    {
        rect(x, y - i * size[2], x + size[1], y - (i - 1) * size[2],col = rev(col), xpd = TRUE, lty=0)
        #rect(x, y - i * size[2], x + size[1], y - (i - 1) * size[2],col = col, xpd = TRUE, lty=0)
        rect(x, y-max(i)*size[2], x+size[1], y, lty=1)
    }


    # --------------------------------------------------------------------------------
    # Sort out the label positions etc
    if(!values)
    {
        if(is.null(breaks))
        {stop("values=FALSE but no breaks supplied")}

        if(!(length(breaks)==(n+1)))
        {stop("Specified number of breaks does not match n+1 specified colours")}

        if(any(diff(breaks) < 0))
        {stop("breaks are not a vector of increasing numbers")}

        bks <- breaks

    }
    else
    {
        rng <- range(z, na.rm = TRUE)
        bks <- seq(from = rng[2], to = rng[1], length = n + 1)
    }

    bks <- formatC(bks, format="f", digits=digits)

    labels <- match.arg(labels)
    if (labels == "breaks")
        ypts <- y - c(0, i) * size[2]
    else {
        bks <- paste(bks[-1], bks[-(n+1)], sep = " - ")
        ypts <- y - (i - 0.5) * size[2]
    }

    # --------------------------------------------------------------------------------
    # Now sort out what label strings we will use
    # SB reversed order so 0 at bottom

    if(is.null(label.text))
    {

        # If the user has not specified a vector of scale labels,
        # we'll use the (possibly nicely prettified) calculated bks

        #label.text <- rev(bks)
        label.text <- bks
    }
    else
    {
        # Otherwise, we'll use what the user has specified
        #label.text <- rev(label.text)
        label.text <- label.text
    }

    text(x = x + 1.4 * size[1], y = ypts, labels = label.text, adj = ifelse(size[1]>0, 0, 1), xpd = TRUE,cex=0.7)
    # SB made text smaller and further away from legend box
    #text(x = x +3.5 * size[1], y = ypts, labels = label.text, adj = 1, xpd = TRUE,cex=0.8)

    # --------------------------------------------------------------------------------
    # Add title (requested by default)
    if(title.add)
    {
        if(!is.null(title.text))
        {TITLE <- title.text}
        else
        {TITLE <- "Key"}

        text(x-0.5*size[2],y+((diff(i) * size[2])),labels=TITLE,adj=title.adj,cex=title.cex,pos=4,offset=0.5)
        #text(x,y+(0.5*(diff(i) * size[2])),labels=TITLE,adj=title.adj,cex=0.8,pos=4,offset=0.5)#<-- Position the title half a rect higher than the rest...


    }
}
