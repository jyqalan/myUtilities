distance.lat.long <-
function(x1, y1, x2, y2, units="metres")
  {
    # A function to return the distance in either KMs or Ms separating two points on the Earth's surface
    # i.e. the start and end points of a tow or set...

    # Uses the simple spherical model

    # Points MUST be decimalised degrees

    radians <- function(xin)
      {
        tmp.rad <- (pi*xin)/180
        return(tmp.rad)
      }

    
    if(units=="metres")
      {
        r <- 6378388
       }
    else if(units=="km")
      {
        r <- 6378.388
      }
    else if(is.numeric(units)==TRUE)
      {
        r <- units
      }
    else
      {
        stop("Not a valid unit type...")
      }

    tmp.trig <- (sin(radians(y1)) * sin(radians(y2))) + (cos(radians(y1)) * cos(radians(y2)) * cos(radians(x1) - radians(x2)))
      
    tmp.d <- r * acos(tmp.trig)

    return(tmp.d)
  }
