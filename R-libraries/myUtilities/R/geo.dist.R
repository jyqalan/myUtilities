geo.dist <-
function(x, y, x2 = NULL, y2 = NULL, r = 1, f = 0)

# Function to calculate the angular or great-circle distance between pairs of
#   coordinates on the Earth's surface. Coordinates are entered in decimal
#   degrees.

# Usage

# dist.col(x, y, x2 = NULL, y2 = NULL, r = 1, f = 0)

# Arguments

#   x, y, x2, y2    either matrices, data frames or vectors of the sets of
#                   coordinates of pairs of points between which the angle or
#                   distance is calculated. The coordinates of each set of
#                   points is either in a data frame or matrix or in a pair of
#                   vectors. In a data frame or matrix either the longitudes and
#                   latitudes have column names "long" and "lat respectively, or
#                   the first first and second column are the longitudes and
#                   latitudes, respectively. See Details for usage.
#   r               Local radius of the Earth in appropriate units. Default is
#                   r = 1 in which case the angular distance in radians is
#                   returned. Equatorial radius of the Earth is r = 6378.137 km.
#                   At NZ latitudes the local radius is approx 6365 km.
#                   If f (!= 0) is specified (###TBD) then r must be the
#                   equatorial radius.
#   f               Flattening of the polar ellipse in the oblate spheroid model
#                   for the Earth. NOT IMPLEMENTED. Default is f = 0 which
#                   assumes a sphere. Actual flattening of the Earth is
#                   f = 0.00335281. f can also be negative (prolate sphere).
#                   In general the local radius at latitude theta is
#                   r/sqrt(1 + (2*f - f^2)*(sin(theta))^2/(1 - f)^2). CHECK!!!

# Details

# Function will handle any of the following command syntaxes in relation to the
# entry of the coordinates of the pairs of points:
#   1. geo.dist(mat1, mat2),
#   2. geo.dist(mat1, vec3, vec4),              ###TBD
#   3. geo.dist(mat1, vec3, y2 = vec4),         ###TBD
#   4. geo.dist(mat1, x2 = vec3, y2 = vec4),
#   5. geo.dist(vec1, vec2, vec3, vec4),
# where:
#   mat1 is a data frame or matrix with 1st set of coords,
#   mat1 is a data frame or matrix with 2nd set of coords,
#   vec1 and vec2 are vectors of 1st set of longs and lats,
#   vec3 and vec4 are vectors of 2nd set of longs and lats,

# Updates

#   11 Nov 2005 Redefined the roles of y, x1 and x2 in the case when x is a
#               matrix or data frame and the second set of coordinates is
#               specified by 2 vectors (now x2 and y2, but was y and x2).


{
    if (missing(y)) y <- NULL

    if (is.matrix(x) | is.data.frame(x)) {
        if (all(c("long", "lat") %in% colnames(x))) {
            long1 <- x[, "long"]
            lat1 <-  x[, "lat"]
        } else {
            long1 <- x[, 1]
            lat1 <-  x[, 2]
        }
        if (is.matrix(y) | is.data.frame(y)) {
            if (all(c("long", "lat") %in% colnames(y)))
            {
                long2 <- y[, "long"]
                lat2 <-  y[, "lat"]
            } else {
                long2 <- y[, 1]
                lat2 <-  y[, 2]
            }
        } else {
            if (!(is.vector(x2) & is.vector(y2)))
                stop("Coordinates of second points are not in vector, matrix or data frame format")
            long2 <- x2
            lat2 <-  y2
        }
    } else {
        if (!(is.vector(x) & is.vector(y) & is.vector(x2) & is.vector(y2)))
            stop("Coordinates of points are not in vector, matrix or data frame format")
        long1 <- x
        lat1 <-  y
        long2 <- x2
        lat2 <-  y2
    }

    llg1 <- length(long1)
    llg2 <- length(long2)
    if (llg1 != length(lat1))
        stop("vectors of latitude and longititude for first points are NOT equal")
    if (llg2 != length(lat2))
        stop("vectors of latitude and longititude for second points are NOT equal")

    if (llg2 < llg1) {
            # equalise lengths by recycling shortest
        l0 <- numeric(llg1)
        long2 <- long2 + l0
        lat2 <- lat2 + l0
        if(llg2 > 1)
            warning("second points are being recycled")
    }
    if (llg1 < llg2) {
            # equalise lengths by recycling shortest
        l0 <- numeric(llg2)
        long1 <- long1 + l0
        lat1 <- lat1 + l0
        if(llg1 > 1)
            warning("first points are being recycled")
    }
    if (any(abs(lat1)[!is.na(lat1)] >= 90))
        stop("Some latitudes for the first points are out of range")
    if (any(abs(lat2)[!is.na(lat2)] >= 90))
        stop("Some latitudes for the second points are out of range")

        # Convert degrees to radians
    long1 <- (long1 %% 360)*pi/180
    long2 <- (long2 %% 360)*pi/180
    lat1 <- lat1*pi/180
    lat2 <- lat2*pi/180

    if (abs(f) > 1.0e-6) { ###TBD
        warning(paste("Oblate spheroid Earth not implemented,",
            "assuming Earth is a sphere"))
    }

    return(r*acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(long1 - long2)))

#    V1 <- cbind(cos(lat1)*cos(long1), cos(lat1)*sin(long1), sin(lat1))
#    V2 <- cbind(cos(lat2)*cos(long2), cos(lat2)*sin(long2), sin(lat2))
#    return(r*acos(apply(V1*V2, 1, sum)))
}
