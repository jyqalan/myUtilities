r.squared <-
function(GLM,decimals=3)
{
    # A function to extract the r.squared value from a fitted glm object
    RES <- round((GLM$null.deviance-GLM$deviance)/GLM$null.deviance,decimals)
    return(RES)
}
