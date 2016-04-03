tighten <-
function(data.frame)
{
#return a data frame where all factors (ordered) have been refactor to take out redundant levels
#

vars <- seq(length=length(data.frame))
for(j in vars){
  x <- data.frame[[j]]
  if(is.factor(x)){
    if(any(class(x)=="ordered"))
      data.frame[[j]] <- ordered(x)
    else
      data.frame[[j]] <- factor(x)
  }
}
data.frame
  
}
