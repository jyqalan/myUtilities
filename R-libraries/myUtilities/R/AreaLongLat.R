AreaLongLat <-
function(Long,Lat)
{
# find area of polygon of lat,long in decimal form (eg 174.5 = 174deg. 30 min)
# km^2
# if only Long then assume a list (x,y)
#
# eg  for SurveyBoundry we get 646.7414  (cf 647.3 for rstn on frc)
#            (BonNonSub()  2.6% too high? - some small errors?)
#  x<-matrix(scan("SurveyBoundry"),ncol=4,byrow=T)
#  AreaLongLat(x[,3]+x[,4]/60,x[,1]+x[,2]/60)
if(missing(Lat))Poly<-Long else Poly<-list(x=Long,y=Lat)
# convert to Km
Tna<-!is.na(Poly$x)
Poly$x[Tna]<-Poly$x[Tna]* (111.41*cos(pi/180*Poly$y[Tna])-
 0.01*cos(3*pi/180*Poly$y[Tna]))
Tna<-!is.na(Poly$y)
Poly$y[Tna]<-Poly$y[Tna]* (111.14-
 0.56*cos(2*pi/180*Poly$y[Tna]))
return(Area(Poly))
}
