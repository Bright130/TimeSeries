library(astsa)
library(ggplot2)
library(readr)
quake <- read_csv("quake.csv")
ggplot(quake,aes(Year))+geom_line(aes(y=quake$Quakes),color="black")+geom_point(aes(y=quake$Quakes),color="black")+labs(y="Number of quakes",title="The annual number of worldwide earthquakes with magnitude greater than 7",caption="source : earthquake.usgs.gov")
pacf(quake$Quakes,plot=TRUE)
actual <- quake
quake = ts(quake)
plot(quake, type="b")
xlag1=lag(quake,-1) # Creates a lag 1 of x variable. See note 2
xlag2=lag(quake,-2) # Creates a lag 1 of x variable. See note 2
xlag3=lag(quake,-3) # Creates a lag 1 of x variable. See note 2

y=cbind(quake,xlag1,xlag2,xlag3) # See note 3 below
y<-na.omit(y)
ar1fit=lm(y[,2]~y[,4]+y[,6]+y[,8])#Does regression, stores results object named ar1fit
arfit=lm(y[,2]~y[,4])#Does regression, stores results object named ar1fit
summary(ar1fit) # This lists the regression results
summary(arfit)
#plot(ar1fit$fit,ar1fit$residuals) #plot of residuals versus fits
#acf(ar1fit$residuals, xlim=c(1,18)) # ACF of the residuals for lags 1 to 18
anova(ar1fit)
anova(arfit)
actual <- actual[c(4:100),]
ggplot(actual,aes(Year))+geom_line(aes(y=actual$Quakes),color="black")+geom_point(aes(y=actual$Quakes),color="black")+geom_line(aes(y=ar1fit$fit),color="green")+geom_point(aes(y=ar1fit$fit),color="green")+geom_line(aes(y=arfit$fit),color="red")+geom_point(aes(y=arfit$fit),color="red")+labs(y="Number of quakes",title="The annual number of worldwide earthquakes with magnitude greater than 7",caption="source : earthquake.usgs.gov")
