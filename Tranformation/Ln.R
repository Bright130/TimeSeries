#SMA from Time series Douglas Page23
library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
#library(TTR)
sunspot <- read_csv("sunspot.csv")
#adjust zero value

geomean <- exp(mean(log(sunspot$`Yearly Sunspot Number`[is.finite(log(sunspot$`Yearly Sunspot Number`))]))) #geometric mean and avoid -inf
sunspot$ln = log(((sunspot$`Yearly Sunspot Number`^0.5)-1)/(0.5*(geomean^(0.5-1))))#power family at gramma = -0.5 (square root tranformation)
#tranformed
ggplot(sunspot,aes(Year))+geom_line(aes(y=sunspot$ln),color="black")+geom_point(aes(y=sunspot$ln),color="black")+labs(y="ln(Yearly Sunspot Number)",title="Total Sunspot Numbers (Annual)",subtitle="From Solar Influences Data Analysis Center",caption="source : sidc.be/silso/archiveyearlyssnplot")
#source
ggplot(sunspot,aes(Year))+geom_line(aes(y=sunspot$`Yearly Sunspot Number`),color="black")+geom_point(aes(y=sunspot$`Yearly Sunspot Number`),color="black")+labs(y="Yearly Sunspot Number",title="Total Sunspot Numbers (Annual)",subtitle="From Solar Influences Data Analysis Center",caption="source : sidc.be/silso/archiveyearlyssnplot")
print('Source data')
summary(sunspot$`Yearly Sunspot Number`)
sd(sunspot$`Yearly Sunspot Number`)
print('Transformed data')
summary(sunspot$ln)
sd(sunspot$ln)

#Transformed to source format
#sunspot$trans <- (exp(sunspot$SMAtrans)*(0.5*geomean^(0.5-1))+1)^(1/0.5)
#prove
fit = lm(sunspot$`Yearly Sunspot Number` ~sunspot$Year)
fit1 = lm(sunspot$ln ~sunspot$Year)
plot(fit)
plot(fit1)

anova(fit) #Test significant
anova(fit1)
