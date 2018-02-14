#SMA from Time series Douglas Page23
library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(TTR)
NasaGiss <- read_csv("NasaGiss.csv")
#blue
NasaGiss <- NasaGiss %>% mutate(Avg = (Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec)/12.0) %>% select(Year,Avg)
#black
NasaGiss$SMA <- SMA(NasaGiss$Avg,5) #TTR lib
ggplot(NasaGiss,aes(Year))+geom_line(aes(y=NasaGiss$Avg),color="blue")+geom_point(aes(y=NasaGiss$Avg),color="blue",shape=4)+ylim(-0.75,1)+geom_line(aes(y=NasaGiss$SMA),color="black")+geom_point(aes(y=NasaGiss$SMA),color="black")+labs(y="Average Annual Anomaly (oC)",title="Global Mean Estimates Based on Land-Surface Air Temperature Anomalies Only",subtitle="Global-mean monthly, seasonal, and annual means, 1880-2014",caption="source : data.giss.nasa.gov/gistemp")
