## Savitzky Golay Filter

library(tidyverse)
library(signal)

df<- read.csv(file="~/reading/R_work/Data/Fapar_MODIS/fapar_RU-Che_Day.csv")

RUChe_SGfilt<- sgolayfilt(df$fapar, p = 2, n = 101 , m = 0, ts = 1)

RUChe_SGfilt<- as.data.frame(RUChe_SGfilt)



plot(df$date, df$fapar, type- l, lwd=3, col="grey", ann=FALSE, las=2)
par(new=TRUE)
plot(RUChe_p3n11, type= "l", ann=FALSE, axes=FALSE,col='blue')



write.csv(RUChe_SGfilt, file="~/reading/R_work/Data/Fapar_MODIS/fapar_RU-Che_SGfilt2_101.csv")