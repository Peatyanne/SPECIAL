## Savitzky Golay Filter

library(tidyverse)
library(signal)
library(lubridate)

df <- read_csv("~/reading/R_work/Data/WET_5sites_inputs/fapar_CZ-wet_day.csv", col_types = cols(
  date = col_date(format="%d/%m/%Y"),
  fapar = col_double()))


##FOR LOESS: as in ingestr ################

idxs    <- which(!is.na(df$fapar))
ndayyear = ifelse( leap_year( df$date ), 366, 365)
year_dec = year(df$date) + (yday(df$date)-1) / ndayyear 
myloess <- try( with( df, loess(df$fapar[idxs] ~ year_dec[idxs], span=0.1 ) ))
i <- 0
while (class(myloess)=="try-error" && i<50){
  i <- i + 1
  # print(paste("i=",i))
  myloess <- try( with( df, loess( df$fapar[idxs] ~ year_dec[idxs], span=(0.1+0.02*(i-1)) ) ))
}

## predict LOESS
tmp <- try( with( df, predict( myloess, year_dec ) ) )
if (class(tmp)!="try-error"){
  loess <- tmp
} else {
  loess <- rep( NA, nrow(ddf) )
}
Loess<- as.data.frame(loess)
write.csv(Loess,file="~/reading/R_work/Data/WET_5sites_inputs/fapar_CZ-wet_Loess.csv", row.names=FALSE)

##FOR SPLINE: as in ingestr#################

idxs   <- which(!is.na(df$fapar))
spline <- try( with( df, smooth.spline( year_dec[idxs], df$fapar[idxs], spar=0.01 ) ) )

## predict SPLINE
tmp <- try( with( df, predict( spline, year_dec ) )$y)
if (class(tmp)!="try-error"){
  spline <- tmp
} else {
  spline <- rep( NA, nrow(df) )
}

Spline<- as.data.frame(spline)
write.csv(Spline,file="~/reading/R_work/Data/WET_5sites_inputs/fapar_CZ-wet_Spline.csv", row.names=FALSE)


##FOR SG FILTER: as in ingestr ###############

sgfilter <- rep( NA, nrow(df) )
idxs <- which(!is.na(df$fapar))
tmp <- try(signal::sgolayfilt( df$fapar[idxs], p=3, n=101 ))
if (class(tmp)!="try-error"){
  sgfilter[idxs] <- tmp
}

Sgfilter<- as.data.frame(sgfilter)
write.csv(Sgfilter,file="~/reading/R_work/Data/WET_5sites_inputs/fapar_CZ-wet_SGfilterp3n101.csv", row.names=FALSE)



##previous SG expriment #############

RUChe_SGfilt<- sgolayfilt(df$fapar, p = 3, n = 11 , m = 0, ts = 1)

RUChe_SGfilt<- as.data.frame(RUChe_SGfilt)

plot(df$date, df$fapar, type- l, lwd=3, col="grey", ann=FALSE, las=2)
par(new=TRUE)
plot(RUChe_p3n11, type= "l", ann=FALSE, axes=FALSE,col='blue')



write.csv(RUChe_SGfilt, file="~/reading/R_work/Data/Fapar_MODIS/fapar_RU-Che_SGfilt2_101.csv")