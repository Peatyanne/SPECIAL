#BUILD INPUT DATA FRAME FOR 1 SITE


#Extract Fapar values from MERIS 10-day files  (from Shirley)

####!!!! change site name and code:

fapar_site <- read.csv("~/reading/R_work/Data/WET_5sites_inputs/fapar_RU-Che_4day.csv")                        
fapar_time <- fapar_site[,1]
fapar_site <- fapar_site[,44] #select column r5_c5 which is the closest point to the center
# one of the following to build a matrix (not sure which one, to be tested):
fapar_time<- as.data.frame(fapar_time)
fapar_site<- as.data.frame(fapar_site)
fapar_mat<- data.frame(fapar_time, fapar_site)

#fapar_mat <- matrix(c(fapar_time,fapar_site),nrow=length(fapar_time))
names(fapar_mat) <- c("Date", "fapar")

# Interpolate 10-day MERIS into daily values

####!!!! change site name:
write.csv(fapar_mat,file="~/reading/R_work/Data/WET_5sites_inputs/fapar_RU-Che_4day.csv", row.names=FALSE) 

####!!!! change site name:
fapar_site <- read_csv("~/reading/R_work/Data/WET_5sites_inputs/fapar_CZ-wet_4day.csv", col_types = cols(    
  date = col_date(format="%d/%m/%Y"),
  fapar = col_double())) 
fapar_site2<- fapar_site 


for(j in 3:nrow(fapar_site2)) {if (fapar_site2[j,2]>1.1) {fapar_site2[(j-2):(j-1),2]=1.01}} 
for(j in 3:nrow(fapar_site2)) {if (fapar_site2[j,2]>1.1) {fapar_site2[(j+1):(j+2),2]=1.01}}
#for(j in 1:nrow(fapar_site2)) {if (fapar_site2[j,2]>1.2) {fapar_site2[(j+1):(j+2),2]=9999}}

fapar_site_NA<- fapar_site2[1:(nrow(fapar_site)),] 

fapar_site_NA$fapar<- ifelse(fapar_site_NA$fapar>1, NA, fapar_site_NA$fapar)

library(zoo)
#zd<- read.zoo(fapar_Ivo, header=FALSE) 
zd<- read.zoo(fapar_site_NA, header=TRUE, drop=FALSE) 
tt <- as.Date(seq(start(zd), end(zd), "day"))  
zm <- na.approx(zd, as.Date, xout = tt, maxgap=Inf, na.rm=FALSE)

##zm <- na.spline(zd, as.Date, xout = tt)
##zm <- na.spline(zd, as.Date, xout = tt, maxgap=2, na.rm=FALSE)

fapar_siteDay<- fortify.zoo(zm, name="date")
names(fapar_siteDay)<- c("date", "fapar")

####!!!! change site name:
write.csv(fapar_siteDay,file="~/reading/R_work/Data/WET_5sites_inputs/fapar_CZ-wet_day.csv", row.names=FALSE) 

##########################################

# select month in tempstamp and calculate monthly mean fapar from 10-day data set
library(tidyverse)
fapar_Ivo <- fapar_Ivomat

fapar_Ivo <- as.data.frame(fapar_Ivo)
fapar_Ivo <- mutate(fapar_Ivo,month= floor(V1/100))
faparIvo <- summarise(group_by(fapar_Ivo,month),monthmean= mean(V2))
write.csv(faparIvo,file="faparIvo", row.names=FALSE)

#Extract column needed to run p-model from Fluxnet-FULLSET data
# following done using Files and right click import :
library (tidyverse)
FLX_US_Ivo_FLUXNET2015_FULLSET_MM_2004_2007_1_4 <- read_csv("Data/Fluxnet/US-Ivo/FLX_US-Ivo_FLUXNET2015_FULLSET_MM_2004-2007_1-4.csv")
Flux_Ivo_all <- as.data.frame(FLX_US_Ivo_FLUXNET2015_FULLSET_MM_2004_2007_1_4)
Flux_Ivo <- select(Flux_Ivo_all, TIMESTAMP, TA_F, VPD_F, PA_F, CO2_F_MDS, SW_IN_F, PPFD_IN)
Fapar_Ivo0407 <- filter(faparIvo, month > 200312 & month < 200801)
Data_faparIvo <- select(Fapar_Ivo0407,"monthmean")
colnames(Data_faparIvo) <- "FAPAR"
Data_Ivo <- bind_cols(Flux_Ivo, Data_faparIvo)

write.csv(Data_Ivo, file = "~/reading/R_work/Data/SITES_input_data/Data_Ivo_2004_2007", row.names = FALSE)

# consider replacing -9999 values with either linaearly interpolated values or NA?(don't know yet which format is ok for missing values) or deleting rows with -9999 values to be able to run the code:

temperature<- Data_Ivo["TA_F"]
vappresd<- Data_Ivo["VPD_F"]
CO2_conc<- Data_Ivo["CO2_F_MDS"]
fapar_MERIS<-Data_Ivo["FAPAR"]
PPFD_vals<-Data_Ivo["SW_IN_F"]
atmosp<-Data_Ivo["PA_F"]

out_ts_pmodel <- rpmodel( 
       tc             = temperature,
       vpd            = vappresd,
       co2            = CO2_conc,
       fapar          = fapar_MERIS,
       ppfd           = PPFD_vals,
       patm            = atmosp,         
       kphio          = 0.05,         
       beta           = 146,
       method_optci   = "prentice14",
       method_jmaxlim = "none",
       do_ftemp_kphio = FALSE,
       verbose        = FALSE
)
       print(out_ts_pmodel$gpp)
       
       library(purrr)
       df <- tibble(
               tc             = temperature,
               vpd            = vappresd,
               co2            = CO2_conc,
               fapar          = fapar_MERIS,
               ppfd           = PPFD_vals
       ) %>%
               mutate( out_pmodel = purrr::pmap(., rpmodel, 
                                                patm            = atmosp,         
                                                kphio          = 0.05,         
                                                beta           = 146,
                                                method_optci   = "prentice14",
                                                method_jmaxlim = "none",
                                                do_ftemp_kphio = FALSE
               ) )
       print(df)

       
 #try to get ingestr to work: from instructions https://rpubs.com/stineb/ingestr
       
       
   #1- build site info data frame    
       
       sitename <- c('US-Los','RU-Che','US-Ivo','US-Atq','CZ-wet','DE-Akm','DE-SfN')
       lon <- c(-89.9792, 161.34143, -155.7503, -157.4089, 14.77035, 13.68342, 11.3275)
       lat  <- c(46.0827, 68.61304, 68.4865, 70.4696, 49.02465, 53.86617, 47.80639)
       elv <- c(480, 6, 568, 15, 426, -1, 590)
       #year_start <- as.Date(c('2001-1-1','2002-1-1','2004-1-1','2003-1-1','2006-1-1','2009-1-1','2012-1-1'))
       #year_end <- as.Date(c('2014-12-31','2005-12-31','2007-12-31','2008-12-31','2014-12-31','2014-12-31','2014-12-31'))
       year_start <- as.Date(c('2001','2002','2004','2003','2006','2009','2012'))
       year_end <- as.Date(c('2014','2005','2007','2008','2014','2014','2014'))
       
       siteinfo<- data.frame(sitename, lon, lat, year_start, year_end, stringsAsFactors=FALSE)
       write_csv(siteinfo, "~/reading/R_work/Data/Fluxnet/Fluxnet2015/siteinfo_fluxnet2015.csv")
       
    #2- Load site info to run ingestr (https://stineb.github.io/ingestr/articles/example.html#examples-for-a-site-ensemble)   
       library(purrr)
       library(dplyr)
       library(lubridate)
       library(tidyr)
       library(raster)
       library(ncdf4)
       
       mysites<- c("US-Los", "Ru-Che", "US-Ivo", "US-Atq", "CZ-wet", "DE-Akm", "DE-SfN")
       siteinfo <- readr::read_csv("~/reading/R_work/Data/Fluxnet/Fluxnet2015/siteinfo_fluxnet2015.csv") %>%
               filter(sitename %in% mysites) %>%
               mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
               mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))
       
       
    #3- Ingest meteo data
       
       ddf_fluxnet <- ingest(
               siteinfo = siteinfo,
               source    = "fluxnet",
               getvars   = list(temp = "TA_F_DAY", prec = "P_F", vpd  = "VPD_F_DAY", ppfd =  "SW_IN_F", netrad = "NETRAD", patm = "PA_F"),
               dir       = "~/reading/R_work/Data/Fluxnet/Fluxnet2015/",
               settings  = list(dir_hh = "~/reading/R_work/Data/Fluxnet/Fluxnet2015/", getswc = FALSE),
               timescale = "d"
       )
     
       library(ingestr)
       ddf_fluxnet <- ingest(
               siteinfo  = siteinfo %>% filter(sitename %in% mysites),
               source    = "fluxnet",
               getvars   = list(temp = "TA_F_DAY", prec = "P_F", vpd  = "VPD_F", swin =  "SW_IN_F", patm = "PA_F"),
               dir       = paste0(path.package("rsofun"), "/extdata/"),
               settings  = list(dir_hh = paste0(path.package("rsofun"), "/extdata/"), dir_hr = paste0(path.package("rsofun"), "/extdata/"), getswc = FALSE),
               timescale = "d"
       )