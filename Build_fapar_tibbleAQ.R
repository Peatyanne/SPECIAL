#Build fapar tibble 
library(tidyverse)

fapar_CZwet <- read_csv("~/reading/R_work/Data/WET_5sites_inputs/fapar_CZ-wet_DayFullYrs.csv")
fapar_USLos <- read_csv("~/reading/R_work/Data/WET_5sites_inputs/fapar_US-Los_DayFullYrs.csv")
fapar_USIVo <- read_csv("~/reading/R_work/Data/WET_5sites_inputs/fapar_US-Ivo_DayFullYrs.csv")
fapar_USAtq <- read_csv("~/reading/R_work/Data/WET_5sites_inputs/fapar_US-Atq_DayFullYrs.csv")
fapar_RUChe <- read_csv("~/reading/R_work/Data/WET_5sites_inputs/fapar_RU-Che_DayFullYrs.csv")
#fapar_DESfN <- read_csv("~/reading/R_work/Data/WET_5sites_inputs/fapar_DE-SfN_Day.csv")
#fapar_DEAkm <- read_csv("~/reading/R_work/Data/WET_5sites_inputs/fapar_DE-SfN_Day.csv")



fapar_USIVo<-as_tibble(fapar_USIVo)
fapar_USLos<-as_tibble(fapar_USLos)
fapar_USAtq<-as_tibble(fapar_USAtq)
fapar_RUChe<-as_tibble(fapar_RUChe)
fapar_CZwet<-as_tibble(fapar_CZwet)
#fapar_DEAkm<-as_tibble(fapar_DEAkm)
#fapar_DESfN<-as_tibble(fapar_DESfN)

names(fapar_USIVo)<- c("date", "fapar")
names(fapar_USLos)<- c("date", "fapar")
names(fapar_USAtq)<- c("date", "fapar")
names(fapar_RUChe)<- c("date", "fapar")
names(fapar_CZwet)<- c("date", "fapar")

fapar_USAtq$sitename<- "US-Atq"
fapar_RUChe$sitename<- "RU-Che"
fapar_USIVo$sitename<- "US-Ivo"
fapar_USLos$sitename<- "US-Los"
fapar_CZwet$sitename<- "CZ-wet"

fapar_USAtq$date<- as.Date(as.POSIXct(fapar_USAtq$date, format="%d/%m/%Y"))
fapar_RUChe$date<- as.Date(as.POSIXct(fapar_RUChe$date, format="%d/%m/%Y"))
fapar_USIVo$date<- as.Date(as.POSIXct(fapar_USIVo$date, format="%d/%m/%Y"))
fapar_USLos$date<- as.Date(as.POSIXct(fapar_USLos$date, format="%d/%m/%Y"))
fapar_CZwet$date<- as.Date(as.POSIXct(fapar_CZwet$date, format="%d/%m/%Y"))
#fapar_DEAkm$Date<- as.Date(fapar_DEAkm$Date)
#fapar_DESfN$Date<- as.Date(fapar_DESfN$Date)

## replace NA values with 0 to be able to run the model because Gap filling interpolation does not work when first or last value of the vector is NA.
fapar_USAtq$fapar<- replace(fapar_USAtq$fapar, fapar_USAtq$fapar>1, 0)
fapar_RUChe$fapar<- replace(fapar_RUChe$fapar, fapar_RUChe$fapar>1, 0)
fapar_USIVo$fapar<- replace(fapar_USIVo$fapar, fapar_USIVo$fapar>1, 0)
fapar_USLos$fapar<- replace(fapar_USLos$fapar, fapar_USLos$fapar>1, 0)
fapar_CZwet$fapar<- replace(fapar_CZwet$fapar, fapar_CZwet$fapar>1, 0)

df2<- bind_rows(fapar_USLos,fapar_RUChe,fapar_USIVo, fapar_USAtq, fapar_CZwet)
#df2<- bind_rows(fapar_USLos,fapar_RUChe,fapar_USIVo, fapar_USAtq, fapar_CZwet,fapar_DEAkm,fapar_DESfN)
data<-df2 %>% group_by(sitename) %>% nest()
df_fapar_5sites<-data


save(df_fapar_5sites,file="~/reading/R_work/Data/WET_5sites_inputs/df_fapar_5sites.Rdata")
                              

