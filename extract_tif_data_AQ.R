## Extract data from tif

require(raster)

##filenames<- read.csv("~/reading/R_work/Data/soil_moist/PRODUCT/Filename.csv")

temp = list.files(path= "~/reading/R_work/Data/soil_moist/PRODUCT/", pattern="*.tif")
siteinfo<- read.csv("~/reading/R_work/Data/Fluxnet/Fluxnet2015/siteinfo_fluxnet2015.csv")


out<-data.frame()

for (j in temp){ 
  filename <- paste("~/reading/R_work/Data/soil_moist/PRODUCT/",j, sep="")
    
  r<- raster(filename) 
  
  for (i in 1:nrow(siteinfo)) {
    
    points <- as.data.frame( cbind(longitude= siteinfo[i,"lon"],latitude= siteinfo[i,"lat"]))
    
   
    out[j,i]<- extract(r, points)}
  
}
names(out)<- c("US-Los","RU-Che","US-Ivo", "US-Atq","CZ-wet", "DE-Akm", "DE-SfN")

write.csv(fapar_siteDay,file="~/reading/R_work/Data/Fapar_MODIS/fapar_US-Ivo_DayFilter.csv", row.names=FALSE)



r <- raster('~/reading/R_work/Data/soil_moist/PRODUCT/SMY2018DECA36.tif')

##test.dat <- structure(list(latitude = c(46.0827, 68.61304, 68.4865, 70.4696, 49.02465, 53.86617, 47.80639),
##                           longitude = c(-86.9792, 161.3414, -155.75, -157.409, 14.77035, 13.68342, 11.3275)),
##                            .Names = c("latitude", "longitude"), class = "data.frame", row.names = c(NA, 7L))

points <- cbind(test.dat$longitude,test.dat$latitude)

test.dat$out <- extract(r, points)

