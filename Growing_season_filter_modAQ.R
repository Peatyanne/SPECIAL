# growing season code
# 25/09/2020
# starting from Keith's code based on Lasslop 2012

# I created a method which uses: a vertical and orizontal approach (see the figure I sent you)
# to switch from an apporach to another you simply have to comment row 40

library(lubridate) 

# Upload your dataframe
# read the half-hour dataset 
dataOr = read.csv('~/reading/R_work/Data/WET_5sites_inputs/FLX_RU-Che_FLUXNET2015_FULLSET_HH.csv')

# fix the TIME column problem
#dataOr$TIME = ymd_hms(as.character(dataOr$TIME))
dataOr$TIME=0
dataOr$TIME<- ymd_hm(dataOr$TIMESTAMP_START)

# add a column called 'grow' to the uploaded dataset
# I set 0 values for this new column, at the beginning
dataOr$grow = 0
# However, this new column will be filled with the following code

# cycle for the years (Note:I don't have a cycle for the sites because I run the model each site individually)
dataOr$YEAR=0
dataOr$YEAR <- as.numeric(format(dataOr$TIME, "%Y"))
AllYears <- unique(dataOr$YEAR)

# percentual threshold for the growing season (see Lasslop paper)
th_perc = 0.2

for (j in AllYears) {
  # year extraction
  pos_year = which(dataOr$YEAR == j)
  dataOr_year = dataOr[pos_year,]
  
  # using percentile I compute the 'start' and end
  # I am usiing the observed GPP
  top <- quantile(dataOr_year$GPP_DT_CUT_REF, 0.95, na.rm = T)
  toe <- quantile(dataOr_year$GPP_DT_CUT_REF, 0.05, na.rm = T) # we then set this as our zero for the threshold
  
  # find the row for the growing season
  grow = which( dataOr_year$GPP_DT_CUT_REF > (toe + (th_perc * (top - toe))) )
  
  # Add values in the new column above (you can use this column to identify the growing season :-) see the example graph below
  #dataOr_year$grow[seq(min(grow),max(grow))] = 1   # row to comment, if you need an 'orizontal' apporach
  # rm(grow,top,toe)
  
  dataOr_year$grow[grow] = 1
  
  dataOr[pos_year,] = dataOr_year
  
  rm(dataOr_year,pos_year)
  
}
rm(j, AllYears)

# line to see the start and end of the growing season
# an example of the year

pos_growing_season = which(dataOr$YEAR == 2005 & dataOr$grow == 1)
dataOr$TIME[min(pos_growing_season)]
dataOr$TIME[max(pos_growing_season)]




# graph**********************************************************************************
# packeges
library(lubridate)  # to manage TIME
library(ggplot2)    

# GRAPH
ggplot(dataOr) +
  geom_point(aes(x = TIME, y = GPPpOpt)) +
  geom_point(data = dataOr[which(dataOr$grow == 1),],
             aes(x = TIME, y = GPPpOpt),color = 'red', alpha = 0.2) +
  geom_point(aes(x = TIME, y = GPP_DT_CUT_REF)) +
  geom_point(data = dataOr[which(dataOr$grow == 1),],
             aes(x = TIME, y = GPP_DT_CUT_REF),color = 'gray')
