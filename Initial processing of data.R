library("openxlsx")
install.packages("xlsx")
setwd("D:/Rutgers academics/Funds of Analytics/Projects/Traffic data/Usable data")

#data_merg<-merge(data_acc_mont,data_person,by=c("REPORT_NO"),all.x = TRUE)
data_2<-read.xlsx('Crash_Qtr01_2015.xlsx',sheet = 1)
data_3<-read.xlsx('Crash_Qtr01_2016.xlsx',sheet = 1)
data_4<-read.xlsx('Crash_Qtr02_2015.xlsx',sheet = 1)
data_5<-read.xlsx('Crash_Qtr02_2016.xlsx',sheet = 1)
data_6<-read.xlsx('Crash_Qtr03_2015.xlsx',sheet = 1)
data_7<-read.xlsx('Crash_Qtr03_2016.xlsx',sheet = 1)
data_8<-read.xlsx('Crash_Qtr04_2015.xlsx',sheet = 1)
data_9<-read.xlsx('Crash_Qtr04_2016.xlsx',sheet = 1)

data_accident_2015<-rbind(data_2,
                          data_4,
                          data_6,
                          data_8)

data_accident_2016<-rbind(data_3,
                          data_5,
                          data_7,
                          data_9)
data_accident_2015<-data_accident_2015[,c("REPORT_NO","COUNTY_NO","ACC_DATE","MAINROAD_NAME","LATITUDE","LONGITUDE")]
data_accident_2016<-data_accident_2016[,c("REPORT_NO","COUNTY_NO","ACC_DATE","MAINROAD_NAME","LATITUDE","LONGITUDE")]

data_accident_2015<-subset(data_accident_2015,COUNTY_NO=="15")
data_accident_2016<-subset(data_accident_2016,COUNTY_NO=="15")
accident_2015_16<- rbind(data_accident_2015,data_accident_2016)
#accident_mainroad<-data.frame(table(accident_2015_16$MAINROAD_NAME))

#Maryland AADT data is fetched and cleaned below
aadt_data<-read.csv('Maryland_Annual_Average_Daily_Traffic__Annual_Average_Daily_Traffic_SHA_Statewide_AADT_Points.csv',header = TRUE)
names(aadt_data)[2]<-'X'
names(aadt_data)[1]<-'Y'

cols_req<-c('LOCATION_ID','X','Y','COUNTY_DESC','MUNICIPALITY','FUNC_CLASS_DESC','BEGIN_SECTION','END_SECTION','ROAD_SECTION','ROADNAME','K_FACTOR','D_FACTOR','NUM_LANES','AADT','AAWDT','MOTORCYCLE','CAR_PICKUP','BUS','SMALL_TRUCK','LARGE_TRUCK')
aadt_data<-aadt_data[,cols_req]
aadt_data<-subset(aadt_data,COUNTY_DESC=="Montgomery")
barplot(table(aadt_data$FUNC_CLASS_DESC))
# lat_1=39.67023167
# lat_2=39.67472468
# lon_1=-78.70608183
# lon_2=-78.69189262
# lat_3= 39.69858667
# lon_3=-78.63628333
#merging using road_name
library(memisc)
names(accident_2015_16)[4]<-"ROADNAME"
aadt_data$ROADNAME<-trimws(aadt_data$ROADNAME)
# acc_aadt<-merge(aadt_data,accident_2015_16,by="ROADNAME",all.x=TRUE)
# write.csv(acc_aadt,"acc_aadt.csv")
# length(unique(aadt_data$ROADNAME))
# freq_table<-table(acc_aadt$ROADNAME)

library(Imap)

# dist_mat2<-gdist(lon.1 = lon_1,
#                  lat.1 = lat_1,
#                  lon.2 = lon_2,
#                  lat.2 = lat_2,
#                  units="m")
# ed=sqrt(((lat_1-lat_2)^2)+((lon_1-lon_2)^2))
# 
# dist_mat3<-gdist(lon.1 = lon_1,
#                  lat.1 = lat_1,
#                  lon.2 = lon_3,
#                  lat.2 = lat_3,
#                  units="m")
# ed_2=sqrt(((lat_1-lat_3)^2)+((lon_1-lon_3)^2))

# aadt_data<-lat&long for various road sections in Montgomery
# accident_2015_16<-lat&long for each road crash
#rs_points<-aadt_data[,c('LOCATION_ID','X','Y')]
aadt_data<-aadt_data[complete.cases(aadt_data[,c('X','Y')]),]
accident_2015_16<-accident_2015_16[complete.cases(accident_2015_16[,c('LATITUDE','LONGITUDE')]),]

#crash_points<-accident_2015_16[,c('REPORT_NO','LATITUDE','LONGITUDE')]
dist_mat<-data.frame()
for(i in 1:nrow(accident_2015_16)){
road_sub<-subset(aadt_data,ROADNAME==accident_2015_16$ROADNAME[i])
  if(nrow(road_sub)>0){
      dist_mat<-gdist(lon.1 = road_sub$Y, 
                      lat.1 = road_sub$X, 
                      lon.2 = accident_2015_16$LONGITUDE[i], 
                      lat.2 = accident_2015_16$LATITUDE[i] , 
                      units="km")
      accident_2015_16[i,'road_loc']<-road_sub[which.min(dist_mat),'LOCATION_ID']
      accident_2015_16[i,'dist_frm_road']<-min(dist_mat)
      
  }

}

accident_roadsecs<-accident_2015_16[complete.cases(accident_2015_16[,c("road_loc")]),]

roadsec_freq<-data.frame(table(accident_roadsecs$road_loc))
roadsec_freq<-roadsec_freq[roadsec_freq$Freq>0,]
names(roadsec_freq)[1]<-'LOCATION_ID'

merged_aadt<-merge(aadt_data,roadsec_freq,by='LOCATION_ID',all.x=TRUE)

merged_aadt[is.na(merged_aadt$Freq),'Freq']=0
sum(merged_aadt$Freq)
#merged_aadt is the cleaned data set we will be performing modeling or further data transformation on

merged_aadt$MUNICIPALITY_Flag<-ifelse(merged_aadt$MUNICIPALITY=='none','no','yes')
merged_aadt$SEC_LEN<-merged_aadt$END_SECTION-merged_aadt$BEGIN_SECTION
merged_aadt$freq_per_mile<-merged_aadt$Freq/merged_aadt$SEC_LEN

merged_aadt$Exit<-ifelse(grepl('Exit',merged_aadt$ROAD_SECTION),'Yes',ifelse(grepl('EXIT',merged_aadt$ROAD_SECTION),'Yes','No')) 

table(merged_aadt$Exit)
merged_aadt$Exit<-as.factor(merged_aadt$Exit)
merged_aadt$MUNICIPALITY_Flag<-as.factor(merged_aadt$MUNICIPALITY_Flag)

#aadt zeros treatment

aadt_zeros<-merged_aadt[merged_aadt$AADT==0,c("ROAD_SECTION","AADT")]
aadt_zero_data<-read.table("clipboard",sep = "\t",header = TRUE)
a<-merge(aadt_zeros,aadt_zero_data,by="ROAD_SECTION")

for(i in 1:nrow(a)){
      merged_aadt$AADT[merged_aadt$ROAD_SECTION==a[i,"ROAD_SECTION"]]=a[i,"AADT.y"]
      merged_aadt$AAWDT[merged_aadt$ROAD_SECTION==a[i,"ROAD_SECTION"]]=a[i,"AAWDT"]
}

m<-duplicated(merged_aadt$LOCATION_ID)
sum(m)
merged_aadt<-merged_aadt[!m,]

#-----zeros are removed from AADT

#changing FUNC_CLASS_DESC
rural<-c("RURAL INTERSTATE","RURAL LOCAL","RURAL MAJOR COLLECTOR","RURAL MINOR ARTERIAL","RURAL MINOR COLLECTOR","RURAL PRINCIPAL ARTERIAL - OTHER")
merged_aadt$FUNC_CLASS_DESC2<-ifelse(merged_aadt$FUNC_CLASS_DESC %in% rural,"RURAL",ifelse(merged_aadt$FUNC_CLASS_DESC %in% "URBAN MINOR COLLECTOR","URBAN MAJOR COLLECTOR",as.character(merged_aadt$FUNC_CLASS_DESC)) )

# sprintf("%0.23f",aadt_data[which.min(dist_mat),]$Y)
# sprintf("%0.23f",aadt_data[which.min(dist_mat),]$X)
# sprintf("%0.23f",accident_2015_16[i,]$LATITUDE)
# sprintf("%0.23f",accident_2015_16[i,]$LONGITUDE)
# which(accident_2015_16$MAINROAD_NAME=='IVY LEAGUE LA')
# dist_mat2<-sort(dist_mat)
# a<-which(dist_mat==dist_mat2[2])
# aadt_data[a,]
# accident_2015_16[i,]
# sprintf("%0.23f",aadt_data[a,]$Y)
# sprintf("%0.23f",aadt_data[a,]$X)

#getting roadname from google maps
# library(ggmap)
# main_nas<-accident_2015_16[is.na(accident_2015_16$MAINROAD_NAME),]
# revgeocode(as.numeric(data.frame(-77.152,39.082225)))


