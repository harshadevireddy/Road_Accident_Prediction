library(dplyr)
library(spatstat)
library(ggmap) 
library(maptools)
library(scales)
library(mgcv)
par(mar = c(5,5,5,2))
library(dplyr)


merged_aadt$Traffic<-merged_aadt$AADT*2*365/1000000 

avg.rate = sum(merged_aadt$Freq)/sum(merged_aadt$Traffic) 

x = merged_aadt %>% group_by(LOCATION_ID) %>% summarize(
  accidents=sum(Freq),
   traffic=sum(Traffic), #sum(TotalT5YearInM),
rate=accidents/traffic,
baseline=avg.rate*traffic,
Long=unique(Y),Lat=unique(X))

cleaned_aadt<-merged_aadt[merged_aadt$AADT!=0,]
cleaned_aadt$rate<-cleaned_aadt$Freq/cleaned_aadt$Traffic
write.csv(cleaned_aadt,'cleaned_aadt.csv')

#------------------------------------------------
# Some basic plots of accidents vs. traffic vs. rate
# - note: lots of zeros in accidents, so be careful with taking logs
#------------------------------------------------
plot(x$traffic,x$accidents); abline(h=mean(merged_aadt$Freq))
plot(x$traffic,log(x$rate+1)); abline(h=log(avg.rate))
plot(x$traffic,x$rate); abline(h=avg.rate)
plot(log(x$rate/avg.rate)); abline(h=0)
plot(x$baseline,log(.1+x$rate/avg.rate)); abline(h=0)
plot(x$baseline,x$accidents-x$baseline); abline(h=0)
plot(x$baseline,(x$accidents-x$baseline)/x$baseline); abline(h=0)

plot(x$traffic,log(.1+x$rate/avg.rate)); abline(h=0)
plot(x$traffic,x$accidents-x$baseline); abline(h=0)

p1 = qplot(traffic,accidents,data=x) + 
  xlab("Total Traffic (in M)") + ylab("Number of Accidents") + geom_hline(yintercept=avg.rate)

p2 = qplot(traffic,log(.1+x$rate/avg.rate),data=x) + xlab("Total Traffic (in M)") +
  scale_size_area() +ylab("(Road_Segment / Overall) Rate") + geom_hline(yintercept=0)

p3 = qplot(traffic,accidents-baseline,data=x) + xlab("Total Traffic (in M)") +
  ylab("(Actual - Expected) Accidents") + geom_hline(yintercept=0)

hist(merged_aadt$Freq,freq=FALSE,100,xlab = 'Crash Count', ylab = 'Count of Road Segments',main='Histogram of Crash Counts',xlim=c(0,250))
lines(density(merged_aadt$Freq), lty=2, col=2, lwd=2) 

library(ggmap)
montgomery <- get_map("MOntgomery,Maryland", zoom = 7)
montgomery_Map <- ggmap(montgomery, extent = "device", legend = 'topleft', inherit.aes = FALSE)

montgomery_Map + 
  geom_point(data=x, aes(x=Long, y=Lat, fill = log(accidents+1), size=log(rate)),
             shape=21,alpha=.8) +
  scale_fill_gradient(low="deepskyblue", high="red")

#-- Traffic rate vs. Accident Rate
#  Notice: clear distinction between major and minor roads, and accidents increase
#          as traffic increases (but for each type)
rural = (merged_aadt$FUNC_CLASS_DESC %in% c('RURAL INTERSTATE','RURAL LOCAL','RURAL MAJOR COLLECTOR','RURAL PRINCIPAL ARTERIAL - OTHER'))
URBAN_INTERSTATE = (merged_aadt$FUNC_CLASS_DESC == 'URBAN INTERSTATE')
URBAN_LOCAL = (merged_aadt$FUNC_CLASS_DESC == 'URBAN LOCAL')
URBAN_MAJOR_COLLECTOR = (merged_aadt$FUNC_CLASS_DESC == 'URBAN MAJOR COLLECTOR')
URBAN_MINOR_ARTERIAL = (merged_aadt$FUNC_CLASS_DESC == 'URBAN MINOR ARTERIAL')
URBAN_PRINCIPAL_ARTERIAL_OTHER = (merged_aadt$FUNC_CLASS_DESC == 'URBAN PRINCIPAL ARTERIAL - OTHER')
URBAN_PRINCIPAL_ARTERIAL_FREEWAY_EXPRESSWAY  = (merged_aadt$FUNC_CLASS_DESC == 'URBAN PRINCIPAL ARTERIAL -FREEWAY EXPRESSWAY')


plot(log(merged_aadt$Traffic),merged_aadt$Freq,col=ifelse(rural,"red","blue"))
points(log(merged_aadt$Traffic),loess(merged_aadt$Freq~log(merged_aadt$Traffic),)$fitted,pch=19,cex=.5)
scatter.smooth(log(a$TotalT5YearInM),a$X5YrCrashCount,col=ifelse(major,"red","blue"))

scatter.smooth(log(a$TotalT5YearInM[major]),a$X5YrCrashCount[major],col="red")

scatter.smooth(log(a$TotalT5YearInM[minor]),a$X5YrCrashCount[minor],col="red")


fr<-read.table('clipboard',sep = "\t",header = TRUE)

boxplot(freq_per_mile~FUNC_CLASS_DESC2,data=fr,las=1,ylim=c(0,450)
        ,names=c("1","2","3","4","5","6","7"),xlab="Type of road",ylab="Crashes per mile",main="Boxplot of Crashes per mile")

p <- ggplot(fr, aes(FUNC_CLASS_DESC2, freq_per_mile,ymax=450))
p + geom_boxplot(aes(ymin = 0, lower = y25, middle = y50, upper = y75, ymax = y100))


ggplot(fr, aes(x=FUNC_CLASS_DESC2, y=log(freq_per_mile+1))) +
  geom_boxplot(outlier.colour="red") +
  labs(x="Function Classification of Road",y="Crashes per mile(Log trans)")

+ #coord_cartesian(ylim = c(0, 300))

ggplot(merged_aadt, aes(x=MUNICIPALITY_Flag, y=log(freq_per_mile+1))) +
  geom_boxplot(outlier.colour="red")+
  labs(x="Municipality",y="Crashes per mile(Log trans)")
#+coord_cartesian(ylim = c(0, 300)) 



ggplot(merged_aadt, aes(x=Exit, y=log(freq_per_mile+1))) +
  geom_boxplot(outlier.colour="red")+
  labs(x="Exit",y="Crashes per mile(Log trans)")
#+coord_cartesian(ylim = c(0, 300))
                                                   

pairs(~Freq+K_FACTOR+D_FACTOR+NUM_LANES+AADT+SEC_LEN, data=m, cex.labels=2, cex.axis=1,cex=1)

m<-merged_aadt[merged_aadt$Freq>0,]
cor(merged_aadt$NUM_LANES,merged_aadt$AADT)

qplot(merged_aadt$Freq,
      geom="histogram",
      binwidth = 3,  
      main = "Histogram for Crash Counts", 
      xlab = "Crash Count",  
      ylab = "Count of Road Segments",
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(0,200))

qplot(log(merged_aadt$Freq+1),
      geom="histogram",
      main = "Histogram for Crash Counts", 
      xlab = "Crash Count",  
      ylab = "Count of Road Segments(Log trans)",
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))

favstats(~ Freq, data=merged_aadt)
