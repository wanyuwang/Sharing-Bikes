###setwd("C:/Course/STATCOM/Nice_ride_data_2015_season")
setwd("/Users/wanyuwang/Desktop/DA/resume参考／niceride")
#############Data Process

#record data
ride=read.csv("Nice_ride_trip_history_2015_season.csv")
head(ride)

#station locations data
station=read.csv("Nice_Ride_2015_station_locations.csv")
head(station)
station=station[order(station$Terminal),,]
summary(station)

#split date and time
ride$startdate=matrix(unlist(strsplit(as.character(ride$Start.date),' ')),
                      length(ride$Start.date),2,byrow=T)[,1]
ride$starttime=matrix(unlist(strsplit(as.character(ride$Start.date),' ')),
                          length(ride$Start.date),2,byrow=T)[,2]

ride$enddate=matrix(unlist(strsplit(as.character(ride$End.date),' ')),
                    length(ride$End.date),2,byrow=T)[,1]

ride$endtime=matrix(unlist(strsplit(as.character(ride$End.date),' ')),
                        length(ride$End.date),2,byrow=T)[,2]

###############################################3

#Clarify weekday and weekend
library(lubridate)
ride$startdate=as.Date(ride$startdate,format='%m/%d/%Y')
ride$startweek=wday(ride$startdate,label=T,abbr=T)
summary(ride$startweek)


ride$enddate=as.Date(ride$enddate,format='%m/%d/%Y')
ride$endweek=wday(ride$enddate,label=T,abbr=T)
summary(ride$endweek)

# ratio of weekend/weekday
library(dplyr)
ride=filter(ride,ride$Start.station.number!='NRHQ'&ride$End.station.number!='NRHQ')
weekend.start=filter(ride,ride$startweek=='Sun'|ride$startweek=='Sat')

station.startweekend=summarise(group_by(weekend.start,Start.station.number),count=n())
station.startweekend=station.startweekend[order(station.startweekend$Start.station.number),,]
summary(station.startweekend)

weekday.start=filter(ride,ride$startweek!='Sun'&ride$startweek!='Sat')
station.startweekday=summarise(group_by(weekday.start,Start.station.number),count=n())
station.startweekday=station.startweekday[order(station.startweekday$Start.station.number),]

weekend.end=filter(ride,ride$endweek=='Sun'|ride$endweek=='Sat')
station.endweekend=summarise(group_by(weekend.end,End.station.number),count=n())
station.endweekend=station.endweekend[order(station.endweekend$End.station.number),,]
station.endweekend


weekday.end=filter(ride,ride$endweek!='Sun'&ride$endweek!='Sat')
station.endweekday=summarise(group_by(weekday.end,End.station.number),count=n())
station.endweekday=station.endweekday[order(station.endweekday$End.station.number),,]
station.endweekday

weekendratio.start=station.startweekend$count/station.startweekday$count
weekendratio.end=station.endweekend$count/station.endweekday$cound
sum((weekendratio.end-weekendratio.start)^2)

weekendratio=weekendratio.start
station<-cbind(station,weekendratio)
STATION<-station[,-6]

#mean duration
Duration.start=summarise(group_by(ride,Start.station.number),mean(Total.duration..Seconds.))
Duration.start=Duration.start[order(Duration.start$Start.station.number),]
Duration.start=Duration.start[,2]
colnames(Duration.start)<-'Duration.start.sec'

Duration.end=summarise(group_by(ride,End.station.number),mean(Total.duration..Seconds.))
Duration.end=Duration.end[order(Duration.end$End.station.number),]
Duration.end=Duration.end[,2]
colnames(Duration.end)<-'Duration.end.sec'

STATION<-cbind(STATION,Duration.start)
STATION<-cbind(STATION,Duration.end)

#casual/member ratio
casualride=filter(ride,Account.type=='Casual')##casual
memberride=filter(ride,Account.type=='Member')##member

##start startion
casualuser.start=summarise(group_by(casualride,Start.station.number),count=n())
memberuser.start=summarise(group_by(memberride,Start.station.number),count=n())
casualuser.start=casualuser.start[order(casualuser.start$Start.station.number),]
memberuser.start=memberuser.start[order(memberuser.start$Start.station.number),]
station.start=cbind2(casualuser.start,memberuser.start$count)
station.start=cbind2(station.start,casualuser.start$count/memberuser.start$count)
colnames(station.start)=c('Start.station.number','casualuser.start','memberuser.start','casualratio.start')

station.start=station.start[,-1]
station.start

casualuser.end=summarise(group_by(casualride,End.station.number),count=n())
memberuser.end=summarise(group_by(memberride,End.station.number),count=n())
casualuser.end=casualuser.end[order(casualuser.end$End.station.number),]
memberuser.end=memberuser.end[order(memberuser.end$End.station.number),]
station.end=cbind2(casualuser.end,memberuser.end$count)
station.end=cbind2(station.end,casualuser.end$count/memberuser.end$count)
colnames(station.end)=c('End.station.number','casualuser.end','memberuser.end','casualratio.end')

station.end=station.end[,-1]
station.end

STATION<-cbind(STATION,station.start)
STATION<-cbind(STATION,station.end)

##### start not end ratio
startisend=filter(ride,ride$Start.station.number==ride$End.station.number)
startisend=summarise(group_by(startisend,Start.station.number),count=n())
startisend=startisend[order(startisend$Start.station.number),]
startisend=startisend[,2]

startnotend=filter(ride,ride$Start.station.number!=ride$End.station.number)
startnotend=summarise(group_by(startnotend,Start.station.number),count=n())
startnotend=startnotend[order(startnotend$Start.station.number),]
startnotend=startnotend[,2]

ratio.StartNotEnd<-startnotend/startisend
colnames(ratio.StartNotEnd)<-"ratio.StartNotEnd"
ratio.StartNotEnd

STATION<-cbind(STATION,ratio.StartNotEnd)
STATION
head(STATION)


#### use all factors
test=STATION[,c(3:15)]
head(test)
test=scale(test,center = TRUE, scale=TRUE)
kmean=kmeans(test, 5, nstart=100)
cluster<-kmean$cluster
station5=cbind(STATION,cluster)

station51=summarise(group_by(station5,cluster),mean(NbDocks),mean(weekendratio),mean(Duration.start.sec),
          mean(Duration.end.sec),mean(casualuser.start),mean(memberuser.start)
          ,mean(casualratio.start),mean(casualuser.end),mean(memberuser.end),
          mean(ratio.StartNotEnd))
dim(station51)
write.csv(station51,file='cluster5.csv')

### use 4 factors 'weekendratio','Duration.start.sec','casualratio.start','ratio.StartNotEnd'
test4=test[,c('weekendratio','Duration.start.sec','casualratio.start','ratio.StartNotEnd')]
test4=scale(test4,center = TRUE, scale=TRUE)
kmean4=kmeans(test4, 5, nstart=100)
cluster4<-kmean4$cluster
station54=cbind(STATION,cluster4)
station514=summarise(group_by(station54,cluster4),mean(weekendratio),mean(Duration.start.sec)
                    ,mean(casualratio.start),mean(ratio.StartNotEnd))
write.csv(station514,file='cluster54.csv')

###high mean casualratio.start ----- cluster4
station54[which(station54$cluster4==4),2]

##
station54[which(station54$cluster4==1),2]

##
station54[which(station54$cluster4==2),2]
head(station54)


### data    spatial visulization
library(ggmap)
station54$cluster4=as.factor(station54$cluster4)
MinneapolisMap <- qmap("minneapolis", zoom = 11,
                       color = "bw", legend = "topleft")
MinneapolisMap +
  geom_point(aes(x = Longitude, y = Latitude,
                 colour = cluster4, size = 1.3),
             data = station54)


MinneapolisMap +
  stat_bin2d(
    aes(x = Longitude, y = Latitude, colour = cluster4,
        fill = cluster4),
    size =1, bins = 30, alpha = 1/2,
    data = station54)


