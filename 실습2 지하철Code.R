setwd('C:/Users/KimSungWoon/Desktop/Data')
library(sqldf)
# 지하철1~9 호선자료만추출한다.
library(RODBC)
conn<-odbcConnect('census',uid='scscStudent',pwd='1234')

temp <-read.csv('subwayData.csv', header=TRUE, colClass=c(CalDate="character"))

subway <-sqldf("select * from temp where LineNo <= '9'")
#temp의 자료를 불러온다 LineNo가 9이하인 것만 불러온다. 

temp <-read.csv("subwayLocation.csv", header=TRUE)
subwayLocation<-sqldf("select * from temp where xPointWGS> 0")
#지하철역에대한좌표를입력받는다. 좌표미상인것은제외함


line2 <-sqldf("select Station, avg(RideCnt) as RideCnt, avg(OutCnt) as OutCnt from subway where LineNo='2' group by station order by RideCnt, OutCnt")
line2$Diff <-line2$OutCnt -line2$RideCnt
line2<-line2[order(-line2$Diff),]
barplot(line2$Diff,names.arg=line2$Station)
# 2호선역에대해역별로승차인원과하차인원평균을구하고두값의차이를구한다.

barChart<-function(Line) {
  temp<-sqldf(paste("select Station, avg(RideCnt) as RideCnt, avg(OutCnt) as OutCnt from subway where LineNo=",Line," group by station" ))
  temp<-temp[order(-temp$RideCnt),]
  barplot(temp$RideCnt,names.arg=temp$Station)
}
barChart("'2'")
subwayLocation2 <-sqldf("select * from subwayLocation where LineNo= '2' and StationCode<= 243 order by LineNo, StationCode")
line2DB <-merge(subwayLocation2, line2, by="Station")
line2DB <-line2DB[order(line2DB$StationCode),]
install.packages("ggmap")
library(ggmap)
Map_Seoul<-get_map("Seoul", zoom=11, maptype="roadmap")
MM <-ggmap(Map_Seoul)
MM <-MM + geom_point(aes(x=YPointWGS, y=XPointWGS,size=RideCnt), data=line2DB)
MM
MM2 <-MM + geom_path(aes(x=YPointWGS, y=XPointWGS),size=1,linetype=1,colour="green", data=line2DB)
MM2
