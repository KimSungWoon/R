install.packages(RODBC)
library(RODBC)
conn<-odbcConnect('census',uid='scscStudent',pwd='1234')
SiDoData<-sqlQuery(conn,"select ID_1, SiDoName,
                           sum(Pop) as Pop, avg(Gagu) as Gagu, 
                           avg(Income) as Income, avg(SESI) as SESI, 
                           avg(AvgGaguNo) as AvgGaguNo,
                           avg(AptRatio) as AptRatio, avg(MultiHouseRatio) as MultiHouseRatio, 
                           avg(SmeHouseRatio) as SmeHouseRatio,
                           avg(HouseOwner) as HouseOwner, avg(HouseFee) as HouseFee,
                           avg(NoRoom) as NoRoom, Avg(Area14) as Area14, 
                           Avg(Area69) as Area69, avg(RoomDensity) as RoomDensity
                           from censusData group by ID_1")
#시도코드,시도이름, 인구수를 합쳐 인구수로, 가구수 부터 평균을 censusData에서 시도 코드로 구룹을 지어 자료를 나타낸다.

SiDoData[order(-SiDoData$Pop),c(2,3)]
#시도데이터를 인구수를 내림차순으로 코드로 나타내고- > 시도 이름과 인구수의 합을 보여준다.

SiDoData[order(-SiDoData$AvgGaguNo),c(2,7)]
#시도데이터의 평균가구수를 내림차순으로 코드로 나타내고 -> SiDoName과 가구수 평균을 보여준다.

GuData<-sqlQuery(conn,"select ID_2, SiDoName, GuName,
sum(Pop) as Pop, sum(Gagu) as Gagu, avg(Income) as Income, avg(SESI) as SESI, avg(AvgGaguNo) as AvgGaguNo, avg(AptRatio) as AptRatio, avg(MultiHouseRatio) as MultiHouseRatio, avg(SmeHouseRatio) as SmeHouseRatio,
avg(HouseOwner) as HouseOwner, avg(HouseFee) as HouseFee,
avg(NoRoom) as NoRoom, Avg(Area14) as Area14, Avg(Area69) as Area69, avg(RoomDensity) as RoomDensity
 from censusData group by ID_2")
# 시군구 이름, 시도 이름 구이름 , 그리고 인구합,가구합, 나머지 평균을 censusData 로부터 시군구 코드로 구룹지어 나타낸다,

temp <- GuData[,-c(1:3)]
# GuData 에서 1~3열은 지우고 나타내라 (*) order(-SiDoData$AvgGaguNo)에 있는 내림차순과 헛 깔리지 않게 조심하기
panel.cor<-function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr<-par("usr"); on.exit(par(usr))
  par(usr= c(0, 1, 0, 1))
  r <-abs(cor(x, y))
  txt <-format(c(r, 0.123456789), digits=digits)[1]
  txt <-paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor<-0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex= cex.cor* r)
}
pairs(~., data=temp,lower.panel=panel.smooth, upper.panel=panel.cor, pch=20)
#상관계수를 나타낸다.

ZData<-scale(temp)
#scale은 표준 정규화를 하는것이다.(x-mean(x))/sd(x)와 같다.
GuCluster <-kmeans(ZData, 3)
GuCluster$size
#Cluster를 kmeans 3점으로 나누었는데 그 3점안에 들어가있는 시군구의 수를 나타낸것 이다.
GuCluster$centers
# 14차원안에서 3개의 점으로 군집한것들의 점과 가장 가까운 값을 보여준다. 
