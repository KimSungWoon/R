setwd('C:/Users/KimSungWoon/Desktop/data')
install.packages('maptools')
library(maptools)
install.packages('raster')
library(raster)
install.packages('RODBC')
library(RODBC)
conn<-odbcConnect('census',uid='scscStudent',pwd='1234')

temp <- sqlQuery(conn,'select distinct substr(GuName,1,3) from censusData')
##(*) # temp에다가 censusData에 있는 자료로 부터 구이름 으로 시도 코드와 시도이름 으로 나타내라 

IncomeData <-sqlQuery(conn,'select ID_1, SiDoName,
                      avg(Income) as Income from censusData group by ID_1 order by ID_1')
# 시도코드,시도이름, 수입의 합을 censusData의 데이터로부터 시도 코드별로 구룹 지어 시도코드 순서별로 나타 내어라.

# Income Standardization
maxValue<-max(IncomeData$Income)
minValue<-min(IncomeData$Income)
IncomeData$stdIncome <- (IncomeData$Income-minValue)/(maxValue-minValue)
IncomeData$rankIncome <- rank(IncomeData$stdIncome)
#표준화를 시킨다 (수입-수입최소값)/(최대값-최소값) -> 이것을 순서대로 나타낸다
ramp <-colorRamp(c("white", "blue"))
rgbColor<-rgb(ramp(seq(0,1,length=nrow(IncomeData))), max=255)
LocColor<-rgbColor[IncomeData$rankIncome]
## Download Korea level 2 data from gadm.org
load("KOR_adm1.RData")
korea<-gadm[order(gadm$ID_1),]
# ColorMap
plot(korea)
plot(korea, lwd=4, border="skyblue", add=T)
plot(korea,col=LocColor)
invisible(text(getSpPPolygonsLabptSlots(korea), labels=as.character(korea$NAME_1), cex=0.6, col="black", font=1))
mtext(side=3, line=1, "한국소득분포", cex=2)


#서울울의 시군두 단위 소득분포

IncomeData <-sqlQuery(conn,'select ID_1, ID_2, GuName,
                      avg(Income) as Income from censusData group by ID_2 order by ID_2')

# Income Standardization
maxValue<-max(IncomeData$Income)
minValue<-min(IncomeData$Income)
IncomeData$stdIncome <- (IncomeData$Income-minValue)/(maxValue-minValue)
IncomeData$rankIncome <- rank(IncomeData$stdIncome)

ramp <-colorRamp(c("white", "blue"))
rgbColor<-rgb(ramp(seq(0,1,length=nrow(IncomeData))), max=255)
LocColor<-rgbColor[IncomeData$rankIncome]
## Download Korea level 2 data from gadm.org

load('KOR_adm2.RData')
seoul<-gadm[gadm$ID_1==15,]

sqlQuery(conn,'select distinct ID_2 , GuName from censusData ')


# ColorMap
plot(seoul)
plot(seoul, lwd=4, border="skyblue", add=T)
plot(seoul,col=LocColor)
invisible(text(getSpPPolygonsLabptSlots(seoul), labels=as.character(seoul$NAME_2), cex=0.6, col="black", font=1))
mtext(side=3, line=1, "서울 소득분포", cex=2)


