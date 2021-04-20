library(KoNLP)
library(RColorBrewer)
library(wordcloud)
useSejongDic()
#R이 한글을 분석할때 사용 할 한글 사전을 불러온다.

txt<-readLines("C:/Users/KimSungWoon/Desktop/data/bigdata.txt")
# txt에 분석하고 싶은 텍스트를 넣는다.

txtNoun<-sapply(txt,extractNoun,USE.NAMES=F)
#extractNoun 함수에 의해 각줄마다 단어만 남는다. 함수를 사용하기 위해서는 KoNLP 패키지를 꼭설치해야함
#USE.NAMES=F 원 문장 필요없는경우 F

c <-unlist(txtNoun)
# 줄순서에 관계 없이 단어들만 가져온다.

txtNoun<-Filter(function(x) {nchar(x) >= 2} ,c)

#Filter 함수를 활용해 c데이터 안에 있는 단어들중 2글자 이상인것만 의미있는 명사로 기준을 두어 분석을 할것이다.

write.table(txtNoun,"C:/Users/KimSungWoon/Desktop/data/noun.csv", sep=",", quote=F,row.names=F, col.names=T)
#위에서 다듬은 단어들을 csv 파일로 저장하고

noun<-read.table("C:/Users/KimSungWoon/Desktop/data/noun.csv", header=TRUE)
#단어 별로 넘버링을해 저장시킨다.

wordcount<-sort(table(noun),decreasing=T)
#이 파일을 다시 테이블형식으로 변환해 가져온다.

palete<-brewer.pal(9,"Set1")
#이 컬러의 옵션값 brewer을 사용하기 위해서는 맨처음 RColorVrewer 패키지를 로딩 해야한다.

png(filename="C:/Users/KimSungWoon/Desktop/data/bigdata.png", height=500, width=500)

wordcloud(names(wordcount),freq=wordcount,scale=c(5,1),rot.per=0.25,min.freq=2,
          random.order=F,random.color=T,colors=palete)
#워드 클라우드로 출력을한다.

dev.off()
#dev.off 함수로 R이 더 이상 그래프를 다시 보내기(redirect) 못하게 정지한다.
#dev.off 실행하지 않고 pdf함수를 여러번 실행하게 되면, 가장 최근에 열린 파일에 그래프가 저장된다. 