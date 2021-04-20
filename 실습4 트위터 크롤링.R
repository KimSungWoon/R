install.packages('twitteR')
library(twitteR) 
install.packages('ROAuth')
library(ROAuth)

#트위터 접근 URL: 접근 후, 승인번호를 R 창에 입력해야 한다. 
requestURL= "https://api.twitter.com/oauth/request_token" 
accessURL= "https://api.twitter.com/oauth/access_token" 
authURL ="https://api.twitter.com/oauth/authorize" 

#트위터에서 받은 네개의 키 값을 변수에 할당
consumerKey="QoqnQabbIimPGaiwfbddCyMwd"  
consumerSecret="mSUEqPZh34IFakPCaih8QpQnbYV1lSwCK6yyFSpS461OqLyctQ"
twitCred=OAuthFactory$new(consumerKey=consumerKey, 
                          consumerSecret=consumerSecret, 
                          requestURL=requestURL, 
                          accessURL=accessURL, 
                          authURL=authURL)
setwd("C:/Users/KimSungWoon/Desktop/data/bigdata.txt")
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem") 
twitCred$handshake(cainfo="cacert.pem")
7792713
#  Use a local file to cache OAuth access credentials between R sessions? 
#  질문에 2로 답한다.


api_key = "QoqnQabbIimPGaiwfbddCyMwd" 
api_secret = "mSUEqPZh34IFakPCaih8QpQnbYV1lSwCK6yyFSpS461OqLyctQ" 
access_token = "846951662174527488-KfBqEVyf4DPasht2eOmGNzG1TtOXAoc"  
access_token_secret = "PfcyXJswqhPzHSoYAt9lomxbdmjBBIn1mWrzXA8XZMzor" 


#인증 처리: 이부분이 key 값으로 트위터에 연결하는 부분이다.
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
2

# 트위터글을 검색하여 트윗정보를 가져온다
# 트위터에서 키워드로 검색 해서 UTF-8로 저장해라
keyword <-enc2utf8("중간고사")

#빅데이터라는 키워드로 2000개를 한글만 뽑아내라
result <- searchTwitter(keyword, n=2000, lang='ko')

#쓸모 없는 글자들을 제거 한다
temp <- twListToDF(result) #결과중에 텍스트에 해당하는 것만 뽑는다
temp <- temp[temp$retweetCount<5,]  # 리트윗 카운터가 5 이하인것만 가져와라 ->334로 변했는데 그만큼 리트윗이 많다는거 
temp <- temp[temp$isRetweet=="FALSE",]
temp <- temp[order(temp$test),]
result.text <- temp$text

#불필요한 문자를 필터링
result.text <- gsub('중간고사', '', result.text) # 아이폰을 -> 빈칸으로 바꿔라 
result.text <- gsub("[[:punct:]]", "", result.text) # [" "] -> [ ]
result.text <- gsub("[[:digit:]]", "", result.text) #
result.text <- gsub("[A-z]", "", result.text)  # 알파뱃 없애라
result.text <- gsub('\n', '', result.text) #리턴 코드 있으면 없애라
result.text <- gsub('\r', '', result.text) 
result.text <- gsub('RT', '', result.text) # 리트윗 이라는 말을 없애라
result.text <- gsub('st', '', result.text) 
result.text <- gsub("CO", "", result.text) 
result.text <- gsub("co", "", result.text) 
result.text <- gsub("ㅋㅋ", "", result.text) 
result.text <- gsub("ㅋㅋㅋ", "", result.text) 
result.text <- gsub("ㅋㅋㅋㅋ", "", result.text) 
result.text <- gsub("ㅠㅠ", "", result.text) 
result.text <- gsub("ㅠㅠ", "", result.text) 
result.text <- gsub("★", "", result.text)

library(KoNLP)  # 한글 자연어 처리
library(RColorBrewer) 
library(wordcloud) 
install.packages('foreach')
library(foreach) 
useSejongDic() # 이단어가 명사냐 형용사냐 # 분야 별로 새로운 사전을 만들어 쓰는것이 좋음

# 문자 분리 
result_nouns <- Map(extractNoun, result.text) 

result_wordsvec <- unlist(result_nouns, use.name=F) 
result_wordsvec <- gsub('[[:punct:]]','', result_wordsvec) 
result_wordsvec <- gsub(keyword,'', result_wordsvec) 
result_wordsvec <- gsub("데이터",'', result_wordsvec) 
result_wordsvec <- gsub("맵알이",'', result_wordsvec)
result_wordsvec <- gsub("해서",'', result_wordsvec) 
result_wordsvec <- gsub("우리",'', result_wordsvec) 
result_wordsvec <- gsub("공지",'', result_wordsvec) 
result_wordsvec <- gsub("한국의",'한국', result_wordsvec) 
result_wordsvec <- gsub("교육과",'교육', result_wordsvec) 
result_wordsvec <- gsub("가지",'', result_wordsvec) 
result_wordsvec <- gsub("하기",'', result_wordsvec)
result_wordsvec <- gsub("너리",'', result_wordsvec) 
result_wordsvec <- gsub("마이",'', result_wordsvec) 
result_wordsvec <- gsub("엑소",'', result_wordsvec) 
result_wordsvec <- gsub("냉랭",'', result_wordsvec) 
result_wordsvec <- Filter(function(x){nchar(x)>=2 & nchar(x) < 8}, 
                          result_wordsvec)

# 문자 카운팅 
result_wordcount <- table(result_wordsvec) 
result_wordcount <- result_wordcount[ result_wordcount > 1] 
result_wordcount[order(-result_wordcount)] 
# 컬러 세팅 
pal <- brewer.pal(12,'Paired')
# 폰트 세팅 
windowsFonts(malgun=windowsFont('맑은 고딕'))

# 그리기: min.freq를 너무 크게 설정하면 남겨지는 단어가 대폭 줄어들게 됨
wordcloud(names(result_wordcount), freq=result_wordcount, 
          scale=c(5,0.5), min.freq=2, random.order=F, 
          rot.per=.1, colors=pal, family='malgun')


###########################

install.packages('arules')
library(arules)
result_nouns<-Map(extractNoun, result.text)
for(i in 1:length(result_nouns)) {
  result_nouns[[i]]<-result_nouns[[i]][which(nchar(result_nouns[[i]])>=2 & nchar(result_nouns[[i]])<8)]
  result_nouns[[i]]<-setdiff(result_nouns[[i]],c("가지","맵알이","해서","잌ㅅ지","않가는"))
  result_nouns[[i]][which(result_nouns[[i]]=="정권뭘해도")]<-c("정권")
  result_nouns[[i]][which(result_nouns[[i]]=="교육과")]<-c("교육")
  result_nouns[[i]][which(result_nouns[[i]]=="빅데이터로")]<-c("빅데이터")
  result_nouns[[i]][which(result_nouns[[i]]=="빅데이터가")]<-c("빅데이터")
  result_nouns[[i]][which(result_nouns[[i]]=="넷플릭스는")]<-c("넷플릭스")
  result_nouns[[i]][which(result_nouns[[i]]=="아마존은")]<-c("아마존")
  result_nouns[[i]][which(result_nouns[[i]]=="맛집을")]<-c("맛집")
  result_nouns[[i]]<-unique(result_nouns[[i]])
}


lapply(result_nouns, write, "nouns.txt", append=TRUE, ncolumns=1000)

result_new<-read.transactions("nouns.txt", sep= " ")

itemFrequencyPlot(result_new, support = 0.1)
itemFrequencyPlot(result_new, topN= 20)

rules <-apriori(result_new, parameter = list(support = 0.01,confidence = 0.25, minlen=2))

inspect(sort(rules, by = "lift")[1:100])

rule1 <-subset(rules, items %in% "교육")

inspect(sort(rule1, by = "lift")[1:100])
