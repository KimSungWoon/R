#4월 12일 자료분석
#spam.r 코딩

setwd("C:/Users/KimSungWoon/Desktop/data")
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
sms_raw$text<-iconv(enc2utf8(sms_raw$text),sub="byte") 
# Solution of multybyte Error : 
# 이메일 서로 주고받을시 워드프로세서 같이 복잡해졌다. 
# 기본적인 텍스트가 아니라 multybyte가 발생할수 있다. 
# 이상한 텍스트를 인지하지 못한다.

sms_raw$type <- factor(sms_raw$text)
sms_raw$type <- sms_raw$癤퓍ype   #깨진 변수 추가시키면 된다!!
install.packages("tm")
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower)) #소문자로 변호
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) # remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) # remove stop words
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) # remove punctuation
install.packages("SnowballC")
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) # eliminate unneeded whitespace(blank, tab, 개행문자)
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

sms_dtm_train <- sms_dtm[1:4169, ]  #1~4169 training dataset 
sms_dtm_test  <- sms_dtm[4170:5559, ]  #test dataset

# also save the labels
sms_train_labels <- sms_raw[1:4169, ]$type   #레이블을 나누는 이유 : test문제를 주면서 답을주는 꼴이므로 레이블을 나눠준다.
sms_test_labels  <- sms_raw[4170:5559, ]$type
sms_dtm_freq_train <- removeSparseTerms(sms_dtm_train, 0.999) # sms_dtm_train에서 0.999이상인 건 없애라
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)




## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

install.packages("gmodels")
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


## Step 5: Improving model performance ----
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1) #laplace=1로 주면 라플라스를 하겠다는 ㄸ
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))




#knn 유방암 데이터
##### Chapter 3: Classification using Nearest Neighbors --------------------

## Example: Classifying Cancer Samples ----
## Step 2: Exploring and preparing the data ---- 

# import the CSV file
setwd("C:/Users/KimSungWoon/Desktop/data")
wbcd <- read.csv("BCancer.csv", stringsAsFactors = FALSE)  #stringsAsFactors항상 스트링은 팩터형으로 읽ㅇ

# examine the structure of the wbcd data frame
str(wbcd)

# drop the id feature
wbcd <- wbcd[-1]

# table of diagnosis
table(wbcd$diagnosis)

# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

#### table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))  #normalize 대상은 diagnosis를 빼고

# confirm that normalization worked
summary(wbcd_n$area_mean)

# create training and test data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]   #데이터가 골고루 섞여 있다는게 전제되어 져야 한다.

# create labels for training and test data

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

## Step 3: Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

## Step 4: Evaluating model performance ----

# load the "gmodels" library
install.packages("gmodels")
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
