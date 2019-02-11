getwd()
setwd("C:/Users/encaion/Desktop/Rawdata/폭염/")


# 1st elaboration (직접 크롤링 전처리 완료)
setwd("C:/Users/encaion/Desktop/Rawdata/폭염/news_폭염(2013)/")
filenames <- list.files(path="C:/Users/encaion/Desktop/Rawdata/폭염/news_폭염(2013)/",
                        pattern="news+.*.csv")
tables2<-as.data.frame(t(c(1:6)))
colnames(tables2)<-c('titles','urls','presses','datetime','edittime','bodies')

for(i in 1:length(filenames)){
  tables<-read.delim(fileEncoding = 'utf-8',sep=",",file=filenames[i],header=TRUE)
  tables2 <- rbind(tables2,tables)
}
tables2<-tables2[-1,]
setwd("C:/Users/encaion/Desktop/Rawdata/폭염/전처리완료/")
write.csv(tables2,"폭염2013.csv")


# analysis
library(KoNLP)
library(dplyr)
library(stringr)
library(KoNLP)
library(lda)
library(tm)
library(topicmodels)
library(LDAvis)
library(servr)
library(LDAvisData)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(lubridate)
useSejongDic()
useNIADic()

setwd("C:/Users/encaion/Desktop/Rawdata/폭염/뉴스/")
heat2<-read.csv("NEWS_20140101_20170717.csv")
heat2<-heat2[,c(3,6)]
colnames(heat2)<-c('titles','datetime')

heat2014<-subset(heat2,year(heat2$datetime) == 2014 )
heat2015<-subset(heat2,year(heat2$datetime) == 2015 )
heat2016<-subset(heat2,year(heat2$datetime) == 2016 )
heat2017<-subset(heat2,year(heat2$datetime) == 2017 )


setwd("C:/Users/encaion/Desktop/Rawdata/폭염/전처리완료/")
heat2008<-read.csv("폭염2008.csv")
heat2009<-read.csv("폭염2009.csv")
heat2010<-read.csv("폭염2010.csv")
heat2011<-read.csv("폭염2011.csv")
heat2012<-read.csv("폭염2012.csv")
heat2013<-read.csv("폭염2013.csv")
heat1<-rbind(heat2008,heat2009,heat2010,heat2011,heat2012,heat2013)
heat1<-heat1[,c(2,5)]
data1<-rbind(heat1,heat2)
data1[,1]<-as.character(data1[,1])

#change type
heat2008$titles<-as.character(heat2008$titles)
heat2009$titles<-as.character(heat2009$titles)
heat2010$titles<-as.character(heat2010$titles)
heat2011$titles<-as.character(heat2011$titles)
heat2012$titles<-as.character(heat2012$titles)
heat2013$titles<-as.character(heat2013$titles)
heat2014$titles<-as.character(heat2014$titles)
heat2015$titles<-as.character(heat2015$titles)
heat2016$titles<-as.character(heat2016$titles)

setwd("c:/Users/encaion/Desktop/2.생활기상/2018/폭염/")
heat2008_cnt<-wc(heat2008$titles)
heat2009_cnt<-wc(heat2009$titles)
heat2010_cnt<-wc(heat2010$titles)
heat2011_cnt<-wc(heat2011$titles)
heat2012_cnt<-wc(heat2012$titles)
heat2013_cnt<-wc(heat2013$titles)
heat2014_cnt<-wc(heat2014$titles)
heat2015_cnt<-wc(heat2015$titles)
heat2016_cnt<-wc(heat2016$titles)
heat2017_cnt<-wc(heat2017$titles)
#각 년도별 카운트를 저장
options(encoding = 'UTF-8')
write.table(heat2008_cnt,"heat2008_cnt.txt",sep = ",")
write.table(heat2009_cnt,"heat2009_cnt.txt",sep = ",")
write.table(heat2010_cnt,"heat2010_cnt.txt",sep = ",")
write.table(heat2011_cnt,"heat2011_cnt.txt",sep = ",")
write.table(heat2012_cnt,"heat2012_cnt.txt",sep = ",")
write.table(heat2013_cnt,"heat2013_cnt.txt",sep = ",")
write.table(heat2014_cnt,"heat2014_cnt.txt",sep = ",")
write.table(heat2015_cnt,"heat2015_cnt.txt",sep = ",")
write.table(heat2016_cnt,"heat2016_cnt.txt",sep = ",")
write.table(heat2017_cnt,"heat2017_cnt.txt",sep = ",")

#### word cloud ####
# 아래코드의 반복 저장
word_cloud(heat2008$titles) #워드클라우드 함수화
word_cloud(heat2009$titles)
word_cloud(heat2010$titles)

#### 전체 토탈로 corpus계산 ####
stopwords<-read.csv("C:/Users/encaion/Desktop/Rawdata/폭염/불용어.csv",header=F)
stop_words<-as.vector(stopwords[,1])
dustT.end<-wc2(data1[,1])

# tokenize on space and output as a list:
str(data1)
doc.list <- strsplit(data1[,1], "[[:space:]]+") # 드럽게 만들기 힘드네

# compute the table of terms:
term.table <- table(unlist(dustT.end))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]


#################################################################################
##  TF-IDF
# documents<-Corpus(VectorSource (dustT.end) ) #이게 문제
# documents<-tm_map(documents,PlainTextDocument)
# documents<-tm_map(documents, removePunctuation)
# documents<-lexicalize(documents)
# str(documents)
# documents.TDM=TermDocumentMatrix(documents, control=ctrl.termFreq)
#doc를 잘 분류해야함
# vocab<-vocab[,2]
# vocab<-read.csv("더위뉴스(2014.1~2017.7.15).csv")


# MCMC and model tuning parameters:
K <- 4
G <- 1000
alpha <- 0.05
eta <- 0.05

# Fit the model:
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab,
                                   num.iterations = G, alpha = alpha,
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

MovieReviews <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

options(encoding = 'UTF-8') #한글로 결과 보기 ------------------------------------------
#write.csv(MovieReviews,"미세먼지확률.csv") ##에러가 발생
# Thu Mar 22 11:01:25 2018 ------------------------------


## 하이퍼파라미터들의 결과값 저장

#tsphi<-t(phi)
#write.csv(words,"미세먼지예보_phi.csv" )
#words<-top.topic.words(fit$topics,num.words = 100,by.score = TRUE)
#document<-top.topic.documents((fit$document_sums), num.documents = 100, alpha = 0.1)  #깨져서 나옴
#words<-as.data.frame( words)
# View(fit$document_sums)
#write.csv(words,"미세먼지예보_words.csv" )
#write.csv(document,"미세먼지예보_docu_topic.csv")



# create the JSON object to feed the visualization:
json <- createJSON(phi = MovieReviews$phi,
                   theta = MovieReviews$theta,
                   doc.length = MovieReviews$doc.length,
                   vocab = MovieReviews$vocab,
                   term.frequency = MovieReviews$term.frequency, encoding='UTF-8')




serVis(json, out.dir = 'vis2', open.browser = TRUE)





###### 토픽에 따른 워드 카운트   #####
##### 위의 연산과는 별개로 다 끝나면 해야함 ####
# 모든 토픽에서 확률이 0인 단어들 제거

LDA_count<-as.data.frame( fit$topics)
LDA_count[6,]<-colnames(LDA_count)  #k+1에다 새로움 컬럼 생성
colnames(LDA_count)<-1:13425
LDA_count <-t(LDA_count)
LDA_count #수작업으로 저장
write.table(LDA_count,"LDA_cnt.txt",sep = ",")
tables<-read.delim(fileEncoding = 'utf-8',sep=",",file="LDA_cnt.txt",header=TRUE) # 이다음 txt를 엑셀로 불러와서 저장


# 폭염은 여기부터 보류
LDA_count2<-read.csv("LDA_cnt.csv",header=T)
library(sqldf)
new_LDACount<-sqldf("select *
                    from tables where  not 주제1=0 or not 주제2=0 or not 주제3=0 ")
#3개다 0인것을 제외함
write.csv(new_LDACount,"new_LDACount.csv")




######## 함수 ###### ####
word_cloud<-function(data1){
  options(encoding = 'UTF-8')
  data1 <- gsub("'", "", data1)  # remove apostrophes
  data1 <- gsub("^[[:space:]]+", " ", data1) # remove whitespace at beginning of documents
  data1 <- gsub("[[:space:]]+$", " ", data1)
  data1<-gsub("[?]"," ",data1)
  data1<-gsub("\\n","",data1)
  data1<-gsub("\\d+","",data1)
  data1<-gsub("[A-z]","",data1)
  data1<-gsub("\\^","",data1)
  data1<-gsub("[!]","",data1)
  data1<-gsub("[※]","",data1)
  data1<-gsub("[#]","",data1)
  data1<-gsub("'▶'","",data1)
  data1<-gsub("'【'","",data1)
  data1<-gsub("'】'","",data1)
  data1<-gsub("ㅇ","",data1)
  data1<-gsub("ㅎ","",data1)
  data1<-gsub("ㅗ","",data1)
  data1<-gsub("ㅜ","",data1)
  data1<-gsub("ㅣ","",data1)
  data1<-gsub("ㅏ","",data1)
  data1<-gsub("\\'","",data1)
  data1<-gsub("#","",data1)
  data1<-gsub("+","",data1)
  data1<-gsub("<","",data1)
  data1<-gsub(">","",data1)
  data1<-gsub("@","",data1)
  data1<-gsub("&","",data1)
  data1<-gsub('"',"",data1)
  data1<-gsub(";","",data1)
  data1<-gsub(","," ",data1)
  data1<-gsub("~","",data1)
  data1<-gsub("→","",data1)
  data1<-gsub('/',"",data1)
  data1<-gsub('[)]',"",data1)
  data1<-gsub('[.]',"",data1)
  data1<-gsub('[\\]',"",data1)
  data1<-gsub('[(]',"",data1)
  data1<-gsub('=',"",data1)
  data1<-gsub("ㅋ","",data1)
  data1<-gsub("[']","",data1)
  data1<-gsub('["]',"",data1)
  data1<-gsub("-","",data1)
  data1<-gsub("ㅡ","",data1)
  data1<-gsub(":","",data1)
  data1<-gsub("ㄱ","",data1)
  data1<-gsub("ㄴ","",data1)
  data1<-gsub("ㅠ","",data1)
  data1<-gsub("[!]","",data1)
  data1<-gsub("-","",data1)
  data1<-gsub("ㅡ","",data1)
  data1<-gsub(":","",data1)
  data1<-gsub("ㄱ","",data1)
  data1<-gsub("ㄴ","",data1)
  data1<-gsub("ㅠ","",data1)
  data1<-gsub("ㅁ","",data1)
  print('check1')
  data1<-extractNoun(data1,autoSpacing = T)
  data2<-unlist(data1)
  data3<-Filter(function(x){nchar(x)>=2},data2)
  print('check2')
  palete<-brewer.pal(8,"Dark2")
  wordcount2<-table(data3)
  sort(wordcount2,decreasing = T)
  wordcloud(names(wordcount2),max.words = 100,freq = wordcount2,scale=c(4,1),rot.per=0.0,random.order = FALSE,colors = palete)
  print("end_work")
}

wc<-function(data1){
  #정규식처리(특수문자 등)
  data1 <- gsub("'", "", data1)  # remove apostrophes
  data1 <- gsub("^[[:space:]]+", " ", data1) # remove whitespace at beginning of documents
  data1 <- gsub("[[:space:]]+$", " ", data1)
  data1<-gsub("[?]"," ",data1)
  data1<-gsub("\\n","",data1)
  data1<-gsub("\\d+","",data1)
  data1<-gsub("[A-z]","",data1)
  data1<-gsub("\\^","",data1)
  data1<-gsub("[!]","",data1)
  data1<-gsub("[※]","",data1)
  data1<-gsub("[#]","",data1)
  data1<-gsub("'▶'","",data1)
  data1<-gsub("'【'","",data1)
  data1<-gsub("'】'","",data1)
  data1<-gsub("ㅇ","",data1)
  data1<-gsub("ㅎ","",data1)
  data1<-gsub("ㅗ","",data1)
  data1<-gsub("ㅜ","",data1)
  data1<-gsub("ㅣ","",data1)
  data1<-gsub("ㅏ","",data1)
  data1<-gsub("\\'","",data1)
  data1<-gsub("#","",data1)
  data1<-gsub("+","",data1)
  data1<-gsub("<","",data1)
  data1<-gsub(">","",data1)
  data1<-gsub("@","",data1)
  data1<-gsub("&","",data1)
  data1<-gsub('"',"",data1)
  data1<-gsub(";","",data1)
  data1<-gsub(","," ",data1)
  data1<-gsub("~","",data1)
  data1<-gsub("→","",data1)
  data1<-gsub('/',"",data1)
  data1<-gsub('[)]',"",data1)
  data1<-gsub('[.]',"",data1)
  data1<-gsub('[\\]',"",data1)
  data1<-gsub('[(]',"",data1)
  data1<-gsub('=',"",data1)
  data1<-gsub("ㅋ","",data1)
  data1<-gsub("[']","",data1)
  data1<-gsub('["]',"",data1)
  data1<-gsub("-","",data1)
  data1<-gsub("ㅡ","",data1)
  data1<-gsub(":","",data1)
  data1<-gsub("ㄱ","",data1)
  data1<-gsub("ㄴ","",data1)
  data1<-gsub("ㅠ","",data1)
  data1<-gsub("[!]","",data1)
  data1<-gsub("-","",data1)
  data1<-gsub("ㅡ","",data1)
  data1<-gsub(":","",data1)
  data1<-gsub("ㄱ","",data1)
  data1<-gsub("ㄴ","",data1)
  data1<-gsub("ㅠ","",data1)
  data1<-gsub("ㅁ","",data1)

  #단어 수 세기
  data1<-extractNoun(data1,autoSpacing = T)
  data2<-unlist(data1)
  data3<-Filter(function(x){nchar(x)>=2},data2)
  data4<-table(data3)
  data5<-head(sort(data4,decreasing = T),1400)
  data5
} #성공

wc2<-function(data1){
  #정규식처리(특수문자 등)
  data1 <- gsub("'", "", data1)  # remove apostrophes
  data1 <- gsub("^[[:space:]]+", "", data1) # remove whitespace at beginning of documents
  data1 <- gsub("[[:space:]]+$", "", data1)
  data1<-gsub("[?]"," ",data1)
  data1<-gsub("\\n","",data1)
  data1<-gsub("\\d+","",data1)
  data1<-gsub("[A-z]","",data1)
  data1<-gsub("'^'","",data1)
  data1<-gsub("'▶'","",data1)
  data1<-gsub("'【'","",data1)
  data1<-gsub("'】'","",data1)
  data1<-gsub("[!]","",data1)
  data1<-gsub("[※]","",data1)
  data1<-gsub("[#]","",data1)
  data1<-gsub("ㅇ","",data1)
  data1<-gsub("ㅎ","",data1)
  data1<-gsub("ㅗ","",data1)
  data1<-gsub("ㅜ","",data1)
  data1<-gsub("ㅣ","",data1)
  data1<-gsub("ㅏ","",data1)
  data1<-gsub("\\^","",data1)
  data1<-gsub("#","",data1)
  data1<-gsub("+","",data1)
  data1<-gsub("<","",data1)
  data1<-gsub(">","",data1)
  data1<-gsub("@","",data1)
  data1<-gsub("&","",data1)
  data1<-gsub('"',"",data1)
  data1<-gsub(";","",data1)
  data1<-gsub(","," ",data1)
  data1<-gsub("~","",data1)
  data1<-gsub("→","",data1)
  data1<-gsub('/',"",data1)
  data1<-gsub('[)]',"",data1)
  data1<-gsub('[.]',"",data1)
  data1<-gsub('[\\]',"",data1)
  data1<-gsub('[(]',"",data1)
  data1<-gsub('=',"",data1)
  data1<-gsub("ㅋ","",data1)
  data1<-gsub("[']","",data1)
  data1<-gsub('["]',"",data1)
  data1<-gsub("-","",data1)
  data1<-gsub("ㅡ","",data1)
  data1<-gsub(":","",data1)
  data1<-gsub("ㄱ","",data1)
  data1<-gsub("ㄴ","",data1)
  data1<-gsub("ㅠ","",data1)
  data1<-gsub("[!]","",data1)
  data1<-gsub("[?]","",data1)
  data1<-gsub("-","",data1)
  data1<-gsub("ㅡ","",data1)
  data1<-gsub(":","",data1)
  data1<-gsub("ㄱ","",data1)
  data1<-gsub("ㄴ","",data1)
  data1<-gsub("ㅠ","",data1)
  data1<-gsub("ㅁ","",data1)

  #단어 수 세기
  data1<-extractNoun(data1,autoSpacing = T)
  data2<-unlist(data1)
  data3<-Filter(function(x){nchar(x)>=2},data2)
} #성공

wcBytime<-function(selected_list){
  data7<-as.data.frame(c(1:100),c(1:100)) #빈 매트릭스 생성
  for(i in 1:length(selected_list)){
    x<-unlist(selected_list[i])
    print(i)
    data1<-sapply(x,extractNoun,USE.NAMES = T)
    data2<-unlist(data1)
    print(paste0('second_chk',i)) #check2
    data1<-gsub("\\n","",data1)
    data1<-gsub("\\d+","",data1)
    data1<-gsub("[A-z]","",data1)
    data1<-gsub("'^'","",data1)
    data1<-gsub("'▶'","",data1)
    data1<-gsub("'【'","",data1)
    data1<-gsub("'】'","",data1)
    data1<-gsub("[?]","",data1)
    data1<-gsub("[!]","",data1)
    data1<-gsub("[※]","",data1)
    data1<-gsub("[#]","",data1)
    data1<-gsub("ㅇ","",data1)
    data1<-gsub("ㅎ","",data1)
    data1<-gsub("ㅗ","",data1)
    data1<-gsub("ㅜ","",data1)
    data1<-gsub("ㅣ","",data1)
    data1<-gsub("ㅏ","",data1)
    data1<-gsub("\\^","",data1)
    data1<-gsub("#","",data1)
    data1<-gsub("+","",data1)
    data1<-gsub("<","",data1)
    data1<-gsub(">","",data1)
    data1<-gsub("@","",data1)
    data1<-gsub("&","",data1)
    data1<-gsub('"',"",data1)
    data1<-gsub(";","",data1)
    data1<-gsub(","," ",data1)
    data1<-gsub("~","",data1)
    data1<-gsub("→","",data1)
    data1<-gsub('/',"",data1)
    data1<-gsub('[)]',"",data1)
    data1<-gsub('[.]',"",data1)
    data1<-gsub('[\\]',"",data1)
    data1<-gsub('[(]',"",data1)
    data1<-gsub('=',"",data1)
    data1<-gsub("ㅋ","",data1)
    data1<-gsub("[']","",data1)
    data1<-gsub('["]',"",data1)
    data1<-gsub("-","",data1)
    data1<-gsub("ㅡ","",data1)
    data1<-gsub(":","",data1)
    data1<-gsub("ㄱ","",data1)
    data1<-gsub("ㄴ","",data1)
    data1<-gsub("ㅠ","",data1)
    data1<-gsub("[!]","",data1)
    data1<-gsub("[?]","",data1)
    data1<-gsub("-","",data1)
    data1<-gsub("ㅡ","",data1)
    data1<-gsub(":","",data1)
    data1<-gsub("ㄱ","",data1)
    data1<-gsub("ㄴ","",data1)
    data1<-gsub("ㅠ","",data1)
    data1<-gsub("ㅁ","",data1)
    data3<-Filter(function(x){nchar(x)>=2},data2)
    print(paste0('third_chk',i)) #check3
    data4<-table(data3)
    data5<-head(sort(data4,decreasing = T),100)
    data6<-as.data.frame(data5)
    if(nrow(data6)==0){
      print('a')
      data6<-data.frame(AA=integer(length = 100),BB=integer(length = 100))
    }
    colnames(data6)<-c(paste0('w',i),paste0('freq',i))
    print(paste0('4th_chk',i)) #check4
    data7<-cbind(data7,data6)
    print(paste0('end',i)) #check end
  }

}   #성공
