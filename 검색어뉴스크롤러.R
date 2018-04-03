getwd()
install.packages("selectr")
library(curl)
library(rvest)
library(N2H4)
library(lubridate)

# 메인 카테고리 id 가져옵니다.
cate<-getMainCategory()
# 예시를 위해 하나만 선택합니다.
# 여기는 100(정치)를 선택했습니다.
tcate<-cate$sid1[1]
# 정치의 세부 카테고리를 가져옵니다.
subCate<-cbind(sid1=tcate,getSubCategory(sid1=tcate))
# 그중에 1번째, 2번째 세부 카테고리를 정했습니다.
tscate<-subCate$sid2[1:2]

options(stringsAsFactors = F)

success <- function(res){
  cat("Request done! Status:", res$status, "\n")
  #res$content<-iconv(rawToChar(res$content),from="CP949",to="UTF-8")
  res$content<-rawToChar(res$content)
  data <<- c(data, list(res))
}
failure <- function(msg){
  cat("Oh noes! Request failed!", msg, "\n")
}
# 개별 컨텐츠별 성공여부 확인 함수


strTime<-Sys.time()
midTime<-Sys.time()

strDate<-ymd("2004-03-15")
endDate<-ymd("2004-12-31")

# 시간 스케쥴링 관리 변수 선언 


qlist<-c("기후변화")
for (i in 1:length(qlist)){
  dir.create("./data",showWarnings=F)
  dir.create(paste0("./data/news_",qlist[i]),showWarnings=F)
  
  for (date in strDate:endDate){
    date<-as.character(as.Date(date,origin = "1970-01-01"))  # 오리진?
    dateo<-gsub("-",".",date)
    dated<-gsub("-","",date)
    print(paste0(date," / ",qlist[i], "/ start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
    midTime<-Sys.time()
    pageUrli<-paste0("https://search.naver.com/search.naver?where=news&query=",qlist[i],"&ie=utf8&sm=tab_srt&sort=1&photo=0&field=0&reporter_article=&pd=3&ds=",dateo,"&de=",dateo,"&docid=&nso=so%3Ar%2Cp%3Afrom",date,"to",date,"%2Ca%3Aall&mynews=0&mson=0&refresh_start=0&related=0")  
    trym<-0
    max<-try(getMaxPageNum(pageUrli, search=T), silent = T)
    
    
    # 끝 페이지 찾기 재시도 및 정지 (에러시 5번 이면 멈춘다.)
    while(trym<=5&&class(max)=="try-error"){
      max<-try(getMaxPageNum(pageUrli, search=T), silent = T)
      Sys.sleep(abs(rnorm(1)))
      trym<-trym+1
      print(paste0("try again max num: ",trym,"  ",pageUrli))
      date<-as.character(as.Date(date,origin = "1970-01-01"))
      
    }
    
    if(max=="no result"){
      print("no naver news links this time")
      next
    }
    
    if(class(max)=="try-error"){
      # try error시 date를 업데이트 하기로 함 
      date<-as.character(as.Date(date,origin = "1970-01-01"))
      date<-ymd(date)+days(1)
      next
    } 
    
    for (pageNum in 1:max){
      # 검색된 페이지에서 정보를 받아온다 
      start<-(pageNum-1)*10+1
      print(paste0(date," / ",qlist[i], "/ start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
      midTime<-Sys.time()
      pageUrl<-paste0(pageUrli,"&start=",start)
      tryp<-0
      newsList<-try(getUrlListByQuery(pageUrl), silent = T)
      
      while(tryp<=5&&class(newsList)=="try-error"){
        newsList<-try(getUrlListByQuery(pageUrl), silent = T)
        Sys.sleep(abs(rnorm(1)))
        tryp<-tryp+1
        print(paste0("try again max num: ",pageUrl))
      }
      if(newsList$news_links[1]=="no naver news"){ 
        print("no naver news links this time")
        next
      }
      
      if(max=="try-error"){
        print("no naver news links this time")
        next
      }
      
      
      # 여기서 부터 데이터 생성
      pool <- new_pool() # 빈 풀 생성
      data <- list()  # 빈 리스트 생성
      sapply(newsList$news_links, function(x) curl_fetch_multi(x,success,failure))
      res <- multi_run()
      # 여기 중복된 함수가 왜 있는 걸까?
      if( identical(data, list()) ){
        pool <- new_pool()
        data <- list()
        sapply(newsList$news_links, function(x) curl_fetch_multi(x,success,failure))
        res <- multi_run()
      }
      
      closeAllConnections()
      # count에 수집된 정보를 넣고 새로운 연결을 시도
      loc<-sapply(data, function(x) grepl("^http://news.naver",x$url))
      cont<-sapply(data, function(x) x$content)
      cont<-cont[loc]
      
      if(identical(cont,character(0))){ 
        print("no naver news links this time")
        next
      }
      
      # 패키지 내장함수 사용
      titles<-unlist(lapply(cont,function(x) getContentTitle(read_html(x))))
      bodies<-unlist(lapply(cont,function(x) getContentBody(read_html(x))))
      presses<-unlist(lapply(cont,function(x) getContentPress(read_html(x))))
      datetime<-lapply(cont,function(x) getContentDatetime(read_html(x))[1])
      datetime<-sapply(datetime, function(x) (as.character(x)[1]))
      edittime<-lapply(cont,function(x) getContentDatetime(read_html(x))[2])
      edittime<-sapply(edittime, function(x) (as.character(x)[1]))
      
      urls<-sapply(data, function(x) x$url)
      urls<-urls[loc]
      datC<-data.frame(titles,urls,presses,datetime,edittime,bodies)
      
      write.csv(datC, file=paste0("./data/news_",qlist[i],"/news_",date,"_",pageNum,".csv"),row.names = F, fileEncoding="UTF-8")
      
    }

    
    
  }
  
}

