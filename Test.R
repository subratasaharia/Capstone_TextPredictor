# Subrata Saharia
# Script to test accuracy of the algorithm in text prediction on a sample of
# 30 randomly selected texts from the web

library(tm)
library(wordcloud)
library(RWeka)
library(dplyr)

con<-file("./Coursera/Capstone/Coursera-SwiftKey/final/en_US - news/en_US.news.txt")
bannedwords<-read.csv("./Coursera/Capstone/Terms-to-Block.csv",stringsAsFactors = FALSE)
swearwords<-read.table("./Coursera/Capstone/swearWords.txt",stringsAsFactors = FALSE)
Twitter<-readLines(con)
close(con)
no_words_twitter<-length(Twitter)

selectsample<-0.001
Twittersample<-sample(Twitter,no_words_twitter*selectsample)

Twittersample<-processdata(VCorpus(VectorSource(Twittersample),
                                   readerControl = list(language = "eng")))
Testsample<-sample(1:length(Twittersample),30)

Testdata<-sapply(Testsample,x<-function(Testsample){Twittersample[[Testsample]]$content})

Teststring<-sapply(Testdata,function(Testdata){substr(Testdata,1,
                                                      gregexpr("\\W+",Testdata)[[1]][length(gregexpr("\\W+",Testdata)[[1]])])})

Teststring<-unname(Teststring)
Output<-sapply(Testdata,function(Testdata){substr(Testdata,
                                                  gregexpr("\\W+",Testdata)[[1]][length(gregexpr("\\W+",Testdata)[[1]])]+1,nchar(Testdata))})
Output<-unname(Output)

PredictString<-sapply(Teststring,StringSearch)
PredictString<-unname(PredictString)

Testdatatable<-as.data.frame(cbind(Testdata,Teststring,Output,PredictString),stringsAsFactors = FALSE)

Accuracy<-as.logical(NULL)
  for(i in 1:length(Testsample)){
    Detected<-0
      Outcome<-grepl(Output[i],PredictString[i][[1]])
      for(i in 1:length(Outcome)){
        if(isTRUE(Outcome[i])){
          Detected<-1
          break
        }
      }
      
      if(Detected>0)
        Accuracy<-c(Accuracy,TRUE)
       else
        Accuracy<-c(Accuracy,FALSE)
  }

print(sum(Accuracy)/length(Accuracy))