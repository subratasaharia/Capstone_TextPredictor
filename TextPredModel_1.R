# Subrata Saharia

library(tm)
library(wordcloud)
library(RWeka)
library(dplyr)

# Build Connection
con<-file("./Coursera/Capstone/Coursera-SwiftKey/final/en_US - twitter/en_US.twitter.txt")
con<-file("./Coursera/Capstone/Coursera-SwiftKey/final/en_US - blog/en_US.blogs.txt")
con<-file("./Coursera/Capstone/Coursera-SwiftKey/final/en_US - news/en_US.news.txt")

bannedwords<-read.csv("./Coursera/Capstone/Terms-to-Block.csv",stringsAsFactors = FALSE)
swearwords<-read.table("./Coursera/Capstone/swearWords.txt",stringsAsFactors = FALSE)
Twitter<-readLines(con)
close(con)
no_words_twitter<-length(Twitter)
size_twitter<-format(object.size(Twitter), units = "Mb")

# Break the data into sets

standardchunk<-5000
ngrams<-1:4
Samples<-vector("list",length=3)
selectsample<-0.05 # After a few iterations, selecting 5% sample as the optimal sample 
                   # size
Twittersample<-sample(Twitter,no_words_twitter*selectsample)
rm(Twitter)
  
Twittersample<-processdata(VCorpus(VectorSource(Twittersample),
                                          readerControl = list(language = "eng")))

no_of_chunks<-round(length(Twittersample)/(standardchunk),0)
  

# Function to generate n tokens
tokengenerate<-function(VCorpus,n){
  Tokenizer<-function(x){NGramTokenizer(x,Weka_control(min=n, max=n))}
  dtm<-DocumentTermMatrix(VCorpus,control=list(tokenize=Tokenizer))
  dtm<-removeSparseTerms(dtm,0.9999)
  dtm
}  


# Function to create Ngrams
NgramDictionary<-function(Twittersample,n){
  TokenFreqDF<-data.frame(Token=NULL,Freq=NULL)
  j<-1
  for( i in 1:no_of_chunks){
    print(i)
    print("I am Running")
    limit<-i*standardchunk
    
      if( limit>length(Twittersample)){
          limit<-length(Twittersample)-1
          print("I am inside Limit")
          print(limit)
      }

    Twittersamplechunk<-Twittersample[j:limit]
    j<-i*standardchunk  
    dtm<-tokengenerate(Twittersamplechunk,n)
    freqmatrix<-sort(colSums(as.matrix(dtm)),decreasing = TRUE)
    TokenFreqDF<-rbind(TokenFreqDF,data.frame(Token=names(freqmatrix),Freq=freqmatrix))
  }
  print("I am out running")  
  TokenFreqDF<-TokenFreqDF%>%group_by(Token)%>%summarise(Total=sum(Freq))
  TokenFreqDF<-TokenFreqDF%>%arrange(desc(Total))%>%mutate(Prob=Total/sum(Total))
  TokenFreqDF
}

