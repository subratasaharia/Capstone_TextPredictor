# Subrata Saharia
# Text search and predict functions

# Searches the string and locates the database in which the string exists

MaxNGram<-5
debugcode<-FALSE
phrases<-FALSE

bannedwords<-read.csv("Terms-to-Block.csv",stringsAsFactors = FALSE)
swearwords<-read.table("swearWords.csv",stringsAsFactors = FALSE)

#Load new dictionaries (sourced from net and 1, 2gram self created)
load(".Mega1gram.RData")
load(".Mega2gram.RData")
load(".NewMega3gram.RData")
load(".NewMega4gram.RData")
load(".NewMega5gram.RData")

debugger<-function(...){
  if(debugcode)
    print(paste(...))
}

StringSearch<-function(String,...){
  
  String<-processdata(VCorpus(VectorSource(String),
                              readerControl = list(language = "eng")))
  
  debugger("Initiate:",String[[1]]$content)
  String<-String[[1]]$content
  
  if(gregexpr("\\W+",String)[[1]][1]==-1)
    Len<-0 # 1 gram dictionary, help complete the last word
  else
    Len<-length(gregexpr("\\W+",String, useBytes = TRUE)[[1]]) # identifies number of words in the string
  
  if(Len<MaxNGram){
    i<-1
    Positions<-as.integer(NULL)
    FixLen<-Len
    
    while(Len>=0){
      Positions<-DictionarySearch(String,Len)
      if(length(Positions)-1>0){
        debugger("Number of hits",length(Positions)-1) 
        break
      }
       String<-substr(String,gregexpr("\\W+",String,useBytes = TRUE)[[1]][1]+1,nchar(String))
       debugger("Backoff 1:",String)
       Len<-Len-1
    }
  }
  else if(Len>=MaxNGram){
    i<-1
    Positions<-as.integer(NULL)
    FixLen<-Len
    Len<-length(gregexpr("\\W+",String)[[1]])-(MaxNGram-1)# Reduce length to at-max N-gram
    
    while(Len<=FixLen){
      Substring<-substr(String,gregexpr("\\W+",String)[[1]][Len]+1,nchar(String))
      SubstringLen<-length(gregexpr("\\W+",Substring,useBytes = TRUE)[[1]])
      debugger("Backoff 2:",Substring)
      
      if(Len==FixLen)
        Positions<-DictionarySearch(Substring,0)
      else
        Positions<-DictionarySearch(Substring,SubstringLen)
      if(length(Positions)-1>0)
        break
      Len<-Len+1
    } 
  }
  Predicted<-Predict(Positions,...)
  Predicted
}

DictionarySearch<-function(String,Len){
  Positions<-as.integer(NULL)
  
  if(Len==0)
    Database<-MegaDictionary1gram
  else if(Len==1)
    Database<-MegaDictionary2gram
  else if(Len==2)
    Database<-MegaDictionary3gram
  else if(Len==3)
    Database<-MegaDictionary4gram
  else if(Len==4)
    Database<-MegaDictionary5gram
  
  
  if(Len<MaxNGram){
    debugger("Searching",Len+1,"-gram to complete Nth /predict N+1th word")
    Positions<-grep(paste("^",String,sep=""),as.character(Database$Token),
                    ignore.case = TRUE,useBytes = TRUE)
  }
  
  debugger("Process Completed")
  NgramPositions<-c(Positions,Len)
  NgramPositions
}

# Predicts/Completes the word with the top 5 most probable recommendation

Predict<-function(NgramPositions, countofwords,phrases){
  Ngram<-NgramPositions[length(NgramPositions)]
  Len<-length(NgramPositions)-1
  
  debugger("Number of hits in Predict",Len) 
  debugger("Count of words:", countofwords)
  
  if(Ngram==0)
    Database<-MegaDictionary1gram
  else if(Ngram==1)
    Database<-MegaDictionary2gram
  else if(Ngram==2)
    Database<-MegaDictionary3gram
  else if(Ngram==3)
    Database<-MegaDictionary4gram
  else if(Ngram==4)
    Database<-MegaDictionary5gram
  
  
  Predicted<-NULL
  Frequency<-NULL
  OutputList<-as.list(Predicted<-NULL,Freq<-NULL)
  
  i<-0
  if(Len>countofwords){
    debugger("The calculated length is:", Len)
     Len<-countofwords
    debugger("The modified length is:", Len)
  }
  
  if(!phrases){  
    while(i<Len){
      PredictedToken<-as.character(Database[NgramPositions[i+1],]$Token)
      FrequencyToken<-as.numeric(Database[NgramPositions[i+1],]$Total)
      TokenLength<-length(gregexpr("\\W+",PredictedToken)[[1]])
      PredictedWord<-substr(PredictedToken,gregexpr("\\W+",
                    PredictedToken, useBytes = TRUE)[[1]][TokenLength]+1,nchar(PredictedToken))
      Predicted<-c(Predicted,PredictedWord) 
      Frequency<-c(Frequency,FrequencyToken)
      i<-i+1
    } 
  } else {
    while(i<Len){
      Predicted<-c(Predicted,as.character(Database[NgramPositions[i+1],]$Token))      
      Frequency<-c(Frequency,as.numeric(Database[NgramPositions[i+1],]$Total))
      i<-i+1
    }
  }
  
  OutputList<-list(Predicted<-Predicted,Freq<-Frequency)
  OutputList
  
}

# Function to clean and process the data
processdata<-function(Twitter){
  
  # Remove urls
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  Twitter <- tm_map(Twitter, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
  
  # Remove words followed by @
  Twitter <- tm_map(Twitter,toSpace,"@[^\\s]+")
  
  # Remove numbers
  Twitter <- tm_map(Twitter, toSpace,"[0-9]")
  
  # Remove punctuations
  Twitter <- tm_map(Twitter, removePunctuation)
  
  # Remove all non graphical characters
  Twitter <- tm_map(Twitter,toSpace,"[^[:graph:]]")
  
  # Convert all to lowercase
  Twitter <- tm_map(Twitter,content_transformer(tolower))
  
  # Remove banned words
  
  bannedwords<-bannedwords[4:726,2]
  bannedwords<-gsub(",","",bannedwords)
  
  AllBannedWords<-unique(unlist(c(swearwords,bannedwords)))
  
  Twitter<-tm_map(Twitter,removeWords,AllBannedWords)
  
  # Remove extra whitespaces
  Twitter<-tm_map(Twitter,stripWhitespace)
  
  Twitter
}
