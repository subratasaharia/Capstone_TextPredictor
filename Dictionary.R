# Subrata Saharia
# Discrete Scripts used to create N-gram dictionaries
# Not to be run in one go

#Houseclean functions - 1 Gram
Dictionary<-lapply(Samples,NgramDictionary,1)
save(Dictionary,file="./Coursera/Capstone/.Dictionary")
load("./Coursera/Capstone/.Dictionary")
Dictionary<-Dictionary[[2]]
save(Dictionary,file="./Coursera/Capstone/.Dictionary.RData")
load("./Coursera/Capstone/.Dictionary.RData")

DictionaryBlog<-NgramDictionary(Twittersample,1)
save(DictionaryBlog,file="./Coursera/Capstone/.DictionaryBlog.RData")
load("./Coursera/Capstone/.DictionaryBlog.RData")

DictionaryNews<-NgramDictionary(Twittersample,1)
save(DictionaryNews,file="./Coursera/Capstone/.DictionaryNews.RData")
load("./Coursera/Capstone/.DictionaryNews.RData")


#Houseclean functions - 2 Gram
Dictionary2gram<-NgramDictionary(Twittersample, 2)
save(Dictionary2gram,file="./Coursera/Capstone/.Dictionary2gram.RData")
load("./Coursera/Capstone/.Dictionary2gram.RData")

Dictionary2gramBlog<-NgramDictionary(Twittersample, 2)
save(Dictionary2gramBlog,file="./Coursera/Capstone/.Dictionary2gramBlog.RData")
load("./Coursera/Capstone/.Dictionary2gramBlog.RData")

Dictionary2gramNews<-NgramDictionary(Twittersample, 2)
save(Dictionary2gramNews,file="./Coursera/Capstone/.Dictionary2gramNews.RData")
load("./Coursera/Capstone/.Dictionary2gramNews.RData")

#Houseclean functions - 3 Gram
Dictionary3gram<-NgramDictionary(Twittersample, 3)
save(Dictionary3gram,file="./Coursera/Capstone/.Dictionary3gram.RData")
load("./Coursera/Capstone/.Dictionary3gram.RData")

Dictionary3gramBlog<-NgramDictionary(Twittersample, 3)
save(Dictionary3gramBlog,file="./Coursera/Capstone/.Dictionary3gramBlog.RData")
load("./Coursera/Capstone/.Dictionary3gramBlog.RData")

Dictionary3gramNews<-NgramDictionary(Twittersample, 3)
save(Dictionary3gramNews,file="./Coursera/Capstone/.Dictionary3gramNews.RData")
load("./Coursera/Capstone/.Dictionary3gramNews.RData")

#Houseclean functions - 4 Gram
Dictionary4gram<-NgramDictionary(Twittersample, 4)
save(Dictionary4gram,file="./Coursera/Capstone/.Dictionary4gram.RData")
load("./Coursera/Capstone/.Dictionary4gram.RData")

Dictionary4gramBlog<-NgramDictionary(Twittersample, 4)
save(Dictionary4gramBlog,file="./Coursera/Capstone/.Dictionary4gramBlog.RData")
load("./Coursera/Capstone/.Dictionary4gramBlog.RData")

Dictionary4gramNews<-NgramDictionary(Twittersample, 4)
save(Dictionary4gramNews,file="./Coursera/Capstone/.Dictionary4gramNews.RData")
load("./Coursera/Capstone/.Dictionary4gramNews.RData")

#House clean function 5-Gram [Using N-gram tables from the net]
#Davies, Mark. (2011) N-grams data from the Corpus of Contemporary American English 
#(COCA). Downloaded from http://www.ngrams.info on June 9, 2017. 

DictionaryReCreate<-function(N){
  
  input<-paste("./Coursera/Capstone/w",N,"_/w",N,"_.txt",sep="")
  MegaDictionary<-paste("MegaDictionary",N,"gram",sep="")
  filename<-paste("./Coursera/Capstone/",".NewMega",N,"gram.RData",sep="")
  MegaDictionary<-read.table(input)
  
  if(N==2) {
  MegaDictionary<-MegaDictionary%>%
    mutate(Token=paste(V2,V3,sep=" "),Total=V1)
  MegaDictionary<-MegaDictionary%>%arrange(desc(Total))
  MegaDictionary2gram<-MegaDictionary[,4:5]
  save(MegaDictionary2gram,file=filename)
  }
  else if(N==3) {
    MegaDictionary<-MegaDictionary%>%
      mutate(Token=paste(V2,V3,V4,sep=" "),Total=V1)
    MegaDictionary<-MegaDictionary%>%arrange(desc(Total))
    MegaDictionary3gram<-MegaDictionary[,5:6]
    save(MegaDictionary3gram,file=filename)
  }
  else if(N==4) {
    MegaDictionary<-MegaDictionary%>%
      mutate(Token=paste(V2,V3,V4,V5,sep=" "),Total=V1)
    MegaDictionary<-MegaDictionary%>%arrange(desc(Total))
    MegaDictionary4gram<-MegaDictionary[,6:7]
    save(MegaDictionary4gram,file=filename)
  }
  else if(N==5) {
    MegaDictionary<-MegaDictionary%>%
      mutate(Token=paste(V2,V3,V4,V5,V6,sep=" "),Total=V1)
    MegaDictionary<-MegaDictionary%>%arrange(desc(Total))
    MegaDictionary5gram<-MegaDictionary[,7:8]
    save(MegaDictionary5gram,file=filename)
  }
  
}

# Aggregate the dictionaries into one mega dictionary for each n-gram

load("./Coursera/Capstone/.Dictionary.RData")
load("./Coursera/Capstone/.DictionaryBlog.RData")
load("./Coursera/Capstone/.DictionaryNews.RData")
MegaDictionary1gram<-rbind(Dictionary, DictionaryBlog,DictionaryNews)
MegaDictionary1gram<-MegaDictionary1gram%>%group_by(Token)%>%summarise(Total=sum(Total))%>%arrange(desc(Total))
save(MegaDictionary1gram,file="./Coursera/Capstone/.Mega1gram.RData")


load("./Coursera/Capstone/.Dictionary2gram.RData")
load("./Coursera/Capstone/.Dictionary2gramBlog.RData")
load("./Coursera/Capstone/.DictionaryNews2gram.RData")
MegaDictionary2gram<-rbind(Dictionary2gram, Dictionary2gramBlog,DictionaryNews2gram)
MegaDictionary2gram<-MegaDictionary2gram%>%group_by(Token)%>%summarise(Total=sum(Total))%>%arrange(desc(Total))
save(MegaDictionary2gram,file="./Coursera/Capstone/.Mega2gram.RData")

load("./Coursera/Capstone/.Dictionary3gram.RData")
load("./Coursera/Capstone/.Dictionary3gramBlog.RData")
load("./Coursera/Capstone/.DictionaryNews3gram.RData")
MegaDictionary3gram<-rbind(Dictionary3gram, Dictionary3gramBlog,DictionaryNews3gram)
MegaDictionary3gram<-MegaDictionary3gram%>%group_by(Token)%>%summarise(Total=sum(Total))%>%arrange(desc(Total))
save(MegaDictionary3gram,file="./Coursera/Capstone/.Mega3gram.RData")

load("./Coursera/Capstone/.Dictionary4gram.RData")
load("./Coursera/Capstone/.Dictionary4gramBlog.RData")
load("./Coursera/Capstone/.DictionaryNews4gram.RData")
MegaDictionary4gram<-rbind(Dictionary4gram, Dictionary4gramBlog,DictionaryNews4gram)
MegaDictionary4gram<-MegaDictionary4gram%>%group_by(Token)%>%summarise(Total=sum(Total))%>%arrange(desc(Total))
save(MegaDictionary4gram,file="./Coursera/Capstone/.Mega4gram.RData")

# Load the dictionaries 
load("./Coursera/Capstone/.Mega1gram.RData")
load("./Coursera/Capstone/.Mega2gram.RData")
load("./Coursera/Capstone/.Mega3gram.RData")
load("./Coursera/Capstone/.Mega4gram.RData")

#Load new dictionaries (sourced from net and 1, 2gram self created)
load("./Coursera/Capstone/.Mega1gram.RData")
load("./Coursera/Capstone/.Mega2gram.RData")
load("./Coursera/Capstone/.NewMega3gram.RData")
load("./Coursera/Capstone/.NewMega4gram.RData")
load("./Coursera/Capstone/.NewMega5gram.RData")


#Davies, Mark. (2011) N-grams data from the Corpus of Contemporary American English (COCA). 
#Downloaded from http://www.ngrams.info on June 9, 2017. 

