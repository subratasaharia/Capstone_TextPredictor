con<-file("./Coursera/Coursera-SwiftKey/final/en_US/en_US.blogs.txt")

no_of_lines<-length(readLines("./Coursera/Coursera-SwiftKey/final/en_US/en_US.blogs.txt"))
no_of_lines_news<-length(readLines("./Coursera/Coursera-SwiftKey/final/en_US/en_US.news.txt"))
no_of_lines_twitter<-length(readLines("./Coursera/Coursera-SwiftKey/final/en_US/en_US.twitter.txt"))
max<-0
for( i in 1:no_of_lines)
  {
    if(nchar(readLines(con,i))>max)
    max<-nchar(readLines(con,i))
}
max

Q1/3:
  
ovid <- VCorpus(DirSource("./Coursera/Coursera-SwiftKey/final/en_US - Copy/", encoding = "UTF-8"), readerControl = 
                  list(language = "eng"))

x<-lapply(ovid,as.character)
max(nchar(x[[1]]))

love<-grep("love",x[[1]])
hate<-grep("hate",x[[1]])
