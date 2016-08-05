library(tm)
library(SnowballC)

makefreqs <- function (vectsource,sparsity=0.995){
  
  temp.corpus<-Corpus(VectorSource(vectsource))
  temp.corpus<-tm_map(temp.corpus, content_transformer(tolower))
  temp.corpus<-tm_map(temp.corpus,removeWords,stopwords('english'))
  temp.corpus<-tm_map(temp.corpus, stemDocument)
  temp.corpus<-tm_map(temp.corpus, stripWhitespace)
  temp.corpus<-tm_map(temp.corpus,PlainTextDocument)
  
  temp.dtm<-DocumentTermMatrix(temp.corpus)
  temp.dtm<-removeSparseTerms(temp.dtm, sparsity)
  temp.dtm<-as.matrix(temp.dtm)
  word.freq<-colSums(temp.dtm)
  word.freq<-sort(word.freq,decreasing=TRUE)
  word.freq<-data.frame(word=names(word.freq),frequency=word.freq)
  row.names(word.freq)<-NULL
  return(word.freq)
}

