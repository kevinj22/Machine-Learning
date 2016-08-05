# Make corpus to DF
library(tm)
library(SnowballC)

corpustodf<-function(source){
 
  # Make the corpus
  
  trainCorpus<-Corpus(VectorSource(source))
  trainCorpus<-tm_map(trainCorpus,content_transformer(tolower))
  trainCorpus<-tm_map(trainCorpus, removePunctuation)
  trainCorpus<-tm_map(trainCorpus,removeNumbers)
  trainCorpus<-tm_map(trainCorpus,removeWords,stopwords('english'))
  trainCorpus<-tm_map(trainCorpus,stemDocument)
  trainCorpus<-tm_map(trainCorpus,stripWhitespace)
  trainCorpus<-tm_map(trainCorpus,PlainTextDocument)
  
  # Turn into a DF
  
  trainClean<-data.frame(review=sapply(trainCorpus, paste,collapse=''),stringsAsFactors = F)
  return(trainClean)
}