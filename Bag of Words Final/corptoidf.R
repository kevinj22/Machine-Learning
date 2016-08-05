# corpus to tf.idf

corptoidf<- function(source){
    temp.corp<-Corpus(VectorSource(source))
    temp.corp<-tm_map(temp.corp,stripWhitespace)
    temp.corp<-tm_map(temp.corp,PlainTextDocument)
    tf.idf<-DocumentTermMatrix(temp.corp,control=list(weighting=function(x) weightTfIdf(x, normalize=F)))
    return(tf.idf)
}