# Going to clean each test and lab_train individually

library(tm)
library(SnowballC)
source('corpustodf.R')
source('makefreqs.R')
source('corptoidf.R')

# Make the corpus

trainCorpus<-Corpus(VectorSource(lab_train$review))
trainCorpus<-tm_map(trainCorpus,content_transformer(tolower))
trainCorpus<-tm_map(trainCorpus, removePunctuation)
trainCorpus<-tm_map(trainCorpus,removeNumbers)
trainCorpus<-tm_map(trainCorpus,removeWords,stopwords('english'))
trainCorpus<-tm_map(trainCorpus,stemDocument)
trainCorpus<-tm_map(trainCorpus,stripWhitespace)
trainCorpus<-tm_map(trainCorpus,PlainTextDocument)

# Turn into a DF

trainClean<-data.frame(review=sapply(trainCorpus, paste,collapse=''),stringsAsFactors = F)

# Remove Corpus to save memory

rm(trainCorpus)

# Clear lab_train's cleaned reviews and replace them with the stemmed,cleaner ones

lab_train$review.clean<-NULL
lab_train$review.clean<-trainClean$review

# Remove train clean to save space
# Remove the reviews coloumn to save space

rm(trainClean)
#lab_train<-lab_train[,-3]

# Use function for test set

testClean<-corpustodf(test$review)

#  add clean reviews to test

test$review.clean<-testClean$review

# Remove testClean to save space, remove reviews column in test to save space

rm(testClean)
#test<-test[,-2]

# Do the same for unlab_train

unlabTrainClean<-corpustodf(unlab_train$review)
unlab_train$review.clean<-unlabTrainClean$review
rm(unlabTrainClean)
#unlab_train<-unlab_train[,-2]

# Lets make the frequency tables

word.pos.freq<-makefreqs(lab_train$review.clean[lab_train$sentiment==1])
word.neg.freq<-makefreqs(lab_train$review.clean[lab_train$sentiment==0])

# Lets merge together by word

freq.all<-merge(word.neg.freq,word.pos.freq,by='word',all=T)

# Remove unused data frames frames to clear space

rm(word.neg.freq)
rm(word.pos.freq)

# This leaves some NA's

freq.all$frequency.x[is.na(freq.all$frequency.x)] <- 0
freq.all$frequency.y[is.na(freq.all$frequency.y)] <- 0

# Compute the difference

freq.all$diff <- abs(freq.all$frequency.x-freq.all$frequency.y)

# Check out how it looks in order

head(freq.all[order(-freq.all$diff),])

# Lets normalize the differences to account for words seen far to frequently like movie and film

alpha<-2**7

freq.all$ndsi<- abs((freq.all$frequency.x-freq.all$frequency.y)/(freq.all$frequency.x+freq.all$frequency.y+2*alpha))

# Check out the order by ndsi now

head(freq.all[order(-freq.all$ndsi),])

# Pick a number of words to consider

num.features<-2**10

# Sort freq data

freq.all<-freq.all[order(-freq.all$ndsi),]

# Change word to character

freq.all$word<-as.character(freq.all$word)

# Build tf matrix
# Want all the reviews without sentiment together

#library(stringr)
#train<-rbind.fill(lab_train,unlab_train)
#tf<- t(apply(t(train$review.clean),2,function(x) str_count(x,train$review.clean[1:num.features])))

# Use the built in tf.idf for a prediction
library(plyr)
train<-rbind.fill(lab_train,test)
# Remove uncessary columns to save space
train<-train[,c(-1,-2)]

# Make idf matrix
tf.idf<-corptoidf(train)
tf.idf<-removeSparseTerms(tf.idf,0.998)
tf.idf<-as.matrix(tf.idf)
tf.idf<-as.data.frame(tf.idf)

# Try a random forest

library(randomForest)

# Need an index

train.labeled.ind<-1:nrow(lab_train)
test.ind<-(nrow(lab_train)+1):nrow(train)

# Make the model this will take about an hour and a half

ndsi.forest<- randomForest(tf.idf[train.labeled.ind,],as.factor(lab_train$sentiment), ntree=100)

# predict and write output

ndsi.pred <-predict(ndsi.forest, newdata=tf.idf[test.ind,],type='prob')
results<-data.frame(id=test$id,sentiment=ndsi.pred[,2])
results$id<-gsub('"','',results$id)
write.table(results,file='results.csv',sep=',',row.names=F,quote=F)
