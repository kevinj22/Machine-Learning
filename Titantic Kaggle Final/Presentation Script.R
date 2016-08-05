# To run all hit CTRL + ALT + R
# Plots and other data should generate, check bottom right hand corner for the images under the plots tab
# If you wish to clear everything to try from scratch type rm(list=ls())


# Get the Data

train<-read.csv('train.csv',stringsAsFactors = F)
test<-read.csv('test.csv', stringsAsFactors= F)

# Find where na's are

which(is.na(train))
which(is.na(test))

# Further investigate in which coloumns the NA's reside

colnames(train)[colSums(is.na(train))>0]

# Should bind them together and do some computations to determine
# Best values to fill in with

dim(train)
dim(test)

# Need to add a temp survived coloumn to test to bind together

test$Survived<-rep(NA,dim(test)[1])

# Bind data frames together

all_data<-rbind(train,test)

# Find which coloumns of All Data require replacement

colnames(all_data)[colSums(is.na(all_data))>0]

# Before further analysis we should seperate title from names

str(all_data)

# Names is a factor should change to char

all_data$Name<-as.character(all_data$Name)

# Ready to seperate title from name

all_data$Title <- sapply(all_data$Name, FUN=function(x) {strsplit(x,split='[,.]')[[1]][2]})
all_data$Title <- sub(' ','',all_data$Title)

# Looking at the Titles there are quite a few
# When split into the test and train set they become inconsistent (not the same titles)
# Should merge them to insure both have same titles

all_data$Title[all_data$Title %in% c('Capt','Don','Major','Col','Sir')]<-'Sir'
all_data$Title[all_data$Title %in% c('Dona','Lady','the Countess','Jonkheer')]<-'Lady'
all_data$Title[all_data$Title %in% c('Ms','Mlle','Mme')] <-'Mlle'
all_data$Title<-as.factor(all_data$Title)

# Lets look at Fare

which(is.na(all_data$Fare))
all_data$Fare[1044]<-median(all_data$Fare,na.rm=T)

# Some fares are repeated for family purchases
# Break up fare as a per person "price" to help predict survivors and deck

ticket.count <-aggregate(all_data$Ticket, by=list(all_data$Ticket), function(x) sum(!is.na(x)))
all_data$Price<-apply(all_data, 1, function(x) as.numeric(x["Fare"]) / ticket.count[which(ticket.count[, 1] == x["Ticket"]), 2])

# Make data frame information into factors

all_data$Pclass<-as.factor(all_data$Pclass)
all_data$Ticket<-as.factor(all_data$Ticket)
all_data$Sex<-as.factor(all_data$Sex)
all_data$SibSp<-as.factor(all_data$SibSp)
all_data$Embarked<-as.factor(all_data$Embarked)


# Survived, Age and Fare contain NA's
# Lets determine the best age to replace with using random forests

library(rpart)
set.seed(129)
# Currently done on PRICE instead of Fare ****

predicted_age <-rpart(Age~ Pclass + Sex + SibSp + Parch + Price + Ticket + Title + Embarked,data=all_data[!is.na(all_data$Age),],method="anova")
all_data$Age[is.na(all_data$Age)]<-predict(predicted_age, all_data[is.na(all_data$Age),])

# Add a Mother and Child Class

#Adding Mother

all_data$Mother<-0
all_data$Mother[all_data$Sex=='female' & all_data$Parch>0 & all_data$Age>18 & all_data$Title!='Miss']<-1
all_data$Mother<-as.factor(all_data$Mother)

#Adding Child

all_data$Child<-0
all_data$Child[all_data$Parch>0 & all_data$Age<=18]<-1
all_data$Child<-as.factor(all_data$Child)

#FamilyId2

Surname<-sapply(all_data$Name,function(x) strsplit(x,'[.,]')[[1]][1])
FamilyId<-paste0(all_data$FamilySize,Surname)
all_data$FamilyId<-factor(FamilyId)
Family<-data.frame(table(FamilyId))
SmallFamily<-Family$FamilyId[Family$Freq<=2]
FamilyId[FamilyId %in% SmallFamily]<-'Small'
all_data$FamilyId2<-factor(FamilyId)



#Exact Deck from Cabin number

cabin<-as.matrix(all_data$Cabin, stringsAsFactors=F)

for (i in 1:length(cabin)){
  cabin[i]<-strsplit(cabin,'')[[i]][1]
}

all_data$Deck<-cabin
#all_data$Deck<-sapply(all_data$Cabin, function(x) strsplit(x,NULL)[[1]][1])

# Currently done on PRICE instead of Fare ***
deck.fit<-rpart(Deck~Pclass+Price,data=all_data[!is.na(all_data$Deck),])
all_data$Deck[is.na(all_data$Deck)]<-as.character(predict(deck.fit,all_data[is.na(all_data$Deck),],type='class'))
all_data$Deck[is.na(all_data$Deck)]<-'UNK'
all_data$Deck<-as.factor(all_data$Deck)

# Check if any NA left

which(is.na(all_data$Name))

# Get train and test back
# Remember we set the test to have NA for all survived

test<- as.data.frame(all_data[is.na(all_data$Survived),])
train <-as.data.frame(all_data[!is.na(all_data$Survived),])

# Predictions on Data
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

# set seeds
set.seed(100)

# Apply random Forest

tree <- rpart(Survived ~ Age + Pclass + Price + SibSp + Title + Sex +Deck +Mother +Child, data=train, method="class")
pred <- predict(tree,test,type="class")
fancyRpartPlot(tree)

# change title to factor before randomforest doesn't work very well with characters
# Currently done on Price VS FARE ***
forest <-randomForest(as.factor(Survived)~Age + Pclass + Price + SibSp + Title + Mother + Child + Deck+ Sex + Embarked ,data=train, importance=TRUE,ntree=2000)
varImpPlot(forest)

# Use cforest as it works with factors larger than 32 i.e familyid
library(party)
condforest<-cforest(as.factor(Survived)~Age + Pclass + Price + SibSp + Title + Mother + Child + Sex + Embarked+Deck+FamilyId2, data=train, controls=cforest_unbiased(ntree=1000,mtry=3))
prediction <-predict(condforest,test,OOB=TRUE,type='response')
submit<-data.frame(PassengerId = test$PassengerId,Survived=prediction)
write.csv(submit,file="firstforest.csv",row.names=F)
