importRatios <- function(filename,stockname){

# Don't forget to clear empty cells in Excel
# To do this select all the click find and select
# go to special and select all blank rows
# Name first row Year from 2006:2015
# Format all numbers as numbers
# Convert character to numeric on import

df<-as.data.frame(read.csv(filename,header=F,stringsAsFactors=F))
legend<-df$V1 # Save names
indx<-sapply(df,is.character)
df[indx]<-lapply(df[indx],function(x) as.numeric(as.character(x)))
df$V1<-legend # reinstate names

#Now going to tranpose
#first remember the names

n <- df[,1]

#If you transpose anything with a string in it
# All coloumns become strings 
# so transpose all but the first column (name)
df <- as.data.frame(t(df[,-1]))
colnames(df) <- n
df$myfactor <- factor(row.names(df))
row.names(df)<-NULL
str(df) # Check the column types

# Gather desired stats

df2<-as.data.frame(df$Year)
names(df2)[1]<-"Year"
df2$Revenue_CAD_Mil<-df$`Revenue CAD Mil`
df2$Net_Income_CAD_Mil <-df$`Net Income CAD Mil`
df2$Earnings_Per_Share_CAD <-df$`Earnings Per Share CAD`
df2$Dividends_CAD <-df$`Dividends CAD`
df2$Payout_Ratio <- df$`Payout Ratio %`
df2$Debt_to_Equity<- df$`Debt/Equity`
df2$Oper_Marg <- df$`Operating Margin`
df2$Net_Marg <-df$`Net Margin %`
df2$RoA <-df$`Return on Assets %`
df2$RoE <-df$`Return on Equity %`
df2$Free_Cash_Flow_Sales <- df$`Free Cash Flow/Sales %`
df2$Free_Cash_Flow_NI <- df$`Free Cash Flow/Net Income`

# Remove 2016 coloumn as not enough Data to support
df2<-df2[-11,]

# Add stock name
df2$Stock<-rep(stockname,dim(df2)[1])
return(df2)
}