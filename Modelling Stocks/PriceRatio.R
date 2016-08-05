# Format to merge price with ratios
library(quantmod)
library(lubridate)

head<-c("Open","High","Low","Close","Volume","Adjusted")

# Must have ratios pre prepared and named properly
# Use the data base names, no quotes

Price_Ratio_Merge <- function(stockname,stocknameratios){
  stockname<-as.data.frame(stockname)
  names(stockname)<-head
  stockname$Dates<-row.names(stockname)
  row.names(stockname)<-NULL
  stockname$Dates<-as.Date(stockname$Dates)
  stockname$Year<-year(stockname$Dates)
  df<-merge(stockname,stocknameratios)
  df2<-as.data.frame(df$Year)
  names(df2)[1]<-"Year"
  df2$Volume<-df$Volume
  df2$Close<-df$Close
  df2$Revenue_CAD_Mil<-df$Revenue_CAD_Mil
  df2$Net_Income_CAD_Mil <-df$Net_Income_CAD_Mil
  df2$Earnings_Per_Share_CAD <-df$Earnings_Per_Share_CAD
  df2$Dividends_CAD <-df$Dividends_CAD
  df2$Debt_to_Equity<- df$Debt_to_Equity
  df2$Net_Marg <-df$Net_Marg
  df2$RoA <-df$RoA
  df2$RoE <-df$RoE
  return(df2)
}