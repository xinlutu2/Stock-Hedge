#############################################To: Gelber##################################################
##Equity portfolio dnamic hedging                                                                      ##
##References: https://rstudio-pubs-static.s3.amazonaws.com/105565_bbcb9c5c89b042128ca7329e215e36e6.html##
#########################################################################################################

library(quantmod)
library(ggplot2)

getSymbols("KLAC",src="google")
getSymbols("LRCX",src="google")
getSymbols("TXN",src="google")

##### Determind best hedge ratios between three stocks
min_variance_portfolio <-function(er, covmat, target.return)
{
  ones <- rep(1, length(er))
  top <- cbind(2*covmat, er, ones)
  bot <- cbind(rbind(er, ones), matrix(0,2,2))
  A <- rbind(top, bot)
  b.target <- as.matrix(c(rep(0, length(er)), target.return, 1))
  x <- solve(A, b.target)
  w <- x[1:length(er)]
}

data_KLAC<-data.frame(KLAC)
data_KLAC$Date<-as.Date(rownames(data_KLAC))
data_KLAC$Daily_Return=c(NA,diff(log(data_KLAC$KLAC.Close)))
names(data_KLAC)<-c("Open","High","Low","Close","Volume","Date","Daily_Return")
rownames(data_KLAC) <- NULL
data_KLAC<-subset(data_KLAC,Date>=as.Date("2007-01-01")&Date<=as.Date("2013-12-31"),select=c("Date","Open","Close","Daily_Return"))

data_LRCX<-data.frame(LRCX)
data_LRCX$Date<-as.Date(rownames(data_LRCX))
data_LRCX$Daily_Return=c(NA,diff(log(data_LRCX$LRCX.Close)))
names(data_LRCX)<-c("Open","High","Low","Close","Volume","Date","Daily_Return")
rownames(data_LRCX) <- NULL
data_LRCX<-subset(data_LRCX,Date>=as.Date("2007-01-01")&Date<=as.Date("2013-12-31"),select=c("Date","Open","Close","Daily_Return"))

data_TXN<-data.frame(TXN)
data_TXN$Date<-as.Date(rownames(data_TXN))
data_TXN$Daily_Return=c(NA,diff(log(data_TXN$TXN.Close)))
names(data_TXN)<-c("Open","High","Low","Close","Volume","Date","Daily_Return")
rownames(data_TXN) <- NULL
data_TXN<-subset(data_TXN,Date>=as.Date("2007-01-01")&Date<=as.Date("2013-12-31"),select=c("Date","Open","Close","Daily_Return"))

KLAC1<-subset(data_KLAC,Date>=as.Date("2007-01-01")&Date<=as.Date("2013-12-31"))
LRCX1<-subset(data_LRCX,Date>=as.Date("2007-01-01")&Date<=as.Date("2013-12-31"))
TXN1<-subset(data_TXN,Date>=as.Date("2007-01-01")&Date<=as.Date("2013-12-31"))

KLAC1$Share<-3333.3/KLAC1$Open[1]
LRCX1$Share<-3333.3/LRCX1$Open[1]
TXN1$Share<-3333.3/TXN1$Open[1]

KLAC1$Value<-KLAC1$Share*KLAC1$Close
LRCX1$Value<-LRCX1$Share*LRCX1$Close
TXN1$Value<-TXN1$Share*TXN1$Close

Value<-KLAC1$Value+LRCX1$Value+TXN1$Value

mean(diff(log(Value)))*252/(sd(diff(log(Value)))*sqrt(252))

##########
r_target<-function(data_klac,data_lrcx,data_txn,date)
{
  d1<-subset(data_klac,Date>=(date-30)&Date<=date)
  d2<-subset(data_lrcx,Date>=(date-30)&Date<=date)
  d3<-subset(data_txn,Date>=(date-30)&Date<=date)
  
  d1$Share<-3333.3/d1$Open[1]
  d2$Share<-3333.3/d2$Open[1]
  d3$Share<-3333.4/d3$Open[1]
  
  d1$Value<-d1$Share*d1$Close
  d2$Value<-d2$Share*d2$Close
  d3$Value<-d3$Share*d3$Close
  Value<-d1$Value+d2$Value+d3$Value
  
  mean(diff(log(Value)))
}
###########
return_30<-function(data_set,date)
{
  d<-subset(data_set,Date>=(date-30)&Date<=date)
  d$Daily_Return
}

start=nrow(subset(data_TXN,Date<=as.Date("2010-12-31")))+1
data_KLAC$share<-NA
data_LRCX$share<-NA
data_TXN$share<-NA
value=rep(NA,nrow(data_TXN))
value[start-1]=9000
w<-list(nrow(data_TXN)-start+1)
for(i in start:nrow(data_TXN))
  #Each step of the for-loop is a trLRCXng day, and the hedge ratio is updated on every step. 
{
  if (weekdays(data_TXN$Date[i])=="Monday")
  {
    rTarget<-r_target(data_KLAC,data_LRCX,data_TXN,data_KLAC$Date[i])
    
    #Historical returns
    r1=return_30(data_KLAC,data_KLAC$Date[i])
    r2=return_30(data_LRCX,data_LRCX$Date[i])
    r3=return_30(data_TXN,data_TXN$Date[i])
    
    #Expected returns
    r=c(mean(r1),mean(r2),mean(r3))
    
    #Covarian Matrix
    cov_matrix=var(cbind(r1,r2,r3))
    
    #Find the hedge ratio
    w[[i-start+1]]=min_variance_portfolio(r, cov_matrix, rTarget)
    
    data_KLAC$share[i]=value[i-1]*w[[i-start+1]][1]/data_KLAC$Open[i]
    data_LRCX$share[i]=value[i-1]*w[[i-start+1]][2]/data_LRCX$Open[i]
    data_TXN$share[i]=value[i-1]*w[[i-start+1]][3]/data_TXN$Open[i]
    
    value[i]=data_KLAC$share[i]*data_KLAC$Close[i]+data_LRCX$share[i]*data_LRCX$Close[i]+data_TXN$share[i]*data_TXN$Close[i]
  }
  else
  {
    data_KLAC$share[i]=data_KLAC$share[i-1]
    data_LRCX$share[i]=data_LRCX$share[i-1]
    data_TXN$share[i]=data_TXN$share[i-1]
    value[i]=data_KLAC$share[i]*data_KLAC$Close[i]+data_LRCX$share[i]*data_LRCX$Close[i]+data_TXN$share[i]*data_TXN$Close[i]
    w[[i-start+1]]=c(data_KLAC$share[i]*data_KLAC$Open[i]/value[i],data_LRCX$share[i]*data_LRCX$Open[i]/value[i],data_TXN$share[i]*data_TXN$Open[i]/value[i])
  }
}

mean(diff(log(value[!is.na(value)])))*252/(sd(diff(log(value[!is.na(value)])))*sqrt(252))

w1<-rep(0,length(start:nrow(data_TXN)))
w2<-rep(0,length(start:nrow(data_TXN)))
w3<-rep(0,length(start:nrow(data_TXN)))

for(i in 1:length(start:nrow(data_TXN)))
{
  w1[i]<-w[[i]][1]
  w2[i]<-w[[i]][2]
  w3[i]<-w[[i]][3]
}
Date= data_TXN$Date[start:nrow(data_TXN)]
test_data <- data.frame(w1,w2,w3,Date)

ggplot(test_data, aes(Date)) + 
  geom_line(aes(y = w1, colour = "w1")) + 
  geom_line(aes(y = w2, colour = "w2")) +
  geom_line(aes(y = w3, colour = "w3")) 


