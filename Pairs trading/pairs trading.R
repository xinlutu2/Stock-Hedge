#############################################To: Gelber############################################
##Determine hedge ratio and test a cointegrated pair trading                                     ##            
##References: http://gekkoquant.com/2013/01/21/statistical-arbitrage-trading-a-cointegrated-pair/##
###################################################################################################
library("quantmod")
library("PerformanceAnalytics")

##########Determine hedge ratio##########
a = read.csv('CMCSKA.csv')
fit = lm(a[,2]~a[,1])
summary(fit)
#########################################

backtestStartDate = as.Date("2007-01-01") #Starting date for the backtest

symbolLst<-c("CMCSA","CMCSK")
title<-c("Comcast Corporation A vs Comcast Corporation K")
symbolLst<-c("FOX","FOXA")
title<-c("FOX","FOXA")

#########################################
##symbolLst<-c("CMCSA","CMCSK")
##title<-c("Comcast Corporation A vs Comcast Corporation K")
#########################################

### SECTION 1 - Download Data & Calculate Returns ###
symbolData <- new.env() #Make a new environment for quantmod to store data in
getSymbols(symbolLst, env = symbolData, src = "yahoo", from = backtestStartDate)

stockPair <- list(
  a = coredata(Cl(eval(parse(text=paste("symbolData$\"",symbolLst[1],"\"",sep="")))))   #Stock A
  ,b = coredata(Cl(eval(parse(text=paste("symbolData$\"",symbolLst[2],"\"",sep=""))))) #Stock B
  ,hedgeRatio = 0.973068 #####Determined by linear regression
  ,name=title)

simulateTrading <- function(stockPair){
  #Generate the spread
  spread <- stockPair$a - stockPair$hedgeRatio*stockPair$b
  
  #Strategy is if the spread is greater than +/- nStd standard deviations of it's rolling 'lookback' day standard deviation
  #Then go long or short accordingly
  lookback <- 90 #look back 90 days
  nStd <- 1.5 #Number of standard deviations from the mean to trigger a trade
  
  movingAvg = rollmean(spread,lookback, na.pad=TRUE) #Moving average
  movingStd = rollapply(spread,lookback,sd,align="right", na.pad=TRUE) #Moving standard deviation / bollinger bands
  
  upperThreshold = movingAvg + nStd*movingStd
  lowerThreshold = movingAvg - nStd*movingStd
  
  aboveUpperBand <- spread>upperThreshold
  belowLowerBand <- spread<lowerThreshold
  
  aboveMAvg <- spread>movingAvg
  belowMAvg <- spread<movingAvg
  
  aboveUpperBand[is.na(aboveUpperBand)]<-0
  belowLowerBand[is.na(belowLowerBand)]<-0
  aboveMAvg[is.na(aboveMAvg)]<-0
  belowMAvg[is.na(belowMAvg)]<-0
  
  #The cappedCumSum function is where the magic happens
  #Its a nice trick to avoid writing a while loop
  #Hence since using vectorisation is faster than the while loop
  #The function basically does a cumulative sum, but caps the sum to a min and max value
  #It's used so that if we get many 'short sell triggers' it will only execute a maximum of 1 position
  #Short position - Go short if spread is above upper threshold and go long if below the moving avg
  #Note: shortPositionFunc only lets us GO short or close the position
  cappedCumSum <- function(x, y,max_value,min_value) max(min(x + y, max_value), min_value)
  shortPositionFunc <- function(x,y) { cappedCumSum(x,y,0,-1) }
  longPositionFunc <- function(x,y) { cappedCumSum(x,y,1,0) }
  shortPositions <- Reduce(shortPositionFunc,-1*aboveUpperBand+belowMAvg,accumulate=TRUE)
  longPositions <- Reduce(longPositionFunc,-1*aboveMAvg+belowLowerBand,accumulate=TRUE)
  positions = longPositions + shortPositions
  
  dev.new()
  par(mfrow=c(2,1))
  plot(movingAvg,col="red",ylab="Spread",type='l',lty=2)
  title("Comcast A vs Comcast K")
  lines(upperThreshold, col="red")
  lines(lowerThreshold, col="red")
  lines(spread, col="blue")
  legend("topright", legend=c("Spread","Moving Average","Upper Band","Lower Band"), inset = .02,
         lty=c(1,2,1,1),col=c("blue","red","red","red")) # gives the legend lines the correct color and width
  
  plot((positions),type='l')
  
  #Calculate spread daily ret
  stockPair$a - stockPair$hedgeRatio*stockPair$b
  aRet <- Delt(stockPair$a,k=1,type="arithmetic")
  bRet <- Delt(stockPair$b,k=1,type="arithmetic")
  dailyRet <- aRet - stockPair$hedgeRatio*bRet
  dailyRet[is.na(dailyRet)] <- 0
  
  tradingRet <- dailyRet * positions
  simulateTrading <- tradingRet
}

tradingRet <- simulateTrading(stockPair)


#### Performance Analysis ###
#Calculate returns for the index
indexRet <- Delt(Cl(eval(parse(text=paste("symbolData$\"",symbolLst[1],"\"",sep="")))),k=1,type="arithmetic") #Daily returns
indexRet <- as.zoo(indexRet)
tradingRetZoo <- indexRet
tradingRetZoo[,1] <- tradingRet
zooTradeVec <- as.zoo(cbind(tradingRetZoo,indexRet)) #Convert to zoo object
colnames(zooTradeVec) <- c("Comcast Corporation K vs Comcast Corporation A")
zooTradeVec <- na.omit(zooTradeVec)

#Lets see how all the strategies faired against the index
dev.new()
charts.PerformanceSummary(zooTradeVec,main="Performance of Comcast Statarb Strategy",geometric=FALSE)

cat("Sharpe Ratio")
print(SharpeRatio.annualized(zooTradeVec))


