#install.packages("lubridate")
#install.packages("xts")
#install.packages("highfrequency")
#install.packages("quantmod")
#install.packages("tseries")
#install.packages("fArma")
#install.packages("zoo")
#install.packages("vcd")
#install.packages("MASS")
#install.packages("IndependenceTests")
#install.packages("ACDm")
#install.packages("scales")
#install.packages("openair")
#install.packages("fGarch")
library(lubridate)
library(xts)
library(TTR)
library(quantmod)
library(highfrequency)
library(tseries)
library(zoo)
library(vcd)
library(MASS)
library(scales)
library(ACDm)
library(openair)
library(fGarch)
#par reset
resetPar=function() {
  dev.new()
  op=par(no.readonly = TRUE)
  dev.off()
  op
}
#set the working directory
setwd("C:/Users/Charmaine/Desktop/quantitative trading strategy/assignment 1")
#Question 1
#read the csv file
MSFT=read.csv("MSFT.csv",header=TRUE)

#stadarize date and time format and turn it into char
MSFT$DATE=as.character(ymd(MSFT$DATE))
MSFT$TIME=as.character(MSFT$TIME)

#create a new column to store the time
MSFT$NEWTIME=strptime(paste(MSFT$DATE,MSFT$TIME),format="%Y-%m-%d %H:%M:%S",tz="GMT")

#create xts 
rawdata=xts(MSFT[3:5], order.by=MSFT[,6])

#Use only the data for normal trading hours: 9.30am ¨C 4pm
data=exchangeHoursOnly(rawdata, daybegin="09:30:00", dayend = "15:59:59")

#one second merge
OneSecMerge=mergeTradesSameTimestamp(data,selection="weightedaverage")
plot.ts(OneSecMerge$PRICE,main="Price Series",ylab="Price")


#adf.test
adf.test(OneSecMerge$PRICE, alternative = c("stationary", "explosive"),
         k = trunc((length(OneSecMerge$PRICE)-1)^(1/3)))
adf.test(diff(as.numeric(OneSecMerge$PRICE),differences=1), 
         alternative = c("stationary", "explosive"),
         k = trunc((length(OneSecMerge$PRICE)-1)^(1/3)))

#acf.test
acf(as.numeric(OneSecMerge$PRICE),main="acf")#nonstationay 
acf(diff(as.numeric(OneSecMerge$PRICE),differences=1),main="acf")

#pacf.test
pacf(as.numeric(OneSecMerge$PRICE),main="pacf")
pacf(diff(as.numeric(OneSecMerge$PRICE),differences=1),main="pacf")

#arma model fit
fit=arma(diff(as.numeric(OneSecMerge$PRICE)), order = c(1, 1), include.intercept = TRUE)
summary(fit)

returns=makeReturns(OneSecMerge$PRICE)

#autocorrelation test
acf(returns,plot=FALSE)
res=residuals(arma(returns, order = c(1, 0), include.intercept = TRUE))
Box.test(res, lag = 1, type ="Box-Pierce",fitdf = 0)
Box.test(res, lag = 1, type ="Ljung",fitdf=0)



#Duration
fit=data.frame(as.character(MSFT$NEWTIME),MSFT$PRICE,MSFT$SIZE)
colnames(fit)=c("time","price","volume")
Dur=diurnalAdj(computeDurations(fit,open = "09:30:00", close = "15:59:59"), 
               aggregation = "none", method = "supsmu")
NewDur=Dur$durations[(-which.max(Dur$durations))]
plot.ts(Dur$durations)
plot.ts(NewDur)
hist(NewDur, freq = FALSE, breaks = 100, xlim = c(1, quantile(Dur$durations,0.99)),
     main="histogram of duration")

# estimate the parameters
fit=fitdistr(NewDur, "exponential") 

# goodness of fit test
ks.test(jitter(NewDur), "pexp", fit$estimate) # p-value <0.05 distribution refused

# plot a graph
curve(dexp(x, rate=fit$estimate),col="red",add=TRUE)

#independence test
adf.test(Dur$durations, alternative = c("stationary", "explosive"),
         k = trunc((length(Dur$durations)-1)^(1/3)))
acf(as.numeric(Dur$durations),main="acf")
pacf(as.numeric(Dur$durations),main="pacf")


#duration monday friday
Durts=as.xts(as.numeric(diff(index(OneSecMerge))),order.by=index(OneSecMerge[(-1)]))
a=split(Durts, f = "days", drop=TRUE, k = 1)
names(a)=c("2nd","3rd","4th","7th","8th","9th","10th","11th")
par(mfrow=c(3,1))
par(cex = 0.6)
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 3, 3))
plot.ts(a$`7th`[(-1)],ylab="Monday")#Monday
plot.ts(a$`4th`[(-1)])#Friday
plot.ts(a$`11th`[(-1)])#Friday
par(resetPar())

#Question 2
TimeMerge=function(x,y,t)
{EP=endpoints(x,on="minutes",k=t)
f=data.frame(
  aggregatets(x$PRICE,FUN="mean",on="minutes",k=t,weights=x$SIZE,dropna=TRUE),
  as.numeric(period.apply(x$PRICE,EP,FUN=first)),
  period.max(x$PRICE,EP),
  period.min(x$PRICE,EP),
  as.numeric(period.apply(x$PRICE,EP,FUN=last)),
  period.sum(x$SIZE,EP),
  as.numeric(period.apply(y,endpoints(y,on="minutes",k=t),FUN=nrow)))
colnames(f)=c("Price","Open","High","Low","Close","Volume","Num.Trades")
return(f)
}
OneM=TimeMerge(OneSecMerge,data,1)
FiveM=TimeMerge(OneSecMerge,data,5)
OneH=TimeMerge(OneSecMerge,data,60)


#plot VWAP Time Series
ts.plot(OneM$Price,main="1 Min Price Series",ylab="Price")
ts.plot(FiveM$Price,main="5 Min Price Series",ylab="Price")
ts.plot(OneH$Price,main="1 Hour Price Series",ylab="Price")

#test for stionarity
adf.test(OneM$Price, alternative = c("stationary", "explosive"),
         k = trunc((length(OneM$Price)-1)^(1/3)))
adf.test(diff(as.numeric(OneM$Price),differences=1), 
         alternative = c("stationary", "explosive"),
         k = trunc((length(OneM$Price)-1)^(1/3)))

adf.test(FiveM$Price, alternative = c("stationary", "explosive"),
         k = trunc((length(FiveM$Price)-1)^(1/3)))
adf.test(diff(as.numeric(FiveM$Price),differences=1), 
         alternative = c("stationary", "explosive"),
         k = trunc((length(FiveM$Price)-1)^(1/3)))

adf.test(OneH$Price, alternative = c("stationary", "explosive"),
         k = trunc((length(OneH$Price)-1)^(1/3)))
adf.test(diff(as.numeric(OneH$Price),differences=1), 
         alternative = c("stationary", "explosive"),
         k = trunc((length(OneH$Price)-1)^(1/3)))

#ACF & PACF test
acf(as.numeric(OneM$Price),main="1 Min acf")
acf(diff(as.numeric(OneM$Price),differences=1),main="diff 1 Min acf")
pacf(as.numeric(OneM$Price),main="1 Min pacf")
pacf(diff(as.numeric(OneM$Price),differences=1),main="diff 1 Min pacf")

acf(as.numeric(FiveM$Price),main="5 Min acf")
acf(diff(as.numeric(FiveM$Price),differences=1),main="diff 5 Min acf")
pacf(as.numeric(FiveM$Price),main="5 Min pacf")
pacf(diff(as.numeric(FiveM$Price),differences=1),main="diff 5 Min pacf")

acf(as.numeric(OneH$Price),main="1 Hour acf")
acf(diff(as.numeric(OneH$Price),differences=1),main="diff 1 Hour acf")
pacf(as.numeric(OneH$Price),main="1 Hour pacf")
pacf(diff(as.numeric(OneH$Price),differences=1),main="diff 1 Hour pacf")

#arma model fit
fit=arma(diff(as.numeric(OneSecMerge$PRICE)), order = c(0,1), include.intercept = TRUE)
summary(fit)

#autocorrelation test
OneM=as.xts(OneM)
OneMreturns=makeReturns(OneM$Price)
acf(OneMreturns)

FiveM=as.xts(FiveM)
FiveMreturns=makeReturns(FiveM$Price)
acf(FiveMreturns)

OneH=as.xts(OneH)
OneHreturns=makeReturns(OneH$Price)
acf(OneHreturns)


#Fit garch models
OneMGARCH=garchFit(~ garch(1,1), data=as.numeric(OneMreturns))
summary(OneMGARCH)
FiveMGARCH=garchFit(~ garch(1,1), data=as.numeric(FiveMreturns))
summary(FiveMGARCH)
OneHGARCH=garchFit(~ garch(1,1), data=as.numeric(OneHreturns))
summary(OneHGARCH)

#Question 4
FiveM=as.xts(FiveM)
FiveM$Return=makeReturns(FiveM$Price)

#intraday periodic patterns
par(mfrow = c(2, 1))
par(cex = 0.6)
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 3, 3))
plot.ts(FiveM$Num.Trades,col="red",ylab="Num.Trades")
plot.ts(FiveM$Return,col="blue",ylab="Return")
par(resetPar())

#fit ACD model
fit=data.frame(as.character(index(FiveM)),as.numeric(FiveM$Price),as.numeric(FiveM$Volume))
colnames(fit)=c("time","price","volume")
durfit=diurnalAdj(computeDurations(fit),aggregation="none", method = "supsmu")
acdFit(durfit, model = "ACD",
       dist = "exponential", order = c(1,1), dailyRestart = 1)


#particular exchange that offers higher price
EXCH=c("A","B","C","D","J","K","M","P","Q","W","X","Y","Z")
Find1st=selectExchange(data,exch=EXCH[1])
EX=as.xts(TimeMerge(mergeTradesSameTimestamp(Find1st,selection="weightedaverage"),Find1st,5))[,1]
for (i in 2:length(EXCH))
{FindRest=selectExchange(data,exch=EXCH[i])
 EXOther=as.xts(TimeMerge(mergeTradesSameTimestamp(FindRest,selection="weightedaverage"),FindRest,5))[,1]
  EX=merge(EX,EXOther)}
colnames(EX)=EXCH
EX=substituteNA(EX, type = "zeros")
HighPriceProvider=EXCH[which.max(as.data.frame(table(max.col(EX)))[,2])]


#Question 5
plot.ts(FiveM$Volume,main="Volume",xlab="Time",ylab="Volume")

#predict the orderflow 
chartSeries(FiveM,theme="white")
#back test
buildModel(FiveM)


getSymbols('QQQQ',src='yahoo')
q.model = specifyModel(Next(OpCl(QQQQ)) ~ Lag(OpHi(QQQQ),0:3))
buildModel(q.model,method='lm',training.per=c('2006-08-01','2006-09-30'))


