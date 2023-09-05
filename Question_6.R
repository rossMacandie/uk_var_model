setwd("Econometrics")
getwd()
##Import dataset
VAR_data <- read.csv("VAR_data_exam.csv")


## Install packages 
install.packages("xts")
install.packages("tseries")


##Convert variables into a number rather than a character
VAR_data$Prices <- as.numeric(as.character(VAR_data$Prices))
VAR_data$Rates <- as.numeric(as.character(VAR_data$Rates))
VAR_data$TP <- as.numeric(as.character(VAR_data$TP))

##Convert the data into a recognized time series
library(xts)
vardata <- ts(VAR_data, start = c(1990,1), frequency = 4)
vardata <- as.xts(vardata)
head(vardata)

##Plot a time series for the data to visually assess stationarity 
plot(vardata)


## Install and import urca library
install.packages("urca")
library(urca)

## Run an adf test 
summary(ur.df(vardata$Prices,type = c("trend"),lags = 12,selectlags = c("AIC")))
##Cannot reject the null hypothesis, even at the 10%(3.13>3.1207)
summary(ur.df(vardata$TP,type = c("trend"),lags = 12,selectlags = c("AIC")))
##Cannot reject the null hypothesis even at the 10% level (3.13>2.082)
summary(ur.df(vardata$Rates,type = c("trend"),lags = 12,selectlags = c("AIC")))
##Cannot reject the null hypothesis at the 5% level (3.43>3.2534)
##All values should be considered non stationary.

##Find the first differences of each variable
dprices <- diff(vardata$Prices,lags=1)
dtp <-  diff(vardata$TP,lags=1)
drates <- diff(vardata$Rates,lags=1)
##REmove the first value 
dprices <- dprices[-1]
dtp <- dtp[-1]
drates <- drates[-1]

##Conduct adf tests on the first difference of each variable
summary(ur.df(dprices,type = c("trend"),lags = 12,selectlags = c("AIC")))
summary(ur.df(dtp,type = c("trend"),lags = 12,selectlags = c("AIC")))
summary(ur.df(drates,type = c("trend"),lags = 12,selectlags = c("AIC")))
##Reject the null hypothesis of non-stationarity at any significance level for all variables

##Create a new data set containing all the stationary series
vardata1 <- cbind(dprices,dtp,drates)

##Estimate the best VAR model
install.packages("vars")
library(vars)
VARselect(vardata1, lag.max=10,type = c("const"))$selection
##Choose a VAR(1) model as the best model

UKvar <- VAR(vardata1,p=1,type = c("const"))
summary(UKvar)

##Conduct a Breusch-Godfrey test to assess whether residuals are autocorrelated
serial.test(UKvar, lags.bg = 6, type = c("BG"))
##Null hypothesis is not rejected at the 5% level (p=0.06414)

##Conduct a Jaque-Bera test to assess whether residuals are normally distributed
normality.test(UKvar)
##Null hypothesis is rejected

##PART B 
UKirf <- irf(UKvar, n.ahead = 10,ortho=TRUE)

plot(UKirf$irf$Prices[,1],xlab="Periods after shock",ylab="Response of CPI to CPI",xaxt="n",ylim=c(-0.5,0.5),type='l')
axis(1, at = 1:21, labels=0:20)
lines(UKirf$Lower$Prices[,1], col = "blue")
lines(UKirf$Upper$Prices[,1], col = "green")

plot(UKirf$irf$Prices[,2],xlab="Periods after shock",ylab="Response of CPI to TP",xaxt="n",ylim=c(-1,1),type='l')
axis(1, at = 1:21, labels=0:20)
lines(UKirf$Lower$Prices[,2], col = "blue")
lines(UKirf$Upper$Prices[,2], col = "green")

plot(UKirf$irf$Prices[,3],xlab="Periods after shock",ylab="Response of CPI to Bank Rate",xaxt="n",ylim=c(-0.5,0.5),type='l')
axis(1, at = 1:21, labels=0:20)
lines(UKirf$Lower$Prices[,3], col = "blue")
lines(UKirf$Upper$Prices[,3], col = "green")

plot(UKirf$irf$TP[,1],xlab="Periods after shock",ylab="Response of TP to CPI",xaxt="n",ylim=c(-0.5,0.5),type='l')
axis(1, at = 1:21, labels=0:20)
lines(UKirf$Lower$TP[,1], col = "blue")
lines(UKirf$Upper$TP[,1], col = "green")

plot(UKirf$irf$TP[,2],xlab="Periods after shock",ylab="Response of TP to TP",xaxt="n",ylim=c(-1,3.5),type='l')
axis(1, at = 1:21, labels=0:20)
lines(UKirf$Lower$TP[,2], col = "blue")
lines(UKirf$Upper$TP[,2], col = "green")

plot(UKirf$irf$TP[,3],xlab="Periods after shock",ylab="Response of TP to Bank Rate",xaxt="n",ylim=c(-0.5,0.5),type='l')
axis(1, at = 1:21, labels=0:20)
lines(UKirf$Lower$TP[,3], col = "blue")
lines(UKirf$Upper$TP[,3], col = "green")

plot(UKirf$irf$Rates[,1],xlab="Periods after shock",ylab="Response of Bank Rate to CPI",xaxt="n",ylim=c(-0.5,0.5),type='l')
axis(1, at = 1:21, labels=0:20)
lines(UKirf$Lower$Rates[,1], col = "blue")
lines(UKirf$Upper$Rates[,1], col = "green")

plot(UKirf$irf$Rates[,2],xlab="Periods after shock",ylab="Response of Bank Rate to TP",xaxt="n",ylim=c(-0.5,1),type='l')
axis(1, at = 1:21, labels=0:20)
lines(UKirf$Lower$Rates[,2], col = "blue")
lines(UKirf$Upper$Rates[,2], col = "green")

plot(UKirf$irf$Rates[,3],xlab="Periods after shock",ylab="Response of Bank Rate to Bank Rate",xaxt="n",ylim=c(-0.5,0.5),type='l')
axis(1, at = 1:21, labels=0:20)
lines(UKirf$Lower$Rates[,3], col = "blue")
lines(UKirf$Upper$Rates[,3], col = "green")

##PART C
library(FinTS)
grangertest(Prices ~ TP, order = 3, data = vardata1)
##Fail to reject null hypothesis
grangertest(Prices ~ Rates, order = 3, data = vardata1)
##Fail to reject null hypothesis at the 5% level
grangertest(TP ~ Prices, order = 3, data = vardata1)
##Fail to reject null hypothesis 
grangertest(TP~ Rates,order = 3, data = vardata1)
##Fail to reject null hypothesis
grangertest(Rates ~ Prices, order = 3, data = vardata1)
##Fail to reject null hypothesis
grangertest(Rates ~ TP, order = 3, data = vardata1)
##Fail to reject null hypothesis
