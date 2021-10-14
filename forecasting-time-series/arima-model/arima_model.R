# ----------------------------------------------
# PRELIMINARS
# ----------------------------------------------

# PATH
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

# LIBRARIES
library(fBasics)
library(forecast)

# READING DATA
datos<-read.csv("Session6Data.csv",header=TRUE,sep=";",dec=",")

# SEPARATING TIME SERIES IF NECESSARY
y<-datos[,4] # brent dated weekly spot prices
xx<-datos[,2] # GO weekly spot prices

# ----------------------------------------------
# FIRST LOOK AT DATA
# ----------------------------------------------

# FIRST PLOTS TO TAKE A LOOK AT CORRELATIONS
plot(y,xx) #Plot Time Series 1 vs Time Series 2
cor(y,xx) #Calculates the correlation

# PLOTTING THE TIME SERIES
ts.plot(y) #Plot several time series on a common plot -> WHY ARE WE JUST PLOTTING Y?
#ts.plot(xx)

# PLOTTING AUTOCORRELATION AND PARTIAL AUTOCORRELATION
par(mar=c(5,5,5,5))
par(mfrow=c(2,1))
acf(y)  
pacf(y)

#par(mar=c(5,5,5,5))
#par(mfrow=c(2,1))
#acf(xx)  
#pacf(xx)

# ----------------------------------------------
# ACHIEVING STATIONARITY AND IDENTIFYING THE MODEL
# ----------------------------------------------

# NUMBER OF DIFFERENCES REQUIRED FOR STATIONARITY IN MEAN
ndiffs(y, alpha=0.05, test=c("adf"))

# APPLY ALL THE DIFFERENCES NEEDED
z<-diff(y) #Our series from now on

# APPLY LOG IF NECESSARY TO ACHIEVE STATIONARITY IN VARIANCE
# z<-diff(log(y))

# PLOTTING AGAIN
ts.plot(z)

par(mfrow=c(2,1))
acf(z)  
pacf(z)

# ANY EXTRA TRANSFORMATIONS
ndiffs(z, alpha=0.05, test=c("adf"))

# zz<-diff(z)
# ts.plot(zz)  
# par(mfrow=c(2,1))
# acf(zz)  
# pacf(zz)

# ----------------------------------------------
# ESTIMATING THE MODEL FOR THE STATIONARY DATA
# ----------------------------------------------

# WHAT MODELS COULD FIT BEST BASED ON OBSERVATIONS?

# ACF -> MA: 1, 5
# PACF -> AR: 1, 5
# ARIMA(AR,0,MA)

# FITTING ARIMA MODEL
fit<-arima(z,order=c(1,0,0))  # fit<-arima(z,order=c(5,0,0),fixed=c(NA,0,0,0,NA,NA))    
fit # we find the information about the estimated parameters
ts.plot(fit$residuals)
par(mfrow=c(2,1))
acf(fit$residuals)
pacf(fit$residuals)

# TESTING WHITE NOISE

# Examining the null hypothesis of independence in a given time series
# H0 -> INDEPENDENT / HA -> DEPENDENT
# If p-value>0.05: H0 / p-value<0.05: HA

Box.test(fit$residuals,lag=20) 

# TESTING STRICT WHITE NOISE

# Same process but with the squares

par(mfrow=c(2,1))
acf(fit$residuals^2)
pacf(fit$residuals^2) 

Box.test(fit$residuals^2,lag=15)


# TESTING NORMALITY

# SAPHIRO TEST
# HO: Not a normal distribution / HA: Normal distribution
# If p-value>0.05: H0 / p-value<0.05: HA

shapiro.test(fit$residuals)

# PLOT THE HISTOGRAM

hist(fit$residuals,prob=T,ylim=c(0,0.25),xlim=c(mean(fit$residuals)-3*sd(fit$residuals),mean(fit$residuals)+3*sd(fit$residuals)),col="red")
lines(density(fit$residuals),lwd=2)
mu<-mean(fit$residuals)
sigma<-sd(fit$residuals)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue") # theoretical

# ----------------------------------------------
# FORECASTING
# ----------------------------------------------

y.pred<-predict(fit,n.ahead=5)
y.pred$pred # point predictions
y.pred$se  # standard error for the point predictions to compute confidence intervals

# CONFIDENCE INTERVALS

# 95% confidence interval (1.96)
std_residuals=(fit$residuals-mean(fit$residuals))/sd(fit$residuals)
quantile(std_residuals, probs=c(0.025,0.975)) 

# 80% confidence interval (1.28)
quantile(std_residuals, probs=c(0.1,0.9)) 

# 60% confidence interval (0.84)
quantile(std_residuals, probs=c(0.2,0.8)) 

