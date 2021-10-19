library(fBasics)
library(forecast)

datos<-read.csv("C:/Users/Rodrigo Morales/Documents/IE/SUBJECTS/2/Forecasting Time Series/HW 2/coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")
rownames(datos) <- as.Date.character(datos$anntime, format = '%Y%m%d')
y<-datos[,2] 
s= 4 # seasonal parameter

#graph is not stationary

#plot ACF and PACF
par(mfrow=c(3,1))
ts.plot(y)
nlags=60
acf(y,nlags) # many lags out of bounds, and slowly decaying to 0 so therefore non-stationary and it is seasonal 
pacf(y,nlags) # fewer lags out of bounds therefore we will use PACF bc it will be a simpler model


# look for differences in the seasonal and regular data 
nsdiffs(y,m=s,test=c("ocsb")) # seasonal dif = 1 
ndiffs(y,alpha=0.05, test=c("adf")) #regular differences = 1 


#estimate the SAR and analyze the estimated parameters 
fit <- arima(y,order=c(0,1,0), seasonal=list(order=c(0,1,0), period = s))
fit

# ACF and PACF for the transformed data 
# calcualte ACF and PACF of the residuals 
par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags) # no longer as the previous non-transformed data (slowly decaying to 0)
pacf(fit$residuals,nlags) # the PACF has the lag 4 out of bounds that demostrates it is an autoregressive of order 1 


ndiffs(fit$residuals, alpha=0.05, test=c("adf")) #0 regular 
nsdiffs(fit$residuals, m=s, test=c("ocsb")) #0 
       

#FIRST MODEL
# since we know that it is a seasonal, and we transformed the data with 
# 1 dif now we can start with the autoregressive of order 1 
fit1 <- arima(y,order=c(0,1,0), seasonal=list(order=c(1,1,0), period = s))
fit1

par(mfrow=c(3,1))
ts.plot(fit1$residuals)
acf(fit1$residuals,nlags) # no longer as the previous non-transformed data (slowly decaying to 0)
pacf(fit1$residuals,nlags)

abs(0.4842/0.0899) # 4 = significant

Box.test(fit1$residuals,lag=36) # white noise residuals?
# p-value > 0.05 uncorrelated 

Box.test(fit1$residuals,lag=60)# play with the number of lags
Box.test(fit1$residuals^2,lag=60)
shapiro.test(fit1$residuals)  # normality test
#pvalue > 0.05 = normally distributed 
#not normally distributed 

par(mfrow=c(3,1))
ts.plot(fit1$residuals^2)
acf(fit1$residuals^2,nlags) # no longer as the previous non-transformed data (slowly decaying to 0)
pacf(fit1$residuals^2,nlags)

#graph normality 
hist(fit1$residuals,prob=T,ylim=c(0,24), xlim=c(mean(fit1$residuals)-3*sd(fit1$residuals),mean(fit1$residuals)+3*sd(fit1$residuals)),col="red")
lines(density(fit1$residuals), lwd=2)
mu <- mean(fit1$residuals)
sigma <-sd(fit1$residuals)
x<- seq(mu-3*sigma,mu+3*sigma, length=100)
yy <-dnorm(x, mu, sigma)
lines(x, yy,lwd=2, col="blue")
# blue line is the theoretical normal 
# black, smooth version of my histogram 
# my histogram shows that my data is not normally distributed, the hypothesis 


# point predictions and standard errors
y.pred1<-predict(fit1,n.ahead=24)
y.pred1$pred   # point predictions
y.pred1$se    # standard errors

# plotting real data with point predictions

new1 <- c(y,y.pred1$pred) # real data + predicted values

# plotting real data with point predictions
plot.ts(new1,main="Predictions",
        ylab="EPS",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)

#SECOND MODEL 
#estimate the SAR and analyze the estimated parameters
fit2 <- arima(y,order=c(2,1,0), seasonal=list(order=c(1,1,0), period = s))
fit2

abs(-.1912/.1030)
abs(-.2093/.0996) #significant
abs(-.4702/.1003)

# ACF and PACF for the transformed data 
# calcualte ACF and PACF of the residuals 
par(mfrow=c(3,1))
ts.plot(fit2$residuals)
acf(fit2$residuals,nlags) 
pacf(fit2$residuals,nlags)

Box.test(fit2$residuals,lag=36) # uncorrelated = WN 

shapiro.test(fit2$residuals) 

Box.test(fit2$residuals^2,lag=36)# not normally distributed = no GWN

#graph normality 
hist(fit2$residuals,prob=T,ylim=c(0,30), xlim=c(mean(fit2$residuals)-3*sd(fit2$residuals),mean(fit2$residuals)+3*sd(fit2$residuals)),col="red")
lines(density(fit2$residuals), lwd=2)
mu <- mean(fit2$residuals)
sigma <-sd(fit2$residuals)
x<- seq(mu-3*sigma,mu+3*sigma, length=100)
yy <-dnorm(x, mu, sigma)
lines(x, yy,lwd=2, col="blue")

#SWN
par(mfrow=c(2,1))
acf(fit2$residuals^2)
pacf(fit2$residuals^2) 

Box.test(fit2$residuals^2,lag=15) #correlated 
# we do not know if it is SWN but we can say that we can use a nonlinear model 
shapiro.test(fit2$residuals) # not normally distributed 

# point predictions and standard errors
y.pred2<-predict(fit2,n.ahead=24)
y.pred2$pred   # point predictions
y.pred2$se    # standard errors

# plotting real data with point predictions

new2 <- c(y,y.pred2$pred) # real data + predicted values

# plotting real data with point predictions
plot.ts(new2,main="Predictions",
        ylab="EPS",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)


real<-datos[,2][84:107] 
c=1:24
plot(c,real,type="b",col="red", main = "Model vs. Real",
     xlab = "Point Predictions",
     ylab = "EPS",
     xlim = c(1,25), ylim = c(0,1.5))
lines(c,y.pred1$pred,col="blue",type="b")
lines(c,y.pred2$pred,col="green",type="b")
legend("topleft",c("real","forecast-1", "forecast-2"),
       col = c("red","blue", "green"),pch = c(1,1,1),bty ="n")

n<-length(y)
n.estimation<- 83 # 
n.forecasting<-n-n.estimation # 24 observations
horizontes<-2 # number of periods ahead

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE<-matrix(0,nrow=horizontes,ncol=1)
MAPE<-matrix(0,nrow=horizontes,ncol=1)

for (Periods_ahead in 1:horizontes) {
        for (i in 1:n.forecasting) {
                aux.y<-y[1:(n.estimation-Periods_ahead+i)];
                fit1<-arima(y,order=c(0,1,0), seasonal=list(order=c(1,1,0), period = s));
                y.pred1<-predict(fit1,n.ahead=Periods_ahead);
                predicc[i,Periods_ahead]<- (y.pred1$pred[Periods_ahead]);
        }
        error<-real-predicc[,Periods_ahead];
        MSFE[Periods_ahead]<-mean(error^2);
        MAPE[Periods_ahead]<-mean(abs(error/real)) *100;
}
MSFE
MAPE
for (Periods_ahead in 1:horizontes) {
        for (i in 1:n.forecasting) {
                aux.y<-y[1:(n.estimation-Periods_ahead+i)];
                fit2<-arima(y,order=c(2,1,0), seasonal=list(order=c(1,1,0), period = s))
                y.pred2<-predict(fit2,n.ahead=Periods_ahead);
                predicc[i,Periods_ahead]<- (y.pred2$pred[Periods_ahead]);
        }
        error<-real-predicc[,Periods_ahead];
        MSFE[Periods_ahead]<-mean(error^2);
        MAPE[Periods_ahead]<-mean(abs(error/real)) *100;
}
MSFE
MAPE
