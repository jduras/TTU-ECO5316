---
title: "HW_8"
author: "Narendra Raj Tiwari"
date: "May 16, 2019"
output: html_document
---

```{r}
library(Quandl)
library(urca)
library(vars)
library(forecast)
library(zoo)
library(readr)

```

#a)
```{r}
Quandl.api_key("k1RWPy3mMbspsEB3K4kn")

oil_raw <-Quandl("FRED/MCOILWTICO", type="zoo")
gas_raw <-Quandl("FRED/GASREGCOVM",type="zoo")

```


## (a) Plotting a Single Time Series:
```{r}
loil <- log(oil_raw)
lgas <- log(gas_raw)
loil <- window(loil, start=1995+0, end=2017+3/12)
lgas <- window(lgas, start=1995+0, end=2017+3/12)
par(mfrow=c(1,1))
plot(loil, type='l', main="Crude Oil Prices vs. Regular Conventional Gas Prices", col="blue", ylim=c(-1, 5))
lines(lgas, col="red")  
```
#(b) Unit Root Tests:
```{r}
loil.urers1 <- ur.ers(loil, type="P-test", model="trend")
summary(loil.urers1)

lgas.urers2 <- ur.ers(lgas, type="P-test", model="trend")
summary(lgas.urers2)
```

#Interpretation:Under ERS test,
H0: The time series has a unit root. 
 Since 9.5 (oil) and 8.0956 (gas) > 3.96, 5.62, 6.89, we reject the null hypothesis.
 This shows that there is no unit root in the original data.
 
 # First Differences of data and unit root test:
```{r}
doil <- diff(loil)
dgas <- diff(lgas)

doil.urers1 <- ur.ers(doil, type="P-test", model="trend")
summary(doil.urers1)


dgas.urers2 <- ur.ers(dgas, type="P-test", model="trend")
summary(dgas.urers2)
```
 
 #Interpretation: Since 0.8136(oil) and 0.4831(gas) < 3.96, 5.62, 6.89, we fail to reject the null hypothesis.
So, Crude Oil Prices and Regular Conventional Gas Price are I(1).
 
```{r}
par(mfrow=c(1,1))
plot(doil, type='l', main="Log Differences of Crude Oil Prices vs. Log Differences of Gas Prices", col="blue", ylim=c(-0.4, 0.4))
lines(dgas, col="red")
```
 
Conclusion:
Clearly both the time series are I(1).

(c) Determining the Number of Lags:

```{r}
y <- cbind(loil, lgas)
colnames(y) <- c("log.wti","log.gas")
y <- na.trim(y)
y.VAR.IC <- VARselect(y, type="const")

nlags <- y.VAR.IC$selection["SC(n)"]
nlags

y <- window(y, start=1995+0, end=2010+11/12)
y.CA <- ca.jo(y, ecdet="const", type="trace", K=nlags, spec="transitory")
summary(y.CA)

y.CA <- ca.jo(y, ecdet="const", type="eigen", K=nlags, spec="transitory")
summary(y.CA)
```
Conclusion: 
These results show the rejection of the null hypothesis. Hence, oil and gas prices are cointegrated.

#d
```{r}
lttest(y.CA, r=1)
```
#
Clearlyt, the test implies the case 2(restricted constant),
while the plots from (a) suggests the Case 4(restricted trend).


#e) Estimating the Bivariate VEC Model:

```{r}
y.VEC <- cajorls(y.CA, r=1)
y.VEC
```

# (f) Statistical Significance of alpha1 and alpha2 in the Estimated VEC Model:
```{r}
summary(y.VEC$rlm)
```
Interpretati:Here,  ??1 is not significant ,but ??2 is statistically significant .So,iIt does not satisfy the condition for long run stable relationship.

Moreover,Since   ??1  >0 and  ??2 >0, whenever there is a disruption:the signs of the adjustment parameters are consistent with the error correction mechanism.

# g)
Test the restriction ??2 =0 using the likelihood ratio test.
```{r}
rest.alpha <- matrix(c(1,0), c(2,1))
y.CA.ralpha <- alrtest(y.CA, A=rest.alpha, r=1)
summary(y.CA.ralpha)
```
Conclusion:
If ??2=0 and ??1 is not zero, then y2,t gas price is a pure random walk and all the adjustment occurs in y1,t oil price.

# h)
 the intuition for imposing the restriction:
 
 The restriction ??2=0 implies that the adjustment occurs by y1(oil price) only.
 i.e., gas prices cannot affect oil prices.
 
 
i) First,Plotting the ACF and PACF to identify a suitable AR or MA or ARMA model and estimating the AR/MA/ARMA model.
```{r}
y_all <- cbind(loil, lgas)
colnames(y_all) <- c("log.wti","log.gas")
y_all <- na.trim(y_all)

y_all <- window(y_all, start=1995+0, end=2017+3/12)
first.m <- 1995+0
last.m <-2010+11/12

y.p1 <- window(y_all, end=last.m)
y.p2 <- window(y_all, start=last.m+ 1/12)


y.VAR.f1 <-data.frame()
y.VAR.f2 <-data.frame()

for(i in 1:length(y.p2[,2]))
{
  y <- window(y_all, end = last.m + (i-1)/12)
  y.CA <- ca.jo(y, ecdet="const", type="eigen", K=3, spec="transitory")
  y.VAR <- vec2var(y.CA, r=1)
  y.VAR.updt <- predict(y.VAR, n.ahead=1)
  y.VAR.f1 <-rbind(y.VAR.f1, as.ts(y.VAR.updt$fcst$log.wti))
  y.VAR.f2 <-rbind(y.VAR.f2, as.ts(y.VAR.updt$fcst$log.gas))
  
} 

y.VAR.f1 <- ts(y.VAR.f1, start=2011, frequency=12)
y.VAR.f2 <- ts(y.VAR.f2, start=2011, frequency=12)


y.gas <-as.ts(y_all[,2])
y.gas.p1 <-as.ts(y.p1[,2])
y.gas.p2 <-as.ts(y.p2[,2])

par(mfrow=c(1,1))
plot(y.gas, type="l", pch=16, lty="dashed", main="One Month ahead Forecasts of Gas Price")
lines(y.gas.p1, type="o", pch=16, lty="solid")
lines(y.VAR.f2[,1], type="o", pch=16, lty="solid", col="red")
first.m <- 1995+0
last.m <-2010+11/12
diffgas.p1 <- window(diffgas, end=last.m)
diffgas.p2 <- window(diffgas, start=last.m+1/12)


par(mfrow=c(1,2))
Acf(diffgas.p1, type="correlation", lag=48, main="ACF_Gas")
Acf(diffgas.p1, type="partial", lag=48, main="PACF_Gas")
```
 
```{r}
rol.f <-zoo()
for(i in 1: length(diffgas.p2))
{
  y <- window(diffgas, end = last.m+(i-1)/12)
  rol.updt <- arima(y, order=c(2,0,6))
  rol.f <- c(rol.f, forecast(rol.updt, 1)$mean)
}  
```
 
```{r}
rol.f <-as.ts(rol.f)
par(mfrow=c(1,1))
plot(rol.f, type="o", pch=16, xlim=c(1995, 2017), ylim=c(-0.4, 0.2), main="One month ahead Forecasts of log_diff(Gas)")
lines(rol.f, type="p", pch=16, lty="solid", col="red")
lines(diffgas, type="l", pch=16, lty="dashed")
lines(diffgas.p1, type="o", pch=16, lty="solid")
```

