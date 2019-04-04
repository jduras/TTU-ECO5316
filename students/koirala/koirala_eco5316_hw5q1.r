library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggfortify)
library(egg)
library(tsibble)
library(timetk)
library(lubridate)
library(zoo)
library(forecast)
library(broom)
library(sweep)
library(tidyquant)
library(scales)
library(magrittr)
library(tibbletime)
library(tidyverse)
library(urca)
# set default theme for ggplot2
theme_set(theme_bw())


#a. obtain the monthly data for total nonfarm payroll employment,

# import the data on earnings per share for Johnson and Johnson,
# then construct log, change, log-change, seasonal log change
tnpe_tbl_all <-
  tq_get("PAYNSA",
         get = "economic.data",
         from = "1975-01-01",
         to = "2019-02-01") %>%
  
  #transforming the data
  
  tnpe_tbl<-tnpe_tbl_all %>%
  rename(y = price)%>%
  
  
 #b. constructing log, change, log change, seasonal log change
  
   mutate(ly = log(y),
         dy = y - lag(y),
         dly1 = ly - lag(ly),
         dly12 = ly - lag(ly, 12),
         d2ly12_1 = dly12 - lag(dly12))


#plot the original and the transformed timed series

tnpe.long<- tnpe_tbl %>%
  gather(variable, value, -date) %>%
  mutate(variable = factor(variable, ordered = TRUE,
                           levels = c("y", "dy", "ly", "dly1", "dly12", "d2ly12_1"),
                           labels = c("y", 
                                      expression(paste(Delta,"y")),
                                      "ln(y)",
                                      expression(paste(Delta,"ln(y)")),
                                      expression(paste(Delta[12],"ln(y)")),
                                      expression(paste(Delta, Delta[12],"ln(y)")))))


tnpe.long %>%
  ggplot( aes(x=date, y=value))+
  geom_line()+
  labs(x="",y="")+
  facet_wrap(~variable, scales = "free_y", labeller = label_parsed)



#Here we can see that y and  delta ln(y) shows upward trend. 
#We see that delta delta 12 fluctuates is stationary and fluctuates around zero.
#We see that the variance of delta y increases over time 
#But the variance of delta lny does not show that pattern.


#c use ggseasonplot to create seasonal plots

tnpe_tbl %>%
  tk_ts(select=dly1, start=c('1975','1'),frequency=12)
#%>% ggseasonplot()



#d 

#plot the ACF and PACF for ly
Acf(tnpe_tbl$ly, lag.max=48, main=expression(paste("ACF for l(y)")))
Pacf(tnpe_tbl$ly, lag.max=48, main=expression(paste("PACF for l(y)")))

#The decay of acf is slow and is significant for large number of lags.
#Non stationarity in the model as acf is decaying very slowly.
# The pacf have one spike at fist lag. So it is AR(1) process.

#plot the ACF and PACF for dly1
Acf(tnpe_tbl$dly1, lag.max=48, main=expression(paste("ACF for dly1")))
Pacf(tnpe_tbl$dly1, lag.max=48, main=expression(paste("PACF for dly1")))

#The acf shows the cyclical pattern .
# some have positive values (6 and 12) and some lags have negative values regarding the seasonality problem.
#Also for pacf, there are lags with positive and negative values.

#plot the ACF and PACF for dly12
Acf(tnpe_tbl$dly12, lag.max=48, main=expression(paste("ACF for", Delta[12], "ln(y)")))
Pacf(tnpe_tbl$dly12, lag.max=48, main=expression(paste("PACF for", Delta[12], "ln(y)")))

#The value of acf suggests it has a cyclical pattern which means stationarity.
#The figure of pacf value showing a number of positive and negative values
#I can not suggest which process is this.

#plot the ACF and PACF for d2ly12_1

Acf(tnpe_tbl$d2ly12_1, lag.max=48, main=expression(paste("ACF for ", Delta, Delta, "l(y)")))
Pacf(tnpe_tbl$d2ly12_1, lag.max=48, main=expression(paste("PACF for " ,Delta, Delta, " l(y)")))

#The ar is declining slowlywith significant correlation in 12th lag.
#Based on pacf we can say that it is ar(3) process.



#e ADF and KPSS test

### ADF and KPSS test for ly
#ADF
dfly<- 
  ur.df(tnpe_tbl$ly)
summary(dfly)
#ADF test shows that the test statistics is positive when critical values are negative.
#This means we have to accept the null hypothesis and reject alternate hyptohesis.
#Therefore, we can conclude there is presence of unit root.

#KPSS
kpssly<- 
  ur.kpss(tnpe_tbl$ly)
summary(kpssly)
#From also KPSS test we find that the crtical values are lower than the value of test statistics.
#Here also we reject null hypotheses and there is no presence of unit root.


###  and KPSS  test for dly
a<-tnpe_tbl%>%
  filter( !is.na(dly1))

#ADF
dfdly1<- 
  ur.df(a$dly1)
summary(dfdly1)

# here test statistics is less than the critical value
#There is presence of unit roots.


#KPSS
kpssdly1<- 
  ur.kpss(a$dly1)
summary(kpssdly1)

#Here we reject the null hypotheses at 10%.
#Therefore, unit root is present at 10%

### ADF and KPSS test for dly12
b<-tnpe_tbl%>%
  filter( !is.na(dly12))

#ADF
dfdly12<- 
  ur.df(b$dly12)
summary(dfdly12)

#Here we reject the null hypothesis at 5 and 10%.
#So that there is absence of unit root at that level.

#KPSS
kpssdly12<- 
  ur.kpss(b$dly12)
summary(kpssdly12)

#As test statistic is higher the critical value we reject null hypothesis.
#There is no presence of unit root.


### ADF and KPSS test for d2ly12_1
c<-tnpe_tbl%>%
  filter( !is.na(d2ly12_1))

#ADF
dfdly12_1<- 
  ur.df(c$d2ly12_1)
summary(dfdly12_1)
#Here also the test statistics is larger than critical values.
#Therefore, there is no presence of unit roots.

#KPSS
kpssd2ly12_1<- 
  ur.kpss(c$d2ly12_1)
summary(kpssd2ly12_1)
#Here also the test statistics is larger than critical values.
#Therefore, there is no presence of unit roots.



#f) split sample - estimation subsample dates
tnpe_tbl_1<- 
  tnpe_tbl%>%
  ts(start=c(1975,1), frequency = 12) %>%
  tk_tbl(rename_index = "yearm") %>%
  mutate(yearm=yearmonth(yearm)) %>%
  filter( !is.na(d2ly12_1))
train.start<-1975.00
train.end<-2014+(11/12)
test.start<-2015.00
test.end<- 2019+(1/12)


tnpe.ts<- tnpe_tbl_1 %>%
  tk_ts(select = d2ly12_1, start = train.start, frequency = 12)

tnpe.ts.1<-tnpe.ts%>%
  tk_ts( start=train.start,end=train.end, frequency=12)

tnpe.ts.2<-tnpe.ts %>%
  tk_ts(start=test.start,end=test.end, frequency = 12)

#ACF PACF test for arima model
#Plots the ACF and PACF for the d2ly12_1
Acf(tnpe_tbl$d2ly12_1, lag.max=50, main=expression(paste("ACF for ",Delta, Delta,"l(y)")))
Pacf(tnpe_tbl$d2ly12_1, lag.max=50, main=expression(paste("PACF for ",Delta, Delta,"l(y)")))


#The acg is decaying steadily.We can see some seasonality. 
#There is also some seasonality problem in the ACF. 
#There is also a seasonality in the PACF with every 12 month 
#with significance even after four years. the second lag is also highly significant following each of the seasonal terms. 
#The third lag is not that significant, 
#Therefore $SARIMA(2,0,0)(1,0,0) is better.

m1<-Arima(tnpe.ts, order = c(2,0,0), seasonal=c(1,0,0))
ggtsdiag(m1, gof.lag=48)

#The Ljung-Box statistics indicate there is a large problem with the model with close to significance across all lags. 
#These would be significant when we adjust the  degree of freedom. 
#Thus, the lagged residuals are predicting the current residuals. When it comes to the ACF, there is no issues across 
#the residual plot, indicating high values followed high values and low is followed by another. 
#This is likely due to the seasonal AR term not that  is controlling for the seasonality properly.
autoplot(m1)

#Here all the inverse roots are inside the unit circle.
#This suggests the model is stationary

# (g) autoarima
m.auto<-auto.arima(tnpe.ts, seasonal=T, stationary=F, stepwise=F, approximation=F, ic="aic")

ggtsdiag(m.auto, gof.lags=50)

autoplot(m.auto)

#(h) rolling forecast

window.length <- length(tnpe.ts.1)
tic()
results <-
  tnpe_tbl %>%  
  mutate(yearm = yearmonth(date)) %>%
  as_tsibble(index = yearm) %>% # covert to tsibble
  mutate(arma.model = slide(ly, ~Arima(.x, order = c(3,1,0), seasonal = list(order = c(1,1,1), period = 12)), 
                            .size = window.length)) %>%  
  filter(!is.na(arma.model)) %>%                                                              # remove periods at the beginning of sample where model could not be estimated due to lack of data,
  mutate(arma.coefs = map(arma.model, tidy, conf.int = TRUE),                                  # extract coefficients                                
         arma.f = map(arma.model, (. %>% forecast(h = 1) %>% sw_sweep())))                     #extract models
results
toc()

m1.f.1.rol <-
  results %>%
  as_tibble() %>%
  select(date, yearm, arma.f) %>%
  unnest() %>% 
  mutate(yearm = yearm %m+% months(1)) %>%
  filter(key == "forecast")

m1.f.1.rol %>%
  ggplot(aes(x = yearm, y = value)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = "royalblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = "royalblue", alpha = 0.3) +
  geom_line(size = 0.7, col = "blue") +
  geom_line(data = (tnpe_tbl %>% filter(year(date) > 2000)) %>% mutate(yearm = yearmonth(date)), 
            aes(x = yearm, y = ly)) 



#(G)


m2_f_1_rol <-
  results %>%
  select(yearm, arma.f) %>%
  as_tibble() %>%
  unnest(arma.f) %>%
  filter(key == "forecast") %>%
  mutate(yearm = yearm %m+% months(1)) %>%
  mutate_at(vars(value, lo.80, lo.95, hi.80, hi.95), funs(exp)) %>%
  rename(y = value) %>%
  select(yearm, key, y, lo.80, lo.95, hi.80, hi.95)

m2_f_1_rol %>%
  filter(yearm >= "2013-01-01") %>%
  mutate(dy = y - lag(y)) %>%
  ggplot(aes(x = yearm, y = dy, col = key, linetype = key)) +
  # geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "blue", alpha = 0.1) +
  # geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "blue", alpha = 0.2) +
  geom_line(data = (tnpe_tbl %>% filter(year(date) >= 2013)) %>% mutate(yearm = yearmonth(date), key = "actual"), 
            aes(x = yearm, y = dy)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("gray40","darkblue")) +
  scale_linetype_manual(values = c("solid","solid")) 


tnpe.actual<- 
  tnpe_tbl_1  %>%
  tk_ts(select=y, start = train.start, end=test.end, frequency = 12)
# tk_tbl(rename_index = "yearm") %>%
#mutate(yearm=yearmonth(yearm))

tnpe.actual.1<- 
  tnpe_tbl_1  %>%
  tk_ts(select=y, start = train.start, end=train.end, frequency = 12)


tnpe.actual.2<- 
  tnpe_tbl_1  %>%
  tk_ts(select=y, start = test.start, end=test.end, frequency = 12)

y.forecast<-
  m2_f_1_rol %>%
  mutate(yearm = yearmonth(date))
tnpe.forecast<-
  y.forecast%>%
  tk_ts(select=y,start = test.start, end=test.end, frequency = 12)
y.forecast<-
  select(tnpe.error.2,y)

error<- y.actual-tnpe.error.2
