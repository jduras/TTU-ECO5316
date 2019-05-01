
library(magrittr)
library(tidyverse)
library(tidyquant)
library(timetk)
library(ggplot2)
library(tibbletime)
library(urca)
library(tictoc)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggfortify)
library(egg)
library(tsibble)
library(lubridate)
library(zoo)
library(forecast)
library(broom)
library(sweep)


theme_set(theme_bw())


#a
econ_tbl_eco <-
  tq_get("PAYNSA", get = "economic.data", from = "1975-01-01", to = "2019-02-01")

econ_tbl <-
  econ_tbl_eco %>%
  rename(E = price)%>%
  #b
  
  mutate(lE = log(E),
         dE = E - lag(E),
         dlE1 = lE - lag(lE),
         dlE12 = lE - lag(lE, 12),
         d2lE12_1 = dlE12 - lag(dlE12))
# Plot the original and the transformed time series
econ_eco<- econ_tbl %>%
  gather(variable, value,-date) %>%
  mutate(variable = factor(variable, ordered = TRUE,
                           levels = c("E", "dE", "lE", "dlE1", "dlE12", "d2lE12_1"),
                           labels = c("E", 
                                      expression(paste(Delta,"E")),
                                      "ln(E)",
                                      expression(paste(Delta,"ln(E)")),
                                      expression(paste(Delta[12],"ln(E)")),
                                      expression(paste(Delta, Delta[12],"ln(E)")))))



econ_eco %>%
  ggplot( aes(x=date, y=value))+
  geom_line()+
  labs(x="",y="")+
  facet_wrap(~variable, scales = "free_y", labeller = label_parsed)

# From the above graphs, one can easily see that the original monthly data for Total Nonfarm Payroll Employment is 
#nonstationary. It shows some seasonal pattern and increasing trend, so we have to transform the data using the 
#logarithm and the differencing method.


#c

econ_tbl %>% 
  tk_ts(select=dlE1,start=c(1975,1),frequency=12) %>% 
  ggseasonplot(polar =TRUE)

# From the follwing graph, From the graph, both Delta E and Delta log E have depicted the seasonality 
#pattern almost every 12 months. 
#d
Acf(econ_tbl$lE, lag.max=48, main=expression(paste("ACF for l(E)")))
Pacf(econ_tbl$lE, lag.max=48, main=expression(paste("PACF for l(E)")))
#Plots the ACF and PACF for the dly1
Acf(econ_tbl$dlE1, lag.max=48, main=expression(paste("ACF for ",Delta,"l(E)")))
Pacf(econ_tbl$dlE1, lag.max=48, main=expression(paste("PACF for ",Delta,"l(E)")))

#Plots the ACF and PACF for the dly12
Acf(econ_tbl$dlE12, lag.max=48, main=expression(paste("ACF for ",Delta[12],"ln(E)")))
Pacf(econ_tbl$dlE12, lag.max=48, main=expression(paste("PACF for ",Delta[12],"ln(E)")))
#Plots the ACF and PACF for the d2ly12_1
Acf(econ_tbl$d2lE12_1, lag.max=48, main=expression(paste("ACF for ",Delta, Delta,"l(E)")))
Pacf(econ_tbl$d2lE12_1, lag.max=48, main=expression(paste("PACF for ",Delta, Delta,"l(E)")))

# The ACF of ln(E) seems to have high degree of persistence. So, it seems that we have to remove the trend.
#On the other hand, the PACF also shows the sign of a clear seasonality with a peak every 6 months. 


#The ACF still remainsto be highly persistant over the 4 years of included lags due to seasonality issues.
#Similarly, with regard to PACF, it is having the same pattern of seasonality with peaks around every 6 months. 
#However, the lags between the peaks are both significant and negative. With this
#seasonality problem, we see contractions in earnings when seasonal employment falls sharply.

#The ACF for the seasonally-differenced log earning does not seem to overcome the stationarity problem.
#Moreover, the PACF is also showing a large degree of seasonality with a peak each twelve months regardless 
#of the seasonal differences.

#The ACF decay here is pretty close to an exponential decay, similar to what we would observe from a pure AR process. 
#There is also a sign of  correlation around the twelfth lag which is quite similar to a seasonal MA
#component. The PACF is also depicting some similarity to the AR process for seasonally-differenced log changes.
#Two lags are highly significant; while the third lag is also significant still above the confidence interval.
#This proposes AR(2) with one seasonal AR term i.e., AR(3) process.                                                                                                   For Delta \log E_t, the first large spike in the ACF of log E_t has gone now, which is good, but there are some spikes at lag6, lag 12, lag18, lag24, and in the PACF, spikes are at lag6, lag 12, so there must be some seasonal pattern, and it's more like multiplicative seasonal MA model.                                                                        For Delta_{12} \log E_t, it's like AR(3) model.                                                                           For Delta \Delta_{12} \log E_t, There is a gradual decline every 12 lags in ACF, and in PACF, there are some large spikes in lag 1, lag2, lag3 and lag12, and small spikes in lag 13, lag 14 and lag 24, so we can estimate that the model is AR(3) with seasonal pattern, the frequency is 12, namely the model might be ARIMA(3,0,0)(3,0,0)[12], and we can estimate it later.  


#e
### ADF and KPSS test for ly
#ADF
econ.eco_lE<- 
  ur.df(econ_tbl$lE)
summary(econ.eco_lE)
#KPSS
kpss_data.eco_lE<- 
  ur.kpss(econ_tbl$lE)
summary(kpss_data.eco_lE)
###  and KPSS  test for dly
a<-econ_tbl%>%
  filter( !is.na(dlE1))

#ADF
econ.eco_dlE1<- 
  ur.df(a$dlE1)
summary(econ.eco_dlE1)

#KPSS
kpss_data.eco_dlE1<- 
  ur.kpss(a$dlE1)
summary(kpss_data.eco_dlE1) 

### ADF and KPSS test for dly12
b<-econ_tbl%>%
  filter( !is.na(dlE12))

#ADF
econ.eco_dlE12<- 
  ur.df(b$dlE12)
summary(econ.eco_dlE12)


#KPSS
kpssdata.eco_dlE12<- 
  ur.kpss(b$dlE12)
summary(kpssdata.eco_dlE12)

### ADF and KPSS test for d2ly12_1
c<-econ_tbl%>%
  filter( !is.na(d2lE12_1))

#ADF
econ.eco_dlE12_1<- 
  ur.df(c$d2lE12_1)
summary(econ.eco_dlE12_1)
#KPSS
kpssdata.eco_dlE12_1<- 
  ur.kpss(c$d2lE12_1)
summary(kpssdata.eco_dlE12_1)




# FOr log E_t, since tau3 in the DF tests is  -1.8841 which is larger than the critical value at
#1%, 5%, and 10%, so we fail to reject the null hypothesis and conclude that there is a unit root. In KPSS test,
#since the value of test-statistic is greater than the critical values at 1%, 2.5%, 5%, and 10%, 
#so we reject the null hypothesis and conclude that there is no mean stationarity. 


#f) split sample - estimation subsample dates
econ_tbl_1<- 
  econ_tbl%>%
  ts(start=c(1975,1), frequency = 12) %>%
  tk_tbl(rename_index = "yearm") %>%
  mutate(yearm=yearmonth(yearm)) %>%
  filter( !is.na(d2lE12_1))
train.start<-1975.00
train.end<-2014+(11/12)
test.start<-2015.00
test.end<- 2019+(1/12)


econ.ts<- econ_tbl_1 %>%
  tk_ts(select = d2lE12_1, start = train.start, frequency = 12)

econ.ts.1<-econ_tbl_eco.ts%>%
  tk_ts( start=train.start,end=train.end, frequency=12)

econ.ts.2<-
  econ_tbl%>%
  ts(start=c(1975,1), frequency = 12) %>%
  tk_tbl(rename_index = "yearm") %>%
  mutate(yearm=yearmonth(yearm)) %>%
  filter( !is.na(d2lE12_1))%>%
  filter(yearm >= yearmonth(test.start))%>%
  tk_ts( start=train.start,end=train.end, frequency=12)

#ACF PACF test for arima model
#Plots the ACF and PACF for the d2ly12_1
Acf(econ_tbl$d2lE12_1, lag.max=50, main=expression(paste("ACF for ",Delta, Delta,"l(E)")))
Pacf(econ_tbl$d2lE12_1, lag.max=50, main=expression(paste("PACF for ",Delta, Delta,"l(E)")))

m1<-Arima(econ.ts, order = c(2,0,0), seasonal=c(1,0,0))
ggtsdiag(m1, gof.lag=48)
autoplot(m1)

# From the graphs, one can get the estimated  ARIMA model: ARIMA(3,0,0)(3,0,0)[12].It DEPICTSs well in both the  
#estimated sampleand predicted sample. there is  no seasonal pattern present in the ACF of residuals.
#Finally standardized residuals are white noise, and p values are quite large.  


# (g) autoarima
m.auto<-auto.arima(econ.ts, seasonal=T, stationary=F, stepwise=F, approximation=F, ic="aic")

ggtsdiag(m.auto, gof.lags=50)

autoplot(m.auto)

# From  the auto.arima result, one can get the model is ARIMA(3,0,0)(1,0,1)[12] with zero mean. 
#the result is pretty well because no seasonal pattern is present in the ACF of residuals.
#In addition, standardized residuals are white noise, and p values are quite large. 

#(h) Use slide from tsibble package to create a rolling scheme

window.length <- length(econ_tbl_eco.ts.1)
tic()
results <-
  econ_tbl %>%  
  mutate(yearm = yearmonth(date)) %>%
  as_tsibble(index = yearm) %>% # covert to tsibble
  mutate(arma.model = slide(lE, ~Arima(.x, order = c(3,1,0), seasonal = list(order = c(1,1,1), period = 12)), 
                            .size = window.length)) %>%  
  filter(!is.na(arma.model)) %>%                                                              # remove periods at the beginning of sample where model could not be estimated due to lack of data,
  mutate(arma.coefs = map(arma.model, tidy, conf.int = TRUE),                                  # extract coefficients                                
         arma.f = map(arma.model, (. %>% forecast(h = 1) %>% sw_sweep())))                     #extract models
results
toc()


#(i) Plot the forecast for Et from (h)
m.1.fc.eco.rol <-
  results %>%
  as_tibble() %>%
  select(date, yearm, arma.f) %>%
  unnest() %>% 
  mutate(yearm = yearm %m+% months(1)) %>%
  filter(key == "forecast")

m.1.fc.eco.rol %>%
  ggplot(aes(x = yearm, y = value)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = "royalblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = "royalblue", alpha = 0.3) +
  geom_line(size = 0.7, col = "blue") +
  geom_line(data = (econ_tbl %>% filter(year(date) > 2000)) %>% mutate(yearm = yearmonth(date)), 
            aes(x = yearm, y = lE)) 

# From the result, it has been evident that the forecast doesn't perform well.Also the trend and seasonal
#pattern of the forecasts are not consistent with the characteristics of the actual data.   

#(j)Use the forecast for Et from (h) to construct the forecast 


m_2_fc_eco_rol <-
  results %>%
  select(yearm, arma.f) %>%
  as_tibble() %>%
  unnest(arma.f) %>%
  filter(key == "forecast") %>%
  mutate(yearm = yearm %m+% months(1)) %>%
  mutate_at(vars(value, lo.80, lo.95, hi.80, hi.95), funs(exp)) %>%
  rename(y = value) %>%
  select(yearm, key, y, lo.80, lo.95, hi.80, hi.95)

m_2_fc_eco_rol %>%
  filter(yearm >= "2013-01-01") %>%
  mutate(dE = y - lag(y)) %>%
  ggplot(aes(x = yearm, y = dE, col = key, linetype = key)) +
  # geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "blue", alpha = 0.1) +
  # geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "blue", alpha = 0.2) +
  geom_line(data = (econ_tbl %>% filter(year(date) >= 2013)) %>% mutate(yearm = yearmonth(date), key = "actual"), 
            aes(x = yearm, y = dE)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("gray40","darkblue")) +
  scale_linetype_manual(values = c("solid","solid")) 

#From the result, IT IS CLEAR THAT THE  forecast is  better  than the previous one. However, this forecast still 
#does not perform well. That is, forecasts are closer to the actual data from 2008-01-01 to 
#2009-11-01, because the forecast shows the trend and the seasonal pattern which are approximately consistent with
#the actual data. But from 2010, the forecasts deviate from the actual data too much. From the p values for 
#L-jungBoX statistic for the model ARIMA(1,0,0)(0,0,2)[12], P values are large for first 5 lags, and they become 
#quite low after the 5th lag.Hence, we can get some hints about the characteristic of the accuracy of 
#forecast of dE. 



#(k) Construct and plot the forecast errors for Et and for ???Et.
actual.econ<- 
  econ_tbl_1  %>%
  tk_ts(select=E, start = train.start, end=test.end, frequency = 12)
# tk_tbl(rename_index = "yearm") %>%
#mutate(yearm=yearmonth(yearm))

actual.eco.1<- 
  econ_tbl_1  %>%
  tk_ts(select=E, start = train.start, end=train.end, frequency = 12)


actual.eco.2<- 
  econ_tbl_1  %>%
  filter(yearm >= yearmonth(test.start)) %>%
  tk_ts(select=E, start = test.start, end=test.end, frequency = 12)

E.forecast<-
  m_2_fc_eco_rol %>%
  mutate(yearm = yearmonth(yearm))

econ.forecast<-
  E.forecast%>%
  tk_ts(select=y,start = test.start, end=test.end, frequency = 12)


error<- actual.eco.2-econ.forecast

autoplot(error)


#From the results above, although  errors of bothe models are large, but forecasts based on the dE is better 
#than the E's forecasts because results of dE showed lower RMSE and MAE.  

..