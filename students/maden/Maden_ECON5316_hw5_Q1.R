
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


# set default theme for ggplot2
theme_set(theme_bw())

# a. Obtain monthly data for Total Nonfarm Payroll Employment,


tnpe_tbl_all <-
  tq_get("PAYNSA", get = "economic.data", from = "1975-01-01", to = "2019-02-01")

# transforming the data  
  
  tnpe_tbl <-
  tnpe_tbl_all %>%
  rename(y = price)%>%
 
  #b. construct log, change, log-change, seasonal log change
  
  mutate(ly = log(y),
         dy = y - lag(y),
         dly1 = ly - lag(ly),
         dly12 = ly - lag(ly, 12),
         d2ly12_1 = dly12 - lag(dly12))


# Plot the original and the transformed time series
tnpe.l<- tnpe_tbl %>%
  gather(variable, value,-date) %>%
  mutate(variable = factor(variable, ordered = TRUE,
                           levels = c("y", "dy", "ly", "dly1", "dly12", "d2ly12_1"),
                           labels = c("y", 
                                      expression(paste(Delta,"y")),
                                      "ln(y)",
                                      expression(paste(Delta,"ln(y)")),
                                      expression(paste(Delta[12],"ln(y)")),
                                      expression(paste(Delta, Delta[12],"ln(y)")))))



tnpe.l %>%
  ggplot( aes(x=date, y=value))+
  geom_line()+
  labs(x="",y="")+
  facet_wrap(~variable, scales = "free_y", labeller = label_parsed)

# y and ln(y) have clear upwarding trends, they are almost similar . While ln(y)may have trend stationarity.
# Delta y shows that there is an  increase in the variance over time.
#  Delta ln(y) has less variance compared to Delta y. 


# (c) Use ggseasonplot to create seasonal plots 


tnpe_tbl %>% 
  tk_ts(select=dly1,start=c(1975,1),frequency=12) %>% 
  ggseasonplot(polar =TRUE)



#(d) Plot ACF and PACF for log Et, ??? log Et, ???12 log Et, ??????12 log Et


#Plots the ACF and PACF for the ly
Acf(tnpe_tbl$ly, lag.max=48, main=expression(paste("ACF for l(y)")))
Pacf(tnpe_tbl$ly, lag.max=48, main=expression(paste("PACF for l(y)")))

#There is a slow deacay in the acf for ln(y) and significant for large number of lags, 
#The PACF however is significant only at the first lag. Hence it is AR(1) process
# However, A very slowly decaying ACF suggests nonstationarity and presence of
#deterministic or stochastic trend in the time series

#Plots the ACF and PACF for the dly1
Acf(tnpe_tbl$dly1, lag.max=48, main=expression(paste("ACF for ",Delta,"l(y)")))
Pacf(tnpe_tbl$dly1, lag.max=48, main=expression(paste("PACF for ",Delta,"l(y)")))

# The Acf shows that there is a cyclical pattern with lag in multiple of six significant and positive
# while 2,3 and 4th lag being significant and  negative.
# The PACF shows 6th and 12 lag significant and positive 
# and few lags in multiple of 3,4 and 5 significant and negative
# doesn't address the seasonality problem 

#Plots the ACF and PACF for the dly12
Acf(tnpe_tbl$dly12, lag.max=48, main=expression(paste("ACF for ",Delta[12],"ln(y)")))
Pacf(tnpe_tbl$dly12, lag.max=48, main=expression(paste("PACF for ",Delta[12],"ln(y)")))

#The ACF for this also does not seem overcome the stationarity problem. There is a cyclical pattern.
#The PACF is also showing a large degree of seasonality with a peak each twelve months.

#Plots the ACF and PACF for the d2ly12_1
Acf(tnpe_tbl$d2ly12_1, lag.max=48, main=expression(paste("ACF for ",Delta, Delta,"l(y)")))
Pacf(tnpe_tbl$d2ly12_1, lag.max=48, main=expression(paste("PACF for ",Delta, Delta,"l(y)")))

# The ar seems to be declining slowly with significant correlation in 12th lag.
# Based on ACF and PACF i estimate AR(3) model with seasonality. 





#(e) ADF and KPSS test

### ADF and KPSS test for ly
#ADF
dfly<- 
  ur.df(tnpe_tbl$ly)
summary(dfly)

#ADF test shows that the test statistics is positive when critical values are negative. 
# It means we accept null and there is presence of unit root.
#KPSS
kpssly<- 
  ur.kpss(tnpe_tbl$ly)
summary(kpssly)
# From the result of KPSS tess we reject the null and there is presence ot unit root.


###  and KPSS  test for dly
a<-tnpe_tbl%>%
  filter( !is.na(dly1))

#ADF
dfdly1<- 
  ur.df(a$dly1)
summary(dfdly1)

# test statistic is less than the critical value so we conclude the absence of unit roots. 

#KPSS
kpssdly1<- 
  ur.kpss(a$dly1)
summary(kpssdly1)

# From the result of KPSS test we reject the null at 10% significance. 
#So the unit root is present only at 10% significance.


### ADF and KPSS test for dly12
b<-tnpe_tbl%>%
  filter( !is.na(dly12))

#ADF
dfdly12<- 
  ur.df(b$dly12)
summary(dfdly12)
# Here we reject the null only at  5 and 10% level of significance. 
#so we conclude absence of unit root at that level of signigicance.

#KPSS
kpssdly12<- 
  ur.kpss(b$dly12)
summary(kpssdly12)

# here we reject the null hypotehsis as test statistic is greater than the critical values.
# we conclude presence of unit root .


### ADF and KPSS test for d2ly12_1
c<-tnpe_tbl%>%
  filter( !is.na(d2ly12_1))

#ADF
dfdly12_1<- 
  ur.df(c$d2ly12_1)
summary(dfdly12_1)

#With ADF test, we reject the null hypothesis. There is no unit roots. 
#KPSS
kpssd2ly12_1<- 
  ur.kpss(c$d2ly12_1)
summary(kpssd2ly12_1)
# With KPSS test we fail to reject the null hypothesis. There is no unit roots.

#Interpretation:

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

tnpe.ts.2<-
  tnpe_tbl%>%
  ts(start=c(1975,1), frequency = 12) %>%
  tk_tbl(rename_index = "yearm") %>%
  mutate(yearm=yearmonth(yearm)) %>%
  filter( !is.na(d2ly12_1))%>%
  filter(yearm >= yearmonth(test.start))%>%
  tk_ts( start=train.start,end=train.end, frequency=12)


#ACF PACF test for arima model
#Plots the ACF and PACF for the d2ly12_1
Acf(tnpe_tbl$d2ly12_1, lag.max=50, main=expression(paste("ACF for ",Delta, Delta,"l(y)")))
Pacf(tnpe_tbl$d2ly12_1, lag.max=50, main=expression(paste("PACF for ",Delta, Delta,"l(y)")))


#While the ACF really doesn't have any sudden drops, it has decay faster than we would see with a pure AR process. 
#There is also some seasonality problem in the ACF. The seasonal MA term may be inferred to the AR process. 
#One the other hand, there is obviously a seasonality in the PACF with every 12 month with significance even 
#after four years. Further,  the second lag is also highly significant following each of the seasonal terms. 
#Since the third lag is barely significant, I prefer to go with an $SARIMA(2,0,0)(1,0,0)

m1<-Arima(tnpe.ts, order = c(2,0,0), seasonal=c(1,0,0))
ggtsdiag(m1, gof.lag=48)

#The Ljung-Box statistics indicate there is a large problem with the model with close to significance across all lags. 
#This suggest that these would be significant when we adjust the  degree of freedom. 
#Thus, the lagged residuals are predicting the current residuals. When it comes to the ACF, there is no issues across 
#the residual plot, indicating high values followed high values and low is followed by another. 
#This is likely due to the seasonal AR term not that  is controlling for the seasonality properly.
autoplot(m1)
#All of the inverse roots of the AR terms is fitting well inside the unit circle, so the model is stationary.

# (g) autoarima
m.auto<-auto.arima(tnpe.ts, seasonal=T, stationary=F, stepwise=F, approximation=F, ic="aic")

ggtsdiag(m.auto, gof.lags=50)

autoplot(m.auto)

#(h) Use slide from tsibble package to create a rolling scheme

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

#(i) Plot the forecast for Et from (h)
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



#(j)Use the forecast for Et from (h) to construct the forecast 


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
 

#(k) Construct and plot the forecast errors for Et and for ???Et.
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
  filter(yearm >= yearmonth(test.start)) %>%
  tk_ts(select=y, start = test.start, end=test.end, frequency = 12)

y.forecast<-
   m2_f_1_rol %>%
  mutate(yearm = yearmonth(yearm))
 
tnpe.forecast<-
  y.forecast%>%
  tk_ts(select=y,start = test.start, end=test.end, frequency = 12)


error<- tnpe.actual.2-tnpe.forecast

autoplot(error)
