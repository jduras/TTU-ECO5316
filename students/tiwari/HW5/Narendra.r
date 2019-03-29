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
library(tibbletime) 
library(scales) 
library(tictoc) 
library(tseries) 
library(uroot)  
library(urca) 

#a)
data.tbl <-
  tq_get("PAYNSA", get = "economic.data", from = "1975-01-01", to = "2018-12-01")  %>%
  rename(E = price) %>%
  ts(start = c(1975,1), frequency = 12) %>%
  tk_tbl() %>%
  mutate(lE = log(E),
         dE = E - lag(E),
         dlE1 = lE - lag(lE),
         dlE12 = lE - lag(lE, 12),
         d2lE12_1 = dlE12 - lag(dlE12))
fstm <- 1975.00
lstm <- 2018-(1/12)
data_tbl_eco <- data.tbl %>% 
  filter(index <= as.yearmon(lstm)) 
# b)
data_tbl_eco%>% 
  gather(variable, value, -index) %>% 
  mutate(variable_label = factor(variable, ordered = TRUE, 
                                 levels = c("E", "lE", "dE", "dlE1", "dlE12", "d2lE12_1"), 
                                 labels = c("E", "log(E)", 
                                            expression(paste(Delta,"E")), 
                                            expression(paste(Delta,"log(E)")), 
                                            expression(paste(Delta[12],"log(E)")), 
                                            expression(paste(Delta,Delta[12],"log(E)"))))) %>%   
  ggplot(aes(x = index, y = value)) + 
  geom_hline(aes(yintercept = 0), linetype = "dotted") + 
  geom_line() + 
  scale_x_yearmon() + 
  labs(x = "", y = "") + 
  facet_wrap(~variable_label, ncol = 3, scales = "free", labeller = label_parsed) + 
  theme(strip.text = element_text(hjust = 0), 
        strip.background = element_blank())  
# From the above graphs, one can easily see that the original monthly data for Total Nonfarm Payroll Employment is 
#nonstationary. It shows some seasonal pattern and increasing trend, so we have to transform the data using the 
#logarithm and the differencing method.

#c
data_tbl_eco%>% tk_ts(select=dE, start=c(1975 ,1),frequency=12)%>%
  ggseasonplot 
data_tbl_eco%>% tk_ts(select=dlE1, start=c(1975,1),frequency=12)%>% 
  ggseasonplot
# From the follwing graph, From the graph, both Delta E and Delta log E have depicted the seasonality 
#pattern almost every 12 months. 


#d

maxlag <-24 
g1 <- data_tbl_eco  %>% pull(lE) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for log(E)"))) 
g2 <- data_tbl_eco  %>% pull(dlE1) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta,"log(E)"))) 
g3 <- data_tbl_eco  %>% pull(dlE12) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta[12], "log(E)"))) 
g4 <- data_tbl_eco  %>% pull(d2lE12_1) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta, Delta[12], "log(E)"))) 
g5 <- data_tbl_eco  %>% pull(lE) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for log(E)"))) 
g6 <- data_tbl_eco  %>% pull(dlE1) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta, "log(E)"))) 
g7 <- data_tbl_eco  %>% pull(dlE12) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta[12], "log(E)"))) 
g8 <- data_tbl_eco  %>% pull(d2lE12_1) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta,Delta[12], "log(E)"))) 
ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol = 4) 

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
data.eco_lE <- data.tbl %>% 
  tk_ts(select = lE, start = fstm, frequency = 12) 
ur.df(data.eco_lE,type="drift", selectlags="AIC")%>% summary() 
ur.kpss(data.eco_lE,type="mu", lags="long")%>% summary()

data.eco_dlE12 <- na.omit(data.tbl) %>% 
  tk_ts(select = dlE12, start = fstm, frequency = 12) 
ur.df(data.eco_dlE12,type="drift", selectlags="AIC")%>%summary()
ur.kpss(data.eco_dlE12,type="mu", lags="long")%>%summary()

data.eco_d2lE12_1 <- na.omit(data.tbl) %>% 
  tk_ts(select = d2lE12_1, start = fstm, frequency = 12) 
ur.df(data.eco_d2lE12_1,type="drift", selectlags="AIC")%>% summary() 
ur.kpss(data.eco_d2lE12_1,type="mu", lags="long")%>%summary() 
# FOr log E_t, since tau3 in the DF tests is  -1.8841 which is larger than the critical value at
#1%, 5%, and 10%, so we fail to reject the null hypothesis and conclude that there is a unit root. In KPSS test,
#since the value of test-statistic is greater than the critical values at 1%, 2.5%, 5%, and 10%, 
#so we reject the null hypothesis and conclude that there is no mean stationarity.       

#f)
fstm1 <- 1975.000  
lstm1 <- 2014-(1/12) 
fstm2 <- 2015.000  
lstm2 <- 2018-(1/12) 

#Using the ACF and PACF graph from (d), one can easily identify and estimate the suitable model: ARIMA(3,0,0)(3,0,0)[12] 
#Estimation Sample
AR3 <- data_tbl_eco %>% 
  tk_ts(select = d2lE12_1, start = fstm1, frequency = 12) %>%  
  Arima(order = c(3,0,0), seasonal = list(order = c(3,0,0), period = 12)) 
ggtsdiag(AR3, gof.lag = maxlag) 
#Prediction Sample
AR3_1<- data_tbl_eco %>% 
  tk_ts(select = d2lE12_1, start = fstm2, frequency = 12) %>%  
  Arima(order = c(3,0,0), seasonal = list(order = c(3,0,0), period = 12)) 
ggtsdiag(AR3_1, gof.lag = maxlag) 

# From the graphs, one can get the estimated  ARIMA model: ARIMA(3,0,0)(3,0,0)[12].It DEPICTSs well in both the  
#estimated sampleand predicted sample. there is  no seasonal pattern present in the ACF of residuals.
#Finally standardized residuals are white noise, and p values are quite large.  


#g
m_1<- data_tbl_eco %>% 
  tk_ts(select = d2lE12_1, start = fstm1, frequency = 12) %>% 
  auto.arima(ic = "aic", d = 1, D = 1, seasonal = TRUE,stationary = TRUE, stepwise = FALSE, approximation = FALSE) 
m_1 
ggtsdiag(m_1) 

# From  the auto.arima result, one can get the model is ARIMA(3,0,0)(1,0,1)[12] with zero mean. 
#the result is pretty well because no seasonal pattern is present in the ACF of residuals.
#In addition, standardized residuals are white noise, and p values are quite large. 

#h
fstm1 <- 1975  
lstm1 <- 2014-(1/12) 
data.eco.ts<- data.tbl%>% 
  filter(index <= as.yearmon(lstm1)) %>% 
  tk_ts(select = lE, start = lstm1+(1/12), frequency = 12) 
window.length <- nrow(data.eco.ts) 
tic() 
results <- 
  data.tbl %>% 
  mutate(yearm= yearmonth(index)) %>% 
  as_tsibble(index = yearm) %>%                                        
  mutate(sarima.model = slide(lE, ~auto.arima(.x, ic = "aicc", seasonal = TRUE, approximation = FALSE, stepwise = FALSE), 
                              .size = window.length)) %>%             
  filter(!is.na(sarima.model)) %>%                                    
  mutate(sarima.coefs = map(sarima.model, tidy, conf.int = TRUE), 
         sarima.fcst = map(sarima.model, (. %>% forecast(1) %>% sw_sweep()))) 
toc() 
results



#i)
fstm3 <- 1975  
lstm3 <- 2008-(1/12) 
data.eco_2.ts<- data.tbl%>% 
  filter(index <= as.yearmon(lstm1)) %>% 
  tk_ts(select = E, end=lstm3, frequency = 12) 

m_2<- data.eco_2.ts %>% 
  tk_ts(select=E, end=lstm3, frequency = 12) %>% 
  auto.arima(ic = "aic", d = 1, D = 1, seasonal = TRUE,stationary = TRUE, stepwise = FALSE, approximation = FALSE) 
m_2 
ggtsdiag(m_2) 
# 1-month ahead forecasts 
hmax <- 132 
fc_eco_1<- forecast(m_2, hmax)  
fc_eco_1  

autoplot(fc_eco_1) + 
  labs(x = "", y = "", 
       title = "Total Nonfarm Payroll Employment:multistep Forecast") 
# actual data   
actual_data_eco_tbl <- 
  data.tbl %>% 
  select(index, E) %>% 
  mutate(key = "actual", 
         date = as.Date(index)) %>% 
  select(date, key, E) %>% filter(year(date) >= 2008) 
actual_data_eco_tbl 
# extract the multistep forecasts, convert to levels 
m_3_tbl_fc_eco_1 <- 
  fc_eco_1 %>% 
  sw_sweep() %>%    
  filter(key == "forecast") %>% 
  mutate_at(vars(E, lo.80, lo.95, hi.80, hi.95), funs()) %>%   
  mutate(date = as.Date(index)) %>%      
  select(date, key, E, lo.80, lo.95, hi.80, hi.95) 
# forecast & actual data in a single tibble     
fc_actual_data_tbl_eco <- bind_rows(actual_data_eco_tbl,  m_3_tbl_fc_eco_1, .id = NULL) 
fc_actual_data_tbl_eco 
# plot 1-month ahead rolling forecasts - levels 
rfc_eco_1<-fc_actual_data_tbl_eco %>% 
  filter(date >= "1975-01-01") %>% 
  ggplot(aes(x = date, y = E, col = key, linetype = key)) + 
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "blue", alpha = 0.2) + 
  geom_line() + 
  geom_point() + 
  scale_color_manual(values = c("gray40","blue")) + 
  scale_linetype_manual(values = c("solid","solid")) + 
  labs(x = "", y = "", 
       title = "Total Nonfarm Payroll Employment: 1-Step Ahead Rolling Forecast") + 
  theme(legend.position = "none")  
rfc_eco_1 

# From the result, it has been evident that the forecast doesn't perform well.Also the trend and seasonal
#pattern of the forecasts are not consistent with the characteristics of the actual data.   

#j)
fstm3 <- 1975  
lstm3 <- 2008-(1/12) 
data.eco_3.ts<- data.tbl%>% 
  filter(index <= as.yearmon(lstm1)) %>% 
  tk_ts(select = dE, end=lstm3, frequency = 12) 
m_3<- data.eco_3.ts %>% 
  tk_ts(select = dE, end=lstm3, frequency = 12) %>% 
  auto.arima(ic = "aic", d = 1, D = 1, seasonal = TRUE,stationary = TRUE, stepwise = FALSE, approximation = FALSE) 
m_3 
ggtsdiag(m_3) 
# construct 1-month ahead forecasts 
hmax <- 132 
fc_eco_2<- forecast(m_3, hmax)  
fc_eco_2  
# plot 1month ahead forecasts-level 
autoplot(fc_eco_2) + 
  labs(x = "", y = "", 
       title = "The first difference of Total Nonfarm Payroll Employment:multistep Forecast") 
# actual data   
actual_data_eco_tbl_1 <- 
  data.tbl %>% 
  select(index, dE) %>% 
  mutate(key = "actual", 
         date = as.Date(index)) %>% 
  select(date, key, dE) %>% filter(year(date) >= 2008) 
actual_data_eco_tbl_1 
# extract the multistep forecasts, convert to levels 
m_3_tbl_fc_eco_2 <- fc_eco_2 %>% 
  sw_sweep() %>%  
  filter(key == "forecast") %>% 
  mutate_at(vars(dE, lo.80, lo.95, hi.80, hi.95), funs()) %>%   
  mutate(date = as.Date(index)) %>%     #mutate the index to date  
  select(date, key, dE, lo.80, lo.95, hi.80, hi.95) 
# forecast & actual data   
fc_actual_data_tbl_eco_1 <- bind_rows(actual_data_eco_tbl_1,  m_3_tbl_fc_eco_2,.id = NULL) 
fc_actual_data_tbl_eco_1  
# plot 1-month ahead rolling forecasts  
rfc_eco_2<- fc_actual_data_tbl_eco_1 %>% 
  filter(date >= "1975-01-01") %>% 
  ggplot(aes(x = date, y = dE, col = key, linetype = key)) + 
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "blue", alpha = 0.2) + 
  geom_line() + 
  geom_point() + 
  scale_color_manual(values = c("gray40","blue")) + 
  scale_linetype_manual(values = c("solid","solid")) + 
  labs(x = "", y = "", 
       title = "Total Nonfarm Payroll Employment: 1-Step Ahead Rolling Forecast") + 
  theme(legend.position = "none")    
rfc_eco_2 

#From the result, IT IS CLEAR THAT THE  forecast is  better  than the previous one. However, this forecast still 
#does not perform well. That is, forecasts are closer to the actual data from 2008-01-01 to 
#2009-11-01, because the forecast shows the trend and the seasonal pattern which are approximately consistent with
#the actual data. But from 2010, the forecasts deviate from the actual data too much. From the p values for 
#L-jungBoX statistic for the model ARIMA(1,0,0)(0,0,2)[12], P values are large for first 5 lags, and they become 
#quite low after the 5th lag.Hence, we can get some hints about the characteristic of the accuracy of 
#forecast of dE. 

#k)
# E
accuracy(m_3_tbl_fc_eco_1$E, actual_data_eco_tbl$E) 
# dE
accuracy(m_3_tbl_fc_eco_2$dE, actual_data_eco_tbl_1$dE) 

#From the results above, although  errors of bothe models are large, but forecasts based on the dE is better 
#than the E's forecasts because results of dE showed lower RMSE and MAE.  


