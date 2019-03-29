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


#a&b)
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
data_tbl_1 <- data.tbl %>% 
  filter(index <= as.yearmon(lstm)) 

data_tbl_1%>% 
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
#c)
data_tbl_1%>% tk_ts(select=dE, start=c(1975 ,1),frequency=12)%>%
  ggseasonplot 
data_tbl_1%>% tk_ts(select=dlE1, start=c(1975,1),frequency=12)%>% 
  ggseasonplot 
#Both graph showe that there is seasonality in both dE and dlE1.
#d
maxlag <-24 
g1 <- data_tbl_1  %>% pull(lE) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for log(E)"))) 
g2 <- data_tbl_1  %>% pull(dlE1) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta,"log(E)"))) 
g3 <- data_tbl_1  %>% pull(dlE12) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta[12], "log(E)"))) 
g4 <- data_tbl_1  %>% pull(d2lE12_1) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta, Delta[12], "log(E)"))) 
g5 <- data_tbl_1  %>% pull(lE) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for log(E)"))) 
g6 <- data_tbl_1  %>% pull(dlE1) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta, "log(E)"))) 
g7 <- data_tbl_1  %>% pull(dlE12) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta[12], "log(E)"))) 
g8 <- data_tbl_1 %>% pull(d2lE12_1) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta,Delta[12], "log(E)"))) 
ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol = 4) 
# lE: ACF shows a very high degree pf presistence,therefore, the trend needs to be removed. However, every 6 month
#PACF shows seasonlity.
#dlE: the seasonality issue is causing highly persistant in the ACF eventhough the 4 years of lags is included. But
# the lags between the peaks are significant.
#d12lE: the ACF still have the sationtionarity problem.And PACF every 12 month shows a larg degree of seasonlity.
#dd12E: the ACF decay is close to exponential decay of AR process and around the 12 lag there is a sign of correletion
# coefficient which is similar to a seasonal MA. The first , second, and third are significant therefore, we would choose
# AR(2) with one seasonal AR term.
#e)
auto.arima(data_tbl_1$lE, ic = "aic", d = 1, D = 1, seasonal = TRUE, stationary = FALSE, stepwise = FALSE, approximation = FALSE)
data.lE <- data.tbl %>% 
  tk_ts(select = lE, start = fstm, frequency = 12) 
ur.df(data.lE,type="drift", selectlags="AIC")%>%
  summary() 
# we accept null hypothesis for presence of a unit root at all traditional confidence levels.
ur.kpss(data.lE,type="mu", lags="long")%>%
  summary() 
# we reject the null hypothesis because of test-statistic > all traditional confidence levels 
# therefore, there is a unit root present
data.dlE12 <- na.omit(data.tbl) %>% 
  tk_ts(select = dlE12, start = fstm, frequency = 12) 
ur.df(data.dlE12,type="drift", selectlags="AIC")%>%
  summary()
# we accept null hypothesis for presence of a unit root at all traditional confidence levels.
ur.kpss(data.dlE12,type="mu", lags="long")%>%
  summary() 
# we accept null hypothesis at 5% and reject the null hypothesis at 10%. However, the ADF test of ln(E) is stationary
#which coincide with what we het but with a lower degree of certainty compared to ADF.
data.d2lE12_1 <- na.omit(data.tbl) %>% 
  tk_ts(select = d2lE12_1, start = fstm, frequency = 12) 
ur.df(data.d2lE12_1,type="drift", selectlags="AIC")%>%
  summary() 
#we reject null hypothesis for presence of a unit root at all traditional confidence levels. 
ur.kpss(data.d2lE12_1,type="mu", lags="long")%>%
  summary() 
#we accept null hypothesis for presence of a unit root at all traditional confidence levels.
#f)
fstm1 <- 1975.000  
lstm1 <- 2014-(1/12) 
fstm2 <- 2015.000  
lstm2 <- 2018-(1/12) 
#Estimation Sample
ARMA21<- data_tbl_1 %>% 
  tk_ts(select = d2lE12_1, start = fstm1, frequency = 12) %>%  
  Arima(order = c(2,0,1), seasonal = list(order = c(2,0,1), period = 12)) 
ggtsdiag(ARMA21, gof.lag = maxlag) 
#Prediction Sample
ARMA21_p<- data_tbl_1 %>% 
  tk_ts(select = d2lE12_1, start = fstm2, frequency = 12) %>%  
  Arima(order = c(2,0,1), seasonal = list(order = c(2,0,1), period = 12)) 
ggtsdiag(ARMA21_p, gof.lag = maxlag) 
# ARMA21 is preform well in both Estimation and Prdiction Sample
#g)
M1<- data_tbl_1 %>% 
  tk_ts(select = d2lE12_1, start = fstm1, frequency = 12) %>% 
  auto.arima(ic = "aic", d = 1, D = 1, seasonal = TRUE,stationary = TRUE, stepwise = FALSE, approximation = FALSE) 
M1 
ggtsdiag(M1) 
# auto.arima give the model ARIMA(3,0,0)(1,0,1)[12] with zero mean. p-values for Ljung-Box Statistic is significant
# and there is no seasonality in the ACF.
#h)     
fstm1 <- 1975  
lstm1 <- 2014-(1/12) 
data.paH_1.ts<- data.tbl%>% 
  filter(index <= as.yearmon(lstm1)) %>% 
  tk_ts(select = lE, start = lstm1+(1/12), frequency = 12) 
window.length <- nrow(data.paH_1.ts) 
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
data.paH_2.ts<- data.tbl%>% 
  filter(index <= as.yearmon(lstm1)) %>% 
  tk_ts(select = E, end=lstm3, frequency = 12) 

M2<- data.paH_2.ts %>% 
  tk_ts(select=E, end=lstm3, frequency = 12) %>% 
  auto.arima(ic = "aic", d = 1, D = 1, seasonal = TRUE,stationary = TRUE, stepwise = FALSE, approximation = FALSE) 
M2 
ggtsdiag(M2) 
# 1-month ahead forecasts 
hmax <- 132 
fc_1<- forecast(M2, hmax)  
fc_1  

autoplot(fc_1) + 
  labs(x = "", y = "", 
       title = "Total Nonfarm Payroll Employment:multistep Forecast") 
# actual data   
act_data_tbl <- 
  data.tbl %>% 
  select(index, E) %>% 
  mutate(key = "actual", 
         date = as.Date(index)) %>% 
  select(date, key, E) %>% filter(year(date) >= 2008) 
act_data_tbl 
# extract the multistep forecasts, convert to levels 
Mf_tbl_fc_1 <- 
  fc_1 %>% 
  sw_sweep() %>%    
  filter(key == "forecast") %>% 
  mutate_at(vars(E, lo.80, lo.95, hi.80, hi.95), funs()) %>%   
  mutate(date = as.Date(index)) %>%      
  select(date, key, E, lo.80, lo.95, hi.80, hi.95) 
# forecast & actual data in a single tibble     
fc_act_data_tbl_0 <- bind_rows(act_data_tbl,  Mf_tbl_fc_1, .id = NULL) 
fc_act_data_tbl_0 
# plot 1-month ahead rolling forecasts - levels 
mfc1<-fc_act_data_tbl_0 %>% 
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
mfc1 
#j)
fstm3 <- 1975  
lstm3 <- 2008-(1/12) 
data.paH_3.ts<- data.tbl%>% 
  filter(index <= as.yearmon(lstm1)) %>% 
  tk_ts(select = dE, end=lstm3, frequency = 12) 
M3<- data.paH_3.ts %>% 
  tk_ts(select = dE, end=lstm3, frequency = 12) %>% 
  auto.arima(ic = "aic", d = 1, D = 1, seasonal = TRUE,stationary = TRUE, stepwise = FALSE, approximation = FALSE) 
M3 
ggtsdiag(M3) 
# construct 1-month ahead forecasts 
hmax <- 132 
fc2<- forecast(M3, hmax)  
fc2  
# plot 1month ahead forecasts-level 
autoplot(fc2) + 
  labs(x = "", y = "", 
       title = "The first difference of Total Nonfarm Payroll Employment:multistep Forecast") 
# actual data   
act_data_tbl_1 <- 
  data.tbl %>% 
  select(index, dE) %>% 
  mutate(key = "actual", 
         date = as.Date(index)) %>% 
  select(date, key, dE) %>% filter(year(date) >= 2008) 
act_data_tbl_1 
# extract the multistep forecasts, convert to levels 
Mf_tbl_fc2 <- fc2 %>% 
  sw_sweep() %>%  
  filter(key == "forecast") %>% 
  mutate_at(vars(dE, lo.80, lo.95, hi.80, hi.95), funs()) %>%   
  mutate(date = as.Date(index)) %>%     #mutate the index to date  
  select(date, key, dE, lo.80, lo.95, hi.80, hi.95) 
# forecast & actual data   
fc1_act_data_tbl <- bind_rows(act_data_tbl_1,  Mf_tbl_fc2,.id = NULL) 
fc1_act_data_tbl  
# plot 1-month ahead rolling forecasts  
mfc2<- fc1_act_data_tbl %>% 
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
mfc2 
#k)
#for E
accuracy(Mf_tbl_fc_1$E, act_data_tbl$E) 
#for dE
accuracy(Mf_tbl_fc2$dE, act_data_tbl_1$dE) 
