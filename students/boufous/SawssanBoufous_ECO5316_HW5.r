library(tidyquant)
library(timetk)
library(tibbletime)
library(broom)
library(sweep)
library(ggplot2)
library(scales)
library(ggfortify)
library(egg)
library(tsibble)
library(tictoc)
library(forecast)
library(tseries)
library(uroot)
library(urca)

# Set default theme for ggplot2
theme_set(theme_bw())


#(a) Obtain monthly data for Total Nonfarm Payroll Employment, Not Seasonally Adjusted, available on FRED under code [`PAYNSA`]https://fred.stlouisfed.org/series/PAYNSA(). Import the 1975M1-2018M12 sample using `tq_get`.
#&(b) Construct the following transformed time series

TNPE.tbl <-
  tq_get("PAYNSA", get = "economic.data", from = "1975-01-01", to = "2018-12-01")  %>%
  rename(y = price) %>%
  ts(start = c(1975,1), frequency = 12) %>%
  tk_tbl() %>%
  mutate(ly = log(y),
         dy = y - lag(y),
         dly1 = ly - lag(ly),
         dly12 = ly - lag(ly, 12),
         d2ly12_1 = dly12 - lag(dly12))

#Plot the original and the transformed time series
fstm <- 1975.00 
lstm <- 2019-(1/12)
TNPE_tbl_1 <- TNPE.tbl %>%
  filter(index <= as.yearmon(lstm))

TNPE_tbl_1%>%
  gather(variable, value, -index,-date) %>%
  mutate(variable_label = factor(variable, ordered = TRUE,
                                 levels = c("y", "ly", "dy", "dly1", "dly12", "d2ly12_1"),
                                 labels = c("y", "log(y)",
                                            expression(paste(Delta,"y")),
                                            expression(paste(Delta,"log(y)")),
                                            expression(paste(Delta[12],"log(y)")),
                                            expression(paste(Delta,Delta[12],"log(y)"))))) %>%  
  ggplot(aes(x = index, y = value)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  geom_line() +
  scale_x_yearmon() +
  labs(x = "", y = "") +
  facet_wrap(~variable_label, ncol = 3, scales = "free", labeller = label_parsed) +
  theme(strip.text = element_text(hjust = 0),
        strip.background = element_blank())   

# 'y' and 'log (y)' show a trending pattern from which we can state that there are 
#non  stationary, While all other transformations of log(y) seem to be stationary
# Delta log (y) displays a sesonal pattern


#(c) Use `ggseasonplot` to create seasonal plots for Delta y_t and Delta \log y_t
TNPE_tbl_1%>% tk_ts(select=dy, start=c(1975 ,1),frequency=12)%>% ggseasonplot
TNPE_tbl_1%>% tk_ts(select=dly1, start=c(1975,1),frequency=12)%>% ggseasonplot

#The plot shows a seasonal pattern that reaches the highest point in September and the lowest in July


#(d) Plot ACF and PACF for log y, Delta log y, Delta_12log y, Delta Delta12 log y

maxlag <-24
g1 <- TNPE_tbl_1  %>% pull(ly) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for log(y)")))
g2 <- TNPE_tbl_1  %>% pull(dly1) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta,"log(y)")))
g3 <- TNPE_tbl_1  %>% pull(dly12) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta[12], "log(y)")))
g4 <- TNPE_tbl_1  %>% pull(d2ly12_1) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta, Delta[12], "log(y)")))
g5 <- TNPE_tbl_1  %>% pull(ly) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for log(y)")))
g6 <- TNPE_tbl_1  %>% pull(dly1) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta, "log(y)")))
g7 <- TNPE_tbl_1  %>% pull(dly12) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta[12], "log(y)")))
g8 <- TNPE_tbl_1 %>% pull(d2ly12_1) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta,Delta[12], "log(y)")))

ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol = 4)

# ACF for log(y) shows tails off while PACF peaks at lag 1 
#and shows seansonality every 6 months, 
# Same for delta log(y), ACF and PACF  show seasonality for every six months
# While for delta 12 log (y) we observe tails off for both ACF and PACF and 
#seasonality every 12 months
# and same thing is for the twice differenciated  12 log (y)


#(e) Perform the ADF and KPSS tests on $\log E_t, \Delta_{12} \log E_t, \Delta \Delta_{12} \log E_t$. Summarize the results.

# ADF test: if we reject, it is not unit root test, 
#which means that there is  stationarity. Thus, the data doesn't need 
#to be differenced. If we fail to reject H0 then the data is not stationary and thus,
# needs differenciation 

# KPSS Test : rejecting H0 means that the data is not stationary, 
#and it is not otherwise.

TNPE.ts1 <- TNPE.tbl %>%
  tk_ts(select = ly, start = fstm, frequency = 12)
ur.df(TNPE.ts1,type="drift", selectlags="AIC")%>%summary()
ur.kpss(TNPE.ts1,type="mu", lags="long")%>%summary()



TNPE.ts2 <- na.omit(TNPE.tbl) %>%
  tk_ts(select = dly12, start = fstm, frequency = 12)
ur.df(TNPE.ts2,type="drift", selectlags="AIC")%>%summary()
ur.kpss(TNPE.ts2,type="mu", lags="long")%>%summary()




TNPE.ts3 <- na.omit(TNPE.tbl) %>%
  tk_ts(select = d2ly12_1, start = fstm, frequency = 12)
ur.df(TNPE.ts3,type="drift", selectlags="AIC")%>%summary()
ur.kpss(TNPE.ts3,type="mu", lags="long")%>%summary()


#Comments: 
#For log(y), since tau3 in the DF tests is  -1.8841 which is larger than the critical value at 1%, 5%, and 10%, so we fail to reject the null hypothesis and conclude that there is a unit root. 
#In KPSS test, since the value of test-statistic is  2.7416, which is greater than the critical values at 1%, 2.5%, 5%, and 10%, so we reject the null hypothesis and conclude that there is no mean stationarity.                                                                    

#For Delta{12}log (y), since tau3 in the DF tests is -2.6022 which is larger than the critical value at 1%, 5%, so we fail to reject the null hypothesis and conclude that there is a unit root. 
#In KPSS test, since the value of test-statistic is 0.4387, which is smaller than the critical values at 1%, 2.5%, 5%, so we fail to reject the null hypothesis and conclude that there is mean stationarity.                                                                       

#For Delta_Delta{12}log(y), since tau3 in the DF tests is -8.4245 which is smaller than the critical value at 1%, 5%, and 10%, so we reject the null hypothesis and conclude that there is no unit root. 
#In KPSS test, since the value of test-statistic is 0.0241, which is smaller than the critical values at 1%, 2.5%, 5%, and 10%, so we fail to reject the null hypothesis and conclude that there is mean stationarity.                                                                  

#Therefore, by taking two differences of log(y), the data now is mean stationary and with no unit root, which is good. 


#(f) Split the sample into two parts: estimation sample from 1975M1 to 2014M12, and prediction sample from 2015M1 to 2018M12. Use ACF and PACF from (c) to identify and estimate a suitable model for $\Delta \Delta_{12} \log E_t$ using `Arima`. Check the estimated model for adequacy - diagnose residuals using `ggtsdiag`


# split sample - estimation and prediction subsamples
fstm1 <- 1975.000 
lstm1 <- 2015-(1/12)
fstm2 <- 2015.000 
lstm2 <- 2019-(1/12)

##Use ACF and PACF from (d), we can identify and estimate a suitable model is ARIMA(3,0,0)(3,0,0)[12]
TNPE_S<- TNPE_tbl_1 %>%
  tk_ts(select = d2ly12_1, start = fstm1, frequency = 12) %>% 
  Arima(order = c(3,0,0), seasonal = list(order = c(3,0,0), period = 12))
ggtsdiag(TNPE_S, gof.lag = maxlag)

# ACF : tails off and PACFis cut off after lag 3 . 
# AR(3), and sessonal will be aram because both of them were tailing off
# The estimated model ARIMA(3,0,0)(3,0,0)[12] perform well in both estimation sample and prediction sample, no seasonal pattern is present in the ACF of residuals, standardized residuals are white noise, and p values are quite large. 


#(g) Use `auto.arima` to find the best model for $\log E_t$. Check the estimated model for adequacy - diagnose residuals using `ggtsdiag`. 

TNPES_1<- TNPE_tbl_1 %>%
  tk_ts(select = ly, start = fstm1, frequency = 12) %>%
  auto.arima(ic = "aic", seasonal = TRUE,stationary = FALSE, stepwise = FALSE, approximation =  FALSE)
TNPES_1
ggtsdiag(TNPES_1,gof.lag=36)

#(h) Use `slide` from `tsibble` package to create a rolling scheme sequence of 1 period ahead forecasts 

fstm1 <- 1975 
lstm1 <- 2015-(1/12)

window.length <- TNPE.tbl%>%
  filter(index <= as.yearmon(lstm1)) %>%
  nrow()

tic()
results <-
  TNPE.tbl %>%
  mutate(yearm= yearmonth(index)) %>%
  as_tsibble(index = yearm) %>%                                        
  mutate(sarima.model = slide(ly, ~auto.arima(.x, ic = "aicc", seasonal = TRUE, approximation = FALSE, stepwise = TRUE),
                              .size = window.length)) %>%             # estimate models
  filter(!is.na(sarima.model)) %>%                                    
  mutate(sarima.coefs = map(sarima.model, tidy, conf.int = TRUE),
         sarima.fcst = map(sarima.model, (. %>% forecast(1) %>% sw_sweep())))
toc()
results


#(i) Plot the forecast from (h) 

#Forecasts
fstm1 <- 1975 
lstm1 <- 2015-(1/12)
TNPE.ts_4<- TNPE.tbl%>%
  filter(index <= as.yearmon(lstm1)) %>%
  tk_ts(select = y, start=fstm1, frequency = 12)  

#Comments: 
# The model obtained is ARIMA(3,1,0)(1,1,1)[12]. No seasonal pattern is present in the ACF of residuals, standardized residuals are white noise and p values are large.

# Actual data  
ACTNPE_tbl <-
  TNPE.tbl %>%
  select(index, y) %>%
  mutate(key = "actual",
         date = as.Date(index)) %>%
  select(date, key, y) %>% filter(year(date) >= 2008)
ACTNPE_tbl

# Estimate rolling SARIMA model, create 1 step ahead forecasts
TNPE_tbl_f_1 <-   results %>%
  select(yearm, sarima.fcst) %>%
  as_tibble() %>%
  unnest(sarima.fcst) %>%
  filter(key == "forecast") %>%
  mutate(yearm = yearm %m+% months(1) %>% yearmonth()) %>%
  mutate_at(vars(value, lo.80, lo.95, hi.80, hi.95), list(exp)) %>%
  rename(y = value) %>%
  select(yearm, key, y, lo.80, lo.95, hi.80, hi.95)


# Forecast & actual data in a single tibble    
FORTNPE_tbl <- bind_rows(ACTNPE_tbl ,
                     TNPE_tbl_f_1 %>%
                       mutate(date= as.Date(yearm)) %>%
                       select(date, key, y, lo.80, lo.95, hi.80, hi.95), .id = NULL)
FORTNPE_tbl


# Plot 1-month ahead rolling forecasts - levels
ROLL1<-FORTNPE_tbl %>%
  filter(date >= "1975-01-01") %>%
  ggplot(aes(x =date, y = y, col = key, linetype = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "blue", alpha = 0.1) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "blue", alpha = 0.2) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("red","pink")) +
  scale_linetype_manual(values = c("solid","solid")) +
  labs(x = "", y = "",
       title = "Total Nonfarm Payroll Employment: 1-Step Ahead Rolling Forecast") +
  theme(legend.position = "none") 
ROLL1

# Comments: 
# It seems that this forecast performs well. 
#The trend and seasonal pattern of the forecasts are close to the characteristics of the actual data.  
  
  
#(j) Use the forecast  from (h) to construct the forecast for Delta(y) 

TNPE.ts_5<- TNPE.tbl%>%
  filter(index <= as.yearmon(lstm1)) %>%
  tk_ts(select = dy, start=fstm1, frequency = 12)  

# Actual data  
TNPE_tbl_2 <-
  TNPE.tbl %>%
  select(index, dy) %>%
  mutate(key = "actual",
         date = as.Date(index)) %>%
  select(date, key, dy) %>% filter(year(date) >= 2008)
TNPE_tbl_2

# Estimate rolling SARIMA model, create 1 step ahead forecasts
ROLL_tbl_f_2 <-results %>%
  select(yearm, sarima.fcst) %>%
  as_tibble() %>%
  unnest(sarima.fcst) %>%
  filter(key == "forecast") %>%
  mutate(yearm = yearm %m+% months(1) %>% yearmonth()) %>%
  mutate_at(vars(value, lo.80, lo.95, hi.80, hi.95), list(exp)) %>%
  mutate(dy = value-lag(value)) %>%
  select(yearm, key, dy, lo.80, lo.95, hi.80, hi.95)

# Forecast & actual data in a single tibble    
FOREACT_tbl <- bind_rows(TNPE_tbl_2,
                      ROLL_tbl_f_2 %>%
                         mutate(date= as.Date(yearm)) %>%
                         select(date, key, dy, lo.80, lo.95, hi.80, hi.95), .id = NULL)
FOREACT_tbl


# Plot 1-month ahead rolling forecasts - levels
ROLLPLOT<-FOREACT_tbl %>%
  filter(date >= "2013-01-01") %>%
  ggplot(aes(x = date, y = dy, col = key, linetype = key)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("green","pink")) +
  scale_linetype_manual(values = c("solid","solid")) +
  labs(x = "", y = "",
       title = "First difference of Nonfarm Payroll Employment: 1-Step Ahead Rolling Forecast") 

ROLLPLOT

#Comments: 
# This forecast doesn't perform well,  it is not consisent with the seasonality pattern of the actual data. 


#(k) Construct and plot the forecast errors 

#The forecast errors 
FORTNPE_tbl %>% 
  select(date, key, y)%>%
  spread(key,y)%>%
  mutate(error=actual-forecast,
         percen_error=(actual-forecast)/actual*100) %>%
  tk_ts(select=percen_error, start=2008,frequency = 12)%>%
  autoplot()

#The forecast for errors
FOREACT_tbl %>% 
  select(date, key, dy)%>%
  spread(key,dy)%>%
  mutate(error=actual-forecast) %>%
  filter(date >="2015-01-01") %>%
  tk_ts(select=error, start=2008,frequency = 12)%>%
  autoplot()     


#Comments: 
# From the graphs we can get the forecast percentage error term for (y) fluctuates from -1.5% to 1%, is quite large. 
# For the forecast  error term for Delta (y), the range of the error term is still  quite large which leads to problems of seasonal pattern. 
# Therefore both forecasts need improvements. 