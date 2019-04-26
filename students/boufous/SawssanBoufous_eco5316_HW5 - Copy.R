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
library(tsibble)
library(knitr)

# Set default theme for ggplot2
theme_set(theme_bw() + 
            theme(strip.background = element_blank()))

# (a) Obtain monthly data for Total Nonfarm Payroll Employment, 1974M1-2018m12
# & (b) Construct the following transformed time series
TNPE_tbl_all <-
  tq_get("PAYNSA",
         get = "economic.data",
         from = "1975-01-01",
         to = "2018-12-01") %>%
  rename(y = price) %>%
  ts(start = c(1975,1), frequency = 12) %>%
  tk_tbl(rename_index = "yearm") %>%
  mutate(yearm = yearmonth(yearm),
         ly = log(y),
         dy = y - lag(y),
         dly1 = ly - lag(ly),
         dly12 = ly - lag(ly, 12),
         d2ly12_1 = dly12 - lag(dly12))

glimpse(TNPE_tbl_all)
tail(TNPE_tbl_all)

#Plot original and transformed time series 

TNPE_tbl_all %>%
  gather(variable, value, -yearm) %>%
  mutate(variable_label = factor(variable, ordered = TRUE,
                                 levels = c("y", "ly", "dy", "dly1", "dly12", "d2ly12_1"),
                                 labels = c("y", "log(y)",
                                            expression(paste(Delta,"y")),
                                            expression(paste(Delta,"log(y)")),
                                            expression(paste(Delta[12],"log(y)")),
                                            expression(paste(Delta,Delta[12],"log(y)"))))) %>%
  ggplot(aes(x = yearm, y = value)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  geom_line() +
  labs(x = "", y = "") +
  facet_wrap(~variable_label, ncol = 3, scales = "free", labeller = label_parsed) +
  theme(strip.text = element_text(hjust = 0),
        strip.background = element_blank())


# 'y' and 'log (y)' show a trending pattern from which we can state that there are 
#non  stationary, While all other transformations of log(y) seem to be stationary
# Delta log (y) displays a sesonal pattern



#(c)Use ggseasonplot
data_dy <- ts(TNPE_tbl_all[, "dy"], frequency = 12, start = c(1975, 1))
ggseasonplot(data_dy)
data_dly1 <- ts(TNPE_tbl_all[, "dly1"], frequency = 12, start = c(1975, 1))
ggseasonplot(data_dly1)

#The plot shows a seasonal pattern that reaches the highest point in September and the lowest in July


#(d) Plot ACF and PACF

maxlag <-50


g1 <- TNPE_tbl_all %>% pull(ly) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for log(y)")))
g2 <- TNPE_tbl_all %>% pull(dly1) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta,"log(y)")))
g3 <- TNPE_tbl_all %>% pull(dly12) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta[12], "log(y)")))
g4 <- TNPE_tbl_all %>% pull(d2ly12_1) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta, Delta[12], "log(y)")))
g5 <- TNPE_tbl_all %>% pull(ly) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for log(y)")))
g6 <- TNPE_tbl_all %>% pull(dly1) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta, "log(y)")))
g7 <- TNPE_tbl_all %>% pull(dly12) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta[12], "log(y)")))
g8 <- TNPE_tbl_all %>% pull(d2ly12_1) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta,Delta[12], "log(y)")))
ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol = 4)


# ACF for log(y) shows tails off while PACF peaks at lag 1 
#and shows seansonality every 6 months, 
# Same for delta log(y), ACF and PACF  show seasonality for every six months
# While for delta 12 log (y) we observe tails off for both ACF and PACF and 
#seasonality every 12 months
# and same thing is for the twice differenciated  12 log (y)





#(e) Perform ADF and KPSS


# ADF test: if we reject, it is not unit root test, 
#which means that there is  stationarity. Thus, the data doesn't need 
#to be differenced. If we fail to reject H0 then the data is not stationary and thus,
# needs differenciation 

# KPSS Test : rejecting H0 means that the data is not stationary, 
#and it is not otherwise.

###################### ADF Test ################################

afd1<- TNPE_tbl_all %>%
  filter( !is.na(ly))%>% 
  ur.df(ly, type="drift", selectlags="AIC")
summary(afd1)

# Fail to reject H0:  not stationary


afds2 <- TNPE_tbl_all %>%
  filter( !is.na(dly12))%$% 
  ur.df(dly12, type="none", selectlags="AIC")
summary(afds2)


# Reject at 5% and 10% : stationary,  
# Fail to reject at 1% : not stationary


dfd3<-TNPE_tbl_all %>%
  filter( !is.na(d2ly12_1))%>% 
  ur.df(d2ly12_1, type="none", selectlags="AIC")
summary(dfd3)


# Reject: stationary



##################### KPSS Test ##############################


kpssdly<- TNPE_tbl_all %>%
  filter( !is.na(ly))%$% 
  ur.kpss(ly, type="mu", lags="short")
summary(kpssdly)


# Reject:Not stationary


kpssddly12<- TNPE_tbl_all %>%
  filter( !is.na(dly12))%$% 
  ur.kpss(dly12, type="mu", lags="short")
summary(kpssddly12)


# Reject: Not stationary.


kpssddd2ly12_1<- TNPE_tbl_all %>%
  filter( !is.na(d2ly12_1))%$% 
  ur.kpss(d2ly12_1, type="mu", lags="short")
summary(kpssddd2ly12_1)


# Fail to reject : Stationary



#(f) Split the sample and use ACF and PACF and check adequacy


train.start<-"1975-01-01"
train.start.n<-1975
train.end<-"2014-12-01"
train.end.n <- 2014.916
test.start<-"2015-01-01"
test.start.n<-2015
test.end<-"2018-12-01"


freq<-12


train1<-TNPE_tbl_all %>% filter( !is.na(d2ly12_1), date<=date(train.end)) %$%
  ts(d2ly12_1, start=train.start.n, frequency=freq)


test1<-TNPE_tbl_all %>% filter( !is.na(d2ly12_1), date>=date(test.start)) %$%
  ts(d2ly12_1, start=test.start.n, frequency=freq)


Acf(train1, lag.max=50, main=expression(paste("ACF of ", Delta, Delta[12], "d2ly12_1")))
Pacf(train1, lag.max=50, main=expression(paste("PACF of ", Delta, Delta[12], "d2ly12_1")))


# ACF : tails off and PACFis cut off after lag 3 . 
# AR(3), and sessonal will be aram because both of them were tailing off

m1<-Arima(train1, order = c(3,0,0), seasonal=c(1,0,1))
ggtsdiag(m1, gof.lag=50)
autoplot(m1)





#(g) Use auto.arima to find the best model

m1<-Arima(train1, order = c(3,0,0), seasonal=c(1,0,1))
ggtsdiag(m1, gof.lag=50)

autoplot(m1)





#(h) Use slide to create rolling scheme

TNPE_tbl_1 <- TNPE_tbl_all %>%
  filter(yearm <= yearmonth(train.start.n))


# Window length for rolling SARIMA
data.ts.1 <- TNPE_tbl_all %>%
  tk_ts(select = d2ly12_1, start = train.start.n, end = test.start.n , frequency = 12)
window.length <- length(data.ts.1)

# Estimate rolling SARIMA model, create 1 step ahead forecasts
results <-
  TNPE_tbl_all %>%
  as_tsibble(index = yearm) %>%    
  mutate(sarima.model = slide(d2ly12_1, ~Arima(.x, order = c(3,0,0), seasonal = list(order = c(1,0,1), period = 12)),.size = window.length)) %>%            
  filter(!is.na(sarima.model)) %>%                                                              
  mutate(sarima.coefs = map(sarima.model, tidy, conf.int = TRUE),
         sarima.fcst = map(sarima.model, (. %>% forecast(1) %>% sw_sweep())))

# (i) PLot the forecast
# Extract coefficients
coefs_tbl <- results %>%
  select(yearm, sarima.coefs) %>%
  as_tibble() %>%
  unnest(sarima.coefs) %>%
  as_tsibble(key = id("term"), index = yearm) 


# plot estimated coefficients with confidence intervals
coefs_tbl %>%
  ggplot(aes(x = yearm, y = estimate, group = term)) +
  geom_line(color = "red") +
  geom_ribbon(aes(x = yearm, ymin = conf.low, ymax = conf.high), alpha = 0.5, fill = "pink") +
  geom_hline(yintercept = 0, color = "black")+
  labs(x = "", y = "",
       title = "Coefficient Estimates",
       subtitle = paste(window.length, "Months rolling window model"))+
  facet_wrap(~term, nrow = 1) 





# (i) 

m1_f_1_rol <-
  results %>%
  select(yearm, sarima.fcst) %>%
  as_tibble() %>%
  unnest(sarima.fcst) %>%
  filter(key == "forecast") %>%
  mutate(yearm = yearm %m+% months(1) %>% yearmonth()) %>%
  mutate_at(vars(value, lo.80, lo.95, hi.80, hi.95), list(exp)) %>%
  rename(y = value) %>%
  select(yearm, key, y, lo.80, lo.95, hi.80, hi.95)


TNPE_tbl_f_1_rol <- 
  bind_rows(TNPE_tbl_all %>%
              mutate(key = "actual") %>%
              select(yearm, key, y),
            m1_f_1_rol) %>%
  mutate(yearm = yearmonth(yearm))

m1_f_1_rol %>%
  filter(yearm >= yearmonth(1975)) %>%
  ggplot(aes(x = yearm, y = y, col = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "pink", alpha = 0.1) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "pink", alpha = 0.2) +
  geom_line() +
  geom_line(data = TNPE_tbl_all %>% mutate(key = "actual") %>% filter(yearm > yearmonth(1975)), aes(x = yearm, y = y)) +
  geom_point() +
  scale_color_manual(values = c("gray40","darkblue")) +
  labs(x = "", y = "",
       title = "Total Nonfarm Payroll Employment: 1-Step Ahead Rolling Forecast") +
  theme(legend.position = "none")


 
#(j)  Forecast errors and plot 
fst3 <- 1975 
lst3 <- 2008-(1/12)
data.ts.1<- data.tbl%>%
  filter(index <= as.yearmon(train.start)) %>%
  tk_ts(select = dy, end=lst3, frequency = 12)
m3<- data.ts.1 %>%
  tk_ts(select = dy, end=lstm3, frequency = 12) %>%
  auto.arima(ic = "aic", d = 1, D = 1, seasonal = TRUE,stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m3
ggtsdiag(m3)
# construct 1-month ahead forecasts
hmax <- 50
f_0<- forecast(m3, hmax) 
f_0 
# plot 1month ahead forecasts-level
autoplot(f_0) +
  labs(x = "", y = "",
       title = "The first difference of Total Nonfarm Payroll Employment:multistep Forecast")
# actual data  
act_tbl_2 <-
  TNPE_tbl_all %>%
  select(index, dy) %>%
  mutate(key = "actual",
         date = as.Date(index)) %>%
  select(date, key, dy) %>% filter(year(date) >= 2008)
act_tbl_2
# extract the multistep forecasts, convert to levels
TNPE_tbl_f_2 <-
  f_0 %>%
  sw_sweep() %>%   
  filter(key == "forecast") %>%
  mutate_at(vars(dy, lo.80, lo.95, hi.80, hi.95), funs()) %>%   
  mutate(date = as.Date(index)) %>%    
  select(date, key, dy, lo.80, lo.95, hi.80, hi.95)
# forecast & actual data in a single tibble    ##combine the forecasts and actual data, then we can plot them together
fora_tbl_2 <- bind_rows(act_tbl_2,  TNPE_tbl_f_2,.id = NULL)
fora_tbl_2 
# plot 1-month ahead rolling forecasts - levels
ppf<-fora_tbl_2 %>%
  filter(date >= "1975-01-01") %>%
  ggplot(aes(x = date, y = dy, col = key, linetype = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "green", alpha = 0.1) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "green", alpha = 0.2) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("black","green")) +
  scale_linetype_manual(values = c("solid","solid")) +
  labs(x = "", y = "",
       title = "Total Nonfarm Payroll Employment: 1-Step Ahead Rolling Forecast") +
  theme(legend.position = "none")   
ppf
#My code here, doesn't reconize dy when I try to extract coefficients, I don't know what is wrong


#(k) 
accuracy(ms_tbl_f_1$E, actual_tbl$E)
#the forecast errors for $\Delta E_t$
accuracy(ms_tbl_f_2$dE, actual_tbl_2$dE)

##It should get the result if previous code works




