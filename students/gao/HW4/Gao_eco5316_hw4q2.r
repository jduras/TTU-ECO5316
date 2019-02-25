---
title: "5316 homework 4 Problem 2"
author: "Shijia Gao"

#(a) Use `tq_get` to obtain quarterly Real Personal Consumption Expenditures for the 1955Q1-2018Q4 sample, available on FRED under code [`PCECC96`](https://fred.stlouisfed.org/series/PCECC96).
library(tidyquant)
library(timetk)
# library(tibbletime)
library(broom)
library(sweep)
library(ggplot2)
library(scales)
library(ggfortify)
library(egg)
library(tsibble)
library(tictoc)
library(forecast)

data.tbl <-
  tq_get("PCECC96", get = "economic.data", from = "1955-01-01", to = "2018-07-01") %>%    
  rename(y = price) %>%


#(b) Construct the log changes in the Real Personal Consumption Expenditures $\Delta \log c_t = \log c_t - \log c_{t-1}$ where $c_t$ is the original quarterly Real Personal Consumption Expenditures. 

mutate(dly = 4*(log(y) - lag(log(y))))   #approximately get the annualized GDP 
tail(data.tbl) 

#(c) As in the example discussed in class and the lecture slides, split the sample into two parts: first one up to 2008Q4, second one from 2009Q1 onward.
#Use `auto.arima` with `ic = aic` and `stationary = TRUE`, `stepwise = FALSE`, `approximation = FALSE` to find the best model. Check the estimated model
#for adequacy, diagnose residuals using `ggtsdiag`. 

# split sample - estimation and prediction subsamples
fstQ <- 1955.00  
lstQ <- 2008.75 
data.ts <- data.tbl %>%
  tk_ts(select = dly, start = fstQ, frequency = 4)
data.ts.1 <- data.tbl %>%
tk_ts(select = dly, start = fstQ, end = lstQ, frequency = 4) 
data.ts.1  
data.ts.2 <- data.ts %>%    
  window(start = lstQ + 0.25)

# plot ACF and PACF
g1 <- ggAcf(data.ts.1, lag.max = 24)
g2 <- ggPacf(data.ts.1, lag.max = 24)
ggarrange(g1, g2, nrow = 2)      

m1 <- auto.arima(data.ts.1, ic = "aic", stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m1
ggtsdiag(m1) 

# evaluate in-sample accuracy based on model residuals
accuracy(m1)

# from the auto.arima, the best model is ARIMA(0,0,3)(2,0,0)[4]. From the results of p values for Ljung-Box statistic, 
# we can get that we fail to reject the null hypothesis that the residuals are uncorrelated with each other, so the 
# time series is white noise, which is the ideal case. From the in-sample accuracy evaluation, ME value approaches to 0, 
# and MASE is smaller than 1, so the model ARIMA(0,0,3)(2,0,0)[4] is ideal. 


#(d) Use the estimated model with `forecast` to generate 1 to 36 step ahead forecast for the prediction subsample, 2009Q1-2018Q4. 
hmax <- length(data.ts.2)-3 
hmax
m1.f.1.to.hmax <- forecast(m1, hmax)
m1.f.1.to.hmax 
autoplot(m1.f.1.to.hmax) 
#from the forecase, we can get the conditional mean jumps straight to historical mean once h>3.so the model is
#particularly useful for the long-term forecast. 


#(e) Use `slide` from the `tsibble` package to generate a rolling scheme forecast, in particular a sequence of 1 period ahead forecasts for the prediction subsample, 2009Q1-2018Q4. 

window.length <- length(data.ts.2)

tic()
results <-
  data.tbl %>%
  mutate(yearq = yearquarter(date)) %>%
  as_tsibble(index = yearq) %>%                                                               # covert to tsibble
  mutate(ar1.model = slide(dly, ~Arima(.x, order = c(2,0,0)), .size = window.length)) %>%     # estimate models      #search "slide" in Help
  filter(!is.na(ar1.model)) %>%                                                               # remove periods at the beginning of sample where model could not be estimated due to lack of data,
  mutate(ar1.coefs = map(ar1.model, tidy, conf.int = TRUE),                                   # extract coefficients
         ar1.f = map(ar1.model, (. %>% forecast(h = 1) %>% sw_sweep())))                      # extract forecast
results
toc()


#(f) Plot the multistep forecast and the 1 step ahead rolling forecasts, with their confidence intervals.

# plot the 1 period ahead rolling forecasts
m1.f.1.rol <-
  results %>%
  as_tibble() %>%
  select(yearq, ar1.f) %>%   
  unnest(ar1.f) %>%
  filter(key == "forecast") %>%
  mutate(yearq = yearq %m+% months(3) %>% yearquarter())
m1.f.1.rol

m1.f.1.rol %>%
  ggplot(aes(x = yearq, y = value)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = "royalblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = "royalblue", alpha = 0.3) +
  geom_line(size = 0.7, col = "blue") +
  geom_line(data = (data.tbl %>% filter(year(date) > 1999)) %>% mutate(yearq = yearquarter(date)), 
            aes(x = yearq, y = dly)) +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(-0.05, 0.10, 0.05)) +
  scale_color_manual(values = c("black", "darkblue")) +
  labs(x = "", y = "", title = "Real Personal Consumption Expenditures Growth Rate, Quarter over Quarter, Annualized",
       subtitle = "Rolling Forecast with 80% and 95% Confidence Intervals") +
  theme(legend.position = "none")


# plot the multi-period ahead rolling forecasts
tic()
results2 <-
  data.tbl %>%
  mutate(yearq = yearquarter(date)) %>%
  as_tsibble(index = yearq) %>%                                                               # covert to tsibble
  mutate(ar1.model = slide(dly, ~Arima(.x, order = c(2,0,0)), .size = window.length)) %>%     # estimate models      #search "slide" in Help
  filter(!is.na(ar1.model)) %>%                                                               # remove periods at the beginning of sample where model could not be estimated due to lack of data,
  mutate(ar1.coefs = map(ar1.model, tidy, conf.int = TRUE),                                   # extract coefficients
         ar1.f = map(ar1.model, (. %>% forecast(h =36) %>% sw_sweep())))                      # extract forecast
results2
toc()

m1.f.2.rol <-
  results2 %>%
  as_tibble() %>%
  select(yearq, ar1.f) %>%   
  unnest(ar1.f) %>%
  filter(key == "forecast") %>%
  mutate(yearq = yearq %m+% months(3) %>% yearquarter())
m1.f.2.rol

m1.f.2.rol %>%
  ggplot(aes(x = yearq, y = value)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = "royalblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = "royalblue", alpha = 0.3) +
  geom_line(size = 0.7, col = "blue") +
  geom_line(data = (data.tbl %>% filter(year(date) > 1999)) %>% mutate(yearq = yearquarter(date)), 
            aes(x = yearq, y = dly)) +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(-0.05, 0.10, 0.05)) +
  scale_color_manual(values = c("black", "darkblue")) +
  labs(x = "", y = "", title = "Real Personal Consumption Expenditures Growth Rate, Quarter over Quarter, Annualized",
       subtitle = "Rolling Forecast with 80% and 95% Confidence Intervals") +
  theme(legend.position = "none")


#(g) Use `accuracy` to evaluate the out of sample accuracy of the two sets of forecasts.
accuracy(m1.f.1.to.hmax$mean, data.ts.2)    # multistep forecast    
accuracy(m1.f.1.rol$value, data.ts.2)       # 1 step ahead rolling scheme forecast
# since multistep forecast model's MAE value is lower than the 1 step ahead rolling scheme forecast, so 
#multistep forecast is better. 

