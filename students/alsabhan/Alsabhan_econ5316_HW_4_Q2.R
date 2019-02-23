library(tidyquant)
library(timetk)
library(tsibble)
library(broom)
library(sweep)
library(ggplot2)
library(scales)
library(ggfortify)
library(egg)
library(tictoc)
library(forecast)

#(B) Construct the log changes in the Real Personal Consumption Expenditures ???logct =logct ???logct???1 where ct is the 
#original quarterly Real Personal Consumption Expenditures
theme_set(theme_minimal())
Ct.tbl <-
  tq_get("PCECC96", get = "economic.data", from = "1955-01-01", to = "2018-10-01") %>%
  rename( Ct = price) %>%
  mutate(dlCt = 4*(log(Ct) - lag(log(Ct))))
# (C)As in the example discussed in class and the lecture slides, split the sample into two parts: ???rst one up to 2008Q4,
#second one from 2009Q1 onward. Use auto.arima with ic = aic and stationary = TRUE, stepwise = FALSE,
#approximation = FALSE to ???nd the best model. Check the estimated model for adequacy, diagnose residuals using ggtsdiag.
fstQ <- 1955.00
lstQ <- 2008.75
Ct.ts <- Ct.tbl %>%
  tk_ts(select = dlCt, start = fstQ, frequency = 4)
Ct.ts.1 <- Ct.tbl %>%
  tk_ts(select = dlCt, start = fstQ, end = lstQ, frequency = 4)
Ct.ts.2 <- Ct.ts %>%
  window(start = lstQ + 0.25)
m1 <- auto.arima(Ct.ts.1, ic = "aic", stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m1
ggtsdiag(m1) 

# plot ACF and PACF
g1 <- ggAcf(Ct.ts.1, lag.max = 24)
g2 <- ggPacf(Ct.ts.1, lag.max = 24)
ggarrange(g1, g2, nrow = 2)



#we cannot trust autoarima, thereforfer we need to use amima to be more sure.

m0 <- Arima(Ct.ts.1, order = c(1, 0, 0))
m0
ggtsdiag(m1)

m2 <- Arima(Ct.ts.1, order = c(2, 0, 0))
m2
ggtsdiag(m2)

m3 <- Arima(Ct.ts.1, order = c(3, 0, 0))
m3
ggtsdiag(m2)

arma23 <- Arima(Ct.ts.1, order = c(2, 0, 3))
arma23
ggtsdiag(arma23)


arma31 <- Arima(Ct.ts.1, order = c(3, 0, 1))
arma31
ggtsdiag(arma31)

arma33 <- Arima(Ct.ts.1, order = c(3, 0, 3))
arma33
ggtsdiag(arma33)

# autoarima selects the most significant model with consistent p values for Ljung-Box 
#m1 <- Arima(Ct.ts.1, order = c(3, 0, 2))
m1
ggtsdiag(m1) 
autoplot(m1)

accuracy(m1)
accuracy(arma31)
accuracy(arma23)
# the RMSE in m1 is the smaller than the other which indication of the better model 

#(d) Use the estimated model with forecast to generate 1 to 36 step ahead forecast for the prediction subsample,
# 2009Q1-2018Q4.
h36 <- length(Ct.ts.2)

#(e) Use slide from the tsibble package to generate a rolling scheme forecast, in particular a sequence of 1 period
# ahead forecasts for the prediction subsample, 2009Q1-2018Q4
# create 1-step ahead forecasts 

m1.f.1 <- Arima(y = Ct.ts, model = m1)

m1.f.1.to.h36 <- forecast(m1, h36)
m1.f.1.to.h36



accuracy(fitted(m1.f.1), Ct.ts)
accuracy(m1)
# m1.f.1 is slight accuracy than m1 accuracy

m1.f.1.oos <- window(fitted(m1.f.1), start = lstQ+0.25)
# evaluate accuracy of out-of-sample 1-step ahead forecasts

accuracy(fitted(m1.f.1), Ct.ts.2)
# which is more accurate 

m1 %>%
  tidy()
m1 %>%
  forecast(h = 1) %>%
  sw_sweep() %>%
  tail()

window.length <- length(Ct.ts.1)

tic()
results <-
  Ct.tbl %>%
  mutate(yearq = yearquarter(date)) %>%
  as_tsibble(index = yearq) %>%                                                             
  mutate(ar1.model = slide(dlCt, ~Arima(.x, order = c(3,0,2)), .size = window.length)) %>%    
  filter(!is.na(ar1.model)) %>%                                                           
  mutate(ar1.coefs = map(ar1.model, tidy, conf.int = TRUE),                                   
         ar1.f = map(ar1.model, (. %>% forecast(h = 1) %>% sw_sweep())))                     
results
toc()

# plot estimated coefficients with confidence intervals
results %>%
  as_tibble() %>%
  select(yearq, ar1.coefs) %>%
  unnest(ar1.coefs) %>%
  ggplot(aes(x = yearq, y = estimate, group = term)) +
  geom_line(color = "royalblue") +
  geom_ribbon(aes(x = yearq, ymin = conf.low, ymax = conf.high), alpha = 0.5, fill = "lightblue") +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "", y = "",
       title = "Coefficient estimates",
       subtitle = paste(window.length, "quarter rolling window model"))+
  theme_minimal() +
  facet_wrap(~term, scales = "free_y")

#(f) Plot the multistep forecast and the 1 step ahead rolling forecasts, with their con???dence intervals.
# extract the 1 period ahead rolling forecasts
m1.f.1.rol <-
  results %>%
  as_tibble() %>%
  select(yearq, ar1.f) %>%
  unnest(ar1.f) %>%
  filter(key == "forecast") %>%
  mutate(yearq = yearq %m+% months(3) %>% yearquarter())

# multistep forecast
autoplot(m1.f.1.to.h36) +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(-0.1, 0.15, 0.05)) +
  labs(x = "", y = "" , title = "Real Personal Consumption Expenditures, Quarter over Quarter, Annualized",
       subtitle = "Multiperiod Point Forecast with 80% and 95% Confidence Intervals") +
  theme(legend.position = "none")

# 1 step ahead rolling scheme forecast
m1.f.1.rol %>%
  ggplot(aes(x = yearq, y = value)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = "royalblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = "royalblue", alpha = 0.3) +
  geom_line(size = 0.7, col = "blue") +
  geom_line(data = (Ct.tbl %>% filter(year(date) > 2000)) %>% mutate(yearq = yearquarter(date)), 
            aes(x = yearq, y = dlCt)) +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(-0.05, 0.10, 0.05)) +
  scale_color_manual(values = c("black", "darkblue")) +
  labs(x = "", y = "", title = "Real Personal Consumption Expenditures, Quarter over Quarter, Annualized",
       subtitle = "Rolling Forecast with 80% and 95% Confidence Intervals") +
  theme(legend.position = "none")


#(g) Use accuracy to evaluate the out of sample accuracy of the two sets of forecasts.
accuracy(m1.f.1.to.h36$mean, Ct.ts.2)    # multistep forecast
accuracy(m1.f.1.rol$value, Ct.ts.2)       # 1 step ahead rolling scheme forecast