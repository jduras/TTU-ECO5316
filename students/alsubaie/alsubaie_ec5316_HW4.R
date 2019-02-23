library(magrittr)
library(tidyverse)
library(tidyquant)
library(timetk)
library(ggfortify)
library(forecast)
library(tidyverse)
library(timetk)
library(egg)

# get time series for quarterly 
data <- 
  tq_get("PCECC96", get = "economic.data", from = "1955-01-01", to = "2015-12-31")

data_tbl <-
  data %>%
  rename(y = price) %>%
  mutate(dly = 4*(log(y) - lag(log(y))))

# plot time series  - using ggplot
data_tbl %>%
  gather(measure, value, c(y, dly)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "Real Personal Consumption Expenditures") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlrpfi = "Log Change",
                                             rpfi = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))


dly <- data_tbl %>%
  filter(!is.na(dly)) %>%
  tk_xts(date_var = date, select = dly) 


nlags <- 24
g1 <- ggAcf(dly, lag.max = nlags)
g2 <- ggPacf(dly, lag.max = nlags)
ggarrange(g1, g2, ncol = 1)

ARIMA3 <- arima(dly, order=c(3,0,1)) 
ARIMA3
ggtsdiag(m1, gof.lag = nlags)

ARIMA3 <- auto.arima(dlrpfi, ic = "bic", stationary = TRUE, seasonal = FALSE, approximation = FALSE, stepwise = FALSE)
ARIMA3
ggtsdiag(ARIMA3, gof.lag = nlags)


# split sample - estimation subsample dates
fstQ <- 1955.00  
lstQ <- 2009.00  

# convert data into ts, which is the format that Acf, auto.arima and forecast expect
data.ts <- data_tbl %>%
  tk_ts(select = dly, start = fstQ, frequency = 4)



# split sample - estimation and prediction subsamples
data.ts.1 <- data_tbl %>%
  tk_ts(select = dly, start = fstQ, end = lstQ, frequency = 4)
data.ts.2 <- data.ts %>%
  window(start = lstQ + 0.25)

# plot ACF and PACF
g1 <- ggAcf(data.ts.1, lag.max = 24)
g2 <- ggPacf(data.ts.1, lag.max = 24)
ggarrange(g1, g2, nrow = 2)


# m1 <- Arima(data.ts, order = c(3,0,1))
tic()
m1 <- auto.arima(data.ts.1, ic = "bic", stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m1

m1 <- auto.arima(data.ts.1, ic = "aic", stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m1

m1 <- auto.arima(data.ts.1, ic = "aicc", stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m1
toc()
ggtsdiag(m1)
autoplot(m1)

m1 <- Arima(data.ts.1, order = c(3, 0, 0))

m1
ggtsdiag(m1)

# evaluate in-sample accuracy based on model residuals
accuracy(m1)


# largest forecast step hmax = T-t, given how the sample was split
hmax <- length(data.ts.2)

# create h = 1,2,...,hmax step ahead forecasts - 
m1.f.1.to.hmax <- forecast(m1, hmax)
m1.f.1.to.hmax

# evaluate accuracy for
# - in sample residuals (training set)
# - out-of-sample 1,2,...,hmax step ahead forecasts (test set)
# note that in-sample measures are exactly same as those obtained above on line 35 using accuracy(m1)
accuracy(m1.f.1.to.hmax, data.ts.2)

# evaluate only accuracy of out-of-sample h=1,2,...,
accuracy(m1.f.1.to.hmax$mean, data.ts.2)

# here the training set is dropped 

autoplot(m1.f.1.to.hmax)

autoplot(m1.f.1.to.hmax) +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(-0.1, 0.15, 0.05)) +
  labs(x = "", y = "" , title = "Real GDP Growth Rate, Quarter over Quarter, Annualized",
       subtitle = "Multiperiod Point Forecast with 80% and 95% Confidence Intervals") +
  theme(legend.position = "none")




# tidy from broom package extracts the coefficients from the model into a tibble
m1 %>%
  tidy()
# sw_sweep from sweep package transforms the forecast object into a tibble object in a long format
m1 %>%
  forecast(h = 1) %>%
  sw_sweep() %>%
  tail()


# estimate rolling ARMA with window size = window.length, create 1 period ahead rolling forecasts using slide from tsibble package
window.length <- length(data.ts.1)

tic()
results <-
  data_tbl %>%
  mutate(yearq = yearquarter(date)) %>%
  as_tsibble(index = yearq) %>%                                                               # covert to tsibble
  mutate(ar1.model = slide(dly, ~Arima(.x, order = c(1,0,0)), .size = window.length)) %>%     # estimate models
  filter(!is.na(ar1.model)) %>%                                                               # remove periods at the beginning of sample where model could not be estimated due to lack of data,
  mutate(ar1.coefs = map(ar1.model, tidy, conf.int = TRUE),                                   # extract coefficients
         ar1.f = map(ar1.model, (. %>% forecast(h = 1) %>% sw_sweep())))                      # extract forecast
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

# extract the 1 period ahead rolling forecasts
m1.f.1.rol <-
  results %>%
  as_tibble() %>%
  select(yearq, ar1.f) %>%
  unnest(ar1.f) %>%
  filter(key == "forecast") %>%
  mutate(yearq = yearq %m+% months(3) %>% yearquarter())

# plot the 1 period ahead rolling forecasts
m1.f.1.rol %>%
  ggplot(aes(x = yearq, y = value)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = "royalblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = "royalblue", alpha = 0.3) +
  geom_line(size = 0.7, col = "blue") +
  geom_line(data = (data_tbl %>% filter(year(date) > 1999)) %>% mutate(yearq = yearquarter(date)), 
            aes(x = yearq, y = dly)) +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(-0.05, 0.10, 0.05)) +
  scale_color_manual(values = c("black", "darkblue")) +
  labs(x = "", y = "", title = "Real GDP Growth Rate, Quarter over Quarter, Annualized",
       subtitle = "Rolling Forecast with 80% and 95% Confidence Intervals") +
  theme(legend.position = "none")


accuracy(m1.f.1.rol$value, data.ts.2)  