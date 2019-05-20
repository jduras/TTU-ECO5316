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

# set default theme for ggplot2
theme_set(theme_minimal())

# obtain data on real GDP, construct its log change
data.tbl <-
  tq_get("PCECC96",
         get = "economic.data",
         from = "1955-01-01",
         to = "2018-12-31") %>%
  rename(y = price) %>%
  mutate(dly = 4*(log(y) - lag(log(y))))

# split sample - estimation subsample dates
fstQ <- 1955.00  # 1947Q1
lstQ <- 2008.75  # 2008Q4

# convert data into ts, which is the format that Acf, auto.arima and forecast expect
data.ts <- data.tbl %>%
  tk_ts(select = dly, start = fstQ, frequency = 4)

# split sample - estimation and prediction subsamples
data.ts.1 <- data.tbl %>%
  tk_ts(select = dly, start = fstQ, end = lstQ, frequency = 4)
data.ts.2 <- data.ts %>%
  window(start = lstQ + 0.25)

# m1 <- Arima(data.ts, order = c(1,0,0))
tic()
m1 <- auto.arima(data.ts.1, ic = "aic", stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m1
toc()
ggtsdiag(m1)
autoplot(m1)

# create h = 1,2,...,36 step ahead forecasts - forecast origin which is fixed at 2008Q4
m1.f.1.to.36 <- forecast(m1, 36)
m1.f.1.to.36

# create the forecast plot, make it bit nicer
autoplot(m1.f.1.to.36) +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(-0.1, 0.15, 0.05)) +
  labs(x = "", y = "" , title = "Real Personal Consumption Expenditures",
       subtitle = "Multiperiod Point Forecast with 80% and 95% Confidence Intervals") +
  theme(legend.position = "none")

# estimate rolling ARMA with window size = window.length, create 1 period ahead rolling forecasts using slide from tsibble package
window.length <- length(data.ts.1)

tic()
results <-
  data.tbl %>%
  mutate(yearq = yearquarter(date)) %>%
  as_tsibble(index = yearq) %>%                                                               # covert to tsibble
  mutate(ar1.model  = slide(dly, ~Arima(.x, order = c(1,0,0)), .size = window.length),        # estimate models
         arma.model = slide(dly, ~auto.arima(.x, ic = "bic", stationary = TRUE), 
                            .size = window.length)) %>%  
  filter(!is.na(arma.model)) %>%                                                              # remove periods at the beginning of sample where model could not be estimated due to lack of data,
  mutate(ar1.coefs = map(ar1.model, tidy, conf.int = TRUE),                                   # extract coefficients
         arma.coefs = map(arma.model, tidy, conf.int = TRUE),                                 
         ar1.f = map(ar1.model, (. %>% forecast(h = 1) %>% sw_sweep())),                      # extract forecast
         arma.f = map(arma.model, (. %>% forecast(h = 1) %>% sw_sweep())))
results
toc()

# extract the 1 period ahead rolling forecasts
m1.f.1.rol <-
  results %>%
  as_tibble() %>%
  select(yearq, ends_with(".f")) %>%
  gather(model, coefs, ends_with(".f")) %>%
  mutate(model = str_replace(model, ".f", "")) %>%
  unnest() %>% 
  filter(key == "forecast") %>%
  mutate(yearq = yearq %m+% months(3) %>% yearquarter())

# plot the 1 period ahead rolling forecasts
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
  labs(x = "", y = "", title = "Real GDP Growth Rate, Quarter over Quarter, Annualized",
       subtitle = "Rolling Forecast with 80% and 95% Confidence Intervals") +
  facet_grid(model ~ .) +
  theme(legend.position = "none")

# summary
accuracy(m1.f.1)
accuracy(m1.f.1.rol %>% filter(model == "ar1") %>% pull(value), data.ts.2)      # 1 step ahead rolling scheme forecast AR(1) model
accuracy(m1.f.1.rol %>% filter(model == "arma") %>% pull(value), data.ts.2)     # 1 step ahead rolling scheme forecast auto.arima
     