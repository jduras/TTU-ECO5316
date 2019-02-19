
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
    tq_get("GDPC1",
           get = "economic.data",
           from = "1947-01-01",
           to = "2018-12-31") %>%
    rename(y = price) %>%
    mutate(dly = 4*(log(y) - lag(log(y))))

# split sample - estimation subsample dates
fstQ <- 1947.00  # 1947Q1
lstQ <- 2008.75  # 2008Q4

# convert data into ts, which is the format that Acf, auto.arima and forecast expect
data.ts <- data.tbl %>%
    tk_ts(select = dly, start = fstQ, frequency = 4)

# split sample - estimation and prediction subsamples
data.ts.1 <- data.tbl %>%
    tk_ts(select = dly, start = fstQ, end = lstQ, frequency = 4)
data.ts.2 <- data.ts %>%
    window(start = lstQ + 0.25)

# plot ACF and PACF
g1 <- ggAcf(data.ts.1, lag.max = 24)
g2 <- ggPacf(data.ts.1, lag.max = 24)
ggarrange(g1, g2, nrow = 2)


# m1 <- Arima(data.ts, order = c(1,0,0))
m1 <- auto.arima(data.ts.1, ic = "bic", stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m1
ggtsdiag(m1)
autoplot(m1)


# evaluate in-sample accuracy based on model residuals
accuracy(m1)


# largest forecast step hmax = T-t, given how the sample was split
hmax <- length(data.ts.2)
# create h = 1,2,...,hmax step ahead forecasts - forecast origin which is fixed at 2008Q4
m1.f.1.to.hmax <- forecast(m1, hmax)
m1.f.1.to.hmax

# evaluate accuracy for
# - in sample residuals (training set)
# - out-of-sample 1,2,...,hmax step ahead forecasts (test set)
# note that in-sample measures are exactly same as those obtained above on line 35 using accuracy(m1)
accuracy(m1.f.1.to.hmax, data.ts.2)
# evaluate only accuracy of out-of-sample h=1,2,...,hmax step ahead forecasts for 2009Q1-2016Q4 (test set)
accuracy(m1.f.1.to.hmax$mean, data.ts.2)

# create the forecast plot
# note that for AR(p) model l-step ahead forecast converges to unconditional mean as h -> infinity
autoplot(m1.f.1.to.hmax)

# create the forecast plot, make it bit nicer
autoplot(m1.f.1.to.hmax) +
    geom_hline(yintercept = 0, color = "gray50") +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       breaks = seq(-0.1, 0.15, 0.05)) +
    labs(x = "", y = "" , title = "Real GDP Growth Rate, Quarter over Quarter, Annualized",
         subtitle = "Multiperiod Point Forecast with 80% and 95% Confidence Intervals") +
    theme(legend.position = "none")

# create the plot manually
m1.f.1.to.hmax %>%
    # sw_sweep(timetk_idx = TRUE) %>%
    sw_sweep() %>%
    ggplot(aes(x = index, y = dly, col = key)) +
        geom_ribbon(aes(ymin = lo.95, ymax = hi.95), color = NA, fill = "royalblue", alpha = 0.2) +
        geom_ribbon(aes(ymin = lo.80, ymax = hi.80), color = NA, fill = "royalblue", alpha = 0.3) +
        geom_line(size = 0.7) +
        geom_hline(yintercept = 0, color = "gray50") +
        scale_x_yearmon() +
        scale_y_continuous(labels = percent_format(accuracy = 1),
                           breaks = seq(-0.05, 0.10, 0.05)) +
        scale_color_manual(values = c("black","darkblue")) +
        labs(x = "", y = "", title = "Real GDP Growth Rate, Quarter over Quarter, Annualized",
             subtitle = "Multiperiod Forecast with 80% and 95% Confidence Intervals") +
        theme(legend.position = "none")

# create 1-step ahead forecasts - forecast origin is moving from 2008Q4 to 2017Q3
# but always use same estimated model m1 so this is a fixed forecasting scheme
m1.f.1 <- Arima(y = data.ts, model = m1)
# evaluate accuracy of 1-step ahead forecast throughout the whole sample 1947Q2 to 2017Q4
accuracy(m1.f.1)
# which is the same as obtaining fitted values and running
accuracy(fitted(m1.f.1), data.ts)
# recover only the out of sample 1 step ahead forecasts
m1.f.1.oos <- window(fitted(m1.f.1), start = lstQ+0.25)
# evaluate accuracy of out-of-sample 1-step ahead forecasts
accuracy(m1.f.1.oos, data.ts.2)
# which is the same as obtaining fitted values and running
accuracy(fitted(m1.f.1), data.ts.2)


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
    data.tbl %>%
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
             subtitle = paste(window.length, "quarters rolling window model"))+
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
        geom_line(data = (data.tbl %>% filter(year(date) > 1999)) %>% mutate(yearq = yearquarter(date)), 
                  aes(x = yearq, y = dly)) +
        geom_hline(yintercept = 0, color = "gray50") +
        scale_y_continuous(labels = percent_format(accuracy = 1),
                           breaks = seq(-0.05, 0.10, 0.05)) +
        scale_color_manual(values = c("black", "darkblue")) +
        labs(x = "", y = "", title = "Real GDP Growth Rate, Quarter over Quarter, Annualized",
             subtitle = "Rolling Forecast with 80% and 95% Confidence Intervals") +
        theme(legend.position = "none")

# summary
accuracy(m1.f.1.to.hmax$mean, data.ts.2)    # multistep forecast
accuracy(fitted(m1.f.1), data.ts.2)         # 1 step ahead fixed scheme forecast
accuracy(m1.f.1.rol$value, data.ts.2)       # 1 step ahead rolling scheme forecast
