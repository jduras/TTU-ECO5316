
library(magrittr)
library(tidyverse)
library(tidyquant)
library(timetk)
library(ggfortify)
library(forecast)

# set default ggplot theme to theme_bw()
theme_set(theme_bw())

# get time series for quarterly total wages and salaries in Texas
wTX_raw <- 
    tq_get("TXWTOT", get = "economic.data", from = "1980-01-01", to = "2017-12-31")

str(wTX_raw)
glimpse(wTX_raw)

wTX_tbl <-
    wTX_raw %>%
    rename(wTX = price) %>%
    mutate(dlwTX = log(wTX) - lag(log(wTX)))

# plot time series for total wages and salaries in Texas - using autoplot
wTX_tbl %>%
    tk_xts(date_var = date, select = c("wTX", "dlwTX")) %>%
    autoplot() + 
        labs(x = "", y = "", 
             title = "Wages and Salaries in Texas, Seasonally Adjusted Annual Rate") +
        facet_wrap(~plot_group, ncol = 1, scales = "free", 
                   labeller = labeller(plot_group = c(dlwTX = "Log Change", 
                                                      wTX = "Thousands of Dollars"))) +
        theme(strip.text = element_text(hjust = 0))

# plot time series for total wages and salaries in Texas - using ggplot
wTX_tbl %>%
    gather(measure, value, c(wTX, dlwTX)) %>%
    ggplot(aes(x = date, y = value)) +
        geom_line() +
        # geom_hline(yintercept = 0, color = "gray50") +
        labs(x = "", y = "", 
             title = "Total Wages and Salaries in Texas, Seasonally Adjusted Annual Rate") +
        facet_wrap(~measure, ncol = 1, scales = "free",
                   labeller = labeller(measure = c(dlwTX = "Log Change",
                                                   wTX = "Thousands of Dollars"))) +
        theme(strip.text = element_text(hjust = 0))

dlwTX <- wTX_tbl %>%
    filter(!is.na(dlwTX)) %>%
    tk_xts(date_var = date, select = dlwTX) 

str(dlwTX)

# number of lags for ACF and PACF plots
nlags <- 24

ggAcf(dlwTX, lag.max = nlags)
ggPacf(dlwTX, lag.max = nlags)


# estimate ARMA models
m1 <- Arima(dlwTX, order = c(0, 0, 2))
m1
ggtsdiag(m1, gof.lag = nlags)

m2 <- Arima(dlwTX, order = c(2, 0, 0))
m2
ggtsdiag(m2, gof.lag = nlags)


# z-statistics for coefficients of AR(2) model - phi1 is not signifficant at any level
z <- m2$coef/sqrt(diag(m2$var.coef))
# p values
2*(1-pnorm(abs(z))) %>% round(5)

# estimate ARMA model with a restriction on a parameter
m2_rest <- Arima(dlwTX, order = c(2, 0, 0), fixed = c(0, NA, NA))
m2_rest
ggtsdiag(m2_rest, gof.lag = nlags)

# find the best ARIMA model based on either AIC, AICc or BIC
m3 <- auto.arima(dlwTX, ic = "aicc", seasonal = FALSE, stationary = TRUE)
m3
ggtsdiag(m3, gof.lag = nlags)

m4 <- auto.arima(dlwTX, ic = "aicc", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m4
ggtsdiag(m4, gof.lag = nlags)

# check stationarity and invertibility of the estimated model - plot inverse AR and MA roots
plot(m4)
autoplot(m4)
