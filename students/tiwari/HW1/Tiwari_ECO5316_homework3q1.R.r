
library(magrittr)
library(tidyverse)
library(tidyquant)
library(timetk)
library(ggfortify)
library(forecast)

# set default ggplot theme to theme_bw()
theme_set(theme_bw())

# get time series for quarterly total wages and salaries in Texas
RPF_raw <- 
    tq_get("B007RA3Q086SBEA", get = "economic.data", from = "1980-01-01", to = "2017-12-31")

str(RPF_raw)
glimpse(RPF_raw)

RPF_tbl <-
    RPF_raw %>%
    rename(RPF = price) %>%
    mutate(dlRPF = log(RPF) - lag(log(RPF)))

# plot time series for total wages and salaries in Texas - using autoplot
RPF_tbl %>%
    tk_xts(date_var = date, select = c("RPF", "dlRPF")) %>%
    autoplot() + 
        labs(x = "", y = "", 
             title = "Wages and Salaries in Texas, Seasonally Adjusted Annual Rate") +
        facet_wrap(~plot_group, ncol = 1, scales = "free", 
                   labeller = labeller(plot_group = c(dlRPF = "Log Change", 
                                                      RPF = "Thousands of Dollars"))) +
        theme(strip.text = element_text(hjust = 0))

# plot time series for total wages and salaries in Texas - using ggplot
RPF_tbl %>%
    gather(measure, value, c(RPF, dlRPF)) %>%
    ggplot(aes(x = date, y = value)) +
        geom_line() +
        # geom_hline(yintercept = 0, color = "gray50") +
        labs(x = "", y = "", 
             title = "Total Wages and Salaries in Texas, Seasonally Adjusted Annual Rate") +
        facet_wrap(~measure, ncol = 1, scales = "free",
                   labeller = labeller(measure = c(dlRPF = "Log Change",
                                                   RPF = "Thousands of Dollars"))) +
        theme(strip.text = element_text(hjust = 0))

dlRPF <- RPF_tbl %>%
    filter(!is.na(dlRPF)) %>%
    tk_xts(date_var = date, select = dlRPF) 

str(dlRPF)

# number of lags for ACF and PACF plots
nlags <- 24

ggAcf(dlRPF, lag.max = nlags)
ggPacf(dlRPF, lag.max = nlags)


# estimate ARMA models
m1 <- Arima(dlRPF, order = c(0, 0, 2))
m1
ggtsdiag(m1, gof.lag = nlags)

m2 <- Arima(dlRPF, order = c(2, 0, 0))
m2
ggtsdiag(m2, gof.lag = nlags)


# z-statistics for coefficients of AR(2) model - phi1 is not signifficant at any level
z <- m2$coef/sqrt(diag(m2$var.coef))
# p values
2*(1-pnorm(abs(z))) %>% round(5)

# estimate ARMA model with a restriction on a parameter
m2_rest <- Arima(dlRPF, order = c(2, 0, 0), fixed = c(0, NA, NA))
m2_rest
ggtsdiag(m2_rest, gof.lag = nlags)

# find the best ARIMA model based on either AIC, AICc or BIC
m3 <- auto.arima(dlRPF, ic = "aicc", seasonal = FALSE, stationary = TRUE)
m3
ggtsdiag(m3, gof.lag = nlags)

m4 <- auto.arima(dlRPF, ic = "aicc", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m4
ggtsdiag(m4, gof.lag = nlags)

# check stationarity and invertibility of the estimated model - plot inverse AR and MA roots
plot(m4)
autoplot(m4)

