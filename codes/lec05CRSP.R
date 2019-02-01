
library(readr)
library(magrittr)
library(tidyverse)
library(timetk)
library(ggfortify)
library(forecast)

# set black & white as default theme for ggplot2
theme_set(theme_bw())

# data from Center for Research in Security Prices (CRSP) of the University of Chicago
crsp_raw <- read_table2("http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/m-ibm3dx2608.txt")
str(crsp_raw)
crsp_raw

# monthly returns of value-weighted market index in zoo format - whole sample
crsp_xts <- crsp_raw %>%
    mutate(date = date %>% as.character() %>% as.Date(format = "%Y%m%d")) %>%
    tk_xts(select = vwrtn, date_var = date)

str(crsp_xts)

autoplot(crsp_xts) + labs(title = "Monthly returns of value-weighted market index")

# monthly returns of value-weighted market index in ts format - subsample
y <- crsp_xts %>% window(end="2007-12-31")

nlags <- 24
ggAcf(y, lag.max = nlags)
ggPacf(y, lag.max = nlags)

m1 <- Arima(y, order = c(3,0,0))
m1
ggtsdiag(m1, gof.lag = nlags)

# z-statistics for coefficients of AR(3) model - phi2 is signifficant at 5% level, phi3 is marginally insignifficant
m1$coef/sqrt(diag(m1$var.coef))
# p values
(1-pnorm(abs(m1$coef)/sqrt(diag(m1$var.coef))))*2

m2 <- Arima(y, order = c(3,0,0), fixed = c(NA,0,NA,NA))
m2
ggtsdiag(m2, gof.lag = nlags)



auto.arima(y, stationary = TRUE, seasonal = FALSE, ic = "bic")
auto.arima(y, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, ic = "bic")
auto.arima(y, stationary = TRUE, seasonal = FALSE, ic = "aic", trace = TRUE)
auto.arima(y, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, ic = "aic", trace = TRUE)


m3 <- auto.arima(y, stationary = TRUE, stepwise = FALSE, ic = "bic")
m3
ggtsdiag(m3, gof.lag = nlags)


m1.fcst <- forecast(m1, h = nlags)
autoplot(m1.fcst)

# compute intercept phi_0
(1-sum(m1$coef[1:3]))*mean(y)

# compute standard error of residuals
sqrt(m1$sigma2)

