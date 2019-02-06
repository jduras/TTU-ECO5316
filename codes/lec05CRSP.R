
library(readr)
library(tidyverse)
library(timetk)
library(ggfortify)
library(egg)
library(forecast)

# set black & white as default theme for ggplot2
theme_set(theme_bw())

# data from Center for Research in Security Prices (CRSP) of the University of Chicago
crsp_raw <- read_table2("http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/m-ibm3dx2608.txt")
crsp_raw

str(crsp_raw)
glimpse(crsp_raw)


# monthly returns of value-weighted market index in xts format - whole sample
crsp_xts <- crsp_raw %>%
    mutate(date = date %>% as.character() %>% as.Date(format = "%Y%m%d")) %>%
    tk_xts(select = vwrtn, date_var = date) 

str(crsp_xts)

autoplot(crsp_xts) + labs(title = "Monthly returns of value-weighted market index")

# monthly returns of value-weighted market index in xts format - subsample
y <- crsp_xts %>% window(end = "2007-12-31")

# plot ACF and PACF
nlags <- 24
g1 <- ggAcf(y, lag.max = nlags)
g2 <- ggPacf(y, lag.max = nlags)
ggarrange(g1, g2, ncol = 1)

# estimate models
m <- Arima(y, order = c(3,0,0))
m
ggtsdiag(m, gof.lag = nlags)

m <- auto.arima(y, ic = "bic", stationary = TRUE, seasonal = FALSE, approximation = FALSE, stepwise = FALSE)
m
ggtsdiag(m, gof.lag = nlags)

m <- auto.arima(y, ic = "aicc", stationary = TRUE, seasonal = FALSE, approximation = FALSE, stepwise = FALSE)
m
ggtsdiag(m, gof.lag = nlags)
