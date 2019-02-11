
library(magrittr)
library(tidyverse)
library(tidyquant)
library(timetk)
library(ggfortify)
library(forecast)
library(tidyverse)
library(timetk)
library(egg)
library(readr)



IND <- 
  tq_get("INDPRO", get = "economic.data", from = "1919-01-01", to = "2018-12-01")
gtr4g



str(IND)
glimpse(IND)

IND_tbl <-
  IND %>%
  rename(IND = price) %>%
  mutate(dlIND = log(IND) - lag(log(IND)))

# plot time series  - using ggplot
IND_tbl %>%
  gather(measure, value, c(IND, dlINDi)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "Real Private Fixed Investment, Seasonally Adjusted Annual Rate") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlIND = "Log Change",
                                             IND = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))


dlIND <- IND_tbl %>%
  filter(!is.na(dlIND)) %>%
  tk_xts(date_var = date, select = dlIND) 

str(dlIND)


# number of lags for ACF and PACF plots
nlags <- 24

ggAcf(dlIND, lag.max = nlags)
ggPacf(dlIND, lag.max = nlags)

# plot ACF and PACF together in same figure
g1 <- ggAcf(dlIND, lag.max = nlags)
g2 <- ggPacf(dlIND, lag.max = nlags)
ggarrange(g1, g2, ncol = 1)

ar1 <- arima(dlIND, order=c(1,0,0))

ar1

ar2 <- arima(dlIND, order=c(2,0,0))

ar2

ar3 <- arima(dlIND, order=c(3,0,0))

ar3

ar4 <- arima(dlIND, order=c(4,0,0))

ar4

ma1 <- arima(dlIND, order=c(0,0,1))

ma1

ma2 <- arima(dlIND, order=c(0,0,2))

ma2

ma3 <-arima(dlIND, order=c(0,0,3))

ma3

ma4 <-arima(dlIND, order=c(0,0,4))

ma4

ARIMA <- arima(dlIND, order=c(1,0,2))

ARIMA1

ARIMA2 <- arima(dlIND, order=c(2,0,2))

ARIMA2
ARIMA3 <- arima(dlIND, order=c(2,0,1))

ARIMA3





# estimate ARMA models by using  ggtsdiag

ggtsdiag(ARIMA3, gof.lag = nlags)



ARIMA3 <- auto.arima(dlIND, ic = "bic", stationary = TRUE, seasonal = FALSE, approximation = FALSE, stepwise = FALSE)
ARIMA3
ggtsdiag(ARIMA3, gof.lag = nlags)

ARIMA3 <- auto.arima(dlIND, ic = "aicc", stationary = TRUE, seasonal = FALSE, approximation = FALSE, stepwise = FALSE)
ARIMA3
ggtsdiag(ARIMA3, gof.lag = nlags)


