
library(magrittr)
library(tidyverse)
library(tidyquant)
library(timetk)
library(ggfortify)
library(forecast)
library(tidyverse)
library(timetk)
library(egg)
library(readr )
# get time series for quarterly 
rpfi <- 
  tq_get("B007RA3Q086SBEA", get = "economic.data", from = "1947-01-01", to = "2015-04-01")

str(rpfi)
glimpse(rpfi)

rpfi_tbl <-
  rpfi %>%
  rename(rpfi = price) %>%
  mutate(dlrpfi = log(rpfi) - lag(log(rpfi)))

# plot time series  - using ggplot
rpfi_tbl %>%
  gather(measure, value, c(rpfi, dlrpfi)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "Real Private Fixed Investment, Seasonally Adjusted Annual Rate") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlrpfi = "Log Change",
                                             rpfi = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))


dlrpfi <- rpfi_tbl %>%
  filter(!is.na(dlrpfi)) %>%
  tk_xts(date_var = date, select = dlrpfi) 

str(dlrpfi)


# number of lags for ACF and PACF plots
nlags <- 24

ggAcf(dlrpfi, lag.max = nlags)
ggPacf(dlrpfi, lag.max = nlags)

# plot ACF and PACF together in same figure
g1 <- ggAcf(dlrpfi, lag.max = nlags)
g2 <- ggPacf(dlrpfi, lag.max = nlags)
ggarrange(g1, g2, ncol = 1)

ar1 <- arima(dlrpfi, order=c(1,0,0))

ar1

ar2 <- arima(dlrpfi, order=c(2,0,0))

ar2


ma1 <- arima(dlrpfi, order=c(0,0,1))

ma1

ma2 <- arima(dlrpfi, order=c(0,0,2))

ma2

ma3 <-arima(dlrpfi, order=c(0,0,3))

ma3

ma4 <-arima(dlrpfi, order=c(0,0,4))

ma4

ARIMA1 <- arima(dlrpfi, order=c(1,0,1))

ARIMA1






# estimate ARMA models by using  ggtsdiag
m1 <- arima(dlrpfi, order=c(1,0,0)) 
m1
ggtsdiag(m1, gof.lag = nlags)




ARIMA3 <- auto.arima(dlrpfi, ic = "bic", stationary = TRUE, seasonal = FALSE, approximation = FALSE, stepwise = FALSE)
ARIMA3
ggtsdiag(ARIMA3, gof.lag = nlags)

ARIMA3 <- auto.arima(dlrpfi, ic = "aicc", stationary = TRUE, seasonal = FALSE, approximation = FALSE, stepwise = FALSE)
ARIMA3
ggtsdiag(ARIMA3, gof.lag = nlags)


