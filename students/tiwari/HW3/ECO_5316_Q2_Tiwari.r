library(readr)
library(magrittr)
library(tidyverse)
library(timetk)
library(ggfortify)
library(egg)
library(scales)
library(forecast)
library(tidyquant)

Rpfi <-
  tq_get("B007RA3Q086SBEA", get = "economic.data", from = "1947-01-01", to = "2015-04-01")
theme_set(theme_bw())

str(Rpfi)
glimpse(Rpfi)

Rpfi_tbl <-
  Rpfi %>%
  rename( Rpfi = price ) %>%
  mutate(dlRpfi = log(Rpfi) - lag(log(Rpfi)))

Rpfi_tbl %>%
  gather(measure, value, c(Rpfi, dlRpfi)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "Real private fixed investment, Seasonally Adjusted") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlRpfi = "Log Change",
                                             Rpfi = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))
dlRpfi <- Rpfi_tbl %>%
  filter(!is.na(dlRpfi)) %>%
  tk_xts(date_var = date, select = dlRpfi)

nlags <- 24
ggPacf(dlRpfi, lag.max = nlags)
ggAcf(dlRpfi, lag.max = nlags)
g2 <- ggPacf(dlRpfi, lag.max = nlags)
g1 <- ggAcf(dlRpfi, lag.max = nlags)
ggarrange(g1, g2, ncol = 1 )

ar1 <- Arima(dlRpfi, order = c(1, 0, 0))
ar1

ar2 <- Arima(dlRpfi, order = c(2, 0, 0))
ar2

ma1 <- Arima(dlRpfi, order = c(0, 0, 1))
ma1
arma1<- Arima(dlRpfi, order = c(1, 0, 1))
arma1
arma12<-Arima(dlRpfi, order = c(1, 0, 2))
arma12

z <- ar1$coef/sqrt(diag(ar1$var.coef))
coef(ar1) / sqrt(diag(vcov(ar1)))

ggtsdiag(ar1, gof.lag = nlags)

ggtsdiag(ar2, gof.lag = nlags)

min_aic<- auto.arima(dlRpfi, ic = "aic", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
min_aic
min_bic<- auto.arima(dlRpfi, ic = "bic", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
min_bic

ggtsdiag(min_aic, gof.lag = nlags)

ggtsdiag(min_bic, gof.lag = nlags)
