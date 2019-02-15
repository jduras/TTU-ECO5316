library(readr)
library(magrittr)
library(tidyverse)
library(timetk)
library(ggfortify)
library(egg)
library(scales)
library(forecast)
library(tidyquant)


Ip<-
tq_get("INDPRO", get = "economic.data", from = "1919-01-01", to = "2018-12-01")
theme_set(theme_bw())
str(Ip)
glimpse(Ip)

Ip_tbl <-
  Ip %>%
  rename( Ip = price ) %>%
  mutate(dlIp = log(Ip) - lag(log(Ip)))


Ip_tbl %>%
  gather(measure, value, c(Ip, dlIp)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "Real private fixed investment, Seasonally Adjusted") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlIp = "Log Change",
                                             Ip = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))
dlIp <- Ip_tbl %>%
  filter(!is.na(dlIp)) %>%
  tk_xts(date_var = date, select = dlIp)


nlags <- 24
ggPacf(dlIp, lag.max = nlags)
ggAcf(dlIp, lag.max = nlags)
g2 <- ggPacf(dlIp, lag.max = nlags)
g1 <- ggAcf(dlIp, lag.max = nlags)
ggarrange(g1, g2, ncol = 1 )

ar1 <- Arima(dlIp, order = c(1, 0, 0))
ar1
ar2 <- Arima(dlIp, order = c(2, 0, 0))
ar2

arma1<- Arima(dlIp, order = c(1, 0, 1))
arma1

arma2<- Arima(dlIp, order = c(2, 0, 2))
arma2

arma12<- Arima(dlIp, order = c(1, 0, 2))
arma12
arma21<- Arima(dlIp, order = c(2, 0, 1))
arma21


z <- arma21$coef/sqrt(diag(ar1$var.coef))
coef(arma21) / sqrt(diag(vcov(arma21)))

z <- arma1$coef/sqrt(diag(arma1$var.coef))
coef(arma1) / sqrt(diag(vcov(arma1)))

ggtsdiag(arma21, gof.lag = nlags)

ggtsdiag(arma1, gof.lag = nlags)

min_aic<- auto.arima(dlIp, ic = "aic", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
min_aic
min_bic<- auto.arima(dlIp, ic = "bic", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
min_bic


