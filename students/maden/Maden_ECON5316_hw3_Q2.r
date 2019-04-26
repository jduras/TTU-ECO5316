
library(magrittr)
library(tidyverse)
library(tidyquant)
library(timetk)
library(ggfortify)
library(forecast)

# set default ggplot theme to theme_bw()
theme_set(theme_bw())

# get time series for quarterly total wages and salaries in Texas
PFI_raw <- 
  tq_get("B007RA3Q086SBEA", get = "economic.data", from = "1947-01-01", to = "2015-04-01")

str(PFI_raw)
glimpse(PFI_raw)

PFI_tbl <-
  PFI_raw %>%
  rename(PFI = price) %>%
  mutate(dlPFI = log(PFI) - lag(log(PFI)))

PFI_tbl %>%
  gather(measure, value, c(PFI, dlPFI)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line()+
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "Real private fixed investment") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlPFI = "Log Change",
                                             PFI = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))

dlPFI <- PFI_tbl %>%
  filter(!is.na(dlPFI)) %>%
  tk_xts(date_var = date, select = dlPFI) 

str(dlPFI)

# number of lags for ACF and PACF plots
nlags <- 24

g1= ggAcf(dlPFI, lag.max = nlags)
g2= ggPacf(dlPFI, lag.max = nlags)

library(egg)
gl<-ggAcf(dlPFI, lag.max = nlags)
g2 <- ggPacf(dlPFI, lag.max = nlags)
ggarrange(gl,g2,ncol=1)


# estimate ARMA models
m1 <- Arima(dlPFI, order = c(0, 0, 2))
m1
ggtsdiag(m1, gof.lag = nlags)

m2 <- Arima(dlPFI, order = c(2, 0, 0))
m2
ggtsdiag(m2, gof.lag = nlags)

# find the best ARIMA model based on either AIC, AICc or BIC
m3 <- auto.arima(dlPFI, ic = "aicc", seasonal = FALSE, stationary = TRUE, trace=TRUE)
m3
ggtsdiag(m3, gof.lag = nlags)

m4 <- auto.arima(dlPFI, ic = "aicc", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m4
ggtsdiag(m4, gof.lag = nlags)

# check stationarity and invertibility of the estimated model - plot inverse AR and MA roots
plot(m4)
autoplot(m4)