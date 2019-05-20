library(magrittr)
library(tidyverse)
library(tidyquant)
library(timetk)
library(ggfortify)
library(forecast)

# set default ggplot theme to theme_bw()
theme_set(theme_bw())

# get time series for quarterly total wages and salaries in Texas
IPI_raw <- 
  tq_get("INDPRO", get = "economic.data", from = "1919-01-01", to = "2018-12-01")
str(IPI_raw)


IPI_tbl <-
  IPI_raw %>%
  rename(IPI = price) %>%
  mutate(dlIPI = log(IPI) - lag(log(IPI)))


IPI_tbl%>%
  gather(measure, value, c(IPI, dlIPI)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line()+
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "INDUSTRIAL PRODUCTION INDEX") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlIPI = "Log Change",
                                             IPI = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))


dlIPI <- IPI_tbl %>%
  filter(!is.na(dlIPI)) %>%
  tk_xts(date_var = date, select = dlIPI) 

str(dlIPI)

# number of lags for ACF and PACF plots
nlags <- 24

g1 = ggAcf(dlIPI, lag.max = nlags)
g2 = ggPacf(dlIPI, lag.max = nlags)

library(egg)
g1=ggAcf(dlIPI, lag.max = nlags)
g2=ggPacf(dlIPI, lag.max = nlags)
ggarrange(g1, g2, ncol=1)

#ACF is declinig up to 3 lags and is significant up to third lag. 
#Whereas PaCF is significant at first lag. This implies that it follows AR1 process.

m1 <- Arima(dlIPI, order = c(0, 0, 1))
m1
ggtsdiag(m1, gof.lag = nlags)

m2 <- Arima(dlIPI, order = c(1, 0, 0))
m2
ggtsdiag(m2, gof.lag = nlags)
#From the graph we can see that, in the first model (MA1) from the first lag, lags are highly correlated.
#Whereas in the second model (AR1), after the sixth lag, lags are highly correlated.
#Therefore,  second model is better than the first model.
m3 <- auto.arima(dlIPI, ic = "aicc", seasonal = FALSE, stationary = TRUE, trace=TRUE)
m3
ggtsdiag(m3, gof.lag = nlags)
#Here in auto.arima, the lags are uncorrelated till seventh lag and correlated from eight lag. Therefore, this model is  better. 

m4 <- auto.arima(dlIPI, ic = "aicc", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m4
ggtsdiag(m4, gof.lag = nlags)

# check stationarity and invertibility of the estimated model - plot inverse AR and MA roots
plot(m4)
autoplot(m4)

