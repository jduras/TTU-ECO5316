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

#change in industrial production index
IPI_tbl <-
  IPI_raw %>%
  rename(IPI = price) %>%
  mutate(dlIPI = log(IPI) - lag(log(IPI)))

#plotting
IPI_tbl %>%
  gather(measure, value, c(IPI, dlIPI)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line()+
  #geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "Industrial Production Index") +
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

g1= ggAcf(dlIPI, lag.max = nlags)
g2= ggPacf(dlIPI, lag.max = nlags)

library(egg)
gl<-ggAcf(dlIPI, lag.max = nlags)
g2 <- ggPacf(dlIPI, lag.max = nlags)
ggarrange(gl,g2,ncol=1)

# ACF declines gradually and is significant till third lag and the PACF is significant at first lag. Therefore,this
#model follows AR(1) Process.


# estimate ARMA models
m1 <- Arima(dlIPI, order = c(0, 0, 1))
m1
ggtsdiag(m1, gof.lag = nlags)

m2 <- Arima(dlIPI, order = c(1, 0, 0))
m2
ggtsdiag(m2, gof.lag = nlags)

#The MA(1) model shows that the lags are highly correlated from the first lag. with AR(1) process we
#can see that the correlation is higher after fifth lag. So, second model performs better. Still there is high correlation
# among lags. 
# find the best ARIMA model based on either AIC, AICc or BIC
m3 <- auto.arima(dlIPI, ic = "aicc", seasonal = FALSE, stationary = TRUE, trace=TRUE)
m3
ggtsdiag(m3, gof.lag = nlags)

#auto.arima performs silightly better than ARMA(1,0,0) and lags are correlated only after the seventh lag.

m4 <- auto.arima(dlIPI, ic = "aicc", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m4
ggtsdiag(m4, gof.lag = nlags)

# check stationarity and invertibility of the estimated model - plot inverse AR and MA roots
plot(m4)
autoplot(m4)