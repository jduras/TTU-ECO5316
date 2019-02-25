library(magrittr)

library(tidyverse)

library(tidyquant)

library(timetk)

library(ggfortify)

library(egg)

library(forecast)


# set default ggplot theme to theme_bw()

theme_set(theme_bw())


# (a) Use `tq_get` to obtain the monthly industrial production index, available on FRED under code [`INDPRO`]
PI <- tq_get(x = c("INDPRO"), get = "economic.data", from="1919-01-01", to="2018-12-01")

str(PI)

glimpse(PI)


# (b) Construct the log changes in the industrial production $\Delta \log IP_t = \log IP_t - \log IP_{t-1}$ where $IP_t$ is the original industrial production index. 
# Plot the time series for $IP_t$ and $\Delta \log IP_t$ using `ggplot`.
PI_tbl<- PI %>% rename(IPt=price)%>% mutate (dlIPt= log(IPt)-lag(log(IPt)))
PI_tbl

#Plot
PI_tbl %>%
  gather(measure, value, c(IPt, dlIPt)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "Industrial Production Index") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlIPt = "Log Change",
                                             IPt = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))

dlIPt <- PI_tbl %>%
  filter(!is.na(dlIPt)) %>%
  tk_xts(date_var = date, select = dlIPt) 

str(dlIPt)

# (c) Follow the same steps as in Problem 2 parts (c)-(g) to find suitable AR/MA/ARMA model(s), this time for $\Delta \log IP_t$.
## Suitable AR/MA/
# (c.1) Construct and plot the ACF and PACF
nlags<- 45
ggAcf(dlIPt, lag.max = nlags)
ggPacf(dlIPt, lag.max = nlags)

# (c.2)estimate ARMA models
m1 <- Arima(dlIPt, order = c(0, 0, 2))
m1
ggtsdiag(m1, gof.lag = nlags)

m2 <- Arima(dlIPt, order = c(2, 0, 0))
m2
ggtsdiag(m2, gof.lag = nlags)

#(c.3) Use the ACF and PACF to identify suitable AR and/or MA model(s) and estimate them using `Arima`
# estimate ARMA models
# Perform diagnostics of model(s) from part (d) using `ggtsdiag`.
m1 <- Arima(dlIPt, order = c(0, 0, 2))
m1
ggtsdiag(m1, gof.lag = nlags)

m2 <- Arima(dlIPt, order = c(2, 0, 0))
m2
ggtsdiag(m2, gof.lag = nlags)

# (c.4) Use the `auto.arima` function to find the model specification that minimizes AIC and the model specification that minimizes BIC. Again perform the model diagnostics for these two models. 
m3 <- auto.arima(dlIPt, ic = "aic", seasonal = FALSE, stationary = TRUE)
m3
ggtsdiag(m3, gof.lag = nlags)

m4 <- auto.arima(dlIPt, ic = "aic", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m4
ggtsdiag(m4, gof.lag = nlags)

# check stationarity and invertibility of the estimated model - plot inverse AR and MA roots
plot(m4)
autoplot(m4)
