library(magrittr)

library(tidyverse)

library(tidyquant)

library(timetk)

library(ggfortify)

library(egg)

library(forecast)


# set default ggplot theme to theme_bw()

theme_set(theme_bw())


# (a) get time series for quarterly prived investment

         FI <- tq_get(x = c("B007RA3Q086SBEA"), get = "economic.data", from="1947-01-01", to="2015-04-01")

str(FI)

glimpse(FI)

#(b) Construct the log changes in the real private fixed investment $\Delta \log rPFI_t = \log rPFI_t - \log rPFI_{t-1}$ where $rPFI_t$ is the original quarterly real private fixed investment. Plot the time series for $rPFI_t$ and $\Delta \log rPFI_t$ using `ggplot`.

FI_tbl<- FI %>% rename(PFI=price)%>% mutate (dlpfi= log(PFI)-lag(log(PFI)))

#Plot
FI_tbl %>%
  gather(measure, value, c(PFI, dlpfi)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "Real Private fixed Investment") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlpfi = "Log Change",
                                             PFI = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))

dlpfi <- FI_tbl %>%
  filter(!is.na(dlpfi)) %>%
  tk_xts(date_var = date, select = dlpfi) 

str(dlpfi)

 # (c) Construct and plot the ACF and PACF
nlags<- 36
ggAcf(dlpfi, lag.max = nlags)
ggPacf(dlpfi, lag.max = nlags)

# (d) Use the ACF and PACF to identify suitable AR and/or MA model(s) and estimate them using `Arima`
# estimate ARMA models
# Perform diagnostics of model(s) from part (d) using `ggtsdiag`.
m1 <- Arima(dlpfi, order = c(0, 0, 2))
m1
ggtsdiag(m1, gof.lag = nlags)

m2 <- Arima(dlpfi, order = c(2, 0, 0))
m2
ggtsdiag(m2, gof.lag = nlags)



# (e) Perform diagnostics of model(s) from part (d) using `ggtsdiag`. Modify and reestimate the model if needed, if there are several competing specifications use AIC, BIC, Q statistics to compare their properties. 




# (f) Use the `auto.arima` function to find the model specification that minimizes AIC and the model specification that minimizes BIC. Again perform the model diagnostics for these two models. 
m3 <- auto.arima(dlpfi, ic = "aic", seasonal = FALSE, stationary = TRUE)
m3
ggtsdiag(m3, gof.lag = nlags)

m4 <- auto.arima(dlpfi, ic = "aic", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m4
ggtsdiag(m4, gof.lag = nlags)

# check stationarity and invertibility of the estimated model - plot inverse AR and MA roots
plot(m4)
autoplot(m4)

# (g) Summarize 
