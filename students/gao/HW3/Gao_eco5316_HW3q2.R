---
title: "5316 homework 3 Problem 2"
author: "Shijia Gao"

# Problem 2
# set default ggplot theme to theme_bw()
theme_set(theme_bw())
#(a) obtain the quarterly real private fixed investment
PIN_raw <- 
  tq_get("B007RA3Q086SBEA", get = "economic.data", from = "1947-01-01", to = "2015-04-01")
str(PIN_raw)
glimpse(PIN_raw)

#(b) Construct the log changes and plot
PIN_tbl <-
  PIN_raw %>%
  rename(PIN = price) %>%
  mutate(dlPIN = log(PIN) - lag(log(PIN)))
# plot time series - using ggplot
PIN_tbl %>%
  gather(measure, value, c(PIN, dlPIN)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line()+ 
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "real private fixed investment, Seasonally Adjusted Annual Rate") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlPIN = "Log Change",
                                             PIN = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))

dlPIN <- PIN_tbl %>%
  filter(!is.na(dlPIN)) %>%
  tk_xts(date_var = date, select = dlPIN) 

str(dlPIN)

#(c) Construct and plot the ACF and the PACF for log rPFIt using ggAcf and ggPacf
# number of lags for ACF and PACF plots
nlags <- 24

# plot ACF and PACF
g1 <- ggAcf(dlPIN, lag.max = nlags)
g2 <- ggPacf(dlPIN, lag.max = nlags)
ggarrange(g1, g2, ncol = 1)

# From the chart PACF above, we can see that lag1 is significant, acf shows a gradual decay. We can do the estimation from AR(1)

#(d) Use the ACF and PACF to identify suitable AR and/or MA model(s) and estimate them using Arima
#(e) Perform diagnostics of model(s) from part (d) using ggtsdiag. Modify and reestimate the model
#    if needed, if there are several competing specifications use AIC, BIC, Q statistics to compare their properties.
arm1 <- Arima(dlPIN, order = c(1, 0, 0))
arm1
ggtsdiag(arm1, gof.lag = nlags)
mam1.LB <- Box.test(arm1$residuals, lag=2, type = "Ljung")
mam1.LB
pv <- 1-pchisq(arm1.LB$statistic,1)
pv
arm2 <- Arima(dlPIN, order = c(2, 0, 0))
arm2
ggtsdiag(arm2, gof.lag = nlags)
arm3 <- Arima(dlPIN, order = c(3, 0, 0))
arm3
ggtsdiag(arm3, gof.lag = nlags)
arm4 <- Arima(dlPIN, order = c(4, 0, 0))
arm4
ggtsdiag(arm4, gof.lag = nlags)
#from above we conclude that p = 3, for AIC(3)-1331.22 < AIC(4)-1330.55
ma1 <- arima(yt, order = c(0,0,1))
tsdiag(ma1, gof.lag= nlags)
ma1
ma2 <- arima(yt,order = c(0,0,2))
tsdiag(ma2, gof.lag= nlags)
ma2
ma3 <- arima(yt,order = c(0,0,3))
tsdiag(ma3, gof.lag= nlags)
ma3
#from above we conclude that q = 2, for AIC(2)-1975.54 < AIC(3)-1975.22

#(f) Use the auto.arima function to find the model specification that minimizes AIC and the model
#specification that minimizes BIC. Again perform the model diagnostics for these two models.
# find the best ARIMA model based on either AIC, AICc or BIC
m3 <- auto.arima(dlPIN, ic = "aicc", seasonal = FALSE, stationary = TRUE, trace = TRUE)
m3
ggtsdiag(m3, gof.lag = nlags)

m4 <- auto.arima(dlPIN, ic = "aicc", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m4
ggtsdiag(m4, gof.lag = nlags)

# Best model: ARIMA(3,0,2) with non-zero mean, so our conclusion before is correct. 

#(g) Summarize your findings---Having done before.

