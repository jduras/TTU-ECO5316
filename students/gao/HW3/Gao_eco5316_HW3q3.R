---
title: "5316 homework 3 Problem 3"
author: "Shijia Gao"

library(tidyquant)
library(timetk)
library(tsibble)
library(broom)
library(sweep)
library(ggplot2)
library(scales)
library(ggfortify)
library(egg)
library(tictoc)
library(forecast)

# Problem 3
# set default ggplot theme to theme_bw()
theme_set(theme_bw())
#(a) obtain the monthly industrial production index
IPID_raw <- 
  tq_get("INDPRO", get = "economic.data", from = "1919-01-01", to = "2018-12-01")
str(IPID_raw)
glimpse(IPID_raw)

#(b) Construct the log changes and plot
IPID_tbl <-
  IPID_raw %>%
  rename(IPID = price) %>%
  mutate(dlIPID = log(IPID) - lag(log(IPID)))
# plot time series - using ggplot
IPID_tbl %>%
  gather(measure, value, c(IPID, dlIPID)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line()+ 
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "Industrial Production Index, Seasonally Adjusted Annual Rate") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlIPID = "Log Change",
                                             IPID = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))

dlIPID <- IPID_tbl %>%
  filter(!is.na(dlIPID)) %>%
  tk_xts(date_var = date, select = dlIPID) 

str(dlIPID)

#(c) Construct and plot the ACF and the PACF for log rPFIt using ggAcf and ggPacf
# number of lags for ACF and PACF plots
nlags <- 24

# plot ACF and PACF
g1 <- ggAcf(dlIPID, lag.max = nlags)
g2 <- ggPacf(dlIPID, lag.max = nlags)
ggarrange(g1, g2, ncol = 1)

# From the chart PACF above, we can see that lag1 is significant, acf shows a gradual decay. We can do the estimation from AR(1)

#(d) Use the ACF and PACF to identify suitable AR and/or MA model(s) and estimate them using Arima
#(e) Perform diagnostics of model(s) from part (d) using ggtsdiag. Modify and reestimate the model
#    if needed, if there are several competing specifications use AIC, BIC, Q statistics to compare their properties.
arm1 <- Arima(dlIPID, order = c(1, 0, 0))
arm1
ggtsdiag(arm1, gof.lag = nlags)
mam1.LB <- Box.test(arm1$residuals, lag=2, type = "Ljung")
mam1.LB
pv <- 1-pchisq(arm1.LB$statistic,1)
pv
arm2 <- Arima(dlIPID, order = c(2, 0, 0))
arm2
ggtsdiag(arm2, gof.lag = nlags)
arm3 <- Arima(dlIPID, order = c(3, 0, 0))
arm3
ggtsdiag(arm3, gof.lag = nlags)
arm4 <- Arima(dlIPID, order = c(4, 0, 0))
arm4
ggtsdiag(arm4, gof.lag = nlags)
#from above we conclude that p = 2, for AIC(2)-6453.71< AIC(3)-6451.84
ma1 <- arima(yt, order = c(0,0,1))
tsdiag(ma1, gof.lag= nlags)
ma1
ma2 <- arima(yt,order = c(0,0,2))
tsdiag(ma2, gof.lag= nlags)
ma2
#from above we conclude that q = 1, for AIC(1)-5862.71 < AIC(2)-5861.03

#(f) Use the auto.arima function to find the model specification that minimizes AIC and the model
#specification that minimizes BIC. Again perform the model diagnostics for these two models.
# find the best ARIMA model based on either AIC, AICc or BIC
m3 <- auto.arima(dlIPID, ic = "aicc", seasonal = FALSE, stationary = TRUE, trace = TRUE)
m3
ggtsdiag(m3, gof.lag = nlags)

m4 <- auto.arima(dlIPID, ic = "aicc", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
m4
ggtsdiag(m4, gof.lag = nlags)

# Best model: Best model: ARIMA(2,0,1) with non-zero, so our conclusion before is correct. 

#(g) Summarize your findings---Having done before.
