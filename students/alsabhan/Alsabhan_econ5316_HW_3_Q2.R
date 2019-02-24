
library(readr)
library(magrittr)
library(tidyverse)
library(timetk)
library(ggfortify)
library(egg)
library(scales)
library(forecast)
library(tidyquant)


#Use tq_get to obtain the quarterly real private ???xed investment (chain-type quantity index), available on FRED under
#code B007RA3Q086SBEA.

rPFIt_raw <-
  tq_get("B007RA3Q086SBEA", get = "economic.data", from = "1947-01-01", to = "2015-04-01")
theme_set(theme_bw())

str(rPFIt_raw)
glimpse(rPFIt_raw)

#Construct the log changes in the real private ???xed investment ???logrPFIt =logrPFIt ???logrPFIt???1 where rPFIt is the
#original quarterly real private ???xed investment

rPFIt_tbl <-
  rPFIt_raw %>%
  rename( rPFIt = price ) %>%
  mutate(dlrPFIt = log(rPFIt) - lag(log(rPFIt)))

# Plot the time series for rPFIt and ???logrPFIt using ggplot.

rPFIt_tbl %>%
  gather(measure, value, c(rPFIt, dlrPFIt)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = "Real private fixed investment, Seasonally Adjusted") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlrPFIt = "Log Change",
                                             rPFIt = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))
dlrPFIt <- rPFIt_tbl %>%
  filter(!is.na(dlrPFIt)) %>%
  tk_xts(date_var = date, select = dlrPFIt)

#Construct and plot the ACF and the PACF for ???logrPFIt using ggAcf and ggPacf
nlags <- 24
ggPacf(dlrPFIt, lag.max = nlags)
ggAcf(dlrPFIt, lag.max = nlags)
g2 <- ggPacf(dlrPFIt, lag.max = nlags)
g1 <- ggAcf(dlrPFIt, lag.max = nlags)
ggarrange(g1, g2, ncol = 1 )

#While most lags appear to be not significant, the first lag in both the ACF and PACF is very significant. Also the
#second lag is only significant in Acf.Therefore, we could estimate an ARMA(1,2) and ARMA(2,2).

#Use the ACF and PACF to identify suitable AR and/or MA model(s) and estimate them using Arima.
m1 <- Arima(dlrPFIt, order = c(1, 0, 0))
m1
m2<- Arima(dlrPFIt, order = c(2, 0, 0))
m2
arma <-  Arima(dlrPFIt, order = c(1, 0, 1))
arma
arma1 <-  Arima(dlrPFIt, order = c(1, 0, 2))
arma1
arma2<-  Arima(dlrPFIt, order = c(2, 0, 1))
arma2
arma3<-  Arima(dlrPFIt, order = c(2, 0, 2))
arma3

#th of AIC and the AICc with small sample correction both in preference of the ARMA(2,1) while the AR(1)
#has slight preference when it comes to BIC.

z <- m1$coef/sqrt(diag(m1$var.coef))
coef(m1) / sqrt(diag(vcov(m1)))
z <- m2$coef/sqrt(diag(m2$var.coef))
coef(m2) / sqrt(diag(vcov(m2)))


# Perform diagnostics of model(s) from part (d) using ggtsdiag. Modify and reestimate the model if needed, if there
#are several competing speci???cations use AIC, BIC, Q statistics to compare their properties.

ggtsdiag(m1, gof.lag = nlags)

ggtsdiag(m2, gof.lag = nlags)

autoplot(m1)
autoplot(m2)
autoplot(arma2)

#Use the auto.arima function to ???nd the model speci???cation that minimizes AIC and the model speci???cation that minimizes
#BIC. Again perform the model diagnostics for these two models.

min_aic<- auto.arima(dlrPFIt, ic = "aic", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
min_aic

min_bic<- auto.arima(dlrPFIt, ic = "bic", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
min_bic


ggtsdiag(min_aic, gof.lag = nlags)

ggtsdiag(min_bic, gof.lag = nlags)


