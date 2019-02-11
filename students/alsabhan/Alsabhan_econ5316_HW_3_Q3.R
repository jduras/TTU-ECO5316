
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
#code INDPRO.
IPt_raw <-
  tq_get("INDPRO", get = "economic.data", from = "1919-01-01", to = "2018-12-01")
theme_set(theme_bw())


str(IPt_raw)
glimpse(IPt_raw)



#Construct the log changes in the industrial production index ???logrIPt =logrIPt ???logrIPt???1 where logrIPt is the
#original industrial production index. 
IPt_tbl <-
  IPt_raw %>%
  rename( IPt = price ) %>%
  mutate(dlIPt = log(IPt) - lag(log(IPt)))

# Plot the time series for IPt and ???logrIPt using ggplot.
IPt_tbl %>%
  gather(measure, value, c(IPt, dlIPt)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  # geom_hline(yintercept = 0, color = "gray50") +
  labs(x = "", y = "", 
       title = " Industrial Production Index, Seasonally Adjusted") +
  facet_wrap(~measure, ncol = 1, scales = "free",
             labeller = labeller(measure = c(dlIPt = "Log Change",
                                             IPt = "Thousands of Dollars"))) +
  theme(strip.text = element_text(hjust = 0))
dlIPt <- IPt_tbl %>%
  filter(!is.na(dlIPt)) %>%
  tk_xts(date_var = date, select = dlIPt)

#Construct and plot the ACF and the PACF for ???logrIPt using ggAcf and ggPacf
nlags <- 24
ggPacf(dlIPt, lag.max = nlags)
ggAcf(dlIPt, lag.max = nlags)
g2 <- ggPacf(dlIPt, lag.max = nlags)
g1 <- ggAcf(dlIPt, lag.max = nlags)
ggarrange(g1, g2, ncol = 1 )

#Use the ACF and PACF to identify suitable AR and/or MA model(s) and estimate them using Arima.

m1 <- Arima(dlIPt, order = c(1, 0, 0))
m1
m2 <- Arima(dlIPt, order = c(2, 0, 0))
m2

#While most lags appear to be not significant, the first lag in both the ACF and PACF is very significant. Also the
#second lag is only significant in Acf.Therefore, we could estimate an ARMA(1,2) and ARMA(2,2).


arma <-  Arima(dlIPt, order = c(1, 0, 1))
arma
arma1 <-  Arima(dlIPt, order = c(1, 0, 2))
arma1
arma2<-  Arima(dlIPt, order = c(2, 0, 1))
arma2
arma3<-  Arima(dlIPt, order = c(2, 0, 2))
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

ggtsdiag(arma2, gof.lag = nlags)


autoplot(m1)
autoplot(m2)
autoplot(arma3)

#Use the auto.arima function to ???nd the model speci???cation that minimizes AIC and the model speci???cation that minimizes
#BIC. Again perform the model diagnostics for these two models.
min_aic<- auto.arima(dlIPt, ic = "aic", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
min_aic

min_bic<- auto.arima(dlIPt, ic = "bic", seasonal = FALSE, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
min_bic

ggtsdiag(min_aic, gof.lag = nlags)

ggtsdiag(min_bic, gof.lag = nlags)


