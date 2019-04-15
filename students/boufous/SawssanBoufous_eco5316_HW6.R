library(magrittr)
library(timetk)
library(tibbletime)
library(broom)
library(ggplot2)
library(ggfortify)
library(forecast)
library(readr)
library(tidyquant)
library(vars)
library(plotly)

#(a)

gdp.tbl <-
  tq_get(c("GDPC1","GDPDEF"), get = "economic.data",
         from  = "1950-01-01", to = "2017-12-31") %>%
  spread(symbol, price) %>%
  mutate(qtryear = as.yearqtr(date))


spindex.tbl <-
  tq_get("^GSPC", from  = "1950-01-01", to = "2017-12-31") %>%
  mutate(qtryear = as.yearqtr(date)) %>%
  group_by(qtryear) %>%
  summarise(SP500 = mean(adjusted)) %>%
  ungroup()

#(b)

y.tbl <- full_join(gdp.tbl, spindex.tbl, by = "qtryear") %>%
  mutate(dlrGDP = 400*(log(GDPC1) - lag(log(GDPC1))),
         dlrSP500 = 100*( (  log(SP500) - lag(log(SP500)) )    -    ( log(GDPDEF) - lag(log(GDPDEF))  )   ) )%>%
  dplyr::select(qtryear, dlrGDP, dlrSP500) %>%
  na.trim()



new.ts <- tk_ts(y.tbl, select = c("dlrGDP","dlrSP500"), start = 1950, frequency = 4)
new1.ts <- tk_ts(y.tbl, select = c("dlrGDP","dlrSP500"), start = 1990, frequency = 4)

#(c)

VARselect(new1.ts, lag.max = 8, type = "const")

# estimate a reduced form VAR(1)
var1 <- VAR(new1.ts, p = 1, type = "const")
var1
summary(var1)

# estimate VAR(p) using AIC to select p
varp <- VAR(new1.ts, ic = "AIC", lag.max = 8, type = "const")
varp
summary(varp)


#(d)
causality(var1, cause = "dlrGDP")
causality(var1, cause = "dlrSP500")


causality(varp, cause = "dlrGDP")
causality(varp, cause = "dlrSP500")


#(e)
# estimate restricted VAR - based on Granger causality test eliminate lags of RI from the equation for  LA

# define a  matrix with restictions
mat.r <- matrix(1, nrow = 2, ncol = 5)
mat.r[1, c(2,3,3)] <- 0
mat.r
varp.r <- restrict(varp, method = "manual", resmat = mat.r)
varp.r
summary(varp.r)

# estimate restricted VAR - keep only variables with t-value larger than 2.0
varp.r.ser <- restrict(varp, method = "ser", thresh = 2.0)
varp.r.ser
summary(varp.r.ser)
varp.r.ser$restrictions
Acoef(varp.r.ser)


#(f)


var1.f <- predict(var1, n.ahead = 4, ci=0.9)

autoplot(var1.f, is.date = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "", y = "", title = "Multistep forecast for 2019Q1-2019Q4")

fanchart(var1.f)


varp.f <- predict(varp, n.ahead = 4, ci=0.9)
plot(varp.f)
fanchart(varp.f)
autoplot(varp.f, is.date = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "", y = "", title = "Multistep forecast for 2019Q1-2019Q4")

## Compare with Federal Bank of New York Nowcast
# Our forecast is larger than the NY Nowcast forecasting. 

## Compare with GDPNow Federal Bank of Atlanta forecast
#Our forecast shows a larger forecast than the Federal Bank of Atlanta forecasting

## Compare with the Wall Street Journal Economic Forecasting Survey

# Same outcome, our forecasting seems to be larger than the WSJ

# Based on our comparisons, our dlrGDP forecasts display a larger value 
# than the ones provided by the three sources.











