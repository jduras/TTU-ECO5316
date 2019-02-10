#homework3
#homework3-2
library(magrittr)
library(tidyverse)
library(tidyquant)
library(timetk)
library(ggfortify)
library(forecast)
library(egg)
library(readr)
# set default ggplot theme to theme_bw()
theme_set(theme_bw())
# get time series of the quarterly real private fixed investment
rPFI <- 
  tq_get("B007RA3Q086SBEA", get = "economic.data", from = "1947-01-01", to = "2015-04-01")
str(rPFI)
glimpse(rPFI)
#log change, a stationary transformation
rPFI_tbl <-
  rPFI %>%
  rename(rPFI = price) %>%
  mutate(dlrPFI = log(rPFI) - lag(log(rPFI)))
# plot time series for the quarterly real private fixed investment - using ggplot
g1<-ggplot(data=rPFI_tbl,aes(x=date, y= rPFI)) +geom_line() +labs(x = "", y = "", 
       title = "Real private fixed investment(rPFI) ")+ 
       theme(strip.text = element_text(hjust = 0))
g2<-ggplot(data=rPFI_tbl,aes(x=date,y=dlrPFI))+geom_line()+labs(x="",y="",
       title="Real private fixed investment (dlrPFI)")+
       theme(strip.text = element_text(hjust = 0)) 
ggarrange(g1,g2,ncol=1)
#ACF and PACF plots
dlrPFI <- rPFI_tbl %>%
  filter(!is.na(dlrPFI)) %>%
  tk_xts(date_var = date, select = dlrPFI)
str(dlrPFI)
# number of lags for ACF and PACF plots
nlags <- 24
gg1<-ggAcf(dlrPFI, lag.max = nlags)
gg2<-ggPacf(dlrPFI, lag.max = nlags)
ggarrange(gg1,gg2,ncol=1)
# estimate ARMA models
m1 <- Arima(dlrPFI, order = c(0, 0, 3))
m1
ggtsdiag(m1, gof.lag = nlags)

m2 <- Arima(dlrPFI, order = c(3, 0, 0))
m2
ggtsdiag(m2, gof.lag = nlags)

# z-statistics for coefficients of AR(2) model - phi1 is not signifficant at any level
z <- m2$coef/sqrt(diag(m2$var.coef))
# p values
2*(1-pnorm(abs(z))) %>% round(5)
# the model specification that minimizes AIC and the model specification that minimizes BIC
auto.arima(dlrPFI, stationary = TRUE, seasonal = FALSE, ic = "bic")
auto.arima(dlrPFI, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, ic = "bic")
auto.arima(dlrPFI, stationary = TRUE, seasonal = FALSE, ic = "aic", trace = TRUE)
auto.arima(dlrPFI, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, ic = "aic", trace = TRUE)

m3 <- auto.arima(dlrPFI, stationary = TRUE, stepwise = FALSE, ic = "bic")
m3
ggtsdiag(m3, gof.lag = nlags)
m4 <- auto.arima(dlrPFI, stationary = TRUE, stepwise = FALSE, ic = "aic")
m4
ggtsdiag(m4, gof.lag = nlags)
# compute intercept phi_0
(1-sum(m1$coef[1:3]))*mean(dlrPFI)    
# compute standard error of residuals
sqrt(m1$sigma2)


#homework3-3
library(magrittr)
library(tidyverse)
library(tidyquant)
library(timetk)
library(ggfortify)
library(forecast)
library(egg)
library(readr)
# set default ggplot theme to theme_bw()
theme_set(theme_bw())
# get  the monthly industrial production index,
IP <- 
  tq_get("INDPRO", get = "economic.data", from = "1919-01-01", to = "2018-12-01")
str(IP)
glimpse(IP)
#log change, a stationary transformation
IP_tbl <-
  IP %>%
  rename(IP = price) %>%
  mutate(dlIP = log(IP) - lag(log(IP)))
# plot time series for the insutrial production index - using ggplot
g1<-ggplot(data=IP_tbl,aes(x=date, y= IP)) +geom_line() +labs(x = "", y = "", 
                                                                  title = "the insutrial production index (IP) ")+ 
  theme(strip.text = element_text(hjust = 0))
g2<-ggplot(data=IP_tbl,aes(x=date,y=dlIP))+geom_line()+labs(x="",y="",
                                                                title="the insutrial production index (dlIP)")+
  theme(strip.text = element_text(hjust = 0)) 
ggarrange(g1,g2,ncol=1)
#ACF and PACF plots  
dlIP <- IP_tbl %>%
  filter(!is.na(dlIP)) %>%
  tk_xts(date_var = date, select = dlIP)
str(dlIP)
# number of lags for ACF and PACF plots
nlags <- 24
gg1<-ggAcf(dlIP, lag.max = nlags)
gg2<-ggPacf(dlIP, lag.max = nlags)
ggarrange(gg1,gg2,ncol=1)
# estimate ARMA models
m1 <- Arima(dlrPFI, order = c(0, 0, 2))
m1
ggtsdiag(m1, gof.lag = nlags)

m2 <- Arima(dlrPFI, order = c(2, 0, 0))
m2
ggtsdiag(m2, gof.lag = nlags)

# z-statistics for coefficients of AR(2) model - phi1 is not signifficant at any level
z <- m2$coef/sqrt(diag(m2$var.coef))
# p values
2*(1-pnorm(abs(z))) %>% round(5)
# the model specification that minimizes AIC and the model specification that minimizes BIC
auto.arima(dlrPFI, stationary = TRUE, seasonal = FALSE, ic = "bic")
auto.arima(dlrPFI, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, ic = "bic")
auto.arima(dlrPFI, stationary = TRUE, seasonal = FALSE, ic = "aic", trace = TRUE)
auto.arima(dlrPFI, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, ic = "aic", trace = TRUE)

m3 <- auto.arima(dlIP, stationary = TRUE, stepwise = FALSE, ic = "bic")
m3
ggtsdiag(m3, gof.lag = nlags)
m4 <- auto.arima(dlIP, stationary = TRUE, stepwise = FALSE, ic = "aic")
m4
ggtsdiag(m4, gof.lag = nlags)
# compute intercept phi_0
(1-sum(m1$coef[1:3]))*mean(dlIP)    
# compute standard error of residuals
sqrt(m1$sigma2)