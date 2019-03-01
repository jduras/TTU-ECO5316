
# set working directory
# setwd("C:/User/Courses/E5316/Codes")

# import the data on the growth rate of GDP, convert it into time series xts object
#  this data can be downloaded from here
#  http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/q-gnp4791.txt

library(magrittr)
library(forecast)
library(ggplot2)
library(ggfortify)
library(scales)

y <- scan(file = "http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/q-gnp4791.txt") %>%
    ts(start = c(1947, 2), frequency = 4)

str(y)
head(y)
tail(y)

# set default ggplot theme to theme_bw(), and store the previous one in theme_old
# theme_old <- theme_set(theme_bw())
theme_old <- theme_set(theme_minimal())

# plot y using ggplot2 package
autoplot(y)

# customize the plot
autoplot(y) +
    geom_hline(yintercept = 0, color = "gray60") +
    labs(x = "", y = "", title = "Real GNP growth rate") +
    scale_y_continuous(labels = percent_format(accuracy = 1))


# plot ACF and PACF for y up to lag nlags
nlags <- 20
ggAcf(y, lag.max = nlags) + labs(title="Real GNP growth rate")
ggPacf(y, lag.max = nlags) + labs(title="Real GNP growth rate")


# AICc is AIC with a correction for finite sample sizes; for a  univariate linear model with normal residuals
# AICc = AIC + 2(g+1)(g+2)/(T-g-2)


# estimate an AR(1) model - there is only one significant coefficient in the PACF plot for y
m1 <- Arima(y, order = c(1,0,0))
m1
# diagnostics for the AR(1) model using ggtsdiag from ggfortify package -  note that there seems to be a problem with remaining serial correlation at lag 2
ggtsdiag(m1, gof.lag = nlags)


# estimate an AR(2) model to deal with the problem of remaining serial correlation at lag 2
m2 <- Arima(y, order = c(2,0,0))
m2
# diagnostics for the AR(2) model - note that the problem with remaining serial correlation at lag 2 is gone
ggtsdiag(m2, gof.lag = nlags)


# estimate an AR(3) model since PACF for lag 2 and 3 are comparable in size
m3 <- Arima(y, order = c(3,0,0))
m3
# diagnostics for the AR(3) model
ggtsdiag(m3, gof.lag = nlags)


# show the structure of object m3
str(m3)

# z-statistics for coefficients of AR(3) model - phi2 is signifficant at 5% level, phi3 is marginally insignifficant
m3$coef / sqrt(diag(m3$var.coef))
coef(m3) / sqrt(diag(vcov(m3)))

# p values
z <- coef(m3) / sqrt(diag(vcov(m3)))
2*(1-pnorm(abs(z))) %>% round(4)


# Ljung-Box test - for residuals of a model adjust the degrees of freedom m by subtracting the number of parameters g
# this adjustment will not make a very big difference if m is large but matters if m is small

Box.test(m2$residuals, lag = 8, type = "Ljung")
Box.test(m2$residuals, lag = 8, type = "Ljung", fitdf = length(m2$coef))

Box.test(m2$residuals, lag = 12, type = "Ljung")
Box.test(m2$residuals, lag = 12, type = "Ljung", fitdf = length(m2$coef))

Box.test(m2$residuals, lag = 48, type = "Ljung")
Box.test(m2$residuals, lag = 48, type = "Ljung", fitdf = length(m2$coef))
