
# linear Gaussian state space model - local level model with seasonality

library(magrittr)
library(tibble)
library(readr)
library(tidyverse)
library(tidyr)
library(timetk)
library(KFAS)
library(zoo)

theme_set(theme_bw())

# import the data on earnings per share for Johnson and Johnson, this data can be downloaded from
#  http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/q-jnj.txt
jnj.tbl.wide <-
    read_table("http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/q-jnj.txt", col_names = "jnj") %>%
    ts(start = c(1960,1), frequency = 4) %>%
    tk_tbl(rename_index = "qtryear")

y.ts <- jnj.tbl.wide %>%
    tk_ts(select = jnj, start = c(1960,1), frequency = 4) %>%
    log()

# uncomment the following line to undertake the analysis with artificially introduced missing values
# y.ts[c(21:24,61:64,72,76,80)] <- NA

par(mfrow=c(2,1))
plot(y.ts)
plot(exp(y.ts))

# define a local level model with seasonal component
y.LLM <- SSModel(y.ts ~ SSMtrend(degree = 1, Q = NA) + SSMseasonal(period = 4, sea.type = "dummy", Q = NA), H = NA)

# check system matrices
y.LLM$T
y.LLM$R
y.LLM$Z
y.LLM$Q
y.LLM$H

y.LLM$P1
y.LLM$P1inf

# estimate the model parameters using maximum likelihood
y.LLM.ML <- fitSSM(y.LLM, inits = rep(log(var(y.ts)/100), 3), method = "Nelder-Mead")
y.LLM.ML$optim.out$par

y.LLM.ML$model$Q
y.LLM.ML$model$H

# Kalman filtering and smoothing
y.KFS <- KFS(y.LLM.ML$model, smoothing = c("state", "disturbance"))
colnames(y.KFS$alphahat)

# construct smoothed time series - add smmothed level and smoothed seasonal components
y.KF <- y.KFS$a %*% as.vector(y.KFS$model$Z)
y.KS <- y.KFS$alphahat %*% as.vector(y.KFS$model$Z)


?predict.SSModel

y.KF <- predict(y.LLM.ML$model, states = c("level","seasonal"), filtered = TRUE)
y.KS <- predict(y.LLM.ML$model, states = c("level","seasonal"), filtered = FALSE)

# construct 90% confidence intervals for filtered state
y.KF.lvl <- predict(y.LLM.ML$model,states = "level", interval = "confidence", level = 0.9, filtered = TRUE)
y.KF.sea <- predict(y.LLM.ML$model,states = "seasonal", interval = "confidence", level = 0.9, filtered = TRUE)

# construct 90% confidence intervals for smoothed state
y.KS.lvl <- predict(y.LLM.ML$model,states = "level", interval = "confidence", level = 0.9, filtered = FALSE)
y.KS.sea <- predict(y.LLM.ML$model,states = "seasonal", interval = "confidence", level = 0.9, filtered = FALSE)


# log transformed earnigns per share - actual, filtered and seasonal component
par(mfrow=c(2,1))
cbind(y.ts, y.KF.lvl) %>%
    plot.ts(plot.type = "single", col = c("black","blue","blue","blue"), lty = c(1,1,3,3), 
            xlab = "", ylab = "", main = "actual and filtered values")
y.KF.sea %>%
    plot.ts(plot.type = "single", col = "blue", lty = c(1,3,3), xlab = "", ylab = "", main = "seasonal component")
abline(h=0, lty=3)


# log transformed earnigns per share - actual, smoothed and seasonal component
par(mfrow=c(2,1))
cbind(y.ts, y.KS.lvl) %>%
    plot.ts(plot.type = "single", col = c("black","red","red","red"), lty=c(1,1,3,3), 
            xlab = "", ylab = "", main = "actual and smoothed values" )
y.KS.sea %>%
    plot.ts(plot.type = "single", col = "red", lty = c(1,3,3), xlab = "", ylab = "", main = "seasonal component" )
abline(h=0, lty=3)

# earnigns per share - actual, smoothed and seasonal component
par(mfrow=c(3,1))
cbind(y.ts, y.KS.lvl) %>% exp() %>%
    plot.ts(col = c("black","red","red","red"), lty=c(1,1,3,3), plot.type = "single", 
            xlab = "", ylab = "", main = "actual and smoothed values")
y.KS.sea %>% exp() %>%
    plot.ts(plot.type = "single", col = "red", lty=c(1,3,3), xlab = "", ylab = "", main = "seasonal component")
abline(h=1, lty=3)
y.KFS$epshat %>% exp() %>%
    plot.ts(plot.type = "single", col = "red", xlab = "", ylab = "", main = "irregular component")
abline(h=1, lty=3)


# note that the smoothed innovations in state equation for level are not zero mean but rather positive
# local level model is ill-suited for time series that shows upward trend
par(mfrow=c(3,1))
y.KFS$epshat %>%
    plot.ts(plot.type = "single", col = "red", xlab = "", ylab = "", main = "smoothed innovations in observation equation")
abline(h = 0, lty = 3)
y.KFS$etahat[,1] %>%
    plot.ts(plot.type = "single", col = "red", xlab = "", ylab = "", main = "smoothed innovations in state equation - level")
abline(h = 0, lty = 3)
y.KFS$etahat[,2] %>%
    plot.ts(plot.type = "single", col = "red", xlab = "", ylab = "", main = "smoothed innovations in state equation - slope")
abline(h = 0, lty = 3)

mean(y.KFS$epshat)
mean(y.KFS$etahat[,1])
mean(y.KFS$etahat[,2])


# forecast horizon
h <- 16
y.fcst <- predict(y.LLM.ML$model, interval = "confidence", level = 0.9, n.ahead = h)

# plot the forecast
par(mfcol = c(2,1), cex=0.9)
cols <- c(1,2,2,2)
lwds <- c(2,2,1,1)
ltys <- c(1,1,2,2)
cbind(y.ts, y.fcst) %>%
    plot.ts(plot.type = "single", col = cols, lwd = lwds, lty = ltys, 
            xlab = "", ylab = "", main = "Johnson and Johnson: log transformed earnings per share")
legend("topleft", legend = c("actual data","forecast","90% confidence interval"), 
       col = cols, lwd = lwds, lty = ltys, bty = "n", cex = 0.8)

cbind(y.ts, y.fcst) %>% exp() %>%
    plot.ts(plot.type = "single", col = cols, lwd = lwds, lty = ltys, 
            xlab = "", ylab = "", main = "Johnson and Johnson: earnings per share")
legend("topleft", legend = c("actual data","forecast","90% confidence interval"), 
       col = cols, lwd = lwds, lty = ltys, bty = "n", cex = 0.8)
