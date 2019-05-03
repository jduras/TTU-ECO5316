
# linear Gaussian state space model - local linear trend model with seasonality

library(magrittr)
library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(timetk)
library(tibbletime)
library(lubridate)
library(KFAS)
library(zoo)
library(ggplot2)

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

# define a local linear trend model with seasonal component
y.LLT <- SSModel(y.ts ~ SSMtrend(degree = 2, Q = list(NA, NA)) + SSMseasonal(period = 4, sea.type = "dummy", Q = NA), H = NA)

# check system matrices
y.LLT$T
y.LLT$R
y.LLT$Z
y.LLT$Q
y.LLT$H

y.LLT$P1
y.LLT$P1inf

# estimate the model parameters using maximum likelihood
y.LLT.ML <- fitSSM(y.LLT, inits = rep(log(var(y.ts)/100), 4), method = "Nelder-Mead")
y.LLT.ML$optim.out$par

y.LLT.ML$model$Q
y.LLT.ML$model$H

# Kalman filtering and smoothing
y.KFS <- KFS(y.LLT.ML$model, smoothing = c("state","disturbance"))
colnames(y.KFS$alphahat)

# construct smoothed time series - add smmothed level and smoothed seasonal components
y.KF <- y.KFS$a %*% as.vector(y.KFS$model$Z)
y.KS <- y.KFS$alphahat %*% as.vector(y.KFS$model$Z)


?predict.SSModel

y.KF <- predict(y.LLT.ML$model, states = c("level","seasonal"), filtered = TRUE)
y.KS <- predict(y.LLT.ML$model, states = c("level","seasonal"), filtered = FALSE)

# construct 90% confidence intervals for filtered state
y.KF.lvl <- predict(y.LLT.ML$model, states = "level", interval = "confidence", level = 0.9, filtered = TRUE)
y.KF.sea <- predict(y.LLT.ML$model, states = "seasonal", interval = "confidence", level = 0.9, filtered = TRUE)

# construct 90% confidence intervals for smoothed state
y.KS.lvl <- predict(y.LLT.ML$model, states = "level", interval = "confidence", level = 0.9, filtered = FALSE)
y.KS.sea <- predict(y.LLT.ML$model, states = "seasonal", interval = "confidence", level = 0.9, filtered = FALSE)


# log transformed earnigns per share - actual, filtered and seasonal component
par(mfrow = c(2,1))
cbind(y.ts, y.KF.lvl) %>%
    plot.ts(plot.type = "single", col = c("black","blue","blue","blue"), lty = c(1,1,3,3), 
            xlab = "", ylab = "", main = "actual and filtered values")
y.KF.sea %>%
    plot.ts(plot.type = "single", col = "blue", lty = c(1,3,3), xlab = "", ylab = "", main = "seasonal component")
abline(h = 0, lty = 3)


# log transformed earnigns per share - actual, smoothed and seasonal component
par(mfrow = c(2,1))
cbind(y.ts, y.KS.lvl) %>%
    plot.ts(plot.type = "single", col = c("black","red","red","red"), lty=c(1,1,3,3), 
            xlab = "", ylab = "", main = "actual and smoothed values" )
y.KS.sea %>%
    plot.ts(plot.type = "single", col = "red", lty = c(1,3,3), xlab = "", ylab = "", main = "seasonal component" )
abline(h = 0, lty = 3)

# earnigns per share - actual, smoothed and seasonal component
par(mfrow = c(3,1))
cbind(y.ts, y.KS.lvl) %>% exp() %>%
    plot.ts(col = c("black","red","red","red"), lty=c(1,1,3,3), plot.type = "single", 
            xlab = "", ylab = "", main = "actual and smoothed values")
y.KS.sea %>% exp() %>%
    plot.ts(plot.type = "single", col = "red", lty=c(1,3,3), xlab = "", ylab = "", main = "seasonal component")
abline(h = 1, lty = 3)
y.KFS$epshat %>% exp() %>%
    plot.ts(plot.type = "single", col = "red", xlab = "", ylab = "", main = "irregular component")
abline(h = 1, lty = 3)


# smoothed innovations
par(mfrow = c(4,1))
y.KFS$epshat %>%
    plot.ts(plot.type = "single", col = "red", xlab = "", ylab = "", main = "smoothed innovations in observation equation")
abline(h = 0, lty = 3)
y.KFS$etahat[,1] %>%
    plot.ts(plot.type = "single", col = "red", xlab = "", ylab = "", main = "smoothed innovations in state equation - level")
abline(h = 0, lty = 3)
y.KFS$etahat[,2] %>%
    plot.ts(plot.type = "single", col = "red", xlab = "", ylab = "", main = "smoothed innovations in state equation - slope")
abline(h = 0, lty = 3)
y.KFS$etahat[,3] %>%
    plot.ts(plot.type = "single", col = "red", xlab = "", ylab = "", main = "smoothed innovations in state equation - seasonal")
abline(h = 0, lty = 3)

mean(y.KFS$epshat)
mean(y.KFS$etahat[,1])
mean(y.KFS$etahat[,2])
mean(y.KFS$etahat[,3])

# forecast horizon
h <- 16
y.fcst <- predict(y.LLT.ML$model, interval = "confidence", level = 0.9, n.ahead = h)

# plot the forecast
par(mfcol = c(2,1), cex=0.9)
cols <- c(1,2,2,2)
lwds <- c(2,2,1,1)
ltys <- c(1,1,2,2)
cbind(y.ts, y.fcst) %>%
    plot.ts(plot.type = "single", col = cols, lwd = lwds, lty = ltys, xlab = "", ylab = "", main = "Johnson and Johnson: log transformed earnings per share")
legend("topleft", legend = c("actual data","forecast","90% confidence interval"), col = cols, lwd = lwds, lty = ltys, bty = "n", cex = 0.8)

cbind(y.ts, y.fcst) %>% exp() %>%
    plot.ts(plot.type = "single", col = cols, lwd = lwds, lty = ltys, xlab = "", ylab = "", main = "Johnson and Johnson: earnings per share")
legend("topleft", legend = c("actual data","forecast","90% confidence interval"), col = cols, lwd = lwds, lty = ltys, bty = "n", cex = 0.8)




# note: under the hood, the above forecast is obtained by extending existing series by adding NAs and re-running Kalman filter/smoother on extended series
y.ext <- window(y.ts, end= i ndex(y)[length(y)]+h/4, extend=TRUE)
y.LLT <- SSModel(y.ext ~ SSMtrend(degree=1, Q=y.LLT.ML$model$Q[1,1,1]) + SSMseasonal(period=4, sea.type="dummy", Q=y.LLT.ML$model$Q[2,2,1]), H=y.LLT.ML$model$H[1,1,1])
y.LLT <- SSModel(y.ext ~ SSMtrend(degree=2, Q=list(y.LLT.ML$model$Q[1,1,1],y.LLT.ML$model$Q[2,2,1])) + SSMseasonal(period=4, sea.type="dummy", Q=y.LLT.ML$model$Q[3,3,1]), H=y.LLT.ML$model$H[1,1,1])
y.KFS <- KFS(y.LLT, filtering='state', smoothing=c('state','signal'))
# extract the forecast
y.KS <- y.KFS$alphahat %*% as.vector(y.KFS$model$Z)
y.KS <- ts(y.KS, start=(index(y)[1]), frequency=4)
# plot the forecast
par(mfcol = c(1,1), cex=0.9)
plot(cbind(y, y.KS), col = c(1,2), lty=c(1,3), plot.type = "single")
legend("topleft", legend = c("actual data","forecast","90% confidence interval"),
       col = cols, lwd=lwds, lty=ltys, bty="n", cex=0.8 )

# compare the forecast from predict function with the one created manually
cbind(y.ts, y.KS, y.fcst)






#### Rolling Estimation and Forecast ###

# define function for rolling local linear trend model with seasonal component
window.length = 4*15
roll_LLT <-
    rollify(
        function(y) {
            SSModel(y ~ SSMtrend(degree = 2, Q = list(NA, NA)) + SSMseasonal(period = 4, sea.type = "dummy", Q = NA), H = NA)
        },
        window = window.length, unlist = FALSE)

# create 1 period ahead rolling forecasts
results <-
    jnj.tbl.wide %>%
    mutate(date = as.Date(qtryear)) %>%
    tbl_time(index = date) %>%
    mutate(LLT = roll_LLT(log(jnj))) %>%
    filter(!is.na(LLT)) %>%
    mutate(LLT.ML = map(LLT, . %>% fitSSM(inits = rep(log(var(jnj)/100), 4), method = "Nelder-Mead")),
           LLT.ML.f = map(LLT.ML, . %$% model %>%
                              predict(states = c("level","seasonal"), n.ahead = 1, interval = "confidence", level = 0.9) %>%
                              tidy() %>%
                              as.tibble())
    )
results

# extract 1 period ahead rolling forecasts, combine with actual data
tbl.f.1.rol <-
    bind_rows(
        # actual data
        jnj.tbl.wide %>%
            mutate(date = as.Date(qtryear),
                   key = "actual",
                   value = log(jnj)) %>%
            select(date, key, value),
        # forecasts
        results %>%
            select(qtryear, LLT.ML.f) %>%
            unnest() %>%
            rename(value = fit) %>%
            mutate(date = as.Date(qtryear) %m+% months(3),
                   key = "forecast") %>%
            # mutate_at(vars(value, lwr, upr), funs(exp)) %>%
            select(-qtryear)
    ) %>%
    arrange(date)

# plot the 1 period ahead rolling forecasts
tbl.f.1.rol %>%
    ggplot(aes(x = date, y = value, col = key, group = key)) +
    geom_line(size = 0.7) +
        geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "steelblue", col = NA, alpha = 0.2) +
        scale_color_manual(values = c("black","blue")) +
        labs(x = "", y = "", title = "Log of Earnings per share for Johnson and Johnson: Rolling one step ahead forecast",
             subtitle = paste(window.length, "quarters rolling window local linear trend model with seasonal component")) +
        theme(legend.position = "none")
