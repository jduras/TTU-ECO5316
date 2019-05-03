
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(timetk)
library(KFAS)
library(ggplot2)
library(ggfortify)
library(plotly)

theme_set(theme_bw())

# linear Gaussian state space model - local level model for annual flow of river Nile at Ashwan, from 1871 to 1970

# annual flow of the Nile from 1871-1970
data(Nile)
help(Nile)
plot(Nile)

y_ts <- datasets::Nile

# uncomment the following two line to see the analysis with missing values
# y.ts[21:50] <- NA
# y.ts[71:80] <- NA

plot(y_ts)

# define the state-space local level model
y_LLM <- SSModel(y_ts ~ SSMtrend(degree = 1, Q = list(NA)), H = NA)
str(y_LLM)

# check state variable
y_LLM$a
# check system matrices
y_LLM$Z
y_LLM$T
y_LLM$R
y_LLM$H
y_LLM$Q


# initials values for paramaters of Q and H used by numerical procedure below to maximize loglikelihood
initvals <- list(rep(1, 2),
                 rep(0.9, 2),
                 rep(1.1, 2),
                 rep(var(y_ts, na.rm = TRUE), 2)/10000)

# update function which updates model matrices H and Q given new parameters
updatefun <- function(pars, model, ...){
    model$Q[] <- exp(pars[1])
    model$H[] <- exp(pars[2])
    model
}

# method for finding the maximum loglikelihood (or more precisely minimum of -loglikelihood)
#  Nelder-Mead uses only function values and is relatively slow but robust
#  BFGS is a quasi-Newton method, uses function values and gradients, it is faster than Nelder-Mead but does not always converge to a global mimimum
#  CG is a conjugate gradients method which is generally more fragile than BFGS but can be more successful in much larger optimization problems
methods <- list("BFGS", "Nelder-Mead", "CG")

# maximum likelihood estimation of paramaters of Q and H (i.e. variance of the two inovations)
y_LLM_ML <- fitSSM(y_LLM, inits = log(rep(1.1, 2)), method = "BFGS")
y_LLM_ML$optim.out
exp(y_LLM_ML$optim.out$par)
y_LLM_ML$model$Q
y_LLM_ML$model$H

y_LLM_ML <- fitSSM(y_LLM, inits = log(rep(1.1, 2)), updatefn = updatefun, method = "BFGS")
y_LLM_ML$optim.out
exp(y_LLM_ML$optim.out$par)
y_LLM_ML$model$Q
y_LLM_ML$model$H

# try different starting points and methods
# results: column LL contains the value of likelihood function that's being maximized
safe_fitSSM <- possibly(fitSSM, otherwise = NULL)

options(pillar.sigfig = 8)

crossing(methods, initvals) %>%
    mutate(y_LLM_ML = map2(methods, initvals, ~safe_fitSSM(y_LLM, inits = .y, method = .x))) %>%
    unnest(methods) %>%
    mutate(out = map(y_LLM_ML, ~pluck(.x, "optim.out")),
           inits1 = map_dbl(initvals, pluck(1)),
           inits2 = map_dbl(initvals, pluck(2)),
           LL = -map_dbl(out, ~pluck(.x, "value", .default = NA_real_)),
           par1 = map_dbl(out, ~pluck(.x, "par", 1, .default = NA_real_)),
           par2 = map_dbl(out, ~pluck(.x, "par", 2, .default = NA_real_)),
           par1.exp = exp(par1),
           par2.exp = exp(par2)) %>%
    arrange(desc(LL)) %>%
    select(methods, inits1, inits2, par1, par2, par1.exp, par2.exp, LL)

# run Kalman Filter and Smoother with estimated parameters
y_KFS <- KFS(y_LLM_ML$model)
str(y_KFS)
names(y_KFS)

?predict.SSModel

# construct 90% confidence intervals for filtered state - shorter way, using predict function with missing n.ahead option
y_KF <- predict(y_LLM_ML$model, interval = "confidence", level = 0.9, filtered = TRUE)
# construct 90% confidence intervals for smoothed state
y_KS <- predict(y_LLM_ML$model, interval = "confidence", level = 0.9)

str(y_KS)

# construct 90% confidence intervals for filtered state - longer, do it yourself way
y_KF_2 <- as.vector(y_KFS$a) + sqrt(cbind(y_KFS$P)) %*% as.vector( qnorm(0.95)*c(-1,1) )
# construct 90% confidence intervals for smoothed state
y_KS_2 <- as.vector(y_KFS$alphahat) + sqrt(cbind(y_KFS$V)) %*% as.vector( qnorm(0.95)*c(-1,1) )

# replace the filtered state for first period by NA
y_KF[1,] <- NA

# compare confidence intervals
cbind(y_KF, y_KF_2[-(T+1),]) %>% head()
cbind(y_KS, y_KS_2) %>% head()



par(mfrow = c(2,2), cex = 0.6)
# plot filtered state
plot.ts( cbind(y_ts, y_KF, Nile), plot.type = "single",
         col = c(1,4,4,4,1), lwd = c(2,2,2,2,1), lty = c(1,1,2,2,3), xlab = "", ylab = "", main = "")
abline(v = 1898, lty = 3)
legend("topright", legend = c("data","filtered state","90% confidence interval"),
       col = c("black","blue","blue"), lty = c(1,1,2), lwd = c(1,1,1), bty = "n", cex = 0.9 )
plot( ts( c(y_KFS$v[-1]), start = 1871), col = "blue", lwd = 2, xlab = "", ylab = "", main = "forecast error")
abline(h = 0, lty = 3)
plot( ts( c(y_KFS$P)[-1], start = 1871), col = "blue", lwd = 2, xlab = "", ylab = "", main = "variance of state")
plot( ts( c(y_KFS$F)[-1], start = 1871), col = "blue", lwd = 2, xlab = "", ylab = "", main = "variance of forecast error")


par(mfrow = c(1,2), mar = c(3,3,2,1), cex = 0.8)
# filtered
cbind(y_ts, y_KF, Nile) %>%
    plot.ts(plot.type = "single", col = c(1,4,4,4,1), lwd = c(2,2,2,2,1), lty = c(1,1,2,2,3), xlab = "", ylab = "", main="")
abline(v = 1898, lty = 3)
legend("topright", legend=c("data","filtered","90% confidence interval"), col = c(1,4,4), lty = c(1,1,2), lwd = 2, bty = "n", cex = 0.9 )
# smoothed
cbind(y_ts, y_KS, Nile) %>%
    plot.ts(plot.type = "single", col = c(1,2,2,2,1), lwd = c(2,2,2,2,1), lty = c(1,1,2,2,3), xlab = "", ylab = "", main = "")
abline(v = 1898, lty = 3)
legend("topright", legend = c("data","smoothed","90% confidence interval"), col = c(1,2,2), lty = c(1,1,2), lwd = 2, bty = "n", cex = 0.9 )





# plot smoothed state using ggplot with ggfortify
autoplot(y_KFS) +
    labs(x = "", y = "",
         title = "Annual flow of river Nile at Ashwan",
         subtitle = "Actual series vs Kalman smoothed series with 90% confidence interval")

# plot smoothed state with confidence interval using ggplot
cbind(y_ts, y_KS) %>%
    tk_tbl(rename_index = "year") %>%
    gather(variable, value, -year) %>%
    mutate(linetypes = case_when(variable == "y.ts" ~ "solid",
                                 variable == "y.KS.fit" ~ "solid",
                                 variable == "y.KS.lwr" ~ "dashed",
                                 variable == "y.KS.upr" ~ "dashed")) %>%
    ggplot(aes(x = year, y = value, group = variable, col = variable, linetype = linetypes)) +
        geom_line() +
        geom_vline(xintercept = 1898, linetype = "dashed") +
        annotate("text", x = 1896, y = 600, angle = 90, label = "Ashwan dam built") +
        scale_color_manual(values = c("red","red","red","black")) +
        scale_linetype_identity() +
        labs(x = "", y = "",
             title = "Annual flow of river Nile at Ashwan",
             subtitle = "Actual series vs Kalman smoothed series with 90% confidence interval") +
        theme(legend.position = "none")

# plot filtered and smoothed state with confidence interval using ggplot
g <- cbind(y_KS_actual = y_ts, y_KS, y_KF_actual = y_ts, y_KF) %>%
    tk_tbl(rename_index = "year") %>%
    gather(variable, value, -year) %>%
    separate(variable, into = c("y","method","key")) %>%
    mutate(linetypes = if_else(key %in% c("lwr","upr"), "dashed", "solid"),
           colors = case_when(key == "actual" ~ "black",
                              method == "KS" ~ "red",
                              method == "KF" ~ "blue"),
           method.label = case_when(method == "KS" ~ "smoothed",
                                    method == "KF" ~ "filtered")) %>%
    ggplot(aes(x = year, y = value, group = key, col = colors, linetype = linetypes)) +
        geom_line() +
        geom_vline(xintercept = 1898, linetype = "dashed") +
        annotate("text", x = 1896, y = 600, angle = 90, label = "Ashwan dam built") +
        scale_color_identity() +
        scale_linetype_identity() +
        labs(x = "", y = "",
             title = "Annual flow of river Nile at Ashwan",
             subtitle = "Actual series vs Kalman filtered and smoothed series with 90% confidence interval") +
        facet_wrap(~method.label)
g
ggplotly(g)

# plot filtered and smoothed state with confidence interval using ggplot
g <- cbind(y_KS_actual = y_ts, y_KS, y_KF_actual = y_ts, y_KF) %>%
    tk_tbl(rename_index = "year") %>%
    gather(variable, value, -year) %>%
    separate(variable, into = c("y","method","key")) %>%
    mutate(component = case_when(key == "actual"                          ~ "data",
                                 method == "KS" & key == "fit"            ~ "smoothed",
                                 method == "KF" & key == "fit"            ~ "filtered",
                                 method == "KS" & key %in% c("lwr","upr") ~ "smoothed, 90 % confidence interval",
                                 method == "KF" & key %in% c("lwr","upr") ~ "filtered, 90 % confidence interval"),
           method.label = case_when(method == "KS" ~ "smoothed",
                                    method == "KF" ~ "filtered")) %>%
    ggplot(aes(x = year, y = value, group = key, col = component, linetype = component)) +
        geom_line() +
        geom_vline(xintercept = 1898, linetype = "dashed") +
        annotate("text", x = 1900, y = 430, hjust = 0, angle = 0, label = "Ashwan dam built") +
        scale_color_manual(values = c("black","blue","blue","red","red")) +
        scale_linetype_manual(values = c("solid","solid","dotted","solid","dotted")) +
        labs(x = "", y = "",
             title = "Annual flow of river Nile at Ashwan",
             subtitle = "Actual series vs Kalman filtered and smoothed series with 90% confidence interval",
             col = "", linetype = "") +
        facet_wrap(~method.label)
g

ggplotly(g)
