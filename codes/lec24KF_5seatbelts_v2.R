
# linear Gaussian state space model - local level model with seasonality and exogenous variables

library(tidyverse)
library(zoo)
library(lubridate)
library(ggfortify)
library(egg)
library(KFAS)
library(timetk)
	  
# set default ggplot theme
theme_set(theme_minimal() +
          theme(strip.text.x = element_text(hjust = 0),
                strip.text.y = element_text(hjust = 1),
                panel.grid = element_blank(),
                axis.ticks = element_blank(),
                strip.background = element_blank()))

# UK car accidents data
data("Seatbelts")
?Seatbelts

y_tbl <- Seatbelts %>%
    tk_tbl(rename_index = "yearm") %>%
    gather(measure, value, -yearm)
           
y_tbl %>%
    ggplot(aes(x = yearm, y = value)) +
        geom_line() +
        scale_x_yearmon() +
        facet_wrap(~measure, scales = "free")

y_tbl %>%
    filter(measure %in% c("drivers", "front", "rear")) %>%
    ggplot(aes(x = yearm, y = value, col = measure)) +
        geom_line() +
        scale_x_yearmon()

# drivers killed or seriously injured
y <- log(Seatbelts[,"drivers"])

autoplot(y)

# define state-space model - local level with seasonality and two variables
y_SSM <- SSModel(log(drivers) ~ SSMtrend(degree = 1, Q = list(NA))
                                  + SSMseasonal(period = 12, sea.type = "dummy", Q = NA)
                                  + log(PetrolPrice)
                                  + law,
                 data = Seatbelts, H = NA)
y_SSM

# examine state
y_SSM$a
# examine system matrices
y_SSM$T
y_SSM$R
y_SSM$Q
y_SSM$Z
y_SSM$H

# define update function for maximum likelihhod estimation
updatefn_KSI <- function(pars, model, ...){
    model$H[] <- exp(pars[1])
    diag(model$Q[, , 1])<- exp(c(pars[2:3]))
    model
}

# estimate model parameters using maximum likelihood
y_ML <- fitSSM(inits = log( rep(var(log(Seatbelts[,'drivers']))/10, 3) ),
               model = y_SSM,
               # updatefn = updatefn_KSI,
               method = "Nelder-Mead")
str(y_ML$model)

# estimated parameters - variance of innovations in measurement equation, state level equation, seasonal component equation
res <- data.frame(sigma2_measurement = y_ML$model$H[, , 1],
                  sigma2_level = diag(y_ML$model$Q[, , 1])[1],
                  sigma2_seasonality = diag(y_ML$model$Q[, , 1])[2])
res

y_ML$model$Q
y_ML$model$H

# Kalman filtering and smoothing
y_KFS <- KFS(y_ML$model, filtering = "state", smoothing = c("state", "disturbance", "mean"))
y_KFS

# smoothed state
str(y_KFS$alphahat)
dimnames(y_KFS$alphahat)

# estimated parameters for two explanatory variables
y_KFS$alphahat[, c("log(PetrolPrice)", "law")] 

# extract smoothed level
y_lvl_KS <- y_KFS$alphahat[, "level"] + 
    y_KFS$alphahat[, "log(PetrolPrice)"]* log(Seatbelts[, "PetrolPrice"]) +
    y_KFS$alphahat[, "law"]* Seatbelts[, "law"]

# smoothed y combining level, seasonal, and effects of exogenous variables can be calculated as
#  y_KS_all[t] <- y_KFS$alphahat[t,] %*% y_KFS$model$Z[,,t]
# and can be obtained using predict function with missing n.ahead option
y_all_KS <- predict(y_KFS$model, interval = "confidence", level = 0.9)

# actual data and smoothed state component
g1 <- cbind(y, y_lvl_KS) %>%
    exp() %>%
    autoplot(size = 1, facets = FALSE) +
    geom_vline(xintercept = seq(ymd('1969-01-01'), ymd("1985-01-01"), by = "years"), linetype = "dotted", col = "darkgray", size = 0.75) +
    scale_color_manual(values = c("darkgray", "red"), labels = c("actual data", "smoothed level")) + 
    labs(title = "U.K. drivers killed or seriously injured", subtitle = "smoothed level", y = "", col = "") +
    theme(legend.position = c(0.89, 0.98),
          legend.justification = c("left", "top"))

# smoothed seasonal component    
g2 <- y_KFS$alphahat[,"sea_dummy1"] %>%
    exp() %>%
    autoplot(size = 1, colour = "red") +
    geom_vline(xintercept = seq(ymd('1969-01-01'), ymd("1985-01-01"), by = "years"), linetype = "dotted", col = "darkgray", size = 0.75) +
    geom_hline(yintercept = 1, col = "grey") +
    labs(subtitle = "smoothed seasonal component")

# smoothed irregular component
g3 <- y_KFS$epshat %>%
    exp() %>%
    autoplot(size = 1, colour = "red") +
    geom_vline(xintercept = seq(ymd('1969-01-01'), ymd("1985-01-01"), by = "years"), linetype = "dotted", col = "darkgray", size = 0.75) +
    geom_hline(yintercept = 1, col = "grey") +
    labs(subtitle = "smoothed irregular component") 

ggarrange(g1, g2, g3, ncol = 1)
