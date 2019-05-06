
# linear Gaussian state space model - local level model with seasonality and exogenous variables

# UK car accidents data
data("Seatbelts")
help("Seatbelts")

# drivers killed or critically injured
plot( log(Seatbelts[,"drivers"]) )

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
updatefn_KSI <- function(pars,model,...){
    model$H[] <- exp(pars[1])
    diag(model$Q[,,1])<- exp(c(pars[2:3]))
    model
}

# estimate model parameters using maximum likelihhod
y_ML <- fitSSM(inits = log( rep(var(log(Seatbelts[,'drivers']))/10, 3) ),
              model = y_SSM,
              updatefn = updatefn_KSI,
              method = "Nelder-Mead")
str(y_ML$model)

# estimated parameters - variance of innovations in measurement equation, state level equation, seasonal component equation
res <- data.frame(sigma2.measurement = y_ML$model$H[,,1],
                  sigma2.level = diag(y_ML$model$Q[,,1])[1],
                  sigma2.seasonality = diag(y_ML$model$Q[,,1])[2])
res

y_ML$model$Q
y_ML$model$H

# Kalman filtering and smoothing
y_KFS <- KFS(y_ML$model, filtering = c("state"), smoothing = c("state", "disturbance", "mean"))
y_KFS

# smoothed state
str(y_KFS$alphahat)
dimnames(y_KFS$alphahat)

# number of observations
T <- dim(Seatbelts)[1]

# estimated parameters for two explanatory variables
y_KFS$alphahat[T,1]
y_KFS$alphahat[T,2]


# actual values
y <- log(Seatbelts[,"drivers"])

# extract smoothed level, seasonal, and irregular components
y_lvl_KS <- y_KFS$alphahat[,1]*log(as.data.frame(Seatbelts)$PetrolPrice) + y_KFS$alphahat[,2]*(as.data.frame(Seatbelts)$law) + y_KFS$alphahat[,"level"]
y_sea_KS  <- y_KFS$alphahat[,"sea_dummy1"]
y_eps_KS  <- y_KFS$epshat

# smoothed data combinining level, seasonal, and effects of exogenous variables
y.KS_all <- rep(NA, T)
for (t in 1:T) { y.KS_all[t] <- y_KFS$alphahat[t,] %*% y_KFS$model$Z[,,t] }
# can be also obtained using predict function with missing n.ahead option
y.KS_all.CI <- predict(y_KFS$model, interval = "confidence", level = 0.9)

# check to see that the above two methods yield same results
cbind(y.KS_all, y.KS_all.CI)

par(mfrow = c(3,1), cex = 0.7)

# actual data and smoothed state component
cbind(y, y_lvl_KS) %>% ts(start = 1969, frequency = 12 ) %>%
    plot.ts(plot.type = "single", xlab = "", ylab = "log KSI", col = c("darkgrey","red"), lwd = c(1,2))
abline(v = 1969:2003, lty = "dotted", col = "lightgrey")
legend("topright", c("actual data","Kalman smoothed level"), col = c("darkgrey","red"), lwd = c(1,2), bty = "n")

# smoothed seasonal component
y_sea_KS %>% ts(start = 1969, frequency = 12) %>%
    plot(xlab = "", ylab = "log KSI", col = "red", lwd = 2)
abline(h = 0, col = "grey")
abline(v = 1969:2003, lty = "dotted", col = "lightgrey")
legend("topright", "Kalman smoothed seasonal component", col = "red", lwd = 2, bty = "n")

# smoothed irregular component
y_eps_KS %>% ts(start = 1969, frequency = 12) %>%
    plot(xlab = "", ylab = "log KSI", col = "red", lwd = 2)
abline(h = 0, col = "grey")
abline(v = 1969:2003, lty = "dotted", col = "lightgrey")
legend("topright", "irregular component", col = "red", lwd = 2, bty = "n")

