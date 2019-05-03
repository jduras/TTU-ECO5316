
library(readr)
library(magrittr)
library(tidyverse)
library(timetk)
library(KFAS)
library(ggfortify)


# linear Gaussian state space model - capital asset price model with time varying coefficients

# load data on excess returns from January 1990 to December 2003
er_tbl <- read_delim("http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/m-excess-c10sp-9003.txt", delim = " ")

# excess returns for General Motors and for S&P 500
er_ts <-
    er_tbl %>%
    select(SP5, GM) %>%
    rename(SP500 = SP5) %>%
    ts(start = 1990, frequency = 12)

er_ts %>% autoplot()

# estimate a simple capital asset pricing model (CAPM) regression
#  r = alpha + betta*rM + eps
GM_CAPM_OLS <- lm(GM ~ SP500, data = as.data.frame(er_ts))
summary(GM_CAPM_OLS)

er_ts %>% as.data.frame() %>% plot()
abline(GM_CAPM_OLS)



# get number of observatons
T <- nrow(er_ts)

# construct system matrices for state-space model - CAPM with time variable alpha and betta
Zt <- rbind(rep(1,T), er_ts[,"SP500"]) %>% array(dim = c(1, 2, T))
Ht <- matrix(NA)
Tt <- diag(2)
Rt <- diag(2)
Qt <- matrix(c(NA,0,0,NA), 2,2)

# use diffuse prior
a1 <- matrix(c(0,1), 2, 1)
P1 <- matrix(0,2,2)
P1inf <- diag(2)


# define state-space CAPM model
er_SSM <- SSModel(GM ~ -1 + SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H = Ht, data = er_ts)
er_SSM

# check system matrices
er_SSM$T
er_SSM$R
er_SSM$Z
er_SSM$Q
er_SSM$H

er_SSM$P1
er_SSM$P1inf

# # note: the aboce state space model can be also constructed using SSMregression as follows
# er.SSM <- SSModel(GM ~ -1 + SSMregression(~ rep(1,T) + SP500, Q = diag(NA,2)), H = NA, data = er.ts)
# # here -1 and rep(1,T) is used so that the intercept is also subject to innovations in state transition equation,
# # as can be seen from matrices T, R, Q
# er.SSM$T
# er.SSM$R
# er.SSM$Q
# # in other words, using
# er.SSM <- SSModel(GM ~ SSMregression(~ SP500, Q = NA), H = NA, data = er.ts)
# # implies that intercept is constant, not subject to innovations in state transition equation as can be seen from matrices T, R, Q
# er.SSM$T
# er.SSM$R
# er.SSM$Q

# initials values for paramaters of Q and H used by numerical procedure below to maximize loglikelihood
initvals <- list(rep(1, 3),
                 rep(0.9, 3),
                 rep(1.1, 3),
                 rep(var(er_ts[,"GM"], na.rm = TRUE), 3),
                 rep(var(er_ts[,"GM"], na.rm = TRUE), 3)/100)

# method for finding the maximum loglikelihood (or more precisely minimum of -loglikelihood)
#  Nelder-Mead uses only function values and is relatively slow but robust
#  BFGS is a quasi-Newton method, uses function values and gradients, it is faster than Nelder-Mead but does not always converge to a global mimimum
#  CG is a conjugate gradients method which is generally more fragile than BFGS but can be more successful in much larger optimization problems
methods <- list("BFGS", "Nelder-Mead", "CG")

er_SSM_ML_out_tbl <- list()
r <- 1
for (m in seq_along(methods)) {
    er_SSM_ML_out_tbl[[m]] <- list()
    for (i in seq_along(initvals)) {
        er.SSM.ML.tmp <- fitSSM(er_SSM, inits = log(initvals[[i]]), method = methods[[m]], control = list(maxit = 1000))
        er_SSM_ML_out_tbl[[r]] <- tibble(method = methods[[m]], inits = initvals[i], out = list(er.SSM.ML.tmp$optim.out))
        r <- r + 1
    }
}
er_SSM_ML_out_tbl <- bind_rows(er_SSM_ML_out_tbl)

er_SSM_ML_out_tbl %>%
    mutate(LL = -map_dbl(out, ~.$value),
           inits1 = map_dbl(inits, ~.[1]),
           inits2 = map_dbl(inits, ~.[2]),
           inits3 = map_dbl(inits, ~.[3]),
           par1.exp = map_dbl(out, ~exp(.$par[1])),
           par2.exp = map_dbl(out, ~exp(.$par[2])),
           par3.exp = map_dbl(out, ~exp(.$par[3]))) %>%
    arrange(desc(LL)) %>%
    select(method, LL, inits1, inits2, inits3, par1.exp, par2.exp, par3.exp)

# estimate variances of innovations using maximum likelihood
er_SSM_ML <- fitSSM(er_SSM, inits = log(initvals[[1]]), method = "Nelder-Mead", control = list(maxit = 5000))
er_SSM_ML$optim.out
exp(er_SSM_ML$optim.out$par)
er_SSM_ML$model$Q
er_SSM_ML$model$H


# Kalman filtering and smoothing, with parameters in Q and H set to maximum likelihood estimates
er_SSM_KFS <- KFS(er_SSM_ML$model)
er_SSM_KFS
str(er_SSM_KFS)
names(er_SSM_KFS)

head(er_SSM_KFS$a)
head(er_SSM_KFS$alphahat)

# extract filtered and smoothed alpha and betta
alpha_KFS <- cbind(er_SSM_KFS$a[,1], er_SSM_KFS$alphahat[,1]) %>% ts(start=c(1990,1), frequency=12)
betta_KFS <- cbind(er_SSM_KFS$a[,2], er_SSM_KFS$alphahat[,2]) %>% ts(start=c(1990,1), frequency=12)

par(mfcol=c(2,2))

# plot filtered and smoothed state alpha and betta
plot.ts(alpha_KFS, plot.type="single", xlab="",ylab="alpha", col=c("blue","red"), lwd=2)
legend("topright", c("filtered","smoothed"), col=c("blue","red"), lwd=2, cex=0.7, bty="n")
plot.ts(betta_KFS, plot.type="single", xlab="",ylab="betta", col=c("blue","red"), lwd=2)
legend("topright", c("filtered","smoothed"), col=c("blue","red"), lwd=2, cex=0.7, bty="n")

# plot smoothed state alpha and betta
plot.ts(alpha_KFS[,2], plot.type="single", xlab="",ylab="alpha", col="red", lwd=2)
legend("topright", "smoothed", col="red", lwd=2, cex=0.7, bty="n")
plot.ts(betta_KFS[,2], plot.type="single", xlab="",ylab="betta", col="red", lwd=2)
legend("topright", "smoothed", col="red", lwd=2, cex=0.7, bty="n")

