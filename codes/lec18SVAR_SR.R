
# example of SVAR with short run restriction - Enders & Holt (2013)

# remove everything from the environment
rm(list=ls())

library(readxl)
library(timetk)
library(dplyr)
library(ggplot2)
library(vars)
library(stargazer)

theme_set(theme_bw())


#### Data ####

# import data on price of energy, exchange rate, interest rate, price of grain
prices <- read_excel("enders_holt.xlsx")
prices

prices %>%
    gather(variable, value, -Date) %>%
    ggplot(aes(x = Date, y = value)) +
        geom_line() +
        facet_wrap(~variable, ncol = 1, scales = "free_y")

prices.ts <-
    prices %>%
    tk_ts(select = -Date, start = 1974, frequency = 12)


#### SVAR ####

VARselect(prices.ts, lag.max = 12)

# first estimate reduced form VAR
myVAR <- VAR(prices.ts, ic = "AIC", lag.max = 12)

# extract estimation results and use stargazer package to report them
r <- myVAR$varresult

stargazer(r$PE, r$ex, r$r, r$pg, type = "text",
          column.labels = colnames(prices.ts),
          dep.var.labels.include = FALSE)

# specify matrix B0 with contemporaneous restrictions - unrestricted coefficients are left as NA
B0 <- diag(4)
diag(B0) <- NA
B0[4, 1] <- NA
B0[4, 2] <- NA
B0[4, 3] <- NA
B0

# SVAR estimated using direct method - maximizing log-likelihood, see help(optim)
?SVAR
mySVAR <- SVAR(myVAR, estmethod = "direct", Amat = B0, hessian = TRUE, method = "BFGS")
summary(mySVAR)

# result of the test for overidentifying restrictions
mySVAR$LR

# correlation between residuals in exchange and interest rate equations is -.13 which is quite high compared to
# correlation among other residuals
# this suggests using a modified system where real exchange rate is contemporaneously affected by real interest rate shocks
B0[2, 3] <- NA

# SVAR estimated using direct method - maximizing log-likelihood, see help(optim)
mySVAR <- SVAR(myVAR, estmethod = "direct", Amat = B0, hessian = TRUE, method = "BFGS")
summary(mySVAR)

# result of the test for overidentifying restrictions
mySVAR$LR


#### IRF ####

# all IRFs
par(mfrow=c(4,4), cex=.5, mar = c(4,4,2,1))
mySVAR %>% irf(n.ahead = 40) %>% plot(plot.type = "single")

# only IRFs for price of grain
par(mfrow = c(2,2), cex = .5, mar = c(4,4,2,1))
mySVAR %>% irf(n.ahead = 40, response = "pg") %>% plot(plot.type = "single")


#### FEVD ####
par(mar = c(4,5,2,1))
mySVAR %>% fevd(n.ahead = 40) %>% plot(addbars = 3)
