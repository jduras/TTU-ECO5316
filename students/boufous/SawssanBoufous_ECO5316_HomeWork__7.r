library(magrittr)
library(tidyquant)
library(timetk)
library(tibbletime)
library(broom)
library(ggplot2)
library(ggfortify)
library(forecast)
library(Quandl)
library(urca)
library(vars)
library(devtools)
# (a) Importing Data
output <- tq_get(("OPHNFB"), get = "economic.data",
                 from  = "1947-01-01", to = "2017-12-31")

hours <- tq_get(("HOANBS"), get = "economic.data",
                from  = "1947-01-01", to = "2017-12-31")

output
hours



#(b) Test the log of (output) & (hours) and perform ERS unit root test

data.real.output <- Quandl("FRED/OPHNFB", type = 'zoo')
data.hours <- Quandl("FRED/HOANBS", type = 'zoo')

y1 <- data.real.output 
y2 <- data.hours

log.y1 <- 100*log(y1)
log.y2 <- 100*log(y2)

first.diff.y1 <- diff(y1, differences = 1)
first.diff.y2 <- diff(y2, differences = 1)

ERS.1 <- summary(ur.ers(log.y1, type = "DF-GLS", model = "trend"))
ERS.2 <- summary(ur.ers(log.y2, type = "DF-GLS", model = "trend"))

ERS.3 <- summary(ur.ers(first.diff.y1, type = "DF-GLS", model = "trend"))
ERS.4 <- summary(ur.ers(first.diff.y2, type = "DF-GLS", model = "trend"))


ERS.1
ERS.2
ERS.3
ERS.4


# Comment
#The first two tests examine the ERS test statistics of the log transformed data. 
#Both tests fail to reject the null hypothesis which implies that the log-transformed data contains 
#a unit root and therefore is not approximately weakly stationary. 
#The last two tests examine the ERS test statistic of the first difference tranformed data. 
#Both tests reject the null hypothesis at the 1% level, which gives us ground to assume that the first 
#difference of both of our data sets are approximately weakly stationary. 
#Given the results of the ERS tests, we will use the first differnces of our data sets in the following questions. 

# (c) Estimate a bivariate reduced form VAR using AIC

real.output <- first.diff.y1
hours <- first.diff.y2
y <- cbind(real.output, hours)
var.p <- VAR(y, ic = "AIC", lag.max = 12, type = "const")
y <- sweep(y, 2, apply(y, 2, mean))

plot(y)

myVAR <- VAR(y, ic = "AIC", lag.max = 12)

mySVAR <- BQ(myVAR)



# (d) Blanchard & Quah approach to obtain SVAR

mySVAR
summary(mySVAR)


# (e)Report and Interpret the impact and the LR impact matrices for the SVAR
# & (f) Plot the cumulative IRFs
myIRF.c <- irf(mySVAR, n.ahead = 12, ci = .9, cumulative = TRUE)

summary(mySVAR)

plot(irf(mySVAR, nsteps=12))

# (g) Compare the IRFs with Fig2
# figure 2 by Gali  displays the estimated dynamic responses to a nontechnology shock.
# the paper shows that the shock affects positively the outputs hours and productivity. also, the effect on productivity vanishes over time
# but still has a sizable permanent impact on both hours and output and emerge as the main source of 
# the unit root detected in hours.
# in our case, the shock seems to have a negative and persistent effect on the productivity while it affects positively the hours. 
# at the peak productivity increases by a little more than 0.1% while hours fall by around 0.1 percentage points 

# (h) Construct FEVD for the SVAR
 plot( fevd(mySVAR, n.ahead=30) ,addbars=10 )

#overall fluctuations explained in the short run by the two shocks and in the long run: 
# The shocks seem to have a strong positive effect on productivity and hours in short time while in the long run, 
# the positive effect vanishes for the productivity until reaching a null level (0.00) while for hours, the effect follows an increasing
# trend going from a negative percentage to a 0 level.

