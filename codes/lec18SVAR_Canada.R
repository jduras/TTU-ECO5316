
library(vars)

# import macroeconomic time series for Canada
#  e    - employment
#  prod - productivity (real GDP per worker)
#  rw   - real wage in manufacturing
#  U    - unemployment rate
data(Canada)
plot(Canada)
str(Canada)
help(Canada)

# information criteria summary
VARselect(Canada, lag.max=8, type="const")

# estimate an unrestricted VAR
var1 <- VAR(Canada, ic="SC", lag.max=8, type="const")
var3 <- VAR(Canada, ic="AIC", lag.max=8, type="const")


# SVARs with short run restrictions

# specify matrix B0 with contemporaneous restrictions - unrestricted conefficients are left as NA
B0 <- diag(4)
diag(B0) <- NA
B0[2, 1] <- NA
B0[4, 1] <- NA
B0

# use scoring method
svar1 <- SVAR(x=var1, estmethod="scoring", Amat=B0)
svar3 <- SVAR(x=var3, estmethod="scoring", Amat=B0)
# use direct method - maximizing log-likelihood, see help(optim)
svar1.d <- SVAR(x=var1, estmethod="direct", Amat=B0, hessian=TRUE, method="BFGS")
svar3.d <- SVAR(x=var3, estmethod="direct", Amat=B0, hessian=TRUE, method="BFGS")

str(svar1)

# result of the test for overidentifying restrictions
svar1$LR
svar3$LR

# IRFs
plot( irf(svar1, n.ahead=40, impulse="prod") )
plot( irf(svar3, n.ahead=40, impulse="prod") )

par(mfrow=c(2,2))
plot( irf(svar1, n.ahead=40, response="U"), plot.type="single", ask=FALSE )
plot( irf(svar3, n.ahead=40, response="U"), plot.type="single", ask=FALSE )

# FEVD
par(mar=c(4,5,2,1))
plot( fevd(svar1, n.ahead=40) ,addbars=3 )
plot( fevd(svar3, n.ahead=40) ,addbars=3 )



# SVARs with long run restrictions

y.Q <- cbind(Canada[,"prod"], Canada[,"U"])
y.Q <- sweep(y.Q, 2, apply(y.Q, 2, mean))
colnames(y.Q) <- c("prod","U")
plot(y.Q)

VARselect(y.Q, lag.max=8, type="none")

var2 <- VAR(y.Q, ic="SC", lag.max=8, type="none")
svar2 <- BQ(var2)

# IRFs
par(mfrow=c(2,2), cex=0.6)
plot( irf(svar2, n.ahead=60), plot.type="single", ask=FALSE )

# FEVD
par(mar=c(4,5,2,1))
plot( fevd(svar2, n.ahead=60) ,addbars=8 )

