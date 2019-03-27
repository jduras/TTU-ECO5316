
rm(list=ls())

library(magrittr)
library(tidyquant)
library(timetk)
library(tibbletime)
library(broom)
library(ggplot2)
library(ggfortify)

# set default theme for ggplot2
theme_set(theme_bw())


#### Data ####

# obtain data on house price index for Los Angeles MSA and for Riverside MSA
hpi.tbl <-
    tq_get(c("ATNHPIUS31084Q","ATNHPIUS40140Q"), get = "economic.data",
           from  = "1940-01-01", to = "2017-12-31") %>%
    group_by(symbol) %>%
    mutate(y = price,
           dly = log(y) - lag(log(y)),
           msa = case_when(symbol == "ATNHPIUS31084Q" ~ "LA",
                           symbol == "ATNHPIUS40140Q" ~ "RI")) %>%
    ungroup() %>%
    dplyr::select(msa, date, y, dly)

# plot house price index and log change in house price index in Los Angeles MSA and for Riverside MSA
hpi.tbl %>%
    gather(variable, value, -date, -msa) %>%
    mutate(variable_labels = case_when(variable == "y" ~ "House Price Index, quarterly",
                                       variable == "dly" ~ "House Price Index, quarterly, log change")) %>%
    ggplot(aes(x = date, y = value, group = msa)) +
        geom_line(aes(col = msa, linetype = msa)) +
        scale_color_manual(name = "MSA", values = c("blue","red"), labels = c("Los Angeles","Riverside")) +
        scale_linetype_manual(name = "MSA", values = c("solid","dashed"), labels = c("Los Angeles","Riverside")) +
        labs(x = "", y = "", linetype = "") +
        facet_wrap(~variable_labels, scales = "free_y")

# convert log change in house price index in Los Angeles MSA and for Riverside MSA into ts
hpi.ts <-
    hpi.tbl %>%
    dplyr::select(msa, date, dly) %>%
    spread(msa, dly) %>%
    filter(date >= "1976-07-01" & date <= "2012-10-01") %>%
    tk_ts(select = c("LA","RI"), start = 1976.5, frequency = 4)


#### VAR ####

# load package that allows to estimate and analyze VAR models
library(vars)

VARselect(hpi.ts, lag.max = 8, type = "const")

# estimate a reduced form VAR(1)
var1 <- VAR(hpi.ts, p = 1, type = "const")
var1
summary(var1)

# estimate VAR(p) using AIC to select p
varp <- VAR(hpi.ts, ic = "AIC", lag.max = 8, type = "const")
varp
summary(varp)

# using stargazer package to report results of VAR estimation
lm1 <- var1$varresult
lmp <- varp$varresult

library(stargazer)
stargazer(lm1$LA, lm1$RI, lmp$LA, lmp$RI,
          type  ="text", column.labels = rep(colnames(hpi.ts), 2),
          dep.var.labels.include = FALSE)


# plot residuals and their ACF and PACF
plot(varp)
plot(varp, names = "LA")
plot(varp, names = "RI")

str(varp)
names(varp)
names(varp$varresult)


# QQ plot for residuals
library(qqplotr)
ggplot(data = as.tibble(varp$varresult$LA$residuals), mapping = aes(sample = value)) +
    stat_qq_band(alpha = 0.3, conf = 0.95) +
    stat_qq_line() +
    stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Residuals in Los Angeles equation")

ggplot(data = as.tibble(varp$varresult$RI$residuals), mapping = aes(sample = value)) +
    stat_qq_band(alpha = 0.3, conf = 0.95) +
    stat_qq_line() +
    stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Residuals in Riverside equation")


# multivariate Jarque-Bera test
normality.test(varp)


#### Granger causality ####

causality(varp, cause = "LA")
causality(varp, cause = "RI")


#### Restricted VAR ####

# estimate restricted VAR - based on Granger causality test eliminate lags of RI from the equation for  LA

# define a  matrix with restictions
mat.r <- matrix(1, nrow = 2, ncol = 7)
mat.r[1, c(2,4,6)] <- 0
mat.r
varp.r <- restrict(varp, method = "manual", resmat = mat.r)
varp.r
summary(varp.r)
varp.r$restrictions
Acoef(varp.r)

# estimate restricted VAR - keep only variables with t-value larger than 2.0
varp.r.ser <- restrict(varp, method = "ser", thresh = 2.0)
varp.r.ser
summary(varp.r.ser)
varp.r.ser$restrictions
Acoef(varp.r.ser)


#### Forecasting ####

varp.f <- predict(varp, n.ahead = 16)
plot(varp.f)
fanchart(varp.f)
autoplot(varp.f, is.date = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "", y = "", title = "Multistep forecast for House Price Index, quarterly, log change")




# next, estimate rolling VAR with window size = window.length
window.length <- nrow(hpi.ts)
# create rolling VAR function with rollify from tibbletime package
roll_VAR <- rollify(function(LA, RI) {
                        x <- cbind(LA, RI)
                        VAR(x, ic = "AIC", lag.max = 8, type = "const")
                        },
                    window = window.length, unlist = FALSE)


# estimate rolling VAR model, create 1 period ahead rolling forecasts
results <-
    hpi.tbl %>%
    dplyr::select(msa, date, dly) %>%
    spread(msa, dly) %>%
    filter(date >= "1976-07-01") %>%
    as_tbl_time(index = date) %>%                                                           # covert to tibbletime
    mutate(VAR.model = roll_VAR(LA,RI)) %>%                                                 # estimate models
    filter(!is.na(VAR.model)) %>%                                                           # remove periods at the beginning of sample where model could not be estimated due to lack of data,
    mutate(VAR.coefs = map(VAR.model, (. %$% map(varresult, tidy, conf.int = TRUE) %>%      # extract coefficients
                                           map(as.tibble) %>%
                                           bind_rows(.id = "msa"))),
           VAR.f = map(VAR.model, (. %>% predict(n.ahead = 1) %$%                           # extract forecast
                                       fcst %>%
                                       map(as.tibble) %>%
                                       bind_rows(.id = "msa"))))
results

# plot estimated coefficients with confidence intervals
results %>%
    dplyr::select(date, VAR.coefs) %>%
    unnest() %>%
    ggplot(aes(x = date, y = estimate, group = term)) +
        geom_line(color = "royalblue") +
        geom_ribbon(aes(x = date, ymin = conf.low, ymax = conf.high), alpha = 0.5, fill = "lightblue") +
        geom_hline(yintercept = 0, color = "black")+
        labs(x = "", y = "",
             title = "Coefficient estimates",
             subtitle = paste(window.length, "month rolling window VAR model"))+
        facet_grid(term ~ msa, scales = "free_y")

# 1 period ahead rolling forecasts
tbl.f.1.rol <-
    bind_rows(
        # actual data
        hpi.tbl %>%
            dplyr::select(date, msa, dly) %>%
            rename(value = dly) %>%
            mutate(key = "actual"),
        # forecasts
        results %>%
            dplyr::select(date, VAR.f) %>%
            unnest(VAR.f) %>%
            rename(value = fcst) %>%
            mutate(key = "forecast",
                   date = date %m+% months(3))
    ) %>%
    arrange(date, msa)

# plot the 1 period ahead rolling forecasts
tbl.f.1.rol %>%
    dplyr::filter(date >= "2000-01-01") %>%
    mutate(msa.f = factor(msa, labels = c("Los Angeles","Riverside"))) %>%
    ggplot(aes(x = date, y = value, col = key, group = key)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, fill = "steelblue", alpha = 0.2) +
        geom_line(size = 0.7) +
        geom_point(size = 0.7) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_color_manual(values = c("black","blue")) +
        labs(x = "", y = "", title = "Rolling one step ahead forecast for House Price Index, quarterly, log change") +
        facet_grid(msa.f ~ ., scales = "free_y") +
        theme(legend.position = "none")


#### Impulse-Response Functions (IRF) ####

# IRFs - based on Choleski decomposition of variance-covariance matrix var(e)
var1.irfs <- irf(var1, n.ahead = 40)
varp.irfs <- irf(varp, n.ahead = 40)

# plot IRFs using plot from vars package
par(mfcol=c(2,2), cex = 0.6)
plot(varp.irfs, plot.type = "single")

# arrange IRF data into a tibble to be used with ggplot
varp.irfs.tbl <-
    varp.irfs[1:3] %>%
    modify_depth(2, as.tibble) %>%
    modify_depth(1, bind_rows, .id = "impulse") %>%
    map_df(bind_rows, .id = "key") %>%
    gather(response, value, -key, -impulse) %>%
    group_by(key, impulse, response) %>%
    mutate(lag = row_number()) %>%
    ungroup() %>%
    spread(key, value)

# plot IRFs using ggplot
g <- ggplot(data = varp.irfs.tbl, aes(x = lag, y = irf)) +
    geom_ribbon(aes(x = lag, ymin = Lower, ymax = Upper), fill = "gray50", alpha = .3) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "", y = "", title = "Orthogonal Impulse Response Functions (rows: response, columns: impulse)") +
    facet_grid(response ~ impulse, switch = "y") +
    theme(strip.text.y = element_text(angle = 0))
g

# plot IRFs using plotly
library(plotly)
ggplotly(g)


#### Forecast Error Variance Decomposition (FEVD) ####

# FEVD - based on Choleski decomposition of variance-covariance matrix var(e)
varp.fevd <- fevd(varp, n.ahead = 40)
varp.fevd[[1]][c(1,4,8,40),]
varp.fevd[[2]][c(1,4,8,40),]
plot(varp.fevd)
plot(varp.fevd, addbars=8)

# arrange FEVD data into a tibble to be used with ggplot
varp.fevd.tbl <-
    varp.fevd %>%
    modify_depth(1, as.tibble) %>%
    map_df(bind_rows, .id = "variable") %>%
    gather(shock, value, -variable) %>%
    group_by(shock, variable) %>%
    mutate(horizon = row_number()) %>%
    ungroup()

# plot FEVD using ggplot
library(wesanderson)
g <- ggplot(data = varp.fevd.tbl, aes(x = horizon, y = value, fill = shock)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    # scale_fill_manual(values = wes_palette("FantasticFox1")[c(3,5)]) +
    # scale_fill_manual(values = wes_palette("GrandBudapest1")[c(4,3)]) +
    scale_fill_manual(values = c("gray70","gray40")) +
    labs(x = "horizon", y = "fraction of overall variance", title = "Forecast Error Variance Decomposition") +
    facet_grid(variable ~ .)
g

# plot FEVD using plotly
ggplotly(g)

# note: ordering of variables in VAR matters when it comes to IRFs and FEVD

# ordering 1: LA before RI
hpi.ts.ord1 <-
    hpi.tbl %>%
    dplyr::select(msa, date, dly) %>%
    spread(msa, dly) %>%
    filter(date >= "1976-07-01" & date <= "2012-10-01") %>%
    tk_ts(select = c("LA","RI"), start = 1976.75, frequency = 4)

# ordering 2: RI before LA
hpi.ts.ord2 <-
    hpi.tbl %>%
    dplyr::select(msa, date, dly) %>%
    spread(msa, dly) %>%
    filter(date >= "1976-07-01" & date <= "2012-10-01") %>%
    tk_ts(select = c("RI","LA"), start = 1976.75, frequency = 4)

# reduced form VAR(1)
var1.ord1 <- VAR(hpi.ts.ord1, p = 1, type = "const")
var1.ord2 <- VAR(hpi.ts.ord2, p = 1, type = "const")

# IRF based on Choleski decomposition of var(e)
var1.irfs.ord1 <- irf(var1.ord1, n.ahead = 40)
var1.irfs.ord2 <- irf(var1.ord2, n.ahead = 40)
par(mfcol = c(2,2), cex = 0.6)
plot(var1.irfs.ord1, plot.type = "single")
plot(var1.irfs.ord2, plot.type = "single")

# FEVD based on Choleski decomposition of var(e)
var1.fevd.ord1 <- fevd(var1.ord1, n.ahead = 40)
var1.fevd.ord2 <- fevd(var1.ord2, n.ahead = 40)
plot(var1.fevd.ord1)
plot(var1.fevd.ord2)
