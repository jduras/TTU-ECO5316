
library(magrittr)
library(tidyquant)
library(timetk)
library(tibbletime)
library(tsibble)
library(vars)           # package that allows to estimate and analyze VAR models
library(broom)
library(stargazer)
library(listviewer)
library(ggplot2)
library(ggfortify)
library(qqplotr)
library(scales)
library(plotly)

# set default theme for ggplot2
theme_set(theme_bw() +
              theme(strip.text.x = element_text(hjust = 0),
                    strip.text.y = element_text(hjust = 1),
                    axis.ticks = element_blank(),
                    strip.background = element_blank()))



#### Data ####

# obtain data on house price index for Los Angeles MSA and for Riverside MSA
hpi_raw <- 
    tq_get(c("ATNHPIUS31084Q","ATNHPIUS40140Q"), get = "economic.data",
           from  = "1940-01-01", to = "2018-12-31")

write_csv(hpi_raw, path = "data/hpi_raw.csv")

hpi_tbl <-
    hpi_raw %>%
    group_by(symbol) %>%
    mutate(y = price,
           dly = log(y) - lag(log(y)),
           msa = case_when(symbol == "ATNHPIUS31084Q" ~ "LA",
                           symbol == "ATNHPIUS40140Q" ~ "RI")) %>%
    ungroup() %>%
    dplyr::select(msa, date, y, dly)

# plot house price index and log change in house price index in Los Angeles MSA and for Riverside MSA
hpi_tbl %>%
    gather(variable, value, -date, -msa) %>%
    mutate(variable_labels = case_when(variable == "y" ~ "House Price Index, quarterly",
                                       variable == "dly" ~ "House Price Index, quarterly, log change")) %>%
    ggplot(aes(x = date, y = value, group = msa)) +
        geom_line(aes(col = msa, linetype = msa)) +
        scale_color_manual(values = c("blue", "red"), labels = c("Los Angeles MSA", "Riverside MSA")) +
        scale_linetype_manual(values = c("solid", "dashed"), labels = c("Los Angeles MSA", "Riverside MSA")) +
        labs(x = "", y = "", color = "", linetype = "") +
        facet_wrap(~variable_labels, ncol = 1, scales = "free_y") +
        theme(legend.position = c(0.1, 0.94),
              legend.key = element_blank(),
              legend.background = element_blank())

# convert log change in house price index in Los Angeles MSA and for Riverside MSA into ts
hpi_ts <-
    hpi_tbl %>%
    dplyr::select(msa, date, dly) %>%
    spread(msa, dly) %>%
    filter(date >= "1976-07-01" & date <= "2012-10-01") %>%
    tk_ts(select = c("LA","RI"), start = 1976.5, frequency = 4)

autoplot(hpi_ts)



#### VAR ####

VARselect(hpi_ts, lag.max = 8, type = "const")

# estimate a reduced form VAR(1)
var1 <- VAR(hpi_ts, p = 1, type = "const")
var1
summary(var1)

# estimate VAR(p) using AIC to select p
varp <- VAR(hpi_ts, ic = "AIC", lag.max = 8, type = "const")
varp
summary(varp)

# for large nested lsts jsonedit from listviewer package is more convenient than str 
str(var1)
str(varp)
names(varp)
names(varp$varresult)

jsonedit(var1, mode = "view")

# use stargazer package to report var_roll_results of VAR estimation
lm1 <- var1$varresult
lmp <- varp$varresult

# report var_roll_results for all equations
stargazer(lm1, lmp,
          type  ="text", column.labels = rep(colnames(hpi_ts), 2),
          dep.var.labels.include = FALSE)

# report var_roll_results for selected equations only
stargazer(lm1$LA, lm1$RI, lmp$LA, lmp$RI,
          type  ="text", column.labels = rep(colnames(hpi_ts), 2),
          dep.var.labels.include = FALSE)

stargazer(lm1$RI, lmp$RI,
          type  ="text", column.labels = c("RI VAR(1)", "RI VAR(3)"),
          dep.var.labels.include = FALSE)



# plot residuals and their ACF and PACF
plot(varp)
plot(varp, names = "LA")
plot(varp, names = "RI")


# QQ plot for residuals
varp$varresult$LA$residuals

varp %>%
    pluck("varresult", "LA", "residuals") %>% 
    as_tibble() %>%
    ggplot(mapping = aes(sample = value)) +
        stat_qq_band(alpha = 0.3, conf = 0.95) +
        stat_qq_line() +
        stat_qq_point() +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Residuals in Los Angeles equation")

varp %>%
    pluck("varresult", "RI", "residuals") %>% 
    as_tibble() %>%
    ggplot(mapping = aes(sample = value)) +
        stat_qq_band(alpha = 0.3, conf = 0.95) +
        stat_qq_line() +
        stat_qq_point() +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Residuals in Riverside equation")

residuals(varp) %>% 
    as_tibble() %>%
    gather(msa, value) %>%
    ggplot(mapping = aes(sample = value)) +
        stat_qq_band(alpha = 0.3, conf = 0.95) +
        stat_qq_line() +
        stat_qq_point() +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Residuals in Los Angeles equation") +
        facet_wrap(~msa, ncol = 1)

# multivariate Jarque-Bera test
normality.test(varp)



#### Granger causality ####

causality(varp, cause = "LA")
causality(varp, cause = "RI")



#### Restricted VAR ####

# estimate restricted VAR - based on Granger causality test eliminate lags of RI from the equation for  LA

# define a  matrix with restictions
mat_r <- matrix(1, nrow = 2, ncol = 7)
mat_r[1, c(2, 4, 6)] <- 0
mat_r
varp_r <- restrict(varp, method = "manual", resmat = mat_r)
varp_r
summary(varp_r)
varp_r$restrictions
Acoef(varp_r)

# estimate restricted VAR - keep only variables with t-value larger than 2.0
varp_r_ser <- restrict(varp, method = "ser", thresh = 2.0)
varp_r_ser
summary(varp_r_ser)
varp_r_ser$restrictions
Acoef(varp_r_ser)



#### Forecasting ####

# create 1 to 16 quarter ahead forecast
varp_f <- predict(varp, n.ahead = 16)

plot(varp_f)
fanchart(varp_f)

autoplot(varp_f, is.date = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "", y = "", title = "Multistep forecast for House Price Index, quarterly, log change")

# next, estimate rolling VAR with window size = window_length
window_length <- nrow(hpi_ts)
# create rolling VAR function with rollify from tibbletime package
rolling_var <- rollify(function(LA, RI) {
                        x <- cbind(LA, RI)
                        VAR(x, ic = "SC", lag.max = 8, type = "const")
                        },
                    window = window_length, unlist = FALSE)

# estimate rolling VAR model, create 1 period ahead rolling forecasts - using tibbletime
var_roll_results <-
    hpi_tbl %>%
    dplyr::select(msa, date, dly) %>%
    spread(msa, dly) %>%
    filter(date >= "1976-07-01") %>%
    as_tbl_time(index = date) %>%                                                           # covert to tibbletime
    mutate(var_mod = rolling_var(LA,RI)) %>%                                                 # estimate models
    filter(!is.na(var_mod)) %>%                                                           # remove periods at the beginning of sample where model could not be estimated due to lack of data,
    mutate(var_coefs = map(var_mod, (. %$% map(varresult, tidy, conf.int = TRUE) %>%      # extract coefficients
                                           map(as_tibble) %>%
                                           bind_rows(.id = "msa"))),
           var_f = map(var_mod, (. %>% predict(n.ahead = 1) %$%                           # extract forecast
                                       fcst %>%
                                       map(as_tibble) %>%
                                       bind_rows(.id = "msa"))))
var_roll_results

# estimate rolling VAR model, create 1 period ahead rolling forecasts - using tsibble
var_roll_results <-
    hpi_tbl %>%
    dplyr::select(msa, date, dly) %>%
    spread(msa, dly) %>%
    filter(date >= "1976-07-01") %>%
    mutate(yearq = yearquarter(date)) %>%
    as_tsibble(index = yearq) %>%
    mutate(var_mod = slide2(LA, RI, ~ bind_cols(LA = .x, RI = .y) %>% 
                                  VAR(ic = "SC", lag.max = 8, type = "const"), 
                              .size = window_length)) %>%                                   # estimate models
    filter(!is.na(var_mod)) %>%                                                           # remove periods at the beginning of sample where model could not be estimated due to lack of data,
    mutate(var_coefs = map(var_mod, (. %$% map(varresult, tidy, conf.int = TRUE) %>%      # extract coefficients
                                           map(as_tibble) %>%
                                           bind_rows(.id = "msa"))),
           var_f = map(var_mod, (. %>% predict(n.ahead = 1) %$%                           # extract forecast
                                       fcst %>%
                                       map(as_tibble) %>%
                                       bind_rows(.id = "msa"))))
var_roll_results

# plot estimated coefficients with confidence intervals
var_roll_results %>%
    as_tibble() %>%
    dplyr::select(date, var_coefs) %>%
    unnest() %>%
    ggplot(aes(x = date, y = estimate, group = term)) +
        geom_line(color = "royalblue") +
        geom_ribbon(aes(x = date, ymin = conf.low, ymax = conf.high), alpha = 0.5, fill = "lightblue") +
        geom_hline(yintercept = 0, color = "black")+
        labs(x = "", y = "",
             title = "Coefficient estimates",
             subtitle = paste(window_length, "month rolling window VAR model"))+
        facet_grid(term ~ msa, scales = "free_y")

# 1 period ahead rolling forecasts
var_roll_f <-
    bind_rows(
        # actual data
        hpi_tbl %>%
            dplyr::select(date, msa, dly) %>%
            rename(value = dly) %>%
            mutate(key = "actual"),
        # forecasts
        var_roll_results %>%
            as_tibble() %>%
            dplyr::select(date, var_f) %>%
            unnest(var_f) %>%
            rename(value = fcst) %>%
            mutate(key = "forecast",
                   date = date %m+% months(3))
    ) %>%
    arrange(date, msa)

# plot the 1 period ahead rolling forecasts
var_roll_f %>%
    dplyr::filter(date >= "2000-01-01") %>%
    mutate(msa.f = factor(msa, labels = c("Los Angeles", "Riverside"))) %>%
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
var1_irf <- irf(var1, n.ahead = 40)
varp_irf <- irf(varp, n.ahead = 40)

# plot IRFs using plot from vars package
par(mfcol = c(2,2), cex = 0.6)
plot(var1_irf, plot.type = "single")
plot(varp_irf, plot.type = "single")

str(varp_irfs)


# same as above, but using ggplot
ggirf <- function(var_irf, n.ahead = NULL, impulse = NULL, response = NULL) {
    # arrange IRF data into a tibble to be used with ggplot
    var_irf_tbl <-
        var_irf %>%
        keep(names(.) %in% c("irf", "Lower", "Upper")) %>%
        modify_depth(2, as_tibble) %>%
        modify_depth(1, bind_rows, .id = "key_impulse") %>%
        map_df(bind_rows, .id = "component") %>%
        gather(key_response, value, -component, -key_impulse) %>%
        group_by(component, key_impulse, key_response) %>%
        mutate(lag = row_number()) %>%
        ungroup() %>%
        spread(component, value)
    
    if (!is.null(impulse)) var_irf_tbl %<>% filter(key_impulse %in% impulse)
    if (!is.null(response)) var_irf_tbl %<>% filter(key_response %in% response)
    if (!is.null(n.ahead)) var_irf_tbl %<>% filter(lag <= n.ahead)
    
    # plot IRFs using ggplot
    g <- ggplot(data = var_irf_tbl, aes(x = lag, y = irf)) +
        geom_ribbon(aes(x = lag, ymin = Lower, ymax = Upper), fill = "gray50", alpha = .3) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        # scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
        labs(x = "", y = "", title = "Orthogonal Impulse Response Functions (rows: response, columns: impulse)") +
        facet_grid(key_response ~ key_impulse, switch = "y", scales = "free_y") 
    g
}

var1_irf %>% ggirf()
varp_irf %>% ggirf()

# plot IRFs using plotly
var1_irf %>% ggirf() %>% ggplotly()
varp_irf %>% ggirf() %>% ggplotly()



#### Forecast Error Variance Decomposition (FEVD) ####

# FEVD - based on Choleski decomposition of variance-covariance matrix var(e)
varp_fevd <- fevd(varp, n.ahead = 40)
str(varp_fevd)

varp_fevd[["LA"]][c(1,4,8,40),]
varp_fevd[["RI"]][c(1,4,8,40),]
plot(varp_fevd)
plot(varp_fevd, addbars = 8)

# arrange FEVD data into a tibble to be used with ggplot
varp_fevd_tbl <-
    varp_fevd %>%
    modify_depth(1, as_tibble) %>%
    map_df(bind_rows, .id = "variable") %>%
    gather(shock, value, -variable) %>%
    group_by(shock, variable) %>%
    mutate(horizon = row_number()) %>%
    ungroup()

# plot FEVD using ggplot
library(wesanderson)
g <- ggplot(data = varp_fevd_tbl, aes(x = horizon, y = value, fill = shock)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    # scale_fill_manual(values = wes_palette("FantasticFox1")[c(3,5)]) +
    # scale_fill_manual(values = wes_palette("GrandBudapest1")[c(4,3)]) +
    scale_fill_manual(values = c("gray70","gray40")) +
    labs(x = "horizon", y = "fraction of overall variance", title = "Forecast Error Variance Decomposition") +
    facet_wrap(~variable, ncol = 1)
g

# plot FEVD using plotly
ggplotly(g)



#### Note on variable ordering in VAR ####

# ordering of variables in VAR 
# - does not matter for estimated coefficients and forecasts
# - matters when it comes to IRFs and FEVD

# ordering 1: LA before RI
hpi_ts_ord1 <-
    hpi_tbl %>%
    dplyr::select(msa, date, dly) %>%
    spread(msa, dly) %>%
    filter(date >= "1976-07-01" & date <= "2012-10-01") %>%
    tk_ts(select = c("LA","RI"), start = 1976.75, frequency = 4)

# ordering 2: RI before LA
hpi_ts_ord2 <-
    hpi_tbl %>%
    dplyr::select(msa, date, dly) %>%
    spread(msa, dly) %>%
    filter(date >= "1976-07-01" & date <= "2012-10-01") %>%
    tk_ts(select = c("RI","LA"), start = 1976.75, frequency = 4)

# reduced form VAR(1)
var1_ord1 <- VAR(hpi_ts_ord1, p = 1, type = "const")
var1_ord2 <- VAR(hpi_ts_ord2, p = 1, type = "const")

# IRF based on Choleski decomposition of var(e)
var1_irfs_ord1 <- irf(var1_ord1, n.ahead = 40)
var1_irfs_ord2 <- irf(var1_ord2, n.ahead = 40)
par(mfcol = c(2,2), cex = 0.6)
plot(var1_irfs_ord1, plot.type = "single")
plot(var1_irfs_ord2, plot.type = "single")

# FEVD based on Choleski decomposition of var(e)
var1_fevd_ord1 <- fevd(var1_ord1, n.ahead = 40)
var1_fevd_ord2 <- fevd(var1_ord2, n.ahead = 40)
plot(var1_fevd_ord1)
plot(var1_fevd_ord2)
