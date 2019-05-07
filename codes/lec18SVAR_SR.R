
# example of SVAR with short run restriction - Enders & Holt (2013)

library(magrittr)
library(readr)
library(timetk)
library(tidyverse)
library(plotly)
library(vars)
library(ggfortify)
library(stargazer)

theme_set(theme_bw() + 
              theme(strip.text.x = element_text(hjust = 0),
                    strip.text.y = element_text(hjust = 1),
                    strip.background = element_blank()))



#### Data ####

# import data on price of energy, exchange rate, interest rate, price of grain
prices <- read_csv("data/enders_holt.csv")
prices

prices %>%
    gather(variable, value, -Date) %>%
    ggplot(aes(x = Date, y = value)) +
        geom_line() +
        labs(x = "", y = "") +
        facet_wrap(~variable, ncol = 1, scales = "free_y")

prices_ts <-
    prices %>%
    tk_ts(select = -Date, start = 1974, frequency = 12)

autoplot(prices_ts)


#### SVAR ####

VARselect(prices_ts, lag.max = 12)

# first estimate reduced form VAR
mod_var <- VAR(prices_ts, ic = "AIC", lag.max = 12)
# mod_var <- restrict(mod_var, method = "ser", thresh = 2.0)

# extract estimation results and use stargazer package to report them
mod_var$varresult %>%
    stargazer(type = "text", no.space = TRUE,
              column.labels = colnames(prices_ts),
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
mod_svar <- SVAR(mod_var, estmethod = "direct", Amat = B0, hessian = TRUE, method = "BFGS")
summary(mod_svar)

# result of the test for overidentifying restrictions
mod_svar$LR

# correlation between residuals in exchange and interest rate equations is -.13 which is quite high compared to
# correlation among other residuals
# this suggests using a modified system where real exchange rate is contemporaneously affected by real interest rate shocks
B0[2, 3] <- NA

# SVAR estimated using direct method - maximizing log-likelihood, see help(optim)
mod_svar <- SVAR(mod_var, estmethod = "direct", Amat = B0, hessian = TRUE, method = "BFGS")
summary(mod_svar)

# result of the test for overidentifying restrictions
mod_svar$LR



#### IRF ####

# all IRFs
par(mfrow=c(4,4), cex=.5, mar = c(4,4,2,1))
mod_svar %>% irf(n.ahead = 40) %>% plot(plot.type = "single")

# only IRFs for price of grain
par(mfrow = c(2,2), cex = .5, mar = c(4,4,2,1))
mod_svar %>% irf(n.ahead = 40, response = "pg") %>% plot(plot.type = "single")


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

irf_svar <- mod_svar %>% irf(n.ahead = 40)

irf_svar %>% ggirf()
irf_svar %>% ggirf(n.ahead = 10)
irf_svar %>% ggirf(response = "pg")
irf_svar %>% ggirf(response = "pg") %>% ggplotly()



#### FEVD ####
par(mar = c(4,5,2,1))
mod_fevd <- mod_svar %>% fevd(n.ahead = 40) 
mod_fevd %>% plot(addbars = 3)

# same as above, but using ggplot
ggfevd <- function(var_fevd, n.ahead = NULL) {
    
    # arrange FEVD data into a tibble to be used with ggplot
    var_fevd_tbl <-
        var_fevd %>%
        modify_depth(1, as_tibble) %>%
        map_df(bind_rows, .id = "variable") %>%
        gather(shock, value, -variable) %>%
        group_by(shock, variable) %>%
        mutate(horizon = row_number()) %>%
        ungroup()

    if (!is.null(n.ahead)) var_fevd_tbl %<>% filter(horizon <= n.ahead)
    
    # plot FEVD using ggplot
    g <- ggplot(data = var_fevd_tbl, aes(x = horizon, y = value, fill = shock)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        scale_fill_manual(values = wesanderson::wes_palette("GrandBudapest1")[c(3, 2, 4, 1)]) +
        # scale_fill_manual(values = c("gray80", "gray60", "gray40", "gray20")) +
        labs(x = "horizon", y = "fraction of overall variance", title = "Forecast Error Variance Decomposition") +
        facet_grid(variable ~ .)
    g
}

mod_fevd %>% ggfevd()
mod_fevd %>% ggfevd(n.ahead = 10)
mod_fevd %>% ggfevd() %>% ggplotly()
