
# example of SVAR with short run restriction - Enders & Holt (2013)

library(readr)
library(timetk)
library(tidyverse)
library(plotly)
library(vars)
library(stargazer)

theme_set(theme_bw() + 
              theme(strip.text.x = element_text(hjust = 0),
                    strip.text.y = element_text(hjust = 1),
                    strip.background = element_blank()))



#### Data ####

# import data on price of energy, exchange rate, interest rate, price of grain
prices <- read_csv("enders_holt.csv")
prices

prices %>%
    gather(variable, value, -Date) %>%
    ggplot(aes(x = Date, y = value)) +
        geom_line() +
        labs(x = "", y = "") +
        facet_wrap(~variable, ncol = 1, scales = "free_y")

prices.ts <-
    prices %>%
    tk_ts(select = -Date, start = 1974, frequency = 12)



#### SVAR ####

VARselect(prices.ts, lag.max = 12)

# first estimate reduced form VAR
myVAR <- VAR(prices.ts, ic = "AIC", lag.max = 12)

# extract estimation results and use stargazer package to report them
myVAR$varresult %>%
    stargazer(type = "text",
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

# same as above, but using ggplot
ggirf <- function(var.model, n.ahead = 10, impulse = NULL, response = NULL) {
    # IRFs - based on Choleski decomposition of variance-covariance matrix var(e)
    var.irfs <- irf(var.model, impulse = impulse, response = response, n.ahead = n.ahead)
    
    # arrange IRF data into a tibble to be used with ggplot
    var.irfs.tbl <-
        var.irfs %>%
        keep(names(.) %in% c("irf", "Lower", "Upper")) %>%
        modify_depth(2, as_tibble) %>%
        modify_depth(1, bind_rows, .id = "impulse") %>%
        map_df(bind_rows, .id = "key") %>%
        gather(response, value, -key, -impulse) %>%
        group_by(key, impulse, response) %>%
        mutate(lag = row_number()) %>%
        ungroup() %>%
        spread(key, value)
    
    # plot IRFs using ggplot
    g <- ggplot(data = var.irfs.tbl, aes(x = lag, y = irf)) +
        geom_ribbon(aes(x = lag, ymin = Lower, ymax = Upper), fill = "gray50", alpha = .3) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        # scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
        labs(x = "", y = "", title = "Orthogonal Impulse Response Functions (rows: response, columns: impulse)") +
        facet_grid(response ~ impulse, switch = "y", scales = "free_y") 
    g
}

mySVAR %>% ggirf(n.ahead = 40)
mySVAR %>% ggirf(n.ahead = 40, response = "pg")
mySVAR %>% ggirf(n.ahead = 40, response = "pg") %>% ggplotly()



#### FEVD ####
par(mar = c(4,5,2,1))
mySVAR %>% fevd(n.ahead = 40) %>% plot(addbars = 3)

# same as above, but using ggplot
ggfevd <- function(var.model, n.ahead = 10) {
    var.fevd <- fevd(var.model, n.ahead = n.ahead)
    
    # arrange FEVD data into a tibble to be used with ggplot
    var.fevd.tbl <-
        var.fevd %>%
        modify_depth(1, as_tibble) %>%
        map_df(bind_rows, .id = "variable") %>%
        gather(shock, value, -variable) %>%
        group_by(shock, variable) %>%
        mutate(horizon = row_number()) %>%
        ungroup()

    # plot FEVD using ggplot
    g <- ggplot(data = var.fevd.tbl, aes(x = horizon, y = value, fill = shock)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        scale_fill_manual(values = wesanderson::wes_palette("GrandBudapest1")[c(3, 2, 4, 1)]) +
        # scale_fill_manual(values = c("gray80", "gray60", "gray40", "gray20")) +
        labs(x = "horizon", y = "fraction of overall variance", title = "Forecast Error Variance Decomposition") +
        facet_grid(variable ~ .)
    g
}

mySVAR %>% ggfevd(n.ahead = 40)
mySVAR %>% ggfevd(n.ahead = 40) %>% ggplotly()
