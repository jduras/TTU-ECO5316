
library(tidyverse)
library(tidyquant)
library(timetk)
library(rugarch)
library(qqplotr)
library(janitor)
library(egg)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(latex2exp)
# library(xts)
# library(PerformanceAnalytics)
# library(fTrading)
# library(devtools)
# devtools::install_github("jduras/stargazer")
library(stargazer)

theme_set(theme_minimal() + 
              theme(strip.text = element_text(hjust = 0),
                    plot.caption = element_text(hjust = 0)))

#### get data for S&P500 and Microsoft stock ####

stockmkt_tbl_raw <- tq_get(c("^GSPC", "MSFT"),
                           get = "stock.prices", from = "1985-01-01", to = "2017-12-31")

stockmkt_tbl <-
    stockmkt_tbl_raw %>%
    mutate(symbol_label = recode(symbol, `^GSPC` = "S&P 500", MSFT = "Microsoft")) %>%
    select(symbol, symbol_label, date, price = adjusted) %>%
    filter(!is.na(price)) %>%
    group_by(symbol) %>%
    # mutate(return = log(price) - lag(log(price))) %>%
    mutate(return = (price - lag(price))/lag(price)) %>%
    ungroup() 

stockmkt_tbl %>%
    gather(measure, value, c(price, return)) %>%
    ggplot(aes(x = date, y = value, color = measure)) +
        geom_hline(yintercept = 0, color = "gray") +
        geom_line() +
        facet_wrap(~ symbol_label + measure, scales = "free") +
        labs(x = "", y = "", color = "") +
        theme(legend.position = "none")

msft_xts <- stockmkt_tbl %>%
    filter(symbol == "MSFT", date >= "1999-01-04", date <= "2017-12-31") %>%
    tk_xts(select = return, date_var = date)

sp500_xts <- stockmkt_tbl %>%
    filter(symbol == "^GSPC", date >= "1989-01-04", date <= "2017-12-31") %>%
    tk_xts(select = return, date_var = date)

# dataset <- sp500_xts
# title_label <- "S\\&P 500"

dataset <- msft_xts
title_label <- "Microsoft"



#### estimate GARCH Models ####

# AR(1)-GARCH(1,1) model with normal distribution
ar1garch11norm <- 
    ugarchspec(mean.model = list(armaOrder = c(1, 0)),
               variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
               distribution.model = "norm") %>%
    ugarchfit(data = dataset)

# standardized returns, stdret <- (ret - fitted(garchfit)) / sigma(garchfit) or alternatively
ar1garch11norm_stdret <- ar1garch11norm %>% residuals(standardize = TRUE)

# standardized returns - qqplot
ar1garch11norm_stdret %>% tk_tbl() %>%
    ggplot(aes(sample = value)) +
        stat_qq_band(alpha = 0.3, conf = 0.99) +
        stat_qq_line() +
        stat_qq_point() +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# standardized returns - histogram
g_hs <- ggplot() +
    geom_histogram(aes(x = value, y = ..density..), bins = 100, fill = "gray80", color = "white") + 
    geom_density(aes(x = value, y = ..density..), color = "gray10", size = 1) +
    scale_y_continuous(breaks = seq(0, 0.6, 0.1), limits = c(0, 0.6)) +
    labs(x = "", y = "", title = "Histogram and Density for Standardized Returns",
         caption = "assumed theoretical distribution shown in blue") 

g_hs_all <- g_hs %+% 
    {ar1garch11norm_stdret %>% tk_tbl()} +
    stat_function(data = tibble(xsim = c(-3, 3)), aes(x = xsim),
                  fun = dnorm, n = 101, color = "steelblue", size = 1, alpha = 0.75) 
g_hs_all

# standardized returns - tails of the histogram
g_hs_left <- g_hs_all %+% 
    coord_cartesian(xlim = c(-8, -2), ylim = c(0, 0.02)) +
    labs(x = "", y = "", title = "", caption = "")
g_hs_right <- g_hs_all %+% 
    coord_cartesian(xlim = c(2, 8), ylim = c(0, 0.02))+
    labs(x = "", y = "", title = "", caption = "")

ggarrange(g_hs_left, g_hs_right, nrow = 1,
          top = textGrob("Histogram and Density for Standardized Returns, Left and Right Tails", hjust = 0, x = 0.05,
                         gp = gpar(fontface = "bold", fontsize = 12)),
          bottom = textGrob("standard normal distribution is shown in blue", hjust = 0, x = 0.05,
                            gp = gpar(fontsize = 9)))


# AR(1)-GARCH(1,1) model with skewed t distribution

# selected distribution
dis_sel <- "sstd"

# wrappers for distribution functions, with arguments as expected by qqplotr below
ddis <- function(x, mu = 0, sigma = 1, skew = 1, shape = 5) ddist(distribution = dis_sel, y = x, mu, sigma, lambda, skew, shape)
qdis <- function(p, mu = 0, sigma = 1, skew = 1, shape = 5) qdist(distribution = dis_sel, p, mu, sigma, lambda, skew, shape)
rdis <- function(n, mu = 0, sigma = 1, skew = 1, shape = 5) rdist(distribution = dis_sel, n, mu, sigma, lambda, skew, shape)


ar1garch11sstd <- 
    ugarchspec(mean.model = list(armaOrder = c(1, 0)),
               variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
               distribution.model = dis_sel) %>%
    ugarchfit(dataset)

# estimated parameters  
round(ar1garch11sstd@fit$matcoef, 6)

# skewed student t skewness parameter is statistically significantly different from zero. 
# However 0 is not an interesting value to compare with for the skewness parameter. 
# We should compare it with 1, since the the distribution is symmetric 
# if the skewed student t skewenss parameter equals one. 
# Here the estimated skewness parameter is 1.04 and its standard error equals 0.0206. 
# The estimate is thus further than two standard errors away from one 
# and thus we can conclude that the skewness parameter is different from one 
# and that the distribution is thus asymmetric

(ar1garch11sstd@fit$matcoef["skew",][" Estimate"]  - 1) / ar1garch11sstd@fit$matcoef["skew",][" Std. Error"]

# arguments for stat_qq_band, stat_qq_line, stat_qq_point
dis_name <- "dis"
dis_pars <- list(shape = ar1garch11sstd@fit$matcoef["shape", " Estimate"],
                 skew  = ar1garch11sstd@fit$matcoef["skew", " Estimate"])
detrended <- FALSE



# standardized returns - qqplot
ar1garch11sstd %>% residuals(standardize = TRUE) %>% tk_tbl() %>%
    ggplot(mapping = aes(sample = value)) +
        qqplotr::stat_qq_band(distribution = dis_name, dparams = dis_pars, detrend = detrended, alpha = 0.3) +
        qqplotr::stat_qq_line(distribution = dis_name, dparams = dis_pars, detrend = detrended) +
        qqplotr::stat_qq_point(distribution = dis_name, dparams = dis_pars, detrend = detrended) +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
# standardized returns - histogram
g_hs %+% {ar1garch11sstd %>% residuals(standardize = TRUE) %>% tk_tbl()} + 
    stat_function(data = tibble(xsim = c(-3, 3)), aes(x = xsim),
                  fun = ddist, args = c(distribution = dis_sel, dis_pars), 
                  n = 101, color = "steelblue", size = 1, alpha = 0.75) 
    
    

# AR(1)-GJR GARCH(1,1) model with skewed t distribution

# estimate AR(1)-GJR GARCH model
ar1gjrgarch11sstd <- 
    ugarchspec(mean.model = list(armaOrder = c(1,0) ),
               variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
               distribution.model = dis_sel) %>%
    ugarchfit(data = dataset)

# estimated parameters  
round(ar1gjrgarch11sstd@fit$matcoef, 6)

(ar1gjrgarch11sstd@fit$matcoef["skew",][" Estimate"]  - 1) / ar1gjrgarch11sstd@fit$matcoef["skew",][" Std. Error"]

# arguments for stat_qq_band, stat_qq_line, stat_qq_point
dis_name <- "dis"
dis_pars <- list(shape = ar1gjrgarch11sstd@fit$matcoef["shape", " Estimate"],
                 skew  = ar1gjrgarch11sstd@fit$matcoef["skew", " Estimate"])
detrended <- FALSE

# standardized returns - qqplot
ar1gjrgarch11sstd %>% residuals(standardize = TRUE) %>% tk_tbl() %>%
    ggplot(mapping = aes(sample = value)) +
        qqplotr::stat_qq_band(distribution = dis_name, dparams = dis_pars, detrend = detrended, alpha = 0.3) +
        qqplotr::stat_qq_line(distribution = dis_name, dparams = dis_pars, detrend = detrended) +
        qqplotr::stat_qq_point(distribution = dis_name, dparams = dis_pars, detrend = detrended) +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
# standardized returns - histogram
g_hs %+% {ar1gjrgarch11sstd %>% residuals(standardize = TRUE) %>% tk_tbl()} + 
    stat_function(data = tibble(xsim = c(-3, 3)), aes(x = xsim),
                  fun = ddist, args = c(distribution = dis_sel, dis_pars), 
                  n = 101, color = "steelblue", size = 1, alpha = 0.75) 



# GARCH(1,1)-in-Mean model
ar1garch11msstd <- 
    ugarchspec(mean.model = list(armaOrder = c(1,0), archm = TRUE, archpow = 2),
               variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
               distribution.model = dis_sel) %>%
    ugarchfit(data = dataset)

# arguments for stat_qq_band, stat_qq_line, stat_qq_point
dis_pars <- list(shape = ar1garch11msstd@fit$matcoef["shape", " Estimate"],
                 skew  = ar1garch11msstd@fit$matcoef["skew", " Estimate"])

# standardized returns - qqplot
ar1garch11msstd %>% residuals(standardize = TRUE) %>% tk_tbl() %>%
    ggplot(mapping = aes(sample = value)) +
        qqplotr::stat_qq_band(distribution = dis_name, dparams = dis_pars, detrend = detrended, alpha = 0.3) +
        qqplotr::stat_qq_line(distribution = dis_name, dparams = dis_pars, detrend = detrended) +
        qqplotr::stat_qq_point(distribution = dis_name, dparams = dis_pars, detrend = detrended) +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
# standardized returns - histogram
g_hs %+% {ar1garch11msstd %>% residuals(standardize = TRUE) %>% tk_tbl()} + 
    stat_function(data = tibble(xsim = c(-3, 3)), aes(x = xsim),
                  fun = ddist, args = c(distribution = dis_sel, dis_pars), 
                  n = 101, color = "steelblue", size = 1, alpha = 0.75) 


library(fTrading)
stargazer(ar1garch11norm, ar1garch11sstd, ar1gjrgarch11sstd, ar1garch11msstd,
          type = "text", report = "vc*", digits = 4,
          column.sep.width = "0pt", single.row = TRUE, header = FALSE, 
          # model.numbers = FALSE, model.names = FALSE,
          # covariate.labels=c("$\\mu$", "$\\phi_1$", "\\zeta", 
          #                    "$\\omega$", "$\\alpha_1$", "$\\beta_1$", "$\\gamma_1$", "skew", "shape"),
          column.labels = c("AR(1)-GARCH(1,1) norm", "AR(1)-GARCH(1,1) sstd", "AR(1)-GJR GARCH(1,1) sstd", "AR(1)-GARCH(1,1)-M sstd"),
          dep.var.caption = str_c(title_label, " weekly log returns"), dep.var.labels.include = FALSE)


# plot news impact curves
tibble(model = c(ar1garch11sstd, ar1gjrgarch11sstd, ar1garch11msstd),
       model_label = c("GARCH with skewed t innovations", 'GJR GARCH with skewed t innovations', "GARCH-M with skewed t innovations") %>%
           factor(., levels = ., ordered = TRUE)) %>%
    mutate(nic = map(model, ~newsimpact(.x) %>% `[`(c("zx", "zy")) %>% as_tibble())) %>%
    select(model_label, nic) %>%
    unnest() %>%
    group_by(model_label) %>%
    mutate(zy = zy - min(zy)) %>%
    ungroup() %>%
    ggplot(aes(x = zx, y = zy, color = model_label)) +
        geom_line(size = 0.8) +
        scale_color_manual(values = c("black", brewer.pal(3, "Dark2")[1:2])) +
        labs(title = "News Impact Curve",
             caption = TeX("Note: All curves are normalized and plotted as $\\sigma_t^2(\\epsilon_{t-1}) - \\sigma_t^2(0)$"),
             x = expression(epsilon[t - 1]), y = "", col = "") +
        theme(legend.background = element_blank(),
              legend.position = c(0.85, 0.9))







# predicted mean returns and volatility
ar1garch11norm_mean <- fitted(ar1garch11norm)
ar1garch11norm_vol <- sigma(ar1garch11norm)

ar1garch11sstd_mean <- fitted(ar1garch11sstd)
ar1garch11sstd_vol <- sigma(ar1garch11sstd)

ar1gjrgarch11sstd_mean <- fitted(ar1gjrgarch11sstd)
ar1gjrgarch11sstd_vol <- sigma(ar1gjrgarch11sstd)

ar1garch11msstd_mean <- fitted(ar1garch11msstd)
ar1garch11msstd_vol <- sigma(ar1garch11msstd)

# correlation between predicted return across models
cor(merge(ar1garch11norm_mean, ar1garch11sstd_mean, ar1gjrgarch11sstd_mean, ar1garch11msstd_mean))
# correlation between predicted volatilities across models
cor(merge(ar1garch11norm_vol, ar1garch11sstd_vol, ar1gjrgarch11sstd_vol, ar1garch11msstd_vol))


# plot fitted standard devations sigma from 
merge(ar1garch11norm_vol, ar1garch11sstd_vol, ar1gjrgarch11sstd_vol, ar1garch11msstd_vol) %>%
    tk_tbl() %>%
    gather(model, value, -index) %>%
    mutate(model = recode(model, 
                          ar1garch11norm_vol = "AR(1) GARCH(1,1) norm",
                          ar1garch11sstd_vol = "AR(1) GARCH(1,1) sstd",
                          ar1gjrgarch11sstd_vol = "AR(1) GJR GARCH(1,1) sstd", 
                          ar1garch11msstd_vol = "AR(1) GARCH(1,1)-M sstd")) %>%
    ggplot(aes(x = index, y = value, col = model)) +
    geom_line() +
    labs(x = "", y = "", color = "",
         title = "") +
    # facet_wrap(~model, ncol = 1, scales = "free_x") + 
    theme(legend.position = c(0.80, 0.99))

