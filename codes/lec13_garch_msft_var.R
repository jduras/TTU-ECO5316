
# dis_sel <- "norm"
# qdis <- function(p, mu = 0, sigma = 1, skew = 1, shape = 5) qdist(distribution = dis_sel, p, mu, sigma, lambda, skew, shape)
# ar1garch11norm_q <- qdis(p = 0.01, mu = 0, sigma = 1) 
# VaR_1pct <- ar1garch11norm_mean + ar1garch11norm_q*ar1garch11norm_vol
# VaR_1pct <- fitted(ar1garch11norm) + ar1garch11norm_q*sigma(ar1garch11norm)

library(scales)

# 1% VaR with N(0,1) innovations
VaR_1pct <- fitted(ar1garch11norm) + qnorm(p = 0.01)*sigma(ar1garch11norm)

dataset_VaR <- cbind(dataset, VaR_1pct) %>%
    tk_tbl()

# dates when the actual loss exceeds the 1% VaR
exceedance <- (dataset < VaR_1pct)

# plot 1% VaR and actual returns
dataset_VaR %>%
    gather(measure, value, c(return, VaR_1pct)) %>%
    ggplot(aes(x = index, y = value, color = measure)) +
    geom_line() +
    geom_point(data = dataset_VaR[exceedance,] %>%
                   gather(measure, value, c(return, VaR_1pct)) %>%
                   dplyr::filter(measure == "return"), 
               aes(x = index, y = value), color = "magenta", size = 1.5) +
    scale_color_manual(values = c("gray50", "red")) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) + 
    labs(x = "", y = "", title =  str_c(title_label, " weekly log returns: 1% VaR and its exceedances")) +
    theme(legend.position = "none")

# fraction of sample where actual return is below 1% VaR (so when actual loss exceeds the 1% VaR)
sum(dataset < VaR_1pct) / length(VaR_1pct)
sum(exceedance) / length(VaR_1pct)




# selected distribution
dis_sel <- "std"

# wrappers for distribution functions, with arguments as expected by qqplotr below
ddis <- function(x, mu = 0, sigma = 1, skew = 1, shape = 5) ddist(distribution = dis_sel, y = x, mu, sigma, lambda, skew, shape)
qdis <- function(p, mu = 0, sigma = 1, skew = 1, shape = 5) qdist(distribution = dis_sel, p, mu, sigma, lambda, skew, shape)
rdis <- function(n, mu = 0, sigma = 1, skew = 1, shape = 5) rdist(distribution = dis_sel, n, mu, sigma, lambda, skew, shape)

ar1garch11std <- 
    ugarchspec(mean.model = list(armaOrder = c(1, 0)),
               variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
               distribution.model = dis_sel) %>%
    ugarchfit(dataset)

# estimated parameters  
round(ar1garch11std@fit$matcoef, 6)

# arguments for stat_qq_band, stat_qq_line, stat_qq_point
dis_name <- "dis"
dis_pars <- list(shape = ar1garch11sstd@fit$matcoef["shape", " Estimate"])
detrended <- FALSE



# standardized returns - qqplot
ar1garch11std %>% residuals(standardize = TRUE) %>% tk_tbl() %>%
    ggplot(mapping = aes(sample = value)) +
    qqplotr::stat_qq_band(distribution = dis_name, dparams = dis_pars, detrend = detrended, alpha = 0.3) +
    qqplotr::stat_qq_line(distribution = dis_name, dparams = dis_pars, detrend = detrended) +
    qqplotr::stat_qq_point(distribution = dis_name, dparams = dis_pars, detrend = detrended) +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# standardized returns - histogram
g_hs %+% {ar1garch11std %>% residuals(standardize = TRUE) %>% tk_tbl()} + 
    stat_function(data = tibble(xsim = c(-3, 3)), aes(x = xsim),
                  fun = ddist, args = c(distribution = dis_sel, dis_pars), 
                  n = 101, color = "steelblue", size = 1, alpha = 0.75) 

ar1garch11std_mean <- fitted(ar1garch11std)
ar1garch11std_vol <- sigma(ar1garch11std)

ar1garch11std_q <-
    qdis(p = 0.01, mu = 0, sigma = 1, 
         shape = ar1garch11msstd@fit$matcoef["shape", " Estimate"]) 

VaR_1pct <- 
    ar1garch11std_mean + ar1garch11std_q*
    sqrt((ar1garch11std@fit$matcoef["shape", " Estimate"]-2)/ar1garch11std@fit$matcoef["shape", " Estimate"])*
    ar1garch11std_vol

dataset_VaR <- cbind(dataset, VaR_1pct) %>%
    tk_tbl()

exceedance <- (dataset < VaR_1pct)

dataset_VaR %>%
    gather(measure, value, c(return, VaR_1pct)) %>%
    ggplot(aes(x = index, y = value, color = measure)) +
    geom_line() +
    geom_point(data = dataset_VaR[exceedance,] %>%
                   gather(measure, value, c(return, VaR_1pct)) %>%
                   dplyr::filter(measure == "return"), 
               aes(x = index, y = value), color = "magenta", size = 1.5) +
    scale_color_manual(values = c("gray50", "red")) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) + 
    labs(x = "", y = "", title =  str_c(title_label, " weekly log returns: 1% VaR and its exceedances")) +
    theme(legend.position = "none")

sum(dataset < VaR_1pct) / length(VaR_1pct)





ar1garch11msstd_q <-
    qdis(p = 0.01, mu = 0, sigma = 1, 
         skew = ar1garch11msstd@fit$matcoef["skew", " Estimate"], 
         shape = ar1garch11msstd@fit$matcoef["shape", " Estimate"]) 

VaR_1pct <- 
    ar1garch11msstd_mean + ar1garch11msstd_q*
    sqrt((ar1garch11msstd@fit$matcoef["shape", " Estimate"]-2)/ar1garch11msstd@fit$matcoef["shape", " Estimate"])*
    ar1garch11msstd_vol


dataset_VaR <- cbind(dataset, VaR_1pct) %>%
    tk_tbl()

exceedance <- (dataset < VaR_1pct)

dataset_VaR %>%
    gather(measure, value, c(return, VaR_1pct)) %>%
    ggplot(aes(x = index, y = value, color = measure)) +
        geom_line() +
        geom_point(data = dataset_VaR[exceedance,] %>%
                       gather(measure, value, c(return, VaR_1pct)) %>%
                       dplyr::filter(measure == "return"), 
                   aes(x = index, y = value), color = "magenta", size = 1.5) +
        scale_color_manual(values = c("gray50", "red")) +
        scale_y_continuous(labels = percent_format(accuracy = 1)) + 
        labs(x = "", y = "", title =  str_c(title_label, " weekly log returns: 1% VaR and its exceedances")) +
        theme(legend.position = "none")

sum(dataset < VaR_1pct) / length(VaR_1pct)
