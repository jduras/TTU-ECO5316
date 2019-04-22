
# Johansen and Juselius (1990)
# data from Denmark, 1974Q1 to 1987Q3, used to investigate cointegration in money demand function

# load packages
library(magrittr)
library(tidyverse)
library(timetk)
library(lubridate)
library(urca)
library(vars)
library(ggfortify)
library(qqplotr)
library(egg)
library(plotly)

# set default theme for ggplot2
theme_set(theme_bw() +
          theme(strip.text.x = element_text(hjust = 0),
                strip.text.y = element_text(hjust = 1),
                axis.ticks = element_blank(),
                strip.background = element_blank()))


#### Data ####

data(denmark)
str(denmark)
?denmark

# convert data into tibble
denmark_tbl <-
    denmark %>%
    as_tibble() %>%
    mutate(yearq = as.yearqtr(ENTRY, format = "%Y:%q")) %>%
    dplyr::select(c(yearq, LRM, LRY, IBO, IDE))

# convert data into a zoo
# note: everything works with zoo, except for autoplot of VAR model forecast and autoplot of data with facets = FALSE
denmark_zoo <-
    denmark_tbl %>%
    tk_zoo(select = -yearq, date_var = yearq)

# convert data into ts
denmark_ts <-
    denmark_tbl %>%
    tk_ts(select = -yearq, start = year(.$yearq[1]), frequency = 4)


# plot data
autoplot(denmark_ts)

denmark_tbl %>%
    gather(variable, level, -yearq) %>%
    group_by(variable) %>%
    mutate(difference = level - lag(level)) %>%
    ungroup() %>%
    gather(measure, observation, -yearq, -variable) %>%
    mutate(zero = if_else(measure == "difference", 0, NA_real_)) %>%
    gather(component, value, c(observation, zero)) %>%
    mutate(variable_label = case_when(variable == "IBO" ~ "Bond rate (IBO)",
                                      variable == "IDE" ~ "Bank deposit rate (IDE)",
                                      variable == "LRM" ~ "Log of real money M2 (LRM)",
                                      variable == "LRY" ~ "Log of real income (LRY)"),
           measure_label = measure %>% factor(levels = c("level", "difference"), ordered = TRUE),
           combined_label = str_c(variable_label, as.numeric(measure_label), sep = ", ")) %>%
    ggplot(aes(x = yearq, y = value, color = component, size = component)) +
        geom_line() +
        scale_x_yearqtr() +
        scale_color_manual(values = c("black", "gray60")) +
        scale_size_manual(values = c(0.8, 0.3)) +
        labs(x = "", y = "", title = "Data set for Denmark from Johansen & Juselius (1990)") +
        facet_wrap(~combined_label, ncol = 4, scales = "free", strip.position = "top", dir = "v",
                   labeller = labeller(combined_label = as_labeller(c(`Bond rate (IBO), 1` = "Bond rate (IBO) \n\n",
                                                                      `Bank deposit rate (IDE), 1` = "Bank deposit rate (IDE) \n\nlevels", 
                                                                      `Log of real money M2 (LRM), 1` = "Log of real money M2 (LRM) \n\n", 
                                                                      `Log of real income (LRY), 1` = "Log of real income (LRY) \n\n",
                                                                      `Bond rate (IBO), 2` = "",
                                                                      `Bank deposit rate (IDE), 2` = " \n\nfirst difference", 
                                                                      `Log of real money M2 (LRM), 2` = "", 
                                                                      `Log of real income (LRY), 2` = "")),
                                       .multi_line = FALSE)) +
        theme(legend.position = "none",
              strip.text.y = element_text(angle = 180, hjust = 0),
              strip.placement = "outside")
    


#### Unit Root Tests ####

# unit root tests - levels

# approach 1: ADF, ERS, KPSS tests for levels of LRM
denmark_ts[,"LRM"] %>% ur.df(lags = 12, selectlags = "AIC", type = "trend") %>% summary()
denmark_ts[,"LRM"] %>% ur.ers(type = "P-test", lag.max = 8, model = "trend") %>% summary()
denmark_ts[,"LRM"] %>% ur.kpss(lags = "short", type = "tau") %>% summary()

# approach 2: ADF, ERS, KPSS tests for levels LRM - same as above, but more compact
denmark_ts[,"LRM"] %T>%
    {ur.df(., lags = 12, selectlags = "AIC", type = "trend") %>% summary() %>% print()} %T>%
    {ur.ers(., type = "P-test", lag.max = 8, model = "trend") %>% summary() %>% print()} %>%
    {ur.kpss(., lags = "short", type = "tau") %>% summary()}

# approach 3: define a fuction ur.tests that performs ADF, ERS, KPSS tests for first differences of a given time series
# see also the following video when and why you should write a function
# https://campus.datacamp.com/courses/writing-functions-in-r/when-and-how-you-should-write-a-function?ex=1
ur.tests <- function(x, spec) {
        spec <- tibble(adf = if_else(spec == "c", "drift", "trend"),
                       ers = if_else(spec == "c", "constant", "trend"),
                       kpss = if_else(spec == "c", "mu", "tau"))
        x %T>%
            {ur.df(., lags = 12, selectlags = "AIC", type = spec$adf) %>% summary() %>% print()} %T>%
            {ur.ers(., type = "P-test", lag.max = 8, model = spec$ers) %>% summary() %>% print()} %>%
            {ur.kpss(., lags = "short", type = spec$kpss) %>% summary()}
}

# use ur.tests to perform ADF, ERS, KPSS tests for levels of LRM, LRY, IBO, IDE
denmark_ts[,"LRM"] %>% ur.tests(spec = "t")
denmark_ts[,"LRY"] %>% ur.tests(spec = "t")
denmark_ts[,"IBO"] %>% ur.tests(spec = "c")
denmark_ts[,"IDE"] %>% ur.tests(spec = "c")

# use ur.tests and purrr to perform ADF, ERS, KPSS tests for levels of LRM, LRY, IBO, IDE
library(purrr)
map2(tk_tbl(denmark_zoo) %>% 
         dplyr::select(-index), 
     c("t", "t", "c", "c"), 
     ur.tests)



# unit root tests - first differences

# perform ADF, ERS, KPSS tests for first differences of LRM, LRY, IBO, IDE
denmark_ts[,"LRM"] %>% diff() %>% ur.tests(spec = "c")
denmark_ts[,"LRY"] %>% diff() %>% ur.tests(spec = "c")
denmark_ts[,"IBO"] %>% diff() %>% ur.tests(spec = "c")
denmark_ts[,"IDE"] %>% diff() %>% ur.tests(spec = "c")

# use ur.tests and purrr to perform ADF, ERS, KPSS tests for first differences of LRM, LRY, IBO, IDE
library(purrr)
map2(tk_tbl(denmark_zoo) %>% 
         dplyr::select(-index) %>%
         transmute_all(funs(. - lag(.))) %>%
         drop_na(),
     c("t", "t", "c", "c"), 
     ur.tests)



#### Cointegration test - Johansen's methodology ####

denmark_ca <- ca.jo(denmark_ts, ecdet = "const", type = "trace", K = 4, spec = "transitory", season = 4)
summary(denmark_ca)
denmark_ca <- ca.jo(denmark_ts, ecdet = "const", type = "eigen", K = 4, spec = "transitory", season = 4)
summary(denmark_ca)



#### Vector Error Correction Model ####

denmark_vec <- cajorls(denmark_ca, r = 1)
denmark_vec
# to see t-statistics and p-values
summary(denmark_vec$rlm)

# plot residuals and their ACF and PACF
plotres(denmark_ca)

# inspect the structure of denmark_vec
str(denmark_vec)
names(denmark_vec)
names(denmark_vec$rlm)

# QQ plots for residuals
g1 <- ggplot(data = as_tibble(denmark_vec$rlm$residuals[,"LRM.d"]), mapping = aes(sample = value)) +
    stat_qq_band(alpha = 0.3, conf = 0.95) +
    stat_qq_line() +
    stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Residuals in Equation for Log of Real Money")
g2 <- ggplot(data = as_tibble(denmark_vec$rlm$residuals[,"LRY.d"]), mapping = aes(sample = value)) +
    stat_qq_band(alpha = 0.3, conf = 0.95) +
    stat_qq_line() +
    stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Residuals in Equation for Log of Real Income")
g3 <- ggplot(data = as_tibble(denmark_vec$rlm$residuals[,"IBO.d"]), mapping = aes(sample = value)) +
    stat_qq_band(alpha = 0.3, conf = 0.95) +
    stat_qq_line() +
    stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Residuals in Equation for Bond Rate")
g4 <- ggplot(data = as_tibble(denmark_vec$rlm$residuals[,"IDE.d"]), mapping = aes(sample = value)) +
    stat_qq_band(alpha = 0.3, conf = 0.95) +
    stat_qq_line() +
    stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Residuals in Equation for Bank Deposit Rate")
ggarrange(g1, g2, g3, g4, ncol = 2)

# same as above but using imap from purrr 
g_lst <-
    denmark_vec %>%
    pluck("rlm", "residuals") %>%
    as_tibble() %>%
    rename(`Residuals in Equation for Log of Real Money (LRM.d)` = LRM.d,
           `Residuals in Equation for Log of Real Income (LRY.d)` = LRY.d,
           `Residuals in Equation for Bond Rate (IBO.d)` = IBO.d,
           `Residuals in Equation for Bank Deposit Rate (IDE.d)` = IDE.d) %>%
    imap(~.x %>%
             enframe() %>%
             ggplot(mapping = aes(sample = value)) +
                 stat_qq_band(alpha = 0.3, conf = 0.95) +
                 stat_qq_line() +
                 stat_qq_point() +
                 labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = .y))
ggarrange(plots = g_lst)



#### Vector Error Correction Model: Testing Restrictions ####

# test for restricted constant in cointegration relationship rather than  a drift
lttest(denmark_ca, r = 1)

# test for restricted cointegrating vector betta
#  H0: betta2 = -betta1
# so the velocity of money v=Y/(M/p) is a function of interest rates
rest_betta1 <- matrix(data = c(1,-1,0,0,0,
                               0,0,1,0,0,
                               0,0,0,1,0,
                               0,0,0,0,1),
                      nrow = 5, ncol = 4)
denmark_ca_rbetta1 <- blrtest(denmark_ca, H = rest_betta1, r = 1)
summary(denmark_ca_rbetta1)

#  H0: betta2 = -betta1 and betta4 = -betta3
# so the velocity of money v=pY/M is a function of interest rates spread ibonds-ideposits
rest_betta2 <- matrix(data = c(1,-1,0,0,0,
                               0,0,1,-1,0,
                               0,0,0,0,1),
                      nrow = 5, ncol = 3)
denmark_ca_rbetta2 <- blrtest(denmark_ca, H = rest_betta2, r = 1)
summary(denmark_ca_rbetta2)

# test for restricted adjustment parameters alpha
rest_alpha <- matrix(c(1,0,0,0), c(4,1))
denmark_ca_ralpha <- alrtest(denmark_ca, A = rest_alpha, r = 1)
summary(denmark_ca_ralpha)

# joint test for restricted adjustment parameters alpha and restricted cointegrating vector betta
denmark_ca_rboth1 <- ablrtest(denmark_ca, A = rest_alpha, H = rest_betta1, r = 1)
summary(denmark_ca_rboth1)
denmark_ca_rboth2 <- ablrtest(denmark_ca, A = rest_alpha, H = rest_betta2, r = 1)
summary(denmark_ca_rboth2)

# restricted VEC model - note that cajorls ignores restrictions on alpha and only implements restrictions on betta
denmark_vec_rbetta1 <- cajorls(denmark_ca_rbetta1, r = 1)
denmark_vec_rbetta1
summary(denmark_vec_rbetta1$rlm)

denmark_vec_rboth1 <- cajorls(denmark_ca_rboth1, r = 1)
denmark_vec_rboth1
summary(denmark_vec_rboth1$rlm)

# cajorlsX is a modified version of cajorls that implements restrictions on alpha, but only allows r=1 and simple 0/1 restrictions
source("cajorlsX.r")
denmark_vec_rboth1 <- cajorlsX(denmark_ca_rboth1)
denmark_vec_rboth1


# use stargazer ot print out the results for restricted VEC
lms <- denmark_vec_rboth1$vecresult
library(stargazer)
stargazer(lms, type = "text")
stargazer(lms$LRM.d, lms$LRY.d, lms$IBO.d, lms$IDE.d, type = "text")



# to be able to construct forecasts, IRFs and FEVDs, we need to transform the estimated VEC model in differences into a VAR in levels
# note that vec2var function does not support restricted VEC models
denmark_var <- vec2var(denmark_ca, r = 1)

# vec2varX supports restricted VEC models
source("vec2varX.r")
denmark_var <- vec2varX(denmark_ca, r = 1)
denmark_var_rboth1 <- vec2varX(denmark_ca, A = rest_alpha, H = rest_betta2, r = 1)

# multivariate Jarque-Bera test for normality of the residuals of the VAR models
normality.test(denmark_var)
normality.test(denmark_var_rboth1)



#### Forecasts ####

# forecasts using VAR in levels
denmark_var_f <- predict(denmark_var, n.ahead = 8)
denmark_var_rboth1_f <- predict(denmark_var_rboth1, n.ahead = 8)

# plot forecasts
par(mar=c(4,4,2,1), cex=0.75)
plot(denmark_var_f)
plot(denmark_var_rboth1_f)

fanchart(denmark_var_f)

autoplot(denmark_var_f)
autoplot(denmark_var_rboth1_f)



#### Impulse Response Functions ####

denmark_var_irf <- irf(denmark_var, n.ahead = 30, boot = TRUE)

# plot IRFs using plot from vars package
par(mfcol=c(4,4))
plot(denmark_var_irf, plot.type = "single", ask = FALSE)

# arrange IRF data into a tibble to be used with ggplot
denmark_var_irf.tbl <-
    denmark_var_irf[1:3] %>%
    modify_depth(2, as_tibble) %>%
    modify_depth(1, bind_rows, .id = "impulse") %>%
    map_df(bind_rows, .id = "key") %>%
    gather(response, value, -key, -impulse) %>%
    group_by(key, impulse, response) %>%
    mutate(lag = row_number()) %>%
    ungroup() %>%
    mutate(impulse = factor(impulse, levels = c("LRM","LRY","IBO","IDE"), ordered = TRUE),
           response = factor(response, levels = c("LRM","LRY","IBO","IDE"), ordered = TRUE)) %>%
    spread(key, value)

# plot IRFs using ggplot
g <- ggplot(data = denmark_var_irf.tbl, aes(x = lag, y = irf)) +
    geom_ribbon(aes(x = lag, ymin = Lower, ymax = Upper), fill = "gray50", alpha = .3) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "", y = "", title = "Orthogonal Impulse Response Functions (rows: response, columns: impulse)") +
    facet_grid(response ~ impulse, scales = "free_y", switch = "y") +
    theme(strip.text.y = element_text(angle = 180))
g

# plot IRFs using plotly
ggplotly(g)



#### Forecast Error Variance Decomposition ####

# FEVD - based on Choleski decomposition of variance-covariance matrix var(e)
denmark_var_FEVD <- fevd(denmark_var)

# plot FEVDs using plot from vars package
plot(denmark_var_FEVD)

# to plot FEVD using ggplot
# first arrange FEVD data into a tibble to be used with ggplot
# note that variable and shock are converted into ordered factors, to present them in the same order as in the VAR
denmark_var_FEVD.tbl <-
    denmark_var_FEVD %>%
    modify_depth(1, as_tibble) %>%
    map_df(bind_rows, .id = "variable") %>%
    gather(shock, value, -variable) %>%
    mutate(variable = factor(variable, levels = c("LRM","LRY","IBO","IDE"), ordered = TRUE),
           shock = factor(shock, levels = c("LRM","LRY","IBO","IDE"), ordered = TRUE)) %>%
    group_by(shock, variable) %>%
    mutate(horizon = row_number()) %>%
    ungroup()

library(wesanderson)
library(RColorBrewer)
g <- ggplot(data = denmark_var_FEVD.tbl, aes(x = horizon, y = value, fill = shock)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    # scale_fill_manual(values = c("gray20","gray40","gray60","gray80")) +
    scale_fill_manual(values = wes_palette("Moonrise1")) +
    # scale_fill_manual(values = wes_palette("Moonrise2")[c(4,1,2,3)]) +
    # scale_fill_manual(values = wes_palette("FantasticFox1")[c(5,3,1,2)]) +
    # scale_fill_manual(values = wes_palette("GrandBudapest1")[c(3,2,4,1)]) +
    # scale_fill_manual(values = brewer.pal(4, "Set3")) +
    labs(x = "horizon", y = "fraction of overall variance", title = "Forecast Error Variance Decomposition") +
    facet_grid(variable ~ ., switch = "y") +
    theme(strip.text.y = element_text(angle = 0))
g

# plot FEVD using plotly
ggplotly(g)
