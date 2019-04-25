
# example of VEC - long run and short run interest rate, Enders p. 397

# load packages
library(magrittr)
library(tidyquant)
library(broom)
library(timetk)
library(tibbletime)
library(tsibble)
library(lubridate)
library(urca)
library(vars)
library(ggfortify)
library(plotly)

# set default theme for ggplot2
theme_set(theme_bw() +
          theme(strip.text.x = element_text(hjust = 0),
                strip.text.y = element_text(hjust = 1),
                axis.ticks = element_blank(),
                strip.background = element_blank()))


### Data ###

# data from Enders, import quarterly macrodata for U.S., extract 5-year interest rate and 3 month Treasury bill
r_tbl <-
    read_tsv("data/quarterly.tsv") %>%
    mutate(yearq = as.yearqtr(DATE, format = "%YQ%q")) %>%
    rename(tbill = Tbill) %>%
    dplyr::select(yearq, r5, tbill)
r_tbl

# FRED data
r_tbl <-
    tq_get(c("GS5", "TB3MS"), get = "economic.data",
           from = "1960-01-01", to = "2018-12-31") %>%
    mutate(yearq = as.yearqtr(date, format = "%Y-%m-%d")) %>%
    group_by(symbol, yearq) %>%
    summarise_at("price", mean) %>%
    ungroup() %>%
    spread(symbol, price) %>%
    rename(r5 = GS5,
           tbill = TB3MS)
r_tbl

# convert data into a zoo
# note: everything works with zoo, except for autoplot of VAR model forecast and autoplot of data with facets = FALSE
r_zoo <-
    r_tbl %>%
    dplyr::filter(yearq <= as.yearqtr("2012 Q4")) %>%
    tk_zoo(select = -yearq, date_var = yearq)

# convert data into a ts
r_ts <-
    r_tbl %>%
    dplyr::filter(yearq <= as.yearqtr("2012 Q4")) %>%
    tk_ts(select = -yearq, start = year(.$yearq[1]), frequency = 4)

# plot data
autoplot(r_ts)
autoplot(r_ts, facets = FALSE) + labs(col = "")

r_tbl %>%
    gather(variable, value, -yearq) %>%
    mutate(variable_label = case_when(variable == "r5" ~ "5-year interest rate",
                                      variable == "tbill" ~ "3 month Treasury bill rate")) %>%
    ggplot(aes(x = yearq, y = value, col = variable_label)) +
    geom_line() +
    scale_x_yearqtr() +
    labs(x = "", y = "", col = "") +
    theme(legend.position = c(0.18, 0.88),
          legend.background = element_blank())



# Engle-Granger approach using standard OLS
lm(r_ts[,1] ~ r_ts[,2]) %T>% 
    {summary(.) %>% print()} %>% 
    {residuals(.) %>% ur.df() %>% summary() %>% print()}

lm(r_ts[,2] ~ r_ts[,1]) %T>% 
    {summary(.) %>% print()} %>% 
    {residuals(.) %>% ur.df() %>% summary() %>% print()}



#### Cointegration test - Johansen's methodology ###

VARselect(r_ts, type = "const", lag.max = 12)

r_ca <- ca.jo(r_ts, type = "eigen", ecdet = "const", K = 2, spec = "transitory")
summary(r_ca)
r_ca <- ca.jo(r_ts, type = "trace", ecdet = "const", K = 2, spec = "transitory")
summary(r_ca)

# test for the presence of the constant term in the cointegration relationship
lttest(r_ca, r = 1)



#### Vector Error Correction Model ###

# unrestricted VEC
r_vec <- cajorls(r_ca, r = 1)
r_vec
summary(r_vec$rlm)

# r_vec$beta
# r_vec$rlm$coefficients

# test restriction on betta
rest_betta <- matrix(data = c(1,-1,0,0,0,1) , nrow = 3, ncol = 2)
rest_betta
r_ca_rest <- blrtest(r_ca, H = rest_betta, r = 1)
summary(r_ca_rest)

# VEC with restriction on betta
r_vec_rest <- cajorls(r_ca_rest, r = 1)
r_vec_rest
summary(r_vec_rest$rlm)



#### Vector Error Correction Model: Forecast ###

# transform VEC to VAR
r_var <- vec2var(r_ca, r = 1)
source("codes/vec2varX.r")
r_var <- vec2varX(r_ca, r = 1)
r_var_rbetta <- vec2varX(r_ca, H = rest_betta, r = 1)

r_var_f <- predict(r_var, n.ahead = 8)
r_var_rbetta_f <- predict(r_var_rbetta, n.ahead = 8)

# plot forecast
autoplot(r_var_f) + facet_wrap(~ variable, ncol = 1)
autoplot(r_var_rbetta_f) + facet_wrap(~ variable, ncol = 1)



#### Vector Error Correction Model: Rolling Estimation ###

# define rolling version of a function that tests for cointegration
window_length <- nrow(r_ts)
rolling_ca.jo <-
    rollify(
        function(r5, tbill) {
            cbind(r5, tbill) %>% ca.jo(type = "eigen", ecdet = "const", K = 2, spec = "transitory")
        },
        window = window_length, unlist = FALSE)

# rolling estimation - using tibbletime
vec_roll_results <-
    r_tbl %>%
    mutate(date = as.Date(yearq)) %>%
    tbl_time(index = date) %>%
    mutate(ca = rolling_ca.jo(r5, tbill)) %>%
    filter(!is.na(ca)) %>%
    mutate(vec = map(ca, . %>% cajorls(r = 1)),                                                                       # unrestricted VEC
           ca_rest = map(ca, . %>% blrtest(H = matrix(data = c(1,-1,0,0,0,1) , nrow = 3, ncol = 2), r = 1)),          # test restriction on betta
           vec_rest = map(ca_rest, cajorls),                                                                          # VEC with alpha2 = 0
           var = map(ca, . %>% vec2var(r = 1)),                                                                       # convert VEC to VAR in levels
           var_f = map(var, . %>% predict(n.ahead = 1) %>% pluck("fcst") %>% map(as_tibble) %>% bind_rows(.id = "variable"))      # one step ahead forecast
    )
vec_roll_results

# rolling estimation - using tsibble
vec_roll_results <-
    r_tbl %>%
    mutate(date = as.Date(yearq)) %>%
    as_tsibble(index = yearq) %>%
    mutate(ca = slide2(r5, tbill,
                       ~cbind(r5 = .x, tbill = .y) %>% 
                           ca.jo(type = "eigen", ecdet = "const", K = 2, spec = "transitory"),
                       .size = window_length)) %>%
    filter(!is.na(ca)) %>%
    mutate(vec = map(ca, . %>% cajorls(r = 1)),                                                                       # unrestricted VEC
           ca_rest = map(ca, . %>% blrtest(H = matrix(data = c(1,-1,0,0,0,1) , nrow = 3, ncol = 2), r = 1)),          # test restriction on betta
           vec_rest = map(ca_rest, cajorls),                                                                          # VEC with alpha2 = 0
           var = map(ca, . %>% vec2var(r = 1)),                                                                       # convert VEC to VAR in levels
           var_f = map(var, . %>% predict(n.ahead = 1) %>% pluck("fcst") %>% map(as_tibble) %>% bind_rows(.id = "variable"))      # one step ahead forecast
    ) %>%
    as_tibble()
vec_roll_results


# plot statistic for cointegration test
vec_roll_results %>%
    mutate(ca_test = map(ca, ~{bind_cols(as_tibble(.@cval, rownames = "rank"), tibble(teststat = .@teststat))})) %>%
    dplyr::select(date, ca_test) %>%
    unnest() %>%
    gather(key, value, -date, -rank) %>%
    mutate(rank = str_sub(rank, 1, -3) %>% factor(levels = c("r = 0 ", "r <= 1"), ordered = TRUE),
           key = case_when(key == "10pct"    ~ "10%",
                           key == "5pct"     ~ " 5%",
                           key == "1pct"     ~ " 1%",
                           key == "teststat" ~ "test statistic")) %>%
    ggplot(aes(x = date, y = value, col = key)) +
    geom_line() +
    scale_color_manual(values = c("gray10","gray40","gray70","blue")) +
    labs(x = "", y = "", col = "",
         title = "Cointegration test: critical values and test statistic",
         subtitle = paste(window_length, "quarters rolling window")) +
    facet_wrap(~ rank, scales = "free_y")

# plot estimated beta2
vec_roll_results %>%
    mutate(vec_beta = map(vec, . %>% pluck("beta") %>% as_tibble(rownames = "term"))) %>%
    dplyr::select(date, vec_beta) %>%
    unnest() %>%
    filter(term == "tbill.l1") %>%
    ggplot(aes(x = date, y = ect1)) +
    geom_line(col = "blue") +
    geom_hline(yintercept = -1, linetype = "dashed") +
    labs(x = "", y = "", col = "",
         title = "Cointegrating Relationship: Coefficient for 3-month Treasury Bill",
         subtitle = paste(window_length, "quarters rolling window"))

# plot p-value for the test of a restricted VEC with beta2=-1
vec_roll_results %>%
    dplyr::select(date, ca_rest) %>%
    mutate(ca_rest_pval = map_dbl(ca_rest, ~.@pval[1])) %>%
    ggplot(aes(x = date, y = ca_rest_pval)) +
        geom_hline(yintercept = 0.10, col = "gray70") +
        geom_hline(yintercept = 0.05, col = "gray40") +
        geom_hline(yintercept = 0.01, col = "gray10") +
        geom_line(col = "blue") +
        labs(x = "", y = "", col = "",
             title = "Restricted VEC test: p-value",
             subtitle = paste(window_length, "quarters rolling window"))

# plot estimated coefficients with confidence intervals
g <- vec_roll_results %>%
    # mutate(vec_coefs = map(vec, ~(.$rlm %>% tidy()))) %>%
    mutate(vec_coefs = map(vec_rest, ~(.$rlm %>% tidy()))) %>%
    dplyr::select(date, vec_coefs) %>%
    unnest() %>%
    ggplot(aes(x = date, y = estimate, group = term)) +
        geom_line(color = "royalblue") +
        geom_ribbon(aes(x = date, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), alpha = 0.5, fill = "lightblue") +
        geom_hline(yintercept = 0, color = "black")+
        labs(x = "", y = "",
             title = "Coefficient Estimates for Restricted VEC",
             subtitle = paste(window_length, "month rolling window VEC model"))+
        facet_grid(term  ~ response, scales = "free_y", switch = "y") +
        theme(strip.placement = "outside")
g

ggplotly(g)

# 1 period ahead rolling forecasts
vec_roll_f <-
    bind_rows(
        # actual data
        r_tbl %>%
            mutate(date = as.Date(yearq)) %>%
            dplyr::select(date, r5, tbill) %>%
            gather(variable, value, -date) %>%
            mutate(key = "actual"),
        # forecasts
        vec_roll_results %>%
            dplyr::select(date, var_f) %>%
            unnest(var_f) %>%
            rename(value = fcst) %>%
            mutate(key = "forecast",
                   date = date %m+% months(3))
    ) %>%
    arrange(date, variable) %>%
    mutate(yearq = as.yearqtr(date),
           variable_label = case_when(variable == "r5" ~ "5-year interest rate",
                                  variable == "tbill" ~ "3 month Treasury bill rate"))

# plot the 1 period ahead rolling forecasts
g <- vec_roll_f %>%
    ggplot(aes(x = yearq, y = value, col = key, group = key)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, fill = "steelblue", alpha = 0.2) +
        geom_line() +
        scale_x_yearqtr() +
        scale_color_manual(values = c("black","blue")) +
        labs(x = "", y = "", title = "Rolling one step ahead forecast") +
        facet_wrap(~ variable_label, ncol = 1, scales = "free_y") +
        theme(legend.position = "none")
g

ggplotly(g)
