
# see also lec09JNJ.r

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggfortify)
library(egg)
library(tsibble)
library(timetk)
library(lubridate)
library(zoo)
library(forecast)
library(broom)
library(sweep)

# set default theme for ggplot2
theme_set(theme_bw() + 
          theme(strip.background = element_blank()))

# import the data on earnings per share for Johnson and Johnson,
# then construct log, change, log-change, seasonal log change
jnj_tbl_all <-
    # read_table("http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/q-jnj.txt", col_names = "y") %>%
    read_table("data/q-jnj.txt", col_names = "y") %>%
    ts(start = c(1960,1), frequency = 4) %>%
    tk_tbl(rename_index = "yearq") %>%
    mutate(yearq = as.yearqtr(yearq),
           ly = log(y),
           dy = y - lag(y),
           dly1 = ly - lag(ly),
           dly4 = ly - lag(ly, 4),
           d2ly4_1 = dly4 - lag(dly4))

# split sample into two parts - estimation sample and prediction sample
fstQ <- 1960.00  # 1960Q1
lstQ <- 1978.75  # 1978Q4
jnj_tbl_1 <- jnj_tbl_all %>%
    filter(yearq <= as.yearqtr(lstQ))

m2 <- jnj_tbl_1 %>%
    tk_ts(select = ly, start = fstQ, frequency = 4) %>%
    auto.arima(ic = "aicc", seasonal = TRUE, approximation = FALSE, stepwise = FALSE)
ggtsdiag(m2, gof.lag = 24)

# construct 1-quarter to 12-quarters ahead forecasts
hmax <- 12
m2_f_1_to_hmax <- forecast(m2, hmax)

# plot 1-quarter to 12-quarters ahead forecasts - logs
autoplot(m2_f_1_to_hmax) +
    labs(x = "", y = "",
         title = "Log of Earnings per share for Johnson and Johnson: Multistep Forecast")

# extract the multistep forecasts, convert to levels
jnj_tbl_f_1_to_hmax <-
    m2_f_1_to_hmax %>%
    sw_sweep(rename_index = "yearq") %>%
    filter(key == "forecast") %>%
    mutate(yearq = as.yearqtr(yearq)) %>%
    mutate_at(vars(ly, lo.80, lo.95, hi.80, hi.95), exp) %>%
    rename(y = ly) %>%
    select(yearq, key, y, lo.80, lo.95, hi.80, hi.95)

# forecast & actual data in a single tibble
jnj_tbl_f_1_to_hmax <- 
    bind_rows(jnj_tbl_all %>%
                  mutate(key = "actual") %>%
                  select(yearq, key, y),
              jnj_tbl_f_1_to_hmax) %>%
    mutate(yearq = as.yearqtr(yearq))

# plot 1-quarter to 12-quarters ahead forecasts - levels
jnj_tbl_f_1_to_hmax %>%
    filter(yearq >= as.yearqtr(1970)) %>%
    ggplot(aes(x = yearq, y = y, col = key, linetype = key)) +
        geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "blue", alpha = 0.1) +
        geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "blue", alpha = 0.2) +
        geom_line() +
        geom_point() +
        scale_x_yearqtr() +
        scale_color_manual(values = c("gray40", "darkblue")) +
        scale_linetype_manual(values = c("solid", "solid")) +
        labs(x = "", y = "",
             title = "Earnings per share for Johnson and Johnson: Multistep Forecast") +
        theme(legend.position = "none")



# window length for rolling SARIMA
window.length <- nrow(jnj_tbl_1)

# estimate rolling SARIMA model, create 1 step ahead forecasts
jnj_sarima_roll <-
    jnj_tbl_all %>%
    as_tsibble(index = yearq) %>%                                                                                           # covert to tsibble
    mutate(sarima_mod = slide2(yearq, ly, 
                         ~tibble(yearq = .x, ly = .y) %>%                                                                   # rolling tibble with yearq and ly columsn
                             tk_ts(select = ly, start = c(year(.$yearq[1]), quarter(.$yearq[1])), frequency = 4) %>%        # rolling ts
                             auto.arima(ic = "aicc", seasonal = TRUE, approximation = FALSE, stepwise = FALSE),             # estimate models
                         .size = window.length)) %>%
    filter(!is.na(sarima_mod)) %>% 
    mutate(sarima_coefs = map(sarima_mod, tidy, conf.int = TRUE),
           sarima_f = map(sarima_mod, (. %>% forecast(1) %>% sw_sweep())))
jnj_sarima_roll

# extract coefficients
coefs_tbl <- jnj_sarima_roll %>%
    select(yearq, sarima_coefs) %>%
    as_tibble() %>%
    unnest() %>% 
    as_tsibble(key = id("term"), index = yearq) 

# plot estimated coefficients with confidence intervals
coefs_tbl %>%
    ggplot(aes_(x = ~yearq, y = ~estimate, group = ~term)) +
        geom_line(color = "royalblue", linetype = "dotted") +
        geom_point(color = "royalblue") +
        geom_ribbon(aes(x = yearq, ymin = conf.low, ymax = conf.high), alpha = 0.5, fill = "lightblue") +
        geom_hline(yintercept = 0, color = "black")+
        scale_x_yearqtr() +
        labs(x = "", y = "",
             title = "Coefficient estimates",
             subtitle = paste(window.length, "month rolling window model"))+
        facet_wrap(~term, nrow = 1) 

# inspect implicit missingness
coefs_tbl %>%
    has_gaps(.full = TRUE)

# plot showing in which period coefficients are missing
coefs_tbl %>%
    count_gaps(.full = TRUE) %>%
    filter(term != "intercept") %>%
    ggplot(aes(x = term)) +
    geom_linerange(aes(ymin = .from, ymax = .to)) +
    geom_point(aes(y = .from)) +
    geom_point(aes(y = .to)) +
    coord_flip()

# plot estimated coefficients with confidence interval ribbons
coefs_tbl %>%
    fill_gaps(.full = TRUE) %>%
    ggplot(aes(x = yearq, y = estimate, group = term)) +
    geom_line(color = "royalblue", linetype = "dotted") +
    geom_point(color = "royalblue") +
    geom_ribbon(aes(x = yearq, ymin = conf.low, ymax = conf.high), alpha = 0.5, fill = "lightblue") +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_yearqtr() +
    labs(x = "", y = "",
         title = "Coefficient estimates",
         subtitle = paste(window.length, "month rolling window model"))+
    facet_wrap(~term, nrow = 1) 

# extract the 1 period ahead rolling forecasts, convert to levels
m2_f_1_rol <-
    jnj_sarima_roll %>%
    select(yearq, sarima_f) %>%
    as_tibble() %>%
    unnest(sarima_f) %>%
    filter(key == "forecast") %>%
    mutate(yearq = yearq %>% as.Date() %m+% months(3) %>% as.yearqtr()) %>%
    mutate_at(vars(ly, lo.80, lo.95, hi.80, hi.95), exp) %>%
    rename(y = ly) %>%
    select(yearq, key, y, lo.80, lo.95, hi.80, hi.95)

# forecast & actual data in a single tibble
jnj_tbl_f_1_rol <- 
    bind_rows(jnj_tbl_all %>%
                  mutate(key = "actual") %>%
                  select(yearq, key, y),
              m2_f_1_rol) %>%
    mutate(yearq = as.yearqtr(yearq))

# plot 1-quarter ahead rolling forecasts - levels
jnj_tbl_f_1_rol %>%
    filter(yearq >= as.yearqtr(1970)) %>%
    ggplot(aes(x = yearq, y = y, col = key)) +
        geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "blue", alpha = 0.1) +
        geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "blue", alpha = 0.2) +
        geom_line() +
        geom_point() +
        scale_x_yearqtr() +
        scale_color_manual(values = c("gray40","darkblue")) +
        labs(x = "", y = "",
             title = "Earnings per share for Johnson and Johnson: 1-Step Ahead Rolling Forecast") +
        theme(legend.position = "none")


# plot 1-quarter ahead rolling forecasts - levels - alternative way with m2_f_1_rol but extra geom_line
m2_f_1_rol %>%
    filter(yearq >= as.yearqtr(1970)) %>%
    ggplot(aes(x = yearq, y = y, col = key)) +
        geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "blue", alpha = 0.1) +
        geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "blue", alpha = 0.2) +
        geom_line() +
        geom_line(data = jnj_tbl_all %>% mutate(key = "actual") %>% filter(yearq > as.yearqtr(1970)), aes(x = yearq, y = y)) +
        geom_point() +
        scale_x_yearqtr() +
        scale_color_manual(values = c("gray40","darkblue")) +
        labs(x = "", y = "",
             title = "Earnings per share for Johnson and Johnson: 1-Step Ahead Rolling Forecast") +
        theme(legend.position = "none")


# convert actual data in prediction sample into ts format
jnj_ts_2 <- jnj_tbl_all %>%
    filter(yearq > as.yearqtr(lstQ)) %>%
    tk_ts(select = y, start = lstQ+0.25, frequency = 4)

# evaluate accuracy of forecasts - multistep forecast - logs
accuracy(m2_f_1_to_hmax$mean, log(jnj_ts_2))
# evaluate accuracy of forecasts - 1 step ahead rolling scheme forecast - logs
accuracy(log(m2_f_1_rol$y), log(jnj_ts_2))

# evaluate accuracy of forecasts - multistep forecast - levels
accuracy(exp(m2_f_1_to_hmax$mean), jnj_ts_2)
# evaluate accuracy of forecasts - 1 step ahead rolling scheme forecast - levels
accuracy(m2_f_1_rol$y, jnj_ts_2)
