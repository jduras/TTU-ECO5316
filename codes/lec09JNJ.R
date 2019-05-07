
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

# import the data on earnings per share for Johnson and Johnson
# obtained from http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/q-jnj.txt
# then construct log, change, log-change, seasonal log change
jnj_tbl_all <-
    # read_table("http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/q-jnj.txt", col_names = "y") %>%
    read_table("data/q-jnj.txt", col_names = "y") %>%
    ts(start = c(1960,1), frequency = 4) %>%
    tk_tbl(rename_index = "yearq") %>%
    mutate(yearq = yearquarter(yearq),
           ly = log(y),
           dy = y - lag(y),
           dly1 = ly - lag(ly),
           dly4 = ly - lag(ly, 4),
           d2ly4_1 = dly4 - lag(dly4))

# split sample into two parts - estimation sample and prediction sample
fstQ <- 1960.00  # 1960Q1
lstQ <- 1978.75  # 1978Q4
jnj_tbl_1 <- jnj_tbl_all %>%
    filter(yearq <= yearquarter(lstQ))

# plot time series: levels, logs, differences
jnj_tbl_all %>%
    gather(variable, value, -yearq) %>%
    mutate(variable_label = factor(variable, ordered = TRUE,
                               levels = c("y", "ly", "dy", "dly1", "dly4", "d2ly4_1"),
                               labels = c("y", "log(y)",
                                          expression(paste(Delta,"y")),
                                          expression(paste(Delta,"log(y)")),
                                          expression(paste(Delta[4],"log(y)")),
                                          expression(paste(Delta,Delta[4],"log(y)"))))) %>%
    ggplot(aes(x = yearq, y = value)) +
        geom_hline(aes(yintercept = 0), linetype = "dotted") +
        geom_line() +
        labs(x = "", y = "") +
        facet_wrap(~variable_label, ncol = 3, scales = "free", labeller = label_parsed) +
        theme(strip.text = element_text(hjust = 0),
              strip.background = element_blank())

# plot ACF and PACF
maxlag <-24
g1 <- jnj_tbl_1 %>% pull(ly) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for log(y)")))
g2 <- jnj_tbl_1 %>% pull(dly1) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta,"log(y)")))
g3 <- jnj_tbl_1 %>% pull(dly4) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta[4], "log(y)")))
g4 <- jnj_tbl_1 %>% pull(d2ly4_1) %>% ggAcf(lag.max = maxlag, ylab = "", main = expression(paste("ACF for ", Delta, Delta[4], "log(y)")))
g5 <- jnj_tbl_1 %>% pull(ly) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for log(y)")))
g6 <- jnj_tbl_1 %>% pull(dly1) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta, "log(y)")))
g7 <- jnj_tbl_1 %>% pull(dly4) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta[4], "log(y)")))
g8 <- jnj_tbl_1 %>% pull(d2ly4_1) %>% ggPacf(lag.max = maxlag, ylab = "", main = expression(paste("PACF for ", Delta,Delta[4], "log(y)")))
ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol = 4)



# search for best model using AICc criteria

# this is not correct - jnj_tbl_1$ly is not a ts object so auto.arima does not know its frequency
auto.arima(jnj_tbl_1$ly, ic = "aicc", d = 1, D = 1, seasonal = TRUE, stationary = FALSE, stepwise = FALSE, approximation = FALSE)

# jnj_tbl_1$ly can be first converted to ts using tk_ts before passing it on to auto.arima
jnj_tbl_1 %>%
    tk_ts(select = ly, start = fstQ, frequency = 4) %>%
    auto.arima(ic = "aicc", d = 1, D = 1, seasonal = TRUE, stationary = FALSE, stepwise = FALSE, approximation = FALSE)
jnj_tbl_1 %>%
    tk_ts(select = ly, start = fstQ, frequency = 4) %>%
    auto.arima(ic = "aicc", d = 1, seasonal = TRUE, stationary = FALSE, stepwise = FALSE, approximation = FALSE)
jnj_tbl_1 %>%
    tk_ts(select = ly, start = fstQ, frequency = 4) %>%
    auto.arima(ic = "aicc", seasonal = TRUE, stationary = FALSE, stepwise = FALSE, approximation = FALSE)


# estimate model - twice differenced series, ARIMA(0,1,1)(0,1,1)_4 which is suboptimal based on AICc and BIC, but it is the one used in Tsay's textbook
m1 <- jnj_tbl_1 %>%
    tk_ts(select = ly, start = fstQ, frequency = 4) %>%
    Arima(order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 4))
m1
ggtsdiag(m1, gof.lag = maxlag)

# construct 1-quarter to 12-quarters ahead forecasts
hmax <- 12
m1_f_1_to_hmax <- forecast(m1, hmax)

# plot 1-quarter to 12-quarters ahead forecasts - logs
autoplot(m1_f_1_to_hmax) +
    labs(x = "", y = "",
         title = "Log of Earnings per share for Johnson and Johnson: Multistep Forecast")

# extract the multistep forecasts, convert to levels
jnj_tbl_f_1_to_hmax <-
    m1_f_1_to_hmax %>%
    sw_sweep(rename_index = "yearq") %>%
    filter(key == "forecast") %>%
    mutate(yearq = yearquarter(yearq)) %>%
    mutate_at(vars(ly, lo.80, lo.95, hi.80, hi.95), funs(exp)) %>%
    rename(y = ly) %>%
    select(yearq, key, y, lo.80, lo.95, hi.80, hi.95)

# forecast & actual data in a single tibble
jnj_tbl_f_1_to_hmax <- 
    bind_rows(jnj_tbl_all %>%
                  mutate(key = "actual") %>%
                  select(yearq, key, y),
              jnj_tbl_f_1_to_hmax) %>%
    mutate(yearq = yearquarter(yearq))

# plot 1-quarter to 12-quarters ahead forecasts - levels
jnj_tbl_f_1_to_hmax %>%
    filter(yearq >= yearquarter(1970)) %>%
    ggplot(aes(x = yearq, y = y, col = key, linetype = key)) +
        geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "blue", alpha = 0.1) +
        geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "blue", alpha = 0.2) +
        geom_line() +
        geom_point() +
        scale_color_manual(values = c("gray40","darkblue")) +
        scale_linetype_manual(values = c("solid","solid")) +
        labs(x = "", y = "",
             title = "Earnings per share for Johnson and Johnson: Multistep Forecast") +
        theme(legend.position = "none")



# window length for rolling SARIMA
window.length <- nrow(jnj_tbl_1)

# estimate rolling SARIMA model, create 1 step ahead forecasts
results <-
    jnj_tbl_all %>%
    as_tsibble(index = yearq) %>%                                        # covert to tsibble
    mutate(sarima.model = slide(ly, ~Arima(.x, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 4)),
                                .size = window.length)) %>%             # estimate models
    filter(!is.na(sarima.model)) %>%                                    # remove periods at the beginning of sample where model could not be estimated due to lack of data,
    mutate(sarima.coefs = map(sarima.model, tidy, conf.int = TRUE),
           sarima.fcst = map(sarima.model, (. %>% forecast(1) %>% sw_sweep())))

# extract coefficients
coefs_tbl <- results %>%
    select(yearq, sarima.coefs) %>%
    as_tibble() %>%
    unnest(sarima.coefs) %>%
    as_tsibble(key = id("term"), index = yearq) 

# plot estimated coefficients with confidence intervals
coefs_tbl %>%
    ggplot(aes(x = yearq, y = estimate, group = term)) +
        geom_line(color = "royalblue") +
        geom_ribbon(aes(x = yearq, ymin = conf.low, ymax = conf.high), alpha = 0.5, fill = "lightblue") +
        geom_hline(yintercept = 0, color = "black")+
        labs(x = "", y = "",
             title = "Coefficient estimates",
             subtitle = paste(window.length, "month rolling window model"))+
        facet_wrap(~term, nrow = 1) 

# extract the 1 period ahead rolling forecasts, convert to levels
m1_f_1_rol <-
    results %>%
    select(yearq, sarima.fcst) %>%
    as_tibble() %>%
    unnest(sarima.fcst) %>%
    filter(key == "forecast") %>%
    mutate(yearq = yearq %m+% months(3) %>% yearquarter()) %>%
    mutate_at(vars(value, lo.80, lo.95, hi.80, hi.95), funs(exp)) %>%
    rename(y = value) %>%
    select(yearq, key, y, lo.80, lo.95, hi.80, hi.95)

# forecast & actual data in a single tibble
jnj_tbl_f_1_rol <- 
    bind_rows(jnj_tbl_all %>%
                  mutate(key = "actual") %>%
                  select(yearq, key, y),
              m1_f_1_rol) %>%
    mutate(yearq = yearquarter(yearq))

# plot 1-quarter ahead rolling forecasts - levels
jnj_tbl_f_1_rol %>%
    filter(yearq >= yearquarter(1970)) %>%
    ggplot(aes(x = yearq, y = y, col = key)) +
        geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "blue", alpha = 0.1) +
        geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "blue", alpha = 0.2) +
        geom_line() +
        geom_point() +
        scale_color_manual(values = c("gray40","darkblue")) +
        labs(x = "", y = "",
             title = "Earnings per share for Johnson and Johnson: 1-Step Ahead Rolling Forecast") +
        theme(legend.position = "none")


# plot 1-quarter ahead rolling forecasts - levels - alternative way with m1_f_1_rol but extra geom_line
m1_f_1_rol %>%
    filter(yearq >= yearquarter(1970)) %>%
    ggplot(aes(x = yearq, y = y, col = key)) +
        geom_ribbon(aes(ymin = lo.95, ymax = hi.95), linetype = "blank", fill = "blue", alpha = 0.1) +
        geom_ribbon(aes(ymin = lo.80, ymax = hi.80), linetype = "blank", fill = "blue", alpha = 0.2) +
        geom_line() +
        geom_line(data = jnj_tbl_all %>% mutate(key = "actual") %>% filter(yearq > yearquarter(1970)), aes(x = yearq, y = y)) +
        geom_point() +
        scale_color_manual(values = c("gray40","darkblue")) +
        labs(x = "", y = "",
             title = "Earnings per share for Johnson and Johnson: 1-Step Ahead Rolling Forecast") +
        theme(legend.position = "none")


# convert actual data in prediction sample into ts format
jnj_ts_2 <- jnj_tbl_all %>%
    filter(yearq > yearquarter(lstQ)) %>%
    tk_ts(select = y, start = lstQ+0.25, frequency = 4)

# evaluate accuracy of forecasts - multistep forecast - logs
accuracy(m1_f_1_to_hmax$mean, log(jnj_ts_2))
# evaluate accuracy of forecasts - 1 step ahead rolling scheme forecast - logs
accuracy(log(m1_f_1_rol$y), log(jnj_ts_2))

# evaluate accuracy of forecasts - multistep forecast - levels
accuracy(exp(m1_f_1_to_hmax$mean), jnj_ts_2)
# evaluate accuracy of forecasts - 1 step ahead rolling scheme forecast - levels
accuracy(m1_f_1_rol$y, jnj_ts_2)
