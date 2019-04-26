library(tidyquant)
library(timetk)
library(tsibble)
library(broom)
library(sweep)
library(ggplot2)
library(scales)
library(ggfortify)
library(egg)
library(tictoc)
library(forecast)

# Set default theme for ggplot2
theme_set(theme_minimal())

#(a) Use tq_get to obtain quarterly Real Personal Consumption Expenditures for the 1955Q1-2018Q4 sample, available on FRED under code PCECC96. 

data <-   tq_get("PCECC96", get = "economic.data", from = "1955-01-01", to = "2018-12-21")

# (b) Construct the log changes in the Real Personal Consumption Expenditures ???logct =logct ???logct???1 where ct is the original quarterly Real Personal Consumption Expenditures. 
data_tbl <- data %>%  rename(y = price) %>%  mutate(dly = 4*(log(y) - lag(log(y))))


# plot time series  - using ggplot

data_tbl %>% gather(measure, value, c(y, dly)) %>% ggplot(aes(x = date, y = value)) +
  geom_line() + labs(x = "", y = "", title = "Real Personal Consumption Expenditures") +
  facet_wrap(~measure, ncol = 1, scales = "free", labeller = labeller(measure = c(dlrpfi = "Log Change",
 rpfi = "Thousands of Dollars"))) + theme(strip.text = element_text(hjust = 0))

dly <- data_tbl %>% filter(!is.na(dly)) %>%  tk_xts(date_var = date, select = dly) 

nlags <- 36


g1 <- ggAcf(dly, lag.max = nlags)


g2 <- ggPacf(dly, lag.max = nlags)


ggarrange(g1, g2, ncol = 1)


ARIMA3 <- arima(dly, order=c(3,0,1)) 

ARIMA3

ggtsdiag(m1, gof.lag = nlags)


ARIMA3 <- auto.arima(dlrpfi, ic = "bic", stationary = TRUE, seasonal = FALSE, approximation = FALSE, stepwise = FALSE)

ARIMA3

ggtsdiag(ARIMA3, gof.lag = nlags)



#(c) As in the example discussed in class and the lecture slides, 
#split the sample into two parts: ???rst one up to 2008Q4, second one from 2009Q1 onward. 
#Use auto.arima with ic = aic and stationary = TRUE, stepwise = FALSE, approximation = FALSE to ???nd the best model. 
#Check the estimated model for adequacy, diagnose residuals using ggtsdiag.

# c.1. Split the sample into two parts
fstQ <- 1955.00  

lstQ <- 2009.00  


# c.2.Convert data into ts, which is the format that Acf, auto.arima and forecast expect
data.ts <- data_tbl %>%  tk_ts(select = dly, start = fstQ, frequency = 4)


# c.3.split sample - estimation and prediction subsamples

data.ts.1 <- data_tbl %>% tk_ts(select = dly, start = fstQ, end = lstQ, frequency = 4)


data.ts.2 <- data.ts %>% window(start = lstQ + 0.25)


# plot ACF and PACF

g1 <- ggAcf(data.ts.1, lag.max = 36)


g2 <- ggPacf(data.ts.1, lag.max = 36)

ggarrange(g1, g2, nrow = 2)


# m1 <- Arima(data.ts, order = c(3,0,1))


tic()

m1 <- auto.arima(data.ts.1, ic = "bic", stationary = TRUE, stepwise = FALSE, approximation = FALSE)

m1

m1 <- auto.arima(data.ts.1, ic = "aic", stationary = TRUE, stepwise = FALSE, approximation = FALSE)

m1



m1 <- auto.arima(data.ts.1, ic = "aicc", stationary = TRUE, stepwise = FALSE, approximation = FALSE)


m1


toc()

ggtsdiag(m1)

autoplot(m1)


m1 <- Arima(data.ts.1, order = c(3, 0, 0))


m1


ggtsdiag(m1)


# evaluate in-sample accuracy based on model residuals

accuracy(m1)


# (D) Use the estimated model with forecast to generate 1 to 36 step ahead forecast 
#for the prediction subsample, 2009Q1-2018Q4.

hmax <- length(data.ts.2)

# create h = 1,2,...36,hmax step ahead forecasts - 

m1.f.1.to.hmax <- forecast(m1, hmax)

m1.f.1.to.hmax


# Evaluate accuracy  of in sample

accuracy(m1.f.1.to.hmax, data.ts.2)


# Evaluate  accuracy of out-of-sample

accuracy(m1.f.1.to.hmax$mean, data.ts.2)


# Plot

autoplot(m1.f.1.to.hmax)

autoplot(m1.f.1.to.hmax) + geom_hline(yintercept = 0, color = "gray50") + scale_y_continuous(labels = percent_format(accuracy = 1),
                            breaks = seq(-0.1, 0.15, 0.05)) + labs(x = "", y = "" , title = "Real Personal Consumption Expenditure, Quarter over Quarter, Annualized",
          subtitle = "Multiperiod Point Forecast with 80% and 95% Confidence Intervals") +
  
 theme(legend.position = "none")


# We run Tidy from broom package to extract the coefficients from the model into a tibble

m1 %>% tidy()

# sw_sweep from sweep package transforms the forecast object into a tibble object in a long format

m1 %>%forecast(h = 1) %>% sw_sweep() %>% tail()


# (E) Estimate rolling ARMA with window size = window.length, create 1 period ahead rolling forecasts using slide from tsibble package

window.length <- length(data.ts.1)

tic()

results <- data_tbl %>% mutate(yearq = yearquarter(date)) %>% as_tsibble(index = yearq) %>%                                                               # covert to tsibble
   mutate(ar1.model = slide(dly, ~Arima(.x, order = c(1,0,0)), .size = window.length)) %>%     # estimate models
 filter(!is.na(ar1.model)) %>%    mutate(ar1.coefs = map(ar1.model, tidy, conf.int = TRUE),                                   # extract coefficients
 ar1.f = map(ar1.model, (. %>% forecast(h = 1) %>% sw_sweep())))                      # extract forecast


results


toc()


# Plot estimated coefficients with confidence intervals

results %>% as_tibble() %>% select(yearq, ar1.coefs) %>% unnest(ar1.coefs) %>% ggplot(aes(x = yearq, y = estimate, group = term)) +
 geom_line(color = "royalblue") +geom_ribbon(aes(x = yearq, ymin = conf.low, ymax = conf.high), alpha = 0.5, fill = "lightblue") +
geom_hline(yintercept = 0, color = "black") + labs(x = "", y = "", title = "Coefficient estimates",
subtitle = paste(window.length, "quarter rolling window model"))+ theme_minimal() +
 facet_wrap(~term, scales = "free_y")


# Extract the 1 period ahead rolling forecasts

m1.f.1.rol <- results %>% as_tibble() %>% select(yearq, ar1.f) %>% unnest(ar1.f) %>%
filter(key == "forecast") %>% mutate(yearq = yearq %m+% months(3) %>% yearquarter())


# (f) Plot the 1 period ahead rolling forecasts

m1.f.1.rol %>% ggplot(aes(x = yearq, y = value)) + geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = "royalblue", alpha = 0.2) +
geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = "royalblue", alpha = 0.3) +
  geom_line(size = 0.7, col = "blue") + 
  geom_line(data = (data_tbl %>% filter(year(date) > 1999)) %>% mutate(yearq = yearquarter(date)), 
  aes(x = yearq, y = dly)) + geom_hline(yintercept = 0, color = "gray50") +
 scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(-0.05, 0.10, 0.05)) +
 scale_color_manual(values = c("black", "darkblue")) +labs(x = "", y = "", title = "Real Personal Consumption Expenditure, Quarter over Quarter, Annualized",
 subtitle = "Rolling Forecast with 80% and 95% Confidence Intervals") +  theme(legend.position = "none")

#(g) Check for accuracy

accuracy(m1.f.1.rol$value, data.ts.2)    


