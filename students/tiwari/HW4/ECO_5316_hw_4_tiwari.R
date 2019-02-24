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


#Q(a&b)
RPCE.tbl <-
  tq_get("PCECC96", get = "economic.data", from = "1955-01-01", to = "2018-10-01") %>%
  rename( RPCE = price) %>%
  mutate(dlRPCE = 4*(log(RPCE) - lag(log(RPCE))))
theme_set(theme_minimal())


#Q(c)

fstQ <- 1955.00
lstQ <- 2008.75
RPCE.ts <- RPCE.tbl %>%
  tk_ts(select = dlRPCE, start = fstQ, frequency = 4)
RPCE.ts.1 <- RPCE.tbl %>%
  tk_ts(select = dlRPCE, start = fstQ, end = lstQ, frequency = 4)
RPCE.ts.2 <- RPCE.ts %>%
  window(start = lstQ + 0.25)
M1 <- auto.arima(RPCE.ts.1, ic = "aic", stationary = TRUE, stepwise = FALSE, approximation = FALSE)
M1
ggtsdiag(M1) 


g1 <- ggAcf(RPCE.ts.1, lag.max = 24)
g2 <- ggPacf(RPCE.ts.1, lag.max = 24)
ggarrange(g1, g2, nrow = 2)

AR1 <- Arima(RPCE.ts, order = c(1,0,0))
AR1
ggtsdiag(AR1)
AR2 <- Arima(RPCE.ts, order = c(2,0,0))
AR2
ggtsdiag(AR2)
AR3 <- Arima(RPCE.ts, order = c(3,0,0))
AR3
ggtsdiag(AR3)

MA1 <- Arima(RPCE.ts, order = c(0,0,1))
MA1
ggtsdiag(MA1)
MA2 <- Arima(RPCE.ts, order = c(0,0,2))
MA2
ggtsdiag(MA2)

MA3 <- Arima(RPCE.ts, order = c(0,0,3))
MA3
ggtsdiag(MA3)




ARMA32 <- Arima(RPCE.ts, order = c(3,0,2))
ARMA32
ARMA23 <- Arima(RPCE.ts, order = c(2,0,3))
ARMA23
ggtsdiag(ARMA32)
autoplot(ARMA32)
accuracy(AR1)
accuracy(ARMA32)
accuracy(ARMA23)



#From above result, it has been clear that ARMA(3, 0, 2) is the best suited model.


#Q(d)
h36 <- length(RPCE.ts.2)
#Q(e)
ARMA32.f.1 <- Arima(y = RPCE.ts, model = ARMA32)

ARMA32.f.1.to.h36 <- forecast(ARMA32, h36)
ARMA32.f.1.to.h36
accuracy(fitted(ARMA32.f.1), RPCE.ts)
accuracy(ARMA32)

ARMA32.f.1.oos <- window(fitted(ARMA32.f.1), start = lstQ+0.25)

accuracy(fitted(ARMA32.f.1), RPCE.ts.2)


ARMA32 %>%
  tidy()
ARMA32 %>%
  forecast(h = 1) %>%
  sw_sweep() %>%
  tail()


window.length <- length(RPCE.ts.1)

tic()
results <-
  RPCE.tbl %>%
  mutate(yearq = yearquarter(date)) %>%
  as_tsibble(index = yearq) %>%                                                             
  mutate(ar1.model = slide(dlRPCE, ~Arima(.x, order = c(3,0,2)), .size = window.length)) %>%    
  filter(!is.na(ar1.model)) %>%                                                           
  mutate(ar1.coefs = map(ar1.model, tidy, conf.int = TRUE),                                   
         ar1.f = map(ar1.model, (. %>% forecast(h = 1) %>% sw_sweep())))                     
results
toc()

results %>%
  as_tibble() %>%
  select(yearq, ar1.coefs) %>%
  unnest(ar1.coefs) %>%
  ggplot(aes(x = yearq, y = estimate, group = term)) +
  geom_line(color = "royalblue") +
  geom_ribbon(aes(x = yearq, ymin = conf.low, ymax = conf.high), alpha = 0.5, fill = "lightblue") +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "", y = "",
       title = "Coefficient estimates",
       subtitle = paste(window.length, "quarter rolling window model"))+
  theme_minimal() +
  facet_wrap(~term, scales = "free_y")

#Q(f)
ARMA32.f.1.rol <-
  results %>%
  as_tibble() %>%
  select(yearq, ar1.f) %>%
  unnest(ar1.f) %>%
  filter(key == "forecast") %>%
  mutate(yearq = yearq %m+% months(3) %>% yearquarter())

autoplot(ARMA32.f.1.to.h36) +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(-0.1, 0.15, 0.05)) +
  labs(x = "", y = "" , title = "Real Personal Consumption Expenditures, Quarter over Quarter, Annualized",
       subtitle = "Multiperiod Point Forecast with 80% and 95% Confidence Intervals") +
  theme(legend.position = "none")

ARMA32.f.1.rol %>%
  ggplot(aes(x = yearq, y = value)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = "royalblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = "royalblue", alpha = 0.3) +
  geom_line(size = 0.7, col = "blue") +
  geom_line(data = (RPCE.tbl %>% filter(year(date) > 2000)) %>% mutate(yearq = yearquarter(date)), 
            aes(x = yearq, y = dlRPCE)) +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(-0.05, 0.10, 0.05)) +
  scale_color_manual(values = c("black", "darkblue")) +
  labs(x = "", y = "", title = "Real Personal Consumption Expenditures, Quarter over Quarter, Annualized",
       subtitle = "Rolling Forecast with 80% and 95% Confidence Intervals") +
  theme(legend.position = "none")
#Q(g)
accuracy(ARMA32.f.1.to.h36$mean, RPCE.ts.2)    # multistep forecast
accuracy(ARMA32.f.1.rol$value, RPCE.ts.2)       # 1 step ahead rolling scheme forecast








