
library(magrittr)
library(readr)
library(tidyverse)
library(timetk)
library(vars)
library(urca)

theme_set(theme_bw())

# pairs trading strategy - Tsay, p. 446

# stock price data on two multinational companies, Billiton Ltd. of Australia (BHP) and Vale S.A. of Brazil (VALE),
# that belong to natural resources industry and encounter similar risk factors
# this data can be downloaded from http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/
y_tbl <-
    inner_join(read_delim(file = "data/d-bhp0206.txt", delim = " "),
           read_delim(file = "data/d-vale0206.txt", delim = " "),
           by = c("Mon", "day", "year"), suffix = c(".BHP", ".VALE")) %>%
    gather(variable, value, -c("Mon", "day", "year")) %>%
    filter(str_sub(variable, 1, 8) == "adjclose") %>%
    mutate(date = (year*10000 + Mon*100 + day) %>% as.character() %>% as.Date("%Y%m%d" ),
           variable = str_sub(variable, 10, -1),
           logvalue = log(value))

# time series plot - log of adjusted close price for BHP and VALE
y_tbl %>%
    ggplot(aes(x = date, y = value, col = variable)) +
        geom_line() +
        scale_y_log10(breaks = c(0,10,20,30,40)) +
        # scale_color_manual(values = c("gray10","gray60")) +
        labs(x = "", y = "", col = "", title = "Log of adjusted close price for BHP and VALE")

# convert into zoo
y_zoo <-
    y_tbl %>%
    dplyr::select(date, variable, logvalue) %>%
    spread(variable, logvalue) %>%
    tk_zoo(select = -date, date_var = date)


# determine number of lags to be included in cointegration test and in VEC model
nlags <- VARselect(y_zoo, type = "const") %$% selection["AIC(n)"]

# perform cointegration test
y_ca <- ca.jo(y_zoo, ecdet = "const", type = "trace", K = nlags, spec = "transitory")
summary(y_ca)
y_ca <- ca.jo(y_zoo, ecdet = "const", type = "eigen", K = nlags, spec = "transitory")
summary(y_ca)

# estimate VEC model
y_vec <- cajorls(y_ca, r = 1)
y_vec
summary(y_vec$rlm)

# spread
w <- y_zoo %*% y_vec$beta[1:2]
mean(w)
sd(w)

# plot spread and the boundaries that would trigger pairs trading if \Delta = sd(w) \approx 0.045
w %>%
    tk_tbl(rename_index = "date") %>%
    ggplot(aes(x = date, y = V1)) +
        geom_line() +
        geom_hline(yintercept = mean(w), linetype = "solid") +
        geom_hline(yintercept = mean(w) + sd(w), linetype = "dashed") +
        geom_hline(yintercept = mean(w) - sd(w), linetype = "dashed") +
        labs(x = "", y = "", title = "Spread and boundaries that would trigger pairs trading")

