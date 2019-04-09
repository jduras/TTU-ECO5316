
# example of SVAR with long run restriction - Blanchard and Quah (1989)

library(magrittr)
library(readr)
library(tidyquant)
library(timetk)
library(vars)
library(ggfortify)
library(plotly)

theme_set(theme_bw() + 
              theme(strip.text.x = element_text(hjust = 0),
                    strip.text.y = element_text(hjust = 1),
                    strip.background = element_blank()))



#### Data ####

gdp_raw <- tq_get("GDPC1", get = "economic.data", from = "1947-01-01", to = "2018-12-31")
unrate_raw <- tq_get("UNRATE", get = "economic.data", from = "1947-01-01", to = "2018-12-31")

if (is.null(nrow(gdp_raw))) {
    read_csv(file = "data/gdp_raw.csv")  
} else {
    write_csv(gdp_raw, path = "data/gdp_raw.csv")
}
 
if (is.null(nrow(unrate_raw))) {
    read_csv(file = "data/unrate_raw.csv")  
} else {
    write_csv(unrate_raw, path = "data/unrate_raw.csv")
}   
    
# obtain data on real GDP and unemployment rate
# construct approximate quarter-over-quarter GDP growth rates
y_tbl <- inner_join(gdp_raw %>%
                        rename(rGDP = price) %>%
                        mutate(dlrGDP = 100*(log(rGDP) - lag(log(rGDP)))),
                    unrate_raw %>%
                        rename(UR = price) %>%
                        # transform from monthly to quarterly frequency - option 1: last observation is used
                        # rename(yearq = date) %>%
                        # tq_transmute(select = UR, mutate_fun = to.quarterly) %>%
                        # transform from monthly to quarterly frequency - option 2: use average
                        mutate(yearq = as.yearqtr(date)) %>%
                        group_by(yearq) %>%
                        summarise(UR = mean(UR)) %>%
                        ungroup() %>%
                        mutate(date = as.Date(yearq)),
                    by = "date") %>%
    dplyr::select(yearq, dlrGDP, UR)

# plot log change in GDP and unemployment rate
y_tbl %>%
    gather(variable, value, -yearq) %>%
    ggplot(aes(x = yearq, y = value)) +
        geom_line() +
        scale_x_yearqtr() + 
        labs(x = "", y = "") +
        facet_wrap(~variable, ncol = 1, scales = "free_y")

# Blanchard and Quah use 1950Q2 to 1987Q4 as sample, and demean the data
y_ts <- y_tbl %>%
    filter(yearq >= "1950 Q2", yearq <= "1987 Q4") %>%
    mutate_at(vars(dlrGDP,UR), funs(. - mean(.))) %>%
    tk_ts(select= c("dlrGDP", "UR"), start = .$yearq[1])

y_xts <- y_tbl %>%
    filter(yearq >= "1950 Q2", yearq <= "1987 Q4") %>%
    mutate_at(vars(dlrGDP,UR), funs(. - mean(.))) %>%
    tk_xts(select= c("dlrGDP","UR"), date_var = yearq)

# plot log change in GDP and unemployment rate for 1950Q2 to 1987Q4 sample
# with ggfortify package autoplot works also for ts, not just xts  
autoplot(y_ts) 
autoplot(y_xts) 


#### SVAR with long run restrictions ####

# first estimate reduced form VAR
# xts works fine in VARselect, VAR, and also when creating forecast using predict
# but plotting the forecast using autoplot gives an error, this does not happen with ts 
VARselect(y_ts, lag.max = 8)
myVAR <- VAR(y_ts, ic = "SC", lag.max = 8)
summary(myVAR)

# impose Blanchard-Quah long run restriction:
#  row 1 column 2 element of the cumulative effect matrix is going to be restricted to 0
?BQ
mySVAR <- BQ(myVAR)
summary(mySVAR)


#### IRFs ####

# standard non-cumulative IRFs
myIRF <- irf(mySVAR, n.ahead = 40, ci = .9)
# cumulative IRFs
myIRF_c <- irf(mySVAR, n.ahead = 40, ci = .9, cumulative = TRUE)


# arrange IRF data into a tibble to be used with ggplot
# and plot IRFs using ggplot
myIRF_tbl <-
    bind_rows(# standard IRFs for UR
              myIRF %>%
                  keep(names(.) %in% c("irf", "Lower", "Upper")) %>%
                  modify_depth(2, as_tibble) %>%
                  modify_depth(1, bind_rows, .id = "impulse") %>%
                  map_df(bind_rows, .id = "key") %>%
                  dplyr::select(-dlrGDP) %>%
                  gather(response, value, -key, -impulse),
              # cumulative IRFs for dlrGDP
              myIRF_c %>%
                  keep(names(.) %in% c("irf", "Lower", "Upper")) %>%
                  modify_depth(2, as_tibble) %>%
                  modify_depth(1, bind_rows, .id = "impulse") %>%
                  map_df(bind_rows, .id = "key") %>%
                  dplyr::select(-UR) %>%
                  gather(response, value, -key, -impulse)) %>%
    group_by(key, impulse, response) %>%
    mutate(lag = row_number()) %>%
    ungroup() %>%
    # change signs for the non-technology shock IRFs so that they show effects of a positive shock, not a negative one
    mutate(value = if_else(impulse == "UR", -value, value)) %>%
    spread(key, value)

g <- myIRF_tbl %>%
    mutate(impulse_label = case_when(impulse == "dlrGDP" ~ 1,
                                    impulse == "UR"     ~ 2) %>% factor(labels = c("technology shock","non-technology shock")),
           response_label = case_when(response == "dlrGDP" ~ "log(GDP)",
                                      response == "UR" ~ "Unemployment Rate") ) %>%
    ggplot(aes(x = lag, y = irf)) +
        geom_ribbon(aes(x = lag, ymin = Lower, ymax = Upper), fill = "gray50", alpha = .3) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(x = "", y = "", title = "SVAR Impulse Response Functions") +
        facet_grid(response_label ~ impulse_label, switch = "y", scales = "free_y")
g

# plot IRFs using plotly
library(plotly)
ggplotly(g)



# note that by construction the contemporaneous impact matrix is identical to the elements of the IRFs for period 0 (impact period)
summary(mySVAR)
myIRF_c$irf[[1]][1,]
myIRF_c$irf[[2]][1,]
# and that the long run impact matrix from is essentially the same as the elements of the IRFs if the horizon is large enough, e.g. 100 periods
myIRF_c <- irf(mySVAR, n.ahead = 100, ci = .9, cumulative = TRUE, boot = FALSE)
myIRF_c$irf[[1]][101,]
myIRF_c$irf[[2]][101,]



#### FEVD ####
mySVAR %>% fevd(n.ahead = 40) %>% plot(addbars = 10) 

# same as above, but using ggplot
mySVAR_fevd <- fevd(mySVAR, n.ahead = 40)
    
# arrange FEVD data into a tibble to be used with ggplot
mySVAR_fevd_tbl <-
    mySVAR_fevd %>%
    modify_depth(1, as_tibble) %>%
    map_df(bind_rows, .id = "variable") %>%
    gather(shock, value, -variable) %>%
    group_by(shock, variable) %>%
    mutate(horizon = row_number()) %>%
    ungroup() %>%
    mutate(shock = recode(shock, dlrGDP = "technology", UR = "non-technology"))

# plot FEVD using ggplot
g <- ggplot(data = mySVAR_fevd_tbl, aes(x = horizon, y = value, fill = shock)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = c("gray80", "gray40")) +
    labs(x = "horizon", y = "fraction of overall variance", title = "Forecast Error Variance Decomposition") +
    facet_wrap(variable ~ ., ncol = 1)
g

ggplotly(g)


