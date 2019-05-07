
library(magrittr)
library(readxl)
library(tidyquant)
library(timetk)
library(urca)
library(vars)
library(ggfortify)

theme_set(theme_bw() + 
          theme(strip.background = element_blank(),
                strip.text.x = element_text(hjust = 0)))

# import data on price indices and exchange rates
ppp_raw_tbl <- read_csv("data/coint_ppp.csv")
ppp_raw_tbl

glimpse(ppp_raw_tbl)

ppp_tbl <-
    ppp_raw_tbl %>%
    mutate(yearm = as.yearmon(ENTRY, format = "%Y:%m:00")) %>%
    dplyr::select(yearm, everything(), -ENTRY) %>%
    rename(cpi_usa = USCPI,
           cpi_can = CANCPI,
           cpi_jap = JAPANCPI,
           cpi_swe = SWCPI,
           exr_can = CANEX,
           exr_jap = JAPANEX,
           exr_swe = SWEX) %>%
    mutate_if(is.character, as.numeric) 

glimpse(ppp_tbl)
head(ppp_tbl, 12)
tail(ppp_tbl, 12)

summary(ppp_tbl)

ppp_tbl %>%
    tk_ts(select = -yearm, start = year(.$yearm[1]), frequency = 12) %>%
    autoplot()

# rows with some missing values
ppp_tbl %>%
    filter_all(any_vars(is.na(.)))

ppp_tbl_by_country <-
    ppp_tbl %>%
    gather(variable, level, -c(yearm, cpi_usa)) %>%
    separate(variable, c("measure", "country")) %>%
    spread(measure, level) %>%
    arrange(country, yearm) %>%
    # mutate_at(vars(-yearm, - country), log)
    # mutate_if(is.numeric, log)
    mutate(lcpi_usa = log(cpi_usa / cpi_usa[1]),
           lcpi = log(cpi / cpi[1]),
           lexr = log(exr / exr[1])) %>%
    group_by(country) %>%
    nest()

ppp_tbl_by_country %>%
    filter(country == "can") %>%
    pull(data)

ppp_ca_by_country <-
    ppp_tbl_by_country %>%
    mutate(data_ts = map(data, ~.x %>% 
                             tk_ts(select = c(lcpi_usa, lcpi, lexr), start = year(.$yearm[1]), frequency = 12)),
           data_ts_pre1998 = map(data, ~.x %>% 
                                     filter(yearm <= as.yearmon("Dec 1998")) %>%
                                     tk_ts(select = c(lcpi_usa, lcpi, lexr), start = year(.$yearm[1]), frequency = 12)),
           ca = map(data_ts_pre1998, ~ca.jo(.x, ecdet = "none", type = "eigen", K = 4, spec = "transitory", season = 12)),
           vec = map(ca, ~cajorls(.x, r = 1)))

ppp_ca_by_country %$%
    map(ca, summary)

ppp_ca_by_country %$%
    map(vec, ~ .x %>% pluck("rlm") %>% summary())

# plot residuals and their ACF and PACF
# ppp_ca_by_country %$%
#     map(ca, plotres)

# test for restricted constant in cointegration relationship rather than as a drift
lt_test <- ppp_ca_by_country %$%
    map(ca, ~lttest(.x, r = 1)) 
lt_test
