
# estimate series of SVARs with labor productivity and labor input (employment and hours worked)
# with Blanchard-Quah long run restriction, in order to replicate the results reported in 
# Figure 2 in Gali (1999) AER "Technology, Employment, and the Business Cycle: Do Technology Shocks Explain Aggregate Fluctuations?"

library(vars)
library(magrittr)
library(tidyquant)
library(timetk)
library(egg)
library(stargazer)
library(grid)
library(gridExtra)
library(plotly)

theme_set(theme_bw() + 
          theme(strip.text.x = element_text(hjust = 0),
                strip.text.y = element_text(hjust = 1),
                strip.background = element_blank()))



#### Data: Labor Productivity and Employment/Hours worked (quarterly) ###

labor_tbl <-
    tq_get(c("GDPC1", "CNP16OV", "CE16OV", "PRS85006163", "OPHNFB", "PRS85006013", "PRS85006023", "HOANBS"),
           get = "economic.data", from = "1947-01-01", to = "2018-12-31") %>%
    mutate(yearq = as.yearqtr(date),
           symbol = case_when(symbol == "GDPC1"       ~ "rGDP",       # Real GDP
                              symbol == "CNP16OV"     ~ "POP",        # Civilian Noninstitutional Population
                              symbol == "CE16OV"      ~ "E_CPS",      # Civilian Employment Level
                              symbol == "PRS85006163" ~ "Y2E",        # Nonfarm Business Sector: Real Output Per Person
                              symbol == "OPHNFB"      ~ "Y2H",        # Nonfarm Business Sector: Real Output Per Hour of All Persons
                              symbol == "PRS85006013" ~ "E",          # Nonfarm Business Sector: Employment
                              symbol == "PRS85006023" ~ "HPW",        # Nonfarm Business Sector: Average Weekly Hours
                              symbol == "HOANBS"      ~ "H"           # Nonfarm Business Sector: Hours of All Persons
                              )) %>%
    group_by(symbol, yearq) %>%
    summarise(lvl = mean(price)) %>%
    group_by(symbol) %>%
    mutate(dlog = log(lvl) - lag(log(lvl))) %>%
    ungroup() %>%
    select(yearq, symbol, dlog) %>%
    mutate(symbol = str_c("dl", symbol)) %>%
    spread(symbol, dlog) %>%
    mutate(dlY2E_CPS = dlrGDP - dlE_CPS,
           dlEPC_CPS = dlE_CPS - dlPOP,
           dlE_CPS = dlE_CPS - dlPOP,
           dlEPC = dlE - dlPOP,
           dlHPC = dlH - dlPOP) %>%
    select(yearq, 
           dlY2E_CPS, dlY2E, dlY2H,
           dlE_CPS, dlEPC_CPS,
           dlE, dlEPC, 
           dlH, dlHPC, dlHPW)


#### SVAR, IRF, FEVD ####

# qfst <- "1950 Q2"
# qlst <- "1987 Q4"

qfst <- "1950 Q1"
qlst <- "2018 Q4"

# dlY2E, dlE
# dlY2E, dlEPC



#### SVAR ####
svar_bymeasure <- 
    labor_tbl %>%
    select(yearq, dlY2H, dlE, dlEPC, dlH, dlHPC, dlHPW) %>%
    gather(measure, dlN, c(dlE, dlEPC, dlH, dlHPC, dlHPW)) %>%
    group_nest(measure) %>%
    mutate(measure_label = recode(measure,
                                  dlEPC = "Employment Per Capita",
                                  dlE   = "Employment",
                                  dlHPC = "Hours per Capita",
                                  dlHPW = "Hours per Worker",
                                  dlH   = "Total Hours") %>%
               factor(levels = c("Hours per Worker", "Employment", "Total Hours", "Employment Per Capita", "Hours per Capita"), 
                      ordered = TRUE),
           data_xts = map(data, ~.x %>% 
                              drop_na() %>%
                              filter(yearq >= as.yearqtr(qfst) & yearq <= as.yearqtr(qlst)) %>%
                              mutate_at(vars(-yearq), ~ . - mean(.)) %>%
                              tk_xts(select = -yearq, date_var = yearq)),
           var_ic = map(data_xts, ~VARselect(.x, lag.max = 4, type = "none")),
           svar_mod = map(data_xts, ~VAR(.x, ic = "AIC", lag.max = 4, type = "none") %>% 
                          BQ())) %>%
    arrange(measure_label)

# svar_bymeasure %$%
#     set_names(svar_mod, measure) %>%
#     map(~stargazer(.$var$varresult, type = "text", no.space = TRUE))

svar_bymeasure %$%
    set_names(svar_mod, measure) %>%
    map(summary)



#### IRFs ####
svar_bymeasure_irf <-
    svar_bymeasure %>%
    mutate(svar_irf = map(svar_mod, ~irf(.x, n.ahead = 40, ci = .9)),
           svar_irf_c = map(svar_mod, ~irf(.x, n.ahead = 40, ci = .9, cumulative = TRUE))) %>%
    mutate_at(c("svar_irf", "svar_irf_c"), 
              ~map(., ~keep(.x, names(.x) %in% c("irf", "Lower", "Upper")) %>%
                       modify_depth(2, as_tibble) %>%
                       modify_depth(1, bind_rows, .id = "impulse") %>%
                       map_df(bind_rows, .id = "key") %>%
                       mutate(key = recode(key, Lower = "lower", Upper = "upper")) %>%
                       gather(response, value, -key, -impulse) %>%
                       group_by(key, impulse, response) %>%
                       mutate(lag = row_number()) %>%
                       ungroup() %>%
                       spread(key, value)))
           
svar_bymeasure_irf_plot <-
    svar_bymeasure_irf %>%
    mutate(svar_irf_plot = map2(svar_irf_c, measure_label,
                                ~.x %>%
                                   mutate(impulse_label = case_when(impulse == "dlY2H" ~ 1,
                                                                    impulse == "dlN"   ~ 2) %>% 
                                              factor(labels = c("technology shock","non-technology shock")),
                                          response_label = case_when(response == "dlY2H" ~ "Productivity",
                                                                     response == "dlN"   ~ "Labor Input")) %>%
                                   filter(lag <= 20) %>%
                                   ggplot(aes(x = lag, y = irf)) +
                                        geom_ribbon(aes(x = lag, ymin = lower, ymax = upper), fill = "gray50", alpha = .3) +
                                        geom_line() +
                                        geom_hline(yintercept = 0, linetype = "dashed") +
                                        labs(x = "", y = "", title = str_c("SVAR with ", .y)) +
                                        facet_grid(response_label ~ impulse_label, switch = "y", scales = "free_y") +
                                        theme(plot.title = element_text(size = 11, hjust = 0))))

svar_bymeasure_irf_plot %$%
    map(svar_irf_plot, ggplotly)

g <- svar_bymeasure_irf_plot %>%
    pull(svar_irf_plot) %>%
    ggarrange(plots = ., nrow = 1,
              top = textGrob(str_c("Cummulative Impulse Response Functions\n", qfst, ":", qlst),
                                   hjust = 0, x = 0.05,
                             gp = gpar(fontface = "bold", fontsize = 12)))
g



#### FEVDs ####
svar_bymeasure_fevd <-
    svar_bymeasure %>%
    mutate(svar_fevd_tbl = map(svar_mod, ~fevd(.x, n.ahead = 20) %>% 
                                   modify_depth(1, as_tibble) %>%
                                   map_df(bind_rows, .id = "variable") %>%
                                   gather(shock, value, -variable) %>%
                                   group_by(shock, variable) %>%
                                   mutate(horizon = row_number()) %>%
                                   ungroup() %>%
                                   mutate(shock = recode(shock, dlY2H = "technology", dlN = "non-technology"))))

svar_bymeasure_fevd_plot <-
    svar_bymeasure_fevd %>%
    mutate(svar_fevd_plot = map2(svar_fevd_tbl, measure_label,
                                 ~.x %>%
                                     mutate(variable_label = case_when(variable == "dlY2H" ~ "Productivity",
                                                                       variable == "dlN"   ~ "Labor Input")) %>%
                                     ggplot(aes(x = horizon, y = value, fill = shock)) +
                                         geom_col(position = position_stack(reverse = TRUE)) +
                                         scale_fill_manual(values = c("gray80", "gray40")) +
                                         labs(# x = "horizon", y = "fraction of overall variance", fill = "",
                                              x = "", y = "", fill = "",
                                              title = str_c("SVAR with ", .y)) +
                                         # facet_wrap(variable ~ ., ncol = 1) +
                                         facet_grid(variable_label ~ ., switch = "y", scales = "free") +
                                         theme(legend.position = "top", 
                                               legend.key = element_blank(),
                                               plot.title = element_text(size = 11, hjust = 0))))

svar_bymeasure_fevd_plot %$%
    map(svar_fevd_plot, ggplotly)

g <- svar_bymeasure_fevd_plot %>%
    pull(svar_fevd_plot) %>%
    ggarrange(plots = ., nrow = 1,
              top = textGrob(str_c("Forecast Error Variance Decomposition\n", qfst, ":", qlst),
                             hjust = 0, x = 0.025, 
                             gp = gpar(fontface = "bold", fontsize = 12)),
              bottom = "horizon", 
              left = "fraction of overall variance")
g

g <- svar_bymeasure_fevd %>%
    select(measure, measure_label, svar_fevd_tbl) %>%
    unnest() %>%
    mutate(variable_label = case_when(variable == "dlY2H" ~ "Productivity",
                                      variable == "dlN"   ~ "Labor Input")) %>%
    {ggplot(., aes(x = horizon, y = value, fill = shock)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        scale_fill_manual(values = c("gray80", "gray40")) +
        labs(x = "horizon", y = "fraction of overall variance", fill = "type of shock:",
            title = str_c("SVAR with ", .$measure_label)) +
        facet_grid(variable_label ~ measure_label, switch = "y", scales = "free") +
        theme(legend.position = "top", 
              legend.justification = "left",
              legend.key = element_blank(),
              plot.title = element_text(size = 11, hjust = 0))}
g
ggplotly(g)
