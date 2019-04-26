---
title: "koirala_eco5316_hw7.rmd"
author: "Bikal Koirala"
date: "April 24, 2019"
output: pdf_document
---

```{r}
library(magrittr)
library(tidyquant)
library(timetk)
library(tibbletime)
library(broom)
library(ggplot2)
library(ggfortify)
library(forecast)
library(Quandl)
library(urca)
library(vars)
library(devtools)
```

(a)
```{r}

OPHNFB <- tq_get(("OPHNFB"), get = "economic.data",
           from  = "1947-01-01", to = "2017-12-31")

HOANBS <- tq_get(("HOANBS"), get = "economic.data",
           from  = "1947-01-01", to = "2017-12-31")


# convert data into tibble
OPHNFB_tbl <-
    OPHNFB %>%
    as_tibble() %>%
    mutate(yearq = as.yearqtr(date, format = "%Y:%q")) %>%
  dplyr::select(c(yearq, price))

OPHNFB_ts <-
    OPHNFB_tbl %>%
    tk_ts(select = -yearq, start = year(.$yearq[1]), frequency = 4)

HOANBS_tbl <-
   HOANBS %>%
    as_tibble() %>%
    mutate(yearq = as.yearqtr(date, format = "%Y:%q")) %>%
  dplyr::select(c(yearq, price))


HOANBS_ts <-
    HOANBS_tbl %>%
    tk_ts(select = -yearq, start = year(.$yearq[1]), frequency = 4)



```


(b)
```{r}


    
log.OPHNFB_ts <- log(OPHNFB_ts)
log.HOANBS_ts <- log(HOANBS_ts)

first.diff.y1 <- diff(OPHNFB_ts, differences = 1)
first.diff.y2 <- diff(HOANBS_ts, differences = 1)

log.OPHNFB_ts %>% ur.ers(type = "P-test", lag.max = 8, model = "trend") %>% summary()

log.HOANBS_ts %>% ur.ers(type = "P-test", lag.max = 8, model = "trend") %>% summary()


first.diff.y1 %>% ur.ers(type = "P-test", lag.max = 8, model = "trend") %>% summary()

first.diff.y1 %>% ur.ers(type = "P-test", lag.max = 8, model = "trend") %>% summary()


```

(c)
```{r}

y <- cbind(first.diff.y1, first.diff.y2)

var.p <- VAR(y, ic = "AIC", lag.max = 12, type = "const")
var.p
summary(var.p)
```
(d)
```{r}
mySVAR <- BQ(var.p)
mySVAR
summary(mySVAR)
```
(e) 

```{r}

myIRF.c <- irf(mySVAR, n.ahead = 12, ci = .9, cumulative = TRUE)

summary(mySVAR)



```

(f)
```{r}




plot( myIRF.c )

```


(g)
```{r}
compare

```

(h)
```{r}
plot( fevd(mySVAR, n.ahead=40) ,addbars=10 )

```