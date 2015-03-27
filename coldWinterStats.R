## ----echo = FALSE, message = FALSE---------------------------------------
library(ggplot2)
library(lubridate)
library(dplyr)
library(knitr)

opts_chunk$set(echo = FALSE)

## ----, echo = FALSE------------------------------------------------------
subset.ggplot <- function(gg, subset, select, drop = FALSE) {
  # Subsets a ggplot object by applying subset.data.frame code to gg$data
  x = gg[["data"]]
  r <- if (missing(subset)) 
    rep_len(TRUE, nrow(x))
  else {
    e <- substitute(subset)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r)) 
      stop("'subset' must be logical")
    r & !is.na(r)
  }
  vars <- if (missing(select)) 
    TRUE
  else {
    nl <- as.list(seq_along(x))
    names(nl) <- names(x)
    eval(substitute(select), nl, parent.frame())
  }
  gg[["data"]] = x[r, vars, drop = drop]
  gg
}

dataIn = read.csv("data/504345.csv")
dates = data.frame(Date = seq.Date(as.Date("1894-01-01"), as.Date("2015-03-23"), 1))
temps = dataIn %>% 
  mutate(Date = as.Date(ymd(DATE)), 
         TMAX = ifelse(TMAX == -9999, NA, TMAX)) %>% 
  right_join(dates, by = "Date") %>% 
  transmute(Date = Date, jday = as.numeric(format(Date, "%j")), 
            year = as.numeric(format(Date, "%Y")), highT = 1.8 * TMAX / 10 + 32)

## ------------------------------------------------------------------------
goodyears = temps %>% 
  mutate(year = as.numeric(format(Date, "%Y")),
         jday = as.numeric(format(Date, "%j"))) %>% 
  filter(jday < 83) %>% 
  group_by(year) %>% 
  summarize(nna = sum(is.na(highT))) %>% 
  filter(nna < 10) %>% 
  `[[`("year")

temps = temps %>% 
  filter(year %in% goodyears) %>% 
  mutate(highT = zoo::na.approx(highT))

## ------------------------------------------------------------------------
kable(tail(temps))

## ------------------------------------------------------------------------
meanTs = temps %>% 
  filter(jday < 83) %>% 
  group_by(year) %>% 
  summarize(meanT = mean(highT))
g1 = ggplot(meanTs, aes(x = year, y = meanT)) + 
  geom_point(aes(size = (year == 2015), color = (year == 2015))) + 
  theme_minimal()
g1

## ----, echo = FALSE------------------------------------------------------
g1 + stat_smooth(method = "lm")

## ----, echo = FALSE------------------------------------------------------
subset(g1, year != 2015) + stat_smooth(method = "lm")

## ------------------------------------------------------------------------
medianTs = temps %>% 
  filter(jday < 83) %>% 
  group_by(year) %>% 
  summarize(medianT = median(highT))
g2 = ggplot(medianTs, aes(x = year, y = medianT)) + 
  geom_point(aes(size = (year == 2015), color = (year == 2015))) + 
  theme_minimal()
g2

## ------------------------------------------------------------------------
maxTs = temps %>% 
  filter(jday < 83) %>% 
  group_by(year) %>% 
  summarize(maxT = max(highT))
g3 = ggplot(maxTs, aes(x = year, y = maxT)) + 
  geom_point(aes(size = (year == 2015), color = (year == 2015))) + 
  theme_minimal()
g3

## ------------------------------------------------------------------------
rankTs = temps %>% 
  filter(jday < 83) %>% 
  group_by(year) %>% 
  mutate(dayrank = rank(highT, ties = "first"))


g4 = ggplot(filter(rankTs, year == 2015), aes(x = dayrank, y = highT)) +
  geom_point() +
  theme_minimal()
g4

## ------------------------------------------------------------------------
rankTs_med = rankTs %>% 
  group_by(dayrank) %>% 
  summarize(medRankT = median(highT))

g4 + geom_line(data = rankTs_med, aes(x = dayrank, y = medRankT))

## ------------------------------------------------------------------------
rankRanks = rankTs %>% 
  group_by(dayrank) %>% 
  mutate(yearrank = rank(highT)) %>% 
  filter(year == 2015) %>% 
  ungroup() %>% 
  arrange(desc(yearrank)) %>% 
  plot(yearrank ~ dayrank, data = .)

## ------------------------------------------------------------------------
rankRanks = rankTs %>% 
  group_by(dayrank) %>% 
  mutate(yearrank = percent_rank(highT)) %>% 
  filter(year == 2015) %>% 
  ungroup() %>% 
  arrange(desc(yearrank)) %>% 
  plot(yearrank ~ dayrank, data = ., ylim = c(0, 1))

## ------------------------------------------------------------------------
rankTs %>% 
  group_by(dayrank) %>% 
  mutate(yearrank = percent_rank(highT)) %>% 
  filter(year == 2014) %>% 
  ungroup() %>% 
  arrange(desc(yearrank)) %>% 
  plot(yearrank ~ dayrank, data = ., ylim = c(0, 1))

## ------------------------------------------------------------------------
rankTs %>% 
  group_by(dayrank) %>% 
  mutate(yearrank = percent_rank(highT)) %>% 
  filter(year == 2013) %>% 
  ungroup() %>% 
  arrange(desc(yearrank)) %>% 
  plot(yearrank ~ dayrank, data = ., ylim = c(0, 1))

## ------------------------------------------------------------------------
rankTs %>% 
  group_by(dayrank) %>% 
  mutate(yearrank = percent_rank(highT)) %>% 
  filter(year == 2012) %>% 
  ungroup() %>% 
  arrange(desc(yearrank)) %>% 
  plot(yearrank ~ dayrank, data = ., ylim = c(0, 1))

