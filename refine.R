library(tidyverse)
library(magrittr)

load("data/pageviews.RData")
load("data/unique_devices.RData")
gsc_m_eswiki <- read_csv(file = "data/eswiki_m_gsc.csv", col_types = "Dciidd")
gsc_eswiki <- read_csv(file = "data/eswiki_gsc.csv", col_types = "Dciidd")

seed <- 2019
online_start <- as.Date("2018-11-15")
online_end <- as.Date("2018-12-16")

pageviews %<>%
  mutate(
    date = lubridate::ymd(date),
    access_method = ifelse(grepl('^mobile', access_method), 'mobile', 'desktop'),
    referer_class = case_when(
      grepl('^external', referer_class) ~ 'external',
      referer_class == 'none' ~ 'direct',
      TRUE ~ referer_class
      )
    )

unique_devices %<>%
  mutate(
    date = lubridate::ymd(date),
    access_method = case_when(
      grepl('^[a-z]+\\.m\\..+', domain) ~ "mobile",
      grepl('^m\\..+', domain) ~ "mobile",
      TRUE ~ "desktop"
    ),
    domain = gsub("(m\\.)|(\\.org$)", "", domain)
  ) %>%
  rename(total = uniques_estimate, first_visit = uniques_offset,
         return = uniques_underestimate, project = domain) %>%
  gather(key = type, value = uniques, total, first_visit, return)

gsc_eswiki <- rbind(
  gsc_m_eswiki %>% mutate(access_method = "mobile"),
  gsc_eswiki %>% mutate(access_method = "desktop")
) %>%
  mutate(
    date = lubridate::ymd(date)
  )


# internet_users <- data.frame(
#   date = as.Date(c("2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31")),
#   internet_users = c(47441244, 62448892, 65520817, 71340853)
# ) %>%
#   complete(date = seq.Date(min(date), max(date), by="day"))
# Source: INEGI
# http://en.www.inegi.org.mx/contenidos/temas/economia/ticshogares/tabulados/unal564.xlsx


Newyear <- bsts::DateRangeHoliday(
  "NewYear",
  start = as.Date(c("2015-12-31", "2016-12-31", "2017-12-31", "2018-12-31")),
  end = as.Date(c("2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01"))
  )
Christmas <- bsts::DateRangeHoliday(
  "Christmas",
  start = as.Date(c("2015-12-24", "2016-12-24", "2017-12-24", "2018-12-24")),
  end = as.Date(c("2015-12-26", "2016-12-26", "2017-12-26", "2018-12-26"))
  )
HolyWeek <- bsts::DateRangeHoliday(
  "HolyWeek",
  start = as.Date(c("2016-03-20", "2017-04-09", "2018-03-25")),
  end = as.Date(c("2016-03-26", "2017-04-15", "2018-03-31"))
  )
