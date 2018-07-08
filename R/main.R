source("R/theme.R")

## ---- load
library(tidyverse)
library(ochRe)
library(collapsibleTree)
enquiry <- read_rds("data/enquiry.rds") %>%
  filter(channel != "Other") %>%
  mutate(channel = fct_drop(channel)) %>%
  arrange(service, category, channel, date)

## ---- print
enquiry

## ---- tree
enquiry %>%
  distinct(category, service) %>%
  arrange(category) %>%
  collapsibleTree(
    hierarchy = c("category", "service"),
    root = "Brisbane City Councils",
    fill = c(
      "#1b9e77",
      rep("#d95f02", length(unique(.$category))),
      rep("#7570b3", length(unique(.$service)))
    ),
    fontSize = 16,
    height = 600
  )

## ---- tsibble
library(tsibble)
enquiry_tsbl <- enquiry %>%
  as_tsibble(
    key = id(service | category, channel), index = date
  )
enquiry_tsbl

## ---- count-gaps
enquiry_gaps <- enquiry_tsbl %>%
  group_by(channel, category) %>%
  count_gaps(.full = TRUE)

ggplot(enquiry_gaps, aes(colour = channel)) +
  geom_linerange(aes(x = channel, ymin = from, ymax = to)) +
  geom_point(aes(x = channel, y = from), size = 0.5) +
  geom_point(aes(x = channel, y = to), size = 0.5) +
  facet_wrap(~ category, ncol = 3) +
  coord_flip() +
  theme(legend.position = "bottom")

## ---- fill-na1
enquiry_tsbl %>%
  fill_na()

## ---- fill-na2
enquiry_full <- enquiry_tsbl %>%
  fill_na(volume = 0L)
enquiry_full

## ---- full-data
enquiry_full %>%
  ggplot(aes(x = date, y = volume, colour = channel)) +
  geom_line() +
  facet_wrap( ~ category, scales = "free_y", ncol = 2)

## ---- year-month
enquiry_yrmth <- enquiry_full %>%
  group_by(channel, category) %>%
  index_by(yearmth = yearmonth(date)) %>%
  summarise(monthly_volume = sum(volume))

enquiry_yrmth %>%
  ggplot(aes(x = yearmth, y = monthly_volume, colour = channel)) +
  geom_line() +
  facet_wrap( ~ category, scales = "free_y")

## ---- index-by
library(lubridate)
enquiry_full %>%
  group_by(channel, category) %>%
  index_by(year = year(date))

## ---- year
enquiry_year <- enquiry_full %>%
  group_by(channel, category) %>%
  index_by(year = year(date)) %>%
  summarise(annual_volume = sum(volume))
enquiry_year

## ---- col-stack
enquiry_year %>%
  ggplot(aes(x = year, y = annual_volume, fill = channel)) +
  geom_col() +
  facet_wrap(~ category)

## ---- col-fill
enquiry_year %>%
  ggplot(aes(x = year, y = annual_volume, fill = channel)) +
  geom_col(position = "fill") +
  facet_wrap(~ category, labeller = labeller(category = label_wrap_gen(20))) +
  scale_fill_ochre(palette = "healthy_reef") +
  xlab("Year") +
  ylab("Proportion of volume") +
  theme_remark()

## ---- sum
enquiry_sum <- enquiry_full %>%
  summarise(ttl_volume = sum(volume))
enquiry_sum

## ---- slide-hide
enquiry_ma <- enquiry_sum %>%
  mutate(ma = slide_dbl(ttl_volume, mean, .size = 7))
enquiry_ma %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = ttl_volume), colour = "grey80") +
  geom_line(aes(y = ma), colour = "#3182bd", size = 1) +
  xlab("Date") +
  ylab("Total volume") +
  theme_remark()

## ---- slide-hide-animate
library(gganimate)
idx <- lapply(seq_len(nrow(enquiry_ma)), seq_len)
enquiry_ma1 <- as_tibble(map_dfr(idx, function(x) {
  tmp <- enquiry_ma[x, ]
  tmp$group <- length(x)
  tmp
}))

ggplot() +
  geom_line(data = enquiry_ma, aes(x = date, y = ttl_volume), colour = "grey80") +
  geom_line(data = enquiry_ma1, aes(x = date, y = ma, group = group), colour = "#3182bd", size = 1) +
  xlab("Date") +
  ylab("Total volume") +
  transition_manual(group)

## ---- slide-show
enquiry_sum %>%
  mutate(ma = slide_dbl(ttl_volume, mean, .size = 7))

## ---- slide-month-hide
enquiry_sum %>%
  mutate(yrmth = yearmonth(date)) %>%
  nest(-yrmth) %>%
  mutate(ma = slide_dbl(
    data, ~ mean(bind_rows(.)$ttl_volume), .size = 2
  )) %>%
  unnest(data)

## ---- slide-month
enquiry_sum %>%
  mutate(yrmth = yearmonth(date)) %>%
  nest(-yrmth) %>%
  mutate(ma = slide_dbl(data, ~ mean(bind_rows(.)$ttl_volume), .size = 2)) %>%
  unnest(data) %>%
  ggplot() +
  geom_line(aes(x = date, y = ttl_volume), colour = "grey80") +
  geom_line(aes(x = yrmth, y = ma), colour = "#3182bd", size = 2) +
  xlab("Date") +
  ylab("Total volume") +
  theme_remark()
