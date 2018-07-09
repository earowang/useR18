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

## ---- slide-animate
library(gganimate)
slide_window <- slider(enquiry_ma$date, .size = 60) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())
p_slide <- ggplot() +
  geom_line(aes(date, ttl_volume), colour = "grey80", data = enquiry_ma) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = slide_window, fill = "#e6550d", alpha = 0.6) +
  xlab("Date") +
  ylab("Total volume") +
  theme_remark() +
  transition_manual(group)
animate(p_slide, 100, 5, width = 800, height = 200)

tile_window <- tiler(enquiry_ma$date, .size = 60) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())
p_tile <- ggplot() +
  geom_line(aes(date, ttl_volume), colour = "grey80", data = enquiry_ma) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = tile_window, fill = "#e6550d", alpha = 0.6) +
  xlab("Date") +
  ylab("Total volume") +
  theme_remark() +
  transition_manual(group)
animate(p_tile, 100, 5, width = 800, height = 200)

stretch_window <- stretcher(enquiry_ma$date, .init = 60) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())
p_stretch <- ggplot() +
  geom_line(aes(date, ttl_volume), colour = "grey80", data = enquiry_ma) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = stretch_window, fill = "#e6550d", alpha = 0.6) +
  xlab("Date") +
  ylab("Total volume") +
  theme_remark() +
  transition_manual(group)
animate(p_stretch, 100, 5, width = 800, height = 200)

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
