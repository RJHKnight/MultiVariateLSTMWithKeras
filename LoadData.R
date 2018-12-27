library(readr)
library(tidyverse)
library(magrittr)
library(lubridate)

# Load the file from the mirror - this generates some warnings for trailing characters, but data is correct.
pollution <- read_csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/pollution.csv")

# Basic data preparation

# 1 - Correct the date time and rename
pollution %<>%
  mutate(date = ymd_h(paste(year, month, day, hour))) %>%
  select(-No, -year, -month, -day, -hour) %>%
  rename(
    pollution = pm2.5,
    dew = DEWP,
    temp = TEMP,
    press = PRES,
    wnd_dir = cbwd,
    wnd_spd = Iws,
    snow = Is,
    rain = Ir
  )

# Check data - as expected, All NA's for pollution column in the first day
summary(pollution)

pollution %>%
  group_by(dateString = format(date, "%Y.%m.%d")) %>%
  summarise(
    numNA = sum(is.na(pollution)),
    numNonNA = sum(!is.na(pollution))
  ) %>%
  filter(numNA > 0)

# There are actually quite a lot of dates (37) where all pollution data is NA, but the example only excludes the first day...
allNADates <- pollution %>%
  group_by(dateString = format(date, "%Y.%m.%d")) %>%
  summarise(
    numNA = sum(is.na(pollution)),
    numNonNA = sum(!is.na(pollution))
  ) %>%
  filter(numNonNA == 0) %>%
  pull(dateString)

pollution <- tail(pollution, -24)

pollution %<>%
  replace_na(list(pollution = 0))

# And check that we got them all...
summary(pollution)

# And replicate the plot
pollution %>%
  select(-wnd_dir) %>%
  gather(type, value, -date) %>%
  ggplot(., aes(date, value, colour = type)) +
  geom_line() + 
  facet_wrap(~ type, scales = "free_y")

