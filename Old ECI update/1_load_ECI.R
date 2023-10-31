# Mike Konczal

library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(janitor)

#### LOAD ECI DATA ########

eci_wages_start <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.data.1.AllData") %>%
  clean_names()
eci_wages_start$value <- as.numeric(eci_wages_start$value)

eci_wages_series <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.series") %>%
  clean_names()
eci_wages_series$series_id <- str_trim(eci_wages_series$series_id)
eci_industry_series <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.industry") %>%
  clean_names()
eci_owner_series <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.owner") %>%
  clean_names()
eci_subcell <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.subcell") %>%
  clean_names()
eci_occupation_series <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.occupation") %>%
  clean_names()
eci_period <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.periodicity") %>%
  clean_names()
eci_estimate <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.estimate") %>%
  clean_names()

eci_wages_next <- eci_wages_start %>%
  inner_join(eci_wages_series, by="series_id") %>%
  inner_join(eci_industry_series, by="industry_code") %>%
  inner_join(eci_occupation_series, by="occupation_code") %>%
  inner_join(eci_period, by="periodicity_code") %>%
  inner_join(eci_estimate, by="estimate_code") %>%
  inner_join(eci_owner_series, by="owner_code") %>%
  inner_join(eci_subcell, by="subcell_code")

eci_wages_next <- eci_wages_next %>%
  mutate(month = case_when(
    period == "Q01" ~ 3,
    period == "Q02" ~ 6,
    period == "Q03" ~ 9,
    period == "Q04" ~ 12)) # %>%
  #mutate(year = if_else(period == "Q04", year+1, year))
eci_wages_next$date <- paste(eci_wages_next$month, "01", eci_wages_next$year, sep="/")
eci_wages_next$date <- as.Date(eci_wages_next$date, "%m/%d/%Y")

eci <- eci_wages_next

rm(eci_estimate, eci_subcell, eci_owner_series, eci_period, eci_occupation_series, eci_industry_series, eci_wages_series, eci_wages_next, eci_wages_start)