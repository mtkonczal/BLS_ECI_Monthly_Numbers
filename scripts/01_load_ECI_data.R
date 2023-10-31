# Mike Konczal

library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(janitor)

library(httr)
library(data.table)
library(magrittr)


# This file downloads the CPI files from BLS.gov, formats them,
# and merges them.

# Written by: Mike Konczal
# Last updated: 10/10/2023

# Libraries
library(janitor)
library(tidyverse)
library(httr)
library(data.table)

# Configurations
config <- list(
  user_email = "rortybomb@gmail.com",
  cpi_data_url = "https://download.bls.gov/pub/time.series/ci/ci."
)

#' Get and Process Data
#' This function retrieves and processes data from a specified endpoint.
download_data <- function(endpoint, base_url, user_email) {
  response <- GET(paste0(base_url, endpoint), user_agent(user_email))
  
  if (http_error(response)) {
    stop("Data could not be downloaded from URL: ", paste0(base_url, endpoint))
  }
  
  data <- content(response, as = "text") %>%
    fread() %>%
    clean_names()
  
  return(data)
}

# SECTION 1: READ IN AND CLEAN UP DATA
endpoints <- c("data.1.AllData","series","industry","owner","subcell","occupation","periodicity","estimate")

for(i in endpoints){
  assign(i,download_data(i,config$cpi_data_url,config$user_email))
}

data.1.AllData <- data.1.AllData %>%
  mutate(
    value = as.numeric(value),
    series_id = str_trim(series_id)
  )

subcell$subcell_code <- as.numeric(subcell$subcell_code)

series <- series %>%
  mutate(series_id = str_trim(series_id)) %>%
  inner_join(industry, by="industry_code") %>%
  inner_join(occupation, by="occupation_code") %>%
  inner_join(periodicity, by="periodicity_code") %>%
  inner_join(estimate, by="estimate_code") %>%
  inner_join(owner, by="owner_code") %>%
  inner_join(subcell, by="subcell_code")

eci <- inner_join(data.1.AllData, series, by="series_id")

eci <- eci %>%
  mutate(month = case_when(
    period == "Q01" ~ 3,
    period == "Q02" ~ 6,
    period == "Q03" ~ 9,
    period == "Q04" ~ 12))

eci$date <- paste(eci$month, "01", eci$year, sep="/")
eci$date <- as.Date(eci$date, "%m/%d/%Y")


rm(data.1.AllData,series,industry,owner,subcell,occupation,periodicity,estimate, config, endpoints, i, download_data)