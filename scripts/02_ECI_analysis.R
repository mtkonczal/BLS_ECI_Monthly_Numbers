# Mike Konczal
library(hrbrthemes)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(janitor)
library(seasonal)
library(xts)
library(ggh4x)
library(gt)
library(quantmod)


##### SET UP SOME THINGS #####
theme_lass <-   theme_modern_rc(ticks = TRUE) + theme(legend.position = "none", legend.title = element_blank(),
                                                      panel.grid.major.y = element_line(size=0.5),
                                                      panel.grid.minor.y = element_blank(),
                                                      plot.title.position = "plot",
                                                      axis.title.x = element_blank(),
                                                      axis.title.y = element_blank(),
                                                      plot.title = element_text(size = 25, face="bold"),
                                                      plot.subtitle = element_text(size=15, color="white"),
                                                      plot.caption = element_text(size=10, face="italic"),
                                                      legend.text = element_text(size=12),
                                                      axis.text.y = element_text(size=12, face="bold"),
                                                      axis.text.x = element_text(size=12, face="bold"),
                                                      strip.text = element_text(face = "bold", color="white", hjust = 0.5, size = 10),
                                                      panel.grid.major.x = element_blank(),
                                                      panel.grid.minor.x = element_blank(),
                                                      strip.background = element_blank()) +
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit"))

load_eci_long_data <- function(location) {
  # Data is originally from here:
  # https://www.bls.gov/eci/data.htm
  eci_long <- read_csv(location) %>%
    filter(type == "ECI Wage Growth (SIC)") %>%
    mutate(date = date %m-% months(1))
  return(eci_long)
}

prep_FRED_data <- function(x) {
  getSymbols(x, src="FRED")
  df <- get(x)
  df <- as_tibble(data.frame(Date = index(df))) %>%
    bind_cols(setNames(list(as.numeric(df[, x])), x))
  colnames(df) <- tolower(colnames(df))
  return(df)
}


draw_eci_unemployment <- function(eci, graphic_title = "This is clearly not a demand shock, which leaves us with a supply one.", quarter_lag = 4, naming_threshold = -0.01){

  eci_long <- read_csv("data/sic_eci_yoy_long.csv") %>%
    select(date,value) %>%
    filter(date < "2002-03-01") %>%
    mutate(value = value/100)

  eci_full <- eci %>%
    filter(series_id == "CIS2020000000000I") %>%
    select(date, value) %>%
    mutate(value = (value/lag(value,quarter_lag)-1)) %>%
    na.omit() %>%
    rbind(eci_long) %>%
    arrange(date)

  unrate <- prep_FRED_data("UNRATE") %>% mutate(unrate = unrate/100)
  
  eci_full_graphic <- eci_full %>%
    left_join(unrate, by="date") %>%
    rename(eci_wages = value) %>%
    mutate(eci_wagesP = eci_wages - lag(eci_wages,quarter_lag),
           unrateP = unrate - lag(unrate, quarter_lag),
           max_value = date < max(date) %m-% months(3),
           max_value = as.factor(max_value),
           name_value = if_else(eci_wagesP < naming_threshold | date == max(date), as.character(format(date, '%b\n%Y')), as.character(NA)))
  
  lm_model <- lm(eci_wagesP ~ unrate, data = eci_full_graphic)
  eci_full_graphic$predicted <- predict(lm_model, eci_full_graphic)
  
  eci_full_graphic %>%
    ggplot(aes(unrate, eci_wagesP, color=max_value, label=name_value)) +
    theme_lass +
    geom_point() +
    geom_text_repel() +
    geom_line(aes(unrate, predicted), color="#FC8D62") +
    labs(title = graphic_title,
         subtitle = "Change in year-over-year private ECI nominal wage growth from a year ago, versus the unemployment rate. 1976-2023.",
         caption = "ECI data prior to 2002 from SIC, NAICS after. Seasonally adjusted. Mike Konczal, Roosevelt Institute.",
         x = "Unemployment Rate",
         y = "Change in ECI year-over-year private wage growth from one year ago") +
    theme(axis.text.x = element_text(size=15, face="bold"),
          axis.text.y = element_text(size=15, face="bold"),
          axis.title.x = element_text(size=15, margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(size=14, angle = 90, color="white", vjust = 3)) +
    scale_x_continuous(label=percent, breaks = seq(.035,.11, .015)) +
    scale_y_continuous(label=percent) +
    scale_color_brewer(palette="Set2")
  
}



#### Graphic Long Term ####4287f5
draw_long_eci_graphic <- function(eci, eci_long, graphic_title, legend_length = 12) {
  eci_long <- eci %>%
    filter(series_id == "CIS2020000000000Q") %>%
    select(date, value) %>%
    mutate(type = "ECI Wage Growth (NAICS)") %>%
    rbind(eci_long) %>%
    group_by(type) %>%
    mutate(value = (1 + value / 100)^4 - 1) %>%
    ungroup() %>%
    arrange(date)

  date_breaks <- sort(unique(eci_long$date), decreasing = TRUE)
  date_breaks <- date_breaks[seq(1, length(date_breaks), legend_length)]
  final_value = eci_long[which.max(eci_long$date),2]

  eci_long %>%
    mutate(typeF = factor(type, levels = c("ECI Wage Growth (SIC)", "ECI Wage Growth (NAICS)"))) %>%
    mutate(last_value = value[date == max(date)]) %>%
    ggplot(aes(date, value, color = typeF)) +
    geom_line(size = 1) +
    labs(
      x = "", y = "", title = graphic_title,
      subtitle = paste("ECI: 3-month percent change, annualized, nominal wages and salaries for private workers.", "\n",
        "Dotted green line reflects last quarter's value of ", label_percent(accuracy = 0.01)(as.numeric(final_value)),
        sep = ""
      ),
      caption = "(NAICS) and (SOC) refer to North American Industry Classification System and the Standard Occupational Classification System.
       BLS, ECI, Seasonally-Adjusted, Author's Calculations, Mike Konczal, Roosevelt Institute"
    ) +
    scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks) +
    scale_y_continuous(labels = scales::percent) +
    theme_lass +
    theme(legend.position = c(0.25, 0.8)) +
    scale_color_manual(values = c("#6EA4BF", "#1E8456")) +
    # COMMENT OUT LAST LINE IF WE DON'T WANT LINE
    geom_line(aes(date, last_value), size = 0.6, color = "#E2E47E", alpha = 0.6, linetype = "dashed")
}

seasonal_adjustment <- function(eci, ts_series_id = "CIU2020000000710Q") {
  incentive_pay <- eci %>% filter(series_id == ts_series_id)
  incentive_pay_ts <- ts(incentive_pay$value, start = c(min(incentive_pay$year), 2), frequency = 4)
  m <- seas(incentive_pay_ts)
  incentive_pay_SA <- final(m)
  incentive_pay_ts <- data.frame(value = as.matrix(incentive_pay_SA), date = as.Date(as.yearmon(time(incentive_pay_SA)))) %>% mutate(type = "Seasonally-Adjusted, Author's Calculations")
  incentive_pay_ts$date <- incentive_pay_ts$date %m-% months(1)
  return(incentive_pay_ts)
}

draw_eci_SA_series <- function(eci, sa_title = "Default title", ts_series_id = "CIU2020000000710Q") {
  incentive_pay_ts <- seasonal_adjustment(eci)
  eci_ip_dates <- unique(incentive_pay_ts$date)
  eci_ip_dates <- sort(eci_ip_dates, decreasing = TRUE)
  eci_ip_dates <- eci_ip_dates[seq(1, length(eci_ip_dates), 2)]
  incentive_pay <- eci %>% filter(series_id == ts_series_id)

  incentive_pay_graphic <- incentive_pay %>%
    select(value, date) %>%
    mutate(value = (1 + value / 100)^4 - 1) %>%
    filter(date > "2018-01-01") %>%
    mutate(num_label = round(100 * value, 1)) %>%
    mutate(num_label2 = ifelse(date >= "2021-09-01", num_label, NA))
  
  ip_line <- incentive_pay %>%
    filter(year(date)>=2018 & year(date) <=2019) %>%
    select(date, value) %>%
    mutate(ip_line = cumprod(1+value/100),
              ip_lineA = ip_line^(0.5)-1) %>%
    filter(date == max(date)) %>%
    pull(ip_lineA)
  
  incentive_pay_graphic %>%
    ggplot(aes(date, value)) +
    geom_bar(stat="identity",size=0, fill="#4287f5") +
    geom_hline(yintercept = ip_line, color="#f7dc6f", linetype = "dashed", size=1.2) +
    theme(legend.position = "bottom") +
    labs(
      x = "", y = "", title = sa_title,
      subtitle = "Wages, private workers, excluding incentive paid occupations, 3-month percent change annualized.\nDotted line 2018-2019 average. Seasonal adjustment calculated with R's seasonal library, using X-13ARIMA-SEATS.",
      caption = "BLS, ECI, author's calculations, Mike Konczal, Roosevelt Institute"
    ) +
    scale_x_date(date_labels = "%b\n%Y", breaks = eci_ip_dates) +
    scale_y_continuous(labels = scales::percent) +
    theme_lass +
    geom_text(aes(x = date, y = value, label = num_label2), nudge_y = 0.0017, size = 4.5, color = "#FFFFFF", show.legend = FALSE)
}

  
  
  prep_FRED_data <- function(x) {
    getSymbols(x, src="FRED")
    df <- get(x)
    df <- as_tibble(data.frame(Date = index(df))) %>%
      bind_cols(setNames(list(as.numeric(df[, x])), x))
    colnames(df) <- tolower(colnames(df))
    return(df)
  }
  
  eci_versus_inflation_graph <- function(eci, graphic_title="This is a test title.", legend_length = 4){
    
    
    cpi <- prep_FRED_data("CPIAUCSL") %>%
      mutate(cpi = (cpiaucsl/lag(cpiaucsl,3))^4-1) %>%
      select(date, cpi)
    
    #cpi <- read_csv("data/cpi_q2_2023.csv") %>% clean_names() %>% rename(cpi = cpiaucsl) %>%
    #  mutate(cpi = cpi/lag(cpi,1)-1,
    #         date = date %m+% months(2))
    date_breaks <- sort(unique(eci$date), decreasing = TRUE)
    date_breaks <- date_breaks[seq(1, length(date_breaks), legend_length)]
    
    eci %>% filter(series_id == "CIS2020000000000Q") %>%
      select(date, value, series_title) %>%
      mutate(value = (1+value/100)^4-1) %>%
      left_join(cpi, by="date") %>%
      rename(wages = value) %>%
      filter(year(date) > 2017) %>%
      ggplot(aes(x = date)) +
      # One line for Cat rescues
      geom_line(aes(y = cpi, color = "CPI, Headline"), linewidth=1.2) +
      # Another line for Not_Cat rescues
      geom_line(aes(y = wages, color = "ECI, Private Wages"), linewidth=1.2) +
      # stat_difference() from ggh4x package applies the conditional fill
      # based on which of Not_Cat and Cat is larger.
      stat_difference(aes(ymin = wages, ymax = cpi), alpha = 0.25) + theme_lass +
      scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks) +
      scale_y_continuous(labels = scales::percent) + theme_lass + theme(legend.position = c(0.25,0.8)) +
      scale_color_manual(values = c("#E57A77", "#2ECC71")) +
      # Colors for the fill. They are lighter versions of the line colors.
      # The third one is required because lines are sometimes equal
      scale_fill_manual(
        values = c(
          colorspace::lighten("#E57A77"), 
          colorspace::lighten("#2ECC71"), 
          "grey60"
        ),
        labels = c("Real Wages Falling", "Real Wages Increasing")
      ) +
      guides(
        # Order indicates the order of each legend among multiple guides.
        # The guide for 'color' will be placed before the onde for 'fill'
        color = guide_legend(order = 1), 
        fill = guide_legend(order = 2)
      ) +
      labs(subtitle="ECI wages and salaries for all private industry workers. Headline CPI. quarterly change, annualized.",
           caption="Seasonally adjusted. Author's calculations, Mike Konczal, Roosevelt Institute",
           title=graphic_title)
  }  
  


##### GOOD #### Graphic 3 - Union and Nonunion:
## YEAR OVER YEAR VERSION
draw_union <- function(eci, graphic_title = "TKTKT"){
  eci %>%
    filter(series_id %in% c("CIU2020000000510A", "CIU2020000000520A")) %>% mutate(value = value/100) %>%
    ggplot(aes(date, value)) + geom_line(size=1.2, color="#97BC56") + facet_wrap(~subcell_text) +
    labs(x="",y="", title=graphic_title,
         subtitle="ECI, 12-month percent change, wages and salaries for private workers",
         caption="BLS, ECI, Not seasonally adjusted, Author's Calculations, Mike Konczal, Roosevelt Institute") +
    scale_x_date(date_labels = "%b %y") +
    scale_y_continuous(labels = scales::percent) + theme_lass
}



#### TOPLINES ####
draw_toplines_graphic_quick <- function(eci, graphic_title = "Hello World!") {

  toplines <- c("CIS1010000000000Q", "CIS1020000000000Q", "CIS2010000000000Q", "CIS2020000000000Q")
  
  eci %>%
    filter(series_id %in% toplines) %>%
    mutate(series_title = str_remove_all(series_title, " in All industries and occupations, 3-month percent change")) %>%
    mutate(typeF = factor(series_title,
      levels =
        c(
          "Wages and salaries for Private industry workers", "Wages and salaries for All Civilian workers",
          "Total compensation for Private industry workers", "Total compensation for All Civilian workers"
        )
    )) %>%
    filter(date >= "2019-01-01") %>%
    mutate(value = (1 + value / 100)^4 - 1) %>%
    mutate(num_label = round(100 * value, 1)) %>%
    mutate(num_label2 = ifelse(date >= "2021-09-01", num_label, NA)) %>%
    ggplot(aes(date, value, label = num_label2, color=typeF, fill=typeF)) +
    geom_bar(stat = "identity", size = 0) +
    facet_wrap(~typeF) +
    labs(
      x = "", y = "", title = graphic_title,
      subtitle = "Wages, private workers, 3 month percent change annualized.",
      caption = "BLS, ECI, author's calculations, Mike Konczal, Roosevelt Institute"
    ) +
    scale_x_date(date_labels = "%b\n%Y") +
    scale_y_continuous(labels = scales::percent) +
    theme_lass +
    theme(legend.position = "none") +
    geom_text(aes(x = date, y = value, label = num_label2), nudge_y = 0.0025, size = 3.5, show.legend = FALSE) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Set2")
  
}

draw_chart <- function(eci, start_date, graphic_title = "TKTKT"){
  start_date <- as.Date("2018-12-01")
  interval_since_2019 <- interval(start_date, max(eci$date)) / months(1)
  cpi <- prep_FRED_data("CPIAUCSL") %>%
    rename(cpi = cpiaucsl) %>%
    mutate(this = (cpi[date==max(date)]/cpi[date==start_date])^(12/interval_since_2019) -1) %>%
    filter(date == max(date)) %>%
    pull(this)
  
  ahe <- prep_FRED_data("CES0500000003") %>%
    rename(value = ces0500000003) %>%
    mutate(series_title = "Average Hourly Earnings, CES") %>%
    select(series_title, date, value)
  
  cpi <- prep_FRED_data("CPIAUCSL") %>%
    rename(cpi = cpiaucsl) 
  
#  eci_tester <- eci %>% left_join(cpi, by="date")
  toplines <- c("CIS1010000000000I","CIS1020000000000I","CIS2010000000000I","CIS2020000000000I")
  
#  a <- eci %>% filter(date == max(date), seasonal == "S", periodicity_code == "I", occupation_code == "000000",
#                      estimate_text == "Wages and salaries", owner_text == "Private industry workers") %>% pull(series_id)
  #toplines <- a
  
  b <- eci  %>% filter(series_id %in% toplines) %>%
    select(series_title, date, value) %>%
    rbind(ahe) %>%
    left_join(cpi, by="date")
  
  c <- b %>%
    mutate(real_value = value/cpi) %>%
    group_by(series_title) %>%
    summarize(month3_change = (value[date==max(date)]/value[date == max(date) %m-% months(3)])^4-1,
              month6_change = (value[date==max(date)]/value[date == max(date) %m-% months(6)])^2-1,
              change_since_2019 = (value[date==max(date)]/value[date=="2018-12-01"])^(12/interval_since_2019) -1,
              real_change_since_2019 = (real_value[date==max(date)]/real_value[date==start_date])^(12/interval_since_2019) -1,
              real_change_since_2022 = (real_value[date==max(date)]/real_value[date=="2022-12-01"])^(12/interval_since_2019) -1)

  
  c %>%
    mutate(series_title = str_remove(series_title, ", Index"),
           series_title = str_remove(series_title, " Civilian"),
           series_title = str_remove(series_title, " and occupations"),
           series_title = str_remove(series_title, " industry"),) %>%
    gt() %>%
#    gt(groupname_col = "chart_type") %>%
    tab_header(title=md("**What are the ranges of current wage growth?**"),
               subtitle = "A list of wage growth from multiple sources.") %>%
    fmt_percent(decimals = 1)
  
  
#  %>%
 #   cols_label(
  #    value = "Initial Value",
   #   yoy = html("Year-over-Year\n2023"),
    #  month6 = "2nd Half 2023",
#      type = ""
 #   ) %>%
  #  tab_source_note(
   #   source_note = "Random sampling done at monthly level for both sampling and forecasting, 10K trials."
#    ) %>%
#    tab_source_note(
#      source_note = "All values annualized. Preliminary. Mike Konczal, Roosevelt Institute"
#    ) %>%
#    fmt_percent(decimals = 1) %>%
#    opt_stylize(style = 6, color = 'blue') %>%
#    sub_missing(column = value, missing_text = "") %>%
#    gtsave(., filename="graphic/projections_test.png")
  
    
  return(b)
  #tail(eci %>% filter(series_id == "CIS1010000000000I") %>% select(date, series_title, value))
}

compare_pce_eci <- function(eci, graphic_title = "Default title") {
  pce <- prep_FRED_data("PCEPI")
  pce <- pce %>% rename(pce = pcepi)
  
  
  eci %>%
    filter(series_id == "CIS2020000000000I") %>%
    select(date, wages = value) %>%
    left_join(pce, by = "date") %>%
    mutate(
      wages = wages / lag(wages, 4) - 1,
      pce = pce / lag(pce, 4) - 1
    ) %>%
    pivot_longer(wages:pce, names_to = "types", values_to = "values") %>%
    mutate(types = if_else(types == "pce", "Headline PCE, Year over Year Change", "Private ECI Wages, Year over Year Change")) %>%
    ggplot(aes(date, values, color = types)) +
    theme_lass +
    geom_line(size = 1.2) +
    labs(
      title = "Inflation may be leading wages here",
      subtitle = "Year over year change, ECI private wages, headline PCE inflation.",
      caption = "Mike Konczal, Roosevelt Institute."
    ) +
    scale_y_continuous(label = percent) +
    scale_color_brewer(palette = "Set2") +
    theme(legend.position = c(0.5, 0.8))
}
