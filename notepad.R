# Mike Konczal

setwd("/Users/mkonczal/Documents/R Projects/ECI update/")
library(hrbrthemes)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(janitor)
library(seasonal)
library(xts)

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

source("1_load_ECI.R")

title_three_wages <- "Wage Growth Slows Last Quarter"
title_ip <- "The Fed's Favorite Metric Slows Again"
title_long <- "Wage Growth in Line With Historical Values?"


#### Graphic Long Term ####
eci_long <- read_csv("data/eci_long.csv") %>% filter(type == "ECI Wage Growth (SOC)") %>% mutate(date = date %m-% months(1))

eci_long <- eci %>% filter(series_id == "CIS2020000000000Q") %>% select(date, value) %>% mutate(type = "ECI Wage Growth (NAICS)") %>%
  rbind(eci_long) %>% mutate(value = (1+value/100)^4-1)

eci_long_dates <- unique(eci_long$date)
eci_long_dates <- sort(eci_long_dates, decreasing = TRUE)
eci_long_dates = eci_long_dates[seq(1, length(eci_long_dates), 12)]
final_value = eci_long[which.max(eci_long$date),2]

eci_long  %>%
  mutate(typeF = factor(type, levels = c("ECI Wage Growth (SOC)", "ECI Wage Growth (NAICS)"))) %>%
  mutate(last_value = value[date==max(date)]) %>%
  ggplot(aes(date, value,color=typeF)) + geom_line(size=1) +
  labs(x="",y="", title=title_long,
       subtitle=paste("ECI: 3-month percent change, annualized, wages and salaries for private workers.","\n",
       "Dotted green line reflects last quarter's value of ", label_percent(accuracy=0.01)(as.numeric(final_value)), sep=""),
       caption="(NAICS) and (SOC) refer to North American Industry Classification System and the Standard Occupational Classification System.
       BLS, ECI, Seasonally-Adjusted, Author's Calculations, Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b\n%Y", breaks=eci_long_dates) +
  scale_y_continuous(labels = scales::percent) + theme_lass +
  theme(legend.position = c(0.25,0.8)) +
  scale_color_manual(values=c("#6EA4BF","#1E8456")) +
  # COMMENT OUT LAST LINE IF WE DON'T WANT LINE
  geom_line(aes(date,last_value), size=0.6, color="#E2E47E", alpha=0.6, linetype = "dashed")

ggsave("graphics/ECI_long.png", width = 9.5, height=5.34, dpi="retina")

#### FIRST GRAPHIC - BY CATEGORY ####
eci_toplines <- eci %>% mutate(value = value) %>% filter(date >= "2017-01-01") %>%
  filter(estimate_code == "02", periodicity_code == "Q", owner_code == 2,
         occupation_code == "000000", seasonal == "S", industry_code %in% c("G00000", "S00000", "000000"))

eci_g1_dates <- unique(eci_toplines$date)
eci_g1_dates <- sort(eci_g1_dates, decreasing = TRUE)
eci_g1_dates = eci_g1_dates[seq(1, length(eci_g1_dates), 4)]

eci_toplines  %>% mutate(value = value/100, value=(1+value)^4-1) %>% #left_join(annual_averages, by="series_title")
  ggplot(aes(date, value)) + geom_line(size=1) + facet_wrap(~industry_text) +
  labs(x="",y="", title=title_three_wages,
       subtitle="ECI: 3-month percent change, annualized, wages and salaries for private workers.",
       caption="BLS, ECI, Seasonally-Adjusted, Author's Calculations, Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b\n%Y", breaks=eci_g1_dates) +
  scale_y_continuous(labels = scales::percent) + theme_lass

ggsave("graphics/ECI_three_wages.png", width = 9.5, height=5.34, dpi="retina")



### First with excluding “incentive paid occupations” #####

incentive_pay <- eci %>% filter(series_id == "CIU2020000000710Q")

incentive_pay_ts <- ts(incentive_pay$value,start=c(min(incentive_pay$year),2),frequency = 4)
m <- seas(incentive_pay_ts)
incentive_pay_SA <- final(m)

incentive_pay_ts <- data.frame(value=as.matrix(incentive_pay_SA), date=as.Date(as.yearmon(time(incentive_pay_SA)))) %>% mutate(type = "Seasonally-Adjusted, Author's Calculations")
incentive_pay_ts$date <- incentive_pay_ts$date  %m-% months(1)

eci_ip_dates <- unique(incentive_pay_ts$date)
eci_ip_dates <- sort(eci_ip_dates, decreasing = TRUE)
eci_ip_dates = eci_ip_dates[seq(1, length(eci_ip_dates), 2)]

incentive_pay %>% select(value, date) %>% mutate(type = "Not Seasonally Adjusted, BLS") %>%
  rbind(incentive_pay_ts) %>% mutate(value = (1+value/100)^4-1) %>%
  filter(date > "2014-01-01") %>%
  mutate(typeF = factor(type, levels = c("Seasonally-Adjusted, Author's Calculations", "Not Seasonally Adjusted, BLS"))) %>%
  mutate(num_label = round(100*value, 1)) %>%
  mutate(num_label2 = ifelse(date >= "2021-09-01", num_label, NA)) %>%
  mutate(num_label2 = ifelse(type == "Seasonally-Adjusted, Author's Calculations", num_label2, NA)) %>%
  ggplot(aes(date, value, color=typeF, size=typeF, alpha=typeF)) + geom_line() + theme(legend.position='bottom') +
  labs(x="", y="", title=title_ip,
       subtitle="Wages, private workers, excluding incentive paid occupations, 3 month percent change annualized",
       caption="Seasonal adjustment calculated with R's seasonal package, using X-13ARIMA-SEATS.
       BLS, ECI, author's calculations, Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b\n%Y", breaks=eci_ip_dates) +
  scale_y_continuous(labels = scales::percent) + theme_lass + theme(legend.position = c(0.6,0.8)) +
  scale_color_manual(values=c("#97BC56","grey")) +
  scale_size_manual(values=c(1.2,1)) +
  scale_alpha_manual(values=c(1,0.5)) +
  geom_text(aes(x=date, y=value, label=num_label2), nudge_y = 0.003, size=3.5, color="#E2E47E", show.legend = FALSE)

ggsave("graphics/ECI_incentive_SA.png", width = 9.5, height=5.34, dpi="retina")


##### BREAK DOWN INCENTIVE PAY #####

tester <- eci %>% filter(date == max(date))
comp_filter <- c("CIU202G000000710Q", "CIU202G000000710A", "CIU202G000000710I","CIU202S000000710A","CIU202S000000710I","CIU202S000000710Q", "CIU2020000000710Q")

eci %>% filter(series_id %in% comp_filter) %>% filter(periodicity_code == "Q") %>%
  ggplot(aes(date, value, color=industry_code)) + geom_line() + theme_classic() + theme(legend.position='bottom') +
  labs(x="", y="", title="Wages and salaries for Private industry workers, excluding incentive paid occupations, 3-month percent change")



### Blergh, pass:
#### First real graphic - faceted real and nominal, not working yet (mutliple values in date times?)
eci %>%
  filter(estimate_code == "02", periodicity_code %in% c("Q","X"), owner_code == 2, subcell_code == "00",
         occupation_code == "000000", seasonal == "U", industry_code %in% c("G00000", "S00000")) %>%
  ggplot(aes(date, value, color=industry_text)) + geom_line() + facet_wrap(~ periodicity_text, nrow = 1) +
  theme_classic() + theme(legend.position='bottom')



# Test graphics
eci_a <- eci %>% filter(date == max(date)) %>%
  filter(estimate_code == "02", periodicity_code %in% c("Q","X"), owner_code == 2,
         occupation_code == "000000", seasonal == "S")

eci_b <- eci %>% filter(date == max(date)) %>%
  filter(estimate_code == "02", periodicity_code %in% c("Q","X"), owner_code == 2, subcell_code == "00",
         occupation_code == "000000", seasonal == "U", industry_code %in% c("G00000", "S00000"))

# Test graphics
eci %>% filter(year >= 2015) %>%
  filter(estimate_code == "02", periodicity_code == "Q", owner_code == 2,
         industry_code == "000000", seasonal == "S") %>%
  ggplot(aes(date, value, color=occupation_text)) + geom_line() + theme_classic() + theme(legend.position='bottom')




##### Graphic 2 - Year Over Year
####
eci %>% mutate(value = value) %>% filter(date >= "2016-01-01") %>%
  filter(estimate_code == "02", periodicity_code == "I", owner_code == 2,
         occupation_code == "000000", seasonal == "S", industry_code %in% c("G00000", "S00000", "000000")) %>%
  group_by(series_title) %>% arrange(date) %>%
  mutate(YoY = value/lag(value,4)-1) %>% ungroup() %>%
  ggplot(aes(date, YoY)) + geom_line(size=1) + facet_wrap(~industry_title) +
  labs(x="",y="", title="Graphic 1: Wage growth remains high",
       subtitle="12-month percent change, wages and salaries for private workers",
       caption="BLS, ECI, Seasonally-Adjusted, Author's Calculations, Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b %y") +
  scale_y_continuous(labels = scales::percent) + theme_lass

check <- eci %>% mutate(value = value) %>% filter(date >= "2016-01-01") %>%
  filter(estimate_code == "02", periodicity_code == "I", owner_code == 2,
         occupation_code == "000000", seasonal == "S", industry_code %in% c("000000")) %>%
  group_by(series_title) %>% arrange(date) %>%
  mutate(YoY = value/lag(value,4)-1) %>% ungroup() %>% arrange(desc(date))
ggsave("graphics/ECI_test1.png", width = 9.5, height=5.34, dpi="retina")



##### GOOD #### Graphic 3 - Union and Nonunion:
## YEAR OVER YEAR VERSION
eci %>%
  filter(series_id %in% c("CIU2020000000510A", "CIU2020000000520A")) %>% mutate(value = value/100) %>%
  ggplot(aes(date, value)) + geom_line(size=1) + facet_wrap(~subcell_text) +
  labs(x="",y="", title="Indexed What? Union pay increases lag nonunion",
       subtitle="ECI, 12-month percent change, wages and salaries for private workers, not seasonally adjusted",
       caption="BLS, ECI, Author's Calculations, Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b %y") +
  scale_y_continuous(labels = scales::percent) + theme_lass

ggsave("graphics/ECI_union_YoY.png", width = 9.5, height=5.34, dpi="retina")




###### GRID SYSTEM #####

a <- eci %>% filter(date == max(date), periodicity_code == "Q", display_level.x == 0, display_level.x.x == 0)
b <- eci %>% filter(date == max(date), periodicity_code == "Q", seasonal == "S") %>% select(series_id, series_title)

toplines <- c("CIS1010000000000Q","CIS1020000000000Q","CIS2010000000000Q","CIS2020000000000Q")
toplines_benefits <- c(toplines, "CIS2030000000000Q","CIS1030000000000Q")

eci %>% filter(series_id %in% toplines_benefits) %>%
  mutate(series_title = str_remove_all(series_title, ", 3-month percent change")) %>%
  ggplot(aes(date,value)) + geom_line() + facet_wrap(~series_title) +
  theme(strip.text = element_text(face = "bold", color="black", hjust = 0.5, size = 5))


ggsave("graphics/gridview_sample.png", width = 9.5, height=5.34, dpi="retina")

c <- eci %>% filter(series_id %in% toplines) %>% filter(year > 2019)

d <- eci %>%   filter(estimate_code == "02", periodicity_code == "Q", owner_code == 2,
                      industry_code == "000000", seasonal == "S"
                      
                      e <- eci %>%   filter(estimate_code == "02", periodicity_code == "Q", owner_code == 2,
                                            occupation_code == "000000", seasonal == "S", date==max(date))
                      
                      eci %>% filter(estimate_code == "02", periodicity_code == "Q", owner_code == 2, seasonal == "S") %>%
                        filter(industry_code == "000000" | occupation_code == "000000") %>%
                        group_by(series_id) %>% arrange(date) %>% mutate(value_change = value - lag(value,4)) %>%
                        ungroup() %>% filter(date == max(date)) %>%
                        mutate(series_title = str_remove_all(series_title, "Wages and salaries for Private industry workers in")) %>%
                        mutate(series_title = str_remove_all(series_title, "3-month percent change")) %>%
                        ggplot(aes(series_title,value_change)) + geom_bar(stat="identity") + theme_classic() + coord_flip()
                      
                      
                      eci %>% filter(estimate_code == "02", periodicity_code == "Q", owner_code == 2, seasonal == "S") %>%
                        filter(industry_code == "000000" | occupation_code == "000000") %>%
                        group_by(series_id) %>% arrange(date) %>% mutate(value_change = value - lag(value,4)) %>%
                        ungroup() %>% filter(date == max(date) | date == "2021-12-01") %>%
                        mutate(series_title = str_remove_all(series_title, "Wages and salaries for Private industry workers in")) %>%
                        mutate(series_title = str_remove_all(series_title, "3-month percent change")) %>%
                        ggplot(aes(series_title,value, fill=value_change)) + geom_bar(stat="identity") + theme_classic() + coord_flip()
                      