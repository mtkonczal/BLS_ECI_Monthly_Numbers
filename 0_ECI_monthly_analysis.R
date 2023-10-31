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



source("scripts/01_load_ECI_data.R")
source("scripts/02_ECI_analysis.R")



# Graphic that compares now to 1980s SIC values
long_title <- "Nominal wages in line with late 1990s values."
eci_long <- load_eci_long_data("data/eci_long.csv")
draw_long_eci_graphic(eci, eci_long, long_title)
ggsave("graphics/ECI_long.png", width = 12, height=9, dpi="retina")


# Graphic two, seasonal adjusted without performance pay
sa_title <- "Fed's Favorite slows again"
draw_eci_SA_series(eci, sa_title)
ggsave("graphics/ECI_incentive_SA.png", width = 12, height=9, dpi="retina")



eci_cpi_title <- "Hello this is a soft landing"
eci_versus_inflation_graph(eci, eci_cpi_title,4)
ggsave("graphics/eci_difference.png", width = 12, height=9, dpi="retina")


# Graphic of cross section
cross_title = "This is clearly not a demand shock, and more like a supply one."
draw_eci_unemployment(eci, cross_title, naming_threshold = -0.01)
ggsave("graphics/unrate_versus_eci.png", width = 12, height=12, dpi="retina")

# Four top ones
four_title <- "ECI Incomes Broadly Stay the Same in Q3 2023"
draw_toplines_graphic_quick(eci, four_title)
ggsave("graphics/four_bars.png", width = 12, height=12, dpi="retina")

union_title <- "how did this go?"
draw_union(eci, union_title)
ggsave("graphics/ECI_union_YoY.png", width = 9.5, height=5.34, dpi="retina")



compare_title <- "Inflation may be leading wages here"
compare_pce_eci(eci, compare_title)
ggsave("graphics/pce_compare.png", width = 12, height=8, dpi="retina")

