#Load Libraries

library(tidyverse)
library(dplyr)
library(stringr)
library(fs)
library(reshape2)
library(knitr)
library(lubridate)
library(scales)
library(foreign)
library(janitor)
library(purrr)
library(plm)?nest


# load in data for final project app

main <- read_csv("FIRST_REGRESSIONS_CLEAN copy.csv") %>% clean_names() 

#create log variables of gdp and exports for gravity model estimation

main_transformed <- main %>%  mutate(lnTrade_value = log(trade_value_us), lnGdp_1 = log(gdp_country_1), lnGdp_2 = log(gdp_country_2), year = as.factor(year))

#find FTA Variable coefficient using fixed effects on gravity model of trade

test <- main_transformed %>%
  group_by(panel_number) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(lnTrade_value ~ lnGdp_1 + lnGdp_2 + fta_dummy, data = .) %>%
                       coef %>% 
                       as.list %>%
                       as_tibble)) %>% 
  unnest(model) %>%
  unnest(data) %>% 
  select(lnGdp_1, lnGdp_2, lnTrade_value, panel_number, reporter, partner, year_in_force) %>% 
  distinct(panel_number, .keep_all = TRUE)


# create file and send to shiny app folder 

project_directory <- "/Users/charlesflood/Documents/1005_Final_Project/Final_Project_Mk1" 

write.csv( test ,
           file = file.path( project_directory ,
                             "final_project_stats_table.csv" ) )



  
  

