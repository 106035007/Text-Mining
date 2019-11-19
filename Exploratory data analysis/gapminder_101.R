rm(list=ls())
# Prepare the nested dataframe gap_nested
library(tidyverse)
library(gapminder)
library(skimr)
library(rsample)

# import data gapminder
gapminder <- read_rds('../DataScience/Tidyverse in ML/gapminder.rds')
# write_csv(gapminder, '../DataScience/Exploratory data analysis/gapminder.csv')
#data(gapminder)
head(gapminder)
str(gapminder)

# Q1: Examine NAs in the data----
gapminder_nas <- gapminder %>% 
                 purrr::map_df(~sum(is.na(.)))
gapminder_nas

# Q2: Examine your numeric columns----

gapminder %>% select(-year) %>% select_if(is.numeric) %>% 
              skimr::skim()

#
gap_nested <- gapminder %>% 
  group_by(country) %>% 
  nest()
  
gap_nested

# Create the unnested dataframe called gap_unnnested
gap_unnested <- gap_nested %>% 
                unnest()
gap_unnested

# Q3: Calculate the average life expectancy per country. ----
# Which has the longest average life expectancy and which has the shortest average life expectancy?
lifeExp_bycountry <- gapminder %>%
                     group_by(country) %>%
                     summarize(mean_lifeExp = mean(life_expectancy))
                     
lifeExp_bycountry %>%
                  filter(mean_lifeExp == min(mean_lifeExp) | mean_lifeExp == max(mean_lifeExp))

lifeExp_bycountry %>%
  arrange(mean_lifeExp) %>%
  head(1)

lifeExp_bycountry %>%
  arrange(desc(mean_lifeExp)) %>%
  head(1)

# Q3: Calculate the average life expectancy per year. ----
gdp_byyear <- gapminder %>%
  group_by(year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap))

# Q4: Calculate the minimum, maximum, mean and se of each country life-expectancy.----
gapminder %>% group_by(country) %>% 
              summarize(
              mean_le = mean(life_expectancy),
              min_le = min(life_expectancy),
              max_le = max(life_expectancy),
              se_le = sd(life_expectancy)/sqrt(n()))
# Q5: Calculate mean and std for gdpPercap and population respectively only for people with a life expectation above 25.

gdp_pop_bycountry_above25 <- gapminder %>%
  mutate(gdpPercap_25 = ifelse(life_expectancy > 25, gdpPercap, NA)) %>%
  group_by(country) %>%
  summarize(mean_gdpPercap = mean(gdpPercap_25),
            sd_gdpPercap = sd(gdpPercap_25),
            mean_pop = mean(population),
            sd_pop = sd(population)
            )

gdp_pop_bycountry_above25










