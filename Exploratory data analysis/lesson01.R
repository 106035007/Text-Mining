rm(list=ls())

library(tidyverse)
library(readr)
library(dplyr)

# https://github.com/fivethirtyeight/data/tree/master/comic-characters

# Print the first rows of the data
#comics <- read_csv('../DataScience/Exploratory data analysis/comics.csv')
comics <- read_csv('../DataScience/Exploratory data analysis/comics.csv', col_types = cols(
                   id    = col_factor(),
                   align = col_factor(),
                   hair  = col_factor(),
                   gender= col_factor(),
                   alive = col_factor(), 
                   gsm   = col_factor()
                   ))
str(comics)

# Examine NAs for column gender 
comics %>% summarise(number_nas = sum(is.na(gender)))
 
# Examine NAs for all columns
# purrr::map_df() to “map” function over each column
comics %>% 
       purrr::map_df(~sum(is.na(.)))

# Check levels of align
levels(comics$align)

# Check the levels of gender
levels(comics$gender)
levels(comics$gsm)

# Create a 2-way contingency table
tab <- table(comics$align, comics$gender)
tab

# Remove align level
comics_filtered <- comics %>%
  filter(align != "Reformed Criminals") %>%
  droplevels()

# See the result
comics_filtered

# Load ggplot2
library(ggplot2)

# Create side-by-side barchart of gender by alignment
ggplot(comics, aes(x = align, fill = gender)) + 
  geom_bar(position = "dodge")

# Create side-by-side barchart of alignment by gender
ggplot(comics, aes(x = gender, fill = align)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))
#
tab <- table(comics$align, comics$gender)
options(scipen = 999, digits = 3) # Print fewer digits
prop.table(tab)     # Joint proportions
prop.table(tab, 2)  # Conditional on columns

# Plot of gender by align
ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar()

# Plot proportion of gender, conditional on align
ggplot(comics, aes(x = align, fill = gender)) + 
  geom_bar(position = "fill") +
  ylab("proportion")



# Change the order of the levels in align
comics$align <- factor(comics$align, 
                       levels = c("Bad", "Neutral", "Good"))

# Create plot of align
ggplot(comics, aes(x = align)) + 
  geom_bar()

# Plot of alignment broken down by gender
ggplot(comics, aes(x = align)) + 
  geom_bar() +
  facet_wrap(~ gender)

# Put levels of flavor in decending order
lev <- c("apple", "key lime", "boston creme", "blueberry", "cherry", "pumpkin", "strawberry")
pies$flavor <- factor(pies$flavor, levels = lev)

# Create barchart of flavor
ggplot(pies, aes(x = flavor)) + 
  geom_bar(fill = "chartreuse") + 
  theme(axis.text.x = element_text(angle = 90))

# Alternative solution to finding levels
# lev <- unlist(select(arrange(cnt, desc(n)), flavor))





ggplot(comics, aes(x = id, fill = align)) +
  #geom_bar()
  geom_bar(position = "fill") +
  ylab("proportion")







