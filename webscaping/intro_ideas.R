# https://www.freecodecamp.org/news/an-introduction-to-web-scraping-using-r-40284110c848/
rm(list=ls())

# install.packages('selectr')
# install.packages('xml2')
# install.packages('rvest')
# install.packages('stringr')
# install.packages('jsonlite')
library(tidyverse)
library(xml2)
library(rvest)
library(stringr)
library(selectr)

url <- 'https://ideas.repec.org/s/bla/jfinan.html'

webpage <- read_html(url)

#scrape title of the product
content_html <- html_nodes(webpage, 'div#content')
content_html
content <- html_text(content_html)
head(content)
content_1 <- strsplit(content,split="\n \n|\n\n")[[1]]
head(content_1)
tail(content_1,10)
#content_1 <- str_replace_all(content, "[\n]", "")
content_html <- html_nodes(webpage, 'div#myTabContent')
content_html
content <- html_text(content_html)
head(content)
#
scraplinks <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(data.frame(link = link_, url = url_))
}

#  
url_scraped <- url %>% scraplinks() 
# 2 colnames by scraped url given by website: link and url 
colnames(url_scraped)
# convert factor into character
url_scraped_1 <- as.character(url_scraped$link)
str(url_scraped_1)
#
url_scraped_2 <- as.character(url_scraped$url)
str(url_scraped_2)
# extract url
url_paper <- url_scraped_2[grepl('/a/bla/', url_scraped_2)]
# delete first 2 elements which are useless data
url_paper <- url_paper[-c(1,2)]
# total number of papers on this webpage
length(url_paper)
#
title_paper <- url_scraped_1[grepl('/a/bla/', url_scraped_2)]
head(title_paper)
# delete first 2 elements which are useless data
title_paper <- title_paper[-c(1,2)]
length(title_paper)
title_paper



