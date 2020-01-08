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
library(xts)

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
url_scraped$link
# [1] "link" "url" 
# convert factor into character
url_scraped_1 <- as.character(url_scraped$link)
str(url_scraped_1)
#
url_scraped_2 <- as.character(url_scraped$url)
str(url_scraped_2)
url_scraped_2
# extract url for each paper
url_paper <- url_scraped_2[grepl('/a/bla/', url_scraped_2)]
# delete first 2 elements which are useless data
url_paper <- url_paper[-c(1,2)]
url_paper
# total number of papers on this webpage
length(url_paper)
url_paper[1]
#
title_paper <- url_scraped_1[grepl('/a/bla/', url_scraped_2)]
head(title_paper)
# delete first 2 elements which are useless data
title_paper <- title_paper[-c(1,2)]
length(title_paper)
title_paper
#===================
url_paper_complete <- url_paper %>% map(function(x) paste("https://ideas.repec.org", x, sep="")) %>% 
                      unlist()
length(url_paper_complete)
url_paper_i <- url_paper_complete[1]
url_paper_i
#-------------------
# url <- url_paper_i

abstract <- function(url){
            webpage <- xml2::read_html(url)
            # Extract the URLs
            abstract_i <- webpage %>%
            rvest::html_nodes('#abstract-body') %>% 
            html_text
            return(abstract_i)
}
#abstract(url)

#url_i <- url_paper_complete[100:150]
abstract_all <- url_paper_complete %>% sapply(., function(x) abstract(x))
#abstract_i <- url %>% map(function(x) abstract(x)) %>% unlist
abstract_all.tbl <- as_tibble(abstract_all)
abstract_all.tbl

length(abstract_all)
names(abstract_all)
glimpse(abstract_all)

#
# url <- url_paper_i
author <- function(url){
          webpage <- xml2::read_html(url)
          author_i <- webpage %>% 
                      rvest::html_nodes(".authorname") %>% 
                      html_text
          return(author_i)
}

author_all <- url_paper_complete %>% sapply(., function(x) author(x))
length(author_all)
author_all %>% coredata(author_i)
#

