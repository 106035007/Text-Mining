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
suffix <- 2:29
url_all <- paste(paste('https://ideas.repec.org/s/bla/jfinan', suffix, sep=""), '.html', sep="")
url_all <- c(url, url_all)
#
i = url
url.list <- list()
abstract.list <- list()
for (i in url_all){
     url_scraped <- i %>% scraplinks() 
     url_scraped_2 <- as.character(url_scraped$url) 
     url_scraped_2 <- url_scraped_2[grepl('/a/bla/', url_scraped_2)]
     url_scraped_2 <- url_scraped_2[c(-1,-2)]
     url.list[[i]] <- url_scraped_2
     url_paper_complete <- url.list[[i]] %>% map(function(x) paste("https://ideas.repec.org", x, sep="")) %>% 
                           unlist()
     abstract.list[[i]] <- url_paper_complete %>% sapply(., function(x) abstract(x))
}

str(url.list)

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
abstract <- function(url){
            webpage <- xml2::read_html(url)
            # Extract the URLs
            abstract_i <- webpage %>%
            rvest::html_nodes('#abstract-body') %>% 
            html_text
            return(abstract_i)
}
#abstract(url)


