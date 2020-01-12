rm(list=ls())

library(tidyverse)
library(xml2)
library(rvest)
library(stringr)
library(selectr)
library(xts)

url <- 'https://ideas.repec.org/s/bla/corgov.html'
suffix <- 2:4
url_all <- paste(paste('https://ideas.repec.org/s/bla/corgov.html', suffix, sep=""), '.html', sep="")
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

#author
i = url
url.list_author <- list()
author.list <- list()
for (i in url_all){
  url_scraped <- i %>% scraplinks() 
  url_scraped_2 <- as.character(url_scraped$url) 
  url_scraped_2 <- url_scraped_2[grepl('/a/bla/', url_scraped_2)]
  url_scraped_2 <- url_scraped_2[c(-1,-2)]
  url.list_author[[i]] <- url_scraped_2
  url_paper_complete <- url.list[[i]] %>% map(function(x) paste("https://ideas.repec.org", x, sep="")) %>% 
    unlist()
  author.list[[i]] <- url_paper_complete %>% sapply(., function(x) abstract(x))
}

str(url.list_author)

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


