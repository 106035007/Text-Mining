# https://www.freecodecamp.org/news/an-introduction-to-web-scraping-using-r-40284110c848/
# https://text-mining-with-r-a-tidy-approach.netlify.com/tidytext
# https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
# https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html

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
# i is page 1:29 on the web
i = url_all[1]
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
str(abstract.list)
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
# url <- https://ideas.repec.org/a/bla/jfinan/v69y2014i4p1643-1671.html
abstract <- function(url){
            webpage <- xml2::read_html(url)
            # Extract the URLs
            abstract_i <- webpage %>%
            rvest::html_nodes('#abstract-body') %>% html_text 
            # delete unnecessary words
            if ("type=\"main">"" %in% abstract_i){
                gsub(" type=\"main\"> ", "", abstract_i)
            }
            return(abstract_i)
}

#url <- url_paper_complete[[1]]
#webpage <- xml2::read_html(url)
library(tm)

p1 <- data.frame((sapply(abstract.list[[1]],c)), stringsAsFactors = FALSE) 
colnames(p1) <- 'abs_text'

# 
data("stop_words")
# add customize words to stop_words
stop_words <- stop_words %>% add_row(., word = c("abstract", "effects", 
                                                 "find",     "markets",
                                                 "results",  "firms",
                                                 "prices",   "abstract",
                                                 "increase", "decrease",
                                                 "item",     "trading",
                                                 "asset"), 
                                     lexicon = rep('mine', 13)) 

# delete some familiar stop words like 'the', 'and'...
tidy_abstract<- p1 %>% as_tibble() %>% unnest_tokens("word", abs_text) %>% 
                anti_join(stop_words) 

# removing numbers ----
# remove all numeric digits and the ‘-’ sign
tidy_abstract <- tidy_abstract %>% 
                 .[-grep("\\b\\d+\\b", .$word), ] 
          
# removing whitespaces ----
# tidy_abstract$word <- gsub("\\s+","",tidy_abstract$word)
#
# removing stemming ----
# library(SnowballC)
#　tidy_abstract_stem <- tidy_abstract %>%
#                      mutate_at("word", list(~wordStem((.), language="en")))
#
tidy_abstract_count <- tidy_abstract %>% count(word) %>% 
                       arrange(desc(n))
# removing stemming
# library(SnowballC)
# tidy_abstract <- tidy_abstract %>%
#                 mutate_at("word", funs(wordStem((.), language="en")))

tidy_abstract %>%
      count(word, sort = TRUE) %>%
      filter(n > 30) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()

# using Vcorpus() to process stopwords ----
vdocs <- VCorpus(VectorSource(p1$abs_text))
vdocs <- tm_map(vdocs, removeWords, stopwords("english"))
# our custom vector of stop words
my_custom_stopwords <- c("approach", "abstract", "case", "find", "the", "this")
# this is one way to remove custom stopwords
vdocs <- tm_map(vdocs, removeWords, my_custom_stopwords)
tdm <- TermDocumentMatrix(vdocs)
tdm.matrix <- as.matrix(tdm)
tdm.rs <- sort(rowSums(tdm.matrix), decreasing=TRUE)
tdm.df <- data.frame(word = names(tdm.rs), freq = tdm.rs, stringsAsFactors = FALSE)
as.tibble(tdm.df)                          # prevent long printing of dataframe
#
library(wordcloud)
set.seed(1234)
wordcloud(words = tdm.df$word, freq = tdm.df$freq, min.freq = 20,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

