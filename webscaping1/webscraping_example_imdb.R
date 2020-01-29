# https://www.youtube.com/watch?v=n4w8RLCtf7c
# https://www.imdb.com/title/tt0974015/

library(rvest)
url <- "https://www.imdb.com/title/tt0974015/"
#
cast <- read_html(url) %>% html_nodes(".cast_list") %>% html_text() 
cast
#
cast <- read_html(url) %>% html_nodes(".character") %>% html_text() 
cast
#
read_html(url) %>% html_nodes("#titleCast") %>% html_text() %>% 
               strsplit(split= "\n\n |\n")





cast1 <- read_html(url) %>% html_nodes('[href*=name]') %>% html_text()      
cast1



read_html("https://twinpeaks.fandom.com/wiki/Episode_9") %>%
  html_node("[data-source='Director'] .pi-data-value") %>%
  html_text()






