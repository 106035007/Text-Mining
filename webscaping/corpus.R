# https://cbail.github.io/SICSS_Basic_Text_Analysis.html
# https://text-mining-with-r-a-tidy-approach.netlify.com/tidytext


#install.packages("tidytext")
library(tm)
library(tidytext)
library(tidyverse)
library(dplyr)

load(url("https://cbail.github.io/Trump_Tweets.Rdata"))
head(trumptweets$text)
class(trumptweets)

trump_corpus <- Corpus(VectorSource(as.vector(trumptweets$text))) 
trump_corpus

tidy_trump_tweets<- trumptweets %>%
                    select(created_at, text) %>% 
                    unnest_tokens("word", text)

tidy_trump_tweets %>%
                  count(word) %>%
                  arrange(desc(n))


trump_corpus <- tm_map(trump_corpus, removeWords, stopwords("english"))

data("stop_words")
tidy_trump_tweets<-tidy_trump_tweets %>%
  anti_join(stop_words)

tidy_trump_tweets %>%
  count(word) %>%
  arrange(desc(n))

trump_corpus <- tm_map(trump_corpus, content_transformer(removePunctuation))
trump_corpus  <- tm_map(trump_corpus, content_transformer(stemDocument), language = "english")
trump_DTM <- DocumentTermMatrix(trump_corpus, control = list(wordLengths = c(2, Inf)))
inspect(trump_DTM[1:5,3:8])


library(SnowballC)
tidy_trump_tweets<-tidy_trump_tweets %>%
                   mutate_at("word", funs(wordStem((.), language="en")))

tidy_trump_DTM<-
  tidy_trump_tweets %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, n)

text_data <- "In a complicated haste, Tom rushed to fix a new complication, too complicatedly."
# Remove punctuation: rm_punc
rm_punc <- removePunctuation(text_data)
# Create character vector: n_char_vec
n_char_vec <- unlist(strsplit(rm_punc, split = ' '))
# Perform word stemming: stem_doc
stem_doc <- stemDocument(n_char_vec)
# Print stem_doc
stem_doc
# Create the completion dictionary: comp_dict
comp_dict <- c("In", "a", "complicate", "haste", "Tom", "rush", "to", "fix", "new", "too")
# Re-complete stemmed document: complete_doc
complete_doc <- stemCompletion(stem_doc, comp_dict) 
# Print complete_doc
complete_doc

