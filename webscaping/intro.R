# https://www.freecodecamp.org/news/an-introduction-to-web-scraping-using-r-40284110c848/

install.packages('selectr')
install.packages('xml2')
install.packages('rvest')
install.packages('stringr')
install.packages('jsonlite')

library(xml2)
library(rvest)
library(stringr)
library(selectr)

url <- 'https://www.amazon.in/dp/B07KM2PL5C/ref=dp_prsubs_1'

webpage <- read_html(url)

#scrape title of the product
title_html <- html_nodes(webpage, 'h1#title')
title <- html_text(title_html)
head(title)
# remove all space and new lines
# str_replace_all(string, pattern, replacement)
title <- str_replace_all(title, "[\r\n]" , "")
title
# scrape the price of the product
price_html <- html_nodes(webpage, 'span#priceblock_ourprice')
price <- html_text(price_html)
price

price <- str_replace_all(price, "[\r\n]" , "")
price
#scrape product description
desc_html <- html_nodes(webpage, 'div#productDescription')
desc <- html_text(desc_html)
desc

# replace new lines and spaces
desc <- str_replace_all(desc, "[\r\n\t]" , "")
desc <- str_trim(desc)
head(desc)


# scrape product rating 
rate_html <- html_nodes(webpage, 'span#acrPopover')
rate <- html_text(rate_html)

# remove spaces and newlines and tabs 
rate <- str_replace_all(rate, "[\r\n]" , "")
rate <- str_trim(rate)
rate
#
tab <- html_table(webpage, fill = TRUE)
str(tab)
tab[[4]]

#
 html_nodes(webpage, 'div')

# Scrape size of the product
size_html <- html_nodes(webpage, 'div#variation_size_name')
size_html <- html_nodes(size_html, 'span.selection')
size <- html_text(size_html)
size
# remove tab from text
size <- str_trim(size)
size

# Print product size
head(size)

# Scrape product color
color_html <- html_nodes(webpage, 'div#variation_color_name')
color_html <- html_nodes(color_html, 'span.selection')
color <- html_text(color_html)
color

#Combining all the lists to form a data frame
product_data <- data.frame(Title = title, Price = price, Description = desc, Rating = rate, Size = size, Color = color)
str(product_data)

# Include ‘jsonlite’ library to convert in JSON form.
library(jsonlite)
# convert dataframe into JSON format
json_data <- toJSON(product_data)

# print output
cat(json_data)


# https://blog.rsquaredacademy.com/web-scraping/