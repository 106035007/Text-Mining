# Load tidyverse
library(tidyverse)

sisters67 <- read_csv('../DataScience/Supervised ML with case study/sisters.csv')
# View sisters67
glimpse(sisters67)
str(sisters67)
# Plot the histogram
ggplot(sisters67, aes(x = age)) +
  geom_histogram(binwidth = 10)

# Tidy the data set
tidy_sisters <- sisters67 %>%
  select(-sister) %>%
  gather(key, value, -age)

# Print the structure of tidy_sisters
glimpse(tidy_sisters)

# Overall agreement with all questions varied by age
tidy_sisters %>%
  group_by(age) %>%
  summarize(value = mean(value, na.rm = TRUE))

# Number of respondents agreed or disagreed overall
tidy_sisters %>%
  count(value)

# Visualize agreement with age
tidy_sisters %>%
  filter(key %in% paste0("v", 153:170)) %>%
  group_by(key, value) %>%
  summarise(age = mean(age, na.rm = TRUE)) %>%
  ggplot(aes(value, age, color = key)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~key, nrow = 3)

# Remove the sister column
sisters_select <- sisters67 %>% 
  select(-sister)

# Build a simple linear regression model
simple_lm <- lm(age ~ ., 
                data = sisters_select)

# Print the summary of the model
summary(simple_lm)

# Split the data into training and validation/test sets
set.seed(1234)
in_train <- createDataPartition(sisters_select$age, 
                                p = 0.6, list = FALSE)
training <- sisters_select[in_train, ]
validation_test <- sisters_select[-in_train, ]

# Split the validation and test sets
set.seed(1234)
in_test <- createDataPartition(validation_test$age, 
                               p = 0.5, list = FALSE)
testing <- validation_test[in_test, ]
validation <- validation_test[-in_test, ]

# Load caret
library(caret)

# Fit a CART model
sisters_cart <- train(age ~ ., method = "rpart", data = training)
sisters_xgb <- train(age ~ ., method = "xgbLinear", data = training)
sisters_gbm <- train(age ~ ., method = "gbm", data = training)

# Print the CART model
sisters_cart



# Make predictions on the three models
modeling_results <- validation %>%
  mutate(CART = predict(sisters_cart, validation),
         XGB = predict(sisters_xgb, validation),
         GBM = predict(sisters_gbm, validation))

# View the predictions
modeling_results %>% 
  select(CART, XGB, GBM)

# Load yardstick
library(yardstick)

# Compare performance
metrics(modeling_results, truth = age, estimate = CART)
metrics(modeling_results, truth = age, estimate = XGB)
metrics(modeling_results, truth = age, estimate = GBM)

# Calculate RMSE
testing %>%
  mutate(prediction = predict(sisters_gbm, testing)) %>%
  rmse(truth = age, estimate = prediction)