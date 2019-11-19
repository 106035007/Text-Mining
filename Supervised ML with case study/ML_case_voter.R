# Load tidyverse
library(tidyverse)

# Print voters
voters <- read_csv('../DataScience/Supervised ML with case study/voters.csv')
str(voters)
voters$turnout16_2016 <- as.factor(voters$turnout16_2016)

# How many people voted?
voters %>%
  count(turnout16_2016)
# How do the reponses on the survey vary with voting behavior?
voters %>%
    group_by(turnout16_2016) %>%
    summarise(`Elections don't matter` = mean(RIGGED_SYSTEM_1_2016 <= 2),
      `Economy is getting better` = mean(econtrend_2016 == 1),
      `Crime is very important` = mean(imiss_a_2016 == 2))

## Visualize difference by voter turnout
voters %>%
  ggplot(aes(econtrend_2016, ..density.., fill = turnout16_2016)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  labs(title = "Overall, is the economy getting better or worse?")


# Remove the case_indetifier column
voters_select <- voters %>%
  select(-case_identifier)

# Build a simple logistic regression model
simple_glm <- glm(turnout16_2016 ~ .,  family = "binomial", 
                  data = voters_select)

# Print the summary                  
summary(simple_glm)

# Load caret
library(caret)

# Split data into training and testing sets
set.seed(1234)
in_train <- createDataPartition(voters_select$turnout16_2016, 
                                p = 0.8, list = FALSE)
training <- voters_select[in_train, ]
testing <- voters_select[-in_train, ]

# Perform logistic regression with upsampling and no resampling
vote_glm <- train(turnout16_2016 ~ ., method = "glm", family = "binomial",
                  data = training,
                  trControl = trainControl(method = "none",
                                           sampling = "up"))

# Logistic regression
vote_glm <- train(turnout16_2016 ~ ., method = "glm", family = "binomial",
                  data = training,
                  trControl = trainControl(method = "repeatedcv",
                                           repeats = 2,
                                           sampling = "up"))

# Print vote_glm
vote_glm

# Random forest
vote_rf <- train(turnout16_2016 ~ ., method = "rf", 
                 data = training,
                 trControl = trainControl(method = "repeatedcv",
                                          repeats = 2,
                                          sampling = "up"))
# Print vote_rf
vote_rf


# Confusion matrix for logistic regression model on training data
confusionMatrix(predict(vote_glm, training),
                training$turnout16_2016)

confusionMatrix(predict(vote_rf, training),
                training$turnout16_2016)

# Confusion matrix for logistic regression model on testing data
confusionMatrix(predict(vote_glm, testing),
                testing$turnout16_2016)

# Confusion matrix for random forest model on testing data
confusionMatrix(predict(vote_rf, testing),
                testing$turnout16_2016)