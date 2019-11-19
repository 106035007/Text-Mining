# Print stackoverflow
stackoverflow <- read_csv('../DataScience/Supervised ML with case study/stackoverflow.csv')
stackoverflow$Remote <- as.factor(stackoverflow$Remote)

# First count for Remote
stackoverflow %>% 
  count(Remote, sort = TRUE)

# then count for Country
stackoverflow %>% 
  count(Country, sort = TRUE)

#
ggplot(stackoverflow, aes(Remote, YearsCodedJob)) +
  geom_boxplot() +
  labs(x = NULL,
       y = "Years of professional coding experience") 


# Build a simple logistic regression model
simple_glm <- stackoverflow %>%
  select(-Respondent) %>%
  glm(Remote ~ .,
      family = "binomial",
      data = .)

# Print the summary of the model
summary(simple_glm)

# Load caret
library(caret)

# Create stack_select dataset
stack_select <- stackoverflow %>%
  select(-Respondent)

# Split the data into training and testing sets
set.seed(1234)
in_train <- createDataPartition(stack_select$Remote, p = .8, list = FALSE)
training <- stack_select[in_train,]
testing <- stack_select[-in_train,]

# Create the upsampled training set
up_train <- upSample(x = select(training, -Remote),
                     y = training$Remote,
                     yname = "Remote") %>%
  as_tibble()

# Count the number of each type of Remote employee
up_train %>%
  count(Remote)

# Build a logistic regression model
stack_glm <- train(Remote ~ ., method = "glm", family = "binomial",
                   data = training,
                   trControl = trainControl(method = "boot",
                                            sampling = "up"))

# Print the model object
stack_glm
# random forest takes too much time in running!
stack_rf <- train(Remote ~ ., method = "rf", family = "binomial",
                   data = training,
                   trControl = trainControl(method = "boot",
                                            sampling = "up"))

# Print the model object
stack_rf





# Set seed
set.seed(123)

# Confusion matrix for logistic regression model
confusionMatrix(predict(stack_glm, testing),
                testing$Remote)

# Load yardstick
library(yardstick)

# Predict values
testing_results <- testing %>%
  mutate(`Logistic regression` = predict(stack_glm, testing),
         `Random forest` = predict(stack_rf, testing))

## Calculate accuracy
accuracy(testing_results, truth = Remote, estimate = `Logistic regression`)
accuracy(testing_results, truth = Remote, estimate = `Random forest`)

## Calculate positive predict value
ppv(testing_results, truth = Remote, estimate = `Logistic regression`)
ppv(testing_results, truth = Remote, estimate = `Random forest`)



