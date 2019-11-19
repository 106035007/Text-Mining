# Datacamp course: MACHINELEARNINGINTHETIDYVERSE 

# Explore gapminder

rm(list=ls())
# Prepare the nested dataframe gap_nested
library(tidyverse)
library(rsample)

gapminder <- read_rds('../Tidyverse in ML/gapminder.rds')
head(gapminder)
str(gapminder)

gap_nested <- gapminder %>% 
  group_by(country) %>% 
  nest()

# Explore gap_nested
head(gap_nested)

# Create the unnested dataframe called gap_unnnested
gap_unnested <- gap_nested %>% 
                unnest()

# Confirm that your data was not modified  
identical(gapminder, gap_unnested)

# Extract the data of Algeria
algeria_df <- gap_nested$data[[1]]
algeria_df

# Calculate the minimum of the population vector
min(algeria_df$population)

# Calculate the maximum of the population vector
max(algeria_df$population)

# Calculate the mean of the population vector
mean(algeria_df$population)


# Calculate the mean population for each country
pop_nested <- gap_nested %>%
  mutate(mean_pop = map(data, ~mean(.x$population)))

# Take a look at pop_nested
head(pop_nested)

# Extract the mean_pop value by using unnest
pop_mean <- pop_nested %>% 
  unnest(mean_pop)

# Take a look at pop_mean
head(pop_mean)

# Calculate mean population and store result as a double
pop_mean <- gap_nested %>%
  mutate(mean_pop = map_dbl(data, ~mean(.x$population)))

# Take a look at pop_mean
head(pop_mean)

# Build a linear model for each country
gap_models <- gap_nested %>%
  mutate(model = map(data, ~lm(formula = life_expectancy~year, data = .x)))
#
gap_models_01 <- gap_nested %>% 
       mutate(model = map(data, ~lm(formula = life_expectancy~., data = .x))) %>% 
       mutate(out = map(model, ~ as.data.frame(t(as.matrix(coef(.))))))

map_df(gap_models_01$out, rbind)       
        
# Extract the model for Algeria    
algeria_model <- gap_models$model[[1]]

# View the summary for the Algeria model
summary(algeria_model)

library(broom)

# Extract the coefficients of the algeria_model as a dataframe
tidy(algeria_model)

# Extract the statistics of the algeria_model as a dataframe
glance(algeria_model)


# Build the augmented dataframe
algeria_fitted <- augment(algeria_model)

# Compare the predicted values with the actual values of life expectancy
algeria_fitted %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = life_expectancy)) + 
  geom_line(aes(y = .fitted), color = "red")

# Extract the coefficient statistics of each model into nested dataframes
model_coef_nested <- gap_models %>% 
  mutate(coef = map(model, ~tidy(.x)))
model_coef_nested

# Simplify the coef dataframes for each model    
model_coef <- model_coef_nested %>%
              unnest(coef)
model_coef

# Plot a histogram of the coefficient estimates for year         
model_coef %>% 
           filter(term == "year") %>% 
           ggplot(aes(x = estimate)) +
           geom_histogram()

# Question: 
# What can we learn about these 77 countries?
# Explore the model_coef dataframe you just created to answer the following question:
# Which of the following conclusions can we make from the coefficients of our models?
# Possible Answers

# A. About 10% of the 77 countries had a decrease of life expectancy between 1960 and 2011.
# B. The United States experienced the fastest growth in life expectancy.
# C. The majority of the 77 countries experienced a growth in average life expectancy between 1960 and 2011.
# D. All of these conclusions are correct.
# E. None of these conclusions are correct.

# Solution: C

# Extract the fit statistics of each model into dataframes
model_perf_nested <- gap_models %>% 
                     mutate(fit = map(model, ~glance(.x)))

# Simplify the fit dataframes for each model    
model_perf <- model_perf_nested %>% 
              unnest(fit)

# Look at the first six rows of model_perf
head(model_perf)

# Plot a histogram of rsquared for the 77 models    
model_perf %>% 
  ggplot(aes(x = r.squared)) + 
  geom_histogram()

# Extract the 4 best fitting models
best_fit <- model_perf %>% 
  top_n(n = 4, wt = r.squared)
best_fit
# Extract the 4 models with the worst fit
worst_fit <- model_perf %>% 
  top_n(n = 4, wt = -r.squared)
worst_fit

#
best_augmented <- best_fit %>% 
  # Build the augmented dataframe for each country model
  mutate(augmented = map(model, ~augment(.x))) %>% 
  # Expand the augmented dataframes
  unnest(augmented)
best_augmented

#
worst_augmented <- worst_fit %>% 
  # Build the augmented dataframe for each country model
  mutate(augmented = map(model, ~augment(.x))) %>% 
  # Expand the augmented dataframes
  unnest(augmented)

# Compare the predicted values with the actual values of life expectancy 
# for the top 4 best fitting models
best_augmented %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = life_expectancy)) + 
  geom_line(aes(y = .fitted), color = "red") +
  facet_wrap(~country, scales = "free_y")

# Compare the predicted values with the actual values of life expectancy 
# for the top 4 worst fitting models
worst_augmented %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = life_expectancy)) + 
  geom_line(aes(y = .fitted), color = "red") +
  facet_wrap(~country, scales = "free_y")

# Build a linear model for each country using all features
gap_fullmodel <- gap_nested %>% 
  mutate(model = map(data, ~lm(life_expectancy~., data = .x)))

fullmodel_perf <- gap_fullmodel %>% 
  # Extract the fit statistics of each model into dataframes
  mutate(fit = map(model, ~glance(.x))) %>% 
  # Simplify the fit dataframes for each model
  unnest(fit)

fullmodel_perf
fullmodel_perf$data

# View the performance for the four countries with the worst fitting 
# four simple models you looked at before
fullmodel_perf %>% 
  filter(country %in% worst_fit$country) %>% 
  select(country, adj.r.squared)

# Which of these four models do you expect to perform the best for future years?
fullmodel_perf %>% 
  filter(country %in% best_fit$country) %>% 
  select(country, adj.r.squared)

set.seed(42)

# Prepare the initial split object
gap_split <- initial_split(gapminder, prop = 0.75)

gap_split
# Extract the training dataframe
training_data <- training(gap_split)

# Extract the testing dataframe
testing_data <- testing(gap_split)
testing_data
# Calculate teh dimensions of both training_data and testing_data
dim(training_data)
dim(testing_data)

set.seed(42)

# Prepare the dataframe containing the cross validation partitions
# you will split the training data into a series of 5 train-validate sets using the vfold_cv() function 
# from the rsample package.
cv_split <- vfold_cv(training_data, v = 5)
cv_split$splits[[1]]

#
cv_data <- cv_split %>% 
    mutate(
    # Extract the train dataframe for each split
    train = map(splits, ~training(.x)), 
    # Extract the validate dataframe for each split
    validate = map(splits, ~testing(.x))
  )

# Use head() to preview cv_data
head(cv_data)
cv_data$train[[1]]

# Build a model using the train data for each fold of the cross validation
cv_models_lm <- cv_data %>% 
  mutate(model = map(train, ~lm(formula = life_expectancy~., data = .x)))

#lm(formula  = life_expectancy~., data = cv_data$train[[1]])
#
#tt <- cv_data %>% 
#  mutate(model  = map(train, ~lm(formula = life_expectancy~., data = .x))) %>% 
#  mutate(reg = map(model, ~ as.data.frame(t(as.matrix(coef(.))))))

#tt$reg[[1]]
#tt <- coefficients(cv_models_lm$model[[1]])

cv_prep_lm <- cv_models_lm %>% 
  mutate(
    # Extract the recorded life expectancy for the records in the validate dataframes
    validate_actual = map(validate, ~.x$life_expectancy),
    # Predict life expectancy for each validate set using its corresponding model
    validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y))
  )

cv_eval_lm

#
library(Metrics)
# Calculate the mean absolute error for each validate fold       
cv_eval_lm <- cv_prep_lm %>% 
  mutate(validate_mae = map2_dbl(validate_actual, validate_predicted, ~mae(actual = .x, predicted = .y)))

# Print the validate_mae column
cv_eval_lm$validate_mae

# Calculate the mean of validate_mae column
mean(cv_eval_lm$validate_mae)

#
library(ranger)

# Build a random forest model for each fold
cv_models_rf <- cv_data %>% 
  mutate(model = map(train, ~ranger(formula = life_expectancy~., data = .x,
                                    num.trees = 100, seed = 42)))

# Generate predictions using the random forest model
cv_prep_rf <- cv_models_rf %>%
  # Extract the recorded life expectancy for the records in the validate dataframes
  mutate(validate_actual = map(validate, ~.x$life_expectancy),
         validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))

# Calculate validate MAE for each fold
cv_eval_rf <- cv_prep_rf %>% 
  mutate(validate_mae = map2_dbl(validate_actual, validate_predicted, ~mae(actual = .x, predicted = .y)))

# Print the validate_mae column
cv_eval_rf$validate_mae

# Calculate the mean of validate_mae column
mean(cv_eval_rf$validate_mae)

# Prepare for tuning your cross validation folds by varying mtry
cv_tune <- cv_data %>% 
  crossing(mtry = 2:5) 

# Build a model for each fold & mtry combination
cv_model_tunerf <- cv_tune %>% 
  mutate(model = map2(.x = train, .y = mtry, ~ranger(formula = life_expectancy~., 
                                                     data = .x, mtry = .y, 
                                                     num.trees = 100, seed = 42)))
# Generate validate predictions for each model
cv_prep_tunerf <- cv_model_tunerf %>% 
  mutate(validate_actual = map(validate, ~.x$life_expectancy),
         validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))

# Calculate validate MAE for each fold and mtry combination
cv_eval_tunerf <- cv_prep_tunerf %>% 
  mutate(validate_mae = map2_dbl(.x = validate_actual, .y = validate_predicted, ~mae(actual = .x, predicted = .y)))

# Calculate the mean validate_mae for each mtry used  
cv_eval_tunerf %>% 
  group_by(mtry) %>% 
  summarise(mean_mae = mean(validate_mae))


# Build the model using all training data and the best performing parameter
best_model <- ranger(formula = life_expectancy~., data = training_data,
                     mtry = 4, num.trees = 100, seed = 42)

# Prepare the test_actual vector
test_actual <- testing_data$life_expectancy

# Predict life_expectancy for the testing_data
test_predicted <- predict(best_model, testing_data)$predictions

# Calculate the test MAE
mae(test_actual, test_predicted)

# Example of map function ----
tt <- mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map_df(~ as.data.frame(t(as.matrix(coef(.)))))

iris
a.lm <- lm(Petal.Width~Species, data=iris)
anova(a.lm)
