# http://www.lithoguru.com/scientist/statistics/course.html
#--------------------------------#
#--- Logistic Regression in R: Titanic example ----
#--------------------------------#


# Here is an interesting example of logistic regression:
# Who survived the sinking of the Titanic?
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

# install.packages("titanic")
library(titanic)
# SibSp = Number of Siblings/Spouses Aboard
# Parch = Number of Parents/Children Aboard
# Embarked: C = Cherbourg; Q = Queenstown; S = Southampton

data.raw = titanic_train
dim(data.raw)  #shows me the number of rows and columns in the data set
length(data.raw$Pclass)  # Number of data points

# This data has blanks.  Let's convert the blanks to "NA"
data.raw[data.raw==""]  <- NA 

# Check for missing values using the sapply() function
sapply(data.raw, function(x) sum(is.na(x)))
colSums(is.na(data.raw))  # This command does the same thing

# if you want to see how may unique values there are for each column:
sapply(data.raw, function(x) length(unique(x)))


# A few basic ratios from the data:
overall_survival_rate = sum(data.raw$Survived == 1)/length(data.raw$Survived)
cat("Fraction of people who survivied = ", format(overall_survival_rate, digits = 3))
male_survival_rate = sum((data.raw$Survived == 1) & (data.raw$Sex == "male"))/sum(data.raw$Sex == "male")
cat("Fraction of men who survivied = ", format(male_survival_rate, digits = 3))
female_survival_rate = sum((data.raw$Survived == 1) & (data.raw$Sex == "female"))/sum(data.raw$Sex == "female")
cat("Fraction of women who survivied = ", format(female_survival_rate, digits = 3))
class1_survival_rate = sum((data.raw$Survived == 1) & (data.raw$Pclass == 1))/sum(data.raw$Pclass == 1)
cat("Fraction of 1st class passengers who survivied = ", format(class1_survival_rate, digits = 3))
class2_survival_rate = sum((data.raw$Survived == 1) & (data.raw$Pclass == 2))/sum(data.raw$Pclass == 2)
cat("Fraction of 2nd class passengers who survivied = ", format(class2_survival_rate, digits = 3))
class3_survival_rate = sum((data.raw$Survived == 1) & (data.raw$Pclass == 3))/sum(data.raw$Pclass == 3)
cat("Fraction of 3rd class passengers who survivied = ", format(class3_survival_rate, digits = 3))

# Here is another way of looking at a subset of the data
data.T <- na.omit(subset(data.raw,select=c(2,3,5))) #keep only Survived, Pclass, and Sex
table(data.T)
prop.table(table(data.raw$Survived))
prop.table(table(data.raw$Sex))
prop.table(table(data.raw$Pclass))


# Let's drop the columns that we know we are not going to use:
# 1=PassengerId, 4=Name, 9=Ticket, 11=Cabin
data.new <- subset(data.raw,select=c(2,3,5,6,7,8,10,12))

# to check for correlations, I'll convert the factors (male, female, for example)
# into indicator variables using the "dummies" package
# install.packages("dummies")
library(dummies)
data.indicator = subset(data.new,select=c(1:7))
data.indicator$Sex = dummy(data.new$Sex)
cor(data.indicator)


# To see how the factors will be "dummified" (turned into indicators) by R:
is.factor(data.new$Sex)  #is this variable a factor?
contrasts(factor(data.new$Sex))   #show how the factor will be converted to an indicator
levels(factor(data.new$Embarked))  #what are the levels of this factor?
contrasts(factor(data.new$Embarked))


# Now let's perform a logistic regression
model <- glm(Survived ~ .,family=binomial(link='logit'),data=data.new)
summary(model)

#use the anova function to compare the addition of each variable
anova(model, test="Chisq")

# create a small data set, containing only the rows and columns that we will use in out model
data.small <- na.omit(subset(data.new,select=c(1,2,3,4,5)))
model <- glm(Survived ~ .,family=binomial(link='logit'),data=data.small)
summary(model)
confint(model)  #confidence intervals for the coefficients
plot(model)

# let's call a "hit" when predicted>50% matches survived=1 and predicted<50% matches survived=0
y = data.small$Survived - trunc(2*model$fitted)
hits = sum(y==0)
hitratio = hits/length(y)
hitratio

# We can express each of the coefficeints as an odds ratio:
exp(coef(model))

# Odds Ratio with its 95% confience interval
exp(cbind(OddsRatio = coef(model), confint(model)))
