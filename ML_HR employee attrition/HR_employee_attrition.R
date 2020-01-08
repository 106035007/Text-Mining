# HR Employee attrition from Kaggle
# https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset
# https://rstudio-pubs-static.s3.amazonaws.com/397345_30101161deeb4def9bf3570e8899d859.html
rm(list=ls())

library(tidyverse)
library(caret)
library(SuperLearner)
library(gridExtra)

dataset <- read.csv('../ML_HR employee attrition/HR Employee Attrition_ibm.csv') 
# dataset <- read_csv('../ML_HR employee attrition/HR Employee Attrition_ibm.csv') %>% type.convert()
# i <- sapply(dataset, is.character)
# dataset[i] <- lapply(dataset[i], as.factor)
dim(dataset)
str(dataset)

# As we can see from the structure we have lookup values for a few columns like Education, 
# Environment Satisfaction, Job Involvement etc. So first we will replace these values with
# the actual description provided in the dataset .We will also be removing a few columns from 
# our dataset which we wont be using in analysis further.

dataset <- dataset %>%
  mutate(Education = as.factor(if_else(Education == 1,"Below College", if_else(Education == 2, "College", if_else(Education == 3, "Bachelor", if_else(Education == 4, "Master","Doctor")))))
         ,EnvironmentSatisfaction = as.factor(if_else(EnvironmentSatisfaction == 1,"Low",if_else(EnvironmentSatisfaction == 2, "Medium", if_else(EnvironmentSatisfaction == 3, "High", "Very High"))))
         ,JobInvolvement = as.factor(if_else(JobInvolvement == 1,"Low",if_else(JobInvolvement == 2, "Medium",if_else(JobInvolvement == 3, "High", "Very High"))))
         ,JobSatisfaction = as.factor(if_else(JobSatisfaction == 1, "Low",if_else(JobSatisfaction == 2, "Medium",if_else(JobSatisfaction == 3, "High","Very High"))))
         ,PerformanceRating = as.factor(if_else(PerformanceRating == 1, "Low",if_else(PerformanceRating == 2, "Good", if_else(PerformanceRating == 3, "Excellent", "Outstanding"))))
         ,RelationshipSatisfaction = as.factor(if_else(RelationshipSatisfaction == 1, "Low",if_else(RelationshipSatisfaction == 2, "Medium", if_else(RelationshipSatisfaction == 3, "High", "Very High"))))
         ,WorkLifeBalance = as.factor(if_else(WorkLifeBalance == 1, "Bad",if_else(WorkLifeBalance == 2, "Good", if_else(WorkLifeBalance == 3, "Better", "Best"))))
         ,JobLevel = as.factor(JobLevel)
  ) %>%
  select(-EmployeeCount, -EmployeeNumber, -Over18, -StandardHours, -StockOptionLevel, -JobLevel)

#
summary(dataset)
str(dataset)
# Employee Personal Demographics - Numerical Variables
p1 <- ggplot(dataset) + geom_histogram(aes(Age), binwidth = 5, fill = "blue",col = "black")
p2 <- ggplot(dataset) + geom_histogram(aes(DistanceFromHome), binwidth = 5, fill = "blue",col = "black")
p3 <- ggplot(dataset) + geom_histogram(aes(NumCompaniesWorked), binwidth = 2, fill = "blue",col = "black")
p4 <- ggplot(dataset) + geom_histogram(aes(TotalWorkingYears), binwidth = 4, fill = "blue",col = "black")

grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Employee Billing Rate Demographics - Numerical Variables

p1 <- ggplot(dataset) + geom_histogram(aes(HourlyRate), binwidth = 5, fill = "blue",col = "black")
p2 <- ggplot(dataset) + geom_histogram(aes(DailyRate), binwidth = 100, fill = "blue",col = "black")
p3 <- ggplot(dataset) + geom_histogram(aes(MonthlyRate), binwidth = 1000, fill = "blue",col = "black")

grid.arrange(p1, p2, p3, nrow = 3)

# Employee Work Demographics - Numerical Variables
p1 <- ggplot(dataset) + geom_histogram(aes(MonthlyIncome), binwidth = 1000, fill = "blue",col = "black")
p2 <- ggplot(dataset) + geom_histogram(aes(PercentSalaryHike), binwidth = 1, fill = "blue",col = "black")
p3 <- ggplot(dataset) + geom_histogram(aes(YearsAtCompany), binwidth = 2, fill = "blue",col = "black")
p4 <- ggplot(dataset) + geom_histogram(aes(YearsInCurrentRole), binwidth = 2, fill = "blue",col = "black")
p5 <- ggplot(dataset) + geom_histogram(aes(YearsSinceLastPromotion), binwidth = 2, fill = "blue",col = "black")
p6 <- ggplot(dataset) + geom_histogram(aes(YearsWithCurrManager), binwidth = 2, fill = "blue",col = "black")

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)

# Employee Personal Demographics - Categorical Variables
p1<- dataset %>%
  group_by(Gender) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(Gender), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Gender") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 900))

p2<- dataset %>%
  group_by(Education) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(Education), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Education") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 650))

p3 <- dataset %>%
  group_by(EducationField) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(EducationField), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Education Field") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 650))

p4 <- dataset %>%
  group_by(MaritalStatus) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(MaritalStatus), y = counts)) + geom_bar(stat = 'identity', fill = "coral1")+ ggtitle("Marital Status") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 750))

p5 <- dataset %>%
  group_by(RelationshipSatisfaction) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(RelationshipSatisfaction), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Relationship Satisfaction") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())+ scale_y_continuous(limits = c(0, 500))

p6 <- dataset %>%
  group_by(WorkLifeBalance) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(WorkLifeBalance), y = counts)) + geom_bar(stat = 'identity', fill = "coral1")+ ggtitle("Work Life Balance") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 950))

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)

# Employee Work Demographics - Categorical Variables
p1 <- dataset %>%
  group_by(BusinessTravel) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(BusinessTravel), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Business Travel") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ theme(plot.title = element_text(size =10),axis.text.x = element_text(size =10,angle = 45, hjust = 1),axis.title.x=element_blank())+ scale_y_continuous(limits = c(0, 1100))



p2 <- dataset %>%
  group_by(EnvironmentSatisfaction) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(EnvironmentSatisfaction), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Environment Satisfaction") + geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =10,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 500))

p3 <- dataset %>%
  group_by(JobInvolvement) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(JobInvolvement), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Job Involvement") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ theme(plot.title = element_text(size =10),axis.text.x = element_text(size =10,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 900))


p4 <- dataset %>%
  group_by(JobSatisfaction) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(JobSatisfaction), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Job Satisfaction") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 500))

p5 <- dataset %>%
  group_by(OverTime) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(OverTime), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Over Time") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 1100))


p6 <- dataset %>%
  group_by(PerformanceRating) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(PerformanceRating), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Performance Rating") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) + scale_y_continuous(limits = c(0, 1300))

grid.arrange(p1,p2,p3,p4,p5,p6,nrow = 2)

#
p1 <- dataset %>%
  group_by(Department) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(Department), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Department") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ theme(plot.title = element_text(size =10),axis.text.x = element_text(size = 7, angle = 45, hjust = 1),axis.title.x=element_blank())

p2 <- dataset %>%
  group_by(JobRole) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = as.factor(JobRole), y = counts)) + geom_bar(stat = 'identity', fill = "coral1") + ggtitle("Job Role") +geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

grid.arrange(p1,p2 ,ncol = 2)

# Employee Personal Demographics - Numerical Variables
p1 <- dataset %>%
  ggplot(aes(x = Age, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Age") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

p2 <- dataset %>%
  ggplot(aes(x = DistanceFromHome, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Distance From Home")  + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

p3 <- dataset %>%
  ggplot(aes(x = NumCompaniesWorked, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Number of Companies")  + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

p4 <- dataset %>%
  ggplot(aes(x = TotalWorkingYears, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Total Working Years")  + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

# Employee Work Demographics - Numerical Variables

p1 <- dataset %>%
  ggplot(aes(x = MonthlyIncome, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Monthly Income") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


p2 <- dataset %>%
  ggplot(aes(x = PercentSalaryHike, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Percentage Salary Hike") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


p3 <- dataset %>%
  ggplot(aes(x = YearsAtCompany, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years At Company") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


p4 <- dataset %>%
  ggplot(aes(x = YearsInCurrentRole, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years in Current Role") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


p5 <- dataset %>%
  ggplot(aes(x = YearsSinceLastPromotion, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years Since Last Promotion") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


p6 <- dataset %>%
  ggplot(aes(x = YearsWithCurrManager, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years With Current Manager") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


grid.arrange(p1, p2, p3, p4, p5, p6 , nrow = 3, ncol = 2)

# We will use One hot encoding for encoding categorical variables wherein, each category of a 
# categorical variable is converted into a new binary column (1/0).
dmy <- dummyVars(~., data = dataset[-2])
trsf <- data.frame(predict(dmy, newdata = dataset[-2]))
dim(trsf)
str(trsf)


# Removing skewness
trsf <- trsf %>%
  mutate(Age = log(Age + 1)
         ,DailyRate = log(DailyRate + 1)
         ,DistanceFromHome = log(DistanceFromHome + 1)
         ,HourlyRate = log(HourlyRate + 1)
         ,MonthlyIncome = log(MonthlyIncome + 1)
         ,MonthlyRate = log(MonthlyRate + 1)
         ,NumCompaniesWorked = log(NumCompaniesWorked + 1)
         ,PercentSalaryHike = log(PercentSalaryHike + 1)
         ,TotalWorkingYears = log(TotalWorkingYears + 1)
         ,TrainingTimesLastYear = log(TrainingTimesLastYear + 1)
         ,YearsAtCompany = log(YearsAtCompany +1)
         ,YearsInCurrentRole = log(YearsInCurrentRole + 1)
         ,YearsSinceLastPromotion = log(YearsSinceLastPromotion + 1)
         ,YearsWithCurrManager = log(YearsWithCurrManager + 1))
#
prep_num = preProcess(trsf, method=c("center", "scale"))
final_dataset = predict(prep_num, trsf)

# Removing correlated independent variables
# It is not desirable to have correlated features if we are using linear regressions. 
# We will first find out variables which have a corelation of 0.85 or higher
#
cor_mat<- cor(final_dataset)
high_corr <- findCorrelation(cor_mat, cutoff = 0.85)
names(trsf)[high_corr]
#
final_dataset <- cbind(trsf, dataset[2])

final_dataset <- final_dataset %>%
  mutate(Attrition = if_else(Attrition == "Yes",1,0)) %>%
  select(-Department.Research...Development,-Department.Human.Resources,-PerformanceRating.Outstanding,-Gender.Male,-OverTime.Yes)

# Dataset Split in Training and Test Sets
Train <- createDataPartition(final_dataset$Attrition, p=0.7, list=FALSE)
training <- final_dataset[ Train, ]
testing <- final_dataset[ -Train, ]

# Make sure that the proporation is maintained.
prop.table(table(final_dataset$Attrition))
prop.table(table(training$Attrition))
prop.table(table(testing$Attrition))
#
control <- trainControl(method="repeatedcv", number=5)
set.seed(123)
model_lr <- train(as.factor(Attrition)~., data=training, method="glm", trControl=control)

pred_lr <- predict(model_lr, newdata=testing)

confusionMatrix(pred_lr, as.factor(testing$Attrition))

str(pred_lr)
str(testing$Attrition)

#
library(ROCR)

ROCRpred <- prediction(as.numeric(pred_lr), as.numeric(testing$Attrition))
ROCRpref <- performance(ROCRpred,"auc")
auc_lr <- as.numeric(ROCRpref@y.values)
perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(auc_lr, digits=5, scientific=FALSE)))

# Random forest ----
control <- trainControl(method="repeatedcv", number=5)
set.seed(123)
model_rf <- train(as.factor(Attrition)~., data=training, method="rf",  trControl=control)

pred_rf <- predict(model_rf, newdata=testing)
confusionMatrix(as.factor(pred_rf), as.factor(testing$Attrition))

library(ROCR)
ROCRpred <- prediction(as.numeric(pred_rf), as.numeric(testing$Attrition))
ROCRpref <- performance(ROCRpred,"auc")
auc_rf <- as.numeric(ROCRpref@y.values)
perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(auc_rf, digits=5, scientific=FALSE)))




