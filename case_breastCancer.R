# Case study: breast cancer data analysis----
# https://www.kaggle.com/lbronchal/breast-cancer-dataset-analysis

library(needs)
needs(readr,
      dplyr,
      ggplot2,
      corrplot,
      gridExtra,
      pROC,
      MASS,
      caTools,
      caret,
      caretEnsemble,
      doMC)

data <- read.csv("../DataScience/data.csv")
str(data)

data$diagnosis <- as.factor(data$diagnosis)
# the 33 column is not right
data[,33] <- NULL

summary(data)

prop.table(table(data$diagnosis))

corr_mat <- cor(data[,3:ncol(data)])
corrplot(corr_mat, order = "hclust", tl.cex = 1, addrect = 8)

set.seed(1234)
data_index <- createDataPartition(data$diagnosis, p=0.7, list = FALSE)
train_data <- data[data_index, -1]
test_data <- data[-data_index, -1]

# Because there are so much correlation some machine learning models can fail. 
# We are going to create a PCA and LDA version of the data.

pca_res <- prcomp(data[,3:ncol(data)], center = TRUE, scale = TRUE)
plot(pca_res, type="l")

summary(pca_res)

pca_df <- as.data.frame(pca_res$x)
ggplot(pca_df, aes(x=PC1, y=PC2, col=data$diagnosis)) + geom_point(alpha=0.5)

# The data can be easly separated.
g_pc1 <- ggplot(pca_df, aes(x=PC1, fill=data$diagnosis)) + geom_density(alpha=0.25)  
g_pc2 <- ggplot(pca_df, aes(x=PC2, fill=data$diagnosis)) + geom_density(alpha=0.25)  
grid.arrange(g_pc1, g_pc2, ncol=2)


