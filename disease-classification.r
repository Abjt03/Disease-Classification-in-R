library(tidyverse)
library(randomForest)
library(caret)
library(dplyr)

# import the dataset
df_train <- read.csv("Training.csv")
df_test <- read.csv("Testing.csv")

# view the structure of the data frame
str(df_train)
str(df_test)

# view summary statistics of the data frame
summary(df_train)
summary(df_test)

# check for null values
colSums(is.na(df_train))
colSums(is.na(df_test))

# X column has 4920 null values -> Entire column is null column : Hence, we drop the column X
df_train <- subset(df_train, select = -ncol(df_train))
if ("X" %in% colnames(df_train)) {
  print("Column 'X' is present in the data frame.")
} else {
  print("Column 'X' is not present in the data frame.")
}
# separate independent and dependent variables
X_train <- subset(df_train, select = -ncol(df_train))
Y_train <- subset(df_train, select = ncol(df_train))
X_test <- subset(df_test, select = -ncol(df_test))
Y_test <- subset(df_test, select = ncol(df_test))

# check for duplicates
sum(duplicated(df_test))

# Building the model
rfmodel <- randomForest(as.factor(prognosis) ~ ., data = df_train, ntree = 200, nodesize = 2, importance = TRUE, mtry = 12)

# Prediction
predictions <- predict(rfmodel, newdata = df_test)
accuracy <- (sum(as.character(predictions) == Y_test)) / nrow(Y_test)
print(paste("Accuracy is:", accuracy * 100, "%"))

# Confusion Matrix and Importance table
cm <- confusionMatrix(predictions, as.factor(df_test$prognosis))
oldimp <- data.frame(Importance = rfmodel$importance[, 1] * 100, row.names = colnames(df_test)[-133])
oldimp <- arrange(oldimp, desc(Importance))

# Printing Confusion Marix and Importance Table
print(cm)
print(oldimp)

# Removal of Unimportant Predictors
newimp <- subset(oldimp, Importance > quantile(Importance, 0.75))

# Updation of Train and Test Datasets
df_newtrain <- df_train %>% select(rownames(newimp))
df_newtrain <- cbind(df_newtrain, prognosis = df_train$prognosis)
df_newtest <- df_test %>% select(rownames(newimp))
df_newtest <- cbind(df_newtest, prognosis = df_test$prognosis)

# view the structure of the new data frame
str(df_train)
str(df_test)

# view summary statistics of the new data frame
summary(df_train)
summary(df_test)

# Random Forest with new train and test datasets

# separate independent and dependent variables
X_newtrain <- subset(df_newtrain, select = -ncol(df_newtrain))
Y_newtrain <- subset(df_newtrain, select = ncol(df_newtrain))
X_newtest <- subset(df_newtest, select = -ncol(df_newtest))
Y_newtest <- subset(df_newtest, select = ncol(df_newtest))

# check for duplicates
sum(duplicated(df_newtest))

# Building the new model
newrfmodel <- randomForest(as.factor(prognosis) ~ ., data = df_newtrain, ntree = 200, nodesize = 2, importance = TRUE, mtry = 12)

# Prediction of the new model
newpredictions <- predict(newrfmodel, newdata = df_newtest)
newaccuracy <- (sum(as.character(newpredictions) == Y_newtest)) / nrow(Y_newtest)
print(paste("Accuracy of the feature extracted model is:", newaccuracy * 100, "%"))

# Confusion Matrix of the new model
ncm <- confusionMatrix(newpredictions, as.factor(df_newtest$prognosis))

# Printing new Confusion Matrix
print(ncm)