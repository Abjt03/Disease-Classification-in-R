library(tidyverse)

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

# check for duplicates
sum(duplicated(df_test))

# separate independent and dependent variables
X_train <- subset(df_train, select = -ncol(df_train))
Y_train <- subset(df_train, select = ncol(df_train))
X_test <- subset(df_test, select = -ncol(df_test))
Y_test <- subset(df_test, select = ncol(df_test))
