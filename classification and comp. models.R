
# Load dataset
setwd("C:/Users/chole/OneDrive/Advanced BA/QMBE-3730/QMBE-3730")

library(ggplot2)
library(caret)
library(randomForest)
library(lattice)
library(tidyverse)
library(dplyr)
library(pROC)




# Get the shape of the dataset
num_rows <- nrow(Loan_Default)
num_cols <- ncol(Loan_Default)
cat("Dataset has", num_rows, "rows and", num_cols, "columns.\n")


Dataset has 20000 rows and 21 columns.


# Check for missing values
missing_values <- colSums(is.na(Loan_Default))
cat("Missing values per column:\n")
print(missing_values[missing_values > 0])

#No missing values

#If there was I would use - Using Mean Imputation for numeric variables


# Verify no missing values remain
cat("Missing values after imputation:\n")
print(colSums(is.na(Loan_Default)))


# Check for duplicate rows
num_duplicates <- sum(duplicated(Loan_Default))
cat("Number of duplicate rows:", num_duplicates, "\n")

#No Duplicate rows

#To remove any duplicates I would use this code

# Remove duplicate rows if any
Loan_Default <- Loan_Default %>% distinct()



# Plot two variables: avg_bal_cards vs. credit_card_age
ggplot(Loan_Default, aes(x = V3, y = V2)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Credit Card Age vs. Average Balance on Cards",
       x = "Credit Card Age",
       y = "Average Balance on Cards") +
  theme_minimal()

# Plot distribution of a categorical variable: rep_education
ggplot(Loan_Default, aes(x = V20)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Reported Education Levels",
       x = "Education Level",
       y = "Count") +
  theme_minimal()

#The scatter plot shows the relationship between Credit Card Age and Average Balance on Cards. The data points are densely packed, especially on the left side, indicating many individuals have a low credit card age.

#The bar chart illustrates the distribution of education levels in the dataset. Most individuals have a college education, followed by those with a high school education.



table(data$rep_education)
prop.table(table(data$rep_education)) * 100
#   60.68% Of the study has a college degree,26.57% have high school degree or GED,12% with Graduate and 0.71% have other.

table(data$Def_ind)
prop.table(table(data$Def_ind)) * 100
#Not Balanced,  90% - 10% imbalance.
data$Def_ind <- (data$Def_ind - min(data$Def_ind)) / 
  (max(data$Def_ind) - min(data$Def_ind))

#   To address this, I could oversample the smaller category or undersample the larger one. Alternatively, I could apply log transformation, normalization, or standardization.
hist(data$rep_income, 
     main = "Histogram of rep_income", 
     xlab = "rep_income", 
     col = "red", 
     breaks = 30)
#  Looking at this graph, rep_income appears approximately normal with no noticeable skew.





ggplot(data, aes(x = rep_education, y = Def_ind, fill = Def_ind)) +
  geom_bar(stat = "identity") +
  labs(title = "Default Rate by Education Level",
       x = "Education Level",
       y = "Default Rate") +
  theme_minimal()
#   This graph highlights the significantly higher default rate among college-educated individuals compared to other education levels, followed by high school, graduate, and other.



# Split the data set
set.seed(42)
trainIndex <- createDataPartition(data$Def_ind, p=0.8, list=FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

model <- glm(Def_ind ~ ., family = binomial, data = data)


knn_model <- train(Def_ind ~ ., data = train, method = "knn", tuneLength = 5)

#Now you have to make predictions for the test
pred_knn <- predict(knn_model, test)

#  Make sure both the predicted and actual values are factors with matching levels.


pred_knn <- factor(pred_knn, levels = c(0, 1))
test$Def_ind <- factor(test$Def_ind, levels = c(0, 1))

conf_matrix <- confusionMatrix(as.factor(pred_knn), as.factor(test$Def_ind))

print(conf_matrix)

accuracy <- conf_matrix$overall['Accuracy']
cat("Accuracy of KNN model:", accuracy, "\n")
#Model Accuaracy is 0.94.

precision <- conf_matrix$byClass['Precision']
cat("Precision:", precision, "\n")
#Precision = 0.94
recall <- conf_matrix$byClass['Recall']
cat("Recall:", recall, "\n")
#Recall = 1

knn_prob <- predict(knn_model, test, type = "prob")

prob_pos_class <- knn_prob[, 2]

roc_curve <- roc(test$Def_ind, prob_pos_class)

plot(roc_curve, main = "ROC Curve", col = "purple", lwd = 2)

importance <- summary(model)$coefficients
importance <- as.data.frame(importance)
importance <- importance[order(abs(importance$Estimate), decreasing = TRUE),]
importance

# Train and evaluate Decision Tree
# Outcome variable is converted to 2 levels
train$Def_ind <- factor(train$Def_ind, levels = c(0, 1))  

dt_model <- train(Def_ind ~ ., data=train, method='rpart') # Fit Decision Tree Model
pred_dt <- predict(dt_model, test)
conf_matrix_dt <- confusionMatrix(pred_dt, test$Def_ind)
print(conf_matrix_dt)
## Evauluate and interprate both models.
accuracy <- conf_matrix_dt$overall['Accuracy']
cat("Accuracy:", accuracy, "\n")
# 91%
precision <- conf_matrix_dt$byClass['Precision']
cat("Precision:", precision, "\n")
# 91%
recall <- conf_matrix_dt$byClass['Recall']
cat("Recall:", recall, "\n")
# 99%

# KNN performed better, showing higher accuracy, precision, and recall than the decision tree.


