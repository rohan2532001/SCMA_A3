# Load necessary libraries
library(readxl)
library(caret)
library(rpart)
library(pROC)

# Load the dataset
titanic_data <- read_excel("C://Users//HP//Desktop//Rohan A3//TITANIC DATA.xlsx", sheet = "Sheet1")

# Data preprocessing
# Convert categorical variables to factors
titanic_data$Survived <- as.factor(titanic_data$Survived)
titanic_data$Pclass <- as.factor(titanic_data$Pclass)
titanic_data$Sex <- as.factor(titanic_data$Sex)
titanic_data$Embarked <- as.factor(titanic_data$Embarked)

# Handle missing values (e.g., using median/mode for imputation or removing rows)
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)
titanic_data$Embarked[is.na(titanic_data$Embarked)] <- "S" # Mode imputation

# Splitting the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(titanic_data$Survived, p = 0.7, list = FALSE)
train_data <- titanic_data[trainIndex, ]
test_data <- titanic_data[-trainIndex, ]

# Logistic Regression Model
logistic_model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                      data = train_data, family = binomial)

# Summary of the model
summary(logistic_model)

# Predictions and Evaluation
predictions <- predict(logistic_model, test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Confusion Matrix
confusionMatrix(factor(predicted_classes), test_data$Survived)

# ROC Curve
roc_curve <- roc(test_data$Survived, predictions)
plot(roc_curve)
print(auc(roc_curve))

# Decision Tree Model
tree_model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                    data = train_data, method = "class")

# Plot the tree
plot(tree_model)
text(tree_model)

# Predictions and Evaluation for Decision Tree
tree_predictions <- predict(tree_model, test_data, type = "class")
confusionMatrix(tree_predictions, test_data$Survived)

# ROC Curve for Decision Tree
tree_roc_curve <- roc(test_data$Survived, as.numeric(tree_predictions))
plot(tree_roc_curve)
print(auc(tree_roc_curve))
