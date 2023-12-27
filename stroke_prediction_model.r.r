# Load necessary libraries
library(ggplot2)
library(caret)

# Load the dataset (replace 'your_dataset.csv' with the actual file path)
data <- read.csv("archive/healthcare-dataset-stroke-data.csv")

# Data Exploration
summary(data)
# Check the distribution of the target variable
table(data$stroke)

# Correlation Analysis
correlation_matrix <- cor(data[, c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi", "stroke")])
print(correlation_matrix)

# Visualize correlation matrix
corrplot(correlation_matrix, method = "color")

# Feature Engineering
# (Convert categorical variables to numerical using one-hot encoding)

# Data Splitting
set.seed(123)
splitIndex <- createDataPartition(data$stroke, p = 0.7, list = FALSE)
trainData <- data[splitIndex, ]
testData <- data[-splitIndex, ]

# Model Selection - Logistic Regression
strokeModel <- glm(stroke ~ ., data = trainData, family = "binomial")

# Model Evaluation
predictions <- predict(strokeModel, newdata = testData, type = "response")
predictedLabels <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the model
confusionMatrix(table(predictedLabels, testData$stroke))

# Feature Importance (if applicable)
# (Analyze coefficients if using logistic regression)

# Cross-Validation (if needed)

# Adjustment and Iteration
# (Modify model or perform more advanced feature engineering based on results)
