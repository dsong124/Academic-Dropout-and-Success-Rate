install.packages("caTools")
install.packages("randomForest")
install.packages("caret")
library(caTools)
library(randomForest)
library(caret)

data <- read.csv("Desktop/Data101/schoolsuccess.csv", sep = ";", header = TRUE)
str(data)
data$Target <- as.factor(data$Target)

set.seed(13245)
split <- sample.split(data$Target, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# train rf model
rf_model <- randomForest(Target ~ ., data = train_data, importance = TRUE, ntree = 1000)
print(rf_model)

predictions <- predict(rf_model, test_data)

conf_matrix <- confusionMatrix(predictions, test_data$Target)
print(conf_matrix)

# RMSE
prob_predictions <- predict(rf_model, test_data, type = "prob")
predicted_classes <- max.col(prob_predictions)
true_classes <- as.numeric(test_data$Target)
rmse <- sqrt(mean((predicted_classes - true_classes)^2))
print(paste("RMSE:", rmse))

#extra information
importance(rf_model)
varImpPlot(rf_model)

