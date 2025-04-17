library(e1071)
library(class)
library(caret)
library(MLmetrics)
library(dplyr)

wine <- read.csv("wine.data", header = FALSE)
colnames(wine) <- c("Class", paste0("V", 1:13))


selected_features <- c("V1", "V2", "V3", "V4")
wine_subset <- wine[, c("Class", selected_features)]


set.seed(123)
train_idx <- createDataPartition(wine_subset$Class, p = 0.7, list = FALSE)
train_data <- wine_subset[train_idx, ]
test_data <- wine_subset[-train_idx, ]

train_data$Class <- as.factor(train_data$Class)
test_data$Class <- as.factor(test_data$Class)


tune_linear <- tune.svm(Class ~ ., data = train_data, kernel = "linear", cost = 10^(-1:2))
svm_linear <- tune_linear$best.model
pred_linear <- predict(svm_linear, test_data)


tune_rbf <- tune.svm(Class ~ ., data = train_data, kernel = "radial", 
                     cost = 10^(-1:2), gamma = 10^(-2:1))
svm_rbf <- tune_rbf$best.model
pred_rbf <- predict(svm_rbf, test_data)


knn_pred <- knn(train = train_data[, -1],
                test = test_data[, -1],
                cl = train_data$Class,
                k = 5)

evaluate <- function(pred, actual) {
  cm <- confusionMatrix(as.factor(pred), as.factor(actual))
  cm_table <- cm$table
  
  
  classes <- sort(unique(actual))
  
  
  precisions <- recalls <- f1s <- c()
  
  for (class in classes) {
    TP <- cm_table[class, class]
    FP <- sum(cm_table[, class]) - TP
    FN <- sum(cm_table[class, ]) - TP
    
    precision <- if ((TP + FP) == 0) 0 else TP / (TP + FP)
    recall <- if ((TP + FN) == 0) 0 else TP / (TP + FN)
    f1 <- if ((precision + recall) == 0) 0 else 2 * precision * recall / (precision + recall)
    
    precisions <- c(precisions, precision)
    recalls <- c(recalls, recall)
    f1s <- c(f1s, f1)
  }
  
  return(data.frame(
    Precision = mean(precisions),
    Recall = mean(recalls),
    F1 = mean(f1s)
  ))
}

results_linear <- evaluate(pred_linear, test_data$Class)
results_rbf <- evaluate(pred_rbf, test_data$Class)
results_knn <- evaluate(knn_pred, test_data$Class)


results <- rbind(SVM_Linear = results_linear,
                 SVM_RBF = results_rbf,
                 kNN = results_knn)

print("Comparison of classifier performance")
print(results)



library(e1071)
library(ggplot2)


df <- read.csv("NY-House-Dataset (1).csv")


df <- df[!is.na(df$PRICE) & !is.na(df$PROPERTYSQFT), ]
df <- df[df$PRICE > 0 & df$PROPERTYSQFT > 0, ]


df$PRICE_M <- df$PRICE / 1e6


set.seed(123)
idx <- sample(1:nrow(df), 0.8 * nrow(df))
train <- df[idx, ]
test <- df[-idx, ]


svm_model <- svm(PRICE_M ~ PROPERTYSQFT, data = train)


lm_model <- lm(PRICE_M ~ PROPERTYSQFT, data = train)


svm_preds <- predict(svm_model, test)
lm_preds <- predict(lm_model, test)


par(mfrow = c(1, 2))
plot(test$PRICE_M, svm_preds, col = 'blue', pch = 16,
     main = "SVM: Predicted vs Actual",
     xlab = "Actual Price (Millions)", ylab = "Predicted Price")
abline(0, 1, col = "red")

plot(test$PRICE_M, lm_preds, col = 'green', pch = 16,
     main = "Linear Model: Predicted vs Actual",
     xlab = "Actual Price (Millions)", ylab = "Predicted Price")
abline(0, 1, col = "red")

# Residual plots
svm_residuals <- test$PRICE_M - svm_preds
lm_residuals <- test$PRICE_M - lm_preds

par(mfrow = c(1, 2))
plot(test$PROPERTYSQFT, svm_residuals, col = "blue", pch = 16,
     main = "SVM Residuals", xlab = "Square Footage", ylab = "Residuals")
abline(h = 0, col = "red")

plot(test$PROPERTYSQFT, lm_residuals, col = "green", pch = 16,
     main = "Linear Model Residuals", xlab = "Square Footage", ylab = "Residuals")
abline(h = 0, col = "red")


