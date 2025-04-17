library(e1071)
library(caret)
library(Metrics)
######### ####



df <- read.csv("NY-House-Dataset (1).csv")


df <- df[!is.na(df$PRICE) & !is.na(df$PROPERTYSQFT), ]
df <- df[df$PRICE > 0 & df$PROPERTYSQFT > 0, ]


df$log_price <- log10(df$PRICE)
df$log_sqft <- log10(df$PROPERTYSQFT)


set.seed(123)
train_index <- createDataPartition(df$log_price, p = 0.75, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]


lm_model <- lm(log_price ~ log_sqft, data = train)
lm_preds <- predict(lm_model, newdata = test)


svm_linear <- svm(log_price ~ log_sqft, data = train, kernel = "linear")
svm_linear_preds <- predict(svm_linear, newdata = test)

svm_rbf <- svm(log_price ~ log_sqft, data = train, kernel = "radial")
svm_rbf_preds <- predict(svm_rbf, newdata = test)


get_metrics <- function(true, pred) {
  mae_val <- mae(true, pred)
  mse_val <- mse(true, pred)
  rmse_val <- rmse(true, pred)
  return(c(MAE = mae_val, MSE = mse_val, RMSE = rmse_val))
}


lm_metrics <- get_metrics(test$log_price, lm_preds)
svm_linear_metrics <- get_metrics(test$log_price, svm_linear_preds)
svm_rbf_metrics <- get_metrics(test$log_price, svm_rbf_preds)

results <- rbind(
  Linear_Regression = lm_metrics,
  SVM_Linear = svm_linear_metrics,
  SVM_Radial = svm_rbf_metrics
)


print(round(results, 4))
