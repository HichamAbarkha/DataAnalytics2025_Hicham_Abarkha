# Load required libraries
library(e1071)
library(caret)
library(Metrics)


# Load the dataset
df <- read.csv("NY-House-Dataset (1).csv")

# Filter out missing or zero values
df <- df[!is.na(df$PRICE) & !is.na(df$PROPERTYSQFT), ]
df <- df[df$PRICE > 0 & df$PROPERTYSQFT > 0, ]

# Optional: Log transform to reduce skewness
df$log_price <- log10(df$PRICE)
df$log_sqft <- log10(df$PROPERTYSQFT)

# Split into training and testing sets (75/25)
set.seed(123)
train_index <- createDataPartition(df$log_price, p = 0.75, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]

# -----------------------
# Model 1: Linear Regression
# -----------------------
lm_model <- lm(log_price ~ log_sqft, data = train)
lm_preds <- predict(lm_model, newdata = test)

# -----------------------
# Model 2: SVM (Linear Kernel)
# -----------------------
svm_linear <- svm(log_price ~ log_sqft, data = train, kernel = "linear")
svm_linear_preds <- predict(svm_linear, newdata = test)

# -----------------------
# Model 3: SVM (Radial Kernel)
# -----------------------
svm_rbf <- svm(log_price ~ log_sqft, data = train, kernel = "radial")
svm_rbf_preds <- predict(svm_rbf, newdata = test)

# -----------------------
# Error Metrics Function
# -----------------------
get_metrics <- function(true, pred) {
  mae_val <- mae(true, pred)
  mse_val <- mse(true, pred)
  rmse_val <- rmse(true, pred)
  return(c(MAE = mae_val, MSE = mse_val, RMSE = rmse_val))
}

# -----------------------
# Evaluate Models
# -----------------------
lm_metrics <- get_metrics(test$log_price, lm_preds)
svm_linear_metrics <- get_metrics(test$log_price, svm_linear_preds)
svm_rbf_metrics <- get_metrics(test$log_price, svm_rbf_preds)

# Combine into a data frame
results <- rbind(
  Linear_Regression = lm_metrics,
  SVM_Linear = svm_linear_metrics,
  SVM_Radial = svm_rbf_metrics
)

# Print results
print(round(results, 4))
