
library(tidyverse)
library(caret)   # For train/test split, confusionMatrix, etc.
library(class)   # For kNN
library(ggplot2) # For plotting

# ======================================================
# 2) Read the wine.names file (usually documentation)
#    We'll store it in a character vector, though it's optional for the analysis.
# ======================================================
setwd("C:/Users/hicha/Downloads/wine")


wine_info <- readLines("wine.names")

column_names <- c("Class",
                  "Alcohol",
                  "Malic_acid",
                  "Ash",
                  "Alcalinity_of_ash",
                  "Magnesium",
                  "Total_phenols",
                  "Flavanoids",
                  "Nonflavanoid_phenols",
                  "Proanthocyanins",
                  "Color_intensity",
                  "Hue",
                  "OD280_OD315_of_diluted_wines",
                  "Proline")

# ======================================================
# 4) Read the wine.data file
#    Set 'header = FALSE' because there's no header row.
#    We assume it's comma-separated (as in the UCI repository).
# ======================================================
wine <- read.csv("wine.data",
                 header = FALSE,
                 col.names = column_names)

# Quick checks
cat("\n=== Head of wine Data ===\n")
head(wine)
cat("\n=== Structure of wine Data ===\n")
str(wine)


X <- wine %>% select(-Class)
y <- as.factor(wine$Class)


X_scaled <- as.data.frame(scale(X))

pca_model <- prcomp(X_scaled, center = FALSE, scale. = FALSE)


scores <- pca_model$x

loadings <- pca_model$rotation

expl_var <- pca_model$sdev^2 / sum(pca_model$sdev^2)

cat("\nExplained variance ratio by each PC:\n")
print(expl_var)

# ======================================================
# 8) Plot the dataset on the first 2 principal components
# ======================================================
plot(scores[, 1], scores[, 2],
     col = y, pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Wine Dataset on First 2 Principal Components")
legend("topright", legend = levels(y), col = 1:length(levels(y)), pch = 19)

# Alternatively, using ggplot:
# df_plot <- data.frame(PC1 = scores[,1], PC2 = scores[,2], Class = y)
# ggplot(df_plot, aes(x = PC1, y = PC2, color = Class)) +
#   geom_point() +
#   ggtitle("Wine Dataset on First 2 Principal Components")

# ======================================================
# 9) Identify which features contribute most/least to PC1
# ======================================================
loadings_pc1 <- pca_model$rotation[, "PC1"]
# Sort by absolute value (descending)
abs_loadings_pc1 <- sort(abs(loadings_pc1), decreasing = TRUE)
cat("\nAbsolute loadings in PC1 (descending):\n")
print(abs_loadings_pc1)

# Example: drop the 2 smallest absolute loadings from PC1
least_important_features <- names(tail(abs_loadings_pc1, 2))
cat("\nDropping least-important features:", paste(least_important_features, collapse = ", "), "\n")

# Create a reduced dataset
X_reduced <- X %>% select(-all_of(least_important_features))
X_reduced_scaled <- as.data.frame(scale(X_reduced))

# Rerun PCA on the reduced set
pca_model_reduced <- prcomp(X_reduced_scaled, center = FALSE, scale. = FALSE)
scores_reduced <- pca_model_reduced$x
expl_var_reduced <- pca_model_reduced$sdev^2 / sum(pca_model_reduced$sdev^2)

cat("\nExplained variance ratio (reduced dataset):\n")
print(expl_var_reduced)

# ======================================================
# 10) Classification using kNN - Original vs. First 3 PCs
#     We'll do a 70/30 train/test split for both methods
# ======================================================

# ------------------
# 10A) kNN with Original (scaled) Data
# ------------------
set.seed(40)  # For reproducible splits
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)

X_train_orig <- X_scaled[trainIndex, ]
X_test_orig  <- X_scaled[-trainIndex, ]
y_train <- y[trainIndex]
y_test  <- y[-trainIndex]

knn_pred_orig <- knn(train = X_train_orig,
                     test = X_test_orig,
                     cl = y_train,
                     k = 5)  # k=5 is just an example

cm_orig <- confusionMatrix(knn_pred_orig, y_test)
cat("\n=== Confusion Matrix (Original Data) ===\n")
print(cm_orig$table)
cat("\n=== Classification Report (Original Data) ===\n")
print(cm_orig$byClass)  # Sensitivity, Specificity, etc. for each class
cat("Overall Accuracy:", cm_orig$overall["Accuracy"], "\n")

# ------------------
# 10B) kNN with First 3 Principal Components
# ------------------
# We'll split the same rows (trainIndex) in the PCA scores
X_pca_all <- scores
X_pca_3   <- X_pca_all[, 1:3]  # first 3 PCs

X_pca_3_train <- X_pca_3[trainIndex, ]
X_pca_3_test  <- X_pca_3[-trainIndex, ]

knn_pred_pca <- knn(train = X_pca_3_train,
                    test = X_pca_3_test,
                    cl = y_train,
                    k = 5)

cm_pca <- confusionMatrix(knn_pred_pca, y_test)
cat("\n=== Confusion Matrix (First 3 PCs) ===\n")
print(cm_pca$table)
cat("\n=== Classification Report (First 3 PCs) ===\n")
print(cm_pca$byClass)
cat("Overall Accuracy:", cm_pca$overall["Accuracy"], "\n")


cm_orig$overall["Accuracy"] 
cm_pca$overall["Accuracy"]


