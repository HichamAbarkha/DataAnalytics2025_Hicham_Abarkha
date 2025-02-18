# Load necessary libraries
library(class)
library(caret)
library(ggplot2)

abalone <- read.csv("C:/Users/hicha/Downloads/abalone_dataset.csv")

# Add new column 'age.group' with three values based on the number of rings
abalone$age.group <- cut(abalone$rings, breaks = c(0, 8, 11, 35), labels = c("young", "adult", "old"))

#Exercise 2


abalone$age.group <- as.numeric(factor(abalone$age.group))

#subsets
features_set_1 <- c("length", "diameter", "height", "whole_weight")
features_set_2 <- c("shucked_wieght", "viscera_wieght", "shell_weight", "rings")

X1 <- abalone[, features_set_1]
X2 <- abalone[, features_set_2]
y <- abalone$age.group

#training/test sets
set.seed(42)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X1_train <- X1[train_index, ]; X1_test <- X1[-train_index, ]
X2_train <- X2[train_index, ]; X2_test <- X2[-train_index, ]
y_train <- y[train_index]; y_test <- y[-train_index]

# standardizing features
X1_train_scaled <- scale(X1_train)
X1_test_scaled <- scale(X1_test)

X2_train_scaled <- scale(X2_train)
X2_test_scaled <- scale(X2_test)

# Training/evaluating kNN models with k=5
k <- 5
y_pred1 <- knn(X1_train_scaled, X1_test_scaled, y_train, k = k)
y_pred2 <- knn(X2_train_scaled, X2_test_scaled, y_train, k = k)

# Compute accuracy scores
accuracy1 <- sum(y_pred1 == y_test) / length(y_test)
accuracy2 <- sum(y_pred2 == y_test) / length(y_test)


# Compute confusion matrices
conf_matrix1 <- table(y_test, y_pred1)
conf_matrix2 <- table(y_test, y_pred2)

# Print results
print(paste("Accuracy Model 1:", accuracy1))
print(paste("Accuracy Model 2:", accuracy2))
print("Confusion Matrix Model 1:")
print(conf_matrix1)
print("Confusion Matrix Model 2:")
print(conf_matrix2)

# Find the optimal k for Model 2
k_values <- 1:20
accuracy_scores <- sapply(k_values, function(k) {
  y_pred <- knn(X2_train_scaled, X2_test_scaled, y_train, k = k)
  sum(y_pred == y_test) / length(y_test)
})

# Get the best k
optimal_k <- k_values[which.max(accuracy_scores)]
best_accuracy <- max(accuracy_scores)

# Print the optimal k
print(paste("Optimal k:", optimal_k))
print(paste("Best Accuracy:", best_accuracy))

#Best K ends up being 1?

accuracy_df <- data.frame(K = k_values, Accuracy = accuracy_scores)

ggplot(accuracy_df, aes(x = K, y = Accuracy)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "K-NN Accuracy for Different K Values",
       x = "K Value",
       y = "Accuracy") +
  theme_minimal()
#------------------------------------------------------------------------
#Exercise 2

# Selecting the best-performing feature subset
best_features <- c("shucked_wieght", "viscera_wieght", "shell_weight", "rings")
abalone_best <- abalone[, best_features]

# Standardizing the features
abalone_scaled <- scale(abalone_best)

# Finding optimal K using the Elbow Method
wss <- numeric(10)  # Store within-cluster sum of squares
for (k in 1:10) {
  kmeans_model <- kmeans(abalone_scaled, centers = k, nstart = 25)
  wss[k] <- kmeans_model$tot.withinss
}

# Ploting the Elbow Method
ggplot(data.frame(K = 1:10, WSS = wss), aes(x = K, y = WSS)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Elbow Method for Optimal K in K-Means", x = "Number of Clusters (K)", y = "Total Within-Cluster Sum of Squares") +
  theme_minimal()

#Elbow point is around k=3

optimal_k <- 3  # K=3 based on elbow method

# ing K-Means with optimal K
set.seed(42)
kmeans_result <- kmeans(abalone_scaled, centers = optimal_k, nstart = 25)

# Adding cluster labels to dataset
abalone$cluster <- as.factor(kmeans_result$cluster)

# Plotting assigned clusters for two features
selected_features <- c("shucked_wieght", "viscera_wieght")
ggplot(abalone, aes(x = get(selected_features[1]), y = get(selected_features[2]), color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "K-Means Clustering on Abalone Dataset",
       x = selected_features[1],
       y = selected_features[2]) +
  theme_minimal()
