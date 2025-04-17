library(ggplot2)
library(dplyr)
library(car)
library(class)      
library(dplyr)      
library(caret)      
library(ggpubr)

################################################################################################# Part 1

epi_data <- read.csv("C:/Users/hicha/Downloads/epi2024results06022024.csv")

# Defining North America and South America country lists
north_america <- c("Canada", "United States", "Mexico", "Guatemala", "Belize",
                   "El Salvador", "Honduras", "Nicaragua", "Costa Rica", "Panama",
                   "Bahamas", "Cuba", "Dominican Republic", "Haiti", "Jamaica", 
                   "Antigua and Barbuda", "Barbados", "Dominica", "Grenada", 
                   "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", 
                   "Trinidad and Tobago")

south_america <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
                   "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")

# Create subsets for North America and South America
north_america_data <- epi_data %>% filter(country %in% north_america)
south_america_data <- epi_data %>% filter(country %in% south_america)



# Histograms with density lines
ggplot() +
  geom_histogram(data = north_america_data, aes(x = get("EPI.new"), y = ..density..), 
                 bins = 10, fill = "blue", alpha = 0.6) +
  geom_density(data = north_america_data, aes(x = get("EPI.new")), color = "blue") +
  geom_histogram(data = south_america_data, aes(x = get("EPI.new"), y = ..density..), 
                 bins = 10, fill = "red", alpha = 0.6) +
  geom_density(data = south_america_data, aes(x = get("EPI.new")), color = "red") +
  labs(title = paste("Histogram of", variable, "with Density Lines - North vs. South America"),
       x = "EPI.new", y = "Density") +
  theme_minimal()

# QQ Plots against normal distribution
par(mfrow = c(1, 2))  

qqPlot(north_america_data[[variable]], main = paste("QQ Plot of", variable, "- North America"), 
       ylab = variable)

qqPlot(south_america_data[[variable]], main = paste("QQ Plot of", variable, "- South America"), 
       ylab = variable)

#################################################################################################  Part 2


# Reload the dataset
epi_data <- read.csv("C:/Users/hicha/Downloads/epi2024results06022024.csv")

# Define predictor and response variables
predictor_1 <- "BDH.new"  # Biodiversity & Habitat Index
predictor_2 <- "MHP.new"  # Marine Protected Areas Index
response_1 <- "EPI.new"    # Environmental Performance Index
response_2 <- "ECO.new"    # Ecosystem Vitality Index

# Remove missing values if there's any
epi_clean <- epi_data %>%
  select(all_of(c(predictor_1, predictor_2, response_1, response_2))) %>%
  na.omit()

# Fit linear models for epi_clean ^^ I couldnt find GDP nor Population columns
model1 <- lm(EPI.new ~ BDH.new + MHP.new, data = epi_clean)
model2 <- lm(ECO.new ~ BDH.new + MHP.new, data = epi_clean)

# Print model summaries
print(summary(model1))
print(summary(model2))

# Scatter plots with regression lines for most significant predictor (BDH.new)
ggplot(epi_clean, aes(x = BDH.new, y = EPI.new)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("BDH.new vs. EPI.new")

ggplot(epi_clean, aes(x = BDH.new, y = ECO.new)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("BDH.new vs. ECO.new")

# Residual plots
par(mfrow=c(1,2))
plot(model1$fitted.values, residuals(model1), main="Residuals of Model 1: EPI.new", xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")

plot(model2$fitted.values, residuals(model2), main="Residuals of Model 2: ECO.new", xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")


south_america <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
                   "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")

# Subset for South America
south_america_clean <- epi_data %>%
  filter(country %in% south_america) %>%
  select(all_of(c(predictor_1, predictor_2, response_1, response_2))) %>%
  na.omit()



# Fit models for South America subset
model_sa1 <- lm(EPI.new ~ BDH.new + MHP.new, data = south_america_clean)
model_sa2 <- lm(ECO.new ~ BDH.new + MHP.new, data = south_america_clean)

# Print South America model summaries
print(summary(model_sa1))
print(summary(model_sa2))

ggplot(south_america_clean, aes(x = BDH.new, y = EPI.new)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("BDH.new vs. EPI.new")

ggplot(south_america_clean, aes(x = BDH.new, y = ECO.new)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("BDH.new vs. ECO.new")



# Residual plots for South America models
ggplot(data = data.frame(fitted = model_sa1$fitted.values, residuals = residuals(model_sa1)),
       aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess", col = "red") +
  ggtitle("Residual Plot - SA Model 1: EPI.new")

ggplot(data = data.frame(fitted = model_sa2$fitted.values, residuals = residuals(model_sa2)),
       aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess", col = "red") +
  ggtitle("Residual Plot - SA Model 2: ECO.new")

################################################################################################# Part 3
     

# Create "region" column
epi_data$region <- ifelse(epi_data$country %in% north_america, "North America",
                          ifelse(epi_data$country %in% south_america, "South America", NA))

# Filter dataset to include only North America and South America
region_data <- na.omit(epi_data)

# Convert region to a factor
region_data$region <- as.factor(region_data$region)

### **Model 1: Using BDH.new, MHP.new, and ECO.new**
variables_set1 <- c("BDH.new", "MHP.new", "ECO.new", "region")
knn_data1 <- na.omit(region_data[, variables_set1])

# Split into training (70%) and testing (30%)
set.seed(42)
train_index1 <- sample(1:nrow(knn_data1), 0.7 * nrow(knn_data1))
train1 <- knn_data1[train_index1,]
test1 <- knn_data1[-train_index1,]

# Standardize the data
train_scaled1 <- scale(train1[, -4])  # Remove region column
test_scaled1 <- scale(test1[, -4])

#Model 2: Using GTI.new, PAR.new, and EPI.new
variables_set2 <- c("GTI.new", "PAR.new", "EPI.new", "region")
knn_data2 <- na.omit(region_data[, variables_set2])

# Split into training (70%) and testing (30%)
set.seed(42)
train_index2 <- sample(1:nrow(knn_data2), 0.7 * nrow(knn_data2))
train2 <- knn_data2[train_index2,]
test2 <- knn_data2[-train_index2,]

# Standardize the data
train_scaled2 <- scale(train2[, -4])  # Remove region column
test_scaled2 <- scale(test2[, -4])


k_values <- c(1,2,3,4,5,6,7,8,9,10)
accuracy_results <- data.frame(k = integer(), Accuracy_Model1 = numeric(), Accuracy_Model2 = numeric())

for (k in k_values) {
  # Train kNN with varying k-values
  knn_pred1 <- knn(train = train_scaled1, test = test_scaled1, cl = train1$region, k = k)
  knn_pred2 <- knn(train = train_scaled2, test = test_scaled2, cl = train2$region, k = k)
  
  accuracy1 <- sum(knn_pred1 == test1$region) / length(test1$region)
  accuracy2 <- sum(knn_pred2 == test2$region) / length(test2$region)
  
  accuracy_results <- rbind(accuracy_results, data.frame(k = k, Accuracy_Model1 = accuracy1, Accuracy_Model2 = accuracy2))
}


print(accuracy_results)

# Determine the best model based on highest accuracy
best_k1 <- accuracy_results$k[which.max(accuracy_results$Accuracy_Model1)]
best_k2 <- accuracy_results$k[which.max(accuracy_results$Accuracy_Model2)]
best_accuracy1 <- max(accuracy_results$Accuracy_Model1)
best_accuracy2 <- max(accuracy_results$Accuracy_Model2)

cat("Best k for Model 1 (BDH.new, MHP.new, ECO.new):", best_k1, "with Accuracy:", best_accuracy1, "\n")
cat("Best k for Model 2 (GTI.new, PAR.new, EPI.new):", best_k2, "with Accuracy:", best_accuracy2, "\n")

# Train kNN models with the best k-values
knn_best_pred1 <- knn(train = train_scaled1, test = test_scaled1, cl = train1$region, k = best_k1)
knn_best_pred2 <- knn(train = train_scaled2, test = test_scaled2, cl = train2$region, k = best_k2)

# Confusion Matrix for Model 1
conf_matrix1 <- confusionMatrix(knn_best_pred1, test1$region)
cat("\nConfusion Matrix for Model 1 (BDH.new, MHP.new, ECO.new):\n")
print(conf_matrix1)

# Confusion Matrix for Model 2
conf_matrix2 <- confusionMatrix(knn_best_pred2, test2$region)
cat("\nConfusion Matrix for Model 2 (GTI.new, PAR.new, EPI.new):\n")
print(conf_matrix2)

