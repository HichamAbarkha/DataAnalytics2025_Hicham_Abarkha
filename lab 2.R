# Load necessary libraries
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Read the dataset
df <- read.csv("NY-House-Dataset.csv")

# View dataset structure
str(df)

df <- df %>% select(PRICE, BEDS, BATH, PROPERTYSQFT)

# Check for missing values
colSums(is.na(df))

#There are no outliers so I'm going to try and eliminate outliers


# Define columns to clean
columns_to_clean <- c("PRICE", "PROPERTYSQFT", "BATH")

# Remove outliers using IQR
df_clean <- df %>%
  filter(
    PRICE >= quantile(PRICE, 0.25) - 1.5 * IQR(PRICE) & PRICE <= quantile(PRICE, 0.75) + 1.5 * IQR(PRICE),
    PROPERTYSQFT >= quantile(PROPERTYSQFT, 0.25) - 1.5 * IQR(PROPERTYSQFT) & PROPERTYSQFT <= quantile(PROPERTYSQFT, 0.75) + 1.5 * IQR(PROPERTYSQFT),
    BATH >= quantile(BATH, 0.25) - 1.5 * IQR(BATH) & BATH <= quantile(BATH, 0.75) + 1.5 * IQR(BATH)
  )

# Check dataset size after cleaning
dim(df_clean)
#Reduces observations by ~725

# Histogram for PRICE and PROPERTYSQFT before transformation
par(mfrow = c(1, 2))
hist(df_clean$PRICE, main="Distribution of PRICE", col="lightblue", breaks=50)
hist(df_clean$PROPERTYSQFT, main="Distribution of PROPERTYSQFT", col="lightblue", breaks=50)

# Apply log transformation if data is skewed
df_clean$LOG_PRICE <- log(df_clean$PRICE)
df_clean$LOG_PROPERTYSQFT <- log(df_clean$PROPERTYSQFT)

# Histogram after transformation
par(mfrow = c(1, 2))
hist(df_clean$LOG_PRICE, main="Log(PRICE) Distribution", col="lightblue", breaks=50)
hist(df_clean$LOG_PROPERTYSQFT, main="Log(PROPERTYSQFT) Distribution", col="lightblue", breaks=50)

# Model 1: LOG_PRICE ~ LOG_PROPERTYSQFT
model1 <- lm(LOG_PRICE ~ LOG_PROPERTYSQFT, data = df_clean)

# Model 2: LOG_PRICE ~ LOG_PROPERTYSQFT + BEDS 
model2 <- lm(LOG_PRICE ~ LOG_PROPERTYSQFT + BEDS, data = df_clean)

# Model 3: LOG_PRICE ~ LOG_PROPERTYSQFT + BATH - (wont be included was just curious to see indiv effect of bath)
model3 <- lm(LOG_PRICE ~ LOG_PROPERTYSQFT + BATH, data = df_clean)

# Model 4: LOG_PRICE ~ LOG_PROPERTYSQFT + BEDS + BATH
model4 <- lm(LOG_PRICE ~ LOG_PROPERTYSQFT + BEDS + BATH, data = df_clean)

# Print summary of each model
summary(model1)
summary(model2)
summary(model3)
summary(model4)

# Identify most significant predictor from Model 3
ggplot(df_clean, aes(x = BATH, y = LOG_PRICE)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Log(Price) vs. Number of Bathrooms",
       x = "Number of Bathrooms", y = "Log(Price)") +
  theme_minimal()

par(mfrow = c(1, 3))  # Arrange plots in 1 row, 3 columns

# Model 1 Residuals
plot(model1$fitted.values, resid(model1),
     main="Residuals of Model 1", xlab="Fitted Values", ylab="Residuals", pch=20)
abline(h = 0, col = "red", lty = 2)

# Model 2 Residuals
plot(model2$fitted.values, resid(model2),
     main="Residuals of Model 2", xlab="Fitted Values", ylab="Residuals", pch=20)
abline(h = 0, col = "red", lty = 2)

# Model 3 Residuals
plot(model3$fitted.values, resid(model3),
     main="Residuals of Model 3", xlab="Fitted Values", ylab="Residuals", pch=20)
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1, 1))  # Reset layout

# Compare R-squared values
cat("Model 1 R-squared:", summary(model1)$r.squared, "\n")
cat("Model 2 R-squared:", summary(model2)$r.squared, "\n")
cat("Model 3 R-squared:", summary(model3)$r.squared, "\n")

# Compare Adjusted R-squared values
cat("Model 1 Adjusted R-squared:", summary(model1)$adj.r.squared, "\n")
cat("Model 2 Adjusted R-squared:", summary(model2)$adj.r.squared, "\n")
cat("Model 3 Adjusted R-squared:", summary(model3)$adj.r.squared, "\n")
