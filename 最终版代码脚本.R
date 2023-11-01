# Set working directory to "Downloads" folder
setwd("~/Downloads")

# Load necessary libraries
library(ggplot2)  # Data visualization
library(tidyr)    # Data tidying
library(dplyr)    # Data manipulation

# Check if lmtest library is installed; if not, install it
if(!require(lmtest)){
  install.packages("lmtest")
}
library(lmtest)   # Durbin-Watson test to check for autocorrelation
library(car)      # Tools for assessing multicollinearity

# Load data from CSV file
data <- read.csv("SusRtlx.csv")

# Preview top and bottom rows of data for initial inspection
head(data)
tail(data)

# Step 1: Data Cleaning

# Check for missing values in the dataset
if(any(is.na(data))) {
  cat("Missing values detected in the dataset. Consider cleaning before analysis.\n")
} else {
  cat("No missing values in the dataset.\n")
}

# Remove outliers in SUS Score based on domain knowledge
data <- data[data$SUS.Score >= 0, ]

# Obtain summary statistics to further detect potential outliers
summary(data)

# Step 2: Descriptive Statistics

# Descriptive statistics for SUS Score
cat("Descriptive statistics for SUS Score:\n")
mean_SUS <- mean(data$SUS.Score)
sd_SUS <- sd(data$SUS.Score)
cat("Mean SUS Score:", mean_SUS, "\n")
cat("Standard Deviation SUS Score:", sd_SUS, "\n")

# Additional descriptive statistics for SUS Score
cat("Median SUS Score:", median(data$SUS.Score), "\n")
cat("1st Quartile SUS Score:", quantile(data$SUS.Score, 0.25), "\n")
cat("3rd Quartile SUS Score:", quantile(data$SUS.Score, 0.75), "\n")
cat("Min SUS Score:", min(data$SUS.Score), "\n")
cat("Max SUS Score:", max(data$SUS.Score), "\n")

# Descriptive statistics for RTLX Score
cat("Descriptive statistics for RTLX Score:\n")
mean_RTLX <- mean(data$RTLX.Score)
sd_RTLX <- sd(data$RTLX.Score)
cat("Mean RTLX Score:", mean_RTLX, "\n")
cat("Standard Deviation RTLX Score:", sd_RTLX, "\n")

# Additional descriptive statistics for RTLX Score
cat("Median RTLX Score:", median(data$RTLX.Score), "\n")
cat("1st Quartile RTLX Score:", quantile(data$RTLX.Score, 0.25), "\n")
cat("3rd Quartile RTLX Score:", quantile(data$RTLX.Score, 0.75), "\n")
cat("Min RTLX Score:", min(data$RTLX.Score), "\n")
cat("Max RTLX Score:", max(data$RTLX.Score), "\n")

# Step 3: Data Visualization

# Generate boxplots for SUS.Score and RTLX.Score to visualize data distribution and identify outliers
boxplot(data$SUS.Score, main="Boxplot for SUS.Score", boxwex=0.1)
boxplot(data$RTLX.Score, main="Boxplot for RTLX.Score", boxwex=0.1)

# Histograms to visualize the distribution of scores
hist(data$SUS.Score, main="Histogram for SUS.Score", xlab="SUS Score")
hist(data$RTLX.Score, main="Histogram for RTLX.Score", xlab="RTLX Score")

# Enhanced scatterplot to visualize potential relationship between SUS and RTLX scores
plot <- ggplot(data, aes(x = RTLX.Score, y = SUS.Score)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of RTLX vs. SUS", x = "RTLX Score", y = "SUS Score") +
  theme_minimal()  
print(plot)

# Step 4: Inferential Statistical Analysis

# Pearson correlation test to assess linear relationship between SUS and RTLX scores
correlation <- cor.test(data$SUS.Score, data$RTLX.Score)
cat("Correlation test results:\n")
print(correlation)

# Linear regression analysis to understand how RTLX Score predicts SUS Score
linear_model <- lm(SUS.Score ~ RTLX.Score, data=data)
summary(linear_model)

# Regression Diagnostics: Durbin-Watson test to check for autocorrelation in residuals
dwtest_result <- dwtest(linear_model)
print(dwtest_result)

# The end of the code

