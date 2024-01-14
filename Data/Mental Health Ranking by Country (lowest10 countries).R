# Install and load necessary packages
install.packages("ggplot2")
library(ggplot2)

# Read data from CSV file
data <- read.csv("lowestDep.csv")
data
# Define custom color palette
custom_colors <- c("#FF6699", "#FFAE99", "#FFFF99", "#A3FF99", "#6BFFD6", "#99FFFF", "#99B3FF", "#9966FF", "#CC66FF", "#27CCFF")


# Plotting
ggplot(data, aes(x = Country, y = DepressionRate, fill = as.factor(Country))) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  labs(title = "Mental Health Ranking by Country (Depression Rate Lowest 10 countries)",
       x = "Country",
       y = "Depression Rate (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove legend for individual country colors


n <- nrow(data)  # Number of observations
mean_value <- mean(data$DepressionRate, na.rm = TRUE)  # Mean
sd_value <- sd(data$DepressionRate, na.rm = TRUE)  # Standard deviation
median_value <- median(data$DepressionRate, na.rm = TRUE)  # Median
trimmed_mean <- mean(data$DepressionRate, trim = 0.1, na.rm = TRUE)  # Trimmed mean
mad_value <- mad(data$DepressionRate, na.rm = TRUE)  # Mean Absolute Deviation
min_value <- min(data$DepressionRate, na.rm = TRUE)  # Minimum value
max_value <- max(data$DepressionRate, na.rm = TRUE)  # Maximum value
first_quartile <- quantile(data$DepressionRate, 0.25, na.rm = TRUE)  # 1st Quartile
third_quartile <- quantile(data$DepressionRate, 0.75, na.rm = TRUE)  # 3rd Quartile
nas_count <- sum(is.na(data$DepressionRate))  # Count of NAs

# Manual calculation of skewness and kurtosis
skewness_value <- sum((data$DepressionRate - mean_value)^3) / (n * sd_value^3)
kurtosis_value <- sum((data$DepressionRate - mean_value)^4) / (n * sd_value^4) - 3

standard_error <- sd_value / sqrt(n)  # Standard Error

# Print the results
cat("Number of observations (n):", n, "\n")
cat("Mean:", mean_value, "\n")
cat("Standard Deviation:", sd_value, "\n")
cat("Median:", median_value, "\n")
cat("Trimmed Mean:", trimmed_mean, "\n")
cat("Mean Absolute Deviation:", mad_value, "\n")
cat("Minimum value:", min_value, "\n")
cat("Maximum value:", max_value, "\n")
cat("1st Quartile:", first_quartile, "\n")
cat("3rd Quartile:", third_quartile, "\n")
cat("Number of NAs:", nas_count, "\n")
cat("Skewness:", skewness_value, "\n")
cat("Kurtosis:", kurtosis_value, "\n")
cat("Standard Error:", standard_error, "\n")