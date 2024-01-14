# Install and load necessary packages
install.packages(c("ggplot2", "nhanes", "dplyr"))
library(ggplot2)
library(NHANES)
library(dplyr)

# Load NHANES dataset
data("NHANES")

# Subset data for specific variables and remove missing values
df <- NHANES %>%
  select(Gender, BMI, SurveyYr) %>%
  filter_all(all_vars(!is.na(.)))

# Define custom color palette
custom_colors <- c("#FF6699", "#99B3FF", "#FFFF99", "#A3FF99", "#6BFFD6", "#99FFFF", "#FFAE99", "#9966FF", "#CC66FF", "#27CCFF")

# Plotting
ggplot(df, aes(x = as.factor(SurveyYr), y = BMI, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  labs(title = "BMI Trends by Survey Year and Gender",
       x = "Survey Year",
       y = "BMI",
       fill = "Gender") +
  theme_minimal()

# Descriptive statistics for Gender, BMI, and SurveyYr
summary_stats_gender <- summary(df$Gender)
summary_stats_bmi <- summary(df$BMI)
summary_stats_surveyyr <- summary(df$SurveyYr)

cat("Descriptive Statistics for Gender:\n")
print(summary_stats_gender)

cat("\nDescriptive Statistics for BMI:\n")
print(summary_stats_bmi)

cat("\nDescriptive Statistics for Survey Year:\n")
print(summary_stats_surveyyr)

# Assuming you have already defined df <- NHANES %>% select(Gender, BMI, SurveyYr) %>%
# filter_all(all_vars(!is.na(.)))

# Number of observations
n <- nrow(df)

# Mean
mean_value <- mean(df$BMI, na.rm = TRUE)

# Standard deviation
sd_value <- sd(df$BMI, na.rm = TRUE)

# Median
median_value <- median(df$BMI, na.rm = TRUE)

# Trimmed mean
trimmed_mean <- mean(df$BMI, trim = 0.1, na.rm = TRUE)

# Mean Absolute Deviation
mad_value <- mad(df$BMI, na.rm = TRUE)

# Minimum value
min_value <- min(df$BMI, na.rm = TRUE)

# Maximum value
max_value <- max(df$BMI, na.rm = TRUE)

# 1st Quartile
first_quartile <- quantile(df$BMI, 0.25, na.rm = TRUE)

# 3rd Quartile
third_quartile <- quantile(df$BMI, 0.75, na.rm = TRUE)

# Count of NAs
nas_count <- sum(is.na(df$BMI))

# Manual calculation of skewness and kurtosis
skewness_value <- sum((df$BMI - mean_value)^3) / (n * sd_value^3)
kurtosis_value <- sum((df$BMI - mean_value)^4) / (n * sd_value^4) - 3

# Standard Error
standard_error <- sd_value / sqrt(n)

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

