# Install and load necessary packages
install.packages(c("ggplot2", "nhanes", "dplyr", "viridis"))
library(ggplot2)
library(NHANES)
library(dplyr)
library(viridis)

# Filter relevant variables and remove missing values
df <- NHANES %>%
  select(BMI, SurveyYr, DaysMentHlthBad) %>%
  filter_all(any_vars(!is.na(.)))

# Plotting
ggplot(df, aes(x = factor(SurveyYr), y = DaysMentHlthBad, fill = BMI)) +
  geom_tile(width = 0.8, height = 0.8) +  # Adjust width and height for spacing
  scale_fill_viridis_c() +
  labs(
    title = "Mean BMI by Year and DaysMentHlthBad",
    x = "Survey Year",
    y = "DaysMentHlthBad",
    fill = "Mean BMI"
  ) +
  theme_minimal()

# Descriptive statistics for BMI
n <- nrow(df)
mean_value <- mean(df$BMI, na.rm = TRUE)
sd_value <- sd(df$BMI, na.rm = TRUE)
median_value <- median(df$BMI, na.rm = TRUE)
trimmed_mean <- mean(df$BMI, trim = 0.1, na.rm = TRUE)
mad_value <- mad(df$BMI, na.rm = TRUE)
min_value <- min(df$BMI, na.rm = TRUE)
max_value <- max(df$BMI, na.rm = TRUE)
first_quartile <- quantile(df$BMI, 0.25, na.rm = TRUE)
third_quartile <- quantile(df$BMI, 0.75, na.rm = TRUE)
nas_count <- sum(is.na(df$BMI))
skewness_value <- sum((df$BMI - mean_value)^3) / (n * sd_value^3)
kurtosis_value <- sum((df$BMI - mean_value)^4) / (n * sd_value^4) - 3
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




