library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(zoo)
library(forecast)
library(scales)
library(gridExtra)
library(corrplot)
library(lubridate)

# Set theme for all plots
theme_set(theme_minimal() + 
            theme(plot.title = element_text(face = "bold", size = 14),
                  axis.text = element_text(size = 10),
                  legend.position = "bottom"))

# ================================================================================
# PART 1: DATA LOADING AND PREPARATION
# ================================================================================

# Load the cleaned dataset
# Adjust the path to your layoffs_clean.csv file
df <- read_csv("/Users/gaoshan/Desktop/layoff_analysis/layoffs_clean.csv", 
               col_types = cols(
                 Company = col_character(),
                 Location_HQ = col_character(),
                 Industry = col_character(),
                 Laid_Off_Count = col_integer(),
                 Layoff_Date = col_date(format = "%Y-%m-%d"),
                 Source = col_character(),
                 Funds_Raised = col_double(),
                 Stage = col_character(),
                 Date_Added = col_date(format = "%Y-%m-%d"),
                 Country = col_character(),
                 Percentage_Laid_Off = col_double()
               ))

# Display dataset structure
cat("Dataset Summary:\n")
cat("================\n")
cat(sprintf("Total Records: %d\n", nrow(df)))
cat(sprintf("Date Range: %s to %s\n", min(df$Layoff_Date, na.rm=TRUE), 
            max(df$Layoff_Date, na.rm=TRUE)))
cat(sprintf("Industries: %d\n", length(unique(df$Industry))))
cat(sprintf("Countries: %d\n\n", length(unique(df$Country))))

# ================================================================================
# PART 1: DESCRIPTIVE & DIAGNOSTIC ANALYSIS
# ================================================================================

cat("\n========================================\n")
cat("PART 1: DESCRIPTIVE STATISTICS\n")
cat("========================================\n\n")

# 1.1 Overall Descriptive Statistics
overall_stats <- df %>%
  summarise(
    Mean = mean(Laid_Off_Count, na.rm = TRUE),
    Median = median(Laid_Off_Count, na.rm = TRUE),
    SD = sd(Laid_Off_Count, na.rm = TRUE),
    Variance = var(Laid_Off_Count, na.rm = TRUE),
    Min = min(Laid_Off_Count, na.rm = TRUE),
    Max = max(Laid_Off_Count, na.rm = TRUE),
    Q1 = quantile(Laid_Off_Count, 0.25, na.rm = TRUE),
    Q3 = quantile(Laid_Off_Count, 0.75, na.rm = TRUE),
    IQR = IQR(Laid_Off_Count, na.rm = TRUE)
  )

cat("Overall Layoff Statistics:\n")
print(overall_stats)
cat("\n")

# 1.2 Descriptive Statistics by Industry
industry_stats <- df %>%
  group_by(Industry) %>%
  summarise(
    Count = n(),
    Mean = mean(Laid_Off_Count, na.rm = TRUE),
    Median = median(Laid_Off_Count, na.rm = TRUE),
    SD = sd(Laid_Off_Count, na.rm = TRUE),
    Variance = var(Laid_Off_Count, na.rm = TRUE),
    Total_Layoffs = sum(Laid_Off_Count, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Layoffs))

cat("Top 10 Industries by Total Layoffs:\n")
print(head(industry_stats, 10))
cat("\n")

# 1.3 Descriptive Statistics by Country
country_stats <- df %>%
  group_by(Country) %>%
  summarise(
    Count = n(),
    Mean = mean(Laid_Off_Count, na.rm = TRUE),
    Median = median(Laid_Off_Count, na.rm = TRUE),
    SD = sd(Laid_Off_Count, na.rm = TRUE),
    Total_Layoffs = sum(Laid_Off_Count, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Layoffs))

cat("Top 10 Countries by Total Layoffs:\n")
print(head(country_stats, 10))
cat("\n")

# 1.4 Identify Highest and Lowest Median Layoffs
highest_median <- industry_stats %>%
  arrange(desc(Median)) %>%
  slice(1:5)

lowest_median <- industry_stats %>%
  filter(Count >= 10) %>%  # Only industries with sufficient data
  arrange(Median) %>%
  slice(1:5)

cat("Industries with Highest Median Layoffs:\n")
print(highest_median %>% select(Industry, Median, Count))
cat("\n")

cat("Industries with Lowest Median Layoffs (min 10 events):\n")
print(lowest_median %>% select(Industry, Median, Count))
cat("\n")

# ================================================================================
# VISUALIZATIONS
# ================================================================================

# 1.5 Histogram of Laid_Off_Count by Industry (Top 10)
top_10_industries <- industry_stats %>%
  slice(1:10) %>%
  pull(Industry)

p1 <- df %>%
  filter(Industry %in% top_10_industries) %>%
  ggplot(aes(x = Laid_Off_Count, fill = Industry)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~Industry, scales = "free_y", ncol = 3) +
  scale_x_continuous(labels = comma) +
  labs(title = "Distribution of Layoff Counts by Industry (Top 10)",
       x = "Number of Employees Laid Off",
       y = "Frequency") +
  theme(legend.position = "none")

ggsave("layoffs_histogram_by_industry.png", p1, width = 14, height = 10, dpi = 300)
cat("✓ Saved: layoffs_histogram_by_industry.png\n")

# 1.6 Boxplot Comparing Layoff Distributions
p2 <- df %>%
  filter(Industry %in% top_10_industries) %>%
  ggplot(aes(x = reorder(Industry, Laid_Off_Count, FUN = median), 
             y = Laid_Off_Count, 
             fill = Industry)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() +
  scale_y_continuous(labels = comma, trans = "log10") +
  labs(title = "Layoff Distribution Comparison Across Industries (Log Scale)",
       x = "Industry",
       y = "Number of Employees Laid Off (Log Scale)") +
  theme(legend.position = "none")

ggsave("layoffs_boxplot_comparison.png", p2, width = 12, height = 8, dpi = 300)
cat("✓ Saved: layoffs_boxplot_comparison.png\n")

# ================================================================================
# ANOMALY DETECTION
# ================================================================================

cat("\n========================================\n")
cat("ANOMALY DETECTION\n")
cat("========================================\n\n")

# 1.7 Detect Outliers (> Mean + 2*SD by Industry)
outliers <- df %>%
  group_by(Industry) %>%
  mutate(
    Mean = mean(Laid_Off_Count, na.rm = TRUE),
    SD = sd(Laid_Off_Count, na.rm = TRUE),
    Threshold = Mean + 2 * SD,
    Is_Outlier = Laid_Off_Count > Threshold
  ) %>%
  ungroup() %>%
  filter(Is_Outlier == TRUE) %>%
  select(Company, Industry, Laid_Off_Count, Funds_Raised, Stage, 
         Country, Mean, SD, Threshold) %>%
  arrange(desc(Laid_Off_Count))

cat(sprintf("Total Outlier Events: %d\n\n", nrow(outliers)))
cat("Top 10 Outlier Companies (Layoffs > Industry Mean + 2*SD):\n")
print(head(outliers, 10))
cat("\n")

# 1.8 Investigate Outliers by Stage
outlier_stage_summary <- outliers %>%
  group_by(Stage) %>%
  summarise(
    Count = n(),
    Avg_Layoffs = mean(Laid_Off_Count),
    Avg_Funds_Raised = mean(Funds_Raised, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Count))

cat("Outliers by Company Stage:\n")
print(outlier_stage_summary)
cat("\n")

# ================================================================================
# PART 2: INFERENTIAL STATISTICS
# ================================================================================

cat("\n========================================\n")
cat("PART 2: HYPOTHESIS TESTING & CORRELATION\n")
cat("========================================\n\n")

# 2.1 Two-Sample T-Test: Tech vs Finance
# Note: Adjust industry names based on your dataset
# Common alternatives: "Technology", "Financial Services", "FinTech"

# First, let's check what industries we have
cat("Available Industries:\n")
print(sort(unique(df$Industry)))
cat("\n")

# Map to appropriate industries in your dataset
# Adjust these based on your actual industry names
tech_industries <- c("Hardware", "Data", "AI", "Security", "Infrastructure")
finance_industries <- c("Finance", "Crypto", "Real Estate")

tech_layoffs <- df %>%
  filter(Industry %in% tech_industries) %>%
  pull(Laid_Off_Count)

finance_layoffs <- df %>%
  filter(Industry %in% finance_industries) %>%
  pull(Laid_Off_Count)

cat("========================================\n")
cat("HYPOTHESIS TEST: Tech vs Finance\n")
cat("========================================\n")
cat(sprintf("Tech industries: %s\n", paste(tech_industries, collapse = ", ")))
cat(sprintf("Finance industries: %s\n\n", paste(finance_industries, collapse = ", ")))

cat("H0: Mean layoffs in Tech = Mean layoffs in Finance\n")
cat("H1: Mean layoffs in Tech ≠ Mean layoffs in Finance\n\n")

cat(sprintf("Tech - Sample Size: %d, Mean: %.2f, SD: %.2f\n", 
            length(tech_layoffs), 
            mean(tech_layoffs, na.rm=TRUE), 
            sd(tech_layoffs, na.rm=TRUE)))

cat(sprintf("Finance - Sample Size: %d, Mean: %.2f, SD: %.2f\n\n", 
            length(finance_layoffs), 
            mean(finance_layoffs, na.rm=TRUE), 
            sd(finance_layoffs, na.rm=TRUE)))

# Perform t-test
t_test_result <- t.test(tech_layoffs, finance_layoffs, 
                        alternative = "two.sided", 
                        var.equal = FALSE)

print(t_test_result)
cat("\n")

# Interpretation
if (t_test_result$p.value < 0.05) {
  cat("CONCLUSION: Reject H0 (p < 0.05)\n")
  cat("There IS a statistically significant difference between Tech and Finance layoffs.\n\n")
} else {
  cat("CONCLUSION: Fail to reject H0 (p >= 0.05)\n")
  cat("There is NO statistically significant difference between Tech and Finance layoffs.\n\n")
}

# ================================================================================
# 2.2 CORRELATION ANALYSIS
# ================================================================================

cat("========================================\n")
cat("CORRELATION ANALYSIS\n")
cat("========================================\n\n")

# Convert Stage to numeric
stage_levels <- c("Unknown", "Seed", "Series A", "Series B", "Series C", 
                  "Series D", "Series E", "Series F", "Series G", "Series H",
                  "Post-IPO", "Acquired")

df <- df %>%
  mutate(Stage_Code = as.numeric(factor(Stage, levels = stage_levels)))

# (a) Correlation: Funds_Raised vs Laid_Off_Count
cor_funds_layoffs <- cor(df$Funds_Raised, df$Laid_Off_Count, 
                         use = "complete.obs")

cat(sprintf("Correlation (Funds_Raised vs Laid_Off_Count): %.4f\n", 
            cor_funds_layoffs))

# Test significance
cor_test_1 <- cor.test(df$Funds_Raised, df$Laid_Off_Count, 
                       use = "complete.obs")
cat(sprintf("p-value: %.4f\n", cor_test_1$p.value))
cat(sprintf("95%% CI: [%.4f, %.4f]\n\n", 
            cor_test_1$conf.int[1], cor_test_1$conf.int[2]))

# (b) Correlation: Stage_Code vs Percentage_Laid_Off
cor_stage_pct <- cor(df$Stage_Code, df$Percentage_Laid_Off, 
                     use = "complete.obs")

cat(sprintf("Correlation (Stage_Code vs Percentage_Laid_Off): %.4f\n", 
            cor_stage_pct))

cor_test_2 <- cor.test(df$Stage_Code, df$Percentage_Laid_Off, 
                       use = "complete.obs")
cat(sprintf("p-value: %.4f\n", cor_test_2$p.value))
cat(sprintf("95%% CI: [%.4f, %.4f]\n\n", 
            cor_test_2$conf.int[1], cor_test_2$conf.int[2]))

# Visualization: Scatterplot with regression line
p3 <- df %>%
  filter(!is.na(Funds_Raised) & !is.na(Laid_Off_Count)) %>%
  ggplot(aes(x = Funds_Raised, y = Laid_Off_Count)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  scale_x_continuous(labels = dollar, trans = "log10") +
  scale_y_continuous(labels = comma, trans = "log10") +
  labs(title = "Correlation: Funds Raised vs Layoff Count",
       subtitle = sprintf("Pearson r = %.3f, p-value = %.4f", 
                          cor_funds_layoffs, cor_test_1$p.value),
       x = "Funds Raised (Log Scale, $)",
       y = "Employees Laid Off (Log Scale)")

ggsave("correlation_funds_vs_layoffs.png", p3, width = 10, height = 7, dpi = 300)
cat("✓ Saved: correlation_funds_vs_layoffs.png\n")

p4 <- df %>%
  filter(!is.na(Stage_Code) & !is.na(Percentage_Laid_Off)) %>%
  ggplot(aes(x = Stage_Code, y = Percentage_Laid_Off)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_smooth(method = "lm", color = "orange", se = TRUE) +
  scale_x_continuous(breaks = 1:length(stage_levels), 
                     labels = stage_levels) +
  labs(title = "Correlation: Company Stage vs Percentage Laid Off",
       subtitle = sprintf("Pearson r = %.3f, p-value = %.4f", 
                          cor_stage_pct, cor_test_2$p.value),
       x = "Company Stage",
       y = "Percentage of Workforce Laid Off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("correlation_stage_vs_percentage.png", p4, width = 10, height = 7, dpi = 300)
cat("✓ Saved: correlation_stage_vs_percentage.png\n\n")

# ================================================================================
# 2.3 CONFIDENCE INTERVALS
# ================================================================================

cat("========================================\n")
cat("95% CONFIDENCE INTERVALS BY INDUSTRY\n")
cat("========================================\n\n")

ci_table <- df %>%
  group_by(Industry) %>%
  summarise(
    n = n(),
    mean_layoffs = mean(Laid_Off_Count, na.rm = TRUE),
    sd_layoffs = sd(Laid_Off_Count, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    se = sd_layoffs / sqrt(n),
    margin_error = qt(0.975, n - 1) * se,
    lower_CI = mean_layoffs - margin_error,
    upper_CI = mean_layoffs + margin_error
  ) %>%
  arrange(desc(mean_layoffs))

print(ci_table)
cat("\n")

# Visualization: CI Plot
p5 <- ci_table %>%
  slice(1:15) %>%  # Top 15 industries
  ggplot(aes(x = reorder(Industry, mean_layoffs), y = mean_layoffs)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), 
                width = 0.2, color = "darkred") +
  coord_flip() +
  labs(title = "95% Confidence Intervals for Mean Layoffs by Industry",
       subtitle = "Error bars represent 95% CI",
       x = "Industry",
       y = "Mean Employees Laid Off") +
  theme_minimal()

ggsave("confidence_intervals_by_industry.png", p5, width = 12, height = 8, dpi = 300)
cat("✓ Saved: confidence_intervals_by_industry.png\n\n")

# ================================================================================
# PART 3: TIME SERIES FORECASTING
# ================================================================================

cat("\n========================================\n")
cat("PART 3: TIME SERIES ANALYSIS & FORECASTING\n")
cat("========================================\n\n")

# 3.1 Convert to monthly time series
df_monthly <- df %>%
  mutate(Month = as.yearmon(Layoff_Date)) %>%
  group_by(Month, Industry) %>%
  summarise(Total_Layoffs = sum(Laid_Off_Count, na.rm = TRUE),
            .groups = 'drop') %>%
  arrange(Month)

# Overall monthly trend (all industries combined)
df_monthly_total <- df %>%
  mutate(Month = as.yearmon(Layoff_Date)) %>%
  group_by(Month) %>%
  summarise(Total_Layoffs = sum(Laid_Off_Count, na.rm = TRUE),
            .groups = 'drop') %>%
  arrange(Month)

cat("Monthly Layoff Trends:\n")
print(tail(df_monthly_total, 12))
cat("\n")

# 3.2 Visualize trends by industry
p6 <- df_monthly %>%
  filter(Industry %in% top_10_industries) %>%
  ggplot(aes(x = as.Date(Month), y = Total_Layoffs, color = Industry)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(title = "Monthly Layoff Trends by Industry (Top 10)",
       x = "Month",
       y = "Total Layoffs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave("monthly_trends_by_industry.png", p6, width = 14, height = 8, dpi = 300)
cat("✓ Saved: monthly_trends_by_industry.png\n")

# 3.3 ARIMA Forecasting for Overall Layoffs
cat("\n========================================\n")
cat("ARIMA FORECASTING (OVERALL LAYOFFS)\n")
cat("========================================\n\n")

# Create time series object
ts_data <- ts(df_monthly_total$Total_Layoffs, 
              start = c(year(min(df_monthly_total$Month)), 
                        month(min(df_monthly_total$Month))),
              frequency = 12)

cat("Time Series Summary:\n")
print(summary(ts_data))
cat("\n")

# Fit ARIMA model
arima_model <- auto.arima(ts_data, 
                          stepwise = TRUE, 
                          approximation = FALSE,
                          trace = FALSE)

cat("Best ARIMA Model:\n")
print(summary(arima_model))
cat("\n")

# Forecast next 3 months
forecast_result <- forecast(arima_model, h = 3)

cat("3-Month Forecast:\n")
print(forecast_result)
cat("\n")

# Visualization: Forecast plot
p7 <- autoplot(forecast_result) +
  labs(title = "ARIMA Forecast: Total Monthly Layoffs (Next 3 Months)",
       x = "Month",
       y = "Total Layoffs") +
  theme_minimal()

ggsave("arima_forecast_overall.png", p7, width = 12, height = 7, dpi = 300)
cat("✓ Saved: arima_forecast_overall.png\n")

# 3.4 Industry-Specific Forecasting (Top 3 Industries)
cat("\n========================================\n")
cat("INDUSTRY-SPECIFIC FORECASTS\n")
cat("========================================\n\n")

forecast_industries <- industry_stats %>%
  slice(1:3) %>%
  pull(Industry)

industry_forecasts <- list()

for (ind in forecast_industries) {
  cat(sprintf("\n--- Forecasting: %s ---\n", ind))
  
  ind_data <- df_monthly %>%
    filter(Industry == ind) %>%
    arrange(Month)
  
  # Create time series
  ts_ind <- ts(ind_data$Total_Layoffs,
               start = c(year(min(ind_data$Month)), month(min(ind_data$Month))),
               frequency = 12)
  
  # Fit model
  model_ind <- auto.arima(ts_ind, trace = FALSE)
  
  # Forecast
  forecast_ind <- forecast(model_ind, h = 3)
  
  print(forecast_ind)
  
  industry_forecasts[[ind]] <- forecast_ind
}

# ================================================================================
# PART 4: FINAL INSIGHTS & INTERPRETATIONS
# ================================================================================

cat("\n\n========================================\n")
cat("FINAL INSIGHTS & INTERPRETATIONS\n")
cat("========================================\n\n")

cat("1. DESCRIPTIVE ANALYSIS:\n")
cat("   - Industries with highest total layoffs:\n")
for (i in 1:5) {
  cat(sprintf("     %d. %s: %s layoffs\n", 
              i, 
              industry_stats$Industry[i], 
              format(industry_stats$Total_Layoffs[i], big.mark = ",")))
}
cat("\n")

cat("2. HYPOTHESIS TEST (Tech vs Finance):\n")
if (t_test_result$p.value < 0.05) {
  diff <- mean(tech_layoffs, na.rm=TRUE) - mean(finance_layoffs, na.rm=TRUE)
  cat(sprintf("   - Statistically significant difference detected (p = %.4f)\n", 
              t_test_result$p.value))
  cat(sprintf("   - Tech has %.0f %s layoffs than Finance on average\n",
              abs(diff), ifelse(diff > 0, "more", "fewer")))
} else {
  cat(sprintf("   - No significant difference detected (p = %.4f)\n", 
              t_test_result$p.value))
}
cat("\n")

cat("3. CORRELATION FINDINGS:\n")
cat(sprintf("   - Funds Raised ↔ Layoffs: r = %.3f (p = %.4f)\n", 
            cor_funds_layoffs, cor_test_1$p.value))
if (abs(cor_funds_layoffs) > 0.3 & cor_test_1$p.value < 0.05) {
  cat("     → Significant correlation: Well-funded companies tend to have larger layoffs\n")
} else {
  cat("     → Weak/No significant correlation\n")
}
cat(sprintf("   - Company Stage ↔ Percentage: r = %.3f (p = %.4f)\n", 
            cor_stage_pct, cor_test_2$p.value))
if (abs(cor_stage_pct) > 0.3 & cor_test_2$p.value < 0.05) {
  cat("     → Significant correlation: Later-stage companies show different layoff patterns\n")
} else {
  cat("     → Weak/No significant correlation\n")
}
cat("\n")

cat("4. FORECASTING INSIGHTS:\n")
forecast_mean <- mean(forecast_result$mean)
recent_mean <- mean(tail(ts_data, 3))
change_pct <- ((forecast_mean - recent_mean) / recent_mean) * 100

cat(sprintf("   - Overall trend: %.1f%% %s over next 3 months\n",
            abs(change_pct), 
            ifelse(change_pct > 0, "increase", "decrease")))

if (change_pct > 10) {
  cat("   - ⚠️ WARNING: Accelerating layoff trend detected\n")
} else if (change_pct < -10) {
  cat("   - ✓ POSITIVE: Recovery trend detected\n")
} else {
  cat("   - → STABLE: Layoffs expected to remain relatively constant\n")
}
cat("\n")

cat("5. PRACTICAL RECOMMENDATIONS:\n")
cat("   For Investors:\n")
cat("   - Monitor industries with high volatility (large CI ranges)\n")
cat("   - Late-stage companies may present higher risk if correlation is positive\n\n")
cat("   For HR Teams:\n")
cat("   - Prepare contingency plans in high-risk industries\n")
cat("   - Benchmark against industry-specific CI ranges\n\n")
cat("   For Policymakers:\n")
cat("   - Focus support on industries showing accelerating trends\n")
cat("   - Consider regional impacts based on country-level statistics\n\n")

cat("========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n")
cat(sprintf("Generated: %s\n", Sys.time()))
cat("All visualizations saved to working directory.\n")
