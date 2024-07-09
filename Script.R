# Load necessary libraries
##C:/Users/SERHernandez/Documents/PsycologicWellBeing/FinalProjectStatisticMasterDegree/agricultural_data.csv

#"C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/set00.csv"
#C:/Users/SERHernandez/Documents/PsycologicWellBeing/FinalProjectStatisticMasterDegree/set00.csv"
# Load the necessary libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr) # Make sure tidyr is loaded for drop_na

# Load the data
data <- fread("C:/Users/SERHernandez/Documents/PsycologicWellBeing/FinalProjectStatisticMasterDegree/set00.csv", sep = ";", stringsAsFactors = FALSE)

# Print column names to check the actual names
print(names(data))

# Replace spaces with underscores in column names
setnames(data, old = names(data), new = gsub(" ", "_", names(data)))

# Data Cleaning and Preparation

# Function to convert numeric columns
clean_numeric <- function(column) {
  as.numeric(gsub("[^0-9.-]", "", column))
}

# Ensure columns exist before applying transformations
data <- data %>%
  mutate(
    Market_Volume_2022 = clean_numeric(MarketVolume2022),
    Market_Volume_2023 = clean_numeric(MarketVolume2023),
    Market_Value_USD_2022 = clean_numeric(MarketValueDollar2022),
    Market_Value_USD_2023 = clean_numeric(MarketValueDollar2023)
  )

# Handling missing values by removing rows with NA in key columns
data <- data %>%
  drop_na(Market_Volume_2022, Market_Volume_2023, Market_Value_USD_2022, Market_Value_USD_2023)

# Descriptive Statistics
summary_stats <- data %>%
  summarise(across(c(Market_Volume_2022, Market_Volume_2023, Market_Value_USD_2022, Market_Value_USD_2023), 
                   list(mean = mean, median = median, sd = sd), na.rm = TRUE))

print(summary_stats)

# Visualization of distributions
ggplot(data, aes(x = Market_Volume_2022)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.7) +
  ggtitle("Distribution of Market Volume 2022") +
  xlab("Market Volume 2022") + ylab("Frequency")

ggplot(data, aes(x = Market_Value_USD_2022)) +
  geom_histogram(binwidth = 500, fill = "green", alpha = 0.7) +
  ggtitle("Distribution of Market Value (USD) 2022") +
  xlab("Market Value (USD) 2022") + ylab("Frequency")

# Year-over-Year Comparison
data <- data %>%
  mutate(Volume_Change = Market_Volume_2023 - Market_Volume_2022,
         Value_Change = Market_Value_USD_2023 - Market_Value_USD_2022)

year_over_year_summary <- data %>%
  summarise(across(c(Volume_Change, Value_Change), list(mean = mean, median = median, sd = sd), na.rm = TRUE))

print(year_over_year_summary)

# Top Performing Crops and Products
top_crops <- data %>%
  group_by(Crop) %>%
  summarise(Total_Value_2023 = sum(Market_Value_USD_2023, na.rm = TRUE)) %>%
  arrange(desc(Total_Value_2023)) %>%
  head(10)

print(top_crops)

# Regional Insights
regional_insights <- data %>%
  group_by(CountryGroup) %>%
  summarise(Total_Value_2023 = sum(Market_Value_USD_2023, na.rm = TRUE),
            Total_Volume_2023 = sum(Market_Volume_2023, na.rm = TRUE))

print(regional_insights)

# Product Line Trends
product_line_trends <- data %>%
  group_by(ProductLine) %>%
  summarise(Total_Value_2023 = sum(Market_Value_USD_2023, na.rm = TRUE),
            Total_Volume_2023 = sum(Market_Volume_2023, na.rm = TRUE))

print(product_line_trends)

# Common Name Impact
common_name_impact <- data %>%
  group_by(Commonname) %>%
  summarise(Total_Value_2023 = sum(Market_Value_USD_2023, na.rm = TRUE),
            Total_Volume_2023 = sum(Market_Volume_2023, na.rm = TRUE))

print(common_name_impact)

# Strategic Crop Analysis
strategic_crop_analysis <- data %>%
  group_by(StrategicCrop) %>%
  summarise(Total_Value_2023 = sum(Market_Value_USD_2023, na.rm = TRUE),
            Total_Volume_2023 = sum(Market_Volume_2023, na.rm = TRUE))

print(strategic_crop_analysis)

# Visualizations for top performing crops and products
ggplot(top_crops, aes(x = reorder(Crop, Total_Value_2023), y = Total_Value_2023)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.7) +
  coord_flip() +
  ggtitle("Top Performing Crops in 2023") +
  xlab("Crop") + ylab("Total Market Value (USD) 2023")

# Save the summary statistics and other results to CSV files
write.csv(summary_stats, "C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/summary_stats.csv")
write.csv(year_over_year_summary, "C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/year_over_year_summary.csv")
write.csv(top_crops, "C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/top_crops.csv")
write.csv(regional_insights, "C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/regional_insights.csv")
write.csv(product_line_trends, "C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/product_line_trends.csv")
write.csv(common_name_impact, "C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/common_name_impact.csv")
write.csv(strategic_crop_analysis, "C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/strategic_crop_analysis.csv")
