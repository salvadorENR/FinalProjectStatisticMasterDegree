# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data
data <- read.csv("C:/Users/SERHernandez/Documents/PsycologicWellBeing/FinalProjectStatisticMasterDegree/agricultural_data.csv", sep = ";")

# Clean and prepare the data for analysis
data$Market_Value_USD_2022 <- as.numeric(gsub(",", ".", gsub("[^0-9,]", "", data$Market_Value_USD_2022)))
data$Market_Value_USD_2023 <- as.numeric(gsub(",", ".", gsub("[^0-9,]", "", data$Market_Value_USD_2023)))
data <- data %>% filter(!is.na(Market_Value_USD_2022) & !is.na(Market_Value_USD_2023))

# 1. Table of Frequencies for CountryGroup
# Create a frequency table for CountryGroup
tabla_frecuencias <- table(data$CountryGroup)
tabla_frecuencias_df <- as.data.frame(tabla_frecuencias)
colnames(tabla_frecuencias_df) <- c("CountryGroup", "Frecuencia")

# Save the frequency table to a CSV file
write.csv(tabla_frecuencias_df, "tabla_frecuencias.csv", row.names = FALSE)

# 2. Bar Plot for CountryGroup
# Create and save the bar plot for CountryGroup
barplot_path <- "barplot_countrygroup.png"
ggplot(data, aes(x = CountryGroup)) +
  geom_bar(fill = 'steelblue') +
  labs(title = "Distribución de la Muestra por CountryGroup", x = "CountryGroup", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(barplot_path, width = 10, height = 6)

# 3. Boxplot for Market_Value_USD_2022 by CountryGroup
# Create and save the boxplot for Market_Value_USD_2022 by CountryGroup
boxplot_path <- "boxplot_value_2022_by_countrygroup.png"
ggplot(data, aes(x = CountryGroup, y = Market_Value_USD_2022)) +
  geom_boxplot(fill = 'steelblue') +
  labs(title = "Valor de Mercado USD 2022 por CountryGroup", x = "CountryGroup", y = "Valor de Mercado USD 2022") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(boxplot_path, width = 10, height = 6)

# 4. ANOVA for Market_Value_USD_2022 by CountryGroup
# Perform ANOVA to determine significant differences in Market_Value_USD_2022 between CountryGroups
anova_result <- aov(Market_Value_USD_2022 ~ CountryGroup, data = data)
anova_summary <- summary(anova_result)

# Save the ANOVA summary to a text file
sink("anova_summary.txt")
print(anova_summary)
sink()
