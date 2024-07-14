# Load necessary libraries
##C:/Users/SERHernandez/Documents/PsycologicWellBeing/FinalProjectStatisticMasterDegree/agricultural_data.csv
library(dplyr)
library(ggplot2)
library(readr)

# Load the data
data <- read.csv("C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/agricultural_data.csv", sep = ";")

# Inspect the data structure
print("Estructura de los datos:")
print(str(data))

# Clean the specific columns by removing any non-numeric characters except for dots and replacing commas with dots
data <- data %>%
  mutate(across(c(Market_Volume_2022, Market_Volume_2023, Market_Value_USD_2022, Market_Value_USD_2023), 
                ~ gsub("[^0-9.,]", "", .))) %>%
  mutate(across(c(Market_Volume_2022, Market_Volume_2023, Market_Value_USD_2022, Market_Value_USD_2023), 
                ~ gsub(",", ".", .)))

# Convert columns to numeric and handle any non-numeric values
data <- data %>%
  mutate(across(c(Market_Volume_2022, Market_Volume_2023, Market_Value_USD_2022, Market_Value_USD_2023), 
                ~ as.numeric(.))) 

# Inspect the data after conversion
print("Estructura de los datos después de la conversión:")
print(str(data))

# Identify rows with NA values in the specific columns after conversion
na_rows <- data %>%
  filter(is.na(Market_Volume_2022) | is.na(Market_Volume_2023) | is.na(Market_Value_USD_2022) | is.na(Market_Value_USD_2023))

print("Filas con valores NA después de la conversión a numérico:")
print(na_rows)

# Remove rows with any NA values in the specific columns
data <- data %>%
  filter(!is.na(Market_Volume_2022) & !is.na(Market_Volume_2023) & !is.na(Market_Value_USD_2022) & !is.na(Market_Value_USD_2023))

# Save the cleaned data to a new CSV file
write_csv(data, "C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/cleaned_agricultural_data.csv")

# Summary statistics
summary_statistics <- summary(data)

# Correlation matrix
correlation_matrix <- cor(data %>% select(Market_Volume_2022, Market_Volume_2023, Market_Value_USD_2022, Market_Value_USD_2023))

# Plotting relevant data with simple design and white background
# Scatter plot for Market Volume vs. Market Value
p1 <- ggplot(data, aes(x = Market_Volume_2022, y = Market_Value_USD_2022)) +
  geom_point() +
  labs(title = "Volumen de Mercado 2022 vs Valor de Mercado USD 2022",
       x = "Volumen de Mercado 2022",
       y = "Valor de Mercado USD 2022") +
  theme_minimal()

p2 <- ggplot(data, aes(x = Market_Volume_2023, y = Market_Value_USD_2023)) +
  geom_point() +
  labs(title = "Volumen de Mercado 2023 vs Valor de Mercado USD 2023",
       x = "Volumen de Mercado 2023",
       y = "Valor de Mercado USD 2023") +
  theme_minimal()

# Simple boxplots with white background
p3 <- ggplot(data, aes(x = "", y = Market_Volume_2022)) +
  geom_boxplot() +
  labs(title = "Caja y Bigotes del Volumen de Mercado 2022",
       y = "Volumen de Mercado 2022") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))

p4 <- ggplot(data, aes(x = "", y = Market_Volume_2023)) +
  geom_boxplot() +
  labs(title = "Caja y Bigotes del Volumen de Mercado 2023",
       y = "Volumen de Mercado 2023") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))

p5 <- ggplot(data, aes(x = "", y = Market_Value_USD_2022)) +
  geom_boxplot() +
  labs(title = "Caja y Bigotes del Valor de Mercado USD 2022",
       y = "Valor de Mercado USD 2022") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))

p6 <- ggplot(data, aes(x = "", y = Market_Value_USD_2023)) +
  geom_boxplot() +
  labs(title = "Caja y Bigotes del Valor de Mercado USD 2023",
       y = "Valor de Mercado USD 2023") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))

# Histograms corresponding to each boxplot with white bars and black edges
h1 <- ggplot(data, aes(x = Market_Volume_2022)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  labs(title = "Histograma del Volumen de Mercado 2022",
       x = "Volumen de Mercado 2022",
       y = "Frecuencia") +
  theme_minimal()

h2 <- ggplot(data, aes(x = Market_Volume_2023)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  labs(title = "Histograma del Volumen de Mercado 2023",
       x = "Volumen de Mercado 2023",
       y = "Frecuencia") +
  theme_minimal()

h3 <- ggplot(data, aes(x = Market_Value_USD_2022)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  labs(title = "Histograma del Valor de Mercado USD 2022",
       x = "Valor de Mercado USD 2022",
       y = "Frecuencia") +
  theme_minimal()

h4 <- ggplot(data, aes(x = Market_Value_USD_2023)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  labs(title = "Histograma del Valor de Mercado USD 2023",
       x = "Valor de Mercado USD 2023",
       y = "Frecuencia") +
  theme_minimal()

# Save the plots
ggsave("C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/scatter_plot_2022.png", p1)
ggsave("C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/scatter_plot_2023.png", p2)
ggsave("C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/boxplot_volume_2022.png", p3)
ggsave("C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/boxplot_volume_2023.png", p4)
ggsave("C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/boxplot_value_2022.png", p5)
ggsave("C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/boxplot_value_2023.png", p6)

ggsave("C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/histogram_volume_2022.png", h1)
ggsave("C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/histogram_volume_2023.png", h2)
ggsave("C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/histogram_value_2022.png", h3)
ggsave("C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/histogram_value_2023.png", h4)

# Print the summary statistics and correlation matrix
print(summary_statistics)
print(correlation_matrix)

# Save the summary statistics and correlation matrix to a text file
sink("C:/Users/salva/Documents/FinalProjectStatisticMasterDegree/analysis_results.txt")
print("Estadísticas Resumidas:")
print(summary_statistics)
print("Matriz de Correlación:")
print(correlation_matrix)
sink()
