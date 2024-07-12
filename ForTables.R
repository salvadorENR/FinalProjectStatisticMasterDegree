# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(grid)

# Summary statistics data
summary_data <- data.frame(
  Variable = c('Volumen de Mercado 2022', 'Volumen de Mercado 2023', 'Valor de Mercado USD 2022', 'Valor de Mercado USD 2023'),
  Minimo = c(0.000, 0.000, 0.00, 0.000),
  `1er Cuartil` = c(0.170, 0.200, 3.56, 3.485),
  Mediana = c(1.150, 1.130, 19.20, 19.100),
  Media = c(9.639, 9.618, 94.19, 90.486),
  `3er Cuartil` = c(5.950, 5.915, 96.05, 92.610),
  Maximo = c(930.760, 750.460, 999.90, 996.510)
)

# Correlation matrix data with labels in Spanish
correlation_data <- matrix(c(
  1.00, 0.95, 0.58, 0.49,
  0.95, 1.00, 0.57, 0.49,
  0.58, 0.57, 1.00, 0.86,
  0.49, 0.49, 0.86, 1.00
), nrow = 4, byrow = TRUE)
colnames(correlation_data) <- c('Volumen Mercado 2022', 'Volumen Mercado 2023', 'Valor Mercado USD 2022', 'Valor Mercado USD 2023')
rownames(correlation_data) <- c('Volumen Mercado 2022', 'Volumen Mercado 2023', 'Valor Mercado USD 2022', 'Valor Mercado USD 2023')

# Convert to dataframe for ggplot
correlation_df <- as.data.frame(correlation_data)
correlation_df <- round(correlation_df, 2)

# Function to create table plot
create_table_plot <- function(data, title) {
  grid.table(data, rows = NULL)
  grid.text(title, x = 0.5, y = 0.9, gp = gpar(fontsize = 20, fontface = "bold"))
}

# Save summary statistics table as image
png("summary_statistics_table.png", width = 800, height = 400)
create_table_plot(summary_data, "Estadísticas Resumidas")
dev.off()

# Save correlation matrix table as image
png("correlation_matrix_table.png", width = 800, height = 400)
create_table_plot(correlation_df, "Matriz de Correlación")
dev.off()

