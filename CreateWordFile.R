# Load necessary libraries
library(ggplot2)
library(dplyr)
library(officer)
library(flextable)

# Load the data
data <- read.csv("C:/Users/SERHernandez/Documents/PsycologicWellBeing/FinalProjectStatisticMasterDegree/agricultural_data.csv", sep = ";")

# Clean and prepare the data for analysis
data$Market_Value_USD_2022 <- as.numeric(gsub(",", ".", gsub("[^0-9,]", "", data$Market_Value_USD_2022)))
data$Market_Value_USD_2023 <- as.numeric(gsub(",", ".", gsub("[^0-9,]", "", data$Market_Value_USD_2023)))
data <- data %>% filter(!is.na(Market_Value_USD_2022) & !is.na(Market_Value_USD_2023))

# Create frequency table
tabla_frecuencias <- table(data$CountryGroup)
tabla_frecuencias_df <- as.data.frame(tabla_frecuencias)
colnames(tabla_frecuencias_df) <- c("CountryGroup", "Frecuencia")
write.csv(tabla_frecuencias_df, "tabla_frecuencias.csv", row.names = FALSE)

# Create and save bar plot for CountryGroup
barplot_path <- "barplot_countrygroup.png"
ggplot(data, aes(x = CountryGroup)) +
  geom_bar(fill = 'steelblue') +
  labs(title = "Distribución de la Muestra por CountryGroup", x = "CountryGroup", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(barplot_path, width = 10, height = 6)

# Create and save boxplot for Market_Value_USD_2022 by CountryGroup
boxplot_path <- "boxplot_value_2022_by_countrygroup.png"
ggplot(data, aes(x = CountryGroup, y = Market_Value_USD_2022)) +
  geom_boxplot(fill = 'steelblue') +
  labs(title = "Valor de Mercado USD 2022 por CountryGroup", x = "CountryGroup", y = "Valor de Mercado USD 2022") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(boxplot_path, width = 10, height = 6)

# Perform ANOVA
anova_result <- aov(Market_Value_USD_2022 ~ CountryGroup, data = data)
anova_summary <- summary(anova_result)

# Save ANOVA summary to text file
anova_summary_path <- "anova_summary.txt"
sink(anova_summary_path)
print(anova_summary)
sink()

# Create Word document
doc <- read_docx()

# Title and authors
doc <- doc %>%
  body_add_par("Análisis Estadístico de Datos Agrícolas", style = "heading 1") %>%
  body_add_par("Universidad:", style = "heading 2") %>%
  body_add_par("Espacio para el nombre de la universidad") %>%
  body_add_par("Título del Informe:", style = "heading 2") %>%
  body_add_par("Análisis Estadístico de Datos Agrícolas") %>%
  body_add_par("Nombres de los Autores:", style = "heading 2") %>%
  body_add_par("1. Nombre del Autor 1\n2. Nombre del Autor 2\n3. Nombre del Autor 3")

# Introduction
doc <- doc %>%
  body_add_par("1. Introducción", style = "heading 2") %>%
  body_add_par("Este informe presenta un análisis estadístico de datos agrícolas, centrándose en la distribución de la muestra por grupo de países (CountryGroup), así como el valor de mercado en USD para el año 2022 (Market_Value_USD_2022). El análisis incluye tablas de frecuencias, gráficos de barras, diagramas de caja (boxplots) y un análisis de varianza (ANOVA).")

# Structure of the Sample
doc <- doc %>%
  body_add_par("2. Estructura de la Muestra", style = "heading 2") %>%
  body_add_par("Tabla de Frecuencias", style = "heading 3") %>%
  body_add_flextable(flextable(tabla_frecuencias_df))

# Bar Plot
doc <- doc %>%
  body_add_par("Gráfico de Barras", style = "heading 3") %>%
  body_add_par("El gráfico de barras a continuación ilustra la distribución de la muestra por CountryGroup.") %>%
  body_add_img(src = barplot_path, width = 5, height = 3)

# Comparative Analysis of Means
doc <- doc %>%
  body_add_par("3. Análisis Comparativo de Medias", style = "heading 2") %>%
  body_add_par("Boxplot por Categoría", style = "heading 3") %>%
  body_add_par("El siguiente diagrama de caja compara el Market_Value_USD_2022 entre los diferentes CountryGroup.") %>%
  body_add_img(src = boxplot_path, width = 5, height = 3)

# ANOVA Analysis
doc <- doc %>%
  body_add_par("4. Análisis de Varianza (ANOVA)", style = "heading 2") %>%
  body_add_par("Resultados del ANOVA", style = "heading 3") %>%
  body_add_par("Se realizó un análisis de varianza para determinar si existen diferencias significativas en el Market_Value_USD_2022 entre los diferentes CountryGroup. Los resultados del ANOVA se resumen en la siguiente tabla:")

anova_summary_table <- data.frame(
  `Fuente de Variación` = c("CountryGroup", "Residuals"),
  `Grados de Libertad (Df)` = c(2, 12048),
  `Suma de Cuadrados (Sum Sq)` = c(8.064e+11, 2.707e+12),
  `Media de Cuadrados (Mean Sq)` = c(4.032e+11, 2.247e+08),
  `Valor F (F value)` = c(1794, ""),
  `Valor P (Pr(>F))` = c("< 2e-16 ***", "")
)

doc <- doc %>%
  body_add_flextable(flextable(anova_summary_table)) %>%
  body_add_par("\nCódigos de Significación: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")

# Interpretation of Results
doc <- doc %>%
  body_add_par("5. Interpretación de Resultados", style = "heading 2") %>%
  body_add_par("Estructura de la Muestra", style = "heading 3") %>%
  body_add_par("La mayoría de las observaciones provienen del grupo \"Central America & Caribbean\", seguido por \"Andean\" y \"Mexico\".") %>%
  body_add_par("Análisis Comparativo de Medias", style = "heading 3") %>%
  body_add_par("El diagrama de caja muestra diferencias en la dispersión y los valores centrales del Market_Value_USD_2022 entre los grupos de países. El valor de mercado en USD tiende a ser más alto en \"Mexico\" en comparación con \"Andean\" y \"Central America & Caribbean\".") %>%
  body_add_par("Resultados del ANOVA", style = "heading 3") %>%
  body_add_par("El análisis de varianza muestra que hay diferencias significativas en el Market_Value_USD_2022 entre los diferentes CountryGroup (valor p < 2e-16).")

# Conclusions
doc <- doc %>%
  body_add_par("6. Conclusiones", style = "heading 2") %>%
  body_add_par("La estructura de la muestra está dominada por el grupo \"Central America & Caribbean\". Existen diferencias significativas en el valor de mercado en USD para el año 2022 entre los diferentes grupos de países, con \"Mexico\" mostrando valores más altos en general. Los resultados del ANOVA confirman que estas diferencias son estadísticamente significativas.") %>%
  body_add_par("\nEste informe proporciona una visión clara de la estructura de la muestra y las diferencias en el valor de mercado entre los grupos de países, apoyando decisiones basadas en datos para el análisis agrícola.")

# Save the document
doc_path <- "Informe_Analisis_Estadistico.docx"
print(doc, target = doc_path)
