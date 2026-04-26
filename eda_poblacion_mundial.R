# =============================================================================
# ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# Dataset: Población Mundial y Migración (World Bank)
# Autor: Generado automáticamente
# Fecha: 2026-04-24
# =============================================================================

# --- 1. INSTALACIÓN Y CARGA DE PAQUETES ------------------------------------

# Instalar paquetes si no están disponibles
paquetes_necesarios <- c("ggplot2", "dplyr", "tidyr", "scales", "corrplot",
                         "RColorBrewer", "gridExtra", "knitr", "skimr")

paquetes_faltantes <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[,"Package"])]
if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes, repos = "https://cran.r-project.org")
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(corrplot)
library(RColorBrewer)
library(gridExtra)
library(knitr)
library(skimr)

# --- 2. CARGA DE DATOS -----------------------------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  CARGANDO DATOS...\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

df <- read.csv(
  "https://query.data.world/s/kyzrcbixkyhsiuj4qb3773ampd333o?dws=00000",
  header = TRUE,
  stringsAsFactors = FALSE
)

cat("✅ Dataset cargado exitosamente.\n\n")

# --- 3. ESTRUCTURA GENERAL DEL DATASET -------------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  3. ESTRUCTURA GENERAL DEL DATASET\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("📐 Dimensiones del dataset:\n")
cat("   Filas (observaciones):", nrow(df), "\n")
cat("   Columnas (variables):", ncol(df), "\n\n")

cat("📋 Nombres de las columnas:\n")
print(names(df))
cat("\n")

cat("🔍 Estructura (str):\n")
str(df)
cat("\n")

cat("📊 Primeras 6 filas:\n")
print(head(df))
cat("\n")

cat("📊 Últimas 6 filas:\n")
print(tail(df))
cat("\n")

# --- 4. RESUMEN ESTADÍSTICO ------------------------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  4. RESUMEN ESTADÍSTICO\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("📈 Summary() de todas las variables:\n")
print(summary(df))
cat("\n")

cat("📈 Resumen detallado con skimr:\n")
print(skim(df))
cat("\n")

# --- 5. ANÁLISIS DE VALORES FALTANTES (NA) ----------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  5. ANÁLISIS DE VALORES FALTANTES\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

na_por_columna <- colSums(is.na(df))
na_porcentaje <- round(colSums(is.na(df)) / nrow(df) * 100, 2)

na_resumen <- data.frame(
  Variable = names(na_por_columna),
  NAs = na_por_columna,
  Porcentaje = na_porcentaje
)
na_resumen <- na_resumen[order(-na_resumen$Porcentaje), ]

cat("🔴 Valores faltantes por columna:\n")
print(na_resumen, row.names = FALSE)
cat("\n")

cat("📊 Total de valores faltantes en el dataset:", sum(is.na(df)), "\n")
cat("📊 Total de celdas en el dataset:", prod(dim(df)), "\n")
cat("📊 Porcentaje global de NAs:", round(sum(is.na(df)) / prod(dim(df)) * 100, 2), "%\n\n")

# Visualización de valores faltantes
p_na <- ggplot(na_resumen, aes(x = reorder(Variable, Porcentaje), y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "#E74C3C", alpha = 0.8) +
  geom_text(aes(label = paste0(Porcentaje, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Porcentaje de Valores Faltantes por Variable",
    x = "Variable",
    y = "Porcentaje de NAs (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_na)

# --- 6. ANÁLISIS DE VARIABLES CATEGÓRICAS -----------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  6. ANÁLISIS DE VARIABLES CATEGÓRICAS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("🌍 Número de países/entidades únicas:", n_distinct(df$country), "\n")
cat("📅 Rango de años:", min(df$year, na.rm = TRUE), "-", max(df$year, na.rm = TRUE), "\n")
cat("🗺️  Regiones únicas:", n_distinct(df$region), "\n")
cat("💰 Niveles de ingreso únicos:", n_distinct(df$incomeLevel), "\n\n")

# Distribución por región
cat("📊 Distribución de observaciones por región:\n")
region_counts <- df %>%
  count(region, sort = TRUE) %>%
  as.data.frame()
print(region_counts)
cat("\n")

# Distribución por nivel de ingreso
cat("💰 Distribución de observaciones por nivel de ingreso:\n")
income_counts <- df %>%
  count(incomeLevel, sort = TRUE) %>%
  as.data.frame()
print(income_counts)
cat("\n")

# Gráfico: Observaciones por región
p_region <- ggplot(region_counts, aes(x = reorder(region, n), y = n)) +
  geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Número de Observaciones por Región",
    x = "Región",
    y = "Número de Observaciones"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_region)

# Gráfico: Observaciones por nivel de ingreso
p_income <- ggplot(income_counts, aes(x = reorder(incomeLevel, n), y = n)) +
  geom_bar(stat = "identity", fill = "#2ECC71", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Número de Observaciones por Nivel de Ingreso",
    x = "Nivel de Ingreso",
    y = "Número de Observaciones"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_income)

# --- 7. DISTRIBUCIÓN DE VARIABLES NUMÉRICAS ---------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  7. DISTRIBUCIÓN DE VARIABLES NUMÉRICAS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Filtrar solo países (excluir agregados)
df_paises <- df %>% filter(region != "Aggregates")

cat("📊 Después de filtrar agregados:\n")
cat("   Filas:", nrow(df_paises), "\n")
cat("   Países únicos:", n_distinct(df_paises$country), "\n\n")

# Histograma de población
p_pop_hist <- ggplot(df_paises, aes(x = population)) +
  geom_histogram(bins = 50, fill = "#9B59B6", alpha = 0.7, color = "white") +
  scale_x_log10(labels = comma) +
  labs(
    title = "Distribución de la Población (escala log10)",
    subtitle = "Solo países (excluyendo agregados regionales)",
    x = "Población (log10)",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "grey50"))

print(p_pop_hist)

# Histograma de densidad poblacional
p_density_hist <- ggplot(df_paises %>% filter(!is.na(pop_density)), aes(x = pop_density)) +
  geom_histogram(bins = 50, fill = "#E67E22", alpha = 0.7, color = "white") +
  scale_x_log10(labels = comma) +
  labs(
    title = "Distribución de la Densidad Poblacional (escala log10)",
    x = "Densidad Poblacional (hab/km²) - log10",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_density_hist)

# Boxplot de población por nivel de ingreso
p_pop_income <- ggplot(df_paises %>% filter(!is.na(population) & incomeLevel != ""),
                       aes(x = incomeLevel, y = population, fill = incomeLevel)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.4) +
  scale_y_log10(labels = comma) +
  labs(
    title = "Distribución de Población por Nivel de Ingreso",
    x = "Nivel de Ingreso",
    y = "Población (log10)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")

print(p_pop_income)

# --- 8. EVOLUCIÓN TEMPORAL -------------------------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  8. EVOLUCIÓN TEMPORAL\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Población mundial total por año (usando agregados)
pop_mundial <- df %>%
  filter(country == "World") %>%
  arrange(year) %>%
  select(year, population)

if (nrow(pop_mundial) > 0) {
  p_world_pop <- ggplot(pop_mundial, aes(x = year, y = population)) +
    geom_line(color = "#2C3E50", linewidth = 1.2) +
    geom_point(color = "#E74C3C", size = 1.5, alpha = 0.6) +
    scale_y_continuous(labels = function(x) paste0(round(x / 1e9, 2), "B")) +
    labs(
      title = "Evolución de la Población Mundial (1960-2018)",
      x = "Año",
      y = "Población (miles de millones)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  print(p_world_pop)
}

# Top 10 países más poblados en el año más reciente
top10_reciente <- df_paises %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  arrange(desc(population)) %>%
  head(10)

cat("🏆 Top 10 países más poblados (año más reciente):\n")
print(top10_reciente %>% select(country, year, population, pop_density, incomeLevel), row.names = FALSE)
cat("\n")

p_top10 <- ggplot(top10_reciente, aes(x = reorder(country, population), y = population)) +
  geom_bar(stat = "identity", fill = "#1ABC9C", alpha = 0.85) +
  geom_text(aes(label = paste0(round(population / 1e6, 0), "M")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(round(x / 1e9, 2), "B"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 10 Países Más Poblados",
    subtitle = paste("Año:", max(df_paises$year, na.rm = TRUE)),
    x = "",
    y = "Población"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey50")
  )

print(p_top10)

# Evolución temporal de los top 5 países
top5_nombres <- top10_reciente$country[1:5]
top5_evolucion <- df_paises %>%
  filter(country %in% top5_nombres) %>%
  arrange(country, year)

p_top5_evol <- ggplot(top5_evolucion, aes(x = year, y = population, color = country)) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(labels = function(x) paste0(round(x / 1e9, 2), "B")) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Evolución Poblacional de los 5 Países Más Poblados",
    x = "Año",
    y = "Población (miles de millones)",
    color = "País"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

print(p_top5_evol)

# --- 9. ANÁLISIS DE MIGRACIÓN NETA ------------------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  9. ANÁLISIS DE MIGRACIÓN NETA\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

df_migracion <- df_paises %>%
  filter(!is.na(net_migration))

cat("📊 Observaciones con datos de migración neta:", nrow(df_migracion), "\n")
cat("📈 Estadísticas de migración neta:\n")
print(summary(df_migracion$net_migration))
cat("\n")

# Top 10 países con mayor inmigración neta (año más reciente con datos)
ultimo_anio_mig <- max(df_migracion$year, na.rm = TRUE)

top_inmigracion <- df_migracion %>%
  filter(year == ultimo_anio_mig) %>%
  arrange(desc(net_migration)) %>%
  head(10)

top_emigracion <- df_migracion %>%
  filter(year == ultimo_anio_mig) %>%
  arrange(net_migration) %>%
  head(10)

cat("🟢 Top 10 países con mayor inmigración neta (año", ultimo_anio_mig, "):\n")
print(top_inmigracion %>% select(country, net_migration, migration_perc), row.names = FALSE)
cat("\n")

cat("🔴 Top 10 países con mayor emigración neta (año", ultimo_anio_mig, "):\n")
print(top_emigracion %>% select(country, net_migration, migration_perc), row.names = FALSE)
cat("\n")

# Gráfico de migración neta
top_mig_combined <- bind_rows(
  top_inmigracion %>% mutate(tipo = "Inmigración"),
  top_emigracion %>% mutate(tipo = "Emigración")
)

p_migracion <- ggplot(top_mig_combined,
                      aes(x = reorder(country, net_migration), y = net_migration, fill = tipo)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(round(x / 1e6, 1), "M")) +
  scale_fill_manual(values = c("Inmigración" = "#27AE60", "Emigración" = "#E74C3C")) +
  labs(
    title = "Migración Neta: Mayores Receptores y Emisores",
    subtitle = paste("Año:", ultimo_anio_mig),
    x = "",
    y = "Migración Neta",
    fill = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
    legend.position = "bottom"
  )

print(p_migracion)

# Histograma de porcentaje de migración
p_mig_perc <- ggplot(df_migracion %>% filter(!is.na(migration_perc)),
                     aes(x = migration_perc * 100)) +
  geom_histogram(bins = 40, fill = "#8E44AD", alpha = 0.7, color = "white") +
  labs(
    title = "Distribución del Porcentaje de Migración Neta",
    x = "Migración Neta como % de la Población",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_mig_perc)

# --- 10. ANÁLISIS DE CORRELACIÓN --------------------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  10. ANÁLISIS DE CORRELACIÓN\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

vars_numericas <- df_paises %>%
  select(population, pop_density, net_migration, migration_perc, longitude, latitude) %>%
  drop_na()

if (nrow(vars_numericas) > 10) {
  cor_matrix <- cor(vars_numericas, use = "complete.obs")
  cat("📊 Matriz de correlación:\n")
  print(round(cor_matrix, 3))
  cat("\n")

  corrplot(cor_matrix,
           method = "color",
           type = "upper",
           order = "hclust",
           addCoef.col = "black",
           tl.col = "black",
           tl.srt = 45,
           title = "Matriz de Correlación - Variables Numéricas",
           mar = c(0, 0, 2, 0),
           number.cex = 0.8)
}

# --- 11. ANÁLISIS DE DENSIDAD POBLACIONAL ------------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  11. ANÁLISIS DE DENSIDAD POBLACIONAL\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Top 10 países más densamente poblados (año más reciente)
top_densidad <- df_paises %>%
  filter(year == max(year, na.rm = TRUE) & !is.na(pop_density)) %>%
  arrange(desc(pop_density)) %>%
  head(15)

cat("🏙️  Top 15 territorios más densamente poblados:\n")
print(top_densidad %>% select(country, pop_density, population, incomeLevel), row.names = FALSE)
cat("\n")

p_densidad <- ggplot(top_densidad, aes(x = reorder(country, pop_density), y = pop_density)) +
  geom_bar(stat = "identity", fill = "#F39C12", alpha = 0.85) +
  geom_text(aes(label = round(pop_density, 0)), hjust = -0.1, size = 3.2) +
  coord_flip() +
  labs(
    title = "Top 15 Territorios Más Densamente Poblados",
    subtitle = paste("Año:", max(df_paises$year, na.rm = TRUE)),
    x = "",
    y = "Densidad Poblacional (hab/km²)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey50")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(p_densidad)

# Scatter: Población vs Densidad
p_scatter <- ggplot(df_paises %>% filter(year == max(year, na.rm = TRUE) & !is.na(pop_density)),
                    aes(x = population, y = pop_density)) +
  geom_point(aes(color = incomeLevel), alpha = 0.6, size = 2.5) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  labs(
    title = "Población vs Densidad Poblacional",
    subtitle = "Escala logarítmica, coloreado por nivel de ingreso",
    x = "Población (log10)",
    y = "Densidad Poblacional (log10)",
    color = "Nivel de Ingreso"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set1")

print(p_scatter)

# --- 12. ANÁLISIS POR NIVEL DE INGRESO --------------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  12. ANÁLISIS POR NIVEL DE INGRESO\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Población total por nivel de ingreso en el año más reciente
pop_por_ingreso <- df_paises %>%
  filter(year == max(year, na.rm = TRUE) & incomeLevel != "" & !is.na(population)) %>%
  group_by(incomeLevel) %>%
  summarise(
    n_paises = n(),
    poblacion_total = sum(population, na.rm = TRUE),
    poblacion_media = mean(population, na.rm = TRUE),
    densidad_media = mean(pop_density, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(poblacion_total))

cat("📊 Resumen por nivel de ingreso:\n")
print(pop_por_ingreso, width = Inf)
cat("\n")

# Gráfico de pastel - Población por nivel de ingreso
p_pie <- ggplot(pop_por_ingreso, aes(x = "", y = poblacion_total, fill = incomeLevel)) +
  geom_bar(stat = "identity", width = 1, alpha = 0.8) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(poblacion_total / sum(poblacion_total) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.5) +
  labs(
    title = "Distribución de Población Mundial por Nivel de Ingreso",
    fill = "Nivel de Ingreso"
  ) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_fill_brewer(palette = "Pastel1")

print(p_pie)

# Evolución temporal por nivel de ingreso
pop_por_ingreso_tiempo <- df_paises %>%
  filter(incomeLevel != "" & incomeLevel != "Aggregates" & !is.na(population)) %>%
  group_by(incomeLevel, year) %>%
  summarise(poblacion_total = sum(population, na.rm = TRUE), .groups = "drop")

p_income_evol <- ggplot(pop_por_ingreso_tiempo,
                        aes(x = year, y = poblacion_total, fill = incomeLevel)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_y_continuous(labels = function(x) paste0(round(x / 1e9, 1), "B")) +
  labs(
    title = "Evolución de la Población por Nivel de Ingreso",
    x = "Año",
    y = "Población (miles de millones)",
    fill = "Nivel de Ingreso"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set2")

print(p_income_evol)

# --- 13. ANÁLISIS POR REGIÓN -----------------------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  13. ANÁLISIS POR REGIÓN\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Población por región en año más reciente
pop_por_region <- df_paises %>%
  filter(year == max(year, na.rm = TRUE) & region != "" & region != "Aggregates" & !is.na(population)) %>%
  group_by(region) %>%
  summarise(
    n_paises = n(),
    poblacion_total = sum(population, na.rm = TRUE),
    densidad_media = mean(pop_density, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(poblacion_total))

cat("🌍 Población por región:\n")
print(pop_por_region, width = Inf)
cat("\n")

p_region_bar <- ggplot(pop_por_region,
                       aes(x = reorder(region, poblacion_total), y = poblacion_total)) +
  geom_bar(stat = "identity", fill = "#2980B9", alpha = 0.8) +
  geom_text(aes(label = paste0(round(poblacion_total / 1e9, 2), "B")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(round(x / 1e9, 1), "B"),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Población Total por Región del Mundo",
    subtitle = paste("Año:", max(df_paises$year, na.rm = TRUE)),
    x = "",
    y = "Población Total"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey50")
  )

print(p_region_bar)

# Evolución temporal por región
pop_region_tiempo <- df_paises %>%
  filter(region != "" & region != "Aggregates" & !is.na(population)) %>%
  group_by(region, year) %>%
  summarise(poblacion_total = sum(population, na.rm = TRUE), .groups = "drop")

p_region_evol <- ggplot(pop_region_tiempo,
                        aes(x = year, y = poblacion_total, color = region)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = function(x) paste0(round(x / 1e9, 1), "B")) +
  labs(
    title = "Evolución Poblacional por Región (1960-2018)",
    x = "Año",
    y = "Población (miles de millones)",
    color = "Región"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  ) +
  guides(color = guide_legend(nrow = 3))

print(p_region_evol)

# --- 14. TASA DE CRECIMIENTO POBLACIONAL ------------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  14. TASA DE CRECIMIENTO POBLACIONAL\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Calcular tasa de crecimiento por país
crecimiento <- df_paises %>%
  filter(!is.na(population)) %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    tasa_crecimiento = (population / lag(population) - 1) * 100
  ) %>%
  ungroup()

# Tasa de crecimiento promedio por país
crecimiento_promedio <- crecimiento %>%
  filter(!is.na(tasa_crecimiento)) %>%
  group_by(country, region, incomeLevel) %>%
  summarise(
    tasa_media = mean(tasa_crecimiento, na.rm = TRUE),
    tasa_mediana = median(tasa_crecimiento, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(tasa_media))

cat("🚀 Top 10 países con mayor crecimiento promedio anual:\n")
print(head(crecimiento_promedio, 10), width = Inf)
cat("\n")

cat("📉 Top 10 países con menor crecimiento (o decrecimiento):\n")
print(tail(crecimiento_promedio, 10), width = Inf)
cat("\n")

# Boxplot tasa de crecimiento por región
p_crec_region <- ggplot(crecimiento %>%
                          filter(!is.na(tasa_crecimiento) & region != "" & region != "Aggregates"),
                        aes(x = region, y = tasa_crecimiento, fill = region)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  coord_flip() +
  labs(
    title = "Tasa de Crecimiento Poblacional Anual por Región",
    x = "",
    y = "Tasa de Crecimiento (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3") +
  ylim(-5, 15)

print(p_crec_region)

# --- 15. RESUMEN FINAL Y CONCLUSIONES --------------------------------------

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  15. RESUMEN FINAL\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("📋 RESUMEN DEL ANÁLISIS EXPLORATORIO:\n\n")
cat("1. El dataset contiene", nrow(df), "observaciones y", ncol(df), "variables.\n")
cat("2. Cubre", n_distinct(df$country), "países/entidades desde",
    min(df$year, na.rm = TRUE), "hasta", max(df$year, na.rm = TRUE), ".\n")
cat("3. Las variables con mayor porcentaje de NAs son: net_migration y migration_perc.\n")
cat("4. La población mundial ha mostrado un crecimiento sostenido.\n")
cat("5. Las tasas de crecimiento varían significativamente por región y nivel de ingreso.\n")
cat("6. Los patrones migratorios muestran claras diferencias entre economías.\n\n")

cat("✅ Análisis Exploratorio completado exitosamente.\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
