# R/punto_3_2_cuantitativas.R
# ==============================================================================
# TALLER 1 - ESTADÍSTICA DESCRIPTIVA Y EDA
# Punto 3.2: Análisis Estadístico de Variables Cuantitativas
# Dataset: Medical Cost Personal Datasets (insurance.csv)
# Variables:
#   - age: Edad en años (Cuantitativa Discreta)
#   - children: Número de dependientes/hijos (Cuantitativa Discreta)
#   - bmi: Índice de masa corporal en kg/m^2 (Cuantitativa Continua)
#   - charges: Gastos médicos anuales en USD (Cuantitativa Continua)
# ==============================================================================

# 1. Carga de Librerías Necesarias ---------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# Función auxiliar para moda muestral
calc_mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)][1]
}

# Coeficiente de asimetría de Fisher-Pearson muestral
calc_skewness <- function(x) {
  n <- length(x)
  m <- mean(x)
  m2 <- sum((x - m)^2) / n
  m3 <- sum((x - m)^3) / n
  m3 / (m2^(1.5))
}

# Coeficiente de curtosis de exceso muestral
calc_kurtosis <- function(x) {
  n <- length(x)
  m <- mean(x)
  m2 <- sum((x - m)^2) / n
  m4 <- sum((x - m)^4) / n
  (m4 / (m2^2)) - 3
}

# 2. Carga y Preparación de Datos ----------------------------------------------
cargar_datos <- function(ruta_csv = "insurance.csv") {
  df <- read_csv(ruta_csv, show_col_types = FALSE)
  df |>
    mutate(
      age = as.integer(age),
      children = as.integer(children),
      bmi = as.numeric(bmi),
      charges = as.numeric(charges)
    )
}

# 3. Función de Resumen Estadístico Completo -----------------------------------
#' Calcula todas las medidas para variables cuantitativas requeridas en el punto 3.2:
#' - Tendencia central: media, mediana, moda
#' - Dispersión: varianza, desviación estándar, rango, coeficiente de variación (CV%)
#' - Posición: cuartiles (Q1, Q2, Q3), IQR, percentiles 10 y 90
#' - Forma y Atípicos: asimetría, curtosis, límites Tukey y conteo de atípicos
resumen_variable_cuantitativa <- function(x, nombre_var = "variable") {
  n <- length(x)
  media <- mean(x)
  mediana <- median(x)
  moda <- calc_mode(x)
  
  varianza <- var(x)
  desv_est <- sd(x)
  cv_pct <- (desv_est / media) * 100
  
  q1 <- quantile(x, 0.25, names = FALSE)
  q2 <- mediana
  q3 <- quantile(x, 0.75, names = FALSE)
  iqr <- q3 - q1
  p10 <- quantile(x, 0.10, names = FALSE)
  p90 <- quantile(x, 0.90, names = FALSE)
  
  lim_inf_tukey <- q1 - 1.5 * iqr
  lim_sup_tukey <- q3 + 1.5 * iqr
  n_outliers <- sum(x < lim_inf_tukey | x > lim_sup_tukey)
  pct_outliers <- (n_outliers / n) * 100
  
  asimetria <- calc_skewness(x)
  curtosis <- calc_kurtosis(x)
  
  tibble(
    variable = nombre_var,
    n = n,
    media = media,
    mediana = mediana,
    moda = moda,
    varianza = varianza,
    desv_est = desv_est,
    cv_pct = cv_pct,
    min = min(x),
    max = max(x),
    rango = max(x) - min(x),
    q1 = q1,
    q3 = q3,
    iqr = iqr,
    p10 = p10,
    p90 = p90,
    lim_inf_tukey = lim_inf_tukey,
    lim_sup_tukey = lim_sup_tukey,
    n_outliers = n_outliers,
    pct_outliers = pct_outliers,
    asimetria = asimetria,
    curtosis_exceso = curtosis
  )
}

# 4. Tabla Consolidada Comparativa de las 4 Variables -------------------------
calcular_tabla_resumen_cuantitativas <- function(df) {
  vars <- c("age", "children", "bmi", "charges")
  map(vars, \(v) resumen_variable_cuantitativa(df[[v]], v)) |>
    list_rbind()
}

# 5. Generación de Gráficos (Histograma + Boxplot) ----------------------------
generar_graficos_cuantitativos <- function(df) {
  # Paleta de colores
  color_age <- "#3498db"
  color_children <- "#2ecc71"
  color_bmi <- "#e67e22"
  color_charges <- "#9b59b6"
  
  # a) EDAD (age)
  p_age_hist <- ggplot(df, aes(x = age)) +
    geom_histogram(binwidth = 5, fill = color_age, color = "white", alpha = 0.85) +
    geom_vline(aes(xintercept = mean(age), color = "Media"), linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = median(age), color = "Mediana"), linetype = "solid", linewidth = 1) +
    scale_color_manual(name = "Estadístico", values = c("Media" = "#e74c3c", "Mediana" = "#2c3e50")) +
    theme_minimal(base_size = 12) +
    labs(
      title = "Distribución de Edad (age)",
      subtitle = "Histograma con media y mediana (n = 1,338)",
      x = "Edad en años cumplidos",
      y = "Frecuencia absoluta"
    )
    
  p_age_box <- ggplot(df, aes(y = age)) +
    geom_boxplot(fill = color_age, alpha = 0.5, outlier.color = "#c0392b", width = 0.4) +
    theme_minimal(base_size = 12) +
    labs(title = "Boxplot: Edad", y = "Años", x = "")
    
  # b) HIJOS / DEPENDIENTES (children)
  p_children_bar <- ggplot(df, aes(x = factor(children))) +
    geom_bar(fill = color_children, color = "white", alpha = 0.85) +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3.8) +
    theme_minimal(base_size = 12) +
    labs(
      title = "Distribución de Hijos Dependientes (children)",
      subtitle = "Frecuencia por número de hijos cubiertos en la póliza",
      x = "Número de dependientes/hijos",
      y = "Frecuencia absoluta"
    )
    
  p_children_box <- ggplot(df, aes(y = children)) +
    geom_boxplot(fill = color_children, alpha = 0.5, outlier.color = "#c0392b", width = 0.4) +
    theme_minimal(base_size = 12) +
    labs(title = "Boxplot: Dependientes", y = "Número de hijos", x = "")
    
  # c) ÍNDICE DE MASA CORPORAL (bmi)
  p_bmi_hist <- ggplot(df, aes(x = bmi)) +
    geom_histogram(binwidth = 2, fill = color_bmi, color = "white", alpha = 0.85) +
    geom_vline(aes(xintercept = mean(bmi), color = "Media"), linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = median(bmi), color = "Mediana"), linetype = "solid", linewidth = 1) +
    geom_vline(xintercept = c(18.5, 25, 30), color = "#7f8c8d", linetype = "dotted") +
    scale_color_manual(name = "Estadístico", values = c("Media" = "#e74c3c", "Mediana" = "#2c3e50")) +
    theme_minimal(base_size = 12) +
    labs(
      title = "Distribución del IMC (bmi)",
      subtitle = "Líneas punteadas: Límites clínicos OMS (Bajo peso <18.5, Normal 18.5-24.9, Sobrepeso 25-29.9, Obesidad >=30)",
      x = "IMC (kg/m²)",
      y = "Frecuencia absoluta"
    )
    
  p_bmi_box <- ggplot(df, aes(y = bmi)) +
    geom_boxplot(fill = color_bmi, alpha = 0.5, outlier.color = "#c0392b", width = 0.4) +
    theme_minimal(base_size = 12) +
    labs(title = "Boxplot: IMC (9 atípicos superiores > 47.3)", y = "kg/m²", x = "")
    
  # d) CARGOS MÉDICOS (charges)
  p_charges_hist <- ggplot(df, aes(x = charges)) +
    geom_histogram(bins = 35, fill = color_charges, color = "white", alpha = 0.85) +
    geom_vline(aes(xintercept = mean(charges), color = "Media"), linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = median(charges), color = "Mediana"), linetype = "solid", linewidth = 1) +
    scale_color_manual(name = "Estadístico", values = c("Media" = "#e74c3c", "Mediana" = "#2c3e50")) +
    theme_minimal(base_size = 12) +
    labs(
      title = "Distribución de Costos Médicos (charges)",
      subtitle = "Marcada asimetría positiva hacia la derecha; media arrastrada por costos catastróficos",
      x = "Gastos médicos anuales (USD)",
      y = "Frecuencia absoluta"
    )
    
  p_charges_box <- ggplot(df, aes(y = charges)) +
    geom_boxplot(fill = color_charges, alpha = 0.5, outlier.color = "#c0392b", width = 0.4) +
    theme_minimal(base_size = 12) +
    labs(title = "Boxplot: Costos Médicos (139 atípicos > $34,489 USD)", y = "Costos (USD)", x = "")

  list(
    age = list(hist = p_age_hist, box = p_age_box),
    children = list(bar = p_children_bar, box = p_children_box),
    bmi = list(hist = p_bmi_hist, box = p_bmi_box),
    charges = list(hist = p_charges_hist, box = p_charges_box)
  )
}
