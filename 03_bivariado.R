# ==============================================================================
# 03. ANÁLISIS BIVARIADO (CRUZADO)
# Punto 3.3 del Taller 1 - Estadística Descriptiva
# Base de datos: insurance.csv
# ==============================================================================

# Carga de datos si no están en memoria
if (!exists("datos")) {
  datos <- read.csv("insurance.csv", stringsAsFactors = TRUE)
}

# ------------------------------------------------------------------------------
# PAR 1: Cualitativa vs. Cuantitativa -> Hábito de Fumar vs. Gastos Médicos
# (smoker vs. charges)
# ------------------------------------------------------------------------------

# Resumen numérico por grupo
resumen_smoker_charges <- aggregate(charges ~ smoker, data = datos, function(x) {
  c(
    N       = length(x),
    Media   = round(mean(x), 2),
    Mediana = round(median(x), 2),
    DesvEst = round(sd(x), 2),
    Q1      = round(quantile(x, 0.25), 2),
    Q3      = round(quantile(x, 0.75), 2),
    IQR     = round(IQR(x), 2)
  )
})
print("--- Resumen de Costos Médicos por Hábito de Fumar ---")
print(resumen_smoker_charges)

# Gráfico: Boxplot comparativo con marcación de medias
boxplot(charges ~ smoker, data = datos,
        col = c("lightgreen", "tomato"),
        main = "Costos Médicos según Hábito de Fumar",
        xlab = "¿Fuma regularmente?",
        ylab = "Cargos Médicos Anuales (USD)",
        names = c("No Fumador (n=1,064)", "Fumador (n=274)"))

# Añadir punto rojo con la media aritmética para evidenciar asimetría
medias_fum <- tapply(datos$charges, datos$smoker, mean)
points(1:2, medias_fum, col = "darkred", pch = 18, cex = 1.5)
legend("topleft", legend = c("Mediana (Línea central)", "Media (Rombo)"),
       col = c("black", "darkred"), pch = c(NA, 18), lty = c(1, NA))


# ------------------------------------------------------------------------------
# PAR 2: Cuantitativa vs. Cuantitativa -> Edad vs. Gastos Médicos (age vs. charges)
# y discriminado por condición de fumador
# ------------------------------------------------------------------------------

# Coeficiente de correlación de Pearson y Spearman
cor_age_charges_p <- cor(datos$age, datos$charges, method = "pearson")
cor_age_charges_s <- cor(datos$age, datos$charges, method = "spearman")
cat("Correlación de Pearson (Edad vs Charges):", round(cor_age_charges_p, 4), "\n")
cat("Correlación de Spearman (Edad vs Charges):", round(cor_age_charges_s, 4), "\n")

# Diagrama de dispersión con líneas de tendencia
colores_puntos <- ifelse(datos$smoker == "yes", "tomato", "lightgreen")
plot(datos$age, datos$charges,
     col = adjustcolor(colores_puntos, alpha.f = 0.7),
     pch = 19,
     main = "Relación entre Edad y Costos Médicos",
     xlab = "Edad en años",
     ylab = "Costos Médicos (USD)")

# Línea de tendencia lineal global
abline(lm(charges ~ age, data = datos), col = "blue", lwd = 2)

# Líneas de tendencia por grupo
abline(lm(charges ~ age, data = subset(datos, smoker == "no")), col = "darkgreen", lwd = 2, lty = 2)
abline(lm(charges ~ age, data = subset(datos, smoker == "yes")), col = "red", lwd = 2, lty = 2)

legend("topleft",
       legend = c("Fumador (Tendencia roja)", "No fumador (Tendencia verde)", "Tendencia global (Azul)"),
       col = c("tomato", "lightgreen", "blue"),
       pch = c(19, 19, NA), lty = c(2, 2, 1), lwd = 2)


# ------------------------------------------------------------------------------
# PAR 3: Cualitativa vs. Cualitativa -> Sexo vs. Hábito de Fumar (sex vs. smoker)
# ------------------------------------------------------------------------------

# Tabla de contingencia (Frecuencias absolutas)
tab_sex_smoker <- table(datos$sex, datos$smoker)
cat("\n--- Tabla de Contingencia: Sexo vs. Fumador (Frec. Absolutas) ---\n")
print(tab_sex_smoker)

# Proporciones por fila (porcentaje de fumadores dentro de cada sexo)
prop_sex_smoker <- round(prop.table(tab_sex_smoker, margin = 1) * 100, 2)
cat("\n--- Proporciones dentro de cada género (%) ---\n")
print(prop_sex_smoker)

# Gráfico de barras apiladas al 100%
barplot(t(prop_sex_smoker),
        beside = FALSE,
        col = c("lightgreen", "tomato"),
        main = "Proporción de Fumadores según Sexo",
        xlab = "Sexo Biológico",
        ylab = "Porcentaje (%)",
        ylim = c(0, 115))
legend("topright", legend = c("No Fumador", "Fumador"),
       fill = c("lightgreen", "tomato"))

# Prueba de hipótesis de independencia Chi-cuadrado
prueba_chi <- chisq.test(tab_sex_smoker)
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
print(prueba_chi)
