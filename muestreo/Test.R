library(tidyverse)

# =============================================================================
# LECTURA DE DATOS DE ARBOLITOS
# =============================================================================

arbolitos <- read.csv("muestreo/arboliños.csv", sep = " ", stringsAsFactors = FALSE)
arbolitos$Especie <- trimws(arbolitos$Especie, which = "both")

head(arbolitos)
nrow(arbolitos)

# =============================================================================
# 1. PROMEDIO DEL DIÁMETRO POR ESPECIE Y COEFICIENTE DE VARIACIÓN
# =============================================================================

resumen_diametro <- arbolitos |>
  group_by(Especie) |>
  summarise(
    n = n(),
    media_diametro = mean(Diameter, na.rm = TRUE),
    sd_diametro = sd(Diameter, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    cv_diametro = (sd_diametro / media_diametro) * 100
  )

cat("=== RESUMEN DE DIÁMETRO POR ESPECIE ===\n")
print(resumen_diametro)

# =============================================================================
# 2. PROPORCIÓN DE ÁRBOLES POR ESPECIE
# =============================================================================

proporcion_especies <- arbolitos |>
  group_by(Especie) |>
  summarise(
    n = n(),
    .groups = "drop"
  ) |>
  mutate(
    total = sum(n),
    proporcion = n / total,
    porcentaje = proporcion * 100
  ) |>
  select(-total)

cat("\n=== PROPORCIÓN DE ÁRBOLES POR ESPECIE ===\n")
print(proporcion_especies)

# =============================================================================
# 3. MUESTREO ALEATORIO SIMPLE (MAS)
# =============================================================================

set.seed(2026)
n_muestra_mas <- ceiling(0.3 * nrow(arbolitos))  # 30% de la muestra

muestra_mas <- arbolitos |>
  slice_sample(n = n_muestra_mas)

# Resumen de la muestra MAS
resumen_mas <- muestra_mas |>
  group_by(Especie) |>
  summarise(
    n = n(),
    media_diametro = mean(Diameter, na.rm = TRUE),
    sd_diametro = sd(Diameter, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    cv_diametro = (sd_diametro / media_diametro) * 100,
    proporcion = n / sum(n),
    porcentaje = proporcion * 100
  )

cat("\n=== MUESTREO ALEATORIO SIMPLE (MAS) ===\n")
cat("Tamaño de muestra:", n_muestra_mas, "\n")
print(resumen_mas)

# =============================================================================
# 4. MUESTREO BERNOULLI (POISSON SAMPLING)
# =============================================================================

p_bernoulli <- n_muestra_mas / nrow(arbolitos)

muestra_bernoulli <- arbolitos |>
  mutate(
    u = runif(n()),
    seleccionado = u < p_bernoulli
  ) |>
  filter(seleccionado) |>
  select(-u, -seleccionado)

# Resumen de la muestra Bernoulli
resumen_bernoulli <- muestra_bernoulli |>
  group_by(Especie) |>
  summarise(
    n = n(),
    media_diametro = mean(Diameter, na.rm = TRUE),
    sd_diametro = sd(Diameter, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    cv_diametro = (sd_diametro / media_diametro) * 100,
    proporcion = n / sum(n),
    porcentaje = proporcion * 100
  )

cat("\n=== MUESTREO BERNOULLI (POISSON SAMPLING) ===\n")
cat("Probabilidad de selección:", round(p_bernoulli, 4), "\n")
cat("Tamaño de muestra (esperado ~", n_muestra_mas, "):", nrow(muestra_bernoulli), "\n")
print(resumen_bernoulli)

# =============================================================================
# 5. COMPARACIÓN DE MÉTODOS: MAS vs BERNOULLI vs POBLACIÓN
# =============================================================================

comparacion <- bind_rows(
  resumen_diametro |> mutate(metodo = "Población"),
  resumen_mas |> select(Especie, media_diametro, cv_diametro) |> mutate(metodo = "MAS"),
  resumen_bernoulli |> select(Especie, media_diametro, cv_diametro) |> mutate(metodo = "Bernoulli")
) |>
  arrange(Especie, metodo)

cat("\n=== COMPARACIÓN DE MÉTODOS ===\n")
print(comparacion)
