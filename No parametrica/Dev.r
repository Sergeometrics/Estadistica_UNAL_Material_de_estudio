









set.seed(10)
m1<-rnorm(40,10,2)
m2<-rnorm(37,10,2)
m3<-rnorm(42,11,5)
t.test(m1,m2,alternative = "two.sided", conflevel = 0.9)

#para  ANOVA
datos <- data.frame(valor = c(m1, m2, m3), grupo = factor(rep(c("m1", "m2", "m3"),  times = c(length(m1), length(m2), length(m3)))))


modelo <- aov(valor ~ grupo, data = datos)
summary(modelo)

#Comparacion 1 a 1
TukeyHSD(modelo)

#lsd
pairwise.t.test(datos$valor, datos$grupo, p.adjust.method = "none")

#clase 3 creo
1- pbinom(6,10,0.5)

1- pbinom(7,10,0.5)

1- pbinom(8,10,0.5)

# ============================================================
# Distribuciones: Weibull, Gamma, Beta, Uniforme, Triangular
# ============================================================

set.seed(42)
par(mfrow = c(5, 2),            # 5 filas, 2 columnas: PDF | Histograma
    mar = c(4, 4, 2, 1),
    oma = c(0, 0, 2, 0))         # margen exterior para el título
col_curve <- "#1f77b4"
col_hist  <- "#1f77b4"

# ---------- 1. WEIBULL ----------
shape_w <- 2; scale_w <- 1.5
x_w <- seq(0, 6, length.out = 400)
y_w <- dweibull(x_w, shape = shape_w, scale = scale_w)
samp_w <- rweibull(5000, shape = shape_w, scale = scale_w)

plot(x_w, y_w, type = "l", lwd = 2, col = col_curve,
     main = sprintf("Weibull  (shape=%.1f, scale=%.1f)", shape_w, scale_w),
     xlab = "x", ylab = "f(x)")
curve(dweibull(x, shape_w, scale_w), add = TRUE, lwd = 2, col = col_curve)
hist(samp_w, breaks = 40, freq = FALSE, col = adjustcolor(col_hist, 0.6),
     border = "white", main = "Weibull – muestra (n=5000)",
     xlab = "x", ylab = "densidad")
curve(dweibull(x, shape_w, scale_w), add = TRUE, lwd = 2, col = col_curve)

# ---------- 2. GAMMA ----------
shape_g <- 3; rate_g <- 1.2
x_g <- seq(0, 10, length.out = 400)
y_g <- dgamma(x_g, shape = shape_g, rate = rate_g)
samp_g <- rgamma(5000, shape = shape_g, rate = rate_g)

plot(x_g, y_g, type = "l", lwd = 2, col = col_curve,
     main = sprintf("Gamma  (shape=%.1f, rate=%.1f)", shape_g, rate_g),
     xlab = "x", ylab = "f(x)")
hist(samp_g, breaks = 40, freq = FALSE, col = adjustcolor(col_hist, 0.6),
     border = "white", main = "Gamma – muestra (n=5000)",
     xlab = "x", ylab = "densidad")
curve(dgamma(x, shape_g, rate_g), add = TRUE, lwd = 2, col = col_curve)

# ---------- 3. BETA ----------
a_b <- 2; b_b <- 5
x_b <- seq(0, 1, length.out = 400)
y_b <- dbeta(x_b, a_b, b_b)
samp_b <- rbeta(5000, a_b, b_b)

plot(x_b, y_b, type = "l", lwd = 2, col = col_curve,
     main = sprintf("Beta  (shape1=%.1f, shape2=%.1f)", a_b, b_b),
     xlab = "x", ylab = "f(x)")
hist(samp_b, breaks = 40, freq = FALSE, col = adjustcolor(col_hist, 0.6),
     border = "white", main = "Beta – muestra (n=5000)",
     xlab = "x", ylab = "densidad")
curve(dbeta(x, a_b, b_b), add = TRUE, lwd = 2, col = col_curve)

# ---------- 4. UNIFORME ----------
min_u <- 0; max_u <- 1
x_u <- seq(min_u, max_u, length.out = 400)
y_u <- dunif(x_u, min_u, max_u)
samp_u <- runif(5000, min_u, max_u)

plot(x_u, y_u, type = "l", lwd = 2, col = col_curve,
     main = sprintf("Uniforme  (min=%.1f, max=%.1f)", min_u, max_u),
     xlab = "x", ylab = "f(x)")
hist(samp_u, breaks = 40, freq = FALSE, col = adjustcolor(col_hist, 0.6),
     border = "white", main = "Uniforme – muestra (n=5000)",
     xlab = "x", ylab = "densidad")
curve(dunif(x, min_u, max_u), add = TRUE, lwd = 2, col = col_curve)

# ---------- 5. TRIANGULAR (simétrica) ----------
# triangular simétrica: left = a, mode = c, right = b  con c = (a+b)/2
a_t <- 0; b_t <- 6; c_t <- (a_t + b_t) / 2
# función de densidad triangular
dtri  <- function(x, a, c, b) ifelse(x < a | x > b, 0,
                                ifelse(x <= c, 2*(x-a)/((b-a)*(c-a)),
                                              2*(b-x)/((b-a)*(b-c))))
x_t <- seq(a_t, b_t, length.out = 400)
y_t <- dtri(x_t, a_t, c_t, b_t)
# muestreo: inversión de la CDF
rtri <- function(n, a, c, b) {
  u <- runif(n)
  # CDF inversa por tramos
  ifelse(u < (c-a)/(b-a),
         a + sqrt(u * (b-a)*(c-a)),
         b - sqrt((1-u) * (b-a)*(b-c)))
}
samp_t <- rtri(5000, a_t, c_t, b_t)

plot(x_t, y_t, type = "l", lwd = 2, col = col_curve,
     main = sprintf("Triangular  (a=%.1f, c=%.1f, b=%.1f)  [simétrica]", a_t, c_t, b_t),
     xlab = "x", ylab = "f(x)")
hist(samp_t, breaks = 40, freq = FALSE, col = adjustcolor(col_hist, 0.6),
     border = "white", main = "Triangular – muestra (n=5000)",
     xlab = "x", ylab = "densidad")
curve(dtri(x, a_t, c_t, b_t), add = TRUE, lwd = 2, col = col_curve)

mtext("Distribuciones comunes – PDF teórica y muestra", outer = TRUE, cex = 1.4, font = 2)



# ============================================================
# Test de simetría para una muestra
# ============================================================

simetria_test <- function(x, B = 5000, conf = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  
  cat("========================================\n")
  cat(sprintf("  Test de simetría  (n = %d)\n", n))
  cat("========================================\n\n")
  
  # -------- 1) Coeficiente de asimetría (skewness) --------
  m  <- mean(x)
  s  <- sd(x)
  m3 <- mean((x - m)^3)
  g1 <- m3 / s^3                          # skewness de Fisher
  se_g1 <- sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))
  z_g1  <- g1 / se_g1
  p_g1  <- 2 * pnorm(-abs(z_g1))          # prueba bilateral H0: g1 = 0
  
  cat("1) SKEWNESS (asimetría de Fisher)\n")
  cat(sprintf("   g1 = %.4f   |   SE = %.4f   |   z = %.3f   |   p = %.4f\n\n",
              g1, se_g1, z_g1, p_g1))
  
  # -------- 2) Wilcoxon signed-rank (H0: mediana = 0) --------
  #    Si la distribución es simétrica alrededor de la mediana,
  #    los rangos con signo deberían equilibrarse.
  xc <- x - median(x)                      # centrar en la mediana
  w  <- suppressWarnings(wilcox.test(xc, mu = 0, exact = FALSE))
  
  cat("2) WILCOXON SIGNED-RANK (centrado en la mediana)\n")
  cat(sprintf("   W = %.0f   |   p = %.4f\n\n", w$statistic, w$p.value))
  
  # -------- 3) Bootstrap: compara con su distribución espejo --------
  #    Estadístico: S = sum |xi - mediana|
  #    H0: x y su espejo (2*mediana - x) vienen de la misma dist.
  centro <- median(x)
  S_obs  <- sum(abs(x - centro))
  S_boot <- replicate(B, {
    # mezclar los signos preservando las magnitudes
    xs <- sample(c(-1, 1), n, replace = TRUE) * abs(x - centro)
    sum(abs(xs))
  })
  p_boot <- mean(S_boot >= S_obs)          # unilateral derecha
  
  cat(sprintf("3) BOOTSTRAP ESPEJO  (B = %d)\n", B))
  cat(sprintf("   S_obs = %.3f   |   p = %.4f\n\n", S_obs, p_boot))
  
  # -------- Veredicto --------
  alpha <- 1 - conf
  signif <- ifelse(p_g1 < alpha | p_boot < alpha, "NO simétrica", "simétrica")
  cat(sprintf(">> Conclusión al %.0f%%: la muestra parece %s.\n",
              conf*100, signif))
  
  invisible(list(skewness = g1, p_skewness = p_g1,
                 wilcoxon  = w$p.value,
                 p_bootstrap = p_boot))
}

# ============================================================
# Ejemplos
# ============================================================
set.seed(42)

# 1) Muestra claramente asimétrica (exponencial)
x_asim <- rexp(800, rate = 1)
simetria_test(x_asim)

# 2) Muestra simétrica (normal)
x_sim <- rnorm(800)
simetria_test(x_sim)

# 3) Tu propio vector:

gamma<-rgamma(100,100,1)
 simetria_test(gamma)
plot(density(gamma))


library(tidyverse)

set.seed(2026)

df <- tibble(
  fecha = seq.Date(from = as.Date("2025-01-01"), by = "day", length.out = 60),
  region = sample(c("Norte", "Sur", "Centro", "Este", "Oeste"), 60, replace = TRUE),
  categoria = sample(c("Tecnología", "Hogar", "Salud", "Moda", "Deportes"), 60, replace = TRUE),
  edad = sample(18:68, 60, replace = TRUE),
  ingreso_mensual = round(rnorm(60, mean = 3200, sd = 900), 0),
  compras_mes = sample(0:9, 60, replace = TRUE),
  gasto_total = round(ingreso_mensual * (0.08 + compras_mes * 0.045) + rnorm(60, mean = 0, sd = 180), 0),
  cliente_frecuente = if_else(compras_mes >= 5 & region %in% c("Norte", "Centro"), TRUE, FALSE),
  score_satisfaccion = sample(1:5, 60, replace = TRUE),
  canal = sample(c("Web", "App", "Tiendas", "WhatsApp"), 60, replace = TRUE)
)

df
set.seed(8)
x<-rnorm(100,10,2)
y<-rnorm(100,10,2)
 
shapiro.test(x)
shapiro.test(y)

minx<-numeric(1000)
miny<-numeric(1000)
for(i in 1:1000){
  set.seed(i)
  x<-rnorm(100,10,2)
  y<-rnorm(100,10,2)
  minx[i]<-min(x)
  miny[i]<-min(y)
}
hist(minx)
hist(miny)

shapiro.test(minx)
shapiro.test(miny)

####------------------------------Ejercicio 2-------------------------------####
# 1. Crear el dataframe en formato ancho (tal como está en la imagen)
df_ancho <- data.frame(
  Estanque_1 = c(7.68, 7.69, 7.70, 7.70, 7.72, 7.73, 7.73, 7.76),
  Estanque_2 = c(7.71, 7.73, 7.74, 7.74, 7.78, 7.78, 7.80, 7.81),
  Estanque_3 = c(7.74, 7.75, 7.77, 7.78, 7.80, 7.81, 7.84, 7.85),
  Estanque_4 = c(7.71, 7.71, 7.74, 7.79, 7.81, 7.85, 7.87, 7.91)
)

# 2. Convertir a formato largo (óptimo para realizar las pruebas estadísticas)
df_largo <- data.frame(
  pH = c(df_ancho$Estanque_1, df_ancho$Estanque_2, df_ancho$Estanque_3, df_ancho$Estanque_4),
  Estanque = factor(rep(c("Estanque 1", "Estanque 2", "Estanque 3", "Estanque 4"), each = 8))
)

# Ver el dataframe estructurado
print(df_largo)

# Prueba de normalidad para cada estanque de forma individual
shapiro_resultados <- tapply(df_largo$pH, df_largo$Estanque, shapiro.test)

# Mostrar los resultados detallados
print(shapiro_resultados)

kruskal.test(df_largo$pH,df_largo$Estanque)

# ANOVA (one-way) and Tukey HSD for df_largo
modelo_aov_largo <- aov(pH ~ Estanque, data = df_largo)
summary(modelo_aov_largo)

# Post-hoc pairwise comparisons (Tukey)
tukey_largo <- TukeyHSD(modelo_aov_largo)
tukey_largo

# Pairwise t-tests with Holm adjustment
pairwise.t.test(df_largo$pH, df_largo$Estanque, p.adjust.method = "holm")



####------------------------------Ejercicio 1-------------------------------####

# 1. Crear el dataframe en formato ancho (tal como está en la imagen)
df_ancho2 <- data.frame(
  Hierba   = c(180, 173, 175, 182, 181),
  Arbustos = c(172, 158, 167, 160, 175),
  Arbol    = c(163, 170, 158, 162, 170)
)

# 2. Convertir a formato largo (óptimo para el análisis)
df_largo2 <- data.frame(
  Abundancia = c(df_ancho2$Hierba, df_ancho2$Arbustos, df_ancho2$Arbol),
  Tratamiento = factor(rep(c("Hierba", "Arbustos", "Arbol"), each = 5))
)

# Ver el dataframe estructurado
print(df_largo2)

# Realizar el test de Kruskal-Wallis
resultado_kruskal2 <- kruskal.test(Abundancia ~ Tratamiento, data = df_largo2)

# Mostrar el resultado en consola
print(resultado_kruskal2)
