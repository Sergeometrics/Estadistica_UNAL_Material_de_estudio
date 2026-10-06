# ============================================================
# Método acumulativo total (muestreo con probabilidad
# proporcional al tamaño, PPS)
# ============================================================

metodo_acumulativo_total <- function(datos, tamano, n, reemplazo = TRUE, semilla = NULL) {
  # datos    : data.frame con las unidades de la población
  # tamano   : nombre de la columna con la medida de tamaño (Xi)
  # n        : tamaño de la muestra
  # reemplazo: TRUE = con reemplazo (clásico); FALSE = sin reemplazo
  # semilla  : semilla opcional para reproducibilidad
  if (!is.null(semilla)) set.seed(semilla)

  X <- datos[[tamano]]
  N <- nrow(datos)
  if (any(X <= 0)) stop("Todos los tamaños deben ser positivos.")
  if (!reemplazo && n > N) stop("n no puede exceder N sin reemplazo.")

  # Tabla de totales acumulados
  tabla <- data.frame(
    unidad  = seq_len(N),
    tamano  = X,
    lim_inf = c(0, head(cumsum(X), -1)) + 1,
    lim_sup = cumsum(X)
  )
  total <- sum(X)

  seleccionadas <- integer(0)
  while (length(seleccionadas) < n) {
    r <- sample.int(total, 1)                             # número aleatorio entre 1 y Total
    u <- which(r >= tabla$lim_inf & r <= tabla$lim_sup)   # unidad cuyo intervalo contiene r
    if (!reemplazo && u %in% seleccionadas) next          # descarta repetidas
    seleccionadas <- c(seleccionadas, u)
  }

  muestra <- datos[seleccionadas, , drop = FALSE]
  muestra$prob_seleccion <- X[seleccionadas] / total

  list(
    tabla_acumulada = tabla,
    total           = total,
    unidades        = seleccionadas,
    muestra         = muestra
  )
}

# ------------------------------------------------------------
# Ejemplo de uso
# ------------------------------------------------------------
poblacion <- data.frame(
  id        = paste0("U", 1:8),
  empleados = c(12, 30, 5, 48, 20, 9, 35, 41)
)

res <- metodo_acumulativo_total(poblacion, "empleados", n = 3,
                                reemplazo = FALSE, semilla = 123)

print(res$tabla_acumulada)   # intervalos acumulados
print(res$unidades)          # unidades seleccionadas
print(res$muestra)           # muestra con su probabilidad de selección
-