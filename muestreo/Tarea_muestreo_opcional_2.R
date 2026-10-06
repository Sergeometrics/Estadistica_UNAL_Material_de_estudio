x <- data.frame(
  x = 1:10,
  Pi_k = rep(1/10, 10)
)

x$lim_inf <- c(0, cumsum(x$Pi_k[-nrow(x)]))
x$lim_sup <- cumsum(x$Pi_k)


m <- 5
u <- runif(m)

intervalo <- findInterval(u, c(0, x$lim_sup), rightmost.closed = TRUE, all.inside = TRUE)

seleccion <- x$x[intervalo]

muestra <- unique(seleccion)
muestra
n <- length(muestra)


