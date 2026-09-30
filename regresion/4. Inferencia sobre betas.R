#---------------------------------------------------------
#      Intervalo de confianza
#---------------------------------------------------------

#-------
# Ejercicio 1. Intervalo de confianza
n = 100
beta0 = 1
beta1 = 2
sigma <- 1
e = rnorm(n, 0, sigma)
x = runif(n, 1, 5)
y = beta0 + beta1*x +e
resultados <- summary(lm(y~x))$coefficients
beta0.lim.inf <- resultados[1,1] - qt(0.975, n-2)*resultados[1,2]
beta0.lim.sup <- resultados[1,1] + qt(0.975, n-2)*resultados[1,2]
beta1.lim.inf <- resultados[2,1] - qt(0.975, n-2)*resultados[2,2]
beta1.lim.sup <- resultados[2,1] + qt(0.975, n-2)*resultados[2,2]
c(beta0.lim.inf, beta0.lim.sup)
c(beta1.lim.inf, beta1.lim.sup)
beta0.lim.sup - beta0.lim.inf
beta1.lim.sup - beta1.lim.inf

#-------
# Ejercicio 2. Probabilidad de cobertura 1000 simulaciones de 100 datos
n = 100
n.sim = 1000
beta0 = 1
beta1 = 2
sigma <- 1
beta0.cobertura <- beta1.cobertura <- rep(0, n.sim)
for(i in 1:n.sim){
  e = rnorm(n, 0, sigma)
  x = runif(n, 1, 5)
  y = beta0 + beta1*x +e
  resultados <- summary(lm(y~x))$coefficients
  beta0.lim.inf <- resultados[1,1] - qt(0.975, n-2)*resultados[1,2]
  beta0.lim.sup <- resultados[1,1] + qt(0.975, n-2)*resultados[1,2]
  beta1.lim.inf <- resultados[2,1] - qt(0.975, n-2)*resultados[2,2]
  beta1.lim.sup <- resultados[2,1] + qt(0.975, n-2)*resultados[2,2]
  if(beta0 < beta0.lim.sup & beta0 > beta0.lim.inf){
    beta0.cobertura[i] <- 1  
  }
  if(beta1 < beta1.lim.sup & beta1 > beta1.lim.inf){
    beta1.cobertura[i] <- 1  
  }
}
mean(beta0.cobertura)
mean(beta1.cobertura)

#-------
# Ejercicio 3. Probabilidad de cobertura y longitud en 1000 simulaciones de 100 datos
n = seq(5, 1000, by = 5)
n.sim = 100
beta0 = 1
beta1 = 2
sigma <- 1
cobertura.beta0 <- cobertura.beta1 <- ancho.beta0 <- ancho.beta1 <- rep(NA, length(n))
for(j in 1:length(n)){
  beta0.cobertura <- beta1.cobertura <- beta0.ancho <- beta1.ancho <- rep(0, n.sim)
  for(i in 1:n.sim){
    e = rnorm(n[j], 0, sigma)
    x = runif(n[j], 1, 5)
    y = beta0 + beta1*x +e
    resultados <- summary(lm(y~x))$coefficients
    beta0.lim.inf <- resultados[1,1] - qt(0.975, n[j]-2)*resultados[1,2]
    beta0.lim.sup <- resultados[1,1] + qt(0.975, n[j]-2)*resultados[1,2]
    beta1.lim.inf <- resultados[2,1] - qt(0.975, n[j]-2)*resultados[2,2]
    beta1.lim.sup <- resultados[2,1] + qt(0.975, n[j]-2)*resultados[2,2]
    if(beta0 < beta0.lim.sup & beta0 > beta0.lim.inf){
      beta0.cobertura[i] <- 1  
    }
    if(beta1 < beta1.lim.sup & beta1 > beta1.lim.inf){
      beta1.cobertura[i] <- 1  
    }
    beta0.ancho[i] <- beta0.lim.sup - beta0.lim.inf
    beta1.ancho[i] <- beta1.lim.sup - beta1.lim.inf
  }
  cobertura.beta0[j] <- mean(beta0.cobertura)
  cobertura.beta1[j] <- mean(beta1.cobertura)
  ancho.beta0[j] <- mean(beta0.ancho)
  ancho.beta1[j] <- mean(beta1.ancho)
}
plot(n, cobertura.beta0)
abline(h=0.95, col = "red")
plot(n, cobertura.beta1)
abline(h=0.95, col = "red")

plot(n, ancho.beta0)
plot(n, ancho.beta1)
  
  
#---------------------------------------------------------
#      Prueba de hipótesis
#---------------------------------------------------------

#-------
# Ejercicio 1. Prueba de hipótesis
n = 100
beta0 = 1
beta1 = 0
sigma <- 1
e = rnorm(n, 0, sigma)
x = runif(n, 1, 5)
y = beta0 + beta1*x +e
valor.p.beta1 <- summary(lm(y~x))$coefficients[2,4]
valor.p.beta1 < 0.05

#-------
# Ejercicio 2. Nivel de significación de la prueba de hipótesis
n = 100
n.sim = 1000
beta0 = 1
beta1 = 0
sigma <- 1
beta1.significacion <- rep(0, n.sim)
for(i in 1:n.sim){
  e = rnorm(n, 0, sigma)
  x = runif(n, 1, 5)
  y = beta0 + beta1*x +e
  valor.p.beta1 <- summary(lm(y~x))$coefficients[2,4]
  if(valor.p.beta1 < 0.05){
    beta1.significacion[i] <- 1  
  }
}
mean(beta1.significacion)

#-------
# Ejercicio 3. Nivel de significación en 1000 simulaciones de 100 datos
n = seq(5, 1000, by = 5)
n.sim = 100
beta0 = 1
beta1 = 0
sigma <- 1
significacion.beta1 <- rep(NA, length(n))
for(j in 1:length(n)){
  beta1.significacion <- rep(0, n.sim)
  for(i in 1:n.sim){
    e = rnorm(n[j], 0, sigma)
    x = runif(n[j], 1, 5)
    y = beta0 + beta1*x +e
    valor.p.beta1 <- summary(lm(y~x))$coefficients[2,4]
    if(valor.p.beta1 < 0.05){
      beta1.significacion[i] <- 1  
    }
  }
  significacion.beta1[j] <- mean(beta1.significacion)
}
plot(n, significacion.beta1)
abline(h=0.05, col = "red")
