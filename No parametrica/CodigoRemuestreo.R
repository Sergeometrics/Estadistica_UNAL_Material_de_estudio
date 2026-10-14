

setwd("C:/RAMON/MIS CARPETAS/CURSOS/I-2020/NO PARAMETRICA/Talleres R/10. BoostrapJackknifePermtaciones")

#install.packages("bootstrap")
#install.packages("tidyverse")
#install.packages("caret")
library(tidyverse)
library(caret)   
library(bootstrap)
library(nortest)

#-------------------------------------------------------------------
# 1. Usando datos simualados encuentre un IC del 95% para la mediana
# a partir de una muestra n=100
#-------------------------------------------------------------------

#----------------------
# 1.1 Datos poblacionales
#----------------------

set.seed(14)
n=100
alpha=3
beta=2
x = rgamma(n , alpha, beta)
hist(x, main=expression(Densidad~Gamma(3,2)), ylab="Densidad", freq=FALSE)
y=seq(0, 5, length=1000)
lines(y, dgamma(y, alpha, beta), col=2, lwd=2)
abline(v=median(x), lty=2, lwd=3, col=4)


muestra_med = rgamma(20000 , alpha, beta)
summary(muestra_med )
1/sqrt(3)


#--------------------------------------------
# 1.2 Bootstrap. La muestra Bootstrap se obtiene 
# usando sample(x, replace=TRUE)
#--------------------------------------------
# Número de posibles muestras $n^n$
# En este código podrían repetirse muestras
# Abajo se usa la función bootstrap



B=1000
hat_theta=median(x)
MedianaBoot=NULL
for(i in 1:B)
     {
     MedianaBoot[i]=median(sample(x, replace=TRUE))
     }
summary(MedianaBoot)
hist(MedianaBoot)
hist(MedianaBoot, main="Distribución Bootstrap", 
     ylim=c(0, 7), freq=FALSE)
lines(density(MedianaBoot, bw=0.02,kernel="gaussian"), col=2, lwd=2)
lillie.test(MedianaBoot)


#-------------------------------------------
# 1.3 Bootstrap
# sesgo, varianza, intervalos de Confianza
#-------------------------------------------

sesgo= mean(MedianaBoot)-median(x)
varianza=var(MedianaBoot)*(B-1)/B

# IC usando las cuantilas muestrales
IC=c(quantile(MedianaBoot, probs=0.025), quantile(MedianaBoot, probs=0.975))
IC

# IC asumiendo normalidad
IC2= c((median(x)-sesgo)-1.96*sqrt(var(MedianaBoot)), (median(x)-sesgo) +1.96*sqrt(var(MedianaBoot)) )
IC2


#------------------------------------------
# 2. Jackniffe
#    Coeficiente de variación de una Gamma
#-----------------------------------------

# Aprox al cv de la población
# x1=rgamma(100000, 3,2)
# sd(x1)/mean(x1)

# mean = alpha*beta
# variance = alpha*beta^2

#-------------------------------------------------
# Coeficiente de variación en muestras Jackknife
#-------------------------------------------------

?dgamma

set.seed(8)
n=1000
alpha=3
beta=2
y=rgamma(n, alpha,beta)
true_mean=alpha/beta
true_sd=sqrt(alpha/beta^2)
true_cv=true_sd/true_mean

estimated_mean=mean(y)
estimated_sd=sd(y)
estimated_cv=estimated_sd/estimated_mean

cbind(true_mean,estimated_mean)
cbind(true_sd,estimated_sd)
cbind(true_cv,estimated_cv)


cvJack=NULL
x=rgamma(n, 3,2)                                   # Muestra original
muestra=matrix(0, nrow=n, ncol=(n-1))              # Cada fila de la matriz es una muestra Jackniffe (n-1)
for (i in 1:length(x))
     {
     muestra[i,]   =x[-i]                          # x_1,\ldots, x_{i-1}, x_{i+1}, \ldots, x_{n}. Quita la i-esima observación                        
     cvJack[i]= (sd(muestra[i,]))/(mean(muestra[i,]))
     }
hist(cvJack, freq=FALSE, ylim=c(0,160), main="Distribución Jackniffe", 
     xlab="Coeficiente de Variación", ylab="Densidad")
lines(density(cvJack), col=2, lwd=2)
summary(cvJack)    

hat_theta= sd(x)/mean(x)                          # coeficiente de variación de la muestra
hat_theta_n=mean(cvJack)                          # promedio de los cv de las n muestras (cada una de ellas sin la i-esima observación) 
hat_theta_jack=(n*hat_theta)-(n-1)*hat_theta_n    # estimado jackknife de el cv
bias_jack= (n-1)*(hat_theta_n-hat_theta)          # sesgo jackknife (ver diapositiva 15)
var_hat_theta_jack= (n-1)^2*var(cvJack)/n         # varianza jackknife (ver diapositiva 23)
sqrt(var_hat_theta_jack)                          # error estándar de \hat_theta_jack 
                                                  
# intervalo de confianza

# Usando la t
lim_inf1=((hat_theta-bias_jack)-qt(0.975, 99)*sqrt(var_hat_theta_jack))
lim_sup1=((hat_theta-bias_jack)+qt(0.975, 99)*sqrt(var_hat_theta_jack))
IC1=c(lim_inf1, lim_sup1) 
IC1

#usando una normal

lim_inf2=((hat_theta-bias_jack)-qnorm(0.975, 0,1)*sqrt(var_hat_theta_jack))
lim_sup2=((hat_theta-bias_jack)+qnorm(0.975, 0,1)*sqrt(var_hat_theta_jack))
IC2=c(lim_inf2, lim_sup2) 
IC2


# Otra forma: Usando las cuantilas

q1_cv=quantile(cvJack, 0.005)
q3_cv=quantile(cvJack, 0.995)

IC3=c(q1_cv, q3_cv)
IC3

IC1
IC2
IC3


----------------------------------------------------
# Bootstrap y jackniffe usando la librería bootstrap
#---------------------------------------------------

#-------------
# Bootstrap
#-------------

theta <- function(x){median(x)}
results <- bootstrap(x,10000, theta)
hist(results$thetastar, main = "Distribución de la mediana")
sesgo= median(x)-mean(results$thetastar)
varianza=var(results$thetastar)
IC=c(quantile(results$thetastar,probs=0.025), quantile(results$thetastar,probs=0.975))
IC
IC2=c((mean(results$thetastar)-sesgo)-1.96*sqrt(var(results$thetastar)), (mean(results$thetastar)-sesgo) +1.96*sqrt(var(results$thetastar)))  
IC2

#----------------
# Jacknife
#----------------

theta  <- function(x){sd(x)/mean(x)}
results_jack <- jackknife(x,theta)     
names(results_jack)

error_estandar=results_jack[[1]]                               # error estándar de  \hat_theta_jack
sesgo=results_jack[[2]]                                        # sesgo jacknife
results_jack$jack.values-sesgo


hist(results_jack$jack.values, freq=FALSE)
ks.test(results_jack$jack.values, "pnorm")                     # No hay normalidad 
summary(results_jack$jack.values)
stem(results_jack$jack.values)

                                                               # intervalo de confianza usando normalidad
lim_inf=((hat_theta-sesgo)-1.96*error_estandar)
lim_sup=((hat_theta-sesgo)+1.96*error_estandar)
    IC2=c(lim_inf, lim_sup) 
    IC2
    


#------------------------------------------------
# Permutaciones
#------------------------------------------------

install.packages("randomizr")
library(randomizr)

#------------------------------------------------------------------------
# Prueba de hipótesis sobre valor absoluto de la diferencia de medianas
# Muestra inicial simulada de la misma distribución normal
#-----------------------------------------------------------------------

set.seed(8)
p=1000                # número de permutaciones
n=50                  # tamaño de muestra
mu1=20                # media de la muestra 1
sigma1=2              # desviación estándar de la muestra 1 
mu2=20                # media de la muestra 2
sigma2=2              # desviación estándar de la muestra 2
N=100                 # Total de datos

muestra1=rnorm(n, mu1, sigma1)
muestra2=rnorm(n, mu2, sigma2)
hist(muestra1, freq=FALSE)
hist(muestra2, freq=FALSE)
plot(density(muestra1), xlab="Datos", col=2, ylab="Densidad", main="Muestras bajo Ho")
lines(density(muestra2), lty=2, main="", col=4, xlab="Datos", ylab="Densidad")
legend("topright",  bty="n", legend=c("Muestra1", "Muestra2"), col=c(2,4), lty=c(1,2),  cex=0.8)

Tc=abs(median(muestra1)-median(muestra2))
Tc


# Distribución de T (valor absoluto de la diferencia de medianas)
# Bajo Permutaciones cuando es cierto Ho

muestras=c(muestra1, muestra2)
length(muestras)
muestra_permutada=complete_ra(N = 100, conditions = c("m1", "m2"))
is.factor(muestra_permutada)
datos=data.frame(muestras,muestra_permutada)
tapply(datos$muestras, datos$muestra_permutada, median)


T=NULL
for (i in 1:1000)
     {
      muestras=c(muestra1, muestra2)
      muestra_permutada=complete_ra(N = 100, conditions = c("m1", "m2"))
      datos=data.frame(muestras,muestra_permutada)
      medianas=tapply(datos$muestras, datos$muestra_permutada, median)
      T[i]=abs(medianas[[1]]-medianas[[2]])
      }

hist(T, freq=FALSE, main=expression(Distribución~de~abs(tilde(X)[1]-tilde(X)[2])), col=7)
abline(v=Tc, col=2, lty=2)
legend(c(0.5, 2),  bty="n", legend="T Calculado. Valor P=0.939", lty=2, col=2, cex=0.8)
valorP=sum(ifelse(T>Tc, 1,0))/1000
quantile(T, prob=0.95)
?quantile

#legend("topright",  bty="n", legend="Valor P=0.034",  col=2, cex=0.8)
    


#------------------------------------------------------------------------
# Prueba de hipótesis sobre valor absoluto de la diferencia de medianas.
# Muestra inicial simulada de distribuciones normales con
# distinta mediana.
#-----------------------------------------------------------------------

set.seed(8)
p=1000                # número de permutaciones
n=50                  # tamaño de muestra
mu1=20                # media de la muestra 1
sigma1=2              # desviación estándar de la muestra 1 
mu2=21.1              # media de la muestra 2
sigma2=2              # desviación estándar de la muestra 2
N=100                 # Total de datos

muestra1=rnorm(n, mu1, sigma1)
muestra2=rnorm(n, mu2, sigma2)
Tc=abs(median(muestra1)-median(muestra2))
plot(density(muestra1), xlab="Datos", col=2, ylab="Densidad", main="Muestras bajo Ha")
lines(density(muestra2), lty=2, main="", col=4, xlab="Datos", ylab="Densidad")
legend("topright",  bty="n", legend=c("Muestra1", "Muestra2"), col=c(2,4), lty=c(1,2),  cex=0.8)


# Distribución de T (valor absoluto de las medianas)
# Bajo Permutaciones Cuando es cierto Ho


muestras=c(muestra1, muestra2)
# ?complete_ra (ver ayuda de la función)
# implements a random assignment procedure in which fixed numbers of 
# units are assigned to treatment conditions
muestra_permutada=complete_ra(N = 100, conditions = c("m1", "m2"))
is.factor(muestra_permutada)
datos=data.frame(muestras,muestra_permutada)
tapply(datos$muestras, datos$muestra_permutada, median)


T=NULL
tsim=1000
for (i in 1:tsim)
     {
      muestras=c(muestra1, muestra2)
      muestra_permutada=complete_ra(N = 100, conditions = c("m1", "m2"))
      datos=data.frame(muestras,muestra_permutada)
      medianas=tapply(datos$muestras, datos$muestra_permutada, median)
      T[i]=abs(medianas[[1]]-medianas[[2]])
      }

hist(T, freq=FALSE, main=expression(Distribución~de~abs(tilde(X)[1]-tilde(X)[2])), col=5)
abline(v=Tc, col=2, lty=2)
legend("topleft",  bty="n", legend="T Calculado", lty=2, col=2, cex=0.8)
valorP=sum(ifelse(T>Tc, 1,0))/tsim
legend("topright",  bty="n", legend="Valor P=0.014",  col=2, cex=0.8)

quantile(T, probs=0.95)


# Ejercicio: Cociente de varianzas usando permutaciones





#-----------
# Ejercicio
#-----------

# a) Hacer el test de permutaciones con T=(mediana1-mediana2)
# Nota: Cambia la forma en que se define el valor P 
# porque la diferencia puede ser negativa. 

set.seed(123)
p=1000                # número de permutaciones
n=75                  # tamaño de muestra
mu1=15                # media de la muestra 1
sigma1=2              # desviación estándar de la muestra 1 
mu2=14.2              # media de la muestra 2
sigma2= 2             # desviación estándar de la muestra 2
N=150                 # Total de datos


muestra1=rnorm(n, mu1, sigma1)  ## muestra 1: distribución normal
muestra2=rnorm(n, mu2, sigma2)  ## muestra 2: distribución normal
Tc=(median(muestra1)-median(muestra2))  ## Estadístico de prueba
Tc
plot(density(muestra1), xlab="Datos", col=2, ylab="Densidad", main="Muestras bajo Ha")
lines(density(muestra2), lty=2, main="", col=4, xlab="Datos", ylab="Densidad")
legend("topright",  bty="n", legend=c("Muestra1", "Muestra2"), col=c(2,4), lty=c(1,2),  cex=0.8)

muestras=c(muestra1, muestra2)
muestra_permutada=complete_ra(N = 150, conditions = c("m1", "m2"))
is.factor(muestra_permutada)

datos=data.frame(muestras,muestra_permutada)
tapply(datos$muestras, datos$muestra_permutada, median)

T=NULL
tsim=1000
for (i in 1:tsim)
{
  muestras=c(muestra1, muestra2)
  muestra_permutada=complete_ra(N = 150, conditions = c("m1", "m2"))
  datos=data.frame(muestras,muestra_permutada)
  medianas=tapply(datos$muestras, datos$muestra_permutada, median)
  T[i]=medianas[[1]]-medianas[[2]]
}

hist(T, freq=FALSE, main=expression(Distribuci?n~de~tilde(X)[1]-tilde(X)[2]), col="darkblue")
abline(v=Tc, col="black", lty=2)
legend("topleft",  bty="n", legend="T Calculado", lty=2, col=2, cex=0.8)
valorP=sum(ifelse(abs(T)>Tc, 1,0))/tsim
legend("topright",  bty="n", legend="Valor P=0.008",  col=2, cex=0.8)

quantile(T, probs=c(0.025,0.95))


# b) Hacer un test de permutaciones para comparar las varianzas (T=varianza1/varianza2)

set.seed(123)
p=1000                # número de permutaciones
n=50                  # tamaño de muestra
mu1=20                # media de la muestra 1
sigma1=2              # desviación estándar de la muestra 1 
mu2=22                # media de la muestra 2
sigma2=2              # desviación estándar de la muestra 2
N=100                 # Total de datos

muestra1=rnorm(n, mu1, sigma1)
muestra2=rnorm(n, mu2, sigma2)

# Tc=(sigma1)^2/sigma2^2   ## cociente de varianzas
# Tc

# plot(density(muestra1), xlab="Datos", col=2, ylab="Densidad", main="Muestras bajo Ha",ylim=c(0,0.23))
# lines(density(muestra2), lty=2, main="", col=4, xlab="Datos", ylab="Densidad")
# legend("topright",  bty="n", legend=c("Muestra1", "Muestra2"), col=c(2,4), lty=c(1,2),  cex=0.8)


# plot(density(muestra1), xlab="Datos", col=2, ylab="Densidad", main="Muestras bajo Ha",ylim=c(0,0.23))
# lines(density(muestra2), lty=2, main="", col=4, xlab="Datos", ylab="Densidad")
#legend("topright",  bty="n", legend=c("Muestra1", "Muestra2"), col=c(2,4), lty=c(1,2),  cex=0.8)


F_original=sd(muestra1)/sd(muestra2)
F_original

# ???
F=NULL
tsim=1000
for (i in 1:tsim)
{
  muestras=c(muestra1, muestra2)
  muestra_permutada=complete_ra(N = 100, conditions = c("m1", "m2"))
  datos=data.frame(muestras,muestra_permutada)
  dest=tapply(datos$muestras, datos$muestra_permutada, sd)
  F[i]=dest[[1]]/dest[[2]]
}


hist(F, freq=FALSE, main=expression(Distribución~de~cociente), col=5)
abline(v=Tc, col=2, lty=2)
legend("topleft",  bty="n", legend="T Calculado", lty=2, col=2, cex=0.8)
valorP=sum(ifelse(T>Tc, 1,0))/tsim
legend("topright",  bty="n", legend="Valor P=0.014",  col=2, cex=0.8)





#------------------------------------------------
# Simulación de Monte Carlo
#------------------------------------------------

# Muestras de una normal

n=100
mu=10
sigma=2
S2=NULL
for (i in 1:1000)
     {
     datos=rnorm(n, mu, sigma)
     S2[i]=var(datos)
     }
summary(S2)
hist(S2, freq=FALSE, xlab=expression(S^2), main=expression(Distribución~de~S^2),ylab="Densidad", col=2)
hist((n-1)*S2/(sigma^2), freq=FALSE)
x=seq(60, 160, length=1000)
lines(x, dchisq(x, 99), col=2, lwd=2)
qchisq(0.05, 99)
qchisq(0.95, 99)

IC=matrix(0,nrow=1000, ncol=2)
IC[,1]=(99*S2)/123.22
IC[,2]=(99*S2)/77.04
sum(ifelse(IC[,1]<4&IC[,2]>4, 1,0))/1000


# Muestras de una Gamma

n=100
alpha=3
beta=2
S2=NULL
for (i in 1:1000)
     {
     datos=rgamma(n, alpha, scale=beta)
     S2[i]=var(datos)
     }
summary(S2)
hist(S2, freq=FALSE, xlab=expression(S^2), main=expression(Distribución~de~S^2),ylab="Densidad", col=2)
#hist((n-1)*S2/(alpha*beta^2), freq=FALSE)
x=seq(50, 200, length=1000)
lines(x, dchisq(x, 99), col=2, lwd=2)  # No ajusta ???

?dgamma
cuantilas=quantile(S2, c(0.025, 0.975))
IC=c(cuantilas[[1]], cuantilas[[2]])
round(IC,3)
abline(v=IC[1], col=4, lty=2, lwd=2)
abline(v=IC[2], col=4, lty=2, lwd=2)
abline(v=12, col=5, lty=2, lwd=2)
legend("topright",  bty="n", legend="IC del 95%",  col=4, lty=2, lwd=2, cex=0.8)


#---------------------------------------------------------------
# Validación Cruzada
# Tomado de 
# http://www.sthda.com/english/articles/38-regression-model-
#  validation/157-cross-validation-essentials-in-r/
#---------------------------------------------------------------

library(tidyverse)
library(caret)    

# Lectura de Datos

data("swiss")
swiss$Fertility


sample_n(swiss, 5)
dim(swiss)

# Muestra de Entrenamiento y Muestra Test
set.seed(123)
training.samples <- swiss$Fertility %>% createDataPartition(p = 0.8, list = FALSE) 
train.data  <- swiss[training.samples, ]
test.data <- swiss[-training.samples, ]

dim(train.data)
dim(test.data)


# Modelo
model <- lm(Fertility ~., data = train.data)
summary(model)
print(model)

# Predicciones. R2, RMSE y MAE
predictions <- model %>% predict(test.data)
cbind(test.data$Fertility,predictions)

data.frame(R2 = R2(predictions, test.data$Fertility),
            RMSE = RMSE(predictions, test.data$Fertility),
            MAE = MAE(predictions, test.data$Fertility))


# Validación cruzada (dejando uno afuera--- "leave-one-out"---)

# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)



# Validación cruzada dejando 10 fuera

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)


# Repeticion de Validación cruuzada K-fold(k=10)

# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
