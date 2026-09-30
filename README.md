# Estadística Aplicada con R - Universidad Nacional de Colombia

## 📚 Descripción

Repositorio de materiales, ejercicios y prácticas del curso **Estadística Avanzada** impartido en la **Universidad Nacional de Colombia (UNAL)**. Contiene implementaciones en **R** de conceptos fundamentales en estadística, cubriendo tanto métodos paramétricos como no paramétricos, análisis multivariado, teoría de muestreo y regresión lineal.

Este repositorio está diseñado como material de apoyo para estudiantes de pregrado y posgrado en estadística, matemáticas e ingeniería.

---

## 🎯 Contenido Principal

El repositorio está organizado en **4 módulos temáticos principales**:

### 1. **📊 Análisis de Regresión** (`regresion/`)

**Objetivo**: Dominar la modelación lineal múltiple, inferencia sobre parámetros y diagnósticos de regresión.

**Tópicos cubiertos:**
- Regresión lineal simple y múltiple
- Estimación insesgada de parámetros (β₀, β₁, σ²)
- Varianza de estimadores
- Intervalos de confianza y pruebas de hipótesis
- Simulación de cobertura de intervalos
- Regresión con variables categóricas

**Archivos clave:**
- `1. Mazda_descriptivo.R` - Análisis exploratorio con dataset Mazda 2021
- `3. Insesgamiento varianza.R` - Estudio de sesgos e varianza
- `4. Inferencia sobre betas.R` - Pruebas e intervalos de confianza
- `Tarea_pero_Decente.qmd` - Simulación sobre estimadores insesgados

**Datasets incluidos:**
- `mazda2021.xlsx`
- `Taller1_ceosal1.xls`
- `Taller1_vote1.xls`

---

### 2. **🎲 Teoría de Muestreo** (`muestreo/`)

**Objetivo**: Comprender diseños de muestreo, probabilidades de inclusión y propiedades de estimadores.

**Tópicos cubiertos:**
- Muestreo Aleatorio Simple (MAS)
- Probabilidades de inclusión de primer orden (πᵢ) y segundo orden (πᵢⱼ)
- Estimadores de Horvitz-Thompson (HT)
- Varianza y sesgo de estimadores
- Error Cuadrático Medio (MSE)
- Coeficiente de Variación (CV)
- Efecto del Diseño (Deff)

**Archivos clave:**
- `cap2.R` - Muestreo aleatorio simple: enumeración de muestras e inclusion probabilities
- `exercise_2_1.R` - Ejercicios sobre muestreo secuencial
- `probability_matrices.R` - Cálculos de matrices de probabilidad

**Datasets incluidos:**
- `arboliños.csv` y `arboliños.xlsx`

---

### 3. **📈 Análisis Multivariado** (`Multivariada/`)

**Objetivo**: Reducción dimensional, agrupamiento y visualización de datos complejos.

**Tópicos cubiertos:**
- **Análisis de Componentes Principales (PCA/ACP)**
  - Eigenvalores y eigenvectores
  - Contribuciones de variables
  - Métricas de calidad (cos²)
- **Análisis de Correspondencias (ACS)**
  - Tablas de contingencia
  - Perfiles de fila y columna
  - Interpretación de dimensiones
- **Análisis de Agrupamientos (Clustering)**
  - K-means
  - Clustering jerárquico
  - Índice de Calinski-Harabasz
  - Análisis de siluetas
  - HCPC (Clustering jerárquico basado en PCA)

**Archivos clave:**
- `LABORATORIO_3.Rmd` - PCA aplicado a infraestructura urbana
- `LABORATORIO_4.Rmd` - Análisis de correspondencias
- `LABORATORIO_5.Rmd` - Clustering y agrupamientos

**Datasets incluidos:**
- `Ciudades.xlsx`
- `r14_Sci_Qs_Webometrics.csv`
- `ECC_completa_19426.csv` (70.9MB)

---

### 4. **📉 Estadística No Paramétrica** (`No parametrica/`)

**Objetivo**: Pruebas de hipótesis sin supuestos distribucionales restrictivos.

**Tópicos cubiertos:**
- Prueba de Wilcoxon (signed-rank test)
- Prueba U de Mann-Whitney (Wilcoxon rank-sum test)
- Distribuciones teóricas: Normal, Exponencial, Weibull, Gamma, Beta, Uniforme, Triangular
- Comparación: métodos paramétricos vs no paramétricos
- ANOVA y comparaciones post-hoc (Tukey HSD)
- Pruebas de asimetría y simetría

**Archivos clave:**
- `Wilcoxon_y_MannWhitney_Version4.R` - Implementación de pruebas no paramétricas
- `Dev.r` - Demostraciones de distribuciones y pruebas

**Datasets incluidos:**
- `Goats.csv`

---

## 🛠️ Requisitos Técnicos

### Versión de R
- **R ≥ 4.0**

### Librerías principales

```r
# Instalación de todas las librerías necesarias
packages <- c(
  # Data manipulation
  "tidyverse", "dplyr", "readxl", "haven",
  
  # Visualization
  "ggplot2", "ggridges", "factoextra",
  
  # Multivariate analysis
  "FactoMineR",
  
  # Clustering
  "fpc", "NbClust",
  
  # Sampling & combinatorics
  "combinat",
  
  # Statistical tests
  "nortest",
  
  # Document generation
  "rmarkdown", "quarto"
)

install.packages(packages)
```

---

## 📖 Cómo usar este repositorio

### Opción 1: Ejecutar en RStudio
1. Clonar o descargar el repositorio
2. Abrir `Repo.Rproj` en RStudio
3. Navegar a la carpeta de interés y abrir los archivos `.R` o `.Rmd`
4. Ejecutar el código con `Ctrl+Enter` (o `Cmd+Enter` en Mac)

### Opción 2: Ejecutar scripts individuales
```r
# Ejemplo: cargar un script de regresión
source("regresion/1. Mazda_descriptivo.R")
```

### Opción 3: Renderizar documentos Quarto
```r
quarto::quarto_render("regresion/Tarea_pero_Decente.qmd")
```

---

## 📊 Estructura de directorios

```
Repo/
├── README.md                          # Este archivo
├── index.html                         # Página índice interactiva
├── Repo.Rproj                         # Configuración del proyecto R
│
├── regresion/                         # Módulo de Regresión
│   ├── 1. Mazda_descriptivo.R
│   ├── 2. Mazda_X_categorica.R
│   ├── 3. Insesgamiento varianza.R
│   ├── 4. Inferencia sobre betas.R
│   ├── Tarea_pero_Decente.qmd
│   ├── tarea.R
│   └── datasets/
│       ├── mazda2021.xlsx
│       ├── Taller1_ceosal1.xls
│       └── Taller1_vote1.xls
│
├── muestreo/                          # Módulo de Teoría de Muestreo
│   ├── cap2.R
│   ├── exercise_2_1.R
│   ├── probability_matrices.R
│   ├── Test.R
│   └── datasets/
│       ├── arboliños.csv
│       └── arboliños.xlsx
│
├── Multivariada/                      # Módulo de Análisis Multivariado
│   ├── LABORATORIO_3.Rmd
│   ├── LABORATORIO_4.Rmd
│   ├── LABORATORIO_5.Rmd
│   ├── idk.R
│   └── datasets/
│       ├── Ciudades.xlsx
│       ├── r14_Sci_Qs_Webometrics.csv
│       └── ECC_completa_19426.csv
│
└── No parametrica/                    # Módulo de Estadística No Paramétrica
    ├── Wilcoxon_y_MannWhitney_Version4.R
    ├── Dev.r
    └── datasets/
        └── Goats.csv
```

---

## 🚀 Ejemplos rápidos

### Ejemplo 1: Regresión lineal con Mazda
```r
source("regresion/1. Mazda_descriptivo.R")
# Genera análisis descriptivo y modelo de regresión
```

### Ejemplo 2: Muestreo aleatorio simple
```r
source("muestreo/cap2.R")
# Demuestra probabilidades de inclusión y estimadores HT
```

### Ejemplo 3: PCA con ciudades
```r
rmarkdown::render("Multivariada/LABORATORIO_3.Rmd")
# Genera reporte HTML con análisis de componentes principales
```

### Ejemplo 4: Pruebas no paramétricas
```r
source("No parametrica/Wilcoxon_y_MannWhitney_Version4.R")
# Compara pruebas de Wilcoxon y Mann-Whitney
```

---

## 📚 Conceptos estadísticos clave

### Regresión
- **Estimadores insesgados**: Propiedades de β₀, β₁, σ²
- **Inferencia**: Intervalos de confianza, p-valores, pruebas t
- **Validación**: Simulación de cobertura de intervalos

### Muestreo
- **Diseño**: Probabilidades de inclusión πᵢ y πᵢⱼ
- **Estimadores**: Horvitz-Thompson (HT), propiedades estadísticas
- **Eficiencia**: MSE, CV, Deff

### Multivariado
- **PCA**: Reducción dimensional, interpretación de varianza explicada
- **Correspondencias**: Análisis de tablas de contingencia
- **Clustering**: Agrupamientos jerárquicos y K-means

### No Paramétrico
- **Pruebas de rango**: Wilcoxon, Mann-Whitney sin supuestos distribucionales
- **Robustez**: Alternativas a t-test y ANOVA

---

## 🔍 Conjuntos de datos

| Dataset | Tamaño | Descripción | Módulo |
|---------|--------|-------------|--------|
| `mazda2021.xlsx` | - | Características de vehículos Mazda 2021 | Regresión |
| `arboliños.csv` | - | Datos de árboles (inventario forestal) | Muestreo |
| `Ciudades.xlsx` | - | Infraestructura urbana (RH, INFRA) | Multivariado |
| `Goats.csv` | - | Datos sobre cabras (no paramétrico) | No Paramétrico |
| `r14_Sci_Qs_Webometrics.csv` | - | Rankings de universidades | Multivariado |
| `ECC_completa_19426.csv` | 70.9 MB | Dataset grande (correspondencias) | Multivariado |

---

## 💡 Consejos de aprendizaje

1. **Comenzar por Regresión**: Fundamenta el método del análisis estadístico
2. **Luego Muestreo**: Entiende la variabilidad en estimadores
3. **Multivariado**: Aplica técnicas de reducción dimensional
4. **Finalmente No Paramétrico**: Aprende métodos robustos sin supuestos

Cada módulo incluye:
- ✅ Explicación teórica
- ✅ Código comentado
- ✅ Ejemplos con datos reales
- ✅ Simulaciones para validar propiedades

---

## 🤝 Contribuciones

Este repositorio es material educativo de la **Universidad Nacional de Colombia**. 

Para reportar errores, sugerencias o mejoras, por favor abrir un issue o contactar con el instructor del curso.

---

## 📝 Licencia

Materiales educativos de uso académico. Consultar con UNAL para política de distribución.

---

## 📧 Contacto

- **Universidad**: Universidad Nacional de Colombia
- **Programa**: Estadística Aplicada
- **Año**: 2026

---

**Última actualización**: Septiembre 30, 2026  
**Versión de R**: 4.6.1  
**IDE recomendado**: RStudio / Positron
