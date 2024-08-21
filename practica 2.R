# Librerías necesarias
library(quantmod)
library(tseries)
library(FinTS)

######################################################################################################
############################################## EJERCICIO 1 ###########################################
######################################################################################################


# Ejercicio: Descargar datos del S&P 500
# --------------------------------------
getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = "2021-12-31", periodicity = "daily")
SP500 <- GSPC$GSPC.Close

# Verificar los datos descargados
print(head(SP500))
print(tail(SP500))
print(sum(is.na(SP500))) # Contar valores NA
  
# Eliminar valores nulos
SP500 <- na.omit(SP500)

# Ejercicio: Calcular rendimientos
# --------------------------------
# Calcular rendimientos (diferencia de logaritmos)
yield <- diff(log(SP500))
yield <- yield * 100

# Verificar los rendimientos calculados
print(head(yield))
print(tail(yield))
print(sum(is.na(yield))) # Contar valores NA

# Eliminar valores nulos resultantes
yield <- na.omit(yield)

# Ejercicio: Calcular estadísticas descriptivas
# ---------------------------------------------
# Funciones para asimetría y curtosis
kurtosis <- function(yield) {
  m4 <- mean((yield - mean(yield))^4)
  kurt <- m4 / (sd(yield)^4) - 3
  return(kurt)
}

skewness <- function(yield) {
  m3 <- mean((yield - mean(yield))^3)
  skew <- m3 / (sd(yield)^3)
  return(skew)
}

# Estadísticos descriptivos
media <- mean(yield)
mediana <- median(yield)
desv <- sd(yield)
maximo <- max(yield)
minimo <- min(yield)
asimetria <- skewness(yield)
curtosis <- kurtosis(yield)

# Mostrar resultados
resultados <- list(
  media = media,
  mediana = mediana,
  desviacion = desv,
  maximo = maximo,
  minimo = minimo,
  asimetria = asimetria,
  curtosis = curtosis
)

print(resultados)


######################################################################################################
############################################## EJERCICIO 2 ###########################################
######################################################################################################

# Ejercicio: Representación gráfica
# ---------------------------------
# Representación gráfica de la evolución del índice SP500 y sus rendimientos
plot(SP500, main = "Evolución del índice SP500", col = "blue")
plot(yield, main = "Rendimientos del índice SP500", col = "blue")

# Ejercicio: Análisis de la función de correlación simple (ACF)
# -------------------------------------------------------------
# Calcular la función de correlación simple (acf) de los rendimientos
acf(yield, lag.max = 15)

# Ejercicio: Test de Ljung-Box para 1, 10 y 20 retardos
# -----------------------------------------------------
lags <- c(1, 10, 20)
for (lag in lags) {
  lb_test <- Box.test(yield, lag = lag, type = "Ljung-Box")
  cat(paste("Test de Ljung-Box para", lag, "retardos:\n"))
  print(lb_test)
}



######################################################################################################
############################################## EJERCICIO 3 ###########################################
######################################################################################################


# Calcular rendimientos al cuadrado
yield_squared <- yield^2

# Calcular la función de correlación simple (acf) de los rendimientos al cuadrado
acf_squared <- acf(yield_squared, lag.max = 15)

# Mostrar resultados
cat("ACF de los rendimientos al cuadrado:\n")
print(acf_squared)

# Comparación con los resultados del apartado (b) del ejercicio anterior
cat("\nComparación con los resultados del ejercicio anterior (ACF de los rendimientos):\n")
acf_yield <- acf(yield, lag.max = 15, plot = FALSE)
print(acf_yield)


######################################################################################################
############################################## EJERCICIO 4 ###########################################
######################################################################################################

# Cargar librerías necesarias
library(quantmod)
library(tseries)

# Función para calcular el test de sesgo de signo y tamaño
sesgo_signo <- function(yield) {
  # Calculamos los residuos estandarizados
  residuos <- yield
  z <- residuos / sd(residuos)
  
  # Calculamos z al cuadrado
  z_square <- z^2
  
  # Variables dummy I_negative y I_positive
  I_negative <- ifelse(residuos < 0, 1, 0)
  I_positive <- ifelse(residuos > 0, 1, 0)
  
  # Calculamos el sesgo de signo y tamaño
  negative_bias <- I_negative * residuos
  positive_bias <- I_positive * residuos
  
  # Preparamos los datos para la regresión
  data <- data.frame(z_square, I_negative, negative_bias, I_positive, positive_bias)
  
  # Eliminamos la primera fila porque I_negative e I_positive tienen un valor inicial de 0
  data <- data[-1, ]
  
  # Estimación del modelo de regresión lineal MCO
  modelo <- lm(z_square ~ I_negative + negative_bias + I_positive + positive_bias, data = data)
  
  return(modelo)
}

# Descargar datos del S&P 500
getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = "2021-12-31", periodicity = "daily")
SP500 <- GSPC$GSPC.Close

# Calcular rendimientos
yield <- diff(log(SP500)) * 100
yield <- na.omit(yield)

# Estimar el modelo de sesgo de signo y tamaño
modelo <- sesgo_signo(yield)

# Mostrar resultados del modelo
summary(modelo)

# Test F para la significatividad conjunta de los coeficientes
anova(modelo)
