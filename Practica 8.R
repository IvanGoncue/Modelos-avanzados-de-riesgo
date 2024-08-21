# Instalar y cargar las librerías necesarias
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("evd", quietly = TRUE)) install.packages("evd")
if (!requireNamespace("fitdistrplus", quietly = TRUE)) install.packages("fitdistrplus")

library(quantmod)
library(evd)
library(fitdistrplus)

# Cargar los datos del índice HSI
getSymbols("^HSI", src = "yahoo", from = "2000-01-01", to = "2021-12-31")

# Extraer los precios de cierre
HSI <- Cl(HSI)

# Eliminar datos nulos
HSI <- na.omit(HSI)

# Calcular los rendimientos (diferencia de logaritmos)
returns_negative <- diff(log(HSI)) * 100
returns_negative <- na.omit(returns_negative)

# (a) Calcular el rendimiento umbral dado por el percentil 90
threshold <- quantile(returns_negative, 0.90)
cat("Rendimiento umbral (percentil 90):", round(threshold, 2), "\n")

# (b) Representar los rendimientos del índice con el umbral
plot(index(returns_negative), returns_negative, type = "l", col = "blue", main = "Rendimientos del Índice con Umbral",
     xlab = "Fecha", ylab = "Rendimiento")
abline(h = threshold, col = "red", lty = 2)
legend("topright", legend = c("Rendimientos", "Umbral Percentil 90"), col = c("blue", "red"), lty = c(1, 2))

# (c) Extraer la muestra de datos sobre el umbral
exceedances <- returns_negative[returns_negative > threshold]

# Cambiar el signo de los rendimientos
returns_negative_negative <- -returns_negative

# Representar los rendimientos cambiados de signo
plot(index(returns_negative_negative), returns_negative_negative, type = "l", col = "green", main = "Rendimientos Cambiados de Signo",
     xlab = "Fecha", ylab = "Rendimiento")

# Filtrar los rendimientos por encima del umbral y representarlos
exceedances <- as.numeric(exceedances)
plot(index(returns_negative[returns_negative > threshold]), exceedances, type = "l", col = "purple", main = "Rendimientos sobre el Umbral",
     xlab = "Fecha", ylab = "Rendimiento")

# (d) Calcular los excesos
excesses <- exceedances - threshold

# Representar el histograma de los excesos
hist(excesses, breaks = 50, probability = TRUE, main = "Histograma de Excesos sobre el Umbral",
     xlab = "Exceso sobre el umbral", ylab = "Densidad", col = "lightblue")


# E)  Ajustar la distribución Generalizada de Pareto (GPD) usando fitdistplus
if (length(excesses) > 1) {
  
  # Ajustar la GPD a los datos de excesos
  fit_gpd <- fitdist(excesses, "gpd", start = list(scale = sd(excesses), shape = 0.1))
  
  # Obtener parámetros de la GPD
  params <- fit_gpd$estimate
  cat("Parámetros de la distribución Generalizada de Pareto:\n")
  cat("Escala:", round(params["scale"], 4), "\n")
  cat("Forma:", round(params["shape"], 4), "\n")
  
  # Función de densidad de la GPD
  dgpd <- function(x, scale, shape) {
    (1 / scale) * (1 + shape * x / scale)^(-1 / shape - 1)
  }
  
  # Crear un rango de valores para la densidad
  x <- seq(min(excesses), max(excesses), length = 100)
  
  # Superponer la densidad ajustada de la GPD al histograma
  lines(x, dgpd(x, scale = params["scale"], shape = params["shape"]), col = "red", lwd = 2)
  
} else {
  cat("No hay suficientes datos sobre el umbral para ajustar la GPD y realizar el test.\n")
}



######################################
######################################
######################################  rendimientos neg
######################################
######################################

# Instalar y cargar las librerías necesarias
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("evd", quietly = TRUE)) install.packages("evd")
if (!requireNamespace("fitdistrplus", quietly = TRUE)) install.packages("fitdistrplus")

library(quantmod)
library(evd)
library(fitdistrplus)

# Cargar los datos del índice HSI
getSymbols("^HSI", src = "yahoo", from = "2000-01-01", to = "2021-12-31")

# Extraer los precios de cierre
HSI <- Cl(HSI)

# Eliminar datos nulos
HSI <- na.omit(HSI)

# Calcular los rendimientos (diferencia de logaritmos)
returns_negative <- diff(log(HSI)) * 100
returns_negative <- na.omit(returns_negative)
returns_negative <- returns_negative*-1

# (a) Calcular el rendimiento umbral dado por el percentil 90
threshold <- quantile(returns_negative, 0.90)
cat(" Signo cambiado - Rendimiento umbral (percentil 90):", round(threshold, 2), "\n")

# (b) Representar los rendimientos del índice con el umbral
plot(index(returns_negative), returns_negative, type = "l", col = "blue", main = "Rendimientos del Índice con Umbral",
     xlab = "Fecha", ylab = "Rendimiento signo cambiado")
abline(h = threshold, col = "red", lty = 2)
legend("topright", legend = c("Rendimientos", "Umbral Percentil 90"), col = c("blue", "red"), lty = c(1, 2))

# (c) Extraer la muestra de datos sobre el umbral
exceedances <- returns_negative[returns_negative > threshold]

# Cambiar el signo de los rendimientos
returns_negative_negative <- -returns_negative

# Representar los rendimientos cambiados de signo
plot(index(returns_negative_negative), returns_negative_negative, type = "l", col = "green", main = "Rendimientos signo normal",
     xlab = "Fecha", ylab = "Rendimiento")

# Filtrar los rendimientos por encima del umbral y representarlos
exceedances <- as.numeric(exceedances)
plot(index(returns_negative[returns_negative > threshold]), exceedances, type = "l", col = "purple", main = "Rendimientos sobre el Umbral",
     xlab = "Fecha", ylab = "Rendimiento")

# (d) Calcular los excesos
excesses <- exceedances - threshold

# Representar el histograma de los excesos
hist(excesses, breaks = 50, probability = TRUE, main = "Histograma de Excesos sobre el Umbral",
     xlab = "Exceso sobre el umbral", ylab = "Densidad", col = "lightblue")


# E)  Ajustar la distribución Generalizada de Pareto (GPD) usando fitdistplus
if (length(excesses) > 1) {
  
  # Ajustar la GPD a los datos de excesos
  fit_gpd <- fitdist(excesses, "gpd", start = list(scale = sd(excesses), shape = 0.1))
  
  # Obtener parámetros de la GPD
  params <- fit_gpd$estimate
  cat("Parámetros de la distribución Generalizada de Pareto:\n")
  cat("Escala:", round(params["scale"], 4), "\n")
  cat("Forma:", round(params["shape"], 4), "\n")
  
  # Función de densidad de la GPD
  dgpd <- function(x, scale, shape) {
    (1 / scale) * (1 + shape * x / scale)^(-1 / shape - 1)
  }
  
  # Crear un rango de valores para la densidad
  x <- seq(min(excesses), max(excesses), length = 100)
  
  # Superponer la densidad ajustada de la GPD al histograma
  lines(x, dgpd(x, scale = params["scale"], shape = params["shape"]), col = "red", lwd = 2)
  
} else {
  cat("No hay suficientes datos sobre el umbral para ajustar la GPD y realizar el test.\n")
}










######################################
######################################
###################################### umbral optimo
######################################
######################################


# Instalar y cargar las librerías necesarias
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("evd", quietly = TRUE)) install.packages("evd")
if (!requireNamespace("fitdistrplus", quietly = TRUE)) install.packages("fitdistrplus")

library(quantmod)
library(evd)
library(fitdistrplus)

# Cargar los datos del índice HSI
getSymbols("^HSI", src = "yahoo", from = "2000-01-01", to = "2021-12-31")

# Extraer los precios de cierre
HSI <- Cl(HSI)

# Eliminar datos nulos
HSI <- na.omit(HSI)

# Calcular los rendimientos (diferencia de logaritmos)
returns <- diff(log(HSI)) * 100
returns <- na.omit(returns)
returns <- returns*-1#cambiar colas calcular perdidas en largo IMPORTANTE

# Rango de percentiles para evaluar
percentiles <- seq(0.85, 0.99, by = 0.01)

# Variables para guardar el mejor umbral y el p-valor más alto
best_threshold <- NA
best_p_value <- -Inf

# Función para realizar el ajuste y test de Kolmogorov-Smirnov
perform_ks_test <- function(threshold) {
  exceedances <- returns[returns > threshold]
  if (length(exceedances) > 1) {
    excesses <- exceedances - threshold
    excesses <- as.numeric(excesses)  # Asegurarse de que los excesos sean un vector numérico
    
    # Intentar ajustar la GPD
    fit_gpd <- tryCatch({
      fitdist(excesses, "gpd", start = list(scale = sd(excesses), shape = 0.1))
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(fit_gpd)) {
      # Obtener parámetros de la GPD
      scale <- fit_gpd$estimate["scale"]
      shape <- fit_gpd$estimate["shape"]
      
      # Realizar el test de Kolmogorov-Smirnov
      ks_test <- tryCatch({
        ks.test(excesses, "pgpd", scale = scale, shape = shape)
      }, error = function(e) {
        return(NULL)
      })
      
      if (!is.null(ks_test)) {
        return(ks_test$p.value)
      } else {
        return(NA)
      }
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

# Evaluar cada percentil como umbral candidato
for (percentile in percentiles) {
  threshold <- quantile(returns, percentile)
  p_value <- perform_ks_test(threshold)
  
  # Verificar si el p-valor es el mayor encontrado hasta ahora
  if (!is.na(p_value) && p_value > best_p_value) {
    best_p_value <- p_value
    best_threshold <- threshold
  }
}

# Imprimir el umbral óptimo y el p-valor correspondiente
if (!is.na(best_threshold)) {
  cat("Umbral óptimo (percentil):", round(best_threshold, 2), "\n")
  cat("P-valor máximo del test de Kolmogorov-Smirnov:", round(best_p_value, 4), "\n")
} else {
  cat("No se encontraron umbrales válidos para ajustar la GPD y realizar el test.\n")
}

# Mostrar umbral óptimo y p-valor máximo
if (!is.na(best_threshold)) {
  cat("Umbral óptimo (percentil):", round(best_threshold, 2), "\n")
  cat("P-valor máximo del test de Kolmogorov-Smirnov:", round(best_p_value, 4), "\n")
  
  # Representar los rendimientos con el umbral óptimo
  plot(index(returns), returns, type = "l", col = "blue", main = "Rendimientos del Índice con Umbral Óptimo",
       xlab = "Fecha", ylab = "Rendimiento")
  abline(h = best_threshold, col = "red", lty = 2)
  legend("topright", legend = c("Rendimientos", "Umbral Óptimo"), col = c("blue", "red"), lty = c(1, 2))
  
} else {
  cat("No se encontró un umbral válido.\n")
}




######################################
######################################
###################################### 
######################################
######################################



# Instalar y cargar las librerías necesarias
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("evd", quietly = TRUE)) install.packages("evd")
if (!requireNamespace("fitdistrplus", quietly = TRUE)) install.packages("fitdistrplus")

library(quantmod)
library(evd)
library(fitdistrplus)

# Cargar los datos del índice HSI
getSymbols("^HSI", src = "yahoo", from = "2000-01-01", to = "2021-12-31")

# Extraer los precios de cierre
HSI <- Cl(HSI)

# Eliminar datos nulos
HSI <- na.omit(HSI)

# Calcular los rendimientos (diferencia de logaritmos)
returns_negative <- diff(log(HSI)) * 100
returns_negative <- na.omit(returns_negative)
returns_negative <- returns_negative*-1

# (a) Calcular el rendimiento umbral dado por el percentil 90
threshold <- 2.14
cat("Rendimiento umbral óptimo:", round(threshold, 2), "\n")

# (b) Representar los rendimientos del índice con el umbral
plot(index(returns_negative), returns_negative, type = "l", col = "blue", main = "Rendimientos del Índice con Umbral óptimo",
     xlab = "Fecha", ylab = "Rendimiento")
abline(h = threshold, col = "red", lty = 2)
legend("topright", legend = c("Rendimientos", "Umbral óptimo"), col = c("blue", "red"), lty = c(1, 2))

# (c) Extraer la muestra de datos sobre el umbral
exceedances <- returns_negative[returns_negative > threshold]


# Filtrar los rendimientos por encima del umbral y representarlos
exceedances <- as.numeric(exceedances)
plot(index(returns_negative[returns_negative > threshold]), exceedances, type = "l", col = "purple", main = "Rendimientos sobre el Umbral óptimo",
     xlab = "Fecha", ylab = "Rendimiento")

# (d) Calcular los excesos
excesses <- exceedances - threshold

# Representar el histograma de los excesos
hist(excesses, breaks = 50, probability = TRUE, main = "Histograma de Excesos sobre el Umbral",
     xlab = "Exceso sobre el umbral", ylab = "Densidad", col = "lightblue")


# E)  Ajustar la distribución Generalizada de Pareto (GPD) usando fitdistplus
if (length(excesses) > 1) {
  
  # Ajustar la GPD a los datos de excesos
  fit_gpd <- fitdist(excesses, "gpd", start = list(scale = sd(excesses), shape = 0.1))
  
  # Obtener parámetros de la GPD
  params <- fit_gpd$estimate
  cat("Parámetros de la distribución Generalizada de Pareto:\n")
  cat("Escala:", round(params["scale"], 4), "\n")
  cat("Forma:", round(params["shape"], 4), "\n")
  
  # Función de densidad de la GPD
  dgpd <- function(x, scale, shape) {
    (1 / scale) * (1 + shape * x / scale)^(-1 / shape - 1)
  }
  
  # Crear un rango de valores para la densidad
  x <- seq(min(excesses), max(excesses), length = 100)
  
  # Superponer la densidad ajustada de la GPD al histograma
  lines(x, dgpd(x, scale = params["scale"], shape = params["shape"]), col = "red", lwd = 2)
  
} else {
  cat("No hay suficientes datos sobre el umbral para ajustar la GPD y realizar el test.\n")
}


#######################
#VAR FORMULA MANUAL

# Definir los niveles de significancia
significance_levels <- c(0.01, 0.05)

# Extraer los parámetros de la distribución GPD ajustada
# Asegúrate de que estos valores están correctamente definidos
xi <- params["shape"]    # Parámetro de forma (k)
scale <- params["scale"] # Parámetro de escala (??)

# Calcular el número de excesos y el número total de observaciones
n_excess <- length(exceedances)
n_total <- length(returns_negative)

# Calcular el VaR para cada nivel de significancia utilizando la fórmula ajustada
VaRs_EVT <- sapply(significance_levels, function(alpha) {
  # Calcular el VaR usando la fórmula ajustada
  VaR_EVT <- threshold + (scale / xi) * (((n_excess * alpha / n_total)^(-xi)) - 1)
  return(VaR_EVT)
})

# Mostrar el resultado de VaRs
names(VaRs_EVT) <- paste0("VaR_", significance_levels * 100, "%")
VaRs_EVT


######################################
#VAR BIEN CALCULADO SEGUN INTERNTE

# Definir los niveles de significancia
significance_levels <- c(0.01, 0.05)

# Calcular el VaR para cada nivel de significacion utilizando la distribución Generalizada de Pareto (GPD)
VaRs_EVT <- sapply(significance_levels, function(alpha) {
  n_excess <- length(exceedances)
  n_total <- length(returns_negative)
  scale = params["scale"]
  shape = params["shape"]
  
  p_excess <- n_excess / n_total
  q_excess <- (alpha - p_excess) / (1 - p_excess)
  
  VaR_EVT <- threshold + (scale / shape) * (((1 - q_excess)^(-shape)) - 1)
  return(VaR_EVT)
})

# Mostrar el resultado de VaRs
names(VaRs_EVT) <- paste0("VaR_", significance_levels * 100, "%")
VaRs_EVT


###################
###################################### 
######################################
######################################



# Instalar y cargar las librerías necesarias
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("evd", quietly = TRUE)) install.packages("evd")
if (!requireNamespace("fitdistrplus", quietly = TRUE)) install.packages("fitdistrplus")

library(quantmod)
library(evd)
library(fitdistrplus)

# Cargar los datos del índice HSI
getSymbols("^HSI", src = "yahoo", from = "2000-01-01", to = "2021-12-31")

# Extraer los precios de cierre
HSI <- Cl(HSI)

# Eliminar datos nulos
HSI <- na.omit(HSI)

# Calcular los rendimientos (diferencia de logaritmos)
returns_negative <- diff(log(HSI)) * 100
returns_negative <- na.omit(returns_negative)
returns_negative <- returns_negative*-1

# (a) Calcular el rendimiento umbral dado por el percentil 90
threshold <- 2.14
cat("Rendimiento umbral óptimo:", round(threshold, 2), "\n")

# (b) Representar los rendimientos del índice con el umbral
plot(index(returns_negative), returns_negative, type = "l", col = "blue", main = "Rendimientos del Índice con Umbral óptimo",
     xlab = "Fecha", ylab = "Rendimiento")
abline(h = threshold, col = "red", lty = 2)
legend("topright", legend = c("Rendimientos", "Umbral óptimo"), col = c("blue", "red"), lty = c(1, 2))

# (c) Extraer la muestra de datos sobre el umbral
exceedances <- returns_negative[returns_negative > threshold]


# Filtrar los rendimientos por encima del umbral y representarlos
exceedances <- as.numeric(exceedances)
plot(index(returns_negative[returns_negative > threshold]), exceedances, type = "l", col = "purple", main = "Rendimientos sobre el Umbral óptimo",
     xlab = "Fecha", ylab = "Rendimiento")

# (d) Calcular los excesos
excesses <- exceedances - threshold

# Representar el histograma de los excesos
hist(excesses, breaks = 50, probability = TRUE, main = "Histograma de Excesos sobre el Umbral",
     xlab = "Exceso sobre el umbral", ylab = "Densidad", col = "lightblue")


# E)  Ajustar la distribución Generalizada de Pareto (GPD) usando fitdistplus
if (length(excesses) > 1) {
  
  # Ajustar la GPD a los datos de excesos
  fit_gpd <- fitdist(excesses, "gpd", start = list(scale = sd(excesses), shape = 0.1))
  
  # Obtener parámetros de la GPD
  params <- fit_gpd$estimate
  cat("Parámetros de la distribución Generalizada de Pareto:\n")
  cat("Escala:", round(params["scale"], 4), "\n")
  cat("Forma:", round(params["shape"], 4), "\n")
  
  # Función de densidad de la GPD
  dgpd <- function(x, scale, shape) {
    (1 / scale) * (1 + shape * x / scale)^(-1 / shape - 1)
  }
  
  # Crear un rango de valores para la densidad
  x <- seq(min(excesses), max(excesses), length = 100)
  
  # Superponer la densidad ajustada de la GPD al histograma
  lines(x, dgpd(x, scale = params["scale"], shape = params["shape"]), col = "red", lwd = 2)
  
} else {
  cat("No hay suficientes datos sobre el umbral para ajustar la GPD y realizar el test.\n")
}






############################## VaR diario
############################## VaR diario
############################## VaR diario
############################## VaR diario
############################## VaR diario
############################## VaR diario
############################## VaR diario
############################## VaR diario
############################## VaR diario
############################## VaR diario
############################## VaR diario
############################## VaR diario
############################## VaR diario




# Instalar y cargar las librerías necesarias
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("rugarch", quietly = TRUE)) install.packages("rugarch")
if (!requireNamespace("fitdistrplus", quietly = TRUE)) install.packages("fitdistrplus")
if (!requireNamespace("evd", quietly = TRUE)) install.packages("evd")

library(quantmod)
library(rugarch)
library(fitdistrplus)
library(evd)

# Cargar los datos del índice HSI
getSymbols("^HSI", src = "yahoo", from = "2000-01-01", to = "2021-12-31")

# Extraer los precios de cierre
HSI <- Cl(HSI)

# Eliminar datos nulos
HSI <- na.omit(HSI)

# Calcular los rendimientos (diferencia de logaritmos)
returns <- diff(log(HSI)) * 100
returns <- na.omit(returns)

# Paso 1: Multiplicar los rendimientos por -1 para posiciones largas
returns_neg <- -returns

# Paso 2: Ajustar un modelo GARCH(1,1) para obtener la varianza condicional
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "std"
)
garch_fit <- ugarchfit(spec = garch_spec, data = returns_neg)

# Obtener la desviación estándar condicional
sigma_t <- sqrt(sigma(garch_fit)^2)

# Estandarizar los rendimientos
standardized_returns <- returns_neg / sigma_t

# Paso 3: Fijar el umbral
threshold <- 2.14

# Paso 4: Seleccionar los datos que están por encima del umbral y ajustar la GPD
excesses <- standardized_returns[standardized_returns > threshold]

# Ajustar la distribución GPD a los excesos
gpd_fit <- fpot(excesses, threshold = threshold)
shape <- gpd_fit$estimate["shape"]
scale <- gpd_fit$estimate["scale"]

# Paso 5: Calcular el VaR condicional al 99%
conf_level <- 0.99
percentile <- 1 - conf_level

# Calcular el VaR condicional
VaR_conditional <- rep(NA, length(returns))

for (i in 1:length(returns)) {
  # Calcular el VaR usando la GPD ajustada
  if (standardized_returns[i] > threshold) {
    # Para excesos, usamos la GPD
    VaR_conditional[i] <- -sigma_t[i] * (threshold + qgpd(percentile, shape, scale))
  } else {
    # Para datos no excesivos, simplemente usamos la desviación estándar condicional
    VaR_conditional[i] <- -sigma_t[i] * qnorm(conf_level)
  }
}

# Crear un gráfico para visualizar los rendimientos y el VaR condicional
plot(index(returns), returns, type = 'l', col = 'blue', lty = 1, ylab = 'Rendimientos', xlab = 'Fecha', main = 'Rendimientos y VaR Condicional usando GPD')

# Añadir línea para el VaR condicional
lines(index(returns), VaR_conditional, col = 'red', lty = 2)
legend("topright", legend = c("Rendimientos", "VaR Condicional al 99%"), col = c("blue", "red"), lty = c(1, 2))





# Paso 5: Calcular el VaR condicional al 95%
conf_level <- 0.95
percentile <- 1 - conf_level

# Calcular el VaR condicional
VaR_conditional <- rep(NA, length(returns))

for (i in 1:length(returns)) {
  # Calcular el VaR usando la GPD ajustada
  if (standardized_returns[i] > threshold) {
    # Para excesos, usamos la GPD
    VaR_conditional[i] <- -sigma_t[i] * (threshold + qgpd(percentile, shape, scale))
  } else {
    # Para datos no excesivos, simplemente usamos la desviación estándar condicional
    VaR_conditional[i] <- -sigma_t[i] * qnorm(conf_level)
  }
}

# Crear un gráfico para visualizar los rendimientos y el VaR condicional
plot(index(returns), returns, type = 'l', col = 'grey', lty = 1, ylab = 'Rendimientos', xlab = 'Fecha', main = 'Rendimientos y VaR Condicional usando GPD')

# Añadir línea para el VaR condicional
lines(index(returns), VaR_conditional, col = 'purple', lty = 2)
legend("topright", legend = c("Rendimientos", "VaR Condicional al 95%"), col = c("grey", "purple"), lty = c(1, 2))

