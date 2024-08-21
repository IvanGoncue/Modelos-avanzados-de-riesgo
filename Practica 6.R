# Cargar las librerías necesarias
library(quantmod)
library(fGarch)
library(tseries)

# Descargar los datos del índice S&P 500 desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
sp500_close <- Cl(GSPC)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)

# Calcular estadísticos descriptivos
mean_return <- mean(sp500_returns)
sd_return <- sd(sp500_returns)

# Calcular asimetría (skewness) y curtosis (kurtosis) manualmente
n <- length(sp500_returns)
mean_ret <- mean(sp500_returns)
sd_ret <- sd(sp500_returns)
skewness_return <- sum((sp500_returns - mean_ret)^3) / (n * sd_ret^3)
kurtosis_return <- sum((sp500_returns - mean_ret)^4) / (n * sd_ret^4) - 3

# Mostrar los resultados
cat("Estadísticos Descriptivos de los Rendimientos Diarios del S&P 500:\n")
cat("Media: ", mean_return, "\n")
cat("Desviación Estándar: ", sd_return, "\n")
cat("Asimetría: ", skewness_return, "\n")
cat("Curtosis: ", kurtosis_return, "\n")

# Pruebas de normalidad
# Prueba de Jarque-Bera (requiere el paquete "tseries")
jb_test <- jarque.bera.test(sp500_returns)
cat("\nPrueba de Jarque-Bera:\n")
print(jb_test)

# Graficar el histograma y la curva normal ajustada
hist(sp500_returns, breaks = 50, probability = TRUE, col = "lightgray",
     main = "Histograma de Rendimientos Diarios del S&P 500",
     xlab = "Rendimientos Diarios", ylab = "Densidad")
curve(dnorm(x, mean = mean_return, sd = sd_return), add = TRUE, col = "blue", lwd = 2)

# QQ-Plot
qqnorm(sp500_returns, main = "QQ-Plot de Rendimientos Diarios del S&P 500")
qqline(sp500_returns, col = "blue", lwd = 2)




############################# ESTANDARIZADOS

# Cargar las librerías necesarias
library(quantmod)
library(fGarch)
library(tseries)

# Descargar los datos del índice S&P 500 desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
sp500_close <- Cl(GSPC)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)

# Ajustar un modelo GARCH(1,1) para los rendimientos
garch_model <- garchFit(~ garch(1,1), data = sp500_returns, cond.dist = "norm", trace = FALSE)

# Obtener la volatilidad condicional
sigma_cond <- volatility(garch_model)

# Estándarizar los rendimientos
standardized_returns <- sp500_returns / sigma_cond

# Calcular estadísticos descriptivos para los rendimientos estandarizados
mean_standardized <- mean(standardized_returns)
sd_standardized <- sd(standardized_returns)

# Calcular asimetría (skewness) y curtosis (kurtosis) manualmente para rendimientos estandarizados
n <- length(standardized_returns)
mean_std <- mean(standardized_returns)
sd_std <- sd(standardized_returns)
skewness_standardized <- sum((standardized_returns - mean_std)^3) / (n * sd_std^3)
kurtosis_standardized <- sum((standardized_returns - mean_std)^4) / (n * sd_std^4) - 3

# Mostrar los resultados
cat("Estadísticos Descriptivos de los Rendimientos Diarios Estandarizados del S&P 500:\n")
cat("Media: ", mean_standardized, "\n")
cat("Desviación Estándar: ", sd_standardized, "\n")
cat("Asimetría: ", skewness_standardized, "\n")
cat("Curtosis: ", kurtosis_standardized, "\n")

# Pruebas de normalidad
# Prueba de Jarque-Bera
jb_test_std <- jarque.bera.test(standardized_returns)
cat("\nPrueba de Jarque-Bera para Rendimientos Estandarizados:\n")
print(jb_test_std)

# Graficar el histograma y la curva normal ajustada para los rendimientos estandarizados
hist(standardized_returns, breaks = 50, probability = TRUE, col = "lightgray",
     main = "Histograma de Rendimientos Diarios Estandarizados del S&P 500",
     xlab = "Rendimientos Estandarizados", ylab = "Densidad")
curve(dnorm(x, mean = mean_standardized, sd = sd_standardized), add = TRUE, col = "blue", lwd = 2)

# QQ-Plot para rendimientos estandarizados
qqnorm(standardized_returns, main = "QQ-Plot de Rendimientos Diarios Estandarizados del S&P 500")
qqline(standardized_returns, col = "blue", lwd = 2)


#########################################################
#####################################################
###################################################
# PARAMETRICO T STUDENT

# Cargar las librerías necesarias
library(quantmod)
library(fGarch)

# Descargar los datos del índice S&P 500 desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
sp500_close <- Cl(GSPC)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)

# Ajustar un modelo AR(1)-GARCH(1,1) para los rendimientos con distribución t de Student
ar_garch_t_model <- garchFit(~ arma(1,0) + garch(1,1), data = sp500_returns, cond.dist = "std", trace = FALSE)

# Presentar las estimaciones de los parámetros
summary(ar_garch_t_model)

# Obtener la media condicional y la desviación estándar condicional (volatilidad)
mu_cond <- fitted(ar_garch_t_model)
sigma_cond <- volatility(ar_garch_t_model)

# Obtener los grados de libertad de la distribución t
df <- ar_garch_t_model@fit$coef["shape"]

# Calcular el VaR paramétrico bajo la distribución t de Student
VaR_95 <- qt(0.05, df) * sigma_cond + mu_cond
VaR_99 <- qt(0.01, df) * sigma_cond + mu_cond

# Crear un data frame con las estimaciones diarias de VaR
VaR_data_t <- data.frame(Date = index(sp500_returns),
                         Returns = as.numeric(sp500_returns),
                         VaR_95 = VaR_95,
                         VaR_99 = VaR_99)

# Graficar los rendimientos y las estimaciones de VaR
par(mfrow = c(2, 1))  # Configurar el gráfico en 2 subplots

# Graficar VaR 95%
plot(VaR_data_t$Date, VaR_data_t$Returns, type = "l", col = "black", lwd = 1,
     main = "Rendimientos Diarios y VaR 95% del S&P 500 (AR(1)-GARCH(1,1) con t-Student)",
     xlab = "Fecha", ylab = "Rendimientos / VaR")
lines(VaR_data_t$Date, VaR_data_t$VaR_95, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Rendimientos", "VaR 95%"),
       col = c("black", "blue"), lwd = 2, lty = c(1, 2), cex = 0.4)

# Graficar VaR 99%
plot(VaR_data_t$Date, VaR_data_t$Returns, type = "l", col = "black", lwd = 1,
     main = "Rendimientos Diarios y VaR 99% del S&P 500 (AR(1)-GARCH(1,1) con t-Student)",
     xlab = "Fecha", ylab = "Rendimientos / VaR")
lines(VaR_data_t$Date, VaR_data_t$VaR_99, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Rendimientos", "VaR 99%"),
       col = c("black", "red"), lwd = 2, lty = c(1, 2), cex = 0.4)

###############################################################
#########################################################
#ESTADISTICOS DE LAS ESTIMACIONES

# Cargar las librerías necesarias
library(quantmod)
library(fGarch)
library(tseries)

# Descargar los datos del índice S&P 500 desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
sp500_close <- Cl(GSPC)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)

# Ajustar un modelo AR(1)-GARCH(1,1) para los rendimientos con distribución t de Student
ar_garch_t_model <- garchFit(~ arma(1,0) + garch(1,1), data = sp500_returns, cond.dist = "std", trace = FALSE)

# Obtener la media condicional y la desviación estándar condicional (volatilidad)
mu_cond <- fitted(ar_garch_t_model)
sigma_cond <- volatility(ar_garch_t_model)

# Obtener los grados de libertad de la distribución t
df <- ar_garch_t_model@fit$coef["shape"]



# Calcular el VaR paramétrico bajo la distribución t de Student
VaR_95 <- qt(0.05, df) * sigma_cond + mu_cond
VaR_99 <- qt(0.01, df) * sigma_cond + mu_cond

# Crear un data frame con las estimaciones diarias de VaR
VaR_data_t <- data.frame(Date = index(sp500_returns),
                         Returns = as.numeric(sp500_returns),
                         VaR_95 = VaR_95,
                         VaR_99 = VaR_99)

# Graficar los rendimientos y las estimaciones de VaR
par(mfrow = c(2, 1))  # Configurar el gráfico en 2 subplots

# Graficar VaR 95%
plot(VaR_data_t$Date, VaR_data_t$Returns, type = "l", col = "black", lwd = 1,
     main = "Rendimientos Diarios y VaR 95% del S&P 500 (AR(1)-GARCH(1,1) con t-Student)",
     xlab = "Fecha", ylab = "Rendimientos / VaR")
lines(VaR_data_t$Date, VaR_data_t$VaR_95, col = "green", lwd = 2, lty = 2)
legend("topright", legend = c("Rendimientos", "VaR 95%"),
       col = c("black", "green"), lwd = 2, lty = c(1, 2), cex = 0.4)

# Graficar VaR 99%
plot(VaR_data_t$Date, VaR_data_t$Returns, type = "l", col = "black", lwd = 1,
     main = "Rendimientos Diarios y VaR 99% del S&P 500 (AR(1)-GARCH(1,1) con t-Student)",
     xlab = "Fecha", ylab = "Rendimientos / VaR")
lines(VaR_data_t$Date, VaR_data_t$VaR_99, col = "purple", lwd = 2, lty = 2)
legend("topright", legend = c("Rendimientos", "VaR 99%"),
       col = c("black", "purple"), lwd = 2, lty = c(1, 2), cex = 0.4)

# Calcular el VaR paramétrico bajo la distribución t de Student
VaR_95 <- qt(0.05, df) * sigma_cond + mu_cond
VaR_99 <- qt(0.01, df) * sigma_cond + mu_cond

# Crear un data frame con las estimaciones diarias de VaR
VaR_data_t <- data.frame(Date = index(sp500_returns),
                         VaR_95 = VaR_95,
                         VaR_99 = VaR_99)

# Función para calcular estadísticos descriptivos
calculate_statistics <- function(data) {
  mean_val <- mean(data)
  sd_val <- sd(data)
  skewness_val <- sum((data - mean_val)^3) / (length(data) * sd_val^3)
  kurtosis_val <- sum((data - mean_val)^4) / (length(data) * sd_val^4) - 3
  
  return(c(Mean = mean_val, SD = sd_val, Skewness = skewness_val, Kurtosis = kurtosis_val))
}

# Calcular estadísticos descriptivos para VaR_95
VaR_95_stats <- calculate_statistics(VaR_data_t$VaR_95)
cat("Estadísticos Descriptivos para VaR 95%:\n")
print(VaR_95_stats)

# Calcular estadísticos descriptivos para VaR_99
VaR_99_stats <- calculate_statistics(VaR_data_t$VaR_99)
cat("\nEstadísticos Descriptivos para VaR 99%:\n")
print(VaR_99_stats)
