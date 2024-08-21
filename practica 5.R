
# Cargar las librerías necesarias
library(quantmod)

# Descargar los datos del índice S&P 500 desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
sp500_close <- Cl(GSPC)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)

# Calcular la media y la desviación estándar incondicional de los rendimientos
mean_return <- mean(sp500_returns)
std_dev_return <- sd(sp500_returns)


# Calcular el VaR paramétrico bajo normalidad
confidence_levels <- c(0.95, 0.99)
VaR_95 <- qnorm(1 - confidence_levels[1]) * std_dev_return + mean_return
VaR_99 <- qnorm(1 - confidence_levels[2]) * std_dev_return + mean_return

# Crear un data frame con las estimaciones diarias de VaR
VaR_data <- data.frame(Date = index(sp500_returns),
                       Returns = as.numeric(sp500_returns),
                       VaR_95 = rep(VaR_95, length(sp500_returns)),
                       VaR_99 = rep(VaR_99, length(sp500_returns)))

# Graficar los rendimientos y las estimaciones de VaR
plot(VaR_data$Date, VaR_data$Returns, type = "l", col = "black", lwd = 1,
     main = "Rendimientos Diarios y Estimaciones VaR del S&P 500",
     xlab = "Fecha", ylab = "Rendimientos / VaR")

# Añadir las líneas del VaR al gráfico
lines(VaR_data$Date, VaR_data$VaR_95, col = "blue", lwd = 2, lty = 2)
lines(VaR_data$Date, VaR_data$VaR_99, col = "red", lwd = 2, lty = 2)

# Añadir una leyenda
legend("topright", legend = c("Rendimientos", "VaR 95%", "VaR 99%"),
       col = c("black", "blue", "red"), lwd = 2, lty = c(1, 2, 2))




######################################################################################
####################################################################################
##################################################################################
###################################################################################


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

# Ajustar un modelo AR(1)-GARCH(1,1) para los rendimientos
ar_garch_model <- garchFit(~ arma(1,0) + garch(1,1), data = sp500_returns, cond.dist = "norm", trace = FALSE)

# Presentar las estimaciones de los parámetros
summary(ar_garch_model)




# Obtener la media condicional y la desviación estándar condicional (volatilidad)
mu_cond <- fitted(ar_garch_model)
sigma_cond <- volatility(ar_garch_model)

# Calcular el VaR paramétrico bajo normalidad
VaR_95 <- qnorm(0.05) * sigma_cond + mu_cond
VaR_99 <- qnorm(0.01) * sigma_cond + mu_cond

# Crear un data frame con las estimaciones diarias de VaR
VaR_data <- data.frame(Date = index(sp500_returns),
                       Returns = as.numeric(sp500_returns),
                       VaR_95 = VaR_95,
                       VaR_99 = VaR_99)

# Graficar los rendimientos y las estimaciones de VaR
plot(VaR_data$Date, VaR_data$Returns, type = "l", col = "black", lwd = 1,
     main = "Rendimientos Diarios y Estimaciones VaR del S&P 500 (AR(1)-GARCH(1,1))",
     xlab = "Fecha", ylab = "Rendimientos / VaR")

# Añadir las líneas del VaR al gráfico
lines(VaR_data$Date, VaR_data$VaR_95, col = "blue", lwd = 2, lty = 2)
lines(VaR_data$Date, VaR_data$VaR_99, col = "red", lwd = 2, lty = 2)

# Añadir una leyenda
legend("topright", legend = c("Rendimientos", "VaR 95%", "VaR 99%"),
       col = c("black", "blue", "red"), lwd = 2, lty = c(1, 2, 2))





######################################################################################3


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

# Ajustar un modelo AR(1)-GARCH(1,1) para los rendimientos
ar_garch_model <- garchFit(~ arma(1,0) + garch(1,1), data = sp500_returns, cond.dist = "norm", trace = FALSE)

# Obtener la media condicional y la desviación estándar condicional (volatilidad)
mu_cond <- fitted(ar_garch_model)
sigma_cond <- volatility(ar_garch_model)

# Calcular el VaR paramétrico bajo normalidad
VaR_95 <- qnorm(0.05) * sigma_cond + mu_cond
VaR_99 <- qnorm(0.01) * sigma_cond + mu_cond

# Crear un data frame con las estimaciones diarias de VaR
VaR_data <- data.frame(Date = index(sp500_returns),
                       Returns = as.numeric(sp500_returns),
                       VaR_95 = VaR_95,
                       VaR_99 = VaR_99)

# Configurar el layout de gráficos en dos filas
par(mfrow = c(2, 1))

# Gráfico 1: Rendimientos y VaR 95%
plot(VaR_data$Date, VaR_data$Returns, type = "l", col = "black", lwd = 1,
     main = "Rendimientos Diarios y VaR 95% del S&P 500",
     xlab = "Fecha", ylab = "Rendimientos / VaR 95%")
lines(VaR_data$Date, VaR_data$VaR_95, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Rendimientos", "VaR 95%"),
       col = c("black", "blue"), lwd = 2, lty = c(1, 2), cex = 0.4)

# Gráfico 2: Rendimientos y VaR 99%
plot(VaR_data$Date, VaR_data$Returns, type = "l", col = "black", lwd = 1,
     main = "Rendimientos Diarios y VaR 99% del S&P 500",
     xlab = "Fecha", ylab = "Rendimientos / VaR 99%")
lines(VaR_data$Date, VaR_data$VaR_99, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Rendimientos", "VaR 99%"),
       col = c("black", "red"), lwd = 2, lty = c(1, 2), cex = 0.4)

# Restaurar el layout de gráficos a uno solo
par(mfrow = c(1, 1))
























































################ comprobacion ejercicio uno es constante el VAR

# Cargar las librerías necesarias
library(quantmod)

# Descargar los datos del índice S&P 500 desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
sp500_close <- Cl(GSPC)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)

# Calcular la media y la desviación estándar incondicional de los rendimientos
mean_return <- mean(sp500_returns)
std_dev_return <- sd(sp500_returns)

# Calculo del VaR usando el método paramétrico bajo normalidad (media y desviación estándar constante)
alpha <- c(0.05, 0.01) 
VaR <- mean_return + std_dev_return * qnorm(alpha)

# Crear un data frame con las estimaciones diarias de VaR
VaR_data <- data.frame(Date = index(sp500_returns),
                       Returns = as.numeric(sp500_returns),
                       VaR_95 = rep(VaR[1], length(sp500_returns)),
                       VaR_99 = rep(VaR[2], length(sp500_returns)))

# Graficar los rendimientos y las estimaciones de VaR
plot(VaR_data$Date, VaR_data$Returns, type = "l", col = "black", lwd = 1,
     main = "Rendimientos Diarios y Estimaciones VaR del S&P 500 (Paramétrico Normal)",
     xlab = "Fecha", ylab = "Rendimientos / VaR")

# Añadir las líneas del VaR al gráfico
lines(VaR_data$Date, VaR_data$VaR_95, col = "blue", lwd = 2, lty = 2)
lines(VaR_data$Date, VaR_data$VaR_99, col = "red", lwd = 2, lty = 2)

# Añadir una leyenda
legend("topright", legend = c("Rendimientos", "VaR 95%", "VaR 99%"),
       col = c("black", "blue", "red"), lwd = 2, lty = c(1, 2, 2))

