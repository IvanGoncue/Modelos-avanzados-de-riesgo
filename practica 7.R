# Cargar las librerías necesarias
#library(quantmod)
#library(fGarch)
#library(tseries)

# Descargar los datos del índice S&P 500 desde Yahoo Finance
#getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
#sp500_close <- Cl(GSPC)

# Calcular los rendimientos diarios
#sp500_returns <- diff(log(sp500_close))

# Eliminar valores NA generados por la función diff
#sp500_returns <- na.omit(sp500_returns)

# Ajustar un modelo AR(1)-GARCH(1,1) para los rendimientos con distribución t de Student
#ar_garch_t_model <- garchFit(~ arma(1,0) + garch(1,1), data = sp500_returns, cond.dist = "std", trace = FALSE)

# Obtener la media condicional, la desviación estándar condicional (volatilidad) y los residuos ajustados
#mu_cond <- fitted(ar_garch_t_model)
#sigma_cond <- volatility(ar_garch_t_model)
#residuals_adjusted <- residuals(ar_garch_t_model, standardize = TRUE)

# Obtener los grados de libertad de la distribución t
#df <- ar_garch_t_model@fit$coef["shape"]

# Número de simulaciones y horizonte de predicción
#n_simulations <- 10000
#n_days <- length(sp500_returns)

# Simulación Histórica Filtrada (SHF)
#set.seed(123)  # Para reproducibilidad
#simulated_returns <- matrix(NA, nrow = n_days, ncol = n_simulations)

#for (i in 1:n_simulations) {
 # simulated_returns[, i] <- mu_cond + sigma_cond * rt(n_days, df)
#}

# Calcular el VaR a nivel de confianza del 5% para cada día
#VaR_5_daily <- apply(simulated_returns, 1, function(x) quantile(x, probs = 0.05))

# Crear un data frame con los rendimientos y VaR
#VaR_data_shf <- data.frame(Date = index(sp500_returns),
#                          Returns = as.numeric(sp500_returns),
#                          VaR_5 = VaR_5_daily)

# Graficar los rendimientos y el VaR
#par(mfrow = c(1, 1))  # Configurar el gráfico en 1 subplot

#plot(VaR_data_shf$Date, VaR_data_shf$Returns, type = "l", col = "black", lwd = 1,
#    main = "Rendimientos Diarios y VaR 5% del S&P 500 (Simulación Histórica Filtrada)",
#    xlab = "Fecha", ylab = "Rendimientos / VaR")
#lines(VaR_data_shf$Date, VaR_data_shf$VaR_5, col = "blue", lwd = 2, lty = 2)

# Añadir una leyenda
#legend("topright", legend = c("Rendimientos", "VaR 5%"),
#      col = c("black", "blue"), lwd = 2, lty = c(1, 2), cex=(0.4))





############################################################################
###########################################################################
################################################################################
#################################################################################
#############################################################################


# Cargar las librerías necesarias
library(quantmod)
library(fGarch)
library(tseries)
library(rugarch)  # Necesaria para modelos GARCH con distribuciones no normales

# Descargar los datos del índice S&P 500 desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
sp500_close <- Cl(GSPC)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)

# Estimar el modelo AR(1)-GARCH(1,1) con distribución t-Student
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0)),
  distribution.model = "std"  # Distribución t-Student
)

# Ajustar el modelo a los rendimientos
fit <- ugarchfit(spec = spec, data = sp500_returns)

# Extraer las series de sigma y la media condicional
sigma_t <- sigma(fit)
mu_t <- fitted(fit)

# Realizar la Simulación Histórica Filtrada (SHF)
n_sim <- length(sp500_returns)
residuals_t <- residuals(fit, standardize = TRUE)  # Residuos estandarizados

# Simulaciones para el cálculo del VaR
set.seed(123)
simulated_returns <- mu_t + sigma_t * residuals_t[sample(1:n_sim, n_sim, replace = TRUE)]

# Calcular el VaR al 5% para un horizonte de 1 día
VaR_5_percent <- quantile(simulated_returns, probs = 0.05)

# Mostrar el VaR calculado
VaR_5_percent



# Crear el gráfico con todos los rendimientos del mismo color
plot(index(sp500_returns), sp500_returns, type = "h", col = "blue",
     main = "Rendimientos diarios del S&P 500 y VaR al 5% horizonte 1 día", 
     xlab = "Fecha", ylab = "Rendimientos diarios",
     lwd = 2)

# Añadir la línea del VaR
abline(h = VaR_5_percent, col = "red", lwd = 2, lty = 2)

# Añadir una leyenda
legend("topright", legend = c("Rendimientos", "VaR al 5% - horizonte 1 día"), 
       col = c("blue", "red"), lty = c(1, 2), cex = (0.4),lwd = 2)



##############################################################################
##########################################################################
###########################################################################
#################################################################################


# Cargar las librerías necesarias
#library(quantmod)
#library(rugarch)

# Descargar los datos del índice S&P 500 desde Yahoo Finance
#getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
#sp500_close <- Cl(GSPC)

# Calcular los rendimientos diarios
#sp500_returns <- diff(log(sp500_close))

# Eliminar valores NA generados por la función diff
#sp500_returns <- na.omit(sp500_returns)

# Usar los primeros 4000 datos para estimar el modelo
#training_returns <- sp500_returns[1:4000]
#testing_returns <- sp500_returns[-(1:4000)]

# Especificar el modelo AR(1)-GARCH(1,1) con distribución t-Student
#spec <- ugarchspec(
#variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
#mean.model = list(armaOrder = c(1, 0)),
#distribution.model = "std"  # Distribución t-Student
#)

# Ajustar el modelo a los primeros 4000 rendimientos
#fit <- ugarchfit(spec = spec, data = training_returns)

# Vector para almacenar el VaR diario
#VaR_5_percent <- numeric(length(testing_returns))

# Calcular el VaR diario para cada día de la muestra de prueba
#for (i in 1:length(testing_returns)) {
  # Ajustar el modelo con los datos acumulados (primeros 4000 más i datos adicionales)
#fit_temp <- ugarchfit(spec = spec, data = sp500_returns[1:(4000 + i)], solver.control = list(trace = 0))
  
  # Predecir la media y la volatilidad condicionales para el día i
#forecast <- ugarchforecast(fit_temp, n.ahead = 1)
  
  # Extraer la media y la volatilidad condicionales
#mu_t <- as.numeric(fitted(forecast))
#sigma_t <- as.numeric(sigma(forecast))
  
  # Calcular el VaR al 5% para el día i usando el cuantil t-Student
#VaR_5_percent[i] <- mu_t - sigma_t * qt(0.95, df = fit_temp@fit$coef["shape"])
}

# Convertir el VaR estimado a serie temporal para el gráfico
#VaR_5_percent_xts <- xts(VaR_5_percent, order.by = index(testing_returns))

# Graficar los rendimientos y el VaR estimado
#plot(testing_returns, type = "l", col = "blue", main = "Rendimientos diarios del S&P 500 y VaR diario al 5%",
# xlab = "Fecha", ylab = "Rendimientos / VaR")
#lines(VaR_5_percent_xts, col = "red", lwd = 2)
#legend("bottomleft", legend = c("Rendimientos", "VaR al 5%"), col = c("blue", "red"), lty = 1, lwd = 2)


################################33
##################################3
####################################
#####################################

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

# Obtener la media condicional, la desviación estándar condicional (volatilidad) y los residuos ajustados
mu_cond <- fitted(ar_garch_t_model)
sigma_cond <- volatility(ar_garch_t_model)
residuals_adjusted <- residuals(ar_garch_t_model, standardize = TRUE)

# Obtener los grados de libertad de la distribución t
df <- ar_garch_t_model@fit$coef["shape"]

# Número de simulaciones y horizonte de predicción
n_simulations <- 10000
n_days <- length(sp500_returns)

# Simulación Histórica Filtrada (SHF)
set.seed(123)  # Para reproducibilidad
simulated_returns <- matrix(NA, nrow = n_days, ncol = n_simulations)

for (i in 1:n_simulations) {
  simulated_returns[, i] <- mu_cond + sigma_cond * rt(n_days, df)
}

# Calcular el VaR a nivel de confianza del 5% para cada día
VaR_5_daily <- apply(simulated_returns, 1, function(x) quantile(x, probs = 0.05))

# Crear un data frame con los rendimientos y VaR
VaR_data_shf <- data.frame(Date = index(sp500_returns),
                           Returns = as.numeric(sp500_returns),
                           VaR_5 = VaR_5_daily)

# Graficar los rendimientos y el VaR
plot(VaR_data_shf$Date, VaR_data_shf$Returns, type = "l", col = "black", lwd = 1,
     main = "Rendimientos Diarios y VaR 5% del S&P 500 (Simulación Histórica Filtrada)",
     xlab = "Fecha", ylab = "Rendimientos / VaR")
lines(VaR_data_shf$Date, VaR_data_shf$VaR_5, col = "blue", lwd = 2, lty = 2)

# Añadir una leyenda
legend("topright", legend = c("Rendimientos", "VaR 5%"),
       col = c("black", "blue"), lwd = 2, lty = c(1, 2))

