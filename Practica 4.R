  # Instalar y cargar las librerías necesarias
  if (!requireNamespace("rugarch", quietly = TRUE)) {
    install.packages("rugarch")
  }
  library(rugarch)
  library(quantmod)
  library(xts)
  
  # Cargar los datos del SP500
  getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = "2021-12-31", periodicity = "daily")
  
  # Extraer los precios de cierre
  SP500 <- GSPC$GSPC.Close
  
  # Eliminar datos nulos
  SP500 <- na.omit(SP500)
  
  # Calcular los rendimientos (diferencia de logaritmos)
  yield <- diff(log(SP500))
  yield <- yield * 100
  yield <- na.omit(yield)
  
  # Definir una función para ajustar el modelo GARCH(p,1) y calcular el AIC
  adjust_garch_model <- function(yield, p) {
    # Definir especificaciones del modelo GARCH(p,1)
    spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(p, 1)),
      mean.model = list(armaOrder = c(1, 0)),
      distribution.model = "norm"
    )
    
    # Ajustar el modelo
    modelo <- tryCatch({
      ugarchfit(spec, yield)
    }, error = function(e) {
      message(paste("Error en el ajuste del modelo GARCH(", p, ",1):", e$message))
      NULL
    })
    
    # Calcular el AIC si el modelo se ajustó correctamente
    if (!is.null(modelo)) {
      infocrit <- infocriteria(modelo)
      aic <- infocrit["Akaike"]
      sigmas <- sigma(modelo)
      list(modelo = modelo, AIC = aic, sigmas = sigmas)
    } else {
      list(modelo = NULL, AIC = NA, sigmas = NULL)
    }
  }
  
  # Ajustar modelos GARCH(1,1), GARCH(2,1), y GARCH(3,1)
  models <- list()
  for (p in 1:3) {
    models[[paste("GARCH(", p, ",1)", sep = "")]] <- adjust_garch_model(yield, p)
  }
  
  # Imprimir coeficientes y AIC para cada modelo
  for (name in names(models)) {
    model <- models[[name]]
    cat("\nModelo:", name, "\n")
    if (!is.null(model$modelo)) {
      cat("AIC:", round(model$AIC, 2), "\n")  # Redondear el AIC a 2 decimales
    } else {
      cat("No se pudo ajustar el modelo.\n")
    }
  }
  
  # Calcular el máximo de las volatilidades para definir el límite superior del eje y
  max_volatility <- max(sapply(models, function(m) if (!is.null(m$sigmas)) max(m$sigmas, na.rm = TRUE) else 0))
  
  # Graficar las volatilidades estimadas en gráficos separados
  
  # Crear un diseño de gráficos en 3 filas y 1 columna
  par(mfrow = c(3, 1), mar = c(4, 4, 2, 1) + 0.1)
  
  # Graficar para GARCH(1,1)
  if (!is.null(models[["GARCH(1,1)"]]$sigmas)) {
    plot(index(SP500)[-1], models[["GARCH(1,1)"]]$sigmas, type = "l", col = "blue", lwd = 1.2, 
         ylab = "Volatilidad", xlab = "Fecha", 
         xlim = range(index(SP500)), ylim = c(0, max_volatility),
         xaxt = "n")
    title(main = "Volatilidad Estimada del Modelo GARCH(1,1)")
    axis.Date(1, at = seq(from = start(SP500), to = end(SP500), by = "years"), format = "%Y",
              las = 2, cex.axis = 0.7, tck = -0.02)
  }
  
  # Graficar para GARCH(2,1)
  if (!is.null(models[["GARCH(2,1)"]]$sigmas)) {
    plot(index(SP500)[-1], models[["GARCH(2,1)"]]$sigmas, type = "l", col = "red", lwd = 1.2, 
         ylab = "Volatilidad", xlab = "Fecha", 
         xlim = range(index(SP500)), ylim = c(0, max_volatility),
         xaxt = "n")
    title(main = "Volatilidad Estimada del Modelo GARCH(2,1)")
    axis.Date(1, at = seq(from = start(SP500), to = end(SP500), by = "years"), format = "%Y",
              las = 2, cex.axis = 0.7, tck = -0.02)
  }
  
  # Graficar para GARCH(3,1)
  if (!is.null(models[["GARCH(3,1)"]]$sigmas)) {
    plot(index(SP500)[-1], models[["GARCH(3,1)"]]$sigmas, type = "l", col = "green", lwd = 1.2, 
         ylab = "Volatilidad", xlab = "Fecha", 
         xlim = range(index(SP500)), ylim = c(0, max_volatility),
         xaxt = "n")
    title(main = "Volatilidad Estimada del Modelo GARCH(3,1)")
    axis.Date(1, at = seq(from = start(SP500), to = end(SP500), by = "years"), format = "%Y",
              las = 2, cex.axis = 0.7, tck = -0.02)
  }
  
  # Resetear diseño de gráficos
  par(mfrow = c(1, 1))

  
####################### apartado b
#######################################
  ###############################3
  
# Cargar las librerías necesarias
library(quantmod)
library(tseries)
library(FinTS)
library(fGarch)
library(xts)

# Cargar los datos del SP500
getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = "2021-12-31", periodicity = "daily")

# Extraer los precios de cierre
SP500 <- GSPC$GSPC.Close

# Eliminar datos nulos
SP500 <- na.omit(SP500)

# Calcular los rendimientos (diferencia de logaritmos)
yield <- diff(log(SP500))
yield <- yield * 100
yield <- na.omit(yield)

# Ajustar el modelo GARCH(2,1)
model_garch21 <- garchFit(~arma(1,0) + garch(2,1), data = yield, cond.dist = "norm")

# Obtener residuos estandarizados del modelo GARCH(2,1)
sigmas <- volatility(model_garch21)
z <- residuals(model_garch21) / sigmas
z <- matrix(z, ncol = 1)

# Realizar pruebas Ljung-Box y ARCH
library(FinTS)

# Prueba ARCH con retardos de 10
retardos <- 10
arch_test <- ArchTest(as.vector(z), lags = retardos)
print(arch_test)

# Prueba Ljung-Box con retardos de 10
ljung_box_test <- Box.test(as.vector(z), lag = retardos, type = "Ljung-Box")
print(ljung_box_test)

############################################
###########################################
##############################################
################################################
##################################################


# Cargar las librerías necesarias
if (!require("rugarch")) install.packages("rugarch", dependencies = TRUE)
if (!require("quantmod")) install.packages("quantmod", dependencies = TRUE)
library(rugarch)
library(quantmod)

# Cargar los datos del IBEX35
getSymbols("^IBEX", src = "yahoo", from = "2000-01-01", to = "2021-12-31", periodicity = "daily")

# Extraer los precios de cierre
IBEX35 <- Cl(IBEX)

# Eliminar datos nulos
IBEX35 <- na.omit(IBEX35)

# Calcular los rendimientos (diferencia de logaritmos)
yield <- diff(log(IBEX35))
yield <- yield * 100
yield <- na.omit(yield)

# Definir y ajustar el modelo GARCH(1,1)
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "norm"
)

garch_fit <- ugarchfit(spec = garch_spec, data = yield)

# (a) Presentar en una tabla las estimaciones del modelo
cat("\nEstimaciones del modelo GARCH(1,1):\n")
print(coef(garch_fit))

# Extraer los coeficientes del modelo
coef_garch <- coef(garch_fit)
omega <- coef_garch["omega"]
alpha <- coef_garch["alpha1"]
beta <- coef_garch["beta1"]

# (b) Calcular la varianza incondicional
var_incondicional <- omega / (1 - alpha - beta)
cat("\nVarianza Incondicional:", var_incondicional, "\n")

# (c) Prever la varianza condicional en un horizonte de 10 días
forecast <- ugarchforecast(garch_fit, n.ahead = 10)
var_condicional <- sigma(forecast)^2

# (d) Preparar datos para gráficos
# Obtener las previsiones de la varianza
predicted_var <- as.numeric(var_condicional)

# Crear un vector de fechas para el horizonte de predicción
forecast_dates <- seq(as.Date(index(IBEX35)[length(IBEX35)] + 1, origin = "1970-01-01"), 
                      by = "day", length.out = 10)

# Crear un vector para la varianza incondicional que se ajusta al horizonte de predicción
var_incondicional_vec <- rep(var_incondicional, length(predicted_var))

# Configurar el diseño de gráficos
par(mfrow = c(3, 1), mar = c(5, 4, 2, 1) + 0.1)  # 3 gráficos en 1 columna con márgenes ajustados

# Gráfico de la varianza condicional
plot(forecast_dates, predicted_var, type = "l", col = "blue", lwd = 2, 
     xlab = "Fecha", ylab = "Varianza", 
     main = "Previsión de la Varianza Condicional (10 días)")

# Gráfico de la varianza incondicional
plot(forecast_dates, var_incondicional_vec, type = "l", col = "red", lwd = 2, 
     xlab = "Fecha", ylab = "Varianza", 
     main = "Varianza Incondicional")

# Graficar ambas en un solo gráfico
plot(forecast_dates, predicted_var, type = "l", col = "blue", lwd = 2, 
     xlab = "Fecha", ylab = "Varianza", 
     ylim = range(c(predicted_var, var_incondicional_vec)),  # Ajustar el rango y-lim
     main = "Previsión de la Varianza Condicional y Varianza Incondicional")
lines(forecast_dates, var_incondicional_vec, col = "red", lty = 2)
legend("topright", legend = c("Varianza Condicional (10 días)", "Varianza Incondicional"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 2))

# Resetear diseño de gráficos
par(mfrow = c(1, 1))



###############################################################################
##############################################################################
############################################################################33
#############################################################################


# Cargar las librerías necesarias
library(quantmod)
library(tseries)
library(fGarch)
library(forecast)

# Descargar los datos del índice IBEX35 desde Yahoo Finance
getSymbols("^IBEX", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
ibex_close <- Cl(IBEX)

# Calcular los rendimientos diarios
ibex_returns <- diff(log(ibex_close))

# Eliminar valores NA generados por la función diff
ibex_returns <- na.omit(ibex_returns)

# Modelar la varianza condicional utilizando un modelo APARCH(1,1)
aparch_model <- garchFit(~ arma(1,0) + aparch(1,1), data = ibex_returns, cond.dist = "norm")

# Obtener la volatilidad condicional (desviación estándar condicional)
volatility_cond <- volatility(aparch_model)

# Graficar la volatilidad condicional
plot(index(ibex_returns), volatility_cond, type = "l", col = "blue", lwd = 2, 
     main = "Volatilidad Condicional Estimada por el Modelo APARCH(1,1)", 
     xlab = "Fecha", ylab = "Volatilidad Condicional")

# Agregar una línea horizontal que indique la volatilidad promedio
abline(h = mean(volatility_cond), col = "red", lwd = 2, lty = 2)


#############################################################
################################################################
#########################################################
#################################################################

# Cargar las librerías necesarias
library(quantmod)
library(tseries)
library(fGarch)

# Descargar los datos del índice IBEX35 desde Yahoo Finance
getSymbols("^IBEX", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
ibex_close <- Cl(IBEX)

# Calcular los rendimientos diarios
ibex_returns <- diff(log(ibex_close))

# Eliminar valores NA generados por la función diff
ibex_returns <- na.omit(ibex_returns)

# Ajustar un modelo GARCH(1,1)
garch_model <- garchFit(~ garch(1,1), data = ibex_returns, cond.dist = "norm")

# Obtener la volatilidad condicional (desviación estándar condicional)
volatility_cond <- volatility(garch_model)

# Estandarizar los rendimientos
standardized_returns <- ibex_returns / volatility_cond

# Graficar el histograma de los rendimientos estandarizados
hist(standardized_returns, breaks = 50, probability = TRUE, col = "lightblue",
     main = "Histograma de los Rendimientos Estandarizados del IBEX35",
     xlab = "Rendimientos Estandarizados", ylab = "Densidad")

# Superponer la curva de densidad normal
curve(dnorm(x, mean = mean(standardized_returns), sd = sd(standardized_returns)), 
      add = TRUE, col = "red", lwd = 2)






#############################333
################################
#############################
# comprobacion de que los tres graficos no sean exactamente los mismos

# Instalar y cargar las librerías necesarias
if (!requireNamespace("rugarch", quietly = TRUE)) {
  install.packages("rugarch")
}
library(rugarch)
library(quantmod)
library(xts)

# Cargar los datos del SP500
getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = "2021-12-31", periodicity = "daily")

# Extraer los precios de cierre
SP500 <- GSPC$GSPC.Close

# Eliminar datos nulos
SP500 <- na.omit(SP500)

# Calcular los rendimientos (diferencia de logaritmos)
yield <- diff(log(SP500))
yield <- yield * 100
yield <- na.omit(yield)

# Definir una función para ajustar el modelo GARCH(p,1) y calcular el AIC
adjust_garch_model <- function(yield, p) {
  # Definir especificaciones del modelo GARCH(p,1)
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(p, 1)),
    mean.model = list(armaOrder = c(1, 0)),
    distribution.model = "norm"
  )
  
  # Ajustar el modelo
  modelo <- tryCatch({
    ugarchfit(spec, yield)
  }, error = function(e) {
    message(paste("Error en el ajuste del modelo GARCH(", p, ",1):", e$message))
    NULL
  })
  
  # Calcular el AIC si el modelo se ajustó correctamente
  if (!is.null(modelo)) {
    infocrit <- infocriteria(modelo)
    aic <- infocrit["Akaike"]
    sigmas <- sigma(modelo)
    list(modelo = modelo, AIC = aic, sigmas = sigmas)
  } else {
    list(modelo = NULL, AIC = NA, sigmas = NULL)
  }
}

# Ajustar modelos GARCH(1,1), GARCH(2,1), y GARCH(3,1)
models <- list()
for (p in 1:3) {
  models[[paste("GARCH(", p, ",1)", sep = "")]] <- adjust_garch_model(yield, p)
}

# Imprimir coeficientes y AIC para cada modelo
for (name in names(models)) {
  model <- models[[name]]
  cat("\nModelo:", name, "\n")
  if (!is.null(model$modelo)) {
    cat("AIC:", round(model$AIC, 2), "\n")  # Redondear el AIC a 2 decimales
    cat("Coeficientes:\n")
    print(coef(model$modelo))
  } else {
    cat("No se pudo ajustar el modelo.\n")
  }
}

# Calcular el máximo de las volatilidades para definir el límite superior del eje y
max_volatility <- max(sapply(models, function(m) if (!is.null(m$sigmas)) max(m$sigmas, na.rm = TRUE) else 0))

# Graficar las volatilidades estimadas en gráficos separados

# Crear un diseño de gráficos en 3 filas y 1 columna
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1) + 0.1)

# Graficar para GARCH(1,1)
if (!is.null(models[["GARCH(1,1)"]]$sigmas)) {
  plot(index(SP500)[-1], models[["GARCH(1,1)"]]$sigmas, type = "l", col = "blue", lwd = 1.2, 
       ylab = "Volatilidad", xlab = "Fecha", 
       xlim = range(index(SP500)), ylim = c(0, max_volatility),
       xaxt = "n")
  title(main = "Volatilidad Estimada del Modelo GARCH(1,1)")
  axis.Date(1, at = seq(from = start(SP500), to = end(SP500), by = "years"), format = "%Y",
            las = 2, cex.axis = 0.7, tck = -0.02)
  legend("topright", legend = "GARCH(1,1)", col = "blue", lwd = 1.2)
}

# Graficar para GARCH(2,1)
if (!is.null(models[["GARCH(1,1)"]]$sigmas)) {
  plot(index(SP500)[-1], models[["GARCH(1,1)"]]$sigmas, type = "l", col = "red", lwd = 1.2, 
       ylab = "Volatilidad", xlab = "Fecha", 
       xlim = range(index(SP500)), ylim = c(0, max_volatility),
       xaxt = "n")
  title(main = "Volatilidad Estimada del Modelo GARCH(1,1)")
  axis.Date(1, at = seq(from = start(SP500), to = end(SP500), by = "years"), format = "%Y",
            las = 2, cex.axis = 0.7, tck = -0.02)
  legend("topright", legend = "GARCH(1,1)", col = "red", lwd = 1.2)
}

# Graficar para GARCH(3,1)
if (!is.null(models[["GARCH(1,1)"]]$sigmas)) {
  plot(index(SP500)[-1], models[["GARCH(1,1)"]]$sigmas, type = "l", col = "green", lwd = 1.2, 
       ylab = "Volatilidad", xlab = "Fecha", 
       xlim = range(index(SP500)), ylim = c(0, max_volatility),
       xaxt = "n")
  title(main = "Volatilidad Estimada del Modelo GARCH(3,1)")
  axis.Date(1, at = seq(from = start(SP500), to = end(SP500), by = "years"), format = "%Y",
            las = 2, cex.axis = 0.7, tck = -0.02)
  legend("topright", legend = "GARCH(1,1)", col = "green", lwd = 1.2)
}

# Resetear diseño de gráficos
par(mfrow = c(1, 1))


#AQUI SE REPRESENTAN LAS TRES VECES EL MISMO GARCH(1,1) Y SI SE COMPARAN LOS GRAFICOS SI QUE CAMBIA
#MINIMAMENTE