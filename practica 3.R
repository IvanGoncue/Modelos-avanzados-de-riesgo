# Cargar las librerías necesarias
library(quantmod)
library(tseries)
library(FinTS)

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

# Ejercicio 1: Calcular la varianza condicional de los rendimientos y representarla gráficamente

# Función para calcular la Volatilidad Histórica (VH)
vol_hist <- function(yield, k) {
  VH <- rep(NA, length(yield) - k + 1)
  for (i in 1:(length(yield) - k + 1)) {
    VH[i] <- var(yield[i:(i + k - 1)])
  }
  return(VH)
}

# Calcular VH para ventanas de 100, 250 y 500
VH_100 <- vol_hist(yield, 100)
VH_250 <- vol_hist(yield, 250)
VH_500 <- vol_hist(yield, 500)

# Calcular el índice de tiempo para cada VH
time_index_100 <- index(yield)[-c(1:99)]
time_index_250 <- index(yield)[-c(1:249)]
time_index_500 <- index(yield)[-c(1:499)]

# Ejercicio 2: Calcular la varianza condicional con el modelo EWMA
# Función para calcular EWMA
ewma <- function(yield, lambda) {
  EWMA <- rep(NA, length(yield))
  EWMA[1] <- var(yield)
  for (i in 2:length(yield)) {
    EWMA[i] <- lambda * EWMA[i - 1] + (1 - lambda) * yield[i - 1]^2
  }
  return(EWMA)
}

# Calcular EWMA con lambda = 0.94
lambda <- 0.94
EWMA <- ewma(yield, lambda)

# Ajustar el índice de tiempo para EWMA
time_index_EWMA <- index(yield)[1:length(EWMA)]

# Eliminar valores no finitos de EWMA y ajustar el índice de tiempo
valid_indices <- which(!is.na(EWMA))
EWMA <- EWMA[valid_indices]
time_index_EWMA <- time_index_EWMA[valid_indices]

# Verificar que las longitudes coincidan
if(length(time_index_EWMA) != length(EWMA)) {
  stop("Las longitudes de time_index_EWMA y EWMA no coinciden.")
}

# Gráfica de la volatilidad estimada con el modelo EWMA
plot(time_index_EWMA, EWMA, type = "l", col = "red", main = "Volatilidad Estimada con EWMA", ylab = "Volatility", xlab = "")

# Ajustar longitudes para comparabilidad
min_length <- min(length(VH_100), length(VH_250), length(VH_500), length(EWMA))
VH_100 <- VH_100[(length(VH_100) - min_length + 1):length(VH_100)]
VH_250 <- VH_250[(length(VH_250) - min_length + 1):length(VH_250)]
VH_500 <- VH_500[(length(VH_500) - min_length + 1):length(VH_500)]
EWMA <- EWMA[(length(EWMA) - min_length + 1):length(EWMA)]
time_index <- time_index_EWMA[(length(time_index_EWMA) - min_length + 1):length(time_index_EWMA)]

# Gráfico conjunto de VH y EWMA
plot(time_index, VH_100, type = "l", main ="Volatilidad Histórica (VH) y EWMA", col = "blue", lwd = 1.2, ylab = "Volatility", xlab = "", ylim = c(0, max(c(VH_100, VH_250, VH_500, EWMA))))
lines(time_index, VH_250, type = "l", col = "green")
lines(time_index, VH_500, type = "l", col = "purple")
lines(time_index, EWMA, type = "l", col = "red")
legend("topright", legend = c("VH_100", "VH_250","VH_500", "EWMA"), col = c("blue", "green", "purple", "red"), lty = 1, cex = 0.5)

# Gráficos individuales para cada ventana de VH
# Ajustar longitudes para los gráficos individuales

# VH_100
time_index_100 <- time_index[(length(time_index) - length(VH_100) + 1):length(time_index)]
plot(time_index_100, VH_100, type = "l", col = "blue", main = "Volatilidad Histórica (VH) - Ventana 100", ylab = "Volatility", xlab = "")

# VH_250
time_index_250 <- time_index[(length(time_index) - length(VH_250) + 1):length(time_index)]
plot(time_index_250, VH_250, type = "l", col = "green", main = "Volatilidad Histórica (VH) - Ventana 250", ylab = "Volatility", xlab = "")

# VH_500
time_index_500 <- time_index[(length(time_index) - length(VH_500) + 1):length(time_index)]
plot(time_index_500, VH_500, type = "l", col = "purple", main = "Volatilidad Histórica (VH) - Ventana 500", ylab = "Volatility", xlab = "")