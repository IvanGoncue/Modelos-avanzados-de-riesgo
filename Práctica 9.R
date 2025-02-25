library(quantmod)
getSymbols("AAPL", from = "2002-01-01", to = "2021-12-31")
returns <- dailyReturn(Ad(AAPL))
alpha <- 0.05
VaR <- quantile(returns, probs = alpha)
ES <- mean(returns[returns <= VaR])
VaR
ES

#######################

# Graficar los rendimientos
plot(index(returns), coredata(returns), type = "l", col = "black",
     main = "Rendimientos de APPLE con VaR y ES",
     xlab = "Fecha", ylab = "Rendimientos")

# A�adir l�nea para VaR
abline(h = VaR, col = "red", lty = 2)

# A�adir l�nea para ES
abline(h = ES, col = "blue", lty = 2)

# A�adir leyenda
legend("topright", legend = c("Rendimientos", "VaR", "ES"),
       col = c("black", "red", "blue"), lty = 1:2)

##############################3
##parametrico bajo normalidad

# Par�metros de la distribuci�n normal
mu <- 0.5
sigma <- 1.5
alpha <- 0.05  # nivel de significancia

# Calcular el VaR (percentil 5%) usando la distribuci�n normal
VaR <- mu + sigma * qnorm(alpha)

# Calcular el ES (esperanza condicional)
# Primero, se calcula la funci�n de densidad normal est�ndar en el percentil VaR
normal_density <- dnorm(qnorm(alpha))
# Luego, se usa la f�rmula para calcular el ES
ES <- mu - sigma * (normal_density / alpha)

# Imprimir resultados
print(paste("VaR bajo distribuci�n normal:", VaR))
print(paste("ES bajo distribuci�n normal:", ES))


#######################################3
########################################
### parametrico t studente

# Par�metros de la distribuci�n t-Student
mu <- 0.5
sigma <- 1.5
df <- 3  # grados de libertad
alpha <- 0.05  # nivel de significancia

# Calcular el VaR (percentil 5%) usando la distribuci�n t-Student
VaR <- mu + sigma * qt(alpha, df)

# Calcular el ES (esperanza condicional)
# Primero, se calcula la funci�n de densidad t-Student en el percentil VaR
t_density <- dt(qt(alpha, df), df)
# Luego, se usa la f�rmula para calcular el ES
ES <- mu + sigma * (t_density / alpha) * (df + qt(alpha, df)^2) / (df - 1)

# Imprimir resultados
print(paste("VaR bajo distribuci�n t-Student:", VaR))
print(paste("ES bajo distribuci�n t-Student:", ES))



#######################################3
########################################
### parametrico t studente asimetrica

# Instalar y cargar el paquete sn si a�n no est� instalado
if (!requireNamespace("sn", quietly = TRUE)) install.packages("sn")
library(sn)

# Par�metros de la distribuci�n asim�trica
mu <- 0.5
sigma <- 1.5
df <- 3
xi <- 8  # Par�metro de asimetr�a

# Generar una muestra de datos siguiendo la distribuci�n t-Student asim�trica
set.seed(123)  # Para reproducibilidad
sample_size <- 1000  # Tama�o de la muestra
sample_data <- rsn(sample_size, xi = xi, mean = mu, sd = sigma, nu = df)

# Definir el nivel de significancia
alpha <- 0.05

# Calcular VaR (percentil 5%)
VaR <- qsn(alpha, xi = xi, mean = mu, sd = sigma, nu = df)

# Calcular ES (esperanza condicional)
ES <- mu + sigma * (dsn(qsn(alpha, xi = xi, mean = mu, sd = sigma, nu = df), xi = xi, mean = mu, sd = sigma, nu = df) / alpha)

# Imprimir resultados
print(paste("VaR bajo distribuci�n t-Student asim�trica:", VaR))
print(paste("ES bajo distribuci�n t-Student asim�trica:", ES))



##################################################
#############################################
############### EVT POT


# Instalar y cargar los paquetes necesarios
if (!require(quantmod)) install.packages("quantmod")
if (!require(evd)) install.packages("evd")
library(quantmod)
library(evd)

# Descargar datos del �ndice HSI
getSymbols("^HSI", src = "yahoo", from = "2000-01-01", to = "2021-12-31")
hsi_returns <- dailyReturn(Cl(HSI))

# Convertir los rendimientos en un vector num�rico
returns <- as.numeric(hsi_returns)

# Definir el umbral como el percentil 94
threshold <- quantile(returns, probs = 0.94)

# Aplicar el m�todo POT para ajustar la distribuci�n GPD
excess_returns <- returns[returns > threshold] - threshold
fit_gpd <- fgev(excess_returns)

# Calcular VaR (percentil 5%) usando la distribuci�n GPD ajustada
alpha <- 0.05
VaR_pot <- threshold + qgev(alpha, loc = fit_gpd$estimate["loc"], scale = fit_gpd$estimate["scale"], shape = fit_gpd$estimate["shape"])

# Calcular ES (esperanza condicional) usando la distribuci�n GPD ajustada
ES_pot <- threshold + (fit_gpd$estimate["scale"] / (1 - fit_gpd$estimate["shape"])) * (1 / alpha) * (1 + fit_gpd$estimate["shape"] * qgev(alpha, loc = fit_gpd$estimate["loc"], scale = fit_gpd$estimate["scale"], shape = fit_gpd$estimate["shape"]))

# Imprimir resultados
print(paste("VaR usando Teor�a de Valores Extremos incondicional:", VaR_pot))
print(paste("ES usando Teor�a de Valores Extremos incondicional:", ES_pot))





###########################################################
#33##################################################
######################### EVT condicional



# Instalar y cargar las librer�as necesarias
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("fGarch", quietly = TRUE)) install.packages("fGarch")
if (!requireNamespace("fitdistrplus", quietly = TRUE)) install.packages("fitdistrplus")
if (!requireNamespace("evd", quietly = TRUE)) install.packages("evd")

library(quantmod)
library(fGarch)
library(fitdistrplus)
library(evd)

# Cargar los datos del �ndice HSI
getSymbols("^HSI", src = "yahoo", from = "2000-01-01", to = "2021-12-31")

# Extraer los precios de cierre
HSI <- Cl(HSI)

# Eliminar datos nulos
HSI <- na.omit(HSI)

# Calcular los rendimientos (diferencia de logaritmos)
returns <- diff(log(HSI)) * 100
returns <- na.omit(returns)

# Multiplicar los rendimientos por -1 para posiciones largas
returns_neg <- -returns

# Ajustar un modelo APARCH(1,1) para obtener la varianza condicional
aparch_fit <- garchFit(
  formula = ~ aparch(1, 1),
  data = returns_neg,
  include.mean = TRUE,
  cond.dist = "std",
  trace = FALSE
)



# Obtener la desviaci�n est�ndar condicional
sigma_t <- sqrt(aparch_fit@sigma.t^2)

# Estandarizar los rendimientos
standardized_returns <- returns_neg / sigma_t

# Fijar el umbral (percentil 94 de los rendimientos estandarizados)
threshold <- quantile(standardized_returns, probs = 0.94)

# Seleccionar los datos que est�n por encima del umbral y ajustar la GPD
excesses <- standardized_returns[standardized_returns > threshold] - threshold

# Ajustar la distribuci�n GPD a los excesos
gpd_fit <- fgev(excesses)
shape <- gpd_fit$estimate["shape"]
scale <- gpd_fit$estimate["scale"]
loc <- gpd_fit$estimate["loc"]

# Calcular el VaR condicional al 95%
conf_level <- 0.95
percentile <- 1 - conf_level

# Inicializar vector para VaR condicional
VaR_conditional_95 <- rep(NA, length(returns))

for (i in 1:length(returns)) {
  if (standardized_returns[i] > threshold) {
    # Para excesos, usamos la GPD
    VaR_conditional_95[i] <- -sigma_t[i] * (threshold + qgev(percentile, loc = loc, scale = scale, shape = shape))
  } else {
    # Para datos no excesivos, simplemente usamos la desviaci�n est�ndar condicional
    VaR_conditional_95[i] <- -sigma_t[i] * qnorm(conf_level)
  }
}

# Crear un gr�fico para visualizar los rendimientos y el VaR condicional al 95%
plot(index(returns), returns, type = 'l', col = 'orange', lty = 1, ylab = 'Rendimientos', xlab = 'Fecha', main = 'Rendimientos y VaR Condicional al 95% usando GPD')
lines(index(returns), VaR_conditional_95, col = 'black', lty = 2)
legend("topright", legend = c("Rendimientos", "VaR Condicional al 95%"), col = c("orange", "black"), lty = c(1, 2))

# Calcular el VaR condicional al 99%
conf_level <- 0.99
percentile <- 1 - conf_level

# Inicializar vector para VaR condicional
VaR_conditional_99 <- rep(NA, length(returns))

for (i in 1:length(returns)) {
  if (standardized_returns[i] > threshold) {
    # Para excesos, usamos la GPD
    VaR_conditional_99[i] <- -sigma_t[i] * (threshold + qgev(percentile, loc = loc, scale = scale, shape = shape))
  } else {
    # Para datos no excesivos, simplemente usamos la desviaci�n est�ndar condicional
    VaR_conditional_99[i] <- -sigma_t[i] * qnorm(conf_level)
  }
}

# Crear un gr�fico para visualizar los rendimientos y el VaR condicional al 99%
plot(index(returns), returns, type = 'l', col = 'orange', lty = 1, ylab = 'Rendimientos', xlab = 'Fecha', main = 'Rendimientos y VaR Condicional al 99% usando GPD')
lines(index(returns), VaR_conditional_99, col = 'black', lty = 2)
legend("topright", legend = c("Rendimientos", "VaR Condicional al 99%"), col = c("orange", "black"), lty = c(1, 2))
