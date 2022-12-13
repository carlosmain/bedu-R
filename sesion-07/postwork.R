"OBJETIVO
Estimar modelos ARIMA y realizar predicciones
DESARROLLO
Utilizando el siguiente vector numérico, realiza lo que se indica:"
  
url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"
Global <- scan(url, sep="")
"1. Crea una objeto de serie de tiempo con los datos de Global. La serie debe 
ser mensual comenzado en Enero de 1856"
Global.ts <- ts(Global, start = c(1856,1), freq = 12)

"2. Realiza una gráfica de la serie de tiempo anterior de 2005"
Global.ts2 <- ts(Global, start = c(1856,1), end=c(2004,12), freq=12)
plot(Global.ts2, 
     main = "?", 
     xlab = "Tiempo",
     sub = "Enero de 1856 - Diciembre de 2004")

"3. Ahora realiza una gráfica de la serie de tiempo anterior, transformando a 
la primera diferencia:"
plot(diff(Global.ts2), type = "l", main = "Primera diferencia de X", 
     xlab = "t", ylab = expression(x[t]), 
     sub = expression(x[t]==x[t-1]+w[t]))

"4. ¿Consideras que la serie es estacionaria en niveles o en primera 
diferencia?"
#Es casi estacionaria, excepto por los primeros años

"5. Con base en tu respuesta anterior, obten las funciones de autocorrelación 
y autocorrelación parcial?"
acf(diff(Global.ts2))
pacf(diff(Global.ts2))

arima(Global.ts2, order = c(1, 1, 1))
arima(Global.ts2, order = c(1, 1, 2))
arima(Global.ts2, order = c(1, 1, 3))
arima(Global.ts2, order = c(1, 1, 4))

fit <- arima(Global.ts2, order = c(1, 1, 4))
pr <- predict(fit, 12)$pred
