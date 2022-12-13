"Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre como 
mejorar las ventas de un producto particular, y el conjunto de datos con el que 
disponemos son datos de publicidad que consisten en las ventas de aquel 
producto en 200 diferentes mercados, junto con presupuestos de publicidad para 
el producto en cada uno de aquellos mercados para tres medios de comunicación 
diferentes: TV, radio, y periódico. No es posible para nuestro cliente 
incrementar directamente las ventas del producto. Por otro lado, ellos pueden 
controlar el gasto en publicidad para cada uno de los tres medios de 
comunicación. Por lo tanto, si determinamos que hay una asociación entre 
publicidad y ventas, entonces podemos instruir a nuestro cliente para que 
ajuste los presupuestos de publicidad, y así indirectamente incrementar las 
ventas.

En otras palabras, nuestro objetivo es desarrollar un modelo preciso que pueda 
ser usado para predecir las ventas sobre la base de los tres presupuestos de 
medios de comunicación. Ajuste modelos de regresión lineal múltiple a los datos 
advertisement.csv y elija el modelo más adecuado siguiendo los procedimientos 
vistos"
adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")

"Considera:
  
Y: Sales (Ventas de un producto)
X1: TV (Presupuesto de publicidad en TV para el producto)
X2: Radio (Presupuesto de publicidad en Radio para el producto)
X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)"

"Realizamos un attach para poder acceder a las variables sin tener que llamar 
al dataframe"
attach(adv)

# A continuación mostramos una matriz de gráficos de dispersión de los
# tres predictores continuos y la variable de respuesta. 

pairs(~ Sales + TV + Radio + Newspaper, data = adv, gap = 0.4, cex.labels = 2.5)

# Llevamos a cabo el ajuste de un modelo
# Sales = beta0 + beta1*TV + beta2*Radio + beta3*Newspaper + e
" Se ajusta a un modelo de regresión lineal simple y lo guardamos en una nueva 
variable"

modelo01 <- lm(Sales ~ TV + Radio + Newspaper)

"Usamos el comando summary para obtener el resumen de nuestro nuevo modelo
ajustado a la regresion lineal simple"
summary(modelo01)

"Podemos observar que el coeficiente de regresión de la variable Newspaper,
no es nada significativo por lo que ajustaremos nuestro modelo sin tomar en 
cuenta esa variable"

modelo02 <- update(modelo01, ~.-Newspaper)
summary(modelo02)

residuales <- rstandard(modelo02)
par(mfrow = c(2, 2))
plot(TV, residuales, ylab = "Residuales Estandarizados")
plot(Radio, residuales, ylab = "Residuales Estandarizados")

" Usamos el comando qqnorm para graficar la correlación entre nuestra variable
y la distribución normal"
qqnorm(residuales)
qqline(residuales)

dev.off()

"Usamos el test de shapiro para determinar si una muestra proviene o no de una 
distribución normal"
shapiro.test(residuales)

"En este caso el valor de p es menor que 0.5 por lo que podemos inferir que
nuestros datos no provienen de una muestra que se distribuye normalmente"

# Ahora consideramos ina interacción de TV y Radio, y creamos un nuevo modelo

modelo03 <- lm(Sales ~ TV + Radio + TV:Radio)

summary(modelo03)

"Hacemos una prueba parcial para comparar nuestro modelo con interacciones, y 
comparandolo contro nuestro modelo que no contiene la variable NewsPaper, ya que
anteriormente observamos que su participación no era esencial"
anova(modelo02,modelo03)

"Si ponemos nuestras hipotesís, al principio un hipotesís nula de HO, tomando
en cuenta el modelo02 como si fuera = 0, y nuestra hipotesís H1 que rechaza la
hipotesís H0. Observando el valor de p = 7.633e-07, rechazamos la HO y usamos 
nuestra alternativa de H1"

residuales03 <- rstandard(modelo03)
par(mfrow = c(2, 2))
plot(TV, residuales03, ylab = "Residuales Estandarizados")
plot(Radio, residuales03, ylab = "Residuales Estandarizados")

qqnorm(residuales03)
qqline(residuales03)

shapiro.test(residuales03)

