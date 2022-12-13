"Objetivo
Realizar un análisis probabilístico del total de cargos internacionales de una compañía de telecomunicaciones
Requisitos
- R, RStudio
- Haber trabajado con el prework y el work

Desarrollo
Utilizando la variable total_intl_charge de la base de datos 
telecom_service.csv de la sesión 3" 
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")

"realiza un análisis probabilístico. Para ello, debes determinar la función de 
distribución de probabilidad que más se acerque el comportamiento de los datos. 
Hint: Puedes apoyarte de medidas descriptivas o técnicas de visualización."
Mode(df$total_intl_charge)[1]
mean(df$total_intl_charge)
median(df$total_intl_charge)

hist(df$total_intl_charge, prob=T, main="Histograma total cargos internacionales")

media <- mean(df$total_intl_charge)
desvstd <- sd(df$total_intl_charge)

"Una vez que hayas seleccionado el modelo, realiza lo siguiente:
  
1. Grafica la distribución teórica de la variable aleatoria total_intl_charge"
curve(dnorm(x, mean = media, sd = desvstd), from=0, to=5, 
      col='red', main = "Probabilidad Normal",
      ylab = "f(x)", xlab = "X")

"2. ¿Cuál es la probabilidad de que el total de cargos internacionales sea 
menor a 1.85 usd?"
pnorm(1.85, mean=media, sd=desvstd, lower.tail = T)
  
"3. ¿Cuál es la probabilidad de que el total de cargos internacionales sea 
mayor a 3 usd?"
pnorm(3, mean=media, sd=desvstd, lower.tail=F)
  
"4. ¿Cuál es la probabilidad de que el total de cargos internacionales esté 
entre 2.35usd y 4.85 usd?"
pnorm(4.85, mean=media, sd=desvstd) - pnorm(2.35, mean=media, sd=desvstd)
  
"5. Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales 
más alto que podría esperar?"
qnorm(p=0.48, mean=media, sd=desvstd)
  
"6. ¿Cuáles son los valores del total de cargos internacionales que dejan 
exactamente al centro el 80% de probabilidad?"
qnorm(p=0.10, mean=media, sd=desvstd)
qnorm(p=0.90, mean=media, sd=desvstd)
