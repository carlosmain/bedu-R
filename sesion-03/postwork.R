"Objetivo
Realizar un análisis descriptivo de las variables de un dataframe
Requisitos
1. R, RStudio
2. Haber realizado el prework y seguir el curso de los ejemplos de la sesión
3. Curiosidad por investigar nuevos tópicos y funciones de R

Desarrollo
Utilizando el dataframe boxp.csv realiza el siguiente análisis descriptivo. No 
olvides excluir los missing values y transformar las variables a su tipo y 
escala correspondiente."

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")

str(df)
summary(df)
" Convertir en factor los datos en categoria"
df$Categoria <- factor(df$Categoria)
df$Grupo <- factor(df$Grupo)

"Revisar si hay datos con NA's esto también no los da la función summary"
complete.cases(df)
df.clean <- df[complete.cases(df),]
complete.cases(df.clean)
summary(df.clean)

library(DescTools)

"1) Calcula e interpreta las medidas de tendencia central de la variable 
Mediciones"
mean(df.clean$Mediciones) 
"62.88494 el promedio de una medición fue"
median(df.clean$Mediciones)
"49.3 el 50% tiene una medición menor o igual a 49.3"
Mode(df.clean$Mediciones)
"23.3 La medición más frecuente esta"

"1.1 Con base en tu resultado anteior, ¿qué se puede concluir respecto al sesgo 
de Mediciones?"
"Tiene un sesgo a la derecha, , esto se puede inferir debido que la media, es 
mayor a la mediana y la moda"

"1.2 Calcula e interpreta la desviación estándar y los cuartiles de la 
distribución de Mediciones"
var(df.clean$Mediciones)
sd(df.clean$Mediciones)
"Se tiene una dispersión promedio, respecto a la media, de 53.7697"

cuartiles <- quantile(df.clean$Mediciones, probs = c(0.25, 0.50, 0.75))
cuartiles
"El 25% de los diamantes tienen una medición de 23.45 o menos, el 50% tiene 
una medición de 49.30 o menos, y el 75% de los diamantes tienen una medición de
82.85% o menos"

"1.3 Con ggplot, realiza un histograma separando la distribución de Mediciones 
por Categoría ¿Consideras que sólo una categoría está generando el sesgo?"
library(ggplot2)

hist(df.clean$Mediciones)
ggplot(df.clean, aes(x = Mediciones, fill=Categoria)) +
  geom_histogram(bins = 4) + 
  labs(title = "Histograma", x = "Mediciones", y = "Frequency") + 
  theme_classic()

"1.4 Con ggplot, realiza un boxplot separando la distribución de Mediciones por 
Categoría y por Grupo dentro de cada categoría. 
¿Consideras que hay diferencias entre categorías? 
¿Los grupos al interior de cada categoría podrían estar generando el sesgo?"
ggplot(df, aes(x=Categoria, y=Mediciones, fill=Grupo)) + 
  geom_boxplot() +
  theme_classic()

"El grupo 0 en la categoría C2 y C3 esta creando un sesgo"