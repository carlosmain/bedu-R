"OBJETIVO
Realizar inferencia estadística para extraer información de la muestra que sea 
contrastable con la población
REQUISITOS
= Haber desarrollado los postworks anteriores
= Cubrir los temas del prework
= Replicar los ejemplos de la sesión
DESARROLLO
El data frame iris contiene información recolectada por Anderson sobre 50 
flores de 3 especies distintas (setosa, versicolor y virginca), 
incluyendo medidas en centímetros del largo y ancho del sépalo así como de los 
pétalos.

Estudios recientes sobre las mismas especies muestran que:
  
1) En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual 
a 5.7 cm"

"Primero establecemos nuestras hipotesís"
Ho: prom_sepal_length_setosa == 5.7
Ha: prom_sepal_length_setosa =! 5.7

t.test(iris[iris$Species == 'setosa', "Sepal.Length"], 
            alternative = 'two.sided', mu=5.7)

"Establecemos con 95% de confiabilidad que la media no es igual a 5.7"

"2) En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es 
menor a 2.1 cm"
"Primero establecemos nuestras hipotesís"
Ho: prom_petal_width == 2.1
Ha: prom_petal_width =! 2.1

t.test(iris[iris$Species == 'virginica', "Petal.Width"], 
       alternative = 'less', mu=2.1)

"Establecemos con un 95% de confiabilidad que la media es menor a 2.1 cm"

"3) En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande 
que el promedio del largo del pétalo de la especie versicolor."
Ho: prom_petal_width == 1.1
Ha: prom_petal_width =! 1.1
var.test(iris[iris$Species == "virginica", "Petal.Length"], 
         iris[iris$Species == "versicolor", "Petal.Length"], 
         ratio = 1, alternative = "two.sided")

t.test(iris[iris$Species == "virginica", "Petal.Length"],
       iris[iris$Species == "versicolor", "Petal.Length"],
       alternative = "greater", mu = 1.1, var.equal = TRUE)


"4) En promedio, no existe diferencia en el ancho del sépalo entre las 3 
especies."

boxplot(Sepal.Width ~ Species, data = iris)

anova <- aov(Sepal.Width ~ Species,
             data = iris)

summary(anova)

"Utilizando pruebas de inferencia estadística, concluye si existe evidencia 
suficiente para concluir que los datos recolectados por Anderson están en 
línea con los nuevos estudios.

Utiliza 99% de confianza para toda las pruebas, en cada caso realiza el 
planteamiento de hipótesis adecuado y concluye."