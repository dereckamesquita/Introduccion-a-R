library(readr)
orangeec <- read_csv("orangeec.csv")
x=1:10
y=2*x+5
plot(x,y, type="p")
plot(x,y, type="l")
plot(x,y, type="b")
plot(x,y, type="c")
plot(x,y, type="o")
plot(x,y, type="h")
plot(x,y, type="s")
plot(x,y, type="S")
mtcars <- read_csv("mtcars.csv")
View(mtcars)
#Conocer que tipos de variables tenemos
str(mtcars)
#Obtener la documentacion de la variable
?mtcars
#Obtener la clase de la variable
class(mtcars$vs)
  #vs es una variable dicotomica, haremos la correcion y la volvemos logica
mtcars$vs= as.logical(mtcars$vs)
class(mtcars$vs)
mtcars$vs
#Comprobamos
str(mtcars)

"Continuamos viendo orangeec
"
str(orangeec)
#Obtener datos estadisticos
summary(orangeec)
summary(mtcars)
#convertir el peso
wt=(mtcars$wt*1000/2)
wt
mtcarsnew <- transform(mtcars,wt=mtcars$wt*1000/2)
wt
summary(mtcarsnew)

"
Vectores
"
tiempox <-c(10,5,5,4,3)
tiempoy <-c(10,20,5,4,3)
tiempototal<-c (tiempox+tiempoy)
summary(tiempox)
tiempox
tiempodias <-c("lunes","martes","miercoles","jueves","viernes")
sumatiempo<-sum(tiempototal)
sumatiempo
#Una matriz tiene un solo tipo de datos. Un data frame puede tener datos logicos o numericos

tiempo_matriz<-matrix(c(tiempox,tiempoy),
                      nrow=2,byrow=TRUE)

tiempo<-c("tiempoX","tiempoY")
colnames(tiempo_matriz)<-tiempodias
rownames(tiempo_matriz)<-tiempo
tiempo_matriz

#Ejemplo propio con VECTORES
alumnos <-c("Ramiro", "Juan","Roberto")
Cursos <-c("matematica","comunicacion","ambiente")
notas1<-c(45,18,70)
notas2<-c(15,58,90)
notas3<-c(16,38,60)
plot(notas1, notas2)
notas<-matrix(c(notas1,notas2,notas3), nrow =3,byrow=TRUE )
rownames(notas)<-alumnos
colnames(notas)<-Cursos
notas
#hallar la suma de una columna
colSums(notas)
#hallar la media de una columna
colMeans(notas)
#agregar fila a la matriz
fnotas <- rbind(notas,c(40,50.40))
fnotas
#Conocer cualquier valor de la matriz
fnotas[3:2] #desde la tercera posicion hasta la segunda
fnotas[8:10]
fnotas[1,2] #la ubicacion fila 1 columna 2
"
== igualdad
!= No igual
< menor
<= menor igual
| o
! no
%in% que esta dentro de un dataset"
# Filtrando segun cierta condicion en una variable
mtcars[mtcars$cyl<6,]
mtcars

#Filtrando y creando un nuevo dataset
neworange<- subset(orangeec, `Internet penetration % population` >80 & `Education invest % GDP`>=4.5)
neworange

#seleccionaremos columnas en especifico

neworange<- subset(orangeec, `Internet penetration % population` >80 & `Education invest % GDP`>=4.5, select=`Creat Ind % GDP`)
neworange
#cambiar el nombre a una columna primera forma
  #Le damos la ubicacion con numero
colnames(orangeec)[2] <- 'd'  
orangeec
#cambiar el nombre a una columna segunda forma
  #Le damos la ubicacion entrando otra vez a la matriz

colnames(orangeec)[colnames(orangeec) == "d"] <- "z"
orangeec
#regresamos a nuestro dataset
orangeec <- read_csv("orangeec.csv")
#mostrar primeros 6 elementos del dataset
head(mtcars)
head(orangeec)
#mostrar ultimos 6 elementos del dataset
tail(mtcars)
tail(orangeec)

library(dplyr)

glimpse(orangeec)
#dbl es numerico

vector=1:8
vector
matriz<-matrix(1:9, ncol = 3)
matriz
datafra<-mtcars[1:4,]
datafra
# List nos muestra todo lo que esta dentro
lista<-list(vector,matriz,datafra)
lista
