library(readr)
orangeec <- read_csv("orangeec.csv")
mtcars <- read_csv("mtcars.csv")
#Funcion Pairs (todas la variables)
#Pairs necesita los numeros de cada columna, si usamos coma se entera como todas
pairs(mtcars[,2:6])
"Estamos pidiendo todas la filas (,), y las columnas de la 2 a la 6"
"Interpretar los cuadros de pairs,el nombre (ejemplo mpg) para arriba y abajo es X, 
para los costados es y"
library(dplyr)
#Ordenamos la data
#FIltrado de data con subset
nueva <- subset(mtcars, select = c(2,7:8,11,12))
#estamos pidiendo la columna 2, desde la 7 a la8, la 11 y la 12
pairs(nueva)

#seleccionamos ciertas columnas
pairs(mtcars[,-c(1,3,4,5,6,9,10)])
#Estamos pidiendo todas las filas, menos las columnas 1,3 ...

Eficientes <-filter(mtcars, mpg>=30)
Eficientes
pairs(Eficientes[,2:6])

#Nuevo filtrado por palabras
library(stringr)
merc<- mtcars %>%
  filter(str_detect(model, "Merc"))
merc        

pairs(merc[,2:6])

#Uso de la funcion correlacion
cor(merc[,2:6])

cor(nueva)

#Eliminar problema de datos ausentes con use
cor(orangeec[,2:6], use = "complete.obs")

#cuidado con el promedio
"Usamos el coeficiente de variacion, un optimo es menor del 25%"
CV=sd(mtcars$mpg)/mean(mtcars$mpg)
CV
"los datos estan desviados en un 29.9%"

#Eliminar valores ausentes con na.rm (Na remove)
mean(orangeec$`Creat Ind % GDP`, na.rm=TRUE)


#ajustar datos para mejorar visualizaciones
    
eficientes<-mean(mtcars$mpg)

mtcars <- mtcars %>%
  mutate(maseficientes=ifelse(mpg<eficientes,"bajo promedio","sobre promedio"))
View(mtcars)

#filtrado con seleccion
masveloces<-mtcars[mtcars$qsec>16,]
masveloces

mtcars<-mtcars %>%
  mutate(velocidad=ifelse(qsec<16,"menos de 16","mas de 16"))
#Agregar nueva columna en nuestra data
mtcars<-mtcars%>%
  mutate(Pesokilos=((wt/2)*1000))

mtcars<-mtcars%>%
  mutate(Peso=ifelse(Pesokilos<=1500,"Liviano", "Pesado"))


#Trabajamoscon economia naranja

orangeec <- orangeec %>%
  mutate(Crecimiento_gdp=ifelse(`GDP Growth %`>=2.5,"Mayor a 2.5","Menor a 2.5"))
orangeec$Crecimiento_gdp

#Ranking
orangeec <- orangeec %>%
  arrange(desc(`Creat Ind % GDP`))
  #LO hemos ordenado de mayor a menor con desc que es decreciente

  
   
  
  
  