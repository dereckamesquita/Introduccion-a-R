#EDA Exploratory data analysis
orangeec <- read_csv("orangeec.csv")
mtcars <- read_csv("mtcars.csv")

plot(mtcars$mpg ~mtcars$cyl, xlab="cilindos",ylab = "millas", 
     main="Relacion de cilindos y millas")
plot(mtcars$mpg ~mtcars$hp, xlab="cilindos",ylab = "caballos", 
     main="Relacion de cilindos y caballos")

plot(orangeec$Unemployment~orangeec$`Education invest % GDP`,
     xlab="inversion en educacion", ylab = "desempleo", main="relacion entre inversion y desempleo")

plot(orangeec$`GDP PC`~orangeec$`Creat Ind % GDP`,
     xlab="aporte economia naranja", ylab = "PIB", main="Relacion entre economia naranja y PIB")
head(orangeec$`Creat Ind % GDP`)
orangeec

#Histograma
#q ya no sirve qplot(mtcars$hp, geom="histogram", xlab="caballos de fuerza", main="Carros segun caballos de fuerza")
hist(mtcars$hp, xlab="caballos de fuerza", 
     breaks = seq(50,350,10), #breaks son los puntos de corte # seq define numeros desde el 50 al 130 de 10 en 10
     main="Carros segun caballos de fuerza")
library(ggplot2)
#histrograma basico con ggplot
ggplot(data=mtcars, 
       mapping=aes(x=hp))+
  geom_histogram(bins=10)+
  labs(x="Caballos de fuerza", y="frecuencia",
       tittle="caballos de fuerza en carros seleccionados")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
#Hacer histograma con ajuste de distribucion normal
ggplot(data = mtcars,
       mapping = aes(x = hp)) + 
  geom_histogram(aes(y =..density..),
                 bins = 9,
                 position = 'identity') +
  stat_function(fun = dnorm, 
                args = list(mean = mean(mtcars$hp), 
                            sd = sd(mtcars$hp)))
#Histograma con division
ggplot(data=mtcars, 
       mapping=aes(x=hp, fill= factor(vs)))+
  geom_histogram(bins=30)+
  labs(x="Caballos de fuerza", y="frecuencia",
       tittle="caballos de fuerza en carros seleccionados")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Economia naranja
ggplot(data=orangeec, 
       mapping=aes(x=`GDP PC`))+
  geom_histogram(binwidth = 2000)+
  labs(x="PBI per", y="Cantidad de carros", title="PIB per en paises LATAM")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#EDA con BOX SPOT
boxplot(mtcars$hp, 
        ylab="Caballos de Fuerza",
        main="Caballos de fuerza en carros ")
#Usamos ggplot (tenemos que poner cyl como un factor para que se pueda categorizar
ggplot(data=mtcars,
       mapping = aes(x=as.factor(cyl), y=hp, fill= cyl))+
  geom_boxplot()+
  labs(x="cilindros", y="caballos de fuerza", 
       title ="Caballos de fuerza segun cilindros " )+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#Otra prueba con box plot
ggplot(data=mtcars,
       mapping = aes(x=as.factor(am), y=mpg, fill=am))+
  geom_boxplot()+
  labs(x="Tipo de caja", y="Millas por galon", 
       title ="Millas por galon segun tipo de caja" )+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#Podemos ponerle a "am" si es manual o automatico
#cambiar valores dentro de un data frame
mtcars$am <- factor(mtcars$am, levels = c(0, 1),
                    labels=c("Manual", "Automatico"))
mtcars$am

#Box plot filtrado de paises con mayor pib percapita
economy<- mean(orangeec$`GDP PC`)
economy
"La media PBI percapita es de 14052.94"
library(dplyr)

orangeec <- orangeec %>%
  mutate(strong_ec = ifelse(`GDP PC`<economy,"Por debajo del PBI per","Sobre el PBI per"))
orangeec$strong_ec
#renombramos
colnames(orangeec)[colnames(orangeec) == "strong_ec"] <- "Nivel Economia"
orangeec$`Nivel Economia`
#exportar
write.table(orangeec, file="orangeec1.csv", sep = ",", row.names = FALSE)
#con rownames false aseguramos que no se agreguen nombres a las filas

#importamos el nuevo dataset
orangeec1 <- read_csv("orangeec1.csv")

#Crearemos un box plot segun la media del pbi per
ggplot(data=orangeec,
       mapping = aes(x=`Nivel Economia`, y=`Creat Ind % GDP`, 
                     fill=`Nivel Economia`))+
  geom_boxplot()+
  labs(x="Tipo de caja", y="Millas por galon", 
       title ="Millas por galon segun tipo de caja" )+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#scatter plot con ggplot
ggplot(data=mtcars,
       mapping = aes(hp,mpg))+
  geom_point()+
  labs(x="Caballos de fuerza", y="Millas por galon", 
       title ="Relacion caballos de fuerza y millas" )+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
"Interpretacion a mayor numero de caballos, menor recorrido de millas por galon"

ggplot(data=mtcars,
       mapping = aes(wt,hp))+
  geom_point()+
  labs(x="Peso", y="Caballos de fuerza", 
       title ="Relacion entre peso y caballos de fuerza" )+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#relacionar 4 variables
ggplot(data=mtcars,
       mapping = aes(hp, qsec))+
  geom_point(aes(color=am, size=cyl))+
  labs(s="Caballos de fuerza", y="Tiempo en 1/4 milla", 
       title ="Caballos velocidad segun cilindraje y tipo de caja" )+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#relacionar 4 variables en economia naranja

ggplot(data=orangeec,
       mapping = aes(`Internet penetration % population`, `Creat Ind % GDP`))+
  geom_point(aes(color=factor(`Nivel Economia`), size=`GDP Growth %`))+
  labs(x="Penetracion de internet", y="Aporte economia naranja", 
       title ="Internet y aporte economia naranja, segun economia y crecimiento" )+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#plotly nos agrega la leyenda al plot sin hacer uso de algun comando
install.packages("plotly")
library("plotly")

#Graficando con plotly
migraf <- ggplot(data=orangeec,
               mapping = aes(`Internet penetration % population`, `Creat Ind % GDP`,
                             label=row.names(orangeec)))+
  geom_point()+
  labs(x="Penetracion de internet", y="Aporte economia naranja", 
       title ="Internet y aporte economia naranja, segun economia y crecimiento" )
migraf
p=ggplotly(migraf)
p
