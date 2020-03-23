##########################################
## Class 02: Review and  Data Management
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez
##########################################

#---- Part 1: Review  -------------------

#Estas son las cosas que me gustaría que les queden bien claras

### 1. Sintaxis básica

# Creación de Objetos

x<-NULL #si es un objeto (en blanco)
y<-c(TRUE,FALSE) #dos elementos concatenados con la función c (elementos lógicos)
as.numeric(y) #convertir a numérico (si es que se puede)

A<-1
years<-2010:2020
year<-seq(2010,2020,by = 0.5)
tiktoc<-c("Que", "linda", "te ves", "limpiando", "Esperancita")
#c("Que", "linda", "te ves", "limpiando", "Esperancita",4) si corro esto, R fuerza a 4 a ser del mismo tipo que los demás

paste("hola","mundo",sep=" ") #el sep es como quiero que separe
paste(tiktoc,collapse=" ")

obj2<-c(1,2,3,4,"Esperancita") #transforma los demás en string
as.numeric(obj2)

class(tiktoc)
numeros_en_texto <-c("1","2","3")
as.numeric(numeros_en_texto)

m1<-matrix(1:4,2,2)
m1%*%t(m1) #multiplicó m1 por su traspuesta
diag(m1)
solve(m1) #da la inversa de la matriz

a1<-array(1:12,dim = c(2,2,3)) #tiene 3 dimensiones

d1<-data.frame(m1)
data("quakes") # promise #quakes es una base ya cargada en R
d1<-data.frame(quakes)
View(d1) #abre la tabla de datos

ls()
l1<-list(NumeroUno=A,years,tiktoc,m1) #listas objetos muy complejos, puede tener cualquier cosa

# Manipulación de Objetos
ls()

class(A)
typeof(A) #si los elementos dentro de qué tipo son (me sale doble pq tiene decimal)

A <- 1L
class(A)
typeof(A) 

length(years)
dim(m1)
length(m1) #como es un objeto de 2 dim solamente suma
object.size(d1) #indica los bytes

names(d1) #nombres de las bases de datos
head(d1) #6 primeras observaciones de la base de datos
tail(d1) #6 últimas observaciones

rm(A)

#Bonus: como se borra todo?
rm(list=ls()) #elimina todo el ambiente

# Indexación uso de los []

length(years)
years[1]

dim(m1)
m1[2,3] #segunda fila, tercera columna
m1[1,2] #cuando hay más dimensiones, uso comas

dim(a1)
a1[2,1,3] #segunda fila, primera columa, layer 3

l1[2]
l1[2][[1]][1:2] #se sigue una lógica de encadenamiento
class(l1[2])
class(l1[2][[1]])
class(years)

l1[[2]][3:5]

l1$NumeroUno

d1[1,] #d1 es de dos dimensiones, 1000 y 5
d1[,1] #valores de la primera columna
d1[,"lat"]
d1$mag[1:5]
d1$mag[-1] #todos menos el primero
d1$mag[c(1,3,5)]
d1$mag[seq(1,16,2)]
d1$lat[1:4]

d1[,'lat']
d1[1:4,c('lat','long')]
d1$mag>5 #llamo el objeto con una condición
table(d1$mag>5)
d1[d1$mag>6,] #me lo entrega en formato tabla 
d1[d1$mag>6,"stations"]

d1$dummy_5up<-as.numeric(d1$mag>5) #se creó una variable dummy 
head(d1)

# Distinguir entre funciones, objetos, números y sintaxis básica
# Funciones: palabra + () con argumentos separados por commas
# Objetos: palabras a la izquierda del signo <- 


#---- Part 2: Loops  -------------------

A<-2

if(A==1){
  print("A es un objeto con un elemento numérico 1")
} else {
  print("A no es igual a 1, pero no se preocupe que lo hacemos")
  A<-1L
}

#for loop

for(i in 1:5){
  print(paste("Me le declaro a la ", i))
  Sys.sleep(2) #se duerme el sistema por 2 segundos
  print("no mejor no... fail!")
  Sys.sleep(1)
}

i<-1 #NO ME FUNCIONO
eps<-50/(i^2)
while(eps>0.001){
  print(paste("eps value es still..", eps))
  i<-i+1
}

#---- Part 3: Data Management ----
# Tres formas de trabajar con datos

### 1. R-Base 
#http://github.com/rstudio/cheatsheets/raw/master/base-r.pdf

quakes[quakes$mag>6,'mag']

by(data = quakes$mag,INDICES = quakes$stations,FUN = mean) #sacar el promedio demagnitud por estación
tapply(X = quakes$mag,INDEX = quakes$stations, FUN = mean)

### 2. tydiverse 
#https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
install.packages("tidyverse")
library(tidyverse)
#Cómo se instala el paquete si no lo tengo? Tank!!! ayudaaaa!

quakes %>% #%>% sirve para ir filtrando
  filter(mag>6) %>% 
  select(mag) 

quakes %>% 
  group_by(stations) %>%
  summarise(mean(mag))


### 3. data.table (recommended in this course)
library(data.table)
#https://github.com/rstudio/cheatsheets/raw/master/datatable.pdf

quakes<-data.table(quakes)

quakes[mag>6,.(mag)] #.() sirve para hacer listas
quakes[mag>6,.(mag,depth)]

quakes[,mean(mag),by=.(stations)] #agrega una tercera dimensión

### Reading data from a file

library(readxl)

casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE)

casos<-casos[Región=="Metropolitana",]

library(ggplot2)

ggplot(casos[order(Edad,decreasing = T)],)+geom_bar(stat = 'identity' ,aes(x=`Centro de salud`, y=Edad/Edad, group=Sexo, fill=Edad)) + coord_flip()+ facet_wrap(~Sexo) 

casos[Sexo=="Fememino",Sexo:="Femenino"]

ggplot(casos[order(Edad,decreasing = T),])+geom_bar(stat = 'identity',aes(x=`Centro de salud` ,y=Edad/Edad,fill=Edad)) + coord_flip()+ facet_wrap(~Sexo) +labs(title = "Casos Confirmados por Sexo y Establecimiento",subtitle = "Región Metropolitana - 2020-03-17",caption = "Fuente: https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/")

