install.packages("terra")
install.packages("sf")
install.packages("spdep")
install.packages("tidyverse")
install.packages("rworldxtra")
install.packages("sp")
install.packages("rgeos")
install.packages("gstat")
install.packages("rgdal")
install.packages("raster")

library(terra)
library(sf)
library(tidyverse)
library(rgeos)
library(sp)
library(rworldxtra)
library(terra)
library(spData)
library(spdep)
library(gstat)
library(rgdal)
library(raster)
library(readxl)


## Visualizacion datos espaciales 

#poligonos

data("countriesHigh")
Mundo<-st_as_sf(countriesHigh)

ggplot()+geom_sf(data=Mundo)
ggplot()+geom_sf(data=Mundo, aes(fill=REGION))

colnames(Mundo)

ggplot()+geom_sf(data=Mundo, aes(fill=GDP_MD_EST))

Africa<-Mundo%>% dplyr::filter(continent=="Africa")
ggplot()+geom_sf(data=Africa, aes(fill=POP_EST))

#Modificar datos espaciales 

Africa<-Africa%>%mutate(Poblacion_mill=POP_EST/1000000)
ggplot()+geom_sf(data=Africa, aes(fill=Poblacion_mill))

Africa<-Africa%>%mutate(Pib_percapita=GDP_MD_EST/POP_EST)



#Seleccionar 

Africa<-Africa%>%dplyr::select(NAME,Poblacion_mill,Pib_percapita)


#Exportar 

write_sf(Africa,"Africa.shp")


##otro tipo de poligonos 

#Combinar multiples shape files 

colegios<-readOGR( dsn = "C:\\Users\\sebas\\OneDrive\\Desktop\\ejemplo clase" , layer = "colegios")
colegios_sf<-st_as_sf(colegios)
ggplot()+ geom_sf(data=colegios_sf) 

Huella_urbana<-readOGR( dsn = "C:\\Users\\sebas\\OneDrive\\Desktop\\ejemplo clase" , layer = "Huella_urbana")
Huella_urbana_sf<-st_as_sf(Huella_urbana)
ggplot()+ geom_sf(data=Huella_urbana_sf)

Linea_metro<-readOGR( dsn = "C:\\Users\\sebas\\OneDrive\\Desktop\\ejemplo clase" , layer = "Linea_metro")
Linea_metro_sf<-st_as_sf(Linea_metro)
ggplot()+ geom_sf(data=Linea_metro_sf)

ggplot()+  geom_sf(data=Huella_urbana_sf)+ geom_sf(data=colegios_sf)+ geom_sf(data=Linea_metro_sf) 


#Proyectar Puntos#


direcciones<- read_xlsx("C:\\Users\\sebas\\OneDrive\\Desktop\\Complementaria 3\\Manejo de Datos Espaciales\\Direcciones.xlsx")
direcciones_geo<-direcciones %>% st_as_sf(coords = c("Long", "Lat"))
ggplot() +  geom_sf(data=direcciones_geo)


##Raster##







##Autocorrelacion Espacial##

#Primera ley Tobler Todo esta relacionado con todo lo demas pero las cosas 
#cercanas estan mas relacionadas estan mas relaciondas que las distantes




Covid_1<-readOGR( dsn = "C:\\Users\\sebas\\OneDrive\\Desktop\\Complementaria 3\\Manejo de Datos Espaciales" , layer = "Colombia_COVID19_Coronavirus_Departamento")
Covid_2<-st_as_sf(Covid_1)

colnames(Covid_2)
ggplot()+geom_sf(data=Covid_2,aes(fill=Casos_Conf))


## Creear Matriz de de vecinos 
reina.R.nb=poly2nb(pl=Covid_2,queen = TRUE)
torre.R.nb=poly2nb(pl=Covid_2,queen = FALSE)



lineas_torre<-nb2lines(nb=torre.R.nb, coords=sp::coordinates(obj = Covid_1), proj4string="+proj=longlat +datum=WGS84 +no_defs")
plot(lineas_torre,col="blue")


###De matriz de vecindad a matriz de pesos


pesos_espaciales_torre<-nb2listw(neighbours = torre.R.nb)

pesos_espaciales_torre<-nb2listw(neighbours = torre.R.nb, zero.policy = TRUE)
summary(pesos_espaciales_torre,zero.policy=TRUE)

test<- moran.test(x=Covid_1$Casos_Conf, listw=pesos_espaciales_torre,zero.policy=TRUE, adjust.n=TRUE)

summary(test)


colegios<-readOGR( dsn = "C:\\Users\\sebas\\OneDrive\\Desktop\\ejemplo clase" , layer = "colegios")
coords <- coordinates(colegios)
col.knn <- knearneigh(coords, k=4)
