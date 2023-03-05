library(tidyverse)
library(ggplot2)
library(sf)
library(stringr)
library(rgdal)
library(sp)
library(ggthemes)

voies <- st_read(paste0(path,'/voie.shp'))
quartier <- st_read('D:/r/terrasses/shp/QUARTIER_DE_PARIS.shp')
ter <- st_read('D:/r/terrasses/shp/ter/terrasses-autorisations.shp')

ter10 <- ter %>% 
    filter(arrondissem == 75010)
ggplot()+
    geom_sf(data=quartier %>% filter(C_AR==10 ) ,
            fill='#526a7d',
            alpha=0.3,
            col='#365b81',
            size=1)+
    geom_sf(data=ter10,
            col='black',
            alpha=1,
            pch = 18,
            size=5)+
    geom_sf(data=voies,col='#3c556c',size=1)+
    coord_sf(crs = st_crs('+proj=moll'),
             xlim=c(180973.35,183385.46),
             ylim=c(5754300,5756200))+
    theme_map()