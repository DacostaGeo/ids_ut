# Limpiar espacio de trabajo
rm(list=ls())

# Paquetes
library(readxl)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(viridis)

# Establecer directorio de trabajo
setwd("F:\\repositorio\\rmarkdown\\ids_ut")  

# Importar datos
ids_ut<-read_excel("datos//ids_2020_ut.xlsx")

# Asignar nombres a variables
colnames(ids_ut) <- c("NOMDT", "CVEUT","NAME_UT","IDS","e_ids")

# Estratos de desarrollo social
ids_ut<-ids_ut%>%
  mutate(estrato=case_when(e_ids=="Muy Alto"~ "1) Muy alto",
                           e_ids=="Alto"~ "2) Alto",
                           e_ids=="Medio"~ "3) Medio",
                           e_ids=="Bajo"~ "4) Bajo",
                           e_ids=="Muy Bajo"~ "5) Muy bajo"))

# Importar shapefile de alcaldias
alcaldias<-st_read("shp//alcaldias.shp")%>%
  st_transform('+proj=longlat +datum=WGS84')

# Importar shapefile de Unidades Territoriales (UT)
ut_shp<-st_read("shp//ut.shp")%>%
  st_transform('+proj=longlat +datum=WGS84')

# Union de base de IDS con shapefile de UT
ids_shp<-ut_shp%>%
  left_join(ids_ut,by=c("CVEUT"))

# Categoría Sin información 
ids_shp$estrato[is.na(ids_shp$estrato)] <- "6) Sin información"

# Paleta de colores
pal_ids<-colorFactor(palette = c("#440154FF",
                                 "#3B528BFF",
                                 "#21908CFF",
                                 "#5DC863FF",
                                 "#FDE725FF",
                                 "#434243"), 
                 levels = c("1) Muy alto",
                            "2) Alto",
                            "3) Medio",
                            "4) Bajo",
                            "5) Muy bajo",
                            "6) Sin información"),
                 order=FALSE)

# Etiquetas
my_labels_ids <- paste0(
  "<strong> Clave: </strong> ",st_drop_geometry(ids_shp)$CVEUT, "<br/> ",
  "<strong> Unidad territorial: </strong> ",st_drop_geometry(ids_shp)$NAME, "<br/> ",
  "<strong> IDS: </strong> ", round(st_drop_geometry(ids_shp)$IDS,4), "<br/> ",
  "<strong> Grado: </strong> ", st_drop_geometry(ids_shp)$e_ids, "<br/> ") %>% lapply(htmltools::HTML)

# Mapa dinámico
m1<-leaflet(data=ids_shp,
            options=leafletOptions(dragging=TRUE,minZoom=4,maxZoom=15))%>%
  addProviderTiles(provider = "CartoDB")%>%
  addResetMapButton()%>%
  setView(lng=-99.1308701,lat=19.43626549, zoom=9)%>%
  
  
  addPolygons(data=ids_shp,
              fillColor=~pal_ids(estrato),
              weight = 1,
              opacity = 0.9,
              color = "#BDBDC3",
              dashArray = "1",
              fillOpacity = 0.8,
              group = "UT",
              label=my_labels_ids,
              highlightOptions=
                highlightOptions(weight = 2,
                                 color = "#666",
                                 dashArray = "",
                                 fillOpacity = 0.8,
                                 bringToFront = TRUE))%>%
  
  addPolygons(data=alcaldias,
              weight = 2,
              opacity = 0.5,
              color = "black",
              dashArray = "1",
              fill=FALSE,
              fillOpacity = 0)%>%
  
  addLegend(title ="Grado de desarrollo social",
            position = "bottomright", 
            pal=pal_ids,values = ids_shp$estrato, opacity=0.8)%>%
  addScaleBar(position="bottomleft")%>%
  addSearchFeatures(targetGroups = "UT");m1

