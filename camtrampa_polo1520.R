polo <- read.csv ("camtrap_polo1520.csv", header = TRUE)
polo <- as_tibble(polo)
dir_fotos_polo <- file.path("fotos") 

library(camtrapR)
camtraptabla_metadata<- recordTable(inDir = dir_fotos_polo, IDfrom = "metadata", 
                                  cameraID = "directory", camerasIndependent = FALSE, 
                                  minDeltaTime = 5,deltaTimeComparedTo = "lastIndependentRecord", 
                                  timeZone = "America/Argentina/Buenos_Aires", writecsv = TRUE, 
                                  metadataHierarchyDelimitor = "|", metadataSpeciesTag = "especie" )


# inDir: en dónde las fotos taggeadas
# IDfrom: si metadata tag o carpetas con nombre de etiqueta
# cameraID: si nombre lo toma del directorio o filename que implica cambio de nombre de archivo
# minDeltaTime: la magia, tiempo entre registros independientes
# deltaTimeComparedTo: si lastIndependent por especie o lastRecord genérico
# metadataHierarchyDelimitor: si con digikam por default es |


library(tidyverse)
library(lubridate)
library(grid)
library(sf)
library(tmap)
library (readr)

datos <- as_tibble(camtraptabla_metadata) %>% 
  #mutate(fecha = as_date(Date)) %>% 
  filter(metadata_especie == "huemul") %>% 
    group_by(Station, month = lubridate::floor_date(Date, "month")) %>% 
  summarise(
    sum_det = n()
  ) %>% 
  ungroup()


# recuento por cámara_
HB_cam <- as_tibble(camtraptabla_metadata) %>% 
  #mutate(fecha = as_date(Date)) %>% 
  filter(metadata_especie == "huemul") %>% 
  group_by(Station) %>% 
  summarise(
    sum_det = n()
  ) %>% 
  ungroup()


ggplot(datos,aes(x=month, y = sum_det, col= Station))+
  geom_point(position = "jitter")+
  theme_light()


# mapitas -----------------------------------------------------------------

datoscam.sf <- st_as_sf(polo, coords=c("utm_x","utm_y"), crs=26918)

bbox_new <- st_bbox(datoscam.sf) # current bounding box
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

diseño <- datoscam.sf%>% 
  tm_shape(datoscam.sf)+
  tm_dots("Station",shape=16,size=3,col='darkgray')

qtm(datoscam.sf, text = "Station")
