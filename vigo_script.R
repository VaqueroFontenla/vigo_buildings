# cargamos los paquetes
library(sf) 
library(fs)
library(tidyverse)
library(lubridate)
library(classInt)
library(rvest)
library(tmap)

#link de descarga
link <- 'http://www.catastro.minhap.es/INSPIRE/Buildings/54/54057-VIGO/A.ES.SDGC.BU.54057.zip'

#descarga de datos
temp <- tempfile()
download.file(URLencode(link), temp)
unzip(temp, exdir = 'buildings_vigo')

#importar los datos
file <- dir_ls('buildings_vigo', regexp = 'building.gml')
buildings <- st_read(file)

#preparación de los datos
buildings <- mutate(buildings, 
                        beginning = str_replace(beginning, '^-', '0000') %>% 
                          ymd_hms() %>% as_date()
)


#limitamos al periodo posterior a 1800
buildings_1800 <- filter(buildings, beginning >= '1800-01-01') 

#buffer de 4km de Vigo
vigo_point <- tmaptools::geocode_OSM('Vigo', 
                                       as.sf = TRUE)
vigo_point <- st_transform(vigo_point, 25829)
point_bf <- st_buffer(vigo_point,4000)
buildings_4km <- st_intersection(buildings_1800, point_bf)

#preparar los datos para el mapas
br <- classIntervals(year(buildings_4km$beginning), 15, 'quantile')
lab <- names(print(br, under = '<', over = '>', cutlabels = FALSE))
buildings_4km <- mutate(buildings_4km, 
                          yr_cl = cut(year(beginning), br$brks, labels = lab, include.lowest = TRUE))

#mapa de Vigo
col_spec <- RColorBrewer::brewer.pal(11, 'Spectral')
col_spec_fun <- colorRampPalette(col_spec)

tm <-tm_shape(buildings_4km) +
  tm_polygons('yr_cl', 
              border.col = 'transparent',
              palette = col_spec_fun(15),
              textNA = 'Sin dato',
              title = '') +
  tm_layout(bg.color = 'black',
            main.title = 'Vigo',
            main.title.color = 'white',
            main.title.position = c('center', 'top'),
            main.title.size = 3,
            main.title.fontfamily = 'Roboto',
            outer.bg.color = 'black',
            legend.outside = TRUE,
            legend.text.color = 'white',
            legend.text.fontfamily = 'Roboto', 
            panel.label.fontfamily = 'Roboto',
            panel.label.color = 'white',
            panel.label.bg.color = 'black',
            panel.label.size = 5,
            panel.label.fontface = 'bold')

tmap_save(tm, filename = 'images/vigo_buildings_map.png')

