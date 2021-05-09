# cargamos los paquetes
library(sf) 
library(fs)
library(tidyverse)
library(lubridate)
library(classInt)
library(rvest)
library(tmap)

#link de descarga
link <- "http://www.catastro.minhap.es/INSPIRE/Buildings/11/11900-CADIZ/A.ES.SDGC.BU.11900.zip"

#descarga de datos
temp <- tempfile()
download.file(URLencode(link), temp)
unzip(temp, exdir = "buildings_cadiz")

#importar los datos
file <- dir_ls("buildings_cadiz", regexp = "building.gml")
buildings <- st_read(file)

#preparación de los datos
buildings <- mutate(buildings, 
                        beginning = str_replace(beginning, "^-", "0000") %>% 
                          ymd_hms() %>% as_date()
)


#limitamos al periodo posterior a 1900
buildings_1950 <- filter(buildings_val, beginning >= "1950-01-01")  


#buffer de 4km de Cádiz
cadiz_point <- tmaptools::geocode_OSM("Cadiz, Cadiz", 
                                       as.sf = TRUE)
cadiz_point <- st_transform(cadiz_point, 25829)
point_bf <- st_buffer(cadiz_point,4000)
buildings_4km <- st_intersection(buildings_1950, point_bf)

#preparar los datos para el mapas
br <- classIntervals(year(buildings_4km$beginning), 15, "quantile")
lab <- names(print(br, under = "<", over = ">", cutlabels = FALSE))
buildings_4km <- mutate(buildings_4km, 
                          yr_cl = cut(year(beginning), br$brks, labels = lab, include.lowest = TRUE))

#mapa de Cádiz
col_spec <- RColorBrewer::brewer.pal(11, "Spectral")
col_spec_fun <- colorRampPalette(col_spec)

tm <-tm_shape(buildings_4km) +
  tm_polygons("yr_cl", 
              border.col = "transparent",
              palette = col_spec_fun(15),
              textNA = "Sin dato",
              title = "") +
  tm_layout(bg.color = "black",
            outer.bg.color = "black",
            legend.outside = TRUE,
            legend.text.color = "white",
            legend.text.fontfamily = "Roboto", 
            panel.label.fontfamily = "Roboto",
            panel.label.color = "white",
            panel.label.bg.color = "black",
            panel.label.size = 5,
            panel.label.fontface = "bold")

tmap_save(tm, filename = "images/cadiz_buildings_map.png")

