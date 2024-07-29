## Spatial EDA. C4D Project
# 
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2024


##load packages
library(tidyverse)
library(sf)
library(data.table)
#library(raster)
library(plotly)
library(ggspatial)
#library(rgdal)
library(ggrepel)
library(terra)
library(geodata)


names(caracterizacion) %>% str_subset("Georre|Área|area")
names(caracterizacion) %>% str_subset("Cluster")
names(caracterizacion) %>% str_subset("vendió")

spatial_data_c4d_1 <- caracterizacion %>% 
  dplyr::select(ID_Productor, cluster = "Seleccione Cluster de intervención:",
                all_of(c("Georreferenciar Polígono Cultivo de Cacao",
                         "Georreferenciar Polígono de la Finca",
                         "area_pol_i",
                         "area_pol_ii",
                         "5.1. Área de la Finca en Has:" ,
                         "5.2. Área del cultivo de Cacao en Has:",
                         "5.3. Área de Cacao en Producción Has:",
                         "5.28. Área Cultivo Complementario HAS:"
                         ))) 


spatial_data_c4d_1 %>% dplyr::select(ID_Productor,
                                     cluster, 
                                     area_pol_i,
                                     area_pol_ii,
                                     `5.1. Área de la Finca en Has:`,
                                     `5.2. Área del cultivo de Cacao en Has:`,
                                     `5.3. Área de Cacao en Producción Has:`) %>%
  pivot_longer(data)


## Function to tidy spatial data 
get_polygon <- function(data){
  
  # Separar los datos por ";" y luego por espacios para extraer las coordenadas
  coordinates <- str_split(data, ";", simplify = TRUE) %>% map(str_trim) %>%
    map(as_tibble) %>%
    map(~.x %>% separate(value, into = c("latitude", "longitude", "elevation", "precision"), sep = " ")) %>%
    bind_rows() %>%
    mutate(across(latitude:precision, as.numeric))
  
  
  # Crear un objeto sf con los puntos
  polygon_sf <- coordinates %>%
    select(latitude, longitude) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_cast("POINT") %>%
    summarise(do_union = FALSE) %>%
    st_cast("POLYGON")
  
  
  polygon_sf 
  
  
}



centroides_fincas <- spatial_data_c4d_1 %>% 
  mutate(polygon_cultivo = map(`Georreferenciar Polígono Cultivo de Cacao`,
                                 get_polygon))





### Shapefile sites - departamentos proyecto 

COL_shp <- gadm(country="COL", level=1, path=tempdir()) %>% st_as_sf()
COL2_shp <- gadm(country="COL", level=2, path=tempdir()) %>% st_as_sf()

test2 <- centroides_fincas %>% unnest(polygon_cultivo) %>% st_as_sf()
st_centroid(test2$geometry)



limit_fincas <- st_bbox(test2)
COL_points <- st_centroid(COL_shp)
COL_points <- cbind(COL_shp, st_coordinates(st_centroid(COL_shp$geometry)))




plot_map <- ggplot() + 
  geom_sf(data = st_crop(COL_shp, limit_fincas), alpha = 0, fill = "transparent") +
  geom_sf(data = test2, aes(fill = cluster , color = cluster))  +
  geom_text(data = st_crop(COL_points, limit_fincas), aes(x=X, y=Y, label=NAME_1),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) + 
  labs(title = "Distribucion productores",
       #subtitle = sub_plot,
       #caption = capt_plot,
       #fill = fill_var,
       x = "Longitud", y = "Latitud")

plotly::ggplotly(plot_map)



# Calcular el centroide del polígono
centroid <- map(test2$geometry, ~st_centroid(st_combine(.x))) %>% 
  enframe(name = NULL) %>% unnest(value)

st_intersection(COL_points, centroid)


# Resultados
polygon_sf # Polígono
centroid # Centroide


# Transformar a un sistema de coordenadas proyectadas adecuado
# En este caso, usaremos WGS 84 / UTM zona 18N (EPSG:32618), que es adecuado para la región de Colombia
polygon_sf_proj <- st_transform(polygon_sf, crs = 32618)

# Calcular el área en metros cuadrados
area_m2 <- st_area(polygon_sf)


