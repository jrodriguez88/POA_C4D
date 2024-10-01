## Spatial Analysis. C4D Project
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


# names(caracterizacion_to_join) %>% str_subset("georre|area")
# names(caracterizacion_to_join) %>% str_subset("cluster")
# names(caracterizacion_to_join) %>% str_subset("vendi")

spatial_data_c4d <- caracterizacion_to_join %>% 
  dplyr::select(ID_Productor, departamento, cluster, 
                contains("area"), contains("georreferencia"))
                # all_of(c("Georreferenciar Polígono Cultivo de Cacao",
                #          "Georreferenciar Polígono de la Finca",
                #          "area_pol_i",
                #          "area_pol_ii",
                #          "5.1. Área de la Finca en Has:" ,
                #          "5.2. Área del cultivo de Cacao en Has:",
                #          "5.3. Área de Cacao en Producción Has:",
                #          "5.28. Área Cultivo Complementario HAS:"
                #          ))) 
                # 



### Shapefile sites - departamentos proyecto 
COL_shp <- gadm(country="COL", level=1, path=tempdir()) %>% st_as_sf()
COL2_shp <- gadm(country="COL", level=2, path=tempdir()) %>% st_as_sf()



safe_get_polygon <- purrr::possibly(get_polygon, NULL)

poligonos_lotes <- spatial_data_c4d %>% 
  select(ID_Productor, georreferencia_cultivo) %>%
  mutate(polygon_cultivo = map(georreferencia_cultivo,
                               safe_get_polygon))


poligonos_fincas <- spatial_data_c4d %>% 
  select(ID_Productor, georreferencia_finca) %>%
  mutate(polygon_finca = map(georreferencia_finca,
                               safe_get_polygon))

# 
# test_spa <- spatial_data_c4d %>% sample_n(20) %>% 
#   mutate(polygon_cultivo = map(georreferencia_cultivo,
#                                get_polygon))

safe_get_elevation <- purrr::possibly(get_elevation)

elevation_lotes <- spatial_data_c4d %>% 
  select(ID_Productor, georreferencia_cultivo) %>%
  mutate(elevacion_cultivo = map_dbl(georreferencia_cultivo,
                                     safe_get_elevation))


#revisar_registro <- map_lgl(poligonos_lotes$polygon_cultivo, is.na) %>% which()

centroides_lotes <- poligonos_lotes %>% unnest(polygon_cultivo) %>%
  #slice(poligonos_lotes, c(revisar_registro)) %>% unnest(polygon_cultivo) %>%
  mutate(centroide_cultivo = map(geometry , ~st_centroid(st_combine(.x)))) %>% 
  unnest(centroide_cultivo)


# id_ver <- 1073974513


# which(poligonos_fincas$ID_Productor==id_ver)
# spatial_data_c4d %>% slice(84)

poligono_finca <- poligonos_fincas$polygon_finca[[3]] #%>% st_make_valid
poligono_lote <- poligonos_lotes$polygon_cultivo[[3]] #%>% st_make_valid
centroide <- st_as_sf(data.frame(
  geometry = st_sfc(centroides_lotes$centroide_cultivo[[3]])),
  crs = st_crs(poligono_lote))

ggplot() + #geom_sf(data = COL_shp)+
#  geom_sf(data = filter(COL_shp, NAME_1=="Santander")) +
  geom_sf(data = poligono_finca, color = "black", fill = "darkgreen" ) + 
  geom_sf(data = poligono_lote, color = "red" , fill = "brown", alpha = 0.5) + 
  
  geom_sf(data = centroide, color = "red") +
  theme_bw()

# centroides_lotes$centroide_cultivo %>% length()
# poligonos_lotes$polygon_cultivo[[4872]]

# poligonos_lotes_validos <- poligonos_lotes$polygon_cultivo %>% 
#   map_lgl(st_is_valid)

# table(poligonos_lotes_validos) 

# st_area(poligono_finca)
# st_area(poligono_lote)
# st_within(centroide_sf, poligono_finca_final, sparse = FALSE)[1,1]
# 
# # Transformar el polígono a un sistema de coordenadas proyectado 
# poligono_finca_proj <- st_transform(poligono_finca, crs = 3116)
# 
# # Ahora aplicar el st_snap() con la tolerancia deseada
# poligono_finca_snap <- st_snap(poligono_finca_proj, poligono_finca_proj, tolerance = 0.1)
# 
# # Si todo está correcto, puedes volver a transformar el polígono a WGS 84 si lo necesitas
# poligono_finca_final <- st_transform(st_union(poligono_finca_snap) %>% st_make_valid(), crs = 4326) 
# # Visualizar el resultado
# 
# plot(st_geometry(poligono_finca_final))
# 
# st_is_valid(poligono_finca_final)
# 
# ## Verificar centroide lote en poligono finca

centroide_en_poligono_safe <- possibly(centroide_en_poligono)


verificacion_centroide_cultivo <- poligonos_fincas %>% 
  left_join(centroides_lotes %>% dplyr::select(ID_Productor, centroide_cultivo)) %>% 
  drop_na() %>%
  mutate(lote_in_finca = map2(
  polygon_finca, centroide_cultivo, centroide_en_poligono))

poligonos_revisar <- verificacion_centroide_cultivo %>% 
  mutate(is_null = map_lgl(lote_in_finca, is.null)) %>% 
  filter(is_null)

write_csv(poligonos_revisar %>% select(ID_Productor), file = "revisar_poligonos_cultivo.csv")

centroide_lote_fuera_finca <- verificacion_centroide_cultivo %>% 
  mutate(is_null = map_lgl(lote_in_finca, is.null)) %>% 
  filter(!is_null) %>% unnest(lote_in_finca) %>%
  filter(!lote_in_finca)


write_csv(centroide_lote_fuera_finca %>% select(ID_Productor), file = "revisar_centroide_cultivo_fuera_finca.csv")  



coordenadas_lotes <- centroides_lotes %>% 
  select(ID_Productor, centroide_cultivo) %>%
  mutate(
    longitude = map_dbl(centroide_cultivo, ~st_coordinates(.x)[, 1]),
    latitude = map_dbl(centroide_cultivo, ~st_coordinates(.x)[, 2]))



spatial_data_basic <- left_join(coordenadas_lotes, elevation_lotes) %>% 
  select(-c(centroide_cultivo, georreferencia_cultivo))

save(spatial_data_c4d, poligonos_lotes, poligonos_fincas, elevation_lotes, centroides_lotes,
     coordenadas_lotes, spatial_data_basic,
     file = "data/2_archivos_procesados/spatial_C4D.RData")



# test2 <- poligonos_lotes %>% unnest(polygon_cultivo) %>% st_as_sf()
# 
# 
# 
# 
# limit_fincas <- st_bbox(test2)
# COL_points <- st_centroid(COL_shp)
# COL_points <- cbind(COL_shp, st_coordinates(st_centroid(COL_shp$geometry)))
# 
# 
# 
# 
# plot_map <- ggplot() + 
#   geom_sf(data = st_crop(COL_shp, limit_fincas), alpha = 0, fill = "transparent") +
#   geom_sf(data = test2)  +
#   geom_text(data = st_crop(COL_points, limit_fincas), aes(x=X, y=Y, label=NAME_1),
#             color = "darkblue", fontface = "bold", check_overlap = FALSE) + 
#   labs(title = "Distribucion productores",
#        #subtitle = sub_plot,
#        #caption = capt_plot,
#        #fill = fill_var,
#        x = "Longitud", y = "Latitud")
# 
# plotly::ggplotly(plot_map)
# 
# 
# 
# # Calcular el centroide del polígono
# centroid <- map(test2$geometry, ~st_centroid(st_combine(.x))) %>% 
#   enframe(name = NULL) %>% unnest(value)
# 
# st_intersection(COL_points, centroid)
# 
# 
# # Resultados
# polygon_sf # Polígono
# centroid # Centroide
# 
# 
# # Transformar a un sistema de coordenadas proyectadas adecuado
# # En este caso, usaremos WGS 84 / UTM zona 18N (EPSG:3116), que es adecuado para la región de Colombia
# polygon_sf_proj <- st_transform(polygon_sf, crs = 3116)
# 
# # Calcular el área en metros cuadrados
# area_m2 <- st_area(polygon_sf)


