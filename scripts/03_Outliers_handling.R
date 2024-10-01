## Manejo de datos faltantes y atipicos
## github.com/jrodriguez88
## Agosto 2024


# library(outliers)
# 
# load("caracterizacion_to_join.RData")
# load("productividad_to_join.RData")
# load("pgf_to_join.RData")



# datos de analisis

libros_list <- list(caracterizacion_to_join, pgf_to_join, productividad_to_join) 
  
libros_names <- c("Libro_Caracterizacion", 
                 "Libro_Plan_Gestion_Finca", 
                 "Libro_Productividad")

libros_ids <- list(caracterizacion_to_join$ID_Productor, 
                   pgf_to_join$ID_Productor, 
                   productividad_to_join$ID_Productor)

map2(.x = libros_list, 
     .y = libros_names, 
     ~vis_dat(.x, warn_large_data = FALSE) + 
       labs(title = .y) +
       scale_fill_viridis_d(, option = "D", direction = -1))



## Visualizacion de datos faltantes
map2(.x = libros_list, 
     .y = libros_names, 
     ~vis_miss(.x, warn_large_data = FALSE) + 
       labs(title = .y))

## Crea resumen de missinfg values por variable
map(.x = libros_list, miss_var_summary) %>% 
  set_names(libros_names) %>%
  bind_rows(.id = "Libro") %>% 
  write_csv("data/2_archivos_procesados/missing_values_by_feature.csv")



## Crea matrix de outliers

map(libros_list, detect_outliers2_iqr) %>% 
  set_names(libros_names) %>% 
  bind_rows(.id = "Libro") %>%
  write_csv(file = paste0("data/2_archivos_procesados/Outliers_summary.csv"))

map(libros_list, detect_outliers_iqr) %>% 
  set_names(libros_names) %>% 
  map2(., libros_ids, ~.x %>% mutate(ID = .y) %>%
         select(ID, everything())) %>%
  map2(.x = ., libros_names, 
       ~ write_csv(.x, file = paste0("data/2_archivos_procesados/Outliers", .y, ".csv")))


# calcula porcentaje de datos faltantes luego de reemplazo de outliers
list(original =libros_list %>% map(pct_miss) %>% set_names(libros_names) %>% 
  bind_rows(),

oulier_replace_na = libros_list %>% 
  map(~.x %>% mutate(across(where(is.numeric), replace_outlier))) %>% 
        map(pct_miss) %>% set_names(libros_names)) %>% bind_rows(.id = "Status")



libros_list %>% 
  map(~.x %>% mutate(across(where(is.numeric), replace_outlier))) %>%
  map2(.x = ., 
       .y = paste(libros_names, "- Outliers replace by NA"),
       ~vis_miss(.x, warn_large_data = FALSE) + 
         labs(title = .y))
  


data_without_ouliers <- libros_list %>% 
  map(~.x %>% mutate(across(where(is.numeric), replace_outlier))) %>%
  set_names(libros_names) %>%
  map2(.x = ., .y = libros_names, ~.x %>% 
         nest(data = -ID_Productor) %>% rename(!!.y := data)) %>%
  reduce(left_join)


# write_rds(data_without_ouliers, 
#      file = "data/2_archivos_procesados/data_without_ouliers.rds", compress = "gz")


libros_list %>% 
  map(~.x %>% mutate(across(where(is.numeric), replace_outlier))) %>%
  set_names(libros_names) %>% map(skim) %>%
  map2(.x = ., libros_names, 
       ~ write_csv(.x, file = paste0("data/2_archivos_procesados/summary_", .y, ".csv")))



caracterizacion_no_outliers <- caracterizacion_to_join %>% 
  mutate(across(where(is.numeric), ~replace_outlier(., coef_iqr = 3)))

pgf_no_outliers <- pgf_to_join %>% 
  mutate(across(where(is.numeric), ~replace_outlier(., coef_iqr = 3)))

productividad_no_outliers <- productividad_to_join %>% 
  mutate(across(where(is.numeric), ~replace_outlier(., coef_iqr = 3)))


save(caracterizacion_no_outliers, pgf_no_outliers, productividad_no_outliers, 
     file = "data/2_archivos_procesados/data_no_outliers.RData")


list(caracterizacion_to_join, caracterizacion_no_outliers) %>%
map(~partition(skim(.x, where(is.numeric)))$numeric %>% head(20)) 


caracterizacion_to_join %>%
  select(departamento, 
         area_cacao_produccion, area_cultivo_georref, area_finca_georref,
         edad_cultivo,no_arboles_sembrados,  no_arboles_produccion,
         no_arboles_sembrados_2018_adelante,
         area_cultivo_complementario,
         cacao_seco_vendido,
         cacao_baba_vendido,
         no_personas_hogar,
         no_personas_jovenes,
         no_mujeres,
         cambios_copa_2018_adelante,
         edad_productor,
         antiguedad_proyecto) %>% 
  pivot_longer(cols = -departamento) %>%
  ggplot(aes(value)) + 
  geom_histogram() + facet_wrap(~name, scales = "free") +
  theme_minimal()
# A partir de los histogramas se podrian generar filtros para las variables  

boxplot_caracterizacion <- caracterizacion_to_join %>%
  select(ID_Productor, departamento, municipio, 
         area_cacao_produccion, area_cultivo_georref, area_finca_georref,
         edad_cultivo,no_arboles_sembrados,  no_arboles_produccion,
         no_arboles_sembrados_2018_adelante,
         area_cultivo_complementario,
         cacao_seco_vendido,
         cacao_baba_vendido,
         no_personas_hogar,
         no_personas_jovenes,
         no_mujeres,
         cambios_copa_2018_adelante,
         edad_productor,
         antiguedad_proyecto) %>% 
  pivot_longer(cols = -c(ID_Productor, departamento, municipio)) %>%
  ggplot(aes(value, label1 = ID_Productor, label2 = departamento, label3 = municipio)) + 
  geom_boxplot() + facet_wrap(~name, scales = "free") + 
  theme_minimal() 

plotly::ggplotly(boxplot_caracterizacion)

caracterizacion_to_join %>%
  select(departamento, 
         area_cacao_produccion, area_cultivo_georref, area_finca_georref,
         edad_cultivo,no_arboles_sembrados,  no_arboles_produccion,
         no_arboles_sembrados_2018_adelante,
         area_cultivo_complementario,
         cacao_seco_vendido,
         cacao_baba_vendido,
         no_personas_hogar,
         no_personas_jovenes,
         no_mujeres,
         cambios_copa_2018_adelante,
         edad_productor,
         antiguedad_proyecto) %>% 
  pivot_longer(cols = -departamento) %>%
  ggplot(aes(departamento, value, fill = departamento)) + 
  geom_boxplot() + facet_wrap(~name, scales = "free") + 
  theme_minimal() + theme(axis.text.x=element_blank(),
                          legend.position="bottom")

# A partir de los histogramas s epodrian generar filtros para las variables

partition(skim(caracterizacion_no_outliers, where(is.numeric)))$numeric %>% head(20)




