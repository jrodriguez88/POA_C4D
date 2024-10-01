### Test de agrupacion de variable sor similitud de texto

#options(encoding = "UTF-8")


#data_to_reduce <- caracterizacion

## Funcion para agrupar nombres de variables con la aplicacion de cluster jerarquico
# considerando la longitud  de nombre de las variables se propone el uso de 

group_names_by_hcluster <- function(data_to_reduce, method_dist = "jw", h_cutree  = 0.40){
  
  #create vector of names and short names
  names_vars <- names(data_to_reduce)
  names_vars2 <- str_sub(names(data_to_reduce), 1, 10)
  
  
  # create vector with class of variables
  class_select <- data_to_reduce %>% 
    map(class) %>% map_chr(1) %>% as.vector()
  
 # Cluster analisis 
  names_vars %>% 
    tolower %>% 
    trimws %>% paste0(" - ", class_select) %>%
    stringdist::stringdistmatrix(method = method_dist, p = 0.1) %>% 
    as.dist %>% 
    `attr<-`("Labels", names_vars2) %>% 
    hclust %T>% 
    plot %T>% 
    rect.hclust(h = h_cutree) %>% 
    cutree(h = h_cutree) %>% 
    print -> test_group_caracterizacion
  
  names_vars %>% enframe(name = "group") %>% 
    mutate(group = as.vector(test_group_caracterizacion)) %>% 
    arrange(group) #%>% view()
  
}


#dimension_reduction_by_cluster(caracterizacion, 80)

#dimension_reduction_by_cluster(plan_gestion_finca, 80, h_cutree  = 0.35) %>% view()

#dimension_reduction_by_cluster(productividad, 80) 

calc_variance_df <- function(data_to_reduce){
  
  
  data_to_reduce %>% 
#    select(all_of(na_filter)) %>%
    summarize(across(
      where(is.numeric),~ var(scale(., center = FALSE), na.rm =TRUE))) %>%
    pivot_longer(everything(), names_to ="feature", values_to ="variance") %>%
    arrange(variance) 
  
  
}



# Función para identificar y manejar outliers

detect_outliers_iqr <- function(df, iqr_factor = 2) {
  df %>%
    mutate(across(where(is.numeric), ~ifelse(. > (quantile(., 0.75, na.rm = T) + iqr_factor * IQR(., na.rm = T)) | 
                                               . < (quantile(., 0.25, na.rm = T) - iqr_factor * IQR(., na.rm = T)), TRUE, FALSE))) %>%
    select(where(is.logical))  #%>%
  #   mutate(across(everything(), ~ifelse(. == TRUE, NA, 1)))
  #  summarise(across(where(is.logical), ~list(which(.))))
  # summarise(across(where(is.character), ~which(. == "outlier", na.rm = T)))
}


detect_outliers2_iqr <- function(df, iqr_factor = 2) {
  df %>%
    mutate(across(where(is.numeric), ~ifelse(. > (quantile(., 0.75, na.rm = T) + iqr_factor * IQR(., na.rm =T)) | 
                                               . < (quantile(., 0.25, na.rm = T) - iqr_factor * IQR(., na.rm = T)), "outlier", "normal"))) %>%
    summarise(across(where(is.character), ~sum(. == "outlier", na.rm = T))) %>% 
    pivot_longer(everything(), names_to = "Feature", values_to = "No_ouliers")
}

replace_outlier <- function(data, coef_iqr = 2,  fill = "na"){
  
  fill <- switch(fill,
                 na = NA_real_,
                 median = median(data, na.rm = T),
                 mean = mean(data, na.rm = T))
  
  data[data %in% boxplot.stats(data, coef = coef_iqr)$out] = fill
  
  return(data)
  
}



# calculo rendimiento anual a partir de datos del libro de productividad

## Octubre 1/22 a Septiembre 30/22  - Octubre 1/23 a Septiembre 30/24 ( solo data hasta julio)


# data_productividad <- tt

calcula_produccion_anual <- function(data_productividad, fecha_corte = "2023-09-30"){
  
  
  # filter data and classify by ciclos
  data_p <-   data_productividad  %>%
    mutate(produccion_cacao_mes = map2_dbl(.x = cacao_seco_vendido_mes,
                                           .y = cacao_baba_vendido_mes,
                                           ~sum(.x , (.y/3), na.rm = TRUE)))  %>%
    #  mutate(across(where(is.numeric), ~na_if(., Inf)), across(where(is.numeric), ~na_if(., -Inf))) %>%
    filter(date_registro > as.Date("2022-08-31")) %>%
    mutate(ciclo = case_when(date_registro <= as.Date(fecha_corte) ~ "2022_2023", 
                             date_registro > as.Date(fecha_corte) ~ "2023_2024"))
  
  
  no_visitas <- data_p %>% select(fecha_visita, numero_visita) %>% distinct() %>%
    mutate(ciclo = case_when(fecha_visita <= as_date(fecha_corte) ~ "2022_2023", 
                             fecha_visita > as_date(fecha_corte) ~ "2023_2024")) %>%
    group_by(ciclo) %>% summarise(numero_visitas = n(), .groups = 'drop')
  
  
  proporcion_ciclo <- data_p %>% select(fecha_visita, numero_visita) %>% distinct() %>%
    mutate(ciclo = case_when(fecha_visita <= as_date(fecha_corte) ~ "2022_2023", 
                             fecha_visita > as_date(fecha_corte) ~ "2023_2024")) %>%
    #group_by(ciclo) %>% 
    mutate(proporcion_tiempo_2022_2023 = abs(as.numeric(interval(min(fecha_visita), as_date(fecha_corte)), "years"))) %>%
    mutate(proporcion_tiempo_2023_2024 = abs(as.numeric(interval(max(fecha_visita), as_date(fecha_corte)), "years"))) %>% 
    select(contains("tiempo")) %>% distinct()
    
   
  
  
  data_p %>% 
    group_by(ciclo, date_registro,  year_registro, mes, mes_reg) %>% 
    
    summarise(produccion_cacao = sum(produccion_cacao_mes, na.rm =T), .groups = 'drop') %>%
    ungroup() %>%
    mutate(mes_productivo = produccion_cacao > 0) %>%#group_by(ciclo) %>% summarise(sum(produccion_cacao))
    nest(data = -ciclo) %>%
    mutate(
      produccion_total = map_dbl(data, ~sum(.x$produccion_cacao, na.rm = T)),
      no_meses_productivos = map_dbl(data, ~sum(.x$mes_productivo, na.rm = T)),
      mes_mas_productivo = map(data, ~.x %>%
                                   arrange(desc(produccion_cacao)) %>%
                                   slice_head(n = 1) %>% pull(mes_reg))) %>%
                                  # paste(sep =", ", collapse = "-"))) %>% 
    unnest(mes_mas_productivo) %>% select(-data) %>% 
    left_join(no_visitas, by = join_by(ciclo)) %>%
    pivot_wider(
      names_from = ciclo,
      values_from = c(produccion_total, no_meses_productivos, mes_mas_productivo, numero_visitas)
    ) %>% 
    bind_cols(proporcion_ciclo)
  #no_visitas = map_dbl(data, ~max(.x$max_no_visita))) %>% unnest(mes_mas_productivo)
  #                              filter(produccion_cacao == max(produccion_cacao)) %>%
  #                              pull(mes_reg))) %>% unnest(mes_mas_productivo)
  # pivot_wider(names_from = ciclo, values_from = produccion_total)
  
}


# Calcula produccion mensual del libro de productividad
calcula_produccion_mensual <- function(data_productividad){
  
  data_productividad %>%
    mutate(produccion_cacao_mes = map2_dbl(.x = cacao_seco_vendido_mes,
                                           .y = cacao_baba_vendido_mes,
                                           ~sum(.x , (.y/3), na.rm = TRUE)))  %>%
    #  mutate(across(where(is.numeric), ~na_if(., Inf)), across(where(is.numeric), ~na_if(., -Inf))) %>%
    filter(date_registro >= as.Date("2022-08-31")) %>%
    group_by(year_registro, mes, mes_reg) %>% 
    summarise(produccion_cacao = sum(produccion_cacao_mes, na.rm =T), .groups = 'drop') 
  
}



#Resume factores V1
summariza_factores <- function(data_factores) {
  no_visitas = length(unique(data_factores$numero_visita))
  
  
  data_factores %>% 
    select(-c(ID_Productor, numero_visita, fecha_visita)) %>%
    summarise(across(everything(), sum)) / no_visitas
  
  
}



## Plots EDA
histograma_c4d_data <- function(data) {
  
  data %>%
    select(ID_Productor, where(is.numeric)) %>% 
    pivot_longer(cols = -ID_Productor) %>% 
    ggplot(aes(value)) + 
    geom_histogram(aes(y = ..count..), color = "lightgray") + 
    facet_wrap(~name, scales = "free") +
    #  geom_density(color = "red") + 
    theme_bw() + 
    theme(strip.background =element_rect(fill="white")) +
    labs(title = "Histogramas de distribucion de Variables Numericas", 
         x = NULL,
         y = "Numero de Productores")
  
  
  
}



barplot_c4d <- function(data, levels_no = 7){
  
  data %>%
    # select(ID_Productor, where(is.factor)) %>% 
    select(ID_Productor, all_of(data %>%
                                  select(where(is.factor))  %>% 
                                  map_dbl(n_unique) %>% enframe %>% 
                                  filter(value <= levels_no,
                                         value > 1) %>% 
                                  pull(name))) %>%
    pivot_longer(cols = -ID_Productor) %>% 
    ggplot(aes(value)) +
    geom_bar(color = "lightgray") + 
    facet_wrap(~name, scales = "free") +
    theme_bw() + 
    theme(strip.background =element_rect(fill="white")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    labs(title = "Graficos de barras - Variables categoricas", 
         x = NULL,
         y = "Numero de Registros")
  
  
}



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

## Function to tidy spatial data 
get_elevation <- function(data){
  
  # Separar los datos por ";" y luego por espacios para extraer las coordenadas
  coordinates <- str_split(data, ";", simplify = TRUE) %>% map(str_trim) %>%
    map(as_tibble) %>%
    map(~.x %>% separate(value, into = c("latitude", "longitude", "elevation", "precision"), sep = " ")) %>%
    bind_rows() %>%
    mutate(across(latitude:precision, as.numeric))
  
  
  # Crear un objeto sf con los puntos
  elevation <- coordinates %>%
    select(elevation) %>%
    summarise(mean = median(elevation, na.rm = T)) %>%
    pull(mean)
  
  
  elevation 
  
  
}



# Función para verificar si el centroide está dentro del polígono
centroide_en_poligono <- function(poligono, centroide) {
  # Convertir el centroide a un objeto sf (en caso de no estarlo)
  centroide_sf <- st_as_sf(data.frame(geometry = st_sfc(centroide)), crs = st_crs(poligono))
  
  poligono <- st_make_valid(poligono) 
  poligono_valido <- st_is_valid(poligono)
  
  if(isFALSE(poligono_valido)){
    NULL
  }else{
    # Verificar si el centroide está dentro del polígono
    st_within(centroide_sf, poligono, sparse = FALSE)[1,1]
  }
  
}


summariza_factores <- function(data_factores) {
  no_visitas = length(unique(data_factores$numero_visita))
  
  
  data_factores %>% #select(-ID_Productor) %>%
    select(-c(numero_visita, fecha_visita, variedades_cacao_numero, variedad_cacao_dominante, 
              podas_tipo, contains("jornales"))) %>%
    summarise(across(everything(), ~sum(., na.rm = T))) / no_visitas
  
  
}


summariza_jornales <- function(data_factores) {
  # no_visitas = length(unique(data_factores$numero_visita))
  
  
  data_factores %>% #select(-ID_Productor) %>%
    select(c(contains("jornales"))) %>%
    summarise(across(everything(), ~sum(., na.rm = T))) 
  
  
}



# Función para calcular la moda
get_mode <- function(x) {
  x <- x[!is.na(x)]  # Eliminar NA
  if (length(x) == 0) return(NA)  # Retornar NA si todos los valores son NA
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}