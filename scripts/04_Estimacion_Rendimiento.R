## Estimacion de rendimiento
## github.com/jrodriguez88
## Agosto 2024

# Requeriments
#library(outliers)

# load("caracterizacion_to_join.RData")
# load("productividad_to_join.RData")
# load("pgf_to_join.RData")


## Rendimiento Base - Declarado productor


rendimiento_base_outliers <- caracterizacion_no_outliers %>% 
  #Seleccion de features
  #caracterizacion_to_join %>%
  filter(status == "Activo") %>%
  select(ID_Productor, departamento, edad_cultivo,
         contains("producido"), 
         contains("vendido"), 
         contains("area"), 
         contains("arboles"),
         -contains("complemet"))  %>% #skim()
  
  # Calculo de nuevos features
  mutate(
    produccion_total = map2_dbl(.x = cacao_seco_vendido,
                                .y = cacao_baba_vendido,
                                ~sum(.x , (.y/3), na.rm = TRUE))) %>%
  
  mutate(across(contains("produccion_total"), ~ ifelse(. < 50, NA, .))) %>%
  mutate(
    rendimiento_area_cultivo = produccion_total/area_cultivo_georref,
    rendimiento_area_produccion = produccion_total/area_cacao_produccion,
    rendimiento_arboles_sembrados = produccion_total/no_arboles_sembrados,
    rendimiento_arboles_produccion = produccion_total/no_arboles_produccion,
    densidad_siembra_cultivo = no_arboles_sembrados/area_cultivo_georref,
    densidad_siembra_produccion = no_arboles_produccion/area_cacao_produccion,
    )  %>% 

  select(ID_Productor, departamento, produccion_total, 
         contains("densidad"), 
         contains("rendimiento")) %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)), across(where(is.numeric), ~na_if(., -Inf)) ) %>%
  mutate(across(contains("rendimiento_area"), ~ ifelse(. < 50, NA, .))) %>%
  mutate(across(contains("densidad"), ~ ifelse(. < 100, NA, .))) %>%
  mutate(across(contains("rendimiento_arboles"), ~ ifelse(. < 0.1, NA, .))) %>%
  #       rendimiento_area_produccion = na_if(rendimiento_area_produccion, 0)) %>%
  
  mutate(departamento = str_to_title(departamento)) %>% 
  
  # Outlier detection with Max  an Min ranges - 
  
  mutate(is_outlier_rendimiento = rendimiento_area_produccion > 2000 | rendimiento_area_produccion < 50,
         is_outlier_densidad = densidad_siembra_produccion > 2500 | densidad_siembra_produccion < 200,
         is_outlier_arboles = rendimiento_arboles_produccion > 2.5 | rendimiento_arboles_produccion < 0.05) #%>%
#  filter(!is_outlier, !is_outlier2, !is_outlier3) 




# skim(rendimiento_base_outliers)
# histograma_c4d_data(rendimiento_base_outliers)
# 
# summary_rendimiento <- partition(skim(rendimiento_base_outliers, where(is.numeric)))

vis_miss(rendimiento_base_outliers %>% 
           select(-contains("outlier"), -ID_Productor, - departamento), 
         cluster = F)

vis_miss(rendimiento_base_outliers %>% 
           select(-contains("outlier"), -ID_Productor), 
         cluster = F, facet = departamento)

## Pruebas

# # ## Eliminando primer filtro 
#  rendimiento_base_log_outliers <- rendimiento_base_outliers %>% 
#    filter(!is_outlier_rendimiento, !is_outlier_densidad, !is_outlier_arboles)
# # 
# test_no_outliers_median_impute <- rendimiento_base_outliers %>% 
#   mutate(across(where(is.numeric), ~replace_outlier(., coef_iqr = 2, fill = "median")))
# 
# test_no_outliers_median_impute2 <- rendimiento_base_log_outliers %>% 
#   mutate(across(where(is.numeric), ~replace_outlier(., coef_iqr = 2, fill = "median")))
# 
# 
# # easy
 # test_no_outliers_2IQR <- rendimiento_base_outliers %>% 
 #   mutate(across(where(is.numeric), ~replace_outlier(.)))
 # 
 # skim(test_no_outliers_2IQR)
# 
# test_no_outliers_log1_default <- rendimiento_base_log_outliers %>% 
#   mutate(across(where(is.numeric), ~replace_outlier(.)))


test_no_outliers_3IQR <- rendimiento_base_outliers %>%
  mutate(across(where(is.numeric), ~replace_outlier(., coef_iqr = 3)))
# 
 # skim(test_no_outliers_3IQR)
 # histograma_c4d_data(test_no_outliers_3IQR)
# 
# test_no_outliers_median_impute <- rendimiento_base_outliers %>% 
#   mutate(across(where(is.numeric), ~replace_outlier(., coef_iqr = 3, fill = "median")))
# 


# list(data_inicial_outliers = rendimiento_base_outliers, 
#      data_filtro1_outliers = rendimiento_base_log_outliers, 
#      test_outliers_inicial_2IQR = test_no_outliers_default, 
#      test_outliers_filtro1_2IQR = test_no_outliers_log1_default,
#      test_outliers_inicial_3IQR = test_no_outliers_3IQR, 
#      test_no_outliers_inicial_medianImpute_3IQR = test_no_outliers_median_impute) %>% 
#   map(~partition(skim(.x, where(is.numeric))))
# 
# list(data_inicial_outliers = rendimiento_base_outliers,
#      data_filtro1_outliers = rendimiento_base_log_outliers,
#      test_outliers_inicial_2IQR = test_no_outliers_default, 
#      test_outliers_inicial_3IQR = test_no_outliers_3IQR, 
#      test_no_outliers_inicial_medianImpute_3IQR = test_no_outliers_median_impute) %>% 
#   map(~partition(skim(.x, where(is.numeric))))
# 
# 
# list(data_inicial_outliers = rendimiento_base_outliers,
#      data_filtro1_outliers = rendimiento_base_log_outliers,
#      test_outliers_inicial_2IQR = test_no_outliers_default, 
#      test_outliers_inicial_3IQR = test_no_outliers_3IQR, 
#      test_no_outliers_inicial_medianImpute_3IQR = test_no_outliers_median_impute) %>% 
#   map(~mean(.x$rendimiento_area_produccion, na.rm = T))


# test_plots <- list(data_inicial_outliers = rendimiento_base_outliers,
#      data_filtro1_outliers = rendimiento_base_log_outliers,
#      test_outliers_inicial_2IQR = test_no_outliers_2IQR, 
#      test_outliers_inicial_3IQR = test_no_outliers_3IQR, 
#      test_no_outliers_inicial_medianImpute_3IQR = test_no_outliers_median_impute)
# 
#   map2(.x = test_plots, .y = names(test_plots), ~.x %>% drop_na %>%
#   mutate(departamento = str_to_title(departamento)) %>% 
#   mutate(departamento = fct_reorder(departamento, rendimiento_area_produccion, .fun='median')) %>%
#   select(ID_Productor, departamento, rendimiento_area_produccion) %>%
#   ggplot(aes(x = reorder(departamento, rendimiento_area_produccion),
#              y = rendimiento_area_produccion)) +
#   stat_summary(fun.data=mean_cl_boot, position=position_dodge(0.95), geom="errorbar")+
#   stat_summary(fun.data=mean_cl_boot, position=position_dodge(0.95), 
#                size=3, shape=21, fill="white", col = "red", geom = "point") + 
#   # geom_vline(xintercept = mean(rendimiento_base$rendimiento_area_produccion, na.rm = T), col = "red", linewidth = 1.2) + 
#   # annotate(y=mean(rendimiento_base$rendimiento_area_produccion, na.rm = T),
#   #        x=+Inf,label="Target Date",vjust=2,geom="label")
#   coord_flip() + theme_minimal() +
#   labs(title = paste0("Rendimiento del Cacao -  ", .y),
#        x = "Departamento", y = "Kg/ha"))




rendimiento_base <- test_no_outliers_3IQR
# skim(rendimiento_base)

 
## Promedio Rendimiento nacional 
## Promedio Rendimiento nacional 


summary_rendimiento_base <- rendimiento_base %>% #drop_na %>%
  mutate(departamento = str_to_title(departamento)) %>% 
  group_by(departamento) %>%
  summarise(n_productores = n(),
            promedio_rendimiento = mean(rendimiento_area_produccion, na.rm =T),
            media_rendimiento = median(rendimiento_area_produccion, na.rm =T),
            sd_rendimiento = sd(rendimiento_area_produccion, na.rm =T),
            lower_ci = quantile(rendimiento_area_produccion,0.025, na.rm =T),    
            upper_ci = quantile(rendimiento_area_produccion,0.975, na.rm =T)) %>%
  arrange(desc(n_productores))

# rendimiento_base %>% pivot_longer(-c(departamento, ID_Productor)) %>% 
#   group_by(departamento, name) %>%
#   summarise(n = n(),
#             promedio = mean(value, na.rm = T),
#             media = median(value, na.rm = T),
#             sd = sd(value, na.rm = T)) %>%
#   filter(name == "rendimiento_area_produccion")
# 
# rendimiento_base %>% pivot_longer(-c(departamento, ID_Productor)) %>% 
#   group_by(departamento, name) %>%
#   summarise(n = n(),
#             promedio = mean(value, na.rm = T),
#             media = median(value, na.rm = T),
#             sd = sd(value, na.rm = T)) %>%
#   filter(name == "densidad_siembra_cultivo")


#skim(rendimiento_base)




#### rendimiento_productividad

#productividad_to_join %>% skim


productividad_test <- productividad_to_join %>%
  group_by(ID_Productor) %>% nest() %>% ungroup 


# productividad_test %>%
#   slice(10) %>% 
#   unnest() %>% view()
# 
# 
 # tt <- productividad_test %>%
 #   slice(10) %>%
 #   unnest(data)
 # 


productividad_mensual_agg <- productividad_test %>%
  mutate(produccion_mensual = map(data, calcula_produccion_mensual)) %>%
  select(-data) %>%
  unnest(produccion_mensual)
  

productividad_anual_agg <- productividad_test %>% 
  mutate(produccion_anual = map(data, calcula_produccion_anual)) %>%
  unnest(produccion_anual)

# histograma_c4d_data(productividad_anual_agg %>% select(-data))


productividad_anual <- caracterizacion_to_join %>% 
  select(ID_Productor, area_produccion = area_cacao_produccion) %>%
  left_join(productividad_anual_agg %>% select(-data) %>%
              mutate(across(where(is.numeric), ~replace_outlier(., coef_iqr = 3)))) %>%
  mutate(across(contains("area"), ~ ifelse(. < 0.1, NA, .))) %>%
  mutate(rendimiento_2022_2023 = produccion_total_2022_2023/area_produccion) %>%
  mutate(rendimiento_2023_2024 = produccion_total_2023_2024/area_produccion) %>% #skim()
  mutate(across(contains("rendimiento"), ~ ifelse(. < 50, NA, .))) #%>%
  # filter(no_meses_productivos_2022_2023 >= 2) %>%
  # filter(no_meses_productivos_2023_2024 >= 2)
#  mutate(across(contains("meses_productivos"), ~ ifelse(. <= 3, NA, .))) 


# productividad_anual %>%
#   left_join(
#     caracterizacion_to_join %>% 
#       select(ID_Productor, departamento, cluster)) %>% 
#   count(departamento)
# 
# productividad_anual %>% drop_na
# 
# productividad_mensual_agg %>% filter(produccion_cacao > 5) %>%
#   left_join(
#     caracterizacion_to_join %>% 
#       select(ID_Productor, departamento, cluster)) %>% 
#   group_by(mes_reg) %>%
#   count(departamento) %>% 
#   pivot_wider(names_from = mes_reg, values_from = n)
# 
# 
# caracterizacion_to_join %>% 
#   group_by(departamento, fecha_inclusion) %>% 
#   summarise(No_Productores = n())  %>% ungroup() %>% 
#   group_by(departamento) %>%
#   mutate(No_Productores_total = cumsum(No_Productores)) %>%  
#   filter(fecha_inclusion < as.Date("2023-03-01")) %>% view()



###### MESES DE COSECHA

picos_cosecha <- data.frame(
  departamento = factor(str_to_title(
    c("Cesar", "Huila", "Cordoba", "Santander", "Antioquia", "Magdalena", 
      "Caldas", "Tolima",
      "Risaralda", "Meta", "Cundinamarca", "Cauca", 
      "Bolivar", "La Guajira")), 
                        levels = caracterizacion_to_join$departamento %>% levels()),
  mes_de_cosecha_1 = c("oct", "oct", "ene", "ene", "ene", "ene", "abr", "jun", "sept", "sept", "feb", "ago", "ene", "ene"),
  mes_de_cosecha_2 = c("nov", "nov", "oct", "oct", "feb", "feb", "may", "jul", "oct", "oct", "mar", "sept", "feb", "feb"),
  mes_de_cosecha_3 = c("dic", "dic", "nov", "nov", "nov", "mar", "jun", "ago", "nov", "nov", "abr", "oct", "mar", "mar"),
  mes_de_cosecha_4 = c(NA, NA, "dic", "dic", "dic", NA, "jul", NA, NA, NA, "may", NA, "nov", NA),
  mes_de_cosecha_5 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "dic", NA)) %>% 
  pivot_longer(cols = -departamento, values_to = "mes_reg") %>%
  mutate(mes_reg = factor(mes_reg, 
                          levels = productividad_mensual_agg$mes_reg %>% levels())) %>%
  select(-name) %>% drop_na(mes_reg) %>% mutate(departamento = str_to_title(departamento)) 


productividad_mensual_summary <- productividad_mensual_agg %>% left_join(
  caracterizacion_to_join %>% select(ID_Productor, departamento, cluster, status, fecha_visita)) %>%
#  filter(status =="Activo", fecha_inclusion < as.Date("2023-01-31")) %>%
  filter(produccion_cacao > 1) %>%
  drop_na(departamento) %>%# mutate(departamento = str_to_title(departamento)) %>%
  group_by(departamento, year_registro, mes_reg) %>%
  summarise(registros = n(),
    total_produccion = sum(produccion_cacao, na.rm = T)) 





# productividad_mensual_agg %>%
#   left_join(
#     caracterizacion_no_outliers %>% 
#       select(ID_Productor, departamento, cluster)) %>% 
#   drop_na() %>% filter(produccion_cacao <= 5000, produccion_cacao>10) %>%
#   ggplot(aes(mes_reg, produccion_cacao, fill = cluster)) +
# #  geom_smooth() +
#   stat_summary(fun.data = mean_cl_boot, shape=21, position=position_dodge(0.5)) +
#   facet_grid(~year_registro) +
#   theme_bw()
# 
# 
# 
# productividad_mensual_agg %>%
#   left_join(
#     caracterizacion_to_join %>% 
#       select(ID_Productor, departamento, cluster)) %>% 
#   drop_na() %>% filter(produccion_cacao <= 3000) %>%
#   ggplot(aes(mes_reg, produccion_cacao, fill=departamento)) +
#   stat_summary(fun.data = mean_cl_boot, shape=21, position=position_dodge(0.5)) +
#   facet_wrap(~cluster, scales = "free") + 
#   theme_bw()
# 
# productividad_mensual_agg %>%
#   left_join(
#     caracterizacion_to_join %>% 
#       select(ID_Productor, departamento, cluster)) %>% 
#   drop_na() %>% filter(produccion_cacao <= 3000) %>%
#   ggplot(aes(mes_reg, produccion_cacao, fill=cluster)) +
#   stat_summary(fun.data = mean_cl_boot, shape=21) +
#   facet_wrap(~cluster, scales = "free") + 
#   theme_bw() + guides(fill="none")





#### casos dos cluster

# caracterizacion_to_join %>% 
#       select(ID_Productor, departamento, municipio, cluster) %>% 
#   drop_na() %>% 
#   mutate(departamento = str_to_title(departamento)) %>% 
#   nest(-departamento) %>% 
#   mutate(clusters = map_dbl(data, ~length(unique(.x$cluster)))) %>%
#   filter(clusters > 1) 
  

  
casos_municipio_cluster <- caracterizacion_to_join %>% 
  select(ID_Productor, departamento, municipio, cluster) %>%
  drop_na() %>% 
  mutate(departamento = str_to_title(departamento)) %>% 
  nest(data = -c(departamento, municipio)) %>% 
  mutate(clusters = map_dbl(data, ~length(unique(.x$cluster)))) %>%
  filter(clusters > 1) %>% unnest(data) %>% nest(-cluster) %>% 
  slice(2) %>%
  pull(data)

write_csv(casos_municipio_cluster[[1]],
          file = "data/2_archivos_procesados/verificar_casos_error_cluster.csv")

ID_review <- caracterizacion_to_join %>%
  filter(ID_Productor %in% casos_municipio_cluster[[1]]$ID_Productor) %>% 
  pull(ID_Productor)







###### Crear comparativa productividad



comparativa_productividad <- caracterizacion_to_join %>% 
  select(ID_Productor, departamento)%>% 
  left_join(rendimiento_base %>% 
              select(ID_Productor, produccion_linea_base = produccion_total, 
                     rendimiento_linea_base = rendimiento_area_produccion,
                     densidad_siembra = densidad_siembra_produccion, 
                     rendimiento_por_arbol = rendimiento_arboles_produccion)) %>%
  left_join(productividad_anual) %>%
#  filter(rendimiento_2022_2023 < 1500, rendimiento_2023_2024 < 1500) %>% 
  mutate(no_visitas = numero_visitas_2022_2023 + numero_visitas_2023_2024) 


mean_2023_2024 <- mean(comparativa_productividad$rendimiento_2023_2024, na.rm = T)
mean_2022_2023 <- mean(comparativa_productividad$rendimiento_2022_2023, na.rm = T)
# Draw a histogram of the resample means


summary_rendimiento_2022_2023 <- caracterizacion_to_join %>% select(ID_Productor, departamento) %>%
  left_join(
    comparativa_productividad  %>% select(-departamento)) %>% 
  filter(proporcion_tiempo_2022_2023 > 0.75) %>%
  drop_na(rendimiento_2022_2023) %>%
  group_by(departamento) %>%
  summarise(n_productores = n(),
            promedio_rendimiento = mean(rendimiento_2022_2023, na.rm =T),
            media_rendimiento = median(rendimiento_2022_2023, na.rm =T),
            sd_rendimiento = sd(rendimiento_2022_2023, na.rm =T),
            lower_ci = quantile(rendimiento_2022_2023,0.025, na.rm =T),    
            upper_ci = quantile(rendimiento_2022_2023,0.975, na.rm =T)) %>%
  arrange(desc(n_productores))


summary_rendimiento_2023_2024 <- caracterizacion_to_join %>% select(ID_Productor, departamento) %>%
  left_join(
  comparativa_productividad  %>% select(-departamento)) %>%
  filter(proporcion_tiempo_2023_2024 > 0.75) %>%
  drop_na(rendimiento_2023_2024) %>%
  group_by(departamento) %>%
  summarise(n_productores = n(),
            promedio_rendimiento = mean(rendimiento_2023_2024, na.rm =T),
            media_rendimiento = median(rendimiento_2023_2024, na.rm =T),
            sd_rendimiento = sd(rendimiento_2023_2024, na.rm =T),
            lower_ci = quantile(rendimiento_2023_2024,0.025, na.rm =T),    
            upper_ci = quantile(rendimiento_2023_2024,0.975, na.rm =T)) %>%
  arrange(desc(n_productores))

# comparativa_productividad %>%
#   left_join(
#     caracterizacion_to_join %>% 
#       select(ID_Productor, cluster)) %>% 
#   drop_na(cluster) %>%
#   ggplot(aes(departamento, rendimiento_2023_2024, fill=departamento)) +
#   geom_boxplot() + 
#   # stat_summary(fun.data = mean_cl_boot, shape=21, geom = "boxblot") +
#   # facet_wrap(~cluster, scales = "free_x") + 
#   coord_flip() +
#   theme_bw()
# 
# comparativa_productividad %>%
#   left_join(
#     caracterizacion_to_join %>% 
#       select(ID_Productor, departamento, cluster)) %>%
#   select(ID_Productor, departamento, cluster, rendimiento_2022_2023, rendimiento_2023_2024) %>%
#   pivot_longer(cols = -c(ID_Productor, departamento, cluster)) %>%
#   ggplot(aes(name, value, color=name)) +
#   stat_summary(fun.data = mean_cl_boot) +
#   facet_wrap(~departamento) + 
#   theme_bw()
# 
# 
# left_join(comparativa_productividad %>% drop_na(rendimiento_2022_2023) %>%
#   mutate(departamento = str_to_title(departamento))  %>%
#   count(departamento) %>% arrange(desc(n)) %>% select(departamento, n1 = n),
# 
# comparativa_productividad %>% drop_na(rendimiento_2023_2024) %>%
#   mutate(departamento = str_to_title(departamento))  %>%
#   count(departamento) %>% arrange(desc(n)), by = "departamento")


filtro_dpts <- c("Magdalena", "La Guajira", "Meta", "Risaralda", "Cundinamarca", "Cauca")



# #############
# comparativa_productividad %>% drop_na(rendimiento_2023_2024) %>%
#   mutate(departamento = str_to_title(departamento))  %>%
#   mutate(departamento = fct_reorder(departamento, rendimiento_2023_2024, .fun='median')) %>%
#   count(departamento) %>% arrange(desc(departamento)) %>%
#   filter(!(departamento %in% filtro_dpts)) #%>% view()
# 
# 
# 
# comparativa_productividad %>% 
#   mutate(departamento = str_to_title(departamento)) %>% 
#   filter(!(departamento %in% filtro_dpts)) %>% filter(no_visitas > 4) %>%
#   mutate(departamento = fct_reorder(departamento, rendimiento_2023_2024, .fun='mean')) %>%
#   drop_na(rendimiento_2023_2024) %>% drop_na(rendimiento_2022_2023) %>%

# nrow(comparativa_productividad)

# comparativa_productividad %>% 
#   mutate(departamento = str_to_title(departamento)) %>% 
#   filter(!(departamento %in% filtro_dpts)) %>%
#   mutate(departamento = fct_reorder(departamento, rendimiento_2023_2024, .fun='mean')) %>%
#   
#   select(ID_Productor, departamento, contains("rendimiento")) %>% drop_na(rendimiento_2023_2024) %>%
#   drop_na(rendimiento_2022_2023) %>%
#   group_by(departamento) %>% count(departamento)



summary_productividad_2024 <- comparativa_productividad %>% 
  
  filter(proporcion_tiempo_2023_2024 > 0.6) %>%
  filter(proporcion_tiempo_2022_2023 > 0.6, no_visitas > 3) %>%
#  mutate(departamento = str_to_title(departamento)) %>% 
 # filter(!(departamento %in% filtro_dpts)) %>%
  mutate(departamento = fct_reorder(departamento, rendimiento_2023_2024, .fun='mean')) %>% 
  select(-contains("linea_base")) %>% drop_na %>%
  
  select(ID_Productor, departamento, contains("rendimiento")) %>% 
  #select(-rendimiento_linea_base ) %>% 
  group_by(departamento) %>% 
  # drop_na(rendimiento_2023_2024) %>%
  # drop_na(rendimiento_2022_2023) %>%
  summarise(n_productores = n(),
            promedio_rendimiento_2022_2023 = mean(rendimiento_2022_2023, na.rm =T),
            promedio_rendimiento_2023_2024 = mean(rendimiento_2023_2024, na.rm =T),
            sd_rendimiento_2022_2023 = sd(rendimiento_2022_2023, na.rm =T),
            sd_rendimiento_2023_2024 = sd(rendimiento_2023_2024, na.rm =T),
            lower_ci_2022_2023 = quantile(rendimiento_2022_2023,0.025, na.rm =T),    
            upper_ci_2022_2023 = quantile(rendimiento_2022_2023,0.975, na.rm =T),
            lower_ci_2023_2024 = quantile(rendimiento_2023_2024,0.025, na.rm =T),    
            upper_ci_2023_2024 = quantile(rendimiento_2023_2024,0.975, na.rm =T)) %>%
  arrange(desc(n_productores))

write_csv(summary_productividad_2024, "data/2_archivos_procesados/summary_rendimiento_2022_2024.csv")



  
# 
# comparativa_productividad %>% filter(no_visitas>5)




  
save(rendimiento_base_outliers, rendimiento_base, comparativa_productividad, picos_cosecha,
     productividad_anual_agg, productividad_anual, 
     summary_rendimiento_base, summary_rendimiento_2022_2023, summary_rendimiento_2023_2024, summary_productividad_2024, 
     productividad_mensual_summary, productividad_mensual_agg, file = "data/2_archivos_procesados/rendimiento_base.RData")


# 
# load("data/2_archivos_procesados/rendimiento_base.RData")



