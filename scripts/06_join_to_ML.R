#### Join for ML
fecha_corte = "2023-09-30"



list.files("data/2_archivos_procesados/", pattern = "RData", full.names = TRUE) %>% 
  map(~load(file = .x))
  

#caracterizacion_base
# caracterizacion_factores <- caracterizacion_to_join %>% select(where(is.factor)) %>%
#   select(-c(nombre_tecnico, vereda_poblado, municipio, contains("georreferencia"), organizacion_afil))
# #skim(factores_categorizacion)
# 
# 
# caracterizacion_numericas <- caracterizacion_no_outliers %>% 
#   select(ID_Productor, where(is.numeric), -contains("producido")) %>%
# skim(caracterizacion_numericas)

#spatial and ext data
chirp_data_ciclos <- chirp_data_poa %>% 
  filter(date >= as_date("2022-09-01")) %>%
  mutate(ciclo = case_when(date <= as_date(fecha_corte) ~ "2022_2023", 
                           date > as_date(fecha_corte) ~ "2023_2024")) %>%
  group_by(ID_Productor, ciclo) %>%
  summarise(sum_rain = sum(value), .groups = 'drop') %>% 
  pivot_wider(names_prefix = "lluvia_", names_from = ciclo, values_from = sum_rain) %>%
  left_join(chirp_data_poa_anual %>%
              summarise(lluvia_normal = median(rain_anual)))
  

#pgf - extension
# promedio_visitas
# variedad_cacao_dominante




test_data <- list(caracterizacion_no_outliers, 
             spatial_data_basic, select(comparativa_productividad, - departamento),
             chirp_data_ciclos, promedio_visitas, variedad_cacao_dominante, 
             test_factores_exito2 %>% select(-cultivo_complementario)) %>%
  reduce(left_join) %>% 
  select(-no_visitas) %>%
  mutate(across(contains("lag_visita"), ~na_if(., Inf)), 
         across(contains("lag_visita"), ~na_if(., -Inf))) %>%
  mutate(across(contains("jornales"), ~ ifelse(. >1000, NA, .)))

save(test_data, file= "data/2_archivos_procesados/test_data_models.RData")

#skim(test_data)


# Split the test_data data into test and train sets
set.seed(246)
test_data_f1 <- test_data %>% filter(status == "Activo") %>%
  select(-contains("georrefer"), -contains("vereda"), 
         -contains("_producido"), 
         -contains("_produccion"),
         -contains("area"), 
         -contains("fecha"), 
         -cluster,
         #-contains("rendimiento"),
#         -latitude, 
#         -elevacion_cultivo,
         -contains("uso_suelo"),
         -titulo_propiedad,
#        -longitude,
         -departamento,
         -nombre_tecnico, 
         -organizacion_afil, 
         -cultivo_complementario, 
         -municipio,
         -cuenta_bancaria_entidad,
         - status) 



numeric_data <- test_data %>% select(ID_Productor, where(is.numeric))


prep_recipe_numeric <- 
  recipe( ~ ., data = numeric_data)  %>%
  update_role(ID_Productor, new_role = "ID")  %>% 
  step_filter_missing(all_predictors(), threshold = 0.3) %>% 
  step_nzv(all_numeric()) %>%
  
 # step_date(arrival_date, features = c("dow", "week", "month")) %>%
  
  step_corr(all_numeric_predictors(), threshold = 0.9) %>% 
  
  # Impute missing values using the knn imputation algorithm
  step_impute_knn(all_predictors()) %>% 
  
  # Create dummy variables for all nominal predictors
  step_dummy(all_nominal_predictors()) %>%
  
  step_nzv(all_numeric()) %>%
  prep()


#

# Apply the recipe to the data
filtered_test_data <- 
  prep_recipe_numeric %>% 
  bake(new_data = NULL)

filtered_test_data %>% vis_miss()

# Identify the features that were removed
tidy(prep_recipe_numeric, number = 3)

split <- filtered_test_data %>% initial_split(prop = 3/4, strata = NULL)
test <- testing(split)
train <- training(split)







