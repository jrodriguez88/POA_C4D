## Ingenieria de caracteristicas 
## github.com/jrodriguez88
## Agosto 2024


## load requeriments
#load("ini_data.RData")


# Key Index for join
key_index <- plan_gestion_finca %>% 
  select(ID_Productor,
         fecha_visita = `Fecha de Visita:`,
         numero_visita = `Especifique el número de visita:`,
         `_parent_index` = `_index`)


# Crear DataFrame nombre meses 
df_meses <- data.frame(
  mes_nombre = c(
    "Enero", "Febrero", "Marzo", "Abril", "Mayo", 
    "Junio", "Julio", "Agosto", "Septiembre", 
    "Octubre", "Noviembre", "Diciembre"),
  mes = 1:12)

#############################
## Caracterizacion
#############################

#####
### Seleccionar y renombrar variables de caracterizacion - Diccionario 1


# lAs variables resultado del script 02_Reduc... 
# caracterizacion_filter$value
# caracterizacion_newnames %>% names 
### Limpieza datos

caracterizacion_to_join <- caracterizacion %>% 
  select(all_of(caracterizacion_newnames)) %>% 
  mutate(across(contains("area"), as.numeric),
    #     mutate(across(where(is.character), ~replace_with_na(., "<NA>"))),
         across(where(is.POSIXct), as.Date),
         # Nombre de organizacion - upper case
         organizacion_afil = toupper(organizacion_afil),
         # Estandarizacion nombres (capital letter)
         across(.cols = c(cedula_cacaotera, 
                          departamento,
                          municipio,
                          sexo, 
                          lee_escribe, 
                          habita_territorio_etnico, 
                          tenencia_tierra, 
                          cuenta_bancaria, 
                          sistema_agroforestal, 
                          cultivo_autoconsumo,
                          cria_animales_autoconsumo), str_to_title),
         #Calculo edad de productor
         edad_productor = as.numeric(interval(fecha_nacimiento, Sys.Date()), "years"),
         antiguedad_proyecto = as.numeric(interval(fecha_visita, Sys.Date()), "years")) %>%
  # Antiguedad arboles
#  mutate(edad_arboles = case_when(no_arboles_sembrados_2017_antes ==))  %>% 
    mutate(across(where(is.character), as.factor)) #%>%
 
# column_to_rownames(var = "ID_Productor")

# caracterizacion_to_join$cacao_seco_vendido %>% hist
# caracterizacion_to_join$cacao_baba_vendido %>% hist
# caracterizacion_to_join$area_cacao_produccion %>% hist
# caracterizacion_to_join$area_cultivo_declarada %>% hist
# caracterizacion_to_join$area_cultivo_georref %>% hist
# 





skim(caracterizacion_to_join)

#caracterizacion_to_join$edad_cultivo %>% boxplot()
# histograma_c4d_data(caracterizacion_no_outliers %>% select(where(is.numeric), ID_Productor))


# caracterizacion_to_join   %>%
#   mutate(across(contains("area"), ~na_if(., 0)),
#          # across(contains("vendido"), ~na_if(., 0)),
#          no_arboles_sembrados = na_if(no_arboles_sembrados, 0)) %>%
#   mutate(across(where(is.numeric), ~na_if(., Inf)), across(where(is.numeric), ~na_if(., -Inf))) %>%
#            mutate(across(contains("area"), ~ ifelse(. < 0.1, NA, .))) %>%
#            mutate(rendimiento_2022_2023 = produccion_total_2022_2023/area,
#                   rendimiento_2023_2024 = produccion_total_2023_2024/area) %>% #skim()
#            mutate(across(contains("rendimiento"), ~ ifelse(. < 50, NA, .))) %>%
#            mutate(across(contains("visitas"), ~ ifelse(. <= 2, NA, .))) %>%
#            mutate(across(where(is.numeric), 
#                          ~replace_outlier(., coef_iqr = 3))) %>% skim


# 
# 
# table(
#   caracterizacion_to_join$departamento,
#   caracterizacion_to_join$no_arboles_sembrados_2017_antes < caracterizacion_to_join$no_arboles_sembrados*0.50
#       )
# table(caracterizacion_to_join$departamento,
#   caracterizacion_to_join$no_arboles_sembrados_2018_adelante > caracterizacion_to_join$no_arboles_sembrados*0.5)
# 
# table(caracterizacion_to_join$no_arboles_sembrados_2017_antes > caracterizacion_to_join$no_arboles_sembrados)
# 
# table(caracterizacion_to_join$no_arboles_sembrados_2018_adelante > caracterizacion_to_join$no_arboles_sembrados)
# 
# caracterizacion_to_join %>%
#   filter(caracterizacion_to_join$no_arboles_produccion > caracterizacion_to_join$no_arboles_sembrados) 
# 
# 
# 
# caracterizacion_to_join %>%
#   filter(caracterizacion_to_join$no_arboles_sembrados_2018_adelante > caracterizacion_to_join$no_arboles_sembrados) %>%
#   skim
#   
# caracterizacion_to_join %>%
#   filter(caracterizacion_to_join$no_arboles_sembrados_2017_antes > caracterizacion_to_join$no_arboles_sembrados) %>%
#   count(departamento) %>% arrange(desc(n)) #%>% arrange(desc(departamento))
# 
# caracterizacion_to_join %>%
#   filter(caracterizacion_to_join$no_arboles_produccion > caracterizacion_to_join$no_arboles_sembrados) %>%
#   count(departamento) %>% arrange(desc(n)) #%>% arrange(desc(departamento))
# 
# 
# caracterizacion %>% select(contains("propor")) %>% names 

# save(caracterizacion_to_join, file = "caracterizacion_to_join.RData")


#############################
## Productividad
#############################





# Limpieza datos

productividad_to_join <- productividad %>% 
  left_join(key_index) %>% 
  select(ID_Productor, fecha_visita, numero_visita, `2.2. Año a registrar`, all_of(productividad_filter$value)) %>%
  rename(all_of(productividad_newnames)) %>%
  left_join(df_meses, join_by(mes_registro == mes_nombre)) %>%
  mutate(date_registro = make_date(year = year_registro, month = mes), 
         mes_reg =  month(date_registro, label= T)) %>%
  mutate(across(where(is.POSIXct), as.Date),
         across(where(is.character), as.factor),
         numero_visita = as.factor(numero_visita))

skim(productividad_to_join)
# save(productividad_to_join, file = "productividad_to_join.RData")


#############################
## Plan Gestion Finca
#############################
# V1



pgf_to_join <- plan_gestion_finca %>% select(all_of(pgf_filter$value)) %>% 
 # rename(all_of(factores_newnames)) %>% 
  select(all_of(pgf_newnames)) %>% #skim
  mutate(across(where(is.POSIXct), as.Date),
#         across(where(is.character),  ~ ifelse(. == "<NA>", NA, .)),
       across(where(is.character), as.factor),
       numero_visita = as.factor(numero_visita)) #%>% skim

skim(pgf_to_join)


# pgf_na_filte2 <- miss_var_summary(plan_gestion_finca %>% 
#                                     select(all_of(pgf_filter$value)) ) %>% 
#   filter(pct_miss <= 90) %>%
#   pull(variable)

# plan_gestion_finca$`Secar el cacao en pisos de madera` %>% head
# plan_gestion_finca$`Secó el cacao en pisos de madera` %>% head
# plan_gestion_finca$`Fermentar por separado el fino y el corriente` %>% head
# 
# plan_gestion_finca$`Manejó de coberturas vegetales` %>% head(20)
# plan_gestion_finca$`Manejar coberturas vegetales` %>% head(20)

# save(pgf_to_join, file = "pgf_to_join.RData")

#skim(plan_gestion_finca)


# pgf_to_join$variedad_cacao_dominante %>% table 
# 
# pgf_to_join %>%  select(where(is.factor)) 
#   # map_dbl(n_unique) %>% enframe %>% arrange(desc(value))
# pgf_to_join$cultivo_complementario %>% 
#     top_counts(max_char = 20, max_levels = 20) %>% 
#     str_split(",")
# 
# 
# pgf_to_join$otras_fuentes_ingresos  %>% 
#   top_counts(max_char = 20, max_levels = 20) %>% 
#   str_split(",")
# 
# 
# pgf_to_join %>%
#   count(otras_fuentes_ingresos) %>% arrange(desc(n))


save(caracterizacion_to_join, pgf_to_join, productividad_to_join, 
     file = "data/2_archivos_procesados/data_selected.RData")





















