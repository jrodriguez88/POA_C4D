## Analisis Inicial PGF
## github.com/jrodriguez88
## Agosto 2024



## Factores de exito
## Identificar
## explorar
## renombrar 
## Recodificar (Si = 1, No = 0, NA = N/A según Cacaograma)
##             (Presencia = 1, Ninguno = 0, NA = empty)
#library(naniar)
# load("raw_data.RData")
# load("caracterizacion_to_join.RData")
# load("productividad_to_join.RData")
# load("pgf_to_join.RData")
# load("Rendimiento_base.RData")


fecha_corte = "2023-09-30"


skim(pgf_to_join)


# # explore
# factores_exito_sub1$`1.1.1. Aplicó fertilizante edafico según Cacaograma` %>% table
# 
# factores_exito_sub1$`1.9.3. ¿Qué productos pecuarios de seguridad alimentaria para el consumo familiar, produce en su finca?` %>% 
#   str_subset("Ninguno")
# 
# factores_exito_sub1$`1.9.1. ¿Qué productos agrícolas de seguridad alimentaria para el consumo familiar, produce en su finca?` %>%
#   str_subset("Ninguno") 
# 
# factores_exito_sub1$`1.10.1. Seleccione que otra(s) fuente de ingresos tiene` %>% 
#   str_subset("Ninguno")
# 
# factores_exito_sub1$`1.7.1. Actividades de Cosecha y Beneficio` %>% table
# 
# factores_exito_sub1[factores_exito_sub1 %>% map(unique) %>% map_dbl(length) < 10] %>%
  # map(table)

factores_newnames <- c(
  ID_Productor = "ID_Productor",
  numero_visita = "Especifique el número de visita:",
  fecha_visita = "Fecha de Visita:",
  semana_cacaograma = "Ingrese Semana Cacaograma:",
  fertilizante_edafico = "1.1.1. Aplicó fertilizante edafico según Cacaograma",
  fertilizante_organico = "1.2.1. Aplicó fertilizante orgánico según Cacaograma",
  enmiendas = "1.3.1. Aplicó enmiendas según Cacaograma",
  #  mipe_hormiga = "Hormiga Arriera y/o Polvo de Tabaco – Identifica y ejerce control químico u orgánico al hormiguero - Semanal Control2",
  mipe_monilia = "Monilia – Enterrar o colocar bajo la hojarasca frutos enfermos - Semanal Control",
  mipe_escoba_bruja = "Escoba de Bruja – Realizar control de escoba de bruja en días secos - Semanal Control",
  mipe_phytophthora = "Phytophthora – Enterrar o colocar bajo la hojarasca frutos enfermos - Semanal Control",
  mipe_carmenta = "Carmenta – Identificar, cortar y enterrar los frutos afectados - Semanal Control",
  mipe_monalonion = "Monalonion – Identificar, cortar y enterrar los frutos afectados - Semanal Control",
  podas = "1.4.1. Aplicó podas según Cacaograma",
  podas_tipo = "1.4.1.1. Tipo de poda que aplicó",
  myc_malezas = "1.6.1. Aplicó actividades de Manejo y Control de Malezas según Cacaograma",
  myco_malezas_tipo = "Controlar malezas vía herbicida o manual",
  cosecha_beneficio = "1.7.1. Actividades de Cosecha y Beneficio",
  cultivo_complementario = "1.8.1. Nombre cultivo complementario:",
  area_cultivo_complementario = "1.8.5. Área en Has del Cultivo Complementario",
  produccion_agricola_consumo = "1.9.1. ¿Qué productos agrícolas de seguridad alimentaria para el consumo familiar, produce en su finca?",
  produccion_pecuaria_consumo = "1.9.3. ¿Qué productos pecuarios de seguridad alimentaria para el consumo familiar, produce en su finca?",
  otras_fuentes_ingresos = "1.10.1. Seleccione que otra(s) fuente de ingresos tiene",
  registra_produccion_ventas = "1.10.1. Completó registro de producción y ventas",
  actualiza_poster = "1.10.2. Actualizó póster (Calendario)",
  sistema_riego = "1.11.1. ¿Cuenta con sistema de riego en operación?",
  drenajes = "1.11.2. ¿Realizó adecuación y/o construcción de drenajes?"#,
)



# Subset_visitas
visitas_c4d <- pgf_to_join  %>% select(ID_Productor, numero_visita, fecha_visita) 

# Calcular la diferencia de días entre visitas para cada productor
visita_mas_tardada <- visitas_c4d %>%
  group_by(ID_Productor) %>%
  arrange(fecha_visita) %>%
  mutate(diferencia_dias = as.numeric(difftime(fecha_visita, lag(fecha_visita), units = "days"))) %>%
  # filter(!is.na(diferencia_dias)) %>% # Excluir el primer registro, que no tiene diferencia
  slice_max(diferencia_dias, with_ties = FALSE) %>% # Obtener la visita con la mayor diferencia de días
  select(ID_Productor, visita_mas_tardada = numero_visita)

visita_menos_tardada <- visitas_c4d %>%
  group_by(ID_Productor) %>%
  arrange(fecha_visita) %>%
  mutate(diferencia_dias = as.numeric(difftime(fecha_visita, lag(fecha_visita), units = "days"))) %>%
  # filter(!is.na(diferencia_dias)) %>% # Excluir el primer registro, que no tiene diferencia
  slice_min(diferencia_dias, with_ties = FALSE) %>% # Obtener la visita con la mayor diferencia de días
  select(ID_Productor, visita_menos_tardada = numero_visita)


# Calcular la diferencia de días entre visitas para cada productor
ultima_visita_tiempo <- visitas_c4d %>%
  group_by(ID_Productor)  %>%
  mutate(dias_utima_visita = abs(as.numeric(difftime(max(fecha_visita), today(), units = "days"))))  %>%
  select(ID_Productor, dias_utima_visita) %>% distinct()  %>% ungroup()


# Calcular la diferencia de días entre visitas para cada productor
promedio_visitas <- visitas_c4d %>%
  group_by(ID_Productor) %>% 
  arrange(fecha_visita) %>%
  mutate(diferencia_dias = as.numeric(difftime(fecha_visita, lag(fecha_visita), units = "days"))) %>% 
  
#  slice(1:1000) %>% view
#  filter(!is.na(diferencia_dias)) %>% # Excluir el primer registro, que no tiene diferencia
  summarise(n_visitas = n(), 
            promedio_dias_entre_visitas = mean(diferencia_dias, na.rm = TRUE),
            max_lag_visita = max(diferencia_dias, na.rm = TRUE),
            min_lag_visita = min(diferencia_dias, na.rm = TRUE),
            ) %>% 
  left_join(visita_mas_tardada) %>%
  left_join(visita_menos_tardada) %>%
  left_join(ultima_visita_tiempo) %>%
  mutate(visita_mas_tardada = as.numeric(visita_mas_tardada),
         visita_menos_tardada = as.numeric(visita_menos_tardada),
         max_lag_last_visit = n_visitas == visita_mas_tardada,
         min_lag_penult_visit = (n_visitas -1) == visita_menos_tardada)


# # Ver el resultado
# print(promedio_visitas)







# pgf_to_join %>% select(any_of(factores_newnames %>% enframe() %>% pull(name))) 



# pgf_to_join %>% select(ID_Productor, numero_visita, fecha_visita, contains("podas")) %>%
#   arrange(ID_Productor)

test_pgf <-  pgf_to_join %>% #skim#select(any_of(factores_newnames %>% enframe() %>% pull(name))) %>%skim
  mutate(across(.cols = c(fertilizante_edafico:podas, myc_malezas, myco_malezas_tipo),
                ~case_when(.x == "Si" ~ 1,
                           .x == "N/A según Cacaograma" ~ NA,
                           .x == "No hay" ~ NA,
                           is.na(.x) ~ NA,
                           TRUE ~ 0)),
         across(.cols = c(cultivo_complementario, produccion_agricola_consumo, produccion_pecuaria_consumo, otras_fuentes_ingresos), 
                ~ case_when(.x == "Ninguno" ~ 0,
                            .x =="No" ~ 0,
                            is.na(.x) ~ NA,
                            TRUE ~  1)),
         across(.cols = c(sistema_riego, drenajes, registra_produccion_ventas,
                          actualiza_poster, cosecha_beneficio, fermenta_cajones_madera,
                          destina_4dias_secado, selecciona_excelso_pasilla, conoce_humedad_venta,
                          fermenta_5dias, fermenta_separado_fino, empaca_sacos_fique, secado_pisos_madera,
                          beneficia_separado_sanos), 
                ~case_when(.x == "Si" ~ 1,
                           .x == "N/A" ~ NA,
                           is.na(.x) ~ NA,
                           TRUE ~ 0)) 
        # across(.cols = aplica_recomendaciones, 
        #        ~ case_when(.x == "No la Aplicó" ~ 0,
        #                    .x == "Si, Parcialmente" ~ 1,
        #                    .x == "Si, Totalmente" ~ 2, 
         #                   TRUE ~ NA))
         ) %>%
  select(-c(semana_cacaograma)) %>%#, -contains("variedad"), -contains("fermenta"), 
         # -contains("destina"), -contains("selecciona"), -contains('conoce_humedad'), 
         # -contains("empaca_sacos_fique"),
         # -contains("secado_pisos_madera"),
         # -contains("beneficia_separado_sanos")) %>% 
  arrange(ID_Productor)

# caracterizacion_to_join %>%
#   filter(fecha_visita < as.Date("2023-12-31")) #%>% 

# skim(pgf_to_join)
# skim(test_pgf)

# productividad_mensual_agg
# 
# 
# table(productividad_anual_agg$proporcion_tiempo_2022_2023>0.75 )
# table(productividad_anual_agg$proporcion_tiempo_2023_2024>0.75 )
# 




variedad_cacao_dominante <- pgf_to_join %>% select(ID_Productor, contains("variedad")) %>% 
  arrange(ID_Productor) %>% 
  group_by(ID_Productor) %>%
  summarise(variedad_cacao_dominante = get_mode(variedad_cacao_dominante),
            variedades_cacao_numero = get_mode(variedades_cacao_numero))


#table(cultivo_complementario_principal$cultivo_complementario_principal, cultivo_complementario_principal$cultivo_complementario)
  

# pgf_to_join %>% count(actualiza_poster ) %>% arrange(desc(n)) #%>% view()




pgf <- test_pgf %>%
  mutate(ciclo = case_when(fecha_visita <= as_date(fecha_corte) ~ "2022_2023", 
                           fecha_visita > as_date(fecha_corte) ~ "2023_2024"))  %>% 
  nest(data = -c(ID_Productor, ciclo))


pgf2 <- test_pgf %>%
  nest(data = -c(ID_Productor))

#  tt <- pgf %>%
#    slice(15) %>% 
#    unnest(data) %>% select(-ID_Productor, -ciclo)
#  
#  
#  data_factores <- tt
# 
# tt %>% select(fecha_visita , contains("poda"))




test_factores_exito <- pgf %>% 
  mutate(test_factores = map(data, summariza_factores)) %>% 
  mutate(test_jornales = map(data, summariza_jornales)) %>% 
  select(-data)  %>% 
  unnest(c(test_factores)) %>% 
  unnest(c(test_jornales))  %>% 
  pivot_wider(
    names_from = ciclo,  # Columna que contiene los ciclos
    values_from = -c(ID_Productor, ciclo), # Columnas a pivotar
    names_sep = "_",     # Para separar las variables por el ciclo
    names_glue = "{.value}_{ciclo}"  # Para construir nombres de columna con el nombre de la variable seguido del ciclo
  )


test_factores_exito2 <- pgf2 %>% 
  mutate(test_factores = map(data, summariza_factores)) %>% 
  mutate(test_jornales = map(data, summariza_jornales)) %>% 
  select(-data)  %>% 
  unnest(c(test_factores)) %>% 
  unnest(c(test_jornales))  #%>% 
  # pivot_wider(
  #   names_from = ciclo,  # Columna que contiene los ciclos
  #   values_from = -c(ID_Productor, ciclo), # Columnas a pivotar
  #   names_sep = "_",     # Para separar las variables por el ciclo
  #   names_glue = "{.value}_{ciclo}"  # Para construir nombres de columna con el nombre de la variable seguido del ciclo
  # )



# test_factores_exito$registra_produccion_ventas_2022_2023 %>% hist
# test_factores_exito$registra_produccion_ventas_2023_2024 %>% hist
# 
# test_factores_exito$actualiza_poster_2022_2023 %>% hist
# test_factores_exito$actualiza_poster_2023_2024 %>% hist

save(promedio_visitas, test_pgf, 
     variedad_cacao_dominante, test_factores_exito, test_factores_exito2, file = "data/2_archivos_procesados/test_factores_exito.RData")


test_factores_exito2 %>% select(any_of(factores_newnames %>% enframe %>% pull(name)))



  
