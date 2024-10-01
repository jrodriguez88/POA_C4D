## Reduccion de dimensionalidad
## github.com/jrodriguez88
## Agosto 2024

## 4 filtros 
# Por % de datos faltantes
# Por varianza cero (<0.01)
# por redundancia y alta correlacion
# Por agrupacion de cluster jerarquico y criterio

### Filter based-missing values
#load("ini_data.RData")

na_pct_filter <- 80

# Discard features with 80% missing values
caracterizacion_na_filter <- miss_var_summary(caracterizacion) %>% 
  filter(pct_miss <= na_pct_filter) %>%
  pull(variable)

pgf_na_filter <- miss_var_summary(plan_gestion_finca) %>% 
  filter(pct_miss <= na_pct_filter) %>%
  pull(variable)

productividad_na_filter <- miss_var_summary(productividad) %>% 
  filter(pct_miss <= 95) %>%
  pull(variable)



# Variance analysis

caracterizacion_varZero_filter  <- calc_variance_df(caracterizacion) %>% 
  filter(variance < 0.1) %>% pull(feature)

productividad_varZero_filter <- calc_variance_df(productividad) %>% 
  filter(variance < 0.1) %>% pull(feature)

pgf_varZero_filter <- calc_variance_df(plan_gestion_finca) %>% 
  filter(variance < 0.1) %>% pull(feature)


# Por agrupacion de cluster jerarquico y criterio

caracterizacion_group <- caracterizacion %>%
  select(all_of(c(caracterizacion_na_filter)), 
         -caracterizacion_varZero_filter) %>% 
  names() %>% enframe
 # group_names_by_hcluster()

#caracterizacion_group %>% view()

pgf_group <- plan_gestion_finca %>%
  select(all_of(c(pgf_na_filter, pgf_varZero_filter)), 
         -pgf_varZero_filter) %>%
  names() %>% enframe
  # group_names_by_hcluster(h_cutree = 0.4)

#pgf_group %>% view()

productividad_group <- productividad %>%
  select(all_of(c(productividad_na_filter, productividad_varZero_filter)), 
         -productividad_varZero_filter) %>%
  names() %>% enframe
  # group_names_by_hcluster(h_cutree  = 0.35)

#productividad_group %>% view()


caracterizacion_filter <- caracterizacion_group %>%
#  filter(!(group %in% c(1, 2, 11, 15, 16))) %>% 
  filter(str_detect(.$value, 
                    pattern = "Foto|foto|_id|index|Reporte|URL|formulario|satelital|georreferenciando|device|version", 
                    negate = T)) 

pgf_filter <- pgf_group %>%
#  filter(!(group %in% c(7, 8, 14, 15))) %>% 
  filter(str_detect(.$value, 
                    pattern = "Foto|_id|Reporte|URL|formulario|satelital|_fv$|version|device", negate = T)) 

productividad_filter <- productividad_group %>%
 # filter(!(group %in% c(1, 6, 8, 9))) %>% 
  filter(str_detect(.$value, 
                    pattern = "Foto|foto|_id|Reporte|URL|formulario|table|^_index|submission|Atipico", negate = T)) 



## Analisis de correlacion


pgf_high_correlate <- plan_gestion_finca %>%
  select(all_of(pgf_filter$value)) %>%
  select(where(is.numeric)) %>%
  correlate() %>% 
  stretch() %>%
  filter(abs(r) > 0.8) #%>% view()

caracterizacion_high_correlate <- caracterizacion %>%
  select(all_of(caracterizacion_filter$value)) %>%
  select(where(is.numeric)) %>%
  correlate() %>% 
  stretch() %>%
  filter(abs(r) > 0.8) #%>% view()

productividad_high_correlate <- productividad %>%
  select(all_of(productividad_filter$value)) %>%
  select(where(is.numeric)) %>%
  correlate() %>% 
  stretch() %>%
  filter(abs(r) > 0.8) #%>% view()



## Variables discarted

variables_caracterizacion_discated <- caracterizacion %>% 
  select(!all_of(caracterizacion_filter$value)) %>% names

variables_pgf_discated <- plan_gestion_finca %>% 
  select(!all_of(pgf_filter$value)) %>% names

variables_productividad_discated <- productividad %>% 
  select(!all_of(productividad_filter$value)) %>% names


list(caracterizacion = variables_caracterizacion_discated, 
     pgf = variables_pgf_discated,
     productividad = variables_productividad_discated) %>% 
  enframe(name = "Libro", value = "Variable") %>% unnest(Variable)



###### Recoding  y seleccion de variables de analisis 

# Caracterizacion

caracterizacion_newnames <-  c(
  ID_Productor = "ID_Productor",
  status = "Participación en el proyecto",
  cedula_cacaotera = "1.13. Tiene cédula cacaotera?",
  nombre_tecnico = "Seleccione Nombre del Técnico",
  fecha_visita = "Fecha de Visita:",
  fecha_inclusion = "Fecha inclusión",
  cluster =  "Seleccione Cluster de intervención:" ,
  fecha_nacimiento = "1.8. Fecha de nacimiento:",
  sexo = "1.9. Sexo:",
  comunidad_etnica = "1.10. A que comunidad étnica pertenece:",
  estado_civil = "1.11. Estado civil:",
  lee_escribe = "3.1. Sabe leer y escribir?",
  departamento = "2.1. Departamento:",
  municipio = "2.2. Municipio:" ,
  vereda_poblado = "2.3. Vereda o Centro Poblado:" ,
  organizacion_afil = "2.5. Organización a la que esta afiliado:" ,
  habita_territorio_etnico = "2.6. Habita en Territorio Étnico:" ,
  tenencia_tierra = "4.1. Tenencia de la Tierra:",
  tenencia_tierra_origen = "4.3. Seleccione el origen de la tenencia de la tierra",
  tenencia_tierra_titulo = "4.5. Seleccione el Estado del Título de propiedad",
  cuenta_bancaria = "4.7. Tiene Cuenta Bancaria:",
  cuenta_bancaria_entidad = "4.8. En que entidad bancaria:",
  # Actualizacion Areas - "date(2024-08-20)
  area_finca_declarada = "Área Finca (Caract, PGF, GAPS)", #"5.1. Área de la Finca en Has:",                                                                                                                                                                                                                                        
  area_cultivo_declarada = "Área Cacao Sembrada  (Caract, PGF, GAPS)", # "5.2. Área del cultivo de Cacao en Has:",                                                                                                                                                                                                                               
  area_cacao_produccion = "Área Cacao en Producción (Caract, PGF, GAPS)", # "5.3. Área de Cacao en Producción Has:",
  area_cultivo_georref ="area_pol_i  (Caract, GAPS)", #  "area_pol_i",                                                                                                                                                                                                                                                           
  area_finca_georref = "area_pol_ii",
  georreferencia_cultivo = "Georreferenciar Polígono Cultivo de Cacao (Caract, GAPS)",# "Georreferenciar Polígono Cultivo de Cacao",
  georreferencia_finca = "Georreferenciar Polígono de la Finca",
  edad_cultivo = "5.4. Edad en años del cultivo de cacao Has:",                                                                                                                                                                                                                          
  no_arboles_sembrados ="Árboles Sembrados (Caract, PGF, GAPS)", #  "5.5. Número de árboles de cacao sembrados:",                                                                                                                                                                                                                           
  no_arboles_produccion = "Número de árboles de Cacao en Producción  (Caract, PGF, GAPS)", # "5.6. Numero de árboles de cacao en producción:",                                                                                                                                                                                                                       
  no_arboles_sembrados_2017_antes = "5.7. ¿Cuántos árboles de cacao fueron sembrados en el año 2017 o antes?",                                                                                                                                                                                              
  no_arboles_sembrados_2018_adelante = "5.8. ¿Cuántos árboles de cacao fueron sembrados en el año 2018 y en adelante?",
  uso_suelo_2013_2018 = "5.11. Sobre el lote donde se encuentran los árboles de cacao que fueron sembrados en el año 2018 y en adelante, cinco años (2013-2018) atrás de haber establecido ese cultivo de cacao, ¿qué tenía sembrado allí, qué había, o cuál era el  uso del suelo?",           
  sistema_agroforestal = "5.12. Cacao está bajo sistema agroforestal:",                                                                                                                                                                                                                          
  cultivo_complementario = "5.27. Seleccione el cultivo complementario más dominante:",                                                                                                                                                                                                            
  area_cultivo_complementario = "5.28. Área Cultivo Complementario HAS:",                                                                                                                                                                                                                               
  cultivo_autoconsumo = "5.29. ¿Cultivan alimentos en la finca para el consumo familiar?",                                                                                                                                                                                                      
  cria_animales_autoconsumo = "5.30. ¿Tiene cria de animales para el consumo familiar?",
  cacao_seco_producido = "5.14. Cantidad Kg de Cacao en seco producido en el año anterior:",
  cacao_baba_producido = "5.15. Cantidad Kg de Cacao en baba producido en el año anterior:",
  tipo_agroforesteria = "5.13. Tipo Agroforestería:",
  titulo_propiedad = "4.6. Seleccione el tipo de título de propiedad que poseen o que están tramitando:",
  cacao_seco_vendido = "5.20. Cantidad Kg de Cacao en seco vendió en el año anterior:",  
  no_personas_infantes = "3.6. Número de personas infantes (menos 15 años):",
  no_personas_hogar = "3.2. Total de personas en el hogar:",                                                                                                                                                                                                                                  
  no_mujeres = "3.3. Número de mujeres:",                                                                                                                                                                                                                                              
  no_personas_jovenes = "3.4. Número de personas jóvenes (15 años a 29 años):",                                                                                                                                                                                                                 
  no_mujeres_jovenes = "3.5. Número de mujeres jóvenes (15 años a 29 años):",
  cacao_baba_vendido = "5.18. Cantidad Kg de Cacao en baba vendió en el año anterior:",
  cambios_copa_2018_adelante = "5.17. Para los cultivos del 2018 y en adelante ¿Cuántos cambios de copa realizó?"
)

write_csv(enframe(caracterizacion_newnames), "data/2_archivos_procesados/diccionario_caracterizacion.csv")

print("  ---  Caracterizacion   -----")
print(caracterizacion_filter$value)


# Productividad 

productividad_newnames <- c(
  ID_Productor = "ID_Productor",                                                           
  fecha_visita = "fecha_visita",                                                           
  numero_visita = "numero_visita",                                                          
  comprador_cacao_baba = "2.5. ¿Quién le compró el cacao en baba?",                                
  cacao_baba_producido_mes = "2.3. Kg de Cacao en baba producidos en el mes",                          
  cacao_seco_producido_mes = "2.4. Kg de Cacao en seco producidos en el mes",                          
  precio_kg_cacao_baba = "2.4. Precio promedio KG de cacao baba",                                  
  comprador_cacao_seco = "2.8. ¿Quién le compró el cacao en seco?",                                
  precio_kg_cacao_seco = "2.7. Precio promedio KG de cacao seco",                                  
  valor_primas_adicionales =  "2.9. Valor de primas adicionales que le pagaron por Kg de cacao en seco",
  cacao_utilizado_transformacion =  "2.10. ¿Cuantos Kg de cacao en seco utilizó para transformar?",           
  mes_registro =  "2.1. Mes a registrar",                                                   
  year_registro =  "2.2. Año a registrar",                                                   
  cacao_baba_vendido_mes =  "2.3. Kg de cacao baba vendidos en el mes",                               
  cacao_seco_vendido_mes =  "2.6. KG de cacao seco vendidos en el mes",                               
#  cacao_manteca_consumido =  "2.13.4. ¿Cuántos Kg al mes consumió de Manteca de cacao?",               
  # =  "_parent_index"                                                          
  fecha_carga =  "Fecha de carga"
  
)


write_csv(enframe(productividad_newnames), "data/2_archivos_procesados/diccionario_productividad.csv")
print("  ---  Productividad  ---  ")
print(productividad_filter$value)


# Plan de gestion de finca

pgf_newnames <- c(
  ID_Productor = "ID_Productor",
  numero_visita = "Especifique el número de visita:",
  fecha_visita = "Fecha de Visita:",                    
  semana_cacaograma = "Ingrese Semana Cacaograma:",
  variedades_cacao_numero = "1.13. Determine cuantas variedades de Cacao observa en el cultivo?" ,
  variedad_cacao_dominante = "1.13.1. Seleccione variedad No 1 del cultivo de Cacao", 
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
  
  # practicas cosecha y beneficio
  fermenta_cajones_madera = "Fermentó en cajones de madera",                                                                                                                                                                 
  destina_4dias_secado = "Destinó más de 4 días para secar el cacao",                                                                                                                                                     
  selecciona_excelso_pasilla = "Seleccionó el cacao libre de impurezas y pasilla",                                                                                                                                              
  conoce_humedad_venta  = "Conoció si el cacao está apropiadamente seco para la venta",                                                                                                                                    
  fermenta_5dias = "Fermentó el cacao durante más de 5 días",                                                                                                                                                       
  fermenta_separado_fino ="Fermentó por separado el fino y el corriente",                                                                                                                                                  
  empaca_sacos_fique = "Empacó el cacao en sacos de fique",                                                                                                                                                             
  secado_pisos_madera = "Secó el cacao en pisos de madera",
  beneficia_separado_sanos = "Benefició frutos sanos aparte de los enfermos",
  cultivo_complementario = "1.8.1. Nombre cultivo complementario:",
  
#  area_cultivo_complementario = "1.8.5. Área en Has del Cultivo Complementario",   # disponible en caracterizacion
  produccion_agricola_consumo = "1.9.1. ¿Qué productos agrícolas de seguridad alimentaria para el consumo familiar, produce en su finca?",
  produccion_pecuaria_consumo = "1.9.3. ¿Qué productos pecuarios de seguridad alimentaria para el consumo familiar, produce en su finca?",
  otras_fuentes_ingresos = "1.10.1. Seleccione que otra(s) fuente de ingresos tiene",
  registra_produccion_ventas = "1.10.1. Completó registro de producción y ventas",
  actualiza_poster = "1.10.2. Actualizó póster (Calendario)",
  sistema_riego = "1.11.1. ¿Cuenta con sistema de riego en operación?",
  drenajes = "1.11.2. ¿Realizó adecuación y/o construcción de drenajes?",
  
  # Costos, jornales totales (>20 % data avail.)
  #[1] "¿Total de jornales invertidos en Control Hormiga – Semanal?"                    
  jornales_poda_deschuponado = "¿Total de jornales invertidos en Poda de Deschuponado?",                         
  jornales_manejo_coberturas = "¿Total de jornales invertidos en Manejar coberturas vegetales?",                 
  jornales_poda_fuerte = "¿Total de jornales invertidos en Poda fuerte - Mantenimiento?",                  
  jornales_mipe_escoba_bruja = "¿Total de jornales invertidos en Escoba de Bruja – Semanal?",                    
  jornales_mipe_phytophthora = "¿Total de jornales invertidos en Phytophthora – Semanal?",                       
  jornales_mipe_monilia = "¿Total de jornales invertidos en Monilia – Semanal?",                            
  jornales_myc_malezas = "¿Total de jornales invertidos en Controlar malezas vía herbicida o manual?",     
  jornales_cosecha_beneficio = "¿Total de jornales invertidos en Actividades de Cosecha y Beneficio realizadas?"
)



write_csv(enframe(pgf_newnames), "data/2_archivos_procesados/diccionario_pgf.csv")



print("   ---  Plan de gestion de finca  ---   ")
print(pgf_filter$value)




