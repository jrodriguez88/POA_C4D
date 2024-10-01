

load("caracterizacion_to_join.RData")



caracterizacion_categoricas <- caracterizacion_to_join  %>% 
  select(where(is.factor)) #%>% 
#  filter(status == "Activo") 


caracterizacion_numericas <- 


caracterizacion_categoricas %>%
#  filter(status == "Retirado") %>%
  count(departamento, status) %>% arrange(desc(n)) %>% 
  pivot_wider(names_from = status, values_from = n)


(caracterizacion_no_outliers %>% 
  group_by(fecha_inclusion) %>% 
  summarise(No_Productores = n())  %>% ungroup() %>%
  mutate(No_Productores_total = cumsum(No_Productores)) %>% 
  ggplot(aes(fecha_inclusion, No_Productores_total)) +
  geom_segment( aes(xend = fecha_inclusion, yend=0)) +
  geom_point( size=4, color="orange") +
  geom_vline(xintercept = as_date("2023-09-01"), col = "red", size = 1.2) + 
  annotate(x=as_date("2023-09-01"),
           y=+Inf,label="<--2022_2023 - 2023_2024-->",vjust=2,geom="label")  +
  theme_minimal(14) +  
  labs(title = "Inclusion de productores")) + 


(caracterizacion_categoricas %>% 
  filter(status == "Activo") %>% 
  count(departamento) %>% 
  ggplot( aes(x=fct_reorder(departamento, n), y=n)) +
  geom_segment( aes(xend=departamento, yend=0)) +
  geom_point( size=4, color="orange") +
  theme_minimal(14) +
  xlab("") + coord_flip() + labs(title = "Activos")) +

(caracterizacion_categoricas %>% 
  filter(status != "Activo") %>% 
  count(departamento) %>% 
  ggplot( aes(x=fct_reorder(departamento, n), y=n)) +
  geom_segment( aes(xend=departamento, yend=0)) +
  geom_point( size=4, color="orange") +
  theme_minimal(14) +
  xlab("") + coord_flip() + labs(title = "Inactivos"))



(caracterizacion_categoricas %>%  filter(status == "Activo") %>%
  mutate(across(everything(), str_to_title)) |> 
  mutate(departamento = fct_infreq(departamento, ordered = T)) |>
  count(departamento, sexo)  |>  
  ggplot(aes(x = departamento,
             y = n, fill = sexo)) +
  geom_bar(position='stack', stat='identity', alpha = 0.7) +
  scale_fill_viridis_d() +
  #  geom_text(aes(label = n), color = "black", size = 4) + 
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(title = "Productores por Departamento y Sexo", 
       y = "Numero", x = "Departamento") + guides(fill = "none") +
  theme_minimal(14) 
) +
(caracterizacion_categoricas %>%  filter(status == "Activo") %>%
  mutate(across(everything(), str_to_title)) |> 
  mutate(departamento = fct_infreq(departamento, ordered = T)) |>
  count(departamento, sexo)  |>  
  ggplot(aes(x = departamento,
             y = n, fill = sexo)) +
  geom_col(position='fill', alpha = 0.7) +
  scale_fill_viridis_d() +
  geom_text(aes(label = n), color = "black", size = 5, position = position_fill(vjust = .5)) + 
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(title = NULL, 
       y = "Proporcion", x = NULL, 
       fill = "Sexo") + guides(fill = NULL) +
  theme_minimal(14) + theme(axis.text.y = element_blank())
)






caracterizacion_categoricas %>%  filter(status == "Activo") %>%
  mutate(across(everything(), str_to_title)) |> 
  mutate(departamento = fct_infreq(departamento, ordered = T)) |>
  count(departamento, tenencia_tierra)  |>  
  ggplot(aes(x = tenencia_tierra,
             y = departamento)) +
  geom_tile(aes(fill = n), color = "black") +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  geom_text(aes(label = n), color = "black", size = 4) +
  scale_y_discrete(limits = rev) +
  labs(title = "Tenencia de la Tierra", 
       y = "Departamento", x = "Tipo", 
       fill = "No Productores") +
  theme_minimal(14)
  


caracterizacion_categoricas %>%  filter(status == "Activo") %>%
  mutate(across(everything(), str_to_title)) |> 
  mutate(departamento = fct_infreq(departamento, ordered = T)) |>
  count(departamento, tenencia_tierra_origen)  |>  
  ggplot(aes(x = tenencia_tierra_origen,
             y = departamento)) +
  geom_tile(aes(fill = n), color = "black") +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  geom_text(aes(label = n), color = "black", size = 4) +
  scale_y_discrete(limits = rev) +
  labs(title = "Origen de Tenencia de la Tierra", 
       y = "Departamento", x = "Tipo", 
       fill = "No Productores") +
  theme_minimal(14)


## Numericas



histograma_c4d_data(caracterizacion_to_join)

histograma_c4d_data(caracterizacion_no_outliers)

histograma_c4d_data(productividad_to_join)

histograma_c4d_data(productividad_no_outliers)

histograma_c4d_data(pgf_to_join)

histograma_c4d_data(pgf_no_outliers)


### CAtegoricas

barplot_c4d(caracterizacion_to_join)

barplot_c4d(pgf_to_join)

barplot_c4d(select(productividad_to_join, -mes_reg), 15)




caracterizacion_no_outliers %>% select(ID_Productor,
                                       area_cacao_produccion,
                                       edad_cultivo,
                                       edad_productor,
                                       no_arboles_produccion,
                                       no_arboles_sembrados_2017_antes,
                                       no_arboles_sembrados_2018_adelante,
                                       area_cultivo_complementario,
                                       no_personas_infantes,
                                       no_personas_jovenes,
                                       no_mujeres,
                                       antiguedad_proyecto) %>%  
   left_join(rendimiento_base %>% select(ID_Productor,
                                         densidad_siembra_produccion)) %>%
  left_join(comparativa_productividad %>% select(ID_Productor, 
                                                 rendimiento_linea_base,
                                                 rendimiento_2022_2023,
                                                 rendimiento_2023_2024, 
                                                 no_visitas)) %>% drop_na() %>%
  select(-ID_Productor) %>%
  scale() %>%
  cor %>%
  corrplot::corrplot(type = "lower", method = "ellipse")
  
  correlate() %>%  shave() %>%  
  rplot(print_cor =TRUE) +  
  theme(axis.text.x = element_text(angle =90, hjust =1))



  
  caracterizacion_to_join %>% select(ID_Productor,
                                         area_cacao_produccion,
                                         edad_cultivo,
                                         edad_productor,
                                         no_arboles_produccion,
                                         no_arboles_sembrados_2017_antes,
                                         no_arboles_sembrados_2018_adelante,
                                         area_cultivo_complementario,
                                         no_personas_infantes,
                                         no_personas_jovenes,
                                         no_mujeres,
                                         antiguedad_proyecto, fecha_inclusion) %>%  
    left_join(rendimiento_base %>% select(ID_Productor,
                                          densidad_siembra_produccion)) %>%
    left_join(comparativa_productividad %>% select(ID_Productor, 
                                                   rendimiento_linea_base,
                                                   rendimiento_2022_2023,
                                                   rendimiento_2023_2024, 
                                                   no_visitas)) %>% #drop_na() %>%
    # filter(fecha_inclusion < as.Date("2023-09-01")) %>%
    filter(no_visitas > 2) %>%
    ggplot(aes(no_visitas, rendimiento_2023_2024)) +
    geom_jitter() + 
    geom_smooth(method = "lm") + theme_minimal()
    # geom_smooth(aes(y = rendimiento_2022_2023),  color = "red", method = "lm")
  
  

  caracterizacion_to_join %>% select(ID_Productor,
                                     area_cacao_produccion,
                                     edad_cultivo,
                                     edad_productor,
                                     no_arboles_produccion,
                                     no_arboles_sembrados_2017_antes,
                                     no_arboles_sembrados_2018_adelante,
                                     area_cultivo_complementario,
                                     no_personas_infantes,
                                     no_personas_jovenes,
                                     no_mujeres,
                                     antiguedad_proyecto, fecha_inclusion) %>%  
    left_join(rendimiento_base %>% select(ID_Productor,
                                          densidad_siembra_produccion)) %>%
    left_join(comparativa_productividad %>% select(ID_Productor, 
                                                   rendimiento_linea_base,
                                                   rendimiento_2022_2023,
                                                   rendimiento_2023_2024, 
                                                   no_visitas)) %>%
    filter(fecha_inclusion < as.Date("2023-09-01")) %>%
    filter(no_visitas < 3) %>% view()
  
  