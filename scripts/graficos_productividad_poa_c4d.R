# Graficos Estimaciones de rendimiento



simple_mean_rendimiento <- mean(rendimiento_base$rendimiento_area_produccion, na.rm =T)
simple_mean_rendimiento_arbol <- mean(rendimiento_base$rendimiento_arboles_produccion, na.rm =T)
simple_mean_densidad <- mean(rendimiento_base$densidad_siembra_produccion, na.rm =T)

# Draw a histogram of the resample means
(ggplot(rendimiento_base, aes(rendimiento_area_produccion)) +
    geom_histogram(binwidth = 80, col = "lightgray") + 
    geom_vline(xintercept = simple_mean_rendimiento, col = "red", size = 1.2) + 
    annotate(x = simple_mean_rendimiento,
             y = +Inf, 
             label = paste0("Promedio : ", round(simple_mean_rendimiento)),
             vjust = 2,geom="label") + 
    labs(title = "Rendimiento Nacional (kg/ha)") + 
    #,  subtitle = "Metodo: Media aritmetica") +
    theme_minimal(14) ) +
  
  
  # Draw a histogram of the resample means
  (ggplot(rendimiento_base, aes(rendimiento_arboles_produccion)) +
     geom_histogram(binwidth = 0.1, col = "lightgray") + 
     geom_vline(xintercept = simple_mean_rendimiento_arbol, col = "red", size = 1.2) + 
     annotate(x = simple_mean_rendimiento_arbol,
              y = +Inf, 
              label = paste0("Promedio : ", round(simple_mean_rendimiento_arbol, 2)),
              vjust=2,geom="label") + 
     labs(title = "Rendimiento Nacional - Kg/Arbol") + 
     #,  subtitle = "Metodo: Media aritmetica") +
     theme_minimal(14)) +
  
  
  
  (
    ggplot(rendimiento_base, aes(densidad_siembra_produccion)) +
      geom_histogram(binwidth = 100, col = "lightgray") + 
      geom_vline(xintercept = simple_mean_densidad, col = "red", size = 1.2) + 
      annotate(x = simple_mean_densidad,
               y = +Inf, 
               label = paste0("Promedio : ", round(simple_mean_densidad, 2)),
               vjust=2,geom="label") + 
      labs(title = "Densidad de Siembra Nacional - Arboles/ha") + 
      #,  subtitle = "Metodo: Media aritmetica") +
      theme_minimal(14))



ggplot(rendimiento_base, aes(rendimiento_area_produccion)) +
  geom_histogram(col = "lightgray") + 
  geom_vline(xintercept = simple_mean_rendimiento, col = "red", size = 1.2) + 
  labs(title = "Rendimiento Nacional (kg/ha) - Linea Base de Caracterizacion",
       x = NULL, y = NULL) + 
  #,  subtitle = "Metodo: Media aritmetica") +
  theme_minimal(14) + 
  facet_wrap(~departamento, scales = "free") 

ggplot(rendimiento_base, aes(rendimiento_arboles_produccion)) +
  geom_histogram(binwidth = 0.1, col = "lightgray") + 
  geom_vline(xintercept = simple_mean_rendimiento_arbol, col = "red", size = 1.2) + 
  labs(title = "Rendimiento Nacional - Kg/Arbol",
       x = NULL, y = NULL) + 
  theme_minimal(14) + 
  facet_wrap(~departamento, scales = "free") 

ggplot(rendimiento_base, aes(densidad_siembra_produccion)) +
  geom_histogram(binwidth = 100, col = "lightgray") + 
  geom_vline(xintercept = simple_mean_densidad, col = "red", size = 1.2) + 
  labs(title = "Densidad de Siembra Nacional - Arboles/ha",
       x = NULL, y = NULL) + 
  theme_minimal(14) + 
  facet_wrap(~departamento, scales = "free")






###########

data_to_plot <- rendimiento_base %>%
  mutate(departamento = str_to_title(departamento)) %>% 
  drop_na() %>%
  mutate(departamento = fct_reorder(departamento, rendimiento_area_produccion, .fun='mean')) %>%
  select(ID_Productor, departamento,
         rendimiento_area_produccion, densidad_siembra_produccion, rendimiento_arboles_produccion) %>% 
  pivot_longer(-c(departamento, ID_Productor)) %>% 
  mutate(name = factor(name, 
                       levels = c("rendimiento_area_produccion", 
                                  "rendimiento_arboles_produccion", 
                                  "densidad_siembra_produccion")))




data_to_plot %>% #drop_na %>%
  select(ID_Productor, departamento, name, value) %>% filter(name == "rendimiento_area_produccion") %>%
  ggplot(aes(x = departamento,
             y = value)) +
  stat_summary(fun.data=mean_cl_boot, position=position_dodge(0.95), geom="errorbar")+
  stat_summary(fun.data=mean_cl_boot, position=position_dodge(0.95), 
               size=3, shape=21, fill="white", col = "red", geom = "point") + 
  # geom_vline(xintercept = mean(rendimiento_base$rendimiento_area_produccion, na.rm = T), col = "red", linewidth = 1.2) + 
  # annotate(y=mean(rendimiento_base$rendimiento_area_produccion, na.rm = T),
  #        x=+Inf,label="Target Date",vjust=2,geom="label")
  geom_text(data = summary_rendimiento_base,
            aes(x = departamento, y = -Inf, label = paste0(n_productores, " p")),
            vjust = 0, hjust = 0, color = "blue", size = 3) + 
  coord_flip() + theme_minimal(14) +
  labs(title = "Rendimiento del Cacao -  Linea Base",
       x = "Departamento", y = "Kg/ha",
       caption = "Número total de productores indicado junto al nombre del Departamento")



labels_plot <- c(
  rendimiento_area_produccion = "Rendimiento por Area de Produccion (kg/ha)",
  rendimiento_arboles_produccion = "Rendimiento por arbol en produccion (kg/arbol)",
  densidad_siembra_produccion = "Densidad de Siembra (No. arboles/ha)"
)


rendimiento_plot1 <- data_to_plot %>% #filter(name == "densidad_siembra_produccion") %>%
  # filter(value>0, value <1500) %>% 
  #  mutate(value = ifelse(value <= 3 | value > 2000, NA, value)) %>%
  
  ggplot(aes(departamento, value)) + 
  #  ggplot(aes(reorder(departamento, value, FUN = median), value)) + 
  #  geom_boxplot(alpha = 0.8, fill = "lightgray") + 
  stat_summary(fun.data = mean_cl_boot, col = "red") +
  coord_flip() +
  #geom_histogram(aes(fill = departamento, col = departamento), alpha = 0.3) +
  # geom_hline(aes(yintercept = mean(value)), col = "red") + 
  # geom_vline(aes(xintercept = median(value))) + 
  facet_grid(~name, scales = "free", labeller = as_labeller(labels_plot)) +
  labs(title = "Indicadores de Productividad del cultivo - C4D", x = "Departamento", 
       y = NULL) +
  theme_bw() +
  theme(strip.background =element_rect(fill="white"))


# rendimiento_plot2 <- data_to_plot %>%
#   # filter(value>0, value <1500) %>% 
# #  mutate(value = ifelse(value <= 3 | value > 2000, NA, value)) %>%
#   ggplot(aes(value)) + 
#   geom_density(aes(fill = departamento), alpha = 0.3) + 
#   #geom_histogram(aes(fill = departamento, col = departamento), alpha = 0.3) +
#   facet_wrap(~name, scales = "free", labeller = as_labeller(labels_plot)) +
#   labs(title = "Indicadores de Productividad del cultivo - C4D", x = "Departamento", 
#        y = NULL) +
#   theme_minimal()


rendimiento_plot1
# rendimiento_plot2



######
#picos de cosecha - analisis mensual 



ggplot() +
  geom_col( data = productividad_mensual_summary, aes(mes_reg, registros, fill  = factor(year_registro)), position = "dodge") +
  geom_rect(data = picos_cosecha,
            aes(xmin = as.numeric(mes_reg) - 0.4, xmax = as.numeric(mes_reg) + 0.4,
                ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, inherit.aes = FALSE) +
  facet_wrap(~departamento, scales = "free") + theme_bw() + 
  theme(legend.position = c(0.75, 0.1),
        strip.background =element_rect(fill="white"),
        legend.box.background = element_rect(colour = "black"),
        strip.text = element_text (face = "bold")) +
  labs(x = NULL, y = " Numero de Registros de venta", 
       fill = "Año de Registro") +
  scale_fill_viridis_d()


ggplot() +
  geom_col( data = productividad_mensual_summary, aes(mes_reg, total_produccion, fill  = factor(year_registro)), position = "dodge") +
  geom_rect(data = picos_cosecha,
            aes(xmin = as.numeric(mes_reg) - 0.4, xmax = as.numeric(mes_reg) + 0.4,
                ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, inherit.aes = FALSE) +
  # facet_wrap(~departamento, scales = "free") + theme_bw() + 
  facet_wrap(~departamento, scales = "free") + theme_bw() + 
  theme(legend.position = c(0.75, 0.1),
        strip.background =element_rect(fill="white"),
        legend.box.background = element_rect(colour = "black"),
        strip.text = element_text (face = "bold")) +
  labs(x = NULL, y = "Volumen de ventas (kg)", 
       fill = "Año de Registro") +
  scale_fill_viridis_d()


#Promedio picos de cosecha
productividad_mensual_agg %>%
  left_join(
    caracterizacion_to_join %>% 
      select(ID_Productor, departamento, cluster)) %>% 
  drop_na(produccion_cacao, departamento) %>% 
  mutate(departamento = str_to_title(departamento)) %>%
  ggplot(aes(mes_reg, produccion_cacao, fill=cluster)) +
  stat_summary(fun.data = mean_cl_boot, shape=21) +
  facet_wrap(~departamento, scales = "free") + 
  theme_bw() +
  geom_rect(data = picos_cosecha,
            aes(xmin = as.numeric(mes_reg) - 0.4,
                xmax = as.numeric(mes_reg) + 0.4,
                ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, inherit.aes = FALSE) +
  # facet_wrap(~departamento, scales = "free_y") + theme_bw() + 
  facet_wrap(~departamento, scales = "free_y") + theme_bw() + 
  theme(legend.position = c(0.75, 0.1),
        strip.background =element_rect(fill="white"),
        legend.box.background = element_rect(colour = "black"),
        strip.text = element_text (face = "bold")) +
  labs(title = "Comparativa Produccion mensual promedio - Kg de Cacao",
       x = NULL, y = NULL, fill = "Cluster:") 





#####
##### productividad anual

# Draw a histogram of the resample means
(ggplot(comparativa_productividad, aes(rendimiento_2022_2023)) +
    geom_histogram(binwidth = 50, col = "lightgray") + 
    geom_vline(xintercept = mean_2022_2023, col = "red", size = 1.2) + 
    annotate(x = mean_2022_2023,
             y = +Inf, 
             label = paste0("Promedio : ", round(mean_2022_2023)),
             vjust = 2,geom="label") + 
    labs(title = "Rendimiento Nacional 2022-2023 (kg/ha)") +
    theme_minimal(14)) + (ggplot(comparativa_productividad, aes(rendimiento_2023_2024)) +
                            geom_histogram(binwidth = 50, col = "lightgray") + 
                            geom_vline(xintercept = mean_2023_2024, col = "red", size = 1.2) + 
                            annotate(x = mean_2023_2024,
                                     y = +Inf, 
                                     label = paste0("Promedio : ", round(mean_2023_2024)),
                                     vjust = 2,geom="label") + 
                            labs(title = "Rendimiento Nacional 2023-2024 (kg/ha)") +
                            theme_minimal(14)
    ) 




#####
#### Comparativa de productividad


comparativa_productividad   %>%
  mutate(departamento = fct_reorder(departamento, rendimiento_linea_base, .fun='mean')) %>% 
 # filter(!(departamento %in% c("Cundinamarca", "Risaralda", "Cauca", "Magdalena", "La Guajira", "Meta"))) %>%
  
  select(ID_Productor, departamento, contains("rendimiento"), -contains("arbol")) %>%
  pivot_longer(-c(ID_Productor, departamento)) %>%
  mutate(name = factor(name, levels = c("rendimiento_linea_base", "rendimiento_2022_2023", "rendimiento_2023_2024"))) %>%
  ggplot(aes(x = reorder(departamento, value),
             y = value, color = name, fill = name)) +
  stat_summary(fun.data=mean_cl_boot, position=position_dodge(0.5), geom="errorbar")+
  stat_summary(fun.data=mean_cl_boot, position=position_dodge(0.5), 
               size=3, shape=21, geom = "point") + 
  # geom_vline(xintercept = mean(rendimiento_base$rendimiento_area_produccion, na.rm = T), col = "red", linewidth = 1.2) + 
  # annotate(y=mean(rendimiento_base$rendimiento_area_produccion, na.rm = T),
  #        x=+Inf,label="Target Date",vjust=2,geom="label")
  coord_flip() + theme_minimal(14) +
  theme(legend.position = "bottom") +
  labs(title = "Comparativa Rendimiento del Cacao",
       x = "Departamento", y = "Kg/ha", fill = "Periodo:", color = "Periodo:") 




comparativa_productividad   %>%
select(ID_Productor, departamento, contains("rendimiento"), -contains("arbol")) %>% 
  mutate(departamento = fct_reorder(departamento, rendimiento_2023_2024, .fun='mean')) %>% 
  select(-rendimiento_linea_base ) %>%
  pivot_longer(-c(ID_Productor, departamento)) %>%
  mutate(name = factor(name, levels = c("rendimiento_2022_2023", "rendimiento_2023_2024"))) %>%
  ggplot(aes(x = reorder(departamento, value),
             y = value, color = name, fill = name)) +
  stat_summary(fun.data=mean_cl_boot, position=position_dodge(0.5), geom="errorbar")+
  stat_summary(fun.data=mean_cl_boot, position=position_dodge(0.5), 
               size=3, shape=21, geom = "point") + 
  # geom_vline(xintercept = mean(rendimiento_base$rendimiento_area_produccion, na.rm = T), col = "red", linewidth = 1.2) + 
  # annotate(y=mean(rendimiento_base$rendimiento_area_produccion, na.rm = T),
  #        x=+Inf,label="Target Date",vjust=2,geom="label")
  coord_flip() + theme_minimal(14) +
  theme(legend.position = "bottom") +
  labs(title = "Comparativa Rendimiento del Cacao",
       x = "Departamento", y = "Kg/ha", fill = "Periodo:", color = "Periodo:") +
  scale_fill_viridis_d()



comparativa_productividad %>%
  mutate(departamento = str_to_title(departamento)) %>% 
  mutate(departamento = fct_reorder(departamento, rendimiento_linea_base, .fun='median')) %>%
  select(ID_Productor, departamento, contains("produccion")) %>% 
  pivot_longer(-c(ID_Productor, departamento))  %>%
  mutate(name = factor(name, levels = c("produccion_linea_base", "produccion_total_2022_2023", "produccion_total_2023_2024"))) %>% 
  group_by(departamento, name) %>%
  summarise(produccion_total = sum(value, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(departamento, produccion_total), produccion_total)) +
  geom_col(alpha = 0.8) + coord_flip() + theme_minimal() +
  facet_grid(~name) + 
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Comparativa Produccion del Cacao",
       # subtitle = "No. productores: 4030",
       x = "Departamento", y = "Kg")


comparativa_productividad %>% 
  mutate(departamento = str_to_title(departamento)) %>% 
  filter(!(departamento %in% filtro_dpts))  %>%
  
  ggplot(aes(no_visitas, rendimiento_2023_2024)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(color = "red", method = "lm") + 
  #  geom_smooth(aes(y = rendimiento_2022_2023),color = "green", method = "lm") + 
  facet_wrap(~departamento) +
  theme_minimal()



##### 
# varios 


####
caracterizacion_to_join %>%
  ggplot(aes(cacao_seco_producido, cacao_seco_vendido)) +
  geom_point() + geom_smooth(method = "lm") + 
  facet_wrap(~departamento, scales = "free") + 
  theme_minimal()

lm_areas <- lm(area_cultivo_declarada ~ area_cultivo_georref, 
               data = caracterizacion_no_outliers)

line_areas = data.frame(area_pred = predict(lm_areas, caracterizacion_no_outliers), 
                        area_cultivo_georref=caracterizacion_no_outliers$area_cultivo_georref)

plot_area_cultivo <- caracterizacion_no_outliers %>% filter(area_cultivo_georref < 30) %>%
  ggplot(aes(area_cultivo_declarada, area_cultivo_georref, 
             label1 = ID_Productor, 
             label2 = departamento, label3 = municipio, 
             label4 = nombre_tecnico)) +
  geom_jitter(alpha = 0.7) + #geom_smooth(method = "lm") + 
  geom_line(color='red',data = line_areas, aes(x=area_pred, y=area_cultivo_georref), inherit.aes = F) +
  facet_wrap(~departamento, scales = "free") + 
  theme_minimal() + ylim(0,6) + xlim(0,6)
#plotly::ggplotly(plot_area_cultivo)


caracterizacion_to_join %>% filter(area_finca_declarada < 250) %>%
  ggplot(aes(area_finca_declarada, area_finca_georref)) +
  geom_point() + geom_smooth(method = "lm") + 
  facet_wrap(~departamento, scales = "free") +
  theme_minimal()








## histograma_visitas_c4d <- promedio_visitas %>% 
left_join(caracterizacion_to_join  %>% 
            select(ID_Productor, departamento, status)) %>% 
  filter(status == "Activo") %>%
  ggplot(aes(promedio_dias_entre_visitas)) +
  geom_histogram(color="lightgray") +
  facet_wrap(~departamento, scales = "free") + 
  theme_bw() + labs(title = "Frecuencia de visita - Promedio en dias", x = NULL, y = NULL)



plot_visitas <- promedio_visitas %>% 
  left_join(caracterizacion_to_join  %>% 
              select(ID_Productor, departamento, status)) %>% 
  filter(status == "Activo") %>%
  
  ggplot(aes(departamento, promedio_dias_entre_visitas)) +#, label1 = ID_Productor)) +
  # geom_violin(trim=FALSE) + 
  geom_jitter(aes(label1 = ID_Productor), alpha = 0.5, position = position_jitter(width =0.3)) +
  # stat_summary(fun.data=mean_sdl, mult=1, 
  #               geom="crossbar", width=0.7 , fill = "lightgrey", alpha =0.7) +
  
  geom_boxplot(fill = "lightgray", alpha =0.3, color = "darkred", 
               linewidth = 1, outliers = FALSE, width=0.8) +
  coord_flip() +
  # facet_wrap(~departamento, scales = "free") + 
  theme_bw()


plot_visitas
#plotly::ggplotly(plot_visitas)





caracterizacion_numericas <- caracterizacion_no_outliers %>% 
  select(ID_Productor, 
         where(is.numeric)) %>% 
  select(-contains("producido"))

left_join(caracterizacion_to_join %>% select(ID_Productor, departamento), 
          comparativa_productividad %>% 
            select(ID_Productor, mes_mas_productivo_2022_2023, mes_mas_productivo_2023_2024)) %>%
  drop_na() %>%
  group_by(departamento) %>% 
  ggplot(aes(mes_mas_productivo_2022_2023)) +
  geom_bar() + facet_wrap(~departamento, scales = "free")
