## Grouping Variables and Join Data. C4D Project
## github.com/jrodriguez88
## Julio 2024


library(tidyverse)



### join by productor 

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", 
           "Junio", "Julio", "Agosto", "Septiembre", 
           "Octubre", "Noviembre", "Diciembre")

# Crear el DataFrame
df_meses <- data.frame(mes_nombre = meses, mes = 1:12)

# conect PGF with productivity

key_index <- plan_gestion_finca %>% select(ID_Productor, `Seleccione Cluster de intervención:`, `_parent_index` = `_index`)

productividad_nested <- productividad %>% 
  left_join(key_index) %>% nest(productividad = - ID_Productor)


productividad_nested$productividad[[2000]] %>% select(1,2, contains("vendido")) %>% 
  left_join(df_meses, join_by(`2.1. Mes a registrar` == mes_nombre)) %>%
  mutate(date = make_date(year = `2.2. Año a registrar`, month = mes)) %>%
  select(date, "2.6. KG de cacao seco vendidos en el mes") %>% 
  ggplot(aes(date, `2.6. KG de cacao seco vendidos en el mes`)) + 
  geom_col()


productividad_nested$productividad[[10]] $`2.1. Mes a registrar` %>% unique()



## seleccion de variables por workbook - Analisis cluster 


# Caracterizacion
variables_caracterizacion <- miss_var_summary(caracterizacion) %>% filter(pct_miss <= 51) %>%
  pull(variable)

names_caracterizacion2 <- paste0("V", 1:length(variables_caracterizacion))


variables_caracterizacion %>% 
  tolower %>% 
  trimws %>% 
  stringdist::stringdistmatrix(method = "jw", p = 0.1) %>% 
  as.dist %>% 
  `attr<-`("Labels", names_caracterizacion2) %>% 
  hclust %T>% 
  plot %T>% 
  rect.hclust(h = 0.45) %>% 
  cutree(h = 0.45) %>% 
  print -> test_group_caracterizacion

variables_caracterizacion[c(38,42,41,44,40,36,43)]




# Plan Gestion Finca
variables_pgf <- miss_var_summary(plan_gestion_finca) %>% filter(pct_miss <= 51) %>%
  pull(variable)

names_pgf2 <- paste0("V", 1:length(variables_pgf))


variables_pgf %>% 
  tolower %>% 
  trimws %>% 
  stringdist::stringdistmatrix(method = "jw", p = 0.1) %>% 
  as.dist %>% 
  `attr<-`("Labels", names_pgf2) %>% 
  hclust %T>% 
  plot %T>% 
  rect.hclust(h = 0.45) %>% 
  cutree(h = 0.45) %>% 
  print -> test_group_pgf



# Productividad
variables_productividad <- miss_var_summary(productividad) %>% filter(pct_miss <= 51) %>%
  pull(variable)

names_prod2 <- paste0("V", 1:length(variables_productividad))


variables_productividad %>% 
  tolower %>% 
  trimws %>% 
  stringdist::stringdistmatrix(method = "jw", p = 0.1) %>% 
  as.dist %>% 
  `attr<-`("Labels", names_prod2) %>% 
  hclust %T>% 
  plot %T>% 
  rect.hclust(h = 0.3) %>% 
  cutree(h = 0.3) %>% 
  print -> test_group_productividad








  