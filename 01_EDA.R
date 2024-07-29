## Exploratory Data Analysis 1. C4D Project
## github.com/jrodriguez88
## Julio 2024



## Load libraries
library(tidyverse)
library(readxl)
library(skimr)
library(naniar)
library(corrr)



## read data
path_data <- "2024-02 Minería de Datos - Compartida Consultor/1_archivos_entrada/2. Datos Entrada/Data Maestras/"
shared_files <- list.files(path_data, full.names = T)

shared_files %>% map(excel_sheets) %>% 
  set_names(word(shared_files, 2, sep = "/")) %>% enframe()

data_list <- shared_files %>% map(excel_sheets) %>% 
  set_names(shared_files) %>% enframe() %>% unnest(value) %>%
  mutate(data = map2(.x = name, .y = value, ~read_excel(.x, sheet = .y, skip = 3)))

gc()
save(data_list, file = "ini_data.RData")


load("ini_data.RData")

# Import to R by table
caracterizacion <- data_list$data[[1]]

plan_gestion_finca <- data_list$data[[2]]

productividad <- data_list$data[[3]]

#cacaograma <- data_list_data[[4]]

# Check

## tables dim


list(caracterizacion, plan_gestion_finca, productividad) %>%
  map(dim)


skim(caracterizacion)
skim(plan_gestion_finca)
skim(productividad)

# Detect missing values 

## Caracterization Vis

names_abb_caract <- abbreviate(names(caracterizacion), minlength = 50)

#miss_var_summary(caracterizacion) #%>% view()
#miss_case_summary(caracterizacion)
miss_var_table(caracterizacion)
#miss_case_table(caracterizacion)

gg_miss_var(caracterizacion %>% set_names(names_abb_caract))
#gg_miss_case(caracterizacion)

gg_miss_fct(caracterizacion %>% set_names(names_abb_caract), 
            fct = `Seleccione Cluster de intervención:`)



## PGF Vis

names_abb_pgf <- abbreviate(names(plan_gestion_finca), minlength = 50)

#miss_var_summary(plan_gestion_finca)
#miss_case_summary(plan_gestion_finca)
miss_var_table(plan_gestion_finca)
#miss_case_table(plan_gestion_finca)

gg_miss_var(plan_gestion_finca %>% set_names(names_abb_pgf))
#gg_miss_case(plan_gestion_finca)

gg_miss_fct(plan_gestion_finca %>% set_names(names_abb_pgf), 
            fct = `Seleccione Cluster de intervención:`)


## Productividad Vis

names_abb_prod <- abbreviate(names(productividad), minlength = 50)

#miss_var_summary(productividad)
#miss_case_summary(productividad)
miss_var_table(productividad)
#miss_case_table(productividad)

gg_miss_var(productividad %>% set_names(names_abb_prod))
#gg_miss_case(productividad)

# conect PGF with productivity

key_index <- plan_gestion_finca %>% 
  select(ID_Productor, `Seleccione Cluster de intervención:`, `_parent_index` = `_index`)

productividad2 <- productividad %>% 
  left_join(key_index)

productividad2 %>% set_names(abbreviate(names(productividad2), minlength = 50)) %>% 
  drop_na(`Seleccione Cluster de intervención:`) %>%
  gg_miss_fct(., fct = `Seleccione Cluster de intervención:`)


productividad2$`Seleccione Cluster de intervención:` %>% 
  is.na %>% table





### Initial EDA , data with less than 25 % NA

# Caracterizacion
variables_caracterizacion <- miss_var_summary(caracterizacion) %>% filter(pct_miss <= 51) %>%
  pull(variable)

names_abb_caracterizacion <- abbreviate(variables_caracterizacion, minlength = 50)

# Variance analysis
caracterizacion %>% select(all_of(variables_caracterizacion)) %>%
  summarize(
    across(
      everything(),
      ~ var(., na.rm = TRUE))) %>%  
  pivot_longer(everything(), names_to =  "feature", values_to =  "variance") %>%
  arrange(variance) #%>% view()

## correlation analysis
caracterizacion %>% select(all_of(variables_caracterizacion)) %>% 
  setNames(names_abb_caracterizacion) %>% #names
  select(where(is.numeric), -c(`ID_Productor`, `_id`, `_index`, Reporte)) %>%  
  correlate() %>%  shave() %>%  
  rplot(print_cor =TRUE) +  
  theme(axis.text.x = element_text(angle =90, hjust =1))


# PGF
variables_PGF <- miss_var_summary(plan_gestion_finca) %>% filter(pct_miss <= 51) %>%
  pull(variable)

names_abb_plan <- abbreviate(variables_PGF, minlength = 50)

# Variance analysis
plan_gestion_finca %>% select(all_of(variables_PGF)) %>%
  summarize(
    across(
      everything(),
      ~ var(., na.rm = TRUE))) %>%  
  pivot_longer(everything(), names_to =  "feature", values_to =  "variance") %>%
  arrange(variance) #%>% view()

## correlation analysis
plan_gestion_finca %>% select(all_of(variables_PGF)) %>% 
  setNames(names_abb_plan) %>%
  select(where(is.numeric), -c(`ID_Productor`, `_id`, `_index`, Reporte_PGF)) %>% 
  correlate() %>%  shave() %>%  
  rplot(print_cor =TRUE) +  
  theme(axis.text.x = element_text(angle =90, hjust =1))

# Productividad
variables_productividad <- miss_var_summary(productividad) %>% 
  filter(pct_miss <= 51) %>%
  pull(variable)

names_abb_productividad <- abbreviate(variables_productividad, minlength = 50)

# Variance analysis
productividad %>% select(all_of(variables_productividad)) %>%
  summarize(
    across(
      everything(),
      ~ var(., na.rm = TRUE))) %>%  
  pivot_longer(everything(), names_to =  "feature", values_to =  "variance") %>%
  arrange(variance) #%>% view()

## correlation analysis
productividad %>% select(all_of(variables_productividad)) %>% 
  setNames(names_abb_productividad) %>%
  select(where(is.numeric), -c(`_index`, `_parent_index`, `_submission__id`, Reporte_productividad)) %>% 
  correlate() %>%  shave() %>%  
  rplot(print_cor =TRUE) +  
  theme(axis.text.x = element_text(angle =90, hjust =1))
