## Exploratory Data Analysis 1. C4D Project
## github.com/jrodriguez88
## Julio 2024



## Load libraries
library(tidyverse)
library(readxl)
library(skimr)
library(naniar)



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

# Import to R by table
caracterizacion <- data_list$data[[1]]

plan_gestion_finca <- data_list$data[[2]]

productividad <- data_list$data[[3]]

#cacaograma <- data_list_data[[4]]

# Check
glimpse(caracterizacion)
#skim(caracterizacion)


abbreviate(names(caracterizacion), minlength = 25)

names(plan_gestion_finca) %>% str_subset("jornal")

names(plan_gestion_finca) %>% str_subset("cost")


# Detect missing values 

## Caracterization Vis

miss_var_summary(caracterizacion)
miss_case_summary(caracterizacion)
miss_var_table(caracterizacion)
miss_case_table(caracterizacion)

gg_miss_var(caracterizacion)
gg_miss_case(caracterizacion)

gg_miss_fct(caracterizacion, fct = `Seleccione Cluster de intervención:`)



## PGF Vis

miss_var_summary(plan_gestion_finca)
miss_case_summary(plan_gestion_finca)
miss_var_table(plan_gestion_finca)
miss_case_table(plan_gestion_finca)

gg_miss_var(plan_gestion_finca)
gg_miss_case(plan_gestion_finca)

gg_miss_fct(plan_gestion_finca, fct = `Seleccione Cluster de intervención:`)


## Productividad Vis

miss_var_summary(productividad)
miss_case_summary(productividad)
miss_var_table(productividad)
miss_case_table(productividad)

gg_miss_var(productividad)
gg_miss_case(productividad)

productividad %>% #select(`_index`, `Seleccione Cluster de intervención:`)
  left_join(plan_gestion_finca, by = join_by(`_parent_index` == `_index`)) %>%
gg_miss_fct(., fct = `Seleccione Cluster de intervención:`)


# Variance analysis
caracterizacion %>%
  summarize(
    across(
      everything(),
      ~ var(., na.rm = TRUE))) %>%  
  pivot_longer(everything(), names_to =  "feature", values_to =  "variance")



### Initial EDA , data with less than 25 % NA



### FAMD - Factor Analysis of Mixed Data