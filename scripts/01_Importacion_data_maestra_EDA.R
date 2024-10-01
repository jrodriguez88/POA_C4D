## Importacion data NAestra y Analisis exploratorio. C4D Project
## github.com/jrodriguez88
## Julio 2024



## Import - read data
path_data <- "data/1_archivos_entrada/2. Datos Entrada/Data Maestras/"
shared_files <- list.files(path_data, full.names = T)

shared_files %>% map(excel_sheets) %>% 
  set_names(word(shared_files, 2, sep = "/")) %>% enframe()

data_list <- shared_files %>% map(excel_sheets) %>% 
  set_names(shared_files) %>% enframe() %>% unnest(value) %>%
  mutate(data = map2(.x = name, .y = value, 
                     ~read_excel(.x, sheet = .y, skip = 3, guess_max = 20000)))

## Divide Data maestra in R by table
data_list

caracterizacion <- data_list$data[[1]]

plan_gestion_finca <- data_list$data[[2]]

productividad <- data_list$data[[3]]

#cacaograma <- data_list_data[[4]]



# Data Overview, dimension, variable class, and missing values proportion
## Dimension 
data_list$data %>% set_names(data_list$value) %>%
  map(dim) %>% bind_rows() %>% 
  cbind(tibble(Dim = c("Registros", "Variables")), .)
  
## Variables classes
data_list$data %>% set_names(data_list$value) %>%
  map(~ .x %>% map(class) %>% enframe %>% unnest(value)) %>% 
  map(~table(.x$value)) %>% bind_rows(.id = "Data")

## Missing values exploratory
data_list$data %>% set_names(data_list$value) %>%
  map_dbl(pct_miss)

## Visualizar por porcentaje de datos faltantes por clase de datos
bind_rows(Logical = data_list$data %>% set_names(data_list$value) %>%
  map(~.x %>% select(where(is.logical))) %>%
  map_dbl(pct_miss),

Numeric = data_list$data %>% set_names(data_list$value) %>%
  map(~.x %>% select(where(is.numeric))) %>%
  map_dbl(pct_miss),

Character = data_list$data %>% set_names(data_list$value) %>%
  map(~.x %>% select(where(is.character))) %>%
  map_dbl(pct_miss), .id = "Clase")

# Longitud de los nombres de variables
names(caracterizacion) %>% map_dbl(nchar) %>% mean
names(plan_gestion_finca) %>% map_dbl(nchar) %>% mean
names(productividad) %>% map_dbl(nchar) %>% mean

# Guardar datos importados en R format 
save(caracterizacion, plan_gestion_finca, productividad, 
     file = "data/2_archivos_procesados/data_raw.RData")

