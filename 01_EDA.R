## Exploratory Data Analysis 1. C4D Project
## github.com/jrodriguez88
## Julio 2024



## Load libraries
library(tidyverse)
library(readxl)
library(skimr)
library(naniar)



## read data
shared_files <- list.files("shared_data/", full.names = T)

shared_files %>% map(excel_sheets) %>% 
  set_names(word(shared_files, 2, sep = "/")) %>% enframe()

data_list <- shared_files %>% map(excel_sheets) %>% 
  set_names(shared_files) %>% enframe() %>% unnest(value) %>%
  mutate(data = map2(.x = name, .y = value, ~read_excel(.x, sheet = .y, skip = 2)))



# Import to R by table
caracterizacion <- data_list$data[[1]]

plan_gestion_finca <- data_list$data[[2]]

productividad <- data_list$data[[3]]

cacaograma <- data_list_data[[4]]





glimpse(caracterizacion)
skim(caracterizacion)




### Calculate NA pct by var 



### Initial EDA , data with less tahn 25 % NA



### FAMD - Factor Analysis of Mixed Data



### 







