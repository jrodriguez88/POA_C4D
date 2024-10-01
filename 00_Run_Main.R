## Importacion data Maestra y Analisis exploratorio. C4D Project
## github.com/jrodriguez88
## Julio 2024


## Load libraries

# pauqetes para gestion y visualizacion de datos.
library(tidyverse)
library(readxl)
library(skimr)
library(naniar)
library(corrr)
library(patchwork)

#Paquete para hacer analisis de similitud de variables de texto
library(stringdist)

# Paquetes para Analisis factorial multiple
library(FactoMineR)
library(factoextra)
library(missMDA)
library(visdat)

library(ggfortify)


#paquetes para ML
library(tidymodels)
library(cluster)
library(vip)



source("scripts/funciones_poa_c4d.R")


# Importacion data maestra
source("scripts/01_Importacion_data_maestra_EDA.R")

#

# Reduccion Dimensionalidad
#na_pct_filter <- 80
source("02_Reduccion_dimensionalidad.R")


# Ingenieria de caracteristicas
source("03_Ingenieria_Caracteristicas.R")
gc()

# Manejo de Outliers - Requiere big memory
source("03_Outliers_handling.R")

# Extraccion de datos espaciales
source("04_Spatial_C4D.R")


# Estimacion de rendimiento base (caracterizacion), por ciclos (productividad)
source("04_Estimacion_Rendimiento.R")

# Encoding de factotres de exito 
source("05_Factores_exito.R")



source("06_join_to_ML.R")

load("data/2_archivos_procesados/data_raw.RData")
load("data/2_archivos_procesados/data_selected.RData")


load("data/2_archivos_procesados/data_no_outliers.RData")

load("data/2_archivos_procesados/rendimiento_base.RData")

load("data/2_archivos_procesados/test_factores_exito.RData")

load("data/2_archivos_procesados/rain_chirps_v2_C4D.RData")


load("data/2_archivos_procesados/spatial_C4D.RData")
