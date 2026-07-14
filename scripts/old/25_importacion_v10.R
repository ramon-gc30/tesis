# Librerías requeridas ========================================================

# manejo de datos
library(tidyverse)
# importación de Excel
library(readxl)
# paleta de colores
library(RColorBrewer)
# trabajar encuesta compleja dentro de tidyverse 
library(srvyr)
# regresión a encuesta compleja
library(survey)
# convertir a porcentaje dentro de mutate
library(scales) 
library(gt)
# mapas geográficos
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

# Fuente de datos =============================================================

# PIB vs egresados (INEGI; ANUIES)
pib_vs_egres <- read_csv(
  "datos/PIBvsEgresados.csv",
  col_types = cols(.default = col_double())
)


# tabla sociodemográfica
sdem <- read_csv(
  "datos/ENOE_SDEMT225.csv",
  col_types = cols(.default = col_character())
)

# tabla de ocupación y empleo I
coe1 <- read_csv(
  "datos/ENOE_COE1T225.csv",
  col_types = cols(.default = col_character())
)

# tabla de ocupación y empleo II
coe2 <- read_csv(
  "datos/ENOE_COE2T225.csv",
  col_types = cols(.default = col_character())
)

# tabla comparativa entre SINCO-11 y CIUO-08 (INEGI, 2012)
sinco11_ciuo08 <- read_csv(
  "datos/sinco11_a_ciuo08.csv",
  col_types = cols(.default = col_character()),
  na = "NA"
)

# tabla equivalencia entre v11 y v19 de SINCO (INEGI, 2020: 359-394)
sinco19_11 <- read_csv(
  "datos/sinco19_a_sinco11.csv",
  col_types = cols(.default = col_character()),
  na = "NA"
)

# tabla de ocupaciones de CIUO-08 (ILO, s/f)
ciuo08 <- read_xlsx(
  "datos/ISCO.xlsx",
  sheet = 2,
  col_types = "text"
)