# Librerías requeridas ========================================================

# manejo de datos
library(tidyverse)
# importación de Excel
library(readxl)
# estadísticos descriptivos ponderados
library(matrixStats)
# definir como encuesta de diseño complejo
library(srvyr)
# paleta de colores
library(RColorBrewer)
# letter value plot
library(lvplot)
# regresión
library(modelr)
library(survey) # encuesta disenio complejo
# análisis de residuos
library(performance)
library(car)
library(stats)
library(nortest)
library(moments) # coeficiente asimetría
library(lmtest)
library(see)
library(sandwich)
# convertir a porcentaje dentro de mutate
library(scales) 
library(gt)
# mapas geográficos
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

# Fuente de datos =============================================================

# PIB vs egresados
pib_vs_egres <- read_csv(
  "PIBvsEgresados.csv",
  col_types = cols(
    .default = col_double()
  )
)

# 3. sociodemográfica
sdem <- read_csv(
  "ENOE_SDEMT225.csv",
  col_types = cols(.default = col_character())
)

# 4. ocupación y empleo I
coe1 <- read_csv(
  "ENOE_COE1T225.csv",
  col_types = cols(.default = col_character())
)

# 5. ocupación y empleo II
coe2 <- read_csv(
  "ENOE_COE2T225.csv",
  col_types = cols(.default = col_character())
)

# tabla comparativa entre SINCO-11 y CIUO-08
sinco11_ciuo08 <- read_csv(
  "sinco11_a_ciuo08.csv",
  col_types = cols(.default = col_character()),
  na = "NA"
)

# tabla equivalencia SINCO 11-19
sinco19_11 <- read_csv(
  "sinco19_a_sinco11.csv",
  col_types = cols(.default = col_character()),
  na = "NA"
)

# CIUO-08
ciuo08 <- read_xlsx(
  "ISCO.xlsx",
  sheet = 2,
  col_types = "text"
)