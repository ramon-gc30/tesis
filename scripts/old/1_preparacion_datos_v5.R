# Requisitos ==================================================================

## Librerías -------

# manejo de datos
library(tidyverse)
# importación de Excel
library(readxl)
# trabajar encuesta compleja dentro de tidyverse 
library(srvyr)
# regresión a encuesta compleja
library(survey)
# operador entre llaves 
library(rlang)

## Archivos ------------------
source("C:/Prog/tesis/scripts/0_funciones_v9.R")

# Fuente de datos =============================================================

# egresados (ANUIES, s.f.)
anuies <- read_csv("datos/anuies.csv")

# pib (INEGI, s.f.)
pib <- read_csv(
  "datos/pib.csv"
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

# Construcción ================================================================

## Tablas ---------------------------------------------------------------------

# población total (INEGI, 2023:21)
pob_total <- sdem |> 
  filter(
    r_def == "0",
    # es igual a (c_res == "1" | c_res == "3")
    c_res != "2"
  )

# población de 12 y más para unir sdem y coe INEGI(2023:9)
pob_12ymas <- sdem |> 
  mutate(edad = as.integer(eda)) |> 
  filter(
    r_def == "0",
    c_res != "2",
    between(edad, 12, 98)
  )

# unión de sdem y coe INEGI(2023:9)
completo <- pob_12ymas |>
  full_join(coe1, by = c("tipo", "mes_cal", "cd_a", "ent", "con", "v_sel", "n_hog", "h_mud", "n_ren")) |>
  full_join(coe2, by = c("tipo", "mes_cal", "cd_a", "ent", "con", "v_sel", "n_hog", "h_mud", "n_ren"))

# población de 15 años y más INEGI(2023:21)
# para seleccionar población ocupada INEGI(2023:21)
pob_15ymas <- completo |> 
  filter(
    r_def.x == "0",
    c_res != "2",
    between(edad, 15, 98)
  )

# # PEA
# pea <- pob_15ymas |> 
#   filter(clase1 == 1)

# # PNEA (INEGI,2023: 26)
# pnea <- pob_15ymas %>% 
#   filter(clase1 == 2)

# población ocupada INEGI(2023: 21)
ocupada <- pob_15ymas |> 
  filter(clase2 == 1)

# # población desocupada (INEGI,2023: 25)
# desocupada <- pob_15ymas %>% 
#   filter(clase2 == 2)

# Egresados 

# seleccionan solamente egresados
egresados <- anuies |> 
  filter(TIPO == "EGRESADOS TOTAL")

# se separa ciclo escolar
egresados <- egresados |> 
  separate(
    PERIODO,
    into = c("periodo1", "periodo2"),
    sep = "-",
    convert = TRUE
  )

# se renombran columnas
egresados <-  egresados |> 
  select(periodo1, VALOR) |> 
  rename(
    "periodo" = "periodo1",
    "nivel" = "VALOR"
  )

# se calcula tasa de variación anual
egresados <- egresados |> 
  mutate(variacion = nivel / lag(nivel) - 1)

# PIB

# se calcula PIB anual
pib_anual <- pib |> 
  group_by(Periodo) |> 
  summarise(nivel = sum(Valor))

# se calcula tasa de variación anual
pib_anual <- pib_anual |> 
  mutate(variacion = nivel / lag(nivel) - 1)

# se selecciona periodo de interés
pib_anual <- pib_anual |> 
  filter(between(Periodo, 2000, 2024))

# se unen las tablas egresados y pib
pib_vs_egres <- full_join(egresados, pib_anual, by = c("periodo" = "Periodo")) |> 
  rename(
    "nivel_egres" = "nivel.x",
    "nivel_pib" = "nivel.y",
    "variacion_egres" = "variacion.x",
    "variacion_pib" = "variacion.y"
  )

# Selección de tablas =========================================================

## Población total
pob_total_proc <- construir_variables_ind(pob_total)
pob_total_proc <- construir_educacion_cine11(pob_total_proc)
pob_total_proc <- definir_encuesta(pob_total_proc)

## Población de 15 años y más
pob_15ymas_proc <- construir_variables_ind(pob_15ymas)
pob_15ymas_proc <- construir_educacion_cine11(pob_15ymas_proc)
pob_15ymas_proc <- definir_encuesta(pob_15ymas_proc)

## Jóvenes (25-34) egresados de educación superior 
jovenes_tot <- seleccionar_jov(pob_15ymas_proc)

## Enfoque normativo de OIT en la población ocupada
ocupada_normativo <- construir_variables_ind(ocupada)
ocupada_normativo <- construir_variables_ocu(ocupada_normativo)
ocupada_normativo <- asignar_edu_mismatch(ocupada_normativo, ciuo08, sinco11_ciuo08, sinco19_11)
ocupada_normativo <- convertir_a_factor(ocupada_normativo)
ocupada_normativo_svy <- definir_encuesta(ocupada_normativo)
ocupada_normativo_svy_reg <- seleccionar_obs_reg(ocupada_normativo_svy)

## Enfoque normativo de OIT en la población objetivo
jovenes_normativo <- seleccionar_jov(ocupada_normativo)
jovenes_normativo_svy <- definir_encuesta(jovenes_normativo)
jovenes_normativo_svy_reg <- seleccionar_obs_reg(jovenes_normativo_svy)

## SINCO-19 con educación requerida
sinco_19_edu_req <- determinar_edu_obj(ciuo08, sinco11_ciuo08, sinco19_11)

## Población objetivo antes de las manipulaciones realizadas
jovenes_orig <- ocupada |> 
  mutate(
    edad = as.integer(eda),
    educacion = as.integer(cs_p13_1)
  ) |> 
  filter(between(edad, 25, 34) & between(educacion, 5, 9))

# Guardar tablas ==============================================================

write_rds(pob_total_proc, "C:/Prog/tesis/datos/pob_total.rds")
write_rds(pob_15ymas_proc, "C:/Prog/tesis/datos/pob_15ymas.rds")
write_rds(jovenes_tot, "C:/Prog/tesis/datos/jovenes_tot.rds")
write_rds(ocupada_normativo_svy, "C:/Prog/tesis/datos/ocupada_normativo.rds")
write_rds(ocupada_normativo_svy_reg, "C:/Prog/tesis/datos/ocupada_normativo_reg.rds")
write_rds(jovenes_normativo_svy, "C:/Prog/tesis/datos/jovenes_normativo.rds")
write_rds(jovenes_normativo_svy_reg, "C:/Prog/tesis/datos/jovenes_normativo_reg.rds")
write_rds(sinco_19_edu_req, "C:/Prog/tesis/datos/sinco_19_edu_req.rds")
write_rds(jovenes_orig, "C:/Prog/tesis/datos/jovenes_orig.rds")
write_rds(pib_vs_egres, "C:/Prog/tesis/datos/pib_vs_egres.rds")
