# Requisitos ==================================================================

library(tidyverse)
# trabajar encuesta compleja dentro de tidyverse 
library(srvyr)
# regresión a encuesta compleja
library(survey)
# operador entre llaves 
library(rlang)
# iterar ejecución de funciones
library(purrr)

source("C:/Prog/tesis/scripts/0_funciones_v7.R")

# pob_15ymas <- read_rds("C:/Prog/tesis/datos/pob_15ymas.rds")
jovenes_tot <- read_rds("C:/Prog/tesis/datos/jovenes_tot.rds")
jovenes_ocu <- read_rds("datos/jovenes_normativo.rds")
sinco19 <- read_rds("datos/sinco_19_edu_req.rds")

jovenes_pea <- jovenes_tot |> 
  filter(clase1 == 1)

jovenes_pnea <- jovenes_tot |> 
  filter(clase1 == 2)

# Análisis descriptivo ========================================================

## Entradas -------------------------------------

# tamaño de la muestra
arg_total <- tribble(
  ~datos,
  # ---
  "jovenes_tot",
  "jovenes_ocu",
  "jovenes_pea",
  "jovenes_pnea"
)

# proporción por grupos
arg_prop <- tribble(
  ~datos, ~grupo,
  # ---
  "jovenes_tot", "clase1",
  "jovenes_pea", "clase2",
  "jovenes_pnea", "clase2",
  "jovenes_tot", "mujer",
  "jovenes_tot", "educacion",
  "jovenes_tot", "edad",
  "jovenes_tot", "campo_amplio",
  "jovenes_tot", "regiones",
  "jovenes_tot", "ent",
  "jovenes_ocu", "sector_eco",
  "jovenes_ocu", "tamanio",
  "jovenes_ocu", "division"
)

# proporción por subgrupos
arg_prop_sub <- tribble(
  ~datos, ~grupo, ~subgrupo,
  # ---
  "jovenes_tot", "clase1", "clase2",
  "jovenes_tot", "mujer", "clase1",
  "jovenes_pea", "mujer", "clase2",
  "jovenes_pnea", "mujer", "clase2",
  "jovenes_tot", "educacion", "clase1",
  "jovenes_pea", "educacion", "clase2",
  "jovenes_pnea", "educacion", "clase2",
  "jovenes_tot", "edad", "clase1",
  "jovenes_pea", "edad", "clase2",
  "jovenes_pnea", "edad", "clase2",
  "jovenes_tot", "campo_amplio", "clase1",
  "jovenes_pea", "campo_amplio", "clase2",
  "jovenes_pnea", "campo_amplio", "clase2",
  "jovenes_tot", "regiones", "clase1",
  "jovenes_pea", "regiones", "clase2",
  "jovenes_pnea", "regiones", "clase2",
  "jovenes_tot", "ent", "clase1",
  "jovenes_pea", "ent", "clase2",
  "jovenes_pnea", "ent", "clase2",
  "jovenes_ocu", "educacion", "edu_requerida"
)

# tasas general
arg_tasas <- tribble(
  ~datos, ~nombre, ~campo, ~operador, ~valor,
  # ---
  "jovenes_pea", "to", "clase2", "==", 1,
  "jovenes_pea", "td", "clase2", "==", 2,
  "jovenes_tot", "tp", "clase1", "==", 1,
  "jovenes_ocu", "tcco", "tcco", "!=", 0,
  "jovenes_ocu", "til1", "emp_ppal", "==", 1
)

# tasas por grupos
arg_tasas_sub <- tribble(
  ~datos, ~grupo, ~nombre, ~campo, ~operador, ~valor,
  # ---
  "jovenes_pea", "mujer", "to", "clase2", "==", "1",
  "jovenes_pea", "mujer", "td", "clase2", "==", "2",
  "jovenes_tot", "mujer", "tp", "clase1", "==", "1",
  "jovenes_ocu", "mujer", "tcco", "tcco", "!=", "0",
  "jovenes_ocu", "mujer", "til1", "emp_ppal", "==", "1",
  "jovenes_pea", "educacion", "to", "clase2", "==", "1",
  "jovenes_pea", "educacion", "td", "clase2", "==", "2",
  "jovenes_tot", "educacion", "tp", "clase1", "==", "1",
  "jovenes_ocu", "educacion", "tcco", "tcco", "!=", "0",
  "jovenes_ocu", "educacion", "til1", "emp_ppal", "==", "1",
  "jovenes_pea", "campo_amplio", "to", "clase2", "==", "1",
  "jovenes_pea", "campo_amplio", "td", "clase2", "==", "2",
  "jovenes_tot", "campo_amplio", "tp", "clase1", "==", "1",
  "jovenes_ocu", "campo_amplio", "tcco", "tcco", "!=", "0",
  "jovenes_ocu", "campo_amplio", "til1", "emp_ppal", "==", "1",
  "jovenes_pea", "regiones", "to", "clase2", "==", "1",
  "jovenes_pea", "regiones", "td", "clase2", "==", "2",
  "jovenes_tot", "regiones", "tp", "clase1", "==", "1",
  "jovenes_ocu", "regiones", "tcco", "tcco", "!=", "0",
  "jovenes_ocu", "regiones", "til1", "emp_ppal", "==", "1",
  "jovenes_pea", "ent", "to", "clase2", "==", "1",
  "jovenes_pea", "ent", "td", "clase2", "==", "2",
  "jovenes_tot", "ent", "tp", "clase1", "==", "1",
  "jovenes_ocu", "ent", "tcco", "tcco", "!=", "0",
  "jovenes_ocu", "ent", "til1", "emp_ppal", "==", "1"
)

## Tablas ---------------------------------------

# tamaño de la muestra
descriptivo_total <- pmap(arg_total, calcular_total_map) |> 
  list_rbind()

# proporciones por grupos
descriptivo_prop <- pmap(arg_prop, calcular_prop_map) |> 
  list_rbind()

# proporciones por subgrupos
descriptivo_prop_sub <- pmap(arg_prop_sub, calcular_prop_sub_map) |> 
  list_rbind()

# tasas complementarias de ocupación y desocupación general
descriptivo_tasas <- pmap(arg_tasas, calcular_tasas_ocu_map) |> 
  list_rbind()

# tasas complementarias de ocupación y desocupación por grupos
descriptivo_tasas_sub <- pmap(arg_tasas_sub, calcular_tasas_ocu_sub_map) |> 
  list_rbind()

# divisiones del SINCO-19 por grupos unitario de ocupación 
# según educación requerida
descriptivo_s19_div <- sinco19 |> 
  group_by(s19_div) |> 
  count(edu_requerida, .drop = FALSE) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup()

## Guardar --------------------------------------

write_rds(descriptivo_total, "datos/descriptivo_total.rds")
write_rds(descriptivo_prop, "datos/descriptivo_prop.rds")
write_rds(descriptivo_prop_sub, "datos/descriptivo_prop_sub.rds")
write_rds(descriptivo_tasas, "datos/descriptivo_tasas.rds")
write_rds(descriptivo_tasas_sub, "datos/descriptivo_tasas_sub.rds")
write_rds(descriptivo_s19_div, "datos/descriptivo_s19_div.rds")

# Incidencia ==================================================================

