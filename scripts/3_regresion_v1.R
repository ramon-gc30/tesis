# REQUISITOS ==================================================================
library(tidyverse)
library(survey)
library(srvyr)
library(here)

jovenes_reg <- read_rds(here::here("datos", "export", "jovenes_reg.rds"))

# Regresión ===================================================================

## General -----
model_jovenes <- svyglm(
  log(ingreso_mensual) ~ 
    anios_estudios + over_obj + exper + I(exper**2) + 
    log(horas_lab) + mujer + jefe_hogar + regiones + campo_amplio +
    division + pos_ocu + tamanio + informal + 
    jornada + unidad_eco, 
  design = jovenes_reg
)

## Términos de interacción -----

# Años de estudio
model_jovenes_edu <- svyglm(
  log(ingreso_mensual) ~ 
    over_obj * anios_estudios + exper + I(exper**2) + 
    log(horas_lab) + mujer + jefe_hogar + regiones + campo_amplio +
    division + pos_ocu + tamanio + informal + 
    jornada + unidad_eco, 
  design = jovenes_reg
)

# Género
model_jovenes_muj <- svyglm(
  log(ingreso_mensual) ~ 
    anios_estudios + over_obj * mujer + exper + I(exper**2) + 
    log(horas_lab) + jefe_hogar + regiones + campo_amplio +
    division + pos_ocu + tamanio + informal + 
    jornada + unidad_eco, 
  design = jovenes_reg
)

# Campos de formación académica
model_jovenes_cam <- svyglm(
  log(ingreso_mensual) ~ 
    anios_estudios + over_obj * campo_amplio + exper + I(exper**2) + 
    log(horas_lab) + mujer + jefe_hogar + regiones +
    division + pos_ocu + tamanio + informal + 
    jornada + unidad_eco, 
  design = jovenes_reg
)

# Regiones
model_jovenes_reg <- svyglm(
  log(ingreso_mensual) ~ 
    anios_estudios + over_obj * regiones + exper + I(exper**2) + 
    log(horas_lab) + mujer + jefe_hogar + campo_amplio +
    division + pos_ocu + tamanio + informal + 
    jornada + unidad_eco, 
  design = jovenes_reg
)

## Prueba Wald -----

contraste_over_edu <- regTermTest(model_jovenes_edu, "over_obj:anios_estudios")
contraste_over_muj <- regTermTest(model_jovenes_muj, "over_obj:mujer")
contraste_over_cam <- regTermTest(model_jovenes_cam, "over_obj:campo_amplio")
contraste_over_reg <- regTermTest(model_jovenes_reg, "over_obj:regiones")

# Resultados ==================================================================

resultados_jov <- summary(model_jovenes)
resultados_edu <- summary(model_jovenes_edu)
resultados_muj <- summary(model_jovenes_muj)
resultados_cam <- summary(model_jovenes_cam)
resultados_reg <- summary(model_jovenes_reg)
