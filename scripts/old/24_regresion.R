# Regresión ===================================================================

## MCO ------------------------------------------------------------------------

### General

model_general <- lm(
  log(ingreso_mensual) ~ 
    educacion + over_obj + under_obj + exper + I(exper**2) + 
    log(horas_lab) + mujer + casada + jefe_hogar + regiones + 
    ocupaciones + pos_ocu + sector_eco + tamanio + informal + 
    jornada + unidad_eco, 
  data = ocupada
  )

### Jóvenes

model_jov <- lm(
  log(ingreso_mensual) ~ 
    educacion + over_obj + under_obj + exper + I(exper**2) + 
    log(horas_lab) + mujer + casada + jefe_hogar + regiones + 
    ocupaciones + pos_ocu + sector_eco + tamanio + informal + 
    jornada + unidad_eco, 
  data = jovenes
  )

## Tipo survey ----------------------------------------------------------------


### General

model_general_svy <- svyglm(
  log(ingreso_mensual) ~ 
    educacion + over_obj + under_obj + exper + I(exper**2) + 
    log(horas_lab) + mujer + casada + jefe_hogar + regiones + 
    ocupaciones + pos_ocu + sector_eco + tamanio + informal + 
    jornada + unidad_eco, 
  design = ocupada_svy
  )

### Jóvenes

model_jov_svy <- svyglm(
  log(ingreso_mensual) ~ 
    educacion + over_obj + under_obj + exper + I(exper**2) + 
    log(horas_lab) + mujer + casada + jefe_hogar + regiones + 
    ocupaciones + pos_ocu + sector_eco + tamanio + informal + 
    jornada + unidad_eco, 
  design = jovenes_svy
  )