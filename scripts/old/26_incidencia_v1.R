# Análisis descriptivo ========================================================

# Tamaño de la muestra
obtener_tamanio_muestra <- function(datos){
  datos_procesados <- datos |> 
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL)  
    )
  return(datos_procesados)
}

# Variables dicotómicas o categóricas
obtener_tot_svy <- function(datos){
  datos_procesados <- datos |>
    group_by(variable) |>
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    )
  return(datos_procesados)
}

# variables continuas
obtener_est_descriptivos <- function(datos){
  datos_procesados <- datos |> 
    summarise(
      minimo = min(variable, na.rm = TRUE),
      promedio = survey_mean(variable, na.rm = TRUE, vartype = NULL),
      desv_est = survey_sd(variable, na.rm = TRUE),
      maximo = max(variable, na.rm = TRUE)
    )
  return(datos_procesados)
}

# Incidencia ==================================================================

# General
calcular_incidencia_svy_tot <- function(datos){
  datos_procesados <- datos |>
    group_by(desajuste) |>
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    )
  return(datos_procesados)
}

# Desagregada
# Por tipo de desajuste educativo
calcular_inc_svy_des <- function(datos){
  datos_procesados <- datos |>
    group_by(grupo, desajuste) |>
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    )
  return(datos_procesados)
}

# Por grupo
calcular_inc_svy_grp <- function(datos){
  datos_procesados <- datos |>
    group_by(desajuste, grupo) |>
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    )
  return(datos_procesados)
}