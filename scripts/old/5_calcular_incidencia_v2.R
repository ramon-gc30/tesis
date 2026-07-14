# Análisis descriptivo ========================================================

# Tamaño de la muestra
obtener_tamanio_muestra <- function(datos){
  datos |> 
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL)
    )
}

# Variables dicotómicas o categóricas
obtener_tot_svy <- function(datos, variable){
  datos |>
    group_by({{  variable  }}) |>
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    )
}

# variables continuas
obtener_est_descriptivos <- function(datos, variable){
  datos |> 
    filter(!is.na({{  variable  }})) |> 
    summarise(
      minimo = min({{  variable  }}),
      promedio = survey_mean({{  variable  }}, vartype = NULL),
      desv_est = survey_sd({{  variable  }}),
      maximo = max({{  variable  }})
    )
}


# Incidencia ==================================================================

# General
calcular_incidencia_svy_tot <- function(datos, desajuste){
  datos |>
    group_by({{  desajuste  }}) |>
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    )
}

# Desagregada
# Por tipo de desajuste educativo
calcular_inc_svy_des <- function(datos, grupo, desajuste){
  datos |>
    group_by({{  grupo }}, {{  desajuste  }}) |>
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    )
}

# Por grupo
calcular_inc_svy_grp <- function(datos, desajuste, grupo){
  datos |>
    group_by({{  desajuste  }}, {{  grupo }}) |>
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    )
}