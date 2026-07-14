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