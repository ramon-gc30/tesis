# Análisis descriptivo ========================================================

# tasa de variación
calcular_tvar <- function(datos, nivel){
  egresados |> 
    mutate(variacion = {{  nivel  }} / lag({{  nivel  }}) - 1)
}

# tasa de variación promedio 
calcular_tcp <- function(datos, nivel){
  datos |> 
    summarise(
      vf = last({{ nivel }}),
      vi = first({{ nivel }}),
      n = n(),
      tcp = (vf / vi)**(1/n) - 1
    )
}

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
  # se enmascara nombre de columna
  nombre_var <- as_label(enquo(variable))
  
  datos |> 
    filter(!is.na({{  variable  }})) |> 
    summarise(
      # se inserta nombre de la columna
      variable = nombre_var, 
      minimo = min({{  variable  }}),
      promedio = survey_mean({{  variable  }}, vartype = NULL),
      desv_est = survey_sd({{  variable  }}),
      maximo = max({{  variable  }})
    )
}

