# Cuadros =====================================================================

## Calcular incidencia --------------------------------------------------------

### Objetivo ----
calcular_incidencia <- function(datos){
  datos_procesados <- datos |> 
    group_by(edu_mismatch_obj) |> 
    summarise(
      n = n(),
      tot = sum(fac_tri)
    ) |> 
    mutate(prop = tot / sum(tot))
  
  return(datos_procesados)
}

## Estadística descriptiva ----------------------------------------------------

### tasa de variación promedio 
calcular_tcp <- function(datos){
  datos_procesados <- datos |> 
    summarise(
      vf = last(nivel),
      vi = first(nivel),
      n = n(),
      tcp = (vf / vi)**(1/n) - 1
    )
  return(datos_procesados)  
}