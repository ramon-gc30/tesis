# Funciones ===================================================================

## nivel de estudios en términos de CINE-11 ------------------------------------ 
# (UNESCO, 2013: 22)
construir_educacion_cine11 <- function(datos){
  datos_procesados <- datos |> 
    mutate(
      educacion = case_when(
        # si no concluyó sus estudios y reporta educación previa
        # se le asigna el nivel reportado en cs_p15 
        cs_p16 == "2" & !is.na(edu_previa) ~ edu_previa,
        # si no concluyó sus estudios y no reporta educación previa
        # doctorado baja a maestría
        cs_p16 == "2" & is.na(edu_previa) & educacion == 9 ~ 8,
        # maestría baja a licenciatura
        cs_p16 == "2" & is.na(edu_previa) & educacion == 8 ~ 7,
        # licenciatura baja a preparatoria
        cs_p16 == "2" & is.na(edu_previa) & educacion == 7 ~ 4,
        # carrera técnica baja a preparatoria
        cs_p16 == "2" & is.na(edu_previa) & educacion == 6 ~ 4,
        # normal baja a preparatoria
        cs_p16 == "2" & is.na(edu_previa) & educacion == 5 ~ 4,
        # preparatoria baja a secundaria
        cs_p16 == "2" & is.na(edu_previa) & educacion == 4 ~ 3,
        # secundaria baja a primaria 
        cs_p16 == "2" & is.na(edu_previa) & educacion == 3 ~ 2,
        # primaria baja a preescolar
        cs_p16 == "2" & is.na(edu_previa) & educacion == 2 ~ 1,
        # preescolar baja a ninguna
        cs_p16 == "2" & is.na(edu_previa) & educacion == 1 ~ 0,
        # si concluyó se le asigna el nivel educativo reportado
        TRUE ~ educacion
      )
    )
  return(datos_procesados)
}

## Métodos para determinar educación requerida ---------------------------------

### Objetivo ----

determinar_edu_obj <- function(ciuo08){
  # se añade skill level en grupo unitario de CIUO-08 (ILO, 2023: 46)
  ciuo08 <- ciuo08 |> 
    mutate(
      skill_level = case_when(
        # skill level 4
        sub_major == "11" | sub_major == "12" |
          sub_major == "13" | major == "2" |
          sub_major == "01" ~ "4",
        # skill level 3
        sub_major == "14" | major == "3" ~ "3",
        # skill level 2
        major == "4" | major == "5" |
          major == "6" | major == "7" |
          major == "8" | sub_major == "02" ~ "2",
        # skill level 1
        major == "9" | sub_major == "03" ~ "1",
        TRUE ~ NA,
      )
    ) |> 
    # seleccionamos unit y skill_level
    select(unit, skill_level) |> 
    # se renombran variables
    rename("c08_unit" = "unit")
  
  # se une tabla comparativa entre SINCO-11 con CIUO-08 
  # con ciuo08 que tiene skill level (INEGI, 2012)
  sinco11_ciuo08 <- sinco11_ciuo08 |> 
    left_join(ciuo08, by = "c08_unit")
  
  # se crea tabla sinco11 con valores únicos en grupo unitario
  sinco11 <- sinco11_ciuo08 |> 
    distinct(s11_unit, skill_level) |> 
    # para evitar errores al pivotar
    mutate(id = row_number()) |> 
    pivot_wider(
      id_cols = s11_unit,
      names_from = skill_level,
      values_from = skill_level,
      names_sort = TRUE
    ) |> 
    select(-`NA`)
  
  # se une tabla de equivalencia entre SINCO v11 y v19 
  # con sinco11 (INEGI, 2020: 359-394)
  sinco19_11 <- sinco19_11 |> 
    left_join(sinco11, by = "s11_unit")
  
  # se crea tabla sinco19 para tener más de un skill level en
  # una misma ocupación
  sinco19 <- sinco19_11 |> 
    pivot_longer(
      `1`:`4`,
      values_to = "skill_level"
    ) |> 
    select(-(s11_unit:name))
  
  # se pivotea a lo ancho para tener valores únicos de grupo unitario
  sinco19 <- sinco19 |> 
    distinct(s19_unit, skill_level) |> 
    # para evitar errores al pivotar
    mutate(dummy = "1") |> 
    pivot_wider(
      id_cols = s19_unit,
      names_from = skill_level,
      names_prefix = "skill_",
      names_sort = TRUE,
      values_from = dummy
    ) |> 
    select(-skill_NA)
  
  # se añade educación mínima y máxima por ocupación
  sinco19 <- sinco19 |> 
    mutate(
      objetivo_min = case_when(
        !is.na(skill_1) ~ 1,
        is.na(skill_1) & !is.na(skill_2) ~ 3,
        is.na(skill_1) & is.na(skill_2) & !is.na(skill_3) ~ 6,
        is.na(skill_1) & is.na(skill_2) & is.na(skill_3) & !is.na(skill_4) ~ 7,
        TRUE ~ NA
      ),
      objetivo_max = case_when(
        !is.na(skill_4) ~ 9,
        is.na(skill_4) & !is.na(skill_3) ~ 6,
        is.na(skill_4) & is.na(skill_3) & !is.na(skill_2) ~ 4,
        is.na(skill_4) & is.na(skill_3) & is.na(skill_2) & !is.na(skill_1) ~ 2,
        TRUE ~ NA
      )
    ) |> 
    select(-contains("skill"))
  return(sinco19)
}

## Criterio para asignar tipo de desajuste educativo --------------------------

asignar_edu_mismatch <- function(datos, ciuo08){
  
  ### Objetivo ----
  
  # llama función que define nivel educativo en términos de CINE-11
  datos_procesados <- construir_educacion_cine11(datos)
  
  # llama la función que contiene el procedimiento para
  # determinar la educación requerida
  sinco19 <- determinar_edu_obj(ciuo08)
  
  # realiza el criterio para asignar el tipo de desajuste educativo
  datos_procesados <- datos_procesados %>% 
    left_join(sinco19, by = c("p3" = "s19_unit")) %>%  
    mutate(
      educacion = as.integer(educacion),
      objetivo_min = as.integer(objetivo_min),
      objetivo_max = as.integer(objetivo_max),
      edu_mismatch_obj = case_when(
        # required
        between(educacion, objetivo_min, objetivo_max) ~ 0,
        # over
        educacion > objetivo_max ~ 1,
        # under
        educacion < objetivo_min ~ 2,
        TRUE ~ NA
      ),
      # variables binarias
      over_obj = if_else(educacion > objetivo_max, 1, 0),
      required_obj = if_else(between(educacion, objetivo_min, objetivo_max), 1, 0),
      under_obj = if_else(educacion < objetivo_min, 1, 0)
      # variables de años de sobre o infra
      # objetivo: no se puede obtener ya que la educación requerida está en intervalos
    )
  return(datos_procesados)
}

