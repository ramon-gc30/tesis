# Funciones ===================================================================

## Construir variables --------------------------------------------------------

construir_variables_ind <- function(datos){
  # nivel educativo y años de escolaridad ----
  datos_procesados <- datos |> 
    mutate(
      edad = as.integer(eda),
      # años de escolaridad (INEGI, 2024: 25)
      anios_estudios = as.integer(anios_esc),
      # 99 indica no especificado, se convierte a NA
      anios_estudios = if_else(anios_estudios != 99, anios_estudios, NA),
      # nivel escolar
      educacion = as.integer(cs_p13_1),
      # 99 pasa a NA para evitar 99 - edu_req
      educacion = if_else(educacion != 99, educacion, NA),
      # 5 pasa a 7 ambos son estudios profesionales
      educacion = if_else(educacion == 5, 7, educacion),
      # cambian los valores de cs_p15 para que coincidan con cs_p13_1
      edu_previa = case_when(
        # primaria
        cs_p15 == "1" ~ 2,
        # secundaria
        cs_p15 == "2" ~ 3,
        # preparatoria
        cs_p15 == "3" ~ 4,
        TRUE ~ NA
      ),
      # si reporta estudios previos de secundaria en carrera técnica
      # se le asigna estudios de preparatoria (LGE, Art 47; LGES, Art 3)
      educacion = if_else(educacion == 6 & edu_previa == 3, 4, educacion)
    ) |> 
    # grupos de edad ----
  mutate(
    # 98 y 99 indican edad no especificada
    # se convierten a NA para evitar errores de cálculo en exper
    edad = if_else(edad == 98 | edad == 99, NA, edad),
    # para crear grupo de jóvenes 25-34 años OECD(2019: 135) 
    # y comparar incidencia por grupos de edad
    edad_7g = case_when(
      between(edad, 15, 24) ~ 1,
      between(edad, 25, 34) ~ 2,
      between(edad, 35, 44) ~ 3,
      between(edad, 45, 54) ~ 4,
      between(edad, 55, 64) ~ 5,
      between(edad, 65, 97) ~ 6,
      # valor 7 indica no especificado
      TRUE ~ 7
    )
  ) |> 
    # campos de formación académica ----
  # a 1 dígito de CMPE (INEGI, 2016: 13) 
  mutate(
    campo_amplio = if_else(str_length(cs_p14_c) == 5, str_c("0", cs_p14_c), cs_p14_c),
    campo_espec = if_else(str_length(cs_p14_c) == 5, str_c("0", cs_p14_c), cs_p14_c),
    campo_detal = if_else(str_length(cs_p14_c) == 5, str_c("0", cs_p14_c), cs_p14_c)
  ) %>% 
    separate(
      campo_amplio,
      # campo amplio a 1 dígito de CMPE
      into = c("campo_amplio", "res"),
      sep = 2
    ) %>%
    separate(
      campo_espec,
      # campo específico a 2 dígitos de CMPE
      into = c("campo_espec", "res"),
      sep = 3
    ) %>% 
    separate(
      campo_detal,
      # campo detallado a 3 dígitos de CMPE
      into = c("campo_detal", "res"),
      sep = 4
    ) %>%
    select(-res) |> 
    # regiones ----
  # agrupar estados según (CESOP, 2022: 13)
  mutate(
    regiones = case_when(
      # sur
      ent == 12 | ent == 20 | ent == 7 | ent == 30 | ent == 27 | 
        ent == 4 | ent == 31 | ent == 23 ~ "1",
      # centro
      ent == 11 | ent == 22 | ent == 13 | ent == 15 | ent == 9 | 
        ent == 17 | ent == 29 | ent == 21 ~ "2",
      # centro-norte
      ent == 14 | ent == 1 | ent == 6 | ent == 16 | ent == 24 ~ "3",
      # norte
      ent == 2 | ent == 26 | ent == 8 | ent == 5 | ent == 19 | 
        ent == 28 ~ "4",
      # norte-occidente
      ent == 3 | ent == 25 | ent == 18 | ent == 10 | ent == 32 ~ "5",
      TRUE ~ NA
    )
  ) |> 
    # otras ----
  mutate(
    # ponderador trimestral
    fac_tri = as.double(fac_tri),
    # experiencia laboral potencial
    # Zamudio e Islas(1999) y Mincer(1974)
    exper = edad - anios_estudios - 6,
    mujer = if_else(sex == 2, 1, 0),
    casada = if_else(e_con == 5, 1, 0),
    # situación de hogar (INEGI, 2020:37)
    jefe_hogar = if_else(par_c == 101, 1, 0)
  )
  return(datos_procesados)
}

construir_variables_ocu <- function(datos){
  ### ocupaciones 
  # a 1 dígito de SINCO (INEGI, 2019: 22)
  datos_procesados <- datos %>% 
    mutate(division = p3) %>% 
    separate(
      division, 
      into = c("division", "res"),
      sep = 1
    ) %>% 
    select(-res) |> 
    mutate(
      # ocupación 9999 pertenece a ocupaciones no especificadas (INEGI, 2020: 356)
      # se crea una división que contiene casos no especificados
      division = if_else(p3 == "9999", "10", division),
      # ingreso mensual
      ingreso_mensual = as.double(ingocup),
      # 0 pasa a NA ya que 0 en ingocup es igual a NA en p6b2
      ingreso_mensual = if_else(ingreso_mensual != 0, ingreso_mensual, NA),
      # resto de variables explicativas
      # horas trabajadas
      # 999 indica no especificado, se convierte a NA (INEGI, 2024:81)
      horas_lab = if_else(p5b_thrs != 999, p5b_thrs, NA),
      horas_lab = as.double(horas_lab),
      # tamaño de unidad económica (INEGI, 2024: 21)
      tamanio = case_when(
        # valores 2 y 3 pertenecen a micronegocios
        ambito2 == 2 | ambito2 == 3 ~ "1",
        # valores 0 y 8 indican no especificado
        ambito2 == 0 | ambito2 == 8 ~ "8",
        TRUE ~ ambito2
      ),
      # empleo informal (INEGI, 2024: 26)
      informal = if_else(emp_ppal == 1, "1", "0"),
      # nivel de ingreso
      salario = as.double(salario),
      ingreso_nivel = case_when(
        # hasta un salario mínimo
        ingreso_mensual <= salario ~ "1",
        # más de 1 hasta 2 salarios mínimos
        (ingreso_mensual > salario) & (ingreso_mensual <= (salario * 2)) ~ "2",
        # más de 2 hasta 3 salarios mínimos
        (ingreso_mensual > (salario * 2)) & (ingreso_mensual <= (salario * 3)) ~ "3",
        # más de 3 hasta 5 salarios mínimos
        (ingreso_mensual > (salario * 3)) & (ingreso_mensual <= (salario * 5)) ~ "4",
        # más de 5 salarios mínimos
        ingreso_mensual > (salario * 5) ~ "5",
        # ingreso no reportado
        is.na(ingreso_mensual) ~ "6",
        TRUE ~ NA
      )
    ) 
  return(datos_procesados)
}


### Convertir a factor -----

convertir_a_factor <- function(datos){
  datos_procesados <- datos |> 
    mutate(
      edad_7g = factor(edad_7g),
      educacion = factor(educacion),
      regiones = factor(regiones),
      campo_amplio = factor(campo_amplio),
      division = factor(division),
      pos_ocu = factor(pos_ocu),
      sector_eco = factor(rama_est1),
      tamanio = factor(tamanio),
      jornada = factor(dur_est),
      unidad_eco = factor(tue1)
    )
  return(datos_procesados)
}

### Establecer tipo de educación -----

establecer_edu_tipo <- function(datos){
  datos |>
    mutate(
      educacion = as.integer(levels(educacion))[educacion],
      edu_tipo = case_when(
        # Menor a básica
        between(educacion, 0, 1) ~ 0,
        # Básica
        between(educacion, 2, 3) ~ 1,
        # Media superior
        educacion == 4 ~ 2,
        # Superior
        between(educacion, 6, 9) ~ 3,
        TRUE ~ NA
      ),
      educacion = factor(educacion),
      edu_tipo = factor(edu_tipo)
    )
}

### Establecer educación requerida aproximada -----

establecer_edu_req_aprox <- function(datos, edu_req){
  datos |> 
    mutate(
      edu_req_aprox = case_when(
        # menor a básica
        {{  edu_req  }} == "1-2" ~ "0",
        # básica a media superior
        {{  edu_req  }} == "1-4" | {{  edu_req  }} == "3-4" ~ "1",
        # educación superior
        {{  edu_req  }} == "6-6" | {{  edu_req  }} == "6-9" | 
          {{  edu_req  }} == "7-9" ~ "2",
        # otros
        # secundaria-técnico superior o secundaria-doctorado
        {{  edu_req  }} == "3-6" |  {{  edu_req  }} == "3-9" ~ "3",
        # no especificado
        TRUE ~ NA
      )
    )
}

## Tablas ---------------------------------------------------------------------

### Tabla jóvenes -----

seleccionar_jov <- function(datos){
  datos_procesados <- datos |> 
    # se define jóvenes a personas de 25 a 34 años 
    # egresados de educación superior
    filter(
      edad_7g == "2" & 
        (educacion == "6" | educacion == "7" | 
           educacion == "8" | educacion == "9")
    )
  return(datos_procesados)
}

### Tabla para la regresión -----

# se selecciona ingreso reportado, horas trabajadas especificadas, 
# experiencia calculada, desajuste educativo determinado
seleccionar_obs_reg <- function(datos){
  datos_procesados <- datos |>
    filter(
      !is.na(ingreso_mensual) & 
        !is.na(horas_lab) & 
        !is.na(exper) & 
        !is.na(over_obj)
    )
  return(datos_procesados)
}

### Tabla de encuesta compleja -----
# se define como encuesta compleja
definir_encuesta <- function(datos){
  datos_procesados <- datos |> 
    as_survey_design(
      ids = upm,
      strata = est,
      weights = fac_tri,
      nest = TRUE
    )
  return(datos_procesados)
}

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

determinar_edu_obj <- function(ciuo08, sinco11_ciuo08, sinco19_11){
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
  
  # se limpia tabla comparativa de CIUO-08 con SINCO-11
  sinco11_ciuo08 <- sinco11_ciuo08 |>
    select(sinco11, ciuo08) |>
    # nos quedamos con grupo unitario
    filter(str_length(sinco11) == 4) |>
    # se renombran variables para unión
    rename(
      "s11_unit" = "sinco11",
      "c08_unit" = "ciuo08"
    )
  
  # se limpia tabla de equivalencia entre v11 y v19 de SINCO
  sinco19_11 <- sinco19_11 |>
    select(sinco_19, sinco_11) |>
    # nos quedamos con grupo unitario
    filter(str_length(sinco_19) == 4) |>
    # se renombran variables para unión
    rename(
      "s19_unit" = "sinco_19",
      "s11_unit" = "sinco_11"
    )
  
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
      ),
      edu_requerida = str_c(objetivo_min, "-", objetivo_max)
    )
  return(sinco19)
}

## Criterio para asignar tipo de desajuste educativo --------------------------

asignar_edu_mismatch <- function(datos, ciuo08, sinco11_ciuo08, sinco19_11){
  
  ### Objetivo ----
  
  # llama función que define nivel educativo en términos de CINE-11
  datos_procesados <- construir_educacion_cine11(datos)
  
  # llama la función que contiene el procedimiento para
  # determinar la educación requerida
  sinco19 <- determinar_edu_obj(ciuo08, sinco11_ciuo08, sinco19_11)
  
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

## Análisis descriptivo =======================================================

# tasa de variación
calcular_tasa_var <- function(datos, nivel){
  egresados |> 
    mutate(variacion = {{  nivel  }} / lag({{  nivel  }}) - 1)
}

# tasa de variación promedio 
calcular_tasa_mean <- function(datos, nivel){
  datos |> 
    summarise(
      vf = last({{ nivel }}),
      vi = first({{ nivel }}),
      n = n(),
      tcp = (vf / vi)**(1/n) - 1
    )
}

# calcular tasas complementarias de ocupación y desocupación
calcular_tasas_ocup <- function(datos, nombre, campo, valor){
  datos |>
    mutate(ponderador = as.double(fac_tri)) |>
    summarise(
      nombre = nombre,
      numerador = sum(ponderador[{{  campo  }} == valor]),
      denominador = sum(ponderador),
      tasa = numerador / denominador
    )
}

# tasas complementarios por grupos
calcular_tasas_ocu_sub <- function(datos, grupo, campo, valor){
  datos |> 
    mutate(ponderador = as.double(fac_tri)) |> 
    group_by({{ grupo }}) |> 
    summarise(
      numerador = sum(ponderador[{{  campo  }} == valor]),
      denominador = sum(ponderador),
      tasa = numerador / denominador
    )
}

# tasas complementarias con operador
calcular_tasas_ocup_operador <- function(datos, nombre, campo, operador, valor){
  # concatenar
  filtro <- paste(campo, operador, valor)
  
  datos |>
    mutate(ponderador = as.double(fac_tri)) |>
    summarise(
      nombre = nombre,
      numerador = sum(ponderador[ !!parse_expr(filtro) ]),
      denominador = sum(ponderador),
      tasa = numerador / denominador
    )
}

# tasas complementarias con operador por grupos
calcular_tasas_ocup_operador_sub <- function(datos, grupo, campo, operador, valor){
  # concatenar
  filtro <- paste(campo, operador, valor)
  
  datos |>
    mutate(ponderador = as.double(fac_tri)) |>
    group_by({{  grupo  }}) |> 
    summarise(
      numerador = sum(ponderador[ !!parse_expr(filtro) ]),
      denominador = sum(ponderador),
      tasa = numerador / denominador
    )
}

# Tamaño de la muestra
calcular_total <- function(datos){
  datos |> 
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL)
    )
}

# Variables dicotómicas o categóricas
calcular_prop <- function(datos, variable){
  datos |>
    group_by({{  variable  }}) |>
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    )
}

# Variables dicotómicas o categóricas desagregadas
calcular_prop_sub <- function(datos, grupo, variable){
  datos |>
    group_by({{  grupo  }}, {{  variable  }}) |>
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    )
}

calcular_prop_sub_var <- function(datos, grupo, subgrupo, variable){
  datos |>
    group_by({{  grupo  }}, {{  subgrupo  }}, {{  variable  }}) |>
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    )
}

# variables continuas
calcular_descriptivos <- function(datos, variable){
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

# funciones que iteran el cálculo de tasas de ocupación y proporciones

# tasas complementarias de ocupación y desocupación
calcular_tasas_ocu_map <- function(datos, nombre, campo, operador, valor){
  # busca el tibble especificado por el usuario
  poblacion <- get(datos)
  # concatena
  filtro <- paste(campo, operador, valor)
  
  poblacion |> 
    mutate(ponderador = as.double(fac_tri)) |> 
    summarise(
      datos = datos,
      nombre = nombre,
      numerador = sum(ponderador[  !!parse_expr(filtro)  ]),
      denominador = sum(ponderador),
      tasa = numerador / denominador
    ) 
}

calcular_tasas_ocu_sub_map <- function(datos, grupo, nombre, campo, operador, valor){
  # busca el tibble especificado por el usuario
  poblacion <- get(datos)
  # pasa de cadena de texto a símbolo
  grupo_sym <- sym(grupo)
  # concatena
  filtro <- paste(campo, operador, valor)
  
  poblacion |> 
    mutate(ponderador = as.double(fac_tri)) |> 
    group_by(  !!grupo_sym  ) |> 
    summarise(
      grupo = grupo,
      nombre = nombre,
      numerador = sum(ponderador[  !!parse_expr(filtro)  ]),
      denominador = sum(ponderador),
      tasa = numerador / denominador,
      # elimina agrupación
      .groups = "drop"
    ) |> 
    # renombra columna que contiene valores del grupo
    rename("subgrupo" :=  !!grupo_sym  ) |> 
    mutate(subgrupo = as.character(subgrupo)) |> 
    relocate(subgrupo, .after = grupo)
}

# tamaño de la muestra
calcular_total_map <- function(datos){
  poblacion <- get(datos)
  
  poblacion |> 
    summarise(
      poblacion = datos,
      n = n(),
      tot = survey_total(vartype = NULL)
    )
}

# calcular proporciones por grupos
calcular_prop_map <- function(datos, grupo){
  poblacion <- get(datos)
  grupo_sym <- sym(grupo)
  
  poblacion |>
    group_by(  !!grupo_sym  ) |>
    summarise(
      poblacion = datos,
      grupo = grupo,
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    ) |> 
    # homologar para unir
    rename("valor" :=  !!grupo_sym  ) |> 
    mutate(valor = as.character(valor)) |> 
    relocate(valor, .after = grupo)
}

# calcular proporciones por subgrupos
calcular_prop_sub_map <- function(datos, grupo, subgrupo){
  poblacion <- get(datos)
  grupo_sym <- sym(grupo)
  subgrupo_sym <- sym(subgrupo)
  
  poblacion |>
    group_by(  !!grupo_sym  ,  !!subgrupo_sym  ) |>
    summarise(
      poblacion = datos,
      gpo_lab = grupo,
      sub_lab = subgrupo,
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL),
      .groups = "drop"
    ) |>
    # homologar para unir
    rename(
      "gpo_val" :=  !!grupo_sym,
      "sub_val" :=  !!subgrupo_sym
    ) |> 
    mutate(
      gpo_val = as.character(gpo_val),
      sub_val = as.character(sub_val)
    ) |> 
    relocate(gpo_val, .after = sub_lab) |> 
    relocate(sub_val, .before = n)
}

## Incidencia =================================================================

# General
calcular_incidencia_tot <- function(datos, desajuste){
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
calcular_incidencia_sub <- function(datos, grupo, subgrupo){
  datos |>
    group_by({{  grupo }}, {{  subgrupo  }}) |>
    summarise(
      n = n(),
      tot = survey_total(vartype = NULL),
      prop = survey_prop(vartype = NULL)
    )
}

## Figuras ====================================================================
# función para guardar gráfica
guardar_figura <- function(grafica, nombre_archivo){
  ggsave(
    filename = nombre_archivo,
    plot = grafica,
    width = 17,
    height = 10,
    units = "cm",
    dpi = 300 # asegura alta calidad
  )
  
  # mensaje de confirmación
  # message(paste("Gráfica guardada exitosamente como:", nombre_archivo))
}