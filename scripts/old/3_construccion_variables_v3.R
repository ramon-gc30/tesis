# Funciones ===================================================================

## Construir variables --------------------------------------------------------

construir_variables <- function(datos){
  # nivel educativo y años de escolaridad ----
  datos_procesados <- datos |> 
    mutate(
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
    )
  
  # grupos de edad ----
  datos_procesados <- datos_procesados |> 
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
    )
  
  # campos de formación académica ----
  # a 1 dígito de CMPE (INEGI, 2016: 13) 
  datos_procesados <- datos_procesados %>% 
    mutate(
      campo_amplio = if_else(str_length(cs_p14_c) == 5, str_c("0", cs_p14_c), cs_p14_c)
    ) %>% 
    separate(
      campo_amplio,
      # campo amplio a 1 dígito de CMPE
      into = c("campo_amplio", "res"),
      sep = 2
    ) %>% 
    select(-res)
  
  # ocupaciones ---- 
  # a 1 dígito de SINCO (INEGI, 2019: 22)
  datos_procesados <- datos_procesados %>% 
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
      division = if_else(p3 == "9999", "10", division)
    )
  
  # regiones ----
  # agrupar estados según (CESOP, 2022: 13)
  datos_procesados <- datos_procesados %>% 
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
    )
  
  # otras ----
  datos_procesados <- datos_procesados %>% 
    mutate(
      # ponderador trimestral
      fac_tri = as.double(fac_tri),
      # ingreso mensual
      ingreso_mensual = as.double(ingocup),
      # 0 pasa a NA ya que 0 en ingocup es igual a NA en p6b2
      ingreso_mensual = if_else(ingreso_mensual != 0, ingreso_mensual, NA),
      # resto de variables explicativas
      # horas trabajadas
      # 999 indica no especificado, se convierte a NA (INEGI, 2024:81)
      horas_lab = if_else(p5b_thrs != 999, p5b_thrs, NA),
      horas_lab = as.double(horas_lab),
      # experiencia laboral potencial
      # Zamudio e Islas(1999) y Mincer(1974)
      exper = edad - anios_estudios - 6,
      mujer = if_else(sex == 2, 1, 0),
      casada = if_else(e_con == 5, 1, 0),
      # situación de hogar (INEGI, 2020:37)
      jefe_hogar = if_else(par_c == 101, 1, 0),
      # tamaño de unidad económica (INEGI, 2024: 21)
      tamanio = case_when(
        # valores 2 y 3 pertenecen a micronegocios
        ambito2 == 2 | ambito2 == 3 ~ "1",
        # valores 0 y 8 indican no especificado
        ambito2 == 0 | ambito2 == 8 ~ "8",
        TRUE ~ ambito2
      ),
      # empleo informal (INEGI, 2024: 26)
      informal = if_else(emp_ppal == 1, "1", "0")
    )
  return(datos_procesados)
}

## Convertir a factor ---------------------------------------------------------

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

