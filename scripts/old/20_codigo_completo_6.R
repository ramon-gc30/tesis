# Librerías requeridas ========================================================

# manejo de datos
library(tidyverse)
# estadísticos descriptivos ponderados
library(matrixStats)
# definir como encuesta de diseño complejo
library(srvyr)
# paleta de colores
library(RColorBrewer)
# letter value plot
library(lvplot)
# regresión
library(modelr)
library(survey) # encuesta disenio complejo
# análisis de residuos
library(performance)
library(car)
library(stats)
library(nortest)
library(moments) # coeficiente asimetría
library(lmtest)
library(see)
library(sandwich)
# convertir a porcentaje dentro de mutate
library(scales) 
library(gt)
# mapas geográficos
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

# Fuente de datos =============================================================

# PIB vs egresados
pib_vs_egres <- read_csv(
  "PIBvsEgresados.csv",
  col_types = cols(
    .default = col_double()
  )
)

# 3. sociodemográfica
sdem <- read_csv(
  "ENOE_SDEMT225.csv",
  col_types = cols(.default = col_character())
)

# 4. ocupación y empleo I
coe1 <- read_csv(
  "ENOE_COE1T225.csv",
  col_types = cols(.default = col_character())
)

# 5. ocupación y empleo II
coe2 <- read_csv(
  "ENOE_COE2T225.csv",
  col_types = cols(.default = col_character())
)

# Segmentación ================================================================

sdem <- sdem %>% mutate(
  # edad
  edad = as.integer(eda),
  # años de estudios
  anios_estudios = as.integer(anios_esc),
  # 99 pasa a NA para evitar 99 - edu_req
  anios_estudios = if_else(anios_estudios != 99, anios_estudios, NA),
  # grado escolar
  escolaridad = as.integer(cs_p13_1),
  # 99 pasa a NA para evitar 99 - edu_req
  escolaridad = if_else(escolaridad != 99, escolaridad, NA),
  # ponderador trimestral
  fac_tri = as.double(fac_tri),
  # ingreso mensual
  ingreso_mensual = as.double(ingocup),
  # 0 pasa a NA ya que 0==NA en p6b2
  ingreso_mensual = if_else(ingreso_mensual != 0, ingreso_mensual, NA),
  # promedio de ingreso por hora
  ingreso_hora = as.double(ing_x_hrs),
  # 0 pasa a NA ya que 0==NA en p6b2 y en dur9c == 1 & 9
  ingreso_hora = if_else(ingreso_hora != 0, ingreso_hora, NA)
)

# Construcción de variables ===================================================

# grupos de edadd OECD(2019: 135)
sdem <- sdem %>%
  mutate(
    edad_7g = case_when(
      between(edad, 0, 14) ~ "1",
      between(edad, 15, 24) ~ "2",
      between(edad, 25, 34) ~ "3",
      between(edad, 35, 44) ~ "4",
      between(edad, 45, 54) ~ "5",
      between(edad, 55, 64) ~ "6",
      between(edad, 65, 97) ~ "7",
      # edad no especificada mayores a 12 INEGI(2024:16)
      edad == "98" ~ "98",
      # edad no especificada menores a 12 INEGI(2024:16)
      edad == "99" ~ "99",
      # NA sin comillas para valores faltantes
      TRUE ~ NA
    )
  )

# carreras universitaria INEGI(2016: 13)
sdem <- sdem %>% 
  mutate(
    carrera1 = if_else(str_length(cs_p14_c) == 5, str_c("0", cs_p14_c), cs_p14_c),
    carrera2 = if_else(str_length(cs_p14_c) == 5, str_c("0", cs_p14_c), cs_p14_c),
    carrera3 = if_else(str_length(cs_p14_c) == 5, str_c("0", cs_p14_c), cs_p14_c)
  ) %>% 
  separate(
    carrera1,
    # campo amplio
    into = c("carrera1", "res"),
    sep = 2
  ) %>% 
  separate(
    carrera2,
    # campo específico
    into = c("carrera2", "res"),
    sep = 3
  ) %>% 
  separate(
    carrera3,
    # campo detallado
    into = c("carrera3", "res"),
    sep = 4
  ) %>% 
  select(-res)

# grupos ocupacionales INEGI(2019: 22)
coe1 <- coe1 %>% 
  mutate(
    ocupacion1 = p3,
    ocupacion2 = p3,
    ocupacion3 = p3
  ) %>% 
  separate(
    ocupacion1, 
    into = c("ocupacion1", "res"),
    sep = 1
  ) %>% 
  separate(
    ocupacion2,
    into = c("ocupacion2", "res"),
    sep = 2
  ) %>% 
  separate(
    ocupacion3,
    into = c("ocupacion3", "res"),
    sep = 3
  ) %>%
  select(-res)

# nivel de estudios concluidos ILO(s.f.)
sdem <- sdem %>% 
  mutate(
    escolaridad = case_when(
      # de doctorado a maestría
      escolaridad == 9 & cs_p16 == 2 ~ 8,
      # de maestría a licenciatura
      escolaridad == 8 & cs_p16 == 2 ~ 7,
      # de licenciatura, normal y técnica a preparatoria 
      between(escolaridad, 5, 7) & cs_p16 == 2 ~ 4,
      # de preparatoria a secundaria 
      escolaridad == 4 & cs_p16 == 2 ~ 3,
      # de secundaria a primaria
      escolaridad == 3 & cs_p16 == 2 ~ 2,
      # de primaria a NA
      escolaridad == 2 & cs_p16 == 2 ~ NA,
      TRUE ~ escolaridad
    )
  )

# agrupar estados por regiones
sdem <- sdem %>% 
  mutate(
    regiones = case_when(
      # centro
      ent == 11 | ent == 22 | ent == 13 | ent == 15 | ent == 9 | 
        ent == 17 | ent == 29 | ent == 21 ~ "1",
      # centro-norte
      ent == 14 | ent == 1 | ent == 6 | ent == 16 | ent == 24 ~ "2",
      # norte
      ent == 2 | ent == 26 | ent == 8 | ent == 5 | ent == 19 | 
        ent == 28 ~ "3",
      # norte-occidente
      ent == 3 | ent == 25 | ent == 18 | ent == 10 | ent == 32 ~ "4",
      # sur
      ent == 12 | ent == 20 | ent == 7 | ent == 30 | ent == 27 | 
        ent == 4 | ent == 31 | ent == 23 ~ "5",
      TRUE ~ NA
    )
  )

# Creación de tablas ==========================================================

# población de 12 y más para unir sdem y coe INEGI(2023:9)
pob_12ymas <- sdem %>% 
  filter(
    r_def == "0",
    c_res != "2",
    between(edad, 12, 98)
  )

# nrow(pob_12ymas)
# nrow(coe1)

# población total INEGI(2023:21)
pob_total <- sdem %>% 
  filter(
    r_def == "0",
    # es igual a (c_res == "1" | c_res == "3")
    c_res != "2"
  )

# población de 12 y más para unir sdem y coe INEGI(2023:9)
pob_12ymas <- sdem %>% 
  filter(
    r_def == "0",
    c_res != "2",
    between(edad, 12, 98)
  )

# unión de sdem y coe INEGI(2023:9)
completo <- pob_12ymas %>%
  full_join(coe1, by = c("tipo", "mes_cal", "cd_a", "ent", "con", "v_sel", "n_hog", "h_mud", "n_ren")) %>%
  full_join(coe2, by = c("tipo", "mes_cal", "cd_a", "ent", "con", "v_sel", "n_hog", "h_mud", "n_ren"))

# población de 15 años y más INEGI(2023:21)
pob_15ymas <- completo %>% 
  filter(
    r_def.x == "0",
    c_res != "2",
    between(edad, 15, 98)
  )

# INEGI(2023: 21)
ocupada <- pob_15ymas %>% 
  filter(clase2 == 1)

# INEGI(2023: 25)
desocupada <- pob_15ymas %>% 
  filter(clase2 == 2)

# INEGI(2023: 26)
pnea <- pob_15ymas %>% 
  filter(clase1 == 2)

# Método objetivo ============================================================

# con el grupo división INEGI(2020: 22)
educacion_req_objetivo <- ocupada %>% 
  group_by(ocupacion1) %>% 
  summarise(total = sum(fac_tri.x)) %>% 
  mutate(
    ocupacion1 = as.integer(ocupacion1),
    objetivo_min = case_when(
      between(ocupacion1, 1, 2) ~ 5,
      between(ocupacion1, 3, 8) ~ 3,
      ocupacion1 == 9 ~ 2,
      TRUE ~ NA
    ),
    objetivo_max = case_when(
      between(ocupacion1, 1, 2) ~ 9,
      between(ocupacion1, 3, 8) ~ 4,
      ocupacion1 == 9 ~ 2,
      TRUE ~ NA
    ),
    ocupacion1 = as.character(ocupacion1)
  ) %>% 
  select(-total)

# Criterio para la incidencia =================================================

# desajuste educativo con el método objetivo
objetivo <- ocupada %>% left_join(educacion_req_objetivo, by = "ocupacion1") %>%  
  mutate(
    desajuste_educativo = case_when(
      # required
      between(escolaridad, objetivo_min, objetivo_max) ~ 0,
      # over
      escolaridad > objetivo_max ~ 1,
      # under
      escolaridad < objetivo_min ~ 2,
      TRUE ~ NA
    ),
    # variables binarias
    over_obj = if_else(escolaridad > objetivo_max, 1, 0),
    required_obj = if_else(between(escolaridad, objetivo_min, objetivo_max), 1, 0),
    under_obj = if_else(escolaridad < objetivo_min, 1, 0)
    # variables de años de sobre o infra
    # objetivo: no se puede obtener ya que son grados y están en intervalos
  )

# Encuesta diseño complejo ====================================================

# necesario para calcular estadísticos descriptivos
# objetivo <- objetivo %>% 
#   as_survey_design(
#     ids = upm,
#     strata = est,
#     weights = fac_tri.x,
#     nest = TRUE
#   )

# Tabla jóvenes ===============================================================

jovenes <- objetivo %>% 
  filter(edad_7g == 3 & between(escolaridad, 5, 9))

# Tamaño de la muestra (análisis descriptivo) =================================

# muestra1 <- pob_total %>% 
#   summarise(n = n(), tot = sum(fac_tri)) %>% 
#   mutate(tabla = "pob_total") %>% 
#   select(tabla, n, tot)
# 
# muestra2 <- pob_15ymas %>% 
#   summarise(n = n(), tot = sum(fac_tri.x)) %>% 
#   mutate(tabla = "pob_15ymas") %>% 
#   select(tabla, n, tot)
# 
# muestra3 <- objetivo %>% 
#   summarise(n = n(), tot = sum(fac_tri.x)) %>% 
#   mutate(tabla = "objetivo") %>% 
#   select(tabla, n, tot)
# 
# muestra4 <- desocupada %>% 
#   summarise(n = n(), tot = sum(fac_tri.x)) %>% 
#   mutate(tabla = "desocupada") %>% 
#   select(tabla, n, tot)
# 
# muestra5 <- jovenes %>% 
#   summarise(n = n(), tot = sum(fac_tri.x)) %>% 
#   mutate(tabla = "jovenes") %>% 
#   select(tabla, n, tot)
# 
# muestra1 %>% 
#   add_row(muestra2) %>% 
#   add_row(muestra3) %>%
#   add_row(muestra4) %>% 
#   add_row(muestra5)

# Construcción de variables explicativas ======================================

## Objetivo ------------------------------------------------
objetivo <- objetivo %>%
  mutate(
    # horas trabajadas
    # 999 indica no específicado, se convierte a NA INEGI(2024:81)
    horas_lab = if_else(p5b_thrs != 999, p5b_thrs, NA),
    horas_lab = as.double(horas_lab),
    # experiencia laboral potencial
    # Zamudio e Islas(1999) y Mincer(1974)
    exper = edad - anios_estudios - 6,
    # si exper <= 0, se convierte a NA; no afecta a jóvenes
    # si edad == 98, se convierte a NA
    exper = if_else(exper > 0 & edad != 98, exper, NA),
    mujer = if_else(sex == 2, 1, 0),
    # estado civil == 1 si es casada
    casada = if_else(e_con == 5, 1, 0),
    # situación de hogar == 1 si es jefe INEGI(2020:37)
    jefe_hogar = if_else(par_c == 101, 1, 0),
    ocupaciones = factor(ocupacion1),
    # valor No sabe pasa a NA
    carreras = if_else(carrera1 != "99", carrera1, NA),
    carreras = factor(carreras),
    # posición en la ocupacion
    # valor no especificado pasa a NA
    pos_ocu = if_else(pos_ocu != 5, pos_ocu, NA),
    pos_ocu = factor(pos_ocu),
    # sector económico agregado
    # valor no especificado pasa a NA
    sector_eco = if_else(rama_est1 != 4, rama_est1, NA),
    sector_eco = factor(sector_eco),
    # núm de trabajadores
    # valor no espcificado pasa a NA
    # trabajadores = if_else(emple7c != 7, emple7c, NA),
    # trabajadores = factor(trabajadores),
    # tamaño de unidad económica
    tamanio = case_when(
      ambito2 == 2 | ambito2 == 3 ~ "1",
      # valor no especificado pasa a NA
      ambito2 == 0 | ambito2 == 8 ~ NA,
      TRUE ~ ambito2
    ),
    tamanio = factor(tamanio),
    informal = if_else(emp_ppal == 1, "1", "0"),
    # nivel educativo
    escolaridad = factor(escolaridad),
    # regiones
    regiones = factor(regiones)
  )

# Tablas para regresión =======================================================

# # general
# reg_general <- objetivo %>%
#   filter(
#     !is.na(ingreso_mensual),
#     !is.na(anios_estudios),
#     !is.na(horas_lab),
#     !is.na(exper)
#   )
# 
# # jovenes
# reg_jovenes <- objetivo %>%
#   mutate(escolaridad = as.integer(escolaridad)) %>% 
#   filter(edad_7g == 3 & between(escolaridad, 5, 9)) %>% 
#   mutate(escolaridad = factor(escolaridad))