# Librerías requeridas ========================================================

# manejo de datos
library(tidyverse)
# importación de Excel
library(readxl)
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
  "datos/PIBvsEgresados.csv",
  col_types = cols(
    .default = col_double()
  )
)

# 3. sociodemográfica
sdem <- read_csv(
  "datos/ENOE_SDEMT225.csv",
  col_types = cols(.default = col_character())
)

# 4. ocupación y empleo I
coe1 <- read_csv(
  "datos/ENOE_COE1T225.csv",
  col_types = cols(.default = col_character())
)

# 5. ocupación y empleo II
coe2 <- read_csv(
  "datos/ENOE_COE2T225.csv",
  col_types = cols(.default = col_character())
)

# tabla comparativa entre SINCO-11 y CIUO-08
sinco11_ciuo08 <- read_csv(
  "datos/sinco11_a_ciuo08.csv",
  col_types = cols(.default = col_character()),
  na = "NA"
)

# tabla equivalencia SINCO 11-19
sinco19_11 <- read_csv(
  "datos/sinco19_a_sinco11.csv",
  col_types = cols(.default = col_character()),
  na = "NA"
)

# CIUO-08
ciuo08 <- read_xlsx(
  "datos/ISCO.xlsx",
  sheet = 2,
  col_types = "text"
)

# Construcción ================================================================

## Variables ------------------------------------------------------------------

# grupos de edad OECD(2019: 135)
sdem <- sdem %>%
  mutate(
    # para facilitar manipulación
    edad = as.integer(eda),
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
sdem <- sdem |> 
  mutate(
    # nivel escolar
    educacion = as.integer(cs_p13_1),
    # 99 pasa a NA para evitar 99 - edu_req
    educacion = if_else(educacion != 99, educacion, NA),
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
    ),
    # 5 pasa a 7 para método objetivo a 4 dígitos
    educacion = if_else(educacion == 5, 7, educacion)
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

## Tablas ---------------------------------------------------------------------

# población de 12 y más para unir sdem y coe INEGI(2023:9)
pob_12ymas <- sdem %>% 
  filter(
    r_def == "0",
    c_res != "2",
    between(edad, 12, 98)
  )

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

# Métodos para determinar educación requerida =================================

## Método objetivo ------------------------------------------------------------

# 1) se añade skill level a tabla ciuo08
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

# se limpia tabla
sinco11_ciuo08 <- sinco11_ciuo08 |>
  select(sinco11, ciuo08) |> 
  # nos quedamos con unitario
  filter(str_length(sinco11) == 4) |>
  # se renombran variables
  rename(
    "s11_unit" = "sinco11",
    "c08_unit" = "ciuo08"
  )

# 2) se une con ciuo08 que tiene skill level
sinco11_ciuo08 <- sinco11_ciuo08 |> 
  left_join(ciuo08, by = "c08_unit")

# 3) se crea tabla sinco11 con valores únicos
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

# se limpia tabla 
sinco19_11 <- sinco19_11 |> 
  select(sinco_19, sinco_11) |> 
  filter(str_length(sinco_19) == 4) |> 
  rename(
    "s19_unit" = "sinco_19",
    "s11_unit" = "sinco_11"
  ) 

# 4) se une sinco19_11 con sinco11
sinco19_11 <- sinco19_11 |> 
  left_join(sinco11, by = "s11_unit")

# 5) se crea tabla sinco19 pivotando a lo largo
sinco19 <- sinco19_11 |> 
  pivot_longer(
    `1`:`4`,
    values_to = "skill_level"
  ) |> 
  select(-(s11_unit:name))

# se pivotea a lo ancho
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

# 6) se añade educación mínima y máxima
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

# Criterio para calcular incidencia ===========================================

## Objetivo -------------------------------------------------------------------
ocupada <- ocupada %>% 
  left_join(sinco19, by = c("p3" = "s19_unit")) %>%  
  mutate(
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

# Limpieza ====================================================================

ocupada <- ocupada %>% 
  mutate(
    # años de estudios
    anios_estudios = as.integer(anios_esc),
    # 99 pasa a NA para evitar 99 - edu_req
    anios_estudios = if_else(anios_estudios != 99, anios_estudios, NA),
    # ponderador trimestral
    fac_tri = as.double(fac_tri),
    # ingreso mensual
    ingreso_mensual = as.double(ingocup),
    # 0 pasa a NA ya que 0==NA en p6b2
    ingreso_mensual = if_else(ingreso_mensual != 0, ingreso_mensual, NA),
    # promedio de ingreso por hora
    ingreso_hora = as.double(ing_x_hrs),
    # 0 pasa a NA ya que 0==NA en p6b2 y en dur9c == 1 & 9
    ingreso_hora = if_else(ingreso_hora != 0, ingreso_hora, NA),
    # resto de variables explicativas
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
    educacion = factor(educacion),
    # regiones
    regiones = factor(regiones)
  )