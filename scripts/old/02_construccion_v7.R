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