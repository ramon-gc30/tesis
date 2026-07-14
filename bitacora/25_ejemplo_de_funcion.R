# Librerías requeridas ========================================================

# manejo de datos
library(tidyverse)
# importación de Excel
library(readxl)

# Fuente de datos =============================================================

# PIB vs egresados (INEGI; ANUIES)
pib_vs_egres <- read_csv(
  "datos/PIBvsEgresados.csv",
  col_types = cols(.default = col_double())
)

# tabla sociodemográfica
sdem <- read_csv(
  "datos/ENOE_SDEMT225.csv",
  col_types = cols(.default = col_character())
)

# tabla de ocupación y empleo I
coe1 <- read_csv(
  "datos/ENOE_COE1T225.csv",
  col_types = cols(.default = col_character())
)

# tabla de ocupación y empleo II
coe2 <- read_csv(
  "datos/ENOE_COE2T225.csv",
  col_types = cols(.default = col_character())
)

# tabla comparativa entre SINCO-11 y CIUO-08 (INEGI, 2012)
sinco11_ciuo08 <- read_csv(
  "datos/sinco11_a_ciuo08.csv",
  col_types = cols(.default = col_character()),
  na = "NA"
)

# tabla equivalencia entre v11 y v19 de SINCO (INEGI, 2020: 359-394)
sinco19_11 <- read_csv(
  "datos/sinco19_a_sinco11.csv",
  col_types = cols(.default = col_character()),
  na = "NA"
)

# tabla de ocupaciones de CIUO-08 (ILO, s/f)
ciuo08 <- read_xlsx(
  "datos/ISCO.xlsx",
  sheet = 2,
  col_types = "text"
)

# Construcción ================================================================

## Tablas ---------------------------------------------------------------------

# población de 12 y más para unir sdem y coe INEGI(2023:9)
pob_12ymas <- sdem |> 
  mutate(edad = as.integer(eda)) |> 
  filter(
    r_def == "0",
    c_res != "2",
    between(edad, 12, 98)
  )

# unión de sdem y coe INEGI(2023:9)
completo <- pob_12ymas |>
  full_join(coe1, by = c("tipo", "mes_cal", "cd_a", "ent", "con", "v_sel", "n_hog", "h_mud", "n_ren")) |>
  full_join(coe2, by = c("tipo", "mes_cal", "cd_a", "ent", "con", "v_sel", "n_hog", "h_mud", "n_ren"))

# población de 15 años y más INEGI(2023:21)
# para seleccionar población ocupada INEGI(2023:21)
pob_15ymas <- completo |> 
  filter(
    r_def.x == "0",
    c_res != "2",
    between(edad, 15, 98)
  )

# población ocupada INEGI(2023: 21)
ocupada <- pob_15ymas |> 
  filter(clase2 == 1)

# población total (INEGI, 2023:21)
pob_total <- sdem |> 
  filter(
    r_def == "0",
    # es igual a (c_res == "1" | c_res == "3")
    c_res != "2"
  )

# población desocupada (INEGI,2023: 25)
desocupada <- pob_15ymas %>% 
  filter(clase2 == 2)

# PNEA (INEGI,2023: 26)
pnea <- pob_15ymas %>% 
  filter(clase1 == 2)

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

# nivel educativo y años de escolaridad
ocupada <- ocupada |> 
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
    # si reporta estudireos previos de secundaria en carrera técnica
    # se le asigna estudios de preparatoria (LGE, Art 47; LGES, Art 3)
    educacion = if_else(educacion == 6 & edu_previa == 3, 4, educacion)
  )


# función ejemplo

# nivel de estudios en términos de CINE-11 (UNESCO, 2013: 22) 
asignar_educacion_cine11 <- function(datos){
  datos_procesados <- datos |>
    mutate(
      educacion_cine11 = case_when(
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

ocupada_procesada <- asignar_educacion_cine11(ocupada)

ocupada_procesada |> 
  filter(cs_p16 == "2") |> 
  select(cs_p16, contains("educacion"))

# Sí funciona!!!