# Requisitos ==================================================================

## Librerías -------

# manejo de datos
library(tidyverse)
# importación de Excel
library(readxl)
# trabajar encuesta compleja dentro de tidyverse 
library(srvyr)
# regresión a encuesta compleja
library(survey)
# operador entre llaves 
library(rlang)
# ubicación relativa
library(here)
# iterar ejecución de funciones
library(purrr)
# exportación a Excel
library(writexl)

## Archivos ------------------
source(here::here("scripts", "0_funciones_v11.R"))

# Fuente de datos =============================================================

## Egresados (ANUIES, s.f.) ------------------
anuies <- read_csv(here::here("datos", "import", "anuies.csv"))

## PIB (INEGI, s.f.) ------------------
pib <- read_csv(
  here::here("datos", "import", "pib.csv")
)

## microdatos de la ENOE 2T-2025 ------------------
url <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/enoe_2025_trim2_csv.zip"

directorio <- tempdir()

archivo <- tempfile(tmpdir = directorio, fileext = ".zip")

download.file(url = url, destfile = archivo)

archivo <- unzip(
  # ubicación del archivo comprimido
  zipfile = archivo,
  # extrae todos los archivos
  files = NULL,
  # sobreescribe
  overwrite = TRUE,
  # directorio donde se almacenarán los archivos
  exdir = directorio
)

unlink(directorio)

# tabla sociodemográfica
sdem <- read_csv(
  archivo[[4]],
  col_types = cols(.default = col_character())
)

# tabla de ocupación y empleo I
coe1 <- read_csv(
  archivo[[1]],
  col_types = cols(.default = col_character())
)

# tabla de ocupación y empleo II
coe2 <- read_csv(
  archivo[[2]],
  col_types = cols(.default = col_character())
)

## Tablas comparativas ------------------

# tabla comparativa entre SINCO-11 y CIUO-08 (INEGI, 2012)
sinco11_ciuo08 <- read_csv(
  here::here("datos", "import", "sinco11_a_ciuo08.csv"),
  col_types = cols(.default = col_character()),
  na = "NA"
)

# tabla equivalencia entre v11 y v19 de SINCO (INEGI, 2020: 359-394)
sinco19_11 <- read_csv(
  here::here("datos", "import", "sinco19_a_sinco11.csv"),
  col_types = cols(.default = col_character()),
  na = "NA"
)

# tabla de ocupaciones de CIUO-08 (ILO, s/f)
ciuo08 <- read_xlsx(
  here::here("datos", "import", "ISCO.xlsx"),
  sheet = 2,
  col_types = "text"
)

# Construcción ================================================================

## Tablas ---------------------------------------------------------------------

# población total (INEGI, 2023:21)
# pob_total <- sdem |> 
#   filter(
#     r_def == "0",
#     # es igual a (c_res == "1" | c_res == "3")
#     c_res != "2"
#   )

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

# # PEA
# pea <- pob_15ymas |> 
#   filter(clase1 == 1)

# # PNEA (INEGI,2023: 26)
# pnea <- pob_15ymas %>% 
#   filter(clase1 == 2)

# población ocupada INEGI(2023: 21)
ocupada <- pob_15ymas |> 
  filter(clase2 == 1)

# # población desocupada (INEGI,2023: 25)
# desocupada <- pob_15ymas %>% 
#   filter(clase2 == 2)

# PIB vs egresados 
anuies <- anuies |> 
  pivot_wider(
    names_from = TIPO,
    values_from = VALOR
  ) |> 
  separate(
    PERIODO,
    into = c("periodo_inicio", "periodo_fin"),
    sep = "-",
    convert = TRUE
  ) |> 
  calcular_tasa_var(`EGRESADOS TOTAL`, "egresados_var")

pib_anual <- pib |> 
  group_by(Periodo) |> 
  summarise(pib_nivel = sum(Valor)) |> 
  calcular_tasa_var(pib_nivel, "pib_var")

pib_vs_egres <- pib_anual |> 
  left_join(anuies, by = c("Periodo" = "periodo_inicio"))

# Selección de tablas =========================================================

## Población total
# pob_total_proc <- construir_variables_ind(pob_total)
# pob_total_proc <- construir_educacion_cine11(pob_total_proc)
# pob_total_proc <- definir_encuesta(pob_total_proc)

## Población de 15 años y más
pob_15ymas_proc <- construir_variables_ind(pob_15ymas)
pob_15ymas_proc <- construir_educacion_cine11(pob_15ymas_proc)
pob_15ymas_proc <- definir_encuesta(pob_15ymas_proc)

## Total de jóvenes (25-34) egresados de educación superior
# de la población en edad de trabajar
jovenes_tot <- seleccionar_jov(pob_15ymas_proc)

## Enfoque normativo de OIT en la población ocupada
ocupada_normativo <- construir_variables_ind(ocupada)
ocupada_normativo <- construir_variables_ocu(ocupada_normativo)
ocupada_normativo <- asignar_edu_mismatch(ocupada_normativo, ciuo08, sinco11_ciuo08, sinco19_11)
ocupada_normativo <- convertir_a_factor(ocupada_normativo)
# ocupada_normativo_svy <- definir_encuesta(ocupada_normativo)
# ocupada_normativo_svy_reg <- seleccionar_obs_reg(ocupada_normativo_svy)

## Enfoque normativo de OIT en la población objetivo
jovenes_ocu <- seleccionar_jov(ocupada_normativo)
jovenes_ocu <- definir_encuesta(jovenes_ocu)
jovenes_reg <- seleccionar_obs_reg(jovenes_ocu)

## SINCO-19 con educación requerida
sinco19 <- determinar_edu_obj(ciuo08, sinco11_ciuo08, sinco19_11) |> 
  left_join(sinco19_11, by = c("s19_unit" = "sinco_19"), multiple = "any") |>
  mutate(
    desc_obj_min = case_when(
      objetivo_min == 7 ~ "Licenciatura",
      objetivo_min == 6 ~ "Técnico superior",
      objetivo_min == 3 ~ "Secundaria",
      objetivo_min == 1 ~ "Preescolar",
      TRUE ~NA
    ),
    desc_obj_max = case_when(
      objetivo_max == 9 ~ "Doctorado",
      objetivo_max == 6 ~ "Técnico superior",
      objetivo_max == 4 ~ "Preparatoria",
      objetivo_max == 2 ~ "Primaria",
      TRUE ~NA
    )
    # desc_obj_tot = str_c(desc_obj_min, "-", desc_obj_max)
  ) |> 
  select(-contains("11"))

## Población objetivo antes de las manipulaciones realizadas
jovenes_orig <- ocupada |> 
  mutate(
    edad = as.integer(eda),
    educacion = as.integer(cs_p13_1),
    fac_tri = as.double(fac_tri)
  ) |> 
  filter(between(edad, 25, 34) & between(educacion, 5, 9))

jovenes_orig <- definir_encuesta(jovenes_orig)

# PEA
jovenes_pea <- jovenes_tot |> 
  filter(clase1 == 1)

# PNEA
jovenes_pnea <- jovenes_tot |> 
  filter(clase1 == 2)

# eliminar tablas que no se utilizarán
base::remove(
  list = c(
    "anuies",
    "ciuo08",
    "coe1",
    "coe2",
    "completo",
    "ocupada",
    "ocupada_normativo",
    "pib",
    "pib_anual",
    "pob_12ymas",
    "pob_15ymas",
    "pob_15ymas_proc",
    "sdem",
    "sinco11_ciuo08",
    "sinco19_11"
  )
)

# Análisis descriptivo ========================================================

## Entradas -------------------------------------

# tamaño de la muestra
arg_total <- tribble(
  ~datos,
  # ---
  "jovenes_tot",
  "jovenes_orig",
  "jovenes_ocu",
  "jovenes_reg"
)

# proporción por grupos
arg_prop <- tribble(
  ~datos, ~grupo,
  # ---
  "jovenes_tot", "clase1",
  "jovenes_pea", "clase2",
  "jovenes_pnea", "clase2",
  "jovenes_orig", "cs_p13_1", # nivel educativo
  "jovenes_orig", "cs_p15", # estudios previos
  "jovenes_orig", "cs_p16", # situación egreso
  "jovenes_ocu", "ingreso_nivel",
  "jovenes_ocu", "educacion",
  "jovenes_ocu", "edad",
  "jovenes_ocu", "mujer",
  "jovenes_ocu", "casada",
  "jovenes_ocu", "jefe_hogar",
  "jovenes_ocu", "campo_amplio",
  "jovenes_ocu", "regiones",
  "jovenes_ocu", "ent",
  "jovenes_ocu", "division",
  "jovenes_ocu", "pos_ocu",
  "jovenes_ocu", "sector_eco",
  "jovenes_ocu", "tamanio",
  "jovenes_ocu", "informal",
  "jovenes_ocu", "jornada",
  "jovenes_ocu", "unidad_eco",
  "jovenes_ocu", "edu_requerida",
  "jovenes_ocu", "edu_req_aprox",
  "jovenes_reg", "ingreso_nivel",
  "jovenes_reg", "educacion",
  "jovenes_reg", "edad",
  "jovenes_reg", "over_obj",
  "jovenes_reg", "mujer",
  "jovenes_reg", "casada",
  "jovenes_reg", "jefe_hogar",
  "jovenes_reg", "regiones",
  "jovenes_reg", "ent",
  "jovenes_reg", "campo_amplio",
  "jovenes_reg", "division",
  "jovenes_reg", "pos_ocu",
  "jovenes_reg", "sector_eco",
  "jovenes_reg", "tamanio",
  "jovenes_reg", "informal",
  "jovenes_reg", "jornada",
  "jovenes_reg", "unidad_eco"
)

# proporción por subgrupos
arg_prop_sub <- tribble(
  ~datos, ~grupo, ~subgrupo,
  # ---
  "jovenes_orig", "cs_p13_1", "cs_p15", #estudios previos por nivel edu
  "jovenes_ocu", "campo_amplio", "mujer" #género por carrera
)

# tasas general
arg_tasas <- tribble(
  ~datos, ~nombre, ~campo, ~operador, ~valor,
  # ---
  "jovenes_pea", "to", "clase2", "==", 1,
  "jovenes_pea", "td", "clase2", "==", 2,
  "jovenes_tot", "tp", "clase1", "==", 1,
  "jovenes_ocu", "tcco", "tcco", "!=", 0,
  "jovenes_ocu", "til1", "emp_ppal", "==", 1
)


# estadísticos descriptivos
arg_est_des <- tribble(
  ~datos, ~variable,
  "jovenes_ocu", "ingreso_mensual",
  "jovenes_ocu", "anios_estudios",
  "jovenes_ocu", "exper",
  "jovenes_ocu", "horas_lab",
  "jovenes_ocu", "edad",
  "jovenes_reg", "ingreso_mensual",
  "jovenes_reg", "anios_estudios",
  "jovenes_reg", "exper",
  "jovenes_reg", "horas_lab",
  "jovenes_reg", "edad"
)

## Tablas ---------------------------------------

# tamaño de la muestra
descriptivo_total <- pmap(arg_total, calcular_total_map) |> 
  list_rbind()

# proporciones por grupos
descriptivo_prop <- pmap(arg_prop, calcular_prop_map) |> 
  list_rbind()

# proporciones por subgrupos
descriptivo_prop_sub <- pmap(arg_prop_sub, calcular_prop_sub_map) |> 
  list_rbind()

# tasas complementarias de ocupación y desocupación general
descriptivo_tasas <- pmap(arg_tasas, calcular_tasas_ocu_map) |> 
  list_rbind()

# estadísticos descriptivos
descriptivo_est <- pmap(arg_est_des, calcular_descriptivos_map) |> 
  list_rbind()

# divisiones del SINCO-19 por educación requerida aproximada
descriptivo_s19_1 <- sinco19 |> 
  count(edu_req_aprox) |> 
  mutate(prop = n / sum(n))

descriptivo_s19_2 <-  sinco19 |>
  count(s19_div, edu_req_aprox) |>
  group_by(s19_div) |>
  mutate(prop = n / sum(n))

# Incidencia ==================================================================

## Entrada --------------------------------------
argumentos_inc_tot <- tribble(
  ~datos, ~grupo,
  # ---
  "jovenes_ocu", "edu_mismatch_obj"
)

argumentos_inc_sub <- tribble(
  ~datos, ~grupo, ~subgrupo,
  # ---
  "jovenes_ocu", "ingreso_nivel", "edu_mismatch_obj",
  "jovenes_ocu", "educacion", "edu_mismatch_obj",
  "jovenes_ocu", "mujer", "edu_mismatch_obj",
  "jovenes_ocu", "edad", "edu_mismatch_obj",
  "jovenes_ocu", "casada", "edu_mismatch_obj",
  "jovenes_ocu", "jefe_hogar", "edu_mismatch_obj",
  "jovenes_ocu", "regiones", "edu_mismatch_obj",
  "jovenes_ocu", "ent", "edu_mismatch_obj",
  "jovenes_ocu", "campo_amplio", "edu_mismatch_obj",
  "jovenes_ocu", "campo_espec", "edu_mismatch_obj",
  "jovenes_ocu", "edu_mismatch_obj", "division",
  "jovenes_ocu", "pos_ocu", "edu_mismatch_obj",
  "jovenes_ocu", "sector_eco", "edu_mismatch_obj",
  "jovenes_ocu", "tamanio", "edu_mismatch_obj",
  "jovenes_ocu", "informal", "edu_mismatch_obj",
  "jovenes_ocu", "jornada", "edu_mismatch_obj",
  "jovenes_ocu", "unidad_eco", "edu_mismatch_obj",
  "jovenes_ocu", "ingreso_nivel", "edu_mismatch_obj"
)

## Tablas ---------------------------------------

incidencia_tot <- pmap(argumentos_inc_tot, calcular_prop_map) |> 
  list_rbind()

incidencia_sub <- pmap(argumentos_inc_sub, calcular_prop_sub_map) |> 
  list_rbind()

### Por género y carreras
incidencia_car_muj <- jovenes_ocu |> 
  calcular_prop_sub_var(campo_amplio, mujer, edu_mismatch_obj)

### Por regiones y carreras
incidencia_reg_car <- jovenes_ocu |> 
  calcular_prop_sub_var(regiones, campo_amplio, edu_mismatch_obj)

## Ingreso promedio
incidencia_ingreso_mean <- jovenes_ocu |> 
  group_by(edu_mismatch_obj) |> 
  summarise(
    n = n(),
    tot = survey_total(vartype = NULL),
    promedio = survey_mean(ingreso_mensual, vartype = c("se", "cv", "ci"), na.rm = TRUE)
  )

## Análisis de sensibilidad
incidencia_sensibilidad <- jovenes_ocu |> 
  filter(
    edu_req_aprox == "0" |
    edu_req_aprox == "1" |
    edu_req_aprox == "2" |
    is.na(edu_req_aprox)
  ) |> 
  calcular_prop(edu_mismatch_obj)

# Nivel de precisión estadística ==============================================

descriptivo_prop <- validar_precision(descriptivo_prop, tot_cv)
descriptivo_prop_sub <- validar_precision(descriptivo_prop_sub, tot_cv)
descriptivo_est <- validar_precision(descriptivo_est, media_cv)
incidencia_tot <- validar_precision(incidencia_tot, tot_cv)
incidencia_sub <- validar_precision(incidencia_sub, tot_cv)
incidencia_car_muj <- validar_precision(incidencia_car_muj, tot_cv)
incidencia_reg_car <- validar_precision(incidencia_reg_car, tot_cv)
incidencia_ingreso_mean <- validar_precision(incidencia_ingreso_mean, promedio_cv)
incidencia_sensibilidad <- validar_precision(incidencia_sensibilidad, tot_cv)

# Guardar =====================================================================

## Guardar --------------------------------------

# tablas completas para regresión
write_rds(jovenes_reg, here::here("datos", "export", "jovenes_reg.rds"))
# tablas procesadas para realizar gráficas
write_rds(pib_vs_egres, here::here("datos", "export", "pib_vs_egres.rds"))
write_rds(descriptivo_prop, here::here("datos", "export", "descriptivo_prop.rds"))
write_rds(descriptivo_s19_2, here::here("datos", "export", "descriptivo_sinco19.rds"))
write_rds(incidencia_sub, here::here("datos", "export", "incidencia_sub.rds"))

## Exportar -------------------------------------

# tabla de población objetivo
jovenes_ocu <- as_tibble(jovenes_ocu)
write_csv(jovenes_ocu, here::here("datos", "export", "jovenes_ocu.csv"))

# datos procesados
hojas_nombre <- list(
  "descriptivo_total" = descriptivo_total,
  "descriptivo_prop" = descriptivo_prop,
  "descriptivo_prop_sub" = descriptivo_prop_sub,
  "descriptivo_tasas" = descriptivo_tasas,
  "descriptivo_est" = descriptivo_est,
  "descriptivo_s19_1" = descriptivo_s19_1,
  "descriptivo_s19_2" = descriptivo_s19_2,
  "incidencia_tot" = incidencia_tot,
  "incidencia_sub" = incidencia_sub,
  "incidencia_car_muj" = incidencia_car_muj,
  "incidencia_reg_car" = incidencia_reg_car,
  "incidencia_ingreso_mean" = incidencia_ingreso_mean,
  "incidencia_sensibilidad" = incidencia_sensibilidad,
  "sinco19" = sinco19
)

write_xlsx(hojas_nombre, here::here("datos", "export", "datos_procesados.xlsx"))
