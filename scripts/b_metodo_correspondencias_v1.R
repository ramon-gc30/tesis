# Requisitos ==================================================================

## Librerías -------

# manejo de datos
library(tidyverse)
# importación de Excel
library(readxl)
# ubicación relativa
library(here)
# exportación a Excel
library(writexl)

## Archivos ------------------

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

## Funciones ------------------------------------

# Establecer educación requerida aproximada 
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

# Determinar educación requerida
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
      edu_requerida = str_c(objetivo_min, "-", objetivo_max),
      s19_div = str_sub(s19_unit, 1, 1)
    ) |> 
    relocate(s19_div, .before = s19_unit)
  
  # se añade educación requerida aprox
  sinco19 <- establecer_edu_req_aprox(sinco19, edu_requerida)
  return(sinco19)
}

# Procedimiento ===============================================================
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
    ),
    descripcion_sinco19 = str_remove(descripcion_sinco19, "- ") 
  ) |> 
  select(-contains("11"))

# Guardar =====================================================================
write_xlsx(
  list("sinco19" = sinco19),
  here::here("datos", "export", "sinco19_metodo_correspondencias.xlsx")
)
