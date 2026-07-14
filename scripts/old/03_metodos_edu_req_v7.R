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