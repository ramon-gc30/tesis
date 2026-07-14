# TABLAS DE CIUO-08 y SINCO ===================================================

# IMPORTACIÓN

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

# LIMPIEZA

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