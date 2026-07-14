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

# Egresados 

# seleccionan solamente egresados
egresados <- anuies |> 
  filter(TIPO == "EGRESADOS TOTAL")

# se separa ciclo escolar
egresados <- egresados |> 
  separate(
    PERIODO,
    into = c("periodo1", "periodo2"),
    sep = "-",
    convert = TRUE
  )

# se renombran columnas
egresados <-  egresados |> 
  select(periodo1, VALOR) |> 
  rename(
    "periodo" = "periodo1",
    "nivel" = "VALOR"
  )

# se calcula tasa de variación anual
egresados <- egresados |> 
  mutate(variacion = nivel / lag(nivel) - 1)

# PIB

# se calcula PIB anual
pib_anual <- pib |> 
  group_by(Periodo) |> 
  summarise(nivel = sum(Valor))

# se calcula tasa de variación anual
pib_anual <- pib_anual |> 
  mutate(variacion = nivel / lag(nivel) - 1)

# se selecciona periodo de interés
pib_anual <- pib_anual |> 
  filter(between(Periodo, 2000, 2024))

# se unen las tablas egresados y pib
pib_vs_egres <- full_join(egresados, pib_anual, by = c("periodo" = "Periodo")) |> 
  rename(
    "nivel_egres" = "nivel.x",
    "nivel_pib" = "nivel.y",
    "variacion_egres" = "variacion.x",
    "variacion_pib" = "variacion.y"
  )