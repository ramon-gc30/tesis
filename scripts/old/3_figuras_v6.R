# REQUISITOS ==================================================================

library(tidyverse)
library(scales)

source("C:/Prog/tesis/scripts/0_funciones_v9.R")

# ANÁLISIS DESCRIPTIVO ========================================================

## Tablas --------------------------------------
total <- read_rds("datos/descriptivo_total.rds")
prop <- read_rds("datos/descriptivo_prop.rds")
prop_sub <- read_rds("datos/descriptivo_prop_sub.rds")
tasas <- read_rds("datos/descriptivo_tasas.rds")
tasas_sub <- read_rds("datos/descriptivo_tasas_sub.rds")
sinco19 <- read_rds("datos/descriptivo_sinco19.rds")

## Género ---------------------------------------
etiquetas_var1 <- c(
  "0" = "Hombre",
  "1" = "Mujer"
)

etiquetas_var2 <- c(
  "1" = "PEA",
  "2" = "PNEA"
)

fig_descriptivo_mujer <- prop_sub |> 
  filter(gpo_lab == "mujer" & poblacion == "jovenes_tot") |> 
  mutate(prop_label = scales::percent(prop)) |> 
  ggplot(aes(gpo_val, prop, group = sub_val, fill = sub_val)) + 
  geom_col() + 
  geom_text(
    aes(label = prop_label), 
    # poner etiqueta en medio de cada grupo 
    position = position_stack(vjust = 0.5)
  ) + 
  scale_x_discrete(
    labels = etiquetas_var1, 
    # distancia entre el eje y con el geom
    expand = 0
  ) + 
  scale_y_continuous(
    labels = NULL,
    breaks = NULL,
    expand = 0
  ) +
  scale_fill_brewer(
    palette = "Paired", 
    # invierte la paleta 
    direction = -1, 
    name = NULL, 
    labels = etiquetas_var2
  ) +
  theme_classic(base_size = 12, base_family = "Trebuchet MS") + 
  theme(
    # eliminar la línea de los ejes
    axis.line = element_blank(), 
    # eliminar la marca de cada etiqueta del eje x
    axis.ticks.x = element_blank()
  ) + 
  labs(x = NULL, y = NULL)

guardar_figura(fig_descriptivo_mujer, "figuras/fig_descriptivo_mujer.png")

## Educación ------------------------------------
etiquetas_var1 <- c(
  "6" = "Técnico \nsuperior",
  "7" = "Licenciatura",
  "8" = "Maestría",
  "9" = "Doctorado"
)

fig_descriptivo_edu <- tasas_sub |> 
  filter(grupo == "educacion" & nombre == "til1") |> 
  mutate(tasa_label = scales::percent(tasa, accuracy = 1)) |> 
  ggplot(aes(subgrupo, tasa)) + 
  geom_point(color = "#314763", size = 2.5) + 
  geom_text(aes(y = tasa + 0.005, label = tasa_label)) + 
  scale_x_discrete(labels = etiquetas_var1) + 
  scale_y_continuous(breaks = NULL) + 
  theme_classic(base_size = 12, base_family = "Trebuchet MS") + 
  labs(x = NULL, y = NULL)

guardar_figura(fig_descriptivo_edu, "figuras/fig_descriptivo_edu.png")

## Carreras educativas --------------------------
fig_descriptivo_carreras <- tasas_sub |> 
  filter(grupo == "campo_amplio" & nombre == "td" & subgrupo != "99") |> 
  mutate(
    campo_amplio = factor(subgrupo),
    campo_amplio = fct_recode(
      campo_amplio,
      "Educación" = "01",
      "Artes y humanidades" = "02",
      "Ciencias sociales y derecho" = "03",
      "Administración y negocios" = "04",
      "Ciencias naturales, matemáticas y estadística" = "05",
      "Tecnologías de la información y la comunicación" = "06",
      "Ingeniería, manufactura y construcción" = "07",
      "Agronomía y veterinaria" = "08",
      "Ciencias de la salud" = "09",
      "Servicios" = "10"
    ),
    # etiqueta en forma de porcentaje
    tasa_label = scales::percent(tasa, accuracy = 0.1) 
  ) |> 
  ggplot(aes(fct_reorder(campo_amplio, tasa), tasa)) + 
  geom_col(fill = "#314763") + 
  geom_text(aes(y = tasa - 0.004, label = tasa_label), color = "white") + 
  scale_x_discrete(labels = label_wrap(30)) +
  scale_y_continuous(breaks = NULL, expand = 0) +
  theme_classic(base_size = 12, base_family = "Trebuchet MS") +
  theme(axis.line = element_blank()) +
  labs(x = NULL, y = NULL) + 
  coord_flip()

guardar_figura(fig_descriptivo_carreras, "figuras/fig_descriptivo_carreras.png")

## Estados --------------------------------------
etiquetas_var1 <- c(
  "1" = "AGS",
  "2" = "BC",
  "3" = "BCS",
  "4" = "CAMP",
  "5" = "COAH",
  "6" = "COL",
  "7" = "CHIS",
  "8" = "CHIH",
  "9" = "CDMX",
  "10" = "DGO",
  "11" = "GTO",
  "12" = "GRO",
  "13" = "HGO",
  "14" = "JAL",
  "15" = "MÉX",
  "16" = "MICH",
  "17" = "MOR",
  "18" = "NAY",
  "19" = "NL",
  "20" = "OAX",
  "21" = "PUE",
  "22" = "QRO",
  "23" = "QROO",
  "24" = "SLP",
  "25" = "SIN",
  "26" = "SON",
  "27" = "TAB",
  "28" = "TAMPS",
  "29" = "TLAX",
  "30" = "VER",
  "31" = "YUC",
  "32" = "ZAC"
)

fig_descriptivo_ent <- tasas_sub |> 
  filter(grupo == "ent" & nombre == "til1") |> 
  mutate(subgrupo = as.integer(subgrupo)) |> 
  arrange(subgrupo) |> 
  mutate(
    ent = factor(subgrupo, labels = etiquetas_var1),
    tasa_label = if_else(
      tasa == min(tasa) | tasa == max(tasa), 
      percent(tasa, accuracy = 1), 
      ""
    )
  ) |> 
  relocate(ent, .after = subgrupo) |> 
  ggplot(aes(fct_reorder(ent, -tasa), tasa)) + 
  geom_col(fill = "#314763") + 
  geom_text(
    aes(y = tasa - 0.025, label = tasa_label), 
    color = "white",
    angle = 90
  ) + 
  scale_y_continuous(
    breaks = breaks_width(0.1),
    labels = label_percent(),
    expand = 0
  ) +
  theme_classic(base_size = 12, base_family = "Trebuchet MS") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) + 
  labs(x = NULL, y = NULL)

guardar_figura(fig_descriptivo_ent, "figuras/fig_descriptivo_ent.png")

## Sector económico -----------------------------
etiquetas_var1 <- c(
  "1" = "Primario",
  "2" = "Secundario",
  "3" = "Terciario",
  "4" = "No especificado"
)

fig_descriptivo_sec_eco <- prop |> 
  filter(grupo == "sector_eco") |> 
  mutate(
    prop_label = if_else(prop > 0.01, percent(prop, accuracy = 1), "")
  ) |> 
  ggplot(aes(grupo, prop, group = valor, fill = valor)) + 
  geom_col() + 
  geom_text(
    aes(label = prop_label), 
    position = position_stack(vjust = 0.5)
  ) + 
  scale_fill_brewer(
    palette = "Set1", 
    direction = -1,
    name = NULL,
    labels = etiquetas_var1
  ) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  # theme_classic(base_size = 12, base_family = "Trebuchet MS") + 
  theme(text = element_text(family = "Trebuchet MS", size = 12)) + 
  # convertir a gráfica de pastel
  coord_radial(
    # construye a partir de valores del eje y
    theta = "y", 
    # permite que se agrupe
    expand = FALSE
  ) + 
  labs(x = NULL, y = NULL)

guardar_figura(fig_descriptivo_sec_eco, "figuras/fig_descriptivo_sec_eco.png")

## Tamaño ---------------------------------------
etiquetas_var1 <- c(
  "1" = "Micronegocios",
  "4" = "Pequeños establecimientos",
  "5" = "Medianos establecimientos",
  "6" = "Grandes establecimientos",
  "7" = "Gobierno",
  "8" = "Otros"
)

fig_descriptivo_tamanio <- prop |> 
  filter(grupo == "tamanio") |> 
  mutate(
    prop_label = percent(prop, accuracy = 1),
  ) |> 
  ggplot(aes(grupo, prop, group = valor, fill = valor)) + 
  geom_col() + 
  geom_text(
    aes(label = prop_label), 
    position = position_stack(vjust = 0.5)
  ) + 
  scale_fill_brewer(
    palette = "Paired", 
    name = NULL,
    labels = etiquetas_var1
  ) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  # theme_classic(base_size = 12, base_family = "Trebuchet MS") + 
  theme(text = element_text(family = "Trebuchet MS", size = 12)) + 
  # convertir a gráfica de pastel
  coord_radial(
    # construye a partir de valores del eje y
    theta = "y", 
    # permite que se agrupe
    expand = FALSE
  ) + 
  labs(x = NULL, y = NULL)

guardar_figura(fig_descriptivo_tamanio, "figuras/fig_descriptivo_tamanio.png")

## Educación requerida --------------------------

etiquetas_var1 <- c(
  "0" = "Menor a básica",
  "1" = "Básica a Media superior",
  "2" = "Superior",
  "3" = "Otros",
  "NA" = "No especificado"
)

fig_descriptivo_edu_req_aprox <- sinco19 |> 
  count(edu_req_aprox) |> 
  mutate(
    prop = n / sum(n),
    prop_label = percent(prop, accuracy = 1)
  ) |> 
  ggplot(aes("", prop, group = edu_req_aprox, fill = edu_req_aprox)) + 
  geom_col() + 
  geom_text(aes(label = prop_label), position = position_stack(vjust = 0.5)) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) + 
  scale_fill_brewer(
    palette = "Paired", 
    direction = -1,
    name = "Educación requerida",
    labels = etiquetas_var1
  ) +
  theme(text = element_text(family = "Trebuchet MS", size = 12)) +
  labs(x = NULL, y = NULL) + 
  coord_radial(theta = "y", expand = FALSE)

guardar_figura(
  fig_descriptivo_edu_req_aprox,
  "figuras/fig_descriptivo_edu_req_aprox.png"
)

## Desajuste ------------------------------------
etiquetas_var1 <- c(
  "6" = "Técnico \nsuperior",
  "7" = "Licenciatura",
  "8" = "Maestría",
  "9" = "Doctorado"
)

etiquetas_var2 <- c(
  "0" = "Menor a básica",
  "1" = "Básica a Media superior",
  "2" = "Superior",
  "3" = "Otros",
  "4" = "No especificado"
)

fig_descriptivo_desajuste <- prop_sub |> 
  filter(sub_lab == "edu_requerida") |> 
  establecer_edu_req_aprox(sub_val) |> 
  group_by(gpo_val, edu_req_aprox) |> 
  summarise(
    n = sum(n),
    tot = sum(tot)
  ) |> 
  mutate(
    prop = tot / sum(tot),
    prop_label = if_else(prop > 0.03, percent(prop, accuracy = 1), "")
  ) |> 
  ungroup() |> 
  ggplot(aes(gpo_val, prop, group = edu_req_aprox, fill = edu_req_aprox)) + 
  geom_col() + 
  geom_text(aes(label = prop_label), position = position_stack(0.5)) +
  scale_fill_brewer(
    palette = "Set1",
    direction = -1,
    name = "Educación requerida",
    labels = etiquetas_var2
  ) +
  scale_x_discrete(labels = etiquetas_var1) + 
  scale_y_continuous(breaks = NULL, expand = 0) +
  theme_classic(base_size = 12, base_family = "Trebuchet MS") + 
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank()
  ) + 
  labs(x = NULL, y = NULL)

guardar_figura(fig_descriptivo_desajuste, "figuras/fig_descriptivo_desajuste.png")

