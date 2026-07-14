# REQUISITOS ==================================================================

library(tidyverse)
library(scales)
library(ggrepel)
# mapas geográficos
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
# ubicación relativa
library(here)

source(here::here("scripts", "0_funciones_v11.R"))

# Introducción
pib_vs_egres <- read_rds(here::here("datos", "export", "pib_vs_egres.rds"))

# Análisis descriptivo
prop <- read_rds(here::here("datos", "export", "descriptivo_prop.rds"))
sinco19 <- read_rds(here::here("datos", "export", "descriptivo_sinco19.rds"))

# Incidencia
incidencia_sub <- read_rds(here::here("datos", "export", "incidencia_sub.rds"))

# elementos comunes 
otros_elementos <- list(labs(x = NULL,y = NULL))

# INTRODUCCIÓN ================================================================

## PIB vs egresados -----------------------------
etiquetas_var <- c(
  "pib_var" = "PIB",
  "egresados_var" = "Egresados"
)

fig_pib_vs_egres <- pib_vs_egres |> 
  filter(between(Periodo, 2000, 2024)) |> 
  pivot_longer(
    cols = c(pib_var, egresados_var),
    names_to = "variable",
    values_to = "nivel"
  ) |> 
  ggplot(aes(
    x = Periodo, y = nivel, 
    color = variable, shape = variable, linetype = variable)
  ) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    labels = scales::label_percent(),
    breaks = breaks_width(0.05)
  ) +
  scale_color_brewer(palette = "Set1", name = NULL, labels = etiquetas_var) + 
  scale_shape_discrete(name = NULL, labels = etiquetas_var) + 
  scale_linetype_discrete(name = NULL, labels = etiquetas_var) + 
  theme_classic(base_size = 16, base_family = "Times New Roman") + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  ) +
  otros_elementos

# ANÁLISIS DESCRIPTIVO ========================================================

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
  filter(poblacion == "jovenes_ocu" & grupo == "tamanio") |> 
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
  # theme_classic(base_size = 12, base_family = "Times New Roman") + 
  theme(text = element_text(family = "Times New Roman", size = 12)) + 
  # convertir a gráfica de pastel
  coord_radial(
    # construye a partir de valores del eje y
    theta = "y", 
    # permite que se agrupe
    expand = FALSE
  ) + 
  labs(x = NULL, y = NULL)

## Educación requerida --------------------------

etiquetas_var1 <- c(
  "1"	 = 	"1 Funcionarios, directores y jefes",
  "2"	 = 	"2 Profesionistas y técnicos",
  "3"	 = 	"3 Trabajadores auxiliares en actividades administrativas",
  "4"	 = 	"4 Comerciantes, empleados en ventas y agentes de ventas",
  "5"	 = 	"5 Trabajadores en servicios personales y de vigilancia",
  "6"	 = 	"6 Trabajadores en actividades agrícolas, ganaderas, forestales, caza y pesca",
  "7"	 = 	"7 Trabajadores artesanales, en la construcción y otros oficios",
  "8"	 = 	"8 Operadores de maquinaria industrial, ensambladores, choferes y conductores de transporte",
  "9"	 = 	"9 Trabajadores en actividades elementales y de apoyo"
)

etiquetas_var2 <- c(
  "0" = "Menor a básica",
  "1" = "Básica a \nmedia superior",
  "2" = "Superior",
  "3" = "Otros",
  "NA" = "No especificado"
)

fig_descriptivo_sinco19 <- sinco19 |> 
  mutate(
    prop_label = if_else(prop > 0.03, percent(prop, accuracy = 1), ""),
  ) |> 
  ggplot(aes(s19_div, prop, fill = edu_req_aprox, group = edu_req_aprox)) +
  geom_col() + 
  geom_text(aes(label = prop_label), position = position_stack(0.5)) + 
  scale_x_discrete(labels = label_wrap(30)(etiquetas_var1)) +
  scale_y_continuous(breaks = NULL, expand = 0) +
  # scale_fill_discrete(palette = "Paired", labels = etiquetas_var2) + 
  scale_fill_brewer(
    palette = "Paired", 
    direction = -1, 
    name = "Rango \neducativo",
    labels = etiquetas_var2,
    # relleno para valores nulos 
    na.value = "grey50"
  ) + 
  theme_classic(base_size = 12, base_family = "Times New Roman") + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  ) + 
  labs(x = NULL, y = NULL)

## Proporción por división ----------------------

etiquetas_var1 <- c(
  "1"	 = 	"1 Funcionarios, directores y jefes",
  "2"	 = 	"2 Profesionistas y técnicos",
  "3"	 = 	"3 Trabajadores auxiliares en actividades administrativas",
  "4"	 = 	"4 Comerciantes, empleados en ventas y agentes de ventas",
  "5"	 = 	"5 Trabajadores en servicios personales y de vigilancia",
  "6"	 = 	"6 Trabajadores en actividades agrícolas, ganaderas, forestales, caza y pesca",
  "7"	 = 	"7 Trabajadores artesanales, en la construcción y otros oficios",
  "8"	 = 	"8 Operadores de maquinaria industrial, ensambladores, choferes y conductores de transporte",
  "9"	 = 	"9 Trabajadores en actividades elementales y de apoyo"
  # "99" = "No especificado"
)

fig_descriptivo_division <- prop |> 
  filter(poblacion == "jovenes_ocu" & grupo == "division" & valor != "99") |> 
  mutate(
    valor_label = factor(valor, labels = etiquetas_var1),
    prop_label = percent(prop, accuracy = 1)
  ) |> 
  ggplot(aes(fct_reorder(valor_label, -prop), prop)) +
  geom_col(fill = "#314763") + 
  geom_text(aes(y = prop + 0.03, label = prop_label)) + 
  scale_x_discrete(labels = label_wrap(30)) +
  scale_y_continuous(
    breaks = NULL,
    expand = expansion(mult = c(0, 0.1))
  ) + 
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(x = NULL, y = NULL)


# INCIDENCIA ==================================================================

## Educación ------------------------------------
etiquetas_var <- c(
  "6" = "Técnico \nsuperior",
  "7" = "Licenciatura",
  "8" = "Maestría",
  "9" = "Doctorado"
)

fig_incidencia_edu <- incidencia_sub |> 
  filter(gpo_lab == "educacion" & sub_val == "1") |> 
  mutate(
    prop_label = scales::percent(prop, accuracy = 1)
  ) |> 
  ggplot(aes(gpo_val, prop)) + 
  geom_col(fill = "#314763") + 
  geom_text(aes(y = prop + 0.02, label = prop_label)) +
  scale_x_discrete(labels = etiquetas_var) +
  scale_y_continuous(
    labels = NULL, 
    breaks = NULL, 
    # 0 espacio hacia abajo y 0.1 espacio hacia arriba
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank()
  ) + 
  otros_elementos

## Carrera educativa ----------------------------
fig_incidencia_carreras <- incidencia_sub |> 
  filter(gpo_lab == "campo_amplio" & sub_val == "1" & gpo_val != "99") |> 
  mutate(
    prop_label = scales::percent(prop, accuracy = 1),
    campo_amplio = factor(gpo_val),
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
    prop_label = scales::percent(prop, accuracy = 1) 
  ) |>  
  ggplot(aes(fct_reorder(campo_amplio, prop), prop)) + 
  geom_col(fill = "#314763") + 
  geom_text(aes(y = prop + 0.05, label = prop_label)) +
  scale_x_discrete(labels = scales::label_wrap(width = 30)) + 
  scale_y_continuous(
    labels = NULL, 
    breaks = NULL,
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    # eliminar línea de los ejes
    axis.line = element_blank(),
    # eliminar marcas en cada valor del eje
    axis.ticks = element_blank()
  ) +
  otros_elementos + 
  coord_flip()

## Estados --------------------------------------
# mapa de méxico
mexico <- ne_states(
  country = "Mexico",
  returnclass = "sf"
)

# id de cada estado
estados <- tribble(
  ~estados,~id,
  "Aguascalientes",1,
  "Baja California",2,
  "Baja California Sur",3,
  "Campeche",4,
  "Coahuila",5,
  "Colima",6,
  "Chiapas",7,
  "Chihuahua",8,
  "Distrito Federal",9,
  "Durango",10,
  "Guanajuato",11,
  "Guerrero",12,
  "Hidalgo",13,
  "Jalisco",14,
  "México",15,
  "Michoacán",16,
  "Morelos",17,
  "Nayarit",18,
  "Nuevo León",19,
  "Oaxaca",20,
  "Puebla",21,
  "Querétaro",22,
  "Quintana Roo",23,
  "San Luis Potosí",24,
  "Sinaloa",25,
  "Sonora",26,
  "Tabasco",27,
  "Tamaulipas",28,
  "Tlaxcala",29,
  "Veracruz",30,
  "Yucatán",31,
  "Zacatecas",32
)

estados <- estados %>% 
  mutate(id = as.character(id))

# unión con tabla de resultados
df_estados <- incidencia_sub |> 
  filter(gpo_lab == "ent" & sub_val == "1") |> 
  left_join(estados, by = c("gpo_val" = "id"))

# unión con el mapa de méxico
mexico_over <- mexico |> 
  left_join(df_estados, by = c("name" = "estados"))

# gráfica
fig_incidencia_estados <- mexico_over %>% 
  ggplot() +
  geom_sf(aes(fill = prop)) + 
  scale_fill_viridis_c(
    # invierte la intensidad del color
    direction = -1,
    # escoger un tema de paleta de color
    option = "G",
    # elimina el título de la leyenda
    name = NULL,
    # convierte a porcentaje
    labels = scales::percent_format(accuracy = 1)
  ) +
  # elimina las coordenadas
  coord_sf(datum = NA) + 
  theme_classic(base_size = 12, base_family = "Times New Roman")

# GUARDAR =====================================================================

# Introducción
guardar_figura(fig_pib_vs_egres, here::here("figuras", "fig_pib_vs_egres.png"))

# Análisis descriptivo
guardar_figura(fig_descriptivo_tamanio, here::here("figuras", "fig_descriptivo_tamanio.png"))
guardar_figura(
  fig_descriptivo_sinco19,
  here::here("figuras", "fig_descriptivo_sinco19.png")
)
guardar_figura(
  fig_descriptivo_division, 
  here::here("figuras", "fig_descriptivo_division.png")
)

# Incidencia
guardar_figura(fig_incidencia_edu, here::here("figuras", "fig_incidencia_edu.png"))
guardar_figura(fig_incidencia_carreras, here::here("figuras", "fig_incidencia_carreras.png"))
guardar_figura(fig_incidencia_estados, here::here("figuras", "fig_incidencia_estados.png"))


