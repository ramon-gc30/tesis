library(tidyverse)
library(readr)
library(RColorBrewer)
library(ggthemes)
library(ggrepel)
# library(systemfonts)

# Importación
anuies <- read_csv("datos/anuies.csv")

pib <- read_csv(
  "datos/pib.csv"
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

# Egresados vs PIB

# se unen las tablas egresados y pib
pib_vs_egres <- full_join(egresados, pib_anual, by = c("periodo" = "Periodo")) |> 
  rename(
    "nivel_egres" = "nivel.x",
    "nivel_pib" = "nivel.y",
    "variacion_egres" = "variacion.x",
    "variacion_pib" = "variacion.y"
  )

# tasa de variación promedio 
calcular_tcp <- function(datos){
  datos_procesados <- datos |> 
    summarise(
      vf = last(nivel),
      vi = first(nivel),
      n = n(),
      tcp = (vf / vi)**(1/n) - 1
    )
  return(datos_procesados)  
}

# tasa de variación
calcular_tvar <- function(datos){
  datos_procesados <- datos |> 
    mutate(variacion = nivel / lag(nivel) - 1)
  return(datos_procesados)
}

# Gráfica
# pivotar para graficar
datos_pivot <-  pib_vs_egres |> 
  pivot_longer(
    cols = c(variacion_egres, variacion_pib),
    names_to = "variable",
    values_to = "nivel"
  )

# leyendas para la gráfica
labels_vars <- c(
  "variacion_pib" = "PIB",
  "variacion_egres" = "Egresados"
  )

# gráfica
fig_pib_vs_egres <- datos_pivot %>%
  ggplot(aes(
    x = periodo, y = nivel, 
    # color, tipo de punto y de línea según variable
    color = variable, shape = variable, linetype = variable)
  ) + 
  geom_point() + 
  geom_line() + 
  # eje x
  scale_x_continuous(
    # marcas (valores) permitidos
    breaks = seq(2000, 2024, by = 1)
  ) +
  # eje y
  scale_y_continuous(
    # etiquetas
    labels = scales::label_percent(),
    # marcas (valores) 
    breaks = seq(-0.1, 0.2, by = 0.05)
  ) +
  # leyenda y paleta de color
  scale_color_brewer(palette = "Set1", name = NULL, labels = labels_vars) + 
  scale_shape_discrete(name = NULL, labels = labels_vars) + 
  scale_linetype_discrete(name = NULL, labels = labels_vars) + 
  # estilo de fuente
  theme_classic(base_size = 16, base_family = "Trebuchet MS") + 
  theme(
    # orientación de las marcas del eje x
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    # mostrar la leyenda abajo de la gráfica
    legend.position = "bottom",
    # tamaño de letra de la leyenda
    legend.text = element_text(size = 14)
  ) +
  labs(
    # no muestra la variable asociada al eje
    x = NULL,
    y = NULL
  )

# guardar gráfica
# ggsave(
#   "pib_vs_egresados2.png",
#   # tamaño completo en Word
#   width = 17,
#   height = 10,
#   units = "cm"
# )
