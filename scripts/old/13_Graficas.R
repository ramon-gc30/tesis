# Gráficas ====================================================================
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(ggrepel)

# PIB vs Egresados ============================================================
datos <- read_csv(
  "PIBvsEgresados.csv",
  col_types = cols(
    # arroja error
    .default = col_double()
  )
)

datos <- datos %>% 
  mutate(
    prop_pib = PIB / lag(PIB) - 1,
    prop_matr = MATRICULA / lag(MATRICULA) - 1,
    prop_ingreso = NUEVO_INGRESO / lag(NUEVO_INGRESO) - 1,
    prop_egresados = EGRESADOS / lag(EGRESADOS) - 1,
    prop_tit = TITULADOS / lag(TITULADOS) - 1
  )

# pivotar para graficar
datos_pivot <- datos %>%
  pivot_longer(
    cols = c(prop_pib, prop_egresados),
    names_to = "variables",
    values_to = "valor"
  )

labels_vars <- c(
  "prop_pib" = "PIB",
  "prop_egresados" = "Egresados"
  )

datos_pivot %>%
  ggplot(aes(x = ANIO, y = valor, color = variables, shape = variables, linetype = variables)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(
    breaks = seq(2010, 2024, by = 1)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(),
    breaks = seq(-0.1, 0.2, by = 0.05)
  ) +
  scale_color_brewer(palette = "Set1", name = NULL, labels = labels_vars) + 
  scale_shape_discrete(name = NULL, labels = labels_vars) + 
  scale_linetype_discrete(name = NULL, labels = labels_vars) + 
  theme_classic(base_size = 16, base_family = "Trebuchet MS") + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  ) +
  labs(
    x = NULL,
    y = NULL
  )

# library(systemfonts)
# df <- systemfonts::system_fonts()
# df2 <- as_tibble(unique(df$family))
# print(df2, n = Inf)
# 
# df %>% 
#   filter(name == "Spectral")
# 
# systemfonts::match_fonts("Trebuchet MS")

# tamaño completo en Word
ggsave(
  "pib_vs_egresados.png",
  width = 17,
  height = 10,
  units = "cm"
)

# Tasa desempleo por países ===================================================
grafica2 <- tribble(
  ~pais,~promedio,
  "México",0.145,
  "Francia",0.127,
  "España",0.125,
  "América Latina",0.114,
  "Japón",0.105,
  "Reino Unido",0.103,
  "Europa",0.1,
  "Alemania",0.094,
  "Chile",0.09
)

grafica2 %>% 
  mutate(
    pais = factor(pais),
    pais = fct_reorder(pais, -promedio)
  ) %>% 
  ggplot(aes(pais, promedio)) + 
  geom_point(size = 4) + 
  scale_x_discrete(labels = scales::label_wrap(width = 10)) + 
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  # scale_color_brewer(
  #   palette = "Set1", 
  #   name = NULL, 
  #   breaks = NULL, 
  #   labels = NULL
  # ) + 
  theme_classic(base_size = 16, base_family = "Trebuchet MS") + 
  theme(
    axis.text.x = element_text(
      angle = 90, 
      vjust = 0.5
      )
  ) +
  labs(
    x = NULL,
    y = NULL
  )

# tamaño completo en Word
ggsave(
  "tasa_desempleo_paises.png",
  width = 17,
  height = 10,
  units = "cm"
)

# Sobrecalificación por carreras, UVM ========================================
df <- tribble(
  ~carrera,~si,~parcialmente,~no,
  "educación",0.67,0.1,0.23
)

df <- df %>% 
  pivot_longer(
    cols = c("si", "parcialmente", "no")
  )

df

df %>% 
ggplot(aes(carrera, value, fill = name)) + 
  geom_col() + 
  geom_label_repel(aes(label = value, fill = name))

# df_lab <- 
  df %>% 
  group_by(carrera) %>% 
  mutate(pos = cumsum(value) - value/2)   # centro de cada segmento

df_lab <- df %>% 
  group_by(carrera) %>% 
  mutate(pos = cumsum(value) - value/2)   # centro de cada segmento

# texto en cada categoría de gráfica apilada
df_lab %>% 
  ggplot(aes(carrera, value, fill = name)) + 
  geom_col() +
  geom_text(
    aes(y = pos, label = value, fill = name),
    # fill = "white",
    size = 3
  )

# por encima de las columnas
df %>% 
  ggplot(aes(name, value)) +
  geom_col() + 
  geom_text(
    aes(
      y = value + 0.02,
      label = scales::percent(value)
      )
  )
