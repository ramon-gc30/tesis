# Figuras =====================================================================

## PIB vs Egresados -----------------------------------------------------------

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
guardar_fig_pib_vs_egres <- ggsave(
  "pib_vs_egres.png",
  # tamaño completo en Word
  width = 17,
  height = 10,
  units = "cm"
)