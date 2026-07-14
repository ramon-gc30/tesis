# FIGURAS =====================================================================

# archivo de entrada
source("scripts/6_cuadros_v3.R")

# función para guardar gráfica
guardar_figura <- function(grafica, nombre_archivo){
  ggsave(
    filename = nombre_archivo,
    plot = grafica,
    width = 17,
    height = 10,
    units = "cm",
    dpi = 300 # asegura alta calidad
  )
  
  # mensaje de confirmación
  message(paste("Gráfica guardada exitosamente como:", nombre_archivo))
}

# elementos comunes 
otros_elementos <- list(labs(x = NULL,y = NULL))

# PIB vs Egresados --------------------------------------------------
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
    color = variable, shape = variable, linetype = variable)
  ) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    labels = scales::label_percent(),
    breaks = breaks_width(0.05)
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
  otros_elementos

# POR EDAD ----------------------------------------------------------
fig_inc_edad <- tbl_inc_edad |> 
  filter(edu_mismatch_obj == 1) |> 
  mutate(prop_label = scales::percent(prop)) |>
  ggplot(aes(x = edad, y = prop)) +
  geom_point(color = "#314763", size = 3) + 
  geom_line(color = "#314763") + 
  geom_text(aes(y = prop + 0.01, label = prop_label)) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  theme_classic(base_size = 12, base_family = "Trebuchet MS") + 
  otros_elementos

# POR NIVEL EDUCATIVO -----------------------------------------------
labels_vars <- c(
  "6" = "Técnico \nsuperior",
  "7" = "Licenciatura",
  "8" = "Maestría",
  "9" = "Doctorado"
)

fig_inc_educacion <- tbl_inc_educacion |> 
  filter(edu_mismatch_obj == 1) %>% 
  mutate(
    edu_mismatch_obj = factor(edu_mismatch_obj),
    prop_label = scales::percent(prop)
  ) %>% 
  ggplot(aes(educacion, prop)) + 
  geom_col(fill = "#314763") + 
  geom_text(aes(y = prop + 0.05, label = prop_label)) +
  scale_x_discrete(labels = labels_vars) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  theme_classic(base_size = 12, base_family = "Trebuchet MS") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  otros_elementos

# POR CARRERA EDUCATIVA ---------------------------------------------
fig_inc_carreras <- tbl_inc_carreras |> 
  filter(edu_mismatch_obj == 1 & campo_amplio != 99) %>% 
  mutate(
    campo_amplio = factor(campo_amplio),
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
    prop_label = scales::percent(prop) 
  ) %>% 
  ggplot(aes(fct_reorder(campo_amplio, prop), prop)) + 
  geom_col(fill = "#314763") + 
  geom_text(aes(y = prop + 0.05, label = prop_label)) +
  scale_x_discrete(labels = scales::label_wrap(width = 30)) + 
  scale_y_continuous(labels = NULL, breaks = NULL) +
  theme_classic(base_size = 12, base_family = "Trebuchet MS") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  otros_elementos + 
  coord_flip()


# POR ENTIDADES FEDERATIVAS -----------------------------------------
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
df_estados <- tbl_inc_estados |> 
  filter(edu_mismatch_obj == 1) |> 
  left_join(estados, by = c("ent" = "id"))

# unión con el mapa de méxico
mexico_over <- mexico |> 
  left_join(df_estados, by = c("name" = "estados"))

# gráfica
fig_inc_estados <- mexico_over %>% 
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
  theme_classic(base_size = 12, base_family = "Trebuchet MS")

# Guardar -----------------------------------------------------------

# guardar_figura(grafica = fig_pib_vs_egres, 
# nombre_archivo = "figuras/fig_pib_vs_egres_v3.png")

# guardar_figura(grafica = fig_inc_edad, 
#                nombre_archivo = "figuras/fig_inc_edad_v3.png")

# guardar_figura(grafica = fig_inc_educacion, 
#                nombre_archivo = "figuras/fig_inc_educacion_v3.png")

# guardar_figura(grafica = fig_inc_carreras, 
#                nombre_archivo = "figuras/fig_inc_carreras.png")

# guardar_figura(grafica = fig_inc_estados, 
#                nombre_archivo = "figuras/fig_inc_estados_v3.png")