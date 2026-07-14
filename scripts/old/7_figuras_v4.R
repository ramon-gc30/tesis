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

# POR CARRERAS Y NIVEL EDUCATIVO ------------------------------------

labels_vars <- c(
  "1" = "Educación",
  "2" = "Artes y humanidades",
  "3" = "Ciencias sociales y derecho",
  "4" = "Administración y negocios",
  "5" = "Ciencias naturales, matemáticas y estadística",
  "6" = "Tecnologías de la información y la comunicación",
  "7" = "Ingeniería, manufactura y construcción",
  "8" = "Agronomía y veterinaria",
  "9" = "Ciencias de la salud",
  "10" = "Servicios"
)

labels_vars2 <- c(
  "6" = "Técnico \nsuperior",
  "7" = "Licenciatura",
  "8" = "Maestría",
  "9" = "Doctorado"
)

fig_inc_carreras_edu <- tbl_inc_carreras_educacion |>
  filter(edu_mismatch_obj == 1 & campo_amplio != 99) |>
  mutate(campo_amplio = as.integer(campo_amplio)) |>
  ggplot(
    aes(
      x = campo_amplio, y = prop, 
      color = educacion, shape = educacion, linetype = educacion
    )
  ) +
  geom_point(size = 2.5) +
  geom_line() +
  scale_x_continuous(
    labels = label_wrap(20)(labels_vars),
    breaks = seq(1, 10, by = 1)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(),
    breaks = breaks_width(0.1)
  ) +
  scale_color_brewer(palette = "Set1", name = NULL, labels = labels_vars2) + 
  scale_shape_discrete(name = NULL, labels = labels_vars2) + 
  scale_linetype_discrete(name = NULL, labels = labels_vars2) +
  theme_classic(base_size = 12, base_family = "Trebuchet MS") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) + 
  otros_elementos

# OVER Y TIL1 POR ESTADOS
labels_vars <- c(
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

fig_estados_til1_over <- estados_til1_over |> 
  mutate(ent = as.integer(ent)) |> 
  arrange(ent) |> 
  ggplot(aes(tasa, prop)) + 
  geom_point(color = "#314763") + 
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label = labels_vars)) +
  scale_x_continuous(
    breaks = breaks_width(0.05),
    labels = scales::label_percent()
  ) + 
  scale_y_continuous(
    breaks = breaks_width(0.05),
    labels = scales::label_percent()
  ) +
  theme_classic(base_size = 12, base_family = "Trebuchet MS") +
  labs(
    x = "Tasa de informalidad laboral 1 (TIL1)",
    y = "Tasa de sobrecalificación"
  )

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

# guardar_figura(fig_inc_carreras_edu, "figuras/fig_inc_carreras_edu.png")

# guardar_figura(fig_estados_til1_over, "figuras/fig_estados_til1_over.png")