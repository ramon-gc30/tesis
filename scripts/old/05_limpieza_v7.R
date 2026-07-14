# Limpieza ====================================================================

ocupada <- ocupada %>% 
  mutate(
    # años de estudios
    anios_estudios = as.integer(anios_esc),
    # 99 pasa a NA para evitar 99 - edu_req
    anios_estudios = if_else(anios_estudios != 99, anios_estudios, NA),
    # ponderador trimestral
    fac_tri = as.double(fac_tri),
    # ingreso mensual
    ingreso_mensual = as.double(ingocup),
    # 0 pasa a NA ya que 0==NA en p6b2
    ingreso_mensual = if_else(ingreso_mensual != 0, ingreso_mensual, NA),
    # promedio de ingreso por hora
    ingreso_hora = as.double(ing_x_hrs),
    # 0 pasa a NA ya que 0==NA en p6b2 y en dur9c == 1 & 9
    ingreso_hora = if_else(ingreso_hora != 0, ingreso_hora, NA),
    # resto de variables explicativas
    # horas trabajadas
    # 999 indica no específicado, se convierte a NA INEGI(2024:81)
    horas_lab = if_else(p5b_thrs != 999, p5b_thrs, NA),
    horas_lab = as.double(horas_lab),
    # experiencia laboral potencial
    # Zamudio e Islas(1999) y Mincer(1974)
    exper = edad - anios_estudios - 6,
    # si exper <= 0, se convierte a NA; no afecta a jóvenes
    # si edad == 98, se convierte a NA
    exper = if_else(exper > 0 & edad != 98, exper, NA),
    mujer = if_else(sex == 2, 1, 0),
    # estado civil == 1 si es casada
    casada = if_else(e_con == 5, 1, 0),
    # situación de hogar == 1 si es jefe INEGI(2020:37)
    jefe_hogar = if_else(par_c == 101, 1, 0),
    ocupaciones = factor(ocupacion1),
    # valor No sabe pasa a NA
    carreras = if_else(carrera1 != "99", carrera1, NA),
    carreras = factor(carreras),
    # posición en la ocupacion
    # valor no especificado pasa a NA
    pos_ocu = if_else(pos_ocu != 5, pos_ocu, NA),
    pos_ocu = factor(pos_ocu),
    # sector económico agregado
    # valor no especificado pasa a NA
    sector_eco = if_else(rama_est1 != 4, rama_est1, NA),
    sector_eco = factor(sector_eco),
    # núm de trabajadores
    # valor no espcificado pasa a NA
    # trabajadores = if_else(emple7c != 7, emple7c, NA),
    # trabajadores = factor(trabajadores),
    # tamaño de unidad económica
    tamanio = case_when(
      ambito2 == 2 | ambito2 == 3 ~ "1",
      # valor no especificado pasa a NA
      ambito2 == 0 | ambito2 == 8 ~ NA,
      TRUE ~ ambito2
    ),
    tamanio = factor(tamanio),
    informal = if_else(emp_ppal == 1, "1", "0"),
    # nivel educativo
    educacion = factor(educacion),
    # regiones
    regiones = factor(regiones)
  )