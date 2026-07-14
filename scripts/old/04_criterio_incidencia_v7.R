# Criterio para calcular incidencia ===========================================

## Objetivo -------------------------------------------------------------------
ocupada <- ocupada %>% 
  left_join(sinco19, by = c("p3" = "s19_unit")) %>%  
  mutate(
    objetivo_min = as.integer(objetivo_min),
    objetivo_max = as.integer(objetivo_max),
    edu_mismatch_obj = case_when(
      # required
      between(educacion, objetivo_min, objetivo_max) ~ 0,
      # over
      educacion > objetivo_max ~ 1,
      # under
      educacion < objetivo_min ~ 2,
      TRUE ~ NA
    ),
    # variables binarias
    over_obj = if_else(educacion > objetivo_max, 1, 0),
    required_obj = if_else(between(educacion, objetivo_min, objetivo_max), 1, 0),
    under_obj = if_else(educacion < objetivo_min, 1, 0)
    # variables de años de sobre o infra
    # objetivo: no se puede obtener ya que la educación requerida está en intervalos
  )