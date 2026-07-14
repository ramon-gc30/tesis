# CUADROS =====================================================================

# archivos requeridos ----------------------------------------------- 
source("scripts/1_importacion_v12.R")
source("scripts/2_construccion_tablas_v14.R")
source("scripts/3_construccion_variables_v4.R")
source("scripts/4_metodologia_v1.R")
source("scripts/5_calcular_incidencia_v4.R")

# Selección de población objetivo ----------------------------------- 
# Limpieza y construcción de variables
general <- construir_variables(ocupada)

# Aproximación empírica del enfoque normativo de la OIT 
# basado en correspondencias
general <- asignar_edu_mismatch(general, ciuo08)

# Crea la tabla de la población objetivo
jovenes <- seleccionar_jov(general)

# se pasan a factores
jovenes <- convertir_a_factor(jovenes)

# se define como encuesta compleja
datos_svy <- definir_encuesta(jovenes)

# Variables continuas -----------------------------------------------
# se omiten los valores faltantes
tbl_est_desc_ing <- obtener_est_descriptivos(datos_svy, ingreso_mensual) #|> 
tbl_est_desc_anios_est <- obtener_est_descriptivos(datos_svy, anios_estudios)
tbl_est_desc_exper <- obtener_est_descriptivos(datos_svy, exper)
tbl_est_desc_hrs_lab <- obtener_est_descriptivos(datos_svy, horas_lab)

# Variables discretas -----------------------------------------------
tbl_est_desc_educacion <- obtener_tot_svy(datos_svy, educacion)
tbl_est_desc_genero <- obtener_tot_svy(datos_svy, mujer)
tbl_est_desc_edo_civil <- obtener_tot_svy(datos_svy, casada)
tbl_est_desc_hog <- obtener_tot_svy(datos_svy, jefe_hogar)
tbl_est_desc_carreras <- obtener_tot_svy(datos_svy, campo_amplio)
tbl_est_desc_regiones <- obtener_tot_svy(datos_svy, regiones)
tbl_est_desc_div <- obtener_tot_svy(datos_svy, division)
tbl_est_desc_pos_ocu <- obtener_tot_svy(datos_svy, pos_ocu)
tbl_est_desc_sector_eco <- obtener_tot_svy(datos_svy, sector_eco)
tbl_est_desc_tamanio <- obtener_tot_svy(datos_svy, tamanio)
tbl_est_desc_informal <- obtener_tot_svy(datos_svy, informal)
tbl_est_desc_jornada <- obtener_tot_svy(datos_svy, jornada)
tbl_est_desc_unidad_eco <- obtener_tot_svy(datos_svy, unidad_eco)

# Incidencia --------------------------------------------------------
tbl_inc_tot <- calcular_incidencia_svy_tot(datos_svy, edu_mismatch_obj)
tbl_inc_ingreso <- calcular_inc_svy_des(datos_svy, ing7c, edu_mismatch_obj)
tbl_inc_edad <- calcular_inc_svy_des(datos_svy, edad, edu_mismatch_obj)
tbl_inc_genero <- calcular_inc_svy_des(datos_svy, mujer, edu_mismatch_obj)
tbl_inc_educacion <- calcular_inc_svy_des(datos_svy, educacion, edu_mismatch_obj)
tbl_inc_genero <- calcular_inc_svy_des(datos_svy, mujer, edu_mismatch_obj)
tbl_inc_edo_civil <- calcular_inc_svy_des(datos_svy, casada, edu_mismatch_obj)
tbl_inc_carreras <- calcular_inc_svy_des(datos_svy, campo_amplio, edu_mismatch_obj)
tbl_inc_regiones <- calcular_inc_svy_des(datos_svy, regiones, edu_mismatch_obj)
tbl_inc_estados <- calcular_inc_svy_des(datos_svy, ent, edu_mismatch_obj)
tbl_inc_div <- calcular_inc_svy_grp(datos_svy, edu_mismatch_obj, division)
tbl_inc_pos_ocu <- calcular_inc_svy_des(datos_svy, pos_ocu, edu_mismatch_obj)
tbl_inc_sector_eco <- calcular_inc_svy_des(datos_svy, sector_eco, edu_mismatch_obj)
tbl_inc_tamanio <- calcular_inc_svy_des(datos_svy, tamanio, edu_mismatch_obj)
tbl_inc_informal <- calcular_inc_svy_des(datos_svy, informal, edu_mismatch_obj)
tbl_inc_jornada <- calcular_inc_svy_des(datos_svy, jornada, edu_mismatch_obj)
tbl_inc_unidad_eco <- calcular_inc_svy_des(datos_svy, unidad_eco, edu_mismatch_obj)
