# Código completo con funciones V12 ============================================

source("scripts/26_importacion_v11.R")
source("scripts/26_construccion_tablas_v13.R")
source("scripts/26_construccion_variables_v2.R")
source("scripts/26_metodologia_v1.R")
source("scripts/26_cuadros_v1.R")
source("scripts/26_figuras_v1.R")

# Limpieza y construcción de variables
general <- construir_variables(ocupada)

# Aproximación empírica del enfoque normativo de la OIT 
# basado en correspondencias
general <- asignar_edu_mismatch(general, ciuo08)

# Crea la tabla de la población objetivo
jovenes <- seleccionar_jov(general)

# se pasan a factores
jovenes <- convertir_a_factor(jovenes)
