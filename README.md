# Tesis
Juan Ramón Gregorio Carrillo
domingo, 7 de junio de 2026

Repositorio referente a la tesis titulada:

**“La pérdida salarial de la sobrecalificación en jóvenes egresados de
educación superior de México, 2025”**

Este proyecto se realizó, en primera instancia, de forma local y
posteriormente, después de concluir la versión más reciente del mismo,
se decidió integrarlo con Git/GitHub. Aquello con el propósito de
propiciar la transparencia y la reproducibilidad de los resultados
presentados en esta investigación. A continuación, se describen los
principales elementos de este repositorio.

En primer lugar, la estructura general del repositorio es el siguiente:

    .
    ├── bitacora
    ├── datos
    ├── docs
    ├── figuras
    ├── plantillas
    ├── README_v3-1.docx
    ├── README_v3-1.md
    ├── README_v3-1.qmd
    ├── README_v3-1.rmarkdown
    ├── renv
    ├── renv.lock
    ├── scripts
    └── tesis.Rproj

Para ejecutar el proyecto por vez primera, se sugiere seguir los
siguientes pasos:

1.  Abrir `tesis.Rproj`[^1]

2.  Ejecutar en la consola la siguiente instrucción

```` markdown
```{r}
# install.packages("renv")
library(renv)
renv::restore()
```
````

De este modo, se restaurará el entorno computacional empleado durante el
desarrollo de esta investigación, incluyendo las versiones específicas
de los paquetes utilizados. En ese sentido, puede utilizarse la función
`renv::diagnostics()` para verificar el estado de dicho entorno
computacional.

```` markdown
```{r}
renv::diagnostics()
```
````

Por su parte, el proyecto fue desarrollado siguiendo un flujo de trabajo
reproducible. Así, el código generado se encuentra dividido en los
siguientes *scripts* :

    C:/Prog/tesis/scripts
    ├── 0_funciones_v11.R
    ├── 1_preparacion_datos_v8.R
    ├── 2_figuras_v9.R
    ├── 3_regresion_v1.R
    ├── b_metodo_correspondencias_v1.R
    └── old

La <a href="#tbl-scripts" class="quarto-xref">Tabla 1</a> describe la
función de cada *script*:

<div id="tbl-scripts">

Tabla 1: Descripción de los scripts contenidos en el código fuente

| Script | Descripción |
|----|----|
| `0_funciones_v11.R` | Funciones que se utilizan en los scripts restantes. |
| `1_preparacion_datos_v8.R` | Importación, manipulación y exportación de datos. |
| `2_figuras_v9.R` | Creación de figuras en R. |
| `3_regresion_v1.R` | Resultados de las estimaciones mostradas en la sección 4.3 |
| `b_metodo_correspondencias_v1.R` | Determinación de la educación (formal) requerida en las 490 ocupaciones unitarias del SINCO-19, a través del método de correspondencias. |

</div>

*Fuente*. Elaboración propia

Estos *scripts* requieren los datos ubicados en la carpeta
`datos/import`:

    C:/Prog/tesis/datos/import
    ├── anuies.csv
    ├── ISCO.xlsx
    ├── pib.csv
    ├── sinco11_a_ciuo08.csv
    └── sinco19_a_sinco11.csv

Cabe mencionar que los microdatos de la ENOE correspondientes al segundo
trimestre de 2025 se descargaron directamente del sitio web del
INEGI[^2].

La ejecución de los *scripts* genera automáticamente los archivos
contenidos en la carpeta `datos/export`, los cuales constituyen los
insumos para la elaboración de cuadros, figuras y estimaciones
econométricas.

    C:/Prog/tesis/datos/export
    ├── datos_procesados.xlsx
    ├── descriptivo_prop.rds
    ├── descriptivo_sinco19.rds
    ├── incidencia_sub.rds
    ├── jovenes_ocu.csv
    ├── jovenes_reg.rds
    ├── pib_vs_egres.rds
    └── sinco19_metodo_correspondencias.xlsx

Por un lado, las figuras mostradas en el documento escrito se encuentran
en la carpeta del mismo nombre.

    C:/Prog/tesis/figuras
    ├── fig_descriptivo_division.png
    ├── fig_descriptivo_sinco19.png
    ├── fig_descriptivo_tamanio.png
    ├── fig_incidencia_carreras.png
    ├── fig_incidencia_edu.png
    ├── fig_incidencia_estados.png
    ├── fig_pib_vs_egres.png
    └── old

Mientras que, los resultados de las estimaciones econométricas se
encuentran en la carpeta `docs`.

    C:/Prog/tesis/docs
    ├── c4-3_resultados_estimacion.md
    └── c4-3_resultados_estimacion.qmd

La <a href="#tbl-archivos" class="quarto-xref">Tabla 2</a> describe el
contenido de los archivos generados:

<div id="tbl-archivos">

Tabla 2: Descripción de los archivos generados

| Archivo | Tipo | Descripción |
|----|----|----|
| `datos_procesados.xlsx` | XSLX | Datos procesados que posibilitan la creación de cuadros y figuras. |
| `descriptivo_prop.rds`<br>`descriptivo_sinco19.rds`<br>`incidencia_sub.rds`<br>`pib_vs_egres.rds` | RDS | Datos procesados que permiten la realización de figuras en R. |
| `jovenes_ocu.csv` | CSV | Microdatos de la población objetivo: jóvenes ocupados egresados (25-34 años) de educación superior. |
| `jovenes_reg.rds` | RDS | Submuestra de la población objetivo utilizada para las estimaciones en R. |
| `sinco19_metodo_correspondencias.xslx` | XSLX | Rango educativo (formal) compatible con las 490 ocupaciones unitarias del SINCO-19, determinado a través del método de correspondencias. |
| `c4-3_resultados_estimacion.md` | Markdown | Resultados de las estimaciones mostradas en la sección 4.3 |

</div>

*Fuente*. Elaboración propia

En términos generales, el flujo de trabajo del presente proyecto
computacional puede resumirse como sigue:

    datos/import -> script_1 -> datos/export -> scripts_restantes -> figuras | datos/export

Se observa que cada *script* realiza una tarea específica y genera
archivos intermedios que posteriormente son utilizados por los
siguientes *scripts*. Esta organización busca facilitar la revisión del
código, evitar la duplicación de procesos, así como favorecer la
transparencia y reproducibilidad de los resultados obtenidos.

Por tanto, las personas interesadas en reproducir los resultados de la
presente investigación deben ejecutar los *scripts* descritos líneas
arriba. Mientras que, aquellos únicamente interesados en verificar los
resultados reportados pueden consultar el archivo
`datos_procesados.xlsx`, que contiene los datos utilizados para la
construcción de cuadros y figuras presentados en esta investigación.

Cualquier duda, sugerencia, y/o reporte de errores pueden contactarse al
siguiente correo: <ramon_gc@outlook.com>.

[^1]: Para lo cual debe estar instalado RStudio en la computadora donde
    se ejecutará el código. Ingrese a la liga que se muestra a
    continuación para descargar dicho software:
    <https://posit.co/products/open-source/rstudio>.

[^2]: Consulte <https://www.inegi.org.mx/programas/enoe/15ymas/>
