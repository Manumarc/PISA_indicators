## PISA_indicators

Funciones que construyen los indicadores de aprendizaje y recursos físicos de CIMA

## Librerías necesarias 

```
library(foreign)    # Importar y exportar bases de spss
library(openxlsx)   # Facilita la lectura de archivos de Excel (".xls" y ".xlsx")
library(fst)        # Abre archivos ".fst, que son extremadamente rápidos para guardar y cargar grandes conjuntos de datos
library(tidyverse)  # Un conjunto de paquetes interrelacionados diseñados para análisis de datos
library(survey)     # Permite analizar datos provenientes de muestras complejas
library(scales)     # Permite editar tonos, formatos de número y texto
library(intsvy)     # Permite analizar los datos de PISA y otras International Large Scale Assessments
library(furrr)      # Permite realizar análisis paralelizado
```

## Cargar las funciones

```
source(url("https://raw.githack.com/Manumarc/PISA_indicators/main/pisa_indicator.R"))

```
## Preparación de datos

Luego de descargar las bases de datos de PISA (https://www.oecd.org/en/about/programmes/pisa/pisa-data.html), es necesario preparar las bases para calcular los indicadores. Los siguientes scripts contienen la ruta de preparación:

- 00_convertir_datos_a_fst.qmd: las bases se transforman a formato .fst para reducir su tamaño y poder abrirlas rápidamente.
- 01_integrar_bases.qmd: se integran las bases cuestionarios de estudiantes y escuela.
- 02_pegado_escs_depuracion.qmd: se realiza el pegado de la variable ecsc_trend (tendencia comparable de 2012 en adelante) y se procede a depurar las variables.
- 03_indicadores_v1.qmd: se procede a calcular los indicadores de aprendizaje y recursos físicos.

## Calcular resultados de Medida promedio y Niveles de logros a nivel nacional y por estratos

```
# Forma de cálculo individual #
#-----------------------------#

 calcular_pisa(
        data = pisa06,
        anio = 2006,
        compet = "Read",             # Puede tomar los valores "Read" o "Math" o "Scie"
        calculo = "MP",              # Puede tomar los valores "MP" o "Niveles"
        niv_estrat = "Nacional"      # Puede tomar los valores "Nacional" o "Estratos"
      )
```

Si se quiere calcular los resultados de varios años al mismo tiempo, se puede utilizar el siguiente código. Las funciones a nivel nacional y por estratos consideran promedio LAC y promedio OECD para todas las áreas.

```
#==============================================================================#
# Preparación de bases de datos 2000-2022
#==============================================================================#

lista_bases <- list(pisa00, pisa03, pisa06, pisa09, pisa12, pisa15, pisa18, pisa22) %>% 
  set_names(c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022)) 

vars_fijas <- c("CNT", "COUNTRY", "CNTSCHID", "CNTSTUID", "ESCS",
                "sex", "lengua", "quintil_escs", "financia", "area", "gestion")

lista_bases_v2 <- lista_bases %>%
  map(~ .x %>% select(any_of(vars_fijas),
                      matches("^PV\\d{1,2}(READ|MATH|SCIE)$"),
                      matches("^W_")))

#==============================================================================#
# Definición de años y competencias evaluadas
#==============================================================================#

compet_por_anio <- list(
  "2000" = "Read",
  "2003" = c("Read", "Math"),
  "2006" = c("Read", "Math", "Scie"),
  "2009" = c("Read", "Math", "Scie"),
  "2012" = c("Read", "Math", "Scie"),
  "2015" = c("Read", "Math", "Scie"),
  "2018" = c("Read", "Math", "Scie"),
  "2022" = c("Read", "Math", "Scie")
)

#==============================================================================#
# Paralelización
#==============================================================================#

options(future.globals.maxSize = 3*1024^3)  

plan(multisession, workers = 2)

resultados <- future_imap(
  lista_bases_v2,
  function(base, anio_chr) {
    
    calcular_seguro <- possibly(calcular_pisa, otherwise = NULL)
    
    anio <- as.integer(anio_chr)
    
    grid <- expand_grid(
      compet  = compet_por_anio[[anio_chr]],
      calculo = c("MP", "Niveles"),
      niv_estrat = c("Nacional","Estratos")
    )
    
    pmap(
      grid, function(compet, calculo, niv_estrat) {
        
        res <- calcular_seguro(
          data = base, anio = anio,
          compet = compet, 
          calculo = calculo, 
          niv_estrat = niv_estrat
        )
        
        if (is.null(res)) return(NULL)
        
        res %>% mutate(
          calculo = .env$calculo, 
          niv_estrat = .env$niv_estrat
        )
      }
    ) %>%
      bind_rows()
  },
  .options = furrr_options(
    seed      = TRUE,
    packages  = c("intsvy", "dplyr", "tidyr", "purrr", "stringr", "tibble"),
    scheduling = Inf     # reparto dinámico: el worker que termina agarra la siguiente base
  )
) %>%
  bind_rows()

plan(sequential)
gc()

```

## Calcular resultados de recursos físicos a nivel nacional y por estratos

```

# Forma de cálculo individual #
#-----------------------------#

calcular_refis(
  pisa12, 
  anio = 2012,  
  niv_estrat = "Estratos"          # Puede tomar los valores "Nacional" o "Estratos"
)

```
Si se quiere calcular los resultados de varios años al mismo tiempo, se puede utilizar el siguiente código
 
```
#==============================================================================#
# Bases para refis (2009-2022)
#==============================================================================#

lista_bases_rf <- list(pisa09, pisa12, pisa15, pisa18, pisa22) %>%
  set_names(c(2009, 2012, 2015, 2018, 2022))

#==============================================================================#
# Paralelización
#==============================================================================#

options(future.globals.maxSize = 3 * 1024^3)
plan(multisession, workers = 2)

resultados_rf <- future_imap(
  lista_bases_rf,
  function(base, anio_chr) {

    calcular_seguro <- possibly(calcular_refis, otherwise = NULL)
    anio <- as.integer(anio_chr)

    map(
      c("Nacional", "Estratos"),
      function(niv_estrat) {
        res <- calcular_seguro(
          bd_datos   = base,
          anio       = anio,
          niv_estrat = niv_estrat,
          verbose    = FALSE        
        )
        if (is.null(res)) return(NULL)
        res 
      }
    ) %>%
      bind_rows()
  },
  .options = furrr_options(
    seed       = TRUE,
    packages   = c("intsvy", "dplyr", "tidyr", "purrr", "stringr", "tibble"),
    scheduling = Inf     
  )
) %>%
  bind_rows()

plan(sequential)
gc()
```
## Dar formato de SCL data

A la base de datos resultante de PISA se le aplica la función "pisa_to_scldata" y se obtiene el formato de base de datos de SCL data

```
# Dar formato de scl data #
#-------------------------#

pisa_to_scldata(
  bd_datos,
  tipo           # Puede tomar los valores "Aprendizaje" y "Recursos"
)
```

