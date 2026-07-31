# PISA_indicators

Funciones que construyen los indicadores de aprendizaje y recursos físicos de CIMA

# Uso de las funciones

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
