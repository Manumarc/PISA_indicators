#========================================================================#
#========================================================================#
# Función MP + niv. logro + nacional y estratos
#========================================================================#
#========================================================================#

calcular_pisa <- function(data, anio, compet, calculo, niv_estrat) {
  
  #======================================================================#
  # Puntos de corte niveles de logro 
  #======================================================================#
  
  # Descripción: por debajo del nivel 2, entre nivel 2 y nivel 5, nivel 5 en adelante
  
  # Competencia Lectora #
  #---------------------#
  cortelec <- c(407.47, 625.61)
  
  # Competencia Matemática #
  #------------------------#
  
  cortemat <- c(420.07,606.99)
  
  # Competencia Científica #
  #------------------------#
  
  cortecie <- c(409.54,633.33)
  
  if (compet %in% "Read"){
    pcorte <- cortelec
  } else if (compet %in% "Math"){
    pcorte <- cortemat
  } else if (compet %in% "Scie") {
    pcorte <- cortecie
  } else {
    stop("No hay puntos de corte definidos para la competencia seleccionada")
  }
  
  #========================================================================================================#
  # Países considerados para promedios OECD y LAC
  #========================================================================================================#
  
  # PISA 2000 #
  #-----------#
  
  oecd_00 <- c(
    "AUSTRALIA", "AUSTRIA", "BELGIUM", "CANADA", "CZECH REPUBLIC", "DENMARK", "FINLAND", "FRANCE", "GERMANY", "GREECE", "HUNGARY", "ICELAND", "IRELAND", "ITALY", "JAPAN", "KOREA, REPUBLIC OF","LUXEMBOURG", "MEXICO", "NETHERLANDS", "NEW ZEALAND", "NORWAY", "POLAND", "PORTUGAL", "SPAIN", "SWEDEN", "SWITZERLAND", "UNITED KINGDOM", "UNITED STATES"
  ) 
  
  lac_00 <- c(
    "BRAZIL", "ARGENTINA", "CHILE", "PERU", "MEXICO"
  )
  
  # PISA 2003 #
  #-----------#
  
  oecd_03 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Czech Republic", "Denmark", "Finland", "France", "Germany","Greece", "Hungary", "Iceland", "Ireland", "Italy", "Japan", "Korea", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovakia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
  ) 
  
  lac_03 <- c(
    "Brazil", "Uruguay", "Mexico"
  )
  
  # PISA 2006 #
  #-----------#
  
  oecd_06 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Czech Republic", "Denmark", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Japan", "Korea", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
  ) 
  
  lac_06 <- c(
    "Argentina", "Brazil", "Chile", "Colombia", "Uruguay"
  )
  
  # PISA 2009 #
  #-----------#
  
  oecd_09 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic", "Denmark", "Finland", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
  ) 
  
  lac_09 <- c(
    "Chile", "Mexico", "Argentina", "Brazil", "Colombia", "Panama", "Peru", "Uruguay", "Costa Rica"
  )
  
  # PISA 2012 #
  #-----------#
  
  oecd_12 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic", "Denmark", "Finland", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
  ) 
  
  lac_12 <- c(
    "Chile", "Mexico", "Argentina", "Brazil", "Colombia", "Costa Rica", "Peru", "Uruguay"
  )
  
  # PISA 2015 #
  #-----------#

  oecd_15 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic", "Denmark", "Finland", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
  ) 

  lac_15 <- c(
    "Chile", "Mexico", "Brazil", "Colombia", "Costa Rica", "Dominican Republic", "Peru", "Trinidad and Tobago", "Uruguay"
  )
  
  #PISA 2017 #
  #----------#
  
  oecd_17 <- NA
  
  lac_17 <- c("Ecuador","Guatemala","Honduras","Paraguay")
  
  # PISA 2018 #
  #-----------#
  
  oecd_18 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Czech Republic", "Denmark", "Finland", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Lithuania", "Korea", "Latvia", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
  ) 
  
  lac_18 <- c(
    "Chile", "Mexico", "Colombia", "Argentina", "Brazil","Costa Rica", "Dominican Republic", "Panama", "Peru", "Uruguay"  
  )
  
  # PISA 2022 #
  #-----------#
  
  oecd_22 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", "Finland", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Lithuania", "Korea", "Latvia", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Türkiye", "United Kingdom", "United States"
  ) 
  
  lac_22 <- c(
    "Chile", "Mexico", "Colombia", "Argentina", "Brazil","Costa Rica", "Dominican Republic", "El Salvador", "Guatemala", "Jamaica", "Panama", "Paraguay", "Peru", "Uruguay"  
  )
  
  # PISA 2025 #
  #-----------#
  
  oecd_25 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", "Finland", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Lithuania", "Korea", "Latvia", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Türkiye", "United Kingdom", "United States"
  ) # Actualizar
  
  lac_25 <- c(
    "Chile", "Mexico", "Colombia", "Argentina", "Brazil","Costa Rica", "Dominican Republic", "El Salvador", "Guatemala", "Jamaica", "Panama", "Paraguay", "Peru", "Uruguay"  
  ) 
  
  # Actualizar
  
  if (anio == 2000) {
    oecd <- oecd_00
    lac  <- lac_00
  } else if (anio == 2003) {
    oecd <- oecd_03
    lac  <- lac_03
  } else if (anio == 2006) {
    oecd <- oecd_06
    lac  <- lac_06
  } else if (anio == 2009) {
    oecd <- oecd_09
    lac  <- lac_09
  } else if (anio == 2012) {
    oecd <- oecd_12
    lac  <- lac_12
  } else if (anio == 2015) {
    oecd <- oecd_15
    lac  <- lac_15
  } else if (anio == 2018) {
    oecd <- oecd_18
    lac  <- lac_18
  } else if (anio == 2022) {
    oecd <- oecd_22
    lac  <- lac_22
  } else if (anio == 2025) {
    oecd <- oecd_25
    lac  <- lac_25
  }else {
    stop("No hay grupos definidos para el año ", anio)
  }
  
  grupos <- list(OECD = oecd, LAC = lac)
  
  #========================================================================================================#
  # Variables dependientes del año
  #========================================================================================================#
  
  # El número de VP derivado del año: 5 para 2000–2012, 10 desde 2015 #
  #-------------------------------------------------------------------#
  
  n_vp <- if (anio %in% c(2000, 2003, 2006, 2009, 2012)) 5 else 10
  
  # Columna de país según el año: COUNTRY antes de 2009, CNT de 2009 en adelante #
  #------------------------------------------------------------------------------#
  
  col_pais <- if (anio >= 2006) "CNT" else "COUNTRY"
  
  # Sufijo real de los PV y etiqueta legible, sin switch #
  #------------------------------------------------------#
  
  mapa_comp <- c(
    Read = "READ", 
    Math = "MATH", 
    Scie = "SCIE")
  
  mapa_etiq <- c(
    Read = "Lectura", 
    Math = "Matemática", 
    Scie = "Ciencia")
  
  if (!compet %in% names(mapa_comp)) {
    
    # Alerta si se ha digitado mal la competencia evaluada
    stop("compet debe ser 'Read', 'Math' o 'Scie'")
    
  }
  
  competencia   <- mapa_comp[[compet]]
  etiqueta_comp <- mapa_etiq[[compet]]
  
  renombrar <- function(tab) {
    tab %>%
      as_tibble() %>%
      rename(# Ajustar a las etiquetas reales
        valor = Mean, 
        se = `s.e.`
      )   
  }
  
  paises_todos <- unique(c(oecd, lac))
  
  a1 <- data %>%
    filter(.data[[col_pais]] %in% paises_todos) 
  
  
  if (niv_estrat %in% "Nacional"){
    
    if (calculo %in% "MP"){
    
    #========================================================================================================#
    # 1. Medida promedio para cada país considerado
    #========================================================================================================#
    
    por_pais <- pisa.mean.pv(
      pvlabel = paste0("PV", 1:n_vp, competencia),
      by = col_pais,
      data = a1
    ) %>%
      renombrar() %>%
      rename(pais = all_of(col_pais))
    
    #========================================================================================================#
    # 2. Promedios agrupados de OECD y LAC
    #========================================================================================================#
    
    promedios <- map(
      names(grupos), function(nombre_grupo) {
        
        paises <- str_trim(grupos[[nombre_grupo]])
        
        por_pais %>%
          filter(pais %in% paises, !is.na(valor), !is.na(se)) %>%
          summarise(
            valor = mean(valor),           # media aritmética de países seleccionados
            se    = sqrt(sum(se^2))/n()    # promedio no ponderado de estimadores independientes
          ) %>%
          mutate(pais = nombre_grupo)
      }
    ) %>%
      bind_rows()
    
    #========================================================================================================#
    # 3. Integrando bases de países y promedios
    #========================================================================================================#
    
    resultado <- bind_rows(por_pais %>% filter(pais %in% lac), promedios) %>%
      mutate(# Identificadores de competencia evaluada y año de operativo
        competencia = etiqueta_comp,
        anio = anio
      ) %>% 
      dplyr::select(-c(Freq, SD, `s.e`))
    
  } else if (calculo %in% "Niveles"){
    
    #========================================================================================================#
    # 1. Medida promedio para cada país de LAC
    #========================================================================================================#
    
    por_pais <-  pisa.ben.pv(
      pvlabel= paste0("PV", 1:n_vp, competencia),
      cutoff= pcorte, 
      by = col_pais,
      atlevel=TRUE,
      data=a1
    ) %>% 
      filter(
        Benchmarks %in% c(paste0("<= ",pcorte[[1]]), paste0("> ", pcorte[[2]]))
      ) %>% 
      rename(pais = all_of(col_pais), valor = Percentage, se = `Std. err.`) %>% 
      dplyr::select(pais,Benchmarks,valor,se)
    
    #========================================================================================================#
    # 2. Promedios agrupados de OECD y LAC
    #========================================================================================================#
    
    promedios <- map(
      names(grupos), function(nombre_grupo) {
        
        paises <- str_trim(grupos[[nombre_grupo]])
        
        por_pais %>%
          filter(pais %in% paises, !is.na(valor), !is.na(se)) %>%
          group_by(Benchmarks) %>% 
          summarise(
            valor = mean(valor),    # media aritmética de países seleccionados
            se    = sqrt(sum(se^2))/n()        # promedio no ponderado de estimadores independientes
          ) %>%
          mutate(pais = nombre_grupo)
      }
    ) %>%
      bind_rows() %>% 
      dplyr::select(pais, Benchmarks, valor, se)
    
    #========================================================================================================#
    # 3. Integrando bases de países y promedios
    #========================================================================================================#
    
    resultado <- bind_rows(por_pais %>% filter(pais %in% lac), promedios) %>%
      mutate(# Identificadores de competencia evaluada y año de operativo
        competencia = etiqueta_comp,
        anio = anio
      )
    
  } else {
    
    stop("compet debe ser 'Read', 'Math' o 'Scie'")
    
  }
    
  } else if (niv_estrat %in% "Estratos"){
    
    nom_estratos <- intersect(
      c("sex","lengua","quintil_escs","financia","area","gestion"),
      names(a1)
    )
    if (length(nom_estratos) == 0) stop("Ningún estrato disponible para ", anio)
    
    if (calculo %in% "MP"){
    
    #========================================================================================================#
    # 1. Medida promedio para cada país considerado
    #========================================================================================================#
    
      por_pais <- nom_estratos %>%
        set_names() %>%
        map(function(v) {
          
          d <- a1 %>%
            select(all_of(col_pais), all_of(v), starts_with("PV"), starts_with("W_")) %>%
            filter(!is.na(.data[[v]]))
          
          pisa.mean.pv(
            pvlabel= paste0("PV", 1:n_vp, competencia),
            by = c(col_pais, v),
            data = d
          ) %>%
            rename(pais = all_of(col_pais),
                   categoria = all_of(v)) %>%
            mutate(categoria = as.character(categoria), # Identifica la variable de estrato
                   estrato = v, .before = 1
            )
        }) %>% 
        bind_rows() %>% 
        dplyr::select(pais,estrato, categoria, valor = Mean, se = `s.e.`)
    
    #========================================================================================================#
    # 2. Promedios agrupados de OECD y LAC
    #========================================================================================================#
    
    promedios <- map(
      names(grupos), function(nombre_grupo) {
        
        paises <- str_trim(grupos[[nombre_grupo]])
        
        por_pais %>%
          filter(!is.na(valor)) %>% 
          filter(pais %in% paises, !is.na(valor), !is.na(se)) %>%
          group_by(estrato,categoria) %>% 
          summarise(
            valor = mean(valor),            # media aritmética de países seleccionados
            se    = sqrt(sum(se^2))/n(),    # promedio no ponderado de estimadores independientes
            .groups = "drop"
          ) %>%
          mutate(pais = nombre_grupo) %>% 
          dplyr::select(pais,estrato, categoria, valor, se)
      }
    ) %>%
      bind_rows()
    
    #========================================================================================================#
    # 3. Integrando bases de países y promedios
    #========================================================================================================#
    
    resultado <- bind_rows(por_pais %>% filter(pais %in% lac), promedios) %>%
      mutate(# Identificadores de competencia evaluada y año de operativo
        competencia = etiqueta_comp,
        anio = anio
      )
    
  } else if (calculo %in% "Niveles"){
    
    #========================================================================================================#
    # 1. Medida promedio para cada país de LAC
    #========================================================================================================#
    
    por_pais <- nom_estratos %>%
      set_names() %>%
      map(function(v) {
        
        d <- a1 %>%
          dplyr::select(all_of(col_pais), all_of(v), starts_with("PV"), matches("^W_")) %>%
          filter(!is.na(.data[[v]]))
        
        pisa.ben.pv(
          pvlabel = paste0("PV", 1:n_vp, competencia),
          cutoff= pcorte, 
          by = c(col_pais, v),
          atlevel=TRUE,
          data = d
        ) %>%
          filter(
            Benchmarks %in% c(paste0("<= ",pcorte[[1]]), paste0("> ", pcorte[[2]]))
          ) %>% 
          rename(categoria = all_of(v)) %>% 
          mutate(categoria = as.character(categoria), # Identifica la variable de estrato
                 estrato = v, .before = 1
          ) 
      }) %>% bind_rows() %>% 
          dplyr::select(pais = all_of(col_pais), estrato,categoria, Benchmarks, valor = Percentage, se = `Std. err.`) 
    
    #========================================================================================================#
    # 2. Promedios agrupados de OECD y LAC
    #========================================================================================================#
    
    promedios <- map(
      names(grupos), function(nombre_grupo) {
        
        paises <- str_trim(grupos[[nombre_grupo]])
        
        por_pais %>%
          bind_rows() %>% 
          filter(pais %in% paises, !is.na(valor), !is.na(se)) %>%
          group_by(estrato,categoria,Benchmarks) %>% 
          summarise(
            valor = mean(valor),     # media aritmética de países seleccionados
            se    = sqrt(sum(se^2))/n(),        # promedio no ponderado de estimadores independientes
            .groups = "drop"
          ) %>%
          mutate(pais = nombre_grupo)
      }
    ) %>%
      bind_rows() %>% 
      dplyr::select(pais, estrato,categoria, Benchmarks, valor, se) 
    
    #========================================================================================================#
    # 3. Integrando bases de países y promedios
    #========================================================================================================#
    
    resultado <- bind_rows(por_pais %>% filter(pais %in% lac), promedios) %>%
      mutate(# Identificadores de competencia evaluada y año de operativo
        competencia = etiqueta_comp,
        anio = anio
      )
    
  } else {
    
    stop("estrato debe ser 'Nacional' o 'Estratos' ")
    
  }
    
  }
  
  return(resultado)
  
}

#========================================================================#
#========================================================================#
# Función de recursos físicos
#========================================================================#
#========================================================================#

calcular_refis <- function(bd_datos,
                           anio        = NULL,
                           grupo       = NULL,
                           niv_estrat  = c("Nacional", "Estratos"),
                           nom_estratos = c("sex", "lengua", "quintil_escs",
                                            "financia", "area", "gestion"),
                           indic_prop  = c("acceso_pc", "acceso_internet",
                                           "dig_dev_lang", "dig_dev_math",
                                           "dig_dev_scie", "dig_dev_it",
                                           "nervios_mat_prob", "nervios_lang_test"),
                           indic_media = c("dig_dev_learning", "dig_dev_leisure",
                                           "dig_dev_total", "MATHEFF", "SCIEEFF"),
                           indic_razon = c("comp_est", "tablet_est"),
                           verbose = TRUE) {

  #========================================================================================================#
  # Países considerados para promedios OECD y LAC
  #========================================================================================================#

  # PISA 2009 #
  #-----------#

  oecd_09 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic", "Denmark", "Finland", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
  )

  lac_09 <- c(
    "Chile", "Mexico", "Argentina", "Brazil", "Colombia", "Panama", "Peru", "Uruguay", "Costa Rica"
  )

  # PISA 2012 #
  #-----------#

  oecd_12 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic", "Denmark", "Finland", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
  )

  lac_12 <- c(
    "Chile", "Mexico", "Argentina", "Brazil", "Colombia", "Costa Rica", "Peru", "Uruguay"
  )

  # PISA 2015 #
  #-----------#

  oecd_15 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic", "Denmark", "Finland", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
  ) 

  lac_15 <- c(
    "Chile", "Mexico", "Brazil", "Colombia", "Costa Rica", "Dominican Republic", "Peru", "Trinidad and Tobago", "Uruguay"
  )
  
  #PISA 2017 #
  #----------#
  
  oecd_17 <- NA
  
  lac_17 <- c("Ecuador","Guatemala","Honduras","Paraguay")

  # PISA 2018 #
  #-----------#

  oecd_18 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Czech Republic", "Denmark", "Finland", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Lithuania", "Korea", "Latvia", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"
  )

  lac_18 <- c(
    "Chile", "Mexico", "Colombia", "Argentina", "Brazil","Costa Rica", "Dominican Republic", "Panama", "Peru", "Uruguay"
  )

  # PISA 2022 #
  #-----------#

  oecd_22 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", "Finland", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Lithuania", "Korea", "Latvia", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Türkiye", "United Kingdom", "United States"
  )

  lac_22 <- c(
    "Chile", "Mexico", "Colombia", "Argentina", "Brazil","Costa Rica", "Dominican Republic", "El Salvador", "Guatemala", "Jamaica", "Panama", "Paraguay", "Peru", "Uruguay"
  )

  # PISA 2025 #
  #-----------#

  oecd_25 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", "Finland", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Lithuania", "Korea", "Latvia", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Türkiye", "United Kingdom", "United States"
  ) # Actualizar

  lac_25 <- c(
    "Chile", "Mexico", "Colombia", "Argentina", "Brazil","Costa Rica", "Dominican Republic", "El Salvador", "Guatemala", "Jamaica", "Panama", "Paraguay", "Peru", "Uruguay"
  )

  # Actualizar

  if (anio == 2009) {
    oecd <- oecd_09
    lac  <- lac_09
  } else if (anio == 2012) {
    oecd <- oecd_12
    lac  <- lac_12
  } else if (anio == 2015) {
    oecd <- oecd_15
    lac  <- lac_15
  } else if (anio == 2018) {
    oecd <- oecd_18
    lac  <- lac_18
  } else if (anio == 2022) {
    oecd <- oecd_22
    lac  <- lac_22
  } else if (anio == 2025) {
    oecd <- oecd_25
    lac  <- lac_25
  }else {
    stop("No hay grupos definidos para el año ", anio)
  }

  gru_paises <- list(OECD = oecd, LAC = lac)

  # Inicio de los cálculos

  niv_estrat <- match.arg(niv_estrat)

  # Cambio de Country y CNT después de año 2006
  if (is.null(grupo)) {
    if (is.null(anio))
      stop("Indica 'anio' (para derivar la columna de país) o pasa 'grupo' explícito.")
    grupo <- if (anio >= 2006) "CNT" else "COUNTRY"
  }
  if (!grupo %in% names(bd_datos)) {
    alterna <- setdiff(c("CNT", "COUNTRY"), grupo)
    if (alterna %in% names(bd_datos)) {
      warning("'", grupo, "' no está en la base; uso '", alterna, "'."); grupo <- alterna
    } else stop("La base no tiene ni 'CNT' ni 'COUNTRY'.")
  }

  # Verifica si los indicadores existen
  no_todo_na <- function(v) v[map_lgl(v, ~ !all(is.na(bd_datos[[.x]])))]
  indic_prop  <- no_todo_na(intersect(indic_prop,  names(bd_datos)))
  indic_media <- no_todo_na(intersect(indic_media, names(bd_datos)))
  indic_razon <- no_todo_na(intersect(indic_razon, names(bd_datos)))
  presentes <- c(indic_prop, indic_media, indic_razon)
  if (length(presentes) == 0)
    stop("Ninguno de los indicadores solicitados está disponible en la base.")

  # filtro de niv_estrat para hacer la diferenciación
  if (niv_estrat == "Estratos") {
    nom_estratos <- no_todo_na(intersect(nom_estratos, names(bd_datos)))
    if (length(nom_estratos) == 0)
      stop("niv_estrat='Estratos' pero ninguna variable de estrato está en la base.")
    estratos_run <- as.list(nom_estratos)
  } else {
    estratos_run <- list(NULL)
    nom_estratos <- character(0)
  }

  if (verbose) message(
    "País = ", grupo, " | Nivel = ", niv_estrat,
    if (niv_estrat == "Estratos") paste0(" (", paste(nom_estratos, collapse = ", "), ")") else "",
    " | Indicadores: ", paste(presentes, collapse = ", "))

  a_etiqueta <- function(x) {
    if (inherits(x, "haven_labelled") || haven::is.labelled(x)) {
      haven::as_factor(x)
    } else {
      as.factor(as.character(x))
    }
  }

  # Indicadores y estratos
  datos <- bd_datos %>%
    mutate(across(all_of(indic_prop), a_etiqueta)) %>%
    mutate(across(all_of(c(indic_media, indic_razon)), ~ as.numeric(as.character(.x)))) %>%
    mutate(across(all_of(nom_estratos), ~ as.factor(as.character(.x))))   # estratos como grupos

  if (verbose) {
    sin_etiqueta <- indic_prop[map_lgl(indic_prop, function(v) {
      lv <- levels(datos[[v]])
      length(lv) > 0 && all(grepl("^[0-9]+$", lv))
    })]
    if (length(sin_etiqueta))
      warning("Indicador(es) de proporción sin etiquetas (categorías numéricas): ",
              paste(sin_etiqueta, collapse = ", "),
              ". Revisa el etiquetado aguas arriba (as_factor / factor(labels=...)).")
  }

  # Cálculos de resultados
  f_prop <- function(var, estr = NULL) {
    
    d <- if (is.null(estr)) datos else filter(datos, !is.na(.data[[estr]])) # Filtro de missing
    d <- filter(d, !is.na(.data[[var]]))
    
    pisa.table(variable = var, by = c(grupo, estr), data = d) %>%
      transmute(
        !!sym(grupo)  := .data[[grupo]],
        estrato        = if (is.null(estr)) "Nacional" else estr,
        nivel_estrato  = if (is.null(estr)) NA_character_ else as.character(.data[[estr]]),
        indicator = var,
        categoria = as.character(.data[[var]]),
        valor = Percentage,
        se = Std.err.,
        medida = "proporcion"
        )
  }
  
  f_media <- function(var, estr = NULL) {
    d <- if (is.null(estr)) datos else filter(datos, !is.na(.data[[estr]]))
    pisa.mean(variable = var, by = c(grupo, estr), data = d) %>%
      filter(!is.na(Mean)) %>%
      transmute(
        !!sym(grupo)  := .data[[grupo]],
        estrato        = if (is.null(estr)) "Nacional" else estr,
        nivel_estrato  = if (is.null(estr)) NA_character_ else as.character(.data[[estr]]),
        indicator = var,
        categoria = NA_character_,
        valor = Mean,
        se = `s.e.`,
        medida = "media"
        )
  }
  
  f_razon <- function(var, estr = NULL) {
    d <- if (is.null(estr)) datos else filter(datos, !is.na(.data[[estr]]))
    pisa.mean(variable = var, by = c(grupo, estr), data = d) %>%
      filter(!is.na(Mean)) %>%
      transmute(
        !!sym(grupo)  := .data[[grupo]],
        estrato        = if (is.null(estr)) "Nacional" else estr,
        nivel_estrato  = if (is.null(estr)) NA_character_ else as.character(.data[[estr]]),
        indicator = var,
        categoria = NA_character_,
        valor = 1/Mean,
        se = `s.e.`/Mean^2,
        medida = "razon_inv"
        )
  }
  
  seguro <- function(f) possibly(f, otherwise = NULL, quiet = FALSE)


  cruzar <- function(indics, fn) {
    seg <- seguro(fn)
    res <- list()
    for (v in indics) for (e in estratos_run) res <- c(res, list(seg(v, e)))
    res
  }

  resultado <- bind_rows(
    cruzar(indic_prop,  f_prop),
    cruzar(indic_media, f_media),
    cruzar(indic_razon, f_razon)
  )

  if (verbose) {
    fallidos <- setdiff(presentes, unique(resultado$indicator))
    if (length(fallidos))
      warning("Presentes pero sin resultado: ", paste(fallidos, collapse = ", "))
  }

  # Promedios OECD y LAC: media simple entre países
  promedios <- imap_dfr(gru_paises, function(paises, etiqueta) {
    resultado %>%
      filter(.data[[grupo]] %in% paises, !is.na(valor), !is.na(se)) %>%
      group_by(across(all_of(
        c("estrato", "nivel_estrato", "indicator", "categoria", "medida")))) %>%
      summarise(
        n_paises   = n(),
        valor = mean(valor),
        se         = sqrt(sum(se^2))/n(),   # SE de la media de k valores indep.
        .groups    = "drop"
      ) %>%
      mutate(!!sym(grupo) := etiqueta)
  })

  bind_rows(
    resultado %>% filter(.data[[grupo]] %in% lac) %>% mutate(n_paises = NA_integer_),
    promedios
  ) %>%
    arrange(indicator, .data[[grupo]], estrato, nivel_estrato) %>%
    mutate(year = anio) %>%
    rename(pais = !!sym(grupo))

}

#############################################
#############################################
# Formato de base SCL
#############################################
#############################################      

pisa_to_scldata <- function(bd_datos, tipo) {
  
  orden <- c("iddate","year","idgeo","isoalpha3","source","indicator","area",
             "quintile","sex","education_level","age","ethnicity","value","se","cv",
             "sample","language","collection_es","collection_en","theme_es","theme_en",
             "disability","migration","management","funding","month","totals_dummy",
             "admin1_ipums","level","dummy_GDI","scldata3_highlight_profile",
             "scldata3_highlight_census","quality_check","dt",
             "indicadoresAbleToBeMoreThanOne")
 
  # Dimensiones que van como "Total" #
  #----------------------------------#
 
  dims_total <- c("area","quintile","sex","education_level","age","ethnicity",
                  "language","disability","migration","management","funding")
 
  # Nombre de los estratos #
  #------------------------#
 
  rename_estratos <- c(funding    = "financia",
                       management = "gestion",
                       language   = "lengua",
                       quintile   = "quintil_escs")
 
  # Rellena con "Total" las dimensiones ausentes #
  #----------------------------------------------#
 
  ensure_dims <- function(df) {
    faltan <- setdiff(dims_total, names(df))
    if (length(faltan)) df[faltan] <- "Total"
    df
  }
 
  # Constantes fijas #
  #------------------#
 
  add_constantes <- function(df) {
    df %>% mutate(
      iddate = "year",
      idgeo = "country",
      source = "PISA",
      sample = NA,
      collection_es = "Evaluaciones de aprendizaje",
      collection_en = "Learning assessments",
      theme_es = "Educación",
      theme_en = "Education",
      month = NA_character_,
      totals_dummy = 1,
      admin1_ipums = NA,
      level = NA,
      dummy_GDI = 0,
      scldata3_highlight_profile = NA,
      scldata3_highlight_census = NA,
      quality_check = NA,
      dt = paste0(year, "-01-01"),
      indicadoresAbleToBeMoreThanOne = 0
    )
  }
 
  # Estandarización de país #
  #-------------------------#
 
  limpiar_pais <- function(df) {
    df %>% mutate(
      pais = case_when(
        pais %in% c("OECD","LAC") ~ pais,
        TRUE ~ str_to_title(pais)
      ),
      pais = case_when(
        pais %in% "Trinidad And Tobago" ~ "Trinidad and Tobago",
        TRUE ~ pais
      ),
      pais = case_when(
        pais %in% c("OECD","LAC") ~ pais,
        TRUE ~ countrycode(pais, origin = "country.name", destination = "iso3c", warn = TRUE)
      )
    )
  }
 
  # dimensiones + constantes + orden, y unión #
  #-------------------------------------------#
 
  finalizar <- function(nac, estrat) {
    bind_rows(
      nac %>% 
        ensure_dims() %>% 
        add_constantes(),
      estrat %>% 
        ensure_dims() %>% 
        add_constantes()
    ) %>%
      dplyr::select(all_of(orden))
  }
  
  if (tipo %in% "Aprendizaje") {
 
    a1 <- bd_datos %>%
      limpiar_pais() %>%
      mutate(
        indicator = case_when(
          is.na(Benchmarks) & competencia %in% "Lectura" ~ "puntaje_prom_lec",
          is.na(Benchmarks) & competencia %in% "Matemática" ~ "puntaje_prom_mat",
          is.na(Benchmarks) & competencia %in% "Ciencia" ~ "puntaje_prom_cie",
          Benchmarks %in% c("<= 407.47") ~ "tasa_bajo_desemp_lec",
          Benchmarks %in% c("<= 420.07") ~ "tasa_bajo_desemp_mat",
          Benchmarks %in% c("<= 409.54") ~ "tasa_bajo_desemp_cie",
          Benchmarks %in% c("> 625.61") ~ "tasa_alto_desemp_lec",
          Benchmarks %in% c("> 606.99") ~ "tasa_alto_desemp_mat",
          Benchmarks %in% c("> 633.33") ~ "tasa_alto_desemp_cie",
          TRUE ~ NA_character_
        )
      )
 
    # Nacional
    nac <- a1 %>%
      filter(niv_estrat == "Nacional") %>%
      dplyr::select(year = anio, isoalpha3 = pais, value = valor, se, indicator) %>%
      mutate(cv = se/value)
 
    # Estratos
    estrat <- a1 %>%
      filter(niv_estrat == "Estratos") %>%
      dplyr::select(
        year = anio, isoalpha3 = pais, estrato, categoria,
        value = valor, se, indicator
      ) %>%
      mutate(.obs = row_number()) %>%
      pivot_wider(
        names_from = estrato, 
        values_from = categoria,
        values_fill = "Total"
      ) %>%
      dplyr::select(-.obs) %>%
      rename(any_of(rename_estratos)) %>%
      mutate(cv = se/value) %>% 
      mutate(
        area = case_when(
          area %in% c("Rural") ~ "rural",
          area %in% c("Urban") ~ "urban",
          TRUE ~ area
        )
      ) %>% 
      mutate(
        sex = case_when(
          sex %in% c("Male") ~ "man",
          sex %in% c("Female") ~ "woman",
          TRUE ~ sex
        )
      ) %>% 
      mutate(
        management = case_when(
          management %in% c("Private") ~ "private",
          management %in% c("Public") ~ "public",
          TRUE ~ management
        )
      ) %>% 
      mutate(
        funding = case_when(
          funding %in% c("Mixed") ~ "mixed_f",
          funding %in% c("Private") ~ "private_f",
          funding %in% c("Public") ~ "public_f",
          TRUE ~ funding
        )
      ) %>% 
      mutate(
        language = case_when(
          language %in% c("Language of test") ~ "language",
          language %in% c("Another language") ~ "no_language",
          TRUE ~language
        )
      )
 
    finalizar(nac, estrat)
 
  } else if (tipo %in% "Recursos") {
 
    a2 <- bd_datos %>%
      limpiar_pais() %>%
      mutate(
        indicator = case_when(
          indicator %in% "acceso_pc" ~ "Acceso_Compu",
          indicator %in% "acceso_internet" ~ "Acceso_Internet",
          indicator %in% "comp_est" ~ "Estudiantes_Compu",
          indicator %in% "tablet_est" ~ "Estudiantes_Tableta",
          indicator %in% "dig_dev_scie" ~ "dig_dev_sci",
          TRUE ~ indicator
        )
      ) %>%
      filter(!(indicator %in% "Acceso_Internet" & categoria %in% "Sin acceso")) %>%
      filter(!(indicator %in% "Acceso_Compu" & categoria %in% "Con acceso"))
 
    # Nacional 
    a2_nac <- a2 %>%
      filter(estrato %in% "Nacional") %>%
      dplyr::select(year, isoalpha3 = pais, value = valor, se, indicator) %>%
      mutate(cv = NA)
 
    # Estratos
    a2_estrat <- a2 %>%
      filter(!estrato %in% "Nacional") %>%
      dplyr::select(year, isoalpha3 = pais, estrato, nivel_estrato,
                    value = valor, se, indicator) %>%
      mutate(.obs = row_number()) %>%
      pivot_wider(
        names_from = estrato, 
        values_from = nivel_estrato,
                  values_fill = "Total"
        ) %>%
      dplyr::select(-.obs) %>%
      rename(any_of(rename_estratos)) %>%
      mutate(cv = NA) %>% 
      mutate(
        area = case_when(
          area %in% c("Rural") ~ "rural",
          area %in% c("Urban") ~ "urban",
          TRUE ~ area
        )
      ) %>% 
      mutate(
        sex = case_when(
          sex %in% c("Male") ~ "man",
          sex %in% c("Female") ~ "woman",
          TRUE ~ sex
        )
      ) %>% 
      mutate(
        management = case_when(
          management %in% c("Private") ~ "private",
          management %in% c("Public") ~ "public",
          TRUE ~ management
        )
      ) %>% 
      mutate(
        funding = case_when(
          funding %in% c("Mixed") ~ "mixed_f",
          funding %in% c("Private") ~ "private_f",
          funding %in% c("Public") ~ "public_f",
          TRUE ~ funding
        )
      ) %>% 
      mutate(
        language = case_when(
          language %in% c("Language of test") ~ "language",
          language %in% c("Another language") ~ "no_language",
          TRUE ~language
        )
      )
 
    finalizar(a2_nac, a2_estrat)
  }
}
