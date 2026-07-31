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

  # Indicadores y estratos
  datos <- bd_datos %>%
    mutate(across(all_of(indic_prop),  ~ as.factor(as.character(.x)))) %>%
    mutate(across(all_of(c(indic_media, indic_razon)), ~ as.numeric(as.character(.x)))) %>%
    mutate(across(all_of(nom_estratos), ~ as.factor(as.character(.x))))   # estratos como grupos
  
  # Cálculos de resultados
  f_prop <- function(var, estr = NULL) {
    d <- if (is.null(estr)) datos else filter(datos, !is.na(.data[[estr]])) # Filtro de missing
    pisa.table(variable = var, by = c(grupo, estr), data = d) %>%
      transmute(
        !!sym(grupo)  := .data[[grupo]],
        estrato        = if (is.null(estr)) "Nacional" else estr,
        nivel_estrato  = if (is.null(estr)) NA_character_ else as.character(.data[[estr]]),
        indicator = var, categoria = as.character(.data[[var]]),
        estimacion = Percentage, se = Std.err., medida = "proporcion")
  }
  f_media <- function(var, estr = NULL) {
    d <- if (is.null(estr)) datos else filter(datos, !is.na(.data[[estr]]))
    pisa.mean(variable = var, by = c(grupo, estr), data = d) %>%
      filter(!is.na(Mean)) %>%
      transmute(
        !!sym(grupo)  := .data[[grupo]],
        estrato        = if (is.null(estr)) "Nacional" else estr,
        nivel_estrato  = if (is.null(estr)) NA_character_ else as.character(.data[[estr]]),
        indicator = var, categoria = NA_character_,
        estimacion = Mean, se = `s.e.`, medida = "media")
  }
  f_razon <- function(var, estr = NULL) {
    d <- if (is.null(estr)) datos else filter(datos, !is.na(.data[[estr]]))
    pisa.mean(variable = var, by = c(grupo, estr), data = d) %>%
      filter(!is.na(Mean)) %>%
      transmute(
        !!sym(grupo)  := .data[[grupo]],
        estrato        = if (is.null(estr)) "Nacional" else estr,
        nivel_estrato  = if (is.null(estr)) NA_character_ else as.character(.data[[estr]]),
        indicator = var, categoria = NA_character_,
        estimacion = 1 / Mean, se = `s.e.` / Mean^2, medida = "razon_inv")
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
  
  # Promedios OECD y LAC: media simple entre países ----
  promedios <- if (isTRUE(incluir_grupos)) {
    imap_dfr(gru_paises, function(paises, etiqueta) {
      resultado %>%
        filter(.data[[grupo]] %in% paises, !is.na(estimacion), !is.na(se)) %>%
        group_by(across(all_of(
          c("estrato", "nivel_estrato", "indicator", "categoria", "medida")))) %>%
        summarise(
          n_paises   = n(),
          estimacion = mean(estimacion),
          se         = sqrt(sum(se^2)) / n(),   # SE de la media de k estimaciones indep.
          .groups    = "drop"
        ) %>%
        filter(n_paises >= min_paises) %>%
        mutate(!!sym(grupo) := etiqueta)
    })
  } else NULL
  
  bind_rows(
    resultado %>% mutate(n_paises = NA_integer_),
    promedios
  ) %>%
    arrange(indicator, .data[[grupo]], estrato, nivel_estrato) %>%
    mutate(year = anio)
  
}

