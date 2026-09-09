getwd()
rstudioapi::getSourceEditorContext()$path
# ============================================================================
# 1. CARGA DE LIBRERÍAS NECESARIAS
# ============================================================================

# Se cargan los paquetes necesarios para la lectura, transformación
# y consolidación de información.

# readr
# Lectura de archivos CSV.
library(readr)

# dplyr
# Manipulación y transformación de datos:
# filtros, selecciones, agrupaciones y uniones.
library(dplyr)

# purrr
# Aplicación repetitiva de funciones sobre listas y ficheros.
library(purrr)

# tibble
# Estructura moderna de tablas utilizada por tidyverse.
library(tibble)

# ---------------------------------------------------------------------------
# Opcional
# ---------------------------------------------------------------------------

# Los paquetes anteriores forman parte del paquete tidyverse.
# De manera conjunta podrían cargarse mediante:
#
# library(tidyverse)
#
# Sin embargo, este script utiliza únicamente los paquetes indicados
# anteriormente, por lo que se cargan de forma explícita para dejar
# identificadas sus dependencias reales.



##################################################################################
##################################################################################
########### DATOS CASO REAL
##################################################################################
##################################################################################

# ============================================================================
# 2. CARGA DEL CONJUNTO DE CARACTERISTICAS EXPLICATIVAS (X)
# ============================================================================

# Carga del fichero con las variables independientes
# posteriormente serán utilizadas como información de entrada del modelo.

HNANESI_subset_X <- read_csv(
  "C:/....../HNANESI_subset_X.csv"
)

# ============================================================================
# 3. CARGA DE LA VARIABLE OBJETIVO (Y)
# ============================================================================

# Carga el fichero que contiene la variable objetivo.

HNANESI_subset_y <- read_csv(
  "C:/....../HNANESI_subset_y.csv"
)


# ============================================================================
# 4. INTEGRACIÓN EN UNA BBDD DE LAS VARIABLES EXPLICATIVAS Y LA VARIABLE OBJETIVO
# ============================================================================
#
# Objetivo:
# Construir el conjunto de datos completo del estudio mediante la unión
# de las variables explicativas y la variable objetivo.
#
# Creación del dataset HNANESI con toda la información necesaria.
#   
# Visualización del número de filas y columnas de la BBDD.

HNANESI <- full_join(HNANESI_subset_X, HNANESI_subset_y, by = "...1")

dim(HNANESI)

# ============================================================================
# 5. ELIMINACIÓN DE REGISTROS CON INFORMACIÓN INCOMPLETA
# ============================================================================
#
# Objetivo:
# Depuración de la BBDD manteniendo únicamente las observaciones que disponen de información
# completa en todas sus variables (Eliminación de las filas que contienen valores ausentes (NA)).


HNANESI <- HNANESI[complete.cases(HNANESI), ]




# ============================================================================
# 6. CLASIFICACIÓN AUTOMÁTICA DE LOS TIPOS DE VARIABLES
# ============================================================================
#
# Objetivo:
# Identificar la naturaleza de cada variable del conjunto de datos y generar
# un identificador que facilite su posterior análisis.
#
# Clasificación utilizada:
# - NUMÉRICA
# - CATEGÓRICA
# - ORDINAL
# - OTRA
#
# Resultado esperado:
# - Creación de una tabla resumen con el nombre de cada variable,
#   su tipo en R y su clasificación funcional para el análisis.



clasificar_variables <- function(df) {
  

  
  vars <- names(df) 
  
  vars_numericas <- vars[sapply(df, function(x) is.numeric(x) || is.integer(x))]

  vars_ord       <- vars[sapply(df, function(x) is.ordered(x))]

  vars_cat       <- vars[sapply(df, function(x) {
    (is.factor(x) && !is.ordered(x)) || is.character(x)
  })]
  
  tibble::tibble(
    variable = vars,
    
    clase_R  = sapply(df, function(x) paste(class(x), collapse = ", ")),
    

    tipo     = dplyr::case_when(
      variable %in% vars_numericas ~ "NUMÉRICA",
      variable %in% vars_ord       ~ "ORDINAL",
      variable %in% vars_cat       ~ "CATEGÓRICA",
      TRUE                         ~ "OTRA"
    )
  )
}

resultado_tipos <- clasificar_variables(HNANESI)

View(resultado_tipos)



# ============================================================================
# 7. ANALISIS DESCRIPTIVO DE LAS VARIABLES NUMÉRICAS
# ============================================================================
#
# Objetivo:
# Obtener una tabla resumen con los principales estadísticos descriptivos
# de las variables numéricas de la BBDD.
#
# Información estimada:
# - Número de registros total
# - Número de valores ausentes
# - Número de valores válidos
# - Mínimo
# - Percentil 25
# - Mediana 
# - Percentil 75
# - Máximo
# - Media
# - Desviación 
#
# Resultado:
# Una tabla con una fila por variable numérica y sus estadísticos
# descriptivos 



resumen_numericas <- function(df) {

  
  info <- clasificar_variables(df)

  
  vars_num <- info %>% 
    dplyr::filter(tipo == "NUMÉRICA") %>% 
    dplyr::pull(variable)

  
  if (length(vars_num) == 0) {
    message("No se encontraron variables numéricas.")
    return(NULL)
  }

  
  purrr::map_dfr(vars_num, function(v) {
 
    
    x <- df[[v]]
    
    x_num <- as.numeric(x)
    
  # Construir resumen (tamaño, missing, percentiles, min/máx, media, desviación estándar)
    
    tibble::tibble(
      variable = v,
      n_total  = length(x_num),
      n_na     = sum(is.na(x_num)),
      n_valid  = sum(!is.na(x_num)),
      min      = suppressWarnings(min(x_num, na.rm = TRUE)),
      p25      = suppressWarnings(quantile(x_num, 0.25, na.rm = TRUE, names = FALSE)),
      p50      = suppressWarnings(quantile(x_num, 0.50, na.rm = TRUE, names = FALSE)),
      p75      = suppressWarnings(quantile(x_num, 0.75, na.rm = TRUE, names = FALSE)),
      max      = suppressWarnings(max(x_num, na.rm = TRUE)),
      media    = suppressWarnings(mean(x_num, na.rm = TRUE)),
      sd       = suppressWarnings(stats::sd(x_num, na.rm = TRUE))
    )
  })
}


# ============================================================================
# 8. RESUMEN DESCRIPTIVO DE LAS VARIABLES CATEGÓRICAS
# ============================================================================
#
# Objetivo:
# Generar una tabla resumen con las principales características de las
# variables categóricas presentes en la BBDD.
#
# Información estimada:
# - Número total de observaciones
# - Número valores ausentes
# - Número valores válidos
# - Número de las distintas categorías por variable
# - Muestra de las categorías existentes
# - Categorías más frecuentes por variable
#
# Resultado:
# - Una tabla con una fila por cada variable categórica y sus principales
#   características descriptivas.


resumen_categoricas <- function(df, max_niveles_mostrar = 20, top_n = 5) {
  
  info <- clasificar_variables(df)

  vars_cat <- info %>% 
    dplyr::filter(tipo == "CATEGÓRICA") %>% 
    dplyr::pull(variable)

  if (length(vars_cat) == 0) {
    message("No se encontraron variables categóricas.")
    return(NULL)
  }

  purrr::map_dfr(vars_cat, function(v) {
  
    x <- df[[v]]
    
    x_fac <- as.factor(x)

    levs <- levels(x_fac)

    if (length(levs) > max_niveles_mostrar) {
      levs_mostrar <- c(levs[1:max_niveles_mostrar], "...(truncado)")
    } else {

      levs_mostrar <- levs
    }

    freq <- sort(table(x_fac), decreasing = TRUE)
   
    top_freq <- head(freq, top_n)

    top_str <- paste0(names(top_freq), " (", as.integer(top_freq), ")", collapse = "; ")

    tibble::tibble(
      variable       = v,
      n_total        = length(x_fac),
      n_na           = sum(is.na(x_fac)),
      n_valid        = sum(!is.na(x_fac)),
      n_niveles      = length(levs),
      niveles_muestra= paste(levs_mostrar, collapse = ", "),
      top_categorias = top_str
    )
  })
}



# ============================================================================
# 9. GENERACIÓN DEL RESUMEN COMPLETO DEL CONJUNTO DE DATOS
# ============================================================================
#
# Objetivo:
# Agregar los resúmenes descriptivos de las variables
# numéricas y categóricas de la BBDD.
#
# Excepcións:
# Las variables ordinales no se incluyen en esta agregación debido a que su
# identificación automática puede ser dudosa y opta por una revisión
# específica posterior.
#
# Resultado:
# - Resumen estadístico de variables numéricas.
# - Resumen descriptivo de variables categóricas.
# - Estructura única para la revisión de resultados.



resumen_rangos_completo <- function(df) {
  list(
    numericas   = resumen_numericas(df),
    categoricas = resumen_categoricas(df)
  )
}



# ============================================================================
# 10. EJECUCIÓN DEL RESUMEN GENERAL DEL CONJUNTO DE DATOS
# ============================================================================
#
# Objetivo:
# Generar los resúmenes descriptivos de las variables numéricas y
# categóricas definidos en los pasos anteriores.
#
# Resultado:
# - Resumen estadístico de variables numéricas.
# - Resumen descriptivo de variables categóricas.


# Generamos el listado completo de resúmenes

res_rangos <- resumen_rangos_completo(HNANESI)

# 1) Rangos numéricos
View(res_rangos$numericas)

# 2) Rango de categóricas (niveles y top categorías)
View(res_rangos$categoricas)


# NOTA:
# No hay salida de ordinales en este punto del análisis



# ============================================================================
# 11. CONSTRUCCIÓN DEL INDICADOR DE RIESGO DE PRESIÓN ARTERIAL
# ============================================================================
#
# Objetivo:
# Crear un indicador de riesgo de presión arterial a partir de las
# variables de presión sistólica y diastólica.
#
# Criterio:
# - Clasificación de la presión sistólica.
# - Clasificación de la presión diastólica.
# - Asignación del nivel de riesgo más desfavorable entre ambas variables.
#
# Variables de entrada:
# - Systolic BP
# - Diastolic BP
#
# Variables salida:
# - Risk_BP     : representación numérica del riesgo (0-4)
# - Risk_BP_cat : representación categórica ordinal del mismo riesgo
#
# Resultado:
# - Incorporación al dataset de un único indicador de riesgo de presión
#   arterial almacenado en formato numérico y categórico ordinal.




codificar_risk_bp <- function(df,
                              var_sys,
                              var_dias,
                              nombre_risk_num = "Risk_BP",
                              nombre_risk_cat = "Risk_BP_cat") {

  
  # Verificamos que la variable sistólica existe
  
  if (!(var_sys %in% names(df))) {
    stop("La variable sistólica '", var_sys, "' no existe en el data.frame.")
  }
  
  # Verificamos que la variable diastólica existe
  
  if (!(var_dias %in% names(df))) {
    stop("La variable diastólica '", var_dias, "' no existe en el data.frame.")
  }
  

  # Convertimos a numérico por si vienen como factor/character
  
  sys <- as.numeric(df[[var_sys]])
  dias <- as.numeric(df[[var_dias]])
  

  # Creamos vectores vacíos con NA para almacenar clasificación
  
  risk_sys  <- rep(NA_integer_, length(sys))
  risk_dias <- rep(NA_integer_, length(dias))
  
  
  # Óptima:   < 120
  risk_sys[!is.na(sys) & sys < 120] <- 0
  
  # Normal: 120–129
  risk_sys[!is.na(sys) & sys >= 120 & sys <= 129] <- 1
  
  # Elevada / Prehipertensión: 130–139
  risk_sys[!is.na(sys) & sys >= 130 & sys <= 139] <- 2
  
  # Hipertensión grado 1: 140–159
  risk_sys[!is.na(sys) & sys >= 140 & sys <= 159] <- 3
  
  # Hipertensión grado 2: ≥ 160
  risk_sys[!is.na(sys) & sys >= 160] <- 4
  

  # Óptima: < 80
  risk_dias[!is.na(dias) & dias < 80] <- 0
  
  # Normal: 80–84
  risk_dias[!is.na(dias) & dias >= 80 & dias <= 84] <- 1
  
  # Elevada / Prehipertensión: 85–89
  risk_dias[!is.na(dias) & dias >= 85 & dias <= 89] <- 2
  
  # Hipertensión grado 1: 90–99
  risk_dias[!is.na(dias) & dias >= 90 & dias <= 99] <- 3
  
  # Hipertensión grado 2: ≥ 100
  risk_dias[!is.na(dias) & dias >= 100] <- 4
  
  
  # Tomamos el máximo entre sistólica y diastólica

  
  risk_comb <- pmax(risk_sys, risk_dias, na.rm = TRUE)
  risk_comb[is.infinite(risk_comb)] <- NA_integer_
  

  df[[nombre_risk_num]] <- as.integer(risk_comb)
  

  # Definimos etiquetas (ordenadas)
  
  niveles_etiqueta <- c("Optima", "Normal", "Elevada", "Hipertension_1", "Hipertension_2")
  
  # Convertimos a factor ordinal
  
  risk_cat <- factor(
    risk_comb,
    levels = 0:4,
    labels = niveles_etiqueta,
    ordered = TRUE  # porque es un riesgo ordinal
  )
  
  # Añadimos al dataframe
  
  df[[nombre_risk_cat]] <- risk_cat
  

  return(df)
}

# ============================================================================
# 12. APLICACIÓN DEL INDICADOR DE RIESGO DE PRESIÓN ARTERIAL
# ============================================================================
#
# Objetivo:
# Incluir en el dataset las variables de riesgo asociadas a la presión
# arterial calculadas a partir de las variables de presión sistólica y diastólica.


HNANESI <- codificar_risk_bp(
  df      = HNANESI,
  var_sys = "Systolic BP",
  var_dias=  "Diastolic BP"
)

# Comprobamos que las variables se han creado correctamente

names(HNANESI)

# Revisamos distribución de la variable categórica

table(HNANESI$Risk_BP_cat, useNA = "ifany")
summary(HNANESI$Risk_BP)


# ============================================================================
# 13. RECODIFICACIÓN DE LA VARIABLE SEXO
# ============================================================================
#
# Objetivo:
# Transformar la codificación numérica original de la variable sexo
# a una variable categórica más interpretable.
#
# Variable de entrada:
# - Sex
#
# Variable generada:
# - SEX_N
#
# Codificación:
# - 1 = Male
# - 2 = Female
#
# Resultado:
# - Variable categórica con etiquetas descriptivas.



HNANESI$SEX_N <- ifelse(HNANESI$Sex == 1, "Male",
                        ifelse(HNANESI$Sex == 2, "Female", NA))


# ============================================================================
# 14. RECÁLCULO DE LA ESTRUCTURA DEL DATASET
# ============================================================================
#
# Objetivo:
# Recalcular la estructura del dataset después de la codificación de las nuevas 
# variables.

resultado_tipos <- clasificar_variables(HNANESI)

# ============================================================================
# 15. CODIFICACION VARIABLE OBJETIVO EN BINARIA
# ============================================================================
#
# Objetivo:
# Transformar la variable objetivo original en una versión binaria para
# análisis de clasificación.
#
# Variable de entrada:
# - y
#
# Variable salida:
# - y_booleana
#
# Criterio:
# - y <= 0  → "0"
# - y > 0   → "1"
#
# Resultado:
# - Incorporación al dataset de una variable binaria de la característica
#   objetivo original.


HNANESI$y_booleana <- ifelse(HNANESI$y <= 0, "0",
                             ifelse(HNANESI$y > 0, "1", NA))


################################################################################
################## APLICACIÓN AL CASO REAL #####################################
################################################################################

# ============================================================================
# 16. DEFINICIÓN DE LAS VARIABLES DE ANÁLISIS
# ============================================================================
#
# Objetivo:
# Seleccionar las variables que formarán parte del dataset final.
#



vars_seleccionadas <- c(
  "Age" ,                 # Variable numérica
  "Race",                 # Variable categórica
  "Serum Cholesterol",    # Variable numérica
  "Risk_BP_cat" ,         # Variable ordinal
  "SEX_N",                # Variable categórica (normalizada a 0/1)
  "y",                    # Variable objetivo numérica 
  "y_booleana"            # Variable objetivo binaria
)



# ============================================================================
# 17. CONSTRUCCIÓN DEL DATASET DE TRABAJO
# ============================================================================
#
# Objetivo:
# Generar un subconjunto de datos que contenga exclusivamente las
# variables seleccionadas para el estudio.



seleccionar_variables <- function(df, vars) {
  
  df %>% dplyr::select(all_of(vars))
}



HNANESI_F <- seleccionar_variables(HNANESI, vars_seleccionadas)


# ============================================================================
# 18. DEFINICIÓN DE CRITERIOS DE CODIFICACIÓN DE LAS VARIABLES
# ============================================================================
#
# Objetivo:
# Establecer las funciones que permitirán aplicar de forma homogénea
# la codificación de variables en función a su naturaleza.

# Convertir a numérica

codificar_numerica <- function(x) {
  as.numeric(x)
}

# Convertir a categórica nominal (sin orden)

codificar_nominal <- function(x) {
  factor(x)
}

# Convertir a categórica ordinal (con orden explícito)

codificar_ordinal <- function(x, niveles_ordenados) {
  factor(x, levels = niveles_ordenados, ordered = TRUE)
}


# ============================================================================
# 19. ASIGNACIÓN METODOLÓGICA DE TIPOS DE VARIABLES
# ============================================================================
#
# Objetivo:
# Definir manualmente la naturaleza de cada variable del dataset.
#
# Clasificaciones utilizadas:
# - Numéricas
# - Categóricas nominales
# - Categóricas ordinales
#
# Resultado:
# - Identificación explícita de la naturaleza de cada variable para garantizar
#   una codificación correcta durante el caso de estudio.

# IMPORTANTE: nombres deben coincidir EXACTAMENTE con el dataset


num_vars  <- c("Age", "Serum Cholesterol","y")                   # Variables numéricas

nom_vars <- c(  "Race",  "SEX_N",  "y_booleana")                  # Variables categóricas nominales

ord_vars  <- c( "Risk_BP_cat")                                    # Variables ordinales    


# ============================================================================
# 20. ESTRUCTURA DEL DATASET DE TRABAJO
# ============================================================================
#
# Objetivo:
# Generar un archivo descriptivo de las variables presentes en el
# dataset final antes de que apliquemos la codificación definitiva.
#
# Información generada:
# - Clase de la variable almacenada en R
# - Tipo de variable
# - Número de valores distintos
# - Valores representativos
# - Valores ausentes


clasificar_variables_unico <- function(df, max_unique = 20) {
 

  map_df(names(df), function(v) {

    x <- df[[v]]
    
    clase <- class(x)

    n_na  <- sum(is.na(x))

    tipo <- case_when(
      is.numeric(x) || is.integer(x) ~ "NUMERICA",
      is.ordered(x)                  ~ "ORDINAL",
      is.factor(x)                   ~ "CATEGORICA",
      is.character(x)                ~ "CATEGORICA",
      TRUE                           ~ "OTRA"
    )
    
    valores <- if (is.numeric(x)) {
      paste0("min=", min(x, na.rm=TRUE), ", max=", max(x, na.rm=TRUE))
    } else {
      u <- unique(x)
      paste(head(u, 20), collapse = ", ")
    }

    tibble::tibble(
      variable = v,
      clase    = paste(clase, collapse = ", "),
      tipo     = tipo,
      n_unique = length(unique(x)),
      valores  = valores,
      missing  = n_na
    )
  })
}


# ============================================================================
# 21. VALIDACIÓN DE LA ESTRUCTURA DEL DATASET FINAL
# ============================================================================
#
# Objetivo:
# Verificar la composición del dataset de trabajo antes de su almacenamiento.
#
# Resultado:
# - Conjunto de variables.
# - Tipos de datos.
# - Valores.
# - Detección de posibles incidencias de codificación.


tabla_codificacion <- clasificar_variables_unico(HNANESI_F)


View(tabla_codificacion)


names(HNANESI_F)


# ============================================================================
# 22. ALMACENAMIENTO DEL DATASET FINAL
# ============================================================================
#
# Objetivo:
# Guardar el dataset preparado para su reutilización posterior sin necesidad
# de repetir el proceso completo de preparación de datos.
# - R base (no requiere librerías adicionales)
#
# Formato de almacenamiento:
# - RDS
#
# Funciones utilizadas:
# - saveRDS() / readRDS()
#   o
# - save() / load() según función elegida

library(crayon)

str(HNANESI_F)

saveRDS(
  HNANESI_F,
  file = "C:/Users/.../HNANESI_F.rds"
)


# ============================================================================
# 23. RECUPERACIÓN DEL DATASET ALMACENADO
# ============================================================================
#
# Objetivo:
# Cargar una versión previamente almacenada del dataset para continuar
# con el análisis sin repetir las etapas de preparación.
#
# Resultado:
# - Restauración del objeto HNANESI_F en memoria.

HNANESI_F <- readRDS("C:/Users/.../HNANESI_F.rds")


# ============================================================================
# 24. FIJACIÓN DE LA SEMILLA DE REPRODUCIBILIDAD
# ============================================================================
#
# Objetivo:
# Garantizar poder garantizar generar los mismos resultados en ejecuciones posteriores.
#
# Aplicación:
# - Selección aleatoria de muestras.
# - División entrenamiento-validación.
# - Validación cruzada.
#
# Resultado:
# - Reproducir completamente los resultados obtenidos.

set.seed(123)


# ============================================================================
# 25. NORMALIZACIÓN DE LOS NOMBRES DE VARIABLES
# ============================================================================
#
# Objetivo:
# Estandarizar los nombres de las características mediante una nomenclatura
# homogenea abreviada de tres caracteres.
#
# Criterio:
# - Cada variable se identificará con los tres primeros caracteres
#   de su nombre original.
#
# Control de calidad:
# - Verificación de posibles duplicidades generadas durante el proceso
#   de abreviación (no aplica en este caso real.
#
# Resultado:
# - Dataset con nombres de variables normalizados y homogéneos.


normalizar_a_tres_letras <- function(df) {
  

  stopifnot(is.data.frame(df))
  

  nombres_originales <- names(df)
  
  nombres_cortos     <- substring(nombres_originales, 1, 3)
  

  if (any(duplicated(nombres_cortos))) {
    stop(
      "Colisión al normalizar nombres a 3 letras: ",
      paste(unique(nombres_cortos[duplicated(nombres_cortos)]), collapse = ", ")
    )
  }
  

  names(df) <- nombres_cortos
  df
}


# ============================================================================
# 26. PREPARACIÓN FINAL DEL DATASET ANALÍTICO
# ============================================================================
#
# Objetivo:
# Preparar el dataset para su utilización en medidas difusas, modelos
# predictivos, cálculo de valores de Shapley y representaciones gráficas.
#
# Procesos incluidos:
# - Conversión a data.frame.
# - Homogeneización de nombres de variables.
# - Validación de las variables objetivo.
# - Detección de incidencias de calidad de datos.
# - Eliminación de registros duplicados.
#
# Resultado:
# - Dataset depurado y preparado para análisis,modelización y representación 
# gráfica.

preparar_HNANESI <- function(df) {
  
  df <- as.data.frame(df)

  names(df) <- gsub(" ", "_", names(df))  # Por si acaso espacios

  names(df)[names(df) == "Serum_Cholesterol"] <- "Cholesterol"
  names(df)[names(df) == "Risk_BP_cat"] <- "Risk_BP"
  names(df)[names(df) == "SEX_N"] <- "Sex"
  

  df$Race <- factor(df$Race)
  

  if (!is.ordered(df$Risk_BP)) {
    
    niveles_bp <- unique(as.character(df$Risk_BP))

    df$Risk_BP <- factor(df$Risk_BP,
                         levels = sort(unique(niveles_bp)),
                         ordered = TRUE)
  }

  df$Sex <- factor(df$Sex)

  df$y <- as.numeric(df$y)

  if ("y_booleana" %in% names(df)) {
    df$y_b <- df$y_booleana
    df$y_booleana <- NULL
  }

  
  if ("y_b" %in% names(df)) {
    df$y_b <- factor(df$y_b, levels = c("0", "1"))
  } else {
    stop("No se encuentra el target de clasificación (y_b ni y_booleana).")
  }

  na_count <- colSums(is.na(df))
  if (any(na_count > 0)) {
    message("⚠️  Atención: la base contiene NA. Revisa:")
    print(na_count[na_count > 0])
  }

  n_dupls <- sum(duplicated(df))
  if (n_dupls > 0) {
    message("⚠️  Eliminando filas duplicadas: ", n_dupls)
    df <- df[!duplicated(df), ]
  }

  const_cols <- names(df)[sapply(df, function(x) length(unique(x)) == 1)]
  if (length(const_cols) > 0) {
    message("⚠️  Columnas constantes eliminadas: ", paste(const_cols, collapse = ", "))
    df <- df[, !(names(df) %in% const_cols)]
  }

  zero_var <- names(df)[sapply(df, function(x) var(as.numeric(x), na.rm = TRUE) == 0)]
  if (length(zero_var) > 0) {
    message("⚠️  Columnas con varianza cero: ", paste(zero_var, collapse = ", "))
  }

  str(df)
  
  message("✅ Base HNANESI depurada y lista.")
  return(df)
}


# ============================================================================
# 27. GENERACIÓN DEL DATASET ANALÍTICO DEFINITIVO
# ============================================================================
#
# Objetivo:
# Construir la versión final del dataset preparada para las
# fases de modelización, medidas difusas, cálculo de valores de Shapley
# y representaciones gráficas.
#
# Procesos aplicados:
# 1. Preparación y depuración.
# 2. Homogeneización de los nombres de variables.
# 3. Normalización de la nomenclatura de las variables a tres caracteres.
# 4. Chequeo de la estructura final.
#
# Resultado:
# - Creación del dataset HNANESI_clean final para el caso real.


set.seed(123)

HNANESI_clean <- preparar_HNANESI(HNANESI_F) # Depuración 

HNANESI_clean <- normalizar_a_tres_letras(HNANESI_clean) # Normalización a tres letras

names(HNANESI_clean)


