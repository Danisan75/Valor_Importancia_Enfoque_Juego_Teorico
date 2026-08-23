# ============================================================================
# PASOS 1: INSTALAR LIBRERÍAS NECESARIAS
# ============================================================================

# readr → leer archivos CSV de forma rápida
library(readr)

# dplyr → manipulación de datos (select, filter, mutate, joins, etc.)
library(dplyr)

# purrr → programación funcional (map, map_dfr, etc.)
library(purrr)

# tibble → creación de dataframes modernos (tibble)
library(tibble)

# ============================================================================
# RECOMENDACIÓN - DEPENDE DE LA VERSION
# ============================================================================

# Si quieres simplificar, puedes cargar todo el ecosistema tidyverse:
# (incluye readr, dplyr, purrr, tibble, ggplot2, etc.)

# install.packages("tidyverse")   # solo la primera vez
# library(tidyverse)

##################################################################################
##################################################################################
########### DATOS REAL CASE
##################################################################################
##################################################################################



# Quitar

HNANESI_subset_X <- read_csv("C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/BBDD HNANES I/HNANESI_subset_X.csv")

HNANESI_subset_y <- read_csv("C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/BBDD HNANES I/HNANESI_subset_y.csv")
names(HNANESI_subset_y)

# ============================================================================
# PASO 2: Leer el dataset de variables explicativas (X)
# ============================================================================

# read_csv() lee un archivo CSV desde la ruta indicada
# El resultado es un data.frame (tibble) en R

HNANESI_subset_X <- read_csv("C:/....../HNANESI_subset_X.csv")  # RUTA ARCHIVOS HNANESI_subset_X

# ============================================================================
# PASO 3: Leer el dataset de la variable objetivo (Y)
# ===========================================================================

HNANESI_subset_y <- read_csv("C:/...../HNANESI_subset_y.csv")  # RUTA ARCHIVOS HNANESI_subset_X


# ============================================================================
# PASO 4: Ver los nombres de las columnas del dataset Y
# Resultado esperado:
# - Devuelve un vector con los nombres de columnas
# Ejemplo: "...1", "y"
# Para qué sirve:
# ===============================================================

names(HNANESI_subset_y)


# ============================================================================
# PASO 5: Unir los dos datasets (FULL JOIN) y analizamos la dimensión
# ============================================================================

# full_join une dos tablas por una columna común
# by = "...1" indica que esa es la clave de unión

HNANESI <- full_join(HNANESI_subset_X, HNANESI_subset_y, by = "...1")

dim(HNANESI)

# ============================================================================
# PASO 6: Eliminar filas con valores missing (NA)
# ============================================================================

# complete.cases() devuelve TRUE si una fila NO tiene NA
# Filtramos solo esas filas

HNANESI<-HNANESI[complete.cases(HNANESI),]

# ============================================================================
# FUNCIÓN: clasificar variables como numéricas, categóricas u ordinales
# ============================================================================
clasificar_variables <- function(df) {
  
  # Paso 1: Obtener los nombres de todas las columnas del dataframe
  
  vars <- names(df) 
  
  # Paso 2: Identificar variables numéricas
  # is.numeric o is.integer → detecta
  
  vars_numericas <- vars[sapply(df, function(x) is.numeric(x) || is.integer(x))]
  
  # Paso 3: Identificar variables ordinales
  # is.ordered → factores con orden (ej: bajo < medio < alto)
  
  vars_ord       <- vars[sapply(df, function(x) is.ordered(x))]
  
  # Paso 4: Identificar variables categóricas
  # factor NO ordenado o character
  
  vars_cat       <- vars[sapply(df, function(x) {
    (is.factor(x) && !is.ordered(x)) || is.character(x)
  })]
  
  # Paso 5: Crear resumen en formato tabla (tibbl
  
  tibble::tibble(
    variable = vars,
    
  # class() devuelve el tipo en R (numeric, factor, etc.)
    
    clase_R  = sapply(df, function(x) paste(class(x), collapse = ", ")),
    
  # Clasificación final en 4 tipos
    
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
# FUNCIÓN: resumen detallado de variables numéricas
# ============================================================================

resumen_numericas <- function(df) {
  
  # Paso 1: Clasificar variables
  
  info <- clasificar_variables(df)
  
  # Paso 2: Filtrar solo las variables numéricas
  
  vars_num <- info %>% 
    dplyr::filter(tipo == "NUMÉRICA") %>% 
    dplyr::pull(variable)
  
  # Paso 3: Control de caso sin variables numérica
  
  if (length(vars_num) == 0) {
    message("No se encontraron variables numéricas.")
    return(NULL)
  }
  
  # Paso 4: Calcular estadísticas para cada variable
  
  purrr::map_dfr(vars_num, function(v) {
    
  # Extraer variable  
    
    x <- df[[v]]
    
  # Convertir a numérico (por seguridad)
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
# FUNCIÓN: resumen detallado de variables categóricas
# ============================================================================

resumen_categoricas <- function(df, max_niveles_mostrar = 20, top_n = 5) {
  
  
  # Paso 1: Clasificar todas las variables del dataset
  # Esto devuelve una tabla con el tipo de cada variable
  
  info <- clasificar_variables(df)
  
  # Paso 2: Filtrar únicamente las variables categóricas
  # Nos quedamos con los nombres de esas variable
  
  vars_cat <- info %>% 
    dplyr::filter(tipo == "CATEGÓRICA") %>% 
    dplyr::pull(variable)
  
  
  # Paso 3: Control de errores
  # Si no se encuentran variables categóricas, avisamos y salimo
  
  if (length(vars_cat) == 0) {
    message("No se encontraron variables categóricas.")
    return(NULL)
  }
  
  
  # Paso 4: Recorrer cada variable categórica y calcular su resumen
  # map_dfr → aplica la función a cada variable y une los resultados en un datafra
  
  purrr::map_dfr(vars_cat, function(v) {
    
    # Paso 4.1: Extraer la variable del dataframe
    
    x <- df[[v]]
    
    # Paso 4.2: Convertir la variable a factor
    # IMPORTANTE:
    # - Esto asegura que podemos trabajar con niveles
    # - Reinterpreta la variable como categórica si venía 
    
    x_fac <- as.factor(x)
    
    # Paso 4.3: Obtener los niveles de la variable
    # levels() devuelve todas las categorías posibles
    
    levs <- levels(x_fac)
    
    
    # Paso 4.4: Limitar el número de niveles a mostrar
    # Si hay demasiadas categorías, evitamos imprimirlas to
    
    if (length(levs) > max_niveles_mostrar) {
      levs_mostrar <- c(levs[1:max_niveles_mostrar], "...(truncado)")
    } else {
      
      # Si no hay demasiados niveles, los mostramos todos
      
      levs_mostrar <- levs
    }
    
    # Paso 4.5: Calcular frecuencias de cada categoría
    # table() cuenta ocurrencias
    
    freq <- sort(table(x_fac), decreasing = TRUE)
    
    # Paso 4.6: Obtener las categorías más frecuentes (top N)
    
    top_freq <- head(freq, top_n)
    
    # Paso 4.7: Construir un texto resumen de las categorías top
    # Ejemplo: "Male (100); Female (80)"
    
    top_str <- paste0(names(top_freq), " (", as.integer(top_freq), ")", collapse = "; ")
    
    # Nombre de la variable
    
    tibble::tibble(
    
      # Nombre de la variable  
      
      variable       = v,
      
      # Número total de observaciones
      
      n_total        = length(x_fac),
      
      # Número de valores NA (faltantes)
      
      n_na           = sum(is.na(x_fac)),
      
      # Número de valores válidos
      
      n_valid        = sum(!is.na(x_fac)),
      
      # Número de niveles distintos
      
      n_niveles      = length(levs),
      
      # Muestra de niveles (limitada si hay muchos)
      
      niveles_muestra= paste(levs_mostrar, collapse = ", "),
      
      # Top categorías con frecuencia
      
      top_categorias = top_str
    )
  })
}


# ============================================================================
# FUNCIÓN: resumen completo de variables (numéricas + categóricas)
# ============================================================================

resumen_rangos_completo <- function(df) {
  
  
  # IMPORTANTE:
  # En este punto del flujo NO incluimos variables ordinales porque:
  #
  # - R no identifica correctamente las variables ordinales automáticamente
  # - Muchas variables ordinales vienen codificadas como numeric o factor sin orden
  # - La clasificación correcta de ordinales se hará más adelante de forma manual
  #
  # Por este motivo, aquí SOLO trabajamos con:
  #   - Variables numéricas
  #   - Variables categóricas
  
  
  list(
    
    # Resumen estadístico de variables numéricas
    
    numericas   = resumen_numericas(df),
    
    # Resumen de variables categóricas
    
    categoricas = resumen_categoricas(df)
  )
}


# ============================================================================
# EJECUCIÓN DE LA FUNCIÓN
# ============================================================================

# Generamos el listado completo de resúmenes


res_rangos <- resumen_rangos_completo(HNANESI)

# 1) Rangos numéricos
View(res_rangos$numericas)

# 2) Rango de categóricas (niveles y top categorías)
View(res_rangos$categoricas)


# NOTA:
# No hay salida de ordinales en este punto del análisis
# porque se tratarán posteriormente en la codificación



# ============================================================================
# FUNCIÓN: construir variable de riesgo de presión arterial
# ============================================================================


codificar_risk_bp <- function(df,
                              var_sys,
                              var_dias,
                              nombre_risk_num = "Risk_BP",
                              nombre_risk_cat = "Risk_BP_cat") {
  
  # --------------------------------------------------------------------------
  # PASO 1: Comprobaciones básicas
  # --------------------------------------------------------------------------
  
  # Verificamos que la variable sistólica existe
  
  if (!(var_sys %in% names(df))) {
    stop("La variable sistólica '", var_sys, "' no existe en el data.frame.")
  }
  
  # Verificamos que la variable diastólica existe
  
  if (!(var_dias %in% names(df))) {
    stop("La variable diastólica '", var_dias, "' no existe en el data.frame.")
  }
  
  
  # --------------------------------------------------------------------------
  # PASO 2: Extraer variables y asegurar tipo numérico
  # --------------------------------------------------------------------------
  
  # Convertimos a numérico por si vienen como factor/character
  
  sys <- as.numeric(df[[var_sys]])
  dias <- as.numeric(df[[var_dias]])
  
  
  # --------------------------------------------------------------------------
  # PASO 3: Inicializar vectores de riesgo
  # --------------------------------------------------------------------------
  
  # Creamos vectores vacíos con NA para almacenar clasificación
  
  risk_sys  <- rep(NA_integer_, length(sys))
  risk_dias <- rep(NA_integer_, length(dias))
  
  
  # --------------------------------------------------------------------------
  # PASO 4: Clasificación de presión SISTÓLICA
  # --------------------------------------------------------------------------
  
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
  
  
  # --------------------------------------------------------------------------
  # PASO 5: Clasificación de presión DIASTÓLICA
  # --------------------------------------------------------------------------
  
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
  
  
  # --------------------------------------------------------------------------
  # PASO 6: Construcción del riesgo combinado
  # --------------------------------------------------------------------------
  
  # Tomamos el máximo entre sistólica y diastólica
  # (criterio clínico: el peor valor domina)
  
  
  risk_comb <- pmax(risk_sys, risk_dias, na.rm = TRUE)
  risk_comb[is.infinite(risk_comb)] <- NA_integer_
  
  
  # --------------------------------------------------------------------------
  # PASO 7: Añadir variable numérica al dataset
  # --------------------------------------------------------------------------
  
  df[[nombre_risk_num]] <- as.integer(risk_comb)
  
  
  # --------------------------------------------------------------------------
  # PASO 8: Crear variable categórica ordinal
  # --------------------------------------------------------------------------
  
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
  
  
  # --------------------------------------------------------------------------
  # PASO 9: Devolver dataframe actualizado
  # --------------------------------------------------------------------------
  
  return(df)
}

# Aplicamos la función al dataset


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
# PASO 3: Crear variable categórica SEX_N
# ===========================================================================

HNANESI$SEX_N <- ifelse(HNANESI$Sex == 1, "Male",
                        ifelse(HNANESI$Sex == 2, "Female", NA))


# ============================================================================
# PASO 4: Reclasificación general de variables
# ============================================================================

resultado_tipos <- clasificar_variables(HNANESI)

# ============================================================================
# PASO 5: Crear variable objetivo binaria
# ============================================================================

HNANESI$y_booleana <- ifelse(HNANESI$y <= 0, "0",
                             ifelse(HNANESI$y > 0, "1", NA))


################################################################################
################## Aplicación al Real Case        ##############################
################################################################################


# ============================================================================
# PASO 1: Ver nombres de variables disponibles
# ============================================================================

# Esto nos permite comprobar que los nombres coinciden antes de seleccionar

names(HNANESI)


# ============================================================================
# PASO 2: Definir variables que formarán el dataset final
# ============================================================================


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
# PASO 3: Seleccionar variables del dataset original
# ============================================================================


seleccionar_variables <- function(df, vars) {
  
  # select(all_of(...)) selecciona solo las variables indicadas
  # Si alguna no existe, lanza error 
  
  
  df %>% dplyr::select(all_of(vars))
}

# Dataset final con variables seleccionadas

HNANESI_F <- seleccionar_variables(HNANESI, vars_seleccionadas)


# ============================================================================
# PASO 4: Funciones de codificación de variables
# ============================================================================

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
# PASO 5: Definir manualmente tipos de variables (CORREGIDO)
# ============================================================================

# IMPORTANTE: nombres deben coincidir EXACTAMENTE con el dataset

# Variables numéricas

num_vars  <- c("Edad", "Serum Cholesterol","y")                   # Variables numéricas

nom_vars <- c(  "Race",  "SEX_N",  "y_booleana")                  # Variables categóricas nominales
# nominales
ord_vars  <- c( "Risk_BP_cat")                                    # Variables ordinales    


# ============================================================================
# PASO 6: Función de diagnóstico de variables
# ============================================================================


clasificar_variables_unico <- function(df, max_unique = 20) {
 
  # map_df recorre cada variable y construye un dataframe resumen
   
  map_df(names(df), function(v) {
    
    # Extraer variable
    
    x <- df[[v]]
    
    # Clase real en R
    
    clase <- class(x)
    
    # Número de valores missing
    
    n_na  <- sum(is.na(x))
    
    # Clasificación automática
    
    tipo <- case_when(
      is.numeric(x) || is.integer(x) ~ "NUMERICA",
      is.ordered(x)                  ~ "ORDINAL",
      is.factor(x)                   ~ "CATEGORICA",
      is.character(x)                ~ "CATEGORICA",
      TRUE                           ~ "OTRA"
    )
    
    # Resumen de valores
    valores <- if (is.numeric(x)) {
      paste0("min=", min(x, na.rm=TRUE), ", max=", max(x, na.rm=TRUE))
    } else {
      u <- unique(x)
      paste(head(u, 20), collapse = ", ")
    }
    
    # Construcción del resultado
    
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
# PASO 7: Ejecutar diagnóstico del dataset final
# ============================================================================


tabla_codificacion <- clasificar_variables_unico(HNANESI_F)

# Visualizar resultados

View(tabla_codificacion)

# Ver nombres finales del dataset

names(HNANESI_F)


####### Guardar archivo e importar datos

library(crayon)

str(HNANESI_F)

save(HNANESI_F, file =  "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/BBDD HNANES I/HNAMES_F.rds")

#######  Importar

HNANESI_F<-readRDS("C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/BBDD HNANES I/HNAMES_F.rds")

class(HNANESI_y$Risk_BP)


# ============================================================================
# PASO: Fijar semilla aleatoria para reproducibilidad
# ============================================================================

# set.seed() fija el punto de inicio del generador de números aleatorios
# Esto significa que cualquier operación aleatoria dará siempre el MISMO resultado


set.seed(123)   # Para poder reproducir la selección


# ============================================================================
# FUNCIÓN: Normalizar nombres de columnas a 3 letras
# ============================================================================


normalizar_a_tres_letras <- function(df) {
  
  
  # --------------------------------------------------------------------------
  # PASO 1: Verificación de entrada
  # --------------------------------------------------------------------------
  
  # stopifnot() detiene la ejecución si la condición no se cumple
  # Aquí verificamos que df es un data.frame
  
  stopifnot(is.data.frame(df))
  
  # --------------------------------------------------------------------------
  # PASO 2: Obtener nombres originales de las columnas
  # -------------------------------------------------------------------
  
  nombres_originales <- names(df)
  
  # Ejemplo:
  # "Age", "Serum Cholesterol", "Risk_BP_cat"
  
  # --------------------------------------------------------------------------
  # PASO 3: Crear nombres cortos (primeros 3 caracteres)
    # Ejemplo:
  # "Age"                -> "Age"
  # "Serum Cholesterol" -> "Ser"
  # "Risk_BP_cat"      -> "Ris" 
  # -----------------------------------------------------------------------
  
  nombres_cortos     <- substring(nombres_originales, 1, 3)
  
  
  # --------------------------------------------------------------------------
  # PASO 4: Control crítico de seguridad (colisiones)
  # --------------------------------------------------------------------------
  
  # duplicated() detecta si hay nombres repetidos
  # Esto es MUY IMPORTANTE porque:
  # - Dos variables podrían tener mismo prefijo
  # - Ejemplo: "Age" y "Agent" → ambas "
  
  if (any(duplicated(nombres_cortos))) {
    stop(
      "Colisión al normalizar nombres a 3 letras: ",
      paste(unique(nombres_cortos[duplicated(nombres_cortos)]), collapse = ", ")
    )
  }
  
  
  # --------------------------------------------------------------------------
  # PASO 5: Asignar nuevos nombres al dataframe y devolverlos
  # --------------------------------------------------------------------
  
  names(df) <- nombres_cortos
  df
}


#############################################################################
############### Preparación final dataset para:
#               Medida difusa
#               Modelos I y IV
#               Valor de Shapley
#               Representaciones gráficas
############################################################################

preparar_HNANESI <- function(df) {
  
  
  # --------------------------------------------------------------------------
  # PASO 1: Asegurar formato data.frame
  # --------------------------------------------------------------------------
  
  # Convertimos a data.frame estándar (por si viene como tibble)
  
  df <- as.data.frame(df)
  
  
  # --------------------------------------------------------------------------
  # PASO 2: Normalización de nombres de columnas
  # --------------------------------------------------------------------------
  
  # Reemplazamos espacios por "_" para evitar problemas en R
  
  names(df) <- gsub(" ", "_", names(df))  # Por si acaso espacios
  
  # Renombramos variables clave para mayor claridad y consistencia
  
  names(df)[names(df) == "Serum_Cholesterol"] <- "Cholesterol"
  names(df)[names(df) == "Risk_BP_cat"] <- "Risk_BP"
  names(df)[names(df) == "SEX_N"] <- "Sex"
  
  
  # --------------------------------------------------------------------------
  # PASO 3: Conversión de tipos 
  # --------------------------------------------------------------------------
  
  # 3.1 Race → variable categórica nominal
  
  df$Race <- factor(df$Race)
  
  
  # 3.2 Risk_BP → variable ordinal (muy importante)
  
  # Si no está como ordered factor, la convertim
  
  if (!is.ordered(df$Risk_BP)) {
    
  # Convertimos a character para evitar problemas
    
    niveles_bp <- unique(as.character(df$Risk_BP))
    
    # Creamos factor ordenado
    # IMPORTANTE:
    # Aquí el orden depende del sort → puede NO ser el clínico real
    
    
    df$Risk_BP <- factor(df$Risk_BP,
                         levels = sort(unique(niveles_bp)),
                         ordered = TRUE)
  }
  
  # 3.3 Sex → categórica nominal
  
  df$Sex <- factor(df$Sex)
  
  # 3.4 y → variable numérica (target regresión)
  
  df$y <- as.numeric(df$y)
  
  
  # --------------------------------------------------------------------------
  # PASO 4: Normalización del target de clasificación (y_b)
  # --------------------------------------------------------------------------
  
  # Caso 1: existe y_booleana → lo renombramos a y_b
  
  if ("y_booleana" %in% names(df)) {
    df$y_b <- df$y_booleana
    df$y_booleana <- NULL
  }
  
  # Caso 2: ya existe y_b
  
  if ("y_b" %in% names(df)) {
    df$y_b <- factor(df$y_b, levels = c("0", "1"))
  } else {
    stop("No se encuentra el target de clasificación (y_b ni y_booleana).")
  }
  
  
  # --------------------------------------------------------------------------
  # PASO 5: DEPURACIÓN DEL DATASET
  # --------------------------------------------------------------------------
  
  # 5.1 Analizar valores missing (NO se eliminan todavía)
  
  na_count <- colSums(is.na(df))
  if (any(na_count > 0)) {
    message("⚠️  Atención: la base contiene NA. Revisa:")
    print(na_count[na_count > 0])
  }
  
  # 5.2 Detectar y eliminar duplicados
  
  n_dupls <- sum(duplicated(df))
  if (n_dupls > 0) {
    message("⚠️  Eliminando filas duplicadas: ", n_dupls)
    df <- df[!duplicated(df), ]
  }
  
  # 5.3 Detectar columnas constantes (sin variabilidad)
  
  const_cols <- names(df)[sapply(df, function(x) length(unique(x)) == 1)]
  if (length(const_cols) > 0) {
    message("⚠️  Columnas constantes eliminadas: ", paste(const_cols, collapse = ", "))
    df <- df[, !(names(df) %in% const_cols)]
  }
  
  # 5.4 Detectar columnas con varianza cero
  
  zero_var <- names(df)[sapply(df, function(x) var(as.numeric(x), na.rm = TRUE) == 0)]
  if (length(zero_var) > 0) {
    message("⚠️  Columnas con varianza cero: ", paste(zero_var, collapse = ", "))
  }
  
  
  # --------------------------------------------------------------------------
  # PASO 6: Validación final
  # --------------------------------------------------------------------------
  
  # Mostramos estructura final del dataset
  
  str(df)
  
  message("✅ Base HNANESI depurada y lista.")
  return(df)
}


####### Muestra pruebas
set.seed(123)
HNANESI_F2 <- HNANESI_F[sample(nrow(HNANESI_F), 20), ] #Quitar

HNANESI_clean <- preparar_HNANESI(HNANESI_F) # Depuración 

HNANESI_clean <- normalizar_a_tres_letras(HNANESI_clean) # Normalización a tres letras

names(HNANESI_clean)


