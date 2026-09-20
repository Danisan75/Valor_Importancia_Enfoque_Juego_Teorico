
################################################################################
# SCRIPT COMPLETO “μ” 
################################################################################


# ============================================================================
# 1. CÁLCULO DEL ERROR CUADRÁTICO MEDIO DEL VACIO
# ============================================================================
#
# Objetivo:

# Calcular el Error Cuadrático Medio al predecir vj de manera aleatoria (ECM) 


calcular_ECM_vacio <- function(y){

  mean((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
}


# ============================================================================
# 2. CÁLCULO DE LA MEDIDA DIFUSA μ
# ============================================================================
#
# Objetivo:
# Calcular la medida difusa μ, que mide la capacidad predictiva de las variables 
# en S respecto a la aleatoridad (vacío) en el dataset.

# Fórmula:
# μ = (ECM_Vacio - ECM_S) / ECM_Vacio
#
# Donde:
# - ECM_Vacio = error al predecir vj de manera aleatoria
# - ECM_S     = error al predecir vj con las variables de S
#
# ============================================================================


calcular_Mu <- function(ECM_Vacio, ECM_S, tol = 1e-12){

  if (is.na(ECM_Vacio) || ECM_Vacio == 0) return(NA_real_)  # Evita división por 0 o NA
  if (is.na(ECM_S)) return(NA_real_)
  
  raw <- (ECM_Vacio - ECM_S) / ECM_Vacio  # μ
  
  if (is.na(raw)) return(NA_real_)
  if (abs(raw) < tol) raw <- 0            # Limpieza de ruido flotante alrededor de 0
  
  max(0, min(1, raw))                  
}

# ============================================================================
# 3. DETECCIÓN DE VARIABLES BINARIAS
# ============================================================================
#
# Objetivo:
# Detectar si una variable es binaria (tiene solo dos valores posibles).
#
# Tipologías identificadas:
# - logical (TRUE/FALSE)
# - factor
# - character
# - numeric
#
# Criterio:
# - TRUE si tiene exactamente 2 valores distintos.
# - En numéricos también tratamos el caso 0/1.
# - FALSE en cualquier otro caso.
#
# ============================================================================


es_binaria_generica <- function(x){

  vals <- x[!is.na(x)]
  if (length(vals) == 0) return(FALSE)
  if (is.logical(vals)) return(TRUE)
  if (is.factor(vals) || is.character(vals)) return(length(unique(vals)) == 2)
  if (is.numeric(vals)){
    u <- sort(unique(vals))
    return(length(u) == 2 || all(u %in% c(0,1)))
  }
  FALSE
}

# ============================================================================
# 4. NORMALIZACIÓN DE VARIABLES BINARIAS
# ============================================================================
#
# Objetivo:
# Convertir una variable binaria a formato numérico 0/1 
#
# Tipologías identificadas:
# - logical    → FALSE = 0, TRUE = 1
# - factor     → se ordenan los niveles y el segundo es 1
# - character  → igual que factor
# - numeric    → si ya es 0/1 se mantiene; si no, el mayor valor será 1
#
# Criterio:
# - Siempre devuelve un vector numérico con valores 0 y 1.
# - En variables con 2 categorías:
#     menor → 0
#     mayor → 1
#
# ============================================================================



normalizar_binaria <- function(x){
  # Convierte una binaria a 0/1 sin ambigüedad.
  vals <- x[!is.na(x)]
  if (is.logical(vals)) return(as.numeric(x))  # FALSE=0, TRUE=1
  
  if (is.factor(x) || is.character(x)){
    # Para factores/char: define 1 como el "segundo" nivel al ordenar alfabéticamente
    u <- sort(unique(as.character(x)))
    if (length(u) != 2) stop("normalizar_binaria: no es binaria")
    return(as.numeric(as.character(x) == u[2]))
  }
  
  if (is.numeric(vals)){
    u <- sort(unique(vals))
    if (all(u %in% c(0,1))) return(as.numeric(x))
    if (length(u) == 2) return(ifelse(x == u[2], 1, 0))
  }
  
  stop("normalizar_binaria: no es binaria")
}

# ============================================================================
# 5. CONSTRUCCIÓN DE FACTORES ORDINALES
# ============================================================================
#
# Objetivo:
# Convertir una variable en un factor ordenado asegurando un orden consistente de los niveles.
#
# Comportamiento:
# - Caso "Risk_BP": aplica un orden predefinido para asegurar el tratamiento correcto.
#
# Input:
# - dataset
#
# Output:
# - Factor ordenado (ordered = TRUE).
#
#
# ============================================================================

construir_factor_ordinal <- function(x0, var_name){

  if (var_name == "Risk_BP"){
    orden_bp <- c("Optima", "Normal", "Elevada", "Hipertension_1", "Hipertension_2")
    return(factor(as.character(x0), levels = orden_bp, ordered = TRUE))
  }
  if (is.ordered(x0)) return(x0)
  factor(x0, levels = unique(x0), ordered = TRUE)
}


# ============================================================================
# 6. PREPROCESAMIENTO DE VARIABLES EXPLICATIVAS (INDEPENDIENTES)
# ============================================================================
#
# Objetivo:
# Preprocesamiento de las variables y creación de variables dummy.
#
# Tratamiento aplicado:
#
# Variables numéricas:
# - Se convierten a formato numérico.
#
# Variables categóricas (sin orden):
# - Se convierten a factor.
# - Se generan variables dummy (0/1), una por cada categoría existente.
#
# Ejemplo interpretación:
# Race = 1, 2, 3
#
# Se generan:
# Race_1
# Race_2
# Race_3
#
# Si la variable tiene k categorías:
# - Se generan k variables dummy, una por categoría.
#
# Variables ordinales (con orden):
# - Se convierten a factor ordenado.
# - Se respeta el orden de las categorías.
# - Se generan variables dummy acumulativas (0/1).
#
# Ejemplo interpretación:
# Risk_BP = Optima, Normal, Elevada, Hipertension_1, Hipertension_2
#
# Se generan:
# Risk_BP_Normal
# Risk_BP_Elevada
# Risk_BP_Hipertension_1
# Risk_BP_Hipertension_2
#
# Interpretación:
# - Risk_BP_Elevada = 1 indica que el orden es igual a Elevada o superior.
# - Risk_BP_Hipertension_1=1 indica que el orden es igual a Hipertesion_1 o superior
# - Risk_BP_Hipertension_2=1 indica que el orden es igual a Hipertesion_2 
#
# Si la variable tiene k categorías:
# - Se generan k-1 variables dummy acumulativas, una menos que k, la inferior.
#
# Resultado:
# - Dataset preprocesado.
#
# ============================================================================

preprocesar_X_num_card_ord <- function(data,
                                       vars_numeric,
                                       vars_cardinal,
                                       vars_ordinal){
  
  df <- data  
  
  ## 1) Numéricas:
  for (v in vars_numeric){
    if (v %in% names(df)) df[[v]] <- as.numeric(df[[v]])
  }
  
  ## 2) Cardinales (nominales)
  for (v in vars_cardinal){
    if (!v %in% names(df)) next
    
    x <- as.factor(df[[v]])          # Asegura factor
    df[[v]] <- x
    
    levs <- levels(x)
    if (length(levs) > 0){
      mm <- stats::model.matrix(~ x - 1)            
      colnames(mm) <- paste0(v, "_", levs)         
      df <- cbind(df, as.data.frame(mm))            
    }
  }
  
  ## 3) Ordinales: 
  for (v in vars_ordinal){
    if (!v %in% names(df)) next
    
    x <- construir_factor_ordinal(df[[v]], v)        
    df[[v]] <- x
    
    levs <- levels(x)
    k    <- length(levs)
    code <- as.integer(x)                           
    
    # Crea k-1 dummies: v_<nivel_j> = 1 si code >= j
    # - nivel mínimo => todas 0
    # - nivel máximo => todas 1
    for (j in 2:k){
      df[[paste0(v, "_", levs[j])]] <- as.integer(code >= j)
    }
  }
  
  df
}


# ============================================================================
# 7. CONSTRUCCIÓN DE BLOQUES DE VARIABLES EXPLICATIVAS
# ============================================================================
#
# Objetivo:
# Agrupar las variables dummy a cada variable original para los
# cálculos posteriores de ECM, μ y contribución de variables.
#
# Estructura de los bloques:
#
# Variables numéricas:
# - El bloque está formado por una única columna.
#
# Ejemplo:
# Age
#
# Variables categóricas (sin orden):
# - El bloque está formado por todas las variables dummy generadas a
#   partir de la variable original durante el prepocesamiento
#
# Ejemplo:
# Race
#
# Bloque:
# Race_1
# Race_2
# Race_3
#
# Variables ordinales (con orden):
# - El bloque está formado por todas las variables dummy acumulativas
#   generadas a partir de la variable original durante el preprocesamiento
#
# Ejemplo:
# Risk_BP
#
# Bloque:
# Risk_BP_Normal
# Risk_BP_Elevada
# Risk_BP_Hipertension_1
# Risk_BP_Hipertension_2
#
# Resultado:
# - Cada variable original queda representada por un único bloque.
# - Los bloques se utilizarán posteriormente para el cálculo de ECM y μ
#
# ============================================================================


crear_bloques_X_num_card_ord <- function(datos_proc,
                                         vars_numeric,
                                         vars_cardinal,
                                         vars_ordinal){
  L <- list()
  
  # Numéricas: bloque = 1 columna
  for (v in vars_numeric){
    if (v %in% names(datos_proc)) L[[v]] <- v
  }
  
  # Cardinales: bloque = conjunto de columnas v_*
  for (v in vars_cardinal){
    cols <- grep(paste0("^", v, "_"), names(datos_proc), value = TRUE)
    cols <- intersect(cols, names(datos_proc))
    if (length(cols) > 0) L[[v]] <- cols
  }
  
  # Ordinales: bloque = conjunto de columnas v_*
  for (v in vars_ordinal){
    cols <- grep(paste0("^", v, "_"), names(datos_proc), value = TRUE)
    cols <- intersect(cols, names(datos_proc))
    if (length(cols) > 0) L[[v]] <- cols
  }
  
  L
}


# ============================================================================
# 8. GENERACIÓN DE COALICIONES DE VARIABLES (S)
# ============================================================================
#
# Objetivo:
# Generar todas las combinaciones posibles de variables de nuestra BBDD (S).
#
# Concepto:
# - Una coalición es un subconjunto de variables (S).
# - Cada coalición será necesaria para calcular el ECM(S) y μ.
#
# Coaliciones generadas:
# - Coalición vacía.
# - Coaliciones con una variable.
# - Coaliciones con dos variables.
# - ...
# - Coalición completa con todas las variables.
#
# Ejemplo:
# Si las variables son:
# A, B, C
#
# Se generan:
# empty
# A
# B
# C
# A+B
# A+C
# B+C
# A+B+C
#
# Resultado:
# - Generación de todas las coaliciones posibles hasta el tamaño máximo
#  de variables de la BBDD
#
# ============================================================================


generar_coaliciones <- function(vars_X, max_size = NULL){
  vars_X <- as.character(vars_X)
  p <- length(vars_X)
  
  if (is.null(max_size)) max_size <- p else max_size <- min(max_size, p)
  
  out <- list()
  out[["empty"]] <- character(0)  # Coalición vacía
  idx <- 2
  
  if (p > 0 && max_size >= 1){
    for (k in 1:max_size){
      cs <- combn(vars_X, k, simplify = FALSE)       # Combinaciones de tamaño k
      for (S in cs){
        out[[idx]] <- S
        names(out)[idx] <- paste(S, collapse = "+")  # Clave canónica
        idx <- idx + 1
      }
    }
  }
  
  out
}


# ============================================================================
# 9. CÁLCULO DEL ECM PARA VARIABLES NUMERICAS Y CADA COALICIÓN DE VARIABLES (S)
# ============================================================================
#
# Objetivo:
# Calcular el modelo de regresión lineal utilizando las variables de una
# coalición y calcular su Error Cuadrático Medio (ECM), esto se hace para todas
# las coaliciones.
#
# Criterio de ajuste:
# - Como mejores modelos se han utilizado los que minimizan el Error Cuadrático
#   Medio para la coalición analizada al ser pocas las variables utilizadas. 
#   Podrían utilizarse procedimientos de selección de variables
#   (stepwise, backward, forward, etc.).
# - En este caso se utiliza directamente el ajuste lineal que minimiza
#   el error para las variables incluidas en la coalición.
#
# Tratamiento aplicado:
#
# Modelo vacío:
# - Si la coalición no contiene variables, la predicción tendrá
#   únicamente el intercepto.
#
# Modelos con variables explicativas:
# - Se seleccionan la/s variable/s de la coalición (S).
# - Se calcula el modelo de regresión lineal.
# - El modelo estima los coeficientes que minimizan el error cuadrático
#   para la coalición analizada.
# - Se calculan las predicciones obtenidas.
# - Se calcula el ECM asociado a dicha coalición.
#
# Ejemplo:
# Si la variable objetivo es Age:
#
# ECM(empty)
# ECM(Race)
# ECM(Risk_BP)
# ECM(Race + Risk_BP)
#
# Resultado:
# - ECM del modelo asociado a la coalición evaluada.
#
#
# ============================================================================


ajustar_lineal_ECM <- function(datos, y, x_cols, intercept = TRUE){
  
  x_cols <- intersect(x_cols, names(datos))         
  y_vec  <- datos[[y]]
  
 
  if (length(x_cols) == 0){
    return(calcular_ECM_vacio(y_vec))
  }
  

  sub <- datos[, c(y, x_cols), drop = FALSE]
  cc  <- complete.cases(sub)
  if (!any(cc)) return(NA_real_)
  
  y_cc <- sub[[y]][cc]
  X_cc <- as.matrix(sub[cc, x_cols, drop = FALSE])
  
 
  if (intercept){
    X_cc <- cbind("(Intercept)" = 1, X_cc)
  }
  
  fit <- lm.fit(X_cc, y_cc)                          
  
  # Si hay colinealidad: coef NA -> 0 para poder predecir
  coefs <- fit$coefficients
  coefs[is.na(coefs)] <- 0
  
  y_hat <- as.vector(X_cc %*% coefs)                 # Predicción
  mean((y_cc - y_hat)^2, na.rm = TRUE)               # ECM
}


# ============================================================================
# 10. CONSTRUCCIÓN DE VARIABLES DUMMY PARA TARGETS DISCRETOS
# ============================================================================
#
# Objetivo:
# Transformar variables objetivo categóricas en variables dummy (0/1)
# que permitan analizar individualmente cada categoría de la variable
# original y construir posteriormente el ECM, la variable difusa para cada
# variable dummy y para la variable original asociada.
#
# Variables transformadas:
# - Variables categóricas nominales.
# - Variables categóricas ordinales.
#
# Transformaciones aplicadas:
#
# Targets nominales:
# - Se genera una variable dummy para cada categoría.
#
# Ejemplo:
# Race
#
# Variables objetivo generadas:
# Y_Race_White
# Y_Race_Black
# Y_Race_Other
#
# Targets ordinales:
# - Se generan variables dummy acumulativas respetando el orden de los
#   niveles (k-1).
#
# Ejemplo:
# Risk_BP
#
# Variables objetivo generadas:
# Y_Risk_BP_Elevada
# Y_Risk_BP_Hipertension_1
# Y_Risk_BP_Hipertension_2
#
# Metodología:
#
# - Cada variable objetivo discreta se transforma en un conjunto de
#   variables dummy binarias.
#
# - Para variables nominales se genera una dummy independiente para cada
#   categoría.
#
# - Para variables ordinales se generan dummies acumulativas asociadas a
#   umbrales crecientes de la variable original.
#
# - Para cada dummy se calcula:
#
#     ws_base = min(#1, #0)
#
#   donde:
#
#     #1 = número de observaciones con valor 1
#     #0 = número de observaciones con valor 0
#
# - Este peso se utilizará posteriormente en la agregación
#   para el calculo de la variable difusa asociada a la variable
#   objetivo original.
#
# Resultado:
# - Dataset con las variables dummy añadidas.
# - Tabla auxiliar con los pesos asociados a cada dummy.
#
# ============================================================================


crear_dummies_targets_discretas <- function(datos,
                                            vars_cardinal_targets,
                                            vars_ordinal_targets){
  
  out <- datos
  W   <- data.frame()
  
  ## A) Targets cardinales (nominales): 
  for (v in vars_cardinal_targets){
    if (!v %in% names(out)) next
    
    xf <- as.factor(out[[v]])
    levs   <- levels(xf)
    levs_s <- make.names(levs)                       
    
    for (i in seq_along(levs)){
      nom <- paste0("Y_", v, "_", levs_s[i])        
      dum <- as.integer(xf == levs[i])             
      
      out[[nom]] <- dum
      
      # Peso base por equilibrio: min(#1,#0)
      w <- min(sum(dum == 1, na.rm = TRUE),
               sum(dum == 0, na.rm = TRUE))
      
      W <- rbind(W, data.frame(
        dummy        = nom,
        var_original = v,
        categoria    = levs[i],
        tipo_dummy   = "cardinal_onehot",
        ws_base      = w,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  ## B) Targets ordinales: dummies acumulativas (umbrales ≥ nivel j), j=2..k
  for (v in vars_ordinal_targets){
    if (!v %in% names(out)) next
    
    x_ord <- construir_factor_ordinal(out[[v]], v)   # Orden consistente
    out[[v]] <- x_ord
    
    levs <- levels(x_ord)
    k    <- length(levs)
    code <- as.integer(x_ord)                        # 1..k
    
    # Crea k-1 dummies umbral: Y_v_<nivel_j> = 1 si code >= j
    for (j in 2:k){
      nom <- paste0("Y_", v, "_", make.names(levs[j]))
      dum <- as.integer(code >= j)
      out[[nom]] <- dum
      
      w <- min(sum(dum == 1, na.rm = TRUE),
               sum(dum == 0, na.rm = TRUE))
      
      W <- rbind(W, data.frame(
        dummy        = nom,
        var_original = v,
        categoria    = levs[j],                      # Umbral asociado
        tipo_dummy   = "ordinal_cumulative",
        ws_base      = w,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  list(datos = out, pesos = W)
}




# ============================================================================
# 11. CÁLCULO DEL ECM PARA VARIABLES DUMMY Y UNA COALICIÓN DE VARIABLES (S)
# ============================================================================
#
# Objetivo:
# Evaluar la capacidad predictiva de una coalición de variables
# explicativas sobre cada variable dummy mediante un modelo de
# regresión logística.
#
# Variables objetivo:
# - Las variables objetivo utilizadas en esta fase son variables dummy
#   generadas previamente a partir de variables categóricas nominales
#   u ordinales.
#
# Ejemplos:
#
# Variables dummy asociadas a Race:
# - Y_Race_White
# - Y_Race_Black
# - Y_Race_Other
#
# Variables dummy asociadas a Risk_BP:
# - Y_Risk_BP_Elevada
# - Y_Risk_BP_Hipertension_1
# - Y_Risk_BP_Hipertension_2
#
# Metodología:
#
# - Cada variable dummy se analiza de forma independiente.
#
# - Para cada coalición S de variables explicativas se ajusta un modelo
#   de regresión logística binaria.
#
# - Si la coalición es vacía (S = ∅), se utiliza un modelo formado
#   únicamente por el intercepto.
#
#
# - La capacidad predictiva de la coalición S se mide mediante el Error
#   Cuadrático Medio (ECM):
#
#     ECMi(S) = (1/m) Σ[(di)k − fci(xk)]²
#
#   donde:
#
#     (di)k  = valor observado de la dummy en la observación k
#     fci(xk) = probabilidad estimada por el modelo para la observación k
#     m       = número de observaciones utilizadas en el ajuste
#
#
# Resultado:
# - ECM asociado a cada variable dummy y a cada coalición evaluada.
#
# - Estos valores se utilizarán posteriormente para calcular la medida μ
#   correspondiente a cada coalición.
#
# ============================================================================

ajustar_logit_ECM <- function(datos, y, x_cols){
  
  # 1) Validación: el target debe ser binario
  if (!es_binaria_generica(datos[[y]]))
    stop(paste0("ajustar_logit_ECM: ", y, " no es binaria."))
  
  # 2) Normalización a 0/1
  yb <- normalizar_binaria(datos[[y]])
  x_cols <- intersect(x_cols, names(datos))
  
  # 3) Modelo vacío: solo intercepto 
  #    ECM = mean((1 - P(correcto))^2) 
  if (length(x_cols) == 0){
    p0 <- mean(yb, na.rm = TRUE)
    PC <- ifelse(yb == 1, p0, 1 - p0)
    return(mean((1 - PC)^2, na.rm = TRUE))
  }
  
  # 4) Filas completas
  sub <- datos[, c(y, x_cols), drop = FALSE]
  sub[[y]] <- yb
  cc <- complete.cases(sub)
  if (!any(cc)) return(NA_real_)
  
  d <- sub[cc, , drop = FALSE]
  
  # 5) Chequeo matemático de predicción perfecta:
  y_cc <- d[[y]]
  X_cc <- as.matrix(d[, x_cols, drop = FALSE])
  if (ncol(X_cc) > 0){
    igual_col <- apply(X_cc, 2, function(col) all(col == y_cc))
    if (any(igual_col)){
      return(0)  # ECM_S=0 => μ=1 si ECM_Vacio>0
    }
  }
  
  # 6) Ajuste glm binomial estándar
  f <- as.formula(paste(y, "~", paste(x_cols, collapse = "+")))
  mod <- suppressWarnings(glm(f, data = d, family = binomial))
  
  pr <- predict(mod, type = "response")              
  PC <- ifelse(d[[y]] == 1, pr, 1 - pr)              
  
  mean((1 - PC)^2, na.rm = TRUE)                    
}




# ============================================================================
# 12. CÁLCULO DE μ PARA VARIABLES TARGET NUMÉRICAS
# ============================================================================
#
# Objetivo:
# Calcular la medida difusa μ para una variable objetivo numérica
# utilizando todas las coaliciones posibles de variables explicativas.
#
# Metodología:
# - Se calcula el ECM del modelo vacío.
# - Para cada coalición S se calcula el ECM asociado al modelo lineal.
# - Se compara ECM(S) frente al ECM vacío.
# - Se obtiene μ(S) de la variable 
#
# Resultado:
# - Valor de μ de cada coalición S.
# - Tabla de resultados asociada a la variable objetivo analizada.
#
# ============================================================================


calcular_Mu_numerica_target <- function(Datos, y, bloques_X, coaliciones){
  
  ECM_Vacio <- calcular_ECM_vacio(Datos[[y]])       # Baseline vacío
  res <- data.frame()
  
  for (S_key in names(coaliciones)){
    S_vars <- coaliciones[[S_key]]
    
    # Traduce variables originales de S a columnas reales de diseño (bloques)
    if (length(S_vars) == 0){
      x_cols <- character(0)
    } else {
      x_cols <- unique(unlist(bloques_X[S_vars]))
    }
    
    # Si y está dentro de S: permitir ECM_S=0 de forma matemática
    if (y %in% S_vars){
      x_cols <- unique(c(x_cols, y))
      use_intercept <- FALSE
    } else {
      use_intercept <- TRUE
    }
    
    ECM_S <- ajustar_lineal_ECM(Datos, y, x_cols, intercept = use_intercept)
    Mu    <- calcular_Mu(ECM_Vacio, ECM_S)
    
    res <- rbind(res, data.frame(
      var_original = y,
      S            = S_key,
      Mu           = Mu,
      tipo_target  = "numerica",
      stringsAsFactors = FALSE
    ))
  }
  
  res
}

# ============================================================================
# 13. CÁLCULO DE μ PARA VARIABLES OBJETIVO CATEGÓRICAS
# ============================================================================
#
# Objetivo:
# Calcular la medida difusa μ para las variables categóricas
# originales (nominales u ordinales) utilizando todas las coaliciones
# posibles de variables explicativas (S).
#
# Metodología:
#
# - Para cada coalición S se dispone de un valor μ asociado a cada
#   variable dummy.
#
# - A cada variable dummy se le asigna un peso en función del equilibrio
#   entre observaciones con valor 1 y valor 0.
#
# - Los pesos se normalizan para que su suma sea igual a 1.
#
# - El valor μ de la variable categórica original se obtiene mediante una
#   agregación ponderada de los valores μ de sus variables dummy.
#
# Resultado:
#
# - Valor de μ para cada variable categórica original y para cada
#   coalición S de variables explicativas.
#
# - Tabla de resultados asociada a la variable objetivo analizada.
#
# ============================================================================

calcular_Mu_discreta_target <- function(Datos, var_name,
                                        bloques_X, coaliciones,
                                        tabla_pesos){
  
  pes  <- tabla_pesos[tabla_pesos$var_original == var_name, ]
  dums <- pes$dummy
  Ws   <- pes$ws_base
  
  res <- data.frame()
  
  for (S_key in names(coaliciones)){
    S_vars <- coaliciones[[S_key]]
    
    # Columnas de predictores según bloques incluidos en S
    if (length(S_vars) == 0){
      x_cols <- character(0)
    } else {
      x_cols <- unique(unlist(bloques_X[S_vars]))
    }
    
    mu_k <- numeric(length(dums))  # μ de cada dummy (categoría o umbral)
    
    for (i in seq_along(dums)){
      yk <- dums[i]                # dummy target (Y_...)
      yv <- Datos[[yk]]
      
      ECM_V <- calcular_ECM_vacio(yv)   # Baseline vacío del dummy (p0)
      if (is.na(ECM_V) || ECM_V == 0){
        mu_k[i] <- 0
      } else {
        ECM_S   <- ajustar_logit_ECM(Datos, yk, x_cols)  # Brier score con logit
        mu_k[i] <- calcular_Mu(ECM_V, ECM_S)
      }
    }
    
    # Agregación ponderada por equilibrio de clases
    if (sum(Ws) == 0){
      Mu_var <- 0
    } else {
      Ws_norm <- Ws / sum(Ws)
      Mu_var  <- sum(Ws_norm * mu_k)
    }
    
    res <- rbind(res, data.frame(
      var_original = var_name,
      S            = S_key,
      Mu           = Mu_var,
      tipo_target  = "discreta",
      stringsAsFactors = FALSE
    ))
  }
  
  res
}


# ============================================================================
# 14. CONSTRUCCIÓN DE LA MATRIZ μ
# ============================================================================
#
# Objetivo:
# Tabla resumen de los resultados de μ en formato matricial para facilitar
# los análisis posteriores.
#
# Metodología:
# - Cada fila representa una coalición S.
# - Cada columna representa una variable original del dataset.
# - Cada celda contiene el valor de la medida difusa asociado a esa combinación.
#
# Ejemplo:
#
#               Age     Race     Risk_BP
# empty         0.00    0.00      0.00
# Age           1.00    0.25      0.30
# Race          0.10    1.00      0.15
# Age+Race      1.00    1.00      0.45
#
# Resultado:
# - Matriz μ con las coaliciones (S) en filas y las variables 
#   en columnas.
# - Estructura preparada para metodología 4
#
# ============================================================================



construir_matriz_mu <- function(tabla_mu, vars_original, coal_keys){
  
  M <- matrix(NA_real_,
              nrow = length(coal_keys),
              ncol = length(vars_original),
              dimnames = list(coal_keys, vars_original))
  
  for (S_key in coal_keys){
    sub <- tabla_mu[tabla_mu$S == S_key, ]
    for (v in vars_original){
      fila <- sub[sub$var_original == v, ]
      if (nrow(fila) == 1) M[S_key, v] <- fila$Mu
    }
  }
  
  as.data.frame(M)
}


# ============================================================================
# 15. AJUSTE MONÓTONO DE LA MEDIDA μ
# ============================================================================
#
# Objetivo:
# Garantizar que la incorporación de nuevas variables a una coalición
# nunca reduzca el valor final de μ.
#
# Problema:
# - Puede ocurrir que una coalición con más variables obtenga una μ
#   inferior a la de alguno de sus subconjuntos con menos variables.
#
# Ejemplo:
#
# μ(A + B)     = 0.80
# μ(A + B + C) = 0.70
#
# Aunque la coalición A+B+C contiene más información, su valor es inferior.
#
# Criterio aplicado:
# - Para cada coalición S se conserva el mejor valor de μ obtenido
#   entre S y todos sus subconjuntos.
#
# Ejemplo:
#
# μ(A + B)     = 0.80
# μ(A + B + C) = 0.70
#
# Resultado ajustado:
#
# μ*(A + B + C) = 0.80
#
# Interpretación:
# - Si una coalición de dos variables presenta mejor capacidad
#   predictiva que una coalición de tres variables, prevalece
#   el valor de la coalición de dos variables.
#
# Resultado:
# - Se obtiene una versión monótona de μ.
# - Añadir variables nunca empeora el valor final de la medida.
#
# ============================================================================



.max_na <- function(x){
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

.clave_canonica <- function(vars, vars_X_all){
  if (length(vars) == 0) return("empty")

  vars <- intersect(vars_X_all, as.character(vars))
  paste(vars, collapse = "+")
}


aplicar_mu_mejor_monotono <- function(tabla_mu, coaliciones, vars_X_all){
  
  if (is.null(tabla_mu) || nrow(tabla_mu) == 0) return(tabla_mu)
  
  coal_keys <- names(coaliciones)
  

  sizes <- vapply(coaliciones, length, integer(1))
  orden_keys <- coal_keys[order(sizes, coal_keys)]  
  
  tabla_out <- tabla_mu
  

  targets <- unique(tabla_out$var_original)
  
  for (tg in targets){
    
    idx_tg <- which(tabla_out$var_original == tg)
    mu_map <- setNames(tabla_out$Mu[idx_tg], tabla_out$S[idx_tg])

    best_map <- setNames(rep(NA_real_, length(coal_keys)), coal_keys)
    
 
    for (S_key in orden_keys){
      
      mu_S <- mu_map[[S_key]]
      S_vars <- coaliciones[[S_key]]
      
      if (length(S_vars) == 0){
      
        best_map[[S_key]] <- mu_S
      } else {
  
        sub_keys <- vapply(S_vars, function(v){
          .clave_canonica(setdiff(S_vars, v), vars_X_all)
        }, character(1))
        
        cand <- c(mu_S, best_map[sub_keys])
        best_map[[S_key]] <- .max_na(cand)
      }
    }
    
    # Reemplazamos Mu por μ_best en la tabla
    for (ii in idx_tg){
      S_key <- tabla_out$S[ii]
      tabla_out$Mu[ii] <- best_map[[S_key]]
    }
  }
  
  tabla_out
}


# ============================================================================
# 16. PROCEDIMIENTO GENERAL DE CÁLCULO DE LA MEDIDA DIFUSA μ
# ============================================================================
#
# Objetivo:
# Ejecutar de forma integrada todo el proceso de cálculo de la medida
# difusa μ para variables objetivo numéricas, nominales y ordinales.
#
# Procesos ejecutados:
#
# 1. Preprocesamiento de variables explicativas.
# 2. Construcción de bloques de variables.
# 3. Generación de coaliciones (S).
# 4. Construcción de variables dummy para targets discretos.
# 5. Cálculo de μ para variables objetivo numéricas.
# 6. Cálculo y agregación de μ para variables objetivo categóricas
#    (nominales y ordinales).
# 7. Aplicación del ajuste monótono.
# 8. Construcción de la matriz μ.
#
# Resultado:
# - Tabla detallada con los valores μ.
# - Matriz μ preparada para método 4.
#
# ============================================================================


calcular_mu_num_card_ord <- function(Datos,
                                     vars_numeric_targets,
                                     vars_cardinal_targets,
                                     vars_ordinal_targets,
                                     vars_X_numeric,
                                     vars_X_cardinal,
                                     vars_X_ordinal,
                                     max_bloques = NULL){
  
  # 1) Preprocesado de X
  Datos_proc <- preprocesar_X_num_card_ord(
    data          = Datos,
    vars_numeric  = vars_X_numeric,
    vars_cardinal = vars_X_cardinal,
    vars_ordinal  = vars_X_ordinal
  )
  
  # 2) Bloques de X
  bloques_X <- crear_bloques_X_num_card_ord(
    datos_proc    = Datos_proc,
    vars_numeric  = vars_X_numeric,
    vars_cardinal = vars_X_cardinal,
    vars_ordinal  = vars_X_ordinal
  )
  
  # 3) Coaliciones S (incluye "empty")
  vars_X_all  <- c(vars_X_numeric, vars_X_cardinal, vars_X_ordinal)
  coaliciones <- generar_coaliciones(vars_X_all, max_size = max_bloques)
  coal_keys   <- names(coaliciones)
  
  # 4) Dummies Y_* para targets discretas:
  aux <- crear_dummies_targets_discretas(
    datos                 = Datos_proc,
    vars_cardinal_targets = vars_cardinal_targets,
    vars_ordinal_targets  = vars_ordinal_targets
  )
  Datos_full  <- aux$datos
  tabla_pesos <- aux$pesos
  
  # 5) μ para targets numéricas
  mu_num <- do.call(rbind,
                    lapply(vars_numeric_targets, function(y){
                      calcular_Mu_numerica_target(Datos_full, y,
                                                  bloques_X, coaliciones)
                    }))
  
  # 6) μ para targets discretas (cardinal + ordinal)
  vars_discretas_targets <- c(vars_cardinal_targets, vars_ordinal_targets)
  mu_disc <- do.call(rbind,
                     lapply(vars_discretas_targets, function(v){
                       calcular_Mu_discreta_target(Datos_full, v,
                                                   bloques_X, coaliciones,
                                                   tabla_pesos)
                     }))
  
  # 7) Cálculo y corrección μ monótono si procece
  tabla_mu <- rbind(mu_num, mu_disc)
  

  tabla_mu <- aplicar_mu_mejor_monotono(
    tabla_mu    = tabla_mu,
    coaliciones = coaliciones,
    vars_X_all  = vars_X_all
  )
  
  # 8) Matriz μ ya ajustada
  vars_obj  <- c(vars_numeric_targets, vars_discretas_targets)
  matriz_mu <- construir_matriz_mu(tabla_mu, vars_obj, coal_keys)
  
  list(
    tabla_mu_detalle = tabla_mu,
    matriz_mu        = matriz_mu
  )
}


# ============================================================================
# 17. ORGANIZACIÓN DE RESULTADOS DE LA MEDIDA DIFUSA μ
# ============================================================================
#
# Objetivo:
# Ejecutar el procedimiento general de cálculo de la medida difusa μ y
# organizar los resultados en los distintos formatos de salida utilizados
# por la metodología.
#
# Procesos ejecutados:
#
# 1. Identificación de variables objetivo categóricas nominales y
#    ordinales.
#
# 2. Ejecución del procedimiento general de cálculo de μ.
#
# 3. Construcción de las distintas estructuras de resultados.
#
# Resultados generados:
#
# 1. tabla_mu_modelos_todos
#    - Tabla detallada de resultados.
#
# 2. tabla_mu_variables
#    - Tabla resumida por variable objetivo, coalición S y valor μ.
#
# 3. matriz_mu
#    - Matriz final de la medida difusa μ.
#    - Filas: coaliciones S.
#    - Columnas: variables objetivo.
#
# Resultado:
#
# - Conjunto completo de resultados asociados al cálculo de la medida
#   difusa μ.
#
# ============================================================================


calcular_todo_Mu <- function(Datos,
                             vars_numeric_original,
                             vars_categoricas_original,
                             vars_numeric_X,
                             vars_cardinal_X,
                             vars_ordinal_X,
                             max_bloques = NULL,
                             usar_step = FALSE){
  

  vars_cardinal_targets <- intersect(vars_categoricas_original, vars_cardinal_X)
  vars_ordinal_targets  <- intersect(vars_categoricas_original, vars_ordinal_X)
  

  core <- calcular_mu_num_card_ord(
    Datos                 = Datos,
    vars_numeric_targets  = vars_numeric_original,
    vars_cardinal_targets = vars_cardinal_targets,
    vars_ordinal_targets  = vars_ordinal_targets,
    vars_X_numeric        = vars_numeric_X,
    vars_X_cardinal       = vars_cardinal_X,
    vars_X_ordinal        = vars_ordinal_X,
    max_bloques           = max_bloques
  )
  
  tabla_detalle <- core$tabla_mu_detalle  
  
  # 1) tabla_mu_modelos_todos (compatibilidad)

  tabla_mu_modelos_todos <- tabla_detalle %>%
    dplyr::mutate(
      categoria         = NA_character_,
      target            = var_original,
      tipo_target_old   = tipo_target,
      bloques_incluidos = S,
      ECM_S             = NA_real_,
      ECM_Vacio         = NA_real_,
      peso_categoria    = 1
    ) %>%
    dplyr::select(
      var_original,
      categoria,
      target,
      tipo_target = tipo_target_old,
      S,
      bloques_incluidos,
      ECM_S,
      ECM_Vacio,
      Mu,
      peso_categoria
    )
  
  # 2) tabla_mu_variables (compacta)
  tabla_mu_variables <- tabla_detalle %>%
    dplyr::select(var_original, S, Mu) %>%
    dplyr::arrange(var_original, S)
  
  # 3) matriz_mu
  matriz_mu <- core$matriz_mu
  
  list(
    tabla_mu_modelos_todos = tabla_mu_modelos_todos,
    tabla_mu_variables     = tabla_mu_variables,
    matriz_mu              = matriz_mu
  )
}

################################################################################
# 18. EJECUCIÓN DE LA MEDIDA DIFUSA μ
################################################################################
#
# Objetivo:
# Ejecutar el procedimiento completo de cálculo de la medida difusa μ
# sobre el datasets de análisis.
#
# Configuración:
# - Definición de variables objetivo.
# - Definición de variables explicativas.
# - Aplicación de la metodología completa de cálculo de μ.
#
# Resultado:
# - Tabla detallada de μ.
# - Tabla resumida por variable.
# - Matriz μ.
#
################################################################################

library(dplyr)

###############################################################################
# DEFINICIÓN DE LAS VARIABLES DEL DATASET
###############################################################################

vars_numeric_original_y      <- c("Age", "Cho")
vars_categoricas_original_y <- c("Rac", "Ris", "Sex")

vars_numeric_X_y  <- c("Age", "Cho")
vars_cardinal_X_y <- c("Rac", "Sex")
vars_ordinal_X_y  <- c("Ris")


###############################################################################
# APLICACIÓN DE LA MEDIDA DIFUSA μ AL DATASET
###############################################################################

res_Mu_y <- calcular_todo_Mu(
  Datos                     = HNANESI_y,
  vars_numeric_original     = vars_numeric_original_y,
  vars_categoricas_original = vars_categoricas_original_y,
  vars_numeric_X            = vars_numeric_X_y,
  vars_cardinal_X           = vars_cardinal_X_y,
  vars_ordinal_X            = vars_ordinal_X_y,
  max_bloques               = NULL,
  usar_step                 = FALSE
)

res_Mu_log <- calcular_todo_Mu(
  Datos                     = HNANESI_log,
  vars_numeric_original     = vars_numeric_original_y,
  vars_categoricas_original = vars_categoricas_original_y,
  vars_numeric_X            = vars_numeric_X_y,
  vars_cardinal_X           = vars_cardinal_X_y,
  vars_ordinal_X            = vars_ordinal_X_y,
  max_bloques               = NULL,
  usar_step                 = FALSE
)


################################################################################
# 19. VALIDACIÓN DE CONSISTENCIA DE LA MATRIZ μ
################################################################################
#
# Objetivo:
# Verificar que los valores obtenidos para la medida difusa μ cumplen
# las propiedades teóricas definidas en la metodología.
#
# Validaciones realizadas:
#
# 1. Coalición vacía:
#    μ(empty) = 0
#
# 2. Inclusión de la variable objetivo:
#    μ(v,S) = 1 cuando la variable objetivo pertenece a S
#
# 3. Acotación:
#    0 ≤ μ ≤ 1
#
# Resultado:
# - Confirmación de la consistencia matemática de la matriz μ.
#
################################################################################


###############################################################################
# 19.1 TEST 1: VALIDACIÓN DE LA COALICIÓN VACÍA
###############################################################################
#
# Condición esperada:
# - μ(empty) = 0 para todas las variables objetivo.


test_mu_vacio <- function(matriz_mu, tol = 1e-10){
  
  if (!("empty" %in% rownames(matriz_mu))){
    stop("No existe la fila 'empty' en matriz_mu.")
  }
  
  mu_empty <- matriz_mu["empty", , drop = TRUE]
  mu_empty <- as.numeric(unlist(mu_empty, use.names = FALSE))
  names(mu_empty) <- colnames(matriz_mu)
  
  errores <- names(mu_empty)[abs(mu_empty) > tol]
  
  if (length(errores) == 0){
    cat("✅ TEST OK: μ(empty) = 0 para todas las variables\n")
  } else {
    cat("❌ ERROR: μ(empty) ≠ 0 en:\n")
    print(mu_empty[errores])
  }
  
  invisible(mu_empty)
}


###############################################################################
# 19.2 TEST 2: VALIDACIÓN DE LA INCLUSIÓN DE LA VARIABLE OBJETIVO
###############################################################################
#
# Propiedad verificada:
# - Una variable debe poder predecirse perfectamente cuando forma
#   parte de la propia coalición.
#
# Condición esperada:
# - μ(v,S) = 1 cuando la variable objetivo pertenece a S.
#
# Observación:
# - Tras la corrección monótona, esta propiedad debe cumplirse en todas
#   las coaliciones que contienen la variable objetivo.

test_mu_variable_en_S <- function(matriz_mu, tol = 1e-8){
  
  vars <- colnames(matriz_mu)
  errores <- list()
  
  for (v in vars){
    
    filas_v <- grep(paste0("(^|\\+)", v, "(\\+|$)"),
                    rownames(matriz_mu),
                    value = TRUE)
    
    if (length(filas_v) == 0) next
    
    mu_vals <- as.numeric(matriz_mu[filas_v, v, drop = TRUE])
    mal <- filas_v[abs(mu_vals - 1) > tol]
    
    if (length(mal) > 0){
      errores[[v]] <- setNames(mu_vals[match(mal, filas_v)], mal)
    }
  }
  
  if (length(errores) == 0){
    cat("✅ TEST OK: μ = 1 cuando la variable está en S\n")
  } else {
    cat("❌ ERROR: μ ≠ 1 cuando la variable está en S:\n")
    print(errores)
  }
  
  invisible(errores)
}


###############################################################################
# 19.3 TEST 3: VALIDACIÓN DEL RANGO DE μ
###############################################################################
#
# Propiedad verificada:
# - La medida difusa μ debe esta [0,1].
#
# Condición esperada:
# - 0 ≤ μ ≤ 1
#


test_mu_rango <- function(matriz_mu, tol = 1e-12){
  
  M <- as.matrix(matriz_mu)
  storage.mode(M) <- "double"
  
  fuera <- which(M < -tol | M > 1 + tol, arr.ind = TRUE)
  
  if (nrow(fuera) == 0){
    cat("✅ TEST OK: todas las μ están en [0,1]\n")
  } else {
    cat("❌ ERROR: μ fuera de [0,1] en:\n")
    print(fuera)
    print(M[fuera])
  }
  
  invisible(fuera)
}


###############################################################################
# 20. PREPARACIÓN DE LA MATRIZ μ PARA VALIDACIÓN Y POSTERIOR TRATAMIENTO
###############################################################################
#
# Objetivo:
# Garantizar que todos los valores de la matriz μ se encuentran en
# formato numérico antes de ejecutar los tests de consistencia.
#
# Tratamiento aplicado:
# - Conversión de posibles factores a formato numérico.
# - Conservación de los nombres originales de las coaliciones.
#
# Resultado:
# - Matriz μ preparada para los procesos de validación y comprobación.
#
###############################################################################

matriz_mu <- res_Mu_y$matriz_mu

matriz_mu <- as.data.frame(
  lapply(matriz_mu, function(x) as.numeric(as.character(x))),
  stringsAsFactors = FALSE
)

rownames(matriz_mu) <- rownames(res_Mu_y$matriz_mu)


###############################################################################
# 21. EJECUCIÓN DE LAS VALIDACIONES DE CONSISTENCIA
###############################################################################
#
# Objetivo:
# Verificar que la matriz μ obtenida cumple las propiedades teóricas
# definidas para la medida difusa.
#
# Validaciones ejecutadas:
#
# 1. Coalición vacía:
#    μ(empty) = 0
#
# 2. Inclusión de la variable objetivo:
#    μ(v,S) = 1 cuando la variable pertenece a la coalición S
#
# 3. Acotación de la medida:
#    0 ≤ μ ≤ 1
#
# Resultado:
# - Confirmación de la consistencia matemática de la matriz μ.
# - Identificación de posibles desviaciones respecto a la metodología.
#
###############################################################################

test_mu_vacio(matriz_mu)

test_mu_variable_en_S(matriz_mu)

test_mu_rango(matriz_mu)

test_mu_vacio(matriz_mu)
test_mu_variable_en_S(matriz_mu)
test_mu_rango(matriz_mu)


# ============================================================================
# 22. ALMACENAMIENTO DE RESULTADOS
# ============================================================================
#
# Objetivo:
# Guardar los resultados obtenidos de la medida difusa μ para su
# análisis y reutilización posterior.
#
# Formato utilizado:
# - CSV
#
# Resultados almacenados:
# - Tabla detallada de valores μ.
# - Matriz μ final.
#
# Resultado:
# - Archivos CSV almacenados en la carpeta de salida definida.
#
# ============================================================================




###############################################################################
# FUNCIÓN DE GUARDADO EN CSV
###############################################################################

guardar_csv <- function(datos, nombre_archivo) {
  
  if (!dir.exists(ruta_resultados)) {
    dir.create(ruta_resultados, recursive = TRUE)
  }
  
  # Si hay rownames, convertirlos en una columna
  if (!is.null(rownames(datos))) {
    
    # Evitar añadir una columna vacía si los rownames son 1:n
    if (!all(rownames(datos) == seq_len(nrow(datos)))) {
      
      datos <- cbind(
        Coalicion = rownames(datos),
        datos
      )
      
    }
  }
  
  write.csv(
    datos,
    file = file.path(ruta_resultados, nombre_archivo),
    row.names = FALSE
  )
  
  invisible(TRUE)
}



###############################################################################
# ALMACENAMIENTO DE RESULTADOS CSV 
###############################################################################

guardar_csv(
  res_Mu_y$tabla_mu_variables,
  "tabla_mu_variables.csv"
)

guardar_csv(
  res_Mu_y$matriz_mu,
  "Mu.csv"
)


###############################################################################
# FUNCIÓN DE GUARDADO EN EXCEL
###############################################################################

# Instalar una única vez
#install.packages("openxlsx")

# Cargar librería
library(openxlsx)

guardar_excel <- function(datos, nombre_archivo) {
  
  if (!dir.exists(ruta_resultados)) {
    dir.create(ruta_resultados, recursive = TRUE)
  }
  
  # Si hay rownames, convertirlos en columna
  if (!is.null(rownames(datos))) {
    
    if (!all(rownames(datos) == seq_len(nrow(datos)))) {
      
      datos <- cbind(
        Coalicion = rownames(datos),
        datos
      )
      
    }
  }
  
  write.xlsx(
    x = as.data.frame(datos),
    file = file.path(ruta_resultados, nombre_archivo),
    rowNames = FALSE
  )
  
  invisible(TRUE)
}


###############################################################################
# ALMACENAMIENTO DE RESULTADOS EXCEL 
###############################################################################

guardar_excel(
  res_Mu_y$tabla_mu_variables,
  "tabla_mu_variables.xlsx"
)

guardar_excel(
  res_Mu_y$matriz_mu,
  "Mu.xlsx"
)
