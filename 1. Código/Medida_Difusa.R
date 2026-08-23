
################################################################################
# SCRIPT COMPLETO — con el AJUSTE “μ MEJOR” (monótono)
################################################################################


# ============================================================
# FUNCIÓN: calcular_ECM_vacio
# ============================================================
# OBJETIVO:
# Calcular el Error Cuadrático Medio (ECM) de un modelo vacío



calcular_ECM_vacio <- function(y){

  mean((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
}

# ============================================================
# FUNCIÓN: calcular_Mu
# ============================================================
# OBJETIVO:
# Calcular la métrica μ (mu), que mide la mejora de un modelo
# respecto a un modelo vacío.
#
# INTERPRETACIÓN DE μ:
# - μ = 0  → el modelo es igual de malo que el modelo vacío
# - μ = 1  → el modelo es perfecto (sin error)
# - 0 < μ < 1 → mejora parcial respecto al modelo vacío
#
# FÓRMULA:
# μ = (ECM_Vacio - ECM_S) / ECM_Vacio
#
# DONDE:
# - ECM_Vacio = error del modelo vacío (baseline)
# - ECM_S     = error del modelo que queremos evaluar
#
# ============================================================


calcular_Mu <- function(ECM_Vacio, ECM_S, tol = 1e-12){

  if (is.na(ECM_Vacio) || ECM_Vacio == 0) return(NA_real_)  # Evita división por 0 o NA
  if (is.na(ECM_S)) return(NA_real_)
  
  raw <- (ECM_Vacio - ECM_S) / ECM_Vacio  # Mejora relativa frente al vacío
  
  if (is.na(raw)) return(NA_real_)
  if (abs(raw) < tol) raw <- 0            # Limpieza de ruido flotante alrededor de 0
  
  max(0, min(1, raw))                  
}

# ============================================================
# FUNCIÓN: es_binaria_generica
# ============================================================
# OBJETIVO:
# Detectar si una variable es binaria (tiene solo dos valores posibles).
#
# TIPOS SOPORTADOS:
# - logical (TRUE/FALSE)
# - factor
# - character
# - numeric
#
# CRITERIO:
# - TRUE si tiene exactamente 2 valores distintos
# - En numéricos también se acepta el caso típico 0/1
# - FALSE en cualquier otro caso
#
# ============================================================


es_binaria_generica <- function(x){
  # Detecta si x es binaria, soportando logical / factor / character / numeric.
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

# ============================================================
# FUNCIÓN: normalizar_binaria
# ============================================================
# OBJETIVO:
# Convertir una variable binaria a formato numérico 0/1.
#
# TIPOS SOPORTADOS:
# - logical    → FALSE = 0, TRUE = 1
# - factor     → se ordenan los niveles y el segundo es 1
# - character  → igual que factor
# - numeric    → si ya es 0/1 se mantiene; si no, el mayor valor será 1
#
# CRITERIO:
# - Siempre devuelve un vector numérico con valores 0 y 1
# - En variables con 2 categorías:
#     menor → 0
#     mayor → 1
#
# NOTAS:
# - Los NA se mantienen
# - Si la variable no es binaria, lanza error
# ============================================================


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

# ============================================================
# FUNCIÓN: construir_factor_ordinal
# ============================================================
# OBJETIVO:
# Convertir una variable en un factor ordenado (ordered factor)
# asegurando un orden consistente de los niveles.
#
# COMPORTAMIENTO:
# - Caso especial "Risk_BP": aplica un orden predefinido
# - Si la variable ya es ordered, se respeta tal cual
# - En otros casos, crea el orden según la primera aparición de valores
#
# INPUT:
# - x0: vector de datos
# - var_name: nombre de la variable (para aplicar reglas específicas)
#
# OUTPUT:
# - Factor ordenado (ordered = TRUE)
#
# NOTAS:
# - Mantiene coherencia en pipelines donde el orden es importante
# - Evita problemas al entrenar modelos con variables ordinales
# ============================================================


construir_factor_ordinal <- function(x0, var_name){

  if (var_name == "Risk_BP"){
    orden_bp <- c("Optima", "Normal", "Elevada", "Hipertension_1", "Hipertension_2")
    return(factor(as.character(x0), levels = orden_bp, ordered = TRUE))
  }
  if (is.ordered(x0)) return(x0)
  factor(x0, levels = unique(x0), ordered = TRUE)
}


# ============================================================
# FUNCIÓN: preprocesar_X_num_card_ord
# ============================================================
# OBJETIVO:
# Preprocesar variables explicativas (X) según su tipo para
# dejarlas listas para modelado.
#
# TRANSFORMACIONES:
# - Numéricas:
#     → Conversión a numeric
#
# - Cardinales (nominales):
#     → Conversión a factor
#     → Creación de variables dummy (one-hot encoding completo)
#     → Formato: nombreVariable_nivel
#
# - Ordinales:
#     → Conversión a factor ordenado (ordered)
#     → Creación de dummies acumulativas (tipo umbral ≥ nivel)
#     → Se generan k-1 variables (evita colinealidad)
#
# INPUT:
# - data: data.frame con los datos originales
# - vars_numeric: vector con nombres de variables numéricas
# - vars_cardinal: vector con variables categóricas nominales
# - vars_ordinal: vector con variables categóricas ordinales
#
# OUTPUT:
# - data.frame transformado con nuevas variables (dummies)
#
# NOTAS:
# - Mantiene las variables originales
# - Ignora variables no presentes en el dataset
# - Requiere función construir_factor_ordinal()
# ============================================================


preprocesar_X_num_card_ord <- function(data,
                                       vars_numeric,
                                       vars_cardinal,
                                       vars_ordinal){
  
  df <- data  # Copia de trabajo
  
  ## 1) Numéricas: forzamos a numeric para evitar factores/char
  for (v in vars_numeric){
    if (v %in% names(df)) df[[v]] <- as.numeric(df[[v]])
  }
  
  ## 2) Cardinales (nominales): factor + one-hot completo
  for (v in vars_cardinal){
    if (!v %in% names(df)) next
    
    x <- as.factor(df[[v]])          # Asegura factor
    df[[v]] <- x
    
    levs <- levels(x)
    if (length(levs) > 0){
      mm <- stats::model.matrix(~ x - 1)             # One-hot sin intercepto
      colnames(mm) <- paste0(v, "_", levs)           # Nombres v_nivel
      df <- cbind(df, as.data.frame(mm))             # Añade dummies al df
    }
  }
  
  ## 3) Ordinales: ordered + dummies acumulativas (umbral ≥)
  for (v in vars_ordinal){
    if (!v %in% names(df)) next
    
    x <- construir_factor_ordinal(df[[v]], v)        # Orden consistente
    df[[v]] <- x
    
    levs <- levels(x)
    k    <- length(levs)
    code <- as.integer(x)                             # 1..k según el orden
    
    # Crea k-1 dummies: v_<nivel_j> = 1 si code >= j
    # - nivel mínimo => todas 0
    # - nivel máximo => todas 1
    for (j in 2:k){
      df[[paste0(v, "_", levs[j])]] <- as.integer(code >= j)
    }
  }
  
  df
}


# ============================================================
# FUNCIÓN: crear_bloques_X_num_card_ord
# ============================================================
# OBJETIVO:
# Construir la estructura de bloques de variables X para modelado.
#
# IDEA:
# - Agrupa las columnas transformadas por variable original
# - Cada bloque representa cómo entra una variable al modelo
#
# ESTRUCTURA DE BLOQUES:
# - Numéricas:
#     → 1 única columna (la propia variable)
#
# - Cardinales (nominales):
#     → Todas sus dummies (v_nivel)
#
# - Ordinales:
#     → Todas sus dummies acumulativas (v_nivel)
#
# INPUT:
# - datos_proc: data.frame ya preprocesado
# - vars_numeric: variables numéricas originales
# - vars_cardinal: variables categóricas nominales
# - vars_ordinal: variables categóricas ordinales
#
# OUTPUT:
# - Lista nombrada:
#     nombre_variable → vector de columnas asociadas
#
# NOTAS:
# - Solo incluye variables presentes en el dataset
# - Usa patrón "v_" para localizar dummies
# - Cada bloque puede tener 1 o varias columnas
# ============================================================


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


# ============================================================
# FUNCIÓN: generar_coaliciones
# ============================================================
# OBJETIVO:
# Generar todas las combinaciones posibles de variables (coaliciones)
# a partir de un conjunto de variables X.
#
# IDEA:
# - Cada coalición representa un subconjunto de variables
# - Incluye siempre la coalición vacía ("empty")
# - Las combinaciones se generan hasta un tamaño máximo
#
# ESTRUCTURA:
# - "empty" → sin variables
# - "A"     → coalición individual
# - "A+B"   → combinación de variables
# - etc.
#
# INPUT:
# - vars_X: vector de nombres de variables
# - max_size: tamaño máximo de las combinaciones (opcional)
#
# OUTPUT:
# - Lista nombrada:
#     nombre_coalición → vector de variables
#
# NOTAS:
# - Si max_size no se define, se generan todas las combinaciones
# - Los nombres tipo "A+B+C" permiten indexación directa posterior
# - Crece de forma combinatoria (cuidado con muchas variables)
# ============================================================


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


# ============================================================
# FUNCIÓN: ajustar_lineal_ECM
# ============================================================
# OBJETIVO:
# Calcular el ECM de un modelo de regresión lineal (OLS)
# a partir de un conjunto de variables explicativas.
#
# COMPORTAMIENTO:
# - Si no hay predictores → usa modelo vacío (media de y)
# - Ajusta el modelo con lm.fit (más eficiente que lm)
# - Permite incluir o no intercepto
#
# PROCESO:
# - Filtra columnas existentes
# - Elimina filas con NA (complete cases)
# - Ajusta el modelo lineal
# - Calcula predicciones
# - Devuelve el ECM
#
# INPUT:
# - datos: data.frame
# - y: nombre de la variable objetivo
# - x_cols: vector de variables explicativas
# - intercept: TRUE/FALSE para incluir intercepto
#
# OUTPUT:
# - Valor numérico (ECM del modelo)
#
# NOTAS:
# - En caso de colinealidad, coeficientes NA se reemplazan por 0
# - Usa solo filas completas en y y X
# - Requiere función calcular_ECM_vacio()
# ============================================================


ajustar_lineal_ECM <- function(datos, y, x_cols, intercept = TRUE){
  
  x_cols <- intersect(x_cols, names(datos))          # Asegura columnas existentes
  y_vec  <- datos[[y]]
  
  # Si no hay predictores => modelo vacío (media)
  if (length(x_cols) == 0){
    return(calcular_ECM_vacio(y_vec))
  }
  
  # Trabaja solo con filas completas en y y X
  sub <- datos[, c(y, x_cols), drop = FALSE]
  cc  <- complete.cases(sub)
  if (!any(cc)) return(NA_real_)
  
  y_cc <- sub[[y]][cc]
  X_cc <- as.matrix(sub[cc, x_cols, drop = FALSE])
  
  # Intercepto manual si se pide
  if (intercept){
    X_cc <- cbind("(Intercept)" = 1, X_cc)
  }
  
  fit <- lm.fit(X_cc, y_cc)                          # Ajuste OLS eficiente
  
  # Si hay colinealidad: coef NA -> 0 para poder predecir
  coefs <- fit$coefficients
  coefs[is.na(coefs)] <- 0
  
  y_hat <- as.vector(X_cc %*% coefs)                 # Predicción
  mean((y_cc - y_hat)^2, na.rm = TRUE)               # ECM
}

# ============================================================
# FUNCIÓN: ajustar_logit_ECM
# ============================================================
# OBJETIVO:
# Calcular el ECM en un modelo de clasificación binaria
# usando regresión logística (Brier score).
#
# COMPORTAMIENTO:
# - Valida que el target sea binario
# - Convierte la variable objetivo a 0/1
# - Si no hay predictores → modelo con solo intercepto
# - Ajusta modelo logístico (glm binomial)
#
# PROCESO:
# - Filtra variables disponibles
# - Usa solo filas completas
# - Detecta predicción perfecta (ECM=0)
# - Calcula probabilidades y Brier score
#
# INPUT:
# - datos: data.frame
# - y: variable objetivo (binaria)
# - x_cols: variables explicativas
#
# OUTPUT:
# - Valor numérico (ECM tipo Brier score)
#
# NOTAS:
# - Requiere es_binaria_generica() y normalizar_binaria()
# - Si alguna X reproduce exactamente y → ECM = 0
# - El ECM mide error en probabilidades, no en clases
# ============================================================


ajustar_logit_ECM <- function(datos, y, x_cols){
  
  # 1) Validación: el target debe ser binario
  if (!es_binaria_generica(datos[[y]]))
    stop(paste0("ajustar_logit_ECM: ", y, " no es binaria."))
  
  # 2) Normalización a 0/1
  yb <- normalizar_binaria(datos[[y]])
  x_cols <- intersect(x_cols, names(datos))
  
  # 3) Modelo vacío: solo intercepto (p0 = prevalencia)
  #    ECM = mean((1 - P(correcto))^2) = Brier score equivalente
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
  #    Si existe alguna columna X exactamente igual a y (en las filas completas),
  #    entonces el mínimo ECM posible es 0 (P(correcto)=1) y no dependemos de glm.
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
  
  pr <- predict(mod, type = "response")              # Probabilidades P(y=1|X)
  PC <- ifelse(d[[y]] == 1, pr, 1 - pr)              # Probabilidad de clase correcta
  
  mean((1 - PC)^2, na.rm = TRUE)                     # Brier score
}


# ============================================================
# FUNCIÓN: crear_dummies_targets_discretas
# ============================================================
# OBJETIVO:
# Transformar variables objetivo categóricas (no numéricas)
# en variables dummy para modelado, y calcular un peso base.
#
# TIPOS DE VARIABLES QUE SE TRANSFORMAN:
# - Variables categóricas NOMINALES (sin orden)
# - Variables categóricas ORDINALES (con orden)
#
# IMPORTANTE:
# - Las variables NUMÉRICAS (edad, ingresos, etc.) NO se transforman
# - Esta función SOLO actúa sobre variables categóricas
#
# TRANSFORMACIONES:
#
# A) Targets nominales (cardinales):
#   → One-hot encoding (una dummy por categoría)
#   → Cada categoría se convierte en una variable binaria (0/1)
#   → Formato: Y_variable_categoria
#
#   Ejemplo:
#       color = {rojo, azul}
#   →   Y_color_rojo
#       Y_color_azul
#
# B) Targets ordinales:
#   → Dummies acumulativas (tipo umbral ≥ nivel)
#   → Se respeta el orden de los niveles
#   → Se crean k-1 variables
#
#   Ejemplo:
#       rango = {soldado < sargento < capitan}
#   →   Y_rango_sargento  (>= sargento)
#       Y_rango_capitan   (>= capitan)
#
# PESOS:
# - Para cada dummy se calcula:
#     ws_base = min(#1, #0)
# - Mide el equilibrio de clases de la dummy
# - Se usa después para ponderar resultados
#
# INPUT:
# - datos: data.frame original
# - vars_cardinal_targets: variables categóricas nominales
# - vars_ordinal_targets: variables categóricas ordinales
#
# OUTPUT:
# - Lista con:
#     $datos  → data.frame con las dummies añadidas
#     $pesos  → tabla con info y pesos de cada dummy
#
# NOTAS:
# - Las variables originales se mantienen
# - Los nombres se normalizan con make.names()
# - Requiere construir_factor_ordinal()

# NOTA GENERAL:
# - El modelo final siempre trabaja con variables numéricas
# - Las variables categóricas se convierten a formato numérico (dummies)

# ============================================================



crear_dummies_targets_discretas <- function(datos,
                                            vars_cardinal_targets,
                                            vars_ordinal_targets){
  
  out <- datos
  W   <- data.frame()
  
  ## A) Targets cardinales (nominales): one-hot por categoría
  for (v in vars_cardinal_targets){
    if (!v %in% names(out)) next
    
    xf <- as.factor(out[[v]])
    levs   <- levels(xf)
    levs_s <- make.names(levs)                       # Nombres seguros
    
    for (i in seq_along(levs)){
      nom <- paste0("Y_", v, "_", levs_s[i])         # Dummy target
      dum <- as.integer(xf == levs[i])              # 1 si pertenece a la categoría
      
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


# ============================================================
# FUNCIÓN: calcular_Mu_numerica_target
# ============================================================
# OBJETIVO:
# Calcular la métrica μ para una VARIABLE OBJETIVO NUMÉRICA
# (variable continua o cuantitativa) en todas las coaliciones
# de variables explicativas.
#
# IMPORTANTE (CLAVE PARA ENTENDER LA FUNCIÓN):
# - "Target numérico" significa que la variable ORIGINAL ya es numérica
#     (ej: edad, ingresos, temperatura…)
#
# - NO significa que otras variables no sean numéricas en el modelo:
#     → En el modelo TODAS las variables son numéricas (dummies incluidas)
#     → Pero aquí distinguimos el TIPO ORIGINAL del target
#
# - Esta función NO se usa para variables categóricas:
#     → Las categóricas (nominales u ordinales) se tratan con:
#         calcular_Mu_discreta_target()
#
# IDEA:
# - Compara el error del modelo con variables (ECM_S)
#   frente al modelo vacío (ECM_Vacio)
# - Evalúa la contribución de cada subconjunto de variables (S)
#
# COMPORTAMIENTO:
# - ECM_Vacio → error usando solo la media de y
# - ECM_S     → error del modelo lineal con variables en S
# - μ         → mejora relativa respecto al modelo vacío
#
# CASO ESPECIAL (IMPORTANTE):
# - Si el target y pertenece a la coalición S:
#     → Se incluye y como predictor
#     → Se elimina el intercepto
#     → Esto permite predicción perfecta:
#         ECM_S = 0  →  μ = 1
#
# INPUT:
# - Datos: data.frame
# - y: variable objetivo ORIGINAL (numérica)
# - bloques_X: lista de bloques de variables (pueden incluir dummies)
# - coaliciones: lista de subconjuntos de variables originales
#
# OUTPUT:
# - data.frame con:
#     var_original → nombre del target
#     S            → coalición evaluada
#     Mu           → valor de la métrica
#     tipo_target  → "numerica"
#
# NOTAS:
# - Usa ajustar_lineal_ECM() y calcular_Mu()
# - El modelo es siempre lineal (OLS)
# - Aunque X pueda contener dummies, el target y es numérico real
# - Base para análisis tipo Shapley
# ============================================================


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


# ============================================================
# FUNCIÓN: calcular_Mu_discreta_target
# ============================================================
# OBJETIVO:
# Calcular la métrica μ para una VARIABLE OBJETIVO CATEGÓRICA
# (nominal o ordinal), usando sus variables dummy asociadas.
#
# IMPORTANTE (CLAVE PARA ENTENDER LA FUNCIÓN):
# - "Target discreto" significa que la variable ORIGINAL es categórica:
#     → Nominal (sin orden)  o  Ordinal (con orden)
#
# - Esta función NO trabaja directamente con la variable original:
#     → Primero esa variable ha sido transformada en dummies (Y_...)
#     → Cada dummy es una variable binaria (0/1)
#
# - Aunque las dummies son numéricas, el problema sigue siendo de CLASIFICACIÓN
#   (no de regresión como en el caso numérico)
#
# - Para variables numéricas originales (edad, ingresos, etc.):
#     → usar calcular_Mu_numerica_target()
#
# IDEA:
# - Cada categoría (nominal) o umbral (ordinal) se representa como una dummy
# - Se evalúa un modelo para cada dummy (problema binario)
# - Se combinan los resultados para obtener un único μ por variable original
#
# COMPORTAMIENTO:
# - ECM_V → error del modelo vacío para cada dummy (baseline)
# - ECM_S → error del modelo logístico (Brier score)
# - μ_k   → mejora relativa por dummy
#
# AGREGACIÓN:
# - μ final = media ponderada de μ_k
# - Pesos: ws_base (equilibrio de clases en cada dummy)
#
# INPUT:
# - Datos: data.frame con dummies ya creadas (Y_...)
# - var_name: nombre de la variable categórica original
# - bloques_X: lista de bloques de variables (pueden incluir dummies)
# - coaliciones: subconjuntos de variables originales
# - tabla_pesos: tabla con información de cada dummy
#
# OUTPUT:
# - data.frame con:
#     var_original → nombre del target original
#     S            → coalición evaluada
#     Mu           → valor agregado
#     tipo_target  → "discreta"
#
# NOTAS:
# - Usa ajustar_logit_ECM() y calcular_Mu()
# - Cada dummy se modela como un problema binario independiente
# - El resultado final se interpreta a nivel de variable original
# - Base para análisis tipo Shapley
# ============================================================



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


# ============================================================
# FUNCIÓN: construir_matriz_mu
# ============================================================
# OBJETIVO:
# Convertir la tabla de resultados de μ (formato largo)
# en una matriz donde:
#   - Filas    → coaliciones S
#   - Columnas → variables objetivo
#
# CONTEXTO:
# - Antes de esta función, μ se ha calculado en formato "tabla larga":
#     (var_original, S, Mu)
#
# - Esta función reorganiza esos resultados en formato matricial,
#   necesario para análisis posteriores (ej: Shapley, comparaciones)
#
# ESTRUCTURA DE SALIDA:
# - Filas: cada coalición S (ej: "A+B", "empty", etc.)
# - Columnas: cada variable objetivo (numérica o categórica original)
# - Valores: μ(S) para cada combinación
#
# EJEMPLO:
#               edad   color   rango
#   empty        0      0       0
#   A            0.2    0.1     0.3
#   A+B          0.4    0.5     0.6
#
# INPUT:
# - tabla_mu: data.frame con columnas:
#       var_original → variable objetivo
#       S            → coalición
#       Mu           → valor calculado
#
# - vars_original: vector con nombres de variables objetivo
# - coal_keys: vector con nombres de coaliciones (orden de filas)
#
# OUTPUT:
# - data.frame (matriz μ):
#       filas = coaliciones
#       columnas = variables objetivo
#
# NOTAS:
# - Si falta alguna combinación (S, variable), se deja como NA
# - Se asume una única fila por (var_original, S)
# - Facilita cálculos posteriores sobre μ (ej: agregaciones)
# ============================================================


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


# ============================================================
# FUNCIÓN: aplicar_mu_mejor_monotono
# ============================================================
# OBJETIVO:
# Corregir la métrica μ para que sea MONÓTONA respecto a la
# inclusión de variables en la coalición.
#
# PROBLEMA (MUY IMPORTANTE):
# - El μ calculado originalmente puede NO ser monótono:
#
#     Puede ocurrir que:
#         μ(A + B) < μ(A)
#
# - Es decir, añadir variables puede empeorar μ, debido a:
#     → ruido
#     → sobreajuste
#     → inestabilidad del modelo
#
# - Esto es coherente estadísticamente, pero:
#     ❗ rompe la intuición de "más información no empeora"
#
# SOLUCIÓN:
# - Para cada coalición S, se redefine:
#
#     μ_best(S) = max{ μ(T) : T ⊆ S }
#
# - Es decir:
#     → Nos quedamos con el MEJOR resultado alcanzable
#       usando cualquier subconjunto de S
#
# INTERPRETACIÓN:
# - μ_best(S) representa:
#     "Lo mejor que puedes conseguir usando las variables de S"
#
# - Nunca será peor que usar menos variables
# - Garantiza comportamiento monotónico
#
# IMPLEMENTACIÓN:
# - Se usa Programación Dinámica (DP)
# - Se recorren las coaliciones por tamaño creciente:
#
#     μ_best(empty) = μ(empty)
#
#     para S ≠ empty:
#     μ_best(S) = max(
#         μ(S),
#         μ_best(S sin cada variable)
#     )
#
# INPUT:
# - tabla_mu: data.frame en formato largo:
#       var_original, S, Mu
#
# - coaliciones: lista de coaliciones (nombre → variables)
#
# - vars_X_all: vector global de variables (define orden canónico)
#
# OUTPUT:
# - data.frame igual que tabla_mu, pero con:
#     Mu → reemplazado por μ_best (monótono)
#
# NOTAS:
# - Se aplica de forma independiente por variable objetivo
# - No modifica la estructura de la tabla (solo los valores)
# - Es clave para análisis tipo Shapley consistentes
#
# CUIDADO:
# - Este ajuste cambia la interpretación:
#     μ ya no es el valor exacto del modelo,
#     sino el mejor valor alcanzable dentro de S
# ============================================================


# max que tolera NA (si todos NA => NA)
.max_na <- function(x){
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

# Clave canónica para un conjunto de vars respetando el orden global vars_X_all
# - Debe coincidir con generar_coaliciones(): "A+B+C" o "empty"
.clave_canonica <- function(vars, vars_X_all){
  if (length(vars) == 0) return("empty")
  # Mantiene el orden del vector global, que es el que usas para crear coaliciones
  vars <- intersect(vars_X_all, as.character(vars))
  paste(vars, collapse = "+")
}

# Aplica el ajuste monótono a UNA tabla de μ en formato largo
# (sin cambiar estructura: reemplaza columna Mu)
aplicar_mu_mejor_monotono <- function(tabla_mu, coaliciones, vars_X_all){
  
  if (is.null(tabla_mu) || nrow(tabla_mu) == 0) return(tabla_mu)
  
  coal_keys <- names(coaliciones)
  
  # Orden de coaliciones por tamaño (para DP)
  sizes <- vapply(coaliciones, length, integer(1))
  orden_keys <- coal_keys[order(sizes, coal_keys)]  # estable y reproducible
  
  tabla_out <- tabla_mu
  
  # Aplicamos DP por cada variable objetivo (var_original) independientemente
  targets <- unique(tabla_out$var_original)
  
  for (tg in targets){
    
    idx_tg <- which(tabla_out$var_original == tg)
    mu_map <- setNames(tabla_out$Mu[idx_tg], tabla_out$S[idx_tg])
    
    # best_map almacenará μ_best para cada coalición S (por clave)
    best_map <- setNames(rep(NA_real_, length(coal_keys)), coal_keys)
    
    # DP en tamaño creciente
    for (S_key in orden_keys){
      
      mu_S <- mu_map[[S_key]]
      S_vars <- coaliciones[[S_key]]
      
      if (length(S_vars) == 0){
        # base: vacío
        best_map[[S_key]] <- mu_S
      } else {
        # candidatos: el propio μ(S) + los mejores de S sin cada elemento
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


# ============================================================
# FUNCIÓN: calcular_mu_num_card_ord
# ============================================================
# OBJETIVO:
# Pipeline completo para calcular la métrica μ para variables
# objetivo de tipo:
#   - Numéricas (regresión)
#   - Categóricas nominales (clasificación)
#   - Categóricas ordinales (clasificación con orden)
#
# IMPORTANTE (CLAVE GLOBAL DEL SISTEMA):
# - Tipos de variables originales:
#     → Numéricas        (ej: edad, ingresos)
#     → Nominales        (ej: color, tipo)
#     → Ordinales        (ej: nivel, rango)
#
# - Transformación:
#     → Numéricas  → NO se transforman
#     → Nominales  → dummies (one-hot)
#     → Ordinales  → dummies acumulativas
#
# - Modelo:
#     → TODAS las variables en el modelo son numéricas
#     → Pero se respeta el tipo ORIGINAL del target:
#
#         Numérico   → modelo lineal
#         Categórico → modelo logit (Brier score)
#
# IDEA GENERAL:
# - Para cada variable objetivo y cada coalición S:
#     → se calcula cuánto mejora el modelo usando S
#     → respecto a no usar ninguna variable (modelo vacío)
#
# - μ(S) mide:
#     "cuánto ayuda el conjunto de variables S a predecir el target"
#
# FLUJO DEL PIPELINE:
#
# 1) Preprocesado de X:
#     → convierte variables categóricas en dummies
#     → deja todo en formato numérico
#
# 2) Construcción de bloques:
#     → agrupa columnas por variable original
#
# 3) Generación de coaliciones:
#     → todos los subconjuntos de variables X
#
# 4) Transformación de targets categóricos:
#     → nominales → one-hot
#     → ordinales → dummies acumulativas
#
# 5) Cálculo de μ para targets numéricas:
#     → regresión lineal (ECM)
#
# 6) Cálculo de μ para targets categóricas:
#     → modelo logit (Brier score)
#     → cálculo por dummy + agregación
#
# 7) AJUSTE MONÓTONO (IMPORTANTE):
#     → μ puede no ser monótono al añadir variables
#     → se corrige con:
#
#         μ_best(S) = max_{T ⊆ S} μ(T)
#
#     → garantiza que añadir variables nunca empeora μ
#
# 8) Construcción de matriz μ:
#     → filas = coaliciones S
#     → columnas = variables objetivo
#
# INPUT:
# - Datos: data.frame original
#
# - Targets:
#     vars_numeric_targets   → variables numéricas objetivo
#     vars_cardinal_targets  → variables nominales objetivo
#     vars_ordinal_targets   → variables ordinales objetivo
#
# - Variables explicativas (X):
#     vars_X_numeric
#     vars_X_cardinal
#     vars_X_ordinal
#
# - max_bloques:
#     → tamaño máximo de coaliciones (control de complejidad)
#
# OUTPUT:
# - lista con:
#     tabla_mu_detalle → resultados en formato largo
#     matriz_mu        → resultados en formato matriz
#
# NOTAS:
# - El número de coaliciones crece combinatoriamente (2^p)
# - El ajuste monótono cambia la interpretación de μ:
#     pasa de valor "real" a "mejor alcanzable"
# - Base para análisis tipo Shapley y explicabilidad
# ============================================================


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
  
  # 7) Tabla larga (Mu “tal cual” modelo)
  tabla_mu <- rbind(mu_num, mu_disc)
  
  # 7b) AJUSTE NUEVO: hacer μ monótono por coalición
  #     Reemplaza Mu por μ_best(S)=max_{T⊆S} μ(T)
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


# ============================================================
# FUNCIÓN: calcular_todo_Mu
# ============================================================
# OBJETIVO:
# Wrapper del pipeline completo para calcular μ, manteniendo
# el formato de salida del código anterior (compatibilidad).
#
# ESTE WRAPPER:
# - Llama al pipeline principal (calcular_mu_num_card_ord)
# - Adapta la salida al formato antiguo
#
# IMPORTANTE (PARA ENTENDER BIEN LA FUNCIÓN):
#
# 1) TIPOS DE VARIABLES ORIGINALES:
# - vars_numeric_original     → targets numéricas (ej: edad)
# - vars_categoricas_original → targets categóricas (nominal + ordinal)
#
# 2) VARIABLES EXPLICATIVAS (X):
# - Se dividen en:
#     → numéricas
#     → categóricas nominales
#     → categóricas ordinales
#
# - Estas se transforman internamente en:
#     → numéricas (sin cambios)
#     → dummies (nominales)
#     → dummies acumulativas (ordinales)
#
# 3) SEPARACIÓN AUTOMÁTICA DE TARGETS:
# - Las targets categóricas se separan en:
#
#     cardinales (nominales) → one-hot
#     ordinales              → acumulativas
#
# - Esto se basa en cómo están definidas en X:
#     vars_cardinal_X / vars_ordinal_X
#
# FLUJO INTERNO (RESUMEN):
# - Llama a calcular_mu_num_card_ord()
# - Calcula μ para:
#     → targets numéricas (regresión lineal)
#     → targets categóricas (logit + agregación)
# - Aplica ajuste monótono:
#
#     μ_best(S) = max_{T ⊆ S} μ(T)
#
# SALIDA (FORMATO COMPATIBLE):
#
# 1) tabla_mu_modelos_todos:
# - Formato extendido (como tu sistema anterior)
# - Incluye placeholders (ECM_S, ECM_Vacio, etc.)
#
# 2) tabla_mu_variables:
# - Formato compacto:
#     var_original, S, Mu
#
# 3) matriz_mu:
# - Filas    → coaliciones S
# - Columnas → variables objetivo
#
# INPUT:
# - Datos: data.frame original
#
# - Targets:
#     vars_numeric_original
#     vars_categoricas_original
#
# - Variables explicativas (X):
#     vars_numeric_X
#     vars_cardinal_X
#     vars_ordinal_X
#
# - max_bloques:
#     → tamaño máximo de coaliciones
#
# - usar_step:
#     → NO utilizado (compatibilidad; se ignora)
#
# OUTPUT:
# - lista con:
#     tabla_mu_modelos_todos
#     tabla_mu_variables
#     matriz_mu
#
# NOTAS IMPORTANTES:
#
# - Este wrapper NO calcula μ directamente:
#     → delega en calcular_mu_num_card_ord()
#
# - Solo transforma la salida para mantener compatibilidad
#
# - Las columnas ECM_S y ECM_Vacio se devuelven vacías (NA)
#
# - El parámetro usar_step se mantiene por compatibilidad,
#   pero actualmente no tiene efecto
# ============================================================


calcular_todo_Mu <- function(Datos,
                             vars_numeric_original,
                             vars_categoricas_original,
                             vars_numeric_X,
                             vars_cardinal_X,
                             vars_ordinal_X,
                             max_bloques = NULL,
                             usar_step = FALSE){
  
  # Separa targets categóricas en cardinales y ordinales según el esquema X
  vars_cardinal_targets <- intersect(vars_categoricas_original, vars_cardinal_X)
  vars_ordinal_targets  <- intersect(vars_categoricas_original, vars_ordinal_X)
  
  # Llama al core (incluye FIX ordinal + ajuste μ monótono)
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
  
  tabla_detalle <- core$tabla_mu_detalle  # var_original, S, Mu, tipo_target
  
  # 1) tabla_mu_modelos_todos (compatibilidad)
  # Nota: requiere dplyr cargado.
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
#############################   EJEMPLO DE USO   ################################
################################################################################
# Ejemplo completo de uso + tests de consistencia
# Compatible con:
# - μ monótono (μ_best(S))
# - targets numéricos, cardinales y ordinales
################################################################################

library(dplyr)

###############################################################################
# DEFINICIÓN DE VARIABLES
###############################################################################

vars_numeric_original_y      <- c("Age", "Cho")
vars_categoricas_original_y <- c("Rac", "Ris", "Sex")

vars_numeric_X_y  <- c("Age", "Cho")
vars_cardinal_X_y <- c("Rac", "Sex")
vars_ordinal_X_y  <- c("Ris")


###############################################################################
# CÁLCULO DE μ PARA DOS DATASETS (COMPARACIÓN)
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
################################################################################
# TESTS DE CONSISTENCIA PARA MATRIZ μ_j(S)
################################################################################
# - test_mu_vacio(): μ(empty) = 0
# - test_mu_variable_en_S(): μ(v | S) = 1 si v ∈ S
# - test_mu_rango(): μ ∈ [0,1]
################################################################################


###############################################################################
# TEST 1: μ(empty) = 0
###############################################################################

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
# TEST 2: μ(v | S) = 1 cuando v ∈ S
###############################################################################
# NOTA IMPORTANTE (CAMBIO CLAVE):
# - Con μ_monótono, esto DEBE cumplirse siempre, incluso si el modelo directo
#   empeoraba, porque μ_best(S) ≥ μ_best({v}) = 1
###############################################################################

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
# TEST 3: μ ∈ [0,1]
###############################################################################

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


################################################################################
# PREPARACIÓN ROBUSTA DE matriz_mu (RECOMENDADO)
################################################################################
# Esto evita problemas de:
# - factors encubiertos
# - columnas tipo character
# - comparaciones numéricas incorrectas
################################################################################

matriz_mu <- res_Mu_y$matriz_mu

matriz_mu <- as.data.frame(
  lapply(matriz_mu, function(x) as.numeric(as.character(x))),
  stringsAsFactors = FALSE
)

rownames(matriz_mu) <- rownames(res_Mu_y$matriz_mu)


################################################################################
# EJECUCIÓN DE LOS TESTS
################################################################################

test_mu_vacio(matriz_mu)
test_mu_variable_en_S(matriz_mu)
test_mu_rango(matriz_mu)

###############################################################################
# CONFIGURACIÓN GLOBAL DE SALIDA
###############################################################################

RUTA_SALIDA <- "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Debug"


# ============================================================
# Helper para guardar salidas de texto (debug) en un .txt
# ============================================================

guardar_txt <- function(nombre_archivo, contenido) {
  
  # Validaciones
  if (!is.character(nombre_archivo) || length(nombre_archivo) != 1) {
    stop("guardar_txt: 'nombre_archivo' debe ser un string.")
  }
  
  if (!is.character(contenido)) {
    stop("guardar_txt: 'contenido' debe ser un vector character.")
  }
  
  # Crear directorio si no existe
  if (!dir.exists(RUTA_SALIDA)) {
    dir.create(RUTA_SALIDA, recursive = TRUE)
  }
  
  ruta_completa <- file.path(RUTA_SALIDA, nombre_archivo)
  
  writeLines(contenido, con = ruta_completa, useBytes = TRUE)
  
  invisible(TRUE)
}
