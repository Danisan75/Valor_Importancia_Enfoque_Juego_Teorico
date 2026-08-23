
###############################################################################
# 1. GENERACIÓN DE SUBMUESTRAS POR COMBINATORIA (MÉTODO I, SIN BUCLES EXPLÍCITOS)
###############################################################################
# - Genera submuestras contrafactuales por combinatoria de predictores
# - Una submuestra por instancia y por combinación S
# - Válido para regresión y clasificación
# - Incluye comprobaciones automáticas de consistencia
###############################################################################

generar_submuestras_combinatoria <- function(df, target) {generar_submuestras_combin <- as.data.frame(df)
n  <- nrow(df)

#---------------------------
# Validaciones básicas
#---------------------------
if (missing(target) || is.null(target))
  stop("Debes indicar un 'target'.")

if (!(target %in% names(df)))
  stop("El target no existe en el data.frame.")

#---------------------------
# Predictores (excluye target)
#---------------------------
vars_pred <- setdiff(names(df), target)
if (length(vars_pred) == 0)
  stop("No quedan predictores.")

#---------------------------
# ✅ TODAS las combinaciones: INCLUYE EL VACÍO
#---------------------------
S_list <- list(character(0))  # ← conjunto vacío

if (length(vars_pred) > 0) {
  S_no_vacio <- unlist(
    lapply(seq_along(vars_pred),
           function(k) combn(vars_pred, k, simplify = FALSE)),
    recursive = FALSE
  )
  S_list <- c(S_list, S_no_vacio)
}

# Nombres canónicos
S_names <- c(
  "empty",
  sapply(S_list[-1], paste, collapse = "_")
)
names(S_list) <- S_names

#---------------------------
# Construcción de submuestras
#---------------------------
submuestras <- lapply(seq_along(S_list), function(j) {
  
  S_vars <- S_list[[j]]
  
  inst_list <- lapply(seq_len(n), function(i) {
    df_sub <- df
    for (v in S_vars) df_sub[[v]] <- df[[v]][i]
    df_sub
  })
  
  names(inst_list) <- paste0("instancia_", seq_len(n))
  inst_list
})

names(submuestras) <- S_names

#---------------------------
# Comprobaciones automáticas
#---------------------------
verificacion <- do.call(
  rbind,
  Map(function(S_vars, S_name) {
    
    do.call(
      rbind,
      lapply(seq_len(n), function(i) {
        
        df_sub <- submuestras[[S_name]][[i]]
        
        ok_fijas <- all(
          sapply(S_vars, function(v) all(df_sub[[v]] == df[[v]][i]))
        )
        
        vars_libres <- setdiff(vars_pred, S_vars)
        ok_libres  <- all(
          sapply(vars_libres, function(v) all(df_sub[[v]] == df[[v]]))
        )
        
        data.frame(
          S              = S_name,
          columnas_fijas = paste(S_vars, collapse = ","),
          instancia      = i,
          ok_fijas       = ok_fijas,
          ok_libres      = ok_libres,
          ok_total       = ok_fijas & ok_libres,
          stringsAsFactors = FALSE
        )
      })
    )
    
  }, S_list, S_names)
)

#---------------------------
# Salida
#---------------------------
list(
  submuestras   = submuestras,
  combinaciones = S_list,
  verificacion  = verificacion,
  target        = target,
  formula       = as.formula(paste(target, "~ .")),
  vars_pred     = vars_pred
)
}




###############################################################################
# 2. PREPARAR X E Y (GLOBAL Y SUBMUESTRAS) – PARA XGBOOST
###############################################################################
# Este bloque se encarga de transformar:
#
#   - El dataset original (global)
#   - Cada submuestra contrafactual
#
# en matrices numéricas coherentes que puedan ser usadas por XGBoost.
#
# OBJETIVO CLAVE:
# - Garantizar que el modelo XGBoost global y las submuestras usan
#   exactamente los mismos predictores (mismas columnas, mismo orden).
#
# IMPORTANTE:
# - Estas funciones NO entrenan ningún modelo.
# - Solo preparan X (predictores) e y (target).
# - Funcionan igual para regresión y clasificación.
###############################################################################

###############################################################################
# 2.1 PREPARAR X/Y DESDE UNA SUBMUESTRA CONTRAFACTUAL
###############################################################################
# Se usa cuando ya hemos construido una submuestra (Método I / II).
#
# ENTRADA:
# - df_sub   : submuestra contrafactual (data.frame)
# - target   : nombre del target
# - vars_pred: vector de predictores que DEBEN estar presentes
#
# SALIDA:
# - X_mat: matriz numérica de predictores (lista para xgboost)
# - y    : vector del target (sin transformar aquí)
#
# COMPROBACIONES:
# - Que todas las variables predictoras existen en la submuestra
# - Que model.matrix produce columnas válidas
###############################################################################

preparar_xy_desde_submuestra <- function(df_sub, target, vars_pred) {
  
  # Aseguramos data.frame
  df_sub <- as.data.frame(df_sub)
  
  #---------------------------
  # Comprobación 1:
  # ¿Están todos los predictores?
  #---------------------------
  if (!all(vars_pred %in% names(df_sub))) {
    faltan <- vars_pred[!vars_pred %in% names(df_sub)]
    stop(
      "ERROR (preparar_xy_desde_submuestra): faltan columnas en df_sub: ",
      paste(faltan, collapse = ", ")
    )
  }
  
  #---------------------------
  # Separación de y y X
  #---------------------------
  y    <- df_sub[[target]]
  X_df <- df_sub[, vars_pred, drop = FALSE]
  
  #---------------------------
  # Comprobación 2:
  # ¿Hay al menos un predictor?
  #---------------------------
  if (ncol(X_df) == 0) {
    stop(
      "ERROR (preparar_xy_desde_submuestra): X_df está vacío; ",
      "no hay predictores en esta submuestra."
    )
  }
  
  #---------------------------
  # Conversión a matriz numérica
  # - model.matrix crea dummies automáticamente si hay factores
  # - Se elimina el intercepto (-1) porque XGBoost no lo necesita
  #---------------------------
  X_mat <- stats::model.matrix(~ . - 1, data = X_df)
  
  #---------------------------
  # Comprobación 3:
  # ¿La matriz resultante es válida?
  #---------------------------
  if (is.null(X_mat) || ncol(X_mat) == 0) {
    stop(
      "ERROR (preparar_xy_desde_submuestra): ",
      "model.matrix produjo una matriz vacía."
    )
  }
  
  #---------------------------
  # Salida
  #---------------------------
  list(
    X_mat = X_mat,
    y     = y
  )
}

###############################################################################
# 2.2 PREPARAR X/Y DEL DATASET GLOBAL
###############################################################################
# Se usa para entrenar el modelo XGBoost global.
#
# DIFERENCIA CON LA FUNCIÓN ANTERIOR:
# - Aquí se puede omitir vars_pred, y se infieren automáticamente
#   como todas las variables excepto el target.
#
# COMPROBACIONES:
# - El target existe
# - Hay predictores
# - model.matrix genera columnas válidas
###############################################################################

preparar_xy_global <- function(df, target, vars_pred = NULL) {
  
  # Aseguramos data.frame
  df <- as.data.frame(df)
  
  #---------------------------
  # Comprobación 1:
  # ¿Existe el target?
  #---------------------------
  if (!(target %in% names(df))) {
    stop(
      "ERROR (preparar_xy_global): target '",
      target,
      "' no existe en el data.frame."
    )
  }
  
  #---------------------------
  # Definición de predictores
  #---------------------------
  if (is.null(vars_pred)) {
    vars_pred <- setdiff(names(df), target)
  }
  
  #---------------------------
  # Comprobación 2:
  # ¿Hay predictores?
  #---------------------------
  if (length(vars_pred) == 0) {
    stop(
      "ERROR (preparar_xy_global): ",
      "no quedan predictores; vars_pred está vacío."
    )
  }
  
  #---------------------------
  # Separación de y y X
  #---------------------------
  y    <- df[[target]]
  X_df <- df[, vars_pred, drop = FALSE]
  
  if (ncol(X_df) == 0) {
    stop(
      "ERROR (preparar_xy_global): X_df global está vacío."
    )
  }
  
  #---------------------------
  # Conversión a matriz numérica
  #---------------------------
  X_mat <- stats::model.matrix(~ . - 1, data = X_df)
  
  #---------------------------
  # Comprobación 3:
  #---------------------------
  if (is.null(X_mat) || ncol(X_mat) == 0) {
    stop(
      "ERROR (preparar_xy_global): ",
      "model.matrix global devolvió matriz vacía."
    )
  }
  
  #---------------------------
  # Salida
  #---------------------------
  list(
    X_mat     = X_mat,
    y         = y,
    vars_pred = vars_pred
  )
}


###############################################################################
# 3. MODELO XGBOOST GLOBAL Y K = MEDIA DE LAS PREDICCIONES
###############################################################################
# Este bloque:
#
# 1) Prepara la matriz X y el vector y del dataset ORIGINAL
# 2) Ajusta un ÚNICO modelo XGBoost global (regresión o clasificación)
# 3) Calcula K = media de las predicciones del modelo sobre el dataset original
#
# IMPORTANTE:
# - El modelo entrenado es FIJO y se reutiliza después
# - No se entrena ningún modelo por submuestra
# - Funciona igual para regresión y clasificación
##########################################################################


ajustar_xgb_global <- function(df,
                               target,
                               tipo    = c("regresion", "clasificacion"),
                               nrounds = 200,
                               params  = list(),
                               verbose = 0,
                               seed    = NULL) {
  
  tipo <- match.arg(tipo)
  
  if (!is.null(seed)) set.seed(seed)
  
  # Preparación X / y
  prep      <- preparar_xy_global(df, target = target)
  X_mat     <- prep$X_mat
  y         <- prep$y
  vars_pred <- prep$vars_pred
  
  # Conversión del target
  if (tipo == "clasificacion") {
    
    if (is.logical(y)) {
      y_num <- as.integer(y)
      
    } else if (is.factor(y) || is.character(y)) {
      y_fac <- factor(y)
      if (nlevels(y_fac) != 2)
        stop("Clasificación: el target debe tener exactamente 2 niveles.")
      y_num <- as.integer(y_fac) - 1
      
    } else {
      y_num <- as.numeric(y)
      if (!all(y_num %in% c(0, 1)))
        warning("Target numérico en clasificación no es 0/1 puro.")
    }
    
  } else {
    y_num <- as.numeric(y)
  }
  
  # DMatrix
  dtrain <- xgboost::xgb.DMatrix(
    data  = X_mat,
    label = y_num
  )
  
  # Parámetros por defecto
  if (length(params) == 0) {
    params <- if (tipo == "clasificacion") {
      list(
        objective        = "binary:logistic",
        eval_metric      = "logloss",
        max_depth        = 3,
        eta              = 0.1,
        subsample        = 0.8,
        colsample_bytree = 0.8
      )
    } else {
      list(
        objective        = "reg:squarederror",
        eval_metric      = "rmse",
        max_depth        = 3,
        eta              = 0.1,
        subsample        = 0.8,
        colsample_bytree = 0.8
      )
    }
  }
  
  # Entrenamiento
  modelo <- xgboost::xgb.train(
    params  = params,
    data    = dtrain,
    nrounds = nrounds,
    verbose = verbose
  )
  
  # Predicciones globales
  pred <- predict(modelo, newdata = dtrain)
  
  # K = media de predicciones
  K <- mean(pred, na.rm = TRUE)
  
  list(
    modelo        = modelo,
    pred          = pred,
    K             = K,
    tipo_modelo   = tipo,
    target        = target,
    vars_pred     = vars_pred,
    params        = params,
    feature_names = colnames(X_mat)
  )
}



###############################################################################
# 4. ALINEAR MATRIZ X DE SUBMUESTRA CON EL MODELO GLOBAL (XGBOOST)
###############################################################################
# Este bloque garantiza que la matriz X construida a partir de una submuestra
# contrafactual sea COMPATIBLE con el modelo XGBoost global entrenado.
#
# ¿Por qué es necesario?
# - El modelo global se entrenó con un conjunto fijo de columnas (feature_names).
# - En una submuestra, model.matrix puede:
#     * no generar algunas columnas (niveles ausentes),
#     * generar columnas en distinto orden.
# - XGBoost requiere que:
#     * el número de columnas sea EXACTAMENTE el mismo,
#     * el orden de las columnas sea EXACTAMENTE el mismo.
#
# Estrategia:
# 1) Construir X_raw desde la submuestra usando preparar_xy_desde_submuestra().
# 2) Crear una matriz X_aligned con TODAS las columnas del modelo global,
#    inicializadas a 0.
# 3) Copiar únicamente las columnas comunes entre X_raw y feature_names.
#
# El resultado es una matriz segura para predict().
###############################################################################

preparar_X_sub_para_global <- function(df_sub,
                                       target,
                                       vars_pred,
                                       feature_names) {
  
  #------------------------------------------------------------
  # Paso 1: Preparar X desde la submuestra (BLOQUE 2)
  #------------------------------------------------------------
  prep_sub <- preparar_xy_desde_submuestra(
    df_sub   = df_sub,
    target   = target,
    vars_pred = vars_pred
  )
  
  X_raw <- prep_sub$X_mat
  # X_raw:
  # - tiene tantas filas como la submuestra
  # - puede tener MENOS columnas que el modelo global
  # - el orden de columnas NO está garantizado
  
  #------------------------------------------------------------
  # Paso 2: Crear la matriz alineada con el modelo global
  #------------------------------------------------------------
  n_fil <- nrow(X_raw)
  n_col <- length(feature_names)
  
  # Inicializamos todo a 0:
  # - si una columna no aparece en la submuestra, se queda a 0
  X_aligned <- matrix(0, nrow = n_fil, ncol = n_col)
  colnames(X_aligned) <- feature_names
  
  #------------------------------------------------------------
  # Paso 3: Copiar solo las columnas comunes
  #------------------------------------------------------------
  # Intersección entre:
  # - columnas que tiene la submuestra
  # - columnas que espera el modelo global
  cols_comunes <- intersect(feature_names, colnames(X_raw))
  
  if (length(cols_comunes) > 0) {
    X_aligned[, cols_comunes] <- X_raw[, cols_comunes, drop = FALSE]
  }
  
  #------------------------------------------------------------
  # Salida
  #------------------------------------------------------------
  X_aligned
}

###############################################################################
# 5. APLICAR EL MODELO XGB GLOBAL A TODAS LAS SUBMUESTRAS
###############################################################################
# Este bloque:
#
# 1) Toma las submuestras contrafactuales generadas en el Paso 1
# 2) Alinea cada submuestra al espacio del modelo XGBoost global
# 3) Aplica el MISMO modelo global (ya entrenado) a cada submuestra
# 4) Guarda las predicciones fila a fila en una nueva columna: pred_xgb
#
# IMPORTANTE:
# - NO se entrena ningún modelo nuevo
# - NO se recalculan parámetros
# - El modelo global es FIJO
# - Funciona igual para regresión y clasificación
###############################################################################



ajustar_xgb_a_res <- function(res,
                              obj_global,
                              verbose = 0) {
  
  #------------------------------------------------------------
  # Información del modelo global (contrato del Paso 3)
  #------------------------------------------------------------
  target        <- obj_global$target
  vars_pred     <- obj_global$vars_pred
  tipo          <- obj_global$tipo_modelo
  feature_names <- obj_global$feature_names
  modelo        <- obj_global$modelo
  K_global      <- obj_global$K
  
  #------------------------------------------------------------
  # Submuestras originales (SIN predicciones)
  #------------------------------------------------------------
  subm_originales  <- res$submuestras
  submuestras_pred <- subm_originales
  
  #------------------------------------------------------------
  # Bucle por cada combinación S
  #------------------------------------------------------------
  for (combo in names(subm_originales)) {
    
    if (verbose > 0) {
      cat("Procesando combinación:", combo, "\n")
    }
    
    lista_inst <- subm_originales[[combo]]
    
    #----------------------------------------------------------
    # Bucle por cada instancia i
    #----------------------------------------------------------
    for (inst_name in names(lista_inst)) {
      
      df_sub <- lista_inst[[inst_name]]
      
      #------------------------------------------------------
      # Alinear X de la submuestra con el modelo global
      #------------------------------------------------------
      X_sub <- preparar_X_sub_para_global(
        df_sub        = df_sub,
        target        = target,
        vars_pred     = vars_pred,
        feature_names = feature_names
      )
      
      # Forzamos orden de columnas (robusto)
      X_sub <- X_sub[, feature_names, drop = FALSE]
      
      #------------------------------------------------------
      # Predicción con el modelo global
      #------------------------------------------------------
      dsub     <- xgboost::xgb.DMatrix(data = X_sub)
      pred_sub <- predict(modelo, newdata = dsub)
      
      #------------------------------------------------------
      # Guardar predicciones
      #------------------------------------------------------
      df_sub$pred_xgb <- pred_sub
      submuestras_pred[[combo]][[inst_name]] <- df_sub
    }
  }
  
  #------------------------------------------------------------
  # Salida FINAL (fuera de los bucles)
  #------------------------------------------------------------
  list(
    submuestras_pred = submuestras_pred,
    target           = target,
    vars_pred        = vars_pred,
    tipo             = tipo,
    K                = K_global
  )
}



###############################################################################
# PASO 6. NOMENCLATURA DE H(S)
###############################################################################
# OBJETIVO DEL PASO 6:
# -------------------
# Este paso NO realiza ningún cálculo numérico.
# Su único objetivo es asignar un nombre ÚNICO, COHERENTE y REPRODUCIBLE
# a cada magnitud H(S), donde S es un conjunto de variables fijadas.
#
# Esto es CRÍTICO porque:
#  - Los valores H(i,S) se almacenan en tablas
#  - Se comparan resultados entre:
#       * submuestras guardadas
#       * streaming
#  - Si los nombres no coinciden exactamente, las tablas no se alinean,
#    aunque los valores numéricos sean correctos.
#
# Por tanto:
#  - Este paso garantiza TRAZABILIDAD
#  - NO afecta a K
#  - NO afecta a H(i,S) numéricamente
###############################################################################


###############################################################################
# 6.1 FUNCIÓN: nombre_H_desde_combo()
###############################################################################
# USO:
# ----
# Se utiliza cuando la combinación S viene codificada como STRING,
# por ejemplo: "edad_ingresos_genero"
#
# TRANSFORMACIÓN:
#  - "edad_ingresos_genero"
#      -> c("edad", "ingresos", "genero")
#      -> c("eda", "ing", "gen")
#      -> "H(eda_ing_gen)"
#
# GARANTÍA:
#  - Submuestras y streaming producirán el MISMO nombre para el MISMO S
###############################################################################

nombre_H_desde_combo <- function(combo) {
  
  #------------------------------------------------------------
  # PASO 6.1.1: Validación de la entrada
  #------------------------------------------------------------
  stopifnot(
    is.character(combo),
    length(combo) == 1,
    !is.na(combo),
    nchar(combo) > 0
  )
  
  #------------------------------------------------------------
  # PASO 6.1.2: Caso especial — conjunto vacío
  #------------------------------------------------------------
  # "empty" representa S = ∅ y DEBE producir siempre H()
  if (combo == "empty") {
    return("H()")
  }
  
  #------------------------------------------------------------
  # PASO 6.1.3: Separar variables fijadas
  #------------------------------------------------------------
  vars_fijas <- strsplit(combo, "_", fixed = TRUE)[[1]]
  
  stopifnot(
    length(vars_fijas) > 0,
    all(nchar(vars_fijas) > 0)
  )
  
  #------------------------------------------------------------
  # PASO 6.1.4: Abreviatura (3 primeras letras)
  #------------------------------------------------------------
  abrevs <- substring(vars_fijas, 1, 3)
  
  stopifnot(
    all(nchar(abrevs) > 0),
    !any(is.na(abrevs))
  )
  
  #------------------------------------------------------------
  # PASO 6.1.5: Advertencia por colisiones (no bloqueante)
  #------------------------------------------------------------
  if (any(duplicated(abrevs))) {
    warning(
      "PASO 6: posibles colisiones de nombres en H(S): ",
      paste(abrevs, collapse = ", ")
    )
  }
  
  #------------------------------------------------------------
  # PASO 6.1.6: Construcción final del nombre H(S)
  #------------------------------------------------------------
  paste0("H(", paste(abrevs, collapse = "_"), ")")
}

###############################################################################
# 6.2 FUNCIÓN: nombre_H_desde_vars()
###############################################################################
# USO:
# ----
# Se utiliza principalmente en STREAMING, cuando S viene como vector:
#   c("edad", "ingresos", "genero")
#
# Debe producir EXACTAMENTE el mismo nombre que nombre_H_desde_combo()
###############################################################################

nombre_H_desde_vars <- function(vars_fijas) {
  
  #------------------------------------------------------------
  # PASO 6.2.1: Caso especial — conjunto vacío S = ∅
  #------------------------------------------------------------
  if (length(vars_fijas) == 0) {
    return("H()")
  }
  
  #------------------------------------------------------------
  # PASO 6.2.2: Validación de entrada
  #------------------------------------------------------------
  stopifnot(
    is.character(vars_fijas),
    all(nchar(vars_fijas) > 0),
    !any(is.na(vars_fijas))
  )
  
  #------------------------------------------------------------
  # PASO 6.2.3: Abreviatura (3 primeras letras)
  #------------------------------------------------------------
  abrevs <- substring(vars_fijas, 1, 3)
  
  #------------------------------------------------------------
  # PASO 6.2.4: Advertencia por colisiones (no bloqueante)
  #------------------------------------------------------------
  if (any(duplicated(abrevs))) {
    warning(
      "PASO 6: posibles colisiones de nombres en H(S): ",
      paste(abrevs, collapse = ", ")
    )
  }
  
  #------------------------------------------------------------
  # PASO 6.2.5: Construcción final del nombre H(S)
  #------------------------------------------------------------
  paste0("H(", paste(abrevs, collapse = "_"), ")")
}


###############################################################################
# PASO 7. TABLA H(i,S) POR INSTANCIA
###############################################################################
# OBJETIVO:
# ---------
# Construir una tabla donde:
#   - Cada FILA representa una instancia i
#   - Cada COLUMNA H(S) representa una combinación de variables fijadas S
#   - Cada celda contiene:
#
#       H(i,S) = media de las predicciones del modelo global
#                sobre la submuestra contrafactual (i,S)
#
# Además, se añade K = H(∅) como referencia global.
#
# IMPORTANTE:
#  - NO se hace ninguna transformación de datos
#  - NO se recalcula ningún modelo
#  - SOLO se agregan (media) predicciones ya calculadas
###############################################################################

tabla_H_por_instancia <- function(res_modelo,
                                  nombre_col_pred = "pred_xgb") {
  
  ###########################################################################
  # 7.1 Extraer submuestras con predicciones
  ###########################################################################
  # res_modelo$submuestras_pred tiene la estructura:
  #   combinación S
  #     └─ instancia i
  #          └─ data.frame con columna de predicción
  ###########################################################################
  subm_pred <- res_modelo$submuestras_pred
  combos    <- names(subm_pred)
  
  # Comprobación básica: debe haber combinaciones
  if (length(combos) == 0) {
    stop("No hay combinaciones en res_modelo$submuestras_pred.")
  }
  
  ###########################################################################
  # 7.2 Obtener el conjunto de instancias (común a todas las combinaciones)
  ###########################################################################
  # Todas las combinaciones S tienen las mismas instancias,
  # así que usamos la primera como referencia.
  ###########################################################################
  primera_combo <- combos[1]
  lista_inst    <- subm_pred[[primera_combo]]
  inst_names    <- names(lista_inst)
  
  # Extraemos los identificadores numéricos de instancia:
  # "instancia_1" -> 1, etc.
  inst_ids   <- as.integer(sub("instancia_", "", inst_names))
  inst_orden <- sort(inst_ids)
  
  ###########################################################################
  # 7.3 Dimensiones de la tabla H(i,S)
  ###########################################################################
  n_inst <- length(inst_orden)   # nº de instancias
  n_comb <- length(combos)       # nº de combinaciones S
  
  # Matriz donde se almacenará H(i,S)
  mat_H     <- matrix(NA_real_, nrow = n_inst, ncol = n_comb)
  H_nombres <- character(n_comb)
  
  ###########################################################################
  # 7.4 Bucle principal: combinaciones S
  ###########################################################################
  for (j in seq_along(combos)) {
    
    combo        <- combos[j]
    
    # Nombre H(S) según el PASO 6 (nomenclatura coherente)
    H_nombre     <- nombre_H_desde_combo(combo)
    H_nombres[j] <- H_nombre
    
    # Submuestras de todas las instancias para esta combinación S
    lista_inst_combo <- subm_pred[[combo]]
    
    #########################################################################
    # 7.5 Bucle interno: instancias i
    #########################################################################
    for (inst_name in names(lista_inst_combo)) {
      
      # Identificador numérico de la instancia
      i_id   <- as.integer(sub("instancia_", "", inst_name))
      fila_i <- match(i_id, inst_orden)
      
      # Submuestra contrafactual concreta (i,S)
      df_sub <- lista_inst_combo[[inst_name]]
      
      # Comprobación crítica:
      # La submuestra debe contener la columna de predicción
      if (!(nombre_col_pred %in% names(df_sub))) {
        stop(
          "Falta columna '", nombre_col_pred,
          "' en una submuestra. ¿Se ha ejecutado correctamente el Paso 5?"
        )
      }
      
      #######################################################################
      # 7.6 Cálculo central del método
      #######################################################################
      # H(i,S) = media de las predicciones del modelo global
      #          sobre la submuestra contrafactual (i,S)
      #######################################################################
      mat_H[fila_i, j] <- mean(df_sub[[nombre_col_pred]], na.rm = TRUE)
    }
  }
  
  ###########################################################################
  # 7.7 Construcción de la tabla final
  ###########################################################################
  colnames(mat_H) <- H_nombres
  
  tabla <- data.frame(
    instancia   = inst_orden,
    mat_H,
    row.names   = NULL,
    check.names = FALSE
  )
  
  ###############################################################################
  # 7.8 Añadir K = H(∅) como referencia global (ROBUSTO)
  ###############################################################################
  
  # Nombre canónico de H(∅)
  H_VACIO <- "H()"
  
  # Buscar la columna H(∅)
  col_H_empty <- which(names(tabla) == H_VACIO)
  
  # Comprobaciones defensivas
  if (length(col_H_empty) == 0) {
    stop(
      "PASO 7.8: no se encuentra la columna '", H_VACIO, 
      "' en la tabla H(i,S)."
    )
  }
  
  if (length(col_H_empty) > 1) {
    stop(
      "PASO 7.8: hay más de una columna '", H_VACIO, 
      "'. Los nombres H(S) no son únicos."
    )
  }
  
  # Extraer K desde la tabla
  K_calculado <- unique(tabla[[col_H_empty]])
  
  # K debe ser único
  if (length(K_calculado) != 1 || is.na(K_calculado)) {
    stop(
      "PASO 7.8: K = H(∅) no es único o es NA. ",
      "Revisa las predicciones del modelo global."
    )
  }
  
  # Añadir K como columna explícita
  tabla$K <- K_calculado
  
  
  ###########################################################################
  # 7.9 Salida
  ###########################################################################
  # Tabla con:
  #   - una fila por instancia
  #   - una columna por H(S)
  #   - una columna K (opcional)
  ###########################################################################
  tabla
}

###############################################################################
# PASO 8. CÁLCULO DE T(i,S) POR INSTANCIA
###############################################################################
# DEFINICIÓN:
# -----------
# Para cada instancia i y cada combinación S se calcula:
#
#   T(i,S) = H(i,S) - K
#
# donde:
#   - H(i,S) es la media de las predicciones del modelo global
#     sobre la submuestra contrafactual (i,S)
#   - K = H(∅) es el valor base global (media de predicciones globales)
#
# IMPORTANTE:
#  - NO se recalcula ningún modelo
#  - NO se transforman datos
#  - SOLO se realiza una resta fila a fila
#  - El resultado mide el efecto contrafactual relativo
###############################################################################

calcular_T_por_instancia <- function(tabla_H_instancias) {
  
  ###########################################################################
  # 8.1 Comprobación de que existe K (H vacío)
  ###########################################################################
  # K es el ancla del método. Sin K no se puede calcular T(i,S).
  ###########################################################################
  if (!("K" %in% names(tabla_H_instancias))) {
    stop(
      "La tabla de entrada debe tener una columna 'K'. ",
      "¿Has usado tabla_H_por_instancia(res_modelo)?"
    )
  }
  
  ###########################################################################
  # 8.2 Identificación de las columnas H(S)
  ###########################################################################
  # Se consideran columnas H(S) todas las que empiezan por 'H('
  ###########################################################################
  h_cols <- grep("^H\\(", names(tabla_H_instancias), value = TRUE)
  
  if (length(h_cols) == 0) {
    stop(
      "No se encuentran columnas que empiecen por 'H(' ",
      "en la tabla de entrada."
    )
  }
  
  ###########################################################################
  # 8.3 Extracción de K por instancia
  ###########################################################################
  # K puede estar repetido por fila, pero conceptualmente es el mismo valor.
  ###########################################################################
  K_vec <- tabla_H_instancias$K
  
  ###########################################################################
  # 8.4 Construcción de la matriz H(i,S)
  ###########################################################################
  # Convertimos las columnas H(S) a matriz numérica para operar eficientemente
  ###########################################################################
  H_mat <- as.matrix(tabla_H_instancias[, h_cols, drop = FALSE])
  
  ###########################################################################
  # 8.5 Cálculo central del método
  ###########################################################################
  # T(i,S) = H(i,S) - K
  # Se resta K fila a fila usando sweep()
  ###########################################################################
  T_mat <- sweep(H_mat, 1, K_vec, FUN = "-")
  
  ###########################################################################
  # 8.6 Renombrado de columnas: H(S) -> T(S)
  ###########################################################################
  colnames(T_mat) <- sub("^H", "T", colnames(T_mat))
  
  ###########################################################################
  # 8.7 Construcción de la tabla final T(i,S)
  ###########################################################################
  tabla_T <- data.frame(
    instancia   = tabla_H_instancias$instancia,
    T_mat,
    row.names   = NULL,
    check.names = FALSE
  )
  
  ###########################################################################
  # 8.8 Salida
  ###########################################################################
  # Tabla con:
  #  - una fila por instancia i
  #  - una columna por T(S)
  ###########################################################################
  tabla_T
}


###############################################################################
# PASO 9. MODELO CLÁSICO GLOBAL (LM / GLM) Y CÁLCULO DE K
###############################################################################
# OBJETIVO:
# ---------
# Ajustar un ÚNICO modelo clásico global (lm o glm logístico) sobre el dataset
# original y calcular el valor base K = H(∅).
#
# DEFINICIÓN DE K:
# ----------------
# - Regresión (lm):
#     K = media de las predicciones del modelo
#
# - Clasificación (glm logístico):
#     K = probabilidad asociada al intercepto (modelo vacío)
#
# IMPORTANTE:
#  - NO se ajustan modelos por submuestra
#  - NO se transforman los datos
#  - El modelo global queda FIJO y se reutiliza después
###############################################################################

ajustar_modelo_global_clasico <- function(df,
                                          target,
                                          tipo_modelo = c("regresion", "clasificacion"),
                                          formula = NULL,
                                          verbose = 0,
                                          seed = NULL) {
  
  ###########################################################################
  # 9.1 Selección del tipo de modelo
  ###########################################################################
  tipo_modelo <- match.arg(tipo_modelo)
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  ###########################################################################
  # 9.2 Validaciones básicas del dataset
  ###########################################################################
  df <- as.data.frame(df)
  
  if (!(target %in% names(df))) {
    stop("ERROR: el target '", target, "' no existe en el data.frame.")
  }
  
  ###########################################################################
  # 9.3 Definición de la fórmula del modelo
  ###########################################################################
  # Si no se pasa fórmula explícita:
  #   - se usan todos los predictores excepto el target
  ###########################################################################
  if (is.null(formula)) {
    
    vars_pred <- setdiff(names(df), target)
    
    if (length(vars_pred) == 0) {
      stop("ERROR: no hay predictores; solo está el target.")
    }
    
    formula <- stats::as.formula(
      paste(target, "~", paste(vars_pred, collapse = " + "))
    )
    
  } else {
    
    vars_in_formula <- all.vars(formula)
    vars_pred       <- setdiff(vars_in_formula, target)
  }
  
  if (verbose > 0) {
    cat("\nAjustando modelo clásico global (", tipo_modelo, ") con fórmula:\n  ",
        deparse(formula), "\n\n")
  }
  
  df_fit <- df
  
  ###########################################################################
  # 9.4 CASO CLASIFICACIÓN (GLM LOGÍSTICO)
  ###########################################################################
  if (tipo_modelo == "clasificacion") {
    
    # --- Validación del target ---
    y <- df_fit[[target]]
    
    if (is.logical(y)) {
      df_fit[[target]] <- as.integer(y)
      
    } else if (is.factor(y)) {
      
      if (nlevels(y) != 2) {
        stop("Para 'clasificacion', el target factor debe tener exactamente 2 niveles.")
      }
      
    } else if (is.numeric(y)) {
      
      vals <- unique(na.omit(y))
      if (!all(vals %in% c(0, 1))) {
        warning("Target numérico para clasificación no es 0/1 puro.")
      }
      
    } else {
      stop("Tipo de target no soportado para clasificación.")
    }
    
    # --- Ajuste GLM ---
    modelo <- stats::glm(
      formula = formula,
      data    = df_fit,
      family  = stats::binomial(link = "logit")
    )
    
    # Predicciones del modelo completo
    pred <- stats::predict(modelo, type = "response")
    
    #######################################################################
    # K en clasificación:
    # -------------------
    # K = H(∅) = probabilidad del modelo vacío
    #         = logistic(intercepto)
    #######################################################################
    beta0 <- coef(modelo)[["(Intercept)"]]
    K     <- plogis(beta0)
    
    ###########################################################################
    # 9.5 CASO REGRESIÓN (LM)
    ###########################################################################
  } else {
    
    modelo <- stats::lm(
      formula = formula,
      data    = df_fit
    )
    
    pred <- stats::predict(modelo, type = "response")
    
    #######################################################################
    # K en regresión:
    # ---------------
    # K = H(∅) = media de las predicciones del modelo global
    #######################################################################
    K <- mean(pred, na.rm = TRUE)
  }
  
  ###########################################################################
  # 9.6 Salida
  ###########################################################################
  list(
    modelo      = modelo,
    pred        = pred,
    K           = K,
    tipo_modelo = tipo_modelo,
    target      = target,
    formula     = formula,
    vars_pred   = vars_pred
  )
}



###############################################################################
# PASO 10. APLICAR EL MODELO CLÁSICO GLOBAL (LM / GLM) A TODAS LAS SUBMUESTRAS
###############################################################################
# OBJETIVO:
# ---------
# Aplicar un ÚNICO modelo clásico global (lm o glm) ya entrenado a todas las
# submuestras contrafactuales generadas en el PASO 1. Calcular predicciones
#
# Para cada combinación S y cada instancia i:
#   - Se calcula la predicción del modelo global
#   - Se guarda fila a fila en una nueva columna: pred_modelo
#
# IMPORTANTE:
#  - NO se reentrena ningún modelo
#  - NO se recalculan parámetros
#  - El modelo global es FIJO
#  - Compatible con regresión y clasificación
###############################################################################

ajustar_modelo_clasico_a_res <- function(res,
                                         obj_global,
                                         verbose = 0) {
  
  ###########################################################################
  # 10.1 Extraer contrato del modelo global (PASO 9)
  ###########################################################################
  target    <- obj_global$target
  vars_pred <- obj_global$vars_pred
  tipo      <- obj_global$tipo_modelo
  modelo    <- obj_global$modelo
  K_global  <- obj_global$K
  
  ###########################################################################
  # 10.2 Submuestras originales (sin predicciones)
  ###########################################################################
  subm_originales  <- res$submuestras
  submuestras_pred <- subm_originales
  
  ###########################################################################
  # 10.3 Bucle por combinaciones S
  ###########################################################################
  for (combo in names(subm_originales)) {
    
    if (verbose > 0) {
      cat("Procesando combinación (modelo clásico):", combo, "\n")
    }
    
    lista_inst <- subm_originales[[combo]]
    
    #########################################################################
    # 10.4 Bucle por instancias i
    #########################################################################
    for (inst_name in names(lista_inst)) {
      
      df_sub <- lista_inst[[inst_name]]
      
      #######################################################################
      # 10.5 Predicción con el modelo global clásico
      #######################################################################
      # lm  -> predict(..., type = "response")
      # glm -> predict(..., type = "response")
      #######################################################################
      pred_sub <- stats::predict(
        modelo,
        newdata = df_sub,
        type    = "response"
      )
      
      #######################################################################
      # 10.6 Guardar predicciones
      #######################################################################
      df_sub$pred_modelo <- pred_sub
      submuestras_pred[[combo]][[inst_name]] <- df_sub
    }
  }
  
  ###########################################################################
  # 10.7 Salida
  ###########################################################################
  list(
    submuestras_pred = submuestras_pred,
    target           = target,
    vars_pred        = vars_pred,
    tipo_modelo      = tipo,
    K                = K_global,
    nombre_col_pred  = "pred_modelo"
  )
}


###############################################################################
# 11. INSPECCIÓN DE SUBMUESTRAS (CON XGB)
###############################################################################

inspeccionar_todas_las_submuestras_xgb <- function(res, res_xgb, n_filas = 10) {
  
  cat("\n", bold$blue("=========== INSPECCIÓN COMPLETA DE SUBMUESTRAS (XGB) ==========="), "\n")
  cat(bold("Target:"), bold$green(as.character(res_xgb$target)), "\n")
  cat(bold("Predictores:"), paste(res_xgb$vars_pred, collapse = ", "), "\n")
  cat(bold("Tipo de modelo XGB:"), res_xgb$tipo, "\n\n")
  
  for (combo in names(res_xgb$submuestras_pred)) {
    
    vars_fijas <- res$combinaciones[[combo]]
    
    cat("\n", bold$yellow("-------------------------------------------------------------"), "\n")
    cat(bold$yellow("COMBINACIÓN:"), bold(combo), "\n")
    cat(bold("Variables fijadas:"), bold$magenta(paste(vars_fijas, collapse = ", ")), "\n\n")
    
    lista_inst <- res_xgb$submuestras_pred[[combo]]
    
    for (inst_name in names(lista_inst)) {
      
      df_sub <- lista_inst[[inst_name]]
      i      <- as.numeric(sub("instancia_", "", inst_name))
      
      cat(bold$green(" >> INSTANCIA FIJADA:"), bold$green(i), "\n")
      
      for (v in vars_fijas) {
        cat("     ", bold$cyan(paste0("• ", v, " = ")),
            yellow(as.character(df_sub[[v]][1])), "\n")
      }
      
      if ("pred_xgb" %in% names(df_sub)) {
        cat("     ", bold$blue("Columna de predicción XGB: 'pred_xgb' (numérica)\n"))
      }
      
      cat("\n  ", bold("Primeras filas de la submuestra (con pred_xgb):"), "\n")
      print(head(df_sub, n_filas))
      cat("\n")
    }
  }
}

###############################################################################
# 11.bis. INSPECCIÓN DE SUBMUESTRAS (MODELO CLÁSICO)
###############################################################################

inspeccionar_todas_las_submuestras_clasico <- function(res, res_mod, n_filas = 10) {
  
  cat("\n=========== INSPECCIÓN COMPLETA DE SUBMUESTRAS (MODELO CLÁSICO) ===========\n")
  cat("Target:", res_mod$target, "\n")
  cat("Predictores:", paste(res_mod$vars_pred, collapse = ", "), "\n")
  cat("Tipo de modelo:", res_mod$tipo_modelo, "\n\n")
  
  for (combo in names(res_mod$submuestras_pred)) {
    
    vars_fijas <- res$combinaciones[[combo]]
    
    cat("\n-------------------------------------------------------------\n")
    cat("COMBINACIÓN:", combo, "\n")
    cat("Variables fijadas:", paste(vars_fijas, collapse = ", "), "\n\n")
    
    lista_inst <- res_mod$submuestras_pred[[combo]]
    
    for (inst_name in names(lista_inst)) {
      
      df_sub <- lista_inst[[inst_name]]
      i      <- as.numeric(sub("instancia_", "", inst_name))
      
      cat(">> INSTANCIA FIJADA:", i, "\n")
      
      for (v in vars_fijas) {
        cat("   •", v, "=", as.character(df_sub[[v]][1]), "\n")
      }
      
      if ("pred_modelo" %in% names(df_sub)) {
        cat("   Columna de predicción (modelo clásico): 'pred_modelo'\n")
      } else {
        cat("   [AVISO] No existe columna 'pred_modelo' en esta submuestra.\n")
      }
      
      cat("\n  Primeras filas de la submuestra (incluye pred_modelo si existe):\n")
      print(utils::head(df_sub, n_filas))
      cat("\n")
    }
  }
}

###############################################################################
# PASO 12. STREAMING XGB – H(i,S), T(i,S) + CHECKS, SIN SHAPLEY
###############################################################################
# OBJETIVO:
# ---------
# Calcular directamente H(i,S) y T(i,S) SIN almacenar submuestras.
#
# Para cada combinación S y cada instancia i:
#  - Se construye la submuestra contrafactual en memoria
#  - Se aplica el modelo XGB global (FIJO)
#  - Se calcula:
#       H(i,S) = media de las predicciones
#       T(i,S) = H(i,S) - K
#
# IMPORTANTE:
#  - NO se almacenan submuestras
#  - NO se recalculan modelos
#  - K = H(∅) se mantiene como ancla
#  - Compatible con regresión y clasificación
###############################################################################

###############################################################################
# PASO 12. STREAMING XGB – H(i,S), T(i,S) + CHECKS, SIN SHAPLEY
###############################################################################

calcular_H_T_streaming_sin_shapley <- function(df,
                                               target,
                                               tipo_modelo = c("regresion", "clasificacion"),
                                               nrounds     = 200,
                                               params      = list(),
                                               verbose     = 0,
                                               seed        = NULL,
                                               guardar_checks = TRUE,
                                               obj_global  = NULL) {
  
  tipo_modelo <- match.arg(tipo_modelo)
  
  df <- as.data.frame(df)
  n  <- nrow(df)
  
  ###########################################################################
  # 12.1 Modelo global XGB (si no se pasa)
  ###########################################################################
  if (is.null(obj_global)) {
    obj_global <- ajustar_xgb_global(
      df      = df,
      target  = target,
      tipo    = tipo_modelo,
      nrounds = nrounds,
      params  = params,
      verbose = verbose,
      seed    = seed
    )
  } else {
    target      <- obj_global$target
    tipo_modelo <- obj_global$tipo_modelo
  }
  
  vars_pred     <- obj_global$vars_pred
  feature_names <- obj_global$feature_names
  
  ###########################################################################
  # 12.2 Combinaciones de predictores (✅ INCLUYE EL VACÍO)
  ###########################################################################
  lista_combos <- c(
    list(character(0)),  # S = ∅
    unlist(
      lapply(seq_along(vars_pred),
             function(k) combn(vars_pred, k, simplify = FALSE)),
      recursive = FALSE
    )
  )
  
  nombres_combos <- sapply(lista_combos, paste, collapse = "_")
  nombres_combos[1] <- "empty"  # nombre explícito para el vacío
  names(lista_combos) <- nombres_combos
  
  n_comb <- length(lista_combos)
  
  ###########################################################################
  # 12.3 Estructuras H(i,S)
  ###########################################################################
  H_mat     <- matrix(NA_real_, nrow = n, ncol = n_comb)
  H_nombres <- character(n_comb)
  
  ###########################################################################
  # 12.4 Checks (opcional)
  ###########################################################################
  checks_list <- if (guardar_checks) vector("list", n * n_comb) else NULL
  idx_check   <- 1L
  
  vars_all <- names(df)
  
  ###########################################################################
  # 12.5 Bucle principal streaming
  ###########################################################################
  for (j in seq_along(lista_combos)) {
    
    cols_fijas <- lista_combos[[j]]
    combo_name <- nombres_combos[j]
    
    # Nombre H(S)
    H_nombres[j] <- if (length(cols_fijas) == 0) {
      "H()"
    } else {
      nombre_H_desde_vars(cols_fijas)
    }
    
    cols_libres <- setdiff(vars_all, cols_fijas)
    
    for (i in seq_len(n)) {
      
      # Submuestra contrafactual
      df_sub <- df
      for (col in cols_fijas) {
        df_sub[[col]] <- df[[col]][i]
      }
      
      # Preparar X alineada
      X_sub <- preparar_X_sub_para_global(
        df_sub        = df_sub,
        target        = target,
        vars_pred     = vars_pred,
        feature_names = feature_names
      )
      
      dsub     <- xgboost::xgb.DMatrix(data = X_sub)
      pred_sub <- predict(obj_global$modelo, newdata = dsub)
      
      #######################################################################
      # Cálculo central
      #######################################################################
      H_mat[i, j] <- mean(pred_sub, na.rm = TRUE)
      
      #######################################################################
      # Checks (opcional)
      #######################################################################
      if (guardar_checks) {
        ok_fijas <- all(
          sapply(cols_fijas, function(col) all(df_sub[[col]] == df[[col]][i]))
        )
        ok_libres <- all(
          sapply(cols_libres, function(col) all(df_sub[[col]] == df[[col]]))
        )
        
        checks_list[[idx_check]] <- data.frame(
          combinacion    = combo_name,
          columnas_fijas = paste(cols_fijas, collapse = ","),
          instancia      = i,
          ok_fijas       = ok_fijas,
          ok_libres      = ok_libres,
          ok_total       = ok_fijas & ok_libres,
          stringsAsFactors = FALSE
        )
        idx_check <- idx_check + 1L
      }
      
      rm(df_sub, X_sub, dsub, pred_sub)
    }
  }
  
  ###########################################################################
  # 12.6 Construcción tabla H(i,S)
  ###########################################################################
  colnames(H_mat) <- H_nombres
  
  tabla_H_instancias <- data.frame(
    instancia = seq_len(n),
    H_mat,
    row.names   = NULL,
    check.names = FALSE
  )
  
  ###########################################################################
  # 12.7 K = H(∅) CALCULADO (NO EXTERNO)
  ###########################################################################
  col_H_empty <- which(colnames(tabla_H_instancias) == "H()")
  
  K_calculado <- unique(tabla_H_instancias[[col_H_empty]])
  stopifnot(length(K_calculado) == 1)
  
  tabla_H_instancias$K <- K_calculado
  
  ###########################################################################
  # 12.8 Tabla T(i,S)
  ###########################################################################
  tabla_T_instancias <- calcular_T_por_instancia(tabla_H_instancias)
  
  ###########################################################################
  # 12.9 Checks finales
  ###########################################################################
  verificacion <- NULL
  if (guardar_checks) {
    verificacion <- do.call(rbind, checks_list[seq_len(idx_check - 1L)])
  }
  
  ###########################################################################
  # 12.10 Salida
  ###########################################################################
  list(
    obj_global         = obj_global,
    vars_pred          = vars_pred,
    K                  = K_calculado,
    tabla_H_instancias = tabla_H_instancias,
    tabla_T_instancias = tabla_T_instancias,
    verificacion       = verificacion
  )
}



###############################################################################
# PASO 13. STREAMING CLÁSICO (lm / glm) – H(i,S), T(i,S) + CHECKS
###############################################################################
# OBJETIVO:
# ---------
# Calcular directamente H(i,S) y T(i,S) SIN almacenar submuestras,
# utilizando un modelo clásico global (lm o glm) ya entrenado.
#
# Para cada combinación S y cada instancia i:
#  - Se construye la submuestra contrafactual en memoria
#  - Se aplica el modelo clásico global (FIJO)
#  - Se calcula:
#       H(i,S) = media de las predicciones
#       T(i,S) = H(i,S) - K
#
# IMPORTANTE:
#  - NO se almacenan submuestras
#  - NO se recalculan modelos
#  - K = H(∅) se mantiene como ancla
#  - Compatible con regresión y clasificación
###############################################################################

###############################################################################
# PASO 13. STREAMING CLÁSICO (lm / glm) – H(i,S), T(i,S) + CHECKS
###############################################################################

calcular_H_T_streaming_sin_shapley_clasico <- function(df,
                                                       target,
                                                       tipo_modelo = c("regresion", "clasificacion"),
                                                       verbose     = 0,
                                                       seed        = NULL,
                                                       guardar_checks = TRUE,
                                                       obj_global  = NULL,
                                                       formula     = NULL) {
  
  tipo_modelo <- match.arg(tipo_modelo)
  
  df <- as.data.frame(df)
  n  <- nrow(df)
  
  ###########################################################################
  # 13.1 Modelo global clásico (si no se pasa)
  ###########################################################################
  if (is.null(obj_global)) {
    obj_global <- ajustar_modelo_global_clasico(
      df          = df,
      target      = target,
      tipo_modelo = tipo_modelo,
      formula     = formula,
      verbose     = verbose,
      seed        = seed
    )
  } else {
    target      <- obj_global$target
    tipo_modelo <- obj_global$tipo_modelo
  }
  
  vars_pred <- obj_global$vars_pred
  
  ###########################################################################
  # 13.2 Combinaciones de predictores (✅ INCLUYE EL VACÍO)
  ###########################################################################
  lista_combos <- c(
    list(character(0)),  # S = ∅
    unlist(
      lapply(seq_along(vars_pred),
             function(k) combn(vars_pred, k, simplify = FALSE)),
      recursive = FALSE
    )
  )
  
  nombres_combos <- sapply(lista_combos, paste, collapse = "_")
  nombres_combos[1] <- "empty"
  names(lista_combos) <- nombres_combos
  
  n_comb <- length(lista_combos)
  
  ###########################################################################
  # 13.3 Estructuras H(i,S)
  ###########################################################################
  H_mat     <- matrix(NA_real_, nrow = n, ncol = n_comb)
  H_nombres <- character(n_comb)
  
  ###########################################################################
  # 13.4 Checks (opcional)
  ###########################################################################
  checks_list <- if (guardar_checks) vector("list", n * n_comb) else NULL
  idx_check   <- 1L
  
  vars_all <- names(df)
  
  ###########################################################################
  # 13.5 Bucle principal streaming
  ###########################################################################
  for (j in seq_along(lista_combos)) {
    
    cols_fijas <- lista_combos[[j]]
    combo_name <- nombres_combos[j]
    
    # Nombre H(S)
    
    # Nombre H(S) — fuente única de verdad
    H_nombres[j] <- nombre_H_desde_vars(cols_fijas)
    
    
    cols_libres <- setdiff(vars_all, cols_fijas)
    
    for (i in seq_len(n)) {
      
      # Submuestra contrafactual
      df_sub <- df
      for (col in cols_fijas) {
        df_sub[[col]] <- df[[col]][i]
      }
      
      # Predicción con modelo clásico global
      pred_sub <- stats::predict(
        obj_global$modelo,
        newdata = df_sub,
        type    = "response"
      )
      
      #######################################################################
      # Cálculo central
      #######################################################################
      H_mat[i, j] <- mean(pred_sub, na.rm = TRUE)
      
      #######################################################################
      # Checks (opcional)
      #######################################################################
      if (guardar_checks) {
        ok_fijas <- all(
          sapply(cols_fijas, function(col) all(df_sub[[col]] == df[[col]][i]))
        )
        ok_libres <- all(
          sapply(cols_libres, function(col) all(df_sub[[col]] == df[[col]]))
        )
        
        checks_list[[idx_check]] <- data.frame(
          combinacion    = combo_name,
          columnas_fijas = paste(cols_fijas, collapse = ","),
          instancia      = i,
          ok_fijas       = ok_fijas,
          ok_libres      = ok_libres,
          ok_total       = ok_fijas & ok_libres,
          stringsAsFactors = FALSE
        )
        idx_check <- idx_check + 1L
      }
      
      rm(df_sub, pred_sub)
    }
  }
  
  ###########################################################################
  # 13.6 Construcción tabla H(i,S)
  ###########################################################################
  colnames(H_mat) <- H_nombres
  
  tabla_H_instancias <- data.frame(
    instancia = seq_len(n),
    H_mat,
    row.names   = NULL,
    check.names = FALSE
  )
  
  ###########################################################################
  # 13.7 K = H(∅) CALCULADO (NO EXTERNO)
  ###########################################################################
  col_H_empty <- which(colnames(tabla_H_instancias) == "H()")
  
  K_calculado <- unique(tabla_H_instancias[[col_H_empty]])
  stopifnot(length(K_calculado) == 1)
  
  tabla_H_instancias$K <- K_calculado
  
  ###########################################################################
  # 13.8 Tabla T(i,S)
  ###########################################################################
  tabla_T_instancias <- calcular_T_por_instancia(tabla_H_instancias)
  
  ###########################################################################
  # 13.9 Checks finales
  ###########################################################################
  verificacion <- NULL
  if (guardar_checks) {
    verificacion <- do.call(rbind, checks_list[seq_len(idx_check - 1L)])
  }
  
  ###########################################################################
  # 13.10 Salida
  ###########################################################################
  list(
    obj_global         = obj_global,
    vars_pred          = vars_pred,
    K                  = K_calculado,
    tabla_H_instancias = tabla_H_instancias,
    tabla_T_instancias = tabla_T_instancias,
    verificacion       = verificacion
  )
}

# HNANESI_clean debe venir de tu pipeline previo
HNANESI_y   <- eliminar_vars(HNANESI_clean, c("y_b"))
HNANESI_log <- eliminar_vars(HNANESI_clean, c("y"))


###############################################################################
# 15. ESQUEMA DE USO – CON BUCLE (STREAMING XGB Y CLÁSICO)
###############################################################################

# XGB REGRESIÓN STREAMING
obj_global_num_xgb <- ajustar_xgb_global(HNANESI_y, "y", tipo = "regresion", nrounds = 200, seed = 123)
res_stream_y_xgb <- calcular_H_T_streaming_sin_shapley(
  df             = HNANESI_y,
  target         = "y",
  tipo_modelo    = "regresion",
  nrounds        = 200,
  seed           = 123,
  obj_global     = obj_global_num_xgb,
  guardar_checks = TRUE
)
M1_Pred_xgb_y_stream <- res_stream_y_xgb$tabla_H_instancias
M1_Delta_xgb_y_stream <- res_stream_y_xgb$tabla_T_instancias


write_xlsx(
  x = M1_Pred_xgb_y_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/XGB_reg/M1_Pred_xgb_y_stream.xlsx"
)
write_xlsx(
  x = M1_Delta_xgb_y_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/XGB_reg/M1_Delta_xgb_y_stream.xlsx"
)

# XGB CLASIFICACIÓN STREAMING
obj_global_yb_xgb <- ajustar_xgb_global(HNANESI_log, "y_b", tipo = "clasificacion", nrounds = 200, seed = 123)
res_stream_yb_xgb <- calcular_H_T_streaming_sin_shapley(
  df             = HNANESI_log,
  target         = "y_b",
  tipo_modelo    = "clasificacion",
  nrounds        = 200,
  seed           = 123,
  obj_global     = obj_global_yb_xgb,
  guardar_checks = TRUE
)
M1_Pred_xgb_yb_stream <- res_stream_yb_xgb$tabla_H_instancias
M1_Delta_xgb_yb_stream <- res_stream_yb_xgb$tabla_T_instancias

write_xlsx(
  x = M1_Pred_xgb_yb_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/XGB_log/M1_Pred_xgb_log_stream.xlsx"
)
write_xlsx(
  x = M1_Delta_xgb_yb_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/XGB_log/M1_Delta_xgb_log_stream.xlsx"
)

# LM REGRESIÓN STREAMING
obj_global_y_lm <- ajustar_modelo_global_clasico(HNANESI_y, "y", tipo_modelo = "regresion", seed = 123)
res_stream_y_lm <- calcular_H_T_streaming_sin_shapley_clasico(
  df             = HNANESI_y,
  target         = "y",
  tipo_modelo    = "regresion",
  seed           = 123,
  obj_global     = obj_global_y_lm,
  guardar_checks = TRUE
)
M1_Pred_lm_y_stream <- res_stream_y_lm$tabla_H_instancias
M1_Delta_lm_y_stream <- res_stream_y_lm$tabla_T_instancias

write_xlsx(
  x = M1_Delta_lm_y_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/lm/M1_Delta_lm_y_stream.xlsx"
)
write_xlsx(
  x = M1_Pred_lm_y_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/lm/M1_Pred_lm_y_stream.xlsx"
)

# GLM CLASIFICACIÓN STREAMING
obj_global_yb_glm <- ajustar_modelo_global_clasico(HNANESI_log, "y_b", tipo_modelo = "clasificacion", seed = 123)
res_stream_yb_glm <- calcular_H_T_streaming_sin_shapley_clasico(
  df             = HNANESI_log,
  target         = "y_b",
  tipo_modelo    = "clasificacion",
  seed           = 123,
  obj_global     = obj_global_yb_glm,
  guardar_checks = TRUE
)
M1_Pred_glm_yb_stream <- res_stream_yb_glm$tabla_H_instancias
M1_Delta_glm_yb_stream <- res_stream_yb_glm$tabla_T_instancias


