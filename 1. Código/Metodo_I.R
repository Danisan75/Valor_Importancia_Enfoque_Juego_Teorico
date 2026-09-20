

# ============================================================================
# 1. GENERACIÓN DE SUBMUESTRAS FIJADAS POR COALICIONES
# ============================================================================
#
# Objetivo:
# Generar las submuestras necesarias para evaluar las predicciones y variaciones
# de predicción de todas las coaliciones posibles de variables explicativas.
#
# Generación de coaliciones:
#
# - Se generan todas las combinaciones posibles de variables
#   explicativas.
#
# - Se incluye la coalición vacía.
#
# Ejemplo:
#
# Si las variables explicativas son:
#
#     A, B y C
#
# Se generan las coaliciones:
#
#     empty
#     A
#     B
#     C
#     A+B
#     A+C
#     B+C
#     A+B+C
#
# Construcción de submuestras:
#
# - Para cada coalición S y para cada instancia i se genera una
#   submuestra.
#
# - Las variables pertenecientes a la coalición S se fijan utilizando
#   los valores observados en la instancia i.
#
# - Las variables no pertenecientes a S conservan sus valores originales
#   en todas las observaciones de la base de datos.
#
# Ejemplo:
#
#     S = {A, B}
#     i = 5
#
# Resultado:
#
# - Las variables A y B permanecen constantes con los valores de la
#   instancia 5.
#
# - El resto de variables mantienen sus valores originales en toda la
#   base de datos.
#
# Validación:
#
# - Se comprueba que las variables fijadas permanecen constantes.
#
# - Se comprueba que las variables no incluidas en S conservan sus
#   valores originales.
#
# Resultado:
#
# - Una submuestra fijada para cada combinación de coalición S e
#   instancia i.
#
# - Conjunto completo de submuestras asociadas a todas las coaliciones
#   generadas.
#
# ============================================================================

generar_submuestras_combinatoria <- function(df, target) {
as.data.frame(df)
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
# TODAS las combinaciones: INCLUYE EL VACÍO
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


# ============================================================================
# 2. PREPARACIÓN DE LAS MATRICES DE PREDICTORES PARA LAS COALICIONES GENERADAS
# ============================================================================
#
# Objetivo:
# Construir las matrices de predictores (X) asociadas a cada una de las
# coaliciones generadas previamente.
#
# Metodología:
# - Para cada coalición S se construye una matriz de predictores X.
# - La información disponible depende de las variables incluidas en S.
#
# Ejemplos:
#
# S = empty
# - No se dispone de información de ninguna variable en la instancia fijada.
#
# S = {A}
# - Solo se dispone de la información de la variable A en la instancia fijada.
#
# S = {A, B}
# - Solo se dispone de la información de las variables A y B en la
#   instancia fijada.
#
# S = {A, B, C}
# - Solo se dispone de la información de las variables A, B y C en la
#   instancia fijada.
#
# Resultado:
# - Para cada coalición se construye una matriz de predictores X.
# - La información disponible en X depende de las variables incluidas en
#   la coalición considerada.
# - Las matrices obtenidas quedan preparadas para el calculo de la variación de 
#   predicción mediante el modelo.
#
# ============================================================================

preparar_xy_desde_submuestra <- function(df_sub, target, vars_pred) {
  

  df_sub <- as.data.frame(df_sub)
  

  if (!all(vars_pred %in% names(df_sub))) {
    faltan <- vars_pred[!vars_pred %in% names(df_sub)]
    stop(
      "ERROR (preparar_xy_desde_submuestra): faltan columnas en df_sub: ",
      paste(faltan, collapse = ", ")
    )
  }
  

  y    <- df_sub[[target]]
  X_df <- df_sub[, vars_pred, drop = FALSE]
  
-
  if (ncol(X_df) == 0) {
    stop(
      "ERROR (preparar_xy_desde_submuestra): X_df está vacío; ",
      "no hay predictores en esta submuestra."
    )
  }
  

  X_mat <- stats::model.matrix(~ . - 1, data = X_df)
  

  if (is.null(X_mat) || ncol(X_mat) == 0) {
    stop(
      "ERROR (preparar_xy_desde_submuestra): ",
      "model.matrix produjo una matriz vacía."
    )
  }
  
  list(
    X_mat = X_mat,
    y     = y
  )
}

# ============================================================================
# 3. PREPARACIÓN DE X E Y DEL DATASET ORIGINAL
# ============================================================================
#
# Objetivo:
# Preparar la base de datos original para calculos posteriores.
#
# Metodología:
#
# - La variable objetivo se separa del resto de variables de la base de
#   datos.
#
# - Las variables restantes constituyen la matriz de
#   predictores X.
#
#
# Interpretación:
#
# - La matriz X contiene toda la información explicativa disponible en
#   la base de datos.
#
# - Esta estructura se utilizará posteriormente para el calculo de 
# S={∅}
#
# Resultado:
#
# - Matriz global de predictores (X).
# ============================================================================

preparar_xy_global <- function(df, target, vars_pred = NULL) {
  
  # Aseguramos data.frame
  df <- as.data.frame(df)
  

  if (!(target %in% names(df))) {
    stop(
      "ERROR (preparar_xy_global): target '",
      target,
      "' no existe en el data.frame."
    )
  }
  

  if (is.null(vars_pred)) {
    vars_pred <- setdiff(names(df), target)
  }
  

  if (length(vars_pred) == 0) {
    stop(
      "ERROR (preparar_xy_global): ",
      "no quedan predictores; vars_pred está vacío."
    )
  }
  

  y    <- df[[target]]
  X_df <- df[, vars_pred, drop = FALSE]
  
  if (ncol(X_df) == 0) {
    stop(
      "ERROR (preparar_xy_global): X_df global está vacío."
    )
  }
  

  X_mat <- stats::model.matrix(~ . - 1, data = X_df)
  

  if (is.null(X_mat) || ncol(X_mat) == 0) {
    stop(
      "ERROR (preparar_xy_global): ",
      "model.matrix global devolvió matriz vacía."
    )
  }
  

  list(
    X_mat     = X_mat,
    y         = y,
    vars_pred = vars_pred
  )
}


# =====================================================================================
# 4. PREDICCIÓN DE S = {∅} AL QUE LLAMAREMOS K (XGBOOST)
# =====================================================================================
#
# Objetivo:
# Predicción media cuando cuando S = {∅}, no es conocida ninguna de las variables
# de la base de datos.
#
# Metodología:
# - Se utiliza la matriz global de predictores X construida previamente.
# - Se ajusta un único modelo XGBoost utilizando toda la base de datos.
# - El modelo se entrena una única vez.
#
# Interpretación:
# - Representa el escenario de desconocimiento completo.
#
# Cálculo de K:
# - Una vez ajustado el modelo global, se calculan las predicciones
#   para todas las instancias de la base de datos.
# - K - (y′1(∅)) se define como la media de dichas predicciones.
#
# Resultado:
# - Modelo XGBoost entrenado.
# - Predicciones asociadas al modelo.
# - Valor K correspondiente a la media de las predicciones.
#
# ============================================================================


ajustar_xgb_global <- function(df,
                               target,
                               tipo    = c("regresion", "clasificacion"),
                               nrounds = 200,
                               params  = list(),
                               verbose = 0,
                               seed    = NULL) {
  
  tipo <- match.arg(tipo)
  
  if (!is.null(seed)) set.seed(seed)
  
 
  prep      <- preparar_xy_global(df, target = target)
  X_mat     <- prep$X_mat
  y         <- prep$y
  vars_pred <- prep$vars_pred
  
 
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
  
  # K = media de predicciones y′1(∅)
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



# ============================================================================
# 5. FUNCIÓN AUXILIAR: PREPARACIÓN DE MATRICES PARA LOS CÁLCULOS
# ============================================================================
#
# Objetivo:
# Adaptar las matrices X construidas para cada coalición a la misma
# estructura utilizada por el modelo global para evitar errores de formato.
#
# Metodología:
# - El modelo global se construye utilizando toda la información
#   disponible de la base de datos.
# - Cada coalición S representa un nivel distinto de conocimiento
#   sobre una instancia.
# - Para poder comparar todas las coaliciones de forma consistente,
#   todas las matrices X deben compartir la misma estructura.
#
# Ejemplos:
#
# S = empty
# - No se dispone de información de ninguna variable en la instancia fijada.
#
# S = {A}
# - Solo se dispone de la información de la variable A en la instancia fijada.
#
# S = {A, B}
# - Solo se dispone de la información de las variables A y B
#   en la instancia fijada.
#
# S = {A, B, C}
# - Se dispone de la información de todas las variables
#   en la instancia fijada.
#
# Resultado:
# - Todas las coaliciones quedan representadas mediante la misma
#   estructura de variables utilizada por el modelo global.
# - Las matrices resultantes son comparables entre sí.
#
# ============================================================================

preparar_X_sub_para_global <- function(df_sub,
                                       target,
                                       vars_pred,
                                       feature_names) {
  

  prep_sub <- preparar_xy_desde_submuestra(
    df_sub   = df_sub,
    target   = target,
    vars_pred = vars_pred
  )
  
  X_raw <- prep_sub$X_mat

  n_fil <- nrow(X_raw)
  n_col <- length(feature_names)
  

  X_aligned <- matrix(0, nrow = n_fil, ncol = n_col)
  colnames(X_aligned) <- feature_names
  

  cols_comunes <- intersect(feature_names, colnames(X_raw))
  
  if (length(cols_comunes) > 0) {
    X_aligned[, cols_comunes] <- X_raw[, cols_comunes, drop = FALSE]
  }
  

  X_aligned
}

# ============================================================================
# 6. PREDICCIONES PARA CADA COALICIÓN y′1({S}) UTILIZANDO XGB
# ============================================================================
#
# Objetivo:
# Obtener las predicciones del modelo XGBoost para todas las
# coaliciones generadas previamente.
#
# ¿Qué hace la función?
#
# - Recorre todas las coaliciones S.
# - Prepara la matriz de datos correspondiente.
# - Aplica el modelo XGBoost ya entrenado.
# - Obtiene las predicciones mediante predict().
# - Guarda las predicciones asociadas a cada coalición.
#
# Importante:
#
# El modelo no se vuelve a entrenar.
#
# Siempre se utiliza el mismo modelo global ajustado previamente.
#
# Resultado:
#
# Para cada coalición S se obtiene un vector de predicciones del
# modelo, que posteriormente se utilizará para calcular las variaciones 
# de las variables.
# A estas predicciones se las denomina H(S)
#
# ============================================================================


ajustar_xgb_a_res <- function(res,
                              obj_global,
                              verbose = 0) {
  

  target        <- obj_global$target
  vars_pred     <- obj_global$vars_pred
  tipo          <- obj_global$tipo_modelo
  feature_names <- obj_global$feature_names
  modelo        <- obj_global$modelo
  K_global      <- obj_global$K
  

  subm_originales  <- res$submuestras
  submuestras_pred <- subm_originales
  

  for (combo in names(subm_originales)) {
    
    if (verbose > 0) {
      cat("Procesando combinación:", combo, "\n")
    }
    
    lista_inst <- subm_originales[[combo]]
    

    for (inst_name in names(lista_inst)) {
      
      df_sub <- lista_inst[[inst_name]]
      

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



# ============================================================================
# 7. NOMENCLATURA DE LAS PREDICCIONES y′1({S}) ->H(S)
# ============================================================================
#
# Objetivo:
# Asignar un nombre identificativo a cada coalición S.
#
# Metodología:
# - La coalición vacía se representa como:
#
#     H()
#
# - Para el resto de coaliciones:
#
#     * Se toman las variables que forman S.
#     * Se utilizan las tres primeras letras de cada variable.
#     * Las abreviaturas se concatenan mediante "_".
#
# Ejemplos:
#
# S = {}
# → H()
#
# S = {edad}
# → H(eda)
#
# S = {edad, ingresos}
# → H(eda_ing)
#
# S = {edad, ingresos, genero}
# → H(eda_ing_gen)
#
# Resultado:
# - Nombre único asociado a cada coalición.
# - Identificación consistente a lo largo de todo el procedimiento.
#
# ============================================================================

nombre_H_desde_combo <- function(combo) {
  
  #------------------------------------------------------------
  # PASO 7.1.1: Validación de la entrada
  #------------------------------------------------------------
  stopifnot(
    is.character(combo),
    length(combo) == 1,
    !is.na(combo),
    nchar(combo) > 0
  )
  
  #------------------------------------------------------------
  # PASO 7.1.2: Caso especial — conjunto vacío
  #------------------------------------------------------------
  # "empty" representa S = ∅ y DEBE producir siempre H()
  if (combo == "empty") {
    return("H()")
  }
  
  #------------------------------------------------------------
  # PASO 7.1.3: Separar variables fijadas
  #------------------------------------------------------------
  vars_fijas <- strsplit(combo, "_", fixed = TRUE)[[1]]
  
  stopifnot(
    length(vars_fijas) > 0,
    all(nchar(vars_fijas) > 0)
  )
  
  #------------------------------------------------------------
  # PASO 7.1.4: Abreviatura (3 primeras letras)
  #------------------------------------------------------------
  abrevs <- substring(vars_fijas, 1, 3)
  
  stopifnot(
    all(nchar(abrevs) > 0),
    !any(is.na(abrevs))
  )
  
  #------------------------------------------------------------
  # PASO 7.1.5: Advertencia por colisiones (no bloqueante)
  #------------------------------------------------------------
  if (any(duplicated(abrevs))) {
    warning(
      "PASO 6: posibles colisiones de nombres en H(S): ",
      paste(abrevs, collapse = ", ")
    )
  }
  
  #------------------------------------------------------------
  # PASO 7.1.6: Construcción final del nombre H(S)
  #------------------------------------------------------------
  paste0("H(", paste(abrevs, collapse = "_"), ")")
}

# ============================================================================
# 7.2 FUNCIÓN AUX. CONSTRUCCIÓN DEL NOMBRE H(S) 
# ============================================================================




nombre_H_desde_vars <- function(vars_fijas) {
  
  #------------------------------------------------------------
  # PASO 7.2.1: Caso especial — conjunto vacío S = ∅
  #------------------------------------------------------------
  if (length(vars_fijas) == 0) {
    return("H()")
  }
  
  #------------------------------------------------------------
  # PASO 7.2.2: Validación de entrada
  #------------------------------------------------------------
  stopifnot(
    is.character(vars_fijas),
    all(nchar(vars_fijas) > 0),
    !any(is.na(vars_fijas))
  )
  
  #------------------------------------------------------------
  # PASO 7.2.3: Abreviatura (3 primeras letras)
  #------------------------------------------------------------
  abrevs <- substring(vars_fijas, 1, 3)
  
  #------------------------------------------------------------
  # PASO 7.2.4: Advertencia por colisiones (no bloqueante)
  #------------------------------------------------------------
  if (any(duplicated(abrevs))) {
    warning(
      "PASO 6: posibles colisiones de nombres en H(S): ",
      paste(abrevs, collapse = ", ")
    )
  }
  
  #------------------------------------------------------------
  # PASO 7.2.5: Construcción final del nombre H(S)
  #------------------------------------------------------------
  paste0("H(", paste(abrevs, collapse = "_"), ")")
}


# ============================================================================
# 8. CONSTRUCCIÓN DE LA TABLA RESUMEN DE PREDICCIONES H(i,S) PARA TODO S
# ============================================================================
#
# Objetivo:
# Calcular los valores H(i,S) para todas las instancias y para todas
# las coaliciones generadas previamente.
#
# Metodología:
# - Para cada coalición S.
# - Para cada instancia i.
# - Se utilizan las predicciones obtenidas mediante la aplicación del
#   modelo global a la submuestra correspondiente.
# - H(i,S) se define como la media de dichas predicciones.
#
# Interpretación:
#
# H(i,S)
#
# representa la predicción media obtenida cuando únicamente se dispone
# de la información contenida en la coalición S para la instancia i.
#
# Ejemplos:
#
# H(i,∅)
# - No se dispone de información de ninguna variable en la instancia.
#
# H(i,{A})
# - Solo se dispone de la información de la variable A.
#
# H(i,{A,B})
# - Solo se dispone de la información de las variables A y B.
#
# H(i,{A,B,C})
# - Se dispone de la información de las variables A, B y C.
#
# Valor de referencia:
# - K se define como H(∅).
# - Representa el valor de referencia asociado a la ausencia total
#   de información.
#
# Resultado:
# - Una tabla con una fila por instancia.
# - Una columna H(S) para cada coalición.
# - Una columna K como referencia global.
#
# ============================================================================

tabla_H_por_instancia <- function(res_modelo,
                                  nombre_col_pred = "pred_xgb") {
  

  subm_pred <- res_modelo$submuestras_pred
  combos    <- names(subm_pred)
  
  # Comprobación básica: debe haber combinaciones
  if (length(combos) == 0) {
    stop("No hay combinaciones en res_modelo$submuestras_pred.")
  }
  

  primera_combo <- combos[1]
  lista_inst    <- subm_pred[[primera_combo]]
  inst_names    <- names(lista_inst)
  
  # Extraemos los identificadores numéricos de instancia:
  # "instancia_1" -> 1, etc.
  inst_ids   <- as.integer(sub("instancia_", "", inst_names))
  inst_orden <- sort(inst_ids)
  

  n_inst <- length(inst_orden)   # nº de instancias
  n_comb <- length(combos)       # nº de combinaciones S
  
  # Matriz donde se almacenará H(i,S)
  mat_H     <- matrix(NA_real_, nrow = n_inst, ncol = n_comb)
  H_nombres <- character(n_comb)
  

  for (j in seq_along(combos)) {
    
    combo        <- combos[j]
    
    # Nombre H(S) según el PASO 6 (nomenclatura coherente)
    H_nombre     <- nombre_H_desde_combo(combo)
    H_nombres[j] <- H_nombre
    
    # Submuestras de todas las instancias para esta combinación S
    lista_inst_combo <- subm_pred[[combo]]
    

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
          "' en una submuestra. ¿Se ha ejecutado correctamente el paso anterior?"
        )
      }
      

      mat_H[fila_i, j] <- mean(df_sub[[nombre_col_pred]], na.rm = TRUE)
    }
  }
  

  colnames(mat_H) <- H_nombres
  
  tabla <- data.frame(
    instancia   = inst_orden,
    mat_H,
    row.names   = NULL,
    check.names = FALSE
  )
  

  
  # Nombre canónico de H(∅)
  H_VACIO <- "H()"
  
  # Buscar la columna H(∅)
  col_H_empty <- which(names(tabla) == H_VACIO)
  
  # Comprobaciones defensivas
  if (length(col_H_empty) == 0) {
    stop(
      "No se encuentra la columna '", H_VACIO, 
      "' en la tabla H(i,S)."
    )
  }
  
  if (length(col_H_empty) > 1) {
    stop(
      "Hay más de una columna '", H_VACIO, 
      "'. Los nombres H(S) no son únicos."
    )
  }
  
  # Extraer K desde la tabla
  K_calculado <- unique(tabla[[col_H_empty]])
  
  # K debe ser único
  if (length(K_calculado) != 1 || is.na(K_calculado)) {
    stop(
      "K = H(∅) no es único o es NA. ",
      "Revisa las predicciones del modelo global."
    )
  }
  
  # Añadir K = H(∅)
  tabla$K <- K_calculado
  

  tabla
}

# ============================================================================
# 9. CÁLCULO DE LA VARIACION DE PREDICCIÓN Δ1({S})DENOMINADA T(i,S)
# ============================================================================
#
# Objetivo:
# Calcular el efecto asociado a cada coalición S para cada instancia i.
#
# Metodología:
# - Se parte de la tabla H(i,S) calculada previamente.
# - Se utiliza K = H(∅) como y′1(∅).
# - Para cada instancia i y cada coalición S se calcula:
#
#     T(i,S) = H(i,S) - K
#
# Interpretación:
# - T(i,S) mide la variación de predicción producida al disponer de la información
#   contenida en la coalición S respecto al escenario sin información.
#
# Ejemplos:
#
# T(i,{A})
# - Variación de predicción cuando S={A}.
#
# T(i,{A,B})
# - Variación de predicción cuando S={A,B}.
#
#
# Resultado:
# - Una tabla con una fila por instancia.
# - Una columna T(S) para cada coalición.
#
# ============================================================================

calcular_T_por_instancia <- function(tabla_H_instancias) {
  
  # Comprobación de la existencia de K
  if (!("K" %in% names(tabla_H_instancias))) {
    stop(
      "La tabla de entrada debe contener la columna 'K'."
    )
  }
  
  # Columnas H(S)
  h_cols <- grep("^H\\(", names(tabla_H_instancias), value = TRUE)
  
  if (length(h_cols) == 0) {
    stop(
      "No se encuentran columnas H(S) en la tabla de entrada."
    )
  }
  

  K_vec <- tabla_H_instancias$K
  H_mat <- as.matrix(tabla_H_instancias[, h_cols, drop = FALSE])
  
  # T(i,S) = H(i,S) - K
  T_mat <- sweep(H_mat, 1, K_vec, FUN = "-")
  
  # Renombrado H(S) -> T(S)
  colnames(T_mat) <- sub("^H", "T", colnames(T_mat))
  
  # Tabla final
  tabla_T <- data.frame(
    instancia   = tabla_H_instancias$instancia,
    T_mat,
    row.names   = NULL,
    check.names = FALSE
  )

  tabla_T
}


# ============================================================================
# 10. PREDICCIÓN DE S = {∅} (k) DE MODELOS CLÁSICOS (LM, GLM)
# ============================================================================
#
# Objetivo:
# Predicción y′1(∅)=K en los modelos clásicos utilizando todo el dataset.
#
# Metodología:
# - Se utiliza toda la información disponible de la base de datos.
# - Se ajusta un único modelo global utilizando toda la base de datos.
# - El modelo se entrena una única vez.
#
# Modelos considerados:
#
# Regresión:
# - Modelo lineal (LM).
#
# Clasificación:
# - Modelo lineal generalizado binario (GLM Logístico).
#
# Interpretación:
# - Representa el escenario de desconocimiento completo de información.
#
# Cálculo de K:
#
# Regresión:
# - K se define como la media de las predicciones obtenidas por el
#   modelo lm cuando no disponemos de información.
#
# Clasificación:
# - K se define como la predicción asociada al modelo logístico cuando no
# disponemos de información.
#
# Resultado:
# - Modelo global entrenado.
# - Predicciones asociadas al modelo global.
# - Valor K (y′1(∅)) utilizado como referencia en los cálculos posteriores 
# (variaciones de predicción).
#
# ============================================================================

ajustar_modelo_global_clasico <- function(df,
                                          target,
                                          tipo_modelo = c("regresion", "clasificacion"),
                                          formula = NULL,
                                          verbose = 0,
                                          seed = NULL) {
  

  tipo_modelo <- match.arg(tipo_modelo)
  
  if (!is.null(seed)) {
    set.seed(seed)
  }

  df <- as.data.frame(df)
  
  if (!(target %in% names(df))) {
    stop("ERROR: el target '", target, "' no existe en el data.frame.")
  }
  

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
    

    beta0 <- coef(modelo)[["(Intercept)"]]
    K     <- plogis(beta0)

  } else {
    
    modelo <- stats::lm(
      formula = formula,
      data    = df_fit
    )
    
    pred <- stats::predict(modelo, type = "response")
    

    K <- mean(pred, na.rm = TRUE)
  }
  
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



# ============================================================================
# 11. PREDICCIONES PARA CADA COALICIÓN y′1({S}) UTILIZANDO LOS MODELOS CLASICOS
# ============================================================================
#
# Objetivo:
# Aplicar el modelo global clásico previamente ajustado a todas las
# submuestras asociadas a las distintas coaliciones generadas (S).
#
# Metodología:
# - Se utiliza un único modelo global previamente entrenado.
# - El modelo permanece fijo durante todo el proceso.
# - Para cada coalición S se utilizan las submuestras generadas
#   previamente.
# - Se generan las predicciones asociadas a cada coalición.
#
# Modelos considerados:
#
# Regresión:
# - Modelo lineal (LM).
#
# Clasificación:
# - Modelo lineal generalizado binario (GLM Logístico).
#
# Interpretación:
# - Todas las predicciones se obtienen utilizando exactamente
#   el mismo modelo global de cada modelo clásico.
#
# Resultado:
# - Predicciones asociadas a cada coalición y a cada instancia.
# - Conservación del valor K calculado a partir del modelo global
# para el posterior calculo de variación de predicción.
#
# ============================================================================

ajustar_modelo_clasico_a_res <- function(res,
                                         obj_global,
                                         verbose = 0) {
  

  target    <- obj_global$target
  vars_pred <- obj_global$vars_pred
  tipo      <- obj_global$tipo_modelo
  modelo    <- obj_global$modelo
  K_global  <- obj_global$K

  subm_originales  <- res$submuestras
  submuestras_pred <- subm_originales
  

  for (combo in names(subm_originales)) {
    
    if (verbose > 0) {
      cat("Procesando combinación (modelo clásico):", combo, "\n")
    }
    
    lista_inst <- subm_originales[[combo]]
    

    for (inst_name in names(lista_inst)) {
      
      df_sub <- lista_inst[[inst_name]]
      

      pred_sub <- stats::predict(
        modelo,
        newdata = df_sub,
        type    = "response"
      )
      

      df_sub$pred_modelo <- pred_sub
      submuestras_pred[[combo]][[inst_name]] <- df_sub
    }
  }
  

  list(
    submuestras_pred = submuestras_pred,
    target           = target,
    vars_pred        = vars_pred,
    tipo_modelo      = tipo,
    K                = K_global,
    nombre_col_pred  = "pred_modelo"
  )
}


# ============================================================================
# 12. INSPECCIÓN DE SUBMUESTRAS Y PREDICCIONES (XGBOOST)
# ============================================================================
#
# Objetivo:
# Facilitar la revisión manual de las submuestras generadas y de las
# predicciones obtenidas mediante el modelo global XGBoost.
#
# Metodología:
# - Se recorren todas las coaliciones generadas.
# - Se recorren todas las instancias asociadas a cada coalición.
# - Se muestran las variables fijadas en cada submuestra.
# - Se muestran las predicciones almacenadas en cada submuestra..
#
# Utilidad:
# - Verificación del correcto funcionamiento del método.
# - Comprobación de las variables fijadas en cada coalición.
# - Inspección de las predicciones asociadas a cada submuestra.
#
# Resultado:
# - Salida descriptiva en pantalla para revisión y depuración.
#
# ============================================================================

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
# ============================================================================
# 13. INSPECCIÓN DE SUBMUESTRAS Y PREDICCIONES (MODELOS CLÁSICOS LM / GLM)
# ============================================================================
#
# Objetivo:
# Facilitar la revisión manual de las submuestras generadas y de las
# predicciones obtenidas mediante modelos clásicos.
#
# Modelos considerados:
# - Modelo lineal (LM).
# - Modelo lineal generalizado binario (GLM Logístico).
#
# Metodología:
# - Se recorren todas las coaliciones generadas.
# - Se recorren todas las instancias asociadas a cada coalición.
# - Se muestran las variables fijadas en cada submuestra.
# - Se muestran las predicciones almacenadas en cada submuestra.
#
# Utilidad:
# - Verificación del funcionamiento del método.
# - Comprobación de las variables fijadas en cada coalición.
# - Inspección de las predicciones asociadas a cada submuestra.
#
# Resultado:
# - Salida descriptiva en pantalla para revisión y depuración.
#
# ============================================================================

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

# ============================================================================
# 14. CÁLCULO DIRECTO DE H(i,S) Y T(i,S) PARA CADA COALICIÓN (S) MEDIANTE 
# BUCLE (XGBOOST)-
# ============================================================================
#
# Objetivo:
# Calcular directamente los valores H(i,S) y T(i,S) sin almacenar las
# submuestras contrafactuales generadas para cada coalición (ahorro computacional).
#
# Metodología:
# - Se utiliza el modelo global XGBoost previamente ajustado.
# - Para cada coalición S y cada instancia i se genera la submuestra
#   (coalición) únicamente durante el cálculo.
# - Se calculan las predicciones asociadas a dicha submuestra.
# - H(i,S) se define como la media de las predicciones obtenidas para todo S.
# - T(i,S) se define como las variaciones de predicción conocida S y con ausencia 
# de información:
#
#     T(i,S) = H(i,S) - K
#
# Interpretación:
# - Produce los mismos valores H(i,S) y T(i,S) que el procedimiento
#   basado en almacenamiento de submuestras.
# - Reduce las necesidades de almacenamiento al generar cada
#   submuestra únicamente cuando es necesaria.
#
# Predicción con ausencia de información:
# - K = H(∅)
#
# Resultado:
# - Tabla H(i,S).
# - Tabla T(i,S).
# - Verificación opcional de las coaliciones generadas.
#
# ============================================================================

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
  
#
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
  

  H_mat     <- matrix(NA_real_, nrow = n, ncol = n_comb)
  H_nombres <- character(n_comb)

  checks_list <- if (guardar_checks) vector("list", n * n_comb) else NULL
  idx_check   <- 1L
  
  vars_all <- names(df)
  

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
      

      H_mat[i, j] <- mean(pred_sub, na.rm = TRUE)
      

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
  

  colnames(H_mat) <- H_nombres
  
  tabla_H_instancias <- data.frame(
    instancia = seq_len(n),
    H_mat,
    row.names   = NULL,
    check.names = FALSE
  )
  

  col_H_empty <- which(colnames(tabla_H_instancias) == "H()")
  
  K_calculado <- unique(tabla_H_instancias[[col_H_empty]])
  stopifnot(length(K_calculado) == 1)
  
  tabla_H_instancias$K <- K_calculado
  
  tabla_T_instancias <- calcular_T_por_instancia(tabla_H_instancias)
  
  verificacion <- NULL
  if (guardar_checks) {
    verificacion <- do.call(rbind, checks_list[seq_len(idx_check - 1L)])
  }
  
  list(
    obj_global         = obj_global,
    vars_pred          = vars_pred,
    K                  = K_calculado,
    tabla_H_instancias = tabla_H_instancias,
    tabla_T_instancias = tabla_T_instancias,
    verificacion       = verificacion
  )
}



# ============================================================================
# 15. CÁLCULO DIRECTO DE H(i,S) Y T(i,S) PARA CADA COALICIÓN (S) MEDIANTE 
# BUCLE (LM / GLM)
# ============================================================================
#
# Objetivo:
# Calcular directamente los valores H(i,S) y T(i,S) sin almacenar las
# submuestras contrafactuales generadas para cada coalición.
#
# Metodología:
# - Se utiliza un modelo global clásico previamente ajustado.
# - Para cada coalición S y cada instancia i se genera la submuestra
#   contrafactual únicamente durante el cálculo.
# - Se calculan las predicciones asociadas a dicha submuestra.
# - H(i,S) se define como la media de las predicciones obtenidas para todo S.
# - T(i,S) se define como las variaciones de predicción conocida S y con ausencia 
# de información:
#
#     T(i,S) = H(i,S) - K
#
# Modelos considerados:
#
# Regresión:
# - Modelo lineal (LM).
#
# Clasificación:
# - Modelo Logístico.
#
# Interpretación:
# - Produce los mismos valores H(i,S) y T(i,S) que el procedimiento
#   basado en almacenamiento de submuestras.
# - Reduce las necesidades de almacenamiento al generar cada
#   submuestra únicamente cuando es necesaria.
#
# Valor de referencia:
# - K = H(∅) -> Ausencia de información
#
# Resultado:
# - Tabla H(i,S).
# - Tabla T(i,S).
# - Verificación opcional de las coaliciones generadas.
#
# ============================================================================

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
  

  H_mat     <- matrix(NA_real_, nrow = n, ncol = n_comb)
  H_nombres <- character(n_comb)
  

  checks_list <- if (guardar_checks) vector("list", n * n_comb) else NULL
  idx_check   <- 1L
  
  vars_all <- names(df)

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
      

      H_mat[i, j] <- mean(pred_sub, na.rm = TRUE)
      

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
  

  colnames(H_mat) <- H_nombres
  
  tabla_H_instancias <- data.frame(
    instancia = seq_len(n),
    H_mat,
    row.names   = NULL,
    check.names = FALSE
  )
  

  col_H_empty <- which(colnames(tabla_H_instancias) == "H()")
  
  K_calculado <- unique(tabla_H_instancias[[col_H_empty]])
  stopifnot(length(K_calculado) == 1)
  
  tabla_H_instancias$K <- K_calculado

  tabla_T_instancias <- calcular_T_por_instancia(tabla_H_instancias)
  

  verificacion <- NULL
  if (guardar_checks) {
    verificacion <- do.call(rbind, checks_list[seq_len(idx_check - 1L)])
  }
  

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
# 16. EJECUCIÓN DATASET – CON BUCLE (STREAMING XGB Y CLÁSICO)
###############################################################################

###############################################################################
# DATASETS DE ANÁLISIS
###############################################################################
HNANESI_y   <- eliminar_vars(HNANESI_clean, c("y_b"))
HNANESI_log <- eliminar_vars(HNANESI_clean, c("y"))

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

library(writexl)

write_xlsx(
  x = M1_Pred_xgb_y_stream,
  path = file.path(
    ruta_resultados,
    "M1-Pred-xgb-y.xlsx" # Guardar archivo de predicciones 
  )
)

write_xlsx(
  x = M1_Delta_xgb_y_stream,
  path = file.path(
    ruta_resultados,"M1_Delta_xgb_y_stream.xlsx" # Guardar archivo de predicciones 
 )
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
  path = file.path(
    ruta_resultados,"M1-Pred-xgb-yb.xlsx" # Guardar archivo de predicciones 
 )
)

write_xlsx(
  x = M1_Delta_xgb_yb_stream,
  path = file.path(
    ruta_resultados, "M1_Delta_xgb_log_stream.xlsx" # Guardar archivo de predicciones 
 )
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
  x = M1_Pred_lm_y_stream,
  path = file.path(
    ruta_resultados, "M1-Pred-lm-y.xlsx" # Guardar archivo de predicciones 
  )
)

write_xlsx(
  x = M1_Delta_lm_y_stream,
  path = file.path(
    ruta_resultados,"M1_Delta_lm_y_stream.xlsx" # Guardar archivo de predicciones 
)
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

write_xlsx(
  x = M1_Pred_glm_yb_stream,
  path = file.path(
    ruta_resultados, "M1-Pred-glm-yb.xlsx" #Carpeta guardar los datos y nombre
  )
)

write_xlsx(
  x = M1_Delta_glm_yb_stream,
  path = file.path(
    ruta_resultados, "M1_Delta_glm_yb_stream.xlsx" #Carpeta guardar los datos y nombre
)
)


