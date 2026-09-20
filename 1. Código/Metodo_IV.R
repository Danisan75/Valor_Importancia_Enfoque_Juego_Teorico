
#################################################################################
################       METODO 4.                        #########################
#################################################################################


library(xgboost)
library(crayon)

# ============================================================================
# 1. EXTRACCIÓN Y VALIDACIÓN DE LAS MATRICES μ(v|S)
# ============================================================================
#
# Objetivo:
# Recuperar las matrices μ(v|S) previamente calculadas para su utilización
# posterior en la construcción de los operadores H(i,S) y T(i,S) predicciones
# en función de la información conocida y variaciones de predicción.
#
# Metodología:
# - Se parte de los resultados obtenidos del cálculo de μ(v|S).
# - Se extrae la matriz μ asociada a cada modelo.
# - Cada matriz contiene la información de la capacidad predictiva independientemente
#   de la aleatoriedad estimada de variables del dataset.
#
#
#
# Casos considerados:
#
# Regresión:
# - Variable objetivo continua y.
#
# Clasificación:
# - Variable objetivo binaria y_b.
#
# Validación:
# - Se verifica la existencia de las matrices μ.
# - Se comprueban sus dimensiones.
# - No se recalcula ningún valor μ.
# - No se modifican resultados obtenidos previamente.
#
# Resultado:
# - Matriz μ(v|S) para regresión.
# - Matriz μ(v|S) para clasificación.
# - Confirmación de la estructura dimensional de ambas matrices para el 
# correcto funcionamiento del script.
#
# ============================================================================



matriz_mu_y   <- res_Mu_y$matriz_mu    # para regresión (target = y)
matriz_mu_log <- res_Mu_log$matriz_mu  # para clasificación (target = y_b)

cat("\n===== MATRIZ μ_j(S) PARA REGRESIÓN (target = y) =====\n")
print(dim(matriz_mu_y))

cat("\n===== MATRIZ μ_j(S) PARA CLASIFICACIÓN (target = y_booleana) =====\n")
print(dim(matriz_mu_log))

# ============================================================================
# 2. PREPARACIÓN DE LOS DATOS PARA EL MODELO GLOBAL XGBOOST
# ============================================================================
#
# Objetivo:
# Construir la matriz de entrada utilizada por el modelo global XGBoost.
#
# Metodología:
# - Se verifica la existencia de la variable objetivo.
# - Se seleccionan las variables predictoras.
# - Los predictores se transforman a una matriz numérica compatible
#   con XGBoost.
#
# Resultado:
# - Matriz X de predictores.
# - Variable objetivo y.
# - Conjunto de variables utilizadas por el modelo global.
#
# ============================================================================

preparar_xy_global_XGB <- function(df, target, vars_pred) {
  df <- as.data.frame(df)
  
  if (!(target %in% names(df))) {
    stop("ERROR (preparar_xy_global_XGB): target '", target, "' no existe en df.")
  }
  if (!all(vars_pred %in% names(df))) {
    faltan <- setdiff(vars_pred, names(df))
    stop("ERROR (preparar_xy_global_XGB): faltan predictores en df: ",
         paste(faltan, collapse = ", "))
  }
  if (length(vars_pred) == 0) {
    stop("ERROR (preparar_xy_global_XGB): vars_pred está vacío.")
  }
  
  y    <- df[[target]]
  X_df <- df[, vars_pred, drop = FALSE]
  
  X_mat <- model.matrix(~ . - 1, data = X_df)
  if (is.null(X_mat) || ncol(X_mat) == 0) {
    stop("ERROR (preparar_xy_global_XGB): model.matrix devolvió matriz vacía.")
  }
  
  list(
    X_mat     = X_mat,
    y         = y,
    vars_pred = vars_pred
  )
}



# ============================================================================
# 3. ENTRENAMIENTO Y PREDICCIÓN DEL MODELO XGBOOST PARA y′4(∅, μ)
# ============================================================================
#
# Objetivo:
# Entrenar un único modelo global XGBoost utilizando toda la base de datos.
#
# Metodología:
# - Se utiliza toda la información disponible.
# - El modelo se ajusta una única vez.
# - El modelo permanece fijo durante todo el procedimiento.
# - No se entrenan modelos específicos para las distintas coaliciones.
#
# Interpretación:
# - El modelo obtenido representa la función predictiva general.
# - Todas las evaluaciones posteriores utilizan este mismo modelo.
# - Las diferencias entre coaliciones se deben únicamente a la
#   información proporcionada al modelo.
#
# Predicción media global:
# - Se calcula la media de las predicciones del modelo sobre toda la
#   base de datos, ausencia de información S={∅}(K_aux).
#
# Resultado:
# - Modelo global XGBoost entrenado.
# - Predicciones para cada instancia cuando S={∅}.
# - Predicción media K_aux.
#
# ============================================================================

ajustar_xgb_global_Mu <- function(df,
                                  target,
                                  tipo      = c("regresion", "clasificacion"),
                                  vars_pred,
                                  nrounds   = 200,
                                  params    = list(),
                                  verbose   = 0,
                                  seed      = NULL) {
  tipo <- match.arg(tipo)
  if (!is.null(seed)) set.seed(seed)
  
  prep      <- preparar_xy_global_XGB(df, target, vars_pred)
  X_mat     <- prep$X_mat
  y         <- prep$y
  vars_pred <- prep$vars_pred
  
  cat(bold$cyan("\n[INFO] Entrenando XGBoost para target = "), target,
      " con predictores: ", paste(vars_pred, collapse = ", "), "\n", sep = "")
  
  if (tipo == "clasificacion") {
    if (is.logical(y)) {
      y_num <- as.numeric(y)  # FALSE=0, TRUE=1
    } else if (is.factor(y)) {
      if (nlevels(y) != 2) {
        stop("En clasificación, el target factor debe tener 2 niveles.")
      }
      y_num <- as.numeric(y) - 1
    } else {
      y_num <- as.numeric(y)
    }
  } else {
    y_num <- as.numeric(y)
  }
  
  dtrain <- xgboost::xgb.DMatrix(data = X_mat, label = y_num)
  
  if (length(params) == 0) {
    if (tipo == "clasificacion") {
      params <- list(
        objective        = "binary:logistic",
        eval_metric      = "logloss",
        max_depth        = 3,
        eta              = 0.1,
        subsample        = 0.8,
        colsample_bytree = 0.8
      )
    } else {
      params <- list(
        objective        = "reg:squarederror",
        eval_metric      = "rmse",
        max_depth        = 3,
        eta              = 0.1,
        subsample        = 0.8,
        colsample_bytree = 0.8
      )
    }
  }
  
  modelo <- xgboost::xgb.train(
    params  = params,
    data    = dtrain,
    nrounds = nrounds,
    verbose = verbose
  )
  
  pred <- predict(modelo, newdata = dtrain)
  

  K_aux <- mean(pred, na.rm = TRUE)
  
  feature_names <- colnames(X_mat)
  
  cat(
    bold$magenta(
      "[INFO] Valor medio de predicción del modelo global = "
    ),
    K_aux, "\n\n", sep = ""
  )
  
  list(
    modelo        = modelo,
    pred          = pred,
    K_aux         = K_aux,   
    tipo_modelo   = tipo,
    target        = target,
    vars_pred     = vars_pred,
    feature_names = feature_names
  )
}


# ============================================================================
# 4. PREPARACIÓN DE LAS COALICIONES (S) PARA XGBOOST - AUXILIAR
# ============================================================================
#
# Objetivo:
# Preparar cada coalición para que pueda ser evaluada por el modelo
# global XGBoost.
#
# Metodología:
# - Se alinean las coaliciones con la estructura utilizada durante el
#   entrenamiento.
#
# Resultado:
# - Matriz compatible con el modelo global XGBoost.
#
# ============================================================================


preparar_X_sub_para_global_Mu <- function(df_sub,
                                          target,
                                          vars_pred,
                                          feature_names) {
  df_sub <- as.data.frame(df_sub)
  
  if (!all(vars_pred %in% names(df_sub))) {
    faltan <- setdiff(vars_pred, names(df_sub))
    stop("ERROR: faltan predictores en df_sub: ", paste(faltan, collapse = ", "))
  }
  
  X_df  <- df_sub[, vars_pred, drop = FALSE]
  X_raw <- model.matrix(~ . - 1, data = X_df)
  
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
# 5. RECUPERACIÓN DE LOS VALORES DIFUSOS μ(v|S)
# ============================================================================
#
# Objetivo:
# Recuperar el valor μ(v|S) a partir de la matriz μ previamente calculada.
#
# Metodología:
# - Los valores μ(v|S) se recuperan directamente de las matrices μ
#   previamente calculadas durante la fase de construcción de μ.
# - La función no recalcula ni modifica ningún valor de la medida difusa.
# - Cuando v pertenece a S se devuelve directamente μ(v|S)=1.
# - Esta propiedad fue previamente demostrada y validada durante la
#   construcción de las matrices μ y coincide con el valor almacenado
#   en dichas matrices.
# - Este tratamiento se mantiene únicamente como medida de robustez
# para evitar posibles problemas de precisión numérica o tolerancias
# de cálculo, sin modificar los valores definidos por las matrices μ.
#
# Comprobación opcional:
# - Puede verificarse que devolver directamente μ(v|S)=1 cuando
#   v pertenece a S produce exactamente el mismo resultado que
#   recuperar dicho valor desde la matriz μ (matriz_mu_y/matriz_mu_log).
# - Esta comprobación confirma que la instrucción
#
#       if (v %in% S_known) return(1)
#
#   no modifica los resultados del método y se mantiene únicamente
#   como medida de robustez y consistencia del código.


#comparar_get_mu_vs_matriz <- function(matriz_mu) {
  
#  res <- data.frame()
  
#  for (S_key in rownames(matriz_mu)) {
    
#    if (S_key == "empty") next
    
#    S_vars <- strsplit(S_key, "\\+")[[1]]
    
#    for (v in S_vars) {
      
#      mu_matriz <- as.numeric(matriz_mu[S_key, v])
      
#      mu_get <- get_mu_matriz(
#        matriz_mu = matriz_mu,
#        S_known   = S_vars,
#        v         = v
#      )
      
#      res <- rbind(
#        res,
#        data.frame(
#        S          = S_key,
#          variable   = v,
#          mu_matriz  = mu_matriz,
#          mu_get     = mu_get,
#          iguales    = identical(mu_matriz, mu_get)
#        )
#      )
#    }
#  }
  
#  res
#}

#test_y <- comparar_get_mu_vs_matriz(matriz_mu_y)

#test_log <- comparar_get_mu_vs_matriz(matriz_mu_log)

#all(test_y$iguales)

#all(test_log$iguales)

#
# Interpretación:
# - La fuente de información utilizada son las matrices μ
#   (matriz_mu_y y matriz_mu_log).
# - La función actúa únicamente como mecanismo de consulta y validación
#   de dichos valores.
#
# Validación:
# - Se comprueba la existencia de la variable y de la coalición.
# - Se verifica que el valor recuperado sea válido.
#
# Resultado:
# - Valor μ(v|S).
#
# ============================================================================

get_mu_matriz <- function(matriz_mu, S_known, v) {
  

  vars_X_all <- colnames(matriz_mu)
  
 
  if (v %in% S_known) return(1)
  

  if (!(v %in% colnames(matriz_mu))) {
    stop("get_mu_matriz: la variable '", v, "' no está en matriz_mu.")
  }
  

  if (length(S_known) == 0) {
    S_key <- "empty"
  } else {
    S_key <- paste(intersect(vars_X_all, S_known), collapse = "+")
  }
  

  if (!(S_key %in% rownames(matriz_mu))) {
    stop("get_mu_matriz: no existe la fila S = '", S_key, "' en matriz_mu.")
  }
  
  mu_val <- as.numeric(matriz_mu[S_key, v])
  
  if (is.na(mu_val)) {
    stop("get_mu_matriz: μ(", v, " | ", S_key, ") es NA.")
  }
  
  if (mu_val < 0 || mu_val > 1) {
    stop("get_mu_matriz: μ fuera de [0,1] para (", v, " | ", S_key, ").")
  }
  
  mu_val
}

# ============================================================================
# 6. CÁLCULO DE y′4({S}, μ) AL QUE DENOMINAREMOS H(i,S) MEDIANTE UN MODELO GLOBAL XGBOOST
# ============================================================================
# Objetivo:
# Calcular la predicción H(i,S) para una instancia i y una coalición S utilizando
# el modelo global XGBoost y la matriz μ.
#
# Metodología:
# - Las variables pertenecientes a S se fijan los valores observados
#   en la instancia i.
# - Las variables fuera de S conservan los valores originales de la
#   base de datos.
# - La matriz μ(v|S) determina la probabilidad de acierto de las
#   variables desconocidas (N\S).
# - Se consideran todos las escenarios posibles de acierto y fallo
#   para (N\S).
# - Para cada coalición se calcula una predicción media mediante el
#   modelo global XGB.
# - H(i,S) se obtiene como la media de esas predicciones
#   utilizando las probabilidades derivadas de μ(v|S).
#
# Interpretación:
# - H(i,S) representa la predicción esperada para la instancia i cuando
#   únicamente se conoce la información contenida en S.
#
# Resultado:
# - Valor de la predicción para cada coalición H(i,S).
#
# ============================================================================



construir_submuestra_M1 <- function(df, S, U_aciertos, i_instancia) {
  df_sub <- df
  
  # Fijar todas las variables en S
  for (v in S) {
    df_sub[[v]] <- df[i_instancia, v, drop = TRUE]
  }
  
  # Fijar SOLO las variables de U que aciertan
  if (length(U_aciertos) > 0) {
    for (v in U_aciertos) {
      df_sub[[v]] <- df[i_instancia, v, drop = TRUE]
    }
  }
  
  df_sub
}


H_instancia_S_matriz_full <- function(df,
                                      obj_global,
                                      matriz_mu,
                                      S,
                                      i_instancia,
                                      debug = FALSE) {
  
  S <- sort(S)
  vars_pred     <- obj_global$vars_pred
  target        <- obj_global$target
  feature_names <- obj_global$feature_names
  modelo        <- obj_global$modelo
  
  n <- nrow(df)
  if (i_instancia < 1 || i_instancia > n) {
    stop("i_instancia fuera de rango.")
  }
  
  # Variables no conocidas
  U <- setdiff(vars_pred, S)
  k <- length(U)
  
  # μ_v(S) para v en U
  mu_unknown <- numeric(k)
  names(mu_unknown) <- U
  for (j in seq_along(U)) {
    mu_unknown[j] <- get_mu_matriz(matriz_mu, S_known = S, v = U[j])
  }
  

  if (k == 0) {
    patterns <- matrix(nrow = 1, ncol = 0)
  } else {
    patterns <- as.matrix(do.call(expand.grid, rep(list(0:1), k)))
    colnames(patterns) <- U
  }
  
  n_scen <- nrow(patterns)
  
  if (debug) {
    cat("\n[DEBUG FULL] H(i,S) para instancia i = ", i_instancia,
        " con S = {", paste(S, collapse = ","), "}\n", sep = "")
    cat("  Variables no conocidas U = {", paste(U, collapse = ","), "}\n", sep = "")
    cat("  μ_v(S):\n")
    print(mu_unknown)
  }
  
  weights <- numeric(n_scen)
  contrib <- numeric(n_scen)
  
  for (s_idx in seq_len(n_scen)) {
    
    flags <- if (k > 0) as.integer(patterns[s_idx, ]) else integer(0)
    names(flags) <- U
    
    U_aciertos <- if (k > 0) U[flags == 1L] else character(0)
    
    df_s <- construir_submuestra_M1(
      df          = df,
      S           = S,
      U_aciertos  = U_aciertos,
      i_instancia = i_instancia
    )
    
    X_s <- preparar_X_sub_para_global_Mu(
      df_sub        = df_s,
      target        = target,
      vars_pred     = vars_pred,
      feature_names = feature_names
    )
    
    dmat_s <- xgboost::xgb.DMatrix(data = X_s)
    pred_s <- predict(modelo, newdata = dmat_s)
    m_s    <- mean(pred_s)
    
    w_s <- if (k > 0) {
      prod(ifelse(flags == 1L, mu_unknown, 1 - mu_unknown))
    } else {
      1
    }
    
    weights[s_idx] <- w_s
    contrib[s_idx] <- w_s * m_s
    
    if (debug) {
      cat("\n  Escenario #", s_idx, "\n", sep = "")
      cat("    Flags:\n")
      print(flags)
      cat("    Peso w_s = ", w_s, "\n", sep = "")
      cat("    m_s = ", m_s, "\n", sep = "")
    }
  }
  
  if (abs(sum(weights) - 1) > 1e-8) {
    stop("Σ w_s != 1")
  }
  
  H_val <- sum(contrib)
  
  if (debug) {
    cat("\n[DEBUG FULL] Resumen H:\n")
    cat("  Σ w_s = ", sum(weights), "\n", sep = "")
    
    if (length(S) == 0) {
      cat("  H() = ", H_val, "\n", sep = "")
    } else {
      cat("  H(", paste(S, collapse = "_"), ") = ", H_val, "\n", sep = "")
    }
  }
  
  H_val
}



H_instancia_S_matriz <- function(df,
                                 obj_global,
                                 matriz_mu,
                                 S,
                                 i_instancia,
                                 debug = FALSE) {
  S <- sort(S)
  vars_pred     <- obj_global$vars_pred
  target        <- obj_global$target
  feature_names <- obj_global$feature_names
  modelo        <- obj_global$modelo
  
  n <- nrow(df)
  if (i_instancia < 1 || i_instancia > n) {
    stop("i_instancia fuera de rango.")
  }
  
  U <- setdiff(vars_pred, S)
  k <- length(U)
  
  inst_vals <- df[i_instancia, vars_pred, drop = FALSE]
  
  if (k == 0L) {  # S = todas las variables (MISMA lógica que M1)
    
    if (debug) {
      cat(bold$green(
        "\n[DEBUG] Caso S = X: submuestra completa con instancia fijada\n"
      ))
    }
    
    # 1) Construir las coaliciones
    df_s <- df
    for (v in vars_pred) {
      df_s[[v]] <- df[i_instancia, v, drop = TRUE]
    }
    
    # 2) Preparar matriz X alineada
    X_s <- preparar_X_sub_para_global_Mu(
      df_sub        = df_s,
      target        = target,
      vars_pred     = vars_pred,
      feature_names = feature_names
    )
    
    # 3) Predecir sobre TODA la base
    dmat_s <- xgboost::xgb.DMatrix(data = X_s)
    pred_s <- predict(modelo, newdata = dmat_s)
    
   
    return(mean(pred_s))
  }
  
  # μ_v(S)
  mu_unknown <- numeric(k)
  names(mu_unknown) <- U
  for (j in seq_along(U)) {
    vj <- U[j]
    mu_unknown[j] <- get_mu_matriz(matriz_mu, S_known = S, v = vj)
  }
  
  # Escenarios de acierto/fallo en U
  patterns <- as.matrix(
    do.call(expand.grid, rep(list(0:1), k))
  )
  colnames(patterns) <- U
  n_scen <- nrow(patterns)
  
  if (debug) {
    cat(bold$blue("\n[DEBUG] H(i,S) para instancia i = "), i_instancia,
        " con S = {", paste(S, collapse = ","), "}\n", sep = "")
    cat("  Variables no conocidas U = {", paste(U, collapse = ","), "}\n", sep = "")
    cat("  μ_v(S):\n")
    print(mu_unknown)
  }
  
  weights <- numeric(n_scen)
  contrib <- numeric(n_scen)
  
  for (s_idx in seq_len(n_scen)) {
    flags <- as.integer(patterns[s_idx, ])
    names(flags) <- U
    
    U_aciertos <- U[flags == 1L]
    
    df_s <- construir_submuestra_M1(
      df          = df,
      S           = S,
      U_aciertos  = U_aciertos,
      i_instancia = i_instancia
    )
    
    X_s   <- preparar_X_sub_para_global_Mu(df_sub = df_s,
                                           target = target,
                                           vars_pred = vars_pred,
                                           feature_names = feature_names)
    dmat_s <- xgboost::xgb.DMatrix(data = X_s)
    pred_s <- predict(modelo, newdata = dmat_s)
    m_s    <- mean(pred_s)
    
    w_s <- prod(ifelse(flags == 1L, mu_unknown, 1 - mu_unknown))
    
    weights[s_idx] <- w_s
    contrib[s_idx] <- w_s * m_s
    
    if (debug) {
      cat(bold$yellow("\n  Escenario #"), s_idx, "\n", sep = "")
      cat("    Flags (0=fallo, 1=acierto) en U:\n")
      print(flags)
      cat("    Peso w_s = ", w_s, "\n", sep = "")
      cat("    m_s (media pred) = ", m_s, "\n", sep = "")
      cat("    Término w_s * m_s = ", contrib[s_idx], "\n", sep = "")
    }
  }
  
  sum_w <- sum(weights)
  H_val <- sum(contrib) # / sum_w Ponderada
  
  if (abs(sum_w - 1) > 1e-8) {
    stop(
      "ERROR CRÍTICO (XGB): Σ w_s = ", sum_w,
      " (debería ser 1). Revisa μ(v|S)."
    )
  }
  
  if (debug) {
    cat(bold$magenta("\n[DEBUG] Resumen H(i,S):\n"))
    cat("  Σ w_s = ", sum_w, "\n", sep = "")
    cat("  Términos w_s * m_s:\n")
    print(contrib)
    cat("  H(i,S) = mean(w_s * m_s) = ", H_val, "\n\n", sep = "")
  }
  
  if (abs(sum_w - 1) > 1e-6) {
    warning("La suma de pesos Σ w_s = ", sum_w,
            " se desvía de 1. Revisa tus μ_j(S) en matriz_mu.")
  }
  
  H_val
}


# ============================================================================
# 7. GENERACIÓN DE LAS COALICIONES (S)
# ============================================================================
# Objetivo:
# Generar todas las coaliciones S de variables predictoras consideradas
# en el método.
#
# Metodología:
# - Se generan todas las combinaciones posibles de las variables
#   predictoras.
# - Cada coalición define implícitamente dos conjuntos:
#
#     S      : variables conocidas.
#     N \ S  : variables no conocidas.
#
# Interpretación:
# - Cada coalición S representa un conjunto concreto de información
#   de S y N\S que se utilizarán posteriormente en el cálculo
#   de H(i,S) mediante los valores μ(v|S).
#
# Resultado:
# - Lista de coaliciones S.
#
# ============================================================================


generar_S_list <- function(vars_pred, max_size = NULL) {
  vars_pred <- as.character(vars_pred)
  p <- length(vars_pred)
  if (p == 0) stop("generar_S_list: vars_pred vacío.")
  
  if (is.null(max_size)) {
    max_size <- p
  } else {
    max_size <- min(max_size, p)
  }
  
  S_list <- list()
  idx    <- 1
  
  for (k in 1:max_size) {
    combs_k <- combn(vars_pred, k, simplify = FALSE)
    for (S in combs_k) {
      S_list[[idx]] <- S
      idx <- idx + 1
    }
  }
  
  S_list
}




# ============================================================================
# 9. CÁLCULO DE PREDICCION DEL ∅ (y′4(∅, μ)=H(∅)=K), PREDICCIONES DE S EN CADA 
# INSTANCIA i (y′4(S, μ)=H(i,S)) Y VARIACION DE PREDICCION Δ4=T(i,S)
# ============================================================================
#
# Objetivo:
# Calcular K, H(i,S) y T(i,S) para todas las instancias y coaliciones.
#
# Metodología:
# - K (predicción sin información) se obtiene a partir de H(i,∅).
# - Se calcula la predicción conocido S, H(i,S) para cada instancia y coalición.
# - Se calcula la variación de predicción T(i,S) como:
#
#       T(i,S) = H(i,S) - K
#
# Resultado:
# - Tabla H(i,S).
# - Tabla T(i,S).
# - Valor K.
#
# ============================================================================

calcular_K_desde_H_empty_matriz <- function(df,
                                            obj_global,
                                            matriz_mu) {
  n <- nrow(df)
  
  H_empty_vals <- sapply(seq_len(n), function(i) {
    H_instancia_S_matriz(
      df          = df,
      obj_global  = obj_global,
      matriz_mu   = matriz_mu,
      S           = character(0),
      i_instancia = i,
      debug       = FALSE
    )
  })
  
  K_vals <- unique(round(H_empty_vals, 12))
  stopifnot(length(K_vals) == 1)
  
  K_vals
}

calcular_H_T_para_Sets_matriz <- function(df,
                                          obj_global,
                                          matriz_mu,
                                          S_list,
                                          debug      = FALSE,
                                          inst_debug = NULL,
                                          S_debug    = NULL) {
  n <- nrow(df)
  K <- calcular_K_desde_H_empty_matriz(
    df         = df,
    obj_global = obj_global,
    matriz_mu  = matriz_mu
  )
  
  H_tabla <- data.frame(instancia = 1:n)
  T_tabla <- data.frame(instancia = 1:n)
  
  for (S in S_list) {
    S <- sort(S)
    nombre_S <- paste(S, collapse = "_")
    col_H    <- paste0("H_S_", nombre_S)
    col_T    <- paste0("T_S_", nombre_S)
    
    if (debug && !is.null(inst_debug)) {
      if (is.null(S_debug) || identical(sort(S_debug), S)) {
        cat(bold$blue("\n=============================================\n"))
        cat(bold$blue("DEBUG H(i,S) para S = {", paste(S, collapse = ","), "}\n", sep = ""))
        invisible(H_instancia_S_matriz_full(
          df          = df,
          obj_global  = obj_global,
          matriz_mu   = matriz_mu,
          S           = S,
          i_instancia = inst_debug,
          debug       = TRUE
        ))
        cat(bold$blue("=============================================\n\n"))
      }
    }
    
    H_vec <- numeric(n)
    for (i in 1:n) {
      H_vec[i] <- H_instancia_S_matriz(
        df          = df,
        obj_global  = obj_global,
        matriz_mu   = matriz_mu,
        S           = S,
        i_instancia = i,
        debug       = FALSE
      )
    }
    
    T_vec <- H_vec - K
    
    H_tabla[[col_H]] <- H_vec
    T_tabla[[col_T]] <- T_vec
  }
  
  # Conjunto vacío: H = K, T = 0
  H_tabla$H_S_empty <- K
  T_tabla$T_S_empty <- 0
  
  # Columna K explícita
  H_tabla$K <- K
  
  list(
    H_tabla = H_tabla,
    T_tabla = T_tabla,
    K       = K
  )
}

# ============================================================================
# 10. FUNCIONES DE DEPURACIÓN Y VALIDACIÓN
# ============================================================================
#
# Objetivo:
# Facilitar la inspección detallada del cálculo de las predicciones H(i,S) para 
# comprobar el correcto funcionamiento del método.
#
# Metodología:
# - Permiten ejecutar el cálculo de H(i,S) para instancias y coaliciones
#   específicas en modo de depuración.
# - Se muestran los valores μ(v|S) utilizados durante el cálculo.
# - Se muestran los escenarios generados y los pesos asociados a cada uno.
# - Se muestran las predicciones y contribuciones que intervienen en la
#   obtención del valor final H(i,S).
#
# Interpretación:
# - Estas funciones tienen una finalidad exclusivamente diagnóstica.
# - No modifican resultados ni intervienen en los cálculos principales
#   del método.
#
# Resultado:
# - Información detallada para la validación y revisión del cálculo
#   de H(i,S).
#
# ============================================================================


debug_instancias_todos_S_matriz <- function(df,
                                            obj_global,
                                            matriz_mu,
                                            S_list,
                                            i_instancia) {
  cat(bold$red("\n=========== DEBUG COMPLETO PARA INSTANCIA i = "),
      i_instancia, " ===========\n", sep = "")
  cat("Target: ", obj_global$target, "  |  Tipo: ", obj_global$tipo_modelo, "\n")
  cat("Predictores: ", paste(obj_global$vars_pred, collapse = ", "), "\n\n")
  
  # ==========================================================
  # ✅ DEBUG EXPLÍCITO PARA S = {}  (CONJUNTO VACÍO)
  # ==========================================================
  # S = {}  (FULL, METODOLÓGICO)
  
  invisible(
    H_instancia_S_matriz_full(
      df          = df,
      obj_global  = obj_global,
      matriz_mu   = matriz_mu,
      S           = character(0),
      i_instancia = i_instancia,
      debug       = TRUE
    )
  )
  
  # ==========================================================
  # ✅ DEBUG PARA EL RESTO DE S (NO VACÍOS)
  # ==========================================================
  for (S in S_list) {
    S <- sort(S)
    nombre_S <- paste(S, collapse = ",")
    cat(bold$green("\n----- S = {", nombre_S, "} -----\n", sep = ""))
    invisible(
      H_instancia_S_matriz_full(
        df          = df,
        obj_global  = obj_global,
        matriz_mu   = matriz_mu,
        S           = S,
        i_instancia = i_instancia,
        debug       = TRUE
      )
    )
  }
}


debug_instancia_S_matriz <- function(df,
                                     obj_global,
                                     matriz_mu,
                                     S,
                                     i_instancia) {
  S <- sort(S)
  nombre_S <- paste(S, collapse = ",")
  
  cat("\n=========== DEBUG PARA INSTANCIA i = ",
      i_instancia, ", S = {", nombre_S, "} ===========\n", sep = "")
  cat("Target: ", obj_global$target, "  |  Tipo: ", obj_global$tipo_modelo, "\n")
  cat("Predictores: ", paste(obj_global$vars_pred, collapse = ", "), "\n\n")
  
  invisible(H_instancia_S_matriz_full(
    df          = df,
    obj_global  = obj_global,
    matriz_mu   = matriz_mu,
    S           = S,
    i_instancia = i_instancia,
    debug       = TRUE
  ))
}


# ============================================================================
# 11. EJECUCIÓN DEL MÉTODO
# ============================================================================
#
# Objetivo:
# Ejecutar el procedimiento completo para obtener K, H(i,S) y T(i,S)
# a partir de una matriz μ previamente calculada.
#
# Metodología:
# - Se utiliza la matriz μ obtenida en fases previas.
# - Se entrena un único modelo global.
# - Se generan las coaliciones S.
# - Se calculan los valores H(i,S).
# - Se calculan los valores T(i,S).
#
# Interpretación:
# - Este bloque integra todas las etapas del método 4.
#
# Resultado:
# - Tabla predicciones H(i,S).
# - Tabla variaciones de predicción T(i,S).
# - Valor predicción del vacío ->K.
# - Coaliciones S.
# - Fit modelo global utilizado.
#
# ============================================================================

calcular_H_T_con_Mu <- function(df,
                                matriz_mu,
                                target,
                                tipo_modelo = c("regresion", "clasificacion"),
                                vars_XGB   = NULL,
                                max_size_S = NULL,
                                nrounds    = 200,
                                inst_debug = NULL,
                                S_debug    = NULL,
                                debug      = FALSE) {
  tipo_modelo <- match.arg(tipo_modelo)
  
  vars_original <- colnames(matriz_mu)  # variables MU
  
  if (is.null(vars_XGB)) {
    vars_XGB <- vars_original
  } else {
    if (!all(vars_XGB %in% vars_original)) {
      faltan <- setdiff(vars_XGB, vars_original)
      stop("calcular_H_T_con_Mu: estas vars_XGB no están en matriz_mu: ",
           paste(faltan, collapse = ", "))
    }
  }
  
  if (target %in% vars_XGB) {
    vars_XGB <- setdiff(vars_XGB, target)
  }
  
  obj_global <- ajustar_xgb_global_Mu(
    df        = df,
    target    = target,
    tipo      = tipo_modelo,
    vars_pred = vars_XGB,
    nrounds   = nrounds,
    verbose   = 0,
    seed      = 123
  )
  
  S_list <- generar_S_list(vars_XGB, max_size = max_size_S)
  
  if (!is.null(inst_debug) && debug) {
    debug_instancias_todos_S_matriz(
      df          = df,
      obj_global  = obj_global,
      matriz_mu   = matriz_mu,
      S_list      = if (!is.null(S_debug)) list(S_debug) else S_list,
      i_instancia = inst_debug
    )
  }
  
  res_HT <- calcular_H_T_para_Sets_matriz(
    df          = df,
    obj_global  = obj_global,
    matriz_mu   = matriz_mu,
    S_list      = S_list,
    debug       = FALSE,
    inst_debug  = NULL,
    S_debug     = NULL
  )
  
  list(
    H_tabla    = res_HT$H_tabla,
    T_tabla    = res_HT$T_tabla,
    K          = res_HT$K,
    vars_XGB   = vars_XGB,
    S_list     = S_list,
    obj_global = obj_global
  )
}

# ============================================================================
# 12. RENOMBRADO Y ORGANIZACIÓN DE LAS TABLAS H(i,S) Y T(i,S)
# ============================================================================
#
# Objetivo:
# Adaptar la nomenclatura y organización de las tablas H(i,S) y T(i,S)
# para facilitar su interpretación.
#
# Metodología:
# - La coalición vacía, N\S se representa mediante H() y T().
# - Las restantes coaliciones se representan mediante H(S) y T(S).
# - Las columnas se reorganizan para mantener una estructura homogénea.
#
# Interpretación:
# - Este paso no modifica ningún resultado del método.
# - Únicamente adapta la presentación de las tablas.
#
# Resultado:
# - Tabla H(i,S) renombrada y ordenada.
# - Tabla T(i,S) renombrada y ordenada.
#
# ============================================================================


renombrar_HT <- function(H_tabla, T_tabla) {
  
  # =========================
  # 1) RENOMBRAR H (SOLO H_S_*)
  # =========================
  nombres_H <- names(H_tabla)
  
  nuevos_H <- nombres_H
  nuevos_H[nombres_H == "H_S_empty"] <- "H()"
  
  idx_H <- grepl("^H_S_", nombres_H) & nombres_H != "H_S_empty"
  nuevos_H[idx_H] <- paste0(
    "H(",
    sub("^H_S_", "", nombres_H[idx_H]),
    ")"
  )
  
  names(H_tabla) <- nuevos_H
  
  # =========================
  # 2) REORDENAR H
  # =========================
  cols_H <- names(H_tabla)
  
  cols_orden_H <- c(
    "instancia",
    "H()",
    setdiff(cols_H, c("instancia", "H()", "K")),
    if ("K" %in% cols_H) "K"
  )
  
  cols_orden_H <- intersect(cols_orden_H, cols_H)
  H_tabla <- H_tabla[, cols_orden_H, drop = FALSE]
  
  # =========================
  # 3) RENOMBRAR T (SOLO T_S_*)
  # =========================
  nombres_T <- names(T_tabla)
  
  nuevos_T <- nombres_T
  nuevos_T[nombres_T == "T_S_empty"] <- "T()"
  
  idx_T <- grepl("^T_S_", nombres_T) & nombres_T != "T_S_empty"
  nuevos_T[idx_T] <- paste0(
    "T(",
    sub("^T_S_", "", nombres_T[idx_T]),
    ")"
  )
  
  names(T_tabla) <- nuevos_T
  
  # =========================
  # 4) REORDENAR T
  # =========================
  cols_T <- names(T_tabla)
  
  cols_orden_T <- c(
    "instancia",
    "T()",
    setdiff(cols_T, c("instancia", "T()"))
  )
  
  cols_orden_T <- intersect(cols_orden_T, cols_T)
  T_tabla <- T_tabla[, cols_orden_T, drop = FALSE]
  
  # =========================
  # SALIDA
  # =========================
  list(
    H_tabla = H_tabla,
    T_tabla = T_tabla
  )
}

# ============================================================================
# 13. EJECUCIÓN DEL MÉTODO Y EXPORTACIÓN DE RESULTADOS
# ============================================================================
#
# Objetivo:
# Aplicación del modelo XGB al método 4.
#
# Metodología:
# - Se define la configuración de cada caso de estudio.
# - Se ejecuta el método utilizando la matriz μ correspondiente.
# - Se obtienen las tablas H(i,S), T(i,S) y el valor K.
# - Se generan salidas de validación y depuración del procedimiento.
# - Los resultados se almacenan de forma independiente para cada caso.
#
# Casos considerados XGB:
# - Regresión.
# - Clasificación.
#
# Resultado:
# - Tabla H(i,S).
# - Tabla T(i,S).
# - Valor K.
# - Archivos de validación y depuración.
# - Archivos de resultados exportados.
#
# ============================================================================



# Paso 1: Definir la configuración de cada caso
config_list <- list(
  list(
    nombre    = "REGRESION_y_XGB",
    df        = HNANESI_y,
    matriz_mu = matriz_mu_y,
    target    = "y",
    tipo      = "regresion"
  ),
  list(
    nombre    = "CLASIFICACION_y_b_XGB",
    df        = HNANESI_log,
    matriz_mu = matriz_mu_log,
    target    = "y_b",
    tipo      = "clasificacion"
  )
)

# Paso 2: Lista donde se guardan los resultados
resultados_xgb <- list()

# Paso 3: Ejecutar el Método 4 para cada configuración
for (cfg in config_list) {
  
  cat("\n========================\n",
      "Ejecutando caso (CON BUCLE): ", cfg$nombre, "\n",
      "========================\n", sep = "")
  
  # Predictores coherentes con la matriz μ
  vars_cfg <- colnames(cfg$matriz_mu)
  
  # Ejecutar el Método 4 (XGBoost + μ)
  res_HT <- calcular_H_T_con_Mu(
    df          = cfg$df,
    matriz_mu   = cfg$matriz_mu,
    target      = cfg$target,
    tipo_modelo = cfg$tipo,
    vars_XGB    = vars_cfg,
    max_size_S  = NULL,   # TODAS las coaliciones S
    nrounds     = 200,
    debug       = FALSE
  )
  
  # Guardar resultados del caso
  resultados_xgb[[cfg$nombre]] <- res_HT
  
  # Paso 4: Generar debug completo del caso
  salida_debug <- capture.output({
    
    cat("============================================================\n")
    cat("   DEBUG COMPLETO – ", cfg$nombre, " (CON BUCLE)\n", sep = "")
    cat("   Generado:", as.character(Sys.time()), "\n")
    cat("============================================================\n\n")
    
    S_list_cfg <- res_HT$S_list
    
    cat("===== DEBUG 1 — Instancia 1, TODAS las S =====\n\n")
    debug_instancias_todos_S_matriz(
      df          = cfg$df,
      obj_global  = res_HT$obj_global,
      matriz_mu   = cfg$matriz_mu,
      S_list      = S_list_cfg,
      i_instancia = 1
    )
    
    cat("\n\n===== DEBUG 2 — Instancias 1:2, TODAS las S =====\n\n")
    for (i in 1:2) {
      debug_instancias_todos_S_matriz(
        df          = cfg$df,
        obj_global  = res_HT$obj_global,
        matriz_mu   = cfg$matriz_mu,
        S_list      = S_list_cfg,
        i_instancia = i
      )
    }
    
    cat("\n\n===== DEBUG 3 — S = {Age, Sex}, Instancia 1 =====\n\n")
    if (all(c("Age", "Sex") %in% colnames(cfg$df))) {
      debug_instancia_S_matriz(
        df          = cfg$df,
        obj_global  = res_HT$obj_global,
        matriz_mu   = cfg$matriz_mu,
        S           = c("Age", "Sex"),
        i_instancia = 1
      )
    } else {
      cat("Variables Age y Sex no presentes en este dataset; se omite debug 3.\n")
    }
  })
  
  # Paso 5: Guardar debug usando el helper unificado
  nombre_debug <- paste0("DEBUG_", cfg$nombre, "_CON_BUCLE.txt")
  guardar_txt(nombre_debug, salida_debug)
  
  cat("\nARCHIVO DEBUG (CON BUCLE) GENERADO PARA ", cfg$nombre, "\n")
}

# Paso 6: Extraer H/T de los resultados CON BUCLE
res_HT_y_con_bucle   <- resultados_xgb[["REGRESION_y_XGB"]]
res_HT_log_con_bucle <- resultados_xgb[["CLASIFICACION_y_b_XGB"]]

H_y_2   <- res_HT_y_con_bucle$H_tabla
T_y_2   <- res_HT_y_con_bucle$T_tabla

M4_Pred_xgb_y_stream<-H_y_2
M4_Delta_xgb_y_stream<-T_y_2


H_log_2 <- res_HT_log_con_bucle$H_tabla
T_log_2 <- res_HT_log_con_bucle$T_tabla

M4_Pred_xgb_log_stream<-H_log_2
M4_Delta_xgb_log_stream<-T_log_2 

library(writexl)
write_xlsx(
  x = M4_Pred_xgb_y_stream,
  path = file.path(
    ruta_resultados, "M4-Pred-xgb-y.xlsx"
)
)

write_xlsx(
  x = M4_Delta_xgb_y_stream,
  path = file.path(
    ruta_resultados, "M4_Delta_xgb_y_stream.xlsx"
)
)

write_xlsx(
  x = M4_Pred_xgb_log_stream,
  path = file.path(
    ruta_resultados, "M4-Pred-xgb-yb.xlsx"
)
)

write_xlsx(
  x = M4_Delta_xgb_log_stream,
  path = file.path(
    ruta_resultados, "M4_Delta_xgb_log_stream.xlsx"
)
)




###############################################################################
########### GLM   - LM   ######################################################
###############################################################################
# ============================================================================
# 14. ENTRENAMIENTO DEL MODELO GLOBAL CLÁSICO
# ============================================================================
#
# Objetivo:
# Ajustar el modelo global utilizado posteriormente para el cálculo de
# H(i,S) y la ausencia de información H(i,∅).
#
# Metodología:
# - Se utiliza toda la información disponible de la base de datos.
# - Se ajusta un único modelo global.
# - El modelo permanece fijo durante todo el procedimiento.
#
# Casos considerados:
# - Regresión (LM).
# - Clasificación binaria (GLM).
#
# Interpretación:
# - El modelo global obtenido se utilizará posteriormente en el cálculo
#   de H(i,S) e y′4(∅,μ)=K.
#
# Resultado:
# - Modelo global ajustado.
#
# ============================================================================


ajustar_modelo_global_Mu_clasico <- function(df,
                                             target,
                                             tipo_modelo = c("regresion", "clasificacion"),
                                             vars_pred,
                                             verbose = 0,
                                             seed = NULL) {
  tipo_modelo <- match.arg(tipo_modelo)
  
  if (!is.null(seed)) set.seed(seed)
  
  df <- as.data.frame(df)
  
  if (!(target %in% names(df))) {
    stop("ERROR (ajustar_modelo_global_Mu_clasico): target '", target, "' no existe en df.")
  }
  if (!all(vars_pred %in% names(df))) {
    faltan <- setdiff(vars_pred, names(df))
    stop("ERROR (ajustar_modelo_global_Mu_clasico): faltan predictores en df: ",
         paste(faltan, collapse = ", "))
  }
  if (length(vars_pred) == 0) {
    stop("ERROR (ajustar_modelo_global_Mu_clasico): vars_pred está vacío.")
  }
  
  # Fórmula: target ~ x1 + x2 + ...
  formula_txt <- paste(target, "~", paste(vars_pred, collapse = " + "))
  formula_mod <- stats::as.formula(formula_txt)
  
  if (verbose > 0) {
    cat("\n[INFO] Ajustando modelo clásico global (", tipo_modelo, ") para target = ",
        target, " con predictores: ", paste(vars_pred, collapse = ", "), "\n",
        "  Fórmula: ", deparse(formula_mod), "\n\n", sep = "")
  }
  
  df_fit <- df
  
  if (tipo_modelo == "clasificacion") {
    # Aseguramos formato adecuado para glm binomial
    y <- df_fit[[target]]
    
    if (is.logical(y)) {
      df_fit[[target]] <- as.integer(y)
      
    } else if (is.factor(y)) {
      if (nlevels(y) != 2) {
        stop("En clasificación, el target factor debe tener exactamente 2 niveles.")
      }
      # glm(binomial) maneja factor 2 niveles directamente
      
    } else if (is.numeric(y)) {
      vals <- sort(unique(na.omit(y)))
      
      if (all(vals %in% c(0, 1))) {
        message("Target numérico para clasificación detectado como 0/1.")
      } else if (length(vals) == 2) {
        message("Target numérico con 2 valores distintos; se convierte a factor con esos niveles.")
        df_fit[[target]] <- factor(df_fit[[target]],
                                   levels = vals)
      } else {
        stop("Target numérico para clasificación tiene más de 2 valores distintos; ",
             "no se puede usar como binario.")
      }
      
    } else {
      stop("Tipo de target no soportado para clasificación (usa lógico, factor o numérico 0/1 o 2 valores).")
    }
    
    modelo <- stats::glm(
      formula = formula_mod,
      data    = df_fit,
      family  = stats::binomial(link = "logit")
    )
    
    pred <- stats::predict(modelo, type = "response")  # Probabilidades
    
  } else {
    # REGRESIÓN: modelo lineal clásico
    modelo <- stats::lm(
      formula = formula_mod,
      data    = df_fit
    )
    
    pred <- stats::predict(modelo, type = "response")  # valores esperados
  }
  
  K <- NA_real_  # placeholder, K real se calculará como H(i, ∅) con μ
  
  if (verbose > 0) {
    cat("[INFO] K_aux (media global predicciones, NO H(∅)) = ",
        K, "\n\n", sep = "")
  }
  
  list(
    modelo      = modelo,
    pred        = pred,
    #K           = K,
    tipo_modelo = tipo_modelo,
    target      = target,
    vars_pred   = vars_pred
  )
}



# ============================================================================
# 15. CÁLCULO DE PREDICCIÓN H(i,S) CON μ(v|S) EN LOS MODELOS CLÁSICOS MEDIANTE
# EL METODO 4
# ============================================================================
#
# Objetivo:
# Calcular la predicción H(i,S) para una instancia i y una coalición S
# utilizando un modelo clásico y la medida difusa (matriz μ).
#
# Metodología:
# - Las variables pertenecientes a S se fijan a los valores observados
#   en la instancia i.
# - Las variables fuera de S conservan los valores originales de la
#   base de datos.
# - La matriz μ(v|S) determina la capacidad predictiva de S sobre vj.
# - Se consideran todos los escenarios posibles de acierto y fallo
#   para las variables de (N\S).
# - Para cada escenario se obtiene una predicción media mediante el
#   modelo global clásico.
# - H(i,S) se calcula como la suma ponderada de dichas predicciones
#   utilizando la información que proporciona μ(v|S).
#
# Interpretación:
# - H(i,S) es la predicción para la instancia i cuando únicamente se 
# conoce la información contenida en S.
#
# Resultado:
# - Valor H(i,S).
#
# ============================================================================


H_instancia_S_matriz_clasico <- function(df,
                                         obj_global,
                                         matriz_mu,
                                         S,
                                         i_instancia,
                                         debug = FALSE) {
  S <- sort(S)
  vars_pred   <- obj_global$vars_pred
  target      <- obj_global$target
  tipo_modelo <- obj_global$tipo_modelo
  modelo      <- obj_global$modelo
  
  n <- nrow(df)
  if (i_instancia < 1 || i_instancia > n) {
    stop("i_instancia fuera de rango.")
  }
  
  # U = variables no conocidas entre los predictores
  U <- setdiff(vars_pred, S)
  k <- length(U)
  
  # Valores de la instancia i para todas las X del modelo
  inst_vals <- df[i_instancia, vars_pred, drop = FALSE]
  
  # Caso U vacío: todas las vars conocidas
  # ✅ Caso límite: S = X → baseline
  # Caso U vacío: S = X
  # MISMA definición que M1 y XGBoost: esperanza empírica
  if (k == 0L) {
    
    if (debug) {
      cat("\n[DEBUG] Caso S = X (clásico): submuestra completa con instancia fijada\n")
    }
    
    # 1) Construir submuestra: replicar la instancia i en TODA la base
    df_s <- df
    for (v in vars_pred) {
      df_s[[v]] <- df[i_instancia, v, drop = TRUE]
    }
    
    # 2) Predecir sobre TODA la base
    pred_s <- stats::predict(
      modelo,
      newdata = df_s,
      type    = "response"
    )
    
    # 3) Media empírica (baseline coherente)
    return(mean(pred_s, na.rm = TRUE))
  }
  
  # μ_v(S) para v en U usando matriz_mu
  mu_unknown <- numeric(k)
  names(mu_unknown) <- U
  for (j in seq_along(U)) {
    vj <- U[j]
    mu_unknown[j] <- get_mu_matriz(matriz_mu, S_known = S, v = vj)
  }
  
  # Todos los patrones de acierto/fallo en U (2^k escenarios)
  patterns <- as.matrix(
    do.call(expand.grid, rep(list(0:1), k))
  )
  colnames(patterns) <- U
  n_scen <- nrow(patterns)
  
  if (debug) {
    cat("\n[DEBUG] H(i,S) para instancia i = ", i_instancia,
        " con S = {", paste(S, collapse = ","), "}\n", sep = "")
    cat("  Variables no conocidas U = {", paste(U, collapse = ","), "}\n", sep = "")
    cat("  μ_v(S):\n")
    print(mu_unknown)
  }
  
  weights <- numeric(n_scen)
  contrib <- numeric(n_scen)
  
  for (s_idx in seq_len(n_scen)) {
    flags <- as.integer(patterns[s_idx, ])
    names(flags) <- U
    
    df_s <- df
    
    # Fijamos S a la instancia i
    for (v in S) {
      if (v %in% names(df_s)) {
        df_s[[v]] <- df[i_instancia, v, drop = TRUE]
      }
    }
    
    # Para cada v en U, según acierto/fallo
    for (j in seq_along(U)) {
      vj <- U[j]
      if (flags[j] == 1L) {
        df_s[[vj]] <- df[i_instancia, vj, drop = TRUE]
      }
    }
    
    # Predicción con el modelo clásico
    pred_s <- stats::predict(
      modelo,
      newdata = df_s,
      type    = "response"
    )
    m_s <- mean(pred_s, na.rm = TRUE)
    
    # Peso w_s = ∏ μ_v(S) si acierto, (1-μ_v(S)) si fallo
    w_s <- prod(ifelse(flags == 1L, mu_unknown, 1 - mu_unknown))
    
    weights[s_idx] <- w_s
    contrib[s_idx] <- w_s * m_s
    
    if (debug) {
      cat("\n  Escenario #", s_idx, "\n", sep = "")
      cat("    Flags (0=fallo, 1=acierto) en U:\n")
      print(flags)
      cat("    Peso w_s = ", w_s, "\n", sep = "")
      cat("    m_s (media pred) = ", m_s, "\n", sep = "")
      cat("    Término w_s * m_s = ", contrib[s_idx], "\n", sep = "")
    }
  }
  
  sum_w <- sum(weights)
  
  if (abs(sum_w - 1) > 1e-8) {
    stop(
      "ERROR CRÍTICO (CLÁSICO): Σ w_s = ", sum_w,
      " (debería ser 1). Revisa μ(v|S)."
    )
  }
  
  H_val <- sum(contrib)
  
  if (debug) {
    cat("\n[DEBUG] Resumen H(i,S):\n")
    cat("  Σ w_s = ", sum_w, "\n", sep = "")
    cat("  Términos w_s * m_s:\n")
    print(contrib)
    cat("  H(i,S) = mean(w_s * m_s) = ", H_val, "\n\n", sep = "")
  }
  
  if (abs(sum_w - 1) > 1e-6) {
    warning("La suma de pesos Σ w_s = ", sum_w,
            " se desvía de 1. Revisa tus μ_j(S) en matriz_mu.")
  }
  
  H_val
}


# ============================================================================
# 16. CÁLCULO DE K, H(i,S) Y T(i,S) PARA TODAS LAS INSTANCIAS Y COALICIONES
# ============================================================================
#
# Objetivo:
# Calcular K, H(i,S) y T(i,S) para todas las instancias y coaliciones.
#
# Metodología:
# - Se calcula H(i,S) para cada instancia y coalición S.
# - K es el caso especifico de S={∅}:
#
#       K = H(i,∅)
#
# - Se calcula:
#
#       T(i,S) = H(i,S) - K
#
# Interpretación:
# - K representa la predicción asociada a la ausencia de información.
# - T(i,S) representa la variación de predicción de H(i,S) respecto a K.
#
# Resultado:
# - Tabla H(i,S).
# - Tabla T(i,S).
# - Valor K.
#
# ============================================================================

calcular_K_desde_H_empty_matriz_clasico <- function(df,
                                                    obj_global,
                                                    matriz_mu) {
  n <- nrow(df)
  
  H_empty_vals <- sapply(seq_len(n), function(i) {
    H_instancia_S_matriz_clasico(
      df          = df,
      obj_global  = obj_global,
      matriz_mu   = matriz_mu,
      S           = character(0),
      i_instancia = i,
      debug       = FALSE
    )
  })
  
  K_vals <- unique(round(H_empty_vals, 12))
  stopifnot(length(K_vals) == 1)
  
  K_vals
}

calcular_H_T_para_Sets_matriz_clasico <- function(df,
                                                  obj_global,
                                                  matriz_mu,
                                                  S_list,
                                                  debug      = FALSE,
                                                  inst_debug = NULL,
                                                  S_debug    = NULL) {
  n <- nrow(df)
  K <- calcular_K_desde_H_empty_matriz_clasico(
    df         = df,
    obj_global = obj_global,
    matriz_mu  = matriz_mu
  )
  
  H_tabla <- data.frame(instancia = 1:n)
  T_tabla <- data.frame(instancia = 1:n)
  
  for (S in S_list) {
    S <- sort(S)
    nombre_S <- paste(S, collapse = "_")
    col_H    <- paste0("H_S_", nombre_S)
    col_T    <- paste0("T_S_", nombre_S)
    
    # Debug opcional
    if (debug && !is.null(inst_debug)) {
      if (is.null(S_debug) || identical(sort(S_debug), S)) {
        cat("\n=============================================\n")
        cat("DEBUG H(i,S) para S = {", paste(S, collapse = ","), "}\n", sep = "")
        invisible(H_instancia_S_matriz_clasico(
          df          = df,
          obj_global  = obj_global,
          matriz_mu   = matriz_mu,
          S           = S,
          i_instancia = inst_debug,
          debug       = TRUE
        ))
        cat("=============================================\n\n")
      }
    }
    
    H_vec <- numeric(n)
    for (i in 1:n) {
      H_vec[i] <- H_instancia_S_matriz_clasico(
        df          = df,
        obj_global  = obj_global,
        matriz_mu   = matriz_mu,
        S           = S,
        i_instancia = i,
        debug       = FALSE
      )
    }
    
    T_vec <- H_vec - K
    
    H_tabla[[col_H]] <- H_vec
    T_tabla[[col_T]] <- T_vec
  }
  
  # Conjunto vacío explícito (definición estricta)
  H_tabla$H_S_empty <- K
  T_tabla$T_S_empty <- H_tabla$H_S_empty - K
  stopifnot(all.equal(T_tabla$T_S_empty, rep(0, nrow(T_tabla))))
  
  # Columna K explícita (para análisis)
  H_tabla$K <- K
  
  list(
    H_tabla = H_tabla,
    T_tabla = T_tabla,
    K       = K
  )
}




# ============================================================================
# 17. FUNCIONES DE DEPURACIÓN Y VALIDACIÓN
# ============================================================================
#
# Objetivo:
# Facilitar la revisión detallada del cálculo de H(i,S) para instancias
# y coaliciones específicas.
#
# Metodología:
# - Permiten visualizar el cálculo paso a paso de H(i,S).
# - Muestran los valores μ(v|S) utilizados.
# - Muestran los escenarios considerados y sus pesos asociados.
# - Muestran las contribuciones que intervienen en el valor final de
#   H(i,S).
#
# Interpretación:
# - Estas funciones tienen una finalidad exclusivamente diagnóstica.
# - No intervienen en los cálculos principales del método.
#
# Resultado:
# - Información detallada para la validación del cálculo de H(i,S).
#
# ============================================================================


debug_instancias_todos_S_matriz_clasico <- function(df,
                                                    obj_global,
                                                    matriz_mu,
                                                    S_list,
                                                    i_instancia) {
  cat("\n=========== DEBUG COMPLETO PARA INSTANCIA i = ",
      i_instancia, " ===========\n", sep = "")
  cat("Target: ", obj_global$target, "  |  Tipo: ", obj_global$tipo_modelo, "\n")
  cat("Predictores: ", paste(obj_global$vars_pred, collapse = ", "), "\n\n")
  
  # ==========================================================
  # ✅ DEBUG EXPLÍCITO PARA S = {}  (CONJUNTO VACÍO)
  # ==========================================================
  cat("\n----- S = {} -----\n")
  invisible(
    H_instancia_S_matriz_clasico(
      df          = df,
      obj_global  = obj_global,
      matriz_mu   = matriz_mu,
      S           = character(0),
      i_instancia = i_instancia,
      debug       = TRUE
    )
  )
  
  # ==========================================================
  # ✅ DEBUG PARA EL RESTO DE S (NO VACÍOS)
  # ==========================================================
  for (S in S_list) {
    S <- sort(S)
    nombre_S <- paste(S, collapse = ",")
    cat("\n----- S = {", nombre_S, "} -----\n", sep = "")
    invisible(
      H_instancia_S_matriz_clasico(
        df          = df,
        obj_global  = obj_global,
        matriz_mu   = matriz_mu,
        S           = S,
        i_instancia = i_instancia,
        debug       = TRUE
      )
    )
  }
}



debug_instancia_S_matriz_clasico <- function(df,
                                             obj_global,
                                             matriz_mu,
                                             S,
                                             i_instancia) {
  S <- sort(S)
  nombre_S <- paste(S, collapse = ",")
  
  cat("\n=========== DEBUG PARA INSTANCIA i = ",
      i_instancia, ", S = {", nombre_S, "} ===========\n", sep = "")
  cat("Target: ", obj_global$target, "  |  Tipo: ", obj_global$tipo_modelo, "\n")
  cat("Predictores: ", paste(obj_global$vars_pred, collapse = ", "), "\n\n")
  
  invisible(H_instancia_S_matriz_clasico(
    df          = df,
    obj_global  = obj_global,
    matriz_mu   = matriz_mu,
    S           = S,
    i_instancia = i_instancia,
    debug       = TRUE
  ))
}

# ============================================================================
# 18. IMPLEMENTACION DEL MÉTODO 4 CON MODELOS CLASICOS
# ============================================================================
#
# Objetivo:
# Implementar el procedimiento completo para obtener K, H(i,S) y T(i,S)
# a partir de una matriz μ previamente calculada.
#
# Metodología:
# - Se utiliza la matriz μ obtenida en fases previas.
# - Se ajusta un único modelo global clásico.
# - Se generan las coaliciones S.
# - Se calculan los valores H(i,S).
# - Se calculan los valores T(i,S).
#
# Interpretación:
# - Este bloque integra todas las etapas del método.
#
# Resultado:
# - Tabla H(i,S).
# - Tabla T(i,S).
# - Valor K.
# - Coaliciones S.
# - Modelo global utilizado.
#
# ============================================================================

calcular_H_T_con_Mu_clasico <- function(df,
                                        matriz_mu,
                                        target,
                                        tipo_modelo = c("regresion", "clasificacion"),
                                        vars_XGB   = NULL,
                                        max_size_S = NULL,
                                        inst_debug = NULL,
                                        S_debug    = NULL,
                                        debug      = FALSE,
                                        verbose    = 0,
                                        seed       = 123) {
  tipo_modelo <- match.arg(tipo_modelo)
  
  vars_original <- colnames(matriz_mu)  # variables originales usadas en μ
  
  # Predictores válidos
  if (is.null(vars_XGB)) {
    vars_XGB <- vars_original
  } else {
    if (!all(vars_XGB %in% vars_original)) {
      faltan <- setdiff(vars_XGB, vars_original)
      stop("calcular_H_T_con_Mu_clasico: estas vars_XGB no están en matriz_mu: ",
           paste(faltan, collapse = ", "))
    }
  }
  
  # Nunca incluir el target como predictor
  if (target %in% vars_XGB) {
    vars_XGB <- setdiff(vars_XGB, target)
  }
  
  # 1) Modelo global clásico
  obj_global <- ajustar_modelo_global_Mu_clasico(
    df          = df,
    target      = target,
    tipo_modelo = tipo_modelo,
    vars_pred   = vars_XGB,
    verbose     = verbose,
    seed        = seed
  )
  
  # 2) Lista de S (subconjuntos no vacíos de vars_XGB)
  S_list <- generar_S_list(vars_XGB, max_size = max_size_S)
  
  # 3) Debug completo para una instancia si se pide
  if (!is.null(inst_debug) && debug) {
    debug_instancias_todos_S_matriz_clasico(
      df          = df,
      obj_global  = obj_global,
      matriz_mu   = matriz_mu,
      S_list      = if (!is.null(S_debug)) list(S_debug) else S_list,
      i_instancia = inst_debug
    )
  }
  
  # 4) H(i,S) y T(i,S)
  res_HT <- calcular_H_T_para_Sets_matriz_clasico(
    df          = df,
    obj_global  = obj_global,
    matriz_mu   = matriz_mu,
    S_list      = S_list,
    debug       = FALSE,
    inst_debug  = NULL,
    S_debug     = NULL
  )
  
  list(
    H_tabla    = res_HT$H_tabla,
    T_tabla    = res_HT$T_tabla,
    K          = res_HT$K,
    vars_XGB   = vars_XGB,
    S_list     = S_list,
    obj_global = obj_global
  )
}

# ============================================================================
# 19. APLICACIÓN DEL MÉTODO 4 Y EXPORTACIÓN DE RESULTADOS
# ============================================================================
#
# Objetivo:
# Aplicar el método clásico a los modelos de regresión y
# clasificación y almacenar los resultados obtenidos.
#
# Metodología:
# - Se define la configuración de cada caso de estudio.
# - Se ejecuta el método utilizando la matriz μ correspondiente.
# - Se obtienen las tablas H(i,S), T(i,S) y el valor K.
# - Se generan salidas de validación y depuración.
# - Los resultados se almacenan y exportan para su análisis posterior.
#
# Casos considerados:
# - Regresión (LM).
# - Clasificación binaria (GLM).
#
# Resultado:
# - Tabla H(i,S).
# - Tabla T(i,S).
# - Valor K.
# - Archivos de depuración.
# - Resultados exportados.
#
# ============================================================================

# Configuración de los casos clásicos
config_list_clasico <- list(
  list(
    nombre    = "REGRESION_y_LM",
    df        = HNANESI_y,
    matriz_mu = matriz_mu_y,
    target    = "y",
    tipo      = "regresion"
  ),
  list(
    nombre    = "CLASIFICACION_y_b_GLM",
    df        = HNANESI_log,
    matriz_mu = matriz_mu_log,
    target    = "y_b",
    tipo      = "clasificacion"
  )
)

# Lista de resultados
resultados_clasico <- list()

# Ejecutar Método 4 clásico para cada configuración
for (cfg in config_list_clasico) {
  
  cat("\n========================\n",
      "Ejecutando caso (CLÁSICO, CON BUCLE): ", cfg$nombre, "\n",
      "========================\n", sep = "")
  
  vars_cfg <- colnames(cfg$matriz_mu)
  
  # Ejecutar método clásico (LM / GLM + μ)
  res_HT <- calcular_H_T_con_Mu_clasico(
    df          = cfg$df,
    matriz_mu   = cfg$matriz_mu,
    target      = cfg$target,
    tipo_modelo = cfg$tipo,
    vars_XGB    = vars_cfg,
    max_size_S  = NULL,
    debug       = FALSE
  )
  
  # Guardar resultados
  resultados_clasico[[cfg$nombre]] <- res_HT
  
  # Debug completo del caso
  salida_debug <- capture.output({
    
    cat("============================================================\n")
    cat("   DEBUG COMPLETO – ", cfg$nombre, " (CLÁSICO, CON BUCLE)\n", sep = "")
    cat("   Generado:", as.character(Sys.time()), "\n")
    cat("============================================================\n\n")
    
    S_list_cfg <- res_HT$S_list
    
    cat("===== DEBUG 1 — Instancia 1, TODAS las S =====\n\n")
    debug_instancias_todos_S_matriz_clasico(
      df          = cfg$df,
      obj_global  = res_HT$obj_global,
      matriz_mu   = cfg$matriz_mu,
      S_list      = S_list_cfg,
      i_instancia = 1
    )
    
    cat("\n\n===== DEBUG 2 — Instancias 1:2, TODAS las S =====\n\n")
    for (i in 1:2) {
      debug_instancias_todos_S_matriz_clasico(
        df          = cfg$df,
        obj_global  = res_HT$obj_global,
        matriz_mu   = cfg$matriz_mu,
        S_list      = S_list_cfg,
        i_instancia = i
      )
    }
    
    cat("\n\n===== DEBUG 3 — S = {Age, Sex}, Instancia 1 =====\n\n")
    if (all(c("Age", "Sex") %in% colnames(cfg$df))) {
      debug_instancia_S_matriz_clasico(
        df          = cfg$df,
        obj_global  = res_HT$obj_global,
        matriz_mu   = cfg$matriz_mu,
        S           = c("Age", "Sex"),
        i_instancia = 1
      )
    } else {
      cat("Variables Age y Sex no presentes en este dataset; se omite debug 3.\n")
    }
  })
  
  # Guardar debug correctamente
  nombre_debug <- paste0("DEBUG_", cfg$nombre, "_CON_BUCLE.txt")
  guardar_txt(nombre_debug, salida_debug)
  
  cat("\nDEBUG (CLÁSICO, CON BUCLE) GENERADO PARA ", cfg$nombre, "\n")
}

# Extraer H/T para comparativas
res_HT_y_lm_con_bucle    <- resultados_clasico[["REGRESION_y_LM"]]
res_HT_log_glm_con_bucle <- resultados_clasico[["CLASIFICACION_y_b_GLM"]]

H_y_lm_2    <- res_HT_y_lm_con_bucle$H_tabla
T_y_lm_2    <- res_HT_y_lm_con_bucle$T_tabla
H_log_glm_2 <- res_HT_log_glm_con_bucle$H_tabla
T_log_glm_2 <- res_HT_log_glm_con_bucle$T_tabla

# Normalizar nombres par comparar.

M4_Pred_lm_y_stream<-H_y_lm_2
M4_Delta_lm_y_stream<-T_y_lm_2 
M4_Pred_glm_yb_stream<-H_log_glm_2
M4_Delta_glm_yb_stream<-T_log_glm_2


# Descarga de datos
library(writexl)

# Exportamos el data.frame a un archivo Excel

write_xlsx(
  x = M4_Delta_lm_y_stream,
  path = file.path(
    ruta_resultados, "M4_Delta_lm_y_stream.xlsx"
)
)

write_xlsx(
  x = M4_Pred_lm_y_stream,
  path = file.path(
    ruta_resultados, "M4-Pred-lm-y.xlsx"
)
)

write_xlsx(
  x = M4_Delta_glm_yb_stream,
  path = file.path(
    ruta_resultados, "M4_Delta_glm_yb_stream.xlsx"
)
)

write_xlsx(
  x = M4_Pred_glm_yb_stream,
  path = file.path(
    ruta_resultados,"M4-Pred-glm-yb.xlsx"
)
)


############################################################
# COMPARATIVA H_y_lm vs H_y_lm_2 y T_y_lm vs T_y_lm_2 (REGRESIÓN, CLÁSICO)
############################################################

# Alinear columnas comunes de H (excluyendo 'instancia')
cols_comunes_H_y_lm <- intersect(names(H_y_lm), names(H_y_lm_2))
cols_comunes_H_y_lm <- setdiff(cols_comunes_H_y_lm, "instancia")

dif_H_y_lm <- as.matrix(
  H_y_lm[, cols_comunes_H_y_lm, drop = FALSE] -
    H_y_lm_2[, cols_comunes_H_y_lm, drop = FALSE]
)

max_diff_H_y_lm <- apply(dif_H_y_lm, 2, function(col) max(abs(col), na.rm = TRUE))
cat("\nMáxima diferencia absoluta por columna en H_y_lm vs H_y_lm_2 (REGRESIÓN, CLÁSICO):\n")
print(max_diff_H_y_lm)

# Alinear columnas comunes de T (excluyendo 'instancia')
cols_comunes_T_y_lm <- intersect(names(T_y_lm), names(T_y_lm_2))
cols_comunes_T_y_lm <- setdiff(cols_comunes_T_y_lm, "instancia")

dif_T_y_lm <- as.matrix(
  T_y_lm[, cols_comunes_T_y_lm, drop = FALSE] -
    T_y_lm_2[, cols_comunes_T_y_lm, drop = FALSE]
)

max_diff_T_y_lm <- apply(dif_T_y_lm, 2, function(col) max(abs(col), na.rm = TRUE))
cat("\nMáxima diferencia absoluta por columna en T_y_lm vs T_y_lm_2 (REGRESIÓN, CLÁSICO):\n")
print(max_diff_T_y_lm)


############################################################
# COMPARATIVA H_log_glm vs H_log_glm_2 y T_log_glm vs T_log_glm_2 (CLASIFICACIÓN)
############################################################

# Alinear columnas comunes de H (excluyendo 'instancia')
cols_comunes_H_log_glm <- intersect(names(H_log_glm), names(H_log_glm_2))
cols_comunes_H_log_glm <- setdiff(cols_comunes_H_log_glm, "instancia")

dif_H_log_glm <- as.matrix(
  H_log_glm[, cols_comunes_H_log_glm, drop = FALSE] -
    H_log_glm_2[, cols_comunes_H_log_glm, drop = FALSE]
)

max_diff_H_log_glm <- apply(dif_H_log_glm, 2, function(col) max(abs(col), na.rm = TRUE))
cat("\nMáxima diferencia absoluta por columna en H_log_glm vs H_log_glm_2 (CLASIFICACIÓN):\n")
print(max_diff_H_log_glm)

# Alinear columnas comunes de T (excluyendo 'instancia')
cols_comunes_T_log_glm <- intersect(names(T_log_glm), names(T_log_glm_2))
cols_comunes_T_log_glm <- setdiff(cols_comunes_T_log_glm, "instancia")

dif_T_log_glm <- as.matrix(
  T_log_glm[, cols_comunes_T_log_glm, drop = FALSE] -
    T_log_glm_2[, cols_comunes_T_log_glm, drop = FALSE]
)

max_diff_T_log_glm <- apply(dif_T_log_glm, 2, function(col) max(abs(col), na.rm = TRUE))
cat("\nMáxima diferencia absoluta por columna en T_log_glm vs T_log_glm_2 (CLASIFICACIÓN):\n")
print(max_diff_T_log_glm)





