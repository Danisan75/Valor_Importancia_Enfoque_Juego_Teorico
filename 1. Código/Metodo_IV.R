
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
# posterior en la construcción de los operadores H(i,S) y T(i,S).
#
# Metodología:
# - Se parte de los resultados obtenidos del cálculo de μ(v|S).
# - Se extrae la matriz μ asociada a cada problema de aprendizaje.
# - Cada matriz contiene la información de dependencia previamente
#   estimada entre variables.
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
# - Confirmación de la estructura dimensional de ambas matrices.
#
# ============================================================================



matriz_mu_y   <- res_Mu_y$matriz_mu    # para regresión (target = y)
matriz_mu_log <- res_Mu_log$matriz_mu  # para clasificación (target = y_b)

cat("\n===== MATRIZ μ_j(S) PARA REGRESIÓN (target = y) =====\n")
print(dim(matriz_mu_y))

cat("\n===== MATRIZ μ_j(S) PARA CLASIFICACIÓN (target = y_booleana) =====\n")
print(dim(matriz_mu_log))

# ============================================================================
# 2. PREPARACIÓN DE LOS DATOS DE ENTRADA PARA EL MODELO GLOBAL XGBOOST
# ============================================================================
#
# Objetivo:
# Construir una representación homogénea de los datos que permita utilizar
# de forma consistente un modelo global XGBoost en todas las fases
# posteriores del método.
#
# Metodología:
# - Se parte de una base de datos original formada por una variable
#   objetivo y un conjunto de variables predictoras.
# - Se verifica la existencia de la variable objetivo seleccionada.
# - Se verifica la disponibilidad de todas las variables predictoras
#   requeridas para el análisis.
# - Se separa la variable objetivo y del conjunto de predictores X.
# - La matriz de predictores se transforma a una representación
#   numérica compatible con XGBoost.
# - En presencia de variables categóricas, se generan automáticamente
#   las codificaciones necesarias para su utilización por el modelo.
#
# Interpretación:
# - Este paso define el espacio de entrada utilizado por el modelo
#   global XGBoost.
# - Todas las predicciones posteriores se realizarán utilizando esta
#   misma estructura de variables.
# - La consistencia de dicha estructura garantiza la comparabilidad
#   entre las distintas coaliciones analizadas posteriormente.
#
# Validación:
# - Se comprueba la existencia de la variable objetivo.
# - Se comprueba la existencia de todos los predictores requeridos.
# - Se verifica que el conjunto de predictores no esté vacío.
# - Se verifica que la matriz numérica generada sea válida para su
#   utilización por XGBoost.
#
# Resultado:
# - Matriz numérica de predictores X preparada para XGBoost.
# - Variable objetivo y asociada.
# - Identificación del conjunto de predictores utilizado.
# - Estructura común que servirá de referencia para todas las fases
#   posteriores del procedimiento.
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
# 3. ENTRENAMIENTO DEL MODELO GLOBAL XGBOOST
# ============================================================================
#
# Objetivo:
# Construir un único modelo global XGBoost utilizando toda la información
# disponible de la base de datos y obtener las predicciones asociadas a
# dicho modelo.
#
# Metodología:
# - Se parte de la matriz de predictores X y de la variable objetivo y
#   previamente preparadas.
# - Se utiliza un único conjunto de datos que contiene todas las
#   observaciones disponibles.
# - Se entrena un único modelo global XGBoost.
# - El modelo se ajusta una sola vez y permanece fijo durante todo
#   el procedimiento posterior.
# - No se entrena ningún modelo específico para coaliciones concretas.
# - No se construyen modelos dependientes de las distintas configuraciones
#   de información consideradas posteriormente.
#
# Casos considerados:
#
# Regresión:
# - La variable objetivo se transforma a formato numérico.
# - Se utiliza una función objetivo de regresión para el entrenamiento
#   del modelo.
#
# Clasificación:
# - Las variables objetivo lógicas se transforman a valores 0 y 1.
# - Las variables objetivo categóricas binarias se transforman a
#   valores 0 y 1.
# - Las variables objetivo numéricas se utilizan directamente.
# - Se utiliza una función objetivo de clasificación binaria para el
#   entrenamiento del modelo.
#
# Interpretación:
# - El modelo obtenido representa la función predictiva global f(·)
#   utilizada en todo el método.
# - Todas las evaluaciones posteriores utilizarán exactamente el mismo
#   modelo entrenado.
# - Las diferencias observadas entre escenarios o coaliciones se deberán
#   exclusivamente a la información proporcionada al modelo y no a cambios
#   en el proceso de entrenamiento.
#
# Relación con la matriz μ:
# - La matriz μ(v|S) no interviene en esta fase.
# - No se utilizan probabilidades de dependencia durante el ajuste
#   del modelo.
# - La integración de μ(v|S) se realiza posteriormente durante la
#   construcción del operador H(i,S).
#
# Predicciones globales:
# - Una vez entrenado el modelo se calculan las predicciones para todas
#   las observaciones de la base de datos.
# - Estas predicciones permiten verificar el comportamiento general
#   del modelo entrenado.
#
# Valor auxiliar K_aux:
# - Se define como la media de las predicciones generadas por el modelo
#   global sobre la base de datos completa.
# - Su función es exclusivamente diagnóstica y de validación.
# - No forma parte de la definición formal del método.
# - No sustituye al valor H(∅).
# - No interviene en el cálculo posterior de H(i,S).
#
# Validación:
# - Se verifica la consistencia de la variable objetivo con el tipo de
#   modelo especificado.
# - Se verifica la correcta construcción de la matriz empleada por
#   XGBoost.
# - Se almacena la estructura exacta de variables utilizada durante
#   el entrenamiento.
#
# Resultado:
# - Modelo global XGBoost entrenado.
# - Predicciones globales asociadas al modelo.
# - Valor auxiliar K_aux para comprobaciones diagnósticas.
# - Identificación del conjunto de predictores utilizado.
# - Estructura exacta de variables empleada por el modelo global.
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
  
  # ✅ Solo sanity-check, no parte del método
  K_aux <- mean(pred, na.rm = TRUE)
  
  feature_names <- colnames(X_mat)
  
  cat(
    bold$magenta(
      "[INFO] Media global predicciones E_D[f(X)] (sanity-check) = "
    ),
    K_aux, "\n\n", sep = ""
  )
  
  list(
    modelo        = modelo,
    pred          = pred,
    K_aux         = K_aux,   # ✅ SOLO informativo
    tipo_modelo   = tipo,
    target        = target,
    vars_pred     = vars_pred,
    feature_names = feature_names
  )
}


# ============================================================================
# 4. PREPARACIÓN Y ALINEACIÓN DE LAS SUBMUESTRAS PARA EL MODELO GLOBAL XGBOOST
# ============================================================================
#
# Objetivo:
# Construir una representación de las submuestras contrafactuales que sea
# completamente compatible con la estructura utilizada durante el
# entrenamiento del modelo global XGBoost.
#
# Metodología:
# - Se parte de una submuestra contrafactual generada previamente.
# - Se seleccionan exclusivamente las variables predictoras utilizadas
#   por el modelo global.
# - Se transforma la información disponible a una representación
#   numérica compatible con XGBoost.
# - Se reconstruye la estructura exacta de variables empleada durante
#   el entrenamiento del modelo global.
# - Cuando alguna variable derivada no está presente en la submuestra,
#   se incorpora manteniendo un valor nulo.
# - Las variables comunes entre la submuestra y el modelo global se
#   conservan sin modificación.
#
# Interpretación:
# - Este paso garantiza que todas las predicciones se realizan dentro
#   del mismo espacio de representación utilizado por el modelo global.
# - La estructura de entrada permanece constante para todas las
#   coaliciones, instancias y escenarios analizados.
# - Las diferencias observadas entre escenarios reflejan únicamente
#   cambios en la información disponible y no cambios en la estructura
#   de predicción.
#
# Consistencia:
# - No se modifica el modelo global previamente entrenado.
# - No se ajustan nuevos modelos.
# - No se recalculan parámetros del modelo.
# - No interviene todavía la matriz μ(v|S).
#
# Validación:
# - Se comprueba la existencia de todas las variables predictoras
#   requeridas en la submuestra.
# - Se verifica la correcta construcción de la representación numérica.
# - Se garantiza la coincidencia entre las variables utilizadas por la
#   submuestra y las esperadas por el modelo global.
#
# Resultado:
# - Matriz de predictores alineada con el modelo global XGBoost.
# - Estructura de variables completamente compatible con las fases
#   posteriores de predicción.
# - Base común para la evaluación consistente de los distintos
#   escenarios considerados en el operador H(i,S).
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
# 5. RECUPERACIÓN DE LOS VALORES μ(v|S)
# ============================================================================
#
# Objetivo:
# Obtener el valor μ(v|S) asociado a una variable v y a una coalición S
# utilizando una matriz μ previamente calculada.
#
# Metodología:
# - Se parte de una matriz μ cuyos valores ya han sido calculados.
# - Se identifica la coalición S formada por las variables conocidas.
# - Se construye el identificador asociado a dicha coalición.
# - Se localiza la fila correspondiente a S en la matriz μ.
# - Se recupera el valor asociado a la variable v.
#
# Caso especial:
# - Si la variable v ya pertenece a la coalición S, se asigna:
#
#     μ(v|S) = 1
#
# Interpretación:
# - El valor recuperado representa el valor μ(v|S) almacenado para la
#   combinación formada por la variable v y la coalición S.
#
# Validación:
# - Se comprueba que la variable v existe en la matriz μ.
# - Se comprueba que la coalición S existe en la matriz μ.
# - Se verifica que el valor recuperado no sea ausente.
# - Se verifica que el valor recuperado pertenezca al intervalo [0,1].
#
# Resultado:
# - Valor μ(v|S) asociado a la variable v y a la coalición S.
#
# ============================================================================

get_mu_matriz <- function(matriz_mu, S_known, v) {
  
  # Variables base inferidas de la matriz μ
  vars_X_all <- colnames(matriz_mu)
  
  # Si v pertenece a S, por definición μ(v | S) = 1
  if (v %in% S_known) return(1)
  
  # Validar variable objetivo
  if (!(v %in% colnames(matriz_mu))) {
    stop("get_mu_matriz: la variable '", v, "' no está en matriz_mu.")
  }
  
  # Construir clave canónica de S
  if (length(S_known) == 0) {
    S_key <- "empty"
  } else {
    S_key <- paste(intersect(vars_X_all, S_known), collapse = "+")
  }
  
  # Comprobar existencia de la fila
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
# 6. CÁLCULO DE H(i,S) MEDIANTE μ(v|S) Y UN MODELO GLOBAL XGBOOST
# ============================================================================
#
# Objetivo:
# Calcular el valor H(i,S) asociado a una instancia i y a una coalición S
# utilizando un modelo global XGBoost y los valores μ(v|S) almacenados
# previamente.
#
# Metodología:
# - Se fija la coalición S utilizando los valores observados en la
#   instancia i.
# - Se identifican las variables no incluidas en S.
# - Para cada una de dichas variables se recupera el valor μ(v|S)
#   correspondiente.
# - Se generan todos los escenarios posibles de acierto y fallo para
#   las variables no incluidas en S.
# - En cada escenario:
#     · Las variables pertenecientes a S permanecen fijadas a los valores
#       observados en la instancia i.
#     · Las variables con acierto se fijan también al valor observado
#       en la instancia i.
#     · Las variables con fallo conservan la variabilidad original de
#       la base de datos.
# - Para cada escenario se construye la correspondiente submuestra
#   contrafactual.
# - La submuestra se transforma a una representación compatible con el
#   modelo global XGBoost.
# - Se obtienen las predicciones del modelo global para todas las filas
#   de la submuestra.
# - Se calcula la media de dichas predicciones.
#
# Ponderación:
# - A cada escenario se le asigna un peso construido a partir de los
#   valores μ(v|S) de las variables no incluidas en S.
# - Los pesos asociados a todos los escenarios deben sumar 1.
#
# Cálculo de H(i,S):
# - H(i,S) se obtiene como la suma ponderada de las medias de predicción
#   calculadas para todos los escenarios considerados.
#
# Caso particular:
# - Cuando S contiene todas las variables predictoras, todas las
#   variables permanecen fijadas a los valores observados en la
#   instancia i.
# - En este caso H(i,S) se obtiene directamente como la media de las
#   predicciones generadas por el modelo sobre la submuestra construida.
#
# Validación:
# - Se comprueba que la instancia solicitada existe en la base de datos.
# - Se verifica la obtención de todos los valores μ(v|S) necesarios.
# - Se comprueba que la suma de los pesos de los escenarios sea igual a 1.
#
# Resultado:
# - Valor H(i,S) asociado a la instancia i y a la coalición S.
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
    
    # 1) Construir submuestra replicando la instancia i en TODA la base
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
    
    # 4) Media empírica (MISMA definición que M1)
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
# 7. GENERACIÓN DE LAS COALICIONES DE VARIABLES PREDICTORAS
# ============================================================================
#
# Objetivo:
# Generar todas las coaliciones de variables predictoras que serán
# utilizadas posteriormente en los cálculos dependientes del conjunto S.
#
# Metodología:
# - Se parte del conjunto completo de variables predictoras.
# - Se generan todas las combinaciones posibles de variables cuyo tamaño
#   sea mayor o igual que uno.
# - Opcionalmente, puede establecerse un tamaño máximo para limitar el
#   número de variables incluidas en cada coalición.
# - Cada coalición se almacena como un conjunto específico de variables.
#
# Interpretación:
# - Cada coalición S representa un nivel concreto de información
#   disponible.
# - Las coaliciones generadas constituyen el conjunto de configuraciones
#   que serán evaluadas posteriormente mediante los procedimientos
#   definidos para el método.
#
# Validación:
# - Se comprueba que exista al menos una variable predictora.
# - Se verifica que el tamaño máximo considerado no supere el número
#   total de variables disponibles.
#
# Resultado:
# - Lista completa de coaliciones de variables predictoras.
# - Una entrada por cada subconjunto S generado.
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
# 9. FUNCIONES DE DEPURACIÓN Y VALIDACIÓN
# ============================================================================
#
# Objetivo:
# Facilitar la inspección detallada del cálculo de H(i,S) para comprobar
# el correcto funcionamiento del método.
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
# 10. EJECUCIÓN DEL MÉTODO
# ============================================================================
#
# Objetivo:
# Ejecutar de forma conjunta todas las fases del método para obtener los
# valores H(i,S) y T(i,S) a partir de una matriz μ(v|S) previamente
# calculada.
#
# Metodología:
# - Se determina el conjunto de variables predictoras que participarán
#   en el análisis.
# - Se entrena un modelo global XGBoost utilizando dichas variables.
# - Se generan las coaliciones de variables predictoras consideradas
#   en el estudio.
# - Se calculan los valores H(i,S) para todas las instancias y
#   coaliciones.
# - A partir de H(i,S) y del valor de referencia K se calculan los
#   valores T(i,S).
#
# Interpretación:
# - Este bloque coordina la ejecución completa del método.
# - Integra la información contenida en μ(v|S), las coaliciones
#   generadas y el modelo global XGBoost para obtener los resultados
#   finales.
#
# Resultado:
# - Tabla H(i,S).
# - Tabla T(i,S).
# - Valor de referencia K.
# - Coaliciones consideradas.
# - Modelo global XGBoost utilizado en el análisis.
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
# 11. RENOMBRADO Y REORDENACIÓN DE LAS TABLAS H(i,S) Y T(i,S)
# ============================================================================
#
# Objetivo:
# Estandarizar la nomenclatura y la organización de las tablas H(i,S) y
# T(i,S) obtenidas durante el método.
#
# Metodología:
# - Se transforman los nombres internos utilizados durante los cálculos
#   a una notación metodológica más interpretable.
# - La coalición vacía se representa mediante H() y T().
# - Las restantes coaliciones se expresan mediante la notación H(S) y
#   T(S), donde S identifica el conjunto de variables considerado.
# - Las columnas se reorganizan para mantener una estructura homogénea
#   y facilitar la interpretación de los resultados.
#
# Interpretación:
# - Este paso no modifica los valores calculados.
# - Únicamente adapta la presentación de las tablas para su análisis,
#   visualización y exportación.
#
# Resultado:
# - Tabla H(i,S) con nomenclatura estandarizada.
# - Tabla T(i,S) con nomenclatura estandarizada.
# - Reordenación consistente de las columnas de resultados.
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
# 12. EJECUCIÓN DEL MÉTODO Y EXPORTACIÓN DE RESULTADOS
# ============================================================================
#
# Objetivo:
# Ejecutar el método completo para los problemas de regresión y
# clasificación, obtener las tablas H(i,S) y T(i,S), realizar
# comprobaciones de depuración y exportar los resultados generados.
#
# Metodología:
# - Se define la configuración de cada caso de estudio, incluyendo la
#   base de datos, la matriz μ(v|S), la variable objetivo y el tipo
#   de problema considerado.
# - Para cada configuración se ejecuta el método completo mediante el
#   procedimiento definido previamente.
# - Se obtienen las tablas H(i,S), T(i,S) y el valor de referencia K.
# - Opcionalmente se generan salidas de depuración para revisar el
#   comportamiento del método en instancias y coaliciones específicas.
# - Los resultados obtenidos se organizan y almacenan de forma
#   independiente para cada caso analizado.
#
# Casos considerados:
#
# Regresión:
# - Variable objetivo continua.
#
# Clasificación:
# - Variable objetivo binaria.
#
# Interpretación:
# - Este paso aplica el método completo a los conjuntos de datos
#   considerados.
# - Permite obtener los resultados finales necesarios para su análisis,
#   comparación e interpretación.
#
# Resultado:
# - Tablas H(i,S) para cada caso de estudio.
# - Tablas T(i,S) para cada caso de estudio.
# - Valor K asociado a cada análisis.
# - Información de depuración para validación del procedimiento.
# - Exportación de los resultados finales.
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
  path = "C:/Users/.../M4_Pred_xgb_y_stream.xlsx"
)
write_xlsx(
  x = M4_Delta_xgb_y_stream,
  path = "C:/Users/.../M4_Delta_xgb_y_stream.xlsx"
)
write_xlsx(
  x = M4_Pred_xgb_log_stream,
  path = "C:/Users/.../M4_Pred_xgb_log_stream.xlsx"
)
write_xlsx(
  x = M4_Delta_xgb_log_stream,
  path = "C:/Users/.../M4_Delta_xgb_log_stream.xlsx"
)




###############################################################################
########### GLM   - LM   ######################################################
###############################################################################
# ============================================================================
# 13. ENTRENAMIENTO DEL MODELO GLOBAL CLÁSICO
# ============================================================================
#
# Objetivo:
# Construir un único modelo global clásico utilizando toda la información
# disponible de la base de datos para su utilización posterior en el
# cálculo de H(i,S).
#
# Metodología:
# - Se selecciona una variable objetivo y un conjunto de variables
#   predictoras.
# - Se construye automáticamente una fórmula que incorpora todos los
#   predictores considerados.
# - Se ajusta un único modelo global utilizando la totalidad de las
#   observaciones disponibles.
# - El modelo se entrena una sola vez y permanece fijo durante todo
#   el procedimiento posterior.
#
# Casos considerados:
#
# Regresión:
# - Se ajusta un modelo lineal clásico.
#
# Clasificación:
# - Se ajusta un modelo logístico binario.
# - La variable objetivo se valida y adapta al formato requerido por
#   el modelo cuando es necesario.
#
# Interpretación:
# - El modelo obtenido representa la función predictiva global utilizada
#   durante todo el método.
# - Todas las evaluaciones posteriores utilizan exactamente el mismo
#   modelo entrenado.
#
# Relación con la matriz μ:
# - La matriz μ(v|S) no interviene en esta fase.
# - No se realizan cálculos asociados a coaliciones.
# - No se generan escenarios contrafactuales.
#
# Predicciones globales:
# - Una vez ajustado el modelo se calculan las predicciones asociadas a
#   todas las observaciones de la base de datos.
#
# Validación:
# - Se verifica la existencia de la variable objetivo.
# - Se verifica la existencia de las variables predictoras.
# - Se comprueba que el conjunto de predictores no esté vacío.
# - Se valida la compatibilidad de la variable objetivo con el tipo
#   de modelo seleccionado.
#
# Resultado:
# - Modelo global clásico entrenado.
# - Predicciones asociadas al modelo.
# - Variable objetivo considerada.
# - Conjunto de predictores utilizado.
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
# 14. CÁLCULO DE H(i,S) MEDIANTE μ(v|S) Y UN MODELO GLOBAL CLÁSICO
# ============================================================================
#
# Objetivo:
# Calcular el valor H(i,S) asociado a una instancia i y a una coalición S
# utilizando un modelo global clásico y los valores μ(v|S)
# previamente disponibles.
#
# Metodología:
# - Se fija la coalición S utilizando los valores observados en la
#   instancia i.
# - Se identifican las variables no incluidas en S.
# - Para cada una de dichas variables se recupera el valor μ(v|S)
#   correspondiente.
# - Se generan todos los escenarios posibles de acierto y fallo para
#   las variables no incluidas en S.
# - En cada escenario:
#     · Las variables pertenecientes a S permanecen fijadas a los valores
#       observados en la instancia i.
#     · Las variables con acierto se fijan también al valor observado
#       en la instancia i.
#     · Las variables con fallo conservan la variabilidad original de
#       la base de datos.
# - Para cada escenario se construye la correspondiente submuestra
#   contrafactual.
# - Se obtienen las predicciones del modelo global para todas las filas
#   de la submuestra.
# - Se calcula la media de dichas predicciones.
#
# Ponderación:
# - A cada escenario se le asigna un peso construido a partir de los
#   valores μ(v|S) de las variables no incluidas en S.
# - Los pesos asociados a todos los escenarios deben sumar 1.
#
# Cálculo de H(i,S):
# - H(i,S) se obtiene como la suma ponderada de las medias de predicción
#   calculadas para todos los escenarios considerados.
#
# Caso particular:
# - Cuando S contiene todas las variables predictoras, todas las
#   variables permanecen fijadas a los valores observados en la
#   instancia i.
# - En este caso H(i,S) se obtiene como la media de las predicciones
#   generadas por el modelo global sobre la submuestra completamente
#   fijada.
#
# Validación:
# - Se comprueba que la instancia solicitada existe en la base de datos.
# - Se verifica la obtención de todos los valores μ(v|S) necesarios.
# - Se comprueba que la suma de los pesos de los escenarios sea igual a 1.
#
# Resultado:
# - Valor H(i,S) asociado a la instancia i y a la coalición S.
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
# 15. CÁLCULO DE H(i,S) Y T(i,S) PARA TODAS LAS INSTANCIAS Y COALICIONES
# ============================================================================
#
# Objetivo:
# Calcular los valores H(i,S) y T(i,S) para todas las instancias de la
# base de datos y para todas las coaliciones de variables consideradas.
#
# Metodología:
# - Se parte de una lista de coaliciones S previamente generada.
# - Para cada coalición S se calcula H(i,S) para todas las instancias
#   de la base de datos.
# - El cálculo de H(i,S) se realiza utilizando el modelo global clásico
#   y los valores μ(v|S) previamente disponibles.
#
# Cálculo del valor de referencia:
# - Se calcula el caso asociado a la coalición vacía S = ∅.
# - Este valor constituye la referencia común utilizada en todos los
#   cálculos posteriores.
# - El valor obtenido se denomina K.
#
# Cálculo de T(i,S):
# - Una vez obtenido H(i,S), se calcula:
#
#     T(i,S) = H(i,S) - K
#
# Interpretación:
# - H(i,S) representa el valor obtenido para la instancia i cuando la
#   información disponible viene determinada por la coalición S.
# - T(i,S) representa la diferencia respecto al escenario de referencia K.
#
# Incorporación del conjunto vacío:
# - El conjunto vacío no forma parte de la lista inicial de coaliciones.
# - Una vez finalizados los cálculos, se incorpora explícitamente:
#
#     H(i,∅) = K
#
#     T(i,∅) = 0
#
# para todas las instancias.
#
# Validación:
# - Se verifica la correcta obtención del valor K.
# - Se comprueba la coherencia de los cálculos realizados para todas
#   las instancias y coaliciones.
#
# Resultado:
# - Tabla H(i,S) para todas las instancias y coaliciones.
# - Tabla T(i,S) para todas las instancias y coaliciones.
# - Valor de referencia K asociado al conjunto vacío.
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
# 16. FUNCIONES DE DEPURACIÓN Y VALIDACIÓN
# ============================================================================
#
# Objetivo:
# Facilitar la inspección detallada del cálculo de H(i,S) para instancias
# y coaliciones específicas en el modelo clásico.
#
# Metodología:
# - Permiten ejecutar el cálculo de H(i,S) en modo de depuración.
# - Muestran los valores μ(v|S) utilizados durante el cálculo.
# - Muestran los escenarios generados, los pesos asociados y las
#   predicciones obtenidas para cada escenario.
#
# Interpretación:
# - Estas funciones permiten verificar y comprender paso a paso el
#   cálculo de H(i,S).
# - Su finalidad es exclusivamente diagnóstica.
# - No modifican resultados ni intervienen en los cálculos principales
#   del método.
#
# Resultado:
# - Información detallada para la validación y revisión del cálculo
#   de H(i,S).
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
# 17. EJECUCIÓN DEL MÉTODO
# ============================================================================
#
# Objetivo:
# Ejecutar de forma conjunta todas las fases del método para obtener los
# valores H(i,S) y T(i,S) a partir de una matriz μ(v|S) previamente
# calculada.
#
# Metodología:
# - Se determina el conjunto de variables predictoras que participarán
#   en el análisis.
# - Se ajusta un modelo global clásico utilizando dichas variables.
# - Se generan las coaliciones de variables predictoras consideradas
#   en el estudio.
# - Se calculan los valores H(i,S) para todas las instancias y
#   coaliciones.
# - A partir de H(i,S) y del valor de referencia K se calculan los
#   valores T(i,S).
#
# Depuración opcional:
# - El procedimiento permite ejecutar funciones de depuración para
#   inspeccionar cálculos específicos sin modificar los resultados
#   finales obtenidos.
#
# Interpretación:
# - Este bloque coordina la ejecución completa del método.
# - Integra la información contenida en μ(v|S), las coaliciones
#   generadas y el modelo global clásico para obtener los resultados
#   finales.
#
# Resultado:
# - Tabla H(i,S).
# - Tabla T(i,S).
# - Valor de referencia K.
# - Coaliciones consideradas.
# - Modelo global utilizado en el análisis.
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
# 18. EJECUCIÓN DEL MÉTODO Y EXPORTACIÓN DE RESULTADOS
# ============================================================================
#
# Objetivo:
# Ejecutar el método completo para los problemas de regresión y
# clasificación, obtener las tablas H(i,S) y T(i,S), realizar
# comprobaciones de depuración y exportar los resultados generados.
#
# Metodología:
# - Se define la configuración de cada caso de estudio, incluyendo la
#   base de datos, la matriz μ(v|S), la variable objetivo y el tipo
#   de problema considerado.
# - Para cada configuración se ejecuta el método completo mediante el
#   procedimiento definido previamente.
# - Se obtienen las tablas H(i,S), T(i,S) y el valor de referencia K.
# - Opcionalmente se generan salidas de depuración para revisar el
#   comportamiento del método en instancias y coaliciones específicas.
# - Los resultados obtenidos se organizan y almacenan de forma
#   independiente para cada caso analizado.
#
# Casos considerados:
#
# Regresión:
# - Variable objetivo continua.
# - Modelo lineal clásico.
#
# Clasificación:
# - Variable objetivo binaria.
# - Modelo logístico binario.
#
# Interpretación:
# - Este paso aplica el método completo a los conjuntos de datos
#   considerados.
# - Permite obtener los resultados finales necesarios para su análisis,
#   comparación e interpretación.
#
# Resultado:
# - Tablas H(i,S) para cada caso de estudio.
# - Tablas T(i,S) para cada caso de estudio.
# - Valor K asociado a cada análisis.
# - Información de depuración para validación del procedimiento.
# - Exportación de los resultados finales.
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
  path = "C:/Users/.../M4_Delta_lm_y_stream.xlsx"
)
write_xlsx(
  x = M4_Pred_lm_y_stream,
  path = "C:/Users/.../M4_Pred_lm_y_stream.xlsx"
)

write_xlsx(
  x = M4_Delta_glm_yb_stream,
  path = "C:/Users/.../M4_Delta_glm_yb_stream.xlsx"
)
write_xlsx(
  x = M4_Pred_glm_yb_stream,
  path = "C:/Users/.../M4_Pred_glm_yb_stream.xlsx"
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





