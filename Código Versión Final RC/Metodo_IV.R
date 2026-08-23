
#################################################################################
################       METODO 4.                        #########################
#################################################################################


###############################################################################
# B. INTEGRACIÓN: USAR matriz_mu PARA CALCULAR H(i,S) Y T(i,S) CON XGBOOST
#
# En este bloque se integra la información de dependencia contenida en la
# matriz μ con un modelo XGBoost global entrenado previamente.
#
# El objetivo es definir y calcular el operador H(i,S), que combina:
#  - Un modelo predictivo f(·) entrenado sobre todos los datos
#  - Un conjunto S de variables conocidas (fijadas a la instancia i)
#  - Un conjunto U = X \ S de variables no conocidas
#  - Probabilidades μ(v | S) que modelan la fiabilidad de fijar v
#
# Este bloque NO recalcula μ, NO reentrena modelos por subconjunto
# y NO altera resultados ya obtenidos. Únicamente integra piezas
# previamente calculadas de forma coherente.
###############################################################################

library(xgboost)
library(crayon)

###############################################################################
# B.0. EXTRAER MATRICES μ (YA CALCULADAS CON calcular_todo_Mu)
#
# En este bloque se extraen las matrices μ_j(S) previamente calculadas.
#
# Cada matriz μ contiene:
#  - Filas: coaliciones S de variables conocidas
#  - Columnas: variables objetivo v
#  - Valores: μ(v | S) ∈ [0,1], que representan la probabilidad de "acierto"
#    al fijar v cuando el conjunto conocido es S
#
# Se separan explícitamente los casos de:
#  - Regresión (target continuo y)
#  - Clasificación (target binario y_b)
#
# Este bloque es puramente de extracción y validación:
#  - NO recalcula μ
#  - NO modifica los valores
#  - Garantiza coherencia de dimensiones antes de la integración
###############################################################################

matriz_mu_y   <- res_Mu_y$matriz_mu    # para regresión (target = y)
matriz_mu_log <- res_Mu_log$matriz_mu  # para clasificación (target = y_b)

cat("\n===== MATRIZ μ_j(S) PARA REGRESIÓN (target = y) =====\n")
print(dim(matriz_mu_y))

cat("\n===== MATRIZ μ_j(S) PARA CLASIFICACIÓN (target = y_booleana) =====\n")
print(dim(matriz_mu_log))

###############################################################################
# B.1. FUNCIONES GENERALES XGBOOST (REGRESIÓN / CLASIFICACIÓN)
#
# Este bloque define funciones auxiliares para preparar los datos de entrada
# a un modelo XGBoost global de forma consistente y segura.
#
# Dado:
#  - Un data.frame df
#  - Una variable objetivo target
#  - Un conjunto de predictores vars_pred
#
# La función:
#  1) Valida la existencia del target y de los predictores
#  2) Extrae y separa (X, y)
#  3) Convierte X en una matriz numérica mediante model.matrix
#  4) Devuelve una estructura homogénea compatible con XGBoost
#
# Esta función NO entrena modelos.
# Su único objetivo es garantizar que todas las predicciones posteriores
# se realizan sobre matrices X alineadas y coherentes.
###############################################################################
###############################################################################
# preparar_xy_global_XGB()
#
# Función auxiliar para preparar los datos de entrada de un modelo XGBoost.
#
# Entrada:
#  - df: data.frame con los datos originales
#  - target: nombre de la variable objetivo
#  - vars_pred: vector de nombres de variables predictoras
#
# Salida:
#  - X_mat: matriz numérica de predictores (one-hot si es necesario)
#  - y: vector de la variable objetivo
#  - vars_pred: predictores utilizados (para trazabilidad)
#
# Esta función garantiza que:
#  - No falten columnas
#  - No se entrene con predictores vacíos
#  - La matriz X sea válida para XGBoost
###############################################################################

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



################################################################################
# Valida columnas y construye (X,y) globales en formato consistente para XGBoost.
# Entrena un ÚNICO modelo XGBoost global y calcula la predicción media global.
#
# Este bloque define el modelo predictivo f(·) que se utilizará posteriormente
# en el operador H(i,S). El modelo se entrena UNA SOLA VEZ sobre todos los datos
# y se mantiene fijo durante todo el método.
#
# Es importante destacar que:
#  - NO se entrena un modelo por subconjunto S
#  - NO se entrena un modelo por escenario
#  - NO se utiliza μ en este punto
#
# La matriz μ se integrará más adelante, exclusivamente a través del operador H.
################################################################################
################################################################################
# ajustar_xgb_global_Mu()
#
# Esta función entrena un modelo XGBoost global f(·) a partir del dataset completo
# y devuelve todos los elementos necesarios para su uso posterior en H(i,S).
#
# Entrada:
#  - df: data.frame con los datos originales
#  - target: variable objetivo
#  - tipo: "regresion" o "clasificacion"
#  - vars_pred: conjunto de variables predictoras
#
# Salida:
#  - modelo: objeto xgboost entrenado
#  - pred: predicciones f(X) sobre el dataset completo
#  - K_aux: media global de las predicciones (solo sanity-check)
#  - feature_names: nombres exactos de las columnas usadas por el modelo
#
# IMPORTANTE:
#  K_aux NO se utiliza como definición formal de H(vacío).
#  El valor H(vacío) se calcula posteriormente aplicando el MISMO operador H
#  con S = ∅ y la matriz μ, garantizando coherencia metodológica.
################################################################################
################################################################################
# Normalización del target según el tipo de modelo:
#
# - En clasificación:
#     * lógicos → 0/1
#     * factores de dos niveles → 0/1
#     * numéricos → se usan directamente
#
# - En regresión:
#     * el target se convierte a numérico
#
# Esto garantiza que el modelo XGBoost reciba siempre un vector de etiquetas
# compatible con la función objetivo especificada.
################################################################################
################################################################################
# K_aux = E_D[f(X)] (SANITY CHECK)
#
# Se calcula la media global de las predicciones del modelo entrenado.
#
# Este valor:
#  - NO forma parte de la definición formal del método
#  - NO sustituye a H(vacío)
#  - NO se utiliza en el cálculo de H(i,S)
#
# Su única función es servir como comprobación auxiliar para verificar que:
#  - El modelo está entrenado correctamente
#  - Las predicciones tienen magnitud y signo razonables
#
# El valor H(vacío) se calcula posteriormente aplicando H(i,S) con S = ∅,
# utilizando explícitamente la matriz μ y los escenarios de fallo.
################################################################################

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


################################################################################
# preparar_X_sub_para_global_Mu()
#
# Esta función prepara matrices X de submuestras contrafactuales para poder
# predecir con el modelo XGBoost global ya entrenado.
#
# Dado un dataset contrafactual df_sub:
#  - Se construye su matriz de diseño mediante model.matrix
#  - Se alinea exactamente con las columnas usadas en el modelo global
#  - Las columnas ausentes se rellenan con ceros
#
# Esto garantiza que:
#  - Todas las predicciones f(X) se realizan en el MISMO espacio de features
#  - No hay incoherencias entre escenarios
#  - No se reentrena ningún modelo
#
# Esta función es esencial para poder integrar f(·) sobre escenarios en H(i,S).
################################################################################


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

################################################################################
# prepara matrices X/y, entrena un modelo global fijo y convierte/alinea
# submuestras para poder predecir con ese mismo modelo de forma consistente.
#
# Este bloque define el modelo predictivo f(·) que se utilizará en todo el
# Método 4. El modelo se entrena UNA SOLA VEZ sobre el dataset completo y se
# mantiene fijo durante todo el proceso.
#
# En este punto:
#  - NO se calcula H(i,S)
#  - NO se utiliza la matriz μ
#  - NO se generan escenarios
#
# Este bloque únicamente fija el modelo f(·) y el espacio de features sobre
# el que se integrarán posteriormente los escenarios definidos por μ.
################################################################################
################################################################################
# ajustar_xgb_global_Mu()
#
# Esta función entrena un modelo XGBoost global f(·) a partir del dataset
# completo y devuelve todos los elementos necesarios para su uso posterior
# en el operador H(i,S).
#
# El modelo entrenado:
#  - Es ÚNICO
#  - No depende del conjunto S
#  - No depende de μ
#  - No se reentrena para subconjuntos ni escenarios
#
# Entrada:
#  - df: data.frame con los datos originales
#  - target: variable objetivo
#  - tipo: "regresion" o "clasificacion"
#  - vars_pred: conjunto de variables predictoras
#
# Salida:
#  - modelo: objeto xgboost entrenado
#  - pred: predicciones f(X) sobre el dataset completo
#  - K_aux: media global de f(X), solo a efectos de comprobación
#  - feature_names: nombres exactos de las columnas utilizadas por el modelo
################################################################################





################################################################################
# Prepara y ALINEA las matrices X de submuestras contrafactuales para poder
# predecir con un modelo XGBoost global YA ENTRENADO.
# 
# - NO entrena ningún modelo.
# - Garantiza que las submuestras tengan exactamente las mismas columnas
#   (feature_names) y en el mismo orden que el modelo global.
################################################################################




###############################################################################
# prepara matrices X/y, entrena un modelo global fijo y convierte/alinea submuestras 
# para poder predecir con ese mismo modelo de forma consistente.
###############################################################################


###############################################################################
# B.2. μ_v(S) DESDE matriz_mu
###############################################################################
###############################################################################
# Dado un conjunto de variables ya conocidas S
# y una variable objetivo v,
# devuelve el valor μ(v | S) almacenado en la matriz μ.
#
# - matriz_mu: matriz donde las filas representan coaliciones S
#   (por ejemplo "empty", "Age", "Age+Sex") y las columnas variables objetivo v.
# - S_known: vector con las variables conocidas (conjunto S).
# - v: variable objetivo cuyo μ(v | S) se quiere recuperar.
#
# La función construye la clave asociada a S y accede a la celda correspondiente
# de matriz_mu para obtener la mejora relativa μ(v | S) frente al modelo vacío.
###############################################################################

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

###############################################################################
# B.3. H(i,S) CON μ Y XGBOOST
###############################################################################
###############################################################################
# Calcula el valor H(i,S) para una instancia fija i y un conjunto de variables
# conocidas S, integrando un modelo XGBoost global sobre escenarios ponderados
# por probabilidades μ(v | S).
#
# Sea:
# - f(·): modelo XGBoost global entrenado y fijo.
# - S: conjunto de variables conocidas (fijadas al valor de la instancia i).
# - U: conjunto de variables no conocidas (U = X \ S).
#
# Para cada variable v ∈ U se dispone de una probabilidad μ(v | S), que representa
# la probabilidad de "acierto" al fijar v al valor observado en la instancia i.
#
# El método genera explícitamente todos los escenarios binarios posibles sobre U
# (2^|U| combinaciones). En cada escenario:
#
# - Las variables de S se fijan al valor observado en la instancia i.
# - Las variables de U con acierto se fijan al valor de la instancia i.
# - Las variables de U con fallo se dejan libres (distribución empírica del dataset).
#
# Para cada escenario s:
# - Se calcula la predicción media m_s = E_D[f(X) | escenario s].
# - Se calcula un peso w_s como el producto de:
#     μ(v | S)      si v acierta en s
#     1 − μ(v | S)  si v falla en s
#
# Finalmente, H(i,S) se define como la esperanza ponderada:
#
#     H(i,S) = Σ_s w_s · m_s
#
# donde la suma recorre todos los escenarios posibles y, bajo independencia,
# Σ_s w_s = 1.
###############################################################################

# ============================================================
# CONSTRUCCIÓN DE SUBMUESTRA CONTRAFACTUAL (MISMO MÉTODO QUE M1)
# ============================================================

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
###############################################################################
# B.3bis. H(i,S) METODOLÓGICO PURO (SIN ATAJOS, SOLO PARA VALIDACIÓN)
###############################################################################

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
  
  # --------------------------------------------------
  # CONSTRUCCIÓN CORRECTA DE ESCENARIOS
  # {0,1}^0 = {∅}
  # --------------------------------------------------
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


###############################################################################
# B.4. GENERAR LISTA DE COALICIONES S DE PREDICTORES
###############################################################################
###############################################################################
# generar_S_list() construye la lista completa de subconjuntos no vacíos S de un
# conjunto de variables predictoras, hasta un tamaño máximo especificado.
#
# Dado un conjunto de variables predictoras vars_pred = {v1, v2, ..., vp},
# la función genera explícitamente todos los subconjuntos S ⊆ vars_pred tales que
# 1 ≤ |S| ≤ max_size, utilizando combinatoria directa.
#
# Cada elemento de la lista devuelta es un vector que representa un conjunto S
# concreto de variables conocidas. Estos subconjuntos S se utilizarán
# posteriormente como niveles de información conocida para evaluar cantidades
# dependientes de S, como H(i,S), μ(v | S) u otros cálculos basados en la
# enumeración sistemática de predictores.
#
# El conjunto vacío no se incluye, ya que el caso S = ∅ se trata de forma separada
# mediante el valor base K (predicción media global).
###############################################################################


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


###############################################################################
# B.5. CALCULAR H(i,S) Y T(i,S) PARA TODO i Y TODOS LOS S
###############################################################################

###############################################################################
# calcular_H_T_para_Sets_matriz() calcula, para todas las instancias i del dataset
# y para todos los subconjuntos de variables S contenidos en S_list, los valores
# H(i,S) y T(i,S).
#
# Para cada conjunto S ∈ S_list, la función evalúa H(i,S) para cada instancia i
# utilizando la función H_instancia_S_matriz(), que integra el modelo XGBoost
# global sobre todos los escenarios definidos por las variables no conocidas
# U = X \ S.
#
# A partir de los valores H(i,S), se calcula la cantidad:
#
#     T(i,S) = H(i,S) − K
#
# donde K es la predicción media global del modelo, equivalente al caso del
# conjunto vacío S = ∅.
#
# Los resultados se organizan en dos tablas:
#
# - H_tabla: contiene, para cada instancia i, los valores H(i,S) asociados a cada
#   subconjunto S, junto con el valor base K.
#
# - T_tabla: contiene, para cada instancia i, los valores T(i,S), que representan
#   la contribución marginal de conocer el conjunto S con respecto al nivel base K.
#
# El conjunto vacío no se incluye en S_list y se añade explícitamente al final,
# asignando H(i, ∅) = K y T(i, ∅) = 0 para todas las instancias.
#
# La función permite opcionalmente activar un modo de depuración para inspeccionar
# el cálculo de H(i,S) en detalle para una instancia y un subconjunto S concretos.
###############################################################################
# ============================================================
# Calcular K = H(i, ∅) usando el MISMO operador H con μ
# ============================================================

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



###############################################################################
# B.6. DEBUGS
###############################################################################

###############################################################################
# Este bloque define un conjunto de funciones auxiliares de depuración cuyo
# objetivo es inspeccionar, validar y comprender en detalle el cálculo de H(i,S)
# realizado por la función H_instancia_S_matriz().
#
# Las funciones permiten ejecutar el cálculo de H(i,S) en modo de depuración
# para:
#
# - Una instancia concreta y todos los subconjuntos S de predictores.
# - Varias instancias y todos los subconjuntos S.
# - Una combinación específica de instancia i y subconjunto S.
#
# Durante la ejecución en modo debug, se muestran de forma explícita los pasos
# internos del cálculo, incluyendo:
#   - El conjunto de variables conocidas S y no conocidas U.
#   - Los valores μ(v | S) utilizados.
#   - Los escenarios de acierto y fallo generados sobre U.
#   - Las tablas contrafactuales construidas para cada escenario.
#   - Las predicciones medias m_s y los pesos w_s asociados.
#
# Estas funciones no modifican resultados, no devuelven nuevos valores analíticos
# ni forman parte del pipeline principal de cálculo. Su finalidad es
# exclusivamente diagnóstica y explicativa, facilitando la validación conceptual
# y numérica del Método 4 y la comprensión detallada de cómo se construye el valor
# final H(i,S).
###############################################################################
###############################################################################
# DEBUG COMPLETO: VARIAS instancias, TODAS las S (SIEMPRE FULL)
###############################################################################



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



###############################################################################
# B.7. ENVOLTORIO PRINCIPAL: calcular_H_T_con_Mu (XGBoost)
###############################################################################

###############################################################################
# calcular_H_T_con_Mu() actúa como envoltorio principal del Método 4, coordinando
# todo el proceso de cálculo de H(i,S) y T(i,S) a partir de una matriz μ ya
# previamente calculada y un modelo XGBoost global.
#
# La función realiza las siguientes operaciones:
#
# 1) Determina el conjunto de variables predictoras que se utilizarán en el modelo
#    XGBoost (vars_XGB), validando su coherencia con las variables presentes en la
#    matriz μ y excluyendo explícitamente la variable objetivo.
#
# 2) Entrena un único modelo XGBoost global, fijo para todo el proceso, utilizando
#    las variables seleccionadas y el tipo de modelo especificado
#    (regresión o clasificación).
#
# 3) Genera la lista completa de subconjuntos no vacíos S de variables predictoras,
#    hasta un tamaño máximo especificado, que representarán los distintos niveles
#    de información conocida.
#
# 4) Calcula, para cada instancia i del dataset y para cada subconjunto S, los
#    valores H(i,S) y T(i,S)=H(i,S)−K mediante la función
#    calcular_H_T_para_Sets_matriz(), donde K es la predicción media global del
#    modelo.
#
# 5) Permite opcionalmente ejecutar el cálculo en modo de depuración para
#    inspeccionar en detalle el comportamiento de H(i,S) para una instancia y un
#    conjunto S concretos, sin afectar al resultado final.
#
# La función devuelve las tablas completas de H(i,S) y T(i,S), el valor base K,
# la lista de subconjuntos S considerados, las variables utilizadas en el modelo
# y el objeto del modelo XGBoost global entrenado.
###############################################################################

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
###############################################################################
# FUNCIÓN AUXILIAR: renombrar y reordenar columnas H y T (VERSIÓN CORRECTA)
###############################################################################

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




###############################################################################
# EJECUCIÓN CON BUCLE (REGRESIÓN Y CLASIFICACIÓN) + EXPORT DEBUG A TXT
###############################################################################

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
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/XGB_reg/M4_Pred_xgb_y_stream.xlsx"
)
write_xlsx(
  x = M4_Delta_xgb_y_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/XGB_reg/M4_Delta_xgb_y_stream.xlsx"
)
write_xlsx(
  x = M4_Pred_xgb_log_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/XGB_log/M4_Pred_xgb_log_stream.xlsx"
)
write_xlsx(
  x = M4_Delta_xgb_log_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/XGB_log/M4_Delta_xgb_log_stream.xlsx"
)


###################################################################################
###################################################################################

###############################################################################
########### GLM   - LM   ######################################################
###############################################################################

############################################################
# B.clásico.1 – MODELO GLOBAL (lm / glm) Y K = media(pred)
############################################################

###############################################################################
# ajustar_modelo_global_Mu_clasico() entrena un único modelo global clásico
# (regresión lineal o regresión logística) a partir del dataset proporcionado y
# calcula el valor base K como la media de las predicciones del modelo.
#
# Dado un conjunto de variables predictoras vars_pred y una variable objetivo
# target, la función construye una fórmula del tipo:
#
#     target ~ v1 + v2 + ... + vp
#
# y ajusta:
#   - un modelo lineal (lm) en el caso de regresión, o
#   - un modelo logístico binomial (glm con enlace logit) en el caso de
#     clasificación binaria.
#
# En el caso de clasificación, la función valida y normaliza el formato del
# target para garantizar que sea compatible con un modelo binomial, aceptando
# variables lógicas, factores de dos niveles o variables numéricas binarias.
#
#
# como la media global de dichas predicciones, que corresponde al caso del
# conjunto vacío S = ∅ (modelo sin información condicionada).
#
# La función devuelve el modelo entrenado, el vector de predicciones, el valor
# base K, el tipo de modelo utilizado, la variable objetivo y el conjunto de
# predictores empleados. Este modelo global se utiliza posteriormente como
# referencia fija para cálculos dependientes de μ, H(i,S) y T(i,S).
###############################################################################

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

###############################################################################
# ajustar_modelo_global_Mu_clasico() entrena un único modelo clásico global 
# (lm o glm binomial) usando los predictores especificados, valida y normaliza (factor)
# el formato del target, calcula las predicciones globales y define K como la media de
# dichas predicciones. Este modelo global y su baseline se utilizan posteriormente como 
# referencia fija para el cálculo de H(i,S) y T(i,S), sin reentrenar modelos por subconjunto.
###############################################################################


############################################################
# B.clásico.2 – Obtener μ_v(S) desde matriz_mu
############################################################

################################################################################
# get_mu_matriz() devuelve el valor μ(v | S) a partir de una matriz μ previamente
# calculada.
#
# Dado un conjunto de variables conocidas S y una variable objetivo v, la función
# construye una representación canónica del conjunto S y accede a la celda
# correspondiente de la matriz μ, donde las filas representan coaliciones S y las
# columnas representan variables v.
#
# El caso del conjunto vacío S = ∅ se representa mediante la clave "empty".
# La función valida que la variable v exista en la matriz μ y gestiona de forma
# segura situaciones en las que la coalición S no esté presente o el valor μ sea
# NA, devolviendo en dichos casos un valor nulo.
#
# El valor devuelto μ(v | S) se utiliza posteriormente como peso probabilístico en
# cálculos contrafactuales y de integración, sin intervenir directamente en el
# ajuste del modelo.
################################################################################

################################################################################
# get_mu_matriz() devuelve el valor μ(v∣S) desde una matriz μ, construyendo una
# representación canónica del conjunto S, validando entradas 
# de forma robusta y segura para su uso en cálculos contrafactuales.
################################################################################


############################################################
# B.clásico.3 – H(i,S) con Mu y modelo clásico (lm / glm)
############################################################

################################################################################
# H_instancia_S_matriz_clasico() calcula H(i,S) para una instancia i y un conjunto
# de variables conocidas S utilizando un modelo global clásico (lm o glm) fijo.
#
# La función define U como el conjunto de variables no conocidas, genera todos los
# escenarios posibles de fijación y no fijación sobre U y, para cada escenario,
# construye un dataset contrafactual en el que las variables de S (y aquellas de U
# con acierto) se fijan al valor observado en la instancia i.
#
# Para cada escenario s, se obtiene la predicción media m_s del modelo clásico y se
# calcula un peso w_s a partir de los valores μ(v | S). El valor final H(i,S) se
# obtiene como la suma ponderada:
#
#     H(i,S) = Σ_s w_s · m_s
#
# donde los pesos w_s verifican Σ_s w_s = 1.
################################################################################


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

################################################################################
# H_instancia_S_matriz_clasico() calcula H(i,S) para una instancia i y un conjunto 
# de variables conocidas SSS usando un modelo global clásico (lm/glm) fijo.
# Define U como las variables no conocidas, genera los escenarios de fijación/no fijación de U,
# construye en cada escenario un dataset contrafactual donde S (y parte de U) se fija al valor de i,
# predice con el modelo global y toma la media de predicciones ms. Cada escenario se pondera
# con ws=μv(S) o 1−μv(S) y se agrega devolviendo H(i,S)=mean(ws ms)H(i,S)
################################################################################

############################################################
# B.clásico.4 – Generar lista de coaliciones S de predictores
############################################################
###############################################################################
# generar_S_list() genera todas las coaliciones no vacías de un conjunto de
# variables predictoras, hasta un tamaño máximo opcional.
#
# Dado un conjunto de predictores vars_pred = {v1, v2, ..., vp}, la función
# construye explícitamente todos los subconjuntos S ⊆ vars_pred tales que
# 1 ≤ |S| ≤ max_size, utilizando combinatoria directa.
#
# Cada subconjunto S representa un conjunto de variables conocidas que se
# utilizará posteriormente para calcular H(i,S) y T(i,S) en el método clásico.
#
# El conjunto vacío no se incluye, ya que el caso S = ∅ se trata de forma
# separada mediante el valor base K (predicción media global del modelo).
###############################################################################


###############################################################################
# generar_S_list() genera todas las coaliciones no vacías de las variables predictoras, 
# hasta un tamaño máximo opcional, para definir los conjuntos S sobre los que se calcularán
# H(i,S) y T(i,S)
##############################################################################

############################################################
# B.clásico.5 – H(i,S) y T(i,S) para todo i y todos los S
############################################################
###############################################################################
# calcular_H_T_para_Sets_matriz_clasico() calcula los valores H(i,S) y T(i,S) para
# todas las instancias i del dataset y para todos los subconjuntos de variables S
# proporcionados en S_list, utilizando un único modelo clásico global (lm o glm).
#
# Para cada conjunto S ∈ S_list, la función:
#
# 1) Evalúa H(i,S) para cada instancia i mediante la función
#    H_instancia_S_matriz_clasico(), que integra el modelo global sobre todos los
#    escenarios de fijación y no fijación definidos por las variables no conocidas
#    U = X \ S, ponderados por μ(v | S).
#
# 2) Calcula la cantidad T(i,S) como:
#
#        T(i,S) = H(i,S) − K
#
#    donde K es la predicción media global del modelo, equivalente al caso del
#    conjunto vacío S = ∅.
#
# Los resultados se organizan en dos tablas:
#
# - H_tabla: contiene, para cada instancia i, los valores H(i,S) asociados a cada
#   subconjunto S, así como el valor base K.
#
# - T_tabla: contiene, para cada instancia i, los valores T(i,S), que representan
#   la contribución marginal de conocer el conjunto S con respecto al nivel base K.
#
# El conjunto vacío no se incluye en S_list y se añade explícitamente al final,
# asignando H(i, ∅) = K y T(i, ∅) = 0 para todas las instancias.
#
# La función permite opcionalmente activar un modo de depuración para inspeccionar
# en detalle el cálculo de H(i,S) para una instancia y un subconjunto S concretos,
# sin afectar al resultado final.
###############################################################################
# ============================================================
# K = H(i, ∅) usando operador H clásico con μ
# ============================================================

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

################################################################################
# calcular_H_T_para_Sets_matriz_clasico() calcula H(i,S) y T(i,S) para todas las instancias 
# y todos los subconjuntos de predictores SSS, aplicando un único modelo clásico global 
# (lm/glm) a contrafactuales construidos por fijación de variables y ponderados mediante μ(v|S).
# Devuelve las tablas completas de H y T, incluyendo el caso del conjunto vacío como referencia.
################################################################################


############################################################
# B.clásico.6 – Debugs
############################################################
###############################################################################
# Este bloque define un conjunto de funciones auxiliares de depuración para el
# método clásico (lm / glm), cuyo objetivo es inspeccionar y validar en detalle
# el cálculo de H(i,S) realizado por la función H_instancia_S_matriz_clasico().
#
# Las funciones permiten ejecutar el cálculo de H(i,S) en modo de depuración para:
#
# - Una instancia concreta y todos los subconjuntos S de predictores.
# - Varias instancias y todos los subconjuntos S.
# - Una combinación específica de instancia i y subconjunto S.
#
# Durante la ejecución en modo debug, se muestran de forma explícita los pasos
# internos del cálculo, incluyendo:
#   - El conjunto de variables conocidas S y no conocidas U.
#   - Los valores μ(v | S) utilizados.
#   - Los escenarios de acierto y fallo generados sobre U.
#   - Las tablas contrafactuales construidas para cada escenario.
#   - Las predicciones medias m_s y los pesos w_s asociados.
#
# Estas funciones no modifican resultados, no devuelven nuevos valores analíticos
# ni forman parte del pipeline principal de cálculo. Su finalidad es
# exclusivamente diagnóstica y explicativa, permitiendo verificar paso a paso
# el comportamiento del método clásico y asegurar la coherencia entre teoría,
# código y resultados numéricos.
###############################################################################

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

############################################################
# B.clásico.7 – ENVOLTORIO PRINCIPAL (lm / glm + μ)
############################################################

################################################################################
# calcular_H_T_con_Mu_clasico() actúa como envoltorio principal del método clásico,
# coordinando el cálculo de H(i,S) y T(i,S) a partir de una matriz μ previamente
# calculada y un único modelo global clásico (lm o glm).
#
# La función realiza las siguientes operaciones:
#
# 1) Determina el conjunto de variables predictoras que se utilizarán en el modelo
#    clásico, validando su coherencia con las variables presentes en la matriz μ y
#    excluyendo explícitamente la variable objetivo.
#
# 2) Ajusta un único modelo global clásico (regresión lineal o regresión logística,
#    según el tipo especificado), que se mantiene fijo durante todo el proceso.
#
# 3) Genera la lista completa de subconjuntos no vacíos S de las variables
#    predictoras, hasta un tamaño máximo opcional, que representan los distintos
#    niveles de información conocida.
#
# 4) Calcula, para cada instancia i del dataset y para cada subconjunto S, los
#    valores H(i,S) y T(i,S)=H(i,S)−K mediante la función
#    calcular_H_T_para_Sets_matriz_clasico(), donde K es la predicción media global
#    del modelo clásico.
#
# 5) Permite opcionalmente ejecutar el cálculo en modo de depuración para inspeccionar
#    en detalle el comportamiento de H(i,S) para una instancia y un conjunto S
#    concretos, sin afectar al resultado final.
#
# La función devuelve las tablas completas de H(i,S) y T(i,S), el valor base K,
# la lista de subconjuntos S considerados, las variables utilizadas como predictores
# y el objeto del modelo clásico global entrenado.
################################################################################

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

################################################################################
# calcular_H_T_con_Mu_clasico() entrena un único modelo clásico global (lm/glm) y, 
# para cada instancia y cada subconjunto de variables S, calcula las predicciones H(i,S)
# aplicando ese modelo a contrafactuales donde S se fija al valor observado y el 
# resto de variables se pondera mediante μ(v|S). A partir de ello obtiene T(i,S)=H(i,S)−K
# , donde K es la media global de predicciones.
################################################################################


############################################################
# EJECUCIÓN CON BUCLE – REGRESIÓN Y CLASIFICACIÓN (LM / GLM)
############################################################

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
  x = M1_Delta_lm_y_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/lm/M1_Delta_lm_y_stream.xlsx"
)
write_xlsx(
  x = M1_Pred_lm_y_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/lm/M1_Pred_lm_y_stream.xlsx"
)
write_xlsx(
  x = M4_Delta_lm_y_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/lm/M4_Delta_lm_y_stream.xlsx"
)
write_xlsx(
  x = M4_Pred_lm_y_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/lm/M4_Pred_lm_y_stream.xlsx"
)
write_xlsx(
  x = M1_Delta_glm_yb_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/glm/M1_Delta_glm_yb_stream.xlsx"
)
write_xlsx(
  x = M1_Pred_glm_yb_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/glm/M1_Pred_glm_yb_stream.xlsx"
)
write_xlsx(
  x = M4_Delta_glm_yb_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/glm/M4_Delta_glm_yb_stream.xlsx"
)
write_xlsx(
  x = M4_Pred_glm_yb_stream,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/glm/M4_Pred_glm_yb_stream.xlsx"
)
write_xlsx(
  x = matriz_mu,
  path = "C:/Users/danis/OneDrive/Escritorio/Phd/4.1. Escritura de Tesis/Real Case. Resultados/Datos V2/Mu.xlsx"
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





