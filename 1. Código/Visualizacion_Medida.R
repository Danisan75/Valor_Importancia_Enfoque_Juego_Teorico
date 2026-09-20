################################################################################
################################################################################
################ Representación bidimensional – Shapley por Posición ###########
################################################################################
################################################################################
# ============================================================================
# 1. NORMALIZACIÓN DE LA REPRESENTACIÓN DE LAS COALICIONES Δ(S)
# ============================================================================
#
# Objetivo:
# Homogeneizar la nomenclatura utilizada para representar las
# contribuciones marginales asociadas a las distintas coaliciones con el fin de
# garantizar una estructura consistente para los cálculos posteriores.
#
# Metodología:
# - Se analiza la estructura de nombres presente en la tabla de
#   contribuciones.
# - Se identifican las columnas que representan la variación de predicción y
#   se homogenizan las notaciones T(...) o T_S_*.
# - Cuando una coalición se encuentra representada mediante la forma
#   T_S_*, ésta se transforma automáticamente a la representación
#   estándar T(S)=Δ(S).
# - La coalición vacía se representa mediante la forma T().
# - No se modifica ningún valor originales.
# - Se identifican y almacenan todas las columnas asociadas a las
#   coaliciones disponibles.
#
# Interpretación:
# - La normalización permite trabajar con una representación única de
#   las coaliciones independientemente del formato utilizado durante
#   etapas previas del procedimiento y homogeneizar las representaciones
# gráficas.
# - Este proceso afecta únicamente a la nomenclatura y no modifica la
#   información contenida en la tabla.
#
# Validación:
# - Se verifica que el objeto de entrada sea una tabla de datos.
# - Se comprueba la existencia de columnas compatibles con la
#   representación de coaliciones.
# - Se garantiza la correcta identificación de la coalición vacía.
#
# Resultado:
# - Tabla variación de predicción T(S)=Δ(S) con nomenclatura normalizada.
# - Vector con los nombres de las columnas correspondientes a las
#   coaliciones disponibles.
#
# ============================================================================

# Archivos para representar la importancia por posición, presencia y ausencia 
# en función del método y modelo.

#M1_Delta_lm_y_stream - Método 1 modelo de regresión lineal
#M1_Delta_glm_yb_stream - Método 1 modelo de regresión logística
#M1_Delta_xgb_y_stream - Método 1 modelo XGBoost para regresión
#M1_Delta_xgb_yb_stream - Método 1 modelo XGBoost para clasificación
#M4_Delta_lm_y_stream - Método 4 modelo de regresión linea
#M4_Delta_glm_yb_stream - Método 4 modelo de regresión logística
#M4_Delta_xgb_y_stream - Método 4 modelo XGBoost para regresión
#M4_Pred_xgb_log_stream - Método 4 modelo XGBoost para clasificación

tabla_T_base <- M4_Delta_xgb_log_stream

# Archivos para representar la importancia por posición, presencia y ausencia 
# en función del método y modelo desde los archivos de resultados exportados al
# calcular el valo de Shapley


# "M1-Shapley-lm-y.xlsx" - Método 1 modelo de regresión lineal
# "M1-Shapley-glm-yb.xlsx" - Método 1 modelo de regresión logística
# "M1-Shapley-xgb-y.xlsx" - Método 1 modelo XGBoost para regresión
# "M1-Shapley-xgb-yb.xlsx" - Método 1 modelo XGBoost para clasificación
# "M4-Shapley-lm-y.xlsx" - Método 4 modelo de regresión lineal
# "M4-Shapley-glm-yb.xlsx" - Método 4 modelo de regresión logística
# "M4-Shapley-xgb-y.xlsx" - Método 4 modelo XGBoost para regresión
# "M4-Shapley-xgb-yb.xlsx" - Método 4 modelo XGBoost para clasificación



#library(readxl)
#tabla_T_base <- read_excel(
#  file.path(ruta_resultados, "M4-Shapley-xgb-yb.xlsx") # Ejemplo
#)
#tabla_T_base <- tabla_T_base[, 1:(ncol(tabla_T_base) - 5)]



stopifnot(is.data.frame(tabla_T_base))

normalizar_tabla_delta <- function(tabla) {
  
  stopifnot(is.data.frame(tabla))
  nms <- names(tabla)
  
  idx_T_par <- grepl("^T\\(", nms)
  idx_T_S   <- grepl("^T_S_", nms)
  
  if (!any(idx_T_par) && !any(idx_T_S)) {
    stop("La tabla no contiene columnas T(...) ni T_S_*.")
  }
  
  tabla_out <- tabla
  nuevos_nombres <- nms
  
  for (i in which(idx_T_S)) {
    if (nms[i] == "T_S_empty") {
      nuevos_nombres[i] <- "T()"
    } else {
      inside <- sub("^T_S_", "", nms[i])
      nuevos_nombres[i] <- paste0("T(", inside, ")")
    }
  }
  
  names(tabla_out) <- nuevos_nombres
  
  list(
    tabla_delta = tabla_out,
    delta_cols  = grep("^T\\(", names(tabla_out), value = TRUE)
  )
}

row_instancia_ordenes <- 1    # elijo la instancia 

delta_info <- normalizar_tabla_delta(tabla_T_base)
tabla_T <- delta_info$tabla_delta

tabla_T_instancia <- tabla_T[row_instancia_ordenes, , drop = FALSE]
stopifnot(nrow(tabla_T_instancia) == 1)

# ============================================================================
# 2. IMPORTANCIA POR POSICIÓN
# ============================================================================
#
# Objetivo:
# Cuantificar la contribución de una variable en función del número de variables
# presentes en el modelo antes que la variable analizada y su representación 
# gráfica en función de los j predecesores.
#
# Metodología:
# - Se reconstruyen automáticamente las coaliciones disponibles a partir
#   de la representación Δ(S)=T(S).
# - Se identifican las distintas combinaciones de variables que pueden
#   aparecer antes de la variable analizada.
# - Se calculan las contribuciones marginales de la variable para todas
#   las coaliciones en las que puede incorporarse.
# - Para cada orden j se calcula la contribución marginal media de la
#   variable considerando todas las combinaciones posibles de
#   predecesores en esa posición.
# - Finalmente se generan las correspondientes representaciones gráficas.
#
# Interpretación:
# - El orden j representa el número de variables presentes antes de
#   incorporar la variable analizada.
# - Cada valor Sh_A^j cuantifica la contribución marginal media de la
#   variable cuando el subconjunto A de variables ya está presente en
#   el modelo.
#
# Validación:
# - Se verifica la correcta reconstrucción de las coaliciones.
# - Se comprueba que las variables analizadas pertenezcan al conjunto
#   de variables disponible.
# - Se verifica la obtención de resultados para todos los órdenes
#   observables.
#
# Resultado:
# - Tablas de importancia por posición para todos los subconjuntos
#   analizados.
# - Representaciones gráficas de la evolución de Sh_A^j.
# - Base metodológica para los análisis posteriores de presencia y
#   ausencia de variables.
#
# ============================================================================

canonizar_coalicion <- function(S) sort(as.character(S))

reconstruir_combos_desde_T <- function(T_cols) {
  combos <- vector("list", length(T_cols))
  for (k in seq_along(T_cols)) {
    inside <- gsub("^T\\(|\\)$", "", T_cols[k])
    combos[[k]] <- if (inside == "") character(0)
    else canonizar_coalicion(strsplit(inside, "_")[[1]])
  }
  names(combos) <- T_cols
  combos
}


library(ggplot2)






calcular_shapley_ordenes <- function(
    tabla_T_instancia,
    A,
    T0 = 0) {
  

  
  T_cols <- grep("^T\\(", names(tabla_T_instancia), value = TRUE)
  
  T_vals <- as.numeric(
    tabla_T_instancia[1, T_cols, drop = TRUE]
  )
  
  names(T_vals) <- T_cols
  

  
  combos <- reconstruir_combos_desde_T(T_cols)
  
  key_S <- vapply(
    combos,
    function(S) paste(sort(S), collapse = "|"),
    character(1)
  )
  
  idx <- setNames(seq_along(key_S), key_S)
  

  
  jugadores <- sort(unique(unlist(combos)))
  
  n <- length(jugadores)
  
  if (!all(A %in% jugadores))
    stop("A contiene variables inexistentes")
  

  
  contrib_por_orden <- vector("list", n)
  

  
  for (k in seq_along(combos)) {
    
    S <- combos[[k]]
    
    j <- length(S)
    
    faltan <- setdiff(A, S)
    
    if (length(faltan) == 0)
      next
    
    key_S_actual <- paste(sort(S), collapse = "|")
    
    T_S <- if (j == 0) {
      T0
    } else {
      T_vals[idx[[key_S_actual]]]
    }
    
    for (g in faltan) {
      
      Sg <- sort(c(S, g))
      
      key_Sg <- paste(Sg, collapse = "|")
      
      if (!key_Sg %in% key_S)
        next
      
      T_Sg <- T_vals[idx[[key_Sg]]]
      
      contrib_por_orden[[j + 1]] <- c(
        contrib_por_orden[[j + 1]],
        T_Sg - T_S
      )
    }
  }
  

  
  Sh <- sapply(
    contrib_por_orden,
    function(x)
      if (length(x)) mean(x) else NA_real_
  )
  
  resultado <- data.frame(
    j = 0:(length(Sh) - 1),
    n_contrib = sapply(contrib_por_orden, length),
    Sh = Sh
  )
  
  resultado <- resultado[
    resultado$n_contrib > 0,
  ]
  
  rownames(resultado) <- NULL
  
  resultado
}




T_cols <- grep("^T\\(", names(tabla_T_instancia), value = TRUE)

combos_all <- lapply(T_cols, function(nm) {
  inside <- gsub("^T\\(|\\)$", "", nm)
  if (inside == "") character(0)
  else strsplit(inside, "_")[[1]]
})

vars <- sort(unique(unlist(combos_all)))

var_ids <- seq_along(vars)
names(var_ids) <- vars
# var_ids["Age"] -> 1



build_label_shapley_ids <- function(A_ids) {
  
  # plotmath REQUIERE list() cuando hay más de un elemento
  inside <- if (length(A_ids) == 1) {
    A_ids
  } else {
    paste0("list(", paste(A_ids, collapse = ","), ")")
  }
  
  paste0(
    "Sh[group('{',", inside, ",'}')]^j * ",
    "group('(', list(Delta), ')')"
  )
}


plot_sh_por_ordenes_A <- function(data, A, fila) {
  
  # Convertir nombres de variables a IDs numéricos
  A_ids <- var_ids[A]
  
  # Construir etiqueta matemática segura (solo IDs)
  etiqueta <- build_label_shapley_ids(A_ids)
  
  # Texto del título:
  #   Age_Race [1,2]
  nombre_titulo <- paste(A, collapse = "_")
  ids_titulo    <- paste(A_ids, collapse = ",")
  
  ggplot() +
    geom_point(
      data = data,
      aes(j, Sh),
      size = 4,
      color = "darkgreen",
      shape = 17
    ) +
    geom_line(
      data = data,
      aes(j, Sh),
      linewidth = 1.3,
      color = "darkgreen"
    ) +
    
    annotate(
      "text",
      x = max(data$j) + 0.3,
      y = max(data$Sh, na.rm = TRUE),
      label = etiqueta,
      parse = TRUE,
      color = "darkgreen",
      size = 4
    ) +
    
    labs(
      x = "Posición j",
      y = "Sh",
      title = paste0(
        "Shapley por órdenes (instancia fila = ",
        fila,
        ") para ",
        nombre_titulo,
        " [",
        ids_titulo,
        "]"
      )
    ) +
    
    scale_x_continuous(
      limits = c(-0.005, max(data$j) + 0.5)
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey65", linewidth = 0.7),
      panel.grid.minor = element_line(color = "grey80", linewidth = 0.4),
      axis.line.x.bottom = element_line(color = "black", linewidth = 1.2),
      axis.line.y.left   = element_line(color = "black", linewidth = 1.2),
      axis.line.x.top    = element_blank(),
      axis.line.y.right  = element_blank(),
      panel.border       = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 1.2),
      axis.ticks.length = unit(0.25, "cm"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
}


lista_A <- unlist(
  lapply(seq_along(vars),
         function(k) combn(vars, k, simplify = FALSE)),
  recursive = FALSE
)



lista_tablas <- list()
lista_plots  <- list()

for (A in lista_A) {
  
  nombre_A <- paste(A, collapse = "_")
  
  tabla_A <- calcular_shapley_ordenes(
    tabla_T_instancia = tabla_T_instancia,
    A = A,
    T0 = 0
  )
  
  lista_tablas[[nombre_A]] <- tabla_A
  
  p <- plot_sh_por_ordenes_A(
    data = tabla_A,
    A = A,
    fila = row_instancia_ordenes
  )
  
  lista_plots[[nombre_A]] <- p
  
  print(p)
}



################################################################################
############################## Presencia #######################################
################################################################################

# ============================================================================
# 3. IMPORTANCIA POR PRESENCIA
# ============================================================================
#
# Objetivo:
# Cuantificar la contribución de una variable o conjunto de variables cuando
# otra variable o conjunto de variables ya está presente previamente en el
# modelo y representar dicha contribución marginal en función del número de
# predecesores.
#
# Metodología:
# - Se reconstruyen automáticamente las coaliciones disponibles a partir
#   de la representación Δ(S)=T(S).
# - Se identifican las coaliciones que contienen el subconjunto de
#   presencia P, es decir, las variables que han sido incorporadas al
#   modelo antes que la variable o conjunto de variables analizado.
# - Sobre dichas coaliciones se calculan las contribuciones marginales
#   medias de la variable o subconjunto analizado A.
# - Las contribuciones se agrupan según el tamaño de la coalición
#   previa, definido como la posición j.
# - Para cada posición j se calcula la contribución marginal media
#   considerando únicamente las coaliciones que contienen P.
#
# Interpretación:
# - La posición j representa el número de variables presentes antes de
#   incorporar la variable o conjunto de variables analizado.
# - Cada valor Sh_{P→A}^j calculamos la contribución marginal media de A
#   cuando el subconjunto P ya forma parte de la coalición (variables presentes).
# - La comparación entre Sh_A^j y Sh_{P→A}^j permite evaluar cómo la
#   presencia de determinadas variables influye en la capacidad predictiva de otras.
#
# Validación:
# - Se verifica la correcta reconstrucción de las coaliciones.
# - Se comprueba que los subconjuntos A y P pertenezcan al conjunto de
#   variables disponible.
# - Se garantiza que A y P no compartan variables (errores de inconsistencia).
#
# Resultado:
# - Tablas de importancia por presencia para las distintas combinaciones 
# de variables analizadas.
# - Comparación entre la importancia por posición y la importancia por
#   presencia.
# - Representaciones gráficas de la importancia por posición y de la
#   importancia por presencia para todas las posibles combinaciones de A(S) y P(T).
#
# ============================================================================

calcular_shapley_presencia <- function(
    tabla_T_instancia,
    P,
    A,
    T0 = 0) {
  

  T_cols <- grep("^T\\(", names(tabla_T_instancia), value = TRUE)
  
  T_vals <- as.numeric(
    tabla_T_instancia[1, T_cols, drop = TRUE]
  )
  
  names(T_vals) <- T_cols
  

  combos <- reconstruir_combos_desde_T(T_cols)
  
  key_S <- vapply(
    combos,
    function(S) paste(sort(S), collapse = "|"),
    character(1)
  )
  
  idx <- setNames(seq_along(key_S), key_S)
  

  jugadores <- sort(unique(unlist(combos)))
  
  n <- length(jugadores)
  
  if (!all(P %in% jugadores))
    stop("P contiene variables inexistentes")
  
  if (!all(A %in% jugadores))
    stop("A contiene variables inexistentes")
  
  if (length(intersect(P, A)) > 0)
    stop("P y A no pueden intersectar")
  

  contrib_por_orden <- vector("list", n)

  
  for (k in seq_along(combos)) {
    
    S <- combos[[k]]
    
    # PRESENCIA
    if (!all(P %in% S))
      next
    
    j <- length(S)
    
    faltan <- setdiff(A, S)
    
    if (length(faltan) == 0)
      next
    
    key_S_actual <- paste(sort(S), collapse = "|")
    
    T_S <- if (j == 0) {
      T0
    } else {
      T_vals[idx[[key_S_actual]]]
    }
    
    for (g in faltan) {
      
      Sg <- sort(c(S, g))
      
      key_Sg <- paste(Sg, collapse = "|")
      
      if (!key_Sg %in% key_S)
        next
      
      T_Sg <- T_vals[idx[[key_Sg]]]
      
      contrib_por_orden[[j + 1]] <- c(
        contrib_por_orden[[j + 1]],
        T_Sg - T_S
      )
    }
  }
  

  Sh <- sapply(
    contrib_por_orden,
    function(x)
      if (length(x)) mean(x) else NA_real_
  )
  
  resultado <- data.frame(
    j = 0:(length(Sh) - 1),
    n_contrib = sapply(contrib_por_orden, length),
    Sh = Sh
  )
  
  resultado <- resultado[
    resultado$n_contrib > 0,
  ]
  
  rownames(resultado) <- NULL
  
  resultado
}








plot_sh_presencia <- function(tab_ord, tab_pres, A, P, fila) {
  
  # IDs numéricos
  A_ids <- var_ids[A]
  P_ids <- var_ids[P]
  
  # ---------------------------------------------------------------------------
  # 1. ETIQUETAS plotmath (MISMO FORMATO QUE EL ORIGINAL)
  # ---------------------------------------------------------------------------
  
  # ÓRDENES (verde)
  etiqueta_ordenes <- paste0(
    "Sh[group('{',",
    if (length(A_ids) == 1) A_ids else paste0("list(", paste(A_ids, collapse=","), ")"),
    ",'}')]^j * group('(', list(Delta), ')')"
  )
  
  # PRESENCIA (azul)
  etiqueta_presencia <- paste0(
    "Sh[P*group('{',",
    if (length(A_ids) == 1) A_ids else paste0("list(", paste(A_ids, collapse=","), ")"),
    ",'}')]^j * group('(', list(Delta, group('{',",
    if (length(P_ids) == 1) P_ids else paste0("list(", paste(P_ids, collapse=","), ")"),
    ",'}')), ')')"
  )
  
  # ---------------------------------------------------------------------------
  # 2. POSICIONES DE LAS ETIQUETAS (COMO ANTES)
  # ---------------------------------------------------------------------------
  yr <- range(c(tab_ord$Sh, tab_pres$Sh), na.rm = TRUE)
  dy <- 0.06 * diff(yr)
  if (!is.finite(dy) || dy == 0) dy <- 0.05
  
  x_text <- max(tab_ord$j) + 0.3
  y_top  <- max(yr)
  y_bot  <- max(yr) - dy
  
  # ---------------------------------------------------------------------------
  # 3. TÍTULO 
  # ---------------------------------------------------------------------------
  nombre_A <- paste(A, collapse = " + ")
  nombre_P <- paste(P, collapse = " + ")
  
  ggplot() +
    
    # Curva general (ÓRDENES)
    geom_point(data = tab_ord, aes(j, Sh),
               size = 4, color = "darkgreen", shape = 17) +
    geom_line (data = tab_ord, aes(j, Sh),
               linewidth = 1.3, color = "darkgreen") +
    
    # Curva PRESENCIA
    geom_point(data = tab_pres, aes(j, Sh),
               size = 4, color = "blue", na.rm = TRUE) +
    geom_line (data = tab_pres, aes(j, Sh),
               linewidth = 1.3, color = "blue", na.rm = TRUE) +
    
    # Etiqueta ÓRDENES
    annotate(
      "text",
      x = x_text,
      y = y_top,
      label = etiqueta_ordenes,
      parse = TRUE,
      color = "darkgreen",
      size = 4
    ) +
    
    # Etiqueta PRESENCIA (debajo)
    annotate(
      "text",
      x = x_text,
      y = y_bot,
      label = etiqueta_presencia,
      parse = TRUE,
      color = "blue",
      size = 4
    ) +
    
    labs(
      x = "Posición j",
      y = "Sh",
      title = paste0(
        "Influencia de ",
        nombre_P,
        " [", paste(P_ids, collapse=","), "]",
        " sobre ",
        nombre_A,
        " [", paste(A_ids, collapse=","), "]",
        " (instancia fila = ",
        fila,
        ")"
      )
    ) +
    
    scale_x_continuous(
      limits = c(-0.005, max(tab_ord$j) + 0.5)
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey65", linewidth = 0.7),
      panel.grid.minor = element_line(color = "grey80", linewidth = 0.4),
      axis.line.x.bottom = element_line(color = "black", linewidth = 1.2),
      axis.line.y.left   = element_line(color = "black", linewidth = 1.2),
      axis.line.x.top    = element_blank(),
      axis.line.y.right  = element_blank(),
      panel.border       = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 1.2),
      axis.ticks.length = unit(0.25, "cm"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
}




# 1 a 1

for (A in vars) {
  for (P in setdiff(vars, A)) {
    
    tab_A <- lista_tablas[[A]]
    tab_P <- calcular_shapley_presencia(tabla_T_instancia, P, A)
    
    print(plot_sh_presencia(tab_A, tab_P, A, P, row_instancia_ordenes))
  }
}

# Varios a 1

for (A in vars) {
  
  otros <- setdiff(vars, A)
  if (length(otros) < 2) next
  
  tab_A <- lista_tablas[[A]]
  
  for (k in 2:length(otros)) {
    for (P in combn(otros, k, simplify=FALSE)) {
      
      tab_P <- calcular_shapley_presencia(tabla_T_instancia, P, A)
      print(plot_sh_presencia(tab_A, tab_P, A, P, row_instancia_ordenes))
    }
  }
}

# 1 a varios



for (kA in 2:length(vars)) {
  
  for (A in combn(vars, kA, simplify = FALSE)) {
    
    key_A <- paste(A, collapse = "_")
    
    tab_A <- lista_tablas[[key_A]]
    
    for (P in setdiff(vars, A)) {
      
      tab_P <- calcular_shapley_presencia(
        tabla_T_instancia = tabla_T_instancia,
        P = P,
        A = A
      )
      
      print(
        plot_sh_presencia(
          tab_A,
          tab_P,
          A,
          P,
          row_instancia_ordenes
        )
      )
    }
  }
}



# varias a varias

for (kA in 2:(length(vars)-1)) {
  
  for (A in combn(vars, kA, simplify = FALSE)) {
    
    key_A <- paste(A, collapse = "_")
    
    tab_A <- lista_tablas[[key_A]]
    
    restantes <- setdiff(vars, A)
    
    if (length(restantes) < 2)
      next
    
    for (kP in 2:length(restantes)) {
      
      for (P in combn(restantes, kP, simplify = FALSE)) {
        
        tab_P <- calcular_shapley_presencia(
          tabla_T_instancia = tabla_T_instancia,
          P = P,
          A = A
        )
        
        print(
          plot_sh_presencia(
            tab_A,
            tab_P,
            A,
            P,
            row_instancia_ordenes
          )
        )
      }
    }
  }
}



################################################################################
################################################################################
############################ Ausencia ##########################################
################################################################################

# ============================================================================
# 4. IMPORTANCIA POR AUSENCIA
# ============================================================================
#
# Objetivo:
# Cuantificar la contribución de una variable o conjunto de variables cuando
# otra variable o conjunto de variables no está presente en el modelo y
# representar dicha contribución marginal en función del número de
# predecesores (j).
#
# Metodología:
# - Se reconstruyen automáticamente las coaliciones disponibles a partir
#   de la representación Δ(S)=T(S).
# - Se identifican las coaliciones que contienen los distintos subconjunto de
#   variables ausentes.
# - Sobre dichas coaliciones se calculan las contribuciones marginales
#   medias de la variable o conjunto de variables analizadas A, T en la metodología.
# - Las contribuciones se agrupan según el tamaño de la coalición
#   previa, definido como el posición j.
# - Para cada posición j se calcula la contribución media marginal
#   considerando la variable o variables ausente Q.
# - Finalmente se comparan los resultados obtenidos con la importancia
#   por posición y con la importancia condicionada por presencia.
#
# Interpretación:
# - La posición j representa el número de variables ausentes antes de
#   incorporar la variable o conjunto de variables analizado.
# - Cada valor Sh_{Q→A}^j cuantifica la contribución marginal media de la variable
#   analizada cuando una variabl o conjunto de variables está ausente.
# - La representación gráfica no permite analizarla influencia sobre una variable o
# conjunto de variables la ausencia de otra variable o conjunto de variables. 
#
# Validación:
# - Se verifica la correcta reconstrucción de las coaliciones.
#
# Resultado:
# - Tablas de importancia por ausencia para las distintas
#   combinaciones de variables analizadas.
# - Comparación entre importancia por posición, importancia por
#   presencia e importancia por ausencia.
# - Representaciones gráficas de la importancia por posición,
#   presencia y ausencia.
#
# ============================================================================


calcular_shapley_ausencia <- function(tabla_T_instancia, Q, A, T0 = 0){
  
  T_cols <- grep("^T\\(", names(tabla_T_instancia), value = TRUE)
  T_vals <- as.numeric(tabla_T_instancia[1, T_cols])
  names(T_vals) <- T_cols
  
  combos <- lapply(T_cols, function(nm) {
    inside <- gsub("^T\\(|\\)$", "", nm)
    if (inside == "") character(0)
    else sort(strsplit(inside, "_")[[1]])
  })
  
  key_S <- vapply(combos, function(S) paste(S, collapse="|"), character(1))
  idx   <- setNames(seq_along(key_S), key_S)
  
  jugadores <- sort(unique(unlist(combos)))
  
  if (!all(Q %in% jugadores)) stop("Error en Q")
  if (!all(A %in% jugadores)) stop("Error en A")
  if (length(intersect(Q, A)) > 0) stop("Q y A no pueden intersectar")
  
  n <- length(jugadores)
  
  contrib_por_orden <- vector("list", n)
  
  for (k in seq_along(combos)) {
    
    S <- combos[[k]]
    
    if (length(intersect(S, Q)) > 0)
      next
    
    j <- length(S)
    
    faltan <- setdiff(A, S)
    
    if (length(faltan) == 0)
      next
    
    key_S_actual <- paste(sort(S), collapse = "|")
    
    T_S <- if (j == 0) {
      T0
    } else {
      T_vals[idx[[key_S_actual]]]
    }
    
    for (g in faltan) {
      
      Sg <- sort(c(S, g))
      
      key_Sg <- paste(Sg, collapse = "|")
      
      if (!key_Sg %in% key_S)
        next
      
      T_Sg <- T_vals[idx[[key_Sg]]]
      
      contrib_por_orden[[j + 1]] <- c(
        contrib_por_orden[[j + 1]],
        T_Sg - T_S
      )
    }
  }  
  
  Sh <- sapply(
    contrib_por_orden,
    function(x)
      if (length(x)) mean(x) else NA_real_
  )
  
  resultado <- data.frame(
    j = 0:(length(Sh) - 1),
    n_contrib = sapply(contrib_por_orden, length),
    Sh = Sh
  )
  
  resultado <- resultado[
    resultado$n_contrib > 0,
  ]
  
  rownames(resultado) <- NULL
  
  resultado
}


build_group_ids <- function(ids) {
  if (length(ids) == 1) {
    paste0("group('{',", ids, ",'}')")
  } else {
    paste0("group('{', list(", paste(ids, collapse=","), "), '}')")
  }
}


# -----------------------------------------------------------------------------
# Crea un objeto textGrob compatible con plotmath (grid)
# -----------------------------------------------------------------------------

textGrob_math <- function(label, col, fontsize = 12,
                          x = 0.5, y = 0.5,
                          hjust = 0.5, vjust = 0.5) {
  grid::textGrob(
    as.expression(parse(text = label)),                 ### convierte string a expresión plotmath
    gp = grid::gpar(col = col, fontsize = fontsize),    ### estilo gráfico
    x = grid::unit(x, "npc"),                           ### posición horizontal (0-1)
    y = grid::unit(y, "npc"),                           ### posición vertical (0-1)
    hjust = hjust,                                      ### alineación horizontal
    vjust = vjust                                       ### alineación vertical
  )
}

plot_sh_presencia_ausencia <- function(tab_ord,
                                       tab_pres,
                                       tab_aus,
                                       A,
                                       P,
                                       fila) {
  
  # ---------------------------------------------------------------------------
  # IDs numéricos
  # ---------------------------------------------------------------------------
  A_ids <- var_ids[A]
  P_ids <- var_ids[P]
  
  # ---------------------------------------------------------------------------
  # ETIQUETAS plotmath
  # ---------------------------------------------------------------------------
  
  etiqueta_ordenes <- paste0(
    "Sh[group('{',",
    if (length(A_ids) == 1) A_ids else paste0("list(", paste(A_ids, collapse=","), ")"),
    ",'}')]^j * group('(', list(Delta), ')')"
  )
  
  etiqueta_presencia <- paste0(
    "Sh[P*group('{',",
    if (length(A_ids) == 1) A_ids else paste0("list(", paste(A_ids, collapse=","), ")"),
    ",'}')]^j * group('(', list(Delta, group('{',",
    if (length(P_ids) == 1) P_ids else paste0("list(", paste(P_ids, collapse=","), ")"),
    ",'}')), ')')"
  )
  
  etiqueta_ausencia <- paste0(
    "Sh[A*group('{',",
    if (length(A_ids) == 1) A_ids else paste0("list(", paste(A_ids, collapse=","), ")"),
    ",'}')]^j * group('(', list(Delta, group('{',",
    if (length(P_ids) == 1) P_ids else paste0("list(", paste(P_ids, collapse=","), ")"),
    ",'}')), ')')"
  )
  
  # ---------------------------------------------------------------------------
  # GRÁFICA BASE
  # ---------------------------------------------------------------------------
  nombre_A <- paste(A, collapse = " + ")
  nombre_P <- paste(P, collapse = " + ")
  
  p <- ggplot() +
    
    # ÓRDENES
    geom_point(data = tab_ord, aes(j, Sh),
               size = 4, color = "darkgreen", shape = 17) +
    geom_line (data = tab_ord, aes(j, Sh),
               linewidth = 1.3, color = "darkgreen") +
    
    # PRESENCIA
    geom_point(data = tab_pres, aes(j, Sh),
               size = 4, color = "blue", na.rm = TRUE) +
    geom_line (data = tab_pres, aes(j, Sh),
               linewidth = 1.3, color = "blue", na.rm = TRUE) +
    
    # AUSENCIA
    geom_point(data = tab_aus, aes(j, Sh),
               size = 4, color = "#FFD700", shape = 15, na.rm = TRUE) +
    geom_line (data = tab_aus, aes(j, Sh),
               linewidth = 1.3, color = "#FFD700", na.rm = TRUE)
  
  # ---------------------------------------------------------------------------
  # ETIQUETAS FUERA DEL PANEL (FORMATO AUSENCIA)
  # ---------------------------------------------------------------------------
  p <- p +
    annotation_custom(
      textGrob_math(etiqueta_ordenes, col = "darkgreen",
                    x = 0.98, y = 0.80, hjust = 1)
    ) +
    annotation_custom(
      textGrob_math(etiqueta_presencia, col = "blue",
                    x = 0.98, y = 0.68, hjust = 1)
    ) +
    annotation_custom(
      textGrob_math(etiqueta_ausencia, col = "#FFD700",
                    x = 0.98, y = 0.56, hjust = 1)
    )
  
  # ---------------------------------------------------------------------------
  # EJES Y TEMA (PROFESIONAL)
  # ---------------------------------------------------------------------------
  p +
    labs(
      x = "Posición j",
      y = "Sh",
      title = paste0(
        "Influencia de ",
        nombre_P, " [", paste(P_ids, collapse=","), "]",
        " sobre ",
        nombre_A, " [", paste(A_ids, collapse=","), "]",
        " (instancia fila = ", fila, ")"
      )
    ) +
    scale_x_continuous(
      limits = c(-0.005, max(tab_ord$j) + 0.5),
      breaks = seq(min(tab_ord$j), max(tab_ord$j), by = 1)
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 13) +
    theme(
      plot.margin = margin(10, 40, 10, 10),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey65", linewidth = 0.7),
      panel.grid.minor = element_line(color = "grey80", linewidth = 0.4),
      axis.line.x.bottom = element_line(color = "black", linewidth = 1.2),
      axis.line.y.left   = element_line(color = "black", linewidth = 1.2),
      axis.line.x.top    = element_blank(),
      axis.line.y.right  = element_blank(),
      panel.border       = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 1.2),
      axis.ticks.length = unit(0.25, "cm"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
}



###############################################################################
# 1 → 1
###############################################################################

for (A in vars) {
  for (P in setdiff(vars, A)) {
    
    tab_A <- lista_tablas[[A]]
    tab_P <- calcular_shapley_presencia(tabla_T_instancia, P, A)
    tab_Q <- calcular_shapley_ausencia(tabla_T_instancia, P, A)
    
    print(
      plot_sh_presencia_ausencia(
        tab_ord = tab_A,
        tab_pres = tab_P,
        tab_aus  = tab_Q,
        A = A,
        P = P,
        fila = row_instancia_ordenes
      )
    )
  }
}

###############################################################################
# VARIOS → 1
###############################################################################

for (A in vars) {
  
  otros <- setdiff(vars, A)
  if (length(otros) < 2) next
  
  tab_A <- lista_tablas[[A]]
  
  for (kP in 2:length(otros)) {
    for (P in combn(otros, kP, simplify = FALSE)) {
      
      tab_P <- calcular_shapley_presencia(tabla_T_instancia, P, A)
      tab_Q <- calcular_shapley_ausencia(tabla_T_instancia, P, A)
      
      print(
        plot_sh_presencia_ausencia(
          tab_ord = tab_A,
          tab_pres = tab_P,
          tab_aus  = tab_Q,
          A = A,
          P = P,
          fila = row_instancia_ordenes
        )
      )
    }
  }
}

###############################################################################
# 1 → VARIOS
###############################################################################

for (kA in 2:length(vars)) {
  for (A in combn(vars, kA, simplify = FALSE)) {
    
    key_A <- paste(A, collapse = "_")
    tab_A <- lista_tablas[[key_A]]
    
    for (P in setdiff(vars, A)) {
      
      tab_P <- calcular_shapley_presencia(tabla_T_instancia, P, A)
      tab_Q <- calcular_shapley_ausencia(tabla_T_instancia, P, A)
      
      print(
        plot_sh_presencia_ausencia(
          tab_ord = tab_A,
          tab_pres = tab_P,
          tab_aus  = tab_Q,
          A = A,
          P = P,
          fila = row_instancia_ordenes
        )
      )
    }
  }
}

###############################################################################
# VARIOS → VARIOS
###############################################################################

for (kA in 2:(length(vars) - 1)) {
  for (A in combn(vars, kA, simplify = FALSE)) {
    
    key_A <- paste(A, collapse = "_")
    tab_A <- lista_tablas[[key_A]]
    
    restantes <- setdiff(vars, A)
    if (length(restantes) < 2) next
    
    for (kP in 2:length(restantes)) {
      for (P in combn(restantes, kP, simplify = FALSE)) {
        
        tab_P <- calcular_shapley_presencia(tabla_T_instancia, P, A)
        tab_Q <- calcular_shapley_ausencia(tabla_T_instancia, P, A)
        
        print(
          plot_sh_presencia_ausencia(
            tab_ord = tab_A,
            tab_pres = tab_P,
            tab_aus  = tab_Q,
            A = A,
            P = P,
            fila = row_instancia_ordenes
          )
        )
      }
    }
  }
}




################################################################################
################################################################################
####################### Exportar gráficas a pdf ################################
################################################################################
################################################################################

# ============================================================================
# 5. ORGANIZACIÓN Y EXPORTACIÓN DE LOS RESULTADOS GRÁFICOS
# ============================================================================
#
# Objetivo:
# Generar un documento estructurado en pdf de los resultados
# obtenidos en la importancia por posición, presencia y
# ausencia, facilitando su revisión, interpretación y posterior
# utilización en distintos documentos o análisis.
#
# Motivación:
# - Los procedimientos anteriores generan un número muy
#   elevado de representaciones gráficas.
# - La interpretación conjunta de dichos resultados requiere una
#   organización adecuada y reproducible.
# - Resulta conveniente disponer de un documento único que integre de
#   forma ordenada todas las visualizaciones generadas durante el
#   análisis para cada modelo concreto.
#
# Metodología:
# - Se crea un documento por método con todos lo resultados gráficos.
# - Los resultados se organizan en secciones donde se reflejan los
#   distintos tipos de relaciones estudiadas entre conjuntos de
#   variables.
# ============================================================================#

library(grid)

open_pdf <- function(file, width = 12, height = 8.5) {
  dir_out <- dirname(file)
  if (!dir.exists(dir_out)) {
    dir.create(dir_out, recursive = TRUE)
  }
  grDevices::pdf(
    file = file,
    onefile = TRUE,
    width = width,
    height = height,
    useDingbats = FALSE
  )
}

close_pdf <- function() grDevices::dev.off()

###############################################################################
# Página de texto (portada / separador)
###############################################################################
print_text_page <- function(title,
                            subtitle = NULL,
                            body = NULL) {
  
  grid.newpage()
  
  y <- 0.85
  
  grid.text(
    title,
    x = 0.5,
    y = y,
    gp = gpar(fontsize = 22, fontface = "bold")
  )
  
  y <- y - 0.10
  
  if (!is.null(subtitle)) {
    grid.text(
      subtitle,
      x = 0.5,
      y = y,
      gp = gpar(fontsize = 16)
    )
    y <- y - 0.12
  }
  
  if (!is.null(body)) {
    grid.text(
      body,
      x = 0.5,
      y = y,
      just = "top",
      gp = gpar(fontsize = 12),
      vp = viewport(width = 0.85)
    )
  }
}

###############################################################################
# EXPORTADOR FINAL: ÓRDENES + PRESENCIA + AUSENCIA
###############################################################################
exportar_presencia_ausencia_pdf <- function(
    ruta_pdf,
    tabla_T_instancia,
    row_instancia,
    vars,
    texto_metodo = "Describe aquí el método."
) {
  
  open_pdf(ruta_pdf)
  on.exit(close_pdf(), add = TRUE)
  
  ###########################################################################
  # PORTADA
  ###########################################################################
  print_text_page(
    title    = "Análisis del Valor de Importancia por Presencia y Ausencia",
    subtitle = paste0("Instancia fila = ", row_instancia),
    body     = texto_metodo
  )
  
  ###########################################################################
  # 1 → 1
  ###########################################################################
  print_text_page(title = "Relación 1 a 1")
  
  for (A in vars) {
    for (P in setdiff(vars, A)) {
      
      tab_A <- lista_tablas[[A]]
      tab_P <- calcular_shapley_presencia(tabla_T_instancia, P, A)
      tab_Q <- calcular_shapley_ausencia(tabla_T_instancia, P, A)
      
      print(
        plot_sh_presencia_ausencia(
          tab_ord = tab_A,
          tab_pres = tab_P,
          tab_aus  = tab_Q,
          A = A,
          P = P,
          fila = row_instancia
        )
      )
    }
  }
  
  ###########################################################################
  # 1 → VARIOS
  ###########################################################################
  print_text_page(title = "Relación 1 a varias")
  
  for (kA in 2:length(vars)) {
    for (A in combn(vars, kA, simplify = FALSE)) {
      
      key_A <- paste(A, collapse = "_")
      tab_A <- lista_tablas[[key_A]]
      
      for (P in setdiff(vars, A)) {
        
        tab_P <- calcular_shapley_presencia(tabla_T_instancia, P, A)
        tab_Q <- calcular_shapley_ausencia(tabla_T_instancia, P, A)
        
        print(
          plot_sh_presencia_ausencia(
            tab_ord = tab_A,
            tab_pres = tab_P,
            tab_aus  = tab_Q,
            A = A,
            P = P,
            fila = row_instancia
          )
        )
      }
    }
  }
  
  ###########################################################################
  # VARIOS → 1
  ###########################################################################
  print_text_page(title = "Relación varias a 1")
  
  for (A in vars) {
    
    otros <- setdiff(vars, A)
    if (length(otros) < 2) next
    
    tab_A <- lista_tablas[[A]]
    
    for (kP in 2:length(otros)) {
      for (P in combn(otros, kP, simplify = FALSE)) {
        
        tab_P <- calcular_shapley_presencia(tabla_T_instancia, P, A)
        tab_Q <- calcular_shapley_ausencia(tabla_T_instancia, P, A)
        
        print(
          plot_sh_presencia_ausencia(
            tab_ord = tab_A,
            tab_pres = tab_P,
            tab_aus  = tab_Q,
            A = A,
            P = P,
            fila = row_instancia
          )
        )
      }
    }
  }
  
  ###########################################################################
  # VARIOS → VARIOS
  ###########################################################################
  print_text_page(title = "Relación varias a varias")
  
  for (kA in 2:(length(vars) - 1)) {
    for (A in combn(vars, kA, simplify = FALSE)) {
      
      key_A <- paste(A, collapse = "_")
      tab_A <- lista_tablas[[key_A]]
      
      restantes <- setdiff(vars, A)
      if (length(restantes) < 2) next
      
      for (kP in 2:length(restantes)) {
        for (P in combn(restantes, kP, simplify = FALSE)) {
          
          tab_P <- calcular_shapley_presencia(tabla_T_instancia, P, A)
          tab_Q <- calcular_shapley_ausencia(tabla_T_instancia, P, A)
          
          print(
            plot_sh_presencia_ausencia(
              tab_ord = tab_A,
              tab_pres = tab_P,
              tab_aus  = tab_Q,
              A = A,
              P = P,
              fila = row_instancia
            )
          )
        }
      }
    }
  }
  
  invisible(TRUE)
}



exportar_presencia_ausencia_pdf(
  ruta_pdf = file.path(ruta_resultados, "Posición_presencia_ausencia.pdf"),
  tabla_T_instancia = tabla_T_instancia,
  row_instancia = row_instancia_ordenes,
  vars = vars,
  texto_metodo = "
Visualización medida de la importancia para el Método 4.
Variación de predicción en S calculada para la instancia 1.
Representación Valor de Shapley por órdenes, presencia y ausencia.
Modelo: XGBoost – Clasificación.
"
)




# ============================================================================
# 6. INSPECCIÓN INDIVIDUAL DE RELACIONES ENTRE VARIABLES Y SUBCONJUNTOS
# ============================================================================
#
# Objetivo:
# Facilitar el análisis detallado de relaciones específicas entre
# subconjuntos de variables mediante la comparación simultánea de la
# importancia por posición, presencia y ausencia para una
# configuración seleccionada por el usuario.
#
# Motivación:
# - Los procedimientos anteriores generan de forma automática un gran
#   número de combinaciones entre subconjuntos de variables.
# - En muchos casos resulta necesario inspeccionar manualmente
#   relaciones concretas de especial interés.
# - Este procedimiento permite analizar casos específicos sin recorrer
#   la totalidad de combinaciones posibles.
#
# Metodología:
# - El usuario selecciona explícitamente los subconjuntos A-> (variable/s analizada/s) y 
# P->(variable/s que influyen).
# - Se calcula el valor de la importancia por posición asociado a A.
# - Se calcula el valor de la importancia presencia de P.
# - Se calcula el valor de la importancia por ausencia de P.
# - Las tres magnitudes obtenidas se representan conjuntamente en una
#   única visualización.
# - Se devuelven además las tablas numéricas correspondientes para su
#   análisis detallado.
#
# Escenarios considerados:
#
# - El procedimiento admite cualquier combinación válida entre A y P.
#
# 1) Uno a uno:
#
#       |A| = 1
#       |P| = 1
#
#   Ejemplo:
#
#       Age ← Ris
#
# - Se analiza la influencia de una variable individual sobre otra.
#
# 2) Uno a varios:
#
#       |A| > 1
#       |P| = 1
#
#   Ejemplo:
#
#       {Age,Ris} ← Cho
#
# - Se analiza la influencia de una variable sobre un subconjunto.
#
# 3) Varios a uno:
#
#       |A| = 1
#       |P| > 1
#
#   Ejemplo:
#
#       Ris ← {Cho,Sex}
#
# - Se analiza la influencia conjunta de varias variables sobre una
#   variable individual.
#
# 4) Varios a varios:
#
#       |A| > 1
#       |P| > 1
#
#   Ejemplo:
#
#       {Age,Rac} ← {Ris,Sex}
#
# - Se analiza la interacción entre dos subconjuntos arbitrarios de
#   variables.
#
# Validación:
# - Se verifica la definición correcta de los subconjuntos A y P.
# - Se comprueba que A y P no compartan variables.
# - Se verifica la disponibilidad de todas las variables implicadas en
#   la tabla Δ(S).
# - Se garantiza la obtención conjunta de los resultados asociados a
#   posición, presencia y ausencia.
#
# Resultado:
# - Comparación gráfica simultánea de:
#    * Posición
#    * Presencia    
#    * Ausencia     
#
# - Tablas numéricas asociadas a cada magnitud.
# - Herramienta de validación e interpretación de relaciones concretas
#   seleccionadas por el usuario.
#
# ============================================================================

check_shapley <- function(
    A,                    # variable(s) analizada(s)
    P,                    # variable(s) que influyen
    tabla_T_instancia,
    fila
) {
  
  # ---------------- VALIDACIONES ----------------
  if (missing(A) || length(A) == 0)
    stop("Debes especificar la(s) variable(s) analizada(s) A.")
  
  if (missing(P) || length(P) == 0)
    stop("Debes especificar la(s) variable(s) que influyen P.")
  
  if (length(intersect(A, P)) > 0)
    stop("A (analizada) y P (influyente) no pueden intersectar.")
  
  # ---------------- ÓRDENES ----------------
  tab_ord <- calcular_shapley_ordenes(
    tabla_T_instancia = tabla_T_instancia,
    A = A,
    T0 = 0
  )
  
  # ---------------- PRESENCIA ----------------
  tab_pres <- calcular_shapley_presencia(
    tabla_T_instancia = tabla_T_instancia,
    P = P,
    A = A
  )
  
  # ---------------- AUSENCIA ----------------
  tab_aus <- calcular_shapley_ausencia(
    tabla_T_instancia = tabla_T_instancia,
    Q = P,
    A = A
  )
  
  # ---------------- GRÁFICA FINAL ----------------
  print(
    plot_sh_presencia_ausencia(
      tab_ord = tab_ord,
      tab_pres = tab_pres,
      tab_aus  = tab_aus,
      A = A,
      P = P,
      fila = fila
    )
  )
  
  invisible(
    list(
      ordenes   = tab_ord,
      presencia = tab_pres,
      ausencia  = tab_aus
    )
  )
}

# 1 a 1

check_shapley(
  A = "Age",
  P = "Ris",
  tabla_T_instancia = tabla_T_instancia,
  fila = row_instancia_ordenes
)


# 1 a 2

check_shapley(
  A = c("Age", "Ris"),
  P = "Cho",
  tabla_T_instancia = tabla_T_instancia,
  fila = row_instancia_ordenes
)


# 1 a 3

check_shapley(
  A = c("Cho", "Ris","Sex"),
  P = "Age",
  tabla_T_instancia = tabla_T_instancia,
  fila = row_instancia_ordenes
)

# 2 a 1

check_shapley(
  A = "Ris",
  P = c("Cho","Sex"),
  tabla_T_instancia = tabla_T_instancia,
  fila = row_instancia_ordenes
)


# 3 a 1

check_shapley(
  A = "Age",
  P = c("Rac","Ris","Sex"),
  tabla_T_instancia = tabla_T_instancia,
  fila = row_instancia_ordenes
)

# 2 a 2

check_shapley(
  A = c("Age", "Rac"),
  P = c("Ris", "Sex"),
  tabla_T_instancia = tabla_T_instancia,
  fila = row_instancia_ordenes
)

