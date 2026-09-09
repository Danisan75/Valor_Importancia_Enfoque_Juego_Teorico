################################################################################
################################################################################
################ Representación bidimensional – Shapley por Órdenes ###########
################################################################################
################################################################################
# ============================================================================
# 1. PREPARACIÓN DE LA TABLA Δ(S) Y FIJACIÓN DE LA INSTANCIA DE ANÁLISIS
# ============================================================================
#
# Objetivo:
# Seleccionar la tabla de contribuciones Δ(S) obtenida previamente,
# normalizar su estructura y fijar la instancia sobre la que se
# realizará posteriormente la representación bidimensional de los
# valores de Shapley por órdenes.
#
# Metodología:
# - Se parte de una tabla T(i,S) obtenida previamente mediante el
#   procedimiento de cálculo de contribuciones.
# - Se verifica que la estructura seleccionada corresponda a una
#   tabla de datos válida.
# - Se identifican las columnas asociadas a las contribuciones
#   correspondientes a las distintas coaliciones S.
# - Se normaliza la nomenclatura utilizada para representar las
#   coaliciones con el fin de obtener una representación homogénea.
# - Las expresiones escritas mediante la forma T_S_* se transforman
#   automáticamente a la forma T(S).
# - La coalición vacía se representa mediante T().
# - Una vez normalizada la tabla, se selecciona una instancia
#   específica del conjunto de datos.
# - La fila correspondiente a dicha instancia se extrae y se utilizará
#   como referencia en todos los cálculos posteriores.
#
# Interpretación:
# - La tabla normalizada constituye la representación de las
#   contribuciones Δ(S) asociadas a todas las coaliciones
#   consideradas.
# - La instancia seleccionada representa la observación concreta para
#   la que se calcularán posteriormente los efectos de las distintas
#   órdenes de interacción.
# - Este paso no modifica los valores de las contribuciones y tiene
#   únicamente una función preparatoria.
#
# Validación:
# - Se verifica que el objeto seleccionado sea una tabla de datos.
# - Se comprueba la existencia de columnas compatibles con la
#   representación T(S).
# - Se verifica que la instancia seleccionada exista en la tabla.
# - Se garantiza que la extracción de la instancia produzca una única
#   observación.
#
# Resultado:
# - Tabla Δ(S) normalizada.
# - Identificación de las coaliciones disponibles.
# - Instancia de análisis seleccionada.
# - Vector de contribuciones Δ(S) asociado a dicha instancia.
#
# ============================================================================


tabla_T_base <- M4_Delta_glm_yb_stream # Elegimos el archivo 

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
# 2. CÁLCULO Y REPRESENTACIÓN DEL VALOR DE SHAPLEY POR ÓRDENES
# ============================================================================
#
# Objetivo:
# Calcular la descomposición del valor de Shapley por órdenes para cada
# variable presente en la tabla Δ(S) asociada a la instancia previamente
# seleccionada y representar gráficamente los resultados obtenidos.
#
# Metodología:
# - Se parte de la tabla Δ(S) correspondiente a una única instancia.
# - Se identifican todas las coaliciones disponibles a partir de las
#   columnas de la forma T(S).
# - Cada coalición se reconstruye explícitamente como un conjunto de
#   variables.
# - Las coaliciones reconstruidas se transforman a una representación
#   canónica mediante la ordenación de sus elementos.
# - La coalición vacía se representa mediante el conjunto vacío.
# - A partir de las coaliciones reconstruidas se identifica el conjunto
#   completo de variables participantes en el modelo.
# - Para cada variable i se consideran todas las coaliciones que la
#   contienen.
# - En cada caso se calcula la contribución marginal:
#
#       Δ(S ∪ {i}) − Δ(S)
#
#   donde S representa la coalición obtenida al eliminar la variable i
#   de la coalición considerada.
#
# - Las contribuciones marginales obtenidas se agrupan según el tamaño
#   de la coalición previa:
#
#       j = |S|
#
# - Para cada orden j se calcula la media de las contribuciones
#   marginales asociadas a dicho tamaño de coalición.
# - El resultado obtenido constituye la representación del valor de
#   Shapley descompuesto por órdenes para la variable considerada.
# - El procedimiento se repite para todas las variables presentes en la
#   tabla Δ(S).
# - Finalmente se genera una representación gráfica de los valores
#   obtenidos para cada orden de interacción.
#
# Interpretación:
# - El orden j representa el número de variables presentes en la
#   coalición antes de incorporar la variable analizada.
# - El valor asociado a cada orden cuantifica la contribución marginal
#   media de la variable cuando interactúa con coaliciones de dicho
#   tamaño.
# - Esta representación permite analizar cómo evoluciona la influencia
#   de una variable a medida que aumenta la complejidad de las
#   interacciones consideradas.
# - A diferencia del valor de Shapley agregado, la descomposición por
#   órdenes permite identificar la estructura de interacción asociada a
#   cada variable.
#
# Validación:
# - Se verifica la correcta reconstrucción de todas las coaliciones.
# - Se comprueba la presencia de la variable analizada en las
#   coaliciones utilizadas para el cálculo.
# - Se verifica la consistencia de las contribuciones marginales
#   calculadas para cada orden.
# - Se garantiza la obtención de una representación por órdenes para
#   todas las variables presentes en la tabla Δ(S).
#
# Resultado:
# - Conjunto de tablas de Shapley por órdenes para todas las variables.
# - Contribuciones marginales medias asociadas a cada orden de
#   interacción.
# - Representaciones gráficas individuales para cada variable.
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


calcular_shapley_por_ordenes <- function(tabla_T_instancia, var_obj, T0 = 0) {
  
  T_cols <- grep("^T\\(", names(tabla_T_instancia), value = TRUE)
  T_vals <- as.numeric(tabla_T_instancia[1, T_cols, drop = TRUE])
  names(T_vals) <- T_cols
  
  combos <- reconstruir_combos_desde_T(T_cols)
  
  key_S <- vapply(combos, function(S) paste(S, collapse="|"), character(1))
  idx_by_key <- setNames(seq_along(key_S), key_S)
  
  jugadores <- sort(unique(unlist(combos)))
  n <- length(jugadores)
  
  if (!var_obj %in% jugadores)
    stop("La variable objetivo no aparece en T(S).")
  
  max_j <- n - 1
  contrib_por_orden <- vector("list", max_j + 1)
  
  for (k in seq_along(combos)) {
    
    S_total <- combos[[k]]
    if (!var_obj %in% S_total) next
    
    S <- setdiff(S_total, var_obj)
    j <- length(S)
    
    T_Si <- T_vals[k]
    T_S  <- if (j == 0) T0 else T_vals[idx_by_key[[paste(S, collapse="|")]]]
    
    contrib_por_orden[[j + 1]] <- c(
      contrib_por_orden[[j + 1]],
      T_Si - T_S
    )
  }
  
  Sh_orden <- sapply(contrib_por_orden, function(x)
    if (length(x)) mean(x) else NA_real_)
  
  data.frame(
    j  = 0:max_j,
    Sh = Sh_orden
  )
}


library(ggplot2)

plot_sh_por_ordenes <- function(data, var_id, var_nombre, fila) {
  
  etiqueta <- paste0(
    "Sh[group('{',", var_id, ",'}')]^j * ",
    "group('(', list(Delta), ')')"
  )
  
  ggplot() +
    geom_point(data = data, aes(j, Sh),
               size = 4, color = "darkgreen", shape = 17) +
    geom_line (data = data, aes(j, Sh),
               linewidth = 1.3, color = "darkgreen") +
    
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
      x = "Orden j",
      y = "Sh",
      title = paste0(
        "Shapley por órdenes (instancia fila = ",
        fila,
        ") para ",
        var_nombre
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


# Extraer variables automáticamente
T_cols <- grep("^T\\(", names(tabla_T_instancia), value = TRUE)
combos_all <- lapply(T_cols, function(nm) {
  inside <- gsub("^T\\(|\\)$", "", nm)
  if (inside == "") character(0) else strsplit(inside, "_")[[1]]
})
vars <- sort(unique(unlist(combos_all)))

# Mapear IDs (1,2,3,...)
var_ids <- seq_along(vars)
names(var_ids) <- vars

# Listas de salida
lista_tablas <- list()
lista_plots  <- list()

for (v in vars) {
  
  tabla_v <- calcular_shapley_por_ordenes(
    tabla_T_instancia = tabla_T_instancia,
    var_obj = v,
    T0 = 0
  )
  
  lista_tablas[[v]] <- tabla_v
  
  p <- plot_sh_por_ordenes(
    data        = tabla_v,
    var_id      = var_ids[v],
    var_nombre = v,
    fila        = row_instancia_ordenes
  )
  
  lista_plots[[v]] <- p
  print(p)
}



################################################################################
################################################################################
############ SHAPLEY POR ÓRDENES – TODOS LOS SUBCONJUNTOS ######################
################################################################################
################################################################################

# ============================================================================
# 3. GENERALIZACIÓN DEL VALOR DE SHAPLEY POR ÓRDENES A SUBCONJUNTOS DE
#    VARIABLES
# ============================================================================
#
# Objetivo:
# Extender la representación del valor de Shapley por órdenes desde
# variables individuales hasta subconjuntos arbitrarios de variables,
# permitiendo analizar contribuciones conjuntas e interacciones de
# cualquier tamaño.
#
# Motivación:
# - El procedimiento definido previamente calcula el valor de Shapley
#   por órdenes para una única variable.
# - Sin embargo, en muchos problemas resulta de interés estudiar la
#   contribución conjunta de varias variables consideradas de forma
#   simultánea.
# - Para ello se reemplaza la variable individual por un subconjunto
#   arbitrario A de variables.
#
# Definición:
#
#     Sh_A^j = E[ Δ(S ∪ A) − Δ(S) : |S| = j ]
#
# donde:
#
# - A representa el subconjunto de variables analizado.
# - S representa una coalición que no contiene variables pertenecientes
#   a A.
# - j representa el tamaño de la coalición previa S.
# - Δ(S) representa la contribución asociada a la coalición S.
#
# Caso particular:
#
# - Cuando el subconjunto A contiene una única variable:
#
#       A = {i}
#
#   se recupera exactamente la formulación del valor de Shapley por
#   órdenes definida para variables individuales.
#
# Metodología:
# - Se parte de la tabla Δ(S) correspondiente a una instancia
#   previamente seleccionada.
# - Se reconstruyen las coaliciones disponibles a partir de su
#   representación simbólica.
# - Se identifica el conjunto completo de variables presentes en el
#   problema.
# - A partir de dichas variables se generan todos los subconjuntos no
#   vacíos posibles.
# - Cada subconjunto generado constituye un candidato A para el
#   análisis.
# - Para cada subconjunto A se consideran todas las coaliciones S que
#   no contienen ninguna de las variables pertenecientes a A.
# - Para cada coalición válida se calcula la contribución marginal:
#
#       Δ(S ∪ A) − Δ(S)
#
# - Las contribuciones marginales se agrupan según el tamaño de la
#   coalición previa:
#
#       j = |S|
#
# - Para cada orden j se calcula la media de las contribuciones
#   marginales observadas.
# - El procedimiento se repite para todos los subconjuntos A
#   considerados.
# - Finalmente se genera una representación gráfica para cada
#   subconjunto analizado.
#
# Interpretación:
# - El valor asociado a cada orden j cuantifica la contribución
#   conjunta media del subconjunto A cuando se incorpora a coaliciones
#   de tamaño j.
# - La metodología permite estudiar efectos de interacción entre
#   grupos de variables manteniendo la interpretación por órdenes
#   utilizada en el caso univariante.
# - Las diferencias observadas entre órdenes reflejan cómo varía la
#   influencia conjunta del subconjunto analizado en función de la
#   información previamente disponible.
# - El caso univariante aparece como una situación particular de esta
#   formulación general.
#
# Validación:
# - Se verifica la correcta reconstrucción de las coaliciones.
# - Se comprueba que las variables pertenecientes al subconjunto A
#   estén presentes en la tabla Δ(S).
# - Se excluyen automáticamente las coaliciones que contienen
#   variables de A.
# - Se verifica la existencia de las coaliciones necesarias para el
#   cálculo de las contribuciones marginales.
# - Se garantiza la obtención de resultados para todos los
#   subconjuntos considerados.
#
# Resultado:
# - Valores Sh_A^j para todos los subconjuntos analizados.
# - Tablas de contribuciones conjuntas por órdenes.
# - Representaciones gráficas de los distintos subconjuntos de
#   variables.
# - Caracterización de interacciones de cualquier nivel de
#   complejidad presente en el problema.
#
# ============================================================================
calcular_shapley_ordenes_A <- function(tabla_T_instancia, A, T0 = 0) {
  
  # 1. Columnas T(S)
  T_cols <- grep("^T\\(", names(tabla_T_instancia), value = TRUE)
  T_vals <- as.numeric(tabla_T_instancia[1, T_cols, drop = TRUE])
  names(T_vals) <- T_cols
  
  # 2. Reconstruir coaliciones S (canónicas)
  combos <- lapply(T_cols, function(nm) {
    inside <- gsub("^T\\(|\\)$", "", nm)
    if (inside == "") character(0)
    else sort(strsplit(inside, "_")[[1]])
  })
  
  # 3. Índice rápido S -> columna
  key_S <- vapply(combos, function(S) paste(S, collapse = "|"), character(1))
  idx   <- setNames(seq_along(key_S), key_S)
  
  # 4. Variables del problema
  jugadores <- sort(unique(unlist(combos)))
  n <- length(jugadores)
  
  if (!all(A %in% jugadores)) {
    stop("Conjunto A contiene variables no presentes en T(S).")
  }
  
  # 5. Órdenes posibles
  max_j <- n - length(A)
  contrib_por_orden <- vector("list", max_j + 1)
  
  # 6. Bucle principal
  for (k in seq_along(combos)) {
    
    S <- combos[[k]]
    
    # S no puede intersectar con A
    if (length(intersect(S, A)) > 0) next
    
    j <- length(S)
    
    SA <- sort(c(S, A))
    key_SA <- paste(SA, collapse = "|")
    
    if (!key_SA %in% key_S) next
    
    T_S  <- if (j == 0) T0 else T_vals[idx[[paste(S, collapse = "|")]]]
    T_SA <- T_vals[idx[[key_SA]]]
    
    contrib_por_orden[[j + 1]] <- c(
      contrib_por_orden[[j + 1]],
      T_SA - T_S
    )
  }
  
  # 7. Media por orden
  Sh <- sapply(contrib_por_orden, function(x)
    if (length(x)) mean(x) else NA_real_)
  
  data.frame(
    j  = 0:max_j,
    Sh = Sh
  )
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
      x = "Orden j",
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
  
  # Cálculo
  tabla_A <- calcular_shapley_ordenes_A(
    tabla_T_instancia = tabla_T_instancia,
    A = A,
    T0 = 0
  )
  
  lista_tablas[[nombre_A]] <- tabla_A
  
  # Gráfica (con IDs correctos y plotmath seguro)
  p <- plot_sh_por_ordenes_A(
    data = tabla_A,
    A    = A,
    fila = row_instancia_ordenes
  )
  
  lista_plots[[nombre_A]] <- p
  print(p)
}



################################################################################
############################## Presencia #######################################
################################################################################

# ============================================================================
# 4. VALOR DE SHAPLEY POR ÓRDENES CONDICIONADO POR PRESENCIA
# ============================================================================
#
# Objetivo:
# Extender la representación del valor de Shapley por órdenes
# generalizado incorporando condiciones de presencia previas, con el
# fin de analizar cómo la contribución de un subconjunto de variables A
# se modifica cuando otro subconjunto de variables P ya forma parte de
# la coalición considerada.
#
# Motivación:
# - Los procedimientos anteriores permiten estudiar la contribución de
#   variables individuales o subconjuntos de variables considerando
#   todas las coaliciones compatibles.
# - Sin embargo, en numerosos problemas resulta de interés analizar la
#   influencia de determinadas variables o grupos de variables sobre la
#   contribución de otras.
# - Para ello se incorpora una condición adicional de presencia que
#   obliga a que determinadas variables P se encuentren previamente en
#   la coalición analizada.
#
# Definición:
#
#     Sh_{P→A}^j =
#       E[ Δ(S ∪ A) − Δ(S) |
#          |S| = j,
#          P ⊆ S,
#          A ∩ S = ∅ ]
#
# donde:
#
# - A representa el subconjunto de variables objetivo.
# - P representa el subconjunto de variables cuya presencia se exige.
# - S representa una coalición compatible con ambas condiciones.
# - j representa el tamaño de la coalición previa S.
# - Δ(S) representa la contribución asociada a la coalición S.
#
# Relación con los procedimientos previos:
#
# - Cuando no se impone ninguna condición de presencia:
#
#       P = ∅
#
#   se recupera el valor de Shapley por órdenes generalizado definido
#   anteriormente.
#
# - La presencia introduce una restricción adicional sobre las
#   coaliciones utilizadas en el cálculo de las contribuciones
#   marginales.
#
# Metodología:
# - Se parte de la tabla Δ(S) asociada a una instancia previamente
#   seleccionada.
# - Se reconstruyen todas las coaliciones disponibles a partir de su
#   representación simbólica.
# - Se identifican los subconjuntos A y P objeto de análisis.
# - Se consideran únicamente aquellas coaliciones S que cumplen
#   simultáneamente:
#
#       P ⊆ S
#
#       A ∩ S = ∅
#
# - Para cada coalición válida se construye:
#
#       S ∪ A
#
# - Se calcula la contribución marginal:
#
#       Δ(S ∪ A) − Δ(S)
#
# - Las contribuciones marginales obtenidas se agrupan según el tamaño
#   de la coalición previa:
#
#       j = |S|
#
# - Para cada orden j se calcula la media de las contribuciones
#   marginales observadas.
# - Finalmente se comparan los resultados condicionados por presencia
#   con los valores de Shapley por órdenes obtenidos sin
#   condicionamiento.
#
# Escenarios considerados:
#
# - El procedimiento se aplica sistemáticamente a todas las
#   combinaciones válidas entre el conjunto de presencia P y el
#   conjunto objetivo A.
#
# 1) Uno a uno:
#
#       |P| = 1
#       |A| = 1
#
#   Se analiza la influencia de una variable sobre otra variable
#   individual.
#
# 2) Varios a uno:
#
#       |P| > 1
#       |A| = 1
#
#   Se analiza la influencia conjunta de varias variables sobre una
#   variable individual.
#
# 3) Uno a varios:
#
#       |P| = 1
#       |A| > 1
#
#   Se analiza la influencia de una variable sobre un subconjunto de
#   variables.
#
# 4) Varios a varios:
#
#       |P| > 1
#       |A| > 1
#
#   Se analiza la influencia conjunta de un subconjunto de variables
#   sobre otro subconjunto de variables.
#
# Restricción:
#
#       P ∩ A = ∅
#
# - Se impide cualquier solapamiento entre las variables cuya presencia
#   se exige y las variables cuya contribución se evalúa.
#
# Interpretación:
# - El valor Sh_{P→A}^j cuantifica la contribución conjunta media del
#   subconjunto A cuando se incorpora a coaliciones de tamaño j que ya
#   contienen las variables de P.
# - La comparación entre Sh_A^j y Sh_{P→A}^j permite identificar cómo
# cambia la contribución de A cuando la información representada por
# P ya está disponible en la coalición.
# - Diferencias entre ambas magnitudes pueden indicar efectos de
#   dependencia, refuerzo, inhibición o interacción entre variables o
#   grupos de variables.
# - Los escenarios uno a uno permiten estudiar relaciones individuales,
#   mientras que los escenarios varios a uno, uno a varios y varios a
#   varios permiten caracterizar interacciones de complejidad creciente.
#
# Validación:
# - Se verifica la correcta reconstrucción de las coaliciones.
# - Se comprueba que los subconjuntos A y P pertenezcan al universo de
#   variables disponible.
# - Se verifica que A y P no compartan elementos.
# - Se garantiza la existencia de las coaliciones necesarias para el
#   cálculo de las contribuciones marginales.
# - Se excluyen automáticamente las configuraciones incompatibles.
#
# Resultado:
# - Valores Sh_{P→A}^j para todas las combinaciones consideradas.
# - Comparación entre efectos condicionados y no condicionados.
# - Representaciones gráficas de las relaciones entre presencia y
#   contribución.
# - Caracterización de dependencias e interacciones entre variables y
#   subconjuntos de variables.
#
# ============================================================================

calcular_shapley_presencia <- function(tabla_T_instancia, P, A, T0 = 0) {
  
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
  
  if (!all(P %in% jugadores)) stop("Error en P")
  if (!all(A %in% jugadores)) stop("Error en A")
  if (length(intersect(P, A)) > 0) stop("P y A no pueden intersectar")
  
  max_j <- length(jugadores) - length(A)
  contrib <- vector("list", max_j + 1)
  
  for (k in seq_along(combos)) {
    
    S <- combos[[k]]
    
    if (!all(P %in% S)) next
    if (length(intersect(S, A)) > 0) next
    
    j <- length(S)
    
    SA <- sort(c(S, A))
    key_SA <- paste(SA, collapse="|")
    
    if (!key_SA %in% key_S) next
    
    T_S  <- if (j == 0) T0 else T_vals[idx[[paste(S, collapse="|")]]]
    T_SA <- T_vals[idx[[key_SA]]]
    
    contrib[[j + 1]] <- c(contrib[[j + 1]], T_SA - T_S)
  }
  
  Sh <- sapply(contrib, function(x)
    if (length(x)) mean(x) else NA_real_)
  
  data.frame(j = 0:max_j, Sh = Sh)
}

###############################################################################
# GRÁFICA FINAL: ÓRDENES + PRESENCIA 
#
# Este bloque genera una visualización conjunta que compara:
#
# 1) Shapley por órdenes estándar (sin condicionamiento)
# 2) Shapley por órdenes condicionado a la presencia de un conjunto P
#
# Todo ello en un único gráfico, manteniendo consistencia visual, claridad
# matemática y estabilidad (sin warnings).
#
# OBJETIVO:
# - Comparar directamente:
#     * efecto total de A
#     * efecto de A condicionado a que P ya esté presente
#
# - Detectar:
#     * dependencia entre variables
#     * cambios de comportamiento según contexto
#     * interacciones reales
#
# INTERPRETACIÓN DEL GRÁFICO:
# - Curva verde:
#     Sh_A^j → efecto medio de A sobre todas las coaliciones de tamaño j
#
# - Curva azul:
#     Sh_{P→A}^j → efecto de A SOLO sobre coaliciones que ya contienen P
#
# - Diferencias entre curvas indican:
#     → influencia de P sobre el impacto de A
#
# ELEMENTOS CLAVE:
#
# 1) ETIQUETAS MATEMÁTICAS (plotmath):
#    - Se generan COMO TEXTO, pero interpretado por R (parse = TRUE)
#    - Uso de group() para representar conjuntos:
#         {1}, {1,2}, etc.
#    - Uso de list() cuando hay varios elementos (obligatorio en plotmath)
#
#    - Etiqueta verde:
#         Sh^{j}({A}) · (Δ)
#
#    - Etiqueta azul:
#         Sh^{j}({A}) · (Δ | {P})
#
# 2) POSICIONAMIENTO INTELIGENTE:
#    - Las etiquetas se colocan dinámicamente:
#         * a la derecha del gráfico
#         * separadas verticalmente
#    - Se evita solapamiento incluso con rangos pequeños
#
# 3) TÍTULO PROFESIONAL:
#    - Combina nombres de variables (legible)
#    - Incluye IDs numéricos (trazabilidad)
#    - Indica claramente:
#         "influencia de P sobre A"
#
# 4) ROBUSTEZ:
#    - na.rm = TRUE evita errores de datos incompletos
#    - control dinámico del rango en eje Y
#    - sin warnings en annotate()
#
# RESULTADO:
# - Gráfico limpio, comparable y listo para análisis o presentación
#
# USO:
# - Diagnóstico de interacciones
# - Interpretabilidad de modelos
# - Análisis condicional de variables
###############################################################################

plot_sh_presencia <- function(tab_ord, tab_pres, A, P, fila) {
  
  # IDs numéricos
  A_ids <- var_ids[A]
  P_ids <- var_ids[P]
  

  
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
  

  yr <- range(c(tab_ord$Sh, tab_pres$Sh), na.rm = TRUE)
  dy <- 0.06 * diff(yr)
  if (!is.finite(dy) || dy == 0) dy <- 0.05
  
  x_text <- max(tab_ord$j) + 0.3
  y_top  <- max(yr)
  y_bot  <- max(yr) - dy
  

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
      x = "Orden j",
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

###############################################################################
# BUCLES DE PRESENCIA – 
#
# Este bloque ejecuta de forma exhaustiva el análisis de Shapley condicionado
# por presencia para TODAS las combinaciones posibles entre:
#
#   - A (conjunto objetivo)
#   - P (conjunto de presencia)
#
# garantizando cobertura completa del espacio de análisis.
#
# CONTEXTO:
# - Ya se dispone de:
#     * vars → conjunto total de variables
#     * lista_tablas → Shapley por órdenes para cada A
#     * función calcular_shapley_presencia()
#     * función plot_sh_presencia()
#
# OBJETIVO:
# - Analizar cómo cambia la influencia de A dependiendo de qué variables P
#   estén presentes previamente
#
# - Cubrir TODOS los casos relevantes:
#     1) 1 → 1       (una variable influye sobre otra)
#     2) varios → 1  (combinación influye sobre una)
#     3) 1 → varios  (una variable influye sobre un conjunto)
#     4) varios → varios (interacciones completas)
#
# PROBLEMA:
# - El número de combinaciones crece muy rápido:
#     Para n variables → combinaciones exponenciales
#
# - Es fácil cometer errores:
#     * repetir combinaciones
#     * permitir intersección A ∩ P
#     * incoherencias en índices
#
# SOLUCIÓN (ESTRUCTURA EN 4 BLOQUES):
#
# 1) 1 → 1:
#    - A es una variable
#    - P es otra variable distinta
#
# 2) varios → 1:
#    - A es una variable
#    - P es subconjunto de tamaño ≥ 2
#
# 3) 1 → varios:
#    - A es subconjunto de tamaño ≥ 2
#    - P es una variable fuera de A
#
# 4) varios → varios:
#    - A y P son subconjuntos de tamaño ≥ 2
#    - A ∩ P = ∅
#
# LÓGICA GENERAL:
# - Para cada combinación válida (A, P):
#     *Se calcula Sh_{P→A}^j
#     *Se compara con Sh_A^j (ya en lista_tablas)
#     *Se genera gráfico comparativo
#
# - Se utiliza:
#     setdiff(vars, A)
#   para garantizar que P nunca intersecta con A
#
# RESULTADO:
# - Visualización completa de dependencias entre variables
# - Identificación de:
#     * relaciones de refuerzo
#     * inhibición
#     * interacciones complejas
#
# IMPORTANTE:
# - Este bloque puede generar MUCHOS gráficos (crecimiento exponencial)
# - Útil para análisis profundo, no para ejecución masiva sin filtros
###############################################################################


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
  for (A in combn(vars, kA, simplify=FALSE)) {
    
    key_A <- paste(A, collapse="_")
    tab_A <- lista_tablas[[key_A]]
    
    for (P in setdiff(vars, A)) {
      
      tab_P <- calcular_shapley_presencia(tabla_T_instancia, P, A)
      print(plot_sh_presencia(tab_A, tab_P, A, P, row_instancia_ordenes))
    }
  }
}

# varias a varias

for (kA in 2:(length(vars)-1)) {
  for (A in combn(vars, kA, simplify=FALSE)) {
    
    key_A <- paste(A, collapse="_")
    tab_A <- lista_tablas[[key_A]]
    
    restantes <- setdiff(vars, A)
    if (length(restantes) < 2) next
    
    for (kP in 2:length(restantes)) {
      for (P in combn(restantes, kP, simplify=FALSE)) {
        
        tab_P <- calcular_shapley_presencia(tabla_T_instancia, P, A)
        print(plot_sh_presencia(tab_A, tab_P, A, P, row_instancia_ordenes))
      }
    }
  }
}



################################################################################
################################################################################
############################ Ausencia ##########################################
################################################################################

# ============================================================================
# 5. VALOR DE SHAPLEY POR ÓRDENES CONDICIONADO POR AUSENCIA
# ============================================================================
#
# Objetivo:
# Extender la representación del valor de Shapley por órdenes
# incorporando condiciones explícitas de ausencia, con el fin de
# analizar cómo cambia la contribución de un subconjunto de variables A
# cuando otro subconjunto de variables Q no está presente en las
# coaliciones consideradas.
#
# Motivación:
# - Los procedimientos anteriores permiten estudiar contribuciones
#   conjuntas sin restricciones o condicionadas a la presencia de un
#   subconjunto P.
# - Sin embargo, en numerosos problemas resulta igualmente relevante
#   analizar el comportamiento de un subconjunto de variables en
#   escenarios donde determinadas variables se encuentran ausentes.
# - Esta situación permite estudiar redundancias, sustituciones,
#   dependencias negativas y relaciones de complementariedad entre
#   variables.
#
# Definición:
#
#     Sh_{AUS(Q)→A}^j =
#       E[ Δ(S ∪ A) − Δ(S) |
#          |S| = j,
#          Q ∩ S = ∅,
#          A ∩ S = ∅ ]
#
# donde:
#
# - A representa el subconjunto de variables objetivo.
# - Q representa el subconjunto de variables cuya ausencia se impone.
# - S representa una coalición compatible con dichas restricciones.
# - j representa el tamaño de la coalición previa S.
# - Δ(S) representa la contribución asociada a la coalición S.
#
# Relación con los procedimientos previos:
#
# - El valor de Shapley por órdenes estándar considera todas las
#   coaliciones compatibles.
#
# - El valor de Shapley condicionado por presencia considera únicamente
#   aquellas coaliciones que incluyen un subconjunto obligatorio P.
#
# - El procedimiento actual considera únicamente aquellas coaliciones
#   que excluyen explícitamente un subconjunto Q.
#
# - Cuando:
#
#       Q = ∅
#
#   se recupera el valor de Shapley por órdenes generalizado definido
#   previamente.
#
# Metodología:
# - Se parte de la tabla Δ(S) correspondiente a una instancia
#   previamente seleccionada.
# - Se reconstruyen todas las coaliciones disponibles a partir de su
#   representación simbólica.
# - Se identifican los subconjuntos A y Q objeto de análisis.
# - Se consideran únicamente aquellas coaliciones S que cumplen
#   simultáneamente:
#
#       Q ∩ S = ∅
#
#       A ∩ S = ∅
#
# - Para cada coalición válida se construye:
#
#       S ∪ A
#
# - Se calcula la contribución marginal:
#
#       Δ(S ∪ A) − Δ(S)
#
# - Las contribuciones marginales obtenidas se agrupan según el tamaño
#   de la coalición previa:
#
#       j = |S|
#
# - Para cada orden j se calcula la media de las contribuciones
#   marginales observadas.
# - Finalmente se comparan los resultados obtenidos con los valores de
#   Shapley por órdenes estándar y con los obtenidos bajo condiciones
#   de presencia.
#
# Escenarios considerados:
#
# - El procedimiento se aplica de forma sistemática a todas las
#   combinaciones válidas entre el conjunto de ausencia Q y el conjunto
#   objetivo A.
#
# 1) Uno a uno:
#
#       |Q| = 1
#       |A| = 1
#
#   Se analiza el efecto de la ausencia de una variable sobre la
#   contribución de otra variable individual.
#
# 2) Varios a uno:
#
#       |Q| > 1
#       |A| = 1
#
#   Se analiza el efecto de la ausencia conjunta de varias variables
#   sobre una variable individual.
#
# 3) Uno a varios:
#
#       |Q| = 1
#       |A| > 1
#
#   Se analiza el efecto de la ausencia de una variable sobre la
#   contribución conjunta de un subconjunto de variables.
#
# 4) Varios a varios:
#
#       |Q| > 1
#       |A| > 1
#
#   Se analiza el efecto de la ausencia conjunta de varias variables
#   sobre otro subconjunto de variables.
#
# Restricción:
#
#       Q ∩ A = ∅
#
# - Se evita cualquier solapamiento entre las variables cuya ausencia
#   se impone y las variables cuya contribución se evalúa.
#
# Interpretación:
# - El valor Sh_{AUS(Q)→A}^j cuantifica la contribución conjunta media
#   del subconjunto A cuando se incorpora a coaliciones de tamaño j que
#   no contienen ninguna variable perteneciente a Q.
# - La comparación entre Sh_A^j y Sh_{AUS(Q)→A}^j permite analizar cómo
#   cambia la contribución de A cuando determinadas variables quedan
#   excluidas del contexto considerado.
# - Diferencias significativas entre ambas magnitudes pueden indicar
#   redundancia, sustitución, dependencia negativa o necesidad de
#   cooperación entre variables.
# - Los escenarios uno a uno permiten estudiar relaciones simples,
#   mientras que los escenarios varios a uno, uno a varios y varios a
#   varios permiten caracterizar mecanismos de interacción más
#   complejos.
#
# Comparación conjunta:
#
# - La comparación simultánea entre:
#
#       Sh_A^j
#
#       Sh_{P→A}^j
#
#       Sh_{AUS(Q)→A}^j
#
#   permite evaluar el comportamiento de A bajo tres contextos:
#
#   · Sin condicionamiento.
#   · Condicionado por presencia.
#   · Condicionado por ausencia.
#
# - Esta comparación proporciona una caracterización más completa de
#   las dependencias estructurales entre variables y subconjuntos de
#   variables.
#
# Validación:
# - Se verifica la correcta reconstrucción de las coaliciones.
# - Se comprueba que los subconjuntos A y Q pertenezcan al universo de
#   variables disponible.
# - Se verifica que A y Q no compartan elementos.
# - Se garantiza la existencia de las coaliciones necesarias para el
#   cálculo de las contribuciones marginales.
# - Se excluyen automáticamente las configuraciones incompatibles.
# - Se controla la existencia de órdenes válidos para cada combinación
#   considerada.
#
# Resultado:
# - Valores Sh_{AUS(Q)→A}^j para todas las combinaciones analizadas.
# - Comparación entre efectos globales, condicionados por presencia y
#   condicionados por ausencia.
# - Representaciones gráficas conjuntas de las tres situaciones.
# - Identificación de dependencias, redundancias y mecanismos de
#   sustitución entre variables y subconjuntos de variables.
#
# ============================================================================


calcular_shapley_ausencia <- function(tabla_T_instancia, Q, A, T0 = 0) {
  
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
  
  max_j <- length(jugadores) - length(A) - length(Q)
  if (max_j < 0) return(data.frame(j = integer(0), Sh = numeric(0)))
  
  contrib <- vector("list", max_j + 1)
  
  for (k in seq_along(combos)) {
    
    S <- combos[[k]]
    if (length(intersect(S, Q)) > 0) next
    if (length(intersect(S, A)) > 0) next
    
    j <- length(S)
    if (j > max_j) next
    
    SA <- sort(c(S, A))
    key_SA <- paste(SA, collapse="|")
    if (!key_SA %in% key_S) next
    
    T_S  <- if (j == 0) T0 else T_vals[idx[[paste(S, collapse="|")]]]
    T_SA <- T_vals[idx[[key_SA]]]
    
    contrib[[j + 1]] <- c(contrib[[j + 1]], T_SA - T_S)
  }
  
  Sh <- sapply(contrib, function(x) if (length(x)) mean(x) else NA_real_)
  data.frame(j = 0:max_j, Sh = Sh)
}


build_group_ids <- function(ids) {
  if (length(ids) == 1) {
    paste0("group('{',", ids, ",'}')")
  } else {
    paste0("group('{', list(", paste(ids, collapse=","), "), '}')")
  }
}




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

###############################################################################
# GRÁFICA FINAL: ÓRDENES + PRESENCIA + AUSENCIA
#
# Este bloque genera la visualización más completa del sistema, integrando en un
# solo gráfico tres niveles de análisis:
#
# 1) Shapley por órdenes (efecto global de A)
# 2) Shapley condicionado a la presencia de P (P → A)
# 3) Shapley condicionado a la ausencia de P (AUS(P) → A)
#
# OBJETIVO:
# - Comparar simultáneamente:
#     * efecto total de A
#     * efecto cuando P está presente
#     * efecto cuando P está ausente
#
# - Identificar patrones de interacción entre variables:
#     * refuerzo (presencia aumenta efecto)
#     * inhibición (presencia reduce efecto)
#     * sustitución (ausencia cambia comportamiento)
#
# INTERPRETACIÓN DEL GRÁFICO:
#
# - Curva verde:
#     Sh_A^j → efecto medio global de A
#
# - Curva azul:
#     Sh_{P→A}^j → efecto de A condicionado a presencia de P
#
# - Curva dorada:
#     Sh_{AUS(P)→A}^j → efecto de A condicionado a ausencia de P
#
# - Comparaciones clave:
#     azul > verde      → P potencia A
#     azul < verde      → P bloquea A
#     dorado > verde    → A mejora sin P (posible sustitución)
#     dorado < verde    → P necesario para A
#
# ETIQUETAS MATEMÁTICAS:
# - Se construyen como expresiones plotmath válidas
# - Uso de group() para representar conjuntos
# - Uso de list() cuando hay más de un elemento
#
# POSICIONAMIENTO:
# - Las etiquetas se colocan fuera del panel usando grid (annotation_custom)
# - Se usa sistema "npc" (0–1) para posicionamiento relativo
# - coord_cartesian(clip = "off") permite dibujar fuera del área visible
#
# ROBUSTEZ:
# - na.rm = TRUE evita errores con valores faltantes
# - Separación vertical fija evita solapamiento de etiquetas
# - Márgenes ampliados garantizan visibilidad
#
# RESULTADO:
# - Gráfico limpio, profesional y listo para presentación (nivel tesis / paper)
###############################################################################

plot_sh_presencia_ausencia <- function(tab_ord,
                                       tab_pres,
                                       tab_aus,
                                       A,
                                       P,
                                       fila) {
  

  A_ids <- var_ids[A]
  P_ids <- var_ids[P]
  

  
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
  
-
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
  # ETIQUETAS FUERA DEL PANEL 
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
  # EJES Y TEMA 
  # ---------------------------------------------------------------------------
  p +
    labs(
      x = "Orden j",
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
# BUCLES DE AUSENCIA – COBERTURA COMPLETA
#
# Este bloque ejecuta el análisis completo combinando simultáneamente:
#
#   - Shapley por órdenes (efecto global de A)
#   - Shapley condicionado a presencia (P → A)
#   - Shapley condicionado a ausencia (AUS(P) → A)
#
# para todas las combinaciones posibles de A (objetivo) y P (contexto).
#
# CONTEXTO:
# - Ya se dispone de:
#     vars → conjunto total de variables
#     lista_tablas → Sh_A^j precomputados
#     calcular_shapley_presencia() → Sh_{P→A}
#     calcular_shapley_ausencia() → Sh_{AUS(P)→A}
#     plot_sh_presencia_ausencia() → visualización conjunta
#
# OBJETIVO:
# - Generar una visión completa de cómo interactúan las variables en tres niveles:
#     1) efecto global
#     2) efecto cuando P está presente
#     3) efecto cuando P está ausente
#
# - Detectar automáticamente:
#     * refuerzos (presencia aumenta efecto)
#     * bloqueos (presencia lo reduce)
#     * dependencias fuertes (ausencia cambia el comportamiento)
#
# ESTRUCTURA DEL BLOQUE:
#
# 1) 1 → 1:
#    - A es una variable
#    - P es otra variable
#
# 2) varios → 1:
#    - A es una variable
#    - P es subconjunto (tamaño ≥ 2)
#
# 3) 1 → varios:
#    - A es subconjunto (tamaño ≥ 2)
#    - P es una variable externa
#
# 4) varios → varios:
#    - A y P son subconjuntos (tamaño ≥ 2)
#    - siempre A ∩ P = ∅
#
# LÓGICA GENERAL:
# - Para cada combinación válida (A, P):
#     1) Se recupera Sh_A^j desde lista_tablas
#     2) Se calcula Sh_{P→A}^j (presencia)
#     3) Se calcula Sh_{AUS(P)→A}^j (ausencia)
#     4) Se genera gráfico comparativo completo
#
# GARANTÍAS:
# - setdiff(vars, A) evita intersección entre A y P
# - estructuras consistentes entre bloques
# - mismo formato de plots en todo el pipeline
#
# RESULTADO:
# - Exploración exhaustiva del sistema
# - Visualización directa de todas las interacciones relevantes
#
# COSTE COMPUTACIONAL:
# - Crecimiento exponencial (2^n)
# - Puede generar un número muy alto de gráficos
#
# USO RECOMENDADO:
# - Análisis profundo (no ejecución masiva sin filtro)
# - Base para construir métricas agregadas posteriores
###############################################################################

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
# 6. ORGANIZACIÓN Y EXPORTACIÓN DE LOS RESULTADOS GRÁFICOS
# ============================================================================
#
# Objetivo:
# Generar una representación documental estructurada de los resultados
# obtenidos en los análisis de Shapley por órdenes, presencia y
# ausencia, facilitando su revisión, interpretación y posterior
# utilización en informes, tesis, artículos o procesos de validación.
#
# Motivación:
# - Los procedimientos anteriores generan un número potencialmente
#   elevado de representaciones gráficas.
# - La interpretación conjunta de dichos resultados requiere una
#   organización sistemática y reproducible.
# - Resulta conveniente disponer de un documento único que integre de
#   forma ordenada todas las visualizaciones generadas durante el
#   análisis.
#
# Metodología:
# - Se crea un documento multipágina que actuará como contenedor único
#   de los resultados.
# - Se incorpora una portada descriptiva con la información general del
#   análisis realizado.
# - Los resultados se organizan en secciones temáticas que reflejan los
#   distintos tipos de relaciones estudiadas entre conjuntos de
#   variables.
# - Para cada combinación analizada se recuperan:
#
#       Sh_A^j
#
#       Sh_{P→A}^j
#
#       Sh_{AUS(P)→A}^j
#
# - A partir de dichas magnitudes se genera la correspondiente
#   representación gráfica comparativa.
# - Cada representación se incorpora como una página independiente
#   dentro del documento final.
# - El procedimiento se aplica sistemáticamente a todas las
#   configuraciones válidas consideradas durante el análisis.
#
# Organización del documento:
#
# - Portada general del análisis.
#
# - Sección 1:
#
#       Uno a uno
#
#   Relaciones entre variables individuales.
#
# - Sección 2:
#
#       Uno a varios
#
#   Influencia de variables individuales sobre subconjuntos de
#   variables.
#
# - Sección 3:
#
#       Varios a uno
#
#   Influencia de subconjuntos de variables sobre variables
#   individuales.
#
# - Sección 4:
#
#       Varios a varios
#
#   Influencia entre subconjuntos arbitrarios de variables.
#
# Interpretación:
# - El documento generado constituye una representación completa del
#   comportamiento de las contribuciones por órdenes bajo los distintos
#   escenarios analizados.
# - La comparación simultánea entre los efectos globales, de presencia
#   y de ausencia facilita la identificación de dependencias,
#   redundancias y mecanismos de interacción entre variables.
# - La organización jerárquica del documento simplifica la exploración
#   de resultados incluso cuando el número de combinaciones analizadas
#   es elevado.
#
# Validación:
# - Se verifica la correcta generación de las representaciones
#   gráficas.
# - Se garantiza la inclusión de todas las combinaciones válidas
#   consideradas durante el análisis.
# - Se verifica la correcta estructuración de las distintas secciones
#   del documento.
# - Se asegura la integridad del proceso de exportación.
#
# Resultado:
# - Documento multipágina con la totalidad de los resultados gráficos.
# - Organización estructurada por tipos de relaciones entre variables.
# - Representación conjunta de los análisis por órdenes, presencia y
#   ausencia.
# - Soporte documental para interpretación, validación y comunicación
#   de resultados.
#
# ============================================================================


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
  #ruta_pdf = "C:\\Users\\ruta...\\Archivo.pdf",
  tabla_T_instancia = tabla_T_instancia,
  row_instancia = row_instancia_ordenes,
  vars = vars,
  texto_metodo = "
Visualización medida de la importancia para el Método IV.
Variación de predicción en S calculada para la instancia 1.
Representación Valor de Shapley por órdenes, presencia y ausencia.
Modelo: Glm – Clasificación.
"
)



# ============================================================================
# 7. INSPECCIÓN DIRIGIDA DE RELACIONES ENTRE VARIABLES Y SUBCONJUNTOS
# ============================================================================
#
# Objetivo:
# Facilitar el análisis detallado de relaciones específicas entre
# subconjuntos de variables mediante la comparación simultánea de los
# valores de Shapley por órdenes, presencia y ausencia para una
# configuración seleccionada por el usuario.
#
# Motivación:
# - Los procedimientos anteriores generan de forma automática un gran
#   número de combinaciones entre subconjuntos de variables.
# - En muchos casos resulta necesario inspeccionar manualmente
#   relaciones concretas de especial interés.
# - Este procedimiento permite analizar casos específicos sin recorrer
#   la totalidad del espacio de combinaciones posibles.
#
# Definición:
#
# - Se distinguen dos subconjuntos:
#
#       A
#
#   subconjunto cuya contribución se desea evaluar.
#
#       P
#
#   subconjunto cuya influencia sobre A se desea analizar.
#
# - Para cada combinación seleccionada se comparan
#   simultáneamente:
#
#       Sh_A^j
#
#       Sh_{P→A}^j
#
#       Sh_{AUS(P)→A}^j
#
# Metodología:
# - El usuario selecciona explícitamente los subconjuntos A y P.
# - Se verifica que ambos subconjuntos sean válidos.
# - Se comprueba que no exista solapamiento entre ellos:
#
#       A ∩ P = ∅
#
# - Se calcula el valor de Shapley por órdenes asociado a A.
# - Se calcula el valor de Shapley condicionado por presencia de P.
# - Se calcula el valor de Shapley condicionado por ausencia de P.
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
# Interpretación:
# - La curva correspondiente a Sh_A^j representa el comportamiento
#   global del subconjunto A.
# - La curva correspondiente a Sh_{P→A}^j representa el comportamiento
#   de A cuando las variables de P están presentes.
# - La curva correspondiente a Sh_{AUS(P)→A}^j representa el
#   comportamiento de A cuando las variables de P están ausentes.
# - Las diferencias observadas entre las tres curvas permiten
#   identificar relaciones de dependencia, refuerzo, inhibición,
#   redundancia o complementariedad entre variables.
#
# Validación:
# - Se verifica la definición correcta de los subconjuntos A y P.
# - Se comprueba que A y P no compartan variables.
# - Se verifica la disponibilidad de todas las variables implicadas en
#   la tabla Δ(S).
# - Se garantiza la obtención conjunta de los resultados asociados a
#   órdenes, presencia y ausencia.
#
# Resultado:
# - Comparación gráfica simultánea de:
#
#       Sh_A^j
#       Sh_{P→A}^j
#       Sh_{AUS(P)→A}^j
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
  tab_ord <- calcular_shapley_ordenes_A(
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

