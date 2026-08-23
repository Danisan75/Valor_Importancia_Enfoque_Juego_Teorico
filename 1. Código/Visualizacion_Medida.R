################################################################################
################################################################################
################ Representación bidimensional – Shapley por Órdenes ###########
################################################################################
################################################################################

###############################################################################
# PASO 0. TABLA BASE (SELECCIONABLE)
###############################################################################

tabla_T_base <- M4_Delta_glm_yb_stream

stopifnot(is.data.frame(tabla_T_base))

###############################################################################
# PASO 1. NORMALIZADOR DE TABLAS Δ(S)
# - Acepta T(...) y T_S_*
# - Convierte TODO a T(...)
###############################################################################
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

###############################################################################
# PASO 2. FIJAR INSTANCIA
###############################################################################
row_instancia_ordenes <- 1    # elijo la instancia 

delta_info <- normalizar_tabla_delta(tabla_T_base)
tabla_T <- delta_info$tabla_delta

tabla_T_instancia <- tabla_T[row_instancia_ordenes, , drop = FALSE]
stopifnot(nrow(tabla_T_instancia) == 1)

###############################################################################
# PASO 3. RECONSTRUIR COALICIONES (CANÓNICAS)
#
# Este bloque permite reconstruir coaliciones a partir de nombres en formato
# texto del tipo "T(A_B_C)", que suelen aparecer en matrices de coaliciones,
# modelos cooperativos o cálculos tipo Shapley.
#
# PROBLEMA:
# - Las coaliciones vienen codificadas como texto
# - El orden de los elementos puede variar ("A_B" vs "B_A")
# - Existen coaliciones vacías ("T()")
# - No se pueden usar directamente para comparar o calcular
#
# SOLUCIÓN:
# 1) Convertir el texto en vectores de R (estructura usable)
#    "T(A_B_C)" -> c("A","B","C")
#
# 2) Canonizar las coaliciones (ordenarlas)
#    c("B","A") -> c("A","B")
#    Esto evita duplicados lógicos y permite comparar correctamente
#
# 3) Tratar explícitamente el caso vacío
#    "T()" -> character(0)
#
# 4) Mantener los nombres originales para trazabilidad
#
# DETALLES TÉCNICOS CLAVE:
# - gsub("^T\\(|\\)$", "", x) elimina "T(" y ")"
# - strsplit(..., "_") separa los elementos
# - [[1]] extrae el vector de la lista resultante
# - sort() impone una representación única (canónica)
###############################################################################

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

###############################################################################
# PASO 4. SHAPLEY POR ÓRDENES 
###############################################################################
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

###############################################################################
# PASO 4. SHAPLEY POR ÓRDENES
#
# Este bloque calcula el valor de Shapley de una variable objetivo descomponido
# por órdenes (tamaño de la coalición previa), a partir de una tabla T(S).
#
# CONTEXTO:
# - Se dispone de valores T(S) asociados a cada coalición S
# - Cada coalición viene en formato "T(A_B_C)"
# - Se quiere medir la contribución marginal de una variable (var_obj)
#
# PROBLEMA:
# - El valor de Shapley estándar agrega todas las contribuciones
# - No permite ver cómo influye el tamaño de la coalición
#
# SOLUCIÓN:
# - Se descompone el Shapley en función del número de elementos previos (orden j)
# - Para cada coalición S que contiene a var_obj:
#     Se calcula la contribución marginal:
#
#         T(S ∪ {i}) - T(S)
#
#   donde:
#     i = variable objetivo
#     S = coalición sin la variable
#
# - Luego se agrupan las contribuciones según:
#
#     j = tamaño de S
#
# - Para cada j se calcula la media de contribuciones:
#
#     Sh(j) = media de contribuciones marginales con |S| = j
#
# RESULTADO:
# - Un data.frame con:
#     j  → tamaño de la coalición previa
#     Sh → contribución media en ese orden
#
# DETALLES TÉCNICOS CLAVE:
# - Se filtran solo coaliciones donde aparece var_obj
# - Se reconstruyen coaliciones con reconstruir_combos_desde_T()
# - Se usa una clave tipo "A|B|C" para localizar subconjuntos rápidamente
# - Caso especial j=0 → se usa T0 (valor base)
# - Se calcula la media por cada orden
###############################################################################

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
###############################################################################
# PASO 6. BUCLE PARA TODAS LAS VARIABLES
#
# Este bloque automatiza el cálculo del Shapley por órdenes para todas las
# variables presentes en T(S), evitando tener que hacerlo manualmente una a una.
#
# CONTEXTO:
# - Se dispone de una tabla con columnas tipo "T(A_B_C)"
# - Ya existe una función que calcula Shapley por órdenes para una variable
# - Se quiere aplicar ese cálculo a todas las variables automáticamente
#
# PROBLEMA:
# - Las variables no están explícitas → están dentro de los nombres T(...)
# - No sabemos a priori cuáles son ni cuántas hay
# - Hay que:
#     1) extraerlas
#     2) iterar sobre ellas
#     3) guardar resultados
#     4) generar visualizaciones
#
# SOLUCIÓN:
# 
# 1) EXTRAER VARIABLES:
#    - Se leen todas las columnas T(...)
#    - Se reconstruyen las coaliciones internamente
#    - Se extraen todos los elementos únicos
#
# 2) CREAR IDENTIFICADOR NUMÉRICO:
#    - Se asigna un ID (1,2,3,...) a cada variable
#    - Útil para gráficos o reporting
#
# 3) ITERAR SOBRE VARIABLES:
#    Para cada variable:
#       - Se calcula su Shapley por órdenes
#       - Se guarda en una lista de tablas
#       - Se genera su gráfico
#
# 4) ALMACENAR RESULTADOS:
#    - lista_tablas → resultados numéricos
#    - lista_plots  → visualizaciones
#
# RESULTADO:
# - Un conjunto completo de Shapley descompuesto por variable
# - Todo estructurado y reutilizable
#
# DETALLES TÉCNICOS CLAVE:
# - grep("^T\\(", ...) identifica columnas de coaliciones
# - gsub limpia el formato "T(...)"
# - strsplit separa elementos
# - unique + unlist obtiene variables únicas
# - seq_along crea IDs consistentes
# - listas permiten almacenar resultados dinámicamente
###############################################################################

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


##################################################################################
##################################################################################
####################### Por ordenes varias #######################################
##################################################################################
##################################################################################

################################################################################
################################################################################
############ SHAPLEY POR ÓRDENES – TODOS LOS SUBCONJUNTOS ######################
################################################################################
################################################################################

# Este bloque generaliza el cálculo de valores de Shapley por órdenes a un
# conjunto arbitrario de variables A (no sólo variables individuales).
#
# CONTEXTO:
# - Se trabaja con valores T(S), donde S es una coalición de variables
# - Cada T(S) representa un valor agregado asociado a la combinación S
# - Las coaliciones están codificadas como "T(A_B_C)"
#
# OBJETIVO:
# - Calcular la contribución marginal media de un conjunto A de variables
#   condicionada al tamaño de la coalición previa S
#
# DEFINICIÓN FORMAL:
#   Sh_A^j = E[ T(S ∪ A) − T(S) | |S| = j ]
#
# donde:
# - A: conjunto de variables objetivo (vector de nombres)
# - S: subconjunto que NO contiene elementos de A
# - j: tamaño de S
#
# INTERPRETACIÓN:
# - Para cada orden j:
#     Se evalúa cuánto aporta añadir A a todas las coaliciones S de tamaño j
# - Luego se promedia esa contribución marginal
#
# DIFERENCIA CLAVE frente al caso univariante:
# - Aquí A puede tener varias variables
# - Se excluyen todas las coaliciones S que ya contengan alguna variable de A
#
# LÓGICA DEL ALGORITMO:
# 1) Extraer columnas T(S) y sus valores
# 2) Reconstruir coaliciones en forma canónica (ordenadas)
# 3) Crear un índice eficiente S → posición
# 4) Identificar el conjunto total de variables
# 5) Validar que A esté contenido en el universo
# 6) Recorrer todas las coaliciones S:
#    - ignorar S si intersecta con A
#    - construir S ∪ A
#    - calcular contribución marginal T(SA) − T(S)
#    - agrupar por tamaño j = |S|
# 7) Calcular la media por cada orden j
#
# CASOS IMPORTANTES:
# - S = ∅ → se usa T0 (valor base)
# - Si falta alguna coalición S ∪ A → se ignora
# - Puede haber órdenes sin observaciones → NA
#
# RESULTADO:
# - data.frame con:
#     j  → tamaño del subconjunto previo
#     Sh → contribución media del conjunto A en ese orden
#
# UTILIDAD:
# - Permite analizar interacciones entre variables
# - Identifica efectos marginales condicionados al contexto
# - Base para análisis avanzados de importancia de variables
################################################################################

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

###############################################################################
# PASO 2. MAPEO ÚNICO: NOMBRE DE VARIABLE -> ID NUMÉRICO
#
# Este bloque tiene como objetivo identificar automáticamente todas las
# variables presentes en las coaliciones T(S) y asignarles un identificador
# numérico único.
#
# CONTEXTO:
# - Las variables no están explícitas en columnas separadas
# - Están codificadas dentro de nombres tipo "T(A_B_C)"
# - Cada coalición contiene una combinación de variables
#
# PROBLEMA:
# - No existe una lista explícita de variables
# - No hay identificadores numéricos disponibles (útiles para gráficos o modelos)
# - Extraerlas manualmente sería costoso y propenso a errores
#
# SOLUCIÓN:
#
# 1) EXTRAER TODAS LAS COLUMNAS T(S):
#    - Se identifican mediante un patrón "T(...)"
#
# 2) RECONSTRUIR COALICIONES:
#    - Se elimina el formato "T(...)"
#    - Se separan las variables internas por "_"
#    - Se obtiene una lista de todas las combinaciones
#
# 3) OBTENER VARIABLES ÚNICAS:
#    - Se unen todas las coaliciones
#    - Se eliminan duplicados
#    - Se ordenan alfabéticamente para consistencia
#
# 4) GENERAR IDS NUMÉRICOS:
#    - Se asigna un número a cada variable: 1, 2, 3, ...
#    - Se crea un vector con nombres para acceder fácilmente
#
# RESULTADO:
# - Un vector tipo:
#     Age -> 1
#     Rac -> 2
#     Cho -> 3
#
# USOS:
# - Etiquetar gráficos
# - Construir matrices
# - Integrar con modelos numéricos
#
# DETALLES TÉCNICOS CLAVE:
# - grep("^T\\(", ...) filtra columnas de coaliciones
# - gsub elimina el wrapper "T(...)"
# - strsplit separa variables dentro de la coalición
# - unlist aplana la lista de coaliciones
# - unique elimina duplicados
# - sort garantiza orden consistente
# - names(var_ids) permite acceso directo: var_ids["Age"]
###############################################################################

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

###############################################################################
# PASO 3. FUNCIÓN SEGURA PARA CONSTRUIR ETIQUETAS plotmath (SOLO IDs)
#
# Este bloque construye etiquetas dinámicas compatibles con plotmath (usado en
# gráficos de R, especialmente con ggplot2) para representar valores de Shapley
# por órdenes cuando se trabaja con identificadores numéricos en lugar de nombres.
#
# CONTEXTO:
# - En visualizaciones avanzadas, las expresiones matemáticas deben generarse
#   como texto interpretable por plotmath
# - Se quiere representar expresiones tipo:
#
#     Sh^{j}({i}) · (Δ)
#     Sh^{j}({i,j}) · (Δ)
#
# - Pero usando IDs numéricos en lugar de nombres de variables
#
# PROBLEMA:
# - plotmath tiene reglas estrictas de sintaxis
# - Cuando hay varios elementos, no se pueden escribir directamente (ej: "1,2")
# - Es necesario usar list() para representar conjuntos múltiples
# - Si no se respeta esta sintaxis, la etiqueta falla o no se renderiza
#
# SOLUCIÓN:
# - Construir el interior de la etiqueta de forma condicional:
#
#     Caso 1 elemento:
#         1  → correcto directamente
#
#     Caso múltiples elementos:
#         list(1,2) → requerido por plotmath
#
# - Envolver el resultado en una expresión completa:
#
#     Sh[group('{', ..., '}')]^j * group('(', list(Delta), ')')
#
# INTERPRETACIÓN:
# - group('{', ..., '}') representa el conjunto A
# - superíndice j indica el orden
# - (Delta) representa el incremento marginal
#
# RESULTADO:
# - Devuelve un string listo para ser usado en:
#     labs(title = ...)
#     annotate()
#     ggplot + parse = TRUE
#
# EJEMPLOS:
# - build_label_shapley_ids(1)
#     → Sh^{j}({1}) · (Δ)
#
# - build_label_shapley_ids(c(1,2))
#     → Sh^{j}({1,2}) · (Δ)
#
# DETALLE CLAVE:
# - El uso de list() en múltiples elementos es obligatorio en plotmath
###############################################################################

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

###############################################################################
# FUNCIÓN DE GRÁFICA – TÍTULO CON NOMBRE + IDs
#
# Este bloque genera una visualización del Shapley por órdenes para un conjunto
# de variables A, combinando correctamente dos niveles de información:
#
# 1) REPRESENTACIÓN MATEMÁTICA (plotmath):
#    - Se utiliza una etiqueta construida dinámicamente
#    - Muestra únicamente los IDs numéricos del conjunto A
#    - Formato: Sh^{j}({IDs}) · (Δ)
#    - Es importante usar IDs (y no nombres) porque plotmath requiere sintaxis
#      estricta y controlada para renderizar correctamente expresiones
#
# 2) TÍTULO DEL GRÁFICO (HUMANO):
#    - Incluye los nombres originales de las variables
#    - Añade los IDs entre corchetes para trazabilidad
#    - Ejemplo: "Age_Race [1,2]"
#
# PROBLEMA QUE RESUELVE:
# - Diferenciar claramente entre:
#     * representación matemática (precisa, técnica)
#     * representación descriptiva (legible, interpretativa)
#
# - Evitar errores de renderizado en plotmath al usar strings complejos
#
# - Mantener coherencia entre el cálculo interno (IDs) y la interpretación
#
# LÓGICA DEL FLUJO:
# - Se reciben:
#     data → tabla con columnas (j, Sh)
#     A    → nombres de variables (ej: c("Age","Cho"))
#     fila → identificador de instancia analizada
#
# - Se convierten nombres → IDs usando var_ids
# - Se construye etiqueta matemática con build_label_shapley_ids()
# - Se construye título combinando nombres e IDs
# - Se genera gráfico con:
#     * puntos (geom_point)
#     * línea (geom_line)
#     * anotación matemática (annotate + parse=TRUE)
#
# RESULTADO:
# - Gráfico consistente, interpretable y correcto en términos matemáticos
#
# DETALLES TÉCNICOS CLAVE:
# - annotate(..., parse = TRUE) permite interpretar plotmath
# - max(data$j) ajusta posición dinámica del texto
# - na.rm = TRUE evita errores si hay NA
# - var_ids[A] mantiene correspondencia exacta nombre → ID
###############################################################################

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

###############################################################################
# PASO 5. GENERAR TODOS LOS SUBCONJUNTOS A (NO VACÍOS)
#
# Este bloque genera automáticamente todos los subconjuntos posibles del conjunto
# de variables disponibles (vars), excluyendo el conjunto vacío.
#
# CONTEXTO:
# - Se dispone de un conjunto de variables (vars), por ejemplo:
#     vars = c("Age", "Rac", "Cho")
# - Para análisis de Shapley generalizado, es necesario evaluar no sólo variables
#   individuales, sino también combinaciones (pares, tríos, etc.)
#
# PROBLEMA:
# - Generar manualmente todas las combinaciones posibles es inviable cuando
#   crece el número de variables
# - Se necesita una forma automática, ordenada y completa de generar:
#
#     {Age}, {Rac}, {Cho}
#     {Age, Rac}, {Age, Cho}, {Rac, Cho}
#     {Age, Rac, Cho}
#
# - Pero excluyendo el conjunto vacío {}
#
# SOLUCIÓN:
#
# 1) Se recorren todos los tamaños posibles de subconjunto:
#       k = 1, 2, ..., n
#
# 2) Para cada tamaño k, se generan todas las combinaciones posibles usando:
#       combn(vars, k)
#
# 3) Se devuelve cada combinación como vector (no como matriz)
#
# 4) Se aplana la lista de listas en una única lista final
#
# RESULTADO:
# - lista_A es una lista donde:
#     cada elemento es un subconjunto A (vector de variables)
#
# EJEMPLO:
# - vars = c("Age","Rac","Cho")
#
# - lista_A =
#     [[1]] "Age"
#     [[2]] "Rac"
#     [[3]] "Cho"
#     [[4]] c("Age","Rac")
#     [[5]] c("Age","Cho")
#     [[6]] c("Rac","Cho")
#     [[7]] c("Age","Rac","Cho")
#
# USOS:
# - Iterar sobre todos los subconjuntos A
# - Calcular Shapley generalizado
# - Analizar interacciones de cualquier orden
#
# DETALLES TÉCNICOS CLAVE:
# - seq_along(vars) genera longitudes desde 1 a n
# - combn(..., simplify = FALSE) devuelve listas (no matrices)
# - lapply aplica la generación para cada tamaño k
# - unlist(..., recursive = FALSE) aplana un nivel sin romper vectores internos
###############################################################################

lista_A <- unlist(
  lapply(seq_along(vars),
         function(k) combn(vars, k, simplify = FALSE)),
  recursive = FALSE
)

###############################################################################
# PASO 6. BUCLE GLOBAL: CALCULAR Y GRAFICAR TODO (SIN ERRORES)
#
# Este bloque ejecuta el pipeline completo del análisis de Shapley por órdenes
# para TODOS los subconjuntos de variables previamente generados (lista_A).
#
# CONTEXTO:
# - Se dispone de:
#     * lista_A → todos los subconjuntos no vacíos de variables
#     * función calcular_shapley_ordenes_A() → calcula Sh_A^j
#     * función plot_sh_por_ordenes_A() → genera la gráfica
#
# OBJETIVO:
# - Automatizar el cálculo y visualización para cada subconjunto A
# - Evitar ejecución manual (que sería inviable con muchas combinaciones)
#
# PROBLEMA:
# - El número de subconjuntos crece exponencialmente (2^n - 1)
# - Se necesita:
#     * calcular resultados de forma sistemática
#     * almacenarlos correctamente
#     * generar gráficos consistentes
#
# SOLUCIÓN:
#
# 1) INICIALIZACIÓN:
#    - lista_tablas → almacena resultados numéricos (Sh_A^j)
#    - lista_plots  → almacena gráficos generados
#
# 2) ITERACIÓN SOBRE TODOS LOS SUBCONJUNTOS A:
#    Para cada A:
#
#    a) Se construye un nombre único (clave) usando:
#         "Age_Rac", "Age_Cho", etc.
#
#    b) Se calcula el Shapley por órdenes:
#         Sh_A^j = E[ T(S ∪ A) − T(S) | |S| = j ]
#
#    c) Se guarda la tabla en lista_tablas usando el nombre como clave
#
#    d) Se genera el gráfico asociado:
#         - Usa IDs numéricos internamente
#         - Usa nombres en el título
#
#    e) Se guarda el gráfico en lista_plots
#
#    f) Se imprime directamente (visualización inmediata)
#
# RESULTADO:
# - lista_tablas:
#     Contiene todas las tablas Sh_A^j por subconjunto
#
# - lista_plots:
#     Contiene todos los gráficos generados
#
# - Salida visual:
#     Se muestran todos los gráficos en la consola uno detrás de otro
#
# DETALLES TÉCNICOS CLAVE:
# - paste(A, collapse = "_") crea identificadores únicos
# - Las listas permiten almacenar estructuras heterogéneas
# - El bucle for garantiza control total y robustez
# - print(p) es necesario para forzar la visualización en loops
#
# CONSIDERACIÓN IMPORTANTE:
# - El coste computacional puede ser elevado si el número de variables crece
# - Número de subconjuntos = 2^n - 1
###############################################################################

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

###############################################################################
# SHAPLEY POR PRESENCIA (P → A) POR ÓRDENES
#
# Este bloque extiende el cálculo de Shapley por órdenes incorporando una
# condición de PRESENCIA previa: se evalúa el efecto de añadir A únicamente
# sobre coaliciones S que YA contienen un conjunto P.
#
# CONTEXTO:
# - Se dispone de valores T(S) para todas las coaliciones posibles
# - Se quiere analizar el impacto de A en un contexto condicionado
#
# OBJETIVO:
# - Medir el efecto marginal de añadir A dado que ciertas variables P ya están
#   presentes en la coalición S
#
# DEFINICIÓN FORMAL:
#   Sh_{P→A}^j =
#     E[ T(S ∪ A) − T(S)
#        | |S| = j, P ⊆ S, A ∩ S = ∅ ]
#
# INTERPRETACIÓN:
# - Estamos midiendo cuánto aporta A cuando:
#     * P ya está dentro del sistema (condición obligatoria)
#     * A todavía no está presente (evitar doble conteo)
#     * S tiene tamaño j (análisis por órdenes)
#
# DIFERENCIA CLAVE:
# - No se consideran todas las coaliciones
# - Solo aquellas que cumplen:
#       P ⊆ S        (presencia obligatoria)
#       A ∩ S = ∅    (no solapamiento)
#
# INTUICIÓN:
# - Responde preguntas del tipo:
#     "¿Cuál es el efecto de añadir A cuando ya está presente P?"
#
# - Esto permite analizar:
#     * dependencias
#     * efectos condicionales
#     * interacciones dirigidas
#
# LÓGICA DEL ALGORITMO:
# - Reconstruye todas las coaliciones en formato vector
# - Filtra solo aquellas S que cumplen:
#       * contienen P
#       * no contienen A
# - Construye S ∪ A
# - Calcula contribución marginal:
#       T(S ∪ A) − T(S)
# - Agrupa por tamaño de S (orden j)
# - Calcula la media por orden
#
# VALIDACIONES:
# - P debe estar contenido en las variables del problema
# - A debe estar contenido en las variables del problema
# - P y A no pueden compartir elementos
#
# CASOS IMPORTANTES:
# - S = ∅ sólo es válido si P también es vacío
# - Si falta alguna coalición S ∪ A → se ignora
# - Si un orden no tiene contribuciones → NA
#
# RESULTADO:
# - data.frame con:
#     j  → tamaño de S
#     Sh → contribución media condicionada por presencia
#
# UTILIDAD:
# - Análisis de efectos bajo condiciones iniciales
# - Estudio de dependencias entre variables
# - Descomposición avanzada de interacciones
###############################################################################

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
# GRÁFICA FINAL: ÓRDENES + PRESENCIA (VERSIÓN CORRECTA Y ESTABLE)
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
  # 3. TÍTULO PROFESIONAL
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
# BUCLES DE PRESENCIA – COBERTURA COMPLETA Y ORDEN PROFESIONAL
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

###############################################################################
# SHAPLEY POR AUSENCIA (Q → A) POR ÓRDENES
#
# Este bloque calcula el valor de Shapley por órdenes condicionado a la
# AUSENCIA de un conjunto de variables Q.
#
# CONTEXTO:
# - Se dispone de valores T(S) para todas las coaliciones posibles
# - Se quiere analizar el impacto de un conjunto A bajo la condición de que
#   ciertas variables Q NO estén presentes en la coalición
#
# OBJETIVO:
# - Medir el efecto marginal de añadir A únicamente sobre coaliciones S que:
#       * NO contienen variables de Q (ausencia)
#       * NO contienen variables de A (consistencia del cálculo)
#
# DEFINICIÓN FORMAL:
#   Sh_{AUS(Q)→A}^j =
#     E[ T(S ∪ A) − T(S)
#        | |S| = j, Q ∩ S = ∅, A ∩ S = ∅ ]
#
# INTERPRETACIÓN:
# - Se mide el impacto de A en un contexto donde Q está explícitamente ausente
# - Permite responder preguntas como:
#     "¿Qué efecto tiene A cuando Q no está presente?"
#
# DIFERENCIA CLAVE RESPECTO A PRESENCIA:
# - PRESENCIA:
#       P ⊆ S        → variables obligatorias en S
#
# - AUSENCIA:
#       Q ∩ S = ∅    → variables prohibidas en S
#
# INTUICIÓN:
# - Sirve para detectar:
#     * dependencia negativa (A solo funciona sin Q)
#     * sustitución entre variables
#     * redundancias en el modelo
#
# LÓGICA DEL ALGORITMO:
# 1) Reconstruir todas las coaliciones S (forma canónica)
# 2) Filtrar coaliciones válidas:
#       * S no contiene ningún elemento de Q
#       * S no contiene elementos de A
# 3) Para cada coalición válida:
#       * construir S ∪ A
#       * calcular contribución marginal:
#             T(S ∪ A) − T(S)
# 4) Agrupar por tamaño de S (orden j)
# 5) Calcular la media por cada orden
#
# VALIDACIONES:
# - Q debe estar contenido en el conjunto de variables
# - A debe estar contenido en el conjunto de variables
# - Q y A no pueden solaparse
#
# CONTROL DE LÍMITES:
# - max_j = n - |A| - |Q|
#   Esto evita construir coaliciones imposibles
#
# - Si max_j < 0:
#     → no existen coaliciones válidas
#     → se devuelve un data.frame vacío
#
# CASOS IMPORTANTES:
# - S = ∅ → se usa T0 como valor base
# - Si falta S ∪ A → se ignora (robustez)
# - Si no hay contribuciones en un orden → NA
#
# RESULTADO:
# - data.frame con:
#     j  → tamaño de la coalición S
#     Sh → contribución media en ese orden bajo ausencia de Q
#
# UTILIDAD:
# - Analizar efectos de exclusión
# - Detectar variables sustitutas o redundantes
# - Evaluar robustez del modelo ante ausencia de variables
###############################################################################


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

###############################################################################
# FUNCIONES AUXILIARES: CONSTRUCCIÓN SEGURA DE ETIQUETAS plotmath Y TEXTOS GRID
#
# Este bloque define dos funciones auxiliares fundamentales para trabajar con
# visualizaciones avanzadas que usan expresiones matemáticas en R:
#
# 1) build_group_ids:
#    - Construye correctamente la representación de conjuntos en plotmath
#    - Soporta automáticamente:
#         * un solo elemento  → {1}
#         * múltiples elementos → {1,2,3}
#
#    PROBLEMA:
#    - plotmath NO acepta directamente vectores tipo "1,2"
#    - Cuando hay más de un elemento, exige usar list()
#
#    SOLUCIÓN:
#    - Si hay un solo ID:
#         group('{', 1, '}')
#
#    - Si hay varios:
#         group('{', list(1,2,3), '}')
#
#    RESULTADO:
#    - Devuelve un string válido para plotmath listo para usar con parse = TRUE
#
#
# 2) textGrob_math:
#    - Crea un objeto gráfico (grob) que renderiza expresiones plotmath dentro
#      del sistema grid de R
#
#    CONTEXTO:
#    - ggplot2 usa internamente grid
#    - Para layouts avanzados (ej: grid.arrange, facetting manual, etc.)
#      es necesario crear objetos gráficos directamente
#
#    PROBLEMA:
#    - plotmath necesita convertirse en expresión antes de renderizarse
#    - annotate() no siempre es suficiente en layouts complejos
#
#    SOLUCIÓN:
#    - parse(text = label) → convierte string a expresión
#    - as.expression() → formato compatible con grid
#    - textGrob() → crea el objeto gráfico renderizable
#
#    PARÁMETROS:
#    - label   → string en formato plotmath
#    - col     → color del texto
#    - fontsize→ tamaño del texto
#    - x, y    → posición relativa (0 a 1, sistema "npc")
#    - hjust   → alineación horizontal
#    - vjust   → alineación vertical
#
#    RESULTADO:
#    - Devuelve un grob listo para usar en grid.draw(), arrangeGrob(), etc.
#
# UTILIDAD GLOBAL:
# - Evita errores de sintaxis en plotmath
# - Centraliza la lógica de representación matemática
# - Permite escalar a gráficos complejos (dashboards, layouts, etc.)
###############################################################################

# -----------------------------------------------------------------------------
# Construye correctamente {i} o {i,j,k} en plotmath
# -----------------------------------------------------------------------------

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

###############################################################################
# EXPORTACIÓN A PDF DEL ANÁLISIS COMPLETO (ÓRDENES + PRESENCIA + AUSENCIA)
#
# Este bloque construye un documento PDF profesional que incluye:
#
#   - Portada explicativa
#   - Separadores de secciones
#   - Todas las gráficas generadas en el análisis:
#       * Shapley por órdenes
#       * Shapley por presencia
#       * Shapley por ausencia
#
# en un formato ordenado y listo para lectura (tipo informe / tesis / auditoría).
#
# OBJETIVO:
# - Automatizar completamente la generación de informes
# - Evitar exportación manual de gráficos
# - Generar un documento estructurado y reproducible
#
# COMPONENTES DEL BLOQUE:
#
# 1) GESTIÓN DEL PDF:
#    - open_pdf() → abre dispositivo gráfico PDF
#    - close_pdf() → lo cierra correctamente
#    - Se usa on.exit() para asegurar cierre incluso si hay error
#
# 2) PÁGINAS DE TEXTO:
#    - print_text_page():
#        * crea páginas tipo portada/separador
#        * usa grid (no ggplot)
#        * permite incluir título, subtítulo y texto explicativo
#
# 3) FUNCION PRINCIPAL:
#    exportar_presencia_ausencia_pdf()
#
#    - Orquesta todo el proceso:
#        * abre PDF
#        * genera portada
#        * divide en secciones
#        * recorre todas las combinaciones (A, P)
#        * imprime cada gráfico
#
# ESTRUCTURA DEL DOCUMENTO:
#
# 1) PORTADA:
#    - título del análisis
#    - fila analizada
#    - texto metodológico (editable)
#
# 2) SECCIONES:
#    - Relación 1 a 1
#    - Relación 1 a varias
#    - Relación varias a 1
#    - Relación varias a varias
#
# 3) CONTENIDO:
#    - cada gráfico ocupa una página completa
#    - orden coherente para facilitar lectura
#
# DETALLES TÉCNICOS IMPORTANTES:
#
# - onefile = TRUE:
#     todo va en un único PDF multipágina
#
# - useDingbats = FALSE:
#     evita problemas de tipografía en algunos visores
#
# - grid.newpage():
#     fuerza salto de página
#
# - print():
#     necesario para renderizar ggplot dentro del PDF
#
# ROBUSTEZ:
# - crea directorios automáticamente si no existen
# - evita fugas de dispositivos gráficos
# - mantiene consistencia con todo el pipeline anterior
#
# RESULTADO:
# - PDF completo, ordenado y listo para:
#     * documentación
#     * presentación
#     * análisis posterior
#
# LIMITACIÓN PRÁCTICA:
# - El número de páginas puede crecer mucho (combinaciones exponenciales)
#
# USO RECOMENDADO:
# - análisis controlado (filtrar vars si es necesario)
# - uso en validación de modelos
# - generación de informes reproducibles
###############################################################################

###############################################################################
# PDF helpers
###############################################################################

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
  ruta_pdf = "C:\\Users\\danis\\OneDrive\\Escritorio\\Phd\\4.1. Escritura de Tesis\\Real Case. Resultados\\Shap_Graficos.pdf",
 #ruta_pdf = "C:\\Users\\ruta\\Archivo.pdf",
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




################################## Elegir variables ####################################
########################################################################################
########################################################################################
###############################################################################

###############################################################################
# CHECK MANUAL DEFINITIVO
#
# A : variable(s) analizada(s)
# P : variable(s) que influyen
#
# Se comparan en una sola gráfica:
#   - ÓRDENES (baseline)
#   - PRESENCIA de P
#   - AUSENCIA de P
###############################################################################
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

