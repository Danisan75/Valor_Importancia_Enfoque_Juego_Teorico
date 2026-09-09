# ============================================================================
# 1. CÁLCULO DEL VALOR DE SHAPLEY
# ============================================================================
#
# Objetivo:
# Calcular el valor de Shapley asociado a cada variable a partir de las
# contribuciones Δ(S) obtenidas para todas las coaliciones posibles.
#
# Motivación:
# - La tabla Δ(S) contiene la contribución asociada a cada coalición de
#   variables.
# - El valor de Shapley permite repartir la contribución total del
#   modelo entre las variables participantes de forma consistente con la
#   teoría de juegos cooperativos.
#
# Definición:
#
#                    |S|! (n-|S|-1)!
#     Sh_i = Σ ---------------------- · [Δ(S ∪ {i}) − Δ(S)]
#            S⊆N\{i}        n!
#
# donde:
#
# - i representa la variable analizada.
# - S representa una coalición que no contiene a la variable i.
# - n representa el número total de variables.
#
# Metodología:
# - Se parte de la tabla de contribuciones Δ(S).
# - Para cada variable se identifican todas las coaliciones que no la
#   contienen.
# - Se calcula su contribución marginal:
#
#       Δ(S ∪ {i}) − Δ(S)
#
# - Cada contribución marginal se pondera mediante los coeficientes del
#   valor de Shapley.
# - Las contribuciones ponderadas se agregan para obtener el valor de
#   Shapley de la variable.
# - El procedimiento se repite para todas las variables del modelo.
#
# Validación:
# - Se verifica la propiedad de eficiencia:
#
#       Σ Sh_i = Δ(N) − Δ(∅)
#
# - La suma de los valores de Shapley debe coincidir con la contribución
#   total del modelo.
#
# Resultado:
# - Valor de Shapley para cada variable.
# - Verificación de la propiedad de eficiencia.
# - Tabla final enriquecida con las variables Shap_*.
#
# ============================================================================

# ================================
# SHAPLEY METODO I - LM
# ================================



Archivo<-M1_Delta_lm_y_stream

############ Age ###############

Shap_Age <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Age)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Age_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Age_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Rac_Cho_Ris_Sex)"]]
  )



################# Race

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Rac)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Cho_Ris_Sex)"]]
  )

############## Cho

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Cho)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Ris_Sex)"]]
  )

############# Ris

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Ris)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Ris)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Sex)"]]
  )

############# Sex

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Sex)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Sex)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Ris)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Ris)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Ris)"]]
  )


suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] - Archivo[["T()"]]

summary(suma_shapley - delta_teorica)

################ Construcción archivo Valor de Shapley M1_lm

M1_Shapley_lm <- Archivo
M1_Shapley_lm$Shap_Age  <- Shap_Age
M1_Shapley_lm$Shap_Race <- Shap_Race
M1_Shapley_lm$Shap_Cho  <- Shap_Cho
M1_Shapley_lm$Shap_Ris  <- Shap_Ris
M1_Shapley_lm$Shap_Sex  <- Shap_Sex


# ================================
# SHAPLEY METODO I - XGB - REGRESION
# ================================

Archivo<-M1_Delta_xgb_y_stream

############ Age ###############

Shap_Age <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Age)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Age_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Age_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Rac_Cho_Ris_Sex)"]]
  )



################# Race

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Rac)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Cho_Ris_Sex)"]]
  )

############## Cho

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Cho)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Ris_Sex)"]]
  )

############# Ris

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Ris)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Ris)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Sex)"]]
  )

############# Sex

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Sex)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Sex)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Ris)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Ris)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Ris)"]]
  )


suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] - Archivo[["T()"]]

summary(suma_shapley - delta_teorica)

################ Construcción archivo Valor de Shapley M1_lm

M1_Shapley_xgb <- Archivo
M1_Shapley_xgb$Shap_Age  <- Shap_Age
M1_Shapley_xgb$Shap_Race <- Shap_Race
M1_Shapley_xgb$Shap_Cho  <- Shap_Cho
M1_Shapley_xgb$Shap_Ris  <- Shap_Ris
M1_Shapley_xgb$Shap_Sex  <- Shap_Sex



# ================================
# SHAPLEY METODO I - GLM
# ================================

Archivo<-M1_Delta_glm_yb_stream

############ Age ###############

Shap_Age <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Age)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Age_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Age_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Rac_Cho_Ris_Sex)"]]
  )



################# Race

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Rac)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Cho_Ris_Sex)"]]
  )

############## Cho

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Cho)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Ris_Sex)"]]
  )

############# Ris

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Ris)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Ris)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Sex)"]]
  )

############# Sex

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Sex)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Sex)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Ris)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Ris)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Ris)"]]
  )


suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] - Archivo[["T()"]]

summary(suma_shapley - delta_teorica)

################ Construcción archivo Valor de Shapley M1_lm

M1_Shapley_glm_yb <- Archivo
M1_Shapley_glm_yb$Shap_Age  <- Shap_Age
M1_Shapley_glm_yb$Shap_Race <- Shap_Race
M1_Shapley_glm_yb$Shap_Cho  <- Shap_Cho
M1_Shapley_glm_yb$Shap_Ris  <- Shap_Ris
M1_Shapley_glm_yb$Shap_Sex  <- Shap_Sex


# ================================
# SHAPLEY METODO I - XGB - CLASIFICACION
# ================================

Archivo<-M1_Delta_xgb_yb_stream

############ Age ###############

Shap_Age <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Age)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Age_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Age_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Rac_Cho_Ris_Sex)"]]
  )



################# Race

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Rac)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Cho_Ris_Sex)"]]
  )

############## Cho

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Cho)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Ris_Sex)"]]
  )

############# Ris

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Ris)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Ris)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Sex)"]]
  )

############# Sex

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Sex)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Sex)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Ris)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Ris)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Ris)"]]
  )


suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] - Archivo[["T()"]]

summary(suma_shapley - delta_teorica)

################ Construcción archivo Valor de Shapley M1_lm

M1_Shapley_xgb_yb <- Archivo
M1_Shapley_xgb_yb$Shap_Age  <- Shap_Age
M1_Shapley_xgb_yb$Shap_Race <- Shap_Race
M1_Shapley_xgb_yb$Shap_Cho  <- Shap_Cho
M1_Shapley_xgb_yb$Shap_Ris  <- Shap_Ris
M1_Shapley_xgb_yb$Shap_Sex  <- Shap_Sex


sum(M1_Shapley_lm[["T(Age)"]]-M1_Shapley_lm[["Shap_Age"]])
sum(M1_Shapley_lm[["T(Rac)"]]-M1_Shapley_lm[["Shap_Rac"]])
sum(M1_Shapley_lm[["T(Cho)"]]-M1_Shapley_lm[["Shap_Cho"]])
sum(M1_Shapley_lm[["T(Ris)"]]-M1_Shapley_lm[["Shap_Ris"]])
sum(M1_Shapley_lm[["T(Sex)"]]-M1_Shapley_lm[["Shap_Sex"]])

sum(M1_Shapley_xgb[["T(Age)"]]-M1_Shapley_xgb[["Shap_Age"]])
sum(M1_Shapley_xgb[["T(Rac)"]]-M1_Shapley_xgb[["Shap_Rac"]])
sum(M1_Shapley_xgb[["T(Cho)"]]-M1_Shapley_xgb[["Shap_Cho"]])
sum(M1_Shapley_xgb[["T(Ris)"]]-M1_Shapley_xgb[["Shap_Ris"]])
sum(M1_Shapley_xgb[["T(Sex)"]]-M1_Shapley_xgb[["Shap_Sex"]])

sum(M1_Shapley_glm_yb[["T(Age)"]]-M1_Shapley_glm_yb[["Shap_Age"]])
sum(M1_Shapley_glm_yb[["T(Rac)"]]-M1_Shapley_glm_yb[["Shap_Rac"]])
sum(M1_Shapley_glm_yb[["T(Cho)"]]-M1_Shapley_glm_yb[["Shap_Cho"]])
sum(M1_Shapley_glm_yb[["T(Ris)"]]-M1_Shapley_glm_yb[["Shap_Ris"]])
sum(M1_Shapley_glm_yb[["T(Sex)"]]-M1_Shapley_glm_yb[["Shap_Sex"]])

sum(M1_Shapley_xgb_yb[["T(Age)"]]-M1_Shapley_xgb_yb[["Shap_Age"]])
sum(M1_Shapley_xgb_yb[["T(Rac)"]]-M1_Shapley_xgb_yb[["Shap_Rac"]])
sum(M1_Shapley_xgb_yb[["T(Cho)"]]-M1_Shapley_xgb_yb[["Shap_Cho"]])
sum(M1_Shapley_xgb_yb[["T(Ris)"]]-M1_Shapley_xgb_yb[["Shap_Ris"]])
sum(M1_Shapley_xgb_yb[["T(Sex)"]]-M1_Shapley_xgb_yb[["Shap_Sex"]])



names(M4_Delta_lm_y_stream)

#################################################################################
############## METODO IV ########################################################
#################################################################################

# ================================
# SHAPLEY METODO IV - LM
# ================================


Archivo <- M4_Delta_lm_y_stream

############ Age ###############

Shap_Age <-
  
  (1/5) * (  
    Archivo[["T_S_Age"]] -
      Archivo[["T_S_empty"]]
  )+
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Cho_Rac_Ris_Sex"]]
  )

################# Race ###############

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Rac"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Ris_Sex"]]
  )

############## Cho ###############

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Cho"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Rac_Ris_Sex"]]
  )

############# Ris ###############

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Ris"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Sex"]]
  )

############# Sex ###############

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Sex"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Ris"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Ris"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Ris"]]
  )

############################
# Comprobación Shapley
############################

suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
  Archivo[["T_S_empty"]]

summary(suma_shapley - delta_teorica)

############################
# Construcción archivo Shapley LM
############################

M4_Shapley_lm <- Archivo
M4_Shapley_lm$Shap_Age  <- Shap_Age
M4_Shapley_lm$Shap_Race <- Shap_Race
M4_Shapley_lm$Shap_Cho  <- Shap_Cho
M4_Shapley_lm$Shap_Ris  <- Shap_Ris
M4_Shapley_lm$Shap_Sex  <- Shap_Sex

# ================================
# SHAPLEY METODO IV - XGB
# ================================


Archivo <- M4_Delta_xgb_y_stream

############ Age ###############

Shap_Age <-
  
  (1/5) * (  
    Archivo[["T_S_Age"]] -
      Archivo[["T_S_empty"]]
  )+
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Cho_Rac_Ris_Sex"]]
  )

################# Race ###############

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Rac"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Ris_Sex"]]
  )

############## Cho ###############

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Cho"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Rac_Ris_Sex"]]
  )

############# Ris ###############

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Ris"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Sex"]]
  )

############# Sex ###############

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Sex"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Ris"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Ris"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Ris"]]
  )

############################
# Comprobación Shapley
############################

suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
  Archivo[["T_S_empty"]]

summary(suma_shapley - delta_teorica)

############################
# Construcción archivo Shapley LM
############################

M4_Shapley_xgb <- Archivo
M4_Shapley_xgb$Shap_Age  <- Shap_Age
M4_Shapley_xgb$Shap_Race <- Shap_Race
M4_Shapley_xgb$Shap_Cho  <- Shap_Cho
M4_Shapley_xgb$Shap_Ris  <- Shap_Ris
M4_Shapley_xgb$Shap_Sex  <- Shap_Sex



# ================================
# SHAPLEY METODO IV - GLM - CLASIFICACION
# ================================


Archivo <- M4_Delta_glm_yb_stream

############ Age ###############

Shap_Age <-
  
  (1/5) * (  
    Archivo[["T_S_Age"]] -
      Archivo[["T_S_empty"]]
  )+
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Cho_Rac_Ris_Sex"]]
  )

################# Race ###############

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Rac"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Ris_Sex"]]
  )

############## Cho ###############

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Cho"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Rac_Ris_Sex"]]
  )

############# Ris ###############

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Ris"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Sex"]]
  )

############# Sex ###############

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Sex"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Ris"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Ris"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Ris"]]
  )

############################
# Comprobación Shapley
############################

suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
  Archivo[["T_S_empty"]]

summary(suma_shapley - delta_teorica)

############################
# Construcción archivo Shapley LM
############################

M4_Shapley_glm_yb <- Archivo
M4_Shapley_glm_yb$Shap_Age  <- Shap_Age
M4_Shapley_glm_yb$Shap_Race <- Shap_Race
M4_Shapley_glm_yb$Shap_Cho  <- Shap_Cho
M4_Shapley_glm_yb$Shap_Ris  <- Shap_Ris
M4_Shapley_glm_yb$Shap_Sex  <- Shap_Sex

# ================================
# SHAPLEY METODO IV - XGB CLASIFICACION
# ================================


Archivo <- M4_Delta_xgb_log_stream

############ Age ###############

Shap_Age <-
  
  (1/5) * (  
    Archivo[["T_S_Age"]] -
      Archivo[["T_S_empty"]]
  )+
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Cho_Rac_Ris_Sex"]]
  )

################# Race ###############

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Rac"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Ris_Sex"]]
  )

############## Cho ###############

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Cho"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Rac_Ris_Sex"]]
  )

############# Ris ###############

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Ris"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Sex"]]
  )

############# Sex ###############

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Sex"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Ris"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Ris"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Ris"]]
  )

############################
# Comprobación Shapley
############################

suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
  Archivo[["T_S_empty"]]

summary(suma_shapley - delta_teorica)

############################
# Construcción archivo Shapley LM
############################

M4_Shapley_xgb_log <- Archivo
M4_Shapley_xgb_log$Shap_Age  <- Shap_Age
M4_Shapley_xgb_log$Shap_Race <- Shap_Race
M4_Shapley_xgb_log$Shap_Cho  <- Shap_Cho
M4_Shapley_xgb_log$Shap_Ris  <- Shap_Ris
M4_Shapley_xgb_log$Shap_Sex  <- Shap_Sex



sum(M4_Shapley_lm[["T_S_Age"]]-M4_Shapley_lm[["Shap_Age"]])
sum(M4_Shapley_lm[["T_S_Rac"]]-M4_Shapley_lm[["Shap_Rac"]])
sum(M4_Shapley_lm[["T_S_Cho"]]-M4_Shapley_lm[["Shap_Cho"]])
sum(M4_Shapley_lm[["T_S_Ris"]]-M4_Shapley_lm[["Shap_Ris"]])
sum(M4_Shapley_lm[["T_S_Sex"]]-M4_Shapley_lm[["Shap_Sex"]])

sum(M4_Shapley_xgb[["T_S_Age"]]-M4_Shapley_xgb[["Shap_Age"]])
sum(M4_Shapley_xgb[["T_S_Rac"]]-M4_Shapley_xgb[["Shap_Rac"]])
sum(M4_Shapley_xgb[["T_S_Cho"]]-M4_Shapley_xgb[["Shap_Cho"]])
sum(M4_Shapley_xgb[["T_S_Ris"]]-M4_Shapley_xgb[["Shap_Ris"]])
sum(M4_Shapley_xgb[["T_S_Sex"]]-M4_Shapley_xgb[["Shap_Sex"]])

sum(M4_Shapley_glm_yb[["T_S_Age"]]-M4_Shapley_glm_yb[["Shap_Age"]])
sum(M4_Shapley_glm_yb[["T_S_Rac"]]-M4_Shapley_glm_yb[["Shap_Rac"]])
sum(M4_Shapley_glm_yb[["T_S_Cho"]]-M4_Shapley_glm_yb[["Shap_Cho"]])
sum(M4_Shapley_glm_yb[["T_S_Ris"]]-M4_Shapley_glm_yb[["Shap_Ris"]])
sum(M4_Shapley_glm_yb[["T_S_Sex"]]-M4_Shapley_glm_yb[["Shap_Sex"]])

sum(M4_Shapley_xgb_log[["T_S_Age"]]-M4_Shapley_xgb_log[["Shap_Age"]])
sum(M4_Shapley_xgb_log[["T_S_Rac"]]-M4_Shapley_xgb_log[["Shap_Rac"]])
sum(M4_Shapley_xgb_log[["T_S_Cho"]]-M4_Shapley_xgb_log[["Shap_Cho"]])
sum(M4_Shapley_xgb_log[["T_S_Ris"]]-M4_Shapley_xgb_log[["Shap_Ris"]])
sum(M4_Shapley_xgb_log[["T_S_Sex"]]-M4_Shapley_xgb_log[["Shap_Sex"]])


###############################################################
###################### Exportar Valor de Shapley ##############




write_xlsx(
  x = M1_Shapley_lm,
  path = "C:/Users/.../M1_Shapley_lm.xlsx"
)
write_xlsx(
  x = M1_Shapley_glm_yb,
  path = "C:/Users/.../M1_Shapley_lm.xlsx"
)
# ================================
# SHAPLEY METODO I - LM
# ================================

Archivo<-M1_Delta_lm_y_stream

############ Age ###############

Shap_Age_ <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Age)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Age_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Age_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Rac_Cho_Ris_Sex)"]]
  )



################# Race

Shap_Race_ <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Rac)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Cho_Ris_Sex)"]]
  )

############## Cho

Shap_Cho_ <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Cho)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Ris_Sex)"]]
  )

############# Ris

Shap_Ris_ <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Ris)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Ris)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Sex)"]]
  )

############# Sex

Shap_Sex_ <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Sex)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Sex)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Ris)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Ris)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Ris)"]]
  )


suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] - Archivo[["T()"]]

summary(suma_shapley - delta_teorica)



################ Construcción archivo Valor de Shapley M1_lm

M1_Shapley_lm <- Archivo
M1_Shapley_lm$Shap_Age  <- Shap_Age
M1_Shapley_lm$Shap_Race <- Shap_Race
M1_Shapley_lm$Shap_Cho  <- Shap_Cho
M1_Shapley_lm$Shap_Ris  <- Shap_Ris
M1_Shapley_lm$Shap_Sex  <- Shap_Sex


# ================================
# SHAPLEY METODO I - XGB - REGRESION
# ================================

Archivo<-M1_Delta_xgb_y_stream

############ Age ###############

Shap_Age <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Age)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Age_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Age_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Rac_Cho_Ris_Sex)"]]
  )



################# Race

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Rac)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Cho_Ris_Sex)"]]
  )

############## Cho

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Cho)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Ris_Sex)"]]
  )

############# Ris

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Ris)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Ris)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Sex)"]]
  )

############# Sex

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Sex)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Sex)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Ris)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Ris)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Ris)"]]
  )


suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] - Archivo[["T()"]]

summary(suma_shapley - delta_teorica)

################ Construcción archivo Valor de Shapley M1_lm

M1_Shapley_xgb <- Archivo
M1_Shapley_xgb$Shap_Age  <- Shap_Age
M1_Shapley_xgb$Shap_Race <- Shap_Race
M1_Shapley_xgb$Shap_Cho  <- Shap_Cho
M1_Shapley_xgb$Shap_Ris  <- Shap_Ris
M1_Shapley_xgb$Shap_Sex  <- Shap_Sex



# ================================
# SHAPLEY METODO I - GLM
# ================================

Archivo<-M1_Delta_glm_yb_stream

############ Age ###############

Shap_Age <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Age)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Age_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Age_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Rac_Cho_Ris_Sex)"]]
  )



################# Race

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Rac)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Cho_Ris_Sex)"]]
  )

############## Cho

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Cho)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Ris_Sex)"]]
  )

############# Ris

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Ris)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Ris)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Sex)"]]
  )

############# Sex

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Sex)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Sex)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Ris)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Ris)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Ris)"]]
  )


suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] - Archivo[["T()"]]

summary(suma_shapley - delta_teorica)

################ Construcción archivo Valor de Shapley M1_lm

M1_Shapley_glm_yb <- Archivo
M1_Shapley_glm_yb$Shap_Age  <- Shap_Age
M1_Shapley_glm_yb$Shap_Race <- Shap_Race
M1_Shapley_glm_yb$Shap_Cho  <- Shap_Cho
M1_Shapley_glm_yb$Shap_Ris  <- Shap_Ris
M1_Shapley_glm_yb$Shap_Sex  <- Shap_Sex


# ================================
# SHAPLEY METODO I - XGB - CLASIFICACION
# ================================

Archivo<-M1_Delta_xgb_yb_stream

############ Age ###############

Shap_Age <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Age)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Age_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Age_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Rac_Cho_Ris_Sex)"]]
  )



################# Race

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Rac)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Rac)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Cho_Ris_Sex)"]]
  )

############## Cho

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Cho)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Rac_Cho)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Age_Cho)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Ris)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Cho)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Sex)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Ris_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Ris_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Ris_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Ris_Sex)"]]
  )

############# Ris

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Ris)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Ris)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Ris)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Ris)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Sex)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Ris)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Ris)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Sex)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Sex)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Ris)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Sex)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Sex)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Sex)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Sex)"]]
  )

############# Sex

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T(Sex)"]] -
      Archivo[["T()"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T(Age_Sex)"]] - Archivo[["T(Age)"]]) +
      (Archivo[["T(Rac_Sex)"]] - Archivo[["T(Rac)"]]) +
      (Archivo[["T(Cho_Sex)"]] - Archivo[["T(Cho)"]]) +
      (Archivo[["T(Ris_Sex)"]] - Archivo[["T(Ris)"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T(Age_Rac_Sex)"]] - Archivo[["T(Age_Rac)"]]) +
      (Archivo[["T(Age_Cho_Sex)"]] - Archivo[["T(Age_Cho)"]]) +
      (Archivo[["T(Age_Ris_Sex)"]] - Archivo[["T(Age_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Sex)"]] - Archivo[["T(Rac_Cho)"]]) +
      (Archivo[["T(Cho_Ris_Sex)"]] - Archivo[["T(Cho_Ris)"]]) +
      (Archivo[["T(Rac_Ris_Sex)"]] - Archivo[["T(Rac_Ris)"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T(Age_Rac_Cho_Sex)"]] - Archivo[["T(Age_Rac_Cho)"]]) +
      (Archivo[["T(Age_Rac_Ris_Sex)"]] - Archivo[["T(Age_Rac_Ris)"]]) +
      (Archivo[["T(Age_Cho_Ris_Sex)"]] - Archivo[["T(Age_Cho_Ris)"]]) +
      (Archivo[["T(Rac_Cho_Ris_Sex)"]] - Archivo[["T(Rac_Cho_Ris)"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] -
      Archivo[["T(Age_Rac_Cho_Ris)"]]
  )


suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] - Archivo[["T()"]]

summary(suma_shapley - delta_teorica)

################ Construcción archivo Valor de Shapley M1_lm

M1_Shapley_xgb_yb <- Archivo
M1_Shapley_xgb_yb$Shap_Age  <- Shap_Age
M1_Shapley_xgb_yb$Shap_Race <- Shap_Race
M1_Shapley_xgb_yb$Shap_Cho  <- Shap_Cho
M1_Shapley_xgb_yb$Shap_Ris  <- Shap_Ris
M1_Shapley_xgb_yb$Shap_Sex  <- Shap_Sex


sum(M1_Shapley_lm[["T(Age)"]]-M1_Shapley_lm[["Shap_Age"]])
sum(M1_Shapley_lm[["T(Rac)"]]-M1_Shapley_lm[["Shap_Rac"]])
sum(M1_Shapley_lm[["T(Cho)"]]-M1_Shapley_lm[["Shap_Cho"]])
sum(M1_Shapley_lm[["T(Ris)"]]-M1_Shapley_lm[["Shap_Ris"]])
sum(M1_Shapley_lm[["T(Sex)"]]-M1_Shapley_lm[["Shap_Sex"]])

sum(M1_Shapley_xgb[["T(Age)"]]-M1_Shapley_xgb[["Shap_Age"]])
sum(M1_Shapley_xgb[["T(Rac)"]]-M1_Shapley_xgb[["Shap_Rac"]])
sum(M1_Shapley_xgb[["T(Cho)"]]-M1_Shapley_xgb[["Shap_Cho"]])
sum(M1_Shapley_xgb[["T(Ris)"]]-M1_Shapley_xgb[["Shap_Ris"]])
sum(M1_Shapley_xgb[["T(Sex)"]]-M1_Shapley_xgb[["Shap_Sex"]])

sum(M1_Shapley_glm_yb[["T(Age)"]]-M1_Shapley_glm_yb[["Shap_Age"]])
sum(M1_Shapley_glm_yb[["T(Rac)"]]-M1_Shapley_glm_yb[["Shap_Rac"]])
sum(M1_Shapley_glm_yb[["T(Cho)"]]-M1_Shapley_glm_yb[["Shap_Cho"]])
sum(M1_Shapley_glm_yb[["T(Ris)"]]-M1_Shapley_glm_yb[["Shap_Ris"]])
sum(M1_Shapley_glm_yb[["T(Sex)"]]-M1_Shapley_glm_yb[["Shap_Sex"]])

sum(M1_Shapley_xgb_yb[["T(Age)"]]-M1_Shapley_xgb_yb[["Shap_Age"]])
sum(M1_Shapley_xgb_yb[["T(Rac)"]]-M1_Shapley_xgb_yb[["Shap_Rac"]])
sum(M1_Shapley_xgb_yb[["T(Cho)"]]-M1_Shapley_xgb_yb[["Shap_Cho"]])
sum(M1_Shapley_xgb_yb[["T(Ris)"]]-M1_Shapley_xgb_yb[["Shap_Ris"]])
sum(M1_Shapley_xgb_yb[["T(Sex)"]]-M1_Shapley_xgb_yb[["Shap_Sex"]])



names(M4_Delta_lm_y_stream)

#################################################################################
############## METODO IV ########################################################
#################################################################################

# ================================
# SHAPLEY METODO IV - LM
# ================================


Archivo <- M4_Delta_lm_y_stream

############ Age ###############

Shap_Age <-
  
  (1/5) * (  
     Archivo[["T_S_Age"]] -
     Archivo[["T_S_empty"]]
 )+
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Cho_Rac_Ris_Sex"]]
  )

################# Race ###############

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Rac"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Ris_Sex"]]
  )

############## Cho ###############

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Cho"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Rac_Ris_Sex"]]
  )

############# Ris ###############

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Ris"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Sex"]]
  )

############# Sex ###############

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Sex"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Ris"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Ris"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Ris"]]
  )

############################
# Comprobación Shapley
############################

suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
  Archivo[["T_S_empty"]]

summary(suma_shapley - delta_teorica)

############################
# Construcción archivo Shapley LM
############################

M4_Shapley_lm <- Archivo
M4_Shapley_lm$Shap_Age  <- Shap_Age
M4_Shapley_lm$Shap_Race <- Shap_Race
M4_Shapley_lm$Shap_Cho  <- Shap_Cho
M4_Shapley_lm$Shap_Ris  <- Shap_Ris
M4_Shapley_lm$Shap_Sex  <- Shap_Sex

# ================================
# SHAPLEY METODO IV - XGB
# ================================


Archivo <- M4_Delta_xgb_y_stream

############ Age ###############

Shap_Age <-
  
  (1/5) * (  
    Archivo[["T_S_Age"]] -
      Archivo[["T_S_empty"]]
  )+
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Cho_Rac_Ris_Sex"]]
  )

################# Race ###############

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Rac"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Ris_Sex"]]
  )

############## Cho ###############

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Cho"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Rac_Ris_Sex"]]
  )

############# Ris ###############

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Ris"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Sex"]]
  )

############# Sex ###############

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Sex"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Ris"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Ris"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Ris"]]
  )

############################
# Comprobación Shapley
############################

suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
  Archivo[["T_S_empty"]]

summary(suma_shapley - delta_teorica)

############################
# Construcción archivo Shapley LM
############################

M4_Shapley_xgb <- Archivo
M4_Shapley_xgb$Shap_Age  <- Shap_Age
M4_Shapley_xgb$Shap_Race <- Shap_Race
M4_Shapley_xgb$Shap_Cho  <- Shap_Cho
M4_Shapley_xgb$Shap_Ris  <- Shap_Ris
M4_Shapley_xgb$Shap_Sex  <- Shap_Sex



# ================================
# SHAPLEY METODO IV - GLM - CLASIFICACION
# ================================


Archivo <- M4_Delta_glm_yb_stream

############ Age ###############

Shap_Age <-
  
  (1/5) * (  
    Archivo[["T_S_Age"]] -
      Archivo[["T_S_empty"]]
  )+
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Cho_Rac_Ris_Sex"]]
  )

################# Race ###############

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Rac"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Ris_Sex"]]
  )

############## Cho ###############

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Cho"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Rac_Ris_Sex"]]
  )

############# Ris ###############

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Ris"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Sex"]]
  )

############# Sex ###############

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Sex"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Ris"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Ris"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Ris"]]
  )

############################
# Comprobación Shapley
############################

suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
  Archivo[["T_S_empty"]]

summary(suma_shapley - delta_teorica)

############################
# Construcción archivo Shapley LM
############################

M4_Shapley_glm_yb <- Archivo
M4_Shapley_glm_yb$Shap_Age  <- Shap_Age
M4_Shapley_glm_yb$Shap_Race <- Shap_Race
M4_Shapley_glm_yb$Shap_Cho  <- Shap_Cho
M4_Shapley_glm_yb$Shap_Ris  <- Shap_Ris
M4_Shapley_glm_yb$Shap_Sex  <- Shap_Sex

# ================================
# SHAPLEY METODO IV - XGB CLASIFICACION
# ================================


Archivo <- M4_Delta_xgb_log_stream

############ Age ###############

Shap_Age <-
  
  (1/5) * (  
    Archivo[["T_S_Age"]] -
      Archivo[["T_S_empty"]]
  )+
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Cho_Rac_Ris_Sex"]]
  )

################# Race ###############

Shap_Race <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Rac"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Rac"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Ris_Sex"]]
  )

############## Cho ###############

Shap_Cho <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Cho"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Cho_Rac"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Age_Cho"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Ris"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Cho_Rac"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Rac_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Rac_Sex"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Ris_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Ris_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Rac_Ris_Sex"]]
  )

############# Ris ###############

Shap_Ris <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Ris"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Ris"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Ris"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Ris"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Sex"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Ris"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Ris"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Sex"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Sex"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Ris"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Sex"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Sex"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Sex"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Sex"]]
  )

############# Sex ###############

Shap_Sex <-
  
  # s = 0
  (1/5) * (
    Archivo[["T_S_Sex"]] -
      Archivo[["T_S_empty"]]
  ) +
  
  # s = 1
  (1/20) * (
    (Archivo[["T_S_Age_Sex"]] - Archivo[["T_S_Age"]]) +
      (Archivo[["T_S_Rac_Sex"]] - Archivo[["T_S_Rac"]]) +
      (Archivo[["T_S_Cho_Sex"]] - Archivo[["T_S_Cho"]]) +
      (Archivo[["T_S_Ris_Sex"]] - Archivo[["T_S_Ris"]])
  ) +
  
  # s = 2
  (1/30) * (
    (Archivo[["T_S_Age_Rac_Sex"]] - Archivo[["T_S_Age_Rac"]]) +
      (Archivo[["T_S_Age_Cho_Sex"]] - Archivo[["T_S_Age_Cho"]]) +
      (Archivo[["T_S_Age_Ris_Sex"]] - Archivo[["T_S_Age_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Sex"]] - Archivo[["T_S_Cho_Rac"]]) +
      (Archivo[["T_S_Cho_Ris_Sex"]] - Archivo[["T_S_Cho_Ris"]]) +
      (Archivo[["T_S_Rac_Ris_Sex"]] - Archivo[["T_S_Rac_Ris"]])
  ) +
  
  # s = 3
  (1/20) * (
    (Archivo[["T_S_Age_Cho_Rac_Sex"]] - Archivo[["T_S_Age_Cho_Rac"]]) +
      (Archivo[["T_S_Age_Rac_Ris_Sex"]] - Archivo[["T_S_Age_Rac_Ris"]]) +
      (Archivo[["T_S_Age_Cho_Ris_Sex"]] - Archivo[["T_S_Age_Cho_Ris"]]) +
      (Archivo[["T_S_Cho_Rac_Ris_Sex"]] - Archivo[["T_S_Cho_Rac_Ris"]])
  ) +
  
  # s = 4
  (1/5) * (
    Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
      Archivo[["T_S_Age_Cho_Rac_Ris"]]
  )

############################
# Comprobación Shapley
############################

suma_shapley <-
  Shap_Age + Shap_Race + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
  Archivo[["T_S_empty"]]

summary(suma_shapley - delta_teorica)

############################
# Construcción archivo Shapley LM
############################

M4_Shapley_xgb_log <- Archivo
M4_Shapley_xgb_log$Shap_Age  <- Shap_Age
M4_Shapley_xgb_log$Shap_Race <- Shap_Race
M4_Shapley_xgb_log$Shap_Cho  <- Shap_Cho
M4_Shapley_xgb_log$Shap_Ris  <- Shap_Ris
M4_Shapley_xgb_log$Shap_Sex  <- Shap_Sex



sum(M4_Shapley_lm[["T_S_Age"]]-M4_Shapley_lm[["Shap_Age"]])
sum(M4_Shapley_lm[["T_S_Rac"]]-M4_Shapley_lm[["Shap_Rac"]])
sum(M4_Shapley_lm[["T_S_Cho"]]-M4_Shapley_lm[["Shap_Cho"]])
sum(M4_Shapley_lm[["T_S_Ris"]]-M4_Shapley_lm[["Shap_Ris"]])
sum(M4_Shapley_lm[["T_S_Sex"]]-M4_Shapley_lm[["Shap_Sex"]])

sum(M4_Shapley_xgb[["T_S_Age"]]-M4_Shapley_xgb[["Shap_Age"]])
sum(M4_Shapley_xgb[["T_S_Rac"]]-M4_Shapley_xgb[["Shap_Rac"]])
sum(M4_Shapley_xgb[["T_S_Cho"]]-M4_Shapley_xgb[["Shap_Cho"]])
sum(M4_Shapley_xgb[["T_S_Ris"]]-M4_Shapley_xgb[["Shap_Ris"]])
sum(M4_Shapley_xgb[["T_S_Sex"]]-M4_Shapley_xgb[["Shap_Sex"]])

sum(M4_Shapley_glm_yb[["T_S_Age"]]-M4_Shapley_glm_yb[["Shap_Age"]])
sum(M4_Shapley_glm_yb[["T_S_Rac"]]-M4_Shapley_glm_yb[["Shap_Rac"]])
sum(M4_Shapley_glm_yb[["T_S_Cho"]]-M4_Shapley_glm_yb[["Shap_Cho"]])
sum(M4_Shapley_glm_yb[["T_S_Ris"]]-M4_Shapley_glm_yb[["Shap_Ris"]])
sum(M4_Shapley_glm_yb[["T_S_Sex"]]-M4_Shapley_glm_yb[["Shap_Sex"]])

sum(M4_Shapley_xgb_log[["T_S_Age"]]-M4_Shapley_xgb_log[["Shap_Age"]])
sum(M4_Shapley_xgb_log[["T_S_Rac"]]-M4_Shapley_xgb_log[["Shap_Rac"]])
sum(M4_Shapley_xgb_log[["T_S_Cho"]]-M4_Shapley_xgb_log[["Shap_Cho"]])
sum(M4_Shapley_xgb_log[["T_S_Ris"]]-M4_Shapley_xgb_log[["Shap_Ris"]])
sum(M4_Shapley_xgb_log[["T_S_Sex"]]-M4_Shapley_xgb_log[["Shap_Sex"]])


###############################################################
###################### Exportar Valor de Shapley ##############




write_xlsx(
  x = M1_Shapley_lm,
  path = "C:/Users/.../M1_Shapley_lm.xlsx"
)
write_xlsx(
  x = M1_Shapley_glm_yb,
  path = "C:/Users/.../M1_Shapley_glm_yb.xlsx"
)
write_xlsx(
  x = M1_Shapley_xgb,
  path = "C:/Users/.../M1_Shapley_xgb.xlsx"
)
write_xlsx(
  x = M1_Shapley_xgb_yb,
  path = "C:/Users/.../M1_Shapley_xgb_yb.xlsx"
)

write_xlsx(
  x = M4_Shapley_lm,
  path = "C:/Users/.../M4_Shapley_lm.xlsx"
)
write_xlsx(
  x = M4_Shapley_glm_yb,
  path = "C:/Users/.../M4_Shapley_glm_yb.xlsx"
)
write_xlsx(
  x = M4_Shapley_xgb,
  path = "C:/Users/.../M4_Shapley_xgb.xlsx"
)
write_xlsx(
  x = M4_Shapley_xgb_log,
  path = "C:/Users/.../M4_Shapley_xgb_log_lm.xlsx"
)
