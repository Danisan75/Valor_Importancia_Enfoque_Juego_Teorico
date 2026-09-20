# ============================================================================
# 1. CÁLCULO DEL VALOR DE SHAPLEY
# ============================================================================
#
# Objetivo:
# Calcular el valor de Shapley asociado a cada variable a partir de las
# contribuciones Δ(S) obtenidas para todas las coaliciones posibles.
#
#
# Metodología:
# - Se parte de la tabla de contribuciones Δ(S).
# - Para cada variable se identifican todas las coaliciones que no la
#   contienen.
# - Se calcula su contribución marginal:
#
#       Δ(S ∪ {i}) − Δ(S)
#
#
# Resultado:
# - Valor de Shapley para cada variable.
# - Verificación correcto calculo.
#
# ============================================================================

# Archivos calculo del valor de Shapley

#M1_Delta_lm_y_stream - Método 1 modelo de regresión lineal
#M1_Delta_glm_yb_stream - Método 1 modelo de regresión logística
#M1_Delta_xgb_y_stream - Método 1 modelo XGBoost para regresión
#M1_Delta_xgb_yb_stream - Método 1 modelo XGBoost para clasificación
#M4_Delta_lm_y_stream - Método 4 modelo de regresión linea
#M4_Delta_glm_yb_stream - Método 4 modelo de regresión logística
#M4_Delta_xgb_y_stream - Método 4 modelo XGBoost para regresión
#M4_Pred_xgb_log_stream - Método 4 modelo XGBoost para clasificación


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

Shap_Rac <-
  
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
  Shap_Age + Shap_Rac + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] - Archivo[["T()"]]

summary(suma_shapley - delta_teorica)

################ Construcción archivo Valor de Shapley M1_lm

M1_Shapley_lm <- Archivo
M1_Shapley_lm$Shap_Age  <- Shap_Age
M1_Shapley_lm$Shap_Rac <- Shap_Rac
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

Shap_Rac <-
  
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
  Shap_Age + Shap_Rac + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] - Archivo[["T()"]]

summary(suma_shapley - delta_teorica)

################ Construcción archivo Valor de Shapley M1_xgb

M1_Shapley_xgb <- Archivo
M1_Shapley_xgb$Shap_Age  <- Shap_Age
M1_Shapley_xgb$Shap_Rac <- Shap_Rac
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

Shap_Rac <-
  
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
  Shap_Age + Shap_Rac + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T(Age_Rac_Cho_Ris_Sex)"]] - Archivo[["T()"]]

summary(suma_shapley - delta_teorica)

################ Construcción archivo Valor de Shapley M1_glm

M1_Shapley_glm_yb <- Archivo
M1_Shapley_glm_yb$Shap_Age  <- Shap_Age
M1_Shapley_glm_yb$Shap_Rac <- Shap_Rac
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

Shap_Rac <-
  
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

################ Construcción archivo Valor de Shapley M1_xgb

M1_Shapley_xgb_yb <- Archivo
M1_Shapley_xgb_yb$Shap_Age  <- Shap_Age
M1_Shapley_xgb_yb$Shap_Rac <- Shap_Rac
M1_Shapley_xgb_yb$Shap_Cho  <- Shap_Cho
M1_Shapley_xgb_yb$Shap_Ris  <- Shap_Ris
M1_Shapley_xgb_yb$Shap_Sex  <- Shap_Sex


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

Shap_Rac <-
  
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
  Shap_Age + Shap_Rac + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
  Archivo[["T_S_empty"]]

summary(suma_shapley - delta_teorica)

############################
# Construcción archivo Shapley LM
############################

M4_Shapley_lm <- Archivo
M4_Shapley_lm$Shap_Age  <- Shap_Age
M4_Shapley_lm$Shap_Rac <- Shap_Rac
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

Shap_Rac <-
  
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
  Shap_Age + Shap_Rac + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
  Archivo[["T_S_empty"]]

summary(suma_shapley - delta_teorica)

############################
# Construcción archivo Shapley XGB
############################

M4_Shapley_xgb <- Archivo
M4_Shapley_xgb$Shap_Age  <- Shap_Age
M4_Shapley_xgb$Shap_Rac <- Shap_Rac
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

Shap_Rac <-
  
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
  Shap_Age + Shap_Rac + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
  Archivo[["T_S_empty"]]

summary(suma_shapley - delta_teorica)

############################
# Construcción archivo Shapley GLM
############################

M4_Shapley_glm_yb <- Archivo
M4_Shapley_glm_yb$Shap_Age  <- Shap_Age
M4_Shapley_glm_yb$Shap_Rac <- Shap_Rac
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

Shap_Rac <-
  
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
  Shap_Age + Shap_Rac + Shap_Cho + Shap_Ris + Shap_Sex

delta_teorica <-
  Archivo[["T_S_Age_Cho_Rac_Ris_Sex"]] -
  Archivo[["T_S_empty"]]

summary(suma_shapley - delta_teorica)

############################
# Construcción archivo Shapley XGB
############################

M4_Shapley_xgb_log <- Archivo
M4_Shapley_xgb_log$Shap_Age  <- Shap_Age
M4_Shapley_xgb_log$Shap_Rac <- Shap_Rac
M4_Shapley_xgb_log$Shap_Cho  <- Shap_Cho
M4_Shapley_xgb_log$Shap_Ris  <- Shap_Ris
M4_Shapley_xgb_log$Shap_Sex  <- Shap_Sex






###############################################################
###################### Exportar Valor de Shapley ##############




write_xlsx(
  x = M1_Shapley_lm,
  path = file.path(
    ruta_resultados, "M1-Shapley-lm-y.xlsx"
))
write_xlsx(
  x = M1_Shapley_glm_yb,
  path = file.path(
    ruta_resultados, "M1-Shapley-glm-yb.xlsx"
))
write_xlsx(
  x = M1_Shapley_xgb,
  path = file.path(
    ruta_resultados, "M1-Shapley-xgb-y.xlsx"
))
write_xlsx(
  x = M1_Shapley_xgb_yb,
  path = file.path(
    ruta_resultados, "M1-Shapley-xgb-yb.xlsx"
))

write_xlsx(
  x = M4_Shapley_lm,
  path = file.path(
    ruta_resultados, "M4-Shapley-lm-y.xlsx"
))
write_xlsx(
  x = M4_Shapley_glm_yb,
  path = file.path(
    ruta_resultados, "M4-Shapley-glm-yb.xlsx"
))
write_xlsx(
  x = M4_Shapley_xgb,
  path = file.path(
    ruta_resultados, "M4-Shapley-xgb-y.xlsx"
))
write_xlsx(
  x = M4_Shapley_xgb_log,
  path = file.path(
    ruta_resultados, "M4-Shapley-xgb-yb.xlsx"
))


###############################################################################
###############################################################################

