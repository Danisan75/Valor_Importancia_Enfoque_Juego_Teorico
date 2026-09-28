## Estructura del repositorio

El repositorio se organiza en cinco carpetas:

```text
├── README.md
├── 1. Código/
│   ├── Lectura_Adecuacion_Datos.R
│   ├── Medida_Difusa_2.R
│   ├── Metodo_I.R
│   ├── Metodo_IV.R
│   ├── Valor_Shapley.R
│   └── Visualizacion_Medida.R
│
├── 2. Medidas difusas/
│   └── Mu.xlsx
│
├── 3. Resultados predicciones/
│   ├── M1-Pred-glm-yb.xlsx
│   ├── M1-Pred-lm-y.xlsx
│   ├── M1-Pred-xgb-y.xlsx
│   ├── M1-Pred-xgb-yb.xlsx
│   ├── M4-Pred-glm-yb.xlsx
│   ├── M4-Pred_lm-y.xlsx
│   ├── M4-Pred-xgb-y.xlsx
│   └── M4-Pred-xgb-yb.xlsx
│
├── 4. Resultados variación de predicción y valor de Shapley/
│   ├── M1-Shapley-glm-yb.xlsx
│   ├── M1-Shapley-lm-y.xlsx
│   ├── M1-Shapley-xgb-y.xlsx
│   ├── M1-Shapley-xgb-yb.xlsx
│   ├── M4-Shapley-glm-yb.xlsx
│   ├── M4-Shapley-lm-y.xlsx
│   ├── M4-Shapley-xgb-y.xlsx
│   └── M4-Shapley-xgb-yb.xlsx
│
└── 5. Representaciones gráficas/
    ├── Shap_Graficos_M1_glm_clasificacion.pdf
    ├── Shap_Graficos_M1_lm_regresion.pdf
    ├── Shap_Graficos_M1_xgb_clasificacion.pdf
    ├── Shap_Graficos_M1_xgb_regresion.pdf
    ├── Shap_Graficos_M4_glm_clasificacion.pdf
    ├── Shap_Graficos_M4_lm_regresion.pdf
    ├── Shap_Graficos_M4_xgb_clasificacion.pdf
    └── Shap_Graficos_M4_xgb_regresion.pdf
```

## Carpeta 1 — Código R

Esta carpeta contiene los scripts desarrollados en R para la lectura, transformación y adecuación de los datos, así como para el cálculo de las medidas difusas, la generación de predicciones, la estimación de variaciones de predicción y el cálculo de los valores de Shapley. Los métodos son aplicados a modelos de regresión lineal y XGBoost, así como a los modelos de clasificación GLM y XGBoost.

| Archivo                        | Descripción  
| ------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| `Lectura_Adecuacion_Datos.R`   |Preparación de la base de datos: lectura e integración de las variables explicativas y objetivo; depuración de registros incompletos, cuando existan; análisis descriptivo automatizado e identificación de la naturaleza de las variables para su posterior tratamiento; construcción de nuevas variables (riesgo de presión arterial, sexo recodificado y variable objetivo binaria); selección y codificación de las variables de estudio; validación de la estructura de datos; y almacenamiento del conjunto de datos final preparado para la modelización. Archivo de salida HNANESI_clean. 
| `Medida_Difusa.R`            |Implementación del procedimiento de cálculo de la medida difusa μ para su utilización en el método 4: el script genera internamente una matriz de resultados (matriz_mu) donde las filas representan las distintas coaliciones de variables (S), las columnas las variables del dataset analizado y las distintas celdas el valor de la medida difusa de cada variable asociado a las distintas coaliciones posibles. Esta matriz constituye el archivo input del método 4, asimismo, la matriz generada se proporciona también en el archivo `Mu.xlsx`, descrito posteriormente, permitiendo la revisión, comprobación y validación de los resultados obtenidos. A su vez, se comprueba el cumplimiento de las propiedades teóricas requeridas para la medida difusa.
| `Metodo_I.R`                   |Implementación del método 1 para el cálculo de la importancia de las variables: estimación de las predicciones y variaciones de predicción asociadas a todas las coaliciones posibles de variables explicativas (S). El procedimiento se aplica tanto a modelos clásicos de regresión y clasificación (LM y GLM) como a modelos XGBoost (XGB) de regresión y clasificación.
| `Metodo_IV.R`                  |Implementación del método 4 para el cálculo de la importancia de las variables utilizando las medidas difusas estimadas previamente en el script `Medida_Difusa.R`: cálculo de las predicciones y de las variaciones de predicción asociadas a todas las coaliciones posibles de variables explicativas (S). El procedimiento se aplica tanto a modelos clásicos de regresión y clasificación (LM y GLM) como a modelos XGBoost (XGB) de regresión y clasificación.
| `Valor_Shapley.R`              |Implementación del cálculo de los valores de Shapley a partir de las variaciones de predicción obtenidas mediante los métodos 1 y 4: el procedimiento estima la contribución marginal de cada variable considerando todas las coaliciones posibles (S) y se aplica a modelos de regresión LM y XGBoost, así como a modelos de clasificación GLM y XGBoost. 
| `Visualizacion_Medida.R`       |Implementación del procedimiento de visualización para el análisis de la importancia de las variables, mediante la descomposición marginal del valor Shapley, en función del número de predecesores: el script genera representaciones gráficas de la importancia por posición, presencia y ausencia de una o varias variables, permitiendo analizar las interacciones e influencias entre las distintas coaliciones mediante los métodos 1 y 4.

## Carpeta 2 — XLSX Medidas difusas

Esta carpeta contiene un fichero excel XLSX con los resultados de las medidas difusas para cada una de las variables y las distintas coaliciones posibles.

| Archivo                        | Descripción            
| ------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| `Mu.xlsx`                      |Archivo con los valores de las medidas difusas de cada variable en función de la coalición S. Las columnas corresponden a las variables analizadas, las filas a las distintas coaliciones de S y cada celda contiene el valor de la medida difusa de la variable analizada para la coalición correspondiente.

## Carpeta 3 — XLSX de las predicciones de cada instancia y coalición para el método 1 y 4 de los distintos modelos

Esta carpeta contiene las predicciones individuales, para cada instancia, de todas las coaliciones posibles de S, obtenidas mediante los métodos 1 y 4, empleando los modelos de regresión lineal y XGBoost en regresión, y modelos GLM y XGBoost en clasificación. En este contexto, S representa cada una de las posibles coaliciones de variables explicativas de la base de datos. Cada coalición se construye a partir de una combinación específica de variables utilizadas para generar las predicciones, desde la ausencia total de información, S=∅, hasta el conocimiento completo de todas las variables explicativas de una instancia.
Con el fin de facilitar la verificación de las predicciones obtenidas para cada método, y poder comparar las diferencias en las predicciones, se adoptan nomenclaturas específicas. En el método 1, las predicciones asociadas a cada coalición S, denotadas en la memoria como y1′(S), se identifican mediante el formato H(AAA_BBB_CCC...), donde los códigos incluidos entre paréntesis corresponden a las tres primeras letras de cada variable que forman la coalición. La coalición S=∅ se identifica mediante H() y, adicionalmente, mediante la letra K, utilizada en la formulación y el cálculo de las variaciones de predicción. En el método 4, las predicciones asociadas a cada coalición S, denotadas en la memoria como y4′(S,μ) y siendo μ la medida difusa asociada a esa coalición, se identifican mediante el formato H_S_AAA_BBB_CCC..., donde los H_S_ corresponden a la nomenclatura de la predicción, seguido de las tres primeras letras de las variables que forma la coalición. La coalición S=∅ se representa mediante H_S_Empty y, adicionalmente, mediante la letra K, utilizada en el cálculo de las variaciones de predicción. 


| Archivo                        | Descripción    
| ------------------------------ |--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| `M1-Pred-glm-yb.xlsx`           | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 1 y la regresión logística como modelo de clasificación.  
| `M1-Pred-lm-y.xlsx`             | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 1 y la regresión lineal como modelo de regresión.        
| `M1-Pred-xgb-yb.xlsx`           | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 1 y XGBoost como modelo de clasificación.                
| `M1-Pred-xgb-y.xlsx`            | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 1 y XGBoost como modelo de regresión.                    
| `M4-Pred-glm-yb.xlsx`           | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 4 y la regresión logística como modelo de clasificación.  
| `M4-Pred-lm-y.xlsx`             | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 4 y la regresión lineal como modelo de regresión.        
| `M4-Pred-xgb-yb.xlsx`           | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 4 y XGBoost como modelo de clasificación.                
| `M4-Pred-xgb-y.xlsx`            | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 4 y XGBoost como modelo de regresión.                    

## Carpeta 4 — XLSX variaciones de predicción y valor de Shapley de cada instancia y coalición para el método 1 y 4 de los distintos modelos

Esta carpeta contiene, para cada instancia y para cada coalición de variables, las variaciones de predicción obtenidas, a partir del conocimiento de los valores de una coalición S respecto a no conocer el valor de ninguna variable de la base de datos, así como los valores de Shapley calculados a partir de dichas variaciones. Los resultados se han generado para los métodos 1 y 4, en todas las coaliciones posibles, utilizando los modelos de regresión lineal y XGBoost en regresión, y modelos GLM y XGBoost en clasificación. Las variaciones de predicción asociadas a cada coalición S, denotadas en la memoria como Δ1(S) y Δ4(S) para el método 1 y 4 respectivamente, se identifican mediante una nomenclatura análoga a la empleada para las predicciones descritas en la carpeta 3. En el método 1 es utilizada la notación T(AAA_BBB_CCC...), utilizándose en el método 4 el formato T_S_AAA_BBB_CCC..., donde las siglas AAA, BBB, CCC,... corresponden a las tres primeras letras de las características que forman la coalición S. La coalición S=∅ se representa en el método 1 mediante T() y en el método 4 como T_S_Empty. El resto de las coaliciones, incluida la formada por todas las variables consideradas en el análisis, se identifican concatenando las tres primeras letras de las variables que las componen, como se ha descrito en este epígrafe. Los valores de Shapley asociados a cada característica se identifican mediante el prefijo Sha_, seguido de las tres primeras letras de la característica analizada.

| Archivo                        | Descripción    
| ------------------------------ |--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| `M1-Shapley-glm-yb.xlsx`           | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 1 y la regresión logística como modelo de clasificación.
| `M1-Shapley-lm-y.xlsx`             | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 1 y la regresión lineal como modelo de regresión.
| `M1-Shapley-xgb-yb.xlsx`           | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 1 y XGBoost como modelo de clasificación.
| `M1-Shapley-xgb-y.xlsx`            | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 1 y XGBoost como modelo de regresión.
| `M4-Shapley-glm-yb.xlsx`           | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 4 y la regresión logística como modelo de clasificación.
| `M4-Shapley-lm-y.xlsx`             | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 4 y la regresión lineal como modelo de regresión.
| `M4-Shapley-xgb-yb.xlsx`           | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 4 y XGBoost como modelo de clasificación.
| `M4-Shapley-xgb-y.xlsx`            | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 4 y XGBoost como modelo de regresión.

## Carpeta 5 — PDF con las representaciones gráficas para los métodos 1 y 4 y los modelos de regresión y clasificación

Esta carpeta contiene en archivos pdf las gráficas por posición, presencia y ausencia para las distintas combinaciones de coaliciones. La gráfica en color verde representa la importancia por posición en función de los predecesores de la variable analizada. La gráfica azul representa la importancia por presencia en función del número de predecesores. La gráfica verde representa la importancia por ausencia en función del número de predecesores. Todas las representaciones gráficas son en la primera instancia. Cada gráfica contiene una leyenda donde se detallan las variables analizadas.

| Archivo                                  | Descripción    
| ---------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| `Shap_Graficos_M1_glm_clasificación.pdf` | Archivo con las representaciones gráficas por posición, ausencia y presencia de una variable o coalición utilizando el método 1 y la regresión logística como modelo de clasificación.
| `Shap_Graficos_M1_lm_regresión.pdf`      | Archivo con las representaciones gráficas por posición, ausencia y presencia de una variable o coalición utilizando el método 1 y la regresión lineal como modelo de regresión.
| `Shap_Graficos_M1_xgb_clasificación.pdf` | Archivo con las representaciones gráficas por posición, ausencia y presencia de una variable o coalición utilizando el método 1 y XGBoost como modelo de clasificación.
| `Shap_Graficos_M1_xgb_regresión.pdf`     | Archivo con las representaciones gráficas por posición, ausencia y presencia de una variable o coalición utilizando el método 1 y XGBoost como modelo de regresión.
| `Shap_Graficos_M4_glm_clasificación.pdf` | Archivo con las representaciones gráficas por posición, ausencia y presencia de una variable o coalición utilizando el método 4 y la regresión logística como modelo de clasificación.
| `Shap_Graficos_M4_lm_regresión.pdf`      | Archivo con las representaciones gráficas por posición, ausencia y presencia de una variable o coalición utilizando el método 4 y la regresión lineal como modelo de regresión.
| `Shap_Graficos_M4_xgb_clasificación.pdf` | Archivo con las representaciones gráficas por posición, ausencia y presencia de una variable o coalición utilizando el método 4 y XGBoost como modelo de clasificación.
| `Shap_Graficos_M4_xgb_regresión.pdf`     | Archivo con las representaciones gráficas por posición, ausencia y presencia de una variable o coalición utilizando el método 4 y XGBoost como modelo de regresión.

## Formato de los archivos

* **R** — Scripts utilizados para el procesamiento de los datos, el análisis y cálculo numérico y representaciones gráficas de los métodos 1 y 4.
* **XLSX** — Microsoft Excel contiene las tablas con los distintos resultados.
* **PDF** — Adobe contiene los resultados gráficos de los distintos métodos y modelos.

## Reproducibilidad

Los scripts de R y los archivos de datos incluidos en este repositorio permiten reproducir íntegramente los análisis y resultados presentados en la memoria.

### Método 1

La ejecución debe llevarse a cabo en el siguiente orden:

1. `Lectura_Adecuacion_Datos.R`
2. `Metodo_I.R`
3. `Valor_Shapley.R`

### Método 4

Una vez ejecutado `Lectura_Adecuacion_Datos.R`, la secuencia es la siguiente:

1. `Lectura_Adecuacion_Datos.R`
2. `Medida_Difusa.R`
3. `Metodo_IV.R`
4. `Valor_Shapley.R`

### Visualización de los resultados

Las representaciones gráficas pueden generarse de dos maneras:

Ejecutando directamente el script de visualización, tras completar la secuencia correspondiente de cada uno de los métodos, o bien cargando cualquiera de los archivos de resultados del valor de Shapley, almacenados en la "carpeta 4", resultados variación de predicción y valor de Shapley:

- M1-Shapley-lm-y.xlsx
- M1-Shapley-glm-yb.xlsx
- M1-Shapley-xgb-y.xlsx
- M1-Shapley-xgb-yb.xlsx
- M4-Shapley-lm-y.xlsx
- M4-Shapley-glm-yb.xlsx
- M4-Shapley-xgb-y.xlsx
- M4-Shapley-xgb-yb.xlsx

A continuación se presentan las rutas:   

tabla_T_base <- read_excel( file.path(ruta_resultados, "Archivo.xlsx"))

tabla_T_base <- tabla_T_base[ , 1:(ncol(tabla_T_base) - 5)]

## Software

Los scripts de R requieren el entorno de programación estadística R y los paquetes especificados dentro de cada script. Se ha utilizado la versión de R 4.4.0.

## Cita bibliográfica

Si utiliza este repositorio, cite las siguientes publicaciones:

- D. Santos, I. Gutiérrez, J. Castro, D. Gómez, J.A. Guevara y R. Espínola. «Explanation of machine learning classification models with fuzzy measures: An approach to individual classification». En: Intelligent and fuzzy systems: digital acceleration and the new normal, Infus 2022. Vol. 505. Springer international publishing, 2022, págs. 62-69. doi: 10.1007/978-3-031-09176-6_7.
- I. Gutiérrez, D. Santos, J. Castro, D. Gómez, R. Espínola y J. A. Guevara. «On measuring features importance in machine learning models in a two-dimensional representation scenario». En: IEEE, 2022, págs. 1-9. doi: 10.1109/FUZZ-IEEE55066.2022.9882566.
- I. Gutiérrez, D. Santos, J. Castro, J.A. Hernández-Gonzalo, D. Gómez y R.Espínola. «Machine learning and fuzzy measures: A real approach to individual classification». En: Fuzzy logic and technology, and aggregation operators. Springer, 2023, págs. 137-148. doi: 10.1007/978-3-031-39965-7_12.
- I. Gutiérrez, D. Santos, J. Castro, D. Gómez y R. Espínola. «Understanding fuzzy measures: measurement of interactions in a bi-dimensional scenario». En:2023 IEEE International conference on fuzzy systems. IEEE, 2023, págs. 1-6. doi: 10.1109/FUZZ52849.2023.10309674.
