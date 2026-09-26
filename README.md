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
├── 2. Medidas_Difusas/
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

Esta carpeta contiene los scripts desarrollados en R para la lectura y adecuación de los datos, cálculo de medidas difusas, el cálculo de predicciones, la estimación de variaciones de predicción y el cálculo de los valores de Shapley. Los métodos se aplican sobre modelos de regresión lineal y XGBoost, así como sobre modelos de clasificación GLM y XGBoost.

| Archivo                        | Descripción  
| ------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| `Lectura_Adecuacion_Datos.R`   |Preparación de la base de datos: lectura e integración de variables explicativas y objetivo, depuración de registros incompletos si los hubiera, análisis descriptivo automático, identificación de tipos   |variables para posterior tratamiento, construcción de nuevas variables (riesgo de presión arterial, sexo recodificado y objetivo binario), selección y codificación de variables de estudio, validación |de la estructura de datos, almacenamiento y dataset final preparado para modelización, medidas difusas, predicciones, variaciones de predicción, cálculo del valor de Shapley y representaciones gráficas |por posición, ausencia y presencia. Salida archivo HNANESI_clean
| `Medida_Difusa_2.R`            |Implementación del procedimiento de cálculo de la medida difusa μ para su posterior uso en el metodo 4, construcción de la matriz μ para todas las variables y coaliciones y validación de sus propiedades |teóricas.  
| `Metodo_I.R`                   |Implementación del Método 1 para el cálculo de la importancia de las variables: estimación de las predicciones y variaciones de predicción asociadas a todas las coaliciones posibles de variables |explicativas (S). El procedimiento se aplica tanto a modelos clásicos de regresión y clasificación (LM y GLM) como a modelos XGBoost (XGB) de regresión y clasificación.
| `Metodo_IV.R`                  |Implementación del Método 4 para el cálculo de la importancia de las variables utilizando la medidas difusas previamente estimadas: estimación de las predicciones y variaciones de predicción asociadas a |todas las coaliciones posibles de variables explicativas (S). El procedimiento se aplica tanto a modelos clásicos de regresión y clasificación (LM y GLM) como a modelos XGBoost (XGB) de regresión y |clasificación.
| `Valor_Shapley.R`              |Implementación del cálculo de los valores de Shapley a partir de las variaciones de predicción obtenidas mediante los Métodos 1 y 4. El procedimiento estima la contribución marginal de cada variable   |considerando todas las coaliciones posibles (S) y se aplica a modelos LM y XGBoost de regresión y GLM y XGBoost de clasificación. |
| `Visualizacion_Medida.R`       |Implementación de modelos de visualización para el análisis de la importancia de variables mediante descomposición marginal del valor Shapley en función del número de predecesores. El script representa     |gráficamente la importancia por posición, presencia y ausencia de una o varias variables, permitiendo visualizar las interacciones e influencias entre las distintas coaliciones mediante los Métodos 1 y         |4.

## Carpeta 2 — XLSX Medidas difusas

Esta carpeta contiene un fichero excel XLSX con los resultados de las medidas difusas para cada una de las variables y las distintas coaliciones posibles.

| Archivo                        | Descripción            
| ------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| `Mu.xlsx`                      |Archivo con los valores de las variables difusas de cada variable en función de la coalición S.

## Carpeta 3 — XLSX predicciones distintos modelos y métodos

Esta carpeta recoge las predicciones individuales para cada instancia generadas mediante los métodos 1 y 4 para todas las coaliciones evaluadas, utilizando los modelos GLM y XGBoost tanto en los problemas de regresión como de clasificación. Las predicciones de los métodos 1 y 4 para cada coalición S (es decir, y1′(S) e y4′(S,μ)) las denominaremos H(S), donde cada variable de S se denominará por las tres primeras letras de su denominación.
La predicción asociada a S=∅ (y1′(∅) e y4′(∅,μ)) se han denotado con la letra K.


| Archivo                        | Descripción    
| ------------------------------ |--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| `M1-Pred-glm-yb.xls`           | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 1 y la regresión logística como modelo de clasificación.  
| `M1-Pred-lm-y.xls`             | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 1 y la regresión lineal como modelo de regresión.        
| `M1-Pred-xgb-yb.xls``          | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 1 y XGBoost como modelo de clasificación.                
| `M1-Pred-xgb-y.xls``           | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 1 y XGBoost como modelo de regresión.                    
| `M4-Pred-glm-yb.xls`           | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 4 y la regresión logística como modelo de clasificación.  
| `M4-Pred-lm-y.xls`             | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 4 y la regresión lineal como modelo de regresión.        
| `M4-Pred-xgb-yb.xls``          | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 4 y XGBoost como modelo de clasificación.                
| `M4-Pred-xgb-y.xls``           | Archivo con las predicciones en cada instancia para cada coalición utilizando el método 4 y XGBoost como modelo de regresión.                    

## Carpeta 4 — XLSX variaciones de predicción y valor de Shapley

Esta carpeta recoge, para cada instancia, las variaciones de la predicción derivadas del conocimiento de los valores de la coalición S respecto a no conocer ningún valor de la variables, así como los valores de Shapley correspondientes. Estos resultados se han generado mediante los métodos 1 y 4 para todas las coaliciones evaluadas, utilizando los mismo modelos que en caso anterior para los problemas de regresión como de clasificación. A Δ1(S) y Δ4(S) se las ha denominado T(S). Si S=∅ la denominación será T() para cualquier otra coalición S se denominará con las tres primeras letras de cada variable. El valor de Shapley se etiqueta con Sha_ más las tres primeras letras de cada variable.

| Archivo                        | Descripción    
| ------------------------------ |--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| `M1-Shapley-glm-yb `           | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 1 y la regresión logística como modelo de clasificación.
| `M1-Shapley-lm-y `             | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 1 y la regresión lineal como modelo de regresión.
| `M1-Shapley-xgb-yb `           | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 1 y XGBoost como modelo de clasificación.
| `M1-Shapley-xgb-y `            | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 1 y XGBoost como modelo de regresión.
| `M4-Shapley-glm-yb `           | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 4 y la regresión logística como modelo de clasificación.
| `M4-Shapley-lm-y `             | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 4 y la regresión lineal como modelo de regresión.
| `M4-Shapley-xgb-yb `           | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 4 y XGBoost como modelo de clasificación.
| `M4-Shapley-xgb-y `            | Archivo con las variaciones de predicciones en cada instancia para cada coalición y el valor de Shapley utilizando el método 4 y XGBoost como modelo de regresión.

## Carpeta 5 — PDF documents

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

* **R** — R scripts utilizados para el procesamiento de los datos y el análisis y cálculo numérico y representaciones gráficas de los distintos métodos.
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

1. `Medida_Difusa.R`
2. `Metodo_IV.R`
3. `Valor_Shapley.R`

### Visualización de los resultados

Las representaciones gráficas pueden generarse de dos maneras:

- Ejecutando directamente el script de visualización, tras completar la secuencia correspondiente de cada uno de los métodos.
- Cargando cualquiera de los archivos de resultados del valor de Shapley, almacenados en la "carpeta 4":

M1-Shapley-lm-y.xlsx
M1-Shapley-glm-yb.xlsx
M1-Shapley-xgb-y.xlsx
M1-Shapley-xgb-yb.xlsx
M4-Shapley-lm-y.xlsx
M4-Shapley-glm-yb.xlsx
M4-Shapley-xgb-y.xlsx
M4-Shapley-xgb-yb.xlsx

tabla_T_base <- read_excel( file.path(ruta_resultados, "Archivo.xlsx"))

tabla_T_base <- tabla_T_base[ , 1:(ncol(tabla_T_base) - 5)]

## Software

Los scripts de R requieren el entorno de programación estadística R y los paquetes especificados dentro de cada script.

## Cita bibliográfica

Si utiliza este repositorio, cite las siguientes publicaciones:

- **Explanation of machine learning classification models with fuzzy measures: An approach to individual classification**. DOI: 10.1007/978-3-031-09176-6_7
- **On measuring features importance in machine learning models in a two-dimensional representation scenario**. DOI: 10.1109/FUZZ-IEEE55066.2022.9882566
- **Machine learning and fuzzy measures: A real approach to individual classification**. DOI: 10.1007/978-3-031-39965-7_12
- **Understanding fuzzy measures: measurement of interactions in a bi-dimensional scenario**. DOI: 10.1109/FUZZ52849.2023.10309674  
