# Trabajo Fin de Máster en Ingeniería y Ciencia de Datos - UNED

## Utilización de técnicas multivariantes para el estudio del aprendizaje de la mejora de la accesibilidad en el subtitulado de vídeos

### Autor

**Javier Pérez Arteaga**

### Directores

- **Emilio Letón Molina**
- **Jorge Pérez Martín**

### Institución

**Universidad Nacional de Educación a Distancia (UNED)**  
**Escuela Técnica Superior de Informática**  
**Máster en Ingeniería y Ciencia de Datos**

## Resumen

Este trabajo de fin de máster analiza la capacidad de los estudiantes de la Sexta Edición (2022) del MOOC **Materiales Digitales Accesibles** de la UNED y la Fundación ONCE para evaluar la calidad del subtitulado en vídeos.

### Metodología

Los participantes visualizaron dos vídeos idénticos: uno correctamente subtitulado y otro con errores. Se usó un diseño experimental **triple ciego**, donde:

- El orden de los vídeos fue aleatorizado.
- Se utilizó una escala de Likert de 18 ítems con cinco niveles para evaluar la calidad.
- Se aplicaron modelos estadísticos multivariantes para el análisis de datos.

Se propusieron dos modelos:

1. **Regresión Logística** (con variable dicotomizada).
2. **Regresión Ordinal Acumulativa** (con análisis frecuentista y bayesiano).

### Resultados

Los estudiantes sin experiencia previa en accesibilidad fueron capaces de percibir diferencias en aspectos como:

- **Corrección ortográfica y gramatical.**
- **Literalidad y fidelidad al diálogo.**
- **Identificación de personajes.**

Sin embargo, tuvieron dificultades para evaluar criterios espaciales y temporales, como:

- **Número de líneas y caracteres por línea.**
- **Sincronización y velocidad del subtitulado.**

### Conclusiones

El estudio concluye que los estudiantes del MOOC pudieron detectar errores evidentes en el subtitulado, pero aspectos técnicos más sutiles requirieron mayor experiencia.

## Tecnologías y Herramientas

- **R** para el análisis estadístico.
- **Modelos Lineales Generalizados Mixtos (GLMM).**
- **Análisis Frecuentista y Bayesiano.**
- **Markdown, Latex y Quarto** para la redacción del documento.

### Librerías en R utilizadas

El análisis de datos se realizó en R, utilizando las siguientes librerías:

- **tidyverse**: Para manipulación y visualización de datos.
- **lme4**: Para ajustar Modelos Lineales Generalizados Mixtos.
- **brms**: Para modelado bayesiano con regresión ordinal.
- **ordinal**: Para regresión ordinal acumulativa.
- **ggplot2**: Para la generación de gráficos.
- **knitr** y **rmarkdown**: Para la generación de informes reproducibles.

## Enlaces

- [Presentación](https://surtich.github.io/TFM-Ingenieria-y-Ciencia-de-Datos).
- [Trabajo](./ficheros/entrega/PerezArteagaJavier-TFM.pdf).
