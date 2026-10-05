# Proyecto Final del Módulo 8

De acuerdo con la estadística [Accidentes de Tránsito Terrestre en Zonas Urbanas y Suburbanas (ATUS)](https://www.inegi.org.mx/programas/accidentes/#datos_abiertos) integrada por el INEGI, en **2025** el **17.8%** de los accidentes de tránsito registrados tuvo consecuencias directas sobre al menos una de las personas involucradas, ya sea en forma de lesiones o de fallecimientos.

El presente análisis tiene como objetivo **identificar los factores asociados a la severidad de los accidentes de tránsito** causados por **conductores** mediante el desarrollo y comparación de **modelos de clasificación supervisada**, a partir de variables temporales, geográficas, contextuales del incidente y del conductor responsable, contenidas en la edición 2025 de la ATUS. Para ello, la severidad se define como una variable binaria que distingue entre los accidentes con al menos una persona lesionada o fallecida y aquellos que resultaron únicamente en daños materiales.

Los datos descargados se encuentran en la carpeta [datos](datos) de este repositorio. El archivo [cat_region.csv](datos/cat_region.csv) de esta carpeta es de elaboración propia.

En el script [analisis_completo.R](analisis_completo.R) se presenta todo el análisis realizado durante este proyecto, el cual incluye la exploración visual y estructural de los datos, su transformación y uso en el entrenamiento del modelo, así como la cuantificación, utilizando el modelo, del impacto que los factores tienen sobre la severidad.

El script [preparar_datos.R](preparar_datos.R) contiene el procesamiento de los datos utilizados para el dashboard de shiny. Estos datos procesados se guardan en la carpeta [datos_app](datos_app). En esta misma carpeta se encuentra el archivo [mexico_estados.geojson](datos_app/mexico_estados.geojson), el cual contiene los límites estatales creados por [amCharts](https://www.npmjs.com/package/@amcharts/amcharts4-geodata) y que fueron necesarios para graficar el mapa de la república.

El dashboard se construye con el script [app.R](app.R). En este se presentan diversas visualizaciones descriptivas de los datos, así como los resultados y conclusiones obtenidos con el modelo. El dashboard está hosteado en [Posit Connect Cloud](https://connect.posit.cloud/) y puede explorarse en este [link](https://omargutierrezgut-dashboard-atus.share.connect.posit.cloud/).

El reporte completo puede consultarse en el documento [reporte.html](reporte.html), el cual fue renderizado a partir de [reporte.Rmd](reporte.Rmd).

Autores de este proyecto:

- Betancourt Peralta Diego
- Canul Hernández Erick Iván
- Gutiérrez Gutiérrez José Omar
- Hernández Pérez Maximiliano
- Torres Vargas Brenda Poulette