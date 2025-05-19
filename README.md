# 🛰️ Priorización Espacial con `prioritizr` en R

Este repositorio contiene un script escalable en R para la implementación de un modelo de **priorización espacial de conservación** usando el paquete [`prioritizr`](https://prioritizr.net/). El análisis incorpora información espacial sobre áreas protegidas, costos de oportunidad (valor de unidades de planeación), IHEH y distribución potencial de las especies de interés, para este caso *Tremarctos ornatus* (oso andino). El área de interés corresponde al Complejo de páramos de Colombia y el objetivo de conservación de área se baja en la meta de tamaño de Población Mínimo Viable (PMV) 

## 📁 Estructura de Archivos

```
Prueba_Técnica_Asistente2/
├── output/                     # Carpeta de salida con resultados
├── Paramos/                   # Shapefile de complejos de páramos
├── Runap/                     # Shapefile de áreas protegidas
├── Capa_costos/RASTER/        # Ráster de beneficios económicos por actividades agropecuarias (km2), costo de oportunidad
├── Huella_humana/             # Ráster de huella humana
├── Tremarctos_ornatus/        # Ráster de presencia del oso andino
└── script.R                   # Script principal de análisis
```

## 📦 Paquetes Requeridos

El script usa los siguientes paquetes de R:

```r
pacman::p_load(terra, sf, prioritizr, tmap, dplyr, purrr)
```

Asegúrate de tener instalado `pacman` y el resto de paquetes antes de correr el script.

## ⚙️ Configuración

La sección de configuración (`config`) define rutas, resolución espacial, sistema de coordenadas (EPSG:3116 - MAGNA-SIRGAS), parámetros de presupuesto y penalización por huella humana, así como los paths relativos de las capas utilizadas.

## 🚀 Flujo del Análisis

1. **Carga de paquetes**
2. **Configuración de rutas y parámetros**
3. **Creación de plantilla espacial a partir del shapefile de páramos**
4. **Carga y procesamiento de capas raster y vector**
5. **Definición y resolución del modelo de priorización espacial**
6. **Exportación del resultado (`solution.tif`)**
7. **Visualización básica del resultado**

## 🧠 Lógica del Modelo

El problema de priorización se formula como un modelo de optimización que:

- **Maximiza la cobertura del hábitat del oso andino**
- **Minimiza el costo económico de conservación**
- **Incluye áreas protegidas como bloqueos fijos (locked-in)**
- **Penaliza zonas con alta huella humana**
- **Impone una restricción de área mínima viable (2500 km²)**

## 🖼️ Resultados

El resultado final es un ráster binario (`solution.tif`) donde:

- `0` = No priorizado
- `1` = Área priorizada para conservación

Este puede visualizarse en R o cualquier SIG como QGIS o ArcGIS.

## 📌 Notas

- El script ha sido probado en entorno Windows con resolución de 1000 m.
- El sistema de referencia espacial utilizado es **MAGNA-SIRGAS / Origen Nacional (EPSG:3116)**.

## 📬 Contacto

Desarrollado por **Henry García-Diego**
