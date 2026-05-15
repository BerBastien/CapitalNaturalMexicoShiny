# Capital Natural Mexico Shiny

Este repositorio contiene el codigo y los datos de una aplicacion en R Shiny para explorar y analizar indicadores ambientales en Mexico, con enfasis en su relacion con los servicios ecosistemicos.

## Objetivo del proyecto

La aplicacion integra informacion espacial en tres dominios de variables para apoyar analisis de servicios ecosistemicos:

- Dominio climatico: variables como temperatura, precipitacion, clorofila y otros proxies biofisicos que capturan condicion ambiental y dinamica climatica.
- Dominio humano: variables sociodemograficas y de presion/uso del territorio (por ejemplo poblacion, accesibilidad o infraestructura) que representan demanda, exposicion o impacto humano.
- Dominio ecologico: variables de estado y funcion de los ecosistemas (cobertura, estructura, productividad, conectividad, entre otras) que ayudan a describir oferta y soporte ecosistemico.

En conjunto, estos tres dominios permiten analizar relaciones entre oferta de servicios ecosistemicos, presiones antropicas y contexto ambiental, en una unidad espacial comun.

## Enfoque espacial: muestreo con malla hexagonal

El flujo de trabajo usa una malla hexagonal (p. ej. 10 km y, en algunos procesos, 2 km) como unidad de analisis espacial.

- Estandarizacion espacial: todos los indicadores se llevan a una misma geometria para hacer comparaciones consistentes entre variables y regiones.
- Muestreo y agregacion: los valores raster o vectoriales se extraen/intersectan por hexagono y se resumen (media, suma, proporcion, etc.) segun el tipo de variable.
- Escalabilidad: la malla facilita correr procesos nacionales de forma reproducible y permite cambiar resolucion segun el analisis.
- Integracion multisectorial: al compartir la misma unidad espacial, se pueden combinar capas climaticas, humanas y ecologicas en indicadores compuestos o analisis comparativos.

## Estructura de carpetas

- **server.R**: logica del servidor de la aplicacion Shiny.
- **ui.R**: definicion de la interfaz de usuario.
- **data/**: datos de entrada de la aplicacion.
  - `master_hex_10km.gpkg`: geopackage de la malla hexagonal de 10 km.
  - `master_hex_2km.gpkg`: geopackage de la malla hexagonal de 2 km (normalmente fuera de control de versiones por tamano).
  - `indicadores/`: archivos CSV de indicadores.
- **scripts/**: scripts de preparacion, depuracion y validacion de datos.
  - `build_master_hex_10km.R`: construccion de malla hexagonal de 10 km.
  - `build_master_hex.R`: construccion de grillas hexagonales.
  - `build_population_*.R`: procesos para capas de poblacion en distintos escenarios.
  - `diag_*.R` y `check_*.R`: diagnosticos y verificaciones de consistencia.
- **www/**: recursos estaticos de la aplicacion (por ejemplo estilos CSS).

## Puesta en marcha

1. Clona este repositorio:

  ```bash
  git clone <repository-url>
  ```

2. Abre el proyecto en RStudio (o en tu entorno R preferido).

3. Instala paquetes requeridos:

  ```r
  install.packages(c("shiny", "sf", "tidyverse"))
  ```

4. Ejecuta la aplicacion:

  ```r
  shiny::runApp()
  ```

## Notas

- Algunos archivos grandes (como ciertas grillas de alta resolucion) pueden excluirse del control de versiones para reducir peso del repositorio.
- Verifica que los insumos requeridos esten presentes en `data/` antes de correr la aplicacion o los scripts de preprocesamiento.

## Notas

Elaborado como parte del proyecto SECIHTI CBF-2025-I-345
