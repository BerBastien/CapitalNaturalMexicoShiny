# Reporte de la Primera Etapa

## Proyecto SECIHTI CBF-2025-I-345
### *Impactos del Cambio Climático en los Valores de Uso y No Uso del Capital Natural en México*

**Responsable técnico:** Dr. Bernardo A. Bastien-Olvera
Investigador Asociado C de Tiempo Completo, Instituto de Ciencias de la Atmósfera y Cambio Climático, UNAM. Departamento de Cambio Climático, Grupo Clima y Sociedad, Hub Ecosistemas.
*bbastien@atmosfera.unam.mx*

**Institución ejecutora:** Universidad Nacional Autónoma de México (1602701)
**Convocatoria:** Ciencia Básica y de Frontera 2025 — SECIHTI
**Etapa reportada:** Etapa 1 (8 meses) — *Identificación, recopilación y creación de la base de datos maestra sobre capital natural en México.*
**Fecha de corte:** mayo de 2026

---

## 1. Resumen ejecutivo

La primera etapa del proyecto se planteó como objetivo central la *creación de un repositorio abierto, estandarizado y geográficamente explícito de variables de capital natural para México*, sobre el cual descansarán las etapas 2 y 3 (modelación con DGVMs, funciones de impacto climático, proyección municipal de valor económico y meta-regresión). Esta entrega cumple esa meta y supera el alcance comprometido en tres aspectos:

1. **Repositorio operativo en línea**, no únicamente almacenamiento. Se desarrolló e implementó una aplicación Shiny en R que permite a cualquier persona — desde investigadora especialista hasta tomadora de decisiones — visualizar, filtrar por estado, descargar la malla geoespacial y descargar los datos tabulares de cualquier variable disponible.
2. **Esquema de colaboración descentralizada.** El diseño técnico (un único geopackage maestro de hexágonos + un archivo XLSX por combinación variable–escenario–año) permite que distintas personas y grupos de investigación contribuyan nuevas variables sin tocar la base maestra ni el código de la aplicación.
3. **Doble resolución geoespacial:** una malla nacional de **10 km** para variables continentales, oceánicas y proyecciones climáticas; y una malla **adicional de 2 km** para variables que requieren mayor desagregación urbana (huella de edificios en capitales estatales, por ejemplo).

A la fecha del corte se han integrado **19 variables** distintas en los tres dominios establecidos en la propuesta — *climático, ecológico y humano* — totalizando **96 archivos XLSX activos** (combinaciones variable × escenario socioeconómico × horizonte temporal) sobre un universo de **60 054 hexágonos** (23 502 continentales + 36 552 oceánicos para la malla de 10 km y >611 000 hexágonos para la malla de 2 km).

El entregable contemplado en el plan de trabajo aprobado para esta etapa — *"1 Tecnologías de la Información (Software) | Base de datos"* — se cumple con los siguientes componentes interconectados:

- Aplicación Shiny: `Capital Natural de México ante el Cambio Climático` (interfaz pública).
- Geopackage maestro: `master_hex_10km.gpkg` y `master_hex_2km.gpkg` (geometría compartida).
- Repositorio de indicadores: 96 archivos XLSX autodescriptivos en `data/indicadores/`.
- Documentación reproducible: scripts de generación, esquema de metadatos por archivo, y código fuente versionado.

---

## 2. Arquitectura técnica del repositorio

### 2.1 Filosofía de diseño

El repositorio se diseñó con cuatro principios rectores derivados de los retos identificados en la propuesta original ("integración de bases de datos de formatos diversos" se identifica explícitamente como uno de los riesgos del proyecto):

1. **Una sola geometría compartida.** Toda variable, sin importar su origen, se "aterriza" en la misma malla hexagonal. Esto elimina la necesidad de re-proyectar, re-mosaicar o re-rasterizar al combinar datos.
2. **Un archivo por variable–escenario–año.** Cada XLSX es independiente y autocontenido (datos, catálogo de la variable y metadatos en hojas separadas). Esto permite trabajo colaborativo paralelo: cada autor o grupo genera y valida su propio archivo sin riesgo de sobreescribir el trabajo de otra persona.
3. **Metadatos junto a los datos.** Cada XLSX incluye su propia hoja `catalogo_variable` con la fuente, referencia bibliográfica, unidades, descripción y tipo de dato; y una hoja `metadatos` con la trazabilidad técnica (raster fuente, método de extracción, fecha de generación). El consumidor del dato no necesita acceder a un archivo externo de catálogo.
4. **Descarga abierta.** La aplicación expone botones explícitos para descargar tanto la geometría completa (`.gpkg`) como cualquier subconjunto de datos visualizado, garantizando que el usuario nunca quede atrapado dentro de la interfaz.

### 2.2 ¿Por qué una malla hexagonal?

La elección de hexágonos sobre rejillas cuadradas o sobre límites administrativos (municipios, AGEBs) responde a tres consideraciones técnicas:

- **Equidistancia entre vecinos:** todo hexágono comparte exactamente la misma distancia entre su centroide y los centroides de sus 6 vecinos, lo cual es ideal para análisis de proximidad, cálculos de adyacencia y modelos de difusión espacial.
- **Cobertura uniforme y libre de superposición:** los hexágonos teselan el plano sin huecos ni traslapes y, a diferencia de las rejillas cuadradas, minimizan la distorsión en proyecciones en latitudes medias y altas (México abarca latitudes entre ~14° N y ~33° N).
- **Independencia de límites administrativos cambiantes:** los municipios y AGEBs se reorganizan; los hexágonos no. Esto garantiza la comparabilidad temporal entre 1970 (manglares históricos) y 2100 (proyecciones SSP).

Cada hexágono lleva un identificador estable (`hex_id`, p. ej. `HEX0292043`) y una clasificación `tipo` que distingue **Continental** (23 502 celdas) y **Océano** (36 552 celdas). Esto permite que variables marinas (clorofila), continentales (uso de suelo) y atmosféricas (temperatura) coexistan en la misma estructura sin ambigüedades.

Las dos resoluciones se justifican así:
- **10 km:** la mayoría de los rásteres climáticos (CMIP6, ISIMIP3b, Wang et al. 2025) y de capital natural (CONABIO, JULES, GDP global) tienen una resolución nativa entre 30″ (~1 km) y 0.5° (~50 km). 10 km es el "common ground" que respeta el detalle nacional sin sobre-interpretar la resolución original de los rásteres globales.
- **2 km:** algunos análisis socioecosistémicos urbanos (huella de edificios, isla de calor urbana, vulnerabilidad localizada) requieren detalle intra-municipal. La malla de 2 km cubre cada capital estatal con suficiente resolución para resolver patrones de uso intra-urbano sin saturar la aplicación.

### 2.3 Stack tecnológico

| Componente | Tecnología | Función |
|---|---|---|
| Interfaz interactiva | **R + Shiny + shinyWidgets + shinyjs** | Aplicación web de visualización y descarga |
| Mapa interactivo | **Leaflet** + tiles CartoDB Positron | Visualización dinámica con preferencia de Canvas |
| Geometría compartida | **GeoPackage** (formato OGC abierto) | Un único archivo con la malla y atributos topológicos |
| Lectura geoespacial | **sf** (Simple Features) | Manejo de geometrías vectoriales |
| Lectura de rásteres | **terra**, **raster** | Procesamiento de capas climáticas y de uso de suelo |
| Datos tabulares | **XLSX** (1 archivo por variable–escenario–año) | Formato auditable, abierto y editable por humanos |
| Lectura/escritura XLSX | **readxl**, **openxlsx** | Acceso programático a hojas múltiples |
| Manipulación de datos | **dplyr**, **tidyr**, **purrr**, **stringr**, **forcats** | Transformaciones reproducibles |
| Visualización gráfica | **ggplot2**, **scales** | Histogramas y leyendas continuas/categóricas |
| Tipografía y estilo | **CSS personalizado** + **Google Fonts** (Fraunces / Manrope) | Identidad visual del repositorio |

### 2.4 Estructura de un archivo XLSX de indicador

Cada archivo en `data/indicadores/` sigue una convención de nombre de la forma:

```
[DOMINIO]__[variable]__[escenario]__[año].xlsx
```

donde el dominio es **C** (Climática), **E** (Ecológica) o **H** (Humana). Ejemplos:

- `C__temperatura__SSP370__2050.xlsx`
- `E__porcentaje_cobertura_bosques__SSP585__2100.xlsx`
- `H__poblacion__SSP2__2070.xlsx`
- `H__producto_interno_bruto_per_capita__PRESENTE__2020.xlsx`

Internamente, cada XLSX contiene tres hojas:

- **`datos`** — la tabla de valores: `hex_id` + columna numérica o categórica con el valor por hexágono.
- **`catalogo_variable`** — fila única con la ficha pública de la variable: `variable_id`, `variable_nombre`, `tipo_dato` (continuo / categórico), `unidades`, `descripcion`, `fuente`, `referencia` (cita bibliográfica completa), `columna_id_hex`, `columna_valor` y `resolucion_hex`.
- **`metadatos`** — trazabilidad técnica de la extracción: `geopackage` y `capa_geopackage` usados, `raster` o vector fuente, `metodo_extraccion`, `id_hexagono`, `total_hexagonos`, y `fecha_generacion`.

Para variables categóricas se añade además una hoja **`diccionario`** con las categorías y sus colores hexadecimales sugeridos para representación cartográfica consistente.

### 2.5 Aplicación Shiny: lógica de negocio

La aplicación implementa los siguientes componentes funcionales:

- **Selector dual de variables:** modo *lista plana* con búsqueda en vivo (todas las combinaciones variable–escenario–año en un solo desplegable) y modo *árbol* (Dominio → Variable → Escenario → Año).
- **Filtro por estado:** sobre los 32 estados de la República + opción "Todos", aplicado dinámicamente a la malla.
- **Mapa interactivo:** renderizado con preferencia de Canvas para soportar las 60 mil celdas sin pérdida de fluidez. Soporta paletas continuas y categóricas, popup con metadatos por celda, capas base intercambiables.
- **Histograma sincronizado:** se actualiza automáticamente con cualquier cambio de variable o estado.
- **Caja de metadatos en cuatro paneles:** Variable, Descripción, Fuente, Referencia — siempre visibles bajo el mapa.
- **Botones de descarga:**
  - *Descarga la malla geoespacial* — el GeoPackage completo de la resolución activa.
  - *Descarga los datos mostrados* — el XLSX de la variable–escenario–año seleccionado, filtrado por el estado activo.
- **Indicadores de carga:** banner global "Cargando datos..." al inicio y overlay "Actualizando mapa..." al cambiar selecciones.

### 2.6 Flujo de trabajo colaborativo

El esquema de "un archivo por variable" tiene una consecuencia operativa importante para el equipo de trabajo:

1. Una colaboradora/colaborador descarga el geopackage maestro de la resolución correspondiente.
2. Procesa su capa fuente (raster, shapefile, base tabular) y extrae el valor por hexágono usando la geometría compartida — no necesita decidir nada sobre la rejilla.
3. Genera un XLSX siguiendo la convención de nombre y la plantilla de tres hojas.
4. Coloca el archivo en `data/indicadores/` y la aplicación lo detecta automáticamente en el siguiente arranque, sin modificar `server.R` ni `ui.R`.

Esto permitió que durante esta etapa contribuyeran al repositorio investigadores e investigadoras de varios grupos sin coordinación centralizada de código, eliminando el cuello de botella típico de los repositorios monolíticos.

### 2.7 Portabilidad y reproducibilidad

El código fuente está diseñado para ser portable: no contiene rutas absolutas codificadas en los archivos de ejecución (`server.R`, `ui.R`), únicamente rutas relativas resueltas con `getwd()` desde el directorio raíz de la aplicación. La aplicación puede desplegarse en cualquier instancia de Shiny Server o `shinyapps.io` simplemente copiando la carpeta del proyecto. Los scripts de generación de datos en `scripts/` sí contienen rutas locales a fuentes de datos crudos (rásteres originales fuera del repositorio); estos scripts son herramientas de *backstage* para regenerar los XLSX, no son necesarios en producción.

---

## 3. Inventario de variables y fuentes de datos

A continuación se documenta cada variable integrada en esta primera etapa, su origen, método de procesamiento y referencia bibliográfica completa. Las variables se agrupan por dominio.

### 3.1 Dominio Climático (C)

Las tres variables climáticas del repositorio provienen del **modelo del sistema Tierra GFDL-ESM4**, desarrollado por el *Geophysical Fluid Dynamics Laboratory* de la NOAA y participante en la sexta fase del Coupled Model Intercomparison Project (CMIP6). Las salidas crudas son las que distribuye **ISIMIP3b** (Inter-Sectoral Impact Model Intercomparison Project, fase 3b), procesadas a frecuencia diaria y agregadas en este repositorio a promedios anuales por hexágono. Los tres escenarios disponibles cubren el espectro de la *narrativa SSP-RCP* del IPCC: **SSP1-2.6** (mitigación ambiciosa, +1.8 °C a 2100), **SSP3-7.0** (rivalidad regional, ~+3.6 °C) y **SSP5-8.5** (desarrollo intensivo en fósiles, ~+4.4 °C). Para cada escenario se incluyen los horizontes **2020, 2050, 2070 y 2100**.

**Referencia común:**
> Held, I. M., Guo, H., Adcroft, A., Dunne, J. P., Horowitz, L. W., Krasting, J., et al. (2019). Structure and performance of GFDL's ESM4.0 Earth System Model. *Journal of Advances in Modeling Earth Systems*, 11, 3167–3211. https://doi.org/10.1029/2019MS001829

#### 3.1.1 Temperatura (`C__temperatura`)
Temperatura del aire en superficie (`tas`) en grados Celsius, promedio espacial por celda hexagonal de 10 km. El raster fuente ya viene en °C, sin conversión adicional. Variable continua. Disponible para los 3 escenarios × 4 horizontes = 12 archivos.

#### 3.1.2 Precipitación (`C__precipitacion`)
Precipitación anual acumulada en mm/año, obtenida del raster `pr_*.tif` en frecuencia diaria y agregada anualmente. Muestreada en el centroide de cada hexágono. Variable continua. 12 archivos disponibles.

#### 3.1.3 Radiación solar (`C__radiacion_solar`)
Radiación solar de onda corta descendente en superficie (`rsds`) en W/m². Promedio espacial por hexágono. Variable continua. 12 archivos disponibles.

### 3.2 Dominio Ecológico (E)

#### 3.2.1 Uso de vegetación y uso de suelo
Mapas categóricos del estado del paisaje mexicano bajo distintos escenarios futuros de presión antropogénica y mitigación, derivados del trabajo de **Mendoza-Ponce et al. (2019)**, que identifica *hotspots* de cambio en cobertura del suelo bajo escenarios socioeconómicos y climáticos para México. El repositorio incluye el escenario **Presente (2020)** y los escenarios futuros **Tendencial**, **Pérdida** y **Verde** para los horizontes **2050 y 2070**, totalizando 7 archivos. La extracción se hizo por *moda* del raster dentro de cada hexágono, conservando NA fuera de la cobertura del raster.
> Mendoza-Ponce, A., Corona-Núñez, R. O., Galicia, L. & Kraxner, F. Identifying hotspots of land use cover change under socioeconomic and climate change scenarios in Mexico. *Ambio* 48, 336–349 (2019). https://doi.org/10.1007/s13280-018-1085-0

#### 3.2.2 Porcentaje de cobertura de bosques
Variable continua expresada en porcentaje de cubierta arbórea por hexágono, derivada del **Joint UK Land Environment Simulator (JULES)** — modelo dinámico de vegetación global (DGVM) que simula el balance de energía, agua y carbono terrestre. La cobertura "bosque" se define como la suma de los tipos funcionales de planta `pft-bdldcd`, `pft-bdlevgtemp`, `pft-bdlevgtrop`, `pft-ndldcd`, `pft-ndlevg`, `pft-shrubdcd` y `pft-shrubevg`. Este indicador es el primer puente directo entre el repositorio de la Etapa 1 y la Etapa 2 del proyecto, que utilizará explícitamente salidas de DGVMs. Disponible para los 3 escenarios SSP × 4 horizontes = 12 archivos.
> Best, M. J., Pryor, M., Clark, D. B., Rooney, G. G., Essery, R. L. H., Ménard, C. B., et al. (2011). The Joint UK Land Environment Simulator (JULES), model description – Part 1: energy and water fluxes. *Geoscientific Model Development*, 4(3), 677–699.

#### 3.2.3 Cobertura histórica de manglar en México
Serie temporal de seis cortes históricos (**1970, 2005, 2010, 2015, 2020, 2025** — el último año pendiente de actualización con la versión más reciente del SMMM) que documenta la trayectoria de la cubierta de manglar en kilómetros cuadrados por hexágono. La extracción se realiza por intersección geométrica entre la malla hexagonal y los polígonos del **Sistema de Monitoreo de Manglares de México (SMMM)** de CONABIO; los hexágonos sin intersección con manglar se preservan como NA (no como 0) para distinguirlos correctamente en los cálculos posteriores de cambio. El área de manglar en todo México en 1970/80 se reporta en 8 563.10 km².
> CONABIO. (2013). *Mapa de Distribución de los manglares en México en 1970/1980*, escala 1:50 000. Sistema de Monitoreo de Manglares de México. CDMX, México. <http://www.conabio.gob.mx/informacion/gis/?vns=mis_capas/gis_root/biodiv/monmang/bimagdmo/mexman70gw>
> Velázquez-Salazar, S., Rodríguez-Zúñiga, M. T., Alcántara-Maya, J. A., Villeda-Chávez, E., Valderrama-Landeros, L., Troche-Souza, C., et al. (2021). *Manglares de México. Actualización y análisis de los datos 2020*. CONABIO, México. <https://bioteca.biodiversidad.gob.mx/janium/Documentos/15638.pdf>

#### 3.2.4 Área para restauración de manglar en México
Variable de oportunidad de conservación. Identifica las zonas (en km² por hexágono) que tuvieron cobertura de manglar antes de 2020 (categorías SMMM 8: Manglar Perturbado o 9: Manglar) pero que para 2020 ya habían transitado a categorías compatibles con restauración: 4 (Sin vegetación), 6 (Otra vegetación), 7 (Otros humedales) o 8 (Manglar perturbado). Se descartaron explícitamente categorías que indican usos irreversibles para restauración: 0 (No data), 1 (Nubes), 2 (Desarrollo antrópico), 3 (Agrícola pecuario) y 5 (Cuerpo de agua). El total nacional con potencial de restauración se cuantifica en 1 237.04 km². Mismas referencias que 3.2.3.

#### 3.2.5 Área para restauración de manglar dentro de Áreas Naturales Protegidas
Versión refinada de la variable anterior, intersectada adicionalmente con los polígonos de Áreas Naturales Protegidas federales y estatales de México. Cuantifica las hectáreas con potencial de restauración que ya cuentan con un instrumento jurídico de protección, lo cual maximiza la viabilidad de proyectos de restauración. Total nacional: 613.34 km². Fuentes adicionales para los polígonos de ANP:
> SHP de ANP federales: <http://geoportal.conabio.gob.mx/metadatos/doc/html/anpmx.html>
> SHP de ANP estatales: <http://www.conabio.gob.mx/informacion/gis/?vns=gis_root/region/biotic/anpest25gw>

#### 3.2.6 Tipo hidrológico de suelo
Variable categórica que clasifica cada hexágono según su comportamiento hidrológico siguiendo el sistema **HSG** del USDA, fundamental para modelación hidrológica (curve number, escurrimiento). Las clases incluidas son A (bajo escurrimiento, >90 % arena), B (moderadamente bajo), C (moderadamente alto), D (alto, >40 % arcilla) y las clases dobles A/D, B/D, C/D, D/D que representan suelos con alto potencial de escurrimiento *a menos que* se les aplique drenaje. La fuente es el dataset global a 250 m de resolución publicado por Ross et al. (2018) en el ORNL DAAC.
> Ross, C. W., Prihodko, L., Anchang, J. Y., Kumar, S. S., Ji, W. & Hanan, N. P. (2018). *Global Hydrologic Soil Groups (HYSOGs250m) for Curve Number-Based Runoff Modeling*. ORNL DAAC, Oak Ridge, Tennessee, USA. https://doi.org/10.3334/ORNLDAAC/1566

#### 3.2.7 Altura del dosel
Promedio en metros de la altura del dosel arbóreo dentro de cada hexágono terrestre (los hexágonos de océano se preservan como NA). El raster fuente es **Meta Canopy Height** a 250 m de resolución, derivado de imágenes satelitales de alta resolución mediante deep learning y publicado por el equipo de IA de Meta a través del catálogo de Google Earth Engine de la comunidad. El método de extracción es promedio exacto por hexágono usando `exact_extract`.
> Meta AI. *Canopy height global dataset*. <https://gee-community-catalog.org/projects/meta_trees/>

#### 3.2.8 Concentración de clorofila
Variable continua que reporta la concentración de clorofila-a en superficie del océano en mg/m³, únicamente en celdas oceánicas. Esta variable está actualmente representada con un dataset didáctico para validación del flujo en celdas marinas, y será reemplazada en la Etapa 2 por la fuente operacional de Copernicus Marine Service / NASA OceanColor. Sirve como demostración de la capacidad del repositorio para integrar variables marinas usando el mismo esquema hexagonal.

### 3.3 Dominio Humano (H)

#### 3.3.1 Población
Variable continua (personas por hexágono de 10 km) bajo los **cinco Shared Socioeconomic Pathways (SSP1 a SSP5)** del IPCC AR6 y para los horizontes **2020, 2050, 2070 y 2100**, totalizando 19 archivos disponibles. La fuente son los rásteres globales de población de **Wang et al.** distribuidos en figshare. Es importante notar que durante esta etapa se identificó y corrigió un error en la georreferenciación documentada de los rásteres originales: la extensión real es de 84° N (no 90° N) con píxeles cuadrados de 1/120° (~0.0083°), lo cual era esencial para reproducir correctamente las cifras de población observadas (p. ej., ~1–1.4 millones de personas por hexágono en la Zona Metropolitana del Valle de México, en lugar de los 6–17 mil que producía la fórmula incorrecta). Las celdas oceánicas se preservan correctamente como NA.
> Wang, T. et al. *Global gridded SSP population datasets*. figshare. https://doi.org/10.6084/m9.figshare.19608594

#### 3.3.2 Producto Interno Bruto (PIB PPP)
Variable continua expresada en *2005 International Dollars* (PPP), disponible en versión presente (**2020**) y en escenario **SSP2 a 2050**. El dato se construye como el promedio del PIB PPP dentro de un buffer de 50 km alrededor del centroide de cada hexágono, lo cual suaviza la fuerte heterogeneidad puntual de los rásteres globales de PIB y produce una superficie más adecuada para análisis socioecosistémicos a escala municipal. La fuente es el dataset global de PIB grideado de Wang & Sun (2023).
> Wang, T. & Sun, F. (2023). *Global gridded GDP under the historical and future scenarios* [Data set]. Zenodo. https://doi.org/10.5281/zenodo.7898409

#### 3.3.3 PIB per cápita (PPP)
Variable continua expresada en *2017 International USD* per cápita, presente (**2020**), construida como el valor promedio del raster global de PIB per cápita PPP dentro del hexágono. La fuente es el dataset *downscaled* de Kummu et al. (2025), publicado en *Scientific Data*, que cubre 1990–2022 a alta resolución espacial y constituye actualmente el estándar para análisis subnacionales de PIB per cápita.
> Kummu, M., Kosonen, M. & Masoumzadeh Sayyar, S. (2025). Downscaled gridded global dataset for gross domestic product (GDP) per capita PPP over 1990–2022. *Scientific Data* 12, 178. https://doi.org/10.1038/s41597-025-04487-x

#### 3.3.4 Huella de los edificios
Única variable hasta ahora desplegada en la **malla de 2 km**. Reporta el área total de huella de edificios (en m²) por hexágono, calculada únicamente sobre las **32 capitales de la República Mexicana**. La fuente son los polígonos de edificios de **OpenStreetMap**, descargados por capital estatal y agregados por intersección con la malla fina. La cobertura por capital se conserva como NA fuera de las 32 áreas urbanas analizadas. Esta variable es el caso de uso piloto para análisis de **Urban Cooling** que se desarrollará en la Etapa 2 del proyecto.
> OpenStreetMap contributors. (2026). *OpenStreetMap* [Conjunto de datos]. OpenStreetMap Foundation. https://www.openstreetmap.org

---

## 4. Cumplimiento del plan de trabajo de Etapa 1

El plan aprobado para la Etapa 1 contempla cuatro metas y cinco bloques de actividades. A continuación se documenta el cumplimiento de cada elemento:

| Meta del proyecto | Estado | Evidencia |
|---|---|---|
| Desarrollar y optimizar un repositorio accesible para almacenamiento y manejo eficiente de los datos. | **Cumplido** | Aplicación Shiny operativa con descarga, filtrado, visualización y metadatos. |
| Estandarizar los datos recolectados para garantizar consistencia y precisión. | **Cumplido** | 96 archivos XLSX en formato unificado (3 hojas: datos, catálogo, metadatos). |
| Compilar y descargar datos georreferenciados del capital natural de México. | **Cumplido** | 19 variables integradas de fuentes nacionales (CONABIO, INEGI) e internacionales (CMIP6, ISIMIP, ORNL DAAC, Meta, Wang et al., Kummu, OSM). |
| Crear copias de seguridad integrales para asegurar la preservación de los datos. | **En curso** | Repositorio replicado en Google Drive (origen) y árbol de proyecto local versionado; pendiente publicación en repositorio institucional con DOI. |

| Actividad del cronograma | Estado |
|---|---|
| Mes 1-2: Identificación y mapeo de fuentes de datos relevantes. | Cumplido |
| Mes 2-4: Descargar datos desde INEGI, CONABIO y plataformas internacionales. | Cumplido |
| Mes 4-6: Estandarizar datos y desarrollar el repositorio. | Cumplido |
| Mes 6-8: Pruebas del repositorio, subida y backups. | En curso |

### Hallazgos técnicos no anticipados

Durante la implementación se identificaron y resolvieron varios desafíos técnicos relevantes que conviene documentar para etapas subsecuentes:

1. **Inconsistencias en georreferenciación de fuentes globales.** El caso de los rásteres de población SSP de Wang et al. ilustra un riesgo recurrente: los proveedores de datos globales no siempre documentan correctamente la extensión real de sus rásteres. La adopción de la estrategia "extraer al hexágono y validar contra una zona de control conocida" (en este caso, la población de la ZMVM) detectó el problema de inmediato y permitió corregirlo antes de que contaminara análisis derivados.
2. **Distinción NA / 0.** En variables de cobertura (manglar, edificios, área de restauración), la diferencia entre "no hay dato" y "el valor es cero" es crítica para análisis de cambio temporal. Se estableció como convención del repositorio preservar siempre NA fuera de la cobertura de la fuente original, y reportar 0 únicamente cuando el dato fuente lo indica explícitamente.
3. **Resoluciones heterogéneas.** La introducción de la malla de 2 km, no contemplada en la propuesta original, surgió como respuesta a la incompatibilidad de la malla de 10 km con análisis urbanos finos. La arquitectura del repositorio absorbió esta extensión sin necesidad de reestructurar el código.

---

## 5. Conexión con las etapas 2 y 3

El repositorio entregado en esta Etapa 1 no es un producto terminal sino la base operativa de las etapas siguientes:

- **Etapa 2 (homogeneización con DGVMs y funciones de impacto climático):** las variables `E__porcentaje_cobertura_bosques`, `C__temperatura`, `C__precipitacion` y `C__radiacion_solar` ya están alineadas en escenarios SSP y horizontes idénticos, lo cual permite construir directamente las funciones de impacto clima → ecosistema sin pasos intermedios de re-proyección.
- **Etapa 3 (proyección municipal del valor económico y meta-regresión):** las variables `H__producto_interno_bruto`, `H__producto_interno_bruto_per_capita` y `H__poblacion` proveen los denominadores económicos y demográficos para escalar los valores meta-analíticos del capital natural a nivel hexágono, y de ahí a municipio mediante intersección estándar.
- **Modelo de evaluación integrada:** el formato XLSX por archivo y el geopackage común facilitan que el modelo numérico final consuma los datos directamente sin requerir un *data lake* intermedio.

---

## 6. Equipo y reconocimientos

**Responsable técnico:** Dr. Bernardo A. Bastien-Olvera (UNAM, ICAyCC).
**Colaboradoras y colaboradores que han contribuido datos durante esta etapa:** Xarhini García Cepeda, Mariana (procesamiento de manglares), Clemente Rueda (extracción Meta Canopy), entre otras personas del Hub Ecosistemas del Grupo Clima y Sociedad.

**Cómo citar este repositorio:**
> Bastien-Olvera, B. A. et al. (2026). *Capital Natural de México ante el Cambio Climático: Repositorio interactivo de variables climáticas, ecológicas y humanas en malla hexagonal*. Proyecto SECIHTI CBF-2025-I-345, Instituto de Ciencias de la Atmósfera y Cambio Climático, UNAM.

---

*Reporte generado en mayo de 2026 como entregable de cierre de Etapa 1 del proyecto SECIHTI CBF-2025-I-345.*
