# 0. CARGA DE PAQUETES NECESARIOS ---------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(terra, sf, prioritizr, tmap, dplyr, purrr) 
# Carga los paquetes necesarios usando pacman. Si pacman no está instalado, se instala.

# 1. CONFIGURACIÓN GENERAL ----------------------------------------------
config <- list(
  base_path = "C:/Users/hmgar/OneDrive/Escritorio/Espacio_trabajo/Prueba_Técnica_Asistente2/",
  output_dir = "C:/Users/hmgar/OneDrive/Escritorio/Espacio_trabajo/Prueba_Técnica_Asistente2/output",
  pmv_area = 2500,        # Área mínima viable (en km²) para restricciones espaciales
  resolution = 1000,      # Resolución del ráster (en metros)
  budget_pct = 0.01,      # Porcentaje del presupuesto total (para priorización)
  huella_peso = 0.1,      # Peso de la penalización por huella humana en la función objetivo
  crs = "EPSG:3116",      # Sistema de coordenadas proyectado usado (MAGNA-SIRGAS)
  layers = list(
    paramos = "Paramos/Complejos de Paramos_Escala100k.shp",      # Límite espacial
    protected = "Runap/runap.shp",                                # Áreas protegidas
    beneficio = "Capa_costos/RASTER/Beneficio_Neto_Total.tif",    # Capa de beneficios económicos
    huella = "Huella_humana/IHEH_2018.tif",                        # Huella humana (penalización)
    features = list(oso = "Tremarctos_ornatus/Tremarctos ornatus.tif")  # Especie objetivo
  )
)

# 2. CREACIÓN DE DIRECTORIO DE SALIDA -----------------------------------
if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)  # Crea directorio si no existe
  if (!dir.exists(config$output_dir)) stop("❌ No se pudo crear el directorio de salida.")
  message("📁 Directorio de salida creado: ", config$output_dir)
}

# 3. FUNCIÓN PARA CREAR PLANTILLA ESPACIAL -------------------------------
create_template <- function(boundary_path, crs, res) {
  message("📌 Leyendo shapefile de límites...")
  shp_path <- file.path(config$base_path, boundary_path)
  if (!file.exists(shp_path)) stop("❌ Shapefile no encontrado: ", shp_path)
  
  boundary <- st_read(shp_path, quiet = TRUE) |>  # Carga shapefile y lo une como un único polígono
    st_transform(crs) |>
    st_union()
  
  boundary_vect <- vect(boundary)  # Convierte a objeto 'SpatVector' para trabajar con terra
  
  message("📌 Creando raster plantilla...")
  template <- rast(extent = ext(boundary_vect), resolution = res, crs = crs)
  template[] <- 1  # Inicializa valores para evitar errores en masking
  
  template_masked <- mask(template, boundary_vect)  # Recorta raster al límite
  
  # Validación de que hay valores válidos en el ráster
  if (is.na(global(template_masked, "sum", na.rm = TRUE)[[1]])) {
    stop("❌ El raster de plantilla no contiene valores válidos.")
  }
  
  message("✅ Plantilla espacial creada correctamente.")
  return(template_masked)
}

# 4. FUNCIÓN PARA CARGAR CAPAS -------------------------------------------
load_layer <- function(path, template, type = "raster", method = "bilinear") {
  full_path <- file.path(config$base_path, path)
  if (!file.exists(full_path)) stop("❌ Archivo no encontrado: ", full_path)
  
  if (type == "vector") {
    vect_layer <- vect(full_path)  # Carga shapefile
    vect_layer <- project(vect_layer, crs(template))  # Proyecta al CRS de plantilla
    rast_layer <- rasterize(vect_layer, template, field = 1)  # Rasteriza como binario
    return(rast_layer)
  } else {
    rast_layer <- rast(full_path)  # Carga raster
    rast_layer <- project(rast_layer, template)  # Reproyecta al CRS de plantilla
    rast_layer <- resample(rast_layer, template, method = method)  # Ajusta resolución
    return(rast_layer)
  }
}

# 5. FLUJO PRINCIPAL -----------------------------------------------------
tryCatch({
  
  # 5.1 Crear plantilla espacial a partir del shapefile de páramos
  message("🔧 Paso 1: Creando plantilla...")
  template <- create_template(config$layers$paramos, config$crs, config$resolution)
  
  # 5.2 Cargar áreas protegidas y convertir a binario (1 = protegido)
  message("🔧 Paso 2: Cargando áreas protegidas...")
  protected <- load_layer(config$layers$protected, template, type = "vector")
  protected_raster <- protected > 0
  
  # 5.3 Cargar capa de beneficios, imputar NA con media y calcular costo
  message("🔧 Paso 3: Cargando capa de beneficios...")
  beneficio <- load_layer(config$layers$beneficio, template)
  beneficio[is.na(beneficio)] <- global(beneficio, "mean", na.rm = TRUE)[[1]]
  cost_total <- beneficio / 1e6  # Costos en millones
  cost_total[protected_raster == 1] <- 0  # Las áreas protegidas no tienen costo
  
  # 5.4 Cargar huella humana y normalizar entre 0 y 1
  message("🔧 Paso 4: Cargando huella humana...")
  huella <- load_layer(config$layers$huella, template)
  huella_norm <- huella / global(huella, "max", na.rm = TRUE)[[1]]
  
  # 5.5 Cargar capa de distribución del oso andino
  message("🔧 Paso 5: Cargando distribución de Tremarctos ornatus...")
  oso <- load_layer(config$layers$features$oso, template, method = "near")
  
  # 5.6 Definir presupuesto para priorización según porcentaje
  message("🔧 Paso 6: Configurando y resolviendo el modelo...")
  total_cost <- global(cost_total, "sum", na.rm = TRUE)[[1]]
  budget <- total_cost * config$budget_pct  # Presupuesto asignado al modelo
  
  # 7. Definir el problema de optimización (zonas de conservación)
  problema <- problem(cost_total, features = oso) |>
    add_max_cover_objective(budget = budget) |>  # Maximiza cobertura con presupuesto
    add_absolute_targets(global(oso, "sum", na.rm = TRUE)[[1]]) |>  # Cobertura total del hábitat del oso
    add_locked_in_constraints(protected_raster) |>  # Áreas protegidas se incluyen sí o sí
    add_linear_constraints(  # Restricción de área mínima viable
      threshold = config$pmv_area,
      sense = ">=",
      data = rep(1, ncell(cost_total))
    ) |>
    add_linear_penalties(penalty = config$huella_peso, data = huella_norm) |>  # Penaliza por alta huella humana
    add_binary_decisions()  # Solución binaria (seleccionado/no seleccionado)
  
  # Resolver el modelo y aplicar máscara con plantilla
  solucion <- solve(problema, force = TRUE) |> mask(template)
  
  # 8. Exportar y visualizar resultados ------------------------------------
  message("📤 Exportando resultado...")
  writeRaster(
    solucion,
    file.path(config$output_dir, "solution.tif"),
    overwrite = TRUE,
    datatype = "INT1U"  # Formato de salida entero de 1 byte sin signo
  )
  
  # Mapa básico de salida con escala, norte y leyenda
  par(mar = c(3, 3, 5, 3))  # Márgenes para el gráfico
  plot(solucion,
       col = c("gray90", "#4daf4a"),
       main = "Áreas de Prioridad",
       cex.main = 1.3,
       legend = FALSE,
       axes = FALSE,
       frame = TRUE)
  
  ext <- ext(solucion)
  xrange <- xmax(ext) - xmin(ext)
  yrange <- ymax(ext) - ymin(ext)
  
  # Barra de escala
  sbar(d = 100, type = "bar", label = "100 km", below = "",
       xy = c(xmin(ext) + 0.09 * xrange, ymin(ext) + 0.09 * yrange),
       cex = 0.5,
       divs = 4,
       lwd = 2,
       col = "black")
  
  # Flecha del norte
  north(xy = c(xmin(ext) + 0.05 * xrange, ymax(ext) - 0.15 * yrange),
        type = 2)
  
  # Leyenda
  legend("bottomright",
         legend = c("No priorizado", "Priorizado"),
         fill = c("gray95", "#4daf4a"),
         title = NULL,
         cex = 0.5,
         bty = "n",
         inset = 0.12)
  
   # 9. Informe final -------------------------------------------------------
  message("📊 Generando informe...")
  report <- data.frame(
    Métrica = c("Área total priorizada (km²)", "Celdas con presencia de oso", "Área protegida dentro de solución (km²)"),
    Valor = c(
      global(solucion, "sum", na.rm = TRUE)[[1]],
      global(solucion * oso, "sum", na.rm = TRUE)[[1]],
      global(solucion * protected_raster, "sum", na.rm = TRUE)[[1]]
    ) |> round(2)
  )
  print(report)
  write.csv(report, file.path(config$output_dir, "informe_priorizacion.csv"), row.names = FALSE)
  message("✅ PROCESO COMPLETADO EXITOSAMENTE")
  
}, error = function(e) {
  # Manejador de errores
  message("❌ ERROR DETECTADO: ", e$message)
})

