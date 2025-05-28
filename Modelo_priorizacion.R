# 0. CARGA DE PAQUETES NECESARIOS ---------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(terra, sf, prioritizr, tmap, dplyr, purrr)

# 1. CONFIGURACIÓN GENERAL ----------------------------------------------
config <- list(
  base_path = "C:/Users/hmgar/OneDrive/Escritorio/Espacio_trabajo/Prueba_Técnica_Asistente2/",
  output_dir = "C:/Users/hmgar/OneDrive/Escritorio/Espacio_trabajo/Prueba_Técnica_Asistente2/output",
  pmv_area = 2500,        
  resolution = 1000,      
  budget_pct = 0.01,      
  huella_porcentaje = 0.2,  # 20% del costo mediano como peso de penalización
  crs = "EPSG:3116",      
  layers = list(
    paramos = "Paramos/Complejos de Paramos_Escala100k.shp",
    protected = "Runap/runap.shp",
    beneficio = "Capa_costos/RASTER/Beneficio_Neto_Total.tif",
    huella = "Huella_humana/IHEH_2018.tif",
    features = list(oso = "Tremarctos_ornatus/Tremarctos ornatus.tif")
  )
)

# 2. CREACIÓN DE DIRECTORIO DE SALIDA -----------------------------------
if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
  if (!dir.exists(config$output_dir)) stop("❌ No se pudo crear el directorio de salida.")
  message("📁 Directorio de salida creado: ", config$output_dir)
}

# 3. FUNCIÓN PARA CREAR PLANTILLA ESPACIAL -------------------------------
create_template <- function(boundary_path, crs, res) {
  message("📌 Leyendo shapefile de límites...")
  shp_path <- file.path(config$base_path, boundary_path)
  if (!file.exists(shp_path)) stop("❌ Shapefile no encontrado: ", shp_path)
  
  boundary <- st_read(shp_path, quiet = TRUE) |> 
    st_transform(crs) |>
    st_union()
  
  boundary_vect <- vect(boundary)
  
  message("📌 Creando raster plantilla...")
  template <- rast(extent = ext(boundary_vect), resolution = res, crs = crs)
  template[] <- 1
  
  template_masked <- mask(template, boundary_vect)
  
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
    vect_layer <- vect(full_path)
    vect_layer <- project(vect_layer, crs(template))
    rast_layer <- rasterize(vect_layer, template, field = 1)
    return(rast_layer)
  } else {
    rast_layer <- rast(full_path)
    rast_layer <- project(rast_layer, template)
    rast_layer <- resample(rast_layer, template, method = method)
    return(rast_layer)
  }
}

# 5. FLUJO PRINCIPAL -----------------------------------------------------
tryCatch({
  
  # 5.1 Crear plantilla espacial
  message("🔧 Paso 1: Creando plantilla...")
  template <- create_template(config$layers$paramos, config$crs, config$resolution)
  
  # 5.2 Cargar áreas protegidas
  message("🔧 Paso 2: Cargando áreas protegidas...")
  protected <- load_layer(config$layers$protected, template, type = "vector")
  protected_raster <- protected > 0
  
  # 5.3 Cargar capa de beneficios
  message("🔧 Paso 3: Cargando capa de beneficios...")
  beneficio <- load_layer(config$layers$beneficio, template)
  beneficio[is.na(beneficio)] <- global(beneficio, "mean", na.rm = TRUE)[[1]]
  cost_total <- beneficio / 1e6  # Costos en millones COP/km²
  cost_total[protected_raster == 1] <- 0
  
  # 5.4 Calcular peso de penalización dinámico
  message("🔧 Paso 4: Calculando peso de penalización...")
  costo_mediano <- median(values(cost_total)[values(cost_total) > 0], na.rm = TRUE)
  config$huella_peso <- costo_mediano * config$huella_porcentaje
  message("   ▸ Peso calculado: ", round(config$huella_peso, 2), 
          " (", config$huella_porcentaje*100, "% del costo mediano)")
  
  # 5.5 Cargar huella humana
  message("🔧 Paso 5: Cargando huella humana...")
  huella <- load_layer(config$layers$huella, template)
  huella_norm <- huella / 100  # Normalizar 0-100 → 0-1
  
  # 5.6 Cargar distribución del oso
  message("🔧 Paso 6: Cargando distribución de Tremarctos ornatus...")
  oso <- load_layer(config$layers$features$oso, template, method = "near")
  
  # 5.7 Definir presupuesto
  message("🔧 Paso 7: Calculando presupuesto...")
  total_cost <- global(cost_total, "sum", na.rm = TRUE)[[1]]
  budget <- total_cost * config$budget_pct
  message("   ▸ Presupuesto disponible: ", round(budget, 2), " millones COP")
  
  # 5.8 Construir y resolver modelo
  message("🔧 Paso 8: Configurando modelo...")
  problema <- problem(cost_total, features = oso) |>
    add_max_cover_objective(budget = budget) |>
    add_locked_in_constraints(protected_raster) |>
    add_linear_constraints(
      threshold = config$pmv_area,
      sense = ">=",
      data = rep(1, ncell(cost_total))
    ) |>
    add_linear_penalties(
      penalty = config$huella_peso,
      data = huella_norm
    ) |>  
    add_binary_decisions()
  
  message("⚙️ Resolviendo modelo...")
  solucion <- solve(problema, force = TRUE) |> mask(template)
  
  # 6. Exportar resultados
  message("📤 Exportando solución...")
  writeRaster(
    solucion,
    file.path(config$output_dir, "solution.tif"),
    overwrite = TRUE,
    datatype = "INT1U"
  )
  
  # 7. Visualización
  message("🎨 Generando visualización...")
  # Mapa básico de salida con escala, norte y leyenda
  par(mar = c(3, 3, 5, 3))  # Márgenes para el gráfico
  plot(solucion,
       col = c("gray75", "#4daf4a"),
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
  north(xy = c(xmin(ext) + 0.05 * xrange, ymax(ext) - 0.20 * yrange),
        type = 2)
  
  # Leyenda
  legend("bottomright",
         legend = c("No priorizado", "Priorizado"),
         fill = c("gray75", "#4daf4a"),
         title = NULL,
         cex = 0.3,
         bty = "n",
         inset = 0.15)
  
  # 8. Generar informe
  message("📊 Generando informe técnico...")
  report <- data.frame(
    Métrica = c("Área total priorizada (km²)", 
                "Celdas con presencia de oso",
                "Área en zonas protegidas (km²)",
                "Huella humana promedio en solución"),
    Valor = c(
      global(solucion, "sum", na.rm = TRUE)[[1]] |> round(2),
      global(solucion * oso, "sum", na.rm = TRUE)[[1]] |> round(2),
      global(solucion * protected_raster, "sum", na.rm = TRUE)[[1]] |> round(2),
      global(solucion * huella, "mean", na.rm = TRUE)[[1]] |> round(2)
    )
  )
  print(report)
  write.csv(report, file.path(config$output_dir, "informe_priorizacion.csv"), row.names = FALSE)
  
  message("✅ PROCESO COMPLETADO CON ÉXITO")
  
}, error = function(e) {
  message("❌ ERROR CRÍTICO: ", e$message)
  message("⏳ Revise: 1) Rutas de archivos, 2) Consistencia de CRS, 3) Valores NA")
})
