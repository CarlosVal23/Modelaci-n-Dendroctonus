#Script para extraer los datos de precipitación para cada registro de presencia y para los generados aleatoriamente
#Este script fue escrito por Juan Carlos Valdés como parte de la investigación de descortezadores

library(raster)

# Carpeta donde se encuentran los archivos TIFF
carpeta_tiff <- "C:/Users/carlo/Documents/Proyecto T/Precipitación cortes"

# Obtener una lista de todos los archivos TIFF en la carpeta
archivos_tiff <- list.files(carpeta_tiff, pattern = "\\.tif$", full.names = TRUE)

# Inicializar las matrices vacíos para almacenar los resultados

resultados_pp <- matrix(ncol = 0, nrow = nrow(sp_points_sf))
resultados_pp_a <- matrix(ncol = 0, nrow = nrow(points_sf))
# Inicializar contador
total_archivos <- length(archivos_tiff)

# Iterar sobre cada archivo TIFF en la carpeta
for (idx in seq_along(archivos_tiff)) {
  archivo <- archivos_tiff[idx]
  
  # Mostrar el progreso
  cat("Procesando archivo", idx, "de", total_archivos, "-", basename(archivo), "\n")
  
  # Cargar el archivo TIFF como un stack
  tiff_stack <- stack(archivo)
  
  # Extraer los valores para los puntos espaciales en todas las capas del TIFF
  extract_values_sp_points <- extract(tiff_stack, sp_points_sf)
  extract_values_points_sf <- extract(tiff_stack, points_sf)
  
  # Agregar los valores extraídos a los respectivos data frames
  resultados_pp <- cbind(resultados_pp, extract_values_sp_points)
  resultados_pp_a <- cbind(resultados_pp_a, extract_values_points_sf)
}

# Agregar nombres a las columnas de los data frames, usando el nombre del archivo y números de capa
#colnames(resultados_pp) <- paste(rep(basename(archivos_tiff), each = nlayers(tiff_stack)), "Capa", 1:nlayers(tiff_stack), sep = "_")
#colnames(resultados_pp_a) <- paste(rep(basename(archivos_tiff), each = nlayers(tiff_stack)), "Capa", 1:nlayers(tiff_stack), sep = "_")

# Mostrar los primeros resultados
head(resultados_pp)
head(resultados_pp_a)

