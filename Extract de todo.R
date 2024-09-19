## En este script se hace la extraccion de los valores para cada punto (registros de presencia
# y generados en el muestreo aleatorio)
#Este script fue escrito por Juan Carlos Valdes como parte de la investigación de descortezadores.
library(raster)
library(sf)
library(dplyr)

# Función para extraer valores de TIFF y organizarlos por columnas
extract_values_from_tiff <- function(tiff_folder, points_sf, char_range) {
  # Crear un data.frame para almacenar los resultados
  results <- data.frame(id = 1:nrow(points_sf))
  
  # Obtener la lista de archivos TIFF en la carpeta
  tiff_files <- list.files(tiff_folder, pattern = "\\.tif$", full.names = TRUE)
  
  # Iterar sobre cada archivo TIFF
  for (tiff_file in tiff_files) {
    # Cargar el archivo TIFF
    raster_layer <- raster(tiff_file)
    
    # Extraer valores en los puntos
    extracted_values <- extract(raster_layer, points_sf)
    
    # Crear un nombre de columna basado en el nombre del archivo TIFF
    if (nchar(basename(tiff_file)) >= max(char_range)) {
      column_name <- substr(basename(tiff_file), char_range[1], char_range[2])
    } else {
      stop("El nombre del archivo es demasiado corto para extraer los caracteres especificados")
    }
    
    # Agregar los valores extraídos como una nueva columna en el data.frame
    results[[column_name]] <- extracted_values
  }
  
  return(results)
}

# Leer los puntos generados
points_sf <- st_read("C:/Users/carlo/Documents/puntos_aleatorios2.shp")

# Asegurarse de que sp_points es un objeto sf y tiene la misma proyección
if (!inherits(sp_points, "sf")) {
  sp_points_sf <- st_as_sf(sp_points)
} else {
  sp_points_sf <- sp_points
}
sp_points_sf <- st_transform(sp_points_sf, st_crs(points_sf))

# Rango de caracteres para los nombres de columnas
temperature_char_range <- c(28, 34)
fire_char_range <- c(15, 21)

# Carpeta con los archivos TIFF de temperatura
temperature_tiff_folder <- "C:/Users/carlo/Documents/Proyecto T/Reg temperatura"

# Extraer valores de temperatura para los puntos generados
temperature_values_generated <- extract_values_from_tiff(temperature_tiff_folder, points_sf, temperature_char_range)

# Extraer valores de temperatura para sp_points
temperature_values_sp_points <- extract_values_from_tiff(temperature_tiff_folder, sp_points_sf, temperature_char_range)

# Carpeta con los archivos TIFF de incendios
fire_tiff_folder <- "C:/Users/carlo/Documents/Proyecto T/incendios"

# Extraer valores de incendios para los puntos generados
fire_values_generated <- extract_values_from_tiff(fire_tiff_folder, points_sf, fire_char_range)

# Extraer valores de incendios para sp_points
fire_values_sp_points <- extract_values_from_tiff(fire_tiff_folder, sp_points_sf, fire_char_range)

# Ahora `temperature_values_generated`, `temperature_values_sp_points`, 
# `fire_values_generated`, y `fire_values_sp_points` son data.frames que contienen 
# los valores extraídos organizados por columnas
