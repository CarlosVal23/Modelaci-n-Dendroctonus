# Script para generar puntos aleatorios con una distancia de 10km alejadas de los 
#registros de presencia, para evitar el traslape de las mismas.

# Cargar paquetes necesarios
library(sf)
library(dplyr)
library(units)

# Leer el archivo SHP
shapefile <- st_read("C:/Users/carlo/Documents/vegdisuelto.shp")

# Convertir sp_points a objeto sf
existentes_sf <- st_as_sf(sp_points)

# Transformar la proyección a la misma que el shapefile
existentes_sf <- st_transform(existentes_sf, st_crs(shapefile))

# Definir una función para verificar la distancia mínima
check_points <- function(random_points, existentes_sf, min_distance) {
  dists <- st_distance(random_points, existentes_sf)
  keep <- rowSums(dists < min_distance) == 0
  return(random_points[keep,])
}

# Generar puntos aleatorios que estén a más de 10 km de las coordenadas existentes
num_puntos <- 4806
min_distance <- set_units(10, "km")

# Generar puntos aleatorios
random_points <- st_sample(shapefile, size = num_puntos * 10, type = "random")

# Filtrar puntos que cumplan con la distancia mínima
valid_points <- check_points(random_points, existentes_sf, min_distance)

# Repetir la generación hasta obtener el número deseado de puntos válidos
while (length(valid_points) < num_puntos) {
  additional_points <- st_sample(shapefile, size = num_puntos * 10, type = "random")
  valid_points <- c(valid_points, check_points(additional_points, existentes_sf, min_distance))
  valid_points <- valid_points[!duplicated(st_coordinates(valid_points)),]
}
valid_points <- valid_points[1:num_puntos]

# Convertir los puntos válidos a SpatialPoints
random_spatial_points <- st_as_sf(valid_points)

# Asignar la misma proyección a los puntos aleatorios que el shapefile original
st_crs(random_spatial_points) <- st_crs(shapefile)

# Guardar los puntos aleatorios como un archivo SHP
st_write(random_spatial_points, "puntos_aleatorios2.shp", layer_options = "SHPT=POINT")

# Verificar la clase y proyección
class(random_spatial_points)
st_crs(random_spatial_points)
