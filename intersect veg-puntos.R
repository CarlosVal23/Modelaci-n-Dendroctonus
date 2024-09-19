#script para identificar los tipos de vegetacion en los diferentes puntos espaciales
#Este script fue escrito por Juan Carlos Valdés como parte de la investigación de descortezadores
library(sf)

# Cargar los archivos
uso_suelo_vegetacion <- st_read("C:/Users/carlo/Documents/Proyecto T/Shp puntos y veg/usv250s7gw_valid.shp")
puntos_espaciales <- st_read("C:/Users/carlo/Documents/Proyecto T/Shp puntos y veg/Puntos.shp")

# Verifica los sistemas de coordenadas
# Si no coinciden, transformarlos para que coincidan
uso_suelo_vegetacion <- st_transform(uso_suelo_vegetacion, st_crs(puntos_espaciales))

# Realiza el cruce espacial
resultado <- st_intersects(puntos_espaciales, uso_suelo_vegetacion, sparse = FALSE)

# se Usa la matriz de resultados para obtener la información de vegetación
# Suponiendo que el tipo de vegetación está en una columna llamada "DESCRIPCIO"
tipos_vegetacion <- apply(resultado, 1, function(row) {
  idx <- which(row)
  if (length(idx) > 0) {
    # Devuelve el tipo de vegetación del primer polígono que intersecta
    uso_suelo_vegetacion$DESCRIPCIO[idx[1]]
  } else {
    NA  # Si no hay intersección, devolver NA
  }
})

# Asigna el tipo de vegetación a los puntos
puntos_espaciales$tipo_vegetacion <- tipos_vegetacion

# Ahora, puntos_espaciales tiene una nueva columna con los tipos de vegetación
print(puntos_espaciales)
smp<-st_sample(uso_suelo_vegetacion[49394:49399,],100)
plot(st_geometry(nc)[1:3])
plot(smp,add=T)
#extraer
ras<-raster("C:/Users/carlo/Documents/Proyecto T/Reg temperatura/MOD11A2.061_Clear_sky_days_doy2000081_aid0001.tif")
smp.xy<-as.matrix(st_coordinates(smp))
extract(ras,smp.xy)
