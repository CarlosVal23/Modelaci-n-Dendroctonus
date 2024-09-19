#Script para crear la base de datos, uniendo los dataframes de incendios, temperaturas y registros de presencia 
#Ademas de los modelos de regresión logistica.
#Este script fue escrito por Juan Carlos Valdés como parte de la investigación de descortezadores

#Se unen los dos data frames de incendos 
combined_inc <- rbind(fire_values_generated,fire_values_sp_points)
#Es necesario transformar los valores de incendios, para poder contabilizarlos
# Aplicar la transformación
combined_inc <- as.data.frame(lapply(combined_inc, function(x) ifelse(x > 0, 1, 0)))
#Se obtiene el número total de incendios
inc_sum<- rowSums(combined_inc)
num_incendios<- data.frame(Total_incendios=inc_sum) 

#combinar los resultados de los extract de temperatura
temp_combinado <- rbind(temperature_values_generated,temperature_values_sp_points)
#Se combierte la temperatura de grados k a C
temp_combinado <- temp_combinado[,2:1051]*0.02-273.15

#se obtiene el promedio de la temperatura
temp_mean<- rowMeans(temp_combinado, na.rm = TRUE)
prom_temperatura<- data.frame(Temp_promedio=temp_mean)

#Se obtiene la temperatura mínima y máxima
# Creación del data frame para los resultados
MinMax <- data.frame(Minima = numeric(nrow(temp_combinado)), Maxima = numeric(nrow(temp_combinado)), stringsAsFactors = FALSE)

# Iniciar el contador de filas
contador_filas <- 1

# Ciclo para calcular el mínimo y máximo de cada fila
for (i in 1:nrow(temp_combinado)) {
  # Calcular los valores mínimo y máximo de la fila actual
  minimos <- min(temp_combinado[i, ], na.rm = TRUE)
  maximos <- max(temp_combinado[i, ], na.rm = TRUE)
  
  # Imprimir los valores para depuración
  print(paste("Fila:", i, "Mínimo:", minimos, "Máximo:", maximos))
  
  # Almacenar los resultados en el data frame
  MinMax[contador_filas, "Minima"] <- minimos
  MinMax[contador_filas, "Maxima"] <- maximos
  
  # Incrementar el contador de filas
  contador_filas <- contador_filas + 1
}

#Ahora se cargan la base de los registros de presencia para poder combinarlos en un solo data frame
library(readxl)
archivo_excel <-"C:/Users/carlo/Documents/Proyecto T/Base/Base R.xlsx"
df_combine <- read_excel(archivo_excel)

#Cargar los datos de elevación
load("C:/Users/carlo/Documents/Proyecto T/Scripts Rstudio/elevation_combined.RData")

#Combinación por columnas, en este data frame se tiene la base final
all_df <- cbind(df_combine,MinMax,prom_temperatura,num_incendios,elevation_combined,row_means_of_maxs_df)


##Regresión logistical (Escarabajo con num incendios)
modelo_logi<-glm(Escarabajo~ Total_incendios, data = all_df, family = binomial)
summary(modelo_logi)
plot(modelo_logi)

#Regresión logistica (Escarabajos con temperatura)
modelo_logiT<- glm(Escarabajo~Temp_promedio,data = all_df, family = binomial)
summary(modelo_logiT)
plot(modelo_logiT)

#Regresion logistica (Escarabajos con Elevacion)
modelo_ele<-glm(Escarabajo~Elevación, data = all_df, family = binomial)

#Regresion logistica (Escarabajos con Precipitación)
modelo_p<- glm(Escarabajo~Precipitación,data = all_df,family = binomial)

#Agregamos la colunma de precipitacion al data frame de la base
all_df <- cbind(all_df, row_means_of_maxs_df)
#Regresion logistica multiple Escarabajo con Elevación, temperatura y precipitación
modeloE <- glm(Escarabajo~ Elevación + Temp_promedio+ Precipitación, data = all_df, family = binomial)
summary(modeloE) 
plot(modeloE)
