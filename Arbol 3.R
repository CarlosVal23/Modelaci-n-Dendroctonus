#Script para crear el árbol de decisión de regresión para la presencia del escarabajo 
# y sus variables predictoras.
#Este script fue escrito por Juan Carlos Valdés como parte de la investigación de descortezadores

# Cargar las bibliotecas necesarias
library(rpart)
library(rpart.plot)

# Revisar si hay valores faltantes y manejar los que se encuentren
all_df <- na.omit(all_df)

# Asegurarse de que la columna 'Escarabajo' sea numérica
all_df$Escarabajo <- as.numeric(as.character(all_df$Escarabajo))

# Crear el modelo de árbol de decisión de regresión
modeloA <- rpart(Escarabajo ~ Total_incendios + Temp_promedio + Elevación + Precipitación, data = all_df, method = "anova")

# Guardar el gráfico en un archivo PNG de alta resolución
png(filename = "arbol_decision_escarabajo1.png", width = 2000, height = 1500, res = 300)

# Visualizar el árbol de decisión de regresión con ajustes
rpart.plot(modeloA, type = 3, extra = 101, fallen.leaves = TRUE, 
           main = "Presencia de Escarabajo", cex.main = 1.2)

# Cerrar el dispositivo gráfico
dev.off()

# Evaluar el modelo de regresión
# Predicciones en el conjunto de datos
predicciones <- predict(modeloA, all_df)

# Calcular el error medio cuadrado (MSE)
mse <- mean((all_df$Escarabajo - predicciones)^2)

# Imprimir el MSE
print(mse)
