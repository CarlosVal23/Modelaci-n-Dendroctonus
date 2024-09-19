#Script de la prueba u de Mann-Whitney y pruebas de t para variables predictoras 
##Este script fue escrito por Juan Carlos Valdés como parte de la investigación de descortezadores

total_incendios<-all_df$Total_incendios
escarabajo<- all_df$Escarabajo

#Prueba de Mann-Whitney
wt <- wilcox.test(total_incendios, escarabajo)
print(wt)

#Pruebas de t para las otras variables
t_temperatura <- t.test(Temp_promedio ~ Escarabajo, data = all_df)
print(t_temperatura)

t_precipitación <- t.test(Precipitación ~ Escarabajo, data = all_df)
print(t_precipitación)

t_elevación <- t.test(Elevación ~ Escarabajo, data = all_df)
print(t_elevación)
