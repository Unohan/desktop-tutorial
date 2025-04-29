#===========================
#Modelos Lineales
#==========================
setwd("d:/Users/User/Documents/Respaldo24/Cursos y Libros/Diplomado Econometria")
getwd()
#Librerias
install.packages("dplyr")
library(dplyr)
install.packages("labelled")
library(labelled)
install.packages("gplot2")
library(ggplot2)
install.packages("readxl")
library(readxl)

#Cargando base
base <- read_excel("base_ols.xlsx") #Fijate en el dominio
#Limpiando base
base_final <- base %>% 
  select(estado = State, poblacion = Population,
         ingresos = Income, analfabetismo = Illiteracy,
         esp_vida = `Life Exp`,
         asesinatos = Murder, graduados_secun = `HS Grad`,
         heladas = Frost, area = Area) %>% 
  set_variable_labels(estado = "Nombre del estado",
                      poblacion = "Poblacion en miles",
                      ingresos = "Ingreso percapita en USD",
                      analfabetismo = "Tasa (%)",
                      esp_vida = "Esperanza de vida en años",
                      asesinatos = "Tasa de homicidios (por cada 100,000 habitantes",
                      graduados_secun = "Porcentaje de graduados de secundaria",
                      heladas = "Dias promedio al año con heladas",
                      area = "Area en millas cuadradas")
#======================================
#3. Análisis gráfico
#siempre detectar quien es la endogena
#======================================
base_final %>% 
  ggplot(aes(x = asesinatos, y = esp_vida)) +
  geom_point() +
  geom_smooth(method = "lm", col = "gray", size = 1, linetype ="dashed", se = F) +
  labs(title = "Relacion de asesinatos y esperanza de vida",
       x = "Tasa de asesinatos",
       y = "Esperanza de vida (años)",
       caption = "elaboracion propia")
#=============================================
#4. Análisis Ex-Ante (siempre hacer uno)
#=============================================
#matriz de correlacion
#----------------------
matriz_corelacion <- round(cor( x = base_final[,-1]),3) #observamos la correlación entre las variables

install.packages("psych")
library(psych)

multi.hist(x = scale(base_final[,-1]), dcol = c("blue", "red"), dly = c("dotted","solid"))

#herramienta complementaria
#---------------------------
install.packages("GGally") #Genera un resumen de gráficos y correlaciones
library(GGally)

ggpairs(base_final[,-1], lower = list(continuous = "smooth"), 
        dia = list(continuous = "barDiag"), axisLabels = "none")
#=======================================
#5. Estimacion 
#=======================================
options(scipen = 999)
modelo_1 <- lm(esp_vida ~ poblacion + ingresos + analfabetismo + asesinatos + 
                 graduados_secun + heladas  + area, data = base_final)

summary(modelo_1) #el formato que sigue es: modelo <- lm(y~x1+x2+...+xn)
#Hay ciertas variables que no nos ayudan, por tanto ocuparemos
#Stepwise: selecciona las mejores variables para el modelo
step(object = modelo_1, direction = "both", trace = 1)
modelo_2 <- lm(esp_vida ~ poblacion + asesinatos + 
                 graduados_secun + heladas, data = base_final)

summary(modelo_2)
#===============================
#6. Análisis Post-Estimación
#==============================
install.packages("olsrr")
library(olsrr)

ols_plot_diagnostics(modelo_2)
#Test de Normalidad ; comprobamos normalidad en los residuos
ols_test_normality(modelo_2)
#podemos ver que los p-values de lostest son >alpha por tanto aceptamos normalidad en residuos 
#basandonos en el criterio de la mayoria
#-----------------
#Homocedasticidad
#-----------------
#Ho: Homocedasticidad (varianza residuos son constantes)
#H1: Heterocedasticidad (varianza residuos no son constantes)

ols_test_breusch_pagan(modelo_2)
#Goldfeld-Quandt
install.packages("lmtest")
library(lmtest)
library(help = lmtest)

gqtest(modelo_2)

#Para ambos test el p-value>alpha por tanto validamos H0

#------------------
#Test Multicolinealidad
#------------------
install.packages("car")
library(car)

vif(modelo_2)

