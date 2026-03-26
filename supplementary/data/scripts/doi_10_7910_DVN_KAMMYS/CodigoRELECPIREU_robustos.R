
# TFG - Diego Castel Romero
# Análisis cuantitativo de los PIR en Europa
# Script R - Modelos MCO con errores estándar robustos
# Última actualización: junio de 2025


# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(stringr)
library(car)
library(broom)
library(ggplot2)
library(sandwich)
library(lmtest)

# Leer base de datos
df <- read_excel("ELECPIREU_final.xlsx")

# Estadísticos descriptivos básicos
summary(df)

# Modelo 1: TOTALPIR
mod_totalpir <- lm(TOTALPIR ~ PROTESTAS + DESEMPLEO + DESCONFIEU +
                     LEGADO + COVSIND + DENSIND + CAMBIOPART + 
                     COMPVERDES + COMPDERECHA, data = df)

# Modelo 2: PIRPRINCIPAL
mod_pirprincipal <- lm(PIRPRINCIPAL ~ PROTESTAS + DESEMPLEO + DESCONFIEU +
                         LEGADO + COVSIND + DENSIND + CAMBIOPART + 
                         COMPVERDES + COMPDERECHA, data = df)

# Comprobar multicolinealidad
vif(mod_totalpir)
vif(mod_pirprincipal)

# Coeficientes con errores estándar robustos
robust_totalpir <- coeftest(mod_totalpir, vcov = vcovHC(mod_totalpir, type = "HC1"))
robust_pirprincipal <- coeftest(mod_pirprincipal, vcov = vcovHC(mod_pirprincipal, type = "HC1"))

# Ver resultados
print(robust_totalpir)
print(robust_pirprincipal)


