# Construção do modelo linear múltiple
# datos Año 2017

# leer datos

#library(readr)
datos2017 <- read.delim("C:/Users/sc/Desktop/Karla_corrupcion/datos2017.txt")


View(datos2017) # Mostrar o banco de dados


head(datos2017) # Mostrar as variáveis
str(datos2017)  # Mostrar tipo variáveis


# A 1-nalise a relação entre variáveis

# Cálculo de correlação matricial

round(cor(x = datos2017, method = "pearson"), 3)
#-------------------------

# Método gráfico da matriz de correlação de cálculo

#install.packages("psych")  # Instalar biblioteca psych
library(psych)
multi.hist(x = datos2017, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")

#install.packages("GGally") #Instalar biblioteca psych
library(GGally)
ggpairs(datos2017, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

# -- fin  Coeficiente de correlação do método gráfico final



# 2- Início da construção do modelo


modelo <- lm(CC_EST ~ PV_EST + VA_EST + GE_EST +RQ_EST+ RL_EST, data = datos2017 )
summary(modelo)    #Resumo do modelo


#3. Selecionando os melhores preditores 
#determinar la calidad del modelo va a ser Akaike(AIC).

step(object = modelo, direction = "both", trace = 1)

#O melhor modelo resultante do processo de seleção foi:

modelo <- lm(CC_EST ~ PV_EST + GE_EST + RL_EST, data = datos2017 )
summary(modelo)   
#----

#intervalo de confianza

confint(lm(CC_EST ~ PV_EST + GE_EST + RL_EST , data = datos2017 ))

#4.Validación de condiciones para la regresión múltiple lineal

library(ggplot2)
library(gridExtra)
plot1 <- ggplot(data = datos2017, aes(PV_EST, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos2017, aes(GE_EST, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos2017, aes(RL_EST, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

grid.arrange(plot1, plot2, plot3)

# DISTRIBUCION DE LOS RESIDUOA
qqnorm(modelo$residuals)
qqline(modelo$residuals)
# COMPROBACION
shapiro.test(modelo$residuals)
