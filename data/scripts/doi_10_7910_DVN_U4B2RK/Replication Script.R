## REPLICATION SCRIPT -------------------------------------------------------------------------------
## The ‘withdrawn citizen’. Making sense of the failed constitutional process in Chile. 
## Bulletin of Latin American Research 
## Stéfano Palestini and Rodrigo M. Medel


# NOTES
# All dataframes can be found in the following website https://www.servel.cl/centro-de-datos/resultados-electorales-historicos-gw3/
# Referendum 2020 = Plebiscito 2020
# Referendum 2022 = Plebiscito 2022 



# Libraries and databases ----

pacman::p_load(tidyverse, ggparliament,readxl, ggplot2,ggrepel, scales, reshape,
               dplyr,ggpubr, gridExtra )





setwd("your route")


plebiscito_2020<- read.csv("Plebiscito 2020 (Completo).csv", sep = ";",
                 header = TRUE)


plebiscito_2022<- read.csv("Plebiscito_2022.csv", sep = ";",
                 header = TRUE)





## Recoding -----


plebiscito_2022$Votos.TRICEL=as.numeric(plebiscito_2022$Votos.TRICEL)

servel3=plebiscito_2022%>%dplyr::group_by(Mesa, Local,Circ..Electoral,  Comuna) %>% dplyr::summarize(votacion=sum(Votos.TRICEL, na.rm=TRUE), Electores=first(Electores)) %>%
  dplyr::mutate(Participacion=votacion/Electores)


sum(servel3$votacion, na.rm=TRUE)

plebiscito_2020$Votos.TRICEL=as.numeric(plebiscito_2020$Votos.TRICEL)
class(plebiscito_2020$Votos.TRICEL)

plebiscito_2020_b <-plebiscito_2020%>%dplyr::group_by(Mesas.Fusionadas, Local, Circ.Electoral, Comuna) %>% dplyr::summarize(padron_mesa_2020=mean(Electores, na.rm = TRUE),
                Votacion_mesa_2020=sum(Votos.TRICEL, na.rm=TRUE)) %>% 
  group_by(Local, Circ.Electoral, Comuna) %>% mutate(padron_Local_2020=sum(padron_mesa_2020, na.rm = TRUE), Votacion_Local_2020=sum(Votacion_mesa_2020, na.rm = TRUE), Participacion_Local_2020=(Votacion_Local_2020/padron_Local_2020)) %>% 
  group_by(Circ.Electoral, Comuna) %>% mutate(padron_circ_2020=sum(padron_mesa_2020, na.rm = TRUE), Votacion_circ_2020=sum(Votacion_mesa_2020, na.rm = TRUE), Participacion_circ_2020=(Votacion_circ_2020/padron_circ_2020))%>% 
  group_by(Comuna) %>% mutate(padron_comuna_2020=sum(padron_mesa_2020, na.rm = TRUE), Votacion_comuna_2020=sum(Votacion_mesa_2020, na.rm = TRUE), Participacion_comuna_2020=(Votacion_comuna_2020/padron_comuna_2020))




votacion_apruebo = plebiscito_2022 %>% 
                  filter(grepl('APRUEBO', Opción)) %>%
                  group_by(Local,Circ..Electoral,  Comuna) %>%
                mutate(votos.apruebo_local = sum(Votos.TRICEL, na.rm=TRUE))%>%
                  group_by(Circ..Electoral,  Comuna) %>%
                  mutate(votos.apruebo_circ = sum(Votos.TRICEL, na.rm=TRUE))%>%
                  group_by(Comuna) %>%
                  mutate(votos.apruebo_comuna = sum(Votos.TRICEL, na.rm=TRUE))

    
votacion_apruebo$Electores=NULL


votacion_rechazo <- plebiscito_2022 %>% 
                  group_by(Local,Circ..Electoral,  Comuna) %>%
  mutate(votacion_local = sum(Votos.TRICEL, na.rm=TRUE)) %>%
                  group_by(Circ..Electoral,  Comuna) %>%
  mutate(votacion_circ = sum(Votos.TRICEL, na.rm=TRUE)) %>%
                  group_by( Comuna) %>%
  mutate(votacion_comuna = sum(Votos.TRICEL, na.rm=TRUE)) %>%
                  filter(grepl('RECHAZO', Opción)) %>%
                  group_by(Local,Circ..Electoral,  Comuna) %>%
                  dplyr::mutate(votos.rechazo_local = sum(Votos.TRICEL, na.rm=TRUE)) %>%
                  group_by(Circ..Electoral,  Comuna) %>%
                  dplyr::mutate(votos.rechazo_circ = sum(Votos.TRICEL, na.rm=TRUE)) %>%
                  group_by(Comuna) %>%
                  dplyr::mutate(votos.rechazo_comuna = sum(Votos.TRICEL, na.rm=TRUE))


votacion_rechazo$Electores=NULL

Participacion_2022= plebiscito_2022%>%
  dplyr::group_by(Mesa, Local,Circ..Electoral,  Comuna) %>% 
  dplyr::summarize(votacion=sum(Votos.TRICEL, na.rm=TRUE), Electores=first(Electores)) %>%
  dplyr::mutate(Participacion_2022=votacion/Electores)

sum(Participacion_2022$votacion, na.rm=TRUE)
sum(Participacion_2022$Electores, na.rm=TRUE)


votacion_rechazo$Nro..Región=NULL
votacion_rechazo$Provincia=NULL
votacion_rechazo$Distrito=NULL
votacion_rechazo$Circ..Senatorial=NULL
votacion_rechazo$Región=NULL
votacion_rechazo$Nro..en.Voto=NULL




votacion_comuna2=subset(plebiscito_2020_b, select=c("Comuna", "Circ.Electoral",      "Local", "Participacion_Local_2020",   "Participacion_circ_2020" , "Participacion_comuna_2020" ))

votacion_apruebo=subset(votacion_apruebo, select=c("Comuna", "Circ..Electoral",      "Local", "votos.apruebo_local",  "votos.apruebo_circ",   "votos.apruebo_comuna"))

votacion_comuna2$Circ..Electoral=votacion_comuna2$Circ.Electoral

base.servel=merge(votacion_comuna2, votacion_apruebo, by=c("Local","Circ..Electoral", "Comuna"))
base.servel=merge(base.servel, votacion_rechazo, by=c("Local","Circ..Electoral", "Comuna"))
base.servel=merge(base.servel, Participacion_2022, by=c("Mesa", "Local","Circ..Electoral", "Comuna"))



base.servel$porcentaje.apruebo_local= base.servel$votos.apruebo_local/base.servel$votacion_local
base.servel$porcentaje.rechazo_local= base.servel$votos.rechazo_local/base.servel$votacion_local
base.servel$porcentaje.apruebo_circ= base.servel$votos.apruebo_circ/base.servel$votacion_circ
base.servel$porcentaje.rechazo_circ= base.servel$votos.rechazo_circ/base.servel$votacion_circ
base.servel$porcentaje.apruebo_comuna= base.servel$votos.apruebo_comuna/base.servel$votacion_comuna
base.servel$porcentaje.rechazo_comuna= base.servel$votos.rechazo_comuna/base.servel$votacion_comuna

base.servel=base.servel%>%group_by(Local)%>%mutate(Participacion_2022_local=mean(Participacion_2022, na.rm=TRUE))
base.servel=base.servel%>%group_by(Circ..Electoral)%>%mutate(Participacion_2022_circ=mean(Participacion_2022, na.rm=TRUE))
base.servel=base.servel%>%group_by(Comuna)%>%mutate(Participacion_2022_comuna=mean(Participacion_2022, na.rm=TRUE))




base.plebiscitos <-base.servel%>%group_by(Local,Circ..Electoral, Comuna) %>% dplyr::summarise(Participacion_Local_2020=mean(Participacion_Local_2020, na.rm = TRUE), porcentaje.rechazo_local=mean(porcentaje.rechazo_local, na.rm = TRUE), porcentaje.apruebo_local=mean(porcentaje.apruebo_local, na.rm=TRUE), 
Participacion_2022_local=mean(Participacion_2022_local, na.rm=TRUE), Participacion_circ_2020=mean(Participacion_circ_2020, na.rm = TRUE), porcentaje.rechazo_circ=mean(porcentaje.rechazo_circ, na.rm = TRUE),
porcentaje.apruebo_circ=mean(porcentaje.apruebo_circ, na.rm=TRUE),
Participacion_2022_circ=mean(Participacion_2022_circ, na.rm=TRUE),
Participacion_comuna_2020=mean(Participacion_comuna_2020, na.rm = TRUE), porcentaje.rechazo_comuna=mean(porcentaje.rechazo_comuna, na.rm = TRUE), porcentaje.apruebo_comuna=mean(porcentaje.apruebo_comuna, na.rm=TRUE),
Participacion_2022_comuna=mean(Participacion_2022_comuna, na.rm=TRUE))





## Correlations 1 ----------------------------------------------



base.plebiscitos_comuna=base.plebiscitos%>%group_by(Comuna)%>%summarize(Participacion_comuna_2020=mean(Participacion_comuna_2020, na.rm=TRUE), porcentaje.rechazo_comuna=mean(porcentaje.rechazo_comuna, na.rm=TRUE))

cor.test(base.plebiscitos_comuna$Participacion_comuna_2020, base.plebiscitos_comuna$porcentaje.rechazo_comuna, use="complete.obs", method="spearman")


base.plebiscitos_circ=base.plebiscitos%>%group_by(Circ..Electoral)%>%summarize(Participacion_circ_2020=mean(Participacion_circ_2020, na.rm=TRUE), porcentaje.rechazo_circ=mean(porcentaje.rechazo_circ, na.rm=TRUE))


cor.test(base.plebiscitos_circ$Participacion_circ_2020, base.plebiscitos_circ$porcentaje.rechazo_circ, use="complete.obs", method="spearman")



base.plebiscitos_local=base.plebiscitos%>%group_by(Local)%>%summarize(Participacion_Local_2020=mean(Participacion_Local_2020, na.rm=TRUE), porcentaje.rechazo_local=mean(porcentaje.rechazo_local, na.rm=TRUE))


cor.test(base.plebiscitos_local$Participacion_Local_2020, base.plebiscitos_local$porcentaje.rechazo_local, use="complete.obs", method="spearman")

 


ggplot(base.plebiscitos_comuna, aes( Participacion_comuna_2020, porcentaje.rechazo_comuna,label =Comuna)) +
    geom_text_repel(family = "Times New Roman",
  max.overlaps  = 1, size=2.5) +
  geom_point(color = 'red', size = 3, alpha = 0.7) +
   geom_smooth(method="lm", se=FALSE, linetype = 2,  colour="black")  +
  theme_minimal(base_size = 16) +  # Estilo de tema minimalista
  theme( plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar y negrita para el título
    plot.caption = element_text(size = 10, hjust = 1, face = "italic"),
     axis.title = element_text(face = "bold"),  # Negrita para títulos de los ejes
    axis.text = element_text(color = "black")) + scale_y_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) + scale_x_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))  +
  xlab("Voter turnout in 2020") + ylab("Rejection in 2022") +  labs(title = "", subtitle= "N= 345 / r= - 0.62 / rho= - 0.66") 
 


p1=ggplot(base.plebiscitos_circ, aes( Participacion_circ_2020, porcentaje.rechazo_circ, label = Circ..Electoral)) +
  geom_point(color = 'red', size = 3, alpha = 0.7) +
   geom_smooth(method="lm", se=FALSE, linetype = 2,  colour="black")  +
  theme_minimal(base_size = 16) +  # Estilo de tema minimalista
  theme( plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar y negrita para el título
    plot.caption = element_text(size = 10, hjust = 1, face = "italic"),
     axis.title = element_text(face = "bold"),  # Negrita para títulos de los ejes
    axis.text = element_text(color = "black")) + scale_y_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) + scale_x_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))  +
  xlab("Voter turnout in 2020") + ylab("Rejection in 2022") +  labs(title = "District Level", subtitle= "N= 587 / r= -0.61 / rho= - 0.63") 



p2=ggplot(base.plebiscitos_local, aes( Participacion_Local_2020,porcentaje.rechazo_local, label = Local)) +
  geom_point(color = 'red') +
  geom_point(color = 'red', size = 3, alpha = 0.7) +
   geom_smooth(method="lm", se=FALSE, linetype = 2,  colour="black")  +
  theme_minimal(base_size = 16) +  # Estilo de tema minimalista
  theme( plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar y negrita para el título
    plot.caption = element_text(size = 10, hjust = 1, face = "italic"),
     axis.title = element_text(face = "bold"),  # Negrita para títulos de los ejes
    axis.text = element_text(color = "black")) + scale_y_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) + scale_x_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))  +
  xlab("Voter turnout in 2020") + ylab("Rejection in 2022") +  labs(title = "Polling Place Level", subtitle= " N= 1347 / r= 0.62 / rho= - 0.64") 






grid.arrange(p1, p2, nrow = 1,
  top = "",
  bottom = "")





##   Correlations 2  -----------------------------------------



base.plebiscitos_comuna=base.plebiscitos%>%group_by(Comuna)%>%summarize(Participacion_comuna_2020=mean(Participacion_comuna_2020, na.rm=TRUE), Participacion_2022_comuna=mean(Participacion_2022_comuna, na.rm=TRUE), porcentaje.rechazo_comuna=mean(porcentaje.rechazo_comuna, na.rm=TRUE))


cor.test(base.plebiscitos_comuna$Participacion_comuna_2020, base.plebiscitos_comuna$Participacion_2022_comuna, use="complete.obs", method="spearman")


base.plebiscitos_circ=base.plebiscitos%>%group_by(Circ..Electoral)%>%summarize(Participacion_circ_2020=mean(Participacion_circ_2020, na.rm=TRUE), Participacion_2022_circ=mean(Participacion_2022_circ, na.rm=TRUE), porcentaje.rechazo_circ=mean(porcentaje.rechazo_circ, na.rm=TRUE))


cor.test(base.plebiscitos_circ$Participacion_circ_2020, base.plebiscitos_circ$Participacion_2022_circ, use="complete.obs", method="spearman")



base.plebiscitos_local=base.plebiscitos%>%group_by(Local)%>%summarize(Participacion_Local_2020=mean(Participacion_Local_2020, na.rm=TRUE),Participacion_2022_local=mean(Participacion_2022_local, na.rm=TRUE), porcentaje.rechazo_local=mean(porcentaje.rechazo_local, na.rm=TRUE))


cor.test(base.plebiscitos_local$Participacion_Local_2020, base.plebiscitos_local$Participacion_2022_local, use="complete.obs", method="spearman")




p1=ggplot(base.plebiscitos, aes( Participacion_comuna_2020, Participacion_2022_comuna,label =Comuna)) +
  geom_point(color = 'red', size = 3, alpha = 0.7) +
 geom_text_repel(family = "Times New Roman",
  max.overlaps  = 1, size=2.5) +
   geom_smooth(method="lm", se=FALSE, linetype = 2,  colour="black")  +
  theme_minimal(base_size = 16) +  # Estilo de tema minimalista
  theme( plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar y negrita para el título
    plot.caption = element_text(size = 10, hjust = 1, face = "italic"),
     axis.title = element_text(face = "bold"),  # Negrita para títulos de los ejes
    axis.text = element_text(color = "black")) + scale_y_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) + scale_x_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))  +
  xlab("Voter turnout in 2020") + ylab("Voter turnoutin 2022") +  labs(title = "Municipal Level", subtitle= " N= 345 / rho= 0.42") 

 
 

p2=ggplot(base.plebiscitos_circ, aes( Participacion_circ_2020, Participacion_2022_circ, label = Circ..Electoral)) + 
  geom_point(color = 'red', size = 3, alpha = 0.7) +
   geom_smooth(method="lm", se=FALSE, linetype = 2,  colour="black")  +
  theme_minimal(base_size = 16) +  # Estilo de tema minimalista
  theme( plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar y negrita para el título
    plot.caption = element_text(size = 10, hjust = 1, face = "italic"),
     axis.title = element_text(face = "bold"),  # Negrita para títulos de los ejes
    axis.text = element_text(color = "black")) + scale_y_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) + scale_x_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))  +
  xlab("Voter turnout in 2020") + ylab("Voter turnout in 2022") +  labs(title = "District Level", subtitle= " N= 587 / rho= 0.36") 
 


p3=ggplot(base.plebiscitos_local, aes( Participacion_Local_2020,Participacion_2022_local, label = Local)) +   geom_point(color = 'red', size = 3, alpha = 0.7) +
   geom_smooth(method="lm", se=FALSE, linetype = 2,  colour="black")  +
  theme_minimal(base_size = 16) +  # Estilo de tema minimalista
  theme( plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar y negrita para el título
    plot.caption = element_text(size = 10, hjust = 1, face = "italic"),
     axis.title = element_text(face = "bold"),  # Negrita para títulos de los ejes
    axis.text = element_text(color = "black")) + scale_y_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) + scale_x_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))  +
  xlab("Voter turnout in 2020") + ylab("Voter turnout in 2022") +  labs(title = "Polling Place Level", subtitle= " N= 1347 / rho= 0.25") 
 


grid.arrange(p1, p2, p3, nrow = 1,
  top = "",
  bottom = "")





## Regression models -------------------------------------------------------------


modelo2a=lm(porcentaje.rechazo_comuna~Participacion_comuna_2020 + Participacion_2022_comuna,  data=
base.plebiscitos_comuna)
library(memisc)
mtable(modelo2a)


modelo2b=lm(porcentaje.rechazo_circ~Participacion_circ_2020 + Participacion_2022_circ,  data=
base.plebiscitos_circ)
library(memisc)
mtable(modelo2b)


modelo2c=lm(porcentaje.rechazo_local~Participacion_Local_2020 + Participacion_2022_local,  data=
base.plebiscitos_local)
library(memisc)
mtable(modelo2c)

mtable(modelo2a, modelo2b, modelo2c)


library(car)
VIFs_CAR<-vif(modelo2a)
print(VIFs_CAR)



sjPlot::tab_model(modelo2a, modelo2b, modelo2c)

df_coeficientes <- data.frame(coeficientes = coef(modelo2a), 
                              variables = names(coef(modelo2a)),
                              limites_inferiores = confint(modelo2a)[,1],
                              limites_superiores = confint(modelo2a)[,2])

# Crear el gráfico de coeficientes con intervalos de confianza
df_coeficientes%>% filter(variables!="(Intercept)") %>% ggplot( aes(reorder(variables, coeficientes),y = coeficientes, ymin = limites_inferiores, ymax = limites_superiores)) +
    geom_errorbar(aes(ymin=limites_inferiores, ymax=limites_superiores),
                  width=0.2,
                  size=0.8) +
  geom_text(aes(label = paste(round(coeficientes,2))), position = position_dodge(0.9), vjust =-2, size = 4) +
  geom_pointrange(size = 1, color = "darkred", fill = "white", shape = 21) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "brown") +
  coord_flip() +
  theme_bw(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    plot.caption = element_text(size = 14, hjust = 1, face = "italic"),
    axis.text.y = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "gray85", size = 0.5),
    panel.grid.minor = element_blank()) +
  xlab("") +
  ylab("Beta")   +  scale_x_discrete(labels=c("Participacion_comuna_2020" ="Voter turnbout 2020", "Participacion_2022_comuna" = "Voter turnout 2022")) + 
labs(title = "Predictors of Rejection in the 2022 Referendum",
     subtitle = "Municipal-Level Analysis",
     caption = "R² = 0.512")


