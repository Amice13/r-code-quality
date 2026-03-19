#categorizacion ganaderia
library(foreign)
library(dplyr)
library(data.table)
library(plyr)
library(sf)
library(ggplot2)
getwd()
path<-"D:/BID"
setwd(path)
# cultivos <- read.csv("//dapadfs/workspace_cluster_8/AEPS/COLOMBIA_CNA/Total_nacional(csv)/S06A(Cultivos).csv")%>%
#   as_tibble
# 
# UnidadProductora <- read.csv("//dapadfs/workspace_cluster_8/AEPS/COLOMBIA_CNA/Total_nacional(csv)/S01_15(Unidad_productora).csv")%>%
#   as_tibble()
# 
# #UnidadProductora <- read.csv('D:/JhonValencia/BID/Total_nacional(spss)/Total_nacional/S01_15(Unidad_productora).csv',sep=",")%>%
# #  as_tibble()
# 
# personas <- read.csv("//dapadfs/workspace_cluster_8/AEPS/COLOMBIA_CNA/Total_nacional(csv)/S15P(Personas).csv")%>%
#   as_tibble()
# 
# # mpios <- read.csv('//dapadfs/workspace_cluster_8/AEPS/COLOMBIA_CNA/mpios.csv',sep=",")%>%
# #   as_tibble()
# 
# veredas<-st_read("D:/JhonValencia/CRVeredas_2017/CRVeredas_2017.shp")

names(veredas)
veredas<-veredas%>%
  select(P_MUNIC,CODIGO_VER,NOM_DEP,NOMB_MPIO,NOMBRE_VER)%>%
  dplyr::rename(DPTOMPIO=P_MUNIC)%>%
  as_tibble()

vereda_test<-veredas%>%   #aqui trato de agrupar por mpio y deptos, y el codigo mpio promediarlo por cada nombre de mpio
  select(DPTOMPIO, NOM_DEP,NOMB_MPIO, CODIGO_VER)%>%
  mutate(DPTOMPIO=as.numeric(levels(DPTOMPIO)))%>%
  group_by(CODIGO_VER,NOM_DEP,NOMB_MPIO)%>%
  dplyr::summarise(DPTOMPIO=mean(DPTOMPIO))%>%
  as_tibble()
# 
# vereda_test<-vereda_test%>%
#   select(-geometry)%>%
#   as_tibble()
# View(vereda_test)
# vereda_test<-veredas %>%
#   group_by(NOMB_MPIO, NOM_DEP) %>%
#   mutate(DPTOMPIO=as.numeric(levels(DPTOMPIO))[DPTOMPIO],  sort=F)%>%
#   dplyr::summarize(DPTOMPIO = mean(DPTOMPIO))

#########carga archivos ###########
santander<-read.csv("D:/BID/crudos_priorizados/68Santander/S01_15(Unidad_productora)/CNA2014_ENCABEZADO_68.csv")

santander1<-santander%>%
  #filter(P_MUNIC==68235)%>%
  filter(P_S7P82==1)%>%
  dplyr::select(P_DEPTO,P_MUNIC, ENCUESTA, COD_VEREDA, P_S7P78, P_S7P82, P_S5PAUTOS,P_S7P83D , P_S7P83B,P_S7P83C,P_S7P83D,P_S7P83E,P_S7P83F,P_S7P84B,P_S7P84C,P_S7P84D,P_S7P84E,P_S7P83E, P_S7P84E,P_S7P83B,P_S7P84B,P_S7P84F,P_S7P85B,P_S7P83A,P_S7P82,P_S5PAUTOS,P_S6P66,P_S6P70_SP99,P_S6P76_SP1,P_S6P76_SP2,P_S6P76_SP3,P_S6P76_SP4,P_S6P77_SP1,P_S6P77_SP2,P_S6P77_SP3,P_S6P77_SP4,P_S6P77_SP5,P_S6P77_SP6,P_S6P77_SP7,P_S6P77_SP11,P_S7P80_SP1,P_S7P80_SP2,P_S7P80_SP3,P_S7P80_SP4,P_S11P124_SP1,P_S11P124_SP2,P_S11P124_SP3,P_S11P124_SP4,P_S11P124_SP5,P_S11P124_SP6,P_S11P124_SP7,P_S11P124_SP8,P_S11P124_SP9,P_S11P124_SP10,P_S11P124_SP11,P_S11P125_SP1,P_S11P125_SP2,P_S11P125_SP3,P_S11P125_SP4,P_S11P125_SP5,P_S11P125_SP6,P_S11P125_SP7,P_S11P125_SP8,P_S11P125_SP9,P_S11P125_SP10,P_S11P125_SP11,P_S11P125_SP12,P_S11P126_SP1,P_S11P126_SP2,P_S11P126_SP3,P_S11P126_SP4,P_S11P126_SP5,P_S11P126_SP6,P_S11P126_SP7,P_S11P126_SP8,P_S11P126_SP9,P_S11P127_SP1,P_S11P127_SP2,P_S11P127_SP3,P_S11P127_SP4,P_S11P127_SP5,P_S11P127_SP6,P_S11P127_SP7,P_S11P127_SP8,P_S11P127_SP9,P_S11P127_SP10,P_S11P127_SP11,P_S11P131_SP13,P_S11P135,P_S11P135A_SP1,P_S11P135A_SP2,P_S11P135A_SP3,P_S11P135A_SP4,P_S11P135A_SP5,P_S11P135A_SP6,P_S11P135A_SP7,P_S11P135A_SP8,P_S11P135A_SP9,P_S11P135A_SP10,P_S11P136,P_S11P136A,P_S12P142,P_S12P143,P_S12P144,P_S12P145,P_S12P146,P_S12P147,P_S12P148,P_S12P149,
)%>% #area_vereda_pendiente
  replace(is.na(.), 0)%>%
  as_tibble()

guajira<-read.csv("D:/BID/crudos_priorizados/44Guajira/S01_15(Unidad_productora)/CNA2014_ENCABEZADO_44.csv")
guajira1<-guajira%>%
  #filter(P_MUNIC==44090)%>%
  filter(P_S7P82==1)%>%
  dplyr::select(P_DEPTO,P_MUNIC, ENCUESTA, COD_VEREDA, P_S7P78, P_S7P82, P_S5PAUTOS,P_S7P83D,P_S7P83B,P_S7P83C,P_S7P83D,P_S7P83E,P_S7P83F,P_S7P84B,P_S7P84C,P_S7P84D,P_S7P84E ,P_S7P83E, P_S7P84E,P_S7P83B,P_S7P84B,P_S7P84F,P_S7P85B,P_S7P83A,P_S7P82,P_S5PAUTOS,P_S6P66,P_S6P70_SP99,P_S6P76_SP1,P_S6P76_SP2,P_S6P76_SP3,P_S6P76_SP4,P_S6P77_SP1,P_S6P77_SP2,P_S6P77_SP3,P_S6P77_SP4,P_S6P77_SP5,P_S6P77_SP6,P_S6P77_SP7,P_S6P77_SP11,P_S7P80_SP1,P_S7P80_SP2,P_S7P80_SP3,P_S7P80_SP4,P_S11P124_SP1,P_S11P124_SP2,P_S11P124_SP3,P_S11P124_SP4,P_S11P124_SP5,P_S11P124_SP6,P_S11P124_SP7,P_S11P124_SP8,P_S11P124_SP9,P_S11P124_SP10,P_S11P124_SP11,P_S11P125_SP1,P_S11P125_SP2,P_S11P125_SP3,P_S11P125_SP4,P_S11P125_SP5,P_S11P125_SP6,P_S11P125_SP7,P_S11P125_SP8,P_S11P125_SP9,P_S11P125_SP10,P_S11P125_SP11,P_S11P125_SP12,P_S11P126_SP1,P_S11P126_SP2,P_S11P126_SP3,P_S11P126_SP4,P_S11P126_SP5,P_S11P126_SP6,P_S11P126_SP7,P_S11P126_SP8,P_S11P126_SP9,P_S11P127_SP1,P_S11P127_SP2,P_S11P127_SP3,P_S11P127_SP4,P_S11P127_SP5,P_S11P127_SP6,P_S11P127_SP7,P_S11P127_SP8,P_S11P127_SP9,P_S11P127_SP10,P_S11P127_SP11,P_S11P131_SP13,P_S11P135,P_S11P135A_SP1,P_S11P135A_SP2,P_S11P135A_SP3,P_S11P135A_SP4,P_S11P135A_SP5,P_S11P135A_SP6,P_S11P135A_SP7,P_S11P135A_SP8,P_S11P135A_SP9,P_S11P135A_SP10,P_S11P136,P_S11P136A,P_S12P142,P_S12P143,P_S12P144,P_S12P145,P_S12P146,P_S12P147,P_S12P148,P_S12P149,
  )%>%  #area_vereda_pendiente
  replace(is.na(.), 0)%>%
  as_tibble()

sucre<-read.csv("D:/BID/crudos_priorizados/70Sucre/S01_15(Unidad_productora)/CNA2014_ENCABEZADO_70.csv")
sucre1<-sucre%>%
  #filter(P_MUNIC==70713)%>%
  filter(P_S7P82==1)%>%
  dplyr::select(P_DEPTO,P_MUNIC, ENCUESTA, COD_VEREDA, P_S7P78, P_S7P82, P_S5PAUTOS,P_S7P83D ,P_S7P83E,P_S7P83B,P_S7P83C,P_S7P83D,P_S7P83E,P_S7P83F,P_S7P84B,P_S7P84C,P_S7P84D,P_S7P84E, P_S7P84E,P_S7P83B,P_S7P84B,P_S7P84F,P_S7P85B,P_S7P83A,P_S7P82,P_S5PAUTOS,P_S6P66,P_S6P70_SP99,P_S6P76_SP1,P_S6P76_SP2,P_S6P76_SP3,P_S6P76_SP4,P_S6P77_SP1,P_S6P77_SP2,P_S6P77_SP3,P_S6P77_SP4,P_S6P77_SP5,P_S6P77_SP6,P_S6P77_SP7,P_S6P77_SP11,P_S7P80_SP1,P_S7P80_SP2,P_S7P80_SP3,P_S7P80_SP4,P_S11P124_SP1,P_S11P124_SP2,P_S11P124_SP3,P_S11P124_SP4,P_S11P124_SP5,P_S11P124_SP6,P_S11P124_SP7,P_S11P124_SP8,P_S11P124_SP9,P_S11P124_SP10,P_S11P124_SP11,P_S11P125_SP1,P_S11P125_SP2,P_S11P125_SP3,P_S11P125_SP4,P_S11P125_SP5,P_S11P125_SP6,P_S11P125_SP7,P_S11P125_SP8,P_S11P125_SP9,P_S11P125_SP10,P_S11P125_SP11,P_S11P125_SP12,P_S11P126_SP1,P_S11P126_SP2,P_S11P126_SP3,P_S11P126_SP4,P_S11P126_SP5,P_S11P126_SP6,P_S11P126_SP7,P_S11P126_SP8,P_S11P126_SP9,P_S11P127_SP1,P_S11P127_SP2,P_S11P127_SP3,P_S11P127_SP4,P_S11P127_SP5,P_S11P127_SP6,P_S11P127_SP7,P_S11P127_SP8,P_S11P127_SP9,P_S11P127_SP10,P_S11P127_SP11,P_S11P131_SP13,P_S11P135,P_S11P135A_SP1,P_S11P135A_SP2,P_S11P135A_SP3,P_S11P135A_SP4,P_S11P135A_SP5,P_S11P135A_SP6,P_S11P135A_SP7,P_S11P135A_SP8,P_S11P135A_SP9,P_S11P135A_SP10,P_S11P136,P_S11P136A,P_S12P142,P_S12P143,P_S12P144,P_S12P145,P_S12P146,P_S12P147,P_S12P148,P_S12P149,
  )%>% #area_vereda_pendiente
  replace(is.na(.), 0)%>%
  as_tibble()



Arauca<-read.csv("D:/BID/crudos_priorizados/81Arauca/S01_15(Unidad_productora)/CNA2014_ENCABEZADO_81.csv")
Arauca1<-Arauca%>%
  #filter(P_MUNIC==81300 )%>%
  filter(P_S7P82==1)%>%
  dplyr::select(P_DEPTO,P_MUNIC, ENCUESTA, COD_VEREDA, P_S7P78, P_S7P82, P_S5PAUTOS,P_S7P83D,P_S7P83B,P_S7P83C,P_S7P83D,P_S7P83E,P_S7P83F,P_S7P84B,P_S7P84C,P_S7P84D,P_S7P84E ,P_S7P83E, P_S7P84E,P_S7P83B,P_S7P84B,P_S7P84F,P_S7P85B,P_S7P83A,P_S7P82,P_S5PAUTOS,P_S6P66,P_S6P70_SP99,P_S6P76_SP1,P_S6P76_SP2,P_S6P76_SP3,P_S6P76_SP4,P_S6P77_SP1,P_S6P77_SP2,P_S6P77_SP3,P_S6P77_SP4,P_S6P77_SP5,P_S6P77_SP6,P_S6P77_SP7,P_S6P77_SP11,P_S7P80_SP1,P_S7P80_SP2,P_S7P80_SP3,P_S7P80_SP4,P_S11P124_SP1,P_S11P124_SP2,P_S11P124_SP3,P_S11P124_SP4,P_S11P124_SP5,P_S11P124_SP6,P_S11P124_SP7,P_S11P124_SP8,P_S11P124_SP9,P_S11P124_SP10,P_S11P124_SP11,P_S11P125_SP1,P_S11P125_SP2,P_S11P125_SP3,P_S11P125_SP4,P_S11P125_SP5,P_S11P125_SP6,P_S11P125_SP7,P_S11P125_SP8,P_S11P125_SP9,P_S11P125_SP10,P_S11P125_SP11,P_S11P125_SP12,P_S11P126_SP1,P_S11P126_SP2,P_S11P126_SP3,P_S11P126_SP4,P_S11P126_SP5,P_S11P126_SP6,P_S11P126_SP7,P_S11P126_SP8,P_S11P126_SP9,P_S11P127_SP1,P_S11P127_SP2,P_S11P127_SP3,P_S11P127_SP4,P_S11P127_SP5,P_S11P127_SP6,P_S11P127_SP7,P_S11P127_SP8,P_S11P127_SP9,P_S11P127_SP10,P_S11P127_SP11,P_S11P131_SP13,P_S11P135,P_S11P135A_SP1,P_S11P135A_SP2,P_S11P135A_SP3,P_S11P135A_SP4,P_S11P135A_SP5,P_S11P135A_SP6,P_S11P135A_SP7,P_S11P135A_SP8,P_S11P135A_SP9,P_S11P135A_SP10,P_S11P136,P_S11P136A,P_S12P142,P_S12P143,P_S12P144,P_S12P145,P_S12P146,P_S12P147,P_S12P148,P_S12P149,
  )%>%  #area_vereda_pendiente 
  replace(is.na(.), 0)%>%
  as_tibble()

tame<-Arauca%>%
  filter(P_MUNIC==81794)%>%
  filter(P_S7P82==1)%>%
  dplyr::select(P_DEPTO,P_MUNIC,COD_VEREDA,P_S7P82,P_S5PAUTOS,P_S6P66,P_S6P70_SP99,P_S6P76_SP1,P_S6P76_SP2,P_S6P76_SP3,P_S6P76_SP4,P_S6P77_SP1,P_S6P77_SP2,P_S6P77_SP3,P_S6P77_SP4,P_S6P77_SP5,P_S6P77_SP6,P_S6P77_SP7,P_S6P77_SP11,P_S7P80_SP1,P_S7P80_SP2,P_S7P80_SP3,P_S7P80_SP4,P_S11P124_SP1,P_S11P124_SP2,P_S11P124_SP3,P_S11P124_SP4,P_S11P124_SP5,P_S11P124_SP6,P_S11P124_SP7,P_S11P124_SP8,P_S11P124_SP9,P_S11P124_SP10,P_S11P124_SP11,P_S11P125_SP1,P_S11P125_SP2,P_S11P125_SP3,P_S11P125_SP4,P_S11P125_SP5,P_S11P125_SP6,P_S11P125_SP7,P_S11P125_SP8,P_S11P125_SP9,P_S11P125_SP10,P_S11P125_SP11,P_S11P125_SP12,P_S11P126_SP1,P_S11P126_SP2,P_S11P126_SP3,P_S11P126_SP4,P_S11P126_SP5,P_S11P126_SP6,P_S11P126_SP7,P_S11P126_SP8,P_S11P126_SP9,P_S11P127_SP1,P_S11P127_SP2,P_S11P127_SP3,P_S11P127_SP4,P_S11P127_SP5,P_S11P127_SP6,P_S11P127_SP7,P_S11P127_SP8,P_S11P127_SP9,P_S11P127_SP10,P_S11P127_SP11,P_S11P131_SP13,P_S11P135,P_S11P135A_SP1,P_S11P135A_SP2,P_S11P135A_SP3,P_S11P135A_SP4,P_S11P135A_SP5,P_S11P135A_SP6,P_S11P135A_SP7,P_S11P135A_SP8,P_S11P135A_SP9,P_S11P135A_SP10,P_S11P136,P_S11P136A,P_S12P142,P_S12P143,P_S12P144,P_S12P145,P_S12P146,P_S12P147,P_S12P148,P_S12P149,
  )%>%  #area_vereda_pendiente
  replace(is.na(.), 0)%>%
  as_tibble()

UnidadProductora<-rbind(santander1,guajira1,sucre1,Arauca1)


head(UnidadProductora)

UnidadProductora2<-UnidadProductora%>%   #aqui trato de agrupar por mpio y deptos, y el codigo mpio promediarlo por cada nombre de mpio
  dplyr::select(P_DEPTO,P_MUNIC, ENCUESTA, COD_VEREDA, P_S7P83C,P_S7P78, P_S7P82, P_S5PAUTOS, P_S7P83B,P_S7P83C,P_S7P83D,P_S7P83E,P_S7P83F,P_S7P84B,P_S7P84C,P_S7P84D,P_S7P84E,P_S7P83D ,P_S7P83E, P_S7P84E,P_S7P83B,P_S7P84B,P_S7P84F,P_S7P85B,P_S7P83A)%>%
  #filter(P_MUNIC%in% c("81794", "70713", "81300","44090","68235"))%>%
  as_tibble()
#select(DPTOMPIO, NOM_DEP,NOMB_MPIO, CODIGO_VER)%>%
#mutate(DPTOMPIO=as.numeric(levels(DPTOMPIO)))%>%
#group_by(CODIGO_VER,NOM_DEP,NOMB_MPIO)%>%
#dplyr::summarise(DPTOMPIO=mean(DPTOMPIO))%>%
#as_tibble()

#write.csv(UnidadProductora2, "D:/JhonValencia/BID/Total_nacional(spss)/Total_nacional/variables_mpios.csv")
UnidadProductora<-read.csv("D:/BID/unidad_produc.csv")%>%
  as_tibble()

names(UnidadProductora)

UnidadProductora<-UnidadProductora %>% replace(is.na(.), 0)

UPA_Ganadera_hoy<-UnidadProductora%>%
  select(-P_S7P82)%>%
  filter(P_S7P78==1)%>% #Tenencia ultimos doce meses de ganado bovino
  as_tibble()

# UPA_Ganadera_hoy<-UPA_Ganadera_hoy%>%
#   mutate(filtro1=((P_S7P83D + P_S7P83E)/P_S7P83A)*100, na.rm=TRUE)%>%
#   mutate(filtro2=(P_S7P84E/P_S7P83A)*100, na.rm=TRUE)%>%
#   mutate(filtro3=(((P_S7P83B+P_S7P84B)/P_S7P83A)*100))%>%
#   mutate(filtro4=(P_S7P85B/P_S7P84F))%>%
#   mutate(filtro5=(P_S7P84F/P_S7P83A)*100)%>%
#   mutate(filtro6=(P_S7P83B/P_S7P83A)*100)%>%
#   #tidyr::drop_na()


############### filtro vocacion ###############
write.csv(UPA_Ganadera, "D:/BID/crudos_priorizados/base_variables.csv")
UPA_Ganadera_hoy<-UnidadProductora2%>%
  #filter(P_MUNIC==70713)%>% #solo san onofre P_S7P83B,P_S7P83C,P_S7P83D,P_S7P83E,P_S7P83F,P_S7P84B,P_S7P84C,P_S7P84D,P_S7P84E
  mutate(tot_machos=P_S7P83B+P_S7P83C+P_S7P83D+P_S7P83E+P_S7P83F,
         tot_machos_sin_R=P_S7P83B+P_S7P83C+P_S7P83D+P_S7P83E,
         tot_hembras=P_S7P84B+P_S7P84C+P_S7P84D+P_S7P84E,
         Tot_ganado=tot_machos_sin_R+tot_hembras)%>%
  mutate(filtro1=((P_S7P83D+P_S7P83E)/(Tot_ganado))*100,  #ok
         filtro2=((P_S7P84E)/(Tot_ganado))*100,  #ok
         filtro3=((P_S7P85B/P_S7P84F)), 
         filtro4=P_S7P83B/Tot_ganado*100,
         filtro5=((P_S7P83B+P_S7P84B)/Tot_ganado)*100,
         filtro6=P_S7P84F/Tot_ganado*100)%>% 
    # mutate(MPIO=ifelse(P_MUNIC== "44090","DIBULLA", 
    #                  ifelse(P_MUNIC== "68235","CHUCURÍ",
    #                         ifelse(P_MUNIC== "70713","SAN ONOFRE",
    #                                ifelse(P_MUNIC== "81300","TAME",
    #                                       ifelse(P_MUNIC== "81794","FORTUL","otros"))))))%>%
  replace(is.na(.),0)%>%
  as_tibble()

#########################
#Original
# Ceba= filtro1>=60, 
# Cria=filtro1<60 & filtro2>=20 & filtro3<2.5 & filtro5>=15, 
# Leche=filtro1<60 & filtro3>=9.75 & filtro4<5 & filtro6>=10, 
# DP1=filtro1<60 & between(filtro3,2.5,9.75) & filtro6>=10, 
# Dp2=filtro1<60 & filtro3>=9.75 & filtro4>=5 & filtro6>=10, 
# Carne1=filtro1<60 & filtro2>=20 & filtro3<2.5 &  filtro5<15, 
# Carne2=filtro1<60 & filtro2<20 & filtro3<2.5, 
# Carne3=filtro1<60 & filtro3>=2.5 & filtro6<10

#################

UPA_Ganadera<-UPA_Ganadera_hoy%>%
  select(-P_MUNIC)%>%
  replace(is.na(.),0)%>%
  mutate(Ceba= filtro1>=60, 
         Cria=filtro1<60 & filtro2>=20 & filtro3<2.5 & filtro5>=15, 
         Leche=filtro1<60 & filtro3>=9.75 & filtro4<5 & filtro6>=10, 
         DP1=filtro1<60 & between(filtro3,2.5,9.75) & filtro6>=10, 
         Dp2=filtro1<60 & filtro3>=9.75 & filtro4>=5 & filtro6>=10, 
         Carne1=filtro1<60 & filtro2>=20 & filtro3<2.5 &  filtro5<15, 
         Carne2=filtro1<60 & filtro2<20 & filtro3<2.5, 
         Carne3=filtro1<60 & filtro3>=2.5 & filtro6<10)%>%
  as_tibble()

UPA_Ganadera<-UPA_Ganadera*1

#agrupar categorias
x<-UPA_Ganadera%>%
  mutate(Departamento=ifelse(P_DEPTO== "68","Santander", 
                     ifelse(P_DEPTO== "44","Guajira",
                            ifelse(P_DEPTO== "70","Sucre",
                                   ifelse(P_DEPTO== "81","Arauca","otros"
                                         # ifelse(P_DEPTO== "81794","FORTUL","otros"),
                                          )))))


write.csv(x,"C:/Users/jhonValencia/Pictures/Test_departamentos_ganado/base_departamental_orientaciones.csv")
UPA_GanaderaMpal<-x%>%
  dplyr::rename(UPAS=P_S7P82)%>%
  select(P_DEPTO,UPAS,Ceba,Cria,Leche,DP1,Dp2,Carne1,Carne2,Carne3)%>%
  mutate(Carne=Carne1+Carne2+Carne3+Ceba, DP=DP1+Dp2)%>%
  select(P_DEPTO,UPAS,Cria,Leche,DP,Carne)%>%
  group_by(P_DEPTO)%>%
  dplyr::summarise_all(sum)%>%
  mutate(UPAS_filtros=Cria+Carne+DP+Leche)%>%
  mutate(Faltantes=UPAS-UPAS_filtros)%>%
  mutate(Departamento=ifelse(P_DEPTO== "68","Santander", 
                             ifelse(P_DEPTO== "44","Guajira",
                                    ifelse(P_DEPTO== "70","Sucre",
                                           ifelse(P_DEPTO== "81","Arauca","otros"
                                                  # ifelse(P_DEPTO== "81794","FORTUL","otros"),
                                           )))))
#resumen cabezas de ganado

Cabezas<-x%>%
  select(P_DEPTO,tot_machos_sin_R,tot_hembras,Tot_ganado)%>%
  dplyr::rename(Machos=tot_machos_sin_R,Hembras=tot_hembras, Total_Ganado=Tot_ganado)%>%
  group_by(P_DEPTO)%>%
  dplyr::summarise_all(sum)%>%
  mutate(Departamento=ifelse(P_DEPTO== "68","Santander", 
                             ifelse(P_DEPTO== "44","Guajira",
                                    ifelse(P_DEPTO== "70","Sucre",
                                           ifelse(P_DEPTO== "81","Arauca","otros"
                                                  # ifelse(P_DEPTO== "81794","FORTUL","otros"),
                                           )))))%>%
  select(-P_DEPTO)%>%
  as_tibble()




###plots

# Filtro de datos
data<-UPA_GanaderaMpal%>%
  select(-P_DEPTO,-Faltantes,-UPAS,-UPAS_filtros)%>%
  as_tibble()
#install.packages("reshape2")
#ibrary("reshape2")
data <- melt(data, id="Departamento") 

# Barras por orientaciones
ggplot(data, aes(fill=variable, y=value, x=Departamento)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(title = "Orientaciones", x="Departamentos", y="Número de UPAS")

#grafico torta orientaciones mpales
ggplot(data = data, aes(x = "", y = value, fill = variable  )) + 
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = value), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~ Departamento)  +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  theme(legend.position='right') + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE))+
  labs(title = "Orientaciones Departamentales")

#grafico orientaciones No cabezas de ganado mpal

#ibrary("reshape2")
ganado <- melt(Cabezas, id="Departamento") 

# Grouped


ggplot(ganado, aes(fill=variable, y=value, x=Departamento)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(title = "Población bovina", x="Departamentos", y="No de cabezas")

write.csv(x,"D:/BID/crudos_priorizados/BD_ganaderia_V5.csv")

#######################


mydf %>%
  arrange(desc(value)) %>%
  mutate(prop = percent(value / sum(value))) -> mydf 

pie <- ggplot(mydf, aes(x = "", y = value, fill = fct_inorder(Group))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_label_repel(aes(label = prop), size=5, show.legend = F, nudge_x = 1) +
  guides(fill = guide_legend(title = "Group"))

mydf <- structure(list(Group = structure(c(3L, 1L, 2L), .Label = c("Negative", 
                                                                   "Neutral", "Positive"), class = "factor"), value = c(52L, 239L, 
                                                                                                                        9L)), .Names = c("Group", "value"), class = "data.frame", row.names = c("1", 
                                                                                                                                                                                                "2", "3"))

write.csv(Ceba, "D:/JhonValencia/Carne/Ceba.scv")
Carne<-UPA_Ganadera_hoy%>%
  filter(filtro1<60 & filtro2>0 & filtro3>0 & (between(filtro4,0, 2.44)) & filtro5>0 & filtro6>0)%>%
  select(ENCUESTA,P_S7P78)%>%
  dplyr::rename(Fincas_Carne_mix1=P_S7P78)%>%
  as_tibble()
write.csv(Ceba, "D:/JhonValencia/Carne/Ceba.scv")
Leche1<-UPA_Ganadera_hoy%>%
  filter(filtro1<60 & filtro2>40 & between(filtro3,20,45) & between(filtro4,2.45,9.74) & filtro5>12 & between(filtro6,5,35))%>%
  select(ENCUESTA,P_S7P78)%>%
  dplyr::rename(Fincas_Leche1=P_S7P78)%>%
  as_tibble()
write.csv(Ceba, "D:/JhonValencia/Carne/Ceba.scv")
DP1<-UPA_Ganadera_hoy%>%
  filter(filtro1<60 & filtro2>40 & filtro3<40 & filtro4>0 & filtro5<40 & filtro6<40)%>%
  select(ENCUESTA,P_S7P78)%>%
  dplyr::rename(Fincas_DP1=P_S7P78)%>%
  as_tibble()
write.csv(Ceba, "D:/JhonValencia/Carne/Ceba.scv")
DP_Carne1<-UPA_Ganadera_hoy%>%
  filter(filtro1<60 & filtro2>0 & filtro3>0 & between(filtro4, 2.45,9.74) & filtro5<12 & filtro6>0)%>%
  select(ENCUESTA,P_S7P78)%>%
  dplyr::rename(Fincas_DP_Carne1=P_S7P78)%>%
  as_tibble()
write.csv(Ceba, "D:/JhonValencia/Carne/Ceba.scv")


tabla<-EncuestasMpio%>%
  left_join(Cria1, by='ENCUESTA') %>%
  left_join(Ceba1, by='ENCUESTA') %>%
  left_join(., Carne_mix1, by='ENCUESTA')%>%
  left_join(., Leche1, by='ENCUESTA')%>%
  left_join(., DP1, by='ENCUESTA')%>%
  left_join(., DP_Carne1, by='ENCUESTA')%>%
  replace(is.na(.),0)%>%
  
  as_tibble()


write.csv(UPA_Ganadera_hoy,"D:/BID/Upa_ganadera.csv ")
#Conteo upas
UPA_ganadera_hoy_1<-UPA_Ganadera_hoy%>%
  select(P_MUNIC,P_S7P78)%>%
  group_by(P_MUNIC,P_S7P78)%>%
  dplyr::summarise(UPAS=n())%>%
ungroup()
summarise(P_S15P173_count = n()) %>% 
  ungroup()

#Categorización 

Ceba<-UPA_Ganadera_hoy%>%
  #mutate(MPIO=ifelse(P_MUNIC=="44090", "Dibulla" | P_MUNIC=="81794", "Tame" | P_MUNIC=="70713", "San Onofre" | P_MUNIC=="68235", "Chucurí", | P_MUNIC=="81300", "Fortul")
  filter(filtro1>60)# & filtro2<40 & filtro3<40 & filtro4>0 & filtro5<40 & filtro6<40)%>%
  as_tibble()
#mutate(MPIO=ifelse(P_MUNIC=="44090", "Dibulla" | P_MUNIC=="81794", "Tame"  )
# View(Ceba)
# write.csv(Ceba,"D:/BID/Ceba.csv ")

Cria<-UPA_Ganadera_hoy%>%
  filter(filtro1<60 & (between(filtro2, 41, 60)) & (between(filtro3, 20, 45)) & filtro4<2.44 & filtro5>0 & between(filtro6, 5,35))
write.csv(Cria,"D:/BID/cria.csv ")
Carne_mix<-UPA_Ganadera_hoy%>%
  filter(filtro1<60 & filtro2>0 & filtro3>0 & (between(filtro4,0, 2.44)) & filtro5>0 & filtro6>0)
write.csv(Carne_mix,"D:/BID/Carne_mix.csv ")
Leche<-UPA_Ganadera_hoy%>%
  filter(filtro1<60 & filtro2>40 & between(filtro3,20,45) & between(filtro4,2.45,9.74) & filtro5>12 & between(filtro6,5,35))
write.csv(Leche,"D:/BID/Leche.csv ")
DP<-UPA_Ganadera_hoy%>%
  filter(filtro1<60 & filtro2>40 & filtro3<40 & filtro4>0 & filtro5<40 & filtro6<40)
write.csv(DP,"D:/BID/DP.csv ")
DP_Carne<-UPA_Ganadera_hoy%>%
  filter(filtro1<60 & filtro2>0 & filtro3>0 & between(filtro4, 2.45,9.74) & filtro5<12 & filtro6>0)
write.csv(DP_Carne,"D:/BID/DP_Carne.csv ")



san_onofre<-UPA_Ganadera%>%
  filter(P_MUNIC=="70713")%>%
  
  as_tibble()

cabezas_ganado<-san_onofre$P_S7P85B%>%
  #select(P_MUNIC,COD_VEREDA,P_S7P85B,P_S7P84F, P_S7P83C, P_S7P83D,P_S7P83A,P_S7P84A)
  #replace(is.na(.),0)%>%
  mutate_all(funs(ifelse(is.na(.),0,.)))%>%
  mutate(rendimiento=P_S7P85B/P_S7P84F)%>%
  as_tibble()

########################
#Procesamiento matriz de 80 variables seleccionadas
 

san_onofre$P_S11P135A_SP1[san_onofre$P_S11P135A_SP1>1] <- 0
san_onofre$P_S11P135A_SP2[san_onofre$P_S11P135A_SP2>1] <- 0
san_onofre$P_S11P135A_SP3[san_onofre$P_S11P135A_SP3>1] <- 0
san_onofre$P_S11P135A_SP4[san_onofre$P_S11P135A_SP4>1] <- 0
san_onofre$P_S11P135A_SP5[san_onofre$P_S11P135A_SP5>1] <- 0
san_onofre$P_S11P135A_SP6[san_onofre$P_S11P135A_SP6>1] <- 0
san_onofre$P_S11P135A_SP7[san_onofre$P_S11P135A_SP7>1] <- 0
san_onofre$P_S11P135A_SP8[san_onofre$P_S11P135A_SP8>1] <- 0
san_onofre$P_S11P135A_SP9[san_onofre$P_S11P135A_SP9>1] <- 0
san_onofre$P_S11P135A_SP10[san_onofre$P_S11P135A_SP10>1] <- 0
san_onofre$P_S11P136[san_onofre$P_S11P136>1] <- 0
san_onofre$P_S11P136A[san_onofre$P_S11P136A>1] <- 0





# sum(UPA_Ganadera$P_S11P136)


CereteBd<-test2%>%
  select(-ID_unico.x,-P_DEPTO,-P_MUNIC.y,-UC_UO.y,-COD_VEREDA.y,-ID_unico.y )%>%
  #select(MPIO)%>%
  group_by(P_MUNIC.x)%>%
  dplyr::summarise_all(sum,na.rm = TRUE)
  # mutate(UPAS_filtros=Ceba+Cria+Carne+Leche+DP+DP_leche_carne)%>%
  # mutate(Faltantes=UPAS-UPAS_filtros)

write.csv(san_onofre, "D:/GIZ/Info_base_San_onofre/BD_san_onofre.csv")

san_onofre%>%
  group_by(P_S5PAUTOS,)

BD_sanonofre<-san_onofre%>%
    mutate(UPAS=1072,
           Area_finca=P_S5PAUTOS*0.0001, #m2 a Ha
           utiliza_riego=(P_S6P70_SP99/P_S7P82)*100,
           Manejo_suelo=rowSums(select(.,P_S6P76_SP1:P_S6P76_SP4)),
           Fer_organico=(P_S6P76_SP1/Manejo_suelo)*100,
           Fer_quimico=((P_S6P76_SP2)/Manejo_suelo)*100,
           Enmienda=(P_S6P76_SP3/Manejo_suelo)*100,
           Quemas=(P_S6P76_SP4/Manejo_suelo)*100,
           manejo_plagas_malezas=rowSums(select(.,P_S6P77_SP1:P_S6P77_SP11)),
           Control_manual=(P_S6P77_SP1/manejo_plagas_malezas)*100,
           Control_organico=(P_S6P77_SP2/manejo_plagas_malezas)*100,
           Control_quimico= (P_S6P77_SP3/manejo_plagas_malezas)*100,
           Control_biologico=(P_S6P77_SP4/manejo_plagas_malezas)*100,
           Control_mecanizado=(P_S6P77_SP5/manejo_plagas_malezas)*100,
           Plantas_repelentes=(P_S6P77_SP6/manejo_plagas_malezas)*100,  ##incluir cabezas total de ganado mpio
           Plantas_modificadas=(P_S6P77_SP7/manejo_plagas_malezas)*100,
           Ningun_control=(P_S6P77_SP11/manejo_plagas_malezas)*100,
           forma_pastoreo=rowSums(select(.,P_S7P80_SP1:P_S7P80_SP4)),
           alimentacion_continua=(P_S7P80_SP1/forma_pastoreo*100),
           Rotacional=P_S7P80_SP2/forma_pastoreo*100,
           Pastoreo_encierro=P_S7P80_SP3/forma_pastoreo*100,
           Confinamiento=P_S7P80_SP4/forma_pastoreo*100,
           Origen_agua=rowSums(select(.,P_S11P124_SP1:P_S11P124_SP11)),
           agua_lluvia=P_S11P124_SP1/Origen_agua*100,
           rio_quebrada=P_S11P124_SP2/Origen_agua*100,
           Lago_laguna=P_S11P124_SP3/Origen_agua*100,
           Cienaga=P_S11P124_SP4/Origen_agua*100,
           Embalse=P_S11P124_SP5/Origen_agua*100,
           pozos_reservorio_aljibe=P_S11P124_SP6/Origen_agua*100,
           fuente_natural=P_S11P124_SP7/Origen_agua*100,
           acueducto=P_S11P124_SP8/Origen_agua*100,
           carro_tanque=P_S11P124_SP9/Origen_agua*100,
           distrito_riego=P_S11P124_SP10/Origen_agua*100,
           sin_acceso=P_S11P124_SP11/Origen_agua*100,
           Proteccion_fuentes=rowSums(select(.,P_S11P125_SP1:P_S11P125_SP12)),
           conservacion_vegetal=(P_S11P125_SP1/Proteccion_fuentes*100),
           Planta_arboles=P_S11P125_SP2/Proteccion_fuentes*100,
           bebedores_artifi=P_S11P125_SP3/Proteccion_fuentes*100,
           manejo_rondas=P_S11P125_SP4/Proteccion_fuentes*100,
           reutiliza_agua=P_S11P125_SP5/Proteccion_fuentes*100,
           trat_aguaResi=P_S11P125_SP6/Proteccion_fuentes*100,
           rezos=P_S11P125_SP7/Proteccion_fuentes*100,
           ritos=P_S11P125_SP8/Proteccion_fuentes*100,
           Pagamentos=P_S11P125_SP9/Proteccion_fuentes*100,
           sitio_sagrado=P_S11P125_SP10/Proteccion_fuentes*100,
           No_proteccion=P_S11P125_SP11/Proteccion_fuentes*100,
           Nofuente_agua=P_S11P125_SP12/Proteccion_fuentes*100,
           Problemas_manejo=rowSums(select(.,P_S11P126_SP1:P_S11P126_SP9)),
           contaminacion=P_S11P126_SP1/Problemas_manejo*100,
           lodos=P_S11P126_SP2/Problemas_manejo*100,
           daño_perdidaInfra=P_S11P126_SP3/Problemas_manejo*100,
           Sequía=P_S11P126_SP4/Problemas_manejo*100,
           Corte_servicio=P_S11P126_SP5/Problemas_manejo*100,
           restriccionInstiPar=P_S11P126_SP6/Problemas_manejo*100,
           Fenomenos_nat=P_S11P126_SP7/Problemas_manejo*100,
           NohayInfra=P_S11P126_SP8/Problemas_manejo*100,
           SindificultadesUsoAgua=P_S11P126_SP9/Problemas_manejo*100,
           Conservacion_suelos=rowSums(select(.,P_S11P127_SP1:P_S11P127_SP11)),
           Labranza_minima=P_S11P127_SP1/Conservacion_suelos*100,
           Siembra_directa=P_S11P127_SP2/Conservacion_suelos*100,
           siembra_cober_veg= (P_S11P127_SP3/Conservacion_suelos*100),
           prac_conserva=P_S11P127_SP4/Conservacion_suelos*100,
           sustratos_suelo=P_S11P127_SP5/Conservacion_suelos*100,
           rezos=P_S11P127_SP6/Conservacion_suelos*100,
           ritos=P_S11P127_SP7/Conservacion_suelos*100,
           pagamento=P_S11P127_SP8/Conservacion_suelos*100,
           rotacion_culti=P_S11P127_SP9/Conservacion_suelos*100,
           enrastrojamiento=P_S11P127_SP10/Conservacion_suelos*100,
           Ninguna_actividad=P_S11P127_SP11/Conservacion_suelos*100,
           manejo_residuos=P_S11P131_SP13,
           Asistencia_tecnica=rowSums(select(.,P_S11P135A_SP1:P_S11P135A_SP10)),
           Asistencia_tecnica_recibida=P_S11P135,
           BPA=P_S11P135A_SP1/Asistencia_tecnica*100,
           BPP=P_S11P135A_SP2/Asistencia_tecnica*100,
           PMA=P_S11P135A_SP3/Asistencia_tecnica*100,
           Manejo_suelo=P_S11P135A_SP4/Asistencia_tecnica*100,
           Manejo_posco=P_S11P135A_SP5/Asistencia_tecnica*100,
           Comercializacion=P_S11P135A_SP6/Asistencia_tecnica*100,
           Asociatividad=P_S11P135A_SP7/Asistencia_tecnica*100,
           creditoyfinan=P_S11P135A_SP8/Asistencia_tecnica*100,
           gestionemp=P_S11P135A_SP9/Asistencia_tecnica*100,
           conocimientoTradyAnces=P_S11P135A_SP10/Asistencia_tecnica*100,
           solcitud_credito=P_S11P136,  #credito/NoUpas
           Aprobacion_Credito=P_S11P136A, #aprobacion/No_upas
           Uso_agropecuario=P_S12P142,
           barbecho_descanso_rastrojo=P_S12P143+P_S12P144+P_S12P145,
           Bosques_nat=P_S12P146,
           Infraestructura_agro=P_S12P147,
           Infraestructura_NoAgro=P_S12P148,
           OtrosUsos=P_S12P149, na.rm=T)%>%
           
  dplyr::select(P_MUNIC,UPAS:OtrosUsos)%>%
  as_tibble()

#Falta arreglar asistencia tecnica.
write.csv(BD_maiz_test, "D:/GIZ/Info_base_San_onofre/BD_maiz_test.csv")

