# 1. this R file contains PTF models used for manuscirpt of 
#Yonggen Zhang, Marcel G. Schaap, and Zhongwang Wei (2020). Hierarchical Multimodel Ensemble Estimates of Soil Water Retention with Global Coverage. Geophysical Research Letter. 


#2. The input of USDA sand (0.05 to 20mm), silt (0.002 to 0.05 mm), clay (below 0.002 mm) are textural percentages, (e.g., 20, 30, 50 for sand, silt, and clay)
#bulk denstiy unit: g/cm3
#the input of organic carbon (OC) is percentage organic carbon (e.g., 2 for 2%)


#3. this file contains an example to estimate theta_s, water content at -0.33 bar (or 330cm, or what is called field capacity), water content at -15 bar (or 15000cm, or what is called wilting point)


# Soil water retention models
####################################
# van Genuchten (1980) model
vG=function(parameter,pressure_head) {
  theta_r=parameter$theta_r
  theta_s=parameter$theta_s
  alpha=parameter$alpha
  n=parameter$n
  theta=theta_r+(theta_s-theta_r)/(1+(alpha*pressure_head)^n)^(1-1/n)
  return(theta)
}

# modified van Genuchten model, with m=1; this is used for Vereecken (1989) PTF 
vG_modified_m=function(vG_sub,suction_p) {
  theta_r=vG_sub$theta_r
  theta_s=vG_sub$theta_s
  alpha=vG_sub$alpha
  n=vG_sub$n
  theta=theta_r+(theta_s-theta_r)/(1+(alpha*suction_p)^n)^1
  return(theta)
}


#Campbell (1974） model
Campbell=function(parameter,pressure_head){
  
  b=parameter$b
  psi_s=parameter$psi_s
  theta_s=parameter$th_s
  theta=theta_s*(pressure_head/psi_s)^(-1/b)
  return(theta)
  
}

#Clapp and Hornberger (1978) model
Clapp_Hornberger=function(parameter,pressure_head){
  
  b=parameter$b
  psi_s=parameter$psi_s
  theta_s=parameter$th_s
  m=parameter$m
  n=parameter$n
  psi_i=parameter$psi_i
  
  len_b=length(b)
  theta=vector(mode="numeric", length=len_b)
  
  for (i in seq(len_b)) {
    if (pressure_head[i]>psi_i[i]){
      theta[i]=theta_s[i]*(pressure_head[i]/psi_s[i])^(-1/b[i])
    } else {
      temp1=n[i]+1
      temp2=sqrt((n[i]+1)^2-4*(n[i]+pressure_head[i]/m[i]))
      
      wet=(temp1+temp2)/2
      theta[i]=wet*theta_s[i]
      
    }
  }
  return(theta)
}

#Cosby （1984）model
#by setting theta_r as 0 and 
#when suction_p<psi_s, theta equals to theta_s (this is the same as BC model, which connects two curves)
Cosby=function(clapp,suction_p){
  
  b=clapp$b
  psi_s=clapp$psi_s
  theta_s=clapp$th_s
  
  len_b=length(b)
  theta=vector(mode="numeric", length=len_b)
  
  for (i in seq(len_b)) {
    if (suction_p[i]>psi_s[i]){
      theta[i]=theta_s[i]*(suction_p[i]/psi_s[i])^(-1/b[i])
    } else {
      theta[i]=theta_s[i]
    }
  }
  
  return(theta)
  
}


# read dataset and assign the textural classes for soil samples
####################################
library(soiltexture)
Your_dataset=data.frame(SAND=c(51.5,68.1,19.7),SILT=c(38.1,15.2,12.2),CLAY=c(10.4,16.7,68.1),BD=c(1.25,1.65,1.34),OC=c(5.4, 2.16, 4.9))
#note 1. please check whether SAND+SILT+CLAY equals to 100; if not, check the validity of the dataset or normalize the dataset;  i.e., Your_dataset$SAND+Your_dataset$SILT+Your_dataset$CLAY
#note 2. if your dataset does not have bulk density(BD) or organic carbon content (OC) or even the dataset is only soil textural classes, please consider using Group A, B, C, D models in the paper.

soiltype=TT.points.in.classes(tri.data=Your_dataset, class.sys= "USDA.TT", PiC.type= "t")
# this is used to remove classes at the boundary of two classes, since some samples may belong to two classes;
# you can also average the parameters if the samples belong to two classes
Your_dataset$soiltype=gsub("\\,.*","",soiltype)  




# PTF models
####################################
#----------------------------------
#1) Cosby 1984 parameter,  from Table 3: lookup table. 
# Please also check Table 2  from fei chen et al (2001), which organized the original table much better,  psi_s unit is meter
#----------------------------------
cosby_b=c(2.79,4.26,4.74,      5.25,6.66,10.73,8.17,5.33,5.33,8.72,10.39,11.55)
cosby_psi_s=c(0.069,0.036,0.141,0.355,0.135,0.098,0.263,0.759,0.759,0.617,0.324,0.468)
cosby_psi_s=cosby_psi_s*100  # m to cm
cosby_th_s=c(0.339,0.421,0.434,0.439,0.404,0.406,0.465,0.476,0.476,0.464,0.468,0.468)

Cosby_b_est=Your_dataset$soiltype
Cosby_psi_s_est=Your_dataset$soiltype
Cosby_th_s_est=Your_dataset$soiltype

class_types=c('Sa','LoSa','SaLo','Lo','SaClLo','SaCl','ClLo','SiLo','Si','SiClLo','SiCl','Cl')
len_classes=length(class_types)

for (i in seq(len_classes)) {
  Cosby_b_est[(Cosby_b_est==class_types[i])]=cosby_b[i]
  Cosby_psi_s_est[(Cosby_psi_s_est==class_types[i])]=cosby_psi_s[i]
  Cosby_th_s_est[(Cosby_th_s_est==class_types[i])]=cosby_th_s[i]
}

cosby0=data.frame(b=as.numeric(Cosby_b_est),psi_s=as.numeric(Cosby_psi_s_est),th_s=as.numeric(Cosby_th_s_est))
cosby0_330=Cosby(cosby0,330); cosby0_15000=Cosby(cosby0,15000); cosby0_porosity=Cosby(cosby0,0);

#-------------------------------------
# 2) from from Table 2 of Clapp & Hornberger (1978) model
#--------------------------------------
CH_b=c(4.05,4.38,4.9,5.39,7.12,10.4,8.52,4.9,5.3,7.75,10.4,11.4)
CH_ths=c(0.395,0.41,0.435,0.451,0.42,0.426,0.476,0.485,0.485,0.477,0.492,0.482)
#cm
CH_psi_s=c(12.1,9,21.8,47.8,29.9,15.3,63,78.6,78.6,35.6,49,40.5)

CLAPP_b_est=Your_dataset$soiltype
CLAPP_psi_s_est=Your_dataset$soiltype
CLAPP_ths_est=Your_dataset$soiltype

for (i in seq(len_classes)) {
  CLAPP_b_est[(CLAPP_b_est==class_types[i])]=CH_b[i]
  CLAPP_ths_est[(CLAPP_ths_est==class_types[i])]=CH_ths[i]
  CLAPP_psi_s_est[(CLAPP_psi_s_est==class_types[i])]=CH_psi_s[i]
}

#0.92 was used for Wi, because clapp & Hornberger (1978) stated that 'Because of the restriction on Wi imposed by the exponent b,
#,Wi was maintaied at 0.92'  

CLAPP=data.frame(b=as.numeric(CLAPP_b_est),ths=as.numeric(CLAPP_ths_est),psi_s=as.numeric(CLAPP_psi_s_est))
CH_psi_i=CLAPP$psi_s*(0.92)^(-CLAPP$b)                      # equation (1) in clapp & Hornberger (1978)

CH_m=CH_psi_i/(1-0.92)^2-CH_psi_i*CLAPP$b/(0.92*(1-0.92))   # equation after (3) in clapp & Hornberger (1978)
CH_n=2*0.92-(CH_psi_i*CLAPP$b/CH_m*0.92)-1                  # equation after (3) in clapp & Hornberger (1978)

clapp_horn=data.frame(b=as.numeric(CLAPP$b),psi_s=as.numeric(CLAPP$psi_s),th_s=as.numeric(CLAPP$ths),
                      m=as.numeric(CH_m),n=as.numeric(CH_n),psi_i=as.numeric(CH_psi_i))

Clapp_330=Clapp_Hornberger(clapp_horn,330);Clapp_15000=Clapp_Hornberger(clapp_horn,15000);Clapp_porosity=Clapp_Hornberger(clapp_horn,0);

#-------------------------------------------------
# 3) calculate cosby1 models, using one variable
# from Table 5 of Cosby et al . (1984), using one independent variable to estimate soil parameters
#-------------------------------------------------
cosby1_PTF=function(sand,clay) {
  
  ths=0.489-0.00126*sand
  psi_s=-10^(1.88-0.013*sand)  # I assume psi_s unit is cm
  lamda= 1/(2.91+0.159*clay)
  
  parameter=data.frame(ths,lamda,psi_s)
  return(parameter)
}
cosby1_clapp=cosby1_PTF(sand=Your_dataset$SAND,clay=Your_dataset$CLAY)
cosby1_clapp2=data.frame(th_s=cosby1_clapp$ths,b=1/cosby1_clapp$lamda,psi_s=abs(cosby1_clapp$psi_s))
cosby1_330=Cosby(cosby1_clapp2,330); cosby1_15000=Cosby(cosby1_clapp2,15000);cosby1_porosity=Cosby(cosby1_clapp2,0);


#----------------------------------
# 4) calculate cosby2 models, using two variable
# from Table 5 of Cosby et al . (1984), using two independent variable to estimate soil parameters
#----------------------------------
cosby2_PTF=function(sand,silt,clay) {
  
  ths=0.505-0.00142*sand-0.00037*clay
  psi_s=-10^(1.54-0.0095*sand+0.0063*silt)  # I assume psi_is unit is cm
  lamda= 1/(3.1+0.157*clay-0.003*sand)
  
  parameter=data.frame(ths,lamda,psi_s)
  return(parameter)
  
}

cosby2_clapp=cosby2_PTF(sand=Your_dataset$SAND,silt=Your_dataset$SILT,clay=Your_dataset$CLAY)
cosby2_clapp2=data.frame(th_s=cosby2_clapp$ths,b=1/cosby2_clapp$lamda,psi_s=abs(cosby2_clapp$psi_s))
cosby2_330=Cosby(cosby2_clapp2,330); cosby2_15000=Cosby(cosby2_clapp2,15000);cosby2_porosity=Cosby(cosby2_clapp2,0);

#----------------------------------
# 5) Carsel and Parrish (1988)
# lookup table: from Tables 2, 3, 4, 5
#----------------------------------
CP_thr=c(   0.045,0.057,0.065,  0.078, 0.095,     0.1,   0.095, 0.067, 0.034, 0.089, 0.07,0.068)
CP_ths=c(    0.43,0.41,  0.41,  0.43,  0.41,    0.38,  0.41,  0.45,  0.46,  0.43,  0.36,0.38)
CP_alpha=c(0.145, 0.124, 0.075, 0.036, 0.019,   0.027, 0.019, 0.02,  0.016, 0.01,  0.005,0.008)
CP_npar=c(2.68,   2.28,  1.89,  1.56,  1.31,    1.23,  1.31,  1.41,  1.37, 1.23,   1.09, 1.09)

CP_thr_est=Your_dataset$soiltype
CP_ths_est=Your_dataset$soiltype
CP_alpha_est=Your_dataset$soiltype
CP_npar_est=Your_dataset$soiltype

for (i in seq(len_classes)) {
  CP_thr_est[(CP_thr_est==class_types[i])]=CP_thr[i]
  CP_ths_est[(CP_ths_est==class_types[i])]=CP_ths[i]
  CP_alpha_est[(CP_alpha_est==class_types[i])]=CP_alpha[i]
  CP_npar_est[(CP_npar_est==class_types[i])]=CP_npar[i]
}

CP=data.frame(theta_r=as.numeric(CP_thr_est),theta_s=as.numeric(CP_ths_est),alpha=as.numeric(CP_alpha_est),n=as.numeric(CP_npar_est))
CP_330=vG(CP,330); CP_15000=vG(CP,15000);CP_porosity=vG(CP,0);

#----------------------------------
#6) Rawls and Brakenssiek (1985)
#----------------------------------
#Rawls and Brakenssiek (1985)
#----------------------------------
Rawls_PTF=function(clay,sand,BD) {
  
  ths=1-BD/2.65
  hb=exp(5.3396738+0.1845038*clay-2.48394546*ths-0.00213853*(clay^2)-0.04356349*sand*ths-0.61745089*clay*ths+0.00143598*(sand^2)*(ths^2)-0.00855375*(clay^2)*(ths^2)-0.00001282*(sand^2)*clay+0.00895359*(clay^2)*ths-0.00072472*(sand^2)*ths+0.0000054*(clay^2)*sand+0.5002806*(clay)*(ths^2))
  alpha=1/hb
  npar=1+exp(-0.7842831+0.0177544*sand-1.062498*ths-0.00005304*(sand^2)-0.00273493*(clay^2)+1.11134946*(ths^2)-0.03088295*sand*ths+0.00026587*(sand^2)*(ths^2)-0.00610522*(clay^2)*(ths^2)-0.00000235*(sand^2)*(clay)+0.00798746*(clay^2)*ths-0.0067449*clay*(ths^2))
  thr=-0.0182482+0.00087269*sand+0.00513488*clay+0.02939286*ths-0.00015395*(clay^2)-0.0010827*sand*ths-0.00018233*(clay^2)*(ths^2)+0.00030703*(clay^2)*ths-0.0023584*clay*(ths^2)
  
  #VG=data.frame(thr,ths,alpha,npar)
  VG=data.frame(ths,alpha,npar,thr)
  return(VG)
  
}

Rawls_VG=Rawls_PTF(clay=Your_dataset$CLAY,sand=Your_dataset$SAND,BD=Your_dataset$BD)
Rawls_VG2=data.frame(theta_r=Rawls_VG$thr,theta_s=Rawls_VG$ths,alpha=Rawls_VG$alpha,n=Rawls_VG$npar)
Rawls_330=vG(Rawls_VG2,330); Rawls_15000=vG(Rawls_VG2,15000);Rawls_porosity=vG(Rawls_VG2,0);

#----------------------------------------------------------
#7) Campbell_Shiozawa 1992 ,  Please also see appendix of Dai et al.,(2013) paper, which organized the data in a better way
#---------------------------------------------------------
Campbell_PTF=function(sand,silt,clay,BD) {
  
  ths=1-BD/2.65
  dg_log=0.01*(log(1.025)*sand+log(0.026)*silt+log(0.001)*clay)
  dg=exp(dg_log)
  
  temp1=0.01*(log(1.025)*log(1.025)*sand+log(0.026)*log(0.026)*silt+log(0.001)*log(0.001)*clay)
  temp2=(log(dg))^2
  sigma_g_log=sqrt(temp1-temp2)
  sigma_g=exp(sigma_g_log)
  lamda= 1/(dg^(-0.5)+0.2*sigma_g)
  psi_s=-5*dg^(-0.5)*((BD/1.3)^(0.67/lamda))   # I assume psi_is unit is cm
  
  parameter=data.frame(ths,lamda,psi_s)
  return(parameter)
}

Campbell_clapp=Campbell_PTF(sand=Your_dataset$SAND,silt=Your_dataset$SILT,clay=Your_dataset$CLAY,BD=Your_dataset$BD)
Campbell_clapp2=data.frame(th_s=Campbell_clapp$ths,b=1/Campbell_clapp$lamda,psi_s=abs(Campbell_clapp$psi_s))  # psi_s keeps positive 
Campbell_330=Cosby(Campbell_clapp2,330); Campbell_15000=Cosby(Campbell_clapp2,15000);Campbell_porosity=Cosby(Campbell_clapp2,0);

#----------------------------------
#8) Vereecken 1989, Table 7
#----------------------------------
Vereecken_PTF=function(clay,sand,BD,OC) {
  
  ths=0.81-0.283*BD+0.001*clay
  thr=0.015+0.005*clay+0.014*OC
  alpha_log=-2.486+0.025*sand-0.351*OC-2.617*BD-0.023*clay
  npar_log=0.053-0.009*sand-0.013*clay+0.00015*(sand^2)
  
  alpha=exp(alpha_log)
  npar=exp(npar_log)
  
  VG=data.frame(thr,ths,alpha,npar)
  return(VG)
}

Vereecken_VG=Vereecken_PTF(clay=Your_dataset$CLAY,sand=Your_dataset$SAND,BD=Your_dataset$BD,OC=Your_dataset$OC)
Vereecken_VG2=data.frame(theta_r=Vereecken_VG$thr,theta_s=Vereecken_VG$ths,alpha=Vereecken_VG$alpha,n=Vereecken_VG$npar)
Vereecken_330=vG_modified_m(Vereecken_VG2,330); Vereecken_15000=vG_modified_m(Vereecken_VG2,15000);Vereecken_porosity=vG_modified_m(Vereecken_VG2,0);

#----------------------------------
#9) Weynants 2009 
#----------------------------------
Weynants_PTF=function(clay,sand,BD,OC) {
  
  thr=0
  ths=0.6355+0.0013*clay-0.1631*BD
  alpha=exp(-4.3003-0.0097*clay+0.0138*sand-0.00992*OC)
  npar=1+exp(-1.0846-0.0236*clay-0.0085*sand+0.0001*sand^2)
  
  VG=data.frame(thr,ths,alpha,npar)
  return(VG)
}

Weynants_VG=Weynants_PTF(clay=Your_dataset$CLAY,sand=Your_dataset$SAND,BD=Your_dataset$BD,OC=Your_dataset$OC)
Weynants_VG2=data.frame(theta_r=Weynants_VG$thr,theta_s=Weynants_VG$ths,alpha=Weynants_VG$alpha,n=Weynants_VG$npar)
Weynants_330=vG(Weynants_VG2,330); Weynants_15000=vG(Weynants_VG2,15000); Weynants_porosity=vG(Weynants_VG2,0); 


#----------------------------------
#10) calculate Wosten models 1999
#----------------------------------
Wos_PTF=function(clay,BD,silt,OM,topsoil) {
  
  clay[clay<0.1]=0.1
  silt[silt<0.1]=0.1
  OM[OM>50]=50   #very large npar and small alpha
  OM[OM==0]=0.1  #because of log(OM)
  
  ths=0.7919+0.001691*clay-0.29619*BD-0.000001491*(silt^2)+0.0000821*(OM^2)+0.02427*clay^(-1)+0.01113*silt^(-1)+0.01472*log(silt)-0.0000733*OM*clay-0.000619*BD*clay-0.001183*BD*OM-0.0001664*topsoil*silt
  
  alpha_log=-14.96+0.03135*clay+0.0351*silt+0.646*OM+15.29*BD-0.192*topsoil-4.671*BD^2-0.000781*clay^2-0.00687*OM^2+0.0449*OM^(-1)+0.0663*log(silt)+0.1482*log(OM)-0.04546*BD*silt-0.4852*BD*OM+0.00673*topsoil*clay
  alpha=exp(alpha_log)
  
  npar_log=-25.23-0.02195*clay+0.0074*silt-0.1940*OM+45.5*BD-7.24*BD^2+0.0003658*clay^2+0.002885*OM^2-12.81*BD^(-1)-0.1524*silt^(-1)-0.01958*OM^(-1)-0.2876*log(silt)-0.0709*log(OM)-44.6*log(BD)-0.02264*BD*clay+0.0896*BD*OM+0.00718*topsoil*clay
  npar=exp(npar_log)+1
  
  VG=data.frame(ths,alpha,npar)
  return(VG)
  
}

OM=Your_dataset$OC*1.72
topsoil=1   # topsoil is an ordinal variable having the value of 1 (depth 0-30cm) or 0 (depth>30cm)

# see NCS_function for Wos_PTF
Wos_VG=Wos_PTF(clay=Your_dataset$CLAY,BD=Your_dataset$BD,silt=Your_dataset$SILT,OM=OM,topsoil=topsoil)
Wos_VG2=data.frame(theta_r=0.01,theta_s=Wos_VG$ths,alpha=Wos_VG$alpha,n=Wos_VG$npar)
Wos_330=vG(Wos_VG2,330); Wos_15000=vG(Wos_VG2,15000); Wos_porosity=vG(Wos_VG2,0);


#----------------------------------
# 11) Rosetta3 H1
#----------------------------------
H1_table=read.table("./H1_new.csv",sep=",",row.names = 1)

H1_thr_est=Your_dataset$soiltype
H1_ths_est=Your_dataset$soiltype
H1_alpha_est=Your_dataset$soiltype
H1_npar_est=Your_dataset$soiltype

for (i in seq(len_classes)) {
  soil_type=class_types[i]
  H1_thr_est[(H1_thr_est==soil_type)]=c(t(H1_table[c(soil_type),][1]))
  H1_ths_est[(H1_ths_est==soil_type)]=c(t(H1_table[c(soil_type),][2]))
  H1_alpha_est[(H1_alpha_est==soil_type)]=c(t(H1_table[c(soil_type),][3]))
  H1_npar_est[(H1_npar_est==soil_type)]=c(t(H1_table[c(soil_type),][4]))
}
H1=data.frame(theta_r=as.numeric(H1_thr_est),theta_s=as.numeric(H1_ths_est),alpha=as.numeric(H1_alpha_est),n=as.numeric(H1_npar_est))
H1_330=vG(H1,330); H1_15000=vG(H1,15000); H1_porosity=vG(H1,0);

#----------------------------------
# 12), 13) read data Rosetta3 H2, H3
#----------------------------------
###########################################################################################
#!!! Note: please change the following estimation based on your dataset from Rosetta3-H2w models (using sand, silt ,clay percentages as input)
###########################################################################################
H2_VGM=read.table("Your_estimation_from_Rosetta3_H2.txt",sep="")
#if the estimation has five parameters, i.e., theta_r,theta_s,alpha,n,Ks, then select only the four parameters, i.e., theta_r,theta_s,alpha,n
H2=H2_VGM[,c(1,2,3,4)]
colnames(H2)=c("theta_r","theta_s","alpha","n")

###########################################################################################
#!!! Note: please change the following estimation based on your dataset from Rosetta3-H3w models (using sand, silt ,clay percentages, and bulk density as input)
###########################################################################################
H3_VGK=read.table("Your_estimation_from_Rosetta3_H3.txt",sep="")
#if the estimation has five parameters, i.e., theta_r,theta_s,alpha,n,Ks, then select only the four parameters, i.e., theta_r,theta_s,alpha,n
H3=H3_VGK[,c(1,2,3,4)]
colnames(H3)=c("theta_r","theta_s","alpha","n")

H2_330=vG(H2,330); H2_15000=vG(H2,15000); H2_porosity=vG(H2,0);
H3_330=vG(H3,330); H3_15000=vG(H3,15000); H3_porosity=vG(H3,0);

#----------------------------------
# calculate mean and sd 
# all PTFs: cosby0,cosby1,cosby2,Wos,Rawls,Vereecken,Weynants,Campbell,clapp_horn,CP,H1,H2,H3 
#----------------------------------
#read the weighting 
library(readxl)
coef_all_PTF=read_excel("TableS3_All_data_RMSE_cal_val&weight.xlsx")

#You can also save the excel file of weighting files into csv, and read it easily using the following command
#coef_all_PTF=read.table("TableS3_All_data_RMSE_cal_val&weight.csv",sep=',')


n_bootstrap=nrow(coef_all_PTF)
n_sample=nrow(Your_dataset)

bind_all_theta_s=matrix(,ncol=n_bootstrap,nrow=n_sample) # 
bind_all_330=matrix(,ncol=n_bootstrap,nrow=n_sample) # 
bind_all_15000=matrix(,ncol=n_bootstrap,nrow=n_sample) # 

for (i in seq(n_bootstrap)) {
  
  bind_all_theta_s[,i]=cosby0_theta_s*coef_all_PTF[i,3]+CP_theta_s*coef_all_PTF[i,4]+Clapp_theta_s*coef_all_PTF[i,5]+H1_theta_s*coef_all_PTF[i,6]+
    cosby1_theta_s*coef_all_PTF[i,7]+cosby2_theta_s*coef_all_PTF[i,8]+H2_theta_s*coef_all_PTF[i,9]+Rawls_theta_s*coef_all_PTF[i,10]+
    Campbell_theta_s*coef_all_PTF[i,11]+H3_theta_s*coef_all_PTF[i,12]+Wos_theta_s*coef_all_PTF[i,13]+Weynants_theta_s*coef_all_PTF[i,14]+Vereecken_theta_s*coef_all_PTF[i,15]
  

  bind_all_330[,i]=cosby0_330*coef_all_PTF[i,3]+CP_330*coef_all_PTF[i,4]+Clapp_330*coef_all_PTF[i,5]+H1_330*coef_all_PTF[i,6]+
    cosby1_330*coef_all_PTF[i,7]+cosby2_330*coef_all_PTF[i,8]+H2_330*coef_all_PTF[i,9]+Rawls_330*coef_all_PTF[i,10]+
    Campbell_330*coef_all_PTF[i,11]+H3_330*coef_all_PTF[i,12]+Wos_330*coef_all_PTF[i,13]+Weynants_330*coef_all_PTF[i,14]+Vereecken_330*coef_all_PTF[i,15]
  
  
  bind_all_15000[,i]=cosby0_15000*coef_all_PTF[i,3]+CP_15000*coef_all_PTF[i,4]+Clapp_15000*coef_all_PTF[i,5]+H1_15000*coef_all_PTF[i,6]+
    cosby1_15000*coef_all_PTF[i,7]+cosby2_15000*coef_all_PTF[i,8]+H2_15000*coef_all_PTF[i,9]+Rawls_15000*coef_all_PTF[i,10]+
    Campbell_15000*coef_all_PTF[i,11]+H3_15000*coef_all_PTF[i,12]+Wos_15000*coef_all_PTF[i,13]+Weynants_15000*coef_all_PTF[i,14]+Vereecken_15000*coef_all_PTF[i,15]
}
average_all_theta_s=rowMeans(bind_all_theta_s)
average_all_330=rowMeans(bind_all_330)
average_all_15000=rowMeans(bind_all_15000)

