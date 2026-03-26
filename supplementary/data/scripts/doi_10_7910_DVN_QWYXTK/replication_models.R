pckg <- c("dplyr", "car", "fBasics", "kableExtra", "lmtest", "texreg", "plm", "sandwich", 
          "AER", "ggplot2", "survey", "gridExtra")
invisible(lapply(pckg, require, character.only = TRUE))

#Set Working Directory
setwd("C:/Users/Matias/Dropbox/Proyectos/Confianza social y politica/Replication_File")

#Load, process, recode variables and create wide and long data.frame from original data
source("recode_variables_ELSOC.R")

#SessionInfo
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=Spanish_Chile.1252  LC_CTYPE=Spanish_Chile.1252    LC_MONETARY=Spanish_Chile.1252
# [4] LC_NUMERIC=C                   LC_TIME=Spanish_Chile.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] psych_2.0.9         polycor_0.7-10      readstata13_0.9.2   AER_1.2-9           survival_3.2-7     
# [6] sandwich_3.0-0      plm_2.4-2           texreg_1.37.5       lmtest_0.9-38       zoo_1.8-8          
# [11] kableExtra_1.3.4    fBasics_3042.89.1   timeSeries_3062.100 timeDate_3043.102   car_3.0-10         
# [16] carData_3.0-4       dplyr_1.0.2        
# 
# loaded via a namespace (and not attached):
# [1] httr_1.4.2        viridisLite_0.3.0 splines_4.0.3     tmvnsim_1.0-2     Formula_1.2-4     Rdpack_2.1       
# [7] cellranger_1.1.0  yaml_2.2.1        pillar_1.4.7      lattice_0.20-41   glue_1.4.2        digest_0.6.27    
# [13] rbibutils_2.0     rvest_0.3.6       colorspace_2.0-0  htmltools_0.5.1.1 Matrix_1.2-18     pkgconfig_2.0.3  
# [19] haven_2.3.1       purrr_0.3.4       scales_1.1.1      webshot_0.5.2     svglite_2.0.0     openxlsx_4.2.3   
# [25] rio_0.5.16        tibble_3.0.4      generics_0.1.0    spatial_7.3-12    ellipsis_0.3.1    maxLik_1.4-6     
# [31] mnormt_2.0.2      magrittr_2.0.1    crayon_1.3.4      readxl_1.3.1      evaluate_0.14     nlme_3.1-149     
# [37] MASS_7.3-53       forcats_0.5.0     xml2_1.3.2        foreign_0.8-80    tools_4.0.3       data.table_1.13.2
# [43] hms_0.5.3         gbRd_0.4-11       lifecycle_0.2.0   stringr_1.4.0     munsell_0.5.0     zip_2.1.1        
# [49] compiler_4.0.3    systemfonts_1.0.1 rlang_0.4.11      grid_4.0.3        rstudioapi_0.13   miscTools_0.6-26 
# [55] rmarkdown_2.5     abind_1.4-5       curl_4.3          R6_2.5.0          knitr_1.30        bdsmatrix_1.3-4  
# [61] stringi_1.5.3     parallel_4.0.3    Rcpp_1.0.5        vctrs_0.3.5       tidyselect_1.1.0  xfun_0.19        

#Load data in Wide Format
load("Elsoc_recode_2016-2019_wide.Rdata")

#Subset data for only complete cases are analysed
el <- subset(el, tipo_atricion==1) ##4 out of 4 waves
el <- subset(el, muestra==1)


########################################################################
############################ FIGURE 1) #################################
########################################################################

##--- Evolution Poltical Trust in 4 Waves ---###

# #Normalize social and political trust variables from 0 to 1 and add to El data.frame
vname <- c(paste0("confGob_w0", 1:4), paste0("confCong_w0", 1:4),
           paste0("confPart_w0", 1:4), paste0("confJudi_w0", 1:4),
           paste0("confsoc1_w0", 1:4), paste0("confsoc2_w0", 1:4),
           paste0("confsoc3_w0", 1:4), paste0("confsoc_w0",1:4),
           paste0("polconf_w0",1:4))

normalize <- function(x) {
  z <- (x - min(x, na.rm=T))/(max(x, na.rm=T) - min(x, na.rm=T))
  return(z)
}

trt_n <- data.frame(array(NA,dim=list(length(el[,1]), length(vname))))
for(i in 1:length(vname)) trt_n[,i] <- normalize(eval(parse(text=paste0("el$", vname[i]))))
head(trt_n, 20)
names(trt_n) <- paste0(vname, "n")
el <- cbind(el, trt_n)

##Define Complex Survey Design
elc <- svydesign(id=~segmento_w04, strata=~estrato_w04,
                 weights=~ponderador02_w04, data=subset(el, !is.na(segmento_w04)),
                 nest=TRUE)

#Variables
vv <- c(paste0("confGob_w0", 1:4, "n"), paste0("confCong_w0", 1:4, "n"),
        paste0("confPart_w0", 1:4, "n"), paste0("confJudi_w0", 1:4, "n"))

#Calculates Percentages
cm <- data.frame(trst = colSums(
  sapply(vv, function(x) prop.table(svytable(eval(parse(text=paste0("~",x))), elc)))[3:4,]))
cm$Institution <- factor(rep(1:4, each=4), labels = c("Government", "Congress",
                                                      "Political \nParties", "Judiciary"))
cm$time <- rep(2016:2019, times=4)

#Calculates Index Mean per year
cm2 <- sapply(paste0("polconf_w0", 1:4, "n"), function(x) svymean(~eval(parse(text=x)), elc, na.rm=T))
cm2 <- data.frame(trst=cm2, time=2016:2019, Institution="Trust \nIndex")
conf_time <- rbind(cm, cm2)
conf_time$statistic <- c(rep("% A lot + Quite + \nSome Trust", times=4*4), 
                         rep("Trust Index Average", times=4))
pol_trend <- 
  ggplot(conf_time, aes(time, trst, group=Institution,
                        shape=Institution, color=statistic)) + 
  geom_line(size=0.75) + geom_point(size=3) + theme_bw() + 
  scale_color_manual(values=c("grey68", "black"), 
                     guide=guide_legend(override.aes = list(shape=NA), nrow=2)) +
  labs(x="Year", y="Level of Trust") + 
  labs(title="Evolution Trust in Political Institutions") +
  theme(legend.position="bottom", legend.title = element_blank()) + 
  ylim(0, 0.43) + 
  guides(shape=guide_legend(nrow = 2))


##--- Evolution Social trust 4 waves ----###

#Variables
ss <- c(paste0("confsoc1_w0", 1:4, "n"), paste0("confsoc2_w0", 1:4, "n"),
        paste0("confsoc3_w0", 1:4, "n"))

#Calculates Percentages
sm <- data.frame(
  trst = sapply(ss, function(x) prop.table(svytable(eval(parse(text=paste0("~",x))), elc)))[3,])
sm$Item <- factor(rep(1:3, each=4), labels = c("Trust in others","People are helpful",
                                               "People try to \nbe fair"))
sm$time <- rep(2016:2019, times=3)

#Calculates Index Mean per year
sm2 <- sapply(paste0("confsoc_w0", 1:4, "n"), function(x) svymean(~eval(parse(text=x)), elc, na.rm=T))
sm2 <- data.frame(trst=sm2, time=2016:2019, Item="Trust Index")
sconf_time <- rbind(sm, sm2)
sconf_time$statistic <- c(rep("% Trust Response", times=3*4), 
                          rep("Trust Index Average", times=4))

soc_trend <- 
  ggplot(sconf_time, aes(time, trst, group=Item,
                         shape=Item, color=statistic)) + 
  geom_line(size=0.75) + geom_point(size=2.5) + theme_bw() + 
  scale_color_manual(values=c("grey68", "black"), 
                     guide=guide_legend(override.aes = list(shape=NA), nrow=2)) +
  labs(x="Year", y="Level of Trust") + 
  labs(title="Evolution Social Trust") +
  theme(legend.position="bottom", legend.title = element_blank()) + 
  ylim(0, 0.43) + 
  guides(shape = guide_legend(nrow = 2))

#### MERGE SOCIAL AND POLITICAL TRUST TREND PLOTS
pol_soc_plot <- grid.arrange(soc_trend, pol_trend, nrow=1)
ggsave("evol_pol_soc_conf_4waves.png", pol_soc_plot, units="cm", 
       width=26, height=14)
#ggsave("evol_pol_soc_conf_4waves.tiff", pol_soc_plot, units="cm", 
#       width=26, height=14)

########################################################################
### DESCRIPTIVE STATISTICS (TABLES FROM SECTION C ONLINE SUPPLEMENT) ###
########################################################################

#Political Trust Index
el$polconf_w01 <- with(el, apply(cbind(
  confGob_w01, confPart_w01, confCong_w01, confJudi_w01),
  1, mean, na.rm=T))
el$polconf_w02 <- with(el, apply(cbind(
  confGob_w02, confPart_w02, confCong_w02, confJudi_w02),
  1, mean, na.rm=T))
el$polconf_w03 <- with(el, apply(cbind(
  confGob_w03, confPart_w03, confCong_w03, confJudi_w03),
  1, mean, na.rm=T))
el$polconf_w04 <- with(el, apply(cbind(
  confGob_w04, confPart_w04, confCong_w04, confJudi_w04),
  1, mean, na.rm=T))

#Social Trust Index
el$confsoc_w01 <- with(el, apply(cbind(
  confsoc1_w01, confsoc2_w01, confsoc3_w01), 1, mean, na.rm=T))
el$confsoc_w02 <- with(el, apply(cbind(
  confsoc1_w02, confsoc2_w02, confsoc3_w02), 1, mean, na.rm=T))
el$confsoc_w03 <- with(el, apply(cbind(
  confsoc1_w03, confsoc2_w03, confsoc3_w03), 1, mean, na.rm=T))
el$confsoc_w04 <- with(el, apply(cbind(
  confsoc1_w04, confsoc2_w04, confsoc3_w04), 1, mean, na.rm=T))


############---  Table Summary Statistics Wave 1  ---##############

ola1 = select(el, polconf_w01, confGob_w01, confPart_w01, confCong_w01, confJudi_w01, 
              confsoc_w01, confsoc1_w01, confsoc2_w01, confsoc3_w01, 
              derecha_w01, centro_w01, izq_w01, empleo_w01, desempleo_w01, 
              LifeEval_w01, practica_w01, hombre_w01, edad_w01, educaR_w01)
ola1n=data.frame(lapply(ola1, function(x) as.numeric(x)))

ola1n$basica_01 = car::recode(ola1n$educaR_w01, "1=1; 2:5=0; NA=NA")
ola1n$mediainc_01 = car::recode(ola1n$educaR_w01, "1=0; 2=1; 3:5=0; NA=NA")
ola1n$mediacom_01 = car::recode(ola1n$educaR_w01, "1:2=0; 3=1; 4:5=0; NA=NA")
ola1n$tecnica_01 = car::recode(ola1n$educaR_w01, "1:3=0; 4=1; 5=0; NA=NA")
ola1n$universit_01 = car::recode(ola1n$educaR_w01, "1:4=0; 5=1; NA=NA")

ola1n <- subset(ola1n, select = -c(educaR_w01))
ola1n =ola1n  %>%
  relocate(basica_01, mediainc_01,mediacom_01, tecnica_01,
           universit_01, .before = empleo_w01)

tabla1 = basicStats(ola1n)[c("Minimum", "Maximum", "Mean", "Stdev", "Median", "nobs", "NAs"),]
tabla1 <- as.data.frame(t(as.matrix(tabla1)))
tabla1[, 3:5] <- round(tabla1[, 3:5], digits = 2)

colnames(tabla1)<- c("Min", "Max", "Mean", "SD", "Median", "Obs", "NA")
row.names(tabla1)<- c("Political Trust Index","Trust in Government", 
                      "Trust in Parties", "Trust in Congress", "Trust in Judiciary",
                      "Social Trust Index", "Most people can be trusted", 
                      "Most people try to help others", "People try to be fair",
                      "Right wing", "Center", "Left wing", "Primary or less", 
                      "Secondary incomplete", "Secondary complete", "Terciary technical", 
                      "Universitary", "Employed", "Unemployed", "Life satisfaction", 
                      "Church attendance", "Male", "Age")

kbl(tabla1, format = 'latex', booktabs = T, position="h",  
    caption = "Descriptive statistics of wave 1 (2016)") %>%
  kable_styling(full_width = F) %>%
  pack_rows("Political Trust", 1, 5) %>%
  pack_rows("Social Trust", 6, 9) %>%
  pack_rows("Ideological preference", 10, 12) %>%
  pack_rows("Education", 13, 17) %>%
  pack_rows("Occupational status", 18, 19) %>%
  pack_rows("Other", 20, 23)


############---  Table Summary Statistics Wave 2  ---##############

ola2 = select(el, polconf_w02, confGob_w02, confPart_w02, confCong_w02, confJudi_w02, 
              confsoc_w02, confsoc1_w02, confsoc2_w02, confsoc3_w02, 
              derecha_w02, centro_w02, izq_w02, empleo_w02, desempleo_w02, 
              LifeEval_w02, practica_w02, hombre_w02, edad_w02, educaR_w02)
ola2n=data.frame(lapply(ola2, function(x) as.numeric(x)))

ola2n$basica_02 = car::recode(ola2n$educaR_w02, "1=1; 2:5=0; NA=NA")
ola2n$mediainc_02 = car::recode(ola2n$educaR_w02, "1=0; 2=1; 3:5=0; NA=NA")
ola2n$mediacom_02 = car::recode(ola2n$educaR_w02, "1:2=0; 3=1; 4:5=0; NA=NA")
ola2n$tecnica_02 = car::recode(ola2n$educaR_w02, "1:3=0; 4=1; 5=0; NA=NA")
ola2n$universit_02 = car::recode(ola2n$educaR_w02, "1:4=0; 5=1; NA=NA")

ola2n <- subset(ola2n, select = -c(educaR_w02))
ola2n =ola2n  %>%
  relocate(basica_02, mediainc_02,mediacom_02, tecnica_02,
           universit_02, .before = empleo_w02)

tabla2 = basicStats(ola2n)[c("Minimum", "Maximum", "Mean", "Stdev", "Median", "nobs", "NAs"),]
tabla2 <- as.data.frame(t(as.matrix(tabla2)))
tabla2[, 3:5] <- round(tabla2[, 3:5], digits = 2)

colnames(tabla2)<- c("Min", "Max", "Mean", "SD", "Median", "Obs", "NA")
row.names(tabla2)<- c("Political Trust Index","Trust in Government", 
                      "Trust in Parties", "Trust in Congress", "Trust in Judiciary",
                      "Social Trust Index", "Most people can be trusted", 
                      "Most people try to help others", "People try to be fair",
                      "Right wing", "Center", "Left wing", "Primary or less", 
                      "Secondary incomplete", "Secondary complete", "Terciary technical", 
                      "Universitary", "Employed", "Unemployed", "Life satisfaction", 
                      "Church attendance", "Male", "Age")

kbl(tabla2, format = 'latex', booktabs = T,  
    caption = "Descriptive statistics of wave 2 (2017)") %>%
  kable_styling(full_width = F) %>%
  pack_rows("Political Trust", 1, 5) %>%
  pack_rows("Social Trust", 6, 9) %>%
  pack_rows("Ideological preference", 10, 12) %>%
  pack_rows("Education", 13, 17) %>%
  pack_rows("Occupational status", 18, 19) %>%
  pack_rows("Other", 20, 23)


############---  Table Summary Statistics Wave 3  ---##############

ola3 = select(el, polconf_w03, confGob_w03, confPart_w03, confCong_w03, confJudi_w03, 
              confsoc_w03, confsoc1_w03, confsoc2_w03, confsoc3_w03, 
              derecha_w03, centro_w03, izq_w03, empleo_w03, desempleo_w03, 
              LifeEval_w03, practica_w03, hombre_w03, edad_w03, educaR_w03)
ola3n=data.frame(lapply(ola3, function(x) as.numeric(x)))

ola3n$basica_03 = car::recode(ola3n$educaR_w03, "1=1; 2:5=0; NA=NA")
ola3n$mediainc_03 = car::recode(ola3n$educaR_w03, "1=0; 2=1; 3:5=0; NA=NA")
ola3n$mediacom_03 = car::recode(ola3n$educaR_w03, "1:2=0; 3=1; 4:5=0; NA=NA")
ola3n$tecnica_03 = car::recode(ola3n$educaR_w03, "1:3=0; 4=1; 5=0; NA=NA")
ola3n$universit_03 = car::recode(ola3n$educaR_w03, "1:4=0; 5=1; NA=NA")

ola3n <- subset(ola3n, select = -c(educaR_w03))
ola3n =ola3n  %>%
  relocate(basica_03, mediainc_03,mediacom_03, tecnica_03,
           universit_03, .before = empleo_w03)

tabla3 = basicStats(ola3n)[c("Minimum", "Maximum", "Mean", "Stdev", "Median", "nobs", "NAs"),]
tabla3 <- as.data.frame(t(as.matrix(tabla3)))
tabla3[, 3:5] <- round(tabla3[, 3:5], digits = 2)

colnames(tabla3)<- c("Min", "Max", "Mean", "SD", "Median", "Obs", "NA")
row.names(tabla3)<- c("Political Trust Index","Trust in Government", 
                      "Trust in Parties", "Trust in Congress", "Trust in Judiciary",
                      "Social Trust Index", "Most people can be trusted", 
                      "Most people try to help others", "People try to be fair",
                      "Right wing", "Center", "Left wing", "Primary or less", 
                      "Secondary incomplete", "Secondary complete", "Terciary technical", 
                      "Universitary", "Employed", "Unemployed", "Life satisfaction", 
                      "Church attendance", "Male", "Age")

kbl(tabla3, format = 'latex', booktabs = T, 
    caption = "Descriptive statistics of wave 3 (2018)") %>%
  kable_styling(full_width = F) %>%
  pack_rows("Political Trust", 1, 5) %>%
  pack_rows("Social Trust", 6, 9) %>%
  pack_rows("Ideological preference", 10, 12) %>%
  pack_rows("Education", 13, 17) %>%
  pack_rows("Occupational status", 18, 19) %>%
  pack_rows("Other", 20, 23)



############---  Table Summary Statistics Wave 4  ---##############

ola4 = select(el, polconf_w04, confGob_w04, confPart_w04, confCong_w04, confJudi_w04, 
              confsoc_w04, confsoc1_w04, confsoc2_w04, confsoc3_w04, 
              derecha_w04, centro_w04, izq_w04, empleo_w04, desempleo_w04, 
              LifeEval_w04, practica_w04, hombre_w04, edad_w04, educaR_w04)
ola4n=data.frame(lapply(ola4, function(x) as.numeric(x)))

ola4n$basica_04 = car::recode(ola4n$educaR_w04, "1=1; 2:5=0; NA=NA")
ola4n$mediainc_04 = car::recode(ola4n$educaR_w04, "1=0; 2=1; 3:5=0; NA=NA")
ola4n$mediacom_04 = car::recode(ola4n$educaR_w04, "1:2=0; 3=1; 4:5=0; NA=NA")
ola4n$tecnica_04 = car::recode(ola4n$educaR_w04, "1:3=0; 4=1; 5=0; NA=NA")
ola4n$universit_04 = car::recode(ola4n$educaR_w04, "1:4=0; 5=1; NA=NA")

ola4n <- subset(ola4n, select = -c(educaR_w04))

ola4n =ola4n  %>%
  relocate(basica_04, mediainc_04,mediacom_04, tecnica_04,
           universit_04, .before = empleo_w04)

tabla4 = basicStats(ola4n)[c("Minimum", "Maximum", "Mean", "Stdev", "Median", "nobs", "NAs"),]
tabla4 <- as.data.frame(t(as.matrix(tabla4)))
tabla4[, 3:5] <- round(tabla4[, 3:5], digits = 2)

colnames(tabla4)<- c("Min", "Max", "Mean", "SD", "Median", "Obs", "NA")
row.names(tabla4)<- c("Political Trust Index","Trust in Government", 
                      "Trust in Parties", "Trust in Congress", "Trust in Judiciary",
                      "Social Trust Index", "Most people can be trusted", 
                      "Most people try to help others", "People try to be fair",
                      "Right wing", "Center", "Left wing", "Primary or less", 
                      "Secondary incomplete", "Secondary complete", "Terciary technical", 
                      "Universitary", "Employed", "Unemployed", "Life satisfaction", 
                      "Church attendance", "Male", "Age")

kbl(tabla4, format = 'latex', booktabs = T, 
    caption = "Descriptive statistics of wave 4 (2019)") %>%
  kable_styling(full_width = F) %>%
  pack_rows("Political Trust", 1, 5) %>%
  pack_rows("Social Trust", 6, 9) %>%
  pack_rows("Ideological preference", 10, 12) %>%
  pack_rows("Education", 14, 17) %>%
  pack_rows("Occupational status", 18, 19) %>%
  pack_rows("Other", 20, 23)


##############################################
### CALCULATE MODELS OF TABLE 1 OF ARTICLE ###
### CROSS LAGGEED AND COMTEMPORANEUS MODELS ##  
##############################################

#Load data in wide format
load("Elsoc_recode_2016-2019_wide.Rdata")

#Subset data for only complete cases are analysed
el <- subset(el, tipo_atricion==1) ##4 out of 4 waves
el <- subset(el, muestra==1)

#Datos en formato long
load("Elsoc_2016-2019_long.Rdata")

#Subset data for only complete cases are analysed
ell <- subset(ell, tipo_atricion==1) #4 out of 4 waves
ell <- subset(ell, muestra==1)

#Political Trust (Parties, Congress, udiciary and Government)
el$polconf_w01 <- with(el, apply(cbind(
  confGob_w01, confPart_w01, confCong_w01, confJudi_w01),
  1, mean, na.rm=T))
el$polconf_w02 <- with(el, apply(cbind(
  confGob_w02, confPart_w02, confCong_w02, confJudi_w02),
  1, mean, na.rm=T))
el$polconf_w03 <- with(el, apply(cbind(
  confGob_w03, confPart_w03, confCong_w03, confJudi_w03),
  1, mean, na.rm=T))
el$polconf_w04 <- with(el, apply(cbind(
  confGob_w04, confPart_w04, confCong_w04, confJudi_w04),
  1, mean, na.rm=T))

#Institutional Trust (Parties, Congress, Judiciary and Police)
el$insconf_w01 <- with(el, apply(cbind(
  confPart_w01, confCong_w01, confJudi_w01, confCara_w01),
  1, mean, na.rm=T))
el$insconf_w02 <- with(el, apply(cbind(
  confPart_w02, confCong_w02, confJudi_w02, confCara_w02),
  1, mean, na.rm=T))
el$insconf_w03 <- with(el, apply(cbind(
  confPart_w03, confCong_w03, confJudi_w03, confCara_w03),
  1, mean, na.rm=T))
el$insconf_w04 <- with(el, apply(cbind(
  confPart_w04, confCong_w04, confJudi_w04, confCara_w04),
  1, mean, na.rm=T))

#Social Trust
el$confsoc_w01 <- with(el, apply(cbind(
  confsoc1_w01, confsoc2_w01, confsoc3_w01), 1, mean, na.rm=T))
el$confsoc_w02 <- with(el, apply(cbind(
  confsoc1_w02, confsoc2_w02, confsoc3_w02), 1, mean, na.rm=T))
el$confsoc_w03 <- with(el, apply(cbind(
  confsoc1_w03, confsoc2_w03, confsoc3_w03), 1, mean, na.rm=T))
el$confsoc_w04 <- with(el, apply(cbind(
  confsoc1_w04, confsoc2_w04, confsoc3_w04), 1, mean, na.rm=T))

conf_data <- with(el, data.frame(idencuesta, tipo_atricion, muestra,
                                 insconf_w01, insconf_w02, insconf_w03, insconf_w04,
                                 polconf_w01, polconf_w02, polconf_w03, polconf_w04,
                                 confsoc_w01, confsoc_w02, confsoc_w03, confsoc_w04))
conf_data <- reshape(conf_data, varying=list(c("insconf_w01", "insconf_w02",
                                               "insconf_w03", "insconf_w04"),
                                             c("polconf_w01", "polconf_w02",
                                               "polconf_w03", "polconf_w04"),
                                             c("confsoc_w01", "confsoc_w02",
                                               "confsoc_w03", "confsoc_w04")),
                     v.names = c("insconf","polconf", "confsoc"), direction = "long")

conf_data <- subset(conf_data, select=c("idencuesta", "insconf", "polconf", "confsoc"))
conf_data <- conf_data[order(conf_data$idencuesta),]
conf_data$time <- rep(1:4, length(unique(conf_data$idencuesta)))

#Merge Trust Indexed in Long Format
ell <- ell[order(ell$idencuesta),]
ell <- cbind(ell, conf_data)


###---------- Pooled OLS cross-lagged ------------###
#Recodes
ell <- ell[order(ell$idencuesta, ell$time), ]
ell$hombre_w01 <- ell$hombre[ell$time==1][factor(ell$idencuesta)]
ell$edad_w01 <- ell$edad[ell$time==1][factor(ell$idencuesta)]
ell$educaR_w01 <- ell$educaR[ell$time==1][factor(ell$idencuesta)]
ell$religid_w01 <- ell$religid[ell$time==1][factor(ell$idencuesta)]
ell$religid_w01 <- relevel(ell$religid_w01, "Catolico")
levels(ell$religid_w01)[2] <- levels(ell$religid_w01)[5]
ell$practica_w01 <- ell$practica[ell$time==1][factor(ell$idencuesta)]

#Rescale depndent variables
ell$confsoc01 <- (ell$confsoc - min(ell$confsoc, na.rm = T)) / 
  (max(ell$confsoc, na.rm = T) - min(ell$confsoc, na.rm = T))
ell$polconf01 <- (ell$polconf - min(ell$polconf, na.rm = T)) / 
  (max(ell$polconf, na.rm = T) - min(ell$polconf, na.rm = T))
ell$insconf01 <- (ell$insconf - min(ell$insconf, na.rm = T)) / 
  (max(ell$insconf, na.rm = T) - min(ell$insconf, na.rm = T))

##Models of Table 1 from Article
cl_1 <- plm(confsoc01 ~ lag(confsoc01) + lag(polconf01) + 
              hombre_w01 + edad_w01 + educaR_w01 + practica_w01 + 
              derecha + centro + izq + LifeEval + empleo + desempleo + time, data=ell, 
            index = c("idencuesta", "time"), model = "pooling")
cl_2 <- plm(confsoc01 ~ lag(confsoc01) + polconf01 + 
              hombre_w01 + edad_w01 + educaR_w01 + practica_w01 + 
              derecha + centro + izq + LifeEval + empleo + desempleo + time, data=ell, 
            index = c("idencuesta", "time"), model = "pooling")
cl_3 <- plm(confsoc01 ~ lag(confsoc01) + lag(polconf01) + polconf01 + 
              hombre_w01 + edad_w01 + educaR_w01 + practica_w01 +
              derecha + centro + izq + LifeEval  + empleo + desempleo + time, data=ell, 
            index = c("idencuesta", "time"), model = "pooling")
cl_4 <- plm(polconf01 ~ lag(polconf01) + lag(confsoc01) +
              hombre_w01 + edad_w01 + educaR_w01 + practica_w01 + 
              derecha + centro + izq + LifeEval + empleo + desempleo + time, data=ell, 
            index = c("idencuesta", "time"), model = "pooling")
cl_5 <- plm(polconf01 ~ lag(polconf01) + confsoc01 +
              hombre_w01 + edad_w01 + educaR_w01 + practica_w01 + 
              derecha + centro + izq + LifeEval + empleo + desempleo + time, data=ell, 
            index = c("idencuesta", "time"), model = "pooling")
cl_6 <- plm(polconf01 ~ lag(polconf01) + lag(confsoc01) + confsoc01 + 
              hombre_w01 + edad_w01 + educaR_w01 + practica_w01 + 
              derecha + centro + izq + LifeEval + empleo + desempleo + time, data=ell, 
            index = c("idencuesta", "time"), model = "pooling")

robse_cl_1 <- sqrt(diag(vcovHC(cl_1, type="HC0",cluster="group")))
robse_cl_2 <- sqrt(diag(vcovHC(cl_2, type="HC0",cluster="group")))
robse_cl_3 <- sqrt(diag(vcovHC(cl_3, type="HC0",cluster="group")))
robse_cl_4 <- sqrt(diag(vcovHC(cl_4, type="HC0",cluster="group")))
robse_cl_5 <- sqrt(diag(vcovHC(cl_5, type="HC0",cluster="group")))
robse_cl_6 <- sqrt(diag(vcovHC(cl_6, type="HC0",cluster="group")))

pval_cl_1 <- (1 - pnorm(abs(coef(cl_1))/robse_cl_1))*2
pval_cl_2 <- (1 - pnorm(abs(coef(cl_2))/robse_cl_2))*2
pval_cl_3 <- (1 - pnorm(abs(coef(cl_3))/robse_cl_3))*2
pval_cl_4 <- (1 - pnorm(abs(coef(cl_4))/robse_cl_4))*2
pval_cl_5 <- (1 - pnorm(abs(coef(cl_5))/robse_cl_5))*2
pval_cl_6 <- (1 - pnorm(abs(coef(cl_6))/robse_cl_6))*2

texreg(l = list(cl_1, cl_2, cl_3, cl_4, cl_5, cl_6),
       override.se = list(robse_cl_1, robse_cl_2, robse_cl_3, robse_cl_4, robse_cl_5, robse_cl_6),
       override.pvalues = list(pval_cl_1, pval_cl_2, pval_cl_3, pval_cl_4, pval_cl_5, pval_cl_6),
       single.row = F, digits=3, scalebox=.85,
       custom.coef.map = list("(Intercept)"="Intercept", "lag(confsoc01)"="Social Trust$_{t-1}$",
                              "confsoc01"="Social Trust$_{t}$",  "lag(polconf01)"="Political Trust$_{t-1}$",
                              "polconf01"="Political Trust$_{t}$", "hombre_w01"="Male$_{t=1}$", "edad_w01"="Age$_{t=1}$",
                              "educaR_w01Ed. Media Inc."="Secondary Incomplete$_{t=1}$",
                              "educaR_w01Ed. Media Com."="Secondary Complete$_{t=1}$",
                              "educaR_w01Ed. Tec. Sup."="Tertiary Technical$_{t=1}$",
                              "educaR_w01Ed. Univer."="Univeristary Education$_{t=1}$",
                              "practica_w01"="Chrusch Attendance$_{t}$", "derecha"="Right-wing Ideology$_{t}$",
                              "centro"="Centrist Ideology$_{t}$", "izq"="Left-wing Ideology$_{t}$",
                              "LifeEval"="Life Satisfaction$_{t}$", "empleo"="Employed$_{t}$",
                              "desempleo"="Unemployed$_{t}$", "time3"="Wave 3", "time4"="Wave 4"),
       caption="Cross Lagged and Comtemporaneuos Panel Models for Social and Political Trust",
       custom.header = list("DV: Social Trust" = 1:3, "DV: Political Trust" = 4:6),
       float.pos="H", custom.note = "%stars. \\ Robust standard error in parentheses clustered the level of respondents.", custom.model.names=paste0("Model ", 1:6),
       use.packages=FALSE)


##############################################
### CALCULATE MODELS OF TABLE 2 OF ARTICLE ###
### LINEAR PANEL MODELS                     ##  
##############################################

pl_1a <- plm(diff(confsoc01) ~ -1 + diff(lag(polconf01)) + diff(practica) + diff(derecha) + diff(centro)
             + diff(izq) + diff(desempleo) + diff(empleo) +  diff(LifeEval) + time, 
             data=ell, index = c("idencuesta", "time"), model = "pooling")
ell$tt <- as.numeric(ell$time)
pl_1b <- plm(diff(confsoc01) ~ -1 + diff(polconf01) + diff(practica) + diff(derecha) + diff(centro)
             + diff(izq) + diff(desempleo) + diff(empleo) +  diff(LifeEval) + time, 
             data=subset(ell, tt>=2), 
             index = c("idencuesta", "time"), model = "pooling")
pl_1c <- plm(diff(confsoc01) ~ -1 + diff(lag(polconf01)) + diff(polconf01) +  + diff(practica) + 
               diff(derecha) + diff(centro) + diff(izq) + diff(desempleo) + diff(empleo) + 
               diff(LifeEval) + time, data=ell, index = c("idencuesta", "time"), model = "pooling")
pl_2a <- plm(diff(polconf01) ~ -1 + diff(lag(confsoc01)) + diff(practica) + diff(derecha) + diff(centro)
             + diff(izq) + diff(desempleo) + diff(empleo) +  diff(LifeEval) + time, 
             data=ell, index = c("idencuesta", "time"), model = "pooling")
pl_2b <- plm(diff(polconf01) ~ -1 + diff(confsoc01) + diff(practica) + diff(derecha) + diff(centro)
             + diff(izq) + diff(desempleo) + diff(empleo) +  diff(LifeEval) + time, 
             data=subset(ell, tt>=2), index = c("idencuesta", "time"), model = "pooling")
pl_2c <- plm(diff(polconf01) ~ -1 + diff(lag(confsoc01)) + diff(confsoc01) + diff(practica) + 
               diff(derecha) + diff(centro) + diff(izq) + diff(desempleo) + diff(empleo) +  
               diff(LifeEval) + time, data=ell, index = c("idencuesta", "time"), model = "pooling")

robse_pl_1a <- sqrt(diag(vcovHC(pl_1a, type="HC0",cluster="group")))
robse_pl_1b <- sqrt(diag(vcovHC(pl_1b, type="HC0",cluster="group")))
robse_pl_1c <- sqrt(diag(vcovHC(pl_1c, type="HC0",cluster="group")))
robse_pl_2a <- sqrt(diag(vcovHC(pl_2a, type="HC0",cluster="group")))
robse_pl_2b <- sqrt(diag(vcovHC(pl_2b, type="HC0",cluster="group")))
robse_pl_2c <- sqrt(diag(vcovHC(pl_2c, type="HC0",cluster="group")))
pval_pl_1a <- (1 - pnorm(abs(coef(pl_1a))/robse_pl_1a))*2
pval_pl_1b <- (1 - pnorm(abs(coef(pl_1b))/robse_pl_1b))*2
pval_pl_1c <- (1 - pnorm(abs(coef(pl_1c))/robse_pl_1c))*2
pval_pl_2a <- (1 - pnorm(abs(coef(pl_2a))/robse_pl_2a))*2
pval_pl_2b <- (1 - pnorm(abs(coef(pl_2b))/robse_pl_2b))*2
pval_pl_2c <- (1 - pnorm(abs(coef(pl_2c))/robse_pl_2c))*2

texreg(l = list(pl_1a, pl_1b, pl_1c, pl_2a, pl_2b, pl_2c),
       override.se = list(robse_pl_1a, robse_pl_1b, robse_pl_1c, 
                          robse_pl_2a, robse_pl_2b, robse_pl_2c),
       override.pvalues = list(pval_pl_1a, pval_pl_1b, pval_pl_1c, 
                               pval_pl_2a, pval_pl_2b, pval_pl_2c),
       single.row = F, digits=3,
       custom.coef.map = list("diff(lag(polconf01))"="Political Trust$_{t-1}$", 
                              "diff(polconf01)"="Political Trust$_{t}$", 
                              "diff(lag(confsoc01))"="Social Trust$_{t-1}$", "diff(confsoc01)"="Social Trust$_{t}$", 
                              "diff(practica)"="Church Attendance$_{t}$", "diff(derecha)"="Right-wing Ideology$_{t}$",
                              "diff(centro)"="Centrist Ideology$_{t}$", "diff(izq)"="Left-wing Ideology$_{t}$",
                              "diff(LifeEval)"="Life Satisfaction$_{t}$", "diff(empleo)"="Employed$_{t}$",
                              "diff(desempleo)"="Unemployed$_{t}$", "time3"="Wave 3", "time4"="Wave 4"),
       caption="Linear Panel Models (First Differences) for Social and Political Trust",
       custom.header = list("DV: Social Trust" = 1:3, "DV: Political Trust" = 4:6),
       float.pos="H", custom.note = "%stars. Robust standard error in parentheses clustered the level of respondents.", custom.model.names=paste0("Model ", 7:12), use.packages=FALSE)


##################################################
### CALCULATE MODELS OF TABLE 3 & 4 OF ARTICLE ###
### ARELLANO BOND MODELS                       ###  
##################################################

#FuncTion FOR tables with pgmm
source("pgmm_summary_fix.R")

ell1 <- pdata.frame(ell, index=c("id", "time"))
ell1$wave <- as.numeric(ell1$time)

z1a <- pgmm(confsoc01 ~ lag(confsoc01) + lag(polconf01) +
              practica + derecha + centro + izq + empleo + desempleo + LifeEval |
              lag(confsoc01, 2:3),
            data = ell1, effect = "twoways", model = "twosteps")

z1b <- pgmm(confsoc01 ~ lag(confsoc01) + polconf01 +   
              practica + derecha + centro + izq + empleo + desempleo + LifeEval | 
              lag(confsoc01, 2:3), 
            data = ell1, effect = "twoways",  model = "twosteps")

z1c <- pgmm(confsoc01 ~ lag(confsoc01)+ lag(polconf01) + polconf01 +  
              practica + derecha + centro + izq + empleo + desempleo + LifeEval | 
              lag(confsoc01, 2:3), 
            data = ell1, effect = "twoways",  model = "twosteps")

z1d <- pgmm(confsoc01 ~ lag(confsoc01)+ lag(polconf01)+ 
              practica + derecha + centro + izq + empleo + desempleo + LifeEval | 
              lag(confsoc01, 2:3) + lag(polconf01, 2:3), 
            data = ell1, effect = "twoways",  model = "twosteps")

z1e <- pgmm(confsoc01 ~ lag(confsoc01)+ polconf01 + 
              practica + derecha + centro + izq + empleo + desempleo + LifeEval | 
              lag(confsoc01, 2:3) + lag(polconf01, 1:2), 
            data = ell1, effect = "twoways",  model = "twosteps")

z1f <- pgmm(confsoc01 ~ lag(confsoc01)+ polconf01 + lag(polconf01) + 
              practica + derecha + centro + izq + empleo + desempleo + LifeEval |
              lag(confsoc01, 2:3) + lag(polconf01, 1:2) + lag(polconf01, 2:3),
            data = ell1, effect = "twoways",  model = "twosteps")

z2a <- pgmm(polconf01 ~ lag(polconf01) + lag(confsoc01) + 
              practica + derecha + centro + izq + empleo + desempleo + LifeEval | 
              lag(polconf01, 2),
            data = ell1, effect = "twoways", model = "twosteps")

z2b <- pgmm(polconf01 ~ lag(polconf01) + confsoc01 + 
              practica + derecha + centro + izq + empleo + desempleo + LifeEval  |
              lag(polconf01, 2), 
            data = ell1, effect = "twoways", model = "twosteps")

z2c <- pgmm(polconf01 ~ lag(polconf01) + lag(confsoc01) + confsoc01 + 
              practica + derecha + centro + izq + empleo + desempleo + LifeEval  |
              lag(polconf01, 2), 
            data = ell1, effect = "twoways", model = "twosteps")

z2d <- pgmm(polconf01 ~ lag(polconf01) + lag(confsoc01) +
              practica + derecha + centro + izq + empleo + desempleo + LifeEval |
              lag(polconf01, 2) + lag(confsoc01, 2:3), 
            data = ell1, effect = "twoways", model = "twosteps")

z2e <- pgmm(polconf01 ~ lag(polconf01) + confsoc01 + 
              practica + derecha + centro + izq + empleo + desempleo + LifeEval  |
              lag(polconf01, 2) + lag(confsoc01, 1:2), 
            data = ell1, effect = "twoways", model = "twosteps")

z2f <- pgmm(polconf01 ~ lag(polconf01) + lag(confsoc01) + confsoc01 + 
              practica + derecha + centro + izq + empleo + desempleo + LifeEval |
              lag(polconf01, 2) + lag(confsoc01, 2:3) + lag(confsoc01, 1:2), 
            data = ell1, effect = "twoways", model = "twosteps")

#DV: Social Trust
texreg(l = list(extract.pgmm_R(z1a), extract.pgmm_R(z1b), extract.pgmm_R(z1c), 
                extract.pgmm_R(z1d), extract.pgmm_R(z1e), extract.pgmm_R(z1f)),
       single.row = F, digits=3, scalebox=.9, 
       custom.coef.map = list("lag(confsoc01)"="Social Trust$_{t-1}$", 
                              "lag(polconf01)"="Political Trust$_{t-1}$", "polconf01"="Political Trust$_{t}$", 
                              "practica"="Church Attendance$_{t}$", "derecha"="Right-wing Ideology$_{t}$",
                              "centro"="Centrist Ideology$_{t}$", "izq"="Left-wing Ideology$_{t}$",
                              "LifeEval"="Life Satisfaction$_{t}$", "empleo"="Employed$_{t}$", 
                              "desempleo"="Unemployed$_{t}$",
                              "3"="Wave 2018", "4"="Wave 2019"),
       caption="Dynamic Linear Panel Models (AB estimator) for Social Trust",
       float.pos="H", custom.note = "%stars. Robust standard error in parentheses clustered the level of respondents.", custom.model.names=paste0("Model ", 13:18), use.packages=FALSE)


#DV: Political Trust
texreg(l = list(extract.pgmm_R(z2a), extract.pgmm_R(z2b), extract.pgmm_R(z2c), 
                extract.pgmm_R(z2d), extract.pgmm_R(z2e), extract.pgmm_R(z2f)),
       single.row = F, digits=3, scalebox=.9, 
       custom.coef.map = list("lag(polconf01)"="Political Trust$_{t-1}$", 
                              "lag(confsoc01)"="Social Trust$_{t-1}$", 
                              "confsoc01"="Social Trust$_{t}$", 
                              "practica"="Church Attendance$_{t}$", "derecha"="Right-wing Ideology$_{t}$",
                              "centro"="Centrist Ideology$_{t}$", "izq"="Left-wing Ideology$_{t}$",
                              "LifeEval"="Life Satisfaction$_{t}$", "empleo"="Employed$_{t}$", 
                              "desempleo"="Unemployed$_{t}$",
                              "3"="Wave 2018", "4"="Wave 2019"),
       caption="Dynamic Linear Panel Models (AB estimator) for Political Trust",
       float.pos="H", custom.note = "%stars. Robust standard error in parentheses clustered the level of respondents.", custom.model.names=paste0("Model ", 19:24), use.packages=FALSE)
  


#################################################
### MODELS FROM SECTION E ONLINE SUPPLEMENT)  ###
#################################################

cl_1a <- plm(confsoc01 ~ lag(confsoc01) + lag(insconf01) +
               hombre_w01 + edad_w01 + educaR_w01 + practica_w01 +
               derecha + centro + izq + LifeEval + empleo + desempleo + time, data=ell, 
               index = c("idencuesta", "time"), model = "pooling")
cl_2a <- plm(confsoc01 ~ lag(confsoc01) + insconf01 + 
               hombre_w01 + edad_w01 + educaR_w01 + practica_w01 + 
               derecha + centro + izq + LifeEval + empleo + desempleo + time, data=ell, 
             index = c("idencuesta", "time"), model = "pooling")
cl_3a <- plm(confsoc01 ~ lag(confsoc01) + lag(insconf01) + insconf01 + 
               hombre_w01 + edad_w01 + educaR_w01 + practica_w01 +
               derecha + centro + izq + LifeEval  + empleo + desempleo + time, data=ell, 
             index = c("idencuesta", "time"), model = "pooling")
cl_4a <- plm(insconf01 ~ lag(insconf01) + lag(confsoc01) +
               hombre_w01 + edad_w01 + educaR_w01 + practica_w01 + 
               derecha + centro + izq + LifeEval + empleo + desempleo + time, data=ell, 
             index = c("idencuesta", "time"), model = "pooling")
cl_5a <- plm(insconf01 ~ lag(insconf01) + confsoc01 +
               hombre_w01 + edad_w01 + educaR_w01 + practica_w01 + 
               derecha + centro + izq + LifeEval + empleo + desempleo + time, data=ell, 
             index = c("idencuesta", "time"), model = "pooling")
cl_6a <- plm(insconf01 ~ lag(insconf01) + lag(confsoc01) + confsoc01 + 
               hombre_w01 + edad_w01 + educaR_w01 + practica_w01 + 
               derecha + centro + izq + LifeEval + empleo + desempleo + time, data=ell, 
             index = c("idencuesta", "time"), model = "pooling")

robse_cl_1a <- sqrt(diag(vcovHC(cl_1a, type="HC0",cluster="group")))
robse_cl_2a <- sqrt(diag(vcovHC(cl_2a, type="HC0",cluster="group")))
robse_cl_3a <- sqrt(diag(vcovHC(cl_3a, type="HC0",cluster="group")))
robse_cl_4a <- sqrt(diag(vcovHC(cl_4a, type="HC0",cluster="group")))
robse_cl_5a <- sqrt(diag(vcovHC(cl_5a, type="HC0",cluster="group")))
robse_cl_6a <- sqrt(diag(vcovHC(cl_6a, type="HC0",cluster="group")))

pval_cl_1a <- (1 - pnorm(abs(coef(cl_1a))/robse_cl_1a))*2
pval_cl_2a <- (1 - pnorm(abs(coef(cl_2a))/robse_cl_2a))*2
pval_cl_3a <- (1 - pnorm(abs(coef(cl_3a))/robse_cl_3a))*2
pval_cl_4a <- (1 - pnorm(abs(coef(cl_4a))/robse_cl_4a))*2
pval_cl_5a <- (1 - pnorm(abs(coef(cl_5a))/robse_cl_5a))*2
pval_cl_6a <- (1 - pnorm(abs(coef(cl_6a))/robse_cl_6a))*2

texreg(l = list(cl_1a, cl_2a, cl_3a, cl_4a, cl_5a, cl_6a),
       override.se = list(robse_cl_1a, robse_cl_2a, robse_cl_3a,
                          robse_cl_4a, robse_cl_5a, robse_cl_6a),
       override.pvalues = list(pval_cl_1a, pval_cl_2a, pval_cl_3a,
                               pval_cl_4a, pval_cl_5a, pval_cl_6a),
       single.row = F, digits=3, scalebox=0.85,
       custom.coef.map = list("(Intercept)"="Intercept", "lag(confsoc01)"="Social Trust$_{t-1}$",
                              "confsoc01"="Social Trust$_{t}$", 
                              "lag(insconf01)"="Institutional Trust$_{t-1}$",
                              "insconf01"="Institutional Trust$_{t}$", "hombre_w01"="Male$_{t=1}$", 
                              "edad_w01"="Age$_{t=1}$",  
                              "educaR_w01Ed. Media Inc."="Ed. Secondary Incomplete$_{t=1}$",
                              "educaR_w01Ed. Media Com."="Ed. Secondary (Complete)$_{t=1}$",
                              "educaR_w01Ed. Tec. Sup."="Ed. Tertiary Technical$_{t=1}$",
                              "educaR_w01Ed. Univer."="Ed. Universitary$_{t=1}$",
                              "practica_w01"="Church Attendance$_{t=1}$", "derecha"="Right-wing$_{t}$",
                              "centro"="Centrist$_{t}$", "izq"="Left-wing$_{t}$", 
                              "empleo"="Employed$_{t}$", "desempleo"="Unemployed$_{t}$", 
                              "LifeEval"="Life satisfaction$_{t}$", "time3"="Wave 3", "time4"="Wave 4"),
       caption="Cross Lagged and Comtemporaneuos Panel Models for Social and Institutional Trust",
       custom.header = list("DV: Social Trust" = 1:3, "DV: Institutional Trust" = 4:6),
       float.pos="H", custom.note = "%stars. \\ Robust standard error in parentheses clustered the level of respondents.", custom.model.names=paste0("Model ", 1:6), use.packages=FALSE)


pl_1aa <- plm(diff(confsoc01) ~ -1 + diff(lag(insconf01)) + diff(practica) +
                diff(derecha) + diff(centro) + diff(izq) + diff(desempleo) +
                diff(empleo) +  diff(LifeEval) + time, 
                data=ell, index = c("idencuesta", "time"), model = "pooling")
ell$tt <- as.numeric(ell$time)
pl_1ba <- plm(diff(confsoc01) ~ -1 + diff(insconf01) + diff(practica) +
                diff(derecha) + diff(centro) + diff(izq) + diff(desempleo) +
                diff(empleo) +  diff(LifeEval) + time, 
              data=subset(ell, tt>=2), 
              index = c("idencuesta", "time"), model = "pooling")
pl_1ca <- plm(diff(confsoc01) ~ -1 + diff(lag(insconf01)) + diff(insconf01) + diff(practica) + 
                diff(derecha) + diff(centro) + diff(izq) + diff(desempleo) + diff(empleo) + 
                diff(LifeEval) + time, data=ell, 
              index = c("idencuesta", "time"), model = "pooling")
pl_2aa <- plm(diff(insconf01) ~ -1 + diff(lag(confsoc01)) + diff(practica) + 
                diff(derecha) + diff(centro) + diff(izq) + diff(desempleo) + diff(empleo) +
                diff(LifeEval) + time, data=ell, 
              index = c("idencuesta", "time"), model = "pooling")
pl_2ba <- plm(diff(insconf01) ~ -1 + diff(confsoc01) + diff(practica) + 
                diff(derecha) + diff(centro) + diff(izq) + diff(desempleo) + diff(empleo) +
                diff(LifeEval) + time,
              data=subset(ell, tt>=2), index = c("idencuesta", "time"), model = "pooling")
pl_2ca <- plm(diff(insconf01) ~ -1 + diff(lag(confsoc01)) + diff(confsoc01) + diff(practica) +
                diff(derecha) + diff(centro) + diff(izq) + diff(desempleo) + diff(empleo) +
                diff(LifeEval) + time, data=ell, index = c("idencuesta", "time"), 
              model = "pooling")

robse_pl_1aa <- sqrt(diag(vcovHC(pl_1aa, type="HC0",cluster="group")))
robse_pl_1ba <- sqrt(diag(vcovHC(pl_1ba, type="HC0",cluster="group")))
robse_pl_1ca <- sqrt(diag(vcovHC(pl_1ca, type="HC0",cluster="group")))
robse_pl_2aa <- sqrt(diag(vcovHC(pl_2aa, type="HC0",cluster="group")))
robse_pl_2ba <- sqrt(diag(vcovHC(pl_2ba, type="HC0",cluster="group")))
robse_pl_2ca <- sqrt(diag(vcovHC(pl_2ca, type="HC0",cluster="group")))
pval_pl_1aa <- (1 - pnorm(abs(coef(pl_1aa))/robse_pl_1aa))*2
pval_pl_1ba <- (1 - pnorm(abs(coef(pl_1ba))/robse_pl_1ba))*2
pval_pl_1ca <- (1 - pnorm(abs(coef(pl_1ca))/robse_pl_1ca))*2
pval_pl_2aa <- (1 - pnorm(abs(coef(pl_2aa))/robse_pl_2aa))*2
pval_pl_2ba <- (1 - pnorm(abs(coef(pl_2ba))/robse_pl_2ba))*2
pval_pl_2ca <- (1 - pnorm(abs(coef(pl_2ca))/robse_pl_2ca))*2

texreg(l = list(pl_1aa, pl_1ba, pl_1ca, pl_2aa, pl_2ba, pl_2ca), 
       override.se = list(robse_pl_1aa, robse_pl_1ba, robse_pl_1ca, 
                          robse_pl_2aa, robse_pl_2ba, robse_pl_2ca),
       override.pvalues = list(pval_pl_1aa, pval_pl_1ba, pval_pl_1ca, 
                               pval_pl_2aa, pval_pl_2ba, pval_pl_2ca),
       single.row = F, digits=2, scalebox=1,
       custom.coef.map = list(
         "diff(lag(insconf01))"="Institutional Trust$_{t-1}$", "diff(insconf01)"="Institutiona Trust$_{t}$", 
         "diff(lag(confsoc01))"="Social Trust$_{t-1}$", "diff(confsoc01)"="Social Trust$_{t}$", 
         "diff(practica)"="Church Attendance$_{t}$", "diff(derecha)"="Right-wing Ideology$_{t}$",
         "diff(centro)"="Centrist Ideology$_{t}$", "diff(izq)"="Left-wing Ideology$_{t}$",
         "diff(LifeEval)"="Life Satisfaction$_{t}$", "diff(empleo)"="Employed$_{t}$",
         "diff(desempleo)"="Unemployed$_{t}$", "time3"="Wave 3", "time4"="Wave 4"),
       caption="Linear Panel Models (First Differences) for Social and Institutional Trust",
       custom.header = list("DV: Social Trust" = 1:3, "DV: Institutional Trust" = 4:6),
       float.pos="H", custom.note = "%stars. Robust standard error in parentheses clustered the level of respondents.", custom.model.names=paste0("Model ", 7:12), use.packages=FALSE)


ins1a <- pgmm(confsoc01 ~ lag(confsoc01) + lag(insconf01) +
                practica + derecha + centro + izq + empleo + desempleo + LifeEval |
                lag(confsoc01, 2:3),
                data = ell1, effect = "twoways", model = "twosteps")

ins1b <- pgmm(confsoc01 ~ lag(confsoc01) + insconf01 +   
                practica + derecha + centro + izq + empleo + desempleo + LifeEval | 
                lag(confsoc01, 2:3), 
              data = ell1, effect = "twoways",  model = "twosteps")

ins1c <- pgmm(confsoc01 ~ lag(confsoc01)+ lag(insconf01) + insconf01 +  
                practica + derecha + centro + izq + empleo + desempleo + LifeEval | 
                lag(confsoc01, 2:3), 
              data = ell1, effect = "twoways",  model = "twosteps")

ins1d <- pgmm(confsoc01 ~ lag(confsoc01)+ insconf01 + 
                practica + derecha + centro + izq + empleo + desempleo + LifeEval | 
                lag(confsoc01, 2:3) + lag(insconf01, 1:2), 
              data = ell1, effect = "twoways",  model = "twosteps")

ins1e <- pgmm(confsoc01 ~ lag(confsoc01)+ lag(insconf01)+ 
                practica + derecha + centro + izq + empleo + desempleo + LifeEval | 
                lag(confsoc01, 2:3) + lag(insconf01, 2:3), 
              data = ell1, effect = "twoways",  model = "twosteps")

ins1f <- pgmm(confsoc01 ~ lag(confsoc01)+ insconf01 + lag(insconf01) + 
                practica + derecha + centro + izq + empleo + desempleo + LifeEval |
                lag(confsoc01, 2:3) + lag(insconf01, 1:2) + lag(insconf01, 2:3),
              data = ell1, effect = "twoways",  model = "twosteps")

ins2a <- pgmm(insconf01 ~ lag(insconf01) + confsoc01 + 
                practica + derecha + centro + izq + empleo + desempleo + LifeEval | 
                lag(insconf01, 2),
              data = ell1, effect = "twoways", model = "twosteps")

ins2b <- pgmm(insconf01 ~ lag(insconf01) + lag(confsoc01) + 
                practica + derecha + centro + izq + empleo + desempleo + LifeEval  |
                lag(insconf01, 2), 
              data = ell1, effect = "twoways", model = "twosteps")

ins2c <- pgmm(insconf01 ~ lag(insconf01) + confsoc01 + lag(confsoc01) + 
                practica + derecha + centro + izq + empleo + desempleo + LifeEval  |
                lag(insconf01, 2), 
              data = ell1, effect = "twoways", model = "twosteps")

ins2d <- pgmm(insconf01 ~ lag(insconf01) + confsoc01 + 
                practica + derecha + centro + izq + empleo + desempleo + LifeEval  |
                lag(insconf01, 2) + lag(confsoc01, 1:2), 
              data = ell1, effect = "twoways", model = "twosteps")

ins2e <- pgmm(insconf01 ~ lag(insconf01) + lag(confsoc01) +
                practica + derecha + centro + izq + empleo + desempleo + LifeEval |
                lag(insconf01, 2) + lag(confsoc01, 2:3), 
              data = ell1, effect = "twoways", model = "twosteps")

ins2f <- pgmm(insconf01 ~ lag(insconf01) + confsoc01 + lag(confsoc01) +
                practica + derecha + centro + izq + empleo + desempleo + LifeEval |
                lag(insconf01, 2) + lag(confsoc01, 1:2) + lag(confsoc01, 2:3), 
              data = ell1, effect = "twoways", model = "twosteps")


#DV: Social Trust
texreg(l = list(extract.pgmm_R(ins1a), extract.pgmm_R(ins1b), extract.pgmm_R(ins1c), 
                extract.pgmm_R(ins1e), extract.pgmm_R(ins1d), extract.pgmm_R(ins1f)),
       single.row = F, digits=3, scalebox=.9, 
       custom.coef.map = list("lag(confsoc01)"="Social Trust$_{t-1}$", 
                              "lag(insconf01)"="Institutional Trust$_{t-1}$", "insconf01"="Institutional Trust$_{t}$", 
                              "practica"="Church Attendance$_{t}$", "derecha"="Right-wing Ideology$_{t}$",
                              "centro"="Centrist Ideology$_{t}$", "izq"="Left-wing Ideology$_{t}$",
                              "LifeEval"="Life Satisfaction$_{t}$", "empleo"="Employed$_{t}$", 
                              "desempleo"="Unemployed$_{t}$",
                              "3"="Wave 2018", "4"="Wave 2019"),
       caption="Dynamic Linear Panel Models (AB estimator) for Social Trust",
       float.pos="H", custom.note = "%stars. Robust standard error in parentheses clustered the level of respondents.", 
       custom.model.names=paste0("Model ", 13:18), use.packages=FALSE)

#DV: Institutional Trust
texreg(l = list(extract.pgmm_R(ins2b), extract.pgmm_R(ins2a), extract.pgmm_R(ins2c), 
                extract.pgmm_R(ins2e), extract.pgmm_R(ins2d), extract.pgmm_R(ins2f)),
       single.row = F, digits=3, scalebox=.9, 
       custom.coef.map = list("lag(insconf01)"="Institutional Trust$_{t-1}$", 
                              "lag(confsoc01)"="Social Trust$_{t-1}$", 
                              "confsoc01"="Social Trust$_{t}$", 
                              "practica"="Church Attendance$_{t}$", "derecha"="Right-wing Ideology$_{t}$",
                              "centro"="Centrist Ideology$_{t}$", "izq"="Left-wing Ideology$_{t}$",
                              "LifeEval"="Life Satisfaction$_{t}$", "empleo"="Employed$_{t}$", 
                              "desempleo"="Unemployed$_{t}$",
                              "3"="Wave 2018", "4"="Wave 2019"),
       caption="Dynamic Linear Panel Models (AB estimator) for Institutional Trust",
       float.pos="H", custom.note = "%stars. Robust standard error in parentheses clustered the level of respondents.", 
       custom.model.names=paste0("Model ", 19:24), use.packages=FALSE)