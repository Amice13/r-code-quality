library(bibliometrix)
biblioshiny()

##### Web Of Science ######
# https://www.webofscience.com/wos/woscc/summary/fc9c528c-79d4-4f7c-a132-264fe9c1dd68-6827902c/relevance/1

b1_w <- "C:/Users/THIAGO/Downloads/WOS_riskmg_01.bib"
busca_b1_w <- convert2df(b1_w,dbsource="wos",format="bibtex")

b2_w <- "C:/Users/THIAGO/Downloads/WOS_riskmg_02.bib"
busca_b2_w <- convert2df(b2_w,dbsource="wos",format="bibtex")

b3_w <- "C:/Users/THIAGO/Downloads/WOS_riskmg_03.bib"
busca_b3_w <- convert2df(b3_w,dbsource="wos",format="bibtex")

b4_w <- "C:/Users/THIAGO/Downloads/WOS_riskmg_04.bib"
busca_b4_w <- convert2df(b4_w,dbsource="wos",format="bibtex")

b5_w <- "C:/Users/THIAGO/Downloads/WOS_riskmg_05.bib"
busca_b5_w <- convert2df(b5_w,dbsource="wos",format="bibtex")

b6_w <- "C:/Users/THIAGO/Downloads/WOS_riskmg_06.bib"
busca_b6_w <- convert2df(b6_w,dbsource="wos",format="bibtex")

b7_w <- "C:/Users/THIAGO/Downloads/WOS_riskmg_07.bib"
busca_b7_w <- convert2df(b7_w,dbsource="wos",format="bibtex")

b8_w <- "C:/Users/THIAGO/Downloads/WOS_riskmg_08.bib"
busca_b8_w <- convert2df(b8_w,dbsource="wos",format="bibtex")

b9_w <- "C:/Users/THIAGO/Downloads/WOS_riskmg_09.bib"
busca_b9_w <- convert2df(b9_w,dbsource="wos",format="bibtex")

b10_w <- "C:/Users/THIAGO/Downloads/WOS_riskmg_10.bib"
busca_b10_w <- convert2df(b10_w,dbsource="wos",format="bibtex")

#Risk Management
busca_unica_rm_w <- mergeDbSources(busca_b1_w, busca_b2_w, busca_b3_w, busca_b4_w, busca_b5_w, busca_b6_w, busca_b7_w, busca_b8_w, busca_b9_w, busca_b10_w, remove.duplicated=TRUE)
saveRDS(busca_unica_rm_w,file="C:/Users/THIAGO/Downloads/rm.rds")

#Systemic Risk
bs_w <- "C:/Users/THIAGO/Downloads/WOS_sysrisk.bib"
busca_unica_sr_w <- convert2df(bs_w,dbsource="wos",format="bibtex")

#Non Life
bnl_w <- "C:/Users/THIAGO/Downloads/WOS_nlife.bib"
busca_unica_nl_w <- convert2df(bnl_w,dbsource="wos",format="bibtex")

#Ruin Probability
br_w <- "C:/Users/THIAGO/Downloads/WOS_rprob.bib"
busca_unica_rp_w <- convert2df(br_w,dbsource="wos",format="bibtex")

#IFRS
bi_w <- "C:/Users/THIAGO/Downloads/WOS_ifrs.bib"
busca_unica_ifrs_w <- convert2df(bi_w,dbsource="wos",format="bibtex")

#Solvency
bso_w <- "C:/Users/THIAGO/Downloads/WOS_solv.bib"
busca_unica_solv_w <- convert2df(bso_w,dbsource="wos",format="bibtex")

#Catastrophe and Extreme Events
busca_unica_ext_w <- mergeDbSources(busca_b1_w, busca_b2_w, remove.duplicated=TRUE)

#Cyber Insurance
bcy_w <- "C:/Users/THIAGO/Downloads/WOS_cyber.bib"
busca_unica_cy_w <- convert2df(bcy_w,dbsource="wos",format="bibtex")

#ERM
berm_w <- "C:/Users/THIAGO/Downloads/WOS_ERM_all.bib"
busca_unica_erm_w <- convert2df(berm_w,dbsource="wos",format="bibtex")

#Telematics
bt_w <- "C:/Users/THIAGO/Downloads/WOS_T_all.bib"
busca_unica_t_w <- convert2df(bt_w,dbsource="wos",format="bibtex")

#Climate
busca_unica_cli_w <- mergeDbSources(busca_b1_w, busca_b2_w, busca_b3_w, busca_b4_w, busca_b5_w, busca_b6_w, busca_b7_w, busca_b8_w, remove.duplicated=TRUE)

saveRDS(busca_unica_c_w,file="C:/Users/THIAGO/Downloads/c.rds")
##### Scopus #####
# https://www.scopus.com/results/results.uri?sort=plf-f&src=s&sid=725950a47a56dc44f87c9430c8ee06bf&sot=a&sdt=a&cluster=scosubjabbr%2c%22SOCI%22%2ct%2c%22ECON%22%2ct%2c%22BUSI%22%2ct%2c%22MATH%22%2ct%2c%22ENVI%22%2ct%2c%22EART%22%2ct%2c%22PHYS%22%2ct%2bscosubtype%2c%22ar%22%2ct&sl=111&s=%28+TITLE-ABS-KEY+%28+insurance+OR+reinsurance+%29+AND+PUBYEAR+%3e+2006+AND+PUBYEAR+%3c+2023+%29+AND+%28+weather+OR+climate+%29&origin=searchadvanced&editSaveSearch=&txGid=bc657ffabcbd902f197200e911d6d854
## ( TITLE-ABS-KEY ( insurance  OR  reinsurance )  AND  PUBYEAR  >  2006  AND  PUBYEAR  <  2023 )  AND  ( weather  OR  climate )  AND  ( LIMIT-TO ( SUBJAREA ,  "SOCI" )  OR  LIMIT-TO ( SUBJAREA ,  "ECON" )  OR  LIMIT-TO ( SUBJAREA ,  "BUSI" )  OR  LIMIT-TO ( SUBJAREA ,  "MATH" )  OR  LIMIT-TO ( SUBJAREA ,  "ENVI" )  OR  LIMIT-TO ( SUBJAREA ,  "EART" )  OR  LIMIT-TO ( SUBJAREA ,  "PHYS" ) )  AND  ( LIMIT-TO ( DOCTYPE ,  "ar" ) ) 

b1_s <- "C:/Users/THIAGO/Downloads/Scopus_riskmg_22.bib"
busca_b1_s <- convert2df(b1_s,dbsource="scopus",format="bibtex")

b2_s <- "C:/Users/THIAGO/Downloads/Scopus_riskmg_21.bib"
busca_b2_s <- convert2df(b2_s,dbsource="scopus",format="bibtex")

b3_s <- "C:/Users/THIAGO/Downloads/Scopus_riskmg_20.bib"
busca_b3_s <- convert2df(b3_s,dbsource="scopus",format="bibtex")

b4_s <- "C:/Users/THIAGO/Downloads/Scopus_riskmg_19-18.bib"
busca_b4_s <- convert2df(b4_s,dbsource="scopus",format="bibtex")

b5_s <- "C:/Users/THIAGO/Downloads/Scopus_riskmg_17-16.bib"
busca_b5_s <- convert2df(b5_s,dbsource="scopus",format="bibtex")

b6_s <- "C:/Users/THIAGO/Downloads/Scopus_riskmg_15-13.bib"
busca_b6_s <- convert2df(b6_s,dbsource="scopus",format="bibtex")

b7_s <- "C:/Users/THIAGO/Downloads/Scopus_riskmg_12-10.bib"
busca_b7_s <- convert2df(b7_s,dbsource="scopus",format="bibtex")

b8_s <- "C:/Users/THIAGO/Downloads/Scopus_riskmg_09-07.bib"
busca_b8_s <- convert2df(b8_s,dbsource="scopus",format="bibtex")

#Risk Management
busca_unica_rm_s <- mergeDbSources(busca_b1_s, busca_b2_s, busca_b3_s, busca_b4_s, busca_b5_s, busca_b6_s, busca_b7_s, busca_b8_s, remove.duplicated=TRUE)

#Systemic Risk
bs_s <- "C:/Users/THIAGO/Downloads/Scopus_sysrisk.bib"
busca_unica_sr_s <- convert2df(bs_s,dbsource="scopus",format="bibtex")

#Non Life
bnl_s <- "C:/Users/THIAGO/Downloads/Scopus_nlife.bib"
busca_unica_nl_s <- convert2df(bnl_s,dbsource="scopus",format="bibtex")

#Ruin Probability
br_s <- "C:/Users/THIAGO/Downloads/Scopus_rprob.bib"
busca_unica_rp_s <- convert2df(br_s,dbsource="scopus",format="bibtex")

#IFRS
bi_s <- "C:/Users/THIAGO/Downloads/Scopus_ifrs.bib"
busca_unica_ifrs_s <- convert2df(bi_s,dbsource="scopus",format="bibtex")

#Solvency
bso_s <- "C:/Users/THIAGO/Downloads/Scopus_solv.bib"
busca_unica_solv_s <- convert2df(bso_s,dbsource="scopus",format="bibtex")

#Catastrophe and Extreme Events
bca_s <- "C:/Users/THIAGO/Downloads/Scopus_cat_extreme.bib"
busca_unica_ext_s <- convert2df(bca_s,dbsource="scopus",format="bibtex")

#Cyber Insurance
bcy_s <- "C:/Users/THIAGO/Downloads/Scopus_cyber.bib"
busca_unica_cy_s <- convert2df(bcy_s,dbsource="scopus",format="bibtex")

#ERM
berm_s <- "C:/Users/THIAGO/Downloads/Scopus_ERM_all.bib"
busca_unica_erm_s <- convert2df(berm_s,dbsource="scopus",format="bibtex")

#Telematics
bt_s <- "C:/Users/THIAGO/Downloads/Scopus_T_all.bib"
busca_unica_t_s <- convert2df(bt_s,dbsource="scopus",format="bibtex")

#Climate
busca_unica_cli_s <- mergeDbSources(busca_b1_s, busca_b2_s, busca_b3_s, remove.duplicated=TRUE)



#### União das 2 bases ####

#Risk Management
busca_riskmg <- mergeDbSources(busca_unica_rm_w, busca_unica_rm_s, remove.duplicated=TRUE)

#Systemic Risk
busca_syst <- mergeDbSources(busca_unica_sr_w, busca_unica_sr_s, remove.duplicated=TRUE)

#Non Life
busca_nlife <- mergeDbSources(busca_unica_nl_w, busca_unica_nl_s, remove.duplicated=TRUE)

#Ruin Probability
busca_rprob <- mergeDbSources(busca_unica_rp_w, busca_unica_rp_s, remove.duplicated=TRUE)

#IFRS
busca_ifrs <- mergeDbSources(busca_unica_ifrs_w, busca_unica_ifrs_s, remove.duplicated=TRUE)

#Solvency
busca_solv <- mergeDbSources(busca_unica_solv_w, busca_unica_solv_s, remove.duplicated=TRUE)

#Catastrophe and Extreme Events
busca_ext <- mergeDbSources(busca_unica_ext_w, busca_unica_ext_s, remove.duplicated=TRUE)

#Cyber Insurance
busca_cyber <- mergeDbSources(busca_unica_cy_w, busca_unica_cy_s, remove.duplicated=TRUE)

#ERM
busca_erm <- mergeDbSources(busca_unica_erm_w, busca_unica_erm_s, remove.duplicated=TRUE)

#Telematics
busca_tel <- mergeDbSources(busca_unica_t_w, busca_unica_t_s, remove.duplicated=TRUE)

#Climate
busca_cli <- mergeDbSources(busca_unica_cli_w, busca_unica_cli_s, remove.duplicated=TRUE)



#### Arquivos RDS com os grupos ####

#Todos
todos <- mergeDbSources(busca_riskmg, busca_syst, busca_nlife, busca_rprob, busca_ifrs, 
                        busca_solv, busca_ext, busca_cyber, busca_erm, busca_tel, busca_cli, remove.duplicated=TRUE)
saveRDS(todos,file="C:/Users/THIAGO/Downloads/todos.rds")

#core
core <- mergeDbSources(gerenciamento,solvencia)
saveRDS(core,file="C:/Users/THIAGO/Downloads/core.rds")

#trends
trends <- mergeDbSources(busca_ext,busca_tel,busca_cyber,busca_ifrs,busca_cli)
saveRDS(trends,file="C:/Users/THIAGO/Downloads/trends.rds")

#Gerenciamento de riscos
gerenciamento <- mergeDbSources(busca_riskmg, busca_syst, busca_erm, remove.duplicated=TRUE)
saveRDS(gerenciamento,file="C:/Users/THIAGO/Downloads/gerenciamento.rds")

#Solvência
solvencia <- mergeDbSources(busca_rprob, busca_solv, remove.duplicated=TRUE)
saveRDS(solvencia,file="C:/Users/THIAGO/Downloads/solvencia.rds")

#Eventos extremos
saveRDS(busca_ext,file="C:/Users/THIAGO/Downloads/extremo.rds")

#Telemática
saveRDS(busca_tel,file="C:/Users/THIAGO/Downloads/telematica.rds")

#Seguro cibernético
saveRDS(busca_cyber,file="C:/Users/THIAGO/Downloads/cibernetico.rds")

#IFRS
saveRDS(busca_ifrs,file="C:/Users/THIAGO/Downloads/ifrs.rds")

#Alterações climáticas
saveRDS(busca_cli,file="C:/Users/THIAGO/Downloads/climaticas.rds")