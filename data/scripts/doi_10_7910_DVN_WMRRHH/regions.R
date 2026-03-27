rm(list = ls())
load("regions.RData")

save(list = ls(all = TRUE), file= "regions.RData")

install.packages("")

library(RCzechia)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(lubridate)
library(MASS)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(glmmTMB)
library(treemap)
library(fixest)
library(car)
library(stargazer)
library(zoo)



##############################
########## LOAD DATA #########
##############################

# Source - https://www.psp.cz/sqw/hp.sqw?k=1300 (data of the Chamber of Deputies, tisky.zip).
# Codebook - https://www.psp.cz/sqw/hp.sqw?k=1303.
# In line 323, it is necessary to delete the word München to make the file "tisky" readable.

tisky <- read.csv("data/tisky/tisky.csv", sep = "|", header = F, fileEncoding = "Windows-1250")
colnames(tisky) <- c("id_tisk","id_druh","id_stav","ct","cislo_za","id_navrh","id_org","id_org_obd","id_osoba",
                     "navrhovatel","nazev_tisku","predlozeno","rozeslano","dal","tech_nos_dat","uplny_nazev_tisku",
                     "zm_lhuty","lhuta","rj","t_url","is_eu","roz","is_sdv","status","last")

druh_tisku <- read.csv("data/tisky/druh_tisku.csv", sep = "|", header = F, fileEncoding = "Windows-1250")
colnames(druh_tisku) <- c("id_druh","druh_t","nazev_druh","last")

typ_zakon <- read.csv("data/tisky/typ_zakon.csv", sep = "|", header = F, fileEncoding = "Windows-1250")
colnames(typ_zakon) <- c("id_navrh","druh_navrhovatele","last")

typ_stavu <- read.csv("data/tisky/typ_stavu.csv", sep = "|", header = F, fileEncoding = "Windows-1250")
colnames(typ_stavu) <- c("id_typ","popis_stavu","last")

stavy <- read.csv("data/tisky/stavy.csv", sep = "|", header = F, fileEncoding = "Windows-1250")
colnames(stavy) <- c("id_stav","id_typ","id_druh","popis","lhuta","lhuta_where","last")

hist <- read.csv("data/tisky/hist.csv", sep = "|", header = F, fileEncoding = "Windows-1250")
colnames(hist) <- c("id_hist","id_tisk","datum","id_hlas","id_prechod","id_bod","schuze",
                    "usnes_ps","orgv_id_posl","ps_id_posl","orgv_p_usn","zaver_publik","zaver_sb_castka","zaver_sb_cislo","poznamka","last")
hist$datum <- as.Date(hist$datum,format = "%Y-%m-%d")

prechody <- read.csv("data/tisky/prechody.csv", sep = "|", header = F, fileEncoding = "Windows-1250")
colnames(prechody) <- c("id_prechod","odkud","kam","id_akce","typ_prechodu","last")

proposals <- tisky[tisky$id_druh %in% c(1,2) & !is.na(tisky$id_navrh),] # composition of proposals
proposals$predlozeno <- as.Date(proposals$predlozeno, format = "%d.%m.%Y")
proposals <- proposals[proposals$predlozeno>=as.Date("01.01.2001", format = "%d.%m.%Y"),]
table(proposals$id_navrh)
round(table(proposals$id_navrh)/nrow(proposals)*100,1)


##############################
########## ADJUST DATA #######
##############################

regbills <- tisky[tisky$id_druh==2,] # 2 stands for legislative initiatives
regbills <- regbills[regbills$id_navrh>=6,] # 6 and higher stand for regional proposers
regbills <- regbills[!is.na(regbills$id_tisk),]

regbills$id_typ <- NA # add information on the legislation process
for (i in 1:nrow(regbills)) {
  regbills$id_typ[i] <- stavy$id_typ[stavy$id_stav==regbills$id_stav[i]]
}

regbills$law <- NA # add the number of collection if the bill becomes law
for (i in 1:nrow(regbills)) {
  regbills$law[i] <- paste0(hist$zaver_sb_castka[hist$id_tisk==regbills$id_tisk[i]][which.max(hist$datum[hist$id_tisk==regbills$id_tisk[i]])],
                            "/",hist$zaver_sb_cislo[hist$id_tisk==regbills$id_tisk[i]][which.max(hist$datum[hist$id_tisk==regbills$id_tisk[i]])])
}             
regbills$law <- ifelse(regbills$law=="NA/",NA,regbills$law)
regbills$law[regbills$id_tisk==40249] <- "90/260" # correction of a mistake in the data

regbills$vote <- NA # add the id of the last voting on the bill
for (i in 1:nrow(regbills)) {
  tryCatch({
  regbills$vote[i] <- hist$id_hlas[hist$id_tisk==regbills$id_tisk[i]][which.max(hist$id_hlas[hist$id_tisk==regbills$id_tisk[i]])]
  }, error=function(e){})
}

regbills$region <- NA # add names of the regions
for (i in 1:nrow(regbills)) {
  regbills$region[i] <- typ_zakon$druh_navrhovatele[typ_zakon$id_navrh==regbills$id_navrh[i]]
}
regbills$region <- substr(regbills$region,6,nchar(regbills$region))
regbills$region[regbills$region=="Vysočina"] <- "Kraj Vysočina"
regbills$region[regbills$region=="Královehradecký kraj"] <- "Královéhradecký kraj"

regbills$predlozeno <- as.Date(regbills$predlozeno, format = "%d.%m.%Y")


##############################
########## REGIONAL DATA #####
##############################

regions <- regbills %>% # proposed bills
  group_by(region) %>%
  count(region)
colnames(regions) <- c("region","bills")
  
laws <- regbills[!is.na(regbills$law),] %>% # adopted laws
  group_by(region) %>%
  count(region)
colnames(laws) <- c("region","laws")
regions <- merge(regions, laws, by = "region", all = T)
regions$laws[is.na(regions$laws)] <- 0

regions$success_rate <- regions$laws/regions$bills*100


##########################
########## TIME DATA #####
##########################

terms <- as.data.frame(table(cut(regbills$predlozeno, 'year')))
colnames(terms) <- c("year","laws")

terms_bills <- as.data.frame(table(cut(regbills$predlozeno[!is.na(regbills$law)], 'year')))
colnames(terms_bills) <- c("year","bills")

terms <- merge(terms,terms_bills, by = "year", all = T)
terms$bills[is.na(terms$bills)] <- 0
rm(terms_bills)

terms$average <- c(rep(NA,4),rollmean(terms$laws,5))


##########################
########## FIGURES #######
##########################

tiff("export/figure2.tiff", units = "cm", width = 26, height = 13, res = 600)
barplot <- barplot(terms$laws, las = 2, col = "grey80", xlab = "Year of proposal")
abline(h = 1:12, lt = "dotted", col = "grey60")
barplot(terms$laws, las = 2, col = "grey80", add = T)
barplot(terms$bills, col = "black", add = T, yaxt = "n")
axis(1, at = barplot, labels = 2001:2023)
text(barplot[1],1.5,"Laws", col = "white", srt = 90)
text(barplot[1],5.5,"Failed proposals", col = "black", srt = 90)
lines(barplot,terms$average)
text(barplot[14],7.5,"5-year average", col = "black")
dev.off()



#######################
########## MAPS #######
#######################

map_regions <- kraje("low")
map_regions <- merge(map_regions,regions,by.x = "NAZ_CZNUTS3", by.y = "region")

tiff("export/map1.tiff", units = "cm", width = 16, height = 8, res = 600)
ggplot(data = map_regions) +
  geom_sf(aes(fill = success_rate)) +
  scale_fill_gradient(low = "white", high = "black") +
  geom_sf_text(aes(label = paste0(laws,"/",bills)), color = ifelse(map_regions$laws/map_regions$bills>0.15,"white","black")) +
  labs(title = "",
       fill = "Success rate (%)") +
  theme_void()
dev.off()
             


#########################
########## EXPORT #######
#########################

write.xlsx(regbills[,c("id_tisk","id_org_obd","ct","nazev_tisku","uplny_nazev_tisku","predlozeno","region")], "data/initiatives.xlsx",
           sheetName = "Sheet1", colNames = TRUE, rowNames = F, append = FALSE)



#########################
########## IMPORT #######
#########################

final_inis <- read.xlsx("data/initiatives_211223.xlsx", sheet = 1) # load dataset of initiatives
final_inis <- merge(final_inis,regbills[,c("id_tisk","law")], by = "id_tisk")

final_inis <- final_inis[-c(6)] # replace data on dates of initiatives
final_inis <- merge(final_inis,regbills[,c("id_tisk","predlozeno")],by = "id_tisk")

final_inis$gov_opposition <- ifelse(final_inis$prime_minister==final_inis$governor,0,1) # identify opposition governors

final_inis$gov_opposition_alter <- 0
for (i in 1:nrow(final_inis)) {
  final_inis$gov_opposition_alter[i] <- ifelse(final_inis$governor[i] %in% unlist(strsplit(final_inis$nat_cab[i], " ")),0,1) # identify alternative opposition governors
}

final_inis$reg_opposition <- 0 # identify regional opposition against national cabinet
for (i in 1:nrow(final_inis)) {
  final_inis$reg_opposition[i] <- sum(unlist(strsplit(final_inis$reg_cab[i], " ")) %in% unlist(strsplit(final_inis$nat_cab[i], " ")))
}
final_inis$reg_opposition <- ifelse(final_inis$reg_opposition==0,1,0)

final_inis$law <- ifelse(is.na(final_inis$law),0,1) # recode laws

final_inis$year <- as.numeric(format(final_inis$predlozeno, "%Y")) # identify years


final_ry <- read.xlsx("data/initiatives_211223.xlsx", sheet = 2) # load region-years dataset
final_ry$year <- lubridate::ymd(final_ry$year, truncated = 2L) # adjust dates

final_ry$proposed <- 0 # identify the number of initiatives
for (i in 1:nrow(final_ry)) {
  final_ry$proposed[i] <- nrow(final_inis[final_inis$region==final_ry$region[i] & 
                    as.numeric(format(final_inis$predlozeno,'%Y'))==as.numeric(format(final_ry$year[i],'%Y')),])
}

final_ry$proposed_shared <- 0 # identify the number of shared rule initiatives
for (i in 1:nrow(final_ry)) {
  final_ry$proposed_shared[i] <- nrow(final_inis[final_inis$region==final_ry$region[i] & 
                                            as.numeric(format(final_inis$predlozeno,'%Y'))==as.numeric(format(final_ry$year[i],'%Y')) & final_inis$shared_rule==1,])
}

final_ry$proposal <- ifelse(final_ry$proposed==0,0,1)

final_ry$proposal_shared <- ifelse(final_ry$proposed_shared==0,0,1)

final_ry$gov_opposition <- ifelse(final_ry$prime_minister==final_ry$governor,0,1) # identify opposition governors

final_ry$gov_opposition_alter <- 0
for (i in 1:nrow(final_ry)) {
  final_ry$gov_opposition_alter[i] <- ifelse(final_ry$governor[i] %in% unlist(strsplit(final_ry$nat_cab[i], " ")),0,1) # identify alternative opposition governors
}

final_ry$reg_opposition <- 0 # identify regional opposition against national cabinet
for (i in 1:nrow(final_ry)) {
  final_ry$reg_opposition[i] <- sum(unlist(strsplit(final_ry$reg_cab[i], " ")) %in% unlist(strsplit(final_ry$nat_cab[i], " ")))
}
final_ry$reg_opposition <- ifelse(final_ry$reg_opposition==0,1,0)

final_ry$until_2011 <- ifelse(final_ry$year<=as.Date("2011-01-01", format("%Y-%m-%d")),1,0)  # add binary time variable
final_inis$until_2011 <- ifelse(final_inis$year<=2011,1,0)  # add binary time variable


##########################
########## FIGURES #######
##########################

tiff("export/figure1.tiff", units = "cm", width = 26, height = 13, res = 600)
par(mfrow=c(1,2), "mar"=c(7, 3, 2, 0))
fields <- sort(table(final_inis$field),decreasing = T)
barplot <- barplot(fields, yaxt = "n", col = "white", las = 2, font.main = 1, main = "a) Fields", ylim = c(0,60))
text(barplot,fields,labels = fields, pos = 1)
axis(2, at = seq(0,60,10), labels = seq(0,60,10), las = 2)
results <- sort(table(final_inis$result),decreasing = T)
barplot <- barplot(results, yaxt = "n", col = "white", las = 2, font.main = 1, main = "b) Results", ylim = c(0,60))
text(barplot,results,labels = results, pos = 1)
axis(2, at = seq(0,60,10), labels = seq(0,60,10), las = 2)
dev.off()


######################################
########## REGRESSION ANALYSES #######
######################################

##### proposals

prop_nb_1 <- glm.nb(proposed ~ reg_opposition, 
             data = final_ry)
summary(prop_nb_1)

prop_nb_2 <- glm.nb(proposed ~ gov_opposition_alter + reg_party_share + reg_cab_reg_party + governor_reg_party + enp + 
                    nat_cab_reg_party + nat_cab_majority + mps + distance + until_2011,
                  data=final_ry)
summary(prop_nb_2)

prop_nb_3 <- glm.nb(proposed_shared ~ gov_opposition_alter + reg_party_share + reg_cab_reg_party + governor_reg_party + enp + 
                      nat_cab_reg_party + nat_cab_majority + mps + distance + until_2011,
                    data=final_ry)
summary(prop_nb_3)


##### laws

laws_log_1 <- glm(law ~ shared_rule,
                  family=binomial(link='logit'), data=final_inis)
summary(laws_log_1)

laws_log_2 <- glm(law ~ shared_rule + gov_opposition_alter + reg_party_share + reg_cab_reg_party + governor_reg_party + enp + 
                     nat_cab_majority + mps + distance + until_2011,
                  family=binomial(link='logit'), data=final_inis[final_inis$result!="discussed",])
summary(laws_log_2)


stargazer(list(prop_nb_2, prop_nb_3, laws_log_2), type = "html", out = "export/regressions.html") # export regression models


##############################
########## PLOT MODELS #######
##############################

tiff("export/governor.tiff", units = "cm", width = 16, height = 8, res = 600)
plot_model(prop_nb_2, type = "pred", terms = "gov_opposition_alter", ci.lvl = 0.95,
           colors = "gs", title = "", axis.title = c("Opposition governor","Number of proposals per year"),
           auto.label = T)
dev.off()

tiff("export/figure3.tiff", units = "cm", width = 16, height = 8, res = 600)
plot_model(prop_nb_2, type = "pred", terms = "mps", ci.lvl = 0.95,
           colors = "gs", title = "", axis.title = c("Number of MPs","Number of proposals per year"))
dev.off()

tiff("export/figure4.tiff", units = "cm", width = 16, height = 8, res = 600)
plot_model(prop_nb_2, type = "pred", terms = "distance", ci.lvl = 0.95,
           colors = "gs", title = "", axis.title = c("Distance from Prague","Number of proposals per year"))
dev.off()

tiff("export/enp.tiff", units = "cm", width = 16, height = 8, res = 600)
plot_model(prop_nb_2, type = "pred", terms = "enp", ci.lvl = 0.95,
           colors = "gs", title = "", axis.title = c("Effective number of parties in regional assembly","Number of proposals per year"))
dev.off()

newdata <- with(final_inis, data.frame(shared_rule = c(0, 1), gov_opposition_alter=1, enp=mean(enp), reg_party_share=mean(reg_party_share),
                                       reg_cab_reg_party=0, governor_reg_party=0, nat_cab_reg_party=0, nat_cab_majority=1,
                                       mps=mean(mps), distance=mean(distance), until_2011=1))
preds <- predict(laws_log_2, newdata, type="response", se.fit = T)
preds$fit+qnorm(0.025,0,1)*preds$se.fit
preds$fit
preds$fit+qnorm(0.975,0,1)*preds$se.fit

newdata <- with(final_inis, data.frame(shared_rule = 1, gov_opposition_alter=1, enp=mean(enp), reg_party_share=mean(reg_party_share),
                                       reg_cab_reg_party=0, governor_reg_party=0, nat_cab_reg_party=0, nat_cab_majority=c(0,1),
                                       mps=mean(mps), distance=mean(distance), until_2011=1))
preds <- predict(laws_log_2, newdata, type="response", se.fit = T)
preds$fit+qnorm(0.025,0,1)*preds$se.fit
preds$fit
preds$fit+qnorm(0.975,0,1)*preds$se.fit

table(final_inis$id_org_obd)

sort(unique(hist$datum))


###########################
########## CCP DATA #######
###########################

ccp <- read.csv("data/ccpcnc_v4/ccpcnc/ccpcnc_v4.csv", sep = ",", header = T)
ccp <- ccp[ccp$leg_in_96==1,c("country","year","leg_in_96","leg_in_comments","fedunit")]
ccp <- ccp[!is.na(ccp$country),]
ccp <- ccp[ccp$year == 2021,] # delete older data
ccp <- ccp[ccp$country %in% c("Azerbaijan","Bolivia","Czech Republic","El Salvador","Georgia","Mexico","Moldova",
                              "Nicaragua","Peru","Portugal","Serbia","Switzerland","Uzbekistan"),] # plus Russia and Spain




