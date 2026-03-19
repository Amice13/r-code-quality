date()
library("tidyverse")
library("foreach")
library("haven")
library("iterators")
library("xtable")
sessionInfo()


raw_vict <- read_stata("violence_t_export.dta")
raw_phat <- read_csv("phats.csv")


## Clean victimization data and merge in our preferred CCPs
data_vict <- raw_vict %>%
    select(muni_code, victim_farc, victim_paras, nbi_t, royalties_t, coca_t,
           share_left, lpobl_tot_t, time, army_dist, gini_i, dmr, evlp, gcaribe,
           gpacifica, gorinoquia, gamazonia, farc_dist, paras_dist) %>%
    rename(victim_para = victim_paras,
           para_dist = paras_dist) %>%
    left_join(raw_phat, by = c("muni_code", "time")) %>%
    rename(p_farc_rf = pF, p_para_rf = pP)

# we have data on all variables besides victimization
colSums(is.na(data_vict))


fitFarc <- glm(victim_farc ~ nbi_t + royalties_t + coca_t + share_left +
                        lpobl_tot_t + factor(time) + army_dist + gini_i + dmr +
                        evlp + gcaribe + gpacifica + gorinoquia + gamazonia +
                        farc_dist + p_para_rf,
                    data = data_vict,
                    family = binomial(link = "logit"))

fitPara <- glm(victim_para ~ nbi_t + royalties_t + coca_t + share_left +
                        lpobl_tot_t + factor(time) + army_dist + gini_i + dmr +
                        evlp + gcaribe + gpacifica + gorinoquia + gamazonia +
                        para_dist + p_farc_rf,
                    data = data_vict,
                    family = binomial(link = "logit")) 



# ## SELECTION STUFF
FarcEnterNotPara <- !is.na(data_vict$victim_farc) & is.na(data_vict$victim_para)
ParaEnterNotFarc <- is.na(data_vict$victim_farc) & !is.na(data_vict$victim_para)

 
### impute 50-50 beliefs
data_vict_rand <- data_vict 
data_vict_rand$p_para_rf[FarcEnterNotPara] <- 1/2
data_vict_rand$p_farc_rf[ParaEnterNotFarc] <- 1/2 


fitFarc_rand <- glm(victim_farc ~ nbi_t + royalties_t + coca_t + share_left +
                  lpobl_tot_t + factor(time) + army_dist + gini_i + dmr +
                  evlp + gcaribe + gpacifica + gorinoquia + gamazonia +
                  farc_dist + p_para_rf,
               data = data_vict_rand,
               family = binomial(link = "logit"))

fitPara_rand <- glm(victim_para ~ nbi_t + royalties_t + coca_t + share_left +
                  lpobl_tot_t + factor(time) + army_dist + gini_i + dmr +
                  evlp + gcaribe + gpacifica + gorinoquia + gamazonia +
                  para_dist + p_farc_rf,
               data = data_vict_rand,
               family = binomial(link = "logit")) 


### impute mean beliefs
data_vict_mean <- data_vict 
data_vict_mean$p_para_rf[FarcEnterNotPara] <- mean(data_vict$p_para_rf, na.rm=T)
data_vict_mean$p_farc_rf[ParaEnterNotFarc] <- mean(data_vict$p_farc_rf, na.rm=T) 




fitFarc_mean <- glm(victim_farc ~ nbi_t + royalties_t + coca_t + share_left +
                       lpobl_tot_t + factor(time) + army_dist + gini_i + dmr +
                       evlp + gcaribe + gpacifica + gorinoquia + gamazonia +
                       farc_dist + p_para_rf,
                    data = data_vict_mean,
                    family = binomial(link = "logit"))

fitPara_mean <- glm(victim_para ~ nbi_t + royalties_t + coca_t + share_left +
                       lpobl_tot_t + factor(time) + army_dist + gini_i + dmr +
                       evlp + gcaribe + gpacifica + gorinoquia + gamazonia +
                       para_dist + p_farc_rf,
                    data = data_vict_mean,
                    family = binomial(link = "logit")) 

cfs <- c(fitFarc_rand$coefficients[17],
         fitPara_rand$coefficients[17],
         fitFarc_mean$coefficients[17],
         fitPara_mean$coefficients[17])

ses <- c(sqrt(diag(vcov(fitFarc_rand)))[17],
         sqrt(diag(vcov(fitPara_rand)))[17],
         sqrt(diag(vcov(fitFarc_mean)))[17],
         sqrt(diag(vcov(fitPara_mean)))[17])
ts <- cfs/ses 
pvals <-  pnorm(abs(ts), lower.tail = F)*2

tab<-round(cbind(cfs, ses, ts, pvals), digits=2)
rownames(tab) <- c("Farc a: rand", "Para a: rand", "Farc a: mean", "Para a: mean")
xtable(tab)

date()
