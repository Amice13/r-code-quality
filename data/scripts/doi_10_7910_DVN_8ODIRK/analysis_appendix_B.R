############################
## Replication Material: Script for replicating robustness checks: Appendix B
## Paltra, Sältzer, Stecker
## Affective States - Cultural and Affective Polarization in a Multilevel-Multiparty System 
## 2025
## Political Behavior
############################

# Loading all required definitions and datasets --------------------------------
source("_loadlibraries.R")
source("_definitions.R", encoding = "UTF-8")
load("party_placement.Rda")
load("wordscores.Rda")

# Manipulation of wordscores -------------------------------------
wordscores <- all_wordscores %>% 
  mutate(landyear = str_sub(landyearparty, 1, 6)) %>% 
  filter(land != "de") %>%
  dplyr::select(landyear, land, year, electiondate, party, lr = lire_pos_norm, econ = econ_pos_norm, soci = soci_pos_norm, immi = immi_pos_norm)

# Change CSU to CDU for identification reasons
wordscores$party[wordscores$party == "csu"] <- "cdu"

# Delete wordscores for all other parties
wordscores<-subset(wordscores, party == "fdp" | 
                     party == "pds" |
                     party == "gru" |
                     party == "cdu" |
                     party == "spd" |
                     party == "afd")

# Reshaping the format from long to wide
wordscores$lr_afd[wordscores$party == "afd"] <- wordscores$lr[wordscores$party == "afd"] 
wordscores$lr_fdp[wordscores$party == "fdp"] <- wordscores$lr[wordscores$party == "fdp"]
wordscores$lr_gru[wordscores$party == "gru"] <- wordscores$lr[wordscores$party == "gru"]
wordscores$lr_spd[wordscores$party == "spd"] <- wordscores$lr[wordscores$party == "spd"]
wordscores$lr_pds[wordscores$party == "pds"] <- wordscores$lr[wordscores$party == "pds"]
wordscores$lr_cdu[wordscores$party == "cdu"] <- wordscores$lr[wordscores$party == "cdu"]

wordscores$econ_afd[wordscores$party == "afd"] <- wordscores$econ[wordscores$party == "afd"] 
wordscores$econ_fdp[wordscores$party == "fdp"] <- wordscores$econ[wordscores$party == "fdp"] 
wordscores$econ_gru[wordscores$party == "gru"] <- wordscores$econ[wordscores$party == "gru"] 
wordscores$econ_spd[wordscores$party == "spd"] <- wordscores$econ[wordscores$party == "spd"] 
wordscores$econ_pds[wordscores$party == "pds"] <- wordscores$econ[wordscores$party == "pds"] 
wordscores$econ_cdu[wordscores$party == "cdu"] <- wordscores$econ[wordscores$party == "cdu"] 

wordscores$soci_afd[wordscores$party == "afd"] <- wordscores$soci[wordscores$party == "afd"] 
wordscores$soci_fdp[wordscores$party == "fdp"] <- wordscores$soci[wordscores$party == "fdp"] 
wordscores$soci_gru[wordscores$party == "gru"] <- wordscores$soci[wordscores$party == "gru"] 
wordscores$soci_spd[wordscores$party == "spd"] <- wordscores$soci[wordscores$party == "spd"] 
wordscores$soci_pds[wordscores$party == "pds"] <- wordscores$soci[wordscores$party == "pds"] 
wordscores$soci_cdu[wordscores$party == "cdu"] <- wordscores$soci[wordscores$party == "cdu"] 

wordscores$immi_afd[wordscores$party == "afd"] <- wordscores$immi[wordscores$party == "afd"] 
wordscores$immi_fdp[wordscores$party == "fdp"] <- wordscores$immi[wordscores$party == "fdp"] 
wordscores$immi_gru[wordscores$party == "gru"] <- wordscores$immi[wordscores$party == "gru"] 
wordscores$immi_spd[wordscores$party == "spd"] <- wordscores$immi[wordscores$party == "spd"] 
wordscores$immi_pds[wordscores$party == "pds"] <- wordscores$immi[wordscores$party == "pds"] 
wordscores$immi_cdu[wordscores$party == "cdu"] <- wordscores$immi[wordscores$party == "cdu"] 

ws_final<-aggregate(wordscores[c(10:33)], by = list(wordscores$landyear), FUN = sum, na.rm = TRUE)

# Set not available wordscores to NA
ws_final[ws_final == 0] <- NA

# Change name for identification reasons
colnames(ws_final)[1] <- "landyear"

# Perception vs. Actual

df <- merge(ws_final, party_placement, by = "landyear", all.y = TRUE)

# Calculating Spearmann's rho

df_cor <- c()

econ_einstufung <-  c(df$econ_einstufung_afd,
                      df$econ_einstufung_cdu,
                      df$econ_einstufung_spd,
                      df$econ_einstufung_gru, 
                      df$econ_einstufung_pds,
                      df$econ_einstufung_fdp)

econ_actual <- c(df$econ_afd,
                 df$econ_cdu,
                 df$econ_spd,
                 df$econ_gru, 
                 df$econ_pds,
                 df$econ_fdp)

immi_einstufung <-  c(df$soci_einstufung_afd,
                      df$soci_einstufung_cdu,
                      df$soci_einstufung_spd,
                      df$soci_einstufung_gru, 
                      df$soci_einstufung_pds,
                      df$soci_einstufung_fdp)

immi_actual <- c(df$immi_afd,
                 df$immi_cdu,
                 df$immi_spd,
                 df$immi_gru, 
                 df$immi_pds,
                 df$immi_fdp)

df_cor <- cbind.data.frame(econ_einstufung, econ_actual, immi_einstufung, immi_actual)

df_cor[df_cor == "NaN"] <- NA

df_cor[df_cor < 0] <- 0

df_cor <- na.omit(df_cor)

econ_corr <- cor.test(x= df_cor$econ_actual, y= df_cor$econ_einstufung, method = 'spearman')
econ_corr

immi_corr <- cor.test(x= df_cor$immi_actual, y= df_cor$immi_einstufung, method = 'spearman')
immi_corr

# Economic Dimension

pdf(paste0(file="cor_perception_actual_econ.pdf"))
plot(x= df_cor$econ_actual, y= df_cor$econ_einstufung,
     ylab = "Perception of Economic Position",
     xlab = "Actual Economic Position",
     pch = 19,
     col = "blue",
     lwd = 0.5,
     xaxt="n",
     yaxt="n")
lines(lowess(df_cor$econ_actual, df_cor$econ_einstufung, delta = 0.01 * diff(range(df_cor$econ_actual))),
      col = "red", lwd = 2)
axis(1, at=c(seq(0, 15, by=5)), labels=c("Left", 5, 10, "Right"))
axis(2, at=c(seq(4, 8, by=1)), labels=c("Left", 5, 6, 7, "Right"))
dev.off()

# Cultural Dimension

pdf(paste0(file="cor_perception_actual_cultural.pdf"))
plot(x= df_cor$immi_actual, y= df_cor$immi_einstufung,
     ylab = "Perception of Cultural Position",
     xlab = "Actual Cultural Position",
     pch = 19,
     col = "blue",
     lwd = 0.5,
     xaxt="n",
     yaxt="n")
lines(lowess(df_cor$immi_actual, df_cor$immi_einstufung, delta = 0.01 * diff(range(df_cor$immi_actual))),
      col = "red", lwd = 2)
axis(1, at=c(seq(0, 25, by=5)), labels=c("Left", 5, 10, 15, 20, "Right"))
axis(2, at=c(seq(4, 10, by=1)), labels=c("Left", 5, 6, 7, 8, 9,  "Right"))
dev.off()
