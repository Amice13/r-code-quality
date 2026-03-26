# MID conflict data
MIDA_4_01 <- read.csv("MIDA_4.01.csv",
                      stringsAsFactors = F)
MIDB_4_01 <- read.csv("MIDB_4.01.csv",
                      stringsAsFactors = F)

ccode_name_df <- unique(data.frame(ccode = MIDB_4_01$ccode,
                                   StAbb = MIDB_4_01$StAbb))

## Different countries abbreviation to same country code
duplicate_ccode_name_df <- ccode_name_df[duplicated(ccode_name_df$ccode) |
                                           duplicated(ccode_name_df$ccode, fromLast = T) , ]

#        ccode StAbb
# 15     365   USR
# 808    816   DRV
# 1323   490   ZAI
# 4327   365   RUS
# 4821   816   VTM
# 4888   490   DRC

require(dplyr)
duplicate_ccode_name_df <-
  duplicate_ccode_name_df %>%
  dplyr::group_by(ccode) %>%
  summarize(StAbb = paste(StAbb, collapse = "-"))

ccode_name_df <- ccode_name_df[-which(ccode_name_df$ccode %in% duplicate_ccode_name_df$ccode),]
ccode_name_df <- rbind(ccode_name_df, duplicate_ccode_name_df)

## As dyadic
MIDB_4_01$StDay[MIDB_4_01$StDay == -9] <- 15
MIDB_4_01$EndDay[MIDB_4_01$EndDay == -9] <- 15

MIDB_4_01_disp_cnt_yr <- data.frame()

for (i in 1:nrow(MIDB_4_01)) {
  MIDB_4_01_disp_cnt_yr <- 
    rbind(MIDB_4_01_disp_cnt_yr, 
          data.frame(DispNum3 = MIDB_4_01$DispNum3[i], 
                     StAbb = MIDB_4_01$StAbb[i], 
                     ccode  = MIDB_4_01$ccode[i],
                     year = seq(from = MIDB_4_01$StYear[i],
                                to = MIDB_4_01$EndYear[i]),
                     SideA = MIDB_4_01$SideA[i],
                     Fatality = MIDB_4_01$Fatality[i],
                     HostLev = MIDB_4_01$HostLev[i],
                     StDate = as.Date(paste(MIDB_4_01$StYear[i], 
                                            sprintf("%02d", MIDB_4_01$StMon[i]),
                                            sprintf("%02d", MIDB_4_01$StDay[i]),
                                            sep = "-")),
                     EndDate = as.Date(paste(MIDB_4_01$EndYear[i], 
                                             sprintf("%02d", MIDB_4_01$EndMon[i]),
                                             sprintf("%02d", MIDB_4_01$EndDay[i]),
                                             sep = "-")),
                     stringsAsFactors = F))
}

MIDB_4_01_dyad_yr <-
  merge(MIDB_4_01_disp_cnt_yr[MIDB_4_01_disp_cnt_yr$SideA == 1,],
        MIDB_4_01_disp_cnt_yr[MIDB_4_01_disp_cnt_yr$SideA == 0,],
        by = c("DispNum3","year"),
        suffixes = c("1","2"))

MIDB_4_01_dyad_yr_ordered <- data.frame()
for (i in 1:nrow(MIDB_4_01_dyad_yr)) {
  if (MIDB_4_01_dyad_yr$ccode1[i] < MIDB_4_01_dyad_yr$ccode2[i]) {
    MIDB_4_01_dyad_yr_ordered <- 
      rbind(MIDB_4_01_dyad_yr_ordered, 
            MIDB_4_01_dyad_yr[i,])
  } else {
    MIDB_4_01_dyad_yr_ordered <-
      rbind(MIDB_4_01_dyad_yr_ordered, 
            data.frame(DispNum3 = MIDB_4_01_dyad_yr$DispNum3[i],
                       year = MIDB_4_01_dyad_yr$year[i],
                       StAbb1 =  MIDB_4_01_dyad_yr$StAbb2[i],
                       ccode1 =  MIDB_4_01_dyad_yr$ccode2[i],
                       SideA1 =  MIDB_4_01_dyad_yr$SideA2[i],
                       Fatality1 =  MIDB_4_01_dyad_yr$Fatality2[i],
                       HostLev1 =  MIDB_4_01_dyad_yr$HostLev2[i],
                       StDate1 = MIDB_4_01_dyad_yr$StDate2[i],
                       EndDate1 = MIDB_4_01_dyad_yr$EndDate2[i],
                       StAbb2 =  MIDB_4_01_dyad_yr$StAbb1[i],
                       ccode2 =  MIDB_4_01_dyad_yr$ccode1[i],
                       SideA2 =  MIDB_4_01_dyad_yr$SideA1[i],
                       Fatality2 =  MIDB_4_01_dyad_yr$Fatality1[i],
                       HostLev2  =  MIDB_4_01_dyad_yr$HostLev1[i],
                       StDate2 = MIDB_4_01_dyad_yr$StDate1[i],
                       EndDate2 = MIDB_4_01_dyad_yr$EndDate1[i],
                       stringsAsFactors = F))
  }
}

MIDB_4_01_dyad_yr <- MIDB_4_01_dyad_yr_ordered
rm(MIDB_4_01_dyad_yr_ordered)

## Check for time copresence in same dispute
checkCopresence <- function(StDate1, EndDate1, StDate2, EndDate2) {
  
  print(paste0("StDate1 <- ", StDate1))
  print(paste0("EndDate1 <- ", EndDate1))
  print(paste0("StDate2 <- ", StDate2))
  print(paste0("EndDate2 <- ", EndDate2))
  
  if(EndDate1 < StDate1) {
    tmpSt1 <- EndDate1
    tmpEnd1 <- StDate1
    StDate1 <- tmpSt1
    EndDate1 <- tmpEnd1
  }
  
  if(EndDate2 < StDate2) {
    tmpSt2 <- EndDate2
    tmpEnd2 <- StDate2
    StDate2 <- tmpSt2
    EndDate2 <- tmpEnd2
  }
  
  if(StDate1 == EndDate1) {
    seq1 <- StDate1
  } else {
    seq1 <- seq(from = StDate1, to = EndDate1, by = 1)
  }
  
  if(StDate2 ==  EndDate2) {
    seq2 <- StDate2
  } else {
    seq2 <- seq(from = StDate2, to = EndDate2, by = 1)
  }
  
  return(length(intersect(seq1, seq2)) > 0)
  
  # Test
  # StDate1 = MIDB_4_01_dyad_yr$StDate1[1000]
  # EndDate1 = MIDB_4_01_dyad_yr$EndDate1[1000]
  # StDate2 = MIDB_4_01_dyad_yr$StDate2[234]
  # EndDate2 = MIDB_4_01_dyad_yr$EndDate2[234]
  
}

MIDB_4_01_dyad_yr$coop <- 
  mapply(checkCopresence, 
         MIDB_4_01_dyad_yr$StDate1,
         MIDB_4_01_dyad_yr$EndDate1,
         MIDB_4_01_dyad_yr$StDate2,
         MIDB_4_01_dyad_yr$EndDate2)

MIDB_4_01_dyad_yr <- 
  MIDB_4_01_dyad_yr[MIDB_4_01_dyad_yr$coop == TRUE,]


## Summary of key variables
summary(MIDB_4_01_dyad_yr$Fatality1)
summary(MIDB_4_01_dyad_yr$Fatality2)
summary(MIDB_4_01_dyad_yr$HostLev1)
summary(MIDB_4_01_dyad_yr$HostLev2)

# HostLevRecoded #
# Addition on 26 April 2018 

## IMPORTANT! Missing values replaced with 0s (about 700 cases)
MIDB_4_01_dyad_yr$Fatality1[MIDB_4_01_dyad_yr$Fatality1 == -9] <- 0
MIDB_4_01_dyad_yr$Fatality2[MIDB_4_01_dyad_yr$Fatality2 == -9] <- 0

# Recode

recodeHostLev <- function(HostLev, Fatality) { 
  
  if (HostLev == 1) {
    HostLev <- 1
  } else if (HostLev  %in% c(2,3,4)) {
    HostLev <- 2
  } else if (HostLev == 5) {
    HostLev <- 3
  }
  
  HostLev <- HostLev + 1
  HostLevFatality <- HostLev + (Fatality / HostLev)
  return(HostLevFatality - 1)
}

recodeHostLev5Point <- function(HostLev, Fatality) { 
  
  HostLev <- HostLev + 1
  HostLevFatality <- HostLev + (Fatality / HostLev)
  return(HostLevFatality - 1)
}

MIDB_4_01_dyad_yr$HostLevFatality1 <- 
  mapply(recodeHostLev, MIDB_4_01_dyad_yr$HostLev1, MIDB_4_01_dyad_yr$Fatality1)
MIDB_4_01_dyad_yr$HostLevFatality2 <- 
  mapply(recodeHostLev, MIDB_4_01_dyad_yr$HostLev2, MIDB_4_01_dyad_yr$Fatality2)

MIDB_4_01_dyad_yr$HostLevFatality1.5point <- 
  mapply(recodeHostLev5Point, MIDB_4_01_dyad_yr$HostLev1, MIDB_4_01_dyad_yr$Fatality1)
MIDB_4_01_dyad_yr$HostLevFatality2.5point <- 
  mapply(recodeHostLev5Point, MIDB_4_01_dyad_yr$HostLev2, MIDB_4_01_dyad_yr$Fatality2)

barplot(table(as.factor(MIDB_4_01_dyad_yr$HostLevFatality1)), main = 'HostLevFatality1')
barplot(table(as.factor(MIDB_4_01_dyad_yr$HostLevFatality2)), main = 'HostLevFatality2')

barplot(table(as.factor(MIDB_4_01_dyad_yr$HostLevFatality1.5point)), main = 'HostLevFatality1')
barplot(table(as.factor(MIDB_4_01_dyad_yr$HostLevFatality2.5point)), main = 'HostLevFatality2')

require(ggplot2)
ggplot(as.data.frame(table(as.factor(round(MIDB_4_01_dyad_yr$HostLevFatality1,2)))), aes(x=Var1, y=Freq)) +
  geom_bar(stat='identity')

summary(MIDB_4_01_dyad_yr$HostLev1)
summary(MIDB_4_01_dyad_yr$HostLev2)

save(MIDB_4_01_dyad_yr, file = "MID_4_01_dyad_year.RData")
write.csv(MIDB_4_01_dyad_yr, file = "MID_4_01_dyad_year.csv", row.names = F)

# ICB
icb1v11 <- read.csv("icb1v11.csv", stringsAsFactors = F)
icb2v11 <- read.csv("icb2v11.csv", stringsAsFactors = F)

cracid_actor_df <- unique(data.frame(cracid = icb2v11$cracid,
                                     actor = icb2v11$actor))

cracid_actor_df <- 
  cracid_actor_df %>%
  dplyr::group_by(cracid) %>%
  dplyr::summarize(actor = paste(actor, collapse = "|"))

cracid_actor_df <-
  rbind(cracid_actor_df,
        data.frame(cracid = c(995,996,997,484),
                   actor = c("internal","non-state","more-states","CON")))


## Check data 
# View(icb2v11[is.na(icb2v11$yrterm),])
icb2v11$yrterm[icb2v11$crisname == 'WEST IRIAN I'] <- 1957

icb2v11_crs_cnt_yr <- data.frame()

for (i in 1:nrow(icb2v11)) {
  icb2v11_crs_cnt_yr <- 
    rbind(icb2v11_crs_cnt_yr, 
          data.frame(cracid = icb2v11$cracid[i], 
                     actor = icb2v11$actor[i],
                     crisno = icb2v11$crisno[i],
                     year = seq(icb2v11$yrtrig[i], icb2v11$yrterm[i]),
                     southv = icb2v11$southv[i],
                     sevvio = icb2v11$sevvio[i],
                     cenvio = icb2v11$cenvio[i],
                     stringsAsFactors = F))
}


icb2v11_crs_cnt_yr_ordered <- data.frame() 
for (i in 1:nrow(icb2v11_crs_cnt_yr)) {
  if(icb2v11_crs_cnt_yr$cracid[i] < icb2v11_crs_cnt_yr$southv[i]) {
    icb2v11_crs_cnt_yr_ordered <- 
      rbind(icb2v11_crs_cnt_yr_ordered, 
            data.frame(crisno = icb2v11_crs_cnt_yr$crisno[i],
                       cracid1 = icb2v11_crs_cnt_yr$cracid[i],
                       actor1 = icb2v11_crs_cnt_yr$actor[i],
                       year = icb2v11_crs_cnt_yr$year[i],
                       cracid2 = icb2v11_crs_cnt_yr$southv[i],
                       actor2 = cracid_actor_df$actor[cracid_actor_df$cracid == icb2v11_crs_cnt_yr$southv[i]],
                       sevvio = icb2v11_crs_cnt_yr$sevvio[i],
                       cenvio = icb2v11_crs_cnt_yr$cenvio[i],
                       stringsAsFactors = F))
  } else {
    icb2v11_crs_cnt_yr_ordered <- 
      rbind(icb2v11_crs_cnt_yr_ordered, 
            data.frame(crisno = icb2v11_crs_cnt_yr$crisno[i],
                       year = icb2v11_crs_cnt_yr$year[i],
                       cracid1 = icb2v11_crs_cnt_yr$southv[i],
                       actor1 = cracid_actor_df$actor[cracid_actor_df$cracid == icb2v11_crs_cnt_yr$southv[i]],
                       cracid2 = icb2v11_crs_cnt_yr$cracid[i],
                       actor2 = icb2v11_crs_cnt_yr$actor[i],
                       sevvio = icb2v11_crs_cnt_yr$sevvio[i],
                       cenvio = icb2v11_crs_cnt_yr$cenvio[i],
                       stringsAsFactors = F))
  }
}

# test <- icb2v11_crs_cnt_yr_ordered[dup,]
# test <- test[with(test, order(crisno, cracid1, cracid2, year)), ]
# test_diff <-
#   test %>%
#   dplyr::group_by(crisno, cracid1, cracid2, year) %>%
#   dplyr::summarise(sevvio = mean(sevvio),
#                    cenvio = mean(cenvio))
# test_diff$sevvio_test <- 
#   sapply(test_diff$sevvio, FUN = function(x) x%%1==0)
# test_diff$cenvio_test <- 
#   sapply(test_diff$cenvio, FUN = function(x) x%%1==0)

test_missing <-
  icb2v11_crs_cnt_yr_ordered %>%
  dplyr::group_by(crisno, year, cracid1, actor1, cracid2, actor2) %>%
  dplyr::summarize(sevvio_is_na = sum(is.na(sevvio)),
                   cenvio_is_na = sum(is.na(cenvio)))

icbv11_dyad_yr <-
  icb2v11_crs_cnt_yr_ordered %>%
  dplyr::group_by(crisno, year, cracid1, actor1, cracid2, actor2) %>%
  dplyr::summarize(sevvio = max(sevvio),
                   cenvio = max(cenvio))

save(icbv11_dyad_yr, file = "icbv11_dyad_year.RData")
write.csv(icbv11_dyad_yr, file = "icbv11_dyad_year.csv", row.names = F)

# Merge MID and ICBV datasets
rm(list = ls())
require(data.table)
load("MID_4_01_dyad_year.RData")
load("icbv11_dyad_year.RData")

## MID country code
mid_country_codes <- 
  data.frame(code = c(MIDB_4_01_dyad_yr$ccode1, MIDB_4_01_dyad_yr$ccode2),
             name = c(MIDB_4_01_dyad_yr$StAbb1, MIDB_4_01_dyad_yr$StAbb2), 
             stringsAsFactors = F)
mid_country_codes <- unique(mid_country_codes)
# any(duplicated(mid_country_codes$code))
# View(mid_country_codes[duplicated(mid_country_codes$code) |
#                          duplicated(mid_country_codes$code, fromLast = T),])
require(dplyr)
mid_country_codes <-
  mid_country_codes %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(name = paste(name, collapse = "|"))

## ICB country code
icb_country_codes <- 
  data.frame(code = c(icbv11_dyad_yr$cracid1, icbv11_dyad_yr$cracid2), 
             name = c(icbv11_dyad_yr$actor1, icbv11_dyad_yr$actor2),
             stringsAsFactors = F)
icb_country_codes <- unique(icb_country_codes)
# any(duplicated(icb_country_codes$code))
# View(icb_country_codes[duplicated(icb_country_codes$code) | 
#                          duplicated(icb_country_codes$code, fromLast = T),])
icb_country_codes <- icb_country_codes[-which(icb_country_codes$name %in% c("DRC","CON") &
                                                icb_country_codes$code == 490),]

merged_code <- 
  merge(mid_country_codes, icb_country_codes, 
        by = "code",
        suffixes = c('.mid', ".icb"),
        all = T)

merged_code$same <- merged_code$name.mid == merged_code$name.icb
# table(merged_code$same, useNA = 'always')

# FALSE  TRUE  <NA> 
#   4   135    59 

# write.csv(merged_code, file = "MID_ICB_merged_code.csv")

## Merging rules
### Difference in names ICB
icb_country_codes$code[icb_country_codes$code == 678] <- 679 # YEM -> YEM
icb_country_codes$code[icb_country_codes$code == 219] <- 220 # VFR -> FRN

icbv11_dyad_yr$cracid1[icbv11_dyad_yr$cracid1 == 678] <- 679
icbv11_dyad_yr$cracid2[icbv11_dyad_yr$cracid2 == 678] <- 679
icbv11_dyad_yr$cracid1[icbv11_dyad_yr$cracid1 == 219] <- 220
icbv11_dyad_yr$cracid2[icbv11_dyad_yr$cracid2 == 219] <- 220


merged_code <-
  merge(mid_country_codes, icb_country_codes,
        by = "code",
        suffixes = c('.mid', ".icb"),
        all = T)

### Merging stats
nrow(merged_code[is.na(merged_code$name.mid),])
nrow(merged_code[is.na(merged_code$name.icb),])

MIDB_4_01_dyad_yr <- data.table(MIDB_4_01_dyad_yr)
icbv11_dyad_yr <- data.table(icbv11_dyad_yr)

names(icbv11_dyad_yr)[3] <- "ccode1"
names(icbv11_dyad_yr)[5] <- "ccode2"

### Multiple disputes/crisis
# mid_dup_yr <- 
#   duplicated(MIDB_4_01_dyad_yr[,c("ccode1", "ccode2", "year")]) |
#   duplicated(MIDB_4_01_dyad_yr[,c("ccode1", "ccode2", "year")], fromLast = T)
# View(MIDB_4_01_dyad_yr[mid_dup_yr,])
# icb_dup_yr <-
#   duplicated(icbv11_dyad_yr[,c("ccode1", "ccode2", "year")]) |
#   duplicated(icbv11_dyad_yr[,c("ccode1", "ccode2", "year")], fromLast = T)
# View(icbv11_dyad_yr[icb_dup_yr,])

require(dplyr)
MIDB_4_01_dyad_yr <-
  MIDB_4_01_dyad_yr %>%
  dplyr::group_by(ccode1, ccode2, year) %>%
  dplyr::summarize(Fatality = max(Fatality1 + Fatality2),
                   HostLevel = max(HostLev1 + HostLev2),
                   HostLevelFatality = max(HostLevFatality1 + HostLevFatality2),
                   HostLevelFatality.5point = max(HostLevFatality1.5point + HostLevFatality2.5point) ) %>%
  data.table()

barplot(table(MIDB_4_01_dyad_yr$HostLevelFatality), main = 'HostLevelFatality')
barplot(table(MIDB_4_01_dyad_yr$HostLevelFatality.5point), main = 'HostLevelFatality.5point')

rescaleHostLevFatality <- function(x) {
  sapply(x, FUN = function(x) 
    if (is.na(x)) {return(NA)} 
    else if (x == 0) {return(0)} 
    else {return(x-2)})
}

MIDB_4_01_dyad_yr$HostLevelFatality <-
  rescaleHostLevFatality(MIDB_4_01_dyad_yr$HostLevelFatality)

MIDB_4_01_dyad_yr$HostLevelFatality.5point <-
  rescaleHostLevFatality(MIDB_4_01_dyad_yr$HostLevelFatality.5point)

icbv11_dyad_yr <-
  icbv11_dyad_yr %>%
  dplyr::group_by(ccode1, ccode2, year) %>%
  dplyr::summarize(sevvio = max(sevvio),
                   cenvio = max(cenvio)) %>%
  data.table()

# Not provided
library(haven)
template <- read.csv("BG_dyadtemplate_170627a_stata13.csv")

template <- data.table(template)
setkeyv(template, c("ccode1", "ccode2", "year"))

check_templated_df <-
  as.data.frame(table(mapply(template$ccode1, template$ccode2, FUN = function(x,y) paste0(x,"|",y))))
hist(check_templated_df$Freq, xlab = "Number of years for dyad", main = NA)

dyads_template <- unique(paste(template$ccode1, template$ccode2, sep = "|"))
dyads_mid <- unique(paste(MIDB_4_01_dyad_yr$ccode1, MIDB_4_01_dyad_yr$ccode2, sep = "|"))
dyads_icb <- unique(paste(icbv11_dyad_yr$ccode1, icbv11_dyad_yr$ccode2, sep = "|"))

setkeyv(MIDB_4_01_dyad_yr, c("ccode1", "ccode2", "year"))
setkeyv(icbv11_dyad_yr, c("ccode1", "ccode2", "year"))

mid_and_icb_dyad_yr <- merge(template, MIDB_4_01_dyad_yr, all.x = T)
mid_and_icb_dyad_yr <- merge(mid_and_icb_dyad_yr, icbv11_dyad_yr, all.x = T)

# RECODING of values
setNaToZero <- 
  function(x) {
    if (is.na(x)) return(0)
    else return(x)
  }
## NOTE: this is not imputation. NA are due to dyad/year not included in the two datasets (it is assumed becasue of no ongoing conflict)
cols <- names(mid_and_icb_dyad_yr)[12:16]
for (j in cols) set(mid_and_icb_dyad_yr, j = j, value = sapply(mid_and_icb_dyad_yr[[j]], setNaToZero))

movAvg <- function(x, k) {
  require(zoo)
  if (length(x)<k) {
    return(x) 
  } else {
    return(rollmean(x, k = k, fill = NA))
  }
}

movAvgRightAligned <- function(x, k) {
  require(zoo)
  if (length(x)<k) {
    return(x) 
  } else {
    return(rollmean(x, k = k, fill = NA, align = 'right'))
  }
}

mid_and_icb_dyad_yr <- 
  mid_and_icb_dyad_yr %>%
  dplyr::group_by(ccode1, ccode2) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(Fatality_5ma = movAvg(Fatality, 5),
                HostLevel_5ma = movAvg(HostLevel, 5),
                HostLevelFatality_5ma = movAvg(HostLevelFatality, 5),
                HostLevelFatality_5ma.5point = movAvg(HostLevelFatality.5point, 5),
                sevvio_5ma = movAvg(sevvio, 5),
                cenvio_5ma = movAvg(cenvio, 5),
                Fatality_3ma = movAvg(Fatality, 3),
                HostLevel_3ma = movAvg(HostLevel, 3),
                HostLevelFatality_3ma = movAvg(HostLevelFatality, 3),
                HostLevelFatality_3ma.5point = movAvg(HostLevelFatality.5point, 3),
                sevvio_3ma = movAvg(sevvio, 3),
                cenvio_3ma = movAvg(cenvio, 3),
                Fatality_5mar = movAvgRightAligned(Fatality, 5),
                HostLevel_5mar = movAvgRightAligned(HostLevel, 5),
                HostLevelFatality_5mar = movAvgRightAligned(HostLevelFatality, 5),
                HostLevelFatality_5mar.5point = movAvgRightAligned(HostLevelFatality.5point, 5),
                sevvio_5mar = movAvgRightAligned(sevvio, 5),
                cenvio_5mar = movAvgRightAligned(cenvio, 5),
                Fatality_4mar = movAvgRightAligned(Fatality, 4),
                HostLevel_4mar = movAvgRightAligned(HostLevel, 4),
                HostLevelFatality_4mar = movAvgRightAligned(HostLevelFatality, 4),
                HostLevelFatality_4mar.5point = movAvgRightAligned(HostLevelFatality.5point, 4),
                sevvio_4mar = movAvgRightAligned(sevvio, 4),
                cenvio_4mar = movAvgRightAligned(cenvio, 4),
                Fatality_3mar = movAvgRightAligned(Fatality, 3),
                HostLevel_3mar = movAvgRightAligned(HostLevel, 3),
                HostLevelFatality_3mar = movAvgRightAligned(HostLevelFatality, 3),
                HostLevelFatality_3mar.5point = movAvgRightAligned(HostLevelFatality.5point, 3),
                sevvio_3mar = movAvgRightAligned(sevvio, 3),
                cenvio_3mar = movAvgRightAligned(cenvio, 3),
                Fatality_2mar = movAvgRightAligned(Fatality, 2),
                HostLevel_2mar = movAvgRightAligned(HostLevel, 2),
                HostLevelFatality_2mar = movAvgRightAligned(HostLevelFatality, 2),
                HostLevelFatality_2mar.5point = movAvgRightAligned(HostLevelFatality.5point, 2),
                sevvio_2mar = movAvgRightAligned(sevvio, 2),
                cenvio_2mar = movAvgRightAligned(cenvio, 2)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(ccode1, ccode2, year)

col_drops <-
  c("abbrev1", "abbrev2", "ISOShNm1", "ISOShNm2", "country1", "country2", "state_1", "state_2")

mid_and_icb_dyad_yr <-
  mid_and_icb_dyad_yr[, !(names(mid_and_icb_dyad_yr) %in% col_drops)]

test_rows <- 
  mid_and_icb_dyad_yr$ccode1 == 2 & mid_and_icb_dyad_yr$ccode2 == 365
ggplot(mid_and_icb_dyad_yr[test_rows,],  aes(y=HostLevel_5mar, x=year)) +
  geom_line()

ggplot(mid_and_icb_dyad_yr[test_rows,]) +
  geom_line(aes(y=HostLevel_5mar, x=year), colour = 'red') + 
  geom_line(aes(y=HostLevelFatality_5mar, x=year), colour = 'blue') + 
  theme_bw()

ggplot(mid_and_icb_dyad_yr[test_rows,]) +
  geom_line(aes(y=HostLevelFatality_5mar.5point, x=year), colour = 'red') + 
  geom_line(aes(y=HostLevelFatality_5mar, x=year), colour = 'blue') + 
  theme_bw()

## Add State abbrevations
getName <- function(x,y) {
  if (is.na(x)) return(y)
  else return(x)
}
merged_code$name <- mapply(getName, merged_code$name.mid, merged_code$name.icb)

mid_and_icb_dyad_yr$abbrev1 <- merged_code$name[match(mid_and_icb_dyad_yr$ccode1, merged_code$code)]
mid_and_icb_dyad_yr$abbrev2 <- merged_code$name[match(mid_and_icb_dyad_yr$ccode2, merged_code$code)]

save(mid_and_icb_dyad_yr, file = "mid_and_icb_dyad_yr.RData")
# write.csv(mid_and_icb_dyad_yr, file = "mid_and_icb_dyad_yr.csv")
