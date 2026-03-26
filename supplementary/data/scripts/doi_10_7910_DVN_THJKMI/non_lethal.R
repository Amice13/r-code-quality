## non-lethal estimates

# this data is from Votar Entre Balas


# Reelection Reform and Removing Rival in Mexico

# Adee Weller 

rm(list=ls())


# packages
pkgs <- c("tmap", "dplyr", "plm", "fixest", "kableExtra", "stargazer", "ggplot2", "modelsummary", "ggthemes", "did", "fwildclusterboot", "bacondecomp", "spdep", "xtable", "broom", "purrr", "lfe", "did2s", "didimputation", "glue", "MASS", "sjPlot", "lmtest", "gridExtra", "rddtools", "rdrobust", "rdd", "rddensity", "extrafont", "readr", "foreign")

for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
  lapply(pkg, library, character.only=T )
}

# For user specification. Ensure that subfiles are appropriately set up.
setwd("")

# load data
panel <- read.csv('data\\stacked_data.csv')

cvp <- read.csv('data\\political_violence.csv')

#### How well does my measure correlate with CVP data? ####


head(cvp)
head(panel)

# get the same timeframes to compare

class(cvp$fecha)

cvp$year <- format(as.Date(cvp$fecha, format = "%m/%d/%Y"), "%Y")
cvp$fecha <- as.Date(cvp$fecha, format = "%m/%d/%Y")

head(cvp$fecha)
head(cvp$year)

# date range
min(cvp$fecha)
max(cvp$fecha)

panel2 <- subset(panel, panel$Year > 2017)
cvp2 <- subset(cvp, as.Date(fecha, format = "%m/%d/%Y") < as.Date("2022-07-01") & as.Date(fecha, format = "%m/%d/%Y") > as.Date("2017-12-30"))

head(cvp2)
dim(cvp2)

View(cvp2)

# get only politicians

unique(cvp2$cat_puesto1)

cvp2 <- subset(cvp2, 
  cvp2$cat_puesto1 == "Pre-candidato(a)" | 
  cvp2$cat_puesto1 == "Ex-Alcalde(sa)" | 
  cvp2$cat_puesto1 == "Ex-Candidato(a)" | 
  cvp2$cat_puesto1 == "Candidato(a)" | 
  cvp2$cat_puesto1 == "Alcalde(sa)" | 
  cvp2$cat_puesto1 == "Ex-PreCandidato(a)"
  )


unique(cvp2$tipo_evento)

cvp2$fatal <- ifelse(cvp2$tipo_evento == "Asesinato", 1, 0)
cvp2$non_fatal <- ifelse(cvp2$tipo_evento == "Ataque armado" | cvp2$tipo_evento == "Amenaza" | cvp2$tipo_evento == "Atentado", 1, 0)



# get the count per municipality bin for cvp data

Bin <- 0

cvp2$year <- as.numeric(cvp2$year)

for (i in 1:length(cvp2$year)){
  if (cvp2$year[i] == 2018 | cvp2$year[i] == 2019 |cvp2$year[i] == 2020) {
    Bin[i] <- 3
  } else if (cvp2$year[i] == 2021 | cvp2$year[i] == 2022) {
    Bin[i] <- 4
  }
}

cvp2$Bin <- Bin

cvp_binned <- cvp2 %>%
  group_by(Bin, cve_edoINEGI, cve_munINEGI) %>%
    summarise(fatal_bin = sum(fatal),
              non_fatal_bin = sum(non_fatal))

head(cvp_binned)


# match on ADM2_PCODE
cvp_binned$ADM2_PCODE <- sprintf("%03d", cvp_binned$cve_edoINEGI)

cvp_binned$ADM2_PCODE <- as.integer(paste0(cvp_binned$cve_munINEGI, cvp_binned$ADM2_PCODE, sep = ''))

head(cvp_binned)

cvp_binned <- cvp_binned[,c(1,4:6)]

joined_limited <- left_join(panel2, cvp_binned, by = c("ADM2_PCODE", "Bin") )

dim(joined_limited)
dim(panel2)

names(joined_limited)

table(joined_limited$Bin_attacks2)
table(joined_limited$fatal_bin)

deaths <- lm(Bin_attacks2 ~ fatal_bin, joined_limited)

summary(deaths)

joined_limited$Bin_attacks2_bi <- ifelse(joined_limited$Bin_attacks2 >0 , 1, 0)
joined_limited$fatal_bin_bi <- ifelse(joined_limited$fatal_bin >0 , 1, 0)

deaths2 <- lm(Bin_attacks2_bi ~ fatal_bin_bi, joined_limited)

summary(deaths2)

## there are loads more observations in my data cause they are different types



#### How does fatal attacks compare to non-fatal ones in the CVP data? ####


# for cvp2

cvp_binned <- cvp_binned %>%
  mutate(prop_non_fatal = ifelse(fatal_bin + non_fatal_bin == 0, NA, non_fatal_bin / (fatal_bin + non_fatal_bin)))

cvp_binned3 <- subset(cvp_binned, cvp_binned$Bin == 3)
cvp_binned4 <- subset(cvp_binned, cvp_binned$Bin == 4)

mean(cvp_binned$prop_non_fatal, na.rm = TRUE)
mean(cvp_binned3$prop_non_fatal, na.rm = TRUE)
mean(cvp_binned4$prop_non_fatal, na.rm = TRUE)

mean(cvp_binned$fatal_bin, na.rm = TRUE)

mean(cvp_binned$non_fatal_bin, na.rm = TRUE)

sum(cvp_binned$fatal_bin)
sum(cvp_binned$non_fatal_bin)

sum(panel2$Bin_attacks2)

