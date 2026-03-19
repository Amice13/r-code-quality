# ---------------------------------------------------------------------------------------
# THE COMPARATIVE LEGISLATORS DATABASE
# Sascha Göbel and Simon Munzert
# Script for validation and corresponding figures
# April 2020
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
    './data/diff_party_ger.xlsx'
    './data/diff_sex_ger.xlsx'
    './data/diff_birth_ger.xlsx'
    './data/mismatched_session_ger.xlsx'
    './data/diff_birth_usa.xlsx'
    './data/diff_constituency_usa.xlsx'
    './data/diff_died_usa.xlsx'
    './data/diff_party_usa.xlsx'
    './data/mismatched_session_usa.xlsx'
    './data/verification_add.xlsx'
    ")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
    './figures/missings.pdf', 
    './figures/validation.pdf'
    './figures/validation_2.pdf'
    'figures/validation_3.pdf'
    ")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
    Line 54 - FIGURE A2 
    Line 428 - FIGURE A3
    Line 747 - FIGURE A4
    Line 881 - FIGURE A5
    Line 1047 - DATA FOR TABLE A1
    ")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("comparative-legislators-database")

# install and load packages -------------------------------------------------------------
source("./code/paper-packages.R")
source("./code/paper-functions.R")


#### FIGURE A2 ==========================================================================

# prepare data --------------------------------------------------------------------------
missings <- data.frame(country = rep(c("Austria", "Canada", "Czech Republic",
                                       "France", "Germany", "Ireland",
                                       "Scotland", "Spain", "United Kingdom",
                                       "United States (House)",
                                       "United States (Senate)"), each = 11),
                       variables = rep(c("Sex", "Ethnicity", "Religion",
                                         "Birth", "Birthplace", "Party",
                                         "Constituency", "Service", "Portraits",
                                         "Professions", "Offices"), 11),
                       share = NA
)

austria_core <- get_core(legislature = "aut")
canada_core <- get_core(legislature = "can")
czech_core <- get_core(legislature = "cze")
france_core <- get_core(legislature = "fra")
germany_core <- get_core(legislature = "deu")
ireland_core <- get_core(legislature = "irl")
scotland_core <- get_core(legislature = "sco")
spain_core <- get_core(legislature = "esp")
uk_core <- get_core(legislature = "gbr")
usa_house_core <- get_core(legislature = "usa_house")
usa_senate_core <- get_core(legislature = "usa_senate")

missings[missings$country == "Austria",]$share[1] <- length(which(is.na(austria_core$sex)))/
  length(unique(austria_core$pageid))
missings[missings$country == "Canada",]$share[1] <- length(which(is.na(canada_core$sex)))/
  length(unique(canada_core$pageid))
missings[missings$country == "Czech Republic",]$share[1] <- length(which(is.na(czech_core$sex)))/
  length(unique(czech_core$pageid))
missings[missings$country == "France",]$share[1] <- length(which(is.na(france_core$sex)))/
  length(unique(france_core$pageid))
missings[missings$country == "Germany",]$share[1] <- length(which(is.na(germany_core$sex)))/
  length(unique(germany_core$pageid))
missings[missings$country == "Ireland",]$share[1] <- length(which(is.na(ireland_core$sex)))/
  length(unique(ireland_core$pageid))
missings[missings$country == "Scotland",]$share[1] <- length(which(is.na(scotland_core$sex)))/
  length(unique(scotland_core$pageid))
missings[missings$country == "Spain",]$share[1] <- length(which(is.na(spain_core$sex)))/
  length(unique(spain_core$pageid))
missings[missings$country == "United Kingdom",]$share[1] <- length(which(is.na(uk_core$sex)))/
  length(unique(uk_core$pageid))
missings[missings$country == "United States (House)",]$share[1] <- length(which(is.na(usa_house_core$sex)))/
  length(unique(usa_house_core$pageid))
missings[missings$country == "United States (Senate)",]$share[1] <- length(which(is.na(usa_senate_core$sex)))/
  length(unique(usa_senate_core$pageid))

missings[missings$country == "Austria",]$share[2] <- length(which(is.na(austria_core$ethnicity)))/
  length(unique(austria_core$pageid))
missings[missings$country == "Canada",]$share[2] <- length(which(is.na(canada_core$ethnicity)))/
  length(unique(canada_core$pageid))
missings[missings$country == "Czech Republic",]$share[2] <- length(which(is.na(czech_core$ethnicity)))/
  length(unique(czech_core$pageid))
missings[missings$country == "France",]$share[2] <- length(which(is.na(france_core$ethnicity)))/
  length(unique(france_core$pageid))
missings[missings$country == "Germany",]$share[2] <- length(which(is.na(germany_core$ethnicity)))/
  length(unique(germany_core$pageid))
missings[missings$country == "Ireland",]$share[2] <- length(which(is.na(ireland_core$ethnicity)))/
  length(unique(ireland_core$pageid))
missings[missings$country == "Scotland",]$share[2] <- length(which(is.na(scotland_core$ethnicity)))/
  length(unique(scotland_core$pageid))
missings[missings$country == "Spain",]$share[2] <- 1
missings[missings$country == "United Kingdom",]$share[2] <- length(which(is.na(uk_core$ethnicity)))/
  length(unique(uk_core$pageid))
missings[missings$country == "United States (House)",]$share[2] <- length(which(is.na(usa_house_core$ethnicity)))/
  length(unique(usa_house_core$pageid))
missings[missings$country == "United States (Senate)",]$share[2] <- length(which(is.na(usa_senate_core$ethnicity)))/
  length(unique(usa_senate_core$pageid))

missings[missings$country == "Austria",]$share[3] <- length(which(is.na(austria_core$religion)))/
  length(unique(austria_core$pageid))
missings[missings$country == "Canada",]$share[3] <- length(which(is.na(canada_core$religion)))/
  length(unique(canada_core$pageid))
missings[missings$country == "Czech Republic",]$share[3] <- length(which(is.na(czech_core$religion)))/
  length(unique(czech_core$pageid))
missings[missings$country == "France",]$share[3] <- length(which(is.na(france_core$religion)))/
  length(unique(france_core$pageid))
missings[missings$country == "Germany",]$share[3] <- length(which(is.na(germany_core$religion)))/
  length(unique(germany_core$pageid))
missings[missings$country == "Ireland",]$share[3] <- length(which(is.na(ireland_core$religion)))/
  length(unique(ireland_core$pageid))
missings[missings$country == "Scotland",]$share[3] <- length(which(is.na(scotland_core$religion)))/
  length(unique(scotland_core$pageid))
missings[missings$country == "Spain",]$share[3] <- length(which(is.na(spain_core$religion)))/
  length(unique(spain_core$pageid))
missings[missings$country == "United Kingdom",]$share[3] <- length(which(is.na(uk_core$religion)))/
  length(unique(uk_core$pageid))
missings[missings$country == "United States (House)",]$share[3] <- length(which(is.na(usa_house_core$religion)))/
  length(unique(usa_house_core$pageid))
missings[missings$country == "United States (Senate)",]$share[3] <- length(which(is.na(usa_senate_core$religion)))/
  length(unique(usa_senate_core$pageid))

missings[missings$country == "Austria",]$share[4] <- length(which(is.na(austria_core$birth)))/
  length(unique(austria_core$pageid))
missings[missings$country == "Canada",]$share[4] <- length(which(is.na(canada_core$birth)))/
  length(unique(canada_core$pageid))
missings[missings$country == "Czech Republic",]$share[4] <- length(which(is.na(czech_core$birth)))/
  length(unique(czech_core$pageid))
missings[missings$country == "France",]$share[4] <- length(which(is.na(france_core$birth)))/
  length(unique(france_core$pageid))
missings[missings$country == "Germany",]$share[4] <- length(which(is.na(germany_core$birth)))/
  length(unique(germany_core$pageid))
missings[missings$country == "Ireland",]$share[4] <- length(which(is.na(ireland_core$birth)))/
  length(unique(ireland_core$pageid))
missings[missings$country == "Scotland",]$share[4] <- length(which(is.na(scotland_core$birth)))/
  length(unique(scotland_core$pageid))
missings[missings$country == "Spain",]$share[4] <- length(which(is.na(spain_core$birth)))/
  length(unique(spain_core$pageid))
missings[missings$country == "United Kingdom",]$share[4] <- length(which(is.na(uk_core$birth)))/
  length(unique(uk_core$pageid))
missings[missings$country == "United States (House)",]$share[4] <- length(which(is.na(usa_house_core$birth)))/
  length(unique(usa_house_core$pageid))
missings[missings$country == "United States (Senate)",]$share[4] <- length(which(is.na(usa_senate_core$birth)))/
  length(unique(usa_senate_core$pageid))

missings[missings$country == "Austria",]$share[5] <- length(which(is.na(austria_core$birthplace)))/
  length(unique(austria_core$pageid))
missings[missings$country == "Canada",]$share[5] <- length(which(is.na(canada_core$birthplace)))/
  length(unique(canada_core$pageid))
missings[missings$country == "Czech Republic",]$share[5] <- length(which(is.na(czech_core$birthplace)))/
  length(unique(czech_core$pageid))
missings[missings$country == "France",]$share[5] <- length(which(is.na(france_core$birthplace)))/
  length(unique(france_core$pageid))
missings[missings$country == "Germany",]$share[5] <- length(which(is.na(germany_core$birthplace)))/
  length(unique(germany_core$pageid))
missings[missings$country == "Ireland",]$share[5] <- length(which(is.na(ireland_core$birthplace)))/
  length(unique(ireland_core$pageid))
missings[missings$country == "Scotland",]$share[5] <- length(which(is.na(scotland_core$birthplace)))/
  length(unique(scotland_core$pageid))
missings[missings$country == "Spain",]$share[5] <- length(which(is.na(spain_core$birthplace)))/
  length(unique(spain_core$pageid))
missings[missings$country == "United Kingdom",]$share[5] <- length(which(is.na(uk_core$birthplace)))/
  length(unique(uk_core$pageid))
missings[missings$country == "United States (House)",]$share[5] <- length(which(is.na(usa_house_core$birthplace)))/
  length(unique(usa_house_core$pageid))
missings[missings$country == "United States (Senate)",]$share[5] <- length(which(is.na(usa_senate_core$birthplace)))/
  length(unique(usa_senate_core$pageid))

austria_political <- get_political(legislature = "aut")
canada_political <- get_political(legislature = "can")
czech_political <- get_political(legislature = "cze")
france_political <- get_political(legislature = "fra")
germany_political <- get_political(legislature = "deu")
ireland_political <- get_political(legislature = "irl")
scotland_political <- get_political(legislature = "sco")
spain_political <- get_political(legislature = "esp")
uk_political <- get_political(legislature = "gbr")
usa_house_political <- get_political(legislature = "usa_house")
usa_senate_political <- get_political(legislature = "usa_senate")

missings[missings$country == "Austria",]$share[6] <- length(which(is.na(austria_political$party)))/
  nrow(austria_political)
missings[missings$country == "Canada",]$share[6] <- length(which(is.na(canada_political$party)))/
  nrow(canada_political)
missings[missings$country == "Czech Republic",]$share[6] <- length(which(is.na(czech_political$party)))/
  nrow(czech_political)
missings[missings$country == "France",]$share[6] <- length(which(is.na(france_political$party)))/
  nrow(france_political)
missings[missings$country == "Germany",]$share[6] <- length(which(is.na(germany_political$party)))/
  nrow(germany_political)
missings[missings$country == "Ireland",]$share[6] <- length(which(is.na(ireland_political$party)))/
  nrow(ireland_political)
missings[missings$country == "Scotland",]$share[6] <- length(which(is.na(scotland_political$party)))/
  nrow(scotland_political)
missings[missings$country == "Spain",]$share[6] <- length(which(is.na(spain_political$party)))/
  nrow(spain_political)
missings[missings$country == "United Kingdom",]$share[6] <- length(which(is.na(uk_political$party)))/
  nrow(uk_political)
missings[missings$country == "United States (House)",]$share[6] <- length(which(is.na(usa_house_political$party)))/
  nrow(usa_house_political)
missings[missings$country == "United States (Senate)",]$share[6] <- length(which(is.na(usa_senate_political$party)))/
  nrow(usa_senate_political)

missings[missings$country == "Austria",]$share[7] <- length(which(is.na(austria_political$constituency)))/
  nrow(austria_political)
missings[missings$country == "Canada",]$share[7] <- length(which(is.na(canada_political$constituency)))/
  nrow(canada_political)
missings[missings$country == "Czech Republic",]$share[7] <- length(which(is.na(czech_political$constituency)))/
  nrow(czech_political)
missings[missings$country == "France",]$share[7] <- length(which(is.na(france_political$constituency)))/
  nrow(france_political)
missings[missings$country == "Germany",]$share[7] <- length(which(is.na(germany_political$constituency)))/
  nrow(germany_political)
missings[missings$country == "Ireland",]$share[7] <- length(which(is.na(ireland_political$constituency)))/
  nrow(ireland_political)
missings[missings$country == "Scotland",]$share[7] <- length(which(is.na(scotland_political$constituency)))/
  nrow(scotland_political)
missings[missings$country == "Spain",]$share[7] <- length(which(is.na(spain_political$constituency)))/
  nrow(spain_political)
missings[missings$country == "United Kingdom",]$share[7] <- length(which(is.na(uk_political$constituency)))/
  nrow(uk_political)
missings[missings$country == "United States (House)",]$share[7] <- length(which(is.na(usa_house_political$constituency)))/
  nrow(usa_house_political)
missings[missings$country == "United States (Senate)",]$share[7] <- length(which(is.na(usa_senate_political$constituency)))/
  nrow(usa_senate_political)

missings[missings$country == "Austria",]$share[8] <- length(which(is.na(austria_political$service)))/
  nrow(austria_political)
missings[missings$country == "Canada",]$share[8] <- length(which(is.na(canada_political$service)))/
  nrow(canada_political)
missings[missings$country == "Czech Republic",]$share[8] <- length(which(is.na(czech_political$service)))/
  nrow(czech_political)
missings[missings$country == "France",]$share[8] <- length(which(is.na(france_political$service)))/
  nrow(france_political)
missings[missings$country == "Germany",]$share[8] <- length(which(is.na(germany_political$service)))/
  nrow(germany_political)
missings[missings$country == "Ireland",]$share[8] <- length(which(is.na(ireland_political$service)))/
  nrow(ireland_political)
missings[missings$country == "Scotland",]$share[8] <- length(which(is.na(scotland_political$service)))/
  nrow(scotland_political)
missings[missings$country == "Spain",]$share[8] <- length(which(is.na(spain_political$service)))/
  nrow(spain_political)
missings[missings$country == "United Kingdom",]$share[8] <- length(which(is.na(uk_political$service)))/
  nrow(uk_political)
missings[missings$country == "United States (House)",]$share[8] <- length(which(is.na(usa_house_political$service)))/
  nrow(usa_house_political)
missings[missings$country == "United States (Senate)",]$share[8] <- length(which(is.na(usa_senate_political$service)))/
  nrow(usa_senate_political)

austria_portrait <- get_portrait(legislature = "aut")
canada_portrait <- get_portrait(legislature = "can")
czech_portrait <- get_portrait(legislature = "cze")
france_portrait <- get_portrait(legislature = "fra")
germany_portrait <- get_portrait(legislature = "deu")
ireland_portrait <- get_portrait(legislature = "irl")
scotland_portrait <- get_portrait(legislature = "sco")
spain_portrait <- get_portrait(legislature = "esp")
uk_portrait <- get_portrait(legislature = "gbr")
usa_house_portrait <- get_portrait(legislature = "usa_house")
usa_senate_portrait <- get_portrait(legislature = "usa_senate")

missings[missings$country == "Austria",]$share[9] <- 1-(nrow(austria_portrait)/
                                                          length(unique(austria_core$pageid)))
missings[missings$country == "Canada",]$share[9] <- 1-(nrow(canada_portrait)/
                                                         length(unique(canada_core$pageid)))
missings[missings$country == "Czech Republic",]$share[9] <- 1-(nrow(czech_portrait)/
                                                                 length(unique(czech_core$pageid)))
missings[missings$country == "France",]$share[9] <- 1-(nrow(france_portrait)/
                                                         length(unique(france_core$pageid)))
missings[missings$country == "Germany",]$share[9] <- 1-(nrow(germany_portrait)/
                                                          length(unique(germany_core$pageid)))
missings[missings$country == "Ireland",]$share[9] <- 1-(nrow(ireland_portrait)/
                                                          length(unique(ireland_core$pageid)))
missings[missings$country == "Scotland",]$share[9] <- 1-(nrow(scotland_portrait)/
                                                           length(unique(scotland_core$pageid)))
missings[missings$country == "Spain",]$share[9] <- 1-(nrow(spain_portrait)/
                                                        length(unique(spain_core$pageid)))
missings[missings$country == "United Kingdom",]$share[9] <- 1-(nrow(uk_portrait)/
                                                                 length(unique(uk_core$pageid)))
missings[missings$country == "United States (House)",]$share[9] <- 1-(nrow(usa_house_portrait)/
                                                                        length(unique(usa_house_core$pageid)))
missings[missings$country == "United States (Senate)",]$share[9] <- 1-(nrow(usa_senate_portrait)/
                                                                         length(unique(usa_senate_core$pageid)))
austria_professions <- get_profession(legislature = "aut")
canada_professions <- get_profession(legislature = "can")
czech_professions <- get_profession(legislature = "cze")
france_professions <- get_profession(legislature = "fra")
germany_professions <- get_profession(legislature = "deu")
ireland_professions <- get_profession(legislature = "irl")
scotland_professions <- get_profession(legislature = "sco")
spain_professions <- get_profession(legislature = "esp")
uk_professions <- get_profession(legislature = "gbr")
usa_house_professions <- get_profession(legislature = "usa_house")
usa_senate_professions <- get_profession(legislature = "usa_senate")

missings[missings$country == "Austria",]$share[10] <- 1-(nrow(austria_professions)/
                                                           length(unique(austria_core$pageid)))
missings[missings$country == "Canada",]$share[10] <- 1-(nrow(canada_professions)/
                                                          length(unique(canada_core$pageid)))
missings[missings$country == "Czech Republic",]$share[10] <- 1-(nrow(czech_professions)/
                                                                  length(unique(czech_core$pageid)))
missings[missings$country == "France",]$share[10] <- 1-(nrow(france_professions)/
                                                          length(unique(france_core$pageid)))
missings[missings$country == "Germany",]$share[10] <- 1-(nrow(germany_professions)/
                                                           length(unique(germany_core$pageid)))
missings[missings$country == "Ireland",]$share[10] <- 1-(nrow(ireland_professions)/
                                                           length(unique(ireland_core$pageid)))
missings[missings$country == "Scotland",]$share[10] <- 1-(nrow(scotland_professions)/
                                                            length(unique(scotland_core$pageid)))
missings[missings$country == "Spain",]$share[10] <- 1-(nrow(spain_professions)/
                                                         length(unique(spain_core$pageid)))
missings[missings$country == "United Kingdom",]$share[10] <- 1-(nrow(uk_professions)/
                                                                  length(unique(uk_core$pageid)))
missings[missings$country == "United States (House)",]$share[10] <- 1-(nrow(usa_house_professions)/
                                                                         length(unique(usa_house_core$pageid)))
missings[missings$country == "United States (Senate)",]$share[10] <- 1-(nrow(usa_senate_professions)/
                                                                          length(unique(usa_senate_core$pageid)))

austria_offices <- get_office(legislature = "aut")
canada_offices <- get_office(legislature = "can")
czech_offices <- get_office(legislature = "cze")
france_offices <- get_office(legislature = "fra")
germany_offices <- get_office(legislature = "deu")
ireland_offices <- get_office(legislature = "irl")
scotland_offices <- get_office(legislature = "sco")
spain_offices <- get_office(legislature = "esp")
uk_offices <- get_office(legislature = "gbr")
usa_house_offices <- get_office(legislature = "usa_house")
usa_senate_offices <- get_office(legislature = "usa_senate")

missings[missings$country == "Austria",]$share[11] <- 1-(nrow(austria_offices)/
                                                           length(unique(austria_core$pageid)))
missings[missings$country == "Canada",]$share[11] <- 1-(nrow(canada_offices)/
                                                          length(unique(canada_core$pageid)))
missings[missings$country == "Czech Republic",]$share[11] <- 1-(nrow(czech_offices)/
                                                                  length(unique(czech_core$pageid)))
missings[missings$country == "France",]$share[11] <- 1-(nrow(france_offices)/
                                                          length(unique(france_core$pageid)))
missings[missings$country == "Germany",]$share[11] <- 1-(nrow(germany_offices)/
                                                           length(unique(germany_core$pageid)))
missings[missings$country == "Ireland",]$share[11] <- 1-(nrow(ireland_offices)/
                                                           length(unique(ireland_core$pageid)))
missings[missings$country == "Scotland",]$share[11] <- 1-(nrow(scotland_offices)/
                                                            length(unique(scotland_core$pageid)))
missings[missings$country == "Spain",]$share[11] <- 1-(nrow(spain_offices)/
                                                         length(unique(spain_core$pageid)))
missings[missings$country == "United Kingdom",]$share[11] <- 1-(nrow(uk_offices)/
                                                                  length(unique(uk_core$pageid)))
missings[missings$country == "United States (House)",]$share[11] <- 1-(nrow(usa_house_offices)/
                                                                         length(unique(usa_house_core$pageid)))
missings[missings$country == "United States (Senate)",]$share[11] <- 1-(nrow(usa_senate_offices)/
                                                                          length(unique(usa_senate_core$pageid)))

missings$variables <- factor(missings$variables, levels = c("Sex", "Ethnicity", "Religion",
                                                            "Birth", "Birthplace", "Party",
                                                            "Constituency", "Service", "Portraits",
                                                            "Professions", "Offices"))
missings$country <- factor(missings$country, levels = c("Austria", "Canada", "Czech Republic",
                                                        "France", "Germany", "Ireland",
                                                        "Scotland", "Spain", "United Kingdom",
                                                        "United States (House)",
                                                        "United States (Senate)"),
                           labels = c("Austria", "Canada", "Czech\nRepublic",
                                      "France", "Germany", "Ireland",
                                      "Scotland", "Spain", "United\nKingdom",
                                      "United\nStates\n(House)",
                                      "United\nStates\n(Senate)"))

# prepare - figure ----------------------------------------------------------------------
missings_plot <- ggplot(data = missings) +
  # create a new plot
  geom_tile(aes(x = variables, y = reorder(country, desc(country)), fill = share), color = "gray40", size = 0.5) +
  scale_fill_gradient(low = "white", high = "black", labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
                      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0,1)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_blank(),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 16, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 16, color = "black"),
        legend.title = element_text(size=13),
        legend.text = element_text(size=13),
        legend.position = "right") +
  # specify axis title size and margin
  labs(x = "Variables", y = "", fill = "Percentage\nmissing") +
  # specify axis labels
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) #+
#  coord_fixed()
ggsave("./figures/missings.pdf", missings_plot, width = 14, height = 12, dpi = 1200, device = cairo_pdf)


#### FIGURE A3 ==========================================================================

# prepare data --------------------------------------------------------------------------
# collect revision histories
aut_hist <- get_history("aut")
can_hist <- get_history("can")
cze_hist <- get_history("cze")
fra_hist <- get_history("fra")
deu_hist <- get_history("deu")
irl_hist <- get_history("irl")
sco_hist <- get_history("sco")
esp_hist <- get_history("esp")
gbr_hist <- get_history("gbr")
usa_house_hist <- get_history("usa_house")
usa_senate_hist <- get_history("usa_senate")
usa_hist <- rbind(usa_house_hist, usa_senate_hist) %>%
  distinct(revid, .keep_all = TRUE)
# remove bots
aut_hist <- aut_hist[!str_detect(aut_hist$user, "^Bot|^bot|.+Bot|bot$"),]
can_hist <- can_hist[!str_detect(can_hist$user, "^Bot|^bot|.+Bot|bot$"),]
cze_hist <- cze_hist[!str_detect(cze_hist$user, "^Bot|^bot|.+Bot|bot$"),]
fra_hist <- fra_hist[!str_detect(fra_hist$user, "^Bot|^bot|.+Bot|bot$"),]
deu_hist <- deu_hist[!str_detect(deu_hist$user, "^Bot|^bot|.+Bot|bot$"),]
irl_hist <- irl_hist[!str_detect(irl_hist$user, "^Bot|^bot|.+Bot|bot$"),]
sco_hist <- sco_hist[!str_detect(sco_hist$user, "^Bot|^bot|.+Bot|bot$"),]
esp_hist <- esp_hist[!str_detect(esp_hist$user, "^Bot|^bot|.+Bot|bot$"),]
gbr_hist <- gbr_hist[!str_detect(gbr_hist$user, "^Bot|^bot|.+Bot|bot$"),]
usa_hist <- usa_hist[!str_detect(usa_hist$user, "^Bot|^bot|.+Bot|bot$"),]
# remove anonymous users
aut_hist <- filter(aut_hist, userid != 0)
can_hist <- filter(can_hist, userid != 0)
cze_hist <- filter(cze_hist, userid != 0)
fra_hist <- filter(fra_hist, userid != 0)
deu_hist <- filter(deu_hist, userid != 0)
irl_hist <- filter(irl_hist, userid != 0)
sco_hist <- filter(sco_hist, userid != 0)
esp_hist <- filter(esp_hist, userid != 0)
gbr_hist <- filter(gbr_hist, userid != 0)
usa_hist <- filter(usa_hist, userid != 0)
# number of pages edited by user
aut_num_pages <- aut_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(n = n())
can_num_pages <- can_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(n = n())
cze_num_pages <- cze_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(n = n())
fra_num_pages <- fra_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(n = n())
deu_num_pages <- deu_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(n = n())
irl_num_pages <- irl_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(n = n())
sco_num_pages <- sco_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(n = n())
esp_num_pages <- esp_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(n = n())
gbr_num_pages <- gbr_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(n = n())
usa_num_pages <- usa_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(n = n())
# mean number of page edits per user
aut_num_edits <- aut_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(mean = mean(n))
can_num_edits <- can_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(mean = mean(n))
cze_num_edits <- cze_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(mean = mean(n))
fra_num_edits <- fra_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(mean = mean(n))
deu_num_edits <- deu_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(mean = mean(n))
irl_num_edits <- irl_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(mean = mean(n))
sco_num_edits <- sco_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(mean = mean(n))
esp_num_edits <- esp_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(mean = mean(n))
gbr_num_edits <- gbr_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(mean = mean(n))
usa_num_edits <- usa_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  group_by(userid) %>%
  summarise(mean = mean(n))
# bind together
aut_hist <- left_join(x = aut_hist, y = aut_num_pages, by = "userid")
aut_hist <- left_join(x = aut_hist, y = aut_num_edits, by = c("userid"))
can_hist <- left_join(x = can_hist, y = can_num_pages, by = "userid")
can_hist <- left_join(x = can_hist, y = can_num_edits, by = c("userid"))
cze_hist <- left_join(x = cze_hist, y = cze_num_pages, by = "userid")
cze_hist <- left_join(x = cze_hist, y = cze_num_edits, by = c("userid"))
fra_hist <- left_join(x = fra_hist, y = fra_num_pages, by = "userid")
fra_hist <- left_join(x = fra_hist, y = fra_num_edits, by = c("userid"))
deu_hist <- left_join(x = deu_hist, y = deu_num_pages, by = "userid")
deu_hist <- left_join(x = deu_hist, y = deu_num_edits, by = c("userid"))
irl_hist <- left_join(x = irl_hist, y = irl_num_pages, by = "userid")
irl_hist <- left_join(x = irl_hist, y = irl_num_edits, by = c("userid"))
sco_hist <- left_join(x = sco_hist, y = sco_num_pages, by = "userid")
sco_hist <- left_join(x = sco_hist, y = sco_num_edits, by = c("userid"))
esp_hist <- left_join(x = esp_hist, y = esp_num_pages, by = "userid")
esp_hist <- left_join(x = esp_hist, y = esp_num_edits, by = c("userid"))
gbr_hist <- left_join(x = gbr_hist, y = gbr_num_pages, by = "userid")
gbr_hist <- left_join(x = gbr_hist, y = gbr_num_edits, by = c("userid"))
usa_hist <- left_join(x = usa_hist, y = usa_num_pages, by = "userid")
usa_hist <- left_join(x = usa_hist, y = usa_num_edits, by = c("userid"))
# compute log editor experience
aut_hist$exp <- log(aut_hist$n*aut_hist$mean)
can_hist$exp <- log(can_hist$n*can_hist$mean)
cze_hist$exp <- log(cze_hist$n*cze_hist$mean)
fra_hist$exp <- log(fra_hist$n*fra_hist$mean)
deu_hist$exp <- log(deu_hist$n*deu_hist$mean)
irl_hist$exp <- log(irl_hist$n*irl_hist$mean)
sco_hist$exp <- log(sco_hist$n*sco_hist$mean)
esp_hist$exp <- log(esp_hist$n*esp_hist$mean)
gbr_hist$exp <- log(gbr_hist$n*gbr_hist$mean)
usa_hist$exp <- log(usa_hist$n*usa_hist$mean)
# summarize mean log editor experience
aut_exp <- aut_hist %>%
  group_by(pageid) %>%
  summarise(mean_log_exp = mean(exp))
can_exp <- can_hist %>%
  group_by(pageid) %>%
  summarise(mean_log_exp = mean(exp))
cze_exp <- cze_hist %>%
  group_by(pageid) %>%
  summarise(mean_log_exp = mean(exp))
fra_exp <- fra_hist %>%
  group_by(pageid) %>%
  summarise(mean_log_exp = mean(exp))
deu_exp <- deu_hist %>%
  group_by(pageid) %>%
  summarise(mean_log_exp = mean(exp))
irl_exp <- irl_hist %>%
  group_by(pageid) %>%
  summarise(mean_log_exp = mean(exp))
sco_exp <- sco_hist %>%
  group_by(pageid) %>%
  summarise(mean_log_exp = mean(exp))
esp_exp <- esp_hist %>%
  group_by(pageid) %>%
  summarise(mean_log_exp = mean(exp))
gbr_exp <- gbr_hist %>%
  group_by(pageid) %>%
  summarise(mean_log_exp = mean(exp))
usa_exp <- usa_hist %>%
  group_by(pageid) %>%
  summarise(mean_log_exp = mean(exp))
# summarize log number of editors by page
aut_eds <- aut_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate(log_eds = log(n))
can_eds <- can_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate(log_eds = log(n))
cze_eds <- cze_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate(log_eds = log(n))
fra_eds <- fra_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate(log_eds = log(n))
deu_eds <- deu_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate(log_eds = log(n))
irl_eds <- irl_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate(log_eds = log(n))
sco_eds <- sco_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate(log_eds = log(n))
esp_eds <- esp_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate(log_eds = log(n))
gbr_eds <- gbr_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate(log_eds = log(n))
usa_eds <- usa_hist %>%
  group_by(pageid, userid) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate(log_eds = log(n))
# bind together and add legislature
aut_plot <- left_join(x = aut_exp, y = aut_eds, by = "pageid") %>%
  mutate(legislature = "Austria")
can_plot <- left_join(x = can_exp, y = can_eds, by = "pageid") %>%
  mutate(legislature = "Canada")
cze_plot <- left_join(x = cze_exp, y = cze_eds, by = "pageid") %>%
  mutate(legislature = "Czech Republic")
fra_plot <- left_join(x = fra_exp, y = fra_eds, by = "pageid") %>%
  mutate(legislature = "France")
deu_plot <- left_join(x = deu_exp, y = deu_eds, by = "pageid") %>%
  mutate(legislature = "Germany")
irl_plot <- left_join(x = irl_exp, y = irl_eds, by = "pageid") %>%
  mutate(legislature = "Ireland")
sco_plot <- left_join(x = sco_exp, y = sco_eds, by = "pageid") %>%
  mutate(legislature = "Scotland")
esp_plot <- left_join(x = esp_exp, y = esp_eds, by = "pageid") %>%
  mutate(legislature = "Spain")
gbr_plot <- left_join(x = gbr_exp, y = gbr_eds, by = "pageid") %>%
  mutate(legislature = "United Kingdom")
usa_plot <- left_join(x = usa_exp, y = usa_eds, by = "pageid") %>%
  mutate(legislature = "United States")
# join all together
val_data <- rbind(aut_plot, can_plot, cze_plot, fra_plot,
                  deu_plot, irl_plot, sco_plot, esp_plot, gbr_plot,
                  usa_plot)

# prepare figure ------------------------------------------------------------------------
val_plot <- ggplot() +
  geom_point(data = val_data, aes(x = log_eds, y = mean_log_exp), shape = 20, alpha = 0.1) +
  geom_smooth(data = val_data, aes(x = log_eds, y = mean_log_exp),
              method = "auto", se = TRUE, color = "gray70",
              size = 0.8, linetype = "solid") +
  facet_wrap(~legislature, ncol = 3) +
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 16),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 16),
        # specify axis title size and margin
        strip.background = element_blank(),
        # remove box around plot titles
        strip.text = element_text(size=13)) +
  # specify plot title size
  labs(x = "log number of editors by legislator", y = "mean log editor experience by legislator") +
  # specify axis labels
  scale_y_continuous(limits = c(0, 12), expand = c(0, 0),
                     breaks = seq(2,10,2)) +
  scale_x_continuous(limits = c(0, 9), expand = c(0, 0),
                     breaks = seq(1.5,7.5,1.5))

ggsave("./figures/validation.pdf", val_plot, width = 10, height = 14.25, dpi = 1200, device = cairo_pdf)


# FIGURE A4 =============================================================================

# prepare data --------------------------------------------------------------------------
# germany
deu <- semi_join(x = get_core(legislature = "deu"),
                 y = filter(get_ids(legislature = "deu"), !is.na(btvote)),
                 by = "wikidataid")
deu_pol <- left_join(x = get_core(legislature = "deu"),
                     y = filter(get_political("deu"), session != 18 & session != 19),
                     by = "pageid")
deu_pol <- semi_join(x = deu_pol,
                     y = filter(get_ids(legislature = "deu"), !is.na(btvote)),
                     by = "wikidataid")
# usa
usa_house <- semi_join(x = get_core(legislature = "usa_house"),
                       y = filter(get_ids(legislature = "usa_house"), !is.na(bioguide)),
                       by = "wikidataid")
usa_house_pol <- left_join(x = get_core(legislature = "usa_house"),
                           y = filter(get_political("usa_house")),
                           by = "pageid")
usa_house_pol <- semi_join(x = usa_house_pol,
                           y = filter(get_ids(legislature = "usa_house"), !is.na(bioguide)),
                           by = "wikidataid")
# agreement on variables germany
diff_party_ger <- readxl::read_xlsx("./validation-data/diff_party_ger.xlsx")
agreement_party_ger <- 1-(nrow(diff_party_ger)/nrow(deu_pol))
disagreement_party_ger <- (length(which(diff_party_ger$Correct_dataset == "legislatoR"))/
                             nrow(diff_party_ger))
diff_sex_ger <- readxl::read_xlsx("./validation-data/diff_sex_ger.xlsx")
agreement_sex_ger <- 1
disagreement_sex_ger <- 0
diff_birth_ger <- readxl::read_xlsx("./validation-data/diff_birth_ger.xlsx")
diff_birth_ger <- distinct(diff_birth_ger, pageid, .keep_all = TRUE)
agreement_birth_ger <- 1-(nrow(diff_birth_ger)/nrow(deu))
disagreement_birth_ger <- (length(which(diff_birth_ger$Correct_dataset == "legislatoR"))/
                             nrow(diff_birth_ger))
mismatch_session_ger <- readxl::read_xlsx("./validation-data/mismatched_session_ger.xlsx")
agreement_session_ger <- 1-(nrow(mismatch_session_ger)/nrow(deu_pol))
disagreement_session_ger <- (length(which(mismatch_session_ger$Correct_dataset == "legislatoR"))/
                               nrow(mismatch_session_ger))
# agreement on variables usa
diff_birth_usa <- readxl::read_xlsx("./validation-data/diff_birth_usa.xlsx")
diff_birth_usa <- filter(diff_birth_usa, `Correct dataset` != "none" &  `Correct dataset` != "both")
agreement_birth_usa <- 1-(nrow(diff_birth_usa)/nrow(usa_house))
disagreement_birth_usa <- (length(which(diff_birth_usa$`Correct dataset` == "legislatoR"))/
                             nrow(diff_birth_usa))
diff_constituency_usa <- readxl::read_xlsx("./validation-data/diff_constituency_usa.xlsx")
diff_constituency_usa <- filter(diff_constituency_usa, Correct_dataset != "both")
agreement_constituency_usa <- 1-(nrow(diff_constituency_usa)/nrow(usa_house_pol))
disagreement_constituency_usa <- (length(which(diff_constituency_usa$Correct_dataset == "legislatoR"))/
                                    nrow(diff_constituency_usa))
diff_death_usa <- readxl::read_xlsx("./validation-data/diff_died_usa.xlsx")
diff_death_usa <- filter(diff_death_usa, Correct_dataset != "none")
agreement_death_usa <- 1-(nrow(diff_death_usa)/nrow(usa_house))
disagreement_death_usa <- (length(which(diff_death_usa$Correct_dataset == "legislatoR"))/
                             nrow(diff_death_usa))
diff_party_usa <- readxl::read_xlsx("./validation-data/diff_party_usa.xlsx")
diff_party_usa <- filter(diff_party_usa, Correct_dataset != "none" & Correct_dataset != "both")
agreement_party_usa <- 1-(nrow(diff_party_usa)/nrow(usa_house))
disagreement_party_usa <- (length(which(diff_party_usa$Correct_dataset == "legislatoR"))/
                             nrow(diff_party_usa))
mismatched_session_usa <- readxl::read_xlsx("./validation-data/mismatched_session_usa.xlsx")
agreement_session_usa <- 1-(nrow(mismatched_session_usa)/nrow(usa_house))
disagreement_session_usa <- 1

shares <- data.frame(group = c("sex_1", "birth_1", "party_1", "session_1",
                               "sex_2", "birth_2", "party_2", "session_2",
                               "birth_1", "death_1", "party_1", "constituency_1", "session_1",
                               "birth_2", "death_2", "party_2", "constituency_2", "session_2"),
                     fill = c("agreement", "agreement", "agreement", "agreement",
                              "disagreement", "disagreement", "disagreement", "disagreement",
                              "agreement", "agreement", "agreement", "agreement", "agreement",
                              "disagreement", "disagreement", "disagreement", "disagreement", "disagreement"),
                     dataset = c(rep("CLD \u2013 BTVote", 8), rep("CLD \u2013 Voteview", 10)),
                     share = c(agreement_sex_ger, agreement_birth_ger, agreement_party_ger,
                               agreement_session_ger,
                               disagreement_sex_ger, disagreement_birth_ger, disagreement_party_ger,
                               disagreement_session_ger,
                               agreement_birth_usa, agreement_death_usa, agreement_party_usa,
                               agreement_constituency_usa, agreement_session_usa,
                               disagreement_birth_usa, agreement_death_usa, disagreement_party_usa,
                               disagreement_constituency_usa, disagreement_session_usa))
shares$group <- factor(shares$group, levels = c("sex_1", "birth_1", "death_1", "party_1", "constituency_1", "session_1",
                                                "sex_2", "birth_2", "death_2", "party_2", "constituency_2", "session_2"))
shares$fill <- factor(shares$fill, levels = c("agreement", "disagreement"))
shares$dataset <- factor(shares$dataset, levels = c("CLD \u2013 BTVote", "CLD \u2013 Voteview"))

# prepare figure ------------------------------------------------------------------------
validation_2 <- ggplot(shares, aes(group, share, fill = fill)) +
  geom_bar(stat="identity", width = 0.8, colour = "black", size = 0.4) +
  scale_fill_manual(values = c("gray80", "gray60")) +
  facet_wrap(~ dataset, scales = "free_x", nrow = 2) +
  geom_vline(data = filter(shares, dataset == "CLD \u2013 BTVote"), aes(xintercept =  c(4.5)), linetype = "dashed", colour = "gray25") +
  geom_vline(data = filter(shares, dataset == "CLD \u2013 Voteview"), aes(xintercept =  c(5.5)), linetype = "dashed", colour = "gray25") +
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 12, color = "black"),
        # axis.text.x = element_text(angle = 50, hjust = 1),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 14, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 14, color = "black"),
        plot.title = element_text(size = 13, color = "black")) +
  # specify axis title size and margin
  labs(x = "",
       y = "Share") +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),
                     breaks = seq(0.2,0.8,0.2)) +
  scale_x_discrete(labels = c("sex_1" = "Sex", "sex_2" = "Sex", "birth_1" = "Birth", "birth_2" = "Birth",
                              "death_1" = "Death", "death_2" = "Death", "party_1" = "Party", "party_2" = "Party",
                              "constituency_1" = "District", "constituency_2" = "District", "session_1" = "Session",
                              "session_2" = "Session"))
validation_2 <- ggdraw(validation_2) +
  draw_label("Agreement", fontfamily = "CMU Serif",
             x = 0.335, y = 0.037, size = 14) +
  draw_label("Disagreement", fontfamily = "CMU Serif",
             x = 0.765, y = 0.037, size = 14) +
  draw_label("and legislatoR correct", fontfamily = "CMU Serif",
             x = 0.765, y = 0.0135, size = 14)
ggsave("./figures/validation_2.pdf", validation_2, width = 7, height = 9, dpi = 1200, device = cairo_pdf)


# FIGURE A5 =============================================================================

# prepare data --------------------------------------------------------------------------
google_search <- readxl::read_xlsx("./validation-data/verification_add.xlsx")
google_search[google_search == "NA"] <- NA
google_search$session_verify <- ifelse(google_search$person_found == FALSE &
                                         !is.na(google_search$session), 0,
                                       google_search$session_verify)
google_search$sex_verify <- ifelse(google_search$person_found == FALSE &
                                     !is.na(google_search$sex), 0,
                                   google_search$sex_verify)
google_search$ethnicity_verify <- ifelse(google_search$person_found == FALSE &
                                           !is.na(google_search$ethnicity), 0,
                                         google_search$ethnicity_verify)
google_search$religion_verify <- ifelse(google_search$person_found == FALSE &
                                          !is.na(google_search$religion), 0,
                                        google_search$religion_verify)
google_search$birth_verify <- ifelse(google_search$person_found == FALSE &
                                       !is.na(google_search$birth), 0,
                                     google_search$birth_verify)
google_search$death_verify <- ifelse(google_search$person_found == FALSE &
                                       !is.na(google_search$death), 0,
                                     google_search$death_verify)
google_search$birthplace <- ifelse(google_search$person_found == FALSE &
                                     !is.na(google_search$birthplace), 0,
                                   google_search$birthplace_verify)
google_search$deathplace <- ifelse(google_search$person_found == FALSE &
                                     !is.na(google_search$deathplace), 0,
                                   google_search$deathplace_verify)
google_search$constituency <- ifelse(google_search$person_found == FALSE &
                                       !is.na(google_search$constituency), 0,
                                     google_search$constituency_verify)
google_search$service <- ifelse(google_search$person_found == FALSE &
                                  !is.na(google_search$service), 0,
                                google_search$service_verify)
google_search$party_verify <- ifelse(google_search$party == FALSE &
                                       !is.na(google_search$party), 0,
                                     google_search$party_verify)
shares <- data.frame(group = c("correct_1", "incorrect_1", "na_1",
                               "correct_2", "incorrect_2", "na_2",
                               "correct_3", "incorrect_3", "na_3",
                               "correct_4", "incorrect_4", "na_4",
                               "correct_5", "incorrect_5", "na_5",
                               "correct_6", "incorrect_6", "na_6",
                               "correct_7", "incorrect_7", "na_7",
                               "correct_8", "incorrect_8", "na_8",
                               "correct_9", "incorrect_9", "na_9",
                               "correct_10", "incorrect_10", "na_10",
                               "correct_11", "incorrect_11", "na_11"),
                     fill = rep(c("C", "D", "NF"), 11),
                     share = NA)
shares$share[1] <- prop.table(table(google_search$sex_verify))[2]
shares$share[2] <- prop.table(table(google_search$sex_verify))[3]
shares$share[3] <- prop.table(table(google_search$sex_verify))[1]
shares$share[4] <- prop.table(table(google_search$birth_verify))[2]
shares$share[5] <- prop.table(table(google_search$birth_verify))[3]
shares$share[6] <- prop.table(table(google_search$birth_verify))[1]
shares$share[7] <- prop.table(table(google_search$death_verify))[2]
shares$share[8] <- prop.table(table(google_search$death_verify))[3]
shares$share[9] <- prop.table(table(google_search$death_verify))[1]
shares$share[10] <- prop.table(table(google_search$ethnicity_verify))[2]
shares$share[11] <- 0
shares$share[12] <- prop.table(table(google_search$ethnicity_verify))[1]
shares$share[13] <- prop.table(table(google_search$religion_verify))[2]
shares$share[14] <- prop.table(table(google_search$religion_verify))[3]
shares$share[15] <- prop.table(table(google_search$religion_verify))[1]
shares$share[16] <- prop.table(table(google_search$birthplace_verify))[2]
shares$share[17] <- prop.table(table(google_search$birthplace_verify))[3]
shares$share[18] <- prop.table(table(google_search$birthplace_verify))[1]
shares$share[19] <- prop.table(table(google_search$deathplace_verify))[2]
shares$share[20] <- prop.table(table(google_search$deathplace_verify))[3]
shares$share[21] <- prop.table(table(google_search$deathplace_verify))[1]
shares$share[22] <- prop.table(table(google_search$session_verify))[2]
shares$share[23] <- prop.table(table(google_search$session_verify))[3]
shares$share[24] <- prop.table(table(google_search$session_verify))[1]
shares$share[25] <- prop.table(table(google_search$party_verify))[2]
shares$share[26] <- prop.table(table(google_search$party_verify))[3]
shares$share[27] <- prop.table(table(google_search$party_verify))[1]
shares$share[28] <- prop.table(table(google_search$constituency_verify))[2]
shares$share[29] <- prop.table(table(google_search$constituency_verify))[3]
shares$share[30] <- prop.table(table(google_search$constituency_verify))[1]
shares$share[31] <- prop.table(table(google_search$service_verify))[2]
shares$share[32] <- prop.table(table(google_search$service_verify))[3]
shares$share[33] <- prop.table(table(google_search$service_verify))[1]
shares$group <- factor(shares$group, levels = c("correct_1", "incorrect_1", "na_1",
                                                "correct_2", "incorrect_2", "na_2",
                                                "correct_3", "incorrect_3", "na_3",
                                                "correct_4", "incorrect_4", "na_4",
                                                "correct_5", "incorrect_5", "na_5",
                                                "correct_6", "incorrect_6", "na_6",
                                                "correct_7", "incorrect_7", "na_7",
                                                "correct_8", "incorrect_8", "na_8",
                                                "correct_9", "incorrect_9", "na_9",
                                                "correct_10", "incorrect_10", "na_10",
                                                "correct_11", "incorrect_11", "na_11"))
shares$fill <- factor(shares$fill, levels = c("C", "D", "NF"))

# prepare figures -----------------------------------------------------------------------
validation_3 <- ggplot(shares, aes(group, share, fill = fill)) +
  geom_bar(stat="identity", width = 0.8, colour = "black", size = 0.4) +
  scale_fill_manual(values = c("gray80", "gray60", "gray40")) +
  geom_vline(xintercept =  seq(3.5,30.5,3), linetype = "dashed", colour = "gray25") +
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 12, color = "black"),
        # axis.text.x = element_text(angle = 50, hjust = 1),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 14, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 14, color = "black"),
        plot.title = element_text(size = 13, color = "black")) +
  # specify axis title size and margin
  labs(x = "",
       y = "Share") +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),
                     breaks = seq(0.2,0.8,0.2)) +
  scale_x_discrete(labels = c("correct_1" = "C", "incorrect_1" = "D", "na_1" = "NF",
                              "correct_2" = "C", "incorrect_2" = "D", "na_2" = "NF",
                              "correct_3" = "C", "incorrect_3" = "D", "na_3" = "NF",
                              "correct_4" = "C", "incorrect_4" = "D", "na_4" = "NF",
                              "correct_5" = "C", "incorrect_5" = "D", "na_5" = "NF",
                              "correct_6" = "C", "incorrect_6" = "D", "na_6" = "NF",
                              "correct_7" = "C", "incorrect_7" = "D", "na_7" = "NF",
                              "correct_8" = "C", "incorrect_8" = "D", "na_8" = "NF",
                              "correct_9" = "C", "incorrect_9" = "D", "na_9" = "NF",
                              "correct_10" = "C", "incorrect_10" = "D", "na_10" = "NF",
                              "correct_11" = "C", "incorrect_11" = "D", "na_11" = "NF")) +
  ggtitle("")
validation_3 <- ggdraw(validation_3) +
  draw_label("Sex", fontfamily = "CMU Serif",
             x = 0.11, y = 0.97, size = 12) +
  draw_label("Birth", fontfamily = "CMU Serif",
             x = 0.195, y = 0.97, size = 12) +
  draw_label("Death", fontfamily = "CMU Serif",
             x = 0.28, y = 0.97, size = 12) +
  draw_label("Ethnicity", fontfamily = "CMU Serif",
             x = 0.362, y = 0.97, size = 12) +
  draw_label("Religion", fontfamily = "CMU Serif",
             x = 0.445, y = 0.97, size = 12) +
  draw_label("Birthplace", fontfamily = "CMU Serif",
             x = 0.53, y = 0.97, size = 12) +
  draw_label("Deathplace", fontfamily = "CMU Serif",
             x = 0.615, y = 0.97, size = 12) +
  draw_label("Session", fontfamily = "CMU Serif",
             x = 0.695, y = 0.97, size = 12) +
  draw_label("Party", fontfamily = "CMU Serif",
             x = 0.78, y = 0.97, size = 12) +
  draw_label("Constituency", fontfamily = "CMU Serif",
             x = 0.865, y = 0.97, size = 12) +
  draw_label("Service", fontfamily = "CMU Serif",
             x = 0.95, y = 0.97, size = 12)
ggsave("./figures/validation_3.pdf", validation_3, width = 12, height = 4.5, dpi = 1200, device = cairo_pdf)


# DATA FOR TABLE A1 =====================================================================
# for some countries where we actually have enough data to test this exemplary
# for variables which are largely unobserved
usa_missings <- left_join(x = get_core("usa_house"),
                          y = get_social("usa_house"),
                          by = "wikidataid") %>%
  left_join(x = .,
            y = get_portrait("usa_house"),
            by = "pageid")
usa_house_core <- semi_join(get_core("usa_house"),
                            filter(get_political("usa_house"), session_start > ymd("2006-03-21")))
usa_missings$social_twitter <- ifelse(usa_missings$wikidataid %in% usa_house_core$wikidataid,
                                      TRUE, FALSE)
usa_house_core <- semi_join(get_core("usa_house"),
                            filter(get_political("usa_house"), session_start > ymd("2004-02-04")))
usa_missings$social_facebook <- ifelse(usa_missings$wikidataid %in% usa_house_core$wikidataid,
                                       TRUE, FALSE)
usa_missings$recency <- round(difftime(Sys.time(), usa_missings$birth, units = "days")/360)
usa_missings$sex <- factor(usa_missings$sex)
usa_missings$ethnicity <- ifelse(usa_missings$ethnicity == "islander" |
                                   usa_missings$ethnicity == "asian" |
                                   usa_missings$ethnicity == "native",
                                 "other", usa_missings$ethnicity)
usa_missings$ethnicity <- factor(usa_missings$ethnicity)
explanatory <-  c("sex", "ethnicity", "recency") # sex, ethnicity, birth-today
dependent <- c("birthplace")
usa_missings %>%
  missing_compare(dependent, explanatory)
dependent <- c("ethnicity")
usa_missings %>%
  missing_compare(dependent, explanatory)
dependent <- c("image_url")
usa_missings %>%
  missing_compare(dependent, explanatory)
dependent <- c("religion")
usa_missings %>%
  missing_compare(dependent, explanatory)
dependent <- c("twitter")
usa_missings[usa_missings$social_twitter == TRUE,] %>%
  missing_compare(dependent, explanatory)
dependent <- c("facebook")
usa_missings[usa_missings$social_facebook == TRUE,] %>%
  missing_compare(dependent, explanatory)
uk_missings <- left_join(x = get_core("gbr"),
                         y = get_social("gbr"),
                         by = "wikidataid")
uk_portraits <- get_portrait("gbr")
uk_portraits$pageid <- as.character(uk_portraits$pageid)
uk_missings <- left_join(x = uk_missings,
                         y = uk_portraits,
                         by = "pageid")
gbr_core <- semi_join(get_core("gbr"),
                      filter(get_political("gbr"), session_start > ymd("2006-03-21")))
uk_missings$social_twitter <- ifelse(uk_missings$wikidataid %in% gbr_core$wikidataid,
                                     TRUE, FALSE)
gbr_core <- semi_join(get_core("gbr"),
                      filter(get_political("gbr"), session_start > ymd("2004-02-04")))
uk_missings$social_facebook <- ifelse(uk_missings$wikidataid %in% gbr_core$wikidataid,
                                      TRUE, FALSE)
uk_missings$recency <- round(difftime(Sys.time(), uk_missings$birth, units = "days")/360)
uk_missings$sex <- factor(uk_missings$sex)
uk_missings$ethnicity <- factor(uk_missings$ethnicity)
explanatory <-  c("sex", "ethnicity", "recency") # sex, ethnicity, birth-today
dependent <- c("birthplace")
uk_missings %>%
  missing_compare(dependent, explanatory)
dependent <- c("ethnicity")
uk_missings %>%
  missing_compare(dependent, explanatory)
dependent <- c("image_url")
uk_missings %>%
  missing_compare(dependent, explanatory)
dependent <- c("religion")
uk_missings %>%
  missing_compare(dependent, explanatory)
dependent <- c("twitter")
uk_missings[uk_missings$social_twitter == TRUE,] %>%
  missing_compare(dependent, explanatory)
dependent <- c("facebook")
uk_missings[uk_missings$social_facebook == TRUE,] %>%
  missing_compare(dependent, explanatory)
fra_missings <- left_join(x = get_core("fra"),
                          y = get_social("fra"),
                          by = "wikidataid") %>%
  left_join(x = .,
            y = get_portrait("fra"),
            by = "pageid")
fra_core <- semi_join(get_core("fra"),
                      filter(get_political("fra"), session_start > ymd("2006-03-21")))
fra_missings$social_twitter <- ifelse(fra_missings$wikidataid %in% fra_core$wikidataid,
                                      TRUE, FALSE)
fra_core <- semi_join(get_core("fra"),
                      filter(get_political("fra"), session_start > ymd("2004-02-04")))
fra_missings$social_facebook <- ifelse(fra_missings$wikidataid %in% fra_core$wikidataid,
                                       TRUE, FALSE)
fra_missings$recency <- round(difftime(Sys.time(),fra_missings$birth, units = "days")/360)
fra_missings$sex <- factor(fra_missings$sex)
fra_missings$ethnicity <- ifelse(fra_missings$ethnicity == "arab" |
                                   fra_missings$ethnicity == "asian" |
                                   fra_missings$ethnicity == "hispanic",
                                 "other", fra_missings$ethnicity)
fra_missings$ethnicity <- factor(fra_missings$ethnicity)
explanatory <-  c("sex", "ethnicity", "recency") # sex, ethnicity, birth-today
dependent <- c("birthplace")
fra_missings %>%
  missing_compare(dependent, explanatory)
dependent <- c("ethnicity")
fra_missings %>%
  missing_compare(dependent, explanatory)
dependent <- c("image_url")
fra_missings %>%
  missing_compare(dependent, explanatory)
dependent <- c("religion")
fra_missings %>%
  missing_compare(dependent, explanatory)
dependent <- c("twitter")
fra_missings[fra_missings$social_twitter == TRUE,] %>%
  missing_compare(dependent, explanatory)
dependent <- c("facebook")
fra_missings[fra_missings$social_facebook == TRUE,] %>%
  missing_compare(dependent, explanatory)
