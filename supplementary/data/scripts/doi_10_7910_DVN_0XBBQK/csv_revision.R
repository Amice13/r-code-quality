### OUTPUT CSV OF ANALYSIS RESULTS FOR REVISION

## GLOBAL RESULTS

csv_revision_global_04 <- transpose(as.data.frame(new_res_art_4[,,1]))
colnames(csv_revision_global_04) <- c("mean", "lower", "upper")
csv_revision_global_04$year <- 2013:2019
csv_revision_global_04$age_group <- "0-4"
csv_revision_global_04$geography_type <- "global"
csv_revision_global_04$geography <- "Global"

csv_revision_global_514 <- transpose(as.data.frame(new_res_art_4[,,2]))
colnames(csv_revision_global_514) <- c("mean", "lower", "upper")
csv_revision_global_514$year <- 2013:2019
csv_revision_global_514$age_group <- "5-14"
csv_revision_global_514$geography_type <- "global"
csv_revision_global_514$geography <- "Global"

csv_revision_global <- rbind(csv_revision_global_04, csv_revision_global_514)

## COUNTRY RESULTS

csv_revision_country_04 <- transpose(as.data.frame(new_res_art1[, ,,1]))
colnames(csv_revision_country_04) <- c("mean", "lower", "upper")
csv_revision_country_04$year <- rep(2013:2019, each=185)
csv_revision_country_04$age_group <- "0-4"
csv_revision_country_04$geography_type <- "country"
csv_revision_country_04$geography <- rep(map_countries_2021, 7)
csv_revision_country_514 <- transpose(as.data.frame(new_res_art1[, ,,2]))
colnames(csv_revision_country_514) <- c("mean", "lower", "upper")
csv_revision_country_514$year <- rep(2013:2019, each=185)
csv_revision_country_514$age_group <- "5-14"
csv_revision_country_514$geography_type <- "country"
csv_revision_country_514$geography <- rep(map_countries_2021, 7)
csv_revision_country <- rbind(csv_revision_country_04, csv_revision_country_514)

## REGION RESULTS

csv_revision_AFR <- transpose(as.data.frame(AFR_new_res_art_age))
colnames(csv_revision_AFR) <- c("mean", "lower", "upper")
csv_revision_AFR$year <- rep(2013:2019)
csv_revision_AFR$age_group <- rep(c("0-4", "5-14"), each=7)
csv_revision_AFR$geography_type <- "region"
csv_revision_AFR$geography <- "AFR"

csv_revision_AMR <- transpose(as.data.frame(AMR_new_res_art_age))
colnames(csv_revision_AMR) <- c("mean", "lower", "upper")
csv_revision_AMR$year <- rep(2013:2019)
csv_revision_AMR$age_group <- rep(c("0-4", "5-14"), each=7)
csv_revision_AMR$geography_type <- "region"
csv_revision_AMR$geography <- "AMR"

csv_revision_EMR <- transpose(as.data.frame(EMR_new_res_art_age))
colnames(csv_revision_EMR) <- c("mean", "lower", "upper")
csv_revision_EMR$year <- rep(2013:2019)
csv_revision_EMR$age_group <- rep(c("0-4", "5-14"), each=7)
csv_revision_EMR$geography_type <- "region"
csv_revision_EMR$geography <- "EMR"

csv_revision_EUR <- transpose(as.data.frame(EUR_new_res_art_age))
colnames(csv_revision_EUR) <- c("mean", "lower", "upper")
csv_revision_EUR$year <- rep(2013:2019)
csv_revision_EUR$age_group <- rep(c("0-4", "5-14"), each=7)
csv_revision_EUR$geography_type <- "region"
csv_revision_EUR$geography <- "EUR"

csv_revision_SEA <- transpose(as.data.frame(SEA_new_res_art_age))
colnames(csv_revision_SEA) <- c("mean", "lower", "upper")
csv_revision_SEA$year <- rep(2013:2019)
csv_revision_SEA$age_group <- rep(c("0-4", "5-14"), each=7)
csv_revision_SEA$geography_type <- "region"
csv_revision_SEA$geography <- "SEA"

csv_revision_WPR <- transpose(as.data.frame(WPR_new_res_art_age))
colnames(csv_revision_WPR) <- c("mean", "lower", "upper")
csv_revision_WPR$year <- rep(2013:2019)
csv_revision_WPR$age_group <- rep(c("0-4", "5-14"), each=7)
csv_revision_WPR$geography_type <- "region"
csv_revision_WPR$geography <- "WPR"

csv_revision_region <- rbind(csv_revision_AFR, csv_revision_AMR, csv_revision_EMR, csv_revision_EUR, csv_revision_SEA, csv_revision_WPR)

## PERCENT REPORTED

notifs_2021_country <- map_data_revision %>%
  group_by(country, g_whoregion, age_group, year) %>%
  summarise(notifs = sum(who_cases))

notifs_2021_region <- notifs_2021_country %>%
  group_by(g_whoregion, age_group, year) %>%
  summarise(notifs = sum(notifs))

notifs_2021_global <- notifs_2021_region %>%
  group_by(age_group, year) %>%
  summarise(notifs = sum(notifs))

percent_reported_country <- merge(csv_revision_country, notifs_2021_country, by.x=c("geography", "age_group", "year"),
                                  by.y=c("country", "age_group", "year"), all.x=TRUE)
percent_reported_region <- merge(csv_revision_region, notifs_2021_region, by.x=c("geography", "age_group", "year"),
                                 by.y=c("g_whoregion", "age_group", "year"), all.x=TRUE)
percent_reported_global <- merge(csv_revision_global, notifs_2021_global, by.x=c("age_group", "year"),
                                 by.y=c("age_group", "year"), all.x=TRUE)

percent_reported_country$percent_reported <- percent_reported_country$notifs/percent_reported_country$mean
percent_reported_country$percent_lower <- percent_reported_country$notifs/percent_reported_country$upper
percent_reported_country$percent_upper <- percent_reported_country$notifs/percent_reported_country$lower

percent_reported_region$percent_reported <- percent_reported_region$notifs/percent_reported_region$mean
percent_reported_region$percent_lower <- percent_reported_region$notifs/percent_reported_region$upper
percent_reported_region$percent_upper <- percent_reported_region$notifs/percent_reported_region$lower

percent_reported_global$percent_reported <- percent_reported_global$notifs/percent_reported_global$mean
percent_reported_global$percent_lower <- percent_reported_global$notifs/percent_reported_global$upper
percent_reported_global$percent_upper <- percent_reported_global$notifs/percent_reported_global$lower

percent_reported_country$outcome <- "percent_reported"
percent_reported_region$outcome <- "percent_reported"
percent_reported_global$outcome <- "percent_reported"

percent_reported_country <- percent_reported_country[, c("geography_type", "geography", "year", "age_group", 
                                                         "outcome", "percent_reported", "percent_lower", 
                                                         "percent_upper")]
percent_reported_region <- percent_reported_region[, c("geography_type", "geography", "year", "age_group", 
                                                         "outcome", "percent_reported", "percent_lower", 
                                                         "percent_upper")]
percent_reported_global <- percent_reported_global[, c("geography_type", "geography", "year", "age_group", 
                                                         "outcome", "percent_reported", "percent_lower", 
                                                         "percent_upper")]

## COMBINING

csv_revision_est_2021 <- rbind(csv_revision_country, csv_revision_region, csv_revision_global)
csv_revision_est_2021$outcome <- "model_est"
csv_revision_est_2021 <- csv_revision_est_2021[, c("geography_type", "geography", "year", "age_group", "outcome", "mean", "lower",
                                 "upper")]

percent_reported <- rbind(percent_reported_country, percent_reported_region, percent_reported_global)

colnames(percent_reported)[6:8] <- c("mean", "lower", "upper")

csv_revision_2021_final <- rbind(csv_revision_est_2021, percent_reported)

write.csv(csv_revision_2021_final, "global_peds_TB_estimates_revision.csv", row.names = FALSE)