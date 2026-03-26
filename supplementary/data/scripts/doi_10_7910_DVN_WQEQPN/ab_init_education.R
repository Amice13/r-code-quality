ab_1960 <- subset(ab, ab$year - ab$age < 1950 )
ab_1960$highsch <- as.numeric(ab_1960$educ > 2 )


## agg at the ethnic polygon level, to have more locations.
ab_1960_educ_poly <- ab_1960 %>%
              subset(!(all.eth_poly == "" | is.na(all.eth_poly))  ) %>%
              group_by(all.eth_poly) %>%
              mutate(N=n()) %>%
              group_by(all.eth_poly, N) %>%
              summarize_at("highsch", mean, na.rm = T)
ab_1960_educ_poly$highsch <- ifelse(is.nan(ab_1960_educ_poly$highsch), NA, ab_1960_educ_poly$highsch)
names(ab_1960_educ_poly) <- c("all.eth_poly", "N", "highsch.1960.poly")

ab_1960_educ_lang <- ab_1960 %>%
              subset(!(all.eth_lang == "" | is.na(all.eth_lang))  ) %>%
              group_by(all.eth_lang) %>%
              mutate(N=n()) %>%
              group_by(all.eth_lang, N) %>%
              summarize_at("highsch", mean, na.rm = T)

ab_1960_educ_lang$highsch <- ifelse(is.nan(ab_1960_educ_lang$highsch), NA, ab_1960_educ_lang$highsch)
names(ab_1960_educ_lang) <- c("all.eth_lang", "N", "highsch.1960.lang")
