# ---------------------------------------------------------------------------------------
# THE COMPARATIVE LEGISLATORS DATABASE
# Sascha Göbel and Simon Munzert
# Script for analyses and corresponding figures
# April 2020
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
    './data/lit_review.csv',
    ")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
    './figures/lit_rev.pdf' 
    './figures/tab-pageviews-congress.tex'
    './figures/congress_model_coefplot.pdf'
    './figures/tab-congress-models.tex'
    './figures/legislatoR-traffic-us.pdf'
    './figures/turnover.pdf'
    './figures/data_demand.pdf'
    ")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
    Line 47 - FIGURE 1
    Line 143 - FIGURE 4
    Line 433 - FIGURE 5
    Line 515 - FIGURE A1
    ")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("comparative-legislators-database")

# install and load packages -------------------------------------------------------------
source("./code/paper-packages.R")
source("./code/paper-functions.R")


#### FIGURE 1 ===========================================================================

# prepare data --------------------------------------------------------------------------
lit_survey <- read.csv("./data/lit_review.csv") %>%
  filter(Relevant != "no") %>%
  dplyr::select(Year, Experiment, SourceTypeNEW, DataSourceReference, Country, 
                YearsCovered, 16:20)
lit_survey$original <- str_detect(lit_survey$SourceTypeNEW, "government|original")
lit_survey$us <- str_detect(as.character(lit_survey$Country), "US")
lit_survey$YearsCovered <- as.character(lit_survey$YearsCovered) %>%
  str_replace_all(" ", "")
lit_survey$start <- str_extract_all(lit_survey$YearsCovered, "[[:digit:]]{4}") %>%
  lapply(as.numeric) %>%
  lapply(min) %>%
  unlist()
lit_survey$end <- str_extract_all(lit_survey$YearsCovered, "[[:digit:]]{4}") %>%
  lapply(as.numeric) %>%
  lapply(max) %>%
  unlist()
lit_survey$years <- str_replace_all(lit_survey$YearsCovered, "-", ":") %>%
  str_replace(";", ",") %>%
  str_c("c(", ., ")") %>%
  parse(text = .) %>%
  lapply(eval) %>%
  lapply(length) %>%
  unlist
lit_survey$start  <- ifelse(lit_survey$years == 0, NA, lit_survey$start)
lit_survey$end  <- ifelse(lit_survey$years == 0, NA, lit_survey$end)
lit_survey$years  <- ifelse(lit_survey$years == 0, NA, lit_survey$years)
lit_survey$comparative <- str_detect(lit_survey$Country, ",")
lit_survey$demographic <- lit_survey$VarsSocioDemographic %>% as.character() %>%
  ifelse(. == "", NA, .)
lit_survey$behavior <- lit_survey$VarPolBehavior %>% as.character() %>%
  ifelse(. == "", NA, .)
lit_survey$offices <- lit_survey$VarsOffices %>% as.character() %>%
  ifelse(. == "", NA, .)
lit_survey$district <- lit_survey$VarsDistrict %>% as.character() %>%
  ifelse(. == "", NA, .)
shares <- data.frame(group = c("usa", "original", "comparative", "before1950", "after1950",
                               "after1990", "demographic", "behav", "office", "district"), share = NA)
shares$share[1] <- prop.table(table(lit_survey$us))[[2]]
shares$share[2] <- prop.table(table(lit_survey$original))[[2]]
shares$share[3] <- prop.table(table(lit_survey$comparative))[[2]]
shares$share[4] <- length(which(lit_survey$start <= 1950))/190
shares$share[5] <- length(which(lit_survey$start > 1950 & lit_survey$start <= 1990 ))/190
shares$share[6] <- length(which(lit_survey$start > 1990))/190
shares$share[7] <- prop.table(table(lit_survey$demographic))[[2]]
shares$share[8] <- prop.table(table(lit_survey$behavior))[[2]]
shares$share[9] <- prop.table(table(lit_survey$offices))[[2]]
shares$share[10] <- prop.table(table(lit_survey$district))[[2]]
shares$fill <- c("spatial coverage", "data collection", "spatial coverage",
                 rep("temporal coverage", 3), rep("substantive coverage", 4))

shares$group <- factor(shares$group, levels = c("original", "usa", "comparative",
                                                "before1950", "after1950", "after1990",
                                                "demographic", "behav", "office",
                                                "district"),
                       labels = c("Own effort", "US focus", "Comparative", "Until 1950",
                                  "1950 to 1990", "Since 1990", "Socio-\ndemographic",
                                  "Political\nbehavior", "Office-\nrelated", "District-\nrelated"))

# prepare figure ------------------------------------------------------------------------
composition <-  ggplot(shares, aes(group, share, fill = fill)) +
  geom_bar(stat="identity", width = 0.8, colour = "black", size = 0.4) +
  scale_fill_manual(values = c("gray80", "gray60", "gray20", "gray40")) +
  geom_vline(xintercept =  c(1.5,3.5,6.5), linetype = "dashed", colour = "gray25") +
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
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
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 16, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 16, color = "black"),
        plot.title = element_text(size = 13, color = "black")) +
  # specify axis title size and margin
  labs(x = "",
       y = "Share") +
  ggtitle("    Data                Spatial                         Temporal                                        Substantive
          collection            coverage                         coverage                                          coverage") +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),
                     breaks = seq(0.2,0.8,0.2)) +
  scale_x_discrete(expand = c(0.06, 0))
ggsave("./figures/lit_rev.pdf", composition, width = 10, height = 4.5, dpi = 1200, device = cairo_pdf)


#### FIGURE 4 ===========================================================================

# prepare US House data -----------------------------------------------------------------
# get political data
house_political_df <- left_join(x = filter(get_political(legislature = "usa_house"), as.numeric(session) == 115),
                                y = get_core(legislature = "usa_house"), 
                                by = "pageid")
# get traffic data
house_traffic_df <- get_traffic(legislature = "usa_house") %>% 
  filter(date >= "2017-01-03" & date <= "2019-01-03") %>% 
  group_by(pageid) %>% 
  summarize(traffic_mean = mean(traffic, na.rm = TRUE),
            traffic_max = max(traffic, na.rm = TRUE))
# get offices data
house_offices_df <- get_office(legislature = "usa_house") %>% 
  transmute(wikidataid = wikidataid,
            office_governor = rowSums(dplyr::select(., starts_with("governor_of"))) >= 1,
            office_ltgovernor = rowSums(dplyr::select(., starts_with("lieutenant_governor_of"))) >= 1,
            office_ussecretary = rowSums(dplyr::select(., matches("united_states_secretary_of|united_states_attorney_general"))) >= 1,
            office_judge = rowSums(dplyr::select(., contains("judge"))) >= 1,
            office_mayor = rowSums(dplyr::select(., contains("mayor"))) >= 1,
            office_statehousesenate = rowSums(dplyr::select(., matches("house_of_representatives|member_.+_assembly|state_senate"))) >= 1
  )
# get political positions data
house_positions_df <- get_office(legislature = "usa_house") %>% 
  transmute(wikidataid = wikidataid,
            position_speaker_house = rowSums(dplyr::select(., matches("speaker_of_the_united_states_house_of_representatives|speaker_of_the_house"))) >= 1,
            position_party_chairman = rowSums(dplyr::select(., matches("republican_.+chairman|democratic_.+chairman|party_chair"))) >= 1,
            position_majminleader = rowSums(dplyr::select(., matches("majority_leader|minority_leader"))) >= 1
  )
position_majminleader_house_df <- data.frame(wikidataid = c("Q170581", "Q516515", "Q1209677", "Q577751", "Q949514", "Q766866", "Q497271", "Q11702", "Q1525924", "Q180390", "Q781336", "Q3435918"), position_majminleader = TRUE, stringsAsFactors = FALSE)
position_whip_house_df <- data.frame(wikidataid = c("Q516515", "Q1289889", "Q170581", "Q1174271", "Q3511361", "Q2442506", "Q577751", "Q1857141", "Q766866", "Q497271", "Q1525924", "Q180390", "Q182788"), position_whip = TRUE, stringsAsFactors = FALSE)
position_deputywhip_house_df <- data.frame(wikidataid = c("Q45380", "Q784037", "Q434952", "Q2057809", "Q968214", "Q766866", "Q497271", "Q1525924", "Q553626", "Q7349894", "Q369068", "Q2439827", "Q518650", "Q1174271"), position_deputywhip = TRUE, stringsAsFactors = FALSE)
house_positions_df <- left_join(house_positions_df, position_majminleader_house_df, by = "wikidataid") %>%
  left_join(position_whip_house_df, by = "wikidataid") %>% 
  left_join(position_deputywhip_house_df, by = "wikidataid")
house_positions_df$position_majminleader <- ifelse(house_positions_df$position_majminleader.x == TRUE | house_positions_df$position_majminleader.y == TRUE, TRUE, FALSE)
house_positions_df$position_majminleader.x <- NULL
house_positions_df$position_majminleader.y <- NULL
house_positions_df$position_majminleader[is.na(house_positions_df$position_majminleader)] <- FALSE
house_positions_df$position_whip[is.na(house_positions_df$position_whip)] <- FALSE
house_positions_df$position_deputywhip[is.na(house_positions_df$position_deputywhip)] <- FALSE
# get data on sessions served
house_sessions_served_df <- get_political(legislature = "usa_house") %>% group_by(pageid) %>% dplyr::summarize(sessions_served = n())
# merge
house_df <- left_join(house_political_df, house_sessions_served_df, by = "pageid") %>% left_join(house_offices_df, by = "wikidataid") %>% left_join(house_positions_df, by = "wikidataid") %>% left_join(house_traffic_df, by = "pageid")
# add age and chamber variables
house_df$age <- get_age(house_df$birth, as.POSIXct("2017-01-01"))
house_df$chamber <- "House"
# only keep last session of each member
house_df <- house_df %>% group_by(wikidataid) %>% slice(which.max(session)) %>% ungroup()

# prepare US Senate data ----------------------------------------------------------------
# get political data
senate_political_df <- left_join(x = filter(get_political(legislature = "usa_senate"), as.numeric(session) == 115),
                                 y = get_core(legislature = "usa_senate"), 
                                 by = "pageid")
# get traffic data
senate_traffic_df <- get_traffic(legislature = "usa_senate") %>% 
  filter(date >= "2017-01-03" & date <= "2019-01-03") %>% 
  group_by(pageid) %>% 
  summarize(traffic_mean = mean(traffic, na.rm = TRUE),
            traffic_max = max(traffic, na.rm = TRUE))
# get offices data
senate_offices_df <- get_office(legislature = "usa_senate") %>% 
  transmute(wikidataid = wikidataid,
            office_governor = rowSums(dplyr::select(., starts_with("governor_of"))) >= 1,
            office_ltgovernor = rowSums(dplyr::select(., starts_with("lieutenant_governor_of"))) >= 1,
            office_ussecretary = rowSums(dplyr::select(., matches("united_states_secretary_of|united_states_attorney_general"))) >= 1,
            office_judge = rowSums(dplyr::select(., contains("judge"))) >= 1,
            office_mayor = rowSums(dplyr::select(., contains("mayor"))) >= 1,
            office_statehousesenate = rowSums(dplyr::select(., matches("house_of_representatives|member_.+_assembly|state_senate"))) >= 1
  )
# get political positions data
senate_positions_df <- get_office(legislature = "usa_senate") %>% 
  transmute(wikidataid = wikidataid,
            position_speaker_house = rowSums(dplyr::select(., matches("speaker_of_the_united_states_house_of_representatives|speaker_of_the_house"))) >= 1,
            position_party_chairman = rowSums(dplyr::select(., matches("republican_.+chairman|democratic_.+chairman|party_chair"))) >= 1,
            position_majminleader = rowSums(dplyr::select(., matches("majority_leader|minority_leader"))) >= 1
  )
position_majminleader_senate_df <- data.frame(wikidataid = c("Q380900", "Q314459", "Q443640", "Q368920", "Q276524", "Q355522", "Q31112", "Q50608", "Q319079", "Q1337643"), position_majminleader = TRUE, stringsAsFactors = FALSE)
position_whip_senate_df <- data.frame(wikidataid = c("Q434804", "Q314459", "Q361139", "Q1994957", "Q719568", "Q371031", "Q50608", "Q355522", "Q1239374", "Q2636748", "Q50599"), position_whip = TRUE, stringsAsFactors = FALSE)
senate_positions_df <- left_join(senate_positions_df, position_majminleader_senate_df, by = "wikidataid") %>%
  left_join(position_whip_senate_df, by = "wikidataid") 
senate_positions_df$position_majminleader <- ifelse(senate_positions_df$position_majminleader.x == TRUE | senate_positions_df$position_majminleader.y == TRUE, TRUE, FALSE)
senate_positions_df$position_majminleader.x <- NULL
senate_positions_df$position_majminleader.y <- NULL
senate_positions_df$position_majminleader[is.na(senate_positions_df$position_majminleader)] <- FALSE
senate_positions_df$position_whip[is.na(senate_positions_df$position_whip)] <- FALSE
senate_positions_df$position_deputywhip <- FALSE
# get data on sessions served
senate_sessions_served_df <- get_political(legislature = "usa_senate") %>% group_by(pageid) %>% dplyr::summarize(sessions_served = n())
# merge
senate_df <- left_join(senate_political_df, senate_sessions_served_df, by = "pageid") %>% left_join(senate_offices_df, by = "wikidataid") %>% left_join(senate_positions_df, by = "wikidataid") %>% left_join(senate_traffic_df, by = "pageid")
# add age and chamber variables
senate_df$age <- get_age(senate_df$birth, as.POSIXct("2017-01-01"))
senate_df$chamber <- "Senate"
# only keep last session of each member
senate_df <- senate_df %>% group_by(wikidataid) %>% slice(which.max(session)) %>% ungroup()

# merge and clean data ------------------------------------------------------------------
congress115_df <- bind_rows(senate_df, house_df)
congress115_df$wikidata_id <- str_replace(congress115_df$wikidataid, "Q", "")
congress115_df$wikidataid <- NULL
congress115_df$party[congress115_df$party == "Independent"] <- "I"
congress115_df$party[congress115_df$party == "D/PNP"] <- "D"
congress115_df$party[congress115_df$party == "DFL"] <- "D"
congress115_df$party[congress115_df$party == "DNPL"] <- "D"
congress115_df$party[congress115_df$party == "PNP/R"] <- "R"
congress115_df$dead <- ifelse(!is.na(congress115_df$death), TRUE, FALSE)

# top and bottom 10 pageviews for all Congress members ----------------------------------
congress115_df <- arrange(congress115_df, desc(traffic_mean))
congress115_df$rank <- 1:nrow(congress115_df)
congress_df_sub <- dplyr::select(congress115_df, rank, name, matches("traffic+"))
congress_df_sub_table <- congress_df_sub
names(congress_df_sub_table) <- c("Rank", "Senator/Representative", "Mean", "Maximum")
congress_top10 <- head(congress_df_sub_table, 10)
congress_bottom10 <- tail(congress_df_sub_table, 10)
congress_top_bottom_table <- rbind(congress_top10, congress_bottom10)
print(xtable(congress_top_bottom_table, caption = "Wikipedia article views for top 10 / bottom 10 bottom Members of Congress, 2017--2018.\\label{tab:pageviewstop}", digits = 0), align = c("l", "r", "r", "r"), booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "t!", include.rownames=FALSE, hline.after = c(-1, seq(0, 20, 5)), file = "../figures/tab-pageviews-congress.tex")

# OLS models of log pageviews -----------------------------------------------------------
congress115_df$sessions_served_3 <- congress115_df$sessions_served / 3
congress115_df$traffic_mean_log <- log(congress115_df$traffic_mean)
congress115_df$ethnicity_cat <- ifelse(congress115_df$ethnicity == "asian", "Asian",
                                       ifelse(congress115_df$ethnicity == "black", "Black",
                                              ifelse(congress115_df$ethnicity == "hispanic", "Hispanic",
                                                     ifelse(congress115_df$ethnicity == "white", "White",
                                                            "Other"))))
congress115_df$ethnicity_cat <- factor(congress115_df$ethnicity_cat, levels = c("White", "Asian", "Black", "Hispanic",  "Other"))
covars <- c("chamber", "sessions_served_3", "party",  
            "office_governor", "office_ltgovernor", "office_ussecretary", 
            "position_speaker_house", "position_majminleader", "position_whip", "position_deputywhip", "position_party_chairman", "sex", "ethnicity_cat")
covars_names <- c("Senate", "Sessions served", "Party (Independent)", "Party (Republican)", "Office: Governor", "Office: Lt. Governor", "Office: US Secretary", "Position: House Speaker", "Position: Majority/Minority Leader", "Position: Whip", "Position: Deputy Whip", "Position: Party Chairman", "Male", "Ethnicity: Asian", "Ethnicity: Black", "Ethnicity: Hispanic", "Ethnicity: Other", "(Intercept)")
fmla_traffic <- paste("traffic_mean", paste(covars, collapse = " + "), sep = " ~ ")
summary(traffic_model <- lm(fmla_traffic, congress115_df))
fmla_traffic_log <- paste("traffic_mean_log", paste(covars, collapse = " + "), sep = " ~ ")
summary(log_traffic_model <- lm(fmla_traffic_log, congress115_df))
# export table
stargazer(log_traffic_model, single.row = FALSE, dep.var.caption = "", align = TRUE, covariate.labels = covars_names, style = "ajps", omit.table.layout = "d#", column.labels = c("Prominence", "Influence"), font.size = "scriptsize", table.placement = "t!", title="OLS estimates of log mean daily Wikipedia pageviews of members of 115th US Congress.\\label{tab:congress-models}", out = "../figures/tab-congress-models.tex")
# export coefficient plot
# re-run models, reverse because of stupid coefplot2
covars <- c("chamber", "sessions_served_3", "party",  
            "office_governor", "office_ltgovernor", "office_ussecretary", 
            "position_speaker_house", "position_majminleader", "position_whip", "position_deputywhip", "position_party_chairman",
            "sex",
            "ethnicity_cat_rev")

congress115_df$ethnicity_cat_rev <- factor(congress115_df$ethnicity_cat, levels = c("White", "Other", "Hispanic", "Black", "Asian"))
fmla_traffic_log <- paste("traffic_mean_log", paste(rev(covars), collapse = " + "), sep = " ~ ")
summary(log_traffic_model_rev <- lm(fmla_traffic_log, congress115_df))
colors <- c("black")
covars_names <- c("Senate", "Sessions served", # chamber and seniority
                  "Republican", "Independent", # party
                  "Governor", "Lt. Governor", "US Secretary", # previous office
                  "House Speaker", "Maj/Min Leader", "Whip", "Deputy Whip", "Party Chairman", # leadership position
                  "Male",
                  "Asian", "Black", "Hispanic", "Other")

# prepare OLS figure --------------------------------------------------------------------
pdf(file="../figures/congress_model_coefplot.pdf", height=4.25, width=4, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(2,2,2,.5))
par(xaxs = "i", yaxs = "i")
#par(xpd=TRUE)
coefplot2(rev(list(log_traffic_model_rev)),
          col = colors,
          xlim = c(-2,5),
          ylim = c(0.5,17.5),
          varnames = rev(covars_names),
          intercept = FALSE,
          main = "",
          h.axis = FALSE,
          top.axis = FALSE,
          mar = c(2,8,2.5,.25), # margins
          spacing = .25,
          legend = FALSE
          #legend.args = list(x = 1.6, y = 1.7, inset = .015, legend = c("Prominence", "Influence"), horiz = FALSE, cex = .7, bg = "white", box.col = "white", xpd = NA, title.adj = 1)
)
abline(v = seq(-2,4,1), lty = 5, col = "darkgrey")
abline(v = 0, lty = 1, col = "black")
axis(1, seq(-2,5,1), seq(-2,5,1))
axis(3, seq(-2,5,1), seq(-2,5,1))
arrows(-5.5, .5, -5.5, 4.3, xpd = TRUE, length = 0) # ethnicity
arrows(-5.5, 4.5, -5.5, 5.3, xpd = TRUE, length = 0) # sex
arrows(-5.5, 5.5, -5.5, 10.3, xpd = TRUE, length = 0) # leadership position
arrows(-5.5, 10.5, -5.5, 13.3, xpd = TRUE, length = 0) # previous office
arrows(-5.5, 13.5, -5.5, 15.3, xpd = TRUE, length = 0) # party
arrows(-5.5, 15.5, -5.5, 17.3, xpd = TRUE, length = 0) # chamber and seniority
mtext("Ethnicity", side = 2, line = .2, at = 2.4, outer = FALSE, cex = .8, font = 3)
mtext("Sex", side = 2, line = .2, at = 4.9, outer = FALSE, cex = .8, font = 3)
mtext("Leadership", side = 2, line = .8, at = 7.9, outer = FALSE, cex = .8, font = 3)
mtext("position", side = 2, line = .2, at = 7.9, outer = FALSE, cex = .8, font = 3)
mtext("Office", side = 2, line = .2, at = 11.9, outer = FALSE, cex = .8, font = 3)
mtext("Party", side = 2, line = .2, at = 14.4, outer = FALSE, cex = .8, font = 3)
mtext("Se-", side = 2, line = .8, at = 16.4, outer = FALSE, cex = .8, font = 3)
mtext("niority", side = 2, line = .2, at = 16.4, outer = FALSE, cex = .8, font = 3)
dev.off()

# time series of page traffic data, aggregated ------------------------------------------
# get data, house
house_traffic_df <- get_traffic(legislature = "usa_house") %>% 
  filter(date >= "2017-01-03" & date <= "2019-01-03") %>% 
  right_join(y = filter(get_political(legislature = "usa_house"), as.numeric(session) == 115), by = "pageid") %>%
  left_join(y = get_core(legislature = "usa_house"), by = "pageid") %>%
  dplyr::select(pageid, date, traffic, session, party, name)
# get data, senate
senate_traffic_df <- get_traffic(legislature = "usa_senate") %>% 
  filter(date >= "2017-01-03" & date <= "2019-01-03") %>% 
  right_join(y = filter(get_political(legislature = "usa_senate"), as.numeric(session) == 115), by = "pageid") %>%
  left_join(y = get_core(legislature = "usa_senate"), by = "pageid") %>%
  dplyr::select(pageid, date, traffic, session, party, name)
# aggregate data
us_traffic <- bind_rows(house_traffic_df, senate_traffic_df)
us_traffic$date <- ymd(us_traffic$date)
us_traffic_date <- group_by(us_traffic, date)
us_traffic_legislators <- group_by(us_traffic, pageid)
us_traffic_sum <- summarize(us_traffic_date, mean = mean(traffic, na.rm = TRUE))
us_traffic_sum <- mutate(us_traffic_sum, 
                         mean_l1 = lag(mean, 1), 
                         mean_f1 = lead(mean, 1),
                         peak = (mean >= 1.8*mean_l1 & mean > 1000))
# identify peaks
us_traffic_peaks <- filter(us_traffic_sum, peak == TRUE)
us_traffic_peaks_df <- filter(us_traffic, date %in% us_traffic_peaks$date)
us_traffic_peaks_group <- group_by(us_traffic_peaks_df, date) %>% dplyr::arrange(desc(traffic)) %>% filter(row_number()==1)
us_traffic_peaks_group <- arrange(us_traffic_peaks_group, date)
events_vec <- c("Attorney General Confirmation Hearing", 
                "Comey hearing on Russian interference", 
                "Comey hearing on Russian interference II", 
                "Session hearing on Russian interference",
                "Brain cancer diagnosis",
                "Sexual misconduct allegations",
                "Senate special election in Alabama",
                "Response to State of the Union",
                "Nomination for Secretary of State",
                "Death",
                "Senate seat appointment",
                "Kavanaugh hearing",
                "Midterm elections"
)
events_vec_names <- paste0(events_vec, " (", us_traffic_peaks_group$name, ")")

# prepare time series figure ------------------------------------------------------------
pdf(file="../figures/legislatoR-traffic-us.pdf", height=3, width=8, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(0,2,0,.5))
par(yaxs="i", xaxs="i", bty="n")
layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), 
       heights = c(1, .75), widths = 5)
# page views time series
par(mar=c(2,4,0,.5))
plot(ymd(us_traffic_sum$date), us_traffic_sum$mean, type = "l", ylim= c(0, 1.2*max(us_traffic_sum$mean)), xlim = c(ymd("2017-01-03"), ymd("2019-01-03")),  xaxt = "n", yaxt = "n", xlab = "", ylab = "Avg. page views", col = "white")
#abline(v = dates[day(dates) == 1 & month(dates) %in% c(1)], col = "lightgrey")
abline(h = seq(0, 1.2*max(us_traffic_sum$mean), 2000), col = "lightgrey")
lines(ymd(us_traffic_sum$date), us_traffic_sum$mean, lwd = .5)
dates <- seq(ymd("2017-01-01"), ymd("2019-01-03"), by = 1)
axis(1, dates[day(dates) == 1], labels = FALSE)
axis(1, dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], labels = as.character(month(dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], label = TRUE, abbr = TRUE)), tick = F, lwd = 0, cex.axis = .8, line = -1)
axis(1, dates[day(dates) == 1 & month(dates) %in% c(7)], labels = as.character(year(dates[day(dates) == 15 & month(dates) %in% c(7)])), tick = F, lwd = 0, cex.axis = .8)
axis(2, seq(0, 1.2*max(us_traffic_sum$mean), 2000), las = 2, cex.axis = .8)
# events labels in time series
gap_size <- c(800, 800, 800, 2500, 800, 800, 800, 800, 800, 800, 1600, 1800, 800)
for(i in seq_along(events_vec)) {
  points(us_traffic_peaks_group$date[i], us_traffic_sum$mean[us_traffic_sum$peak == TRUE][i] + gap_size[i], pch=21, cex = 2.2, bg = "white")  
  text(us_traffic_peaks_group$date[i], us_traffic_sum$mean[us_traffic_sum$peak == TRUE][i] + gap_size[i], i, cex = .7)
}
# events labels explained
par(mar=c(0,0,0,0))
plot(0, 0, xlim = c(0, 5), ylim = c(1, 5), xaxt = "n", yaxt = "n", xlab = "", ylab = "", cex = 0)
num_events <- length(events_vec)
num_even <- (num_events %% 2) == 0
if(num_even == TRUE){
  positions <- data.frame(events_xpos = c(rep(0.5, num_events/2), rep(2.6, num_events/2)),
                          events_ypos = rep(seq(4.5, (4.5 - .5*(num_events/2)) + .5, -.5), 2),
                          text_xpos = c(rep(0.55, num_events/2), rep(2.65, num_events/2)))
}else{
  positions <- data.frame(events_xpos = c(rep(0.5, num_events/2 + 1), rep(2.6, num_events/2)),
                          events_ypos = c(seq(4.5, (4.5 - .5*(num_events/2)), -.5), seq(4.5, (4.5 - .5*(num_events/2 - 1)), -.5)),
                          text_xpos = c(rep(0.55, num_events/2 + 1), rep(2.65, num_events/2)))
}
for(i in seq_along(events_vec)) {
  points(positions$events_xpos[i], positions$events_ypos[i], pch = 21, cex = 2, bg = "white")
  text(positions$events_xpos[i], positions$events_ypos[i], i, cex = .7) 
  text(positions$text_xpos[i], positions$events_ypos[i], events_vec_names[i], pos = 4, cex = .75)
}
dev.off()


#### FIGURE 5 ===========================================================================

# prepare data --------------------------------------------------------------------------
turnover <- turnoverRate(parliaments = c("aut", "can", "cze", "fra", "deu", "irl", "sco", 
                                         "esp", "gbr", "usa_house"))
turnover_1 <- tidyr::gather(turnover, group, turnover, alternation:renewal, 
                            factor_key=TRUE)
turnover_1$legislature <- factor(turnover_1$legislature,
                                 levels = c("aut", "can", "cze",
                                            "fra", "deu", "irl",
                                            "sco", "esp", "gbr", "usa_house"),
                                 labels = c("Austria", "Canada", "Czech Republic",
                                            "France", "Germany", "Ireland",
                                            "Scotland", "Spain", "United Kingdom", 
                                            "United States (House)"))
colnames(turnover_1)[4] <- "Turnover"
turnover_1$Turnover <- factor(turnover_1$Turnover,
                              levels = c("alternation", "renewal"),
                              labels = c("Alternation", "Renewal"))
ind_breaks <- function(x) {
  if (min(x) < 1924 & min(x) > 1922) {
    seq(1930, 2010, 20)
  } else if (min(x) < 1873 & min(x) > 1871) {
    seq(1885,2015, 30)
  } else if (min(x) < 1997 & min(x) > 1995) {
    seq(1998,2015,4)
  } else if (min(x) < 1963 & min(x) > 1961) {
    seq(1965,2020,12)
  } else if (min(x) < 1954 & min(x) > 1952) {
    seq(1955,2015,15)
  } else if (min(x) < 1922 & min(x) > 1920) {
    seq(1930,2010,20)
  } else if (min(x) < 2004 & min(x) > 2002) {
    seq(2004,2015,2)
  } else if (min(x) < 1983 & min(x) > 1981) {
    seq(1985,2020,8)
  } else if (min(x) < 1803 & min(x) > 1801) {
    seq(1820,2020,45)
  } else if (min(x) < 1792 & min(x) > 1790) {
    seq(1815,2010,45)
  }
}

# prepare figure ------------------------------------------------------------------------
turnover_plot <- ggplot(data = turnover_1, aes(x = year, y = turnover, 
                                               linetype = Turnover)) +
  geom_line()+
  scale_linetype_manual(values = c(1,2)) +
  facet_wrap(~legislature, scales = "free_x", ncol = 3) +
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
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        #axis.text.x = element_text(angle = 50, hjust = 1),
        # specify axis text size
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 16, color = "black"),
        strip.background = element_blank(),
        # remove box around plot titles
        strip.text = element_text(size=14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 13, margin = margin(l = 5, r = 5)),
        legend.key.width = unit(3, "line")) +
  # specify axis title size and margin
  labs(x = "",
       y = "Share") +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),
                     breaks = seq(0.2,0.8,0.2)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = ind_breaks)
ggsave("./figures/turnover.pdf", turnover_plot, width = 12, height = 10, dpi = 1200, device = cairo_pdf)


# FIGURE A1 =============================================================================

# prepare data --------------------------------------------------------------------------
ts <- lit_survey %>%
  group_by(Year) %>%
  summarize(n = n()) %>%
  filter(Year != 2019)

# prepare figure ------------------------------------------------------------------------
data_demand <- ggplot(data = ts, aes(x = Year, y = n, group = 1)) +
  geom_line(linetype = 2) +
  geom_point(shape = 21, color = "black", fill = "white", size = 1.8) +
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
        axis.text = element_text(size = 12, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 16, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 16, color = "black")) +
  # specify axis title size and margin
  labs(x = "",
       y = "Articles") +
  scale_y_continuous(limits = c(0,30), expand = c(0, 0),
                     breaks = seq(5,25,5)) +
  scale_x_continuous(expand = c(0.001, 0.5),
                     breaks = 2009:2018, labels = c("2009", "2010", "2011", "2012", "2013", "2014", "2015",
                                                    "2016", "2017", "2018"))
ggsave("./figures/data_demand.pdf", data_demand, width = 10, height = 4.5, dpi = 1200, device = cairo_pdf)
