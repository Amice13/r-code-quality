## legislatoR paper --------------------------------
## Sascha Göbel, Simon Munzert
## Explore pageviews (article traffic) for 
## various legislatures
## -------------------------------------------------

## load packages and functions ---------------------
source("packages-add-applications")



### US House and Senate ----------------------------


## get US House data -------------------------------

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


## get US Senate data -------------------------------

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



## merge and clean data ----------------

congress115_df <- bind_rows(senate_df, house_df)
congress115_df$wikidata_id <- str_replace(congress115_df$wikidataid, "Q", "")
congress115_df$wikidataid <- NULL
congress115_df$party[congress115_df$party == "Independent"] <- "I"
congress115_df$party[congress115_df$party == "D/PNP"] <- "D"
congress115_df$party[congress115_df$party == "DFL"] <- "D"
congress115_df$party[congress115_df$party == "DNPL"] <- "D"
congress115_df$party[congress115_df$party == "PNP/R"] <- "R"
congress115_df$dead <- ifelse(!is.na(congress115_df$death), TRUE, FALSE)



## export data -------------------------------         

save(congress115_df, file = "../data/congress115_df.RData")
load("../data/congress115_df.RData")



### top and bottom 10 pageviews for all Congress members ----------

congress115_df <- arrange(congress115_df, desc(traffic_mean))
congress115_df$rank <- 1:nrow(congress115_df)
congress_df_sub <- dplyr::select(congress115_df, rank, name, traffic_mean, traffic_max)
congress_df_sub_table <- congress_df_sub
names(congress_df_sub_table) <- c("Rank", "Senator/Representative", "Mean", "Maximum")
congress_top10 <- head(congress_df_sub_table, 10)
congress_bottom10 <- tail(congress_df_sub_table, 10)
congress_top_bottom_table <- rbind(congress_top10, congress_bottom10)


print(xtable(congress_top_bottom_table, caption = "Top/bottom 10 mean daily page views.\\label{tab:pageviewstopusa}", digits = 0), align = c("l", "r", "r", "r"), booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "t!", include.rownames=FALSE, hline.after = c(-1, seq(0, 20, 5)), file = "../figures/tab-pageviews-usa115.tex")



## OLS models of log pageviews ---------------------------

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
covars_names <- c("Senate", "Sessions served", "Party (Independent)", "Party (Republican)", "Office: Governor", "Office: Lt. Governor", "Office: US Secretary", "Position: House Speaker", "Position: Majority/Minority Leader", "Position: Whip", "Position: Deputy Whip", "Office: Party Chairman", "Male", "Ethnicity: Asian", "Ethnicity: Black", "Ethnicity: Hispanic", "Ethnicity: Other", "(Intercept)")

fmla_traffic <- paste("traffic_mean", paste(covars, collapse = " + "), sep = " ~ ")
summary(traffic_model <- lm(fmla_traffic, congress115_df))

fmla_traffic_log <- paste("traffic_mean_log", paste(covars, collapse = " + "), sep = " ~ ")
summary(log_traffic_model <- lm(fmla_traffic_log, congress115_df))

# export table
texreg(log_traffic_model, custom.coef.names = covars_names, custom.model.names = "Log page views", table = TRUE, single.row = TRUE, booktabs = TRUE, caption.above = TRUE, use.packages = FALSE, dcolumn = TRUE, fontsize = "scriptsize", float.pos = "htb", caption = "OLS estimates of log mean daily Wikipedia pageviews of members of 115th US Congress.", label = "tab:usa115-models", file = "../figures/tab-models-usa115.tex")



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

# print plot
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


## Visualize time series of page traffic data, aggregated -----------------------------------

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


# plot
pdf(file="../figures/traffic-us.pdf", height=3, width=8, family="URWTimes")
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



### GER ----------------------------


## get German Bundestag data -------------------------------

# get political data
political_df <- left_join(x = filter(get_political(legislature = "deu"), as.numeric(session) == 18),
                          y = get_core(legislature = "deu"), 
                          by = "pageid")

# get traffic data
traffic_df <- get_traffic(legislature = "deu") %>% 
  filter(date >= "2013-10-22" & date <= "2017-10-24") %>% 
  group_by(pageid) %>% 
  summarize(traffic_mean = mean(traffic, na.rm = TRUE),
            traffic_max = max(traffic, na.rm = TRUE))

# get offices data
offices_df <- get_office(legislature = "deu") 
offices_df$office_ambassador <- rowSums(offices_df[,str_detect(names(offices_df), "ambassador")]) >= 1
offices_df$office_secretary <- rowSums(offices_df[,str_detect(names(offices_df), "bundesminister|federal_minister|secretary_of_state")]) >= 1
offices_df$office_governor <- rowSums(offices_df[,str_detect(names(offices_df), "minister.president")]) >= 1
offices_df$office_mayor <- rowSums(offices_df[,str_detect(names(offices_df), "mayor")]) >= 1
offices_df$position_party_chairman <- rowSums(offices_df[,str_detect(names(offices_df), "chairman|party_leader")]) >= 1
offices_df <- dplyr::select(offices_df, wikidataid, office_ambassador, office_secretary, office_governor, office_mayor, position_party_chairman)

# get data on sessions served
sessions_served_df <- get_political(legislature = "deu") %>% group_by(pageid) %>% dplyr::summarize(sessions_served = n())

# merge
legislator_df <- left_join(political_df, sessions_served_df, by = "pageid") %>% left_join(offices_df, by = "wikidataid") %>% left_join(traffic_df, by = "pageid") 

# add age and chamber variables
legislator_df$age <- get_age(legislator_df$birth, as.POSIXct("2017-10-24"))

# only keep last session of each member
legislator_df <- legislator_df %>% group_by(wikidataid) %>% slice(which.max(session)) %>% ungroup()

# wikidata id
legislator_df$wikidata_id <- str_replace(legislator_df$wikidataid, "Q", "")
legislator_df$wikidataid <- NULL

# death
legislator_df$dead <- ifelse(!is.na(legislator_df$death), TRUE, FALSE)



## export data -------------------------------         

deu17_df <- legislator_df
save(deu17_df, file = "../data/deu17_df.RData")
load("../data/deu17_df.RData")
legislator_df <- deu17_df
header <- "members of the 17th German Bundestag (2013--2017)"


### top and bottom 10 pageviews ----------

legislator_df <- arrange(legislator_df, desc(traffic_mean))
legislator_df$rank <- 1:nrow(legislator_df)
legislator_df_sub <- dplyr::select(legislator_df, rank, name, traffic_mean, traffic_max)
legislator_df_sub_table <- filter(legislator_df_sub, !is.na(traffic_mean))
names(legislator_df_sub_table) <- c("Rank", "Legislator", "Mean", "Maximum")
legislator_top10 <- head(legislator_df_sub_table, 10)
legislator_bottom10 <- tail(legislator_df_sub_table, 10)
legislator_top_bottom_table <- rbind(legislator_top10, legislator_bottom10)

print(xtable(legislator_top_bottom_table, caption = paste0("Top/bottom 10 mean daily page views for ", header, ".\\label{tab:pageviewstopdeu}"), digits = 0), align = c("l", "r", "r", "r"), booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "htb", include.rownames=FALSE, floating = TRUE, hline.after = c(-1, seq(0, 20, 5)), file = "../figures/tab-pageviews-deu17.tex")



## OLS models of log pageviews ---------------------------

legislator_df$age_log <- log(legislator_df$age)
legislator_df$sessions_served_log <- log(legislator_df$sessions_served)
legislator_df$traffic_mean_log <- log(legislator_df$traffic_mean)

covars <- c("sessions_served_log", "party", "office_secretary", "office_mayor", "position_party_chairman", "sex", "dead", "age_log")
covars_names <- c("Sessions served", "Party: CDU", "Party: CSU", "Party: Left", "Party: None", "Party: SPD", "Office: Secretary",  "Office: Mayor", "Office: Party Chairman", "Male", "Dead", "Age", "(Intercept)")

fmla_traffic <- paste("traffic_mean", paste(covars, collapse = " + "), sep = " ~ ")
summary(traffic_model <- lm(fmla_traffic, legislator_df))

fmla_traffic_log <- paste("traffic_mean_log", paste(covars, collapse = " + "), sep = " ~ ")
summary(log_traffic_model <- lm(fmla_traffic_log, legislator_df))

# export table
texreg(log_traffic_model, custom.coef.names = covars_names, custom.model.names = "Log page views", table = TRUE, single.row = TRUE, booktabs = TRUE, caption.above = TRUE, use.packages = FALSE, dcolumn = TRUE, fontsize = "scriptsize", float.pos = "htb", caption = paste0("OLS estimates of log page views of ", header, "."), label = "tab:deu17-models", file = "../figures/tab-models-deu17.tex")




## Visualize time series of page traffic data, aggregated -----------------------------------

# get data
traffic_df <- get_traffic(legislature = "deu") %>% 
  filter(date >= "2013-10-22" & date <= "2017-10-24") %>%
  right_join(y = filter(get_political(legislature = "deu"), as.numeric(session) == 17), by = "pageid") %>%
  left_join(y = get_core(legislature = "deu"), by = "pageid") %>%
  dplyr::select(pageid, date, traffic, session, party, name)

# aggregate data
traffic_df$date <- ymd(traffic_df$date)
traffic_df_date <- group_by(traffic_df, date)
traffic_df_legislators <- group_by(traffic_df, pageid)
traffic_df_sum <- summarize(traffic_df_date, mean = mean(traffic, na.rm = TRUE))
traffic_df_sum <- mutate(traffic_df_sum, 
                         mean_l1 = lag(mean, 1), 
                         mean_f1 = lead(mean, 1),
                         peak = (mean >= 1.8*mean_l1 & mean > 200))


# identify peaks
traffic_df_peaks <- filter(traffic_df_sum, peak == TRUE)
traffic_df_peaks_df <- filter(traffic_df, date %in% traffic_df_peaks$date)
traffic_df_peaks_group <- group_by(traffic_df_peaks_df, date) %>% dplyr::arrange(desc(traffic)) %>% filter(row_number()==1)
traffic_df_peaks_group <- arrange(traffic_df_peaks_group, date)
events_vec <- c("Appointed first female defence minister", "Deceased", "Drug affair", "Deceased", "Bullying affair", "Candidacy for presidency", "Deceased", "Unidentifiable cause", "Chancellorchip announcement", "Elected federal president", "State election North-Rhine Westphalia", "Policy success", "TV debate", "General election", "Threat to resign")

events_vec_names <- paste0(events_vec, " (", traffic_df_peaks_group$name, ")")
date_range <- ymd(range(traffic_df$date, na.rm = TRUE))



# plot
pdf(file="../figures/traffic-deu.pdf", height=3.2, width=8, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(0,2,0,.5))
par(yaxs="i", xaxs="i", bty="n")
layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), 
       heights = c(1, .8), widths = 5)
# page views time series
par(mar=c(2,4,0,.5))
plot(ymd(traffic_df_sum$date), traffic_df_sum$mean, type = "l", ylim= c(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE)), xlim = date_range + c(0, 14),  xaxt = "n", yaxt = "n", xlab = "", ylab = "Avg. page views", col = "white")
#abline(v = dates[day(dates) == 1 & month(dates) %in% c(1)], col = "lightgrey")
abline(h = seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), col = "lightgrey")
lines(ymd(traffic_df_sum$date), traffic_df_sum$mean, lwd = .5)
dates <- seq(date_range[1], date_range[2], by = 1)
axis(1, dates[day(dates) == 1], labels = FALSE, cex = .7, tck=-0.03)
axis(1, dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], labels = as.character(month(dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], label = TRUE, abbr = TRUE)), tick = F, lwd = 0, cex.axis = .8, line = -1)
axis(1, dates[day(dates) == 1 & month(dates) %in% c(7)], labels = as.character(year(dates[day(dates) == 15 & month(dates) %in% c(7)])), tick = F, lwd = 0, cex.axis = .8)
axis(2, seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), las = 2, cex.axis = .8)
# events labels in time series
gap_size <- rep(100, length(events_vec))
for(i in seq_along(events_vec)) {
  points(traffic_df_peaks_group$date[i], traffic_df_sum$mean[traffic_df_sum$peak == TRUE][i] + gap_size[i], pch=21, cex = 2.2, bg = "white")  
  text(traffic_df_peaks_group$date[i], traffic_df_sum$mean[traffic_df_sum$peak == TRUE][i] + gap_size[i], i, cex = .7)
}
# events labels explained
par(mar=c(0,0,0,0))
plot(0, 0, xlim = c(0, 5), ylim = c(.5, 5), xaxt = "n", yaxt = "n", xlab = "", ylab = "", cex = 0)
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


### AUT ----------------------------


## get Austrian data -------------------------------

session_num <- 26

# get political data
political_df <- left_join(x = filter(get_political(legislature = "aut"), as.numeric(session) == session_num),
                          y = get_core(legislature = "aut"), 
                          by = "pageid")

# get traffic data
traffic_df <- get_traffic(legislature = "aut") %>% 
  filter(date >= political_df$session_start[1] & date <= political_df$session_end[1] + 1) %>% 
  group_by(pageid) %>% 
  summarize(traffic_mean = mean(traffic, na.rm = TRUE),
            traffic_max = max(traffic, na.rm = TRUE))

# get offices data
offices_df <- get_office(legislature = "aut") 
offices_df$office_ambassador <- rowSums(offices_df[,str_detect(names(offices_df), "ambassador")]) >= 1
offices_df$office_secretary <- rowSums(offices_df[,str_detect(names(offices_df), "minister")]) >= 1
offices_df$office_governor <- rowSums(offices_df[,str_detect(names(offices_df), "landeshauptmann")]) >= 1
offices_df$office_mayor <- rowSums(offices_df[,str_detect(names(offices_df), "mayor")]) >= 1
offices_df$position_party_chairman <- rowSums(offices_df[,str_detect(names(offices_df), "chairman|party_leader")]) >= 1
offices_df <- dplyr::select(offices_df, wikidataid, office_ambassador, office_secretary, office_governor, office_mayor, position_party_chairman)

# get data on sessions served
sessions_served_df <- get_political(legislature = "aut") %>% group_by(pageid) %>% dplyr::summarize(sessions_served = n())

# merge
legislator_df <- left_join(political_df, sessions_served_df, by = "pageid") %>% left_join(offices_df, by = "wikidataid") %>% left_join(traffic_df, by = "pageid") 

# add age and chamber variables
legislator_df$age <- get_age(legislator_df$birth, as.POSIXct(political_df$session_end[1]))

# only keep last session of each member
legislator_df <- legislator_df %>% group_by(wikidataid) %>% slice(which.max(session)) %>% ungroup()

# wikidata id
legislator_df$wikidata_id <- str_replace(legislator_df$wikidataid, "Q", "")
legislator_df$wikidataid <- NULL

# death
legislator_df$dead <- ifelse(!is.na(legislator_df$death), TRUE, FALSE)



## export data -------------------------------         

aut26_df <- legislator_df
save(aut26_df, file = "../data/aut26_df.RData")
load("../data/aut26_df.RData")
legislator_df <- aut26_df
header <- "members of the 26th National Council of Austria (2017--2019)"


### top and bottom 10 pageviews ----------

legislator_df <- arrange(legislator_df, desc(traffic_mean))
legislator_df$rank <- 1:nrow(legislator_df)
legislator_df_sub <- dplyr::select(legislator_df, rank, name, traffic_mean, traffic_max)
legislator_df_sub_table <- filter(legislator_df_sub, !is.na(traffic_mean))
names(legislator_df_sub_table) <- c("Rank", "Legislator", "Mean", "Maximum")
legislator_top10 <- head(legislator_df_sub_table, 10)
legislator_bottom10 <- tail(legislator_df_sub_table, 10)
legislator_top_bottom_table <- rbind(legislator_top10, legislator_bottom10)

print(xtable(legislator_top_bottom_table, caption = paste0("Top/bottom 10 mean daily page views for ", header, ".\\label{tab:pageviewstopaut}"), digits = 0), align = c("l", "r", "r", "r"), booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "htb", include.rownames=FALSE, hline.after = c(-1, seq(0, 20, 5)), file = "../figures/tab-pageviews-aut26.tex")



## OLS models of log pageviews ---------------------------

legislator_df$age_log <- log(legislator_df$age)
legislator_df$sessions_served_log <- log(legislator_df$sessions_served)
legislator_df$traffic_mean_log <- log(legislator_df$traffic_mean)

covars <- c("sessions_served_log", "party", "office_secretary", "office_mayor", "position_party_chairman", "sex", "age_log")
covars_names <- c("Sessions served", "Party: NEOS", "Party: ÖVP", "Party: PILZ", "Party: SPÖ", "Office: Secretary",  "Office: Mayor", "Office: Party Chairman", "Male", "Age", "(Intercept)")

fmla_traffic <- paste("traffic_mean", paste(covars, collapse = " + "), sep = " ~ ")
summary(traffic_model <- lm(fmla_traffic, legislator_df))

fmla_traffic_log <- paste("traffic_mean_log", paste(covars, collapse = " + "), sep = " ~ ")
summary(log_traffic_model <- lm(fmla_traffic_log, legislator_df))

# export table
texreg(log_traffic_model, custom.coef.names = covars_names, custom.model.names = "Log page views", table = TRUE, single.row = TRUE, booktabs = TRUE, caption.above = TRUE, use.packages = FALSE, dcolumn = TRUE, fontsize = "scriptsize", float.pos = "htb", caption = paste0("OLS estimates of log page views of ", header, "."), label = "tab:aut26-models", file = "../figures/tab-models-aut26.tex")



## Visualize time series of page traffic data, aggregated -----------------------------------

# get data
traffic_df <- get_traffic(legislature = "aut") %>% 
  filter(date >= political_df$session_start[1] & date <= political_df$session_end[1]) %>%
  right_join(y = filter(get_political(legislature = "aut"), as.numeric(session) == session_num), by = "pageid") %>%
  left_join(y = get_core(legislature = "aut"), by = "pageid") %>%
  dplyr::select(pageid, date, traffic, session, party, name)

# aggregate data
traffic_df$date <- ymd(traffic_df$date)
traffic_df_date <- group_by(traffic_df, date)
traffic_df_legislators <- group_by(traffic_df, pageid)
traffic_df_sum <- summarize(traffic_df_date, mean = mean(traffic, na.rm = TRUE))
traffic_df_sum <- mutate(traffic_df_sum, 
                         mean_l1 = lag(mean, 1), 
                         mean_f1 = lead(mean, 1),
                         peak = (mean >= 1.8*mean_l1 & mean > 180))


# identify peaks
traffic_df_peaks <- filter(traffic_df_sum, peak == TRUE)
traffic_df_peaks_df <- filter(traffic_df, date %in% traffic_df_peaks$date)
traffic_df_peaks_group <- group_by(traffic_df_peaks_df, date) %>% dplyr::arrange(desc(traffic)) %>% filter(row_number()==1)
traffic_df_peaks_group <- arrange(traffic_df_peaks_group, date) %>% filter(date != "2019-05-17") %>% left_join(traffic_df_peaks, by = "date")
events_vec <- c("Inauguration (President of the National Council)", "Inauguration (Chancellor)", "Inauguration of government", "Press conference with Angela Merkel", "Announcement of party leadership", "Ibiza affair", "Motion of no confidence", "Federal election")

events_vec_names <- paste0(events_vec, " (", traffic_df_peaks_group$name, ")")
date_range <- ymd(range(traffic_df$date, na.rm = TRUE))



# plot
pdf(file="../figures/traffic-aut.pdf", height=3.2, width=8, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(0,2,0,.5))
par(yaxs="i", xaxs="i", bty="n")
layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), 
       heights = c(1, .5), widths = 5)
# page views time series
par(mar=c(2,4,0,.5))
plot(ymd(traffic_df_sum$date), traffic_df_sum$mean, type = "l", ylim= c(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE)), xlim = date_range + c(0, 14),  xaxt = "n", yaxt = "n", xlab = "", ylab = "Avg. page views", col = "white")
#abline(v = dates[day(dates) == 1 & month(dates) %in% c(1)], col = "lightgrey")
abline(h = seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), col = "lightgrey")
lines(ymd(traffic_df_sum$date), traffic_df_sum$mean, lwd = .5)
dates <- seq(date_range[1], date_range[2], by = 1)
axis(1, dates[day(dates) == 1], labels = FALSE, cex = .7, tck=-0.03)
axis(1, dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], labels = as.character(month(dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], label = TRUE, abbr = TRUE)), tick = F, lwd = 0, cex.axis = .8, line = -1)
axis(1, dates[day(dates) == 1 & month(dates) %in% c(7)], labels = as.character(year(dates[day(dates) == 15 & month(dates) %in% c(7)])), tick = F, lwd = 0, cex.axis = .8)
axis(2, seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), las = 2, cex.axis = .8)
# events labels in time series
gap_size <- 150 
for(i in seq_along(events_vec)) {
  points(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size, pch=21, cex = 2.2, bg = "white")  
  text(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size, i, cex = .7)
}
# events labels explained
par(mar=c(0,0,0,0))
plot(0, 0, xlim = c(0, 5), ylim = c(2.5, 5), xaxt = "n", yaxt = "n", xlab = "", ylab = "", cex = 0)
num_events <- length(events_vec)
num_even <- (num_events %% 2) == 0
if(num_even == TRUE){
  positions <- data.frame(events_xpos = c(rep(0.5, num_events/2), rep(2.9, num_events/2)),
                          events_ypos = rep(seq(4.5, (4.5 - .5*(num_events/2)) + .5, -.5), 2),
                          text_xpos = c(rep(0.55, num_events/2), rep(2.95, num_events/2)))
}else{
  positions <- data.frame(events_xpos = c(rep(0.5, num_events/2 + 1), rep(2.9, num_events/2)),
                          events_ypos = c(seq(4.5, (4.5 - .5*(num_events/2)), -.5), seq(4.5, (4.5 - .5*(num_events/2 - 1)), -.5)),
                          text_xpos = c(rep(0.55, num_events/2 + 1), rep(2.95, num_events/2)))
}
for(i in seq_along(events_vec)) {
  points(positions$events_xpos[i], positions$events_ypos[i], pch = 21, cex = 2, bg = "white")
  text(positions$events_xpos[i], positions$events_ypos[i], i, cex = .7) 
  text(positions$text_xpos[i], positions$events_ypos[i], events_vec_names[i], pos = 4, cex = .75)
}
dev.off()






### CAN ----------------------------

## get Canadian data -------------------------------

session_num <- 42

# get political data
political_df <- left_join(x = filter(get_political(legislature = "can"), as.numeric(session) == session_num),
                          y = get_core(legislature = "can"), 
                          by = "pageid")

# get traffic data
traffic_df <- get_traffic(legislature = "can") %>% 
  filter(date >= political_df$session_start[1] & date <= political_df$session_end[1] + 1) %>% 
  group_by(pageid) %>% 
  summarize(traffic_mean = mean(traffic, na.rm = TRUE),
            traffic_max = max(traffic, na.rm = TRUE))

# get offices data
offices_df <- get_office(legislature = "can") 
offices_df$office_ambassador <- rowSums(offices_df[,str_detect(names(offices_df), "ambassador")]) >= 1
offices_df$office_secretary <- rowSums(offices_df[,str_detect(names(offices_df), "minister")]) >= 1
offices_df$office_governor <- rowSums(offices_df[,str_detect(names(offices_df), "governor|premier")]) >= 1
offices_df$office_mayor <- rowSums(offices_df[,str_detect(names(offices_df), "mayor")]) >= 1
offices_df$position_party_chairman <- rowSums(offices_df[,str_detect(names(offices_df), "chairman|party_leader|party|leader")]) >= 1
offices_df <- dplyr::select(offices_df, wikidataid, office_ambassador, office_secretary, office_governor, office_mayor, position_party_chairman)

# get data on sessions served
sessions_served_df <- get_political(legislature = "can") %>% group_by(pageid) %>% dplyr::summarize(sessions_served = n())

# merge
legislator_df <- left_join(political_df, sessions_served_df, by = "pageid") %>% left_join(offices_df, by = "wikidataid") %>% left_join(traffic_df, by = "pageid") 

# add age and chamber variables
legislator_df$age <- get_age(legislator_df$birth, as.POSIXct(political_df$session_end[1]))

# only keep last session of each member
legislator_df <- legislator_df %>% group_by(wikidataid) %>% slice(which.max(session)) %>% ungroup()

# wikidata id
legislator_df$wikidata_id <- str_replace(legislator_df$wikidataid, "Q", "")
legislator_df$wikidataid <- NULL

# death
legislator_df$dead <- ifelse(!is.na(legislator_df$death), TRUE, FALSE)



## export data -------------------------------         

can42_df <- legislator_df
save(can42_df, file = "../data/can42_df.RData")
load("../data/can42_df.RData")
legislator_df <- can42_df
header <- "members of the 42nd Parliament of Canada (2015--2019)"


### top and bottom 10 pageviews ----------

legislator_df <- arrange(legislator_df, desc(traffic_mean))
legislator_df$rank <- 1:nrow(legislator_df)
legislator_df_sub <- dplyr::select(legislator_df, rank, name, traffic_mean, traffic_max)
legislator_df_sub_table <- filter(legislator_df_sub, !is.na(traffic_mean))
names(legislator_df_sub_table) <- c("Rank", "Legislator", "Mean", "Maximum")
legislator_top10 <- head(legislator_df_sub_table, 10)
legislator_bottom10 <- tail(legislator_df_sub_table, 10)
legislator_top_bottom_table <- rbind(legislator_top10, legislator_bottom10)

print(xtable(legislator_top_bottom_table, caption = paste0("Top/bottom 10 mean daily page views for ", header, "\\label{tab:pageviewstopcan}"), digits = 0), align = c("l", "r", "r", "r"), booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "htb", include.rownames=FALSE, hline.after = c(-1, seq(0, 20, 5)), file = "../figures/tab-pageviews-can42.tex")



## OLS models of log pageviews ---------------------------

legislator_df$age_log <- log(legislator_df$age)
legislator_df$sessions_served_log <- log(legislator_df$sessions_served)
legislator_df$traffic_mean_log <- log(legislator_df$traffic_mean)

covars <- c("sessions_served_log", "party", "office_secretary", "office_mayor", "position_party_chairman", "sex", "age_log")
covars_names <- c("Sessions served", "Party: Conservative", "Party: Green", "Party: Liberal", "Party: New Democratic", "Office: Secretary",  "Office: Mayor", "Office: Party Chairman", "Male", "Age", "(Intercept)")

fmla_traffic <- paste("traffic_mean", paste(covars, collapse = " + "), sep = " ~ ")
summary(traffic_model <- lm(fmla_traffic, legislator_df))

fmla_traffic_log <- paste("traffic_mean_log", paste(covars, collapse = " + "), sep = " ~ ")
summary(log_traffic_model <- lm(fmla_traffic_log, legislator_df))

# export table
texreg(log_traffic_model, custom.coef.names = covars_names, custom.model.names = "Log page views", table = TRUE, single.row = TRUE, booktabs = TRUE, caption.above = TRUE, use.packages = FALSE, dcolumn = TRUE, fontsize = "scriptsize", float.pos = "htb", caption = paste0("OLS estimates of log page views of ", header, "."), label = "tab:can42-models", file = "../figures/tab-models-can42.tex")



## Visualize time series of page traffic data, aggregated -----------------------------------

# get data
traffic_df <- get_traffic(legislature = "can") %>% 
  filter(date >= political_df$session_start[1] & date <= political_df$session_end[1]) %>%
  right_join(y = filter(get_political(legislature = "can"), as.numeric(session) == session_num), by = "pageid") %>%
  left_join(y = get_core(legislature = "can"), by = "pageid") %>%
  dplyr::select(pageid, date, traffic, session, party, name)

# aggregate data
traffic_df$date <- ymd(traffic_df$date)
traffic_df_date <- group_by(traffic_df, date)
traffic_df_legislators <- group_by(traffic_df, pageid)
traffic_df_sum <- summarize(traffic_df_date, mean = mean(traffic, na.rm = TRUE))
traffic_df_sum <- mutate(traffic_df_sum, 
                         mean_l1 = lag(mean, 1), 
                         mean_f1 = lead(mean, 1),
                         peak = (mean >= 1.8*mean_l1 & mean > 200))
traffic_df_sum <- filter(traffic_df_sum, date >= "2015-10-22")

# identify peaks
traffic_df_peaks <- filter(traffic_df_sum, peak == TRUE)
traffic_df_peaks_df <- filter(traffic_df, date %in% traffic_df_peaks$date)
traffic_df_peaks_group <- group_by(traffic_df_peaks_df, date) %>% dplyr::arrange(desc(traffic)) %>% filter(row_number()==1)
traffic_df_peaks_group <- arrange(traffic_df_peaks_group, date) %>% filter(date > "2015-10-20") %>% left_join(traffic_df_peaks, by = "date")
events_vec <- c("Inauguration (prime minister)", "State visit (Barack Obama)", "Quantum computers remarks", "Statement on US Presidential election", "Appointed minister of foreign affairs", "Québec city mosque shooting", "State visit (Donald Trump)", "Apology for discrimination against LGBT people", "Appointed leader of conservative party", "Appointed leader of new democratic party", "Trump meeting on G7 Summit")

events_vec_names <- paste0(events_vec, " (", traffic_df_peaks_group$name, ")")
date_range <- ymd(range(traffic_df$date, na.rm = TRUE))



# plot
pdf(file="../figures/traffic-can.pdf", height=3.2, width=8, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(0,2,0,.5))
par(yaxs="i", xaxs="i", bty="n")
layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), 
       heights = c(1, .65), widths = 5)
# page views time series
par(mar=c(2,4,0,.5))
plot(ymd(traffic_df_sum$date), traffic_df_sum$mean, type = "l", ylim= c(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE)), xlim = date_range + c(0, 14),  xaxt = "n", yaxt = "n", xlab = "", ylab = "Avg. page views", col = "white")
#abline(v = dates[day(dates) == 1 & month(dates) %in% c(1)], col = "lightgrey")
abline(h = seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), col = "lightgrey")
lines(ymd(traffic_df_sum$date), traffic_df_sum$mean, lwd = .5)
dates <- seq(date_range[1], date_range[2], by = 1)
axis(1, dates[day(dates) == 1], labels = FALSE, cex = .7, tck=-0.03)
axis(1, dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], labels = as.character(month(dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], label = TRUE, abbr = TRUE)), tick = F, lwd = 0, cex.axis = .8, line = -1)
axis(1, dates[day(dates) == 1 & month(dates) %in% c(7)], labels = as.character(year(dates[day(dates) == 15 & month(dates) %in% c(7)])), tick = F, lwd = 0, cex.axis = .8)
axis(2, seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), las = 2, cex.axis = .8)
# events labels in time series
gap_size <- 150 
for(i in seq_along(events_vec)) {
  points(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size, pch=21, cex = 2.2, bg = "white")  
  text(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size, i, cex = .7)
}
# events labels explained
par(mar=c(0,0,0,0))
plot(0, 0, xlim = c(0, 5), ylim = c(1.5, 5), xaxt = "n", yaxt = "n", xlab = "", ylab = "", cex = 0)
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






### CZE ----------------------------

## get Czech data -------------------------------

session_num <- 7

# get political data
political_df <- left_join(x = filter(get_political(legislature = "cze"), as.numeric(session) == session_num),
                          y = get_core(legislature = "cze"), 
                          by = "pageid")

# get traffic data
traffic_df <- get_traffic(legislature = "cze") %>% 
  filter(date >= political_df$session_start[1] & date <= political_df$session_end[1] + 1) %>% 
  group_by(pageid) %>% 
  summarize(traffic_mean = mean(traffic, na.rm = TRUE),
            traffic_max = max(traffic, na.rm = TRUE))

# get offices data
offices_df <- get_office(legislature = "cze") 
offices_df$office_ambassador <- rowSums(offices_df[,str_detect(names(offices_df), "ambassador")]) >= 1
offices_df$office_secretary <- rowSums(offices_df[,str_detect(names(offices_df), "minister")]) >= 1
offices_df$office_governor <- rowSums(offices_df[,str_detect(names(offices_df), "governor|prime")]) >= 1
offices_df$office_mayor <- rowSums(offices_df[,str_detect(names(offices_df), "mayor")]) >= 1
#offices_df$position_party_chairman <- rowSums(offices_df[,str_detect(names(offices_df), "chairman|party_leader|party|leader")]) >= 1
offices_df <- dplyr::select(offices_df, wikidataid, office_ambassador, office_secretary, office_governor, office_mayor)

# get data on sessions served
sessions_served_df <- get_political(legislature = "cze") %>% group_by(pageid) %>% dplyr::summarize(sessions_served = n())

# merge
legislator_df <- left_join(political_df, sessions_served_df, by = "pageid") %>% left_join(offices_df, by = "wikidataid") %>% left_join(traffic_df, by = "pageid") 

# add age and chamber variables
legislator_df$age <- get_age(legislator_df$birth, as.POSIXct(political_df$session_end[1]))

# only keep last session of each member
legislator_df <- legislator_df %>% group_by(wikidataid) %>% slice(which.max(session)) %>% ungroup()

# wikidata id
legislator_df$wikidata_id <- str_replace(legislator_df$wikidataid, "Q", "")
legislator_df$wikidataid <- NULL

# death
legislator_df$dead <- ifelse(!is.na(legislator_df$death), TRUE, FALSE)



## export data -------------------------------         

cze7_df <- legislator_df
save(cze7_df, file = "../data/cze7_df.RData")
load("../data/cze7_df.RData")
legislator_df <- cze7_df
header <- "members of the 7th Parliament of the Czech Republic (2013--2017)"


### top and bottom 10 pageviews ----------

legislator_df <- arrange(legislator_df, desc(traffic_mean))
legislator_df$rank <- 1:nrow(legislator_df)
legislator_df_sub <- dplyr::select(legislator_df, rank, name, traffic_mean, traffic_max)
legislator_df_sub_table <- filter(legislator_df_sub, !is.na(traffic_mean))
names(legislator_df_sub_table) <- c("Rank", "Legislator", "Mean", "Maximum")
legislator_top10 <- head(legislator_df_sub_table, 10)
legislator_bottom10 <- tail(legislator_df_sub_table, 10)
legislator_top_bottom_table <- rbind(legislator_top10, legislator_bottom10)

print(xtable(legislator_top_bottom_table, caption = paste0("Top/bottom 10 mean daily page views for ", header, ".\\label{tab:pageviewstopcze}"), digits = 0), align = c("l", "r", "r", "r"), booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "htb", include.rownames=FALSE, hline.after = c(-1, seq(0, 20, 5)), file = "../figures/tab-pageviews-cze7.tex")



## OLS models of log pageviews ---------------------------

legislator_df$age_log <- log(legislator_df$age)
legislator_df$sessions_served_log <- log(legislator_df$sessions_served)
legislator_df$traffic_mean_log <- log(legislator_df$traffic_mean)

covars <- c("sessions_served_log", "party", "office_secretary", "office_mayor", "sex", "age_log")
covars_names <- c("Sessions served", "Party: CSSD", "Party: KDU-CSL", "Party: KSCM", "Party: ODS", "Party: TOP 09", "Party: Usvit", "Office: Secretary", "Office: Mayor", "Male", "Age", "(Intercept)")

fmla_traffic <- paste("traffic_mean", paste(covars, collapse = " + "), sep = " ~ ")
summary(traffic_model <- lm(fmla_traffic, legislator_df))

fmla_traffic_log <- paste("traffic_mean_log", paste(covars, collapse = " + "), sep = " ~ ")
summary(log_traffic_model <- lm(fmla_traffic_log, legislator_df))

# export table
texreg(log_traffic_model, custom.coef.names = covars_names, custom.model.names = "Log page views", table = TRUE, single.row = TRUE, booktabs = TRUE, caption.above = TRUE, use.packages = FALSE, dcolumn = TRUE, fontsize = "scriptsize", float.pos = "htb", caption = paste0("OLS estimates of log page views of ", header, "."), label = "tab:cze7-models", file = "../figures/tab-models-cze7.tex")


## Visualize time series of page traffic data, aggregated -----------------------------------

# get data
traffic_df <- get_traffic(legislature = "cze") %>% 
  filter(date >= political_df$session_start[1] & date <= political_df$session_end[1]) %>%
  right_join(y = filter(get_political(legislature = "cze"), as.numeric(session) == session_num), by = "pageid") %>%
  left_join(y = get_core(legislature = "cze"), by = "pageid") %>%
  dplyr::select(pageid, date, traffic, session, party, name)

# aggregate data
traffic_df$date <- ymd(traffic_df$date)
traffic_df_date <- group_by(traffic_df, date)
traffic_df_legislators <- group_by(traffic_df, pageid)
traffic_df_sum <- summarize(traffic_df_date, mean = mean(traffic, na.rm = TRUE))
traffic_df_sum <- mutate(traffic_df_sum, 
                         mean_l1 = lag(mean, 1), 
                         mean_f1 = lead(mean, 1),
                         peak = (mean >= 1.8*mean_l1 & mean > 50))
#traffic_df_sum <- filter(traffic_df_sum, date >= "2015-10-22")

# identify peaks
traffic_df_peaks <- filter(traffic_df_sum, peak == TRUE)
traffic_df_peaks_df <- filter(traffic_df, date %in% traffic_df_peaks$date)
traffic_df_peaks_group <- group_by(traffic_df_peaks_df, date) %>% dplyr::arrange(desc(traffic)) %>% filter(row_number()==1)
traffic_df_peaks_group <- arrange(traffic_df_peaks_group, date)  %>% left_join(traffic_df_peaks, by = "date") %>% filter(date != "2017-10-16", date != "2017-10-19")
events_vec <- c("Appointed Minister of Finance", "Regional elections", "Resignation as Prime Minister", "Mass protests", "Stripped of immunity (stork nest affair)", "General election")

events_vec_names <- paste0(events_vec, " (", traffic_df_peaks_group$name, ")")
date_range <- ymd(range(traffic_df$date, na.rm = TRUE))



# plot
pdf(file="../figures/traffic-cze.pdf", height=2.8, width=8, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(0,2,0,.5))
par(yaxs="i", xaxs="i", bty="n")
layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), 
       heights = c(1, .45), widths = 5)
# page views time series
par(mar=c(2,4,0,.5))
plot(ymd(traffic_df_sum$date), traffic_df_sum$mean, type = "l", ylim= c(0, 1.5*max(traffic_df_sum$mean, na.rm = TRUE)), xlim = date_range + c(0, 21),  xaxt = "n", yaxt = "n", xlab = "", ylab = "Avg. page views", col = "white")
#abline(v = dates[day(dates) == 1 & month(dates) %in% c(1)], col = "lightgrey")
abline(h = seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), col = "lightgrey")
lines(ymd(traffic_df_sum$date), traffic_df_sum$mean, lwd = .5)
dates <- seq(date_range[1], date_range[2], by = 1)
axis(1, dates[day(dates) == 1], labels = FALSE, cex = .7, tck=-0.03)
axis(1, dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], labels = as.character(month(dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], label = TRUE, abbr = TRUE)), tick = F, lwd = 0, cex.axis = .8, line = -1)
axis(1, dates[day(dates) == 1 & month(dates) %in% c(7)], labels = as.character(year(dates[day(dates) == 15 & month(dates) %in% c(7)])), tick = F, lwd = 0, cex.axis = .8)
axis(2, seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), las = 2, cex.axis = .8)
# events labels in time series
gap_size <- 50 
for(i in seq_along(events_vec)) {
  points(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size, pch=21, cex = 2.2, bg = "white")  
  text(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size, i, cex = .7)
}
# events labels explained
par(mar=c(0,0,0,0))
plot(0, 0, xlim = c(0, 5), ylim = c(2.5, 5), xaxt = "n", yaxt = "n", xlab = "", ylab = "", cex = 0)
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




### ESP ----------------------------

## get esp data -------------------------------

session_num <- 12

# get political data
political_df <- left_join(x = filter(get_political(legislature = "esp"), as.numeric(session) == session_num),
                          y = get_core(legislature = "esp"), 
                          by = "pageid")

# get traffic data
traffic_df <- get_traffic(legislature = "esp") %>% 
  dplyr::filter(date >= political_df$session_start[1] & date <= political_df$session_end[1]) %>% 
  group_by(pageid) %>% 
  summarize(traffic_mean = mean(traffic, na.rm = TRUE),
            traffic_max = max(traffic, na.rm = TRUE))

# get offices data
offices_df <- get_office(legislature = "esp") 
offices_df$office_ambassador <- rowSums(offices_df[,str_detect(names(offices_df), "ambassador")]) >= 1
offices_df$office_secretary <- rowSums(offices_df[,str_detect(names(offices_df), "minister|secretary_of_state")]) >= 1
#offices_df$office_governor <- rowSums(offices_df[,str_detect(names(offices_df), "governor|prime")]) >= 1
offices_df$office_mayor <- rowSums(offices_df[,str_detect(names(offices_df), "mayor")]) >= 1
#offices_df$position_party_chairman <- rowSums(offices_df[,str_detect(names(offices_df), "parliamentary_group_leader")]) >= 1
offices_df <- dplyr::select(offices_df, wikidataid, office_ambassador, office_secretary, office_mayor)

# get data on sessions served
sessions_served_df <- get_political(legislature = "esp") %>% group_by(pageid) %>% dplyr::summarize(sessions_served = n())

# merge
legislator_df <- left_join(political_df, sessions_served_df, by = "pageid") %>% left_join(offices_df, by = "wikidataid") %>% left_join(traffic_df, by = "pageid") 

# add age and chamber variables
legislator_df$age <- get_age(legislator_df$birth, as.POSIXct(political_df$session_end[1]))

# only keep last session of each member
legislator_df <- legislator_df %>% group_by(wikidataid) %>% slice(which.max(session)) %>% ungroup()

# wikidata id
legislator_df$wikidata_id <- str_replace(legislator_df$wikidataid, "Q", "")
legislator_df$wikidataid <- NULL

# death
legislator_df$dead <- ifelse(!is.na(legislator_df$death), TRUE, FALSE)



## export data -------------------------------         

esp12_df <- legislator_df
save(esp12_df, file = "../data/esp12_df.RData")
load("../data/esp12_df.RData")
legislator_df <- esp12_df
header <- "members of the 12th Congress of Deputies of Spain (2016--2019)"


### top and bottom 10 pageviews ----------

legislator_df <- arrange(legislator_df, desc(traffic_mean))
legislator_df$rank <- 1:nrow(legislator_df)
legislator_df_sub <- dplyr::select(legislator_df, rank, name, traffic_mean, traffic_max)
legislator_df_sub_table <- filter(legislator_df_sub, !is.na(traffic_mean))
names(legislator_df_sub_table) <- c("Rank", "Legislator", "Mean", "Maximum")
legislator_top10 <- head(legislator_df_sub_table, 10)
legislator_bottom10 <- tail(legislator_df_sub_table, 10)
legislator_top_bottom_table <- rbind(legislator_top10, legislator_bottom10)

print(xtable(legislator_top_bottom_table, caption = paste0("Top/bottom 10 mean daily page views for ", header, , ".\\label{tab:pageviewstopesp}"), digits = 0), align = c("l", "r", "r", "r"), booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "htb", include.rownames=FALSE, hline.after = c(-1, seq(0, 20, 5)), file = "../figures/tab-pageviews-esp12.tex")



## OLS models of log pageviews ---------------------------

legislator_df$age_log <- log(legislator_df$age)
legislator_df$sessions_served_log <- log(legislator_df$sessions_served)
legislator_df$traffic_mean_log <- log(legislator_df$traffic_mean)
legislator_df$party2 <- ifelse(!(legislator_df$party %in% c("Cs", "PP", "PSOE", "UP")), "Other", legislator_df$party) %>% factor(levels = c("Other", "Cs", "PP", "PSOE", "UP"))

covars <- c("sessions_served_log", "party2", "office_secretary", "office_mayor", "sex", "age_log")
covars_names <- c("Sessions served", "Party: Cs", "Party: PPL", "Party: PSOE", "Party: UP", "Office: Secretary", "Office: Mayor", "Male", "Age", "(Intercept)")

fmla_traffic <- paste("traffic_mean", paste(covars, collapse = " + "), sep = " ~ ")
summary(traffic_model <- lm(fmla_traffic, legislator_df))

fmla_traffic_log <- paste("traffic_mean_log", paste(covars, collapse = " + "), sep = " ~ ")
summary(log_traffic_model <- lm(fmla_traffic_log, legislator_df))

# export table
texreg(log_traffic_model, custom.coef.names = covars_names, custom.model.names = "Log page views", table = TRUE, single.row = TRUE, booktabs = TRUE, caption.above = TRUE, use.packages = FALSE, dcolumn = TRUE, fontsize = "scriptsize", float.pos = "htb", caption = paste0("OLS estimates of log page views of ", header, "."), label = "tab:esp12-models", file = "../figures/tab-models-esp12.tex")


## Visualize time series of page traffic data, aggregated -----------------------------------

# get data
traffic_df <- get_traffic(legislature = "esp") %>% 
  filter(date >= political_df$session_start[1] & date <= political_df$session_end[1]) %>%
  right_join(y = filter(get_political(legislature = "esp"), as.numeric(session) == session_num), by = "pageid") %>%
  left_join(y = get_core(legislature = "esp"), by = "pageid") %>%
  dplyr::select(pageid, date, traffic, session, party, name)

# aggregate data
traffic_df$date <- ymd(traffic_df$date)
traffic_df_date <- group_by(traffic_df, date)
traffic_df_legislators <- group_by(traffic_df, pageid)
traffic_df_sum <- summarize(traffic_df_date, mean = mean(traffic, na.rm = TRUE))
traffic_df_sum <- mutate(traffic_df_sum, 
                         mean_l1 = lag(mean, 1), 
                         mean_f1 = lead(mean, 1),
                         peak = (mean >= 1.8*mean_l1 & mean > 200))
#traffic_df_sum <- filter(traffic_df_sum, date >= "2015-10-22")

# identify peaks
traffic_df_peaks <- filter(traffic_df_sum, peak == TRUE)
traffic_df_peaks_df <- filter(traffic_df, date %in% traffic_df_peaks$date)
traffic_df_peaks_group <- group_by(traffic_df_peaks_df, date) %>% dplyr::arrange(desc(traffic)) %>% filter(row_number()==1)
traffic_df_peaks_group <- arrange(traffic_df_peaks_group, date)  %>% left_join(traffic_df_peaks, by = "date") 
events_vec <- c("Speech at investiture debate", "Appointed health minster", "", "Vote of no confidence", "Vote of no confidence", "PP leadership run-off", "TV debate", "General election")

events_vec_names <- paste0(events_vec, " (", traffic_df_peaks_group$name, ")")
date_range <- ymd(range(traffic_df$date, na.rm = TRUE))


# plot
pdf(file="../figures/traffic-esp.pdf", height=2.8, width=8, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(0,2,0,.5))
par(yaxs="i", xaxs="i", bty="n")
layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), 
       heights = c(1, .45), widths = 5)
# page views time series
par(mar=c(2,4,0,.5))
plot(ymd(traffic_df_sum$date), traffic_df_sum$mean, type = "l", ylim= c(0, 1.5*max(traffic_df_sum$mean, na.rm = TRUE)), xlim = date_range + c(0, 21),  xaxt = "n", yaxt = "n", xlab = "", ylab = "Avg. page views", col = "white")
#abline(v = dates[day(dates) == 1 & month(dates) %in% c(1)], col = "lightgrey")
abline(h = seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), col = "lightgrey")
lines(ymd(traffic_df_sum$date), traffic_df_sum$mean, lwd = .5)
dates <- seq(date_range[1], date_range[2], by = 1)
axis(1, dates[day(dates) == 1], labels = FALSE, cex = .7, tck=-0.03)
axis(1, dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], labels = as.character(month(dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], label = TRUE, abbr = TRUE)), tick = F, lwd = 0, cex.axis = .8, line = -1)
axis(1, dates[day(dates) == 1 & month(dates) %in% c(7)], labels = as.character(year(dates[day(dates) == 15 & month(dates) %in% c(7)])), tick = F, lwd = 0, cex.axis = .8)
axis(2, seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), las = 2, cex.axis = .8)
# events labels in time series
gap_size <- 150 
for(i in seq_along(events_vec)) {
  points(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size, pch=21, cex = 2.2, bg = "white")  
  text(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size, i, cex = .7)
}
# events labels explained
par(mar=c(0,0,0,0))
plot(0, 0, xlim = c(0, 5), ylim = c(2.5, 5), xaxt = "n", yaxt = "n", xlab = "", ylab = "", cex = 0)
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




### fra ----------------------------

## get fra data -------------------------------

session_num <- 14

# get political data
political_df <- left_join(x = filter(get_political(legislature = "fra"), as.numeric(session) == session_num),
                          y = get_core(legislature = "fra"), 
                          by = "pageid")

# get traffic data
traffic_df <- get_traffic(legislature = "fra") %>% 
  dplyr::filter(date >= political_df$session_start[1] & date <= political_df$session_end[1]) %>% 
  group_by(pageid) %>% 
  summarize(traffic_mean = mean(traffic, na.rm = TRUE),
            traffic_max = max(traffic, na.rm = TRUE))

# get offices data
offices_df <- get_office(legislature = "fra") 
offices_df$office_ambassador <- rowSums(offices_df[,str_detect(names(offices_df), "ambassador")]) >= 1
offices_df$office_secretary <- rowSums(offices_df[,str_detect(names(offices_df), "minister|secretary_of_state")]) >= 1
#offices_df$office_governor <- rowSums(offices_df[,str_detect(names(offices_df), "governor|prime")]) >= 1
offices_df$office_mayor <- rowSums(offices_df[,str_detect(names(offices_df), "mayor")]) >= 1
#offices_df$position_party_chairman <- rowSums(offices_df[,str_detect(names(offices_df), "party_leader")]) >= 1
offices_df <- dplyr::select(offices_df, wikidataid, office_ambassador, office_secretary, office_mayor)

# get data on sessions served
sessions_served_df <- get_political(legislature = "fra") %>% group_by(pageid) %>% dplyr::summarize(sessions_served = n())

# merge
legislator_df <- left_join(political_df, sessions_served_df, by = "pageid") %>% left_join(offices_df, by = "wikidataid") %>% left_join(traffic_df, by = "pageid") 

# add age and chamber variables
legislator_df$age <- get_age(legislator_df$birth, as.POSIXct(political_df$session_end[1]))

# only keep last session of each member
legislator_df <- legislator_df %>% group_by(wikidataid) %>% slice(which.max(session)) %>% ungroup()

# wikidata id
legislator_df$wikidata_id <- str_replace(legislator_df$wikidataid, "Q", "")
legislator_df$wikidataid <- NULL

# death
legislator_df$dead <- ifelse(!is.na(legislator_df$death), TRUE, FALSE)



## export data -------------------------------         

fra14_df <- legislator_df
save(fra14_df, file = "../data/fra14_df.RData")
load("../data/fra14_df.RData")
legislator_df <- fra14_df
header <- "members of the 14 legislature of the French Fifth Republic (2012--2017)"


### top and bottom 10 pageviews ----------

legislator_df <- arrange(legislator_df, desc(traffic_mean))
legislator_df$rank <- 1:nrow(legislator_df)
legislator_df_sub <- dplyr::select(legislator_df, rank, name, traffic_mean, traffic_max)
legislator_df_sub_table <- filter(legislator_df_sub, !is.na(traffic_mean))
names(legislator_df_sub_table) <- c("Rank", "Legislator", "Mean", "Maximum")
legislator_top10 <- head(legislator_df_sub_table, 10)
legislator_bottom10 <- tail(legislator_df_sub_table, 10)
legislator_top_bottom_table <- rbind(legislator_top10, legislator_bottom10)

print(xtable(legislator_top_bottom_table, caption = paste0("Top/bottom 10 mean daily page views for ", header, ".\\label{tab:pageviewstopfra}"), digits = 0), align = c("l", "r", "r", "r"), booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "htb", include.rownames=FALSE, hline.after = c(-1, seq(0, 20, 5)), file = "../figures/tab-pageviews-fra14.tex")



## OLS models of log pageviews ---------------------------

legislator_df$age_log <- log(legislator_df$age)
legislator_df$sessions_served_log <- log(legislator_df$sessions_served)
legislator_df$traffic_mean_log <- log(legislator_df$traffic_mean)
legislator_df$party2 <- ifelse(!(legislator_df$party %in% c("GDR", "LR", "PS", "RRDP", "SER", "SRC", "UDI")), "Other", legislator_df$party) %>% factor(levels = c("Other", "GDR", "LR", "PS", "RRDP", "SER", "SRC", "UDI"))

covars <- c("sessions_served_log", "party2", "office_secretary", "office_mayor", "sex", "age_log")
covars_names <- c("Sessions served", "Party: GDR", "Party: LR", "Party: PS", "Party: RRDP", "Party: SER", "Party: SRC", "Party: UDI", "Office: Secretary", "Office: Mayor", "Male", "Age", "(Intercept)")

fmla_traffic <- paste("traffic_mean", paste(covars, collapse = " + "), sep = " ~ ")
summary(traffic_model <- lm(fmla_traffic, legislator_df))

fmla_traffic_log <- paste("traffic_mean_log", paste(covars, collapse = " + "), sep = " ~ ")
summary(log_traffic_model <- lm(fmla_traffic_log, legislator_df))

# export table
texreg(log_traffic_model, custom.coef.names = covars_names, custom.model.names = "Log page views", table = TRUE, single.row = TRUE, booktabs = TRUE, caption.above = TRUE, use.packages = FALSE, dcolumn = TRUE, fontsize = "scriptsize", float.pos = "htb", caption = paste0("OLS estimates of log page views of ", header, "."), label = "tab:fra14-models", file = "../figures/tab-models-fra14.tex")


## Visualize time series of page traffic data, aggregated -----------------------------------

# get data
traffic_df <- get_traffic(legislature = "fra") %>% 
  filter(date >= political_df$session_start[1] & date <= political_df$session_end[1]) %>%
  right_join(y = filter(get_political(legislature = "fra"), as.numeric(session) == session_num), by = "pageid") %>%
  left_join(y = get_core(legislature = "fra"), by = "pageid") %>%
  dplyr::select(pageid, date, traffic, session, party, name)

# aggregate data
traffic_df$date <- ymd(traffic_df$date)
traffic_df_date <- group_by(traffic_df, date)
traffic_df_legislators <- group_by(traffic_df, pageid)
traffic_df_sum <- summarize(traffic_df_date, mean = mean(traffic, na.rm = TRUE))
traffic_df_sum <- mutate(traffic_df_sum, 
                         mean_l1 = lag(mean, 1), 
                         mean_f1 = lead(mean, 1),
                         peak = (mean >= 1.8*mean_l1 & mean > 425))
#traffic_df_sum <- filter(traffic_df_sum, date >= "2015-10-22")

# identify peaks
traffic_df_peaks <- filter(traffic_df_sum, peak == TRUE)
traffic_df_peaks_df <- filter(traffic_df, date %in% traffic_df_peaks$date)
traffic_df_peaks_group <- group_by(traffic_df_peaks_df, date) %>% dplyr::arrange(desc(traffic)) %>% filter(row_number()==1)
traffic_df_peaks_group <- arrange(traffic_df_peaks_group, date)  %>% left_join(traffic_df_peaks, by = "date") %>% filter(date != "2017-01-25")
events_vec <- c("Appointed prime minister", "Republican primary", "Republicans primary run-off", "Appointed prime minister", "TV debate", "Socialist primary", "Socialist primary run-off", "TV debate", "Presidential election", "Appointed prime minister", "Appointed minister of the economy")

events_vec_names <- paste0(events_vec, " (", traffic_df_peaks_group$name, ")")
date_range <- ymd(range(traffic_df$date, na.rm = TRUE))


# plot
pdf(file="../figures/traffic-fra.pdf", height=2.8, width=8, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(0,2,0,.5))
par(yaxs="i", xaxs="i", bty="n")
layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), 
       heights = c(1, .55), widths = 5)
# page views time series
par(mar=c(2,4,0,.5))
plot(ymd(traffic_df_sum$date), traffic_df_sum$mean, type = "l", ylim= c(0, 1.5*max(traffic_df_sum$mean, na.rm = TRUE)), xlim = date_range + c(0, 21),  xaxt = "n", yaxt = "n", xlab = "", ylab = "Avg. page views", col = "white")
#abline(v = dates[day(dates) == 1 & month(dates) %in% c(1)], col = "lightgrey")
abline(h = seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), col = "lightgrey")
lines(ymd(traffic_df_sum$date), traffic_df_sum$mean, lwd = .5)
dates <- seq(date_range[1], date_range[2], by = 1)
axis(1, dates[day(dates) == 1], labels = FALSE, cex = .7, tck=-0.03)
axis(1, dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], labels = as.character(month(dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], label = TRUE, abbr = TRUE)), tick = F, lwd = 0, cex.axis = .8, line = -1)
axis(1, dates[day(dates) == 1 & month(dates) %in% c(7)], labels = as.character(year(dates[day(dates) == 15 & month(dates) %in% c(7)])), tick = F, lwd = 0, cex.axis = .8)
axis(2, seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), las = 2, cex.axis = .8)
# events labels in time series
#gap_size <- 250 
gap_size <- c(500, 250, 100, 550, 250, 250, 700, 400, 150, 250, 250)
for(i in seq_along(events_vec)) {
  points(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size[i], pch=21, cex = 2.2, bg = "white")  
  text(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size[i], i, cex = .7)
}
# events labels explained
par(mar=c(0,0,0,0))
plot(0, 0, xlim = c(0, 5), ylim = c(1.5, 5), xaxt = "n", yaxt = "n", xlab = "", ylab = "", cex = 0)
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





### irl ----------------------------

## get irl data -------------------------------

session_num <- 32

# get political data
political_df <- left_join(x = filter(get_political(legislature = "irl"), as.numeric(session) == session_num),
                          y = get_core(legislature = "irl"), 
                          by = "pageid")

# get traffic data
traffic_df <- get_traffic(legislature = "irl") %>% 
  dplyr::filter(date >= political_df$session_start[1] & date <= political_df$session_end[1]) %>% 
  group_by(pageid) %>% 
  summarize(traffic_mean = mean(traffic, na.rm = TRUE),
            traffic_max = max(traffic, na.rm = TRUE))

# get offices data
offices_df <- get_office(legislature = "irl") 
offices_df$office_secretary <- rowSums(offices_df[,str_detect(names(offices_df), "minister|secretary_of_state")]) >= 1
#offices_df$office_governor <- rowSums(offices_df[,str_detect(names(offices_df), "governor|prime")]) >= 1
offices_df$office_mayor <- rowSums(offices_df[,str_detect(names(offices_df), "mayor")]) >= 1
offices_df$position_party_chairman <- rowSums(offices_df[,str_detect(names(offices_df), "leader")]) >= 1
offices_df <- dplyr::select(offices_df, wikidataid, office_secretary, office_mayor, position_party_chairman)

# get data on sessions served
sessions_served_df <- get_political(legislature = "irl") %>% group_by(pageid) %>% dplyr::summarize(sessions_served = n())

# merge
legislator_df <- left_join(political_df, sessions_served_df, by = "pageid") %>% left_join(offices_df, by = "wikidataid") %>% left_join(traffic_df, by = "pageid") 

# add age and chamber variables
legislator_df$age <- get_age(legislator_df$birth, as.POSIXct(political_df$session_end[1]))

# only keep last session of each member
legislator_df <- legislator_df %>% group_by(wikidataid) %>% slice(which.max(session)) %>% ungroup()

# wikidata id
legislator_df$wikidata_id <- str_replace(legislator_df$wikidataid, "Q", "")
legislator_df$wikidataid <- NULL

# death
legislator_df$dead <- ifelse(!is.na(legislator_df$death), TRUE, FALSE)



## export data -------------------------------         

irl32_df <- legislator_df
save(irl32_df, file = "../data/irl32_df.RData")
load("../data/irl32_df.RData")
legislator_df <- irl32_df
header <- "members of the 32nd Irish Dáil (2016--2020)"


### top and bottom 10 pageviews ----------

legislator_df <- arrange(legislator_df, desc(traffic_mean))
legislator_df$rank <- 1:nrow(legislator_df)
legislator_df_sub <- dplyr::select(legislator_df, rank, name, traffic_mean, traffic_max)
legislator_df_sub_table <- filter(legislator_df_sub, !is.na(traffic_mean))
names(legislator_df_sub_table) <- c("Rank", "Legislator", "Mean", "Maximum")
legislator_top10 <- head(legislator_df_sub_table, 10)
legislator_bottom10 <- tail(legislator_df_sub_table, 10)
legislator_top_bottom_table <- rbind(legislator_top10, legislator_bottom10)

print(xtable(legislator_top_bottom_table, caption = paste0("Top/bottom 10 mean daily page views for ", header, ".\\label{tab:pageviewstopirl}"), digits = 0), align = c("l", "r", "r", "r"), booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "htb", include.rownames=FALSE, hline.after = c(-1, seq(0, 20, 5)), file = "../figures/tab-pageviews-irl32.tex")



## OLS models of log pageviews ---------------------------

legislator_df$age_log <- log(legislator_df$age)
legislator_df$sessions_served_log <- log(legislator_df$sessions_served)
legislator_df$traffic_mean_log <- log(legislator_df$traffic_mean)
legislator_df$party2 <- ifelse(!(legislator_df$party %in% c("Fianna Fáil", "Fine Gael", "Labour Party", "Sinn Féin", "Independent")), "Other", legislator_df$party) %>% factor(levels = c("Other", "Fianna Fáil", "Fine Gael", "Labour Party", "Sinn Féin", "Independent"))

covars <- c("sessions_served_log", "party2", "office_secretary", "office_mayor", "position_party_chairman", "sex", "age_log")
covars_names <- c("Sessions served", "Party: Fianna Fáil", "Party: Fine Gael", "Party: Labour", "Party: Sinn Féin", "Party: Independent", "Office: Secretary", "Office: Mayor", "Office: Party Leader", "Male", "Age", "(Intercept)")

fmla_traffic <- paste("traffic_mean", paste(covars, collapse = " + "), sep = " ~ ")
summary(traffic_model <- lm(fmla_traffic, legislator_df))

fmla_traffic_log <- paste("traffic_mean_log", paste(covars, collapse = " + "), sep = " ~ ")
summary(log_traffic_model <- lm(fmla_traffic_log, legislator_df))

# export table
texreg(log_traffic_model, custom.coef.names = covars_names, custom.model.names = "Log page views", table = TRUE, single.row = TRUE, booktabs = TRUE, caption.above = TRUE, use.packages = FALSE, dcolumn = TRUE, fontsize = "scriptsize", float.pos = "htb", caption = paste0("OLS estimates of log page views of ", header, "."), label = "tab:irl32-models", file = "../figures/tab-models-irl32.tex")


## Visualize time series of page traffic data, aggregated -----------------------------------

# get data
traffic_df <- get_traffic(legislature = "irl") %>% 
  filter(date >= political_df$session_start[1] & date <= political_df$session_end[1]) %>%
  right_join(y = filter(get_political(legislature = "irl"), as.numeric(session) == session_num), by = "pageid") %>%
  left_join(y = get_core(legislature = "irl"), by = "pageid") %>%
  dplyr::select(pageid, date, traffic, session, party, name)

# aggregate data
traffic_df$date <- ymd(traffic_df$date)
traffic_df_date <- group_by(traffic_df, date)
traffic_df_legislators <- group_by(traffic_df, pageid)
traffic_df_sum <- summarize(traffic_df_date, mean = mean(traffic, na.rm = TRUE))
traffic_df_sum <- mutate(traffic_df_sum, 
                         mean_l1 = lag(mean, 1), 
                         mean_f1 = lead(mean, 1),
                         peak = (mean >= 1.8*mean_l1 & mean > 280))
#traffic_df_sum <- filter(traffic_df_sum, date >= "2015-10-22")

# identify peaks
traffic_df_peaks <- filter(traffic_df_sum, peak == TRUE)
traffic_df_peaks_df <- filter(traffic_df, date %in% traffic_df_peaks$date)
traffic_df_peaks_group <- group_by(traffic_df_peaks_df, date) %>% dplyr::arrange(desc(traffic)) %>% filter(row_number()==1)
traffic_df_peaks_group <- arrange(traffic_df_peaks_group, date)  %>% left_join(traffic_df_peaks, by = "date") 
events_vec <- c("Election results", "Appointed Minister of Health", "McGuinness funeral", "Elected leader of Fine Gael", "Appointed Prime Minister", "Abortion referendum", "Meeting with Boris Johnson", "Meeting with Boris Johnson", "India visit", "Meeting with Michael Barnier")

events_vec_names <- paste0(events_vec, " (", traffic_df_peaks_group$name, ")")
date_range <- ymd(range(traffic_df$date, na.rm = TRUE))


# plot
pdf(file="../figures/traffic-irl.pdf", height=2.8, width=8, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(0,2,0,.5))
par(yaxs="i", xaxs="i", bty="n")
layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), 
       heights = c(1, .55), widths = 5)
# page views time series
par(mar=c(2,4,0,.5))
plot(ymd(traffic_df_sum$date), traffic_df_sum$mean, type = "l", ylim= c(0, 1.5*max(traffic_df_sum$mean, na.rm = TRUE)), xlim = date_range + c(-7, 21),  xaxt = "n", yaxt = "n", xlab = "", ylab = "Avg. page views", col = "white")
#abline(v = dates[day(dates) == 1 & month(dates) %in% c(1)], col = "lightgrey")
abline(h = seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), col = "lightgrey")
lines(ymd(traffic_df_sum$date), traffic_df_sum$mean, lwd = .5)
dates <- seq(date_range[1], date_range[2], by = 1)
axis(1, dates[day(dates) == 1], labels = FALSE, cex = .7, tck=-0.03)
axis(1, dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], labels = as.character(month(dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], label = TRUE, abbr = TRUE)), tick = F, lwd = 0, cex.axis = .8, line = -1)
axis(1, dates[day(dates) == 1 & month(dates) %in% c(7)], labels = as.character(year(dates[day(dates) == 15 & month(dates) %in% c(7)])), tick = F, lwd = 0, cex.axis = .8)
axis(2, seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), las = 2, cex.axis = .8)
# events labels in time series
#gap_size <- 250 
gap_size <- 100
for(i in seq_along(events_vec)) {
  points(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size, pch=21, cex = 2.2, bg = "white")  
  text(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size, i, cex = .7)
}
# events labels explained
par(mar=c(0,0,0,0))
plot(0, 0, xlim = c(0, 5), ylim = c(2, 5), xaxt = "n", yaxt = "n", xlab = "", ylab = "", cex = 0)
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



### gbr ----------------------------

## get gbr data -------------------------------

session_num <- 57

# get political data
political_df <- left_join(x = filter(get_political(legislature = "gbr"), as.numeric(session) == session_num),
                          y = get_core(legislature = "gbr"), 
                          by = "pageid")

# get traffic data
traffic_df <- get_traffic(legislature = "gbr") %>% 
  dplyr::filter(date >= political_df$session_start[1] & date <= political_df$session_end[1]) %>% 
  group_by(pageid) %>% 
  summarize(traffic_mean = mean(traffic, na.rm = TRUE),
            traffic_max = max(traffic, na.rm = TRUE))

# get offices data
offices_df <- get_office(legislature = "gbr") %>% filter(wikidataid %in% political_df$wikidataid)
offices_df <- offices_df[,c(TRUE, as.logical(colSums(offices_df[2:ncol(offices_df)], na.rm = TRUE) > 0))]
offices_df$office_secretary <- rowSums(offices_df[,str_detect(names(offices_df), "^minister|secretary_of_state")]) >= 1
offices_df$office_mayor <- rowSums(offices_df[,str_detect(names(offices_df), "mayor")]) >= 1
offices_df$position_party_chairman <- rowSums(offices_df[,str_detect(names(offices_df), "chairman|chair_.+party$|leader")]) >= 1
offices_df <- dplyr::select(offices_df, wikidataid, office_secretary, office_mayor, position_party_chairman)

# get data on sessions served
sessions_served_df <- get_political(legislature = "gbr") %>% group_by(pageid) %>% dplyr::summarize(sessions_served = n())

# merge
legislator_df <- left_join(political_df, sessions_served_df, by = "pageid") %>% left_join(offices_df, by = "wikidataid") %>% left_join(traffic_df, by = "pageid") 

# add age and chamber variables
legislator_df$age <- get_age(legislator_df$birth, as.POSIXct(political_df$session_end[1]))

# only keep last session of each member
legislator_df <- legislator_df %>% group_by(wikidataid) %>% slice(which.max(session)) %>% ungroup()

# wikidata id
legislator_df$wikidata_id <- str_replace(legislator_df$wikidataid, "Q", "")
legislator_df$wikidataid <- NULL

# death
legislator_df$dead <- ifelse(!is.na(legislator_df$death), TRUE, FALSE)



## export data -------------------------------         

gbr57_df <- legislator_df
save(gbr57_df, file = "../data/gbr57_df.RData")
load("../data/gbr57_df.RData")
legislator_df <- gbr57_df
header <- "members of the 57th Parliament of the United Kingdom (2017--2019)"


### top and bottom 10 pageviews ----------

legislator_df <- arrange(legislator_df, desc(traffic_mean))
legislator_df$rank <- 1:nrow(legislator_df)
legislator_df_sub <- dplyr::select(legislator_df, rank, name, traffic_mean, traffic_max)
legislator_df_sub_table <- filter(legislator_df_sub, !is.na(traffic_mean))
names(legislator_df_sub_table) <- c("Rank", "Legislator", "Mean", "Maximum")
legislator_top10 <- head(legislator_df_sub_table, 10)
legislator_bottom10 <- tail(legislator_df_sub_table, 10)
legislator_top_bottom_table <- rbind(legislator_top10, legislator_bottom10)

print(xtable(legislator_top_bottom_table, caption = paste0("Top/bottom 10 mean daily page views for ", header, ".\\label{tab:pageviewstopgbr}"), digits = 0), align = c("l", "r", "r", "r"), booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "htb", include.rownames=FALSE, hline.after = c(-1, seq(0, 20, 5)), file = "../figures/tab-pageviews-gbr57.tex")



## OLS models of log pageviews ---------------------------

legislator_df$age_log <- log(legislator_df$age)
legislator_df$sessions_served_log <- log(legislator_df$sessions_served)
legislator_df$traffic_mean_log <- log(legislator_df$traffic_mean)
legislator_df$party2 <- ifelse(!(legislator_df$party %in% c("Conservative Party", "Labour Party", "Liberal Democrats", "Scottish National")), "Other", legislator_df$party) %>% factor(levels = c("Other", "Conservative Party", "Labour Party", "Liberal Democrats", "Scottish National"))

covars <- c("sessions_served_log", "party2", "office_secretary", "office_mayor", "position_party_chairman", "sex", "age_log")
covars_names <- c("Sessions served", "Party: Conservative", "Party: Labour", "Party: LibDem", "Party: SNP", "Office: Secretary", "Office: Mayor", "Office: Party Leader", "Male", "Age", "(Intercept)")

fmla_traffic <- paste("traffic_mean", paste(covars, collapse = " + "), sep = " ~ ")
summary(traffic_model <- lm(fmla_traffic, legislator_df))

fmla_traffic_log <- paste("traffic_mean_log", paste(covars, collapse = " + "), sep = " ~ ")
summary(log_traffic_model <- lm(fmla_traffic_log, legislator_df))

# export table
texreg(log_traffic_model, custom.coef.names = covars_names, custom.model.names = "Log page views", table = TRUE, single.row = TRUE, booktabs = TRUE, caption.above = TRUE, use.packages = FALSE, dcolumn = TRUE, fontsize = "scriptsize", float.pos = "htb", caption = paste0("OLS estimates of log page views of ", header, "."), label = "tab:gbr57-models", file = "../figures/tab-models-gbr57.tex")


## Visualize time series of page traffic data, aggregated -----------------------------------

# get data
traffic_df <- get_traffic(legislature = "gbr") %>% 
  filter(date >= political_df$session_start[1] & date <= political_df$session_end[1]) %>%
  right_join(y = filter(get_political(legislature = "gbr"), as.numeric(session) == session_num), by = "pageid") %>%
  left_join(y = get_core(legislature = "gbr"), by = "pageid") %>%
  dplyr::select(pageid, date, traffic, session, party, name)

# aggregate data
traffic_df$date <- ymd(traffic_df$date)
traffic_df_date <- group_by(traffic_df, date)
traffic_df_legislators <- group_by(traffic_df, pageid)
traffic_df_sum <- summarize(traffic_df_date, mean = mean(traffic, na.rm = TRUE))
traffic_df_sum <- mutate(traffic_df_sum, 
                         mean_l1 = lag(mean, 1), 
                         mean_f1 = lead(mean, 1),
                         peak = (mean >= 1.8*mean_l1 & mean > 780))
#traffic_df_sum <- filter(traffic_df_sum, date >= "2015-10-22")

# identify peaks
traffic_df_peaks <- filter(traffic_df_sum, peak == TRUE)
traffic_df_peaks_df <- filter(traffic_df, date %in% traffic_df_peaks$date)
traffic_df_peaks_group <- group_by(traffic_df_peaks_df, date) %>% dplyr::arrange(desc(traffic)) %>% filter(row_number()==1)
traffic_df_peaks_group <- arrange(traffic_df_peaks_group, date)  %>% left_join(traffic_df_peaks, by = "date") 
events_vec <- c("Post-election", "Appointed Home Secretary", "Appointed Brexit Secretary", "Resignation", "Vote of confidence", "Vow to resign", "Announcement to resign", "Tory leadership vote", "Elected Tory leader", "Party switch", "Elected Speaker", "General election")

events_vec_names <- paste0(events_vec, " (", traffic_df_peaks_group$name, ")")
date_range <- ymd(range(traffic_df$date, na.rm = TRUE))


# plot
pdf(file="../figures/traffic-gbr.pdf", height=2.8, width=8, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(0,2,0,.5))
par(yaxs="i", xaxs="i", bty="n")
layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), 
       heights = c(1, .55), widths = 5)
# page views time series
par(mar=c(2,4,0,.5))
plot(ymd(traffic_df_sum$date), traffic_df_sum$mean, type = "l", ylim= c(0, 1.5*max(traffic_df_sum$mean, na.rm = TRUE)), xlim = date_range + c(-7, 21),  xaxt = "n", yaxt = "n", xlab = "", ylab = "Avg. page views", col = "white")
#abline(v = dates[day(dates) == 1 & month(dates) %in% c(1)], col = "lightgrey")
abline(h = seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), col = "lightgrey")
lines(ymd(traffic_df_sum$date), traffic_df_sum$mean, lwd = .5)
dates <- seq(date_range[1], date_range[2], by = 1)
axis(1, dates[day(dates) == 1], labels = FALSE, cex = .7, tck=-0.03)
axis(1, dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], labels = as.character(month(dates[day(dates) == 15 & month(dates) %in% c(1, 4, 7, 10)], label = TRUE, abbr = TRUE)), tick = F, lwd = 0, cex.axis = .8, line = -1)
axis(1, dates[day(dates) == 1 & month(dates) %in% c(7)], labels = as.character(year(dates[day(dates) == 15 & month(dates) %in% c(7)])), tick = F, lwd = 0, cex.axis = .8)
axis(2, seq(0, 1.2*max(traffic_df_sum$mean, na.rm = TRUE), 200), las = 2, cex.axis = .8)
# events labels in time series
gap_size <- 400
for(i in seq_along(events_vec)) {
  points(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size, pch=21, cex = 2.2, bg = "white")  
  text(traffic_df_peaks_group$date[i], traffic_df_peaks_group$mean[i] + gap_size, i, cex = .7)
}
# events labels explained
par(mar=c(0,0,0,0))
plot(0, 0, xlim = c(0, 5), ylim = c(1.5, 5), xaxt = "n", yaxt = "n", xlab = "", ylab = "", cex = 0)
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

