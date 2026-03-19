pacman::p_load(tidyverse, openxlsx, meta)
# Data cleaning ####
Data <- openxlsx::read.xlsx("data/Data_InterventionOutcomes.xlsx")
names(Data) <- c("publication", "control_condition", "treatment_condition",
                 "intervention_type", "medium_document", "medium_website",
                 "medium_sms", "medium_phone", "medium_message_assist",
                 "medium_inperson_assist", "medium_other", "nudge_information",
                 "nudge_friction", "nudge_norm", "nudge_commit",
                 "costs_learning", "costs_compliance", "costs_psychological",
                 "result_description", "result_additional_info", "program",
                 "takeup", "application", "control_variables",
                 "direction_positive", "direction_negative",
                 "direction_neutral", "significant_05", "significant_1",
                 "significant_no", "treatment_size", "treatment_size_positive",
                 "treatment_size_negative", "control_size",
                 "control_size_positive", "control_size_negative",
                 "percent_positive_treatment", "percent_positive_control",
                 "rr_raw", "rr_pval", "rr_lb", "rr_ub", "rr_reduction",
                 "ar_reduction", "protocol", "itt", "pedro_eligible",
                 "pedro_allocation_concealed", "pedro_balance",
                 "pedro_blind_subjects", "pedro_blind_providers",
                 "pedro_blind_assessors", "pedro_outcome_85", "pedro_itt",
                 "pedro_statistical_comparisons", "pedro_variability",
                 "quality_yes", "quality_doesnotapply", "quality_final",
                 "geographic_location", "partnership", "sample")
Data <- Data[3:nrow(Data),] # remove header
Data$result_additional_info[is.na(Data$result_additional_info)] <- ""
 # replace all NAs by empty strings
for (i in 2:nrow(Data)) { # copy values from the previous "Time" row
  if (str_detect(Data$result_additional_info[i], "^Final\\sTime")) {
    time_1 <- max(which(str_detect(Data$result_additional_info[1:(i - 1)],
     "Time\\s1")))
    # copy columns 5 through 18 from time_1 to the current row
    Data[i, 5:18] <- Data[time_1, 5:18]
  }
}
Data <- Data |>
  fill(c(publication, control_condition, intervention_type,
   treatment_condition), # repeat relevant info on all lines
   .direction = "down") |>
   filter(str_detect(result_additional_info, "Time\\s\\d") == FALSE)
Data$publication <- gsub("(Study \\d).*", "\\1", Data$publication)
Data$publication <- gsub("\\)\\s\\(.*", "\\)", Data$publication)
Data$intervention_number <- Data$treatment_condition |>
  str_remove(" — .+\\r?\\n?.+") |>
  str_replace("^(?!Intervention).+", "Intervention 1") |>
  str_remove("\\r\\n.+") |>
  str_remove("(\\r\\n)+") # create column with only intervention number
Data$pub_interv <- paste0(# character string with study name and
  Data$publication, ", ", Data$intervention_number) # intervention number
# loop to verify if the three columns from the previous line are empty
Data[is.na(Data)] <- "" # replace all NAs by empty strings
for (i in 2:nrow(Data)) {
  if (Data$costs_learning[i] == "" && 
      Data$costs_compliance[i] == "" && 
      Data$costs_psychological[i] == "") {
    # replace value by that of previous line
    Data$costs_learning[i] <- Data$costs_learning[i - 1]
    Data$costs_compliance[i] <- Data$costs_compliance[i - 1]
    Data$costs_psychological[i] <- Data$costs_psychological[i - 1]
  }
}

# Nudges and mediums ####
Data[, c(22:23, 31:44, 57:59)] <- map( # make relevant columns numeric
  .x = Data[, c(22:23, 31:44, 58:60)], as.numeric)
Data$Lcosts <- NA
Data$Ccosts <- NA
Data$Pcosts <- NA
Data$LCPcosts <- NA # learning, compliance or psychological costs
Data$LCcosts <- NA # learning or compliance costs
Data$LPcosts <- NA # learning or psychological costs
Data$CPcosts <- NA # compliance or psychological costs
Data$medium <- NA # at least one medium
Data$mediumminor <- NA # at least one minor medium
for (i in 1:nrow(Data)) {
  Data["Lcosts"][i, ] <- table(str_detect(Data[16][i, ], "X"))["TRUE"]
  Data["Ccosts"][i, ] <- table(str_detect(Data[17][i, ], "X"))["TRUE"]
  Data["Pcosts"][i, ] <- table(str_detect(Data[18][i, ], "X"))["TRUE"]
  Data["LCPcosts"][i, ] <- table(str_detect(Data[16:18][i, ], "X"))["TRUE"]
  Data["LCcosts"][i, ] <- table(str_detect(Data[16:17][i, ], "X"))["TRUE"]
  Data["LPcosts"][i, ] <- table(str_detect(Data[c(16, 18)][i, ], "X"))["TRUE"]
  Data["CPcosts"][i, ] <- table(str_detect(Data[17:18][i, ], "X"))["TRUE"]
  Data["medium"][i, ] <- table(str_detect(Data[5:11][i, ], "X"))["TRUE"]
  Data["mediumminor"][i, ] <- table(str_detect(Data[5:11][i, ], "x"))["TRUE"]
}
Data$onemedium <- ifelse(Data$medium == "1", 1, 0)
Data$onemedium[is.na(Data$onemedium)] <- 0
Data$providinginfo <- str_detect(Data$intervention_type,
 "Providing information")
Data$framinginfo <- str_detect(Data$intervention_type, "Framing information")
Data$providingassist <- str_detect(Data$intervention_type,
 "Providing assistance")
Data$facilitatingcommit <- str_detect(Data$intervention_type,
 "Facilitating commitment")
SummaryTable <- data.frame(
  category = c(rep("Targeted administrative burden component", 7),
               rep("Intervention's medium", 16), rep("Nudge/mechanism", 4),
               rep("Intervention type", 4),
               rep("Combined intervention types", 8), NA),
  interventions = c(
    "Learning costs only", "Compliance costs only", "Psychological costs only",
    "Learning + compliance + psychological costs",
    "Learning + compliance costs", "Learning + psychological costs",
    "Compliance + psychological costs",
    'Only one main intervention medium (including "Other")',
    paste('Only one main intervention medium (including "Other"),',
          'no secondary medium'),
    paste('Only one main intervention medium (including "Other"),',
          'one or more secondary media'),
    'At least 2 main intervention media (including "Other")',
    'At least 3 main intervention media (including "Other")',
    'At least 4 main intervention media (including "Other")',
    'At least 5 main intervention media (including "Other")',
    'At least 6 main intervention media (including "Other")',
    'All 7 main intervention media (including "Other")',
    paste0("Written document (Letter/Flyer/Leaflet/Postcard/ Pamphlet/",
           "Reminder/Worksheet/Forms/Email)"),
    "Link to website/Online tool/ Computer software", "SMS", "Phone call",
    "Telephone or text message assistance", "In-person assistance", "Other",
    paste("Information amount and format: priming, salience, reducing",
          "cognitive burden and framing"),
    "Removing frictions and simplifying the application process",
    "Social norms, personal values and identity, and messenger",
    "Facilitating commitment, default options, and active decision",
    "Includes Providing information", "Includes Framing information",
    "Includes Providing assistance", "Includes Facilitating commitment",
    names(table(Data$intervention_type)),
    'Total number of interventions'),
  number = c(
    sum(is.na(Data$Ccosts) & is.na(Data$Pcosts), na.rm = T),
    sum(is.na(Data$Lcosts) & is.na(Data$Pcosts), na.rm = T),
    sum(is.na(Data$Lcosts) & is.na(Data$Ccosts), na.rm = T),
    table(Data$LCPcosts)["3"],
    sum(Data$Lcosts == 1 & Data$Ccosts == 1 & is.na(Data$Pcosts), na.rm = T),
    sum(Data$Lcosts == 1 & Data$Pcosts == 1 & is.na(Data$Ccosts), na.rm = T),
    sum(Data$Ccosts == 1 & Data$Pcosts == 1 & is.na(Data$Lcosts), na.rm = T),
    table(Data$medium)["1"],
    table(Data$medium)["1"] - sum(!is.na(
      Data$mediumminor[Data$medium == "1"])),
    sum(!is.na(Data$mediumminor[Data$medium == "1"])),
    sum(table(Data$medium)) - table(Data$medium)["1"],
    sum(table(Data$medium)) - sum(table(Data$medium)[c("1", "2")]),
    sum(table(Data$medium)) - sum(table(Data$medium)[c("1", "2", "3")]),
    sum(table(Data$medium)) - sum(table(Data$medium)[c("1", "2", "3", "4")]),
    sum(table(Data$medium)) - sum(table(Data$medium)[c("1", "2", "3", "4",
                                                       "5")]),
    0,
    sum(tolower(Data$medium_document) == "x", na.rm = TRUE),
    sum(tolower(Data$medium_website) == "x", na.rm = TRUE),
    sum(tolower(Data$medium_sms) == "x", na.rm = TRUE),
    sum(tolower(Data$medium_phone) == "x", na.rm = TRUE),
    sum(tolower(Data$medium_message_assist) == "x", na.rm = TRUE),
    sum(tolower(Data$medium_inperson_assist) == "x", na.rm = TRUE),
    sum(tolower(Data$medium_other) == "x", na.rm = TRUE),
    sum(tolower(Data$nudge_information) == "x", na.rm = TRUE),
    sum(tolower(Data$nudge_friction) == "x", na.rm = TRUE),
    sum(tolower(Data$nudge_norm) == "x", na.rm = TRUE),
    sum(tolower(Data$nudge_commit) == "x", na.rm = TRUE),
    sum(Data$providinginfo), sum(Data$framinginfo), sum(Data$providingassist),
    sum(Data$facilitatingcommit), table(Data$intervention_type),
    sum(Data$intervention_type != "", na.rm = T)))
SummaryTable$percent <- SummaryTable$number /
  sum(Data$intervention_type != "", na.rm = T) * 100
#openxlsx::write.xlsx(SummaryTable,
#  "data/InterventionsCombinedFrequencies.xlsx")
InterventionData <- Data |>
  group_by(pub_interv) |>
  slice(1) |>
  ungroup()
InterventionSummaryTable <- data.frame(
  category = c(rep("Targeted administrative burden component", 7),
               rep("Intervention's medium", 16), rep("Nudge/mechanism", 4),
               rep("Intervention type", 4),
               rep("Combined intervention types", 8), NA),
  interventions = c(
    "Learning costs only", "Compliance costs only", "Psychological costs only",
    "Learning + compliance + psychological costs",
    "Learning + compliance costs", "Learning + psychological costs",
    "Compliance + psychological costs",
    'Only one main intervention medium (including "Other")',
    paste('Only one main intervention medium (including "Other"),',
          'no secondary medium'),
    paste('Only one main intervention medium (including "Other"),',
          'one or more secondary media'),
    'At least 2 main intervention media (including "Other")',
    'At least 3 main intervention media (including "Other")',
    'At least 4 main intervention media (including "Other")',
    'At least 5 main intervention media (including "Other")',
    'At least 6 main intervention media (including "Other")',
    'All 7 main intervention media (including "Other")',
    paste0("Written document (Letter/Flyer/Leaflet/Postcard/ Pamphlet/",
           "Reminder/Worksheet/Forms/Email)"),
    "Link to website/Online tool/ Computer software", "SMS", "Phone call",
    "Telephone or text message assistance", "In-person assistance", "Other",
    paste("Information amount and format: priming, salience, reducing",
          "cognitive burden and framing"),
    "Removing frictions and simplifying the application process",
    "Social norms, personal values and identity, and messenger",
    "Facilitating commitment, default options, and active decision",
    "Includes Providing information", "Includes Framing information",
    "Includes Providing assistance", "Includes Facilitating commitment",
    names(table(Data$intervention_type)),
    'Total number of interventions'),
  number = c(
    sum(is.na(InterventionData$Ccosts) & is.na(InterventionData$Pcosts),
        na.rm = T),
    sum(is.na(InterventionData$Lcosts) & is.na(InterventionData$Pcosts),
        na.rm = T),
    sum(is.na(InterventionData$Lcosts) & is.na(InterventionData$Ccosts),
        na.rm = T),
    table(InterventionData$LCPcosts)["3"],
    sum(InterventionData$Lcosts == 1 & InterventionData$Ccosts == 1 &
     is.na(InterventionData$Pcosts), na.rm = T),
    sum(InterventionData$Lcosts == 1 & InterventionData$Pcosts == 1 &
     is.na(InterventionData$Ccosts), na.rm = T),
    sum(InterventionData$Ccosts == 1 & InterventionData$Pcosts == 1 &
     is.na(InterventionData$Lcosts), na.rm = T),
    table(InterventionData$medium)["1"],
    table(InterventionData$medium)["1"] - sum(!is.na(
      InterventionData$mediumminor[InterventionData$medium == "1"])),
    sum(!is.na(InterventionData$mediumminor[InterventionData$medium == "1"])),
    sum(table(InterventionData$medium)) - table(InterventionData$medium)["1"],
    sum(table(InterventionData$medium)) - sum(
      table(InterventionData$medium)[c("1", "2")]),
    sum(table(InterventionData$medium)) - sum(
      table(InterventionData$medium)[c("1", "2", "3")]),
    sum(table(InterventionData$medium)) - sum(
      table(InterventionData$medium)[c("1", "2", "3", "4")]),
    sum(table(InterventionData$medium)) - sum(
      table(InterventionData$medium)[c("1", "2", "3", "4", "5")]),
    0,
    sum(tolower(InterventionData$medium_document) == "x", na.rm = TRUE),
    sum(tolower(InterventionData$medium_website) == "x", na.rm = TRUE),
    sum(tolower(InterventionData$medium_sms) == "x", na.rm = TRUE),
    sum(tolower(InterventionData$medium_phone) == "x", na.rm = TRUE),
    sum(tolower(InterventionData$medium_message_assist) == "x", na.rm = TRUE),
    sum(tolower(InterventionData$medium_inperson_assist) == "x", na.rm = TRUE),
    sum(tolower(InterventionData$medium_other) == "x", na.rm = TRUE),
    sum(tolower(InterventionData$nudge_information) == "x", na.rm = TRUE),
    sum(tolower(InterventionData$nudge_friction) == "x", na.rm = TRUE),
    sum(tolower(InterventionData$nudge_norm) == "x", na.rm = TRUE),
    sum(tolower(InterventionData$nudge_commit) == "x", na.rm = TRUE),
    sum(InterventionData$providinginfo), sum(InterventionData$framinginfo),
    sum(InterventionData$providingassist),
    sum(InterventionData$facilitatingcommit),
    table(InterventionData$intervention_type),
    sum(InterventionData$intervention_type != "", na.rm = T)))
InterventionSummaryTable$percent <- InterventionSummaryTable$number /
  sum(InterventionData$intervention_type != "", na.rm = T) * 100
#openxlsx::write.xlsx(InterventionSummaryTable,
#  "data/InterventionsCombinedFrequencies93.xlsx")

# Interventions ####
DataIntervention <- table(Data$publication, Data$intervention_number) |>
  as.data.frame() |> # create table of subgroups within each intervention
  filter(Freq != 0) |>
  arrange(Var1)
names(DataIntervention) <- c("Reference", "Intervention Number",
                             "Number of Subgroups")
#openxlsx::write.xlsx(DataIntervention, "data/DataIntervention.xlsx")

# DV description table ####
Data$dv <- "Other"
Data$dv[Data$takeup == 1] <- "Takeup"
Data$dv[Data$application == 1] <- "Application"
Data$dv <- as.factor(Data$dv)
DVDescription <- table(Data$program, Data$dv) |>
  as.data.frame.matrix() |> # create table with number of subgroups in which
  # each program's application and take-up is tested
  rownames_to_column("Social program") # Apply rownames to column
#openxlsx::write.xlsx(DVDescription, "data/DVs.xlsx")

# Information for thematic summary tables and funnel plots ####
Data$prop_positive_treatment <- Data$percent_positive_treatment / 100
Data$prop_positive_control <- Data$percent_positive_control / 100
Data$effectsize <- Data$prop_positive_treatment - Data$prop_positive_control
Data$se <- sqrt((Data$prop_positive_treatment *
 (1 - Data$prop_positive_treatment) / Data$treatment_size) +
 (Data$prop_positive_control * (1 - Data$prop_positive_control) /
 Data$control_size)) # calculate standard error
Data$pub_interv_prog <- paste0(# character string with study name,
  # intervention number, additional info and program name
  Data$publication, ", ", Data$intervention_number, ", ",
  Data$result_additional_info, ", ", Data$program) |>
  str_replace_all(",\\s,", ",")
Data$pub_group <- ifelse(
  Data$result_additional_info == "", Data$publication,
  paste(Data$publication, "—", Data$result_additional_info))

# Application data analysis ####
ApplicationData <- filter(Data, application == 1)
ApplicationDataGrouped <- ApplicationData |>
  group_by(pub_group, control_size_positive, control_size_negative) |>
  reframe(treatment_size_positive = sum(treatment_size_positive, na.rm = TRUE),
          treatment_size_negative = sum(treatment_size_negative, na.rm = TRUE),
          result_additional_info = result_additional_info,
          program = program,
          quality_final = as.numeric(mean(quality_final, na.rm = T))) |>
  distinct() # remove duplicates
ApplicationDataGrouped$treatment_size_negative[
  ApplicationDataGrouped$treatment_size_negative == 0] <- NA
ApplicationDataGrouped$treatment_size_positive[
  ApplicationDataGrouped$treatment_size_positive == 0] <- NA
ApplicationDataGrouped$control_size <- # size of control group
 ApplicationDataGrouped$control_size_positive +
  ApplicationDataGrouped$control_size_negative
ApplicationDataGrouped$treatment_size <- # size of treatment group
 ApplicationDataGrouped$treatment_size_positive +
  ApplicationDataGrouped$treatment_size_negative
ApplicationDataGrouped$rr_raw <- 1 + ((( # calculate relative risk
  round(ApplicationDataGrouped$treatment_size_positive, 0) / (round(
    ApplicationDataGrouped$treatment_size_positive, 0) + round(
      ApplicationDataGrouped$treatment_size_negative, 0))) - (round(
        ApplicationDataGrouped$control_size_positive, 0) / (round(
          ApplicationDataGrouped$control_size_positive, 0) + round(
            ApplicationDataGrouped$control_size_negative, 0)))) / (round(
              ApplicationDataGrouped$control_size_positive, 0) / (round(
                ApplicationDataGrouped$control_size_positive, 0) +
                  round(ApplicationDataGrouped$control_size_negative, 0))))

# Takeup data analysis ####
TakeupData <- filter(Data, takeup == 1)
TakeupDataGrouped <- TakeupData |>
  group_by(pub_group, control_size_positive, control_size_negative) |>
  reframe(treatment_size_positive = sum(treatment_size_positive, na.rm = TRUE),
          treatment_size_negative = sum(treatment_size_negative, na.rm = TRUE),
          intervention_type = paste(intervention_type, collapse = ", "),
          result_additional_info = result_additional_info,
          program = program,
          quality_final = as.numeric(mean(quality_final, na.rm = T))) |>
  distinct() # remove duplicates
TakeupDataGrouped$treatment_size_negative[
  TakeupDataGrouped$treatment_size_negative == 0] <- NA
TakeupDataGrouped$treatment_size_positive[
  TakeupDataGrouped$treatment_size_positive == 0] <- NA
TakeupDataGrouped$control_size <- # size of control group
 TakeupDataGrouped$control_size_positive +
  TakeupDataGrouped$control_size_negative
TakeupDataGrouped$treatment_size <- # size of treatment group
 TakeupDataGrouped$treatment_size_positive +
  TakeupDataGrouped$treatment_size_negative
TakeupDataGrouped$rr_raw <- 1 + ((( # calculate relative risk
  round(TakeupDataGrouped$treatment_size_positive, 0) / (round(
    TakeupDataGrouped$treatment_size_positive, 0) + round(
      TakeupDataGrouped$treatment_size_negative, 0))) - (round(
        TakeupDataGrouped$control_size_positive, 0) / (round(
          TakeupDataGrouped$control_size_positive, 0) + round(
            TakeupDataGrouped$control_size_negative, 0)))) / (round(
              TakeupDataGrouped$control_size_positive, 0) / (round(
                TakeupDataGrouped$control_size_positive, 0) +
                  round(TakeupDataGrouped$control_size_negative, 0))))

# Plots ####
## Application meta-analysis
ApplicationDataGrouped$rr_reduction <- 100 *
 (1 - ApplicationDataGrouped$rr_raw) # relative risk reduction
ApplicationDataGrouped$ar_reduction <- # absolute risk reduction
  (abs(ApplicationDataGrouped$control_size_positive) /
     (abs(ApplicationDataGrouped$control_size))) * 100 -
  (abs(ApplicationDataGrouped$treatment_size_positive) /
     (abs(ApplicationDataGrouped$treatment_size))) * 100
ApplicationDataGrouped <- filter( # remove programs when data is lacking or
  # two control groups are superseded (in Goldin, EITC privileged over CTC)
  ApplicationDataGrouped, program != "CTC" &
  str_detect(pub_group, "Gestel|Manoli|Avery") == FALSE)
MetaApplication <- meta::metabin( # perform meta-analysis
  event.e = round(treatment_size_positive, 0), n.e = treatment_size,
  event.c = round(control_size_positive, 0), n.c = control_size,
  studlab = pub_group, data = ApplicationDataGrouped, method.tau = "PM")
# PM = Paule-Mandel estimator for between-study variance
summary(MetaApplication)
## Application forest plot ####
png("graphs/application_forest.png", width = 1100, height = 850)
meta::forest(MetaApplication, sortvar = rr_raw, common = F, random = F,
             xlim = c((1 / 40), 40), weight.study = "same",
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c",
                          "rr_reduction", "ar_reduction", "quality_final"),
             leftlabs = c("Study", "Events", "Total", "Events", "Total",
                          "Relative Risk\nReduction",
                          "Absolute Risk\nReduction", "Study\nQuality (/10)"))
dev.off()
## Application funnel plot ####
col.contour <- c("gray75", "gray85", "gray95")
png("graphs/application_funnel.png", width = 550, height = 425)
meta::funnel(MetaApplication, common = F, random = T, # random effects model
             contour = c(0.9, 0.95, 0.99), col.contour = col.contour,
             log = "x")
legend(x = 5.5, y = 0.2, c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)
dev.off()
## Application publication bias plot ####
ApplicationData$t <- ApplicationData$effectsize / ApplicationData$se
ApplicationData$t_5_96 <- ApplicationData$t
ApplicationData$t_5_96[ApplicationData$t > 5.96] <- 6
ggplot(ApplicationData, aes(x = t_5_96)) +
  geom_histogram(binwidth = 1, breaks = seq(-2.04, 6.96, by = 1),
                 color = "black", fill = "white") +
  scale_x_continuous("t-value", breaks = c(-1.96, -0.96, 0, 0.96, 1.96, 2.96,
                                           3.96, 4.96, 5.96)) +
  ylab("Frequency") +
  geom_vline(aes(xintercept = 1.96))
ggsave("graphs/application_pub_bias_plot.png", width = 11, height = 8.5)

## Takeup meta-analysis ####
TakeupDataGrouped$rr_reduction <- 100 * (1 - TakeupDataGrouped$rr_raw)
  # relative risk reduction
TakeupDataGrouped$ar_reduction <- # absolute risk reduction
  (abs(TakeupDataGrouped$control_size_positive) /
     (abs(TakeupDataGrouped$control_size))) * 100 -
  (abs(TakeupDataGrouped$treatment_size_positive) /
     (abs(TakeupDataGrouped$treatment_size))) * 100
TakeupDataGrouped <- filter( # remove programs when data is lacking or
  # two control groups are superseded (in Bettinger, Pell grant privileged over
  # federal student loan
  TakeupDataGrouped, program != "federal student loan" &
  str_detect(pub_group, "Gestel|Manoli|Avery") == FALSE)
MetaTakeup <- meta::metabin( # meta-analysis
  event.e = abs(treatment_size_positive), n.e = treatment_size,
  event.c = abs(control_size_positive), n.c = control_size,
  studlab = pub_group, data = TakeupDataGrouped, method.tau = "PM")
summary(MetaTakeup)
## Takeup forest plot ####
png("graphs/takeup_forest.png", width = 1100, height = 850)
meta::forest(MetaTakeup, sortvar = rr_raw, common = F, random = F,
             xlim = c((1 / 40), 40), weight.study = "same",
             leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c",
                          "rr_reduction", "ar_reduction", "quality_final"),
             leftlabs = c("Study", "Events", "Total", "Events", "Total",
                          "Relative Risk\nReduction",
                          "Absolute Risk\nReduction", "Study\nQuality (/10)"))
dev.off()
## Takeup funnel plot ####
png("graphs/takeup_funnel.png", width = 550, height = 425)
meta::funnel(MetaTakeup, common = F, random = T, # random effects model
             contour = c(0.9, 0.95, 0.99), col.contour = col.contour,
             log = "x")
legend(x = 3.5, y = 0.3, c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)
dev.off()
## Takeup publication bias plot ####
TakeupData$t <- TakeupData$effectsize / TakeupData$se
TakeupData$t_5_96 <- TakeupData$t
TakeupData$t_5_96[TakeupData$t > 5.96] <- 6
ggplot(TakeupData, aes(x = t_5_96)) +
  geom_histogram(binwidth = 1, breaks = seq(-2.04, 6.96, by = 1),
                 color = "black", fill = "white") +
  scale_x_continuous("t-value", breaks = c(-1.96, -0.96, 0, 0.96, 1.96, 2.96,
                                           3.96, 4.96, 5.96)) +
  ylab("Frequency") +
  geom_vline(aes(xintercept = 1.96))
ggsave("graphs/takeup_pub_bias_plot.png", width = 11, height = 8.5)

# Thematic summary tables ####
unique(ApplicationData$pub_interv_prog) == ApplicationData$pub_interv_prog
ApplicationThemSummaries <- ApplicationData |>
  rename(`Intervention type` = intervention_type) |>
  group_by(`Intervention type`) |> # create table of intervention type by
  # effect size and confidence level
  reframe(`Number of tests` = n(),
          `Not statistically significant` = sum(
            str_count(significant_no, "X"), na.rm = T),
          `Approaching statistical significance` = sum(
            str_count(significant_1, "X"), na.rm = T),
          `Statistically significant, negative` = sum(
            str_count(direction_negative[significant_05 == "X"], "X"),
             na.rm = T),
          `Statistically significant, positive` = sum(
            str_count(direction_positive[significant_05 == "X"], "X"),
             na.rm = T),
          `Unknown statistical significance, negative` = sum(
            str_count(direction_negative[significant_05 == "-"], "X"),
             na.rm = T),
          `Unknown statistical significance, positive` = sum(
            str_count(direction_positive[significant_05 == "-"], "X"),
             na.rm = T),
          `Sources` = paste(pub_interv_prog, collapse = "; "))
#openxlsx::write.xlsx(ApplicationThemSummaries,
#                     "data/ApplicationThemSummaries.xlsx")
unique(TakeupData$pub_interv_prog) == TakeupData$pub_interv_prog
TakeupThemSummaries <- TakeupData |>
  rename(`Intervention type` = intervention_type) |>
  group_by(`Intervention type`) |> # create table of intervention type by
  # effect size and confidence level
  reframe(`Number of tests` = n(),
          `Not statistically significant` = sum(
            str_count(significant_no, "X"), na.rm = T),
          `Approaching statistical significance` = sum(
            str_count(significant_1, "X"), na.rm = T),
          `Statistically significant, negative` = sum(
            str_count(direction_negative[significant_05 == "X"], "X"),
             na.rm = T),
          `Statistically significant, positive` = sum(
            str_count(direction_positive[significant_05 == "X"], "X"),
             na.rm = T),
          `Unknown statistical significance, negative` = sum(
            str_count(direction_negative[significant_05 == "-"], "X"),
             na.rm = T),
          `Unknown statistical significance, positive` = sum(
            str_count(direction_positive[significant_05 == "-"], "X"),
             na.rm = T),
          `Sources` = paste(pub_interv_prog, collapse = "; "))
#openxlsx::write.xlsx(TakeupThemSummaries, "data/TakeupThemSummaries.xlsx")

# Calculate p-values from regression table outputs ####
coefficient <- 0.012
standard_error <- 0.005
sample_size <- 57890
parameters_estimated <- 5
# Including intercept and all categories of categorical variables
t_statistic <- coefficient / standard_error
degrees_of_freedom <- sample_size - parameters_estimated
p_value <- 2 * pt(abs(t_statistic), df = degrees_of_freedom,
 lower.tail = FALSE)
print(p_value)