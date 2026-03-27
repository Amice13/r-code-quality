########### CREATE PAMPHLET-LEVEL DATA ##########
# List all objects in the environment
all_objects <- ls()

# Specify the objects to keep
objects_to_keep <- c("lib", 'wd')

# Identify objects to remove
objects_to_remove <- setdiff(all_objects, objects_to_keep)
rm(list = objects_to_remove)

# Set working directory
setwd(wd)
comp <- read.csv("compliance.csv")

# Correct spelling of assembly constituency names
comp$Assembly_name[comp$Assembly_name=="Jamshedpur.East"] <- "Jamshedpur East"
comp$Assembly_name[comp$Assembly_name=="Jamshedpur.West"] <- "Jamshedpur West"
comp$Assembly_name[comp$Assembly_name=="Jarmundi"] <- "Jarmundi"
comp$Assembly_name[comp$Assembly_name=="Khijri"] <- "Khijri"
comp$Assembly_name[comp$Assembly_name=="Jamua"] <- "Jama"

# CREATE PAMPHLET-LEVEL DATASET
pamphlets <- comp[rep(row.names(comp), comp$Pamphlet_received), c(1:7)]
sum(comp$Pamphlet_received) #OK


# CREATE UNIQUE ID FOR EACH PAMPHLET
set.seed(20190426)
pamphlets = pamphlets[order(pamphlets$Assembly_name, 
                            pamphlets$Treatment_assigned, 
                            pamphlets$name_order),]
pamphlets$pamph_ID = 1:nrow(pamphlets)


# Get onboarding survey
onboarding <- read.csv("onboarding_survey.csv")

# Save baseline respondents
baseline = onboarding %>% filter(baseline==1)
write.csv(baseline, "baseline_observations.csv")


# Clean assembly names
onboarding$assembly_name = trimws(onboarding$assembly_mcs) %>% toupper()

# Drop Gaya and Jharkhand MADANA, MEH ANSARI,MANTA, MEJARI NAGAR,MEKULAM,MUNA BHAI,
# NIRDLIYE, PALAMU, PASHCHIMI SINGHBHUM,PEHRAPURTI, PURBI SINGHBHUM, SSHIKARSENI, 
# TALAJOORI from the sample, because these are not located in Jharkhand

onboarding$assembly_name = ifelse(onboarding$assembly_name=="BAGHMARA", "BAHGMARA", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="BAHRAGORA", "Bahragora", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="BARHI"|onboarding$assembly_name=="BARI"  , "BARHI", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="BAKRAGAON"|onboarding$assembly_name=="BARKA GAO"|
                                onboarding$assembly_name=="BARKAGAON"|onboarding$assembly_name=="BARKGAON", "Barkagaon", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="BARKATTA"|onboarding$assembly_name=="BARKHATA"|
                                onboarding$assembly_name=="BARKHTTA"|onboarding$assembly_name=="BARKTTA", "Barkatha", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="BHAWNATHPUR" , "Bhawnathpur", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="BHISAM PUR"|onboarding$assembly_name=="BISHRAAMPUR"|
                                onboarding$assembly_name=="BISHRAMPUR"|onboarding$assembly_name=="VISRAMPUR"|
                                onboarding$assembly_name=="BISHRAMPUR", "BIRSHRAMPUR", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="BUKARO", "BOKARO", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="CHAKARDHARPUR", "Chakradharpur", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="CHATTARPUR"|onboarding$assembly_name=="CHHATARPUR", "Chhatarpur", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="BHANDARIYA"|onboarding$assembly_name=="BHANDRIA"|
                                onboarding$assembly_name=="DALTANGANJ"|onboarding$assembly_name=="DALTEN GANJ"|
                                onboarding$assembly_name=="DALTENGANJ"|onboarding$assembly_name=="DALTONGANJ", "Daltonganj", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="DUMARI", "DUMRI", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="GIRIDIH"|onboarding$assembly_name=="SHALI"|
                                onboarding$assembly_name=="GIRIDH", "GIRIDIH", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="DURIYA", "Gumla", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="HAZRIBAGH", "Hazaribagh", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="HUSSAINABAAD"|onboarding$assembly_name=="HUSSAINABAD", "Hussainabad", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="JAGANNATHPUR", "Jaganathpur", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="JAMESHEDPUR EAST", "Jamshedpur East", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="JAMSHEDPUR WEST", "Jamshedpur West", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="JAMTARA"|onboarding$assembly_name=="JANTRA", "Jamtara", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="JHARMUNDI"|onboarding$assembly_name=="JARMUNDI", "JARMUNDIH", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="JAMA", "JAMA", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="JARIA"|onboarding$assembly_name=="JHARIA", "Jharia", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="KHIJARI"|onboarding$assembly_name=="KHIJRI", "KHIJARI", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="KODERMA", "Kodarma", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="LOHARDEGA", "Lohardaga", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="MAHAGAMA", "Mahgama", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="MAANDU", "Mandu", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="NALA", "Nala", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="PARIYAHAAT"|onboarding$assembly_name=="POREYAHAAT", "Poreyahat", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="RAHCI", "RANCHI", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="SHIMDEHGA", "Simdega", onboarding$assembly_name)
onboarding$assembly_name = ifelse(onboarding$assembly_name=="SINDHRI", "Sindri", onboarding$assembly_name)

wrong = c("GAYA", "JHARKHAND", "MADANA", "MEH ANSARI", "MANTA" , "MEJARI NAGAR", 
          "MEKULAM" , "MUNA BHAI", "NIRDLIYE" ,  "PALAMU", "PASHCHIMI SINGHBHUM",
          "PEHRAPURTI", "PURBI SINGHBHUM", "SSHIKARSENI", "TALAJOORI", "पता नहीं")

onboarding$assembly_name = ifelse(onboarding$assembly_name %in% wrong,"", onboarding$assembly_name)
onboarding$assembly_name = trimws(onboarding$assembly_name) %>% toupper()
onboarding = onboarding %>% rename(assembly_name_mcs_clean = assembly_name)
# Drop those that are duplicates or have missing assembly names
onboarding = onboarding %>% filter(!is.na(pamph_ID_treat))


# Merge with recall survey
recall = read.csv("recall_survey.csv")
onboarding = onboarding %>%
  left_join(recall)


# Merge with pamphlet dataset
pamphlets = pamphlets %>%
  left_join(onboarding, by = c("pamph_ID" = "pamph_ID_treat"))


# Save 
write.csv(pamphlets, "pamphlets_level.csv", row.names = F)




