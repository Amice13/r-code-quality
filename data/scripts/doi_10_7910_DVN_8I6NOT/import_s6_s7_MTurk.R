############################################### #
# The Psychology of Online Political Hostility  #
#                                               #
# This code imports and cleans the data file    #
#       for Studies 4-5                         #
#       from MTurk                              #
#                                               #
# 2019.08.22.                                   #
# code by XXXXXXXXXXX                           #
############################################### #

library(tidyverse)
library(rio)
library(here)
source("C.code/functions.R")

# standard error function
se <- function(x){
        xnn <- x[!is.na(x)]  # x with no na
        (sd(xnn)/sqrt(length(xnn)))
}

# flips reverse coded items 
flip <- function(x){
        minimum <- min(x, na.rm = T)
        maximum <- max(x, na.rm = T)
        
        (((x - maximum) * -1 ) + minimum)
}

# function to recode anes pid variable
pid <- function(xd, xr, xi){
        require(dplyr)
        x <- case_when(xd == 1 ~ 1, 
                       xd == 2 ~ 2, 
                       xi == 2 ~ 3, 
                       xi == 3 ~ 4, 
                       xi == 1 ~ 5, 
                       xr == 2 ~ 6, 
                       xr == 1 ~ 7)
        x
}

# custom recode function combines flipping and rescale to 0-1
recode.godwin <- function(x, flip = T, zero.one = T, twosd = F, max = 7){
        
        # replace na values with NA
        # x <- ifelse(x %in% na.val, NA, x)
        
        minimum <- 1
        maximum <- max
        
        if(flip == T)
                x <- (((x - maximum) * -1 ) + minimum)
        
        if(zero.one == T) {
                x <- (x - minimum) / (maximum - minimum)}
        
        if(twosd == T){
                x <- (x - mean(x, na.rm = TRUE))/(2 * sd(x, na.rm = TRUE))
        }
        
        x
        
} 

twoSD <- function(x){
        (x - mean(x, na.rm = TRUE))/(2 * sd(x, na.rm = TRUE))
}

############################################################################### #
# Pretests                                                              #########
############################################################################### #
        

# import first stimulus pretest data
rawpredf <- import("A.original.data/mturk_pretest_1.csv")

# glimpse(rawpredf)


# Round 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# reshape to long format
predf <- rawpredf %>% 
        gather(target, rating, `1_target`: `24_target`, na.rm = T) %>%
        separate(target, "target", "_") %>% 
        transmute(
                position = ifelse(as.double(target) >= 13, "anti_immig", "pro_immig"),
                target = ifelse(position == "anti_immig", as.double(target)-12, 
                                as.double(target)),
                rating = rating / 6,
                agree = agree,
                
                year = year,
                female = sex -1, 
                pid = ifelse(pid == 7, NA, pid) / 6, 
                democrat = ifelse(pid > 0.5, 1, 0), 
                policy = ifelse(immig == 1, "satisfied", "dissatisfied")) 
# will produce a warning because we cut down parts of a string


# calculate mean and sd for each comment 

results <- predf %>% 
        dplyr::select(target, position, rating) %>% 
        group_by(target, position) %>% 
        summarise(mean  = mean(rating), 
                  se = se(rating), 
                  sd = sd(rating), 
                  n = n()) %>%
        filter(target == 2 | target == 3 | target == 7 | target == 8)%>% 
        ungroup()
# write.csv(results, file = "C.results/pretest1_results.csv")


# Round 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# import second stimulus pretest data
rawpredf2 <- import("A.original.data/mturk_pretest_2.csv")

# glimpse(rawpredf2)


# clean data
# reshape to long format
predf2 <- rawpredf2 %>% 
        dplyr::select(year, sex, pid, immig, agree, contains("target")) %>% 
        gather(target, rating, `1_target`: `10_target`, na.rm = T) %>% 
        separate(target, "target", "_") %>% 
        transmute(position = ifelse(as.double(target) >= 6, "anti_immig", "pro_immig"),
                  target = ifelse(position == "anti_immig", as.double(target)-5, 
                                  as.double(target)),
                  rating = rating / 6,
                  agree = agree,
                  
                  year = year,
                  female = sex -1, 
                  pid = ifelse(pid == 7, NA, pid) / 6, 
                  democrat = ifelse(pid > 0.5, 1, 0), 
                  policy = ifelse(immig == 1, "satisfied", 
                                  ifelse(immig == 2, "dissatisfied", "neither"))) 
        
# will produce a warning because we cut down parts of a string


# calculate mean and se for each comment 
results2 <- predf2 %>% 
        dplyr::select(target, position, rating) %>% 
        group_by(target, position) %>% 
        summarise(mean  = mean(rating), 
                  se = se(rating), 
                  n = n()) %>% 
        filter(target != 3) %>% 
        ungroup() %>% 
        mutate(target = case_when(target == 5 ~ 8, 
                                  TRUE ~ target))

# merge the mean ratings from the two pretests. 
results_merge <- results  %>% 
        mutate(target = case_when(target == 2 ~ 3, 
                                  target == 3 ~ 5, 
                                  target == 7 ~ 6, 
                                  target == 8 ~ 7)) %>% 
        bind_rows(results2) %>% 
        arrange(target)
        
# mean number of raters 
mean(results_merge$n)
range(results_merge$n)

# plot the ratings of the 2x8 final target posts
ggplot(results_merge, aes(x = target, y = mean, color = position)) + 
        geom_point(position = position_dodge(0.2)) + 
        geom_errorbar(aes(ymin = mean - 1.96*se, max = mean + 1.96*se), width = 0,
                      position = position_dodge(0.2)) + 
        # geom_errorbar(aes(ymin = mean - 1.4*se, max = mean + 1.4*se), width = 0.2,
        #               position = position_dodge(0.2)) + 
        scale_x_continuous(breaks = 1:8) + 
        scale_color_discrete(name = "Position", 
                             labels = c("Anti immigration", "Pro immigration")) +
        xlab("Hostility manipulation") +  
        ylab("Group means and CIs") + 
        coord_flip() +
        theme_bw()

# ggsave(file = "D.documents/figures/figf1_target_hostility.jpg")

############################################################################### #
# Study 4                                                               #########
############################################################################### #



# import data 
rawdf3 <- import("A.original.data/mturk_s6.csv") %>% 
        dplyr::select(-contains("_DO"))

# glimpse(rawdf3)

# fix annoying issue with typos in birth-year 
# fix birth for overly transparent respondents
rawdf3$birth[nchar(rawdf3$birth)>4] <- c(1967, 1986, 1962, 1963, 1969, 1967, 
                                         1996, 1991, 1990, 1990)

# update variable names 
# hostility ratings from loop & merge

hostility_items <- which(names(rawdf3) == "1_select_pol"):
        which(names(rawdf3) == "16_select_pol")

names(rawdf3)[hostility_items] <- paste(rep(c("anti", "pro"), each = 8), 
                                        1:8, sep = "_")

# duration
names(rawdf3)[which(names(rawdf3) == "Duration (in seconds)")] <- "duration"
# discussing politics with various groups 
talk_items <- which(names(rawdf3) == "talkpol_1"):
        which(names(rawdf3) == "talkpol_5")

names(rawdf3)[talk_items] <- c("talk_family", "talk_cowork", "talk_strange", 
                               "talk_agree", "talk_disagree")


# store names of personality measures
riskseek <- paste0("riskseek_", 1:14)
aggression <-  paste0("aggression_", 1:12)
NFC <- paste0("NFC_", 1:8)
emotionreg <- paste0("emotionreg_", 1:16)
hostile <- paste0("hostile_", 1:5)
distrust <- paste0("distrust_", 1:8)
talkpol <- c("talk_family", "talk_cowork", "talk_strange",
             "talk_agree", "talk_disagree")

df3 <- rawdf3 %>% 
        gather(target, select_pol, anti_1:pro_8, na.rm = T) %>% 
        separate(target, c("position", "target"), sep = "_") %>% 
        rename(sub_hostile = hostile1_9) %>% 
        mutate(# status driven sdrting
                riskseek_1 = flip(riskseek_1), 
                riskseek_7 = flip(riskseek_7), 
                riskseek_8 = flip(riskseek_8), 
                riskseek_12 = flip(riskseek_12), 
                riskseek_14 = flip(riskseek_14), 
                
                distrust_3 = flip(distrust_3), 
                distrust_4 = flip(distrust_4), 
                distrust_5 = flip(distrust_5), 
                distrust_6 = flip(distrust_6), 
                
                # trait aggression
                aggression_2 = flip(aggression_2), 
                
                target_cont = (as.numeric(target)-1)/7,
                
                # pid 
                partyid = recode.godwin(pid(xd = pidd, xr = pidr,
                                            xi = pidi), flip = F),
                
                # absolute diff between two thermometer rating
                affectivepol = abs(thermo_2-thermo_1),
                
                # relative preference for own party
                affectivepol2 = ifelse(partyid < 0.34, thermo_1 - thermo_2, 
                                       ifelse(partyid > 0.65, thermo_2 - thermo_1, 
                                              NA)), 
                
                immig = fct_recode(as.factor(immig), satisfied = '1', 
                                   dissatisfied = "2", 
                                   undecided = "3"),
                
                congruent = ifelse(immig == "satisfied" & position == "pro" | 
                                           immig == "dissatisfied" & position == "anti", 
                                   1, 
                                   ifelse(immig == "undecided", NA, 0)),
                
                # DV 1 selected political comment 
                select_pol = ifelse(select_pol == 2, 0, select_pol),
                
                highered = ifelse(edu >= 4, 1, 0),
                age_year =  2019 - as.numeric(birth),
                age = zero.one(age_year, min = 18, max = 99),
                female = sex-1,
                
                # pass_comp = ifelse(
                        # comprehension == "2,5" | comprehension == "2" | comprehension == "5",
                        # 1, 0
                # ), 
                attentive = case_when(comprehension == "2,5" ~ "full", 
                                      comprehension == "2" | comprehension == "5" ~ "partly", 
                                      TRUE ~ "not"), 
                ID = 1:nrow(.)
        ) 

# personality vars mean centered and divided by two SD 
#       this helps with multilevel models 
df3$sdrt <-   recode.godwin(rowMeans(df3[, riskseek]), 
                              zero.one = T, twosd = F)
df3$aggression <- recode.godwin(rowMeans(df3[, aggression]), 
                                  zero.one = T, twosd = F)
df3$cfc <-        recode.godwin(rowMeans(df3[, NFC]), 
                                  zero.one = T, twosd = F)
df3$ders <- recode.godwin(rowMeans(df3[, emotionreg]), flip = F,
                            zero.one = T, twosd = F, max = 5)
df3$hostile <- recode.godwin(rowMeans(df3[, hostile]), flip = F, 
                               zero.one = T, twosd = F)
df3$distrust <- recode.godwin(rowMeans(df3[, distrust]), flip = F, 
                                zero.one = T, twosd = F)
df3$talkpol <- recode.godwin(rowMeans(df3[, talkpol]), flip = F, 
                               zero.one = T, twosd = F)

df3$affectivepol_c <- (df3$affectivepol - mean(df3$affectivepol, na.rm = TRUE))/
        (2 * sd(df3$affectivepol, na.rm = TRUE))


vars.s4 <- c(aggression, "aggression", riskseek, "sdrt", hostile, "hostile", 
		emotionreg, "ders", "select_pol", "comment", "sub_hostile")
control.s4 <- c("age", "age_year", "highered", "female", "attentive", "pid", 
		"partyid", "position", "target", "immig")

df3.clean <- dplyr::select(df3, vars.s4, control.s4)

saveRDS(df3.clean, file = "B.analysis.data/s6_exp.rds")
write.csv(df3.clean, file = "B.analysis.data/s6_exp.csv")

############################################################################### #
# Study 4 - Crowdsource                                                 #########
############################################################################### #


rawdf3.crowd <- import("A.original.data/mturk_s6_crowd.csv")
nrow(rawdf3.crowd)
# store number of comments in the data frame
n <- 781

# identify first and last col with ratings
first <- which(names(rawdf3.crowd)=="Q11_9(1)")
last <- which(names(rawdf3.crowd)=="Q11_9(781)")

# update variable names 
# hostility ratings from loop & merge
names(rawdf3.crowd)[first:last] <- paste("c", 1:n, sep = "_")

# calculate average rating for each comment 

results3 <- data.frame(commentID = 1:n, 
                      crowd.mean=colMeans(dplyr::select(rawdf3.crowd, 
                                                        c_1:paste0("c_", n)), 
                                          na.rm = T) / 100, 
                      crowd.n = apply(dplyr::select(rawdf3.crowd, 
                                                    c_1:paste0("c_", n)), 2,
                                      function(x) sum(!is.na(x))),
                      crowd.sd = apply(dplyr::select(rawdf3.crowd, 
                                                     c_1:paste0("c_", n)), 2,
                                       function(x) sd(x, na.rm = T))
)


df3_pol <- df3.clean %>% 
        filter(select_pol == 1 & attentive != "not") %>% 
        mutate(polID = 1:n)

# merge original df with ratings 
df3_pol <- df3_pol %>% 
        left_join(results3, by=c("polID" = "commentID")) 


saveRDS(df3_pol, file = "B.analysis.data/s6_crowd.rds")
write.csv(df3_pol, file = "B.analysis.data/s6_crowd.csv")

############################################################################### #
# Study 5                                                               #########
############################################################################### #

# import data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rawdf4 <- import("A.original.data/mturk_s7.csv")


# update variable names 
# hostility ratings from loop & merge
names(rawdf4)[85:100] <- paste(rep(c("anti", "pro"), each = 8), 1:8, sep = "_")
# retrieved hostility rating
names(rawdf4)[102] <- "hostile_retrieve" 
# duration
names(rawdf4)[6] <- "duration"
# discussing politics with various groups 
names(rawdf4)[27:31] <- c("talk_family", "talk_cowork", "talk_strange", 
                          "talk_agree", "talk_disagree")


# store names of personality measures
riskseek <- names(rawdf4)[33:46]
aggression <- names(rawdf4)[47:58]
NFC <- names(rawdf4)[59:66]
emotionreg <- names(rawdf4)[67:82]



# clean & reshape dataframe ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df4 <- rawdf4 %>% 
        gather(target, hostile, anti_1:pro_8, na.rm = T) %>% 
        separate(target, c("position", "target"), sep = "_") %>% 
        mutate(# status driven riskseeking
                riskseek_1 = flip(riskseek_1), 
                riskseek_7 = flip(riskseek_7), 
                riskseek_8 = flip(riskseek_8), 
                riskseek_12 = flip(riskseek_12), 
                riskseek_14 = flip(riskseek_14), 
                
                # trait aggression
                agression_2 = flip(aggression_2),
                
                # pid 
                partyid = recode.godwin(pid(xd = pidd, xr = pidr,
                                            xi = pidi), flip = F),
                
                # absolute diff between two thermometer rating
                affectivepol = abs(thermo_2-thermo_1),
                
                # relative preference for own party
                #        
                affectivepol2 = ifelse(partyid <= 3, thermo_1 - thermo_2, 
                                       ifelse(partyid >= 5, thermo_2 - thermo_1, 
                                              NA)), 
                
                immig = fct_recode(as.factor(immig), satisfied = '1', 
                                   dissatisfied = "2", 
                                   undecided = "3"),
                female = sex -1, 
                highered = ifelse(edu >= 4, 1, 0)
        ) %>% 
        filter(comprehension == "1,4" | comprehension == "1") 
stopifnot(nrow(df4) == 1640)
df4$ID <- 1:1640

# fix annoying typos in birth year 
df4$birth <- fct_recode(df4$birth, `1980` = '198000', `1993` = "19933", 
                        `1983` = "One thousand nine hundred eighty three")
df4$birth <- as.numeric(str_sub(df4$birth, start = -4))

df4$age_year <- (2018-df4$birth)
df4$age <- zero.one(df4$age_year, min = min(df4$age_year), max = max(df4$age_year))

# personality vars mean centered and divided by two SD 
#       this helps with multilevel models 
df4$sdrt <-   recode.godwin(rowMeans(df4[, riskseek]), 
                                  zero.one = T, twosd = F)
df4$aggression <- recode.godwin(rowMeans(df4[, aggression]), 
                                  zero.one = T, twosd = F)
df4$cfc <-        recode.godwin(rowMeans(df4[, NFC]), 
                                  zero.one = T, twosd = F)
df4$ders <- recode.godwin(rowMeans(df4[, emotionreg]), flip = F,
                                  zero.one = T, twosd = F, max = 5)


df4$congruent <- ifelse(df4$immig == "satisfied" & df4$position == "pro" | 
                                df4$immig == "dissatisfied" & df4$position == "anti", 
                        1, 
                        ifelse(df4$immig == "undecided", NA, 0))
                        
                        
vars.s5 <- c(aggression, "aggression", riskseek, "sdrt", emotionreg, "ders", 
		"comment", "hostile", "hostile_retrieve")
control.s5 <- c("age", "age_year", "highered", "female",  "pid", 
		"partyid", "position", "target", "immig", "ID")

df4 <- dplyr::select(df4, vars.s5, control.s5)


############################################################################### #
# Study 5 - Crowdsource                                                 #########
############################################################################### #

# import data ############
rawdf4a <- import("A.original.data/mturk_s7_crowd_a.csv")
rawdf4b <- import("A.original.data/mturk_s7_crowd_b.csv")
rawdf4.crowd <- bind_rows(rawdf4a, rawdf4b)

first <- which(names(rawdf4.crowd)=="hostile_9(1)")
last <- which(names(rawdf4.crowd)=="hostile_9(1640)")

# update variable names 
# hostility ratings from loop & merge
names(rawdf4.crowd)[first:last] <- paste("c", 1:1640, sep = "_")

# calculate average rating for each comment 


results4 <- data.frame(commentID = 1:1640, 
                      crowd.mean=colMeans(dplyr::select(rawdf4.crowd, c_1:c_1640), 
                                          na.rm = T)/100, 
                      crowd.n = apply(dplyr::select(rawdf4.crowd, c_1:c_1640), 2, 
                                      function(x) sum(!is.na(x))),
                      crowd.sd = apply(dplyr::select(rawdf4.crowd, c_1:c_1640), 2, 
                                       function(x) sd(x, na.rm = T))
)

df4 <- left_join(df4, results4,  by=c("ID" = "commentID"))

# avg rating of 16 target  from study 3 
target.ratings.s4 <- df3_pol %>% 
        dplyr::select(target, position, sub_hostile) %>% 
        group_by(target, position) %>% 
        dplyr::summarise(target.mean.s4  = mean(sub_hostile, na.rm =T)/100) %>% 
        ungroup() 

# avg rating of 16 target  from study 5
target.ratings.s5 <- df4 %>% 
        dplyr::select(target, position, hostile) %>% 
        group_by(target, position) %>% 
        dplyr::summarize(target.mean.s5  = mean(hostile)/100) %>% 
        ungroup() 

target.ratings <- left_join(target.ratings.s4, target.ratings.s5)
saveRDS(target.ratings, file="B.analysis.data/s7_targetratings.rds")

df4 <- df4 %>% left_join(target.ratings)

df4$diff.s4 <- df4$crowd.mean - df4$target.mean.s4 
df4$diff.s5 <- df4$crowd.mean - df4$target.mean.s5 

saveRDS(df4, file = "B.analysis.data/s7_crowd.rds")
write.csv(df4, file = "B.analysis.data/s7_crowd.csv")
