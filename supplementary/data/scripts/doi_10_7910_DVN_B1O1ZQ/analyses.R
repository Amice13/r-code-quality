####   Authors: Scherer, H. and Phillips, J.                      ###
####                                                              ###
####   Title:   Top-Down Effects on Anthropomorphism of a Robot   ###
####                                                              ###
####   Contact: hailey.a.scherer.20@dartmouth.edu                 ###
####            jonathan.s.phillips@dartmouth.edu                 ###

# Set working directory
setwd("~/Dropbox_Hailey.a.scherer/Dropbox/Robots1/_Final/SchererPhillips")
setwd("/Users/jonat/Dropbox/Robots1/_Final/SchererPhillips")

######## LOAD REQUIRED LIBRARIES ########

library(tidyverse)
library(dplyr)
library(irr)
library(psych)
library(ggplot2)
library(ggpubr)
library(corpcor)
library(GPArotation)
library(rstatix)

######## LOAD THE DATA ########
d0 <- read.csv("data/data_study1.csv")

included <- read.csv("data/data_Included.csv")
included <- included$ResponseId

# Linguistic Analysis data: Rater 1
la1 <- read.csv("data/data_LA_R1.csv")

# Linguistic Analysis data: Rater 2
la2 <-read.csv("data/data_LA_R2.csv")
colnames(la2)[1] <- colnames(la1)[4] # errors are sometimes introduced in reading in the first variable name

# Combine 2 linguistic analysis rater files into one file so icc function can handle it easily
la0 <- left_join(la1,la2,by='ResponseId') %>%
  dplyr::select(4:8,10,14:21,23:34) # select relevant variables

# Consensus data for linguistic analysis
la3 <- read.csv("data/data_LA_C.csv")
colnames(la3)[1] <- colnames(la1)[4]  # errors are sometimes introduced in reading in the first variable name

########CLEAN THE DATA########

## Combine qualtrics raw data and linguistic analysis data
d1 <- left_join(d0,la3,by='ResponseId')

## Clean data
d2 <- d1 %>% filter(Finished) %>%    # Keep only participants who completed the study
  dplyr::select(-c(names(d1[,grep("COMP",names(d1))]))) %>%   # Exclude comprehension check columns
  dplyr::select(-c(names(d1[,grep("TIMER",names(d1))]))) %>%  # Exclude timer columns
  dplyr::select(-c(1:7,9:10,56)) %>%  # Simplify to relevant data: Exclude start date, recorded date, total duration, distribution channel etc., as well as the last comprehension checkpoint
  mutate(      # Create factors for description and video conditions
    treatment = factor(Treatment, levels=c("A1","A2","A3","A4","M1","M2","M3","M4","N1","N2","N3","N4")),
    description = factor(c(rep("Anthropomorphic",4),rep("Mechanistic",4),rep("Neutral",4))[treatment]),
    video = factor(rep(c("Solo Video","Mechanistic Interaction","Social Interaction","No Video"),3)[treatment])
  ) %>%
  dplyr::select(-c("Treatment","OPEN_DESC")) %>% # Get rid of duplicate columns for treatment and open descriptions
  rename(MDIL3.N2_1 = Q272_1) %>% 
  filter(ResponseId %in% included) %>% # Make exclusions # There are 533 rows at this point, one for each participant, but scenario questions are still broken out by condition (to allow in Qualtrics for displaying the relevant video thumbnail and description above each scneario question)
  # Put scenario scores into 5 columns
  gather(question_cond,response,54:233,na.rm=T) %>% # Now 7995 rows: question_cond column takes all the scenario scores (which were broken out by condition) and puts them into one long column
  mutate(
    question_cond = as.character(question_cond),
    question = substring(question_cond,1,5)
  ) %>%  # Make one column for each scenario (regardless of participant condition)
  dplyr::select(-question_cond) %>%
  spread(question,response)  # Back to 533 rows, one for each participant, and each participant has each of their scores for the 15 scenarios in their one row

# Reverse moral dilemma items 1, 2, 3
d2 <- d2 %>%  
  mutate(
    MDIL1 = 100 - MDIL1,
    MDIL2 = 100 - MDIL2,
    MDIL3 = 100 - MDIL3
  )

# Add analysis transformations 
d2 <- d2 %>%
  mutate(excap_anth = ((expcapall_1 + expcapall_3 + expcapall_4 + expcapall_5 + expcapall_7 + expcapall_9 + expcapall_15 + expcapall_16)/8)) %>%
  mutate(excap_mech = ((expcapall_2 + expcapall_6 + expcapall_8 + expcapall_10 + expcapall_11 + expcapall_12 + expcapall_13 + expcapall_14)/8)) %>%
  mutate(excap_total = (((excap_anth + excap_mech)/2))) %>%
  mutate(metaphors_anth = ((metaphors_3 + metaphors_4 + metaphors_5 + metaphors_8 + metaphors_9)/5)) %>%
  mutate(metaphors_mech = ((metaphors_1 + metaphors_2 + metaphors_6 + metaphors_7 + metaphors_10)/5)) %>%
  mutate(experience = ((anth_ExpxAg_5 + anth_ExpxAg_6 + anth_ExpxAg_7 + anth_ExpxAg_8)/4)) %>%
  mutate(agency = ((anth_ExpxAg_1 + anth_ExpxAg_2 + anth_ExpxAg_3 + anth_ExpxAg_4)/4)) %>%
  mutate(phys_trust = ((PHYS1 + PHYS2 + PHYS3 + PHYS4 + PHYS5)/5)) %>%
  mutate(psyc_trust = ((PSYC1 + PSYC2 + PSYC3 + PSYC4 + PSYC5)/5)) %>%
  mutate(total_trust = ((phys_trust + psyc_trust)/2)) %>%
  mutate(moral = ((MDIL1 + MDIL2 + MDIL3 + MDIL4 + MDIL5)/5)) %>%
  mutate(disj_anth = ((mach.hum_1 + nonc.conc_1 + arti.living_1)/3)) %>%
  mutate(disj_ani = ((mechanical.lively_1 + inert.interactive_1 + unresponsive.respons_1)/3)) %>%
  mutate(disj_liking = ((dislika.lika_1 + unhelpf.helpf_1 + unpleas.pleas_1)/3))

# Renaming for ease of viewing
d2 <- d2 %>%
  rename(Agency_Thinking=anth_ExpxAg_1,
         Agency_MakingDecisions=anth_ExpxAg_2,
         Agency_UnderstandingEmotions=anth_ExpxAg_3,
         Agency_Comm=anth_ExpxAg_4,
         Exp_ExpEmotions=anth_ExpxAg_5,
         Exp_FeelAffection=anth_ExpxAg_6,
         Exp_FeelPain=anth_ExpxAg_7,
         Exp_FeelPleasure=anth_ExpxAg_8,
         meta_tool=metaphors_1,
         meta_instrument=metaphors_2,
         meta_teammate=metaphors_3,
         meta_companion=metaphors_4,
         meta_playmate=metaphors_5,
         meta_plaything=metaphors_6,
         meta_toy=metaphors_7,
         meta_friend=metaphors_8,
         meta_partner=metaphors_9,
         meta_gadget=metaphors_10,
         excap_ComfortMe=expcapall_1,
         excap_TakePics=expcapall_2,
         excap_ConveyEmotions=expcapall_3,
         excap_UnderstandEmotions=expcapall_4,
         excap_GiveAdvice=expcapall_5,
         excap_Xray=expcapall_6,
         excap_ShareExp=expcapall_7,
         excap_Fingerprints=expcapall_8,
         excap_Fun=expcapall_9,
         excap_PhoneCalls=expcapall_10,
         excap_Measurem=expcapall_11,
         excap_RecordSounds=expcapall_12,
         excap_AccessMusic=expcapall_13,
         excap_ThermalScan=expcapall_14,
         excap_Convo=expcapall_15,
         excap_Joke=expcapall_16)

######## ANALYSIS ########

####Factor Analysis####

#Analyzing metaphors scale, experience and agency scales, and the 3-item pairwise comparison anthropomorphism scale (21 total items)

##To test data suitability for Factor Analysis: KMO and Bartlett
#For any who would like to visualize the correlation matrix of the variables
cormatrix <- cor(d2[,27:47]) 
head(round(cormatrix, 2)) # For viewing

#Kaiser Meyer Olkin Measure of Sampling adequacy: Tests proportion of variance among samples that might stem from a common source 
KMO(cormatrix) # KMO index = 0.94; >.9 is "marvelous" (Kaiser, 1975)

#Bartlett test: Tests the degree that the matrix deviates from an identity matrix; in other words, the assumption that samples are from populations with equal variance
cortest.bartlett(d2[,27:47]) #Find Bartlett's test is highly significant, [chi squared](210) = 10057.39, p < .001; assumption is verified

#From above, data is very well suited for factor analysis; proceeding with factor analysis

#Select number of factors
pc1 <- principal(d2[,27:47], nfactors=21, rotate="none")
plot(pc1$values, type="b")  #Scree plot
# Selecting a three-factor solution
pc2 <- principal(d2[,27:47], nfactors=3, rotate="varimax")
print.psych(pc2, cut=0.3, digits=4, sort=TRUE)

#Solution in total explains 68.55% of the variance in item responses
# First factor, Anthropomorphism, has an eigenvalue = 10.13, explains 48.25% of variance
# Second factor, Tool Attributions, has an eigenvalue = 2.30, explains 10.96% of variance
# Third factor, Advanced Toy Attributions, has an eigenvalue = 1.96, explains 9.34% of variance

#Extract PC scores and attach to dataset
d3 <- cbind(d2, pc2$scores)
d3 <- d3 %>%
  rename(anthropomorphism=RC1) %>%
  rename(advanced_tool=RC2) %>%
  rename(advanced_toy=RC3)


#### Effect of Description and Video on Anthropomorphism Factor ####

# Visual: Effect of description anthropomorphism
# Prepare data
d3_anthfactor_desc <- d3 %>%
  group_by(description) %>%
  summarise(N = length(anthropomorphism),
            mean = mean(anthropomorphism),
            sd = sd(anthropomorphism),
            se = sd / sqrt(N))
  d3_anthfactor_desc$description <- factor(d3_anthfactor_desc$description, c("Mechanistic", "Neutral", "Anthropomorphic")) #reorder factors

# Build plot
lc <- "#00285C"      # set custom line color
anthfactor_desc_plot <- ggplot(d3_anthfactor_desc, aes(description, mean, group=1)) +
  geom_point(color=lc) +
  geom_line(color=lc, size=1) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=.2), color=lc) +
  theme_classic() +
  labs(x="Description Condition", 
       y="Anthropomorphism") +  # change axes titles
  theme(axis.title.x = element_text(vjust=-0.45)) +                # lower x-axis title
  theme(axis.text.x=element_text(size=10, vjust=0.5)) #+           # make condition names larger
  #ggtitle("The Effect of Description on Anthropomorphism") +      # add title
  #theme(plot.title = element_text(size=13, face="bold", hjust = 0.5,
                                  #margin = margin(5,0,8,0)))    # bigger, bold, centered title
anthfactor_desc_plot  # View plot

# Effect of video on anthropomorphism
# Prepare data
d3_anthfactor_vid <- d3 %>%
  group_by(video) %>%
  summarise(N = length(anthropomorphism),
            mean = mean(anthropomorphism),
            sd = sd(anthropomorphism),
            se = sd / sqrt(N))
  d3_anthfactor_vid$video <- factor(d3_anthfactor_vid$video, c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction")) #reorder factors

# Build plot
lc2 <- "#36001F"     # set custom line color
anthfactor_vid_plot <- ggplot(d3_anthfactor_vid, aes(video, mean, group=1)) +
  geom_point(color=lc2) +
  geom_line(color=lc2, size=1) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=.2), color=lc2) +
  theme_classic() +                                               
  labs(x="Video Condition", 
       y="Anthropomorphism") +  # change axes titles
  theme(axis.title.x = element_text(vjust=-0.45)) +               # Lower x-axis title
  theme(axis.text.x=element_text(size=10, vjust=0.5)) #+          # Make condition names larger
  #ggtitle("The Effect of Video on Anthropomorphism") +           # Add title
  #theme(plot.title = element_text(size=13, face="bold", hjust = 0.5,
                                  #margin = margin(5,0,8,0))) +   # Bigger, bold, centered title
anthfactor_vid_plot  # View plot

# Alternative visualization: interaction plot
ggline(d3, x = "description", y = "anthropomorphism", col = "video", palette = c("#F76B00","#5D5D5D","#0037FF","#D403CE"),
       add = c("mean_se"), 
       order = c("Neutral", "Mechanistic", "Anthropomorphic"),
       ylab = "Anthropomorphism", xlab = "Description Condition")

# Analysis: two-way ANOVA
anthropomorphism.aov <- aov(anthropomorphism ~ description * video, data = d3)
summary(anthropomorphism.aov) #Significant main effects of description, F(2, 521) = 5.15, p < .01, and video, F(3, 521) = 3.08, p < .05
describeBy(d3$anthropomorphism, group=d3$description) # Get descriptive statistics
TukeyHSD(anthropomorphism.aov) # Post-hoc tests

# Further analysis
# Did certain descriptions cause participants to explicitly anthropomorphize reliably more or less than the mean?
t.test(d3$anthropomorphism[d3$description == 'Anthropomorphic']) #yes: reliably MORE than the mean, t(167) = 2.17, p < .05
t.test(d3$anthropomorphism[d3$description == 'Mechanistic']) #no, t(171) = 0.16, p=.87
t.test(d3$anthropomorphism[d3$description == 'Neutral']) #yes, reliably LESS than the mean, t(192) = -2.28, p<.05 


#### Experience and Agency -- A Look Back at Gray, Gray, & Wegner (2007) ####

## Experience ##

# Internal consistency of the 4 items
alpha(d3[,41:44]) # a = .97, 95% CI [0.96, 0.97], very high internal consistency

# Effect of Description and Video on Experience
# Main effect of description
ggline(d3, x = "description", y = "experience",
       add = c("mean_se"), 
       order = c("Mechanistic", "Neutral", "Anthropomorphic"),
       ylab = "Perceptions of Experience", xlab = "Description Condition")
# Possible slight interaction -- interesting depression effect with mechanistic description + mechanistic interaction video
ggline(d3, x = "video", y = "experience", col = "description",
       add = c("mean_se"), 
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"),
       ylab = "Perceptions of Experience", xlab = "Video")

#Analysis: 2-way ANOVA
experience.aov <- aov(experience ~ description * video, data = d3)
summary(experience.aov) #Significant main effect of description only, F(2, 521) = 5.34, p < .01
describeBy(d3$experience, group=d3$description) # Get descriptive statistics

## Agency ##

# Internal consistency of the 4 items
alpha(d3[,37:40]) # a = .80, 95% CI [0.78, 0.83] # high internal consistency

# Visuals
# Effect of description
ggline(d3, x = "description", y = "agency",
       add = c("mean_se"), 
       order = c("Mechanistic", "Neutral", "Anthropomorphic"),
       ylab = "Perceptions of Agency", xlab = "Description Condition")
# Interaction plot
ggline(d3, x = "video", y = "agency", col = "description",
       add = c("mean_se"), 
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"),
       ylab = "Perceptions of Agency", xlab = "Video")
agency.aov <- aov(agency ~ description * video, data = d3)
summary(agency.aov) #Significant main effect of description only, F(2, 521) = 4.44, p < .05
describeBy(d3$agency, group=d3$description)

# Notably, only DESCRIPTIONS had a significant effect on perceptions of agency and experience

# Featured plot: Visual comparison to Gray et al. (2007) results

# Add Gray et al. data estimates
d3_expag <- d3 %>%
  add_row(description = "Gray et al. (2007)", video = "Gray et al. Robot\n(estimated)", agency = 46, experience = 3) %>%
  add_row(description = "Gray et al. (2007)", video = "Gray et al. Humans\n(estimated)", agency = 96, experience = 90)
# Format data for plot
expag <- d3_expag %>%
  group_by(description, video) %>%
  summarise(experience = mean(experience),
            agency=mean(agency))
expag$description <- factor(expag$description, c("Anthropomorphic", "Neutral", "Mechanistic", "Gray et al. (2007)")) #reorder factors
expag$video <- factor(expag$video, c("Social Interaction", "Mechanistic Interaction", "Solo Video", "No Video", "Gray et al. Robot\n(estimated)", "Gray et al. Humans\n(estimated)")) #reorder factors
# Add custom colors
anth_color <- "#2342BD" # blue
mech_color <- "#00A385" # teal
neut_color <- "#8B91A2" # cool medium gray
grayetalcolor <- "#121112" # dark purple-gray
# Plot
ggplot(expag, aes(agency, experience, xlab="Agency", ylab="Experience")) +
  geom_point(aes(
                color=factor(description),
                shape=factor(video)),
                alpha = .8,
                size = 3.5) +
  coord_cartesian(xlim = c(0,100), ylim=c(0,100)) +
  theme_classic() +
  scale_color_manual(values = c(anth_color, neut_color, mech_color, grayetalcolor)) +
  scale_shape_manual(values=c(16,17,15,18,3,8)) +
  labs(x="Agency", 
       y="Experience") +                  # change axes titles
  labs(shape = "Video", color = "Description") + # Change legend titles
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) + # Change legend order
  theme(legend.text = element_text(margin = margin(t = 5, b = 5, unit = 'pt'))) +
  theme(axis.title.x = element_text(face = "bold", vjust=-0.2),        # Bold and lower x-axis title
        axis.title.y = element_text(face = "bold")) #+                 # Bold y-axis title         
  #ggtitle("The Effect of Description and Video on Experience and Agency", subtitle= "A Look Back at Gray, Gray, & Wegner (2007)") +  # Add titles
  #theme(plot.title = element_text(size=13, face="bold",               # Adjust titles formatting
                                #margin = margin(1,0,3,0)))
  
#### Linguistic Analysis ####

### Interclass correlation coefficient to measure inter-rater agreement of continuous variables

# Two-way random effects model of interrater reliability test (mean of 2 raters) with measures of absolute agreement
icc(la0[,c(3,15)], model = "twoway", type = "agreement", unit="average") #impersonal pronouns: .88       # Very strong
icc(la0[,c(6,18)], model = "twoway", type = "agreement", unit="average") #total personal pronouns: .96   # Almost perfect
icc(la0[,c(7,19)], model = "twoway", type = "agreement", unit="average") #descriptive verbs: .90         # Almost perfect
icc(la0[,c(8,20)], model = "twoway", type = "agreement", unit="average") #interpretive action verbs: .74 # Strong
icc(la0[,c(9,21)], model = "twoway", type = "agreement", unit="average") #state verbs: .88               # Very strong
icc(la0[,c(10,22)], model = "twoway", type = "agreement", unit="average") #humanlike descriptors: .72    # Strong
icc(la0[,c(13,25)], model = "twoway", type = "agreement", unit="average") #total anth markers: .91       # Almost perfect, DIRECTLY ANALYZED

# Analyzing the effect of description and video on Total Anthropomorphic Markers (sum of personal pronouns, verbs that imply mental states, verbs that refer to mental states, and humanlike descriptors, used in reference to the robot or of which the robot was the grammatical subject)

# Prepare data
d3_without_novid <- filter(d3, video != "No Video") # Exclude "no video" condition (no written in this condition)
lasum <- d3_without_novid %>%
  group_by(description) %>%
  summarise(N = length(Total.anth.markers),
            mean = mean(Total.anth.markers),
            sd = sd(Total.anth.markers),
            se = sd / sqrt(N))
lasum$description <- factor(lasum$description, c("Mechanistic", "Neutral", "Anthropomorphic")) #reorder factors

# Set custom colors
anth_color <- "#2342BD" # blue
mech_color <- "#00A385" # teal
neut_color <- "#8B91A2" # gray

# Main visualization: means
ggplot(lasum, aes(description, mean, fill=description)) +
  geom_bar(stat="identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=.2)) +
  scale_fill_manual(values=c(mech_color,neut_color,anth_color)) + 
  theme_classic() +
  labs(x="Description Condition", 
       y="Mean Linguistic Markers Used") +                   # change axes titles
  theme(axis.title.x = element_text(vjust=-0.45)) +          # lower x-axis title
  theme(axis.text.x=element_text(size=10, vjust=0.5)) #+     # Make the x-value names larger, adjust position
  #theme(plot.margin = unit(c(5,150,5,5),"pt"))  #+          # Make the plot smaller (margins are: t,r,b,l)
  #ggtitle("The Effect of Description on Use of Linguistic\nMarkers of Anthropomorphism") +  # Add title
  #theme(plot.title = element_text(size=13, face="bold", hjust = 0.5, #center               # Center and bold title
                                    #margin = margin(1,0,8,0)))

# Alternative visualization: Stacked bar with total counts
ggplot(d3_without_novid, aes(x = description, y = Total.anth.markers)) +
  geom_col(aes(fill = video), width = 0.7) +
  theme_minimal()

# Analysis: 
totalanthmarkers.aov <- aov(Total.anth.markers ~ description * video, data = d3_without_novid)
summary(totalanthmarkers.aov) #Significant effects of description (p<.01) and video (p<.001); no interaction
describeBy(d3_without_novid$Total.anth.markers, group=d3_without_novid$description) # Get descriptive statistics
describeBy(d3_without_novid$Total.anth.markers, group=d3_without_novid$video) # Get descriptive statistics


#### Expected Capabilities ####

## Effect of description and video on expected ANTHROPOMORPHIC capabilities (agreement with statements like "This robot will be able to give me advice" or "comfort me")##

# Main effect of description
ggline(d3, x = "description", y = "excap_anth", 
       add = c("mean_se"), 
       order = c("Mechanistic", "Neutral", "Anthropomorphic"),
       ylab = "Expected Anthropomorphic Capabilities", xlab = "Description")

# Effect of video, interaction
ggline(d3, x = "video", y = "excap_anth", col = "description",
       add = c("mean_se"), 
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"),
       ylab = "Expectations of Capabilities", xlab = "Video Condition")
 
# Analysis: 2-way ANOVA
excap_anth.aov <- aov(excap_anth ~ description * video, data = d3)
summary(excap_anth.aov) #Significant main effects of both description (p<.05) and video (p<.01); significant interaction (p<.05)
describeBy(d3$excap_anth, group=d3$description) # Summary stats by description
describeBy(d3$excap_anth, group=d3$video) # Summary stats by video
TukeyHSD(excap_anth.aov) # Post-hoc tests
# Interaction: mechanistic (desc) + mechanistic (interaction video) (and all of solo video) had particularly low expectations

## Effect of description and video on expected MECHANISTIC capabilities (agreement with statements like "This robot will be able to access my music library" or "make measurements for me") ##

# Effect of description
ggline(d3, x = "description", y = "excap_mech",
       add = c("mean_se"), 
       order = c("Mechanistic", "Neutral", "Anthropomorphic"),
       ylab = "Expected Mechanistic Capabilities", xlab = "Description")

# Effect of video, possible interaction
ggline(d3, x = "video", y = "excap_mech", col = "description",
       add = c("mean_se"), 
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"),
       ylab = "Expected Mechanistic Capabilities", xlab = "Video Condition")

# Analysis: 2-way ANOVA
excap_mech.aov <- aov(excap_mech ~ description * video, data = d3)
summary(excap_mech.aov) #Significant main effects of both description (p<.05) and video (p<.001); interaction somewhat approaches significance (p=.076)
describeBy(d3$excap_mech, group=d3$description) # Summary stats by description
describeBy(d3$excap_mech, group=d3$video) # Summary stats by video
TukeyHSD(excap_mech.aov) # Post-hoc tests
# (Not significant) interaction: mechanistic description high overall, no video and mechanistic interactionvido conditions relatively high, social interaction video relatively low

# Further analysis: Test for correlation
scatter.smooth(x=d3$excap_anth, y=d3$excap_mech)  # scatterplot
cor.test(d3$excap_anth,d3$excap_mech) # = .47, p < .001 -> significant moderate positive correlation

# Featured Plot for Expected Capabilities: The (complementary) effect of description #

# Prepare data: Effect of Description on Expected Anthropomorphic Capabilities
excap_anth_sum <- d3 %>%
  group_by(description) %>%
  summarise(N = length(excap_anth),
            mean = mean(excap_anth),
            sd = sd(excap_anth),
            se = sd / sqrt(N))
excap_anth_sum$description <- factor(excap_anth_sum$description, c("Mechanistic", "Neutral", "Anthropomorphic")) #reorder factors

# Build sub-plot for anthropomorphic capabilities
lc1 <- "#12559D"      # set custom line color
excapanth_plot <- ggplot(excap_anth_sum, aes(description, mean, group=1)) +
  geom_point(color=lc) +
  geom_line(color=lc, size=.8) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=.2), color=lc) +
  ggtitle("The Effect of Description\non Expectations of Anth. Capabilities") +
  theme_classic() +                                                # theme change -- theme_classic, _light, and _minimal
  theme(plot.title = element_text(size=9, #face="bold", #hjust = 0.5,
                                  margin = margin(1,0,2,0))) +   # bigger, bold, centered title
  labs(x="Description Condition", 
       y="Degree of Expectation") +  # change axes titles
  theme(axis.title.x = element_text(size=9, vjust=-0.25)) +              # lower x-axis title
  theme(axis.title.y = element_text(size=9)) +
  theme(axis.text.x=element_text(size=7, vjust=0.2))

# Prepare data: Effect of Description on Expected Mechanistic Capabilities
excap_mech_sum <- d3 %>%
  group_by(description) %>%
  summarise(N = length(excap_mech),
            mean = mean(excap_mech),
            sd = sd(excap_mech),
            se = sd / sqrt(N))
excap_mech_sum$description <- factor(excap_mech_sum$description, c("Mechanistic", "Neutral", "Anthropomorphic")) #reorder factors

# Build sub-plot for mechanistic capabilities
lc2 <- "#E06721"      # set custom line color
excapmech_plot <- ggplot(excap_mech_sum, aes(description, mean, group=1)) +
  geom_point(color=lc2) +
  geom_line(color=lc2, size=.8) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=.2), color=lc2) +
  ggtitle("The Effect of Description\non Expectations of Mech. Capabilities") +
  theme_classic() +                                                # theme change -- theme_classic, _light, and _minimal
  theme(plot.title = element_text(size=9, #face="bold", #hjust = 0.5,
                                  margin = margin(1,0,2,0))) +   # bigger, bold, centered title
  labs(x="Description Condition", 
       y="Degree of Expectation") +  # change axes titles
  theme(axis.title.x = element_text(size=9, vjust=-0.25)) +              # lower x-axis title
  theme(axis.title.y = element_text(size=9)) +
  theme(axis.text.x=element_text(size=7, vjust=0.2))

# Featured Plot
ggarrange(excapanth_plot, excapmech_plot,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
# (Use Zoom plot viewer to change dimensions of plot)


## Predicting Expected Capabilities with Extracted Factors ##

#  Predicting Expected Anthropomorphic Capabilities with All Factors
scatter.smooth(x=d3$anthropomorphism, y=d3$excap_anth)  # scatterplot
scatter.smooth(x=d3$advanced_tool, y=d3$excap_anth)  # scatterplot
scatter.smooth(x=d3$advanced_toy, y=d3$excap_anth)  # scatterplot
linearMod <- lm(excap_anth ~ anthropomorphism + advanced_tool + advanced_toy, data=d3)  # build linear regression model on full data
summary(aov(linearMod)) # F-statistics; all factors are significant predictors at p<.001; F value for anthropomorphism is 1511.13
partial_eta_squared(linearMod) # Estimate of effect size; for anthropomorphism = .74

# Featured visualization: Anthropomorphism and Expected Anthropomorphic Capabilities
ggplot(d3, aes(anthropomorphism, excap_anth)) +
  geom_point() +
  geom_smooth(method='lm', formula = y~x) +
  
  theme_light() +                                                # theme change -- theme_classic, _light, and _minimal
  labs(x="Anthropomorphism", 
       y="Deg. Exp. of Anth. Cap.") +
  theme(axis.title.x = element_text(size=9)) +              # lower x-axis title
  theme(axis.title.y = element_text(size=8)) +
  theme(axis.text.x=element_text(size=7)) +
  labs(caption = "F(1, 529) = 1511.13, p<.001, ηp2 = .74") +
  theme(plot.caption = element_text(size=8, vjust=1.5))

#  Predicting Expected Mechanistic Capabilities with All Factors
scatter.smooth(x=d3$anthropomorphism, y=d3$excap_mech)  # scatterplot
scatter.smooth(x=d3$advanced_tool, y=d3$excap_mech)  # scatterplot
scatter.smooth(x=d3$advanced_toy, y=d3$excap_mech)  # scatterplot
linearMod <- lm(excap_mech ~ anthropomorphism + advanced_tool + advanced_toy, data=d3)  # build linear regression model on full data
summary(aov(linearMod)) # F-statistics; anthropomorphism and advanced tool factors are both significant predictors at p<.001; F value for advanced tool is 309.24
partial_eta_squared(linearMod) # Estimate of effect size; for advanced_tool = .37

# Visualization 1: Advanced Tool Perceptions and Expected Mechanistic Capabilities
ggplot(d3, aes(advanced_tool, excap_mech)) +
  geom_point() +
  geom_smooth(method='lm', formula = y~x) +
  ggtitle("Advanced Tool Perception vs. Expectations\nof Mechanistic Capabilities") +
  theme_light() +                                                # theme change -- theme_classic, _light, and _minimal
  labs(x="Advanced Tool Perception", 
       y="Degree of Expectation of Mech. Capabilities",
       caption = "F(1, 529) = 309.24, p<.001, ηp2 = .37") +
  theme(plot.title = element_text(size=12, face="bold", #hjust = 0.5,
                                  margin = margin(5,0,8,0)))    # bigger, bold, centered title

# Visualization 2: Anthropomorphism and Expected Mechanistic Capabilities
ggplot(d3, aes(anthropomorphism, excap_mech)) +
  geom_point() +
  geom_smooth(method='lm', formula = y~x) +
  ggtitle("Anthropomorphism vs. Expectations\nof Mechanistic Capabilities") +
  theme_light() +                                                # theme change -- theme_classic, _light, and _minimal
  labs(x="Anthropomorphism", 
       y="Degree of Expectation of Mech. Capabilities",
       caption = "F(1, 529) = 122.25, p<.001, ηp2 = .19") +
  theme(plot.title = element_text(size=12, face="bold", #hjust = 0.5,
                                  margin = margin(5,0,8,0)))     # bigger, bold, centered title


#### Moral Regard: Elections to Save the Robot ####

#   Participants were presented with 5 moral dilemma-type scenarios, which 
# were put in the same section as and randomized with the 10 trust scnearios 
# introduced above,  and asked to indicate how likely they were to take one 
# of two presented actions, one of which indicated moral care for the 
# robot *rather than an endangered human*. Opting for the "moral care" 
# action--deciding in the context of a hypothetical scenario to increase 
# the robot's safety while risking a human's--should thus be considered
# important.

#Internal consistency of the 5 scenario items
alpha(d3[,142:146]) # a = .73, 95% CI [0.69, 0.76], acceptable internal consistency

#Looking at main effects of description and video on elections to save the robot
ggline(d2, x = "video", y = "moral", col = "description",
       add = c("mean_se"),
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"),
       ylab = "Elections to Save the Robot", xlab = "Video Condition")
# No clear pattern emerges
moral.aov <- aov(moral ~ description * video, data = d3)
summary(moral.aov) # No significant main effects or interactions

### Testing the relationship between the extracted factors and decisions to save the robot
# Visuals
scatter.smooth(x=d3$anthropomorphism, y=d3$moral)  # scatterplot
scatter.smooth(x=d3$advanced_tool, y=d3$moral)  # scatterplot
scatter.smooth(x=d3$advanced_toy, y=d3$moral)  # scatterplot
# Tests
linearMod <- lm(moral ~ anthropomorphism + advanced_tool + advanced_toy, data=d3)  # build linear regression model on full data
summary(aov(linearMod)) # F-statistics; anthropomorphism is significant predictor at p<.001
partial_eta_squared(linearMod) # Estimate of effect size; anthropomorphism = .43

# Featured visualization
ggplot(d3, aes(anthropomorphism, moral)) +
  geom_point() +
  geom_smooth(method='lm', formula = y~x) +
  theme_light() +                                                # theme change -- theme_classic, _light, and _minimal
  labs(x="Anthropomorphism", 
       y="Elections to Save Robot") +
  theme(axis.title.x = element_text(size=9)) +              # lower x-axis title
  theme(axis.title.y = element_text(size=8)) +
  theme(axis.text.x=element_text(size=7)) +
  labs(caption = "F(1, 529) = 402.44, p<.001, ηp2 = .43") +
  theme(plot.caption = element_text(size=8, vjust=1.5))


########FURTHER ANALYSES -- NOT INCLUDED IN THE PAPER########

#### Experience (Not Agency) Predicts Moral Regard ####
# Agency and moral regard
scatter.smooth(x=d3$agency, y=d3$moral)  # scatterplot
cor.test(d3$agency,d3$moral) #r(531) = .41, p<.001, weak-moderate positive correlation
# Experience and moral regard
scatter.smooth(x=d3$experience, y=d3$moral)  # scatterplot
cor.test(d3$experience,d3$moral) #r(531) = .64, p<.001, moderate positive correlation
# Linear model
linearMod <- lm(moral ~ experience + agency, data=d3)  # build linear regression model on full data
summary(aov(linearMod)) # Experience is significant predictor (p<.001) and Agency approaches significance (p=.09)
partial_eta_squared(linearMod) # effect size: partial eta squared = .42 for Experience
# ! Aligns with the Nijssen, Müller, & van Baaren (2019) finding that moral regard is explained by attribution of experience, not agency


#### The Effect of Description and Video on Tool Perceptions Factor ####
#Visualizations
ggline(d3, x = "description", y = "advanced_tool", #col = "video", palette = c("#F76B00","#5D5D5D","#0037FF","#D403CE"),
       add = c("mean_se"), 
       order = c("Neutral", "Mechanistic", "Anthropomorphic"),
       ylab = "Tool Perceptions", xlab = "Description Condition")
ggline(d3, x = "video", y = "advanced_tool", col = "description", palette = c("#0037FF","#E9270E","#17BE17"),
       add = c("mean_se"),
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"),
       ylab = "Tool Perceptions", xlab = "Video Condition")
# Analysis: 2-way ANOVA
advtool.aov <- aov(advanced_tool ~ description * video, data = d3)
summary(advtool.aov) #Significant main effects of video, F(3, 521) = 26.92, p < .001
describeBy(d3$advanced_tool, group=d3$video) # Get descriptive statistics
TukeyHSD(advtool.aov) # Post-hoc tests -- mechanistic interaction video caused significantly more advanced tool perceptions than the solo video and the social interaction video; no video condition and mechansitic interaction condition did not significantly differ


#### Effect of Description and Video on Advanced Toy Perceptions ####
ggline(d3, x = "video", y = "advanced_toy", col = "description", palette = c("#0037FF","#E9270E","#17BE17"),
       add = c("mean_se"),
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"),
       ylab = "Advanced Toy Perceptions", xlab = "Video Condition")
# Analysis: 2-way ANOVA
advanced_toy.aov <- aov(advanced_toy ~ description * video, data = d3)
summary(advanced_toy.aov) #Significant main effect of video, F(3, 521) = 11.99, p < .001


#### Cronbach's Alphas of the Most Highly Loading Items in Each Extracted Factor ####

anthropomorphism_list <- d2[,c(42,44,41,39,43,35,47,34,45,29,46,30,37,31,38)]
alpha(anthropomorphism_list) #a = 0.94, 95% CI [0.94, 0.95]  # Excellent internal consistency

advanced_tool_list <- d2[,c(27,28,36,40,38)]
alpha(advanced_tool_list) #a = .67, 95% CI [0.63, 0.72]      # Acceptable

advanced_toy_list <- d2[,c(31,32,33,36)]
alpha(advanced_toy_list) #a = .65, 95% CI [0.60, 0.69]       # Acceptable, but would increase to .72 if "gadget" metaphor (lowest loading item) is dropped


#### Metaphors ####

#  Interesting complementary contrast between these two graphs -- videos seem to influence the metaphors predicted to be used, with
# the mechanistic interaction video increasing the predicted likelihood that the participant will use mechanistic metaphors (e.g. tool),
# and the social interaction video increasing the predicted likelihood that the participant will use anthropomorphic metaphors (e.g. companion).

##Mechanistic metaphors
ggline(d3, x = "video", y = "metaphors_mech", #col = "description",
       add = c("mean_se"), 
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"),
       ylab = "Predicted Likelihood of Using Mechanistic Metaphors", xlab = "Video")
metaphors_mech.aov <- aov(metaphors_mech ~ description * video, data = d3)
summary(metaphors_mech.aov) #Significant main effect of video, F(3,521) = 3.98, p < .01

##Anthropomorphic metaphors
ggline(d3, x = "video", y = "metaphors_anth", #col = "description",
       add = c("mean_se"), 
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"),
       ylab = "Predicted Likelihood of Using Anthropomorphic Metaphors", xlab = "Video")
metaphors_anth.aov <- aov(metaphors_anth ~ description * video, data = d3)
summary(metaphors_anth.aov) #Significant main effect of video, F(3,521) = 4.79, p < .01


#### Animacy ####

alpha(d3[,48:50]) # a = .58; poor internal consistency

# Animacy overall (despite poor internal consistency of items): effect of description and video
ggline(d3, x = "video", y = "disj_ani", col = "description",
       add = c("mean_se"),
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"))
animacy.aov <- aov(disj_ani ~ description * video, data = d3)
summary(animacy.aov) 
#Significant effect of video, interaction (which is important, as shown in graph - no vid condition and anth desc)

# Responsiveness (as opposed to unresponsiveness) perception item
ggline(d3, x = "video", y = "unresponsive.respons_1", col = "description",
       add = c("mean_se"),
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"))
#Interesting contrast between anthropomorphic description/no video (high) and anthropomorphic description/solo video
#Possible explanation is that the anth. desc. raised expectations of responsive entity (as shown by former) and the solo video proved disappointing in contrast to those expectations (which the mech. desc. and neutral desc. did not raise)
#But the kind of expectations raised by neutral and mech. descriptions aligned with mechanistic interaction video, so rated as more "responsive" overall
responsiveness.aov <- aov(unresponsive.respons_1 ~ description * video, data = d3)
summary(responsiveness.aov) 
#Significant interaction (p<.01) and main effect of video (p<.001)

# Interactiveness (as opposed to inertness) perception item 
ggline(d3, x = "video", y = "inert.interactive_1", col = "description",
       add = c("mean_se"),
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"))
#Very similar to "responsiveness" plot above 
#Anthropomorphic description clearly raised perceptions of interactiveness if no video (further bottom-up cues) present
#Interesting contrast between anthropomorphic description/no video (high) and anthropomorphic description/solo video
#Possible explanation is that the anth. desc. raised expectations of responsive entity (as shown by former) and the solo video proved disappointing in contrast to those expectations (which the mech. desc. and neutral desc. did not raise)
#Overall, video (of bottom-up cues) (when available) seems to have had the stronger effect
interactiveness.aov <- aov(inert.interactive_1 ~ description * video, data = d3)
summary(interactiveness.aov) 
#Significant interaction (p<.05) and main effect of video (p<.001)

# "Liveliness" (as opposed to "mechanical"-ness) perception item
ggline(d3, x = "video", y = "mechanical.lively_1", col = "description",
       add = c("mean_se"),
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"))
#Anthropomorphic description clearly raised perceptions of liveliness if no video (further bottom-up cues) present
#Possibly interesting low point for those who both read the mechanical description and watched the mechanical interaction video
#Otherwise, video seems to have had a stronger effect, but seems somewhat disorganized
liveliness.aov <- aov(mechanical.lively_1 ~ description * video, data = d3)
summary(liveliness.aov) 
#Significant main effect of video (p<.05)


#### Liking ####

#Alpha of the 3 likeable/dislikeable, pleasant/unplasant, and helpful/unhelpful items
alpha(d3[,51:53]) # a = .76; acceptable internal consistency

#Effect of description and video on liking (likeableness, pleasantness, and helpfulness perceptions)
ggline(d2, x = "video", y = "disj_liking", col = "description",
       add = c("mean_se"),
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"))
#Somewhat similar to responsiveness above: interesting contrast for the anthropomorphic description condition across the no video and solo video conditions: attributable to high expectations and disappointment in the solo video condition?
#"Helpful" item would be intuitively highest in the mechanistic interaction video, as the human is using the robot to complete a task (anthropomorphically put: robot is shown to be helping the human)
# Due to this last point, perhaps interesting to consider the items separately
liking.aov <- aov(disj_liking ~ description * video, data = d3)
summary(liking.aov) 
#Significant interaction (p<.05) and effect of video (p<.001)

#Effect of description and video on helpfulness (as opposed to unhelpfulness) item
ggline(d2, x = "video", y = "unhelpf.helpf_1", col = "description",
       add = c("mean_se"),
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"))
#As expected, mechanistic interaction group is high and scores seem to be similar regardless of description
#Unexpectedly, the pattern is the same for the "no video" condition as well
#Video seems to be the real determiner of helpfulness perception
#Though again, notably, scores are somewhat higher for the group who also read the anthropomorphic description in the no video and social interaction video conditions
helpfulness.aov <- aov(unhelpf.helpf_1 ~ description * video, data = d3)
summary(helpfulness.aov) 
#Significant interaction (p<.01) and main effect of video (p<.001)

#Effect of description and video on likeableness (as opposed to dislikeableness) item
ggline(d2, x = "video", y = "dislika.lika_1", col = "description",
       add = c("mean_se"),
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"))
#Without video, anthropomorphic description causes the highest; with social interaction video, anthropomorphic description cuases the highest (though not significantly)
#Odd low point of the anthropomorphic description/solo video treatment condition again
likeableness.aov <- aov(dislika.lika_1 ~ description * video, data = d3)
summary(likeableness.aov) 
#Significant effect of video (p<.01); interaction approaches significance (p=.065)

#Effect of description and video on pleasantness (as opposed to unpleasantness) item
ggline(d2, x = "video", y = "unpleas.pleas_1", col = "description",
       add = c("mean_se"),
       order = c("No Video", "Solo Video", "Mechanistic Interaction", "Social Interaction"))
#Similar pattern to animacy items -- anth. desc. is highest in no video, lowest in solo, and highest in social interaction (though not significantly)
#Perhaps because neutral desc. set fewer expectations, highest in solo and mechanistic interaction video conditions (though not significantly)
pleasantness.aov <- aov(unpleas.pleas_1 ~ description * video, data = d3)
summary(pleasantness.aov) 
#Significant effect of video (p<.001)


#### Trust Scenarios ####

#   Participants were presented with 10 hypothetical "trust scenarios," in 
# which the robot was described in a certain hypothetical situation to make a 
# judgement and recommend a particular action. The participant was 
# asked to indicate how likely they were to take one of two presented actions,
# one of which was the action the robot recommended. Choosing this action of
# the two presented actions was taken to be "trusting" the robot. In 5 of 
# the scenarios, the robot made a judgement about PHYSICAL matters, such 
# as the presence of chemical traces of explosives in a security scenario. 
# In the other 5 scenarios, the robot made a judgement about PSYCHOLOGICAL 
# matters, such as whether a person is highly stressed in a security scenario. 

#  Regarding the effect of the descriptions and videos on trust of the
# robot (making judgements concerning the physical and psychological), it
# was hypothesized that anthropomorphism of the robot would be associated 
# with much higher trust of the robot in the psychological-type scenarios, 
# and somewhat higher trust in the physical-type scenarios. Accordingly, it
# was predicted that the anthropomorphic description and social interaction
# video would be associated with higher trust (especially for the 
# psychological-type scenarios). It was also predicted, however, that
# the mechanistic description (compared to neutral) and mechanistic interaction 
# video would increase confidence of the robot's ability to make 
# credible/trustworthy recommendations in the physical-type scenarios.

## PSYCHOLOGICAL ##
#Internal consistency of the 5 scenario items
alpha(d3[,152:156]) # a = .72, 95% CI [0.68, 0.75], acceptable internal consistency

#Main effect of description on trust in psychological-based scenarios
ggline(d2, x = "description", y = "psyc_trust", col = "video",
       add = c("mean_se"),
       order = c("Mechanistic", "Neutral", "Anthropomorphic"))
# Pattern is in accordance with the hypothesis: those who read anthropomorphic description had generally higher trust, regardless of video condition
# -- with the notable and unexpected exception of those who also watched the social interaction video. (Uncanny valley achieved?)
psyc_trust.aov <- aov(psyc_trust ~ description * video, data = d3)
summary(psyc_trust.aov)
# **Main effect of description approaches signficance: p=.066**

#Looking at correlations of the extracted factors with psychological trust
#  Anthropomorphism
scatter.smooth(x=d3$anthropomorphism, y=d3$psyc_trust)  # scatterplot
cor.test(d3$anthropomorphism,d3$psyc_trust) #r(531) = .16, p<.001, very weak positive correlation
#  Advanced Tool
scatter.smooth(x=d3$advanced_tool, y=d3$psyc_trust)  # scatterplot
cor.test(d3$advanced_tool,d3$psyc_trust) #r(531) = .11, p<.01, very weak positive correlation
#  Advanced Toy
scatter.smooth(x=d3$advanced_toy, y=d3$psyc_trust)  # scatterplot
cor.test(d3$advanced_toy,d3$psyc_trust) #r(531) = -.01, p=.6, no correlation

## PHYSICAL ##
#Internal consistency of the 5 scenario items
alpha(d3[,147:151]) # a = .77, 95% CI [0.74, 0.80], acceptable internal consistency

#Looking at main effects of description on trust in physical-based scenarios
ggline(d2, x = "description", y = "phys_trust", col = "video",
       add = c("mean_se"),
       order = c("Mechanistic", "Neutral", "Anthropomorphic"))
# Some trends in accordance with hypothesis: those who read anthropomorphic description had generally slightly higher trust compared to the other two description conditions; 
# Notably, the mech desc. + mech int. vid. treatment had high trust, and the mech desc. + soc. int. vid. treatment condition had relatively low trust; perhaps explained by differently set expectations by the description 
# (expectations set for high physical capacities by the description, but then the video made it seem not so or differently capable);
# An opposite pattern was exhibited for all other video conditions, with general trends toward the highest trust of the video conditions for those who read the anthropomorphic description 
# -- with the notable and unexpected exception of those who also watched the social interaction video. 
phys_trust.aov <- aov(phys_trust ~ description * video, data = d3)
summary(phys_trust.aov)
# No significant main effects or interaction

#Looking at correlations of the extracted factors with physical trust
# Anthropomorphism
scatter.smooth(x=d3$anthropomorphism, y=d3$phys_trust)  # scatterplot
cor.test(d3$anthropomorphism,d3$phys_trust) #r(531) = -.30, p<.001, weak-moderate negative correlation
# Advanced Tool
scatter.smooth(x=d3$advanced_tool, y=d3$phys_trust)  # scatterplot
cor.test(d3$advanced_tool,d3$phys_trust) #r(531) = .03, p=.38, no correlation
# Advanced Toy
scatter.smooth(x=d3$advanced_toy, y=d3$phys_trust)  # scatterplot
cor.test(d3$advanced_toy,d3$phys_trust) #r(531) = .03, p=.46, no correlation

#####Linguistic analysis: Impersonal Pronouns####
# Main effect of description
ggline(d3_without_novid, x = "description", y = "it.its.it.s.itself", #col = "video", palette = c("#F76B00","#5D5D5D","#0037FF","#D403CE"),
       add = c("mean_se"),
       order = c("Mechanistic", "Neutral", "Anthropomorphic"),
       ylab = "Impersonal Pronouns", xlab = "Description Condition")
# Main effect of video
ggline(d3_without_novid, x = "video", y = "it.its.it.s.itself", col = "description", palette = c("#0037FF","#E9270E","#17BE17"),
       add = c("mean_se"),
       order = c("Solo Video", "Mechanistic Interaction", "Social Interaction"),
       ylab = "Impersonal Pronouns", xlab = "Video Condition")
# Analysis: 2-way ANOVA
impersonalpronouns.aov <- aov(it.its.it.s.itself ~ description * video, data = d3)
summary(impersonalpronouns.aov) #Significant main effects of description (p<.01) and video (p<.001)
