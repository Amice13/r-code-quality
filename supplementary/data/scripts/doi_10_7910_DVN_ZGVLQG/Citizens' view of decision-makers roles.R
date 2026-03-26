############################################################################
################REPLICATION CODE: Citizens’ views of######################## 
#######decision-makers roles: a conjoint experiment in 15 countries#########
############################################################################
library(cregg)
library(ggplot2)
library(gtable)
library(grid)
library(lemon)
library(broom)
library(dplyr)
library(jtools)

#####CREATION OF THE REPRESENTATIONAL PARADIGM VARIABLE#####
###Interaction partisanship and accountability
cje$Representational_paradigm <- interaction(cje$PartyAffiliation,
                                             cje$Accountability,
                                             sep = "_")
table(cje$Representational_paradigm)
cje$Representational_paradigm <- as.character(cje$Representational_paradigm)
cje$Representational_paradigm [cje$Representational_paradigm == "No_Experts"] = "Technocrat"
cje$Representational_paradigm [cje$Representational_paradigm == "Yes_Experts"] = "Technopol"
cje$Representational_paradigm [cje$Representational_paradigm == "No_Citizens"] = "Instructed delegate"
cje$Representational_paradigm [cje$Representational_paradigm == "Yes_Citizens"] = "Partisan delegate"
cje$Representational_paradigm [cje$Representational_paradigm == "No_Parliament"] = "Independent"
cje$Representational_paradigm [cje$Representational_paradigm == "Yes_Parliament"] = "Responsible party model"
cje$Representational_paradigm  <- factor(cje$Representational_paradigm, 
                                         levels=c("Responsible party model", 
                                                  "Independent",
                                                  "Technocrat", "Technopol", 
                                                  "Instructed delegate",
                                                  "Partisan delegate"))
table(cje$Representational_paradigm)

### convert to factors for the analysis 
cje$Age <- as.factor(cje$Age)
cje$Gender <- as.factor(cje$Gender)
cje$ParentsOccupation  <- as.factor(cje$ParentsOccupation)
cje$PolicyPriority <- as.factor(cje$PolicyPriority)
cje$CurrentOccupation <- as.factor(cje$CurrentOccupation)
cje$Representational_paradigm <- as.factor(cje$Representational_paradigm)
cje$TypeMinister <- as.factor(cje$TypeMinister)
cje$Country <- as.factor(cje$Country)

#Basic model
f1 <- Minister ~ Age + Gender +
  ParentsOccupation + PolicyPriority+CurrentOccupation+ 
  Representational_paradigm

#Main model - marginal means
stacked10mm <- cj(cje, f1, id = ~ ResponseId, estimate = "mm", alpha = 0.05, 
                  weights = ~ Weights)
#Figure 1 main text
stacked10mm %>% as_tibble()
#Subset to display only representational_paradigm
stacked10mmdf <- subset(stacked10mm, 
                        stacked10mm$feature == "Representational_paradigm")
#Plot Figure 1
p <- ggplot(stacked10mmdf, aes(x = estimate, y = level)) +
  geom_point(aes(x = estimate, y = level), 
             position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), 
                 position = position_dodge(width = 0.6), height = 0.2) +
  labs(x = "Marginal means",
       y = "Representational paradigm") +   theme_minimal() + 
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey")

ggsave("Figure1.tiff", plot = p, device = "tiff",
       width = 8, height = 6, units = "in", dpi = 600)

#Appendix B FIGURE 1.1A
plot(stacked10mm, feature_headers = FALSE, vline = 0.5)

#Main model by Minster Type
stacked1mm <- cj(cje, f1, id = ~ ResponseId, by = ~ TypeMinister, 
                 estimate = "mm", alpha = 0.05, 
                 weights = ~ Weights)
#Figure 1.2A
plt1mm <- plot(stacked1mm, vline = 0.5) + 
  ggplot2::facet_wrap(~ TypeMinister, nrow = 1L) + 
  ggplot2::theme(axis.text = element_text(size=7.5), 
                 legend.position = "none")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.spacing.x = unit(0.005, "lines"))
plt1mm

#REMOVE the NA column from the plot
gt1mm = ggplot_gtable(ggplot_build(plt1mm))
print(gt1mm)
gtable_show_names(gt1mm)
rm_grobs1mm <- gt1mm$layout$name %in% c("strip-t-5-1", "axis-t-5-1", 
                                        "panel-5-1",
                                        "axis-b-5-1", "xlab-b", 
                                        "caption", "axis-l-1-5",
                                        "axis-r-1-5", "ylab-r", 
                                        "title", "subtitle")  
# remove grobs
gt1mm$grobs[rm_grobs1mm] <- NULL
gt1mm$layout <- gt1mm$layout[!rm_grobs1mm, ]
#check result
gtable_show_names(gt1mm)
grid.newpage()
grid.draw(gt1mm)

#Conditional differences by Type of Minister
m22 <- mm_diffs(cje, f1, ~ TypeMinister, 
                id = ~ ResponseId, weights = ~ Weights, alpha = 0.05)
plot(m22)

#Subset to display only representational_paradigm
m22 %>% as_tibble()
m22df <- subset(m22, m22$feature == "Representational_paradigm")

#APPENDIX B - Figure 2.1A
ggplot(m22df, aes(x = estimate, y = level)) +
  geom_point(aes(x = estimate, y = level), 
             position = position_dodge(width = 0.6)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), 
                 position = position_dodge(width = 0.6), height = 0.2) +
  labs(title = "Ministers",
       x = "Conditional differences",
       y = "") +   facet_wrap(~BY) +
  theme_minimal() + geom_vline(xintercept = 0, 
                               linetype = "dashed", color = "grey")

###Figure 2 - Main model by country
#Basic model
table(cje$Country)
levels(cje$Country)[levels(cje$Country) == "Belgium(FR)"] <- "Belgium (VAL)"
levels(cje$Country)[levels(cje$Country) == "Belgium(NL)"] <- "Belgium (VLG)"

f10 <- Minister ~ Age + Gender +
  ParentsOccupation + PolicyPriority+CurrentOccupation+ 
  Representational_paradigm

#Marginal means by country
stacked10mm <- cj(cje, f10, id = ~ ResponseId, 
                  by = ~ Country, estimate = "mm",
                  weights = ~ Weights, alpha = 0.1)

#Subset to display only representational_paradigm
stacked10mm %>% as_tibble()
stacked10mmdf <- subset(stacked10mm, 
                        stacked10mm$feature == "Representational_paradigm")

#Figure 2
q <- ggplot(stacked10mmdf, aes(x = estimate, y = BY)) +
  geom_point(aes(shape = level),
             position = position_dodge(width = 0.6),
             size = 2.5,
             color = "black") +
  geom_errorbarh(aes(xmin = lower, xmax = upper, linetype = level),
                 position = position_dodge(width = 0.6),
                 height = 0.2,
                 color = "black") +
  labs(title = "Estimates by Country and Roles",
       x = "Marginal means",
       y = "Country") +
  facet_wrap(~ level) +
  geom_vline(xintercept = 0.5,
             linetype = "dashed",
             color = "grey40") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Figure2.tiff", plot = q, device = "tiff",
       width = 8, height = 6, units = "in", dpi = 600)

#Model for Figure 2.2A
stacked10mm <- cj(cje, Minister ~ Age + Gender +
                    ParentsOccupation + PolicyPriority+CurrentOccupation+ 
                    Representational_paradigm, id = ~ ResponseId, 
                  by = ~ Country, estimate = "mm",
                  weights = ~ Weights, alpha = 0.1)

plt10mm <- plot(stacked10mm, vline = 0.5) + 
  ggplot2::facet_wrap(~ Country, nrow = 3L) + 
  ggplot2::theme(axis.text = element_text(size=6), legend.position = "none")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.spacing.x = unit(0.005, "lines"))
plt10mm

#REMOVE the NA column from the plot
gt10mm = ggplot_gtable(ggplot_build(plt10mm))
print(gt10mm)
gtable_show_names(gt10mm)
rm_grobs10mm <- gt10mm$layout$name %in% c("strip-t-6-3", "axis-b-6-3", 
                                          "panel-6-3",
                                          "xlab-b", "caption", 
                                          "axis-l-3-6", "axis-t-6-3",
                                          "axis-r-3-6", "ylab-l", "title", 
                                          "subtitle",
                                          "strip-t-5-3", "axis-b-5-3", 
                                          "panel-3-3",
                                          "axis-l-3-5", "axis-r-3-5", 
                                          "axis-t-5-3", "panel-5-3")
gt10mm$grobs[rm_grobs10mm] <- NULL
gt10mm$layout <- gt10mm$layout[!rm_grobs10mm, ]
# check result
gtable_show_names(gt10mm)
grid.newpage()
grid.draw(gt10mm)
#to include Spain, run the same code removing the Minister of Agriculture

###Figure 3 - Conditional differences - Institutional Trust
#Basic model
f2 <- Minister ~ Age + Gender +
  ParentsOccupation + PolicyPriority+CurrentOccupation+ 
  Representational_paradigm

#Change the variable name and convert to factor
cje$InstitutionalTrust <- cje$InstTrust
cje$InstitutionalTrust <- as.factor(cje$InstitutionalTrust)

#Conditional Differences by trust
m2 <- mm_diffs(cje, f2, ~ InstitutionalTrust, 
               id = ~ ResponseId, weights = ~ Weights, alpha = 0.05)

#Subset to display only representational_paradigm
m2 %>% as_tibble()
m2df <- subset(m2, m2$feature == "Representational_paradigm")

#Figure 3
r <- ggplot(m2df, aes(x = estimate, y = level)) +
  geom_point(aes(x = estimate, y = level), 
             position = position_dodge(width = 0.6)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), 
                 position = position_dodge(width = 0.6), height = 0.2) +
  labs(title = "Distrustful (ref. category) vs. Trustful respondents",
       x = "Conditional differences",
       y = "") +   facet_wrap(~BY) +
  theme_minimal() + geom_vline(xintercept = 0, linetype = "dashed", 
                               color = "grey")

ggsave("Figure3.tiff", plot = r, device = "tiff",
       width = 8, height = 6, units = "in", dpi = 600)

#APPENDIX B - Figure 3A
stacked9mm <- cj(cje, f2, id = ~ ResponseId, by = ~ InstitutionalTrust, 
                 estimate = "mm",
                 weights = ~ Weights, alpha = 0.05)
plot(stacked9mm, feature_headers = FALSE, vline = 0.5)

###Figure 4 - Conditional differences - Political interest
cje$Interest <- as.factor(cje$Interest)

#Basic model
f3 <- Minister ~ Age + Gender +
  ParentsOccupation + PolicyPriority+CurrentOccupation+ 
  Representational_paradigm

#Conditional difference by interest
m3 <- mm_diffs(cje, f3, ~ Interest, 
               id = ~ ResponseId, weights = ~ Weights, alpha = 0.05)

#Subset to display only representational_paradigm
m3 %>% as_tibble()
m3df <- subset(m3, m3$feature == "Representational_paradigm")

#Figure 4
s <- ggplot(m3df, aes(x = estimate, y = level)) +
  geom_point(aes(x = estimate, y = level), 
             position = position_dodge(width = 0.6)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), 
                 position = position_dodge(width = 0.6), height = 0.2) +
  labs(title = "Politically interested (ref. category) vs. 
       Politically not interested",
       x = "Conditional differences",
       y = "Interest") +   facet_wrap(~BY) +
  theme_minimal() + geom_vline(xintercept = 0, 
                               linetype = "dashed", color = "grey")

ggsave("Figure4.tiff", plot = s, device = "tiff",
       width = 8, height = 6, units = "in", dpi = 600)

#APPENDIX B - Figure 4A
stacked10mm <- cj(cje, f3, id = ~ ResponseId, by = ~ Interest, 
                 estimate = "mm",
                 weights = ~ Weights, alpha = 0.05)
plot(stacked10mm, feature_headers = FALSE, vline = 0.5)

###Figure 5 - Conditional differences - Populism
#Basic model
f4 <- Minister ~ Age + Gender +
  ParentsOccupation + PolicyPriority+CurrentOccupation+ 
  Representational_paradigm

#Change the variable name and convert to factor
cje$Populism <- cje$populismCat
cje$Populism <- as.factor(cje$Populism)

#Conditional difference by populism
m12 <- mm_diffs(cje, f4, ~ Populism,   alpha = 0.06,
                id = ~ ResponseId, weights = ~ Weights)

#Subset to display only representational_paradigm
m12 %>% as_tibble()
m12df <- subset(m12, m12$feature == "Representational_paradigm")

#Figure 5
t <- ggplot(m12df, aes(x = estimate, y = level)) +
  geom_point(aes(x = estimate, y = level), 
             position = position_dodge(width = 0.6)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), 
                 position = position_dodge(width = 0.6), height = 0.2) +
  labs(title = "Non-populist (ref. category) vs. Populist respondents",
       x = "Conditional differences",
       y = "Populism") +   facet_wrap(~BY) +
  theme_minimal() + geom_vline(xintercept = 0, linetype = "dashed", 
                               color = "grey")

t
ggsave("Figure5.tiff", plot = t, device = "tiff",
       width = 8, height = 6, units = "in", dpi = 600)

#APPENDIX B - Figure 5A
stacked11mm <- cj(cje, f4, id = ~ ResponseId, by = ~ Populism, 
                  estimate = "mm",
                  weights = ~ Weights, alpha = 0.05)
plot(stacked11mm, feature_headers = FALSE, vline = 0.5)
