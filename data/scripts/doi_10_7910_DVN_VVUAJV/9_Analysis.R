################################################################################ 
################################################################################ 
#################### Discrimination in Argentinian Football ####################
################################################################################ 
################################################################################ 

# This script performs the analysis
# for the project
# Discrimination in Argentinian Football
# by Carlos Gomez-Gonzalez, Gwen-Jiro Clochard & Helmut Dietl


################################################################################ 
################################ Importing data ################################
################################################################################ 

data_argentina <- read_excel(paste(pathdata, "Clean//Data_argentina.xlsx", sep="//"))

### Using only emails where we actually sent the emails

data_argentina <-
  subset(data_argentina, sent!=0)


################################################################################ 
############################### Defining controls ##############################
################################################################################ 

controls <- 
  c("female_team")

fixed_effects <- 
  c("factor(Province)")


################################################################################ 
######################### Number of clubs, by treatment ########################
################################################################################ 

table(data_argentina$treatment_region, data_argentina$female_team)
table(data_argentina$foreign, data_argentina$female_team)
table(data_argentina$female_team)


################################################################################ 
########################## Map of clubs, by treatment ##########################
################################################################################ 

############# Map of Argentina and provinces ############# 


argentina <- 
  st_read(
    dsn = paste(pathdata, "arg_adm_unhcr2017_shp/arg_admbnda_adm0_unhcr2017.shp", sep="//")) %>%
  st_as_sf() 

provinces <- 
  st_read(
    dsn = paste(pathdata, "arg_adm_unhcr2017_shp/arg_admbndl_ALL_unhcr2017.shp", sep="//")) %>%
  st_as_sf() 

argentina_map <- 
  tm_shape(argentina) +
  tm_borders()
  
provinces_map <- 
  tm_shape(subset(provinces, AdmLevel<2)) +
  tm_lines()


############# Adding a layer of map of clubs ############# 

clubs_sf = st_as_sf(data_argentina,
                    coords = c('Longitude', 'Latitude'))

club_map<-
  provinces_map +
  argentina_map + 
  tm_shape(clubs_sf)+
  tm_dots(col = 'treatment_region', 
          palette = c("gray", rgb(0/255, 159/255, 183/255), rgb(1, 196/255, 155/255), rgb(195/255, 60/255, 84/255)), 
          title = "Treatment",
          size = 0.1) +
  tm_layout(
    legend.title.size=1,
    legend.text.size = .86,
    legend.position = c("right","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 1,
    frame = FALSE)

club_map


############# Saving the map ############# 

tmap_save(
  tm = club_map,
  filename = paste(pathfigures, "Club_maps.pdf", sep="//"),
  width = 5,
  height = 6
)

rm(argentina, argentina_map, club_map, clubs_sf, provinces, provinces_map)


################################################################################ 
############################ Descriptive statistics ############################
################################################################################ 

############# Defining the function ############# 

myDescriptives = function(x) {
  x = as.numeric(x)
  m = mean(x, na.rm = TRUE)
  sd = sd(x, na.rm = TRUE)
  N = length(x[!is.na(x)])
  return(c(m, sd, N))
}

############# Producing the table ############# 

colnames <- c("Mean", "SD", "N")

characteristics <- 
  c("male_team", "buenos"
  )

variableslist <- 
  list(
    characteristics
  )

labels_char <- 
  c("Male team", "Buenos Aires province"
  )

labels <- 
  list(
    labels_char
  )

datasets <- 
  list(
    "Characteristics" = data_argentina
  )

createDescriptiveTable(
  datasets,
  summary_function = myDescriptives,
  column_names = colnames,
  variable_names = variableslist,
  variable_labels = labels,
  arraystretch = 1.3,
  title = "Descriptive statistics",
  label = "tab: descriptive",
  file = paste(pathtables, "Descriptive_statistics.tex", sep="//"),
  digits = c(3,3,0)
)
summary(data_argentina$buenos)
summary(data_argentina$female_team)
sd(data_argentina$buenos)
sd(data_argentina$female_team)

rm(datasets, labels, characteristics, variableslist, colnames, labels_char)


################################################################################ 
######################## Histogram of responses: foreign #######################
################################################################################ 

summary(data_argentina$positive_response)
summary(subset(data_argentina, male_team==0)$positive_response)
summary(subset(data_argentina, male_team==1)$positive_response)
summary(subset(data_argentina, foreign==1)$positive_response)
summary(subset(data_argentina, foreign==0)$positive_response)


############# All four answer types ############# 

### Producing the graph

histo <- ggplot(
  data = data_argentina, 
  aes(x = type_response, fill = foreign_name)) +
  geom_bar(aes(y = c(..count..[..group..==1]/sum(..count..[..group..==1]),
                     ..count..[..group..==2]/sum(..count..[..group..==2]))), 
               position = "dodge") +
  theme_classic() +
  scale_fill_manual(name = "Name",
                    values=c("gray", rgb(237/255, 212/255, 178/255))) +
  ylab("Share")+
  xlab("Response type") +
  ylim(0,0.9)

histo


### Saving the graph

ggsave(
  plot = histo,
  filename = paste(pathfigures, "Histo_foreign_distrib.pdf", sep="//"),
  width = 7,
  height = 5
)

rm(histo)


############# Positive responses ############# 

### Producing the graph

data_short <-
  data_argentina %>%
  group_by(foreign_name) %>%
  summarise(
    positive_response = mean(positive_response, na.rm = TRUE),
    se = NA
  )

for (i in 0:1) {
  value = sd(subset(data_argentina, foreign == i)$positive_response, na.rm = TRUE)
  nobs = length(subset(data_argentina, foreign == i)$id)
  data_short$se[i+1] = value/sqrt(nobs)
}

histo <- 
  ggplot(
    data = data_short, 
    aes(x = foreign_name, fill = foreign_name)) + 
  geom_bar(aes(y = positive_response), stat = "identity") +
  scale_fill_manual(name = "Name",
                    values = c("gray", rgb(237/255, 212/255, 178/255))) +
  geom_errorbar(aes(ymin = positive_response-1.96*se, ymax = positive_response + 1.96*se)) +
  theme_classic() +
  xlab(" ") +
  ylab("Share of positive responses") +
  ylim(0,0.35)

histo


### Saving the graph

ggsave(
  plot = histo,
  filename = paste(pathfigures, "Histo_foreign.pdf", sep="//"),
  width = 7,
  height = 5
)
rm(histo, data_short, i, nobs, value)


################################################################################ 
######################## Histogram of responses: regions #######################
################################################################################ 

############# All four answer types ############# 

### Producing the graph

histo <- ggplot(
  data = data_argentina, 
  aes(x = type_response, fill = treatment_region)) +
  geom_bar(aes(y = c(..count..[..group..==1]/sum(..count..[..group..==1]),
                     ..count..[..group..==2]/sum(..count..[..group..==2]),
                     ..count..[..group..==3]/sum(..count..[..group..==3]),
                     ..count..[..group..==3]/sum(..count..[..group..==4]))), 
           position = "dodge") +
  theme_classic() +
  scale_fill_manual(name = "Treatment",
                    values=c("gray", rgb(0/255, 159/255, 183/255), rgb(1, 196/255, 155/255), rgb(195/255, 60/255, 84/255))) +
  ylab("Share") +
  xlab("Response type") +
  ylim(0,0.9)

histo


### Saving the graph

ggsave(
  plot = histo,
  filename = paste(pathfigures, "Histo_regions_distrib.pdf", sep="//"),
  width = 7,
  height = 5
)
rm(histo)


############# Positive responses ############# 

### Producing the graph

data_short <-
  data_argentina %>%
  group_by(treatment_region) %>%
  summarise(
    positive_response = mean(positive_response, na.rm = TRUE),
    se = NA
  )

for (i in 1:4) {
  value = sd(subset(data_argentina, treat == i)$positive_response, na.rm = TRUE)
  nobs = length(subset(data_argentina, treat == i)$id)
  data_short$se[i] = value/sqrt(nobs)
}

histo <- 
  ggplot(
    data = data_short, 
    aes(x = treatment_region, fill = treatment_region)) + 
    geom_bar(aes(y = positive_response), stat = "identity") +
    scale_fill_manual(name = "Treatment",
                      values=c("gray", rgb(0/255, 159/255, 183/255), rgb(1, 196/255, 155/255), rgb(195/255, 60/255, 84/255))) +
    geom_errorbar(aes(ymin = positive_response-1.96*se, ymax = positive_response + 1.96*se)) +
    theme_classic() +
    xlab(" ") +
    ylab("Share of positive responses") +
    ylim(0,0.35)

histo

### Saving the graph

ggsave(
  plot = histo,
  filename = paste(pathfigures, "Histo_regions.pdf", sep="//"),
  width = 7,
  height = 5
)
rm(histo, data_short, i, nobs, value)



################################################################################ 
####################### Regression table: All treatments #######################
################################################################################ 

############# Estimations ############# 

### Foreign

lm1 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ", 
        " foreign + ",
        paste(controls, collapse="+"), "+",
        paste(fixed_effects, collapse="+")
      )
    ), 
    data = data_argentina,
    family = binomial(link = "probit")
  )
summary(lm1)

### Regional treatment

lm2 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ", 
        " factor(treatment_region) + ",
        paste(controls, collapse="+"), "+",
        paste(fixed_effects, collapse="+")
      )
    ), 
    data = data_argentina,
    family = binomial(link = "probit")
  )
summary(lm2)


############# Producing the table ############# 

collabels = c("Foreign", "Regions")

covlabels = c("Foreign", "Asia", "Europe", "South America", "Female team")

notes = "The dependent variable is a dummy variable for whether the applicant received a positive response. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include team gender and province-fixed effects."

title = "Treatment effects"

label = "tab: treatment"

stargazer(lm1, lm2,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          # keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "TE_primary.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)
PseudoR2(lm1)
PseudoR2(lm2)
rm(lm1, lm2, collabels, covlabels, notes, title, label)


################################################################################ 
############################# Heterogeneity: Gender ############################
################################################################################ 

############# Table ############# 

##### Estimations

### Foreign, male

lm1 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ", 
        " foreign + ",
        paste(controls, collapse="+"), "+",
        paste(fixed_effects, collapse="+")
      )
    ), 
    data = subset(data_argentina, male_team==1),
    family = binomial(link = "probit")
  )
summary(lm1)


### Foreign, female teams

lm2 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ", 
        " foreign + ",
        paste(controls, collapse="+"), "+",
        paste(fixed_effects, collapse="+")
      )
    ), 
    data = subset(data_argentina, male_team==0),
    family = binomial(link = "probit")
  )
summary(lm2)


### Regional treatment, male teams

lm3 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ", 
        " factor(treatment_region) + ",
        paste(controls, collapse="+"), "+",
        paste(fixed_effects, collapse="+")
      )
    ), 
    data = subset(data_argentina, male_team==1),
    family = binomial(link = "probit")
  )
summary(lm3)


### Regional treatment, female teams

lm4 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ", 
        " factor(treatment_region) + ",
        paste(controls, collapse="+"), "+",
        paste(fixed_effects, collapse="+")
      )
    ), 
    data = subset(data_argentina, male_team==0),
    family = binomial(link = "probit")
  )
summary(lm4)


##### Producing the table

collabels = c("Foreign male", "Foreign female", "Regions male", "Regions female")

covlabels = c("Foreign", "Asia", "Europe", "South America", "Female team")

notes = "The dependent variable is a dummy variable for whether the applicant received a positive response. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include province-fixed effects."

title = "Treatment effects by gender"

label = "tab: treatment gender"

stargazer(lm1, lm2, lm3, lm4, 
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          # keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "TE_gender.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)

PseudoR2(lm1)
PseudoR2(lm2)
PseudoR2(lm3)
PseudoR2(lm4)

rm(lm1, lm2, lm3, lm4, collabels, covlabels, notes, title, label)


############# Histograms ############# 

### Female clubs

data_short <-
  subset(data_argentina, male_team == 0) %>%
  group_by(treatment_region) %>%
  summarise(
    positive_response = mean(positive_response, na.rm = TRUE),
    se = NA
  )

for (i in 1:4) {
  value = sd(subset(data_argentina, treat == i)$positive_response, na.rm = TRUE)
  nobs = length(subset(data_argentina, treat == i)$id)
  data_short$se[i] = value/sqrt(nobs)
}

histo <- 
  ggplot(
    data = data_short, 
    aes(x = treatment_region, fill = treatment_region)) + 
  geom_bar(aes(y = positive_response), stat = "identity") +
  scale_fill_manual(name = "Treatment",
                    values=c("gray", rgb(0/255, 159/255, 183/255), rgb(1, 196/255, 155/255), rgb(195/255, 60/255, 84/255))) +
  geom_errorbar(aes(ymin = positive_response-1.96*se, ymax = positive_response + 1.96*se)) +
  theme_classic() +
  xlab(" ") +
  ylab("Share of positive responses") +
  ylim(0,0.35)

histo

table(subset(data_argentina, female_team==1)$positive_response)

ggsave(
  plot = histo,
  filename = paste(pathfigures, "Histo_female.pdf", sep="//"),
  width = 7,
  height = 5
)
rm(histo, data_short, i, nobs, value)


### Male clubs

data_short <-
  subset(data_argentina, male_team == 1) %>%
  group_by(treatment_region) %>%
  summarise(
    positive_response = mean(positive_response, na.rm = TRUE),
    se = NA
  )

for (i in 1:4) {
  value = sd(subset(data_argentina, treat == i)$positive_response, na.rm = TRUE)
  nobs = length(subset(data_argentina, treat == i)$id)
  data_short$se[i] = value/sqrt(nobs)
}

histo <- 
  ggplot(
    data = data_short, 
    aes(x = treatment_region, fill = treatment_region)) + 
  geom_bar(aes(y = positive_response), stat = "identity") +
  scale_fill_manual(name = "Treatment",
                    values=c("gray", rgb(0/255, 159/255, 183/255), rgb(1, 196/255, 155/255), rgb(195/255, 60/255, 84/255))) +
  geom_errorbar(aes(ymin = positive_response-1.96*se, ymax = positive_response + 1.96*se)) +
  theme_classic() +
  xlab(" ") +
  ylab("Share of positive responses") +
  ylim(0,0.35)

histo

ggsave(
  plot = histo,
  filename = paste(pathfigures, "Histo_male.pdf", sep="//"),
  width = 7,
  height = 5
)
rm(histo, data_short, i, nobs, value)



################################################################################ 
################### Heterogeneity: Country differences, table ##################
################################################################################ 


# ############# Shared border ############# 
# 
# lm1 <-
#   glm(
#     as.formula(
#       paste(
#         "positive_response ~ ",
#         "female_team + ",
#         paste(fixed_effects, collapse="+"), 
#         "+ border"
#       )
#     ),
#     data = subset(data_argentina, foreign == 1),
#     family = binomial(link = "probit")
#   )
# summary(lm1)


############# Shared language ############# 

lm2 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ",
        "female_team + ",
        paste(fixed_effects, collapse="+"), 
        "+ language"
      )
    ),
    data = subset(data_argentina, foreign == 1),
    family = binomial(link = "probit")
  )
summary(lm2)


############# GDP #############

lm3 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ",
        "female_team + ",
        paste(fixed_effects, collapse="+"),
        "+ log(gdp)"
      )
    ),
    data = subset(data_argentina, foreign == 1),
    family = binomial(link = "probit")
  )
summary(lm3)


############# Difference in FIFA ranking ############# 

lm4 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ",
        "female_team + ",
        paste(fixed_effects, collapse="+"), 
        "+ diff_FIFA"
      )
    ),
    data = subset(data_argentina, foreign == 1),
    family = binomial(link = "probit")
  )
summary(lm4)


# ############# HDI ############# 
# 
# lm5 <-
#   glm(
#     as.formula(
#       paste(
#         "positive_response ~ ",
#         "female_team + ",
#         paste(fixed_effects, collapse="+"), 
#         "+ HDI_value"
#       )
#     ),
#     data = subset(data_argentina, foreign == 1),
#     family = binomial(link = "probit")
#   )
# summary(lm5)


############# Immigrant population ############# 

# lm6 <-
#   glm(
#     as.formula(
#       paste(
#         "positive_response ~ ",
#         "female_team + ",
#         paste(fixed_effects, collapse="+"), 
#         "+ Immigrant_pop"
#       )
#     ),
#     data = data_argentina,
#     family = binomial(link = "probit")
#   )
# summary(lm6)


############# Proximity indices ############# 

# lm7 <-
#   glm(
#     as.formula(
#       paste(
#         "positive_response ~ ",
#         "female_team + ",
#         paste(fixed_effects, collapse="+"), 
#         "+ diff_power + diff_masculinity + diff_individualism + diff_uncertainty + diff_indulgence"
#       )
#     ),
#     data = data_argentina,
#     family = binomial(link = "probit")
#   )
# summary(lm7)


############# Producing the table ############# 

collabels = c("Language","FIFA", "GDP")

covlabels = c("")

notes = "The dependent variable is a dummy variable for whether the applicant received a positive response. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include team gender and province-fixed effects."

title = "Treatment effect by different metrics"

label = "tab: heterogeneity country"

stargazer(lm2, lm4, lm3,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          # covariate.labels = covlabels,
          # keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "Heterogeneity_country.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)
PseudoR2(lm1) 
PseudoR2(lm2)
PseudoR2(lm3)
PseudoR2(lm4)
PseudoR2(lm5)
PseudoR2(lm6)
PseudoR2(lm7)
rm(lm1, lm2, lm3, lm4, lm5, lm6, lm7, collabels, covlabels, notes, title, label)


################################################################################ 
#################### Heterogeneity: City differences, table ####################
################################################################################ 

############# Median wage ############# 

lm1 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ",
        "female_team + ",
        paste(fixed_effects, collapse="+"), 
        "+ foreign * median_wage"
      )
    ),
    data = data_argentina,
    family = binomial(link = "probit")
  )
summary(lm1)


############# Immigrant population ############# 

lm2 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ",
        "female_team + ",
        paste(fixed_effects, collapse="+"), 
        "+ foreign * more_immigrants"
      )
    ),
    data = data_argentina,
    family = binomial(link = "probit")
  )
summary(lm2)


############# Producing the table ############# 

collabels = c("Median wage","Immigrant population")

covlabels = c("")

notes = "The dependent variable is a dummy variable for whether the applicant received a positive response. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include team gender and province-fixed effects."

title = "Treatment effect by city characteristics"

label = "tab: heterogeneity city"

stargazer(lm1, lm2,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          # covariate.labels = covlabels,
          # keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "Heterogeneity_city.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)
PseudoR2(lm1) 
PseudoR2(lm2)
rm(lm1, lm2, collabels, covlabels, notes, title, label)


################################################################################ 
#################### Heterogeneity: All dimensions, figures ####################
################################################################################ 

############# Shared border ############# 

data_short <-
  data_argentina %>%
  group_by(border) %>%
  summarise(
    positive_response = mean(positive_response, na.rm = TRUE),
    se = NA
  )

for (i in 0:1) {
  value = sd(subset(data_argentina, border == i)$positive_response, na.rm = TRUE)
  nobs = length(subset(data_argentina, border == i)$id)
  data_short$se[i+1] = value/sqrt(nobs)
}

histo <- 
  ggplot(
    data = data_short, 
    aes(x = border, fill = factor(border))) + 
  geom_bar(aes(y = positive_response), stat = "identity") +
  scale_fill_manual(name = "Shared border",
                    values = c(rgb(237/255, 212/255, 178/255), "gray")) +
  geom_errorbar(aes(ymin = positive_response-1.96*se, ymax = positive_response + 1.96*se)) +
  theme_classic() +
  xlab(" ") +
  ylab("Share of positive responses") +
  ylim(0,0.35)

histo


### Saving the graph

ggsave(
  plot = histo,
  filename = paste(pathfigures, "Histo_border.pdf", sep="//"),
  width = 7,
  height = 5
)
rm(histo, data_short, i, nobs, value)


############# Shared language ############# 

data_short <-
  subset(data_argentina, foreign==1) %>%
  group_by(language) %>%
  summarise(
    positive_response = mean(positive_response, na.rm = TRUE),
    se = NA
  )

for (i in 0:1) {
  value = sd(subset(data_argentina, language == i)$positive_response, na.rm = TRUE)
  nobs = length(subset(data_argentina, language == i)$id)
  data_short$se[i+1] = value/sqrt(nobs)
}

data_short <-
  data_short %>%
  mutate(
    langue = ifelse(language == 1, "Yes", "No")
  )

histo <- 
  ggplot(
    data = data_short, 
    aes(x = language, fill = langue)) + 
  geom_bar(aes(y = positive_response), stat = "identity") +
  scale_fill_manual(name = "Shared language",
                    values = c(rgb(237/255, 212/255, 178/255), "gray")) +
  geom_errorbar(aes(ymin = positive_response-1.96*se, ymax = positive_response + 1.96*se)) +
  theme_classic() +
  xlab(" ") +
  ylab("Share of positive responses") +
  ylim(0,0.35)

histo


### Saving the graph

ggsave(
  plot = histo,
  filename = paste(pathfigures, "Histo_language.pdf", sep="//"),
  width = 7,
  height = 5
)
rm(histo, data_short, i, nobs, value)


############# GDP ############# 

data_short <-
  data_argentina %>%
  group_by(treatment_country) %>%
  summarise(
    positive_response = mean(positive_response, na.rm = TRUE),
    se = NA,
    gdp = mean(gdp, na.rm = TRUE)
  )

for (i in 1:13) {
  value_variable = data_short$gdp[i]
  value = sd(subset(data_argentina, gdp == value_variable)$positive_response, na.rm = TRUE)
  nobs = length(subset(data_argentina, gdp == value_variable)$id)
  data_short$se[i] = value/sqrt(nobs)
}

plt <- 
  ggplot(
    data = data_short, 
    aes(x = gdp, y = positive_response)) + 
  geom_smooth(method = lm, formula = y ~ x, level = FALSE, col = 'blue') +
  geom_point() + 
  geom_text_repel(aes(label = treatment_country), size = 3) +
  theme_classic() +
  xlab("GDP per capita (x1000 US$)") +
  ylab("Share of positive responses") +
  ylim(0,0.35)

plt


### Saving the graph

ggsave(
  plot = plt,
  filename = paste(pathfigures, "Plot_GDP.pdf", sep="//"),
  width = 7,
  height = 5
)
rm(plt, data_short, i, nobs, value, value_variable)


############# Difference in FIFA Ranking ############# 

data_short <-
  subset(data_argentina, foreign==1) %>%
  group_by(treatment_country) %>%
  summarise(
    positive_response = mean(positive_response, na.rm = TRUE),
    se = NA,
    diff_FIFA = mean(diff_FIFA, na.rm = TRUE)
  )

for (i in 1:12) {
  value_variable = data_short$diff_FIFA[i]
  value = sd(subset(data_argentina, diff_FIFA == value_variable)$positive_response, na.rm = TRUE)
  nobs = length(subset(data_argentina, diff_FIFA == value_variable)$id)
  data_short$se[i] = value/sqrt(nobs)
}

plt <- 
  ggplot(
    data = data_short, 
    aes(x = diff_FIFA, y = positive_response)) + 
  geom_smooth(method = lm, formula = y ~ x, level = FALSE, col = 'red') +
  geom_point() + 
  geom_text_repel(aes(label = treatment_country), size = 3) +
  theme_classic() +
  xlab("Difference in FIFA ranking") +
  ylab("Share of positive responses") +
  ylim(0,0.35)

plt


### Saving the graph

ggsave(
  plot = plt,
  filename = paste(pathfigures, "Plot_FIFA.pdf", sep="//"),
  width = 7,
  height = 5
)
rm(plt, data_short, i, nobs, value, value_variable)


############# HDI ############# 

data_short <-
  subset(data_argentina, foreign==1) %>%
  group_by(treatment_country) %>%
  summarise(
    positive_response = mean(positive_response, na.rm = TRUE),
    se = NA,
    HDI_value = mean(HDI_value, na.rm = TRUE)
  )

for (i in 1:12) {
  value_variable = data_short$HDI_value[i]
  value = sd(subset(data_argentina, HDI_value == value_variable)$positive_response, na.rm = TRUE)
  nobs = length(subset(data_argentina, HDI_value == value_variable)$id)
  data_short$se[i] = value/sqrt(nobs)
}

plt <- 
  ggplot(
    data = data_short, 
    aes(x = HDI_value, y = positive_response)) + 
  geom_smooth(method = lm, formula = y ~ x, level = FALSE, col = 'blue') +
  geom_point() + 
  geom_text_repel(aes(label = treatment_country), size = 3) +
  theme_classic() +
  xlab("Human Development Index") +
  ylab("Share of positive responses") +
  ylim(0,0.35)

plt

### Saving the graph

ggsave(
  plot = plt,
  filename = paste(pathfigures, "Plot_HDI.pdf", sep="//"),
  width = 7,
  height = 5
)
rm(plt, data_short, i, nobs, value, value_variable)


############# Immigrant pop ############# 

data_short <-
  data_argentina %>%
  group_by(treatment_country) %>%
  summarise(
    positive_response = mean(positive_response, na.rm = TRUE),
    se = NA,
    Immigrant_pop = mean(Immigrant_pop, na.rm = TRUE)
  )

for (i in 1:13) {
  value_variable = data_short$Immigrant_pop[i]
  value = sd(subset(data_argentina, Immigrant_pop == value_variable)$positive_response, na.rm = TRUE)
  nobs = length(subset(data_argentina, Immigrant_pop == value_variable)$id)
  data_short$se[i] = value/sqrt(nobs)
}

plt <- 
  ggplot(
    data = data_short, 
    aes(x = Immigrant_pop, y = positive_response)) + 
  geom_smooth(method = lm, formula = y ~ x, level = FALSE, col = 'blue') +
  geom_point() + 
  geom_text_repel(aes(label = treatment_country), size = 3) +
  theme_classic() +
  xlab("Immigrant population") +
  ylab("Share of positive responses") +
  ylim(0,0.35)

plt


### Saving the graph

ggsave(
  plot = plt,
  filename = paste(pathfigures, "Plot_Immigrant_pop.pdf", sep="//"),
  width = 7,
  height = 5
)
rm(plt, data_short, i, nobs, value, value_variable)


################################################################################ 
############################ Additional regressions ############################
################################################################################ 

############# Without controls ############# 

### Foreign

lm1 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ", 
        " foreign "
      )
    ), 
    data = data_argentina,
    family = binomial(link = "probit")
  )
summary(lm1)


### Regional treatment

lm2 <-
  glm(
    as.formula(
      paste(
        "positive_response ~ ", 
        " factor(treatment_region)"
      )
    ), 
    data = data_argentina,
    family = binomial(link = "probit")
  )
summary(lm2)


### Producing the table

collabels = c("Foreign", "Regions")

covlabels = c("Foreign", "Asia", "Europe", "South America")

notes = "The dependent variable is a dummy variable for whether the applicant received a positive response. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include team gender and province-fixed effects."

title = "Treatment effects without controls"

label = "tab: treatment without controls"

stargazer(lm1, lm2,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          # keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "TE_no_controls.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)
PseudoR2(lm1)
PseudoR2(lm2)
rm(lm1, lm2, collabels, covlabels, notes, title, label)


############# Linear regression ############# 

### Foreign

lm1 <-
  lm(
    as.formula(
      paste(
        "positive_response ~ ", 
        " foreign "
      )
    ), 
    data = data_argentina
  )
summary(lm1)


### Regional treatment

lm2 <-
  lm(
    as.formula(
      paste(
        "positive_response ~ ", 
        " factor(treatment_region)"
      )
    ), 
    data = data_argentina
  )
summary(lm2)


### Producing the table

collabels = c("Foreign", "Regions")

covlabels = c("Foreign", "Asia", "Europe", "South America")

notes = "The dependent variable is a dummy variable for whether the applicant received a positive response. * p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Controls include team gender and province-fixed effects."

title = "Treatment effects OLS"

label = "tab: treatment OLS"

stargazer(lm1, lm2,
          type='latex', 
          table.placement = "htbp",
          title = title,
          style = "all2",
          label = label,
          # align = TRUE,
          column.labels = collabels,
          covariate.labels = covlabels,
          # keep = keeps,
          keep.stat = c("rsq", "n"),
          summary=FALSE,
          dep.var.caption = NULL,
          mean.sd = TRUE,
          digits = 3,
          notes = notes,
          out = paste(pathtables, "TE_OLS.tex", sep="//"),
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE
)
rm(lm1, lm2, collabels, covlabels, notes, title, label)



