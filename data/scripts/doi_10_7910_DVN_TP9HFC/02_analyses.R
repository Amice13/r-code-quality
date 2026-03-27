####Replication Material for JOP

library(tidyverse)
library(readxl)
library(Hmisc)
library(naniar)
library(showtext)
library(ggthemes)
library(extrafont)
library(gt)
library(haven)
library(estimatr)
library(texreg)
library(jtools)
library(modelsummary)
library(knitr)
library(kableExtra)
library(randomizr)

rm(list=ls())
options(scipen=999)

#Data processed in file 1. 
data<-readRDS("data/processed/data.rds")
data_general<-readRDS("data/processed/data_general_rumors.rds")
data_prompts<-readRDS("data/processed/data_specific_prompts.rds")
game<-readRDS("data/processed/data_polarization_game.rds")



#Data consumption comparing sample and Reuters Institute.
#Table 1: Media consumption pattern

q5_vars <- c("Q5#1", "Q5#2", "Q5#4", "Q5#5")

df_long <- data %>%
  select(all_of(q5_vars)) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "media_code", values_to = "selected") %>%
  filter(selected == 1)

df_recoded <- df_long %>%
  mutate(media_format = case_when(
    media_code == "Q5#1" ~ "Newspaper",
    media_code == "Q5#2" ~ "TV",
    media_code == "Q5#4" ~ "Online",
    media_code == "Q5#5" ~ "Online", # Here I collapse 4 and 5 into one category
    TRUE ~ NA_character_
  ))

# And now I add Q#5 as "Social media"
df_social <- df_long %>%
  filter(media_code == "Q5#5") %>%
  mutate(media_format = "Social media")

# Combine the tables
df_combined <- bind_rows(df_recoded, df_social) %>%
  distinct(id, media_format)  # Avoid to count twice

total_respondents <- nrow(data)

summary_table <- df_combined %>%
  count(media_format) %>%
  mutate(percentage = round(100 * n / total_respondents, 1))

external_data <- tibble(
  media_format = c("Newspaper", "Online", "Social media", "TV"),
  external_percentage = c(10, 78, 54, 46)
)

summary_comparada <- summary_table %>%
  left_join(external_data, by = "media_format") %>% 
  arrange(desc(n))%>%
  select(-n)

kable(summary_comparada)




#Baseline results for beliefs. 
#Table 2: Baseline Results for Belief in Claims and General Narratives about Lula
data_prompts$treat_anti<-data_prompts$treatment
data_general$treat_anti<-data_general$treatment
data_prompts$treat_pt<-data_prompts$treatment
data_general$treat_pt<-data_general$treatment


prompt_antipetistas<-lm_robust(dv~treat_anti, data=data_prompts%>%filter(ideo_group=="Antipetistas"))

nar_antipetistas<-lm_robust(dv~treat_anti, data=data_general%>%filter(ideo_group=="Antipetistas"))



plot_antipt <- modelplot(list(prompt_antipetistas,nar_antipetistas), draw = FALSE)%>%
  filter(
    str_detect(term, "(Intercept)")
  )%>%select(estimate,std.error)%>%
  mutate(Variable=c("Specific Claim","General Narrative"))%>%
  relocate(Variable)


#Baseline for narratives
prompt_petistas<-lm_robust(dv~treat_pt, data=data_prompts%>%filter(ideo_group=="Petistas"))
nar_petistas<-lm_robust(dv~treat_pt, data=data_general%>%filter(ideo_group=="Petistas"))


plot_pt <- modelplot(list(prompt_petistas,nar_petistas), draw = FALSE)%>%
  filter(
    str_detect(term, "(Intercept)")
  )%>%select(estimate,std.error)%>%
  mutate(Variable=c("Specific Claim","General Narrative"))%>%
  relocate(Variable)


plot_belief<-left_join(plot_antipt,plot_pt, by=c("Variable"))%>%
  mutate(across(2:5, round, 2))


kable(plot_belief, format = "latex", booktabs = TRUE, align = "lcccc", caption = "Baseline Results for Belief in Claims and General Narratives about Lula") %>%
  add_header_above(c(" " = 1, "Antipetistas" = 2, "Petistas" = 2))



#Baseline results for all the feeling thermometers. 
#Table 3: Baseline for Measures of Affective Polarization

voters_antipetistas<-lm_robust(affective_polarization_voters~treatment, data=data%>%filter(ideo_group=="Antipetistas" & partido_jugador2=="Petista"))

politicians_antipetistas<-lm_robust(affective_polarization_politicians~treatment, data=data%>%filter(ideo_group=="Antipetistas" & partido_jugador2=="Petista"))

lula_antipetistas<-lm_robust(affective_polarization_lula_bolsonaro~treatment, data=data%>%filter(ideo_group=="Antipetistas" & partido_jugador2=="Petista"))




plot_antipt <- modelplot(list(voters_antipetistas,politicians_antipetistas,lula_antipetistas), draw = FALSE)%>%
  filter(
    str_detect(term, "(Intercept)")
  )%>%select(estimate,std.error)%>%
  mutate(Variable=c("Voters","Politicians","LulaXBolsonaro"))%>%
  relocate(Variable)



voters_petistas<-lm_robust(affective_polarization_voters~treatment, data=data%>%filter(ideo_group=="Petistas" & partido_jugador2=="Antipetista"))

politicians_petistas<-lm_robust(affective_polarization_politicians~treatment, data=data%>%filter(ideo_group=="Petistas" & partido_jugador2=="Antipetista"))

lula_petistas<-lm_robust(affective_polarization_lula_bolsonaro~treatment, data=data%>%filter(ideo_group=="Petistas" & partido_jugador2=="Antipetista"))

plot_pt <- modelplot(list(voters_petistas,politicians_petistas,lula_petistas), draw = FALSE)%>%
  filter(
    str_detect(term, "(Intercept)")
  )%>%select(estimate,std.error)%>%
  mutate(Variable=c("Voters","Politicians","LulaXBolsonaro"))%>%
  relocate(Variable)%>%
  mutate(estimate=ifelse(Variable=="LulaXBolsonaro",estimate*-1,estimate))


plot_thermometer<-left_join(plot_antipt,plot_pt, by=c("Variable"))%>%
  mutate(across(2:5, round, 2))


kable(plot_thermometer, format = "latex", booktabs = TRUE, align = "lcccc", caption = "Baseline Thermometer for Affetive Polarization") %>%
  add_header_above(c(" " = 1, "Antipetistas" = 2, "Petistas" = 2))



#This code makes Figure 2, comparing journalistic treatment versus crude treatment.
#Figure 2: Effects of the Style on Belief in Claims: Comparing Treatments

data_prompts_treat<-readRDS("data/processed/data_specific_prompts.rds")
data_prompts_treat$treatment<-relevel(data_prompts_treat$treatment,ref="Crude")


data_prompts_treat$treat_anti<-data_prompts_treat$treatment
data_prompts_treat$treat_pt<-data_prompts_treat$treatment
data_prompts_treat$treat_np<-data_prompts_treat$treatment



reg_antipetistas<-lm_robust(scale(dv)~treat_anti, data=data_prompts_treat%>%filter(ideo_group=="Antipetistas"))

reg_petistas<-lm_robust(scale(dv)~treat_pt, data=data_prompts_treat%>%filter(ideo_group=="Petistas"))

reg_np<-lm_robust(scale(dv)~treat_np +np_pt +np_antipt , data=data_prompts_treat%>%filter(ideo_group=="Non-partisans"))

reg_all<-lm_robust(scale(dv)~treatment, data=data_prompts_treat,fixed_effects = ideo_group,weights = weight  )


# Define the models
models_main <- list(
  "Antipetistas" = reg_antipetistas,
  "Petistas" = reg_petistas,
  "Non-Partisans" = reg_np,
  "All Groups" = reg_all)

# Define custom colors for the groups
color_map <- c(
  "Antipetistas" = "blue",
  "Petistas" = "red",
  "Non-Partisans" = "orange",
  "All Groups" = "grey"
)

# Extract the data from modelplot
plot_data <- modelplot(models_main, draw = FALSE)

# Define shapes for Crude and Journalistic treatments
plot_data <- plot_data%>%filter(term %in% c("treat_antiJournalistic","treat_ptJournalistic","treat_npJournalistic","treatmentJournalistic"))


# Define the specific order for the 'type' variable (facets)
plot_data$model <- factor(plot_data$model, levels = c("All Groups","Non-Partisans","Petistas", "Antipetistas"))



beliefs1<-ggplot(
  plot_data,
  aes(x = estimate, y = model, color = model)  # Use the ordered shape factor
) +
  geom_point(size = 3) +  # Set point size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.4) +  # Error bars without ticks
  geom_vline(xintercept = 0, linetype = 2, color = "red") +  # Vertical reference line
  scale_color_manual(values = color_map) +  # Apply custom color mapping
  theme_minimal() +
  theme(
    text = element_text(family = "Courier"),
    #axis.text.y = element_blank(),  # Remove y-axis text
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    strip.text.y.left = element_text(size = 12, angle = 0),  # Facet label styling on the left
    strip.placement = "outside",  # Place facet titles outside the panels
    panel.spacing = unit(1, "lines")  # Adjust space between facets
  ) +
  labs(
    x = "Effect of Treatment",
    y = "",
    color = "Partisan Group",
    shape = "Treatment Type"
  ) +
  theme(axis.text=element_text(size=12))



ggsave("output/figures/specific_beliefs_treatments.pdf", beliefs1)



#This code produces the graph for the difference in specif beliefs after treatment and control were exposed to question about it.
#Figure 3: Effects of the Style on Belief in Claims
data_prompts$treat_anti<-data_prompts$treatment
data_prompts$treat_anti_high<-data_prompts$treatment
data_prompts$treat_pt<-data_prompts$treatment
data_prompts$treat_pt_high<-data_prompts$treatment
data_prompts$treat_np<-data_prompts$treatment
data_prompts$treat_np_high<-data_prompts$treatment
data_prompts$treat_all_high<-data_prompts$treatment


reg_antipetistas<-lm_robust(scale(dv)~treat_anti, data=data_prompts%>%filter(ideo_group=="Antipetistas"))

reg_antipetistas_high<-lm_robust(scale(dv)~treat_anti_high, data=data_prompts%>%filter(ideo_group=="Antipetistas" & attention_level=="High" ))


reg_petistas<-lm_robust(scale(dv)~treat_pt, data=data_prompts%>%filter(ideo_group=="Petistas"))

reg_petistas_high<-lm_robust(scale(dv)~treat_pt_high, data=data_prompts%>%filter(ideo_group=="Petistas" & attention_level=="High" ))

reg_np<-lm_robust(scale(dv)~treat_np + np_pt + np_antipt , data=data_prompts%>%filter(ideo_group=="Non-partisans"))

reg_np_high<-lm_robust(scale(dv)~treat_np_high +np_pt +np_antipt, data=data_prompts%>%filter(ideo_group=="Non-partisans" & attention_level=="High" ))

reg_all<-lm_robust(scale(dv)~treatment, data=data_prompts,weights = weight,fixed_effects =  ideo_group)

reg_all_high<-lm_robust(scale(dv)~treat_all_high, data=data_prompts%>%filter(attention_level=="High"),weights = weight, fixed_effects =  ideo_group)




# Define the models
models_main <- list(
  "Antipetistas" = reg_antipetistas,
  "Antipetistas High" = reg_antipetistas_high,
  "Petistas" = reg_petistas,
  "Petistas High" = reg_petistas_high,
  "Non-Partisans" = reg_np,
  "Non-Partisans High" = reg_np_high,
  "All Groups" = reg_all,
  "All Groups High" = reg_all_high
)



# Define custom colors for the groups
color_map <- c(
  "Antipetistas" = "blue",
  "Petistas" = "red",
  "Non-Partisans" = "orange",
  "All Groups" = "grey"
)

# Extract the data from modelplot
plot_data <- modelplot(models_main, draw = FALSE)



# Define shapes for Crude and Journalistic treatments
plot_data <- plot_data%>%
  filter(
    str_detect(term, "treat")
  )%>%
  mutate(shape = case_when(
    str_detect(term, "Crude") ~ "Crude",        # Circle for "Crude"
    str_detect(term, "Journalistic") ~ "Journalistic" # Triangle for "Journalistic"
  ))%>%
  mutate(type=c(rep("Antipetistas",4),rep("Petistas",4),rep("Non-Partisans",4),rep("All Groups",4)))%>%
  mutate(attention=c("All","All","High Attention","High Attention","All","All","High Attention","High Attention","All","All","High Attention","High Attention","All","All","High Attention","High Attention"))


# Define the specific order for the 'type' variable (facets)
plot_data$type <- factor(plot_data$type, levels = c("Antipetistas", "Petistas", "Non-Partisans", "All Groups"))

# Define the specific order for the 'shape' variable
plot_data$shape <- factor(plot_data$shape, levels = c("Journalistic", "Crude"))


# Plot with ordered facets and shape
beliefs2<-ggplot(
  plot_data %>% filter(model %in% c("Antipetistas", "Petistas", "Non-Partisans", "All Groups")),
  aes(x = estimate, y = term, color = model, shape = shape)  # Use the ordered shape factor
) +
  geom_point(size = 3) +  # Set point size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.4) +  # Error bars without ticks
  geom_vline(xintercept = 0, linetype = 2, color = "red") +  # Vertical reference line
  scale_color_manual(values = color_map) +  # Apply custom color mapping
  theme_minimal() +
  theme(
    text = element_text(family = "Courier"),
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    strip.text.y.left = element_text(size = 12, angle = 0),  # Facet label styling on the left
    strip.placement = "outside",  # Place facet titles outside the panels
    panel.spacing = unit(1, "lines")  # Adjust space between facets
  ) +
  labs(
    x = "Effect of Treatment",
    y = "",
    color = "Group",
    shape = "Treatment Type"
  ) +
  guides(color = "none") +  # Remove the color legend
  facet_grid(
    type ~ .,  # Facet rows based on the 'type' variable
    scales = "free_y",  # Allow y-axis to vary across facets
    space = "free",  # Adjust facet spacing dynamically
    switch = "y"  # Move facet labels to the left
  )+
  theme(axis.text=element_text(size=12))


ggsave("output/figures/specific_beliefs.pdf", beliefs2)



#This code produces the graph for general beliefs comparing treatments
#Figure 4: Effects of the Style on Belief in General Narratives: Comparing Treatments

data_general_treat<-readRDS("data/processed/data_general_rumors.rds")

data_general_treat$treatment<-relevel(data_general_treat$treatment,ref="Crude")

data_general_treat$treat_anti<-data_general_treat$treatment
data_general_treat$treat_pt<-data_general_treat$treatment
data_general_treat$treat_np<-data_general_treat$treatment



reg_antipetistas<-lm_robust(scale(dv)~treat_anti, data=data_general_treat%>%filter(ideo_group=="Antipetistas"))

reg_petistas<-lm_robust(scale(dv)~treat_pt, data=data_general_treat%>%filter(ideo_group=="Petistas"))

reg_np<-lm_robust(scale(dv)~treat_np +np_pt +np_antipt , data=data_prompts%>%filter(ideo_group=="Non-partisans"))

reg_all<-lm_robust(scale(dv)~treatment, data=data_general_treat,fixed_effects = ideo_group_leaning,weights = weight  )


# Define the models
models_main <- list(
  "Antipetistas" = reg_antipetistas,
  "Petistas" = reg_petistas,
  "Non-Partisans" = reg_np,
  "All Groups" = reg_all)

# Define custom colors for the groups
color_map <- c(
  "Antipetistas" = "blue",
  "Petistas" = "red",
  "Non-Partisans" = "orange",
  "All Groups" = "grey"
)

# Extract the data from modelplot
plot_data <- modelplot(models_main, draw = FALSE)

# Define shapes for Crude and Journalistic treatments
plot_data <- plot_data%>%filter(term %in% c("treat_antiJournalistic","treat_ptJournalistic","treat_npJournalistic","treatmentJournalistic"))


# Define the specific order for the 'type' variable (facets)
plot_data$model <- factor(plot_data$model, levels = c("All Groups","Non-Partisans","Petistas", "Antipetistas"))



beliefs3<-ggplot(
  plot_data,
  aes(x = estimate, y = model, color = model)  # Use the ordered shape factor
) +
  geom_point(size = 3) +  # Set point size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.4) +  # Error bars without ticks
  geom_vline(xintercept = 0, linetype = 2, color = "red") +  # Vertical reference line
  scale_color_manual(values = color_map) +  # Apply custom color mapping
  theme_minimal() +
  theme(
    text = element_text(family = "Courier"),
    #axis.text.y = element_blank(),  # Remove y-axis text
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    strip.text.y.left = element_text(size = 12, angle = 0),  # Facet label styling on the left
    strip.placement = "outside",  # Place facet titles outside the panels
    panel.spacing = unit(1, "lines")  # Adjust space between facets
  ) +
  labs(
    x = "Effect of Treatment",
    y = "",
    color = "Partisan Group",
    shape = "Treatment Type"
  )+
  theme(axis.text=element_text(size=12))



ggsave("output/figures/general_beliefs_treatments.pdf", beliefs3)


#Figure 5: Effects of the Style on Belief in General Narratives
data_general$treat_anti<-data_general$treatment
data_general$treat_anti_high<-data_general$treatment
data_general$treat_pt<-data_general$treatment
data_general$treat_pt_high<-data_general$treatment
data_general$treat_np<-data_general$treatment
data_general$treat_np_high<-data_general$treatment
data_general$treat_all_high<-data_general$treatment



reg_antipetistas<-lm_robust(scale(dv)~treat_anti, data=data_general%>%filter(ideo_group=="Antipetistas"))

reg_antipetistas_high<-lm_robust(scale(dv)~treat_anti_high, data=data_general%>%filter(ideo_group=="Antipetistas" & attention_level=="High" ))

reg_petistas<-lm_robust(scale(dv)~treat_pt, data=data_general%>%filter(ideo_group=="Petistas"))

reg_petistas_high<-lm_robust(scale(dv)~treat_pt_high, data=data_general%>%filter(ideo_group=="Petistas" & attention_level=="High" ))

reg_np<-lm_robust(scale(dv)~treat_np +np_pt +np_antipt , data=data_general%>%filter(ideo_group=="Non-partisans"))

reg_np_high<-lm_robust(scale(dv)~treat_np_high +np_pt +np_antipt, data=data_general%>%filter(ideo_group=="Non-partisans" & attention_level=="High" ))

reg_all<-lm_robust(scale(dv)~treatment, data=data_general,fixed_effects = ideo_group_leaning,weights = weight  )

reg_all_high<-lm_robust(scale(dv)~treat_all_high, data=data_general%>%filter(attention_level=="High"),weights = weight, fixed_effects =  ideo_group_leaning  )



# Define the models
models_main <- list(
  "Antipetistas" = reg_antipetistas,
  "Antipetistas High" = reg_antipetistas_high,
  "Petistas" = reg_petistas,
  "Petistas High" = reg_petistas_high,
  "Non-Partisans" = reg_np,
  "Non-Partisans High" = reg_np_high,
  "All Groups" = reg_all,
  "All Groups High" = reg_all_high
)


# Define custom colors for the groups
color_map <- c(
  "Antipetistas" = "blue",
  "Petistas" = "red",
  "Non-Partisans" = "orange",
  "All Groups" = "grey"
)

# Extract the data from modelplot
plot_data <- modelplot(models_main, draw = FALSE)


# Define shapes for Crude and Journalistic treatments
plot_data <- plot_data%>%
  filter(
    str_detect(term, "treat")
  )%>%
  mutate(shape = case_when(
    str_detect(term, "Crude") ~ "Crude",        # Circle for "Crude"
    str_detect(term, "Journalistic") ~ "Journalistic" # Triangle for "Journalistic"
  ))%>%
  mutate(type=c(rep("Antipetistas",4),rep("Petistas",4),rep("Non-Partisans",4),rep("All Groups",4)))%>%
  mutate(attention=c("All","All","High Attention","High Attention","All","All","High Attention","High Attention","All","All","High Attention","High Attention","All","All","High Attention","High Attention"))


# Define the specific order for the 'type' variable (facets)
plot_data$type <- factor(plot_data$type, levels = c("Antipetistas", "Petistas", "Non-Partisans", "All Groups"))

# Define the specific order for the 'shape' variable
plot_data$shape <- factor(plot_data$shape, levels = c("Journalistic", "Crude"))


# Plot with ordered facets and shape
beliefs4<-ggplot(
  plot_data %>% filter(model %in% c("Antipetistas", "Petistas", "Non-Partisans", "All Groups")),
  aes(x = estimate, y = term, color = model, shape = shape)  # Use the ordered shape factor
) +
  geom_point(size = 3) +  # Set point size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.4) +  # Error bars without ticks
  geom_vline(xintercept = 0, linetype = 2, color = "red") +  # Vertical reference line
  scale_color_manual(values = color_map) +  # Apply custom color mapping
  theme_minimal() +
  theme(
    text = element_text(family = "Courier"),
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    strip.text.y.left = element_text(size = 12, angle = 0),  # Facet label styling on the left
    strip.placement = "outside",  # Place facet titles outside the panels
    panel.spacing = unit(1, "lines")  # Adjust space between facets
  ) +
  labs(
    x = "Effect of Treatment",
    y = "",
    color = "Group",
    shape = "Treatment Type"
  ) +
  guides(color = "none") +  # Remove the color legend
  facet_grid(
    type ~ .,  # Facet rows based on the 'type' variable
    scales = "free_y",  # Allow y-axis to vary across facets
    space = "free",  # Adjust facet spacing dynamically
    switch = "y"  # Move facet labels to the left
  )+
  theme(axis.text=element_text(size=12))



ggsave("output/figures/general_beliefs.pdf", beliefs4)




#Figure 6: Style of Misinformation on Distribution of Coupons
game<-readRDS("data/processed/data_polarization_game.rds")


game$treat_anti<-game$treatment
game$treat_pt<-game$treatment


reg_antipetistas<-lm_robust(scale(keep)~treat_anti, data=game%>%filter(ideo_group=="Antipetistas" & partido_jugador2=="Petista"))

reg_petistas<-lm_robust(scale(keep)~treat_pt, data=game%>%filter(ideo_group=="Petistas" & partido_jugador2=="Antipetista" ))



# Define the models
models_main <- list("Antipetistas"=reg_antipetistas,"Petistas"=reg_petistas)


# Define custom colors for the groups
color_map <- c(
  "Antipetistas" = "blue",
  "Petistas" = "red")



# Extract the data from modelplot
plot_data <- modelplot(models_main, draw = FALSE)

# Define variables
plot_data <- plot_data%>%
  filter(term!="(Intercept)")%>%
  mutate(Type = case_when(
    str_detect(term, "Journalistic") ~ "Journalistic",      
    str_detect(term, "Crude") ~ "Crude"
  ))

plot_data$Type <- factor(plot_data$Type, levels = c("Journalistic","Crude"))


game_main<-ggplot(
  plot_data,
  aes(x = estimate, y = term, color=model, shape=Type)  # Use the ordered shape factor
) +
  geom_point(size = 3) +  # Set point size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.4) +  # Error bars without ticks
  geom_vline(xintercept = 0, linetype = 2, color = "red") +  # Vertical reference line
  scale_color_manual(values = color_map) +  # Apply custom color mapping
  theme_minimal() +
  theme(
    text = element_text(family = "Courier"),
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    strip.text.y.left = element_text(size = 12, angle = 0),  # Facet label styling on the left
    strip.placement = "outside",  # Place facet titles outside the panels
    panel.spacing = unit(1, "lines")  # Adjust space between facets
  ) +
  labs(
    x = "Effect of Treatment",
    y = "",
    color = "Partisan Group"
  ) +
  facet_grid(
    model ~ .,  # Facet rows based on the 'type' variable
    scales = "free_y",  # Allow y-axis to vary across facets
    space = "free",  # Adjust facet spacing dynamically
    switch = "y"  # Move facet labels to the left
  )+
  theme(axis.text=element_text(size=12))

ggsave("output/figures/game_main.pdf", game_main)


#Figure 7: Style of Misinformation on Social DistanceAntipetistas

social_distance<-readRDS("data/processed/data_polarization_attitudes_1.rds")


social_distance$treat_anti<-social_distance$treatment
social_distance$treat_pt<-social_distance$treatment
social_distance$treat_np<-social_distance$treatment
social_distance$treat_np_anti<-social_distance$treatment


reg_antipetistas<-lm_robust(scale(dv)~treat_anti,fixed_effects = prompt, data=social_distance%>%filter(ideo_group=="Antipetistas" & partido_jugador2=="Petista"))

reg_petistas<-lm_robust(scale(dv)~treat_pt,  fixed_effects = prompt,data=social_distance%>%filter(ideo_group=="Petistas" & partido_jugador2=="Antipetista"))


reg_all<-lm_robust(scale(dv)~treatment, data=social_distance%>%filter(ideo_group!= "Non-partisans"),fixed_effects = ideo_group + prompt,weights = weight  )


# Define the models
models_main <- list(
  "Antipetistas" = reg_antipetistas,
  "Petistas" = reg_petistas,
  "Combined" = reg_all)

# Define custom colors for the groups
color_map <- c(
  "Antipetistas" = "blue",
  "Petistas" = "red",
  "Combined" = "grey")

# Extract the data from modelplot

# Extract the data from modelplot
plot_data <- modelplot(models_main, draw = FALSE)

# Define shapes for Crude and Journalistic treatments
plot_data <- plot_data%>%filter(
  str_detect(term, "treat")
)%>%
  mutate(shape = case_when(
    str_detect(term, "Crude") ~ "Crude",        # Circle for "Crude"
    str_detect(term, "Journalistic") ~ "Journalistic" # Triangle for "Journalistic"
  ))


# Define the specific order for the 'type' variable (facets)
plot_data$model <- factor(plot_data$model, levels = c("Antipetistas", "Petistas", "Combined"))

# Define the specific order for the 'shape' variable
plot_data$shape <- factor(plot_data$shape, levels = c("Journalistic", "Crude"))


# Plot with ordered facets and shape
sd_plot<-ggplot(
  plot_data,
  aes(x = estimate, y = term, color = model, shape = shape)  # Use the ordered shape factor
) +
  geom_point(size = 3) +  # Set point size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.4) +  # Error bars without ticks
  geom_vline(xintercept = 0, linetype = 2, color = "red") +  # Vertical reference line
  scale_color_manual(values = color_map) +  # Apply custom color mapping
  theme_minimal() +
  theme(
    text = element_text(family = "Courier"),
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    strip.text.y.left = element_text(size = 12, angle = 0),  # Facet label styling on the left
    strip.placement = "outside",  # Place facet titles outside the panels
    panel.spacing = unit(1, "lines")  # Adjust space between facets
  ) +
  labs(
    x = "Effect of Treatment",
    y = "",
    color = "Group",
    shape = "Treatment Type"
  ) +
  guides(color = "none") +  # Remove the color legend
  facet_grid(
    model ~ .,  # Facet rows based on the 'type' variable
    scales = "free_y",  # Allow y-axis to vary across facets
    space = "free",  # Adjust facet spacing dynamically
    switch = "y"  # Move facet labels to the left
  )+
  theme(axis.text=element_text(size=12))



ggsave("output/figures/social_distance.pdf", sd_plot)


#Figure 8: Necessary treatment effect to recover results given baseline polarization

sd(data$affective_polarization_voters[data$ideo_group=="Antipetistas" & data$treatment=="Control" & data$partido_jugador2=="Petista" ])

possible.effects <- seq(from=0.1, to=3.5, by=0.1)


power.Placebovscrude <- rep(NA, length(possible.effects))
#power.CrudevsJor<- rep(NA, length(possible.effects))
#power.PlacebovsJor  <- rep(NA, length(possible.effects))
alpha <- 0.01  #(one-tailed test at .05 level)
sims <- 1000
#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.effects)){
  tau <- possible.effects[j]
  p.Placebovscrude <- rep(NA, sims)
  #p.CrudevsJor <- rep(NA, sims)
  # p.PlacebovsJor <- rep(NA, sims)
  c.Placebovscrude <- rep(NA, sims)
  #c.CrudevsJor <- rep(NA, sims)
  #c.PlacebovsJor <- rep(NA, sims)
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=1232, mean=5.1, sd=4)
    tau1<-c(rep(0,530),rep(tau,702))
    Y1<-Y0+tau1
    Y2<-Y0+0.5
    Z.sim <- complete_ra(N = 1232, prob_each = c(1/3,1/3,1/3))
    Y.sim <- Y0*(Z.sim=="T1") + Y1*(Z.sim=="T2") +  Y2*(Z.sim=="T3") 
    frame.sim <- data.frame(Y.sim, Z.sim)
    fit.Placebovscrude <- lm(Y.sim ~ Z.sim=="T2", data=frame.sim%>%filter(Z.sim=="T1" | Z.sim=="T2"))
    #fit.CrudevsJor <- lm(Y.sim ~ Z.sim=="T3", data=frame.sim%>%filter(Z.sim=="T2" | Z.sim=="T3"))
    #fit.PlacebovsJor <- lm(Y.sim ~ Z.sim=="T3", data=frame.sim%>%filter(Z.sim=="T1" | Z.sim=="T3"))
    
    ### Need to capture coefficients and pvalues (one-tailed tests, so signs are important)
    c.Placebovscrude [i] <- summary(fit.Placebovscrude)$coefficients[2,1]
    #c.CrudevsJor [i] <- summary(fit.CrudevsJor)$coefficients[2,1]
    #c.PlacebovsJor [i] <- summary(fit.PlacebovsJor)$coefficients[2,1]
    p.Placebovscrude [i] <- summary(fit.Placebovscrude)$coefficients[2,4]
    # p.CrudevsJor [i] <- summary(fit.CrudevsJor)$coefficients[2,4]
    #p.PlacebovsJor [i] <- summary(fit.PlacebovsJor)$coefficients[2,4]
  }
  
  power.Placebovscrude[j]<- mean(c.Placebovscrude>0 & (p.Placebovscrude < alpha/2))
  
  
  print(j)
}

df_plot3<-bind_cols(possible.effects,power.Placebovscrude)

colnames(df_plot3)<-c("Effect","Power PlaceboXCrude", "Power PlaceboXJor")

df_plot_long3<-df_plot3%>%
  pivot_longer(cols=starts_with("Power"), names_to="Type", values_to = "Power")


simulation<-ggplot(df_plot_long3, aes(x=Effect, y=Power,color=Type, shape=Type))+
  geom_point()+
  theme_bw()+
  geom_hline(yintercept=0.8, linetype="dashed", 
             color = "red", size=0.5)




ggsave("output/figures/simulation.pdf", simulation)

#Figure 9: Additional time spent on survey by respondents in crude misinformation group


data_time_treat<-data%>%
  filter(duration<=3000)

data_time_treat$treatment<-relevel(data_time_treat$treatment,ref="Journalistic")

data_time_treat$treat_anti<-data_time_treat$treatment
data_time_treat$treat_pt<-data_time_treat$treatment
data_time_treat$treat_np<-data_time_treat$treatment


reg_antipetistas<-lm_robust(duration~treat_anti, data=data_time_treat%>%filter(ideo_group=="Antipetistas"))

reg_petistas<-lm_robust(duration~treat_anti, data=data_time_treat%>%filter(ideo_group=="Petistas"))

reg_np<-lm_robust(duration~treat_anti, data=data_time_treat%>%filter(ideo_group=="Non-partisans"))

reg_all<-lm_robust(duration~treatment, data=data_time_treat,fixed_effects = ideo_group ,weights = weight  )




# Define the models
models_main <- list(
  "Antipetistas" = reg_antipetistas,
  "Petistas" = reg_petistas,
  "Non-Partisans" = reg_np,
  "All Groups" = reg_all)

# Define custom colors for the groups
color_map <- c(
  "Antipetistas" = "blue",
  "Petistas" = "red",
  "Non-Partisans" = "orange",
  "All Groups" = "grey"
)

# Extract the data from modelplot
plot_data <- modelplot(models_main, draw = FALSE)

# Define shapes for Crude and Journalistic treatments
plot_data <- plot_data%>%filter(term %in% c("treat_antiCrude","treat_ptCrude","treat_npCrude","treatmentCrude"))


# Define the specific order for the 'type' variable (facets)
plot_data$model <- factor(plot_data$model, levels = c("All Groups","Non-Partisans","Petistas", "Antipetistas"))



time_treat<-ggplot(
  plot_data,
  aes(x = estimate, y = model, color = model)  # Use the ordered shape factor
) +
  geom_point(size = 3) +  # Set point size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.4) +  # Error bars without ticks
  geom_vline(xintercept = 0, linetype = 2, color = "red") +  # Vertical reference line
  scale_color_manual(values = color_map) +  # Apply custom color mapping
  theme_minimal() +
  theme(
    text = element_text(family = "Courier"),
    #axis.text.y = element_blank(),  # Remove y-axis text
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    strip.text.y.left = element_text(size = 12, angle = 0),  # Facet label styling on the left
    strip.placement = "outside",  # Place facet titles outside the panels
    panel.spacing = unit(1, "lines")  # Adjust space between facets
  ) +
  labs(
    x = "Effect of Treatment",
    y = "",
    color = "Partisan Group",
    shape = "Treatment Type"
  )+
  theme(axis.text=element_text(size=12))



ggsave("output/figures/time_treat.pdf", time_treat)


#Figure 10: Difference of time spent in survey comparing treatments and control
data_time<-data%>%
  filter(duration<=3000)

data_time$treatment<-relevel(data_time$treatment,ref="Control")

data_time$treat_anti<-data_time$treatment
data_time$treat_pt<-data_time$treatment
data_time$treat_np<-data_time$treatment




reg_antipetistas<-lm_robust(duration~treat_anti, data=data_time%>%filter(ideo_group=="Antipetistas"))

reg_petistas<-lm_robust(duration~treat_anti, data=data_time%>%filter(ideo_group=="Petistas"))

reg_np<-lm_robust(duration~treat_anti, data=data_time%>%filter(ideo_group=="Non-partisans"))

reg_all<-lm_robust(duration~treatment, data=data_time,fixed_effects = ideo_group ,weights = weight  )


# Define the models
models_main <- list(
  "Antipetistas" = reg_antipetistas,
  "Petistas" = reg_petistas,
  "Non-Partisans" = reg_np,
  "All Groups" = reg_all)

# Define custom colors for the groups
color_map <- c(
  "Antipetistas" = "blue",
  "Petistas" = "red",
  "Non-Partisans" = "orange",
  "All Groups" = "grey"
)

# Extract the data from modelplot
plot_data <- modelplot(models_main, draw = FALSE)

# Define shapes for Crude and Journalistic treatments
plot_data <- plot_data%>%
  filter(
    str_detect(term, "treat")
  )%>%
  mutate(shape = case_when(
    str_detect(term, "Crude") ~ "Crude",        # Circle for "Crude"
    str_detect(term, "Journalistic") ~ "Journalistic"
  ))

# Define the specific order for the 'type' variable (facets)
plot_data$model <- factor(plot_data$model, levels = c("Antipetistas", "Petistas", "Non-Partisans","All Groups"))

# Define the specific order for the 'shape' variable
plot_data$shape <- factor(plot_data$shape, levels = c("Journalistic", "Crude"))


time_control<-ggplot(
  plot_data,
  aes(x = estimate, y = term, color = model, shape = shape)  # Use the ordered shape factor
) +
  geom_point(size = 3) +  # Set point size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.4) +  # Error bars without ticks
  geom_vline(xintercept = 0, linetype = 2, color = "red") +  # Vertical reference line
  scale_color_manual(values = color_map) +  # Apply custom color mapping
  theme_minimal() +
  theme(
    text = element_text(family = "Courier"),
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    strip.text.y.left = element_text(size = 12, angle = 0),  # Facet label styling on the left
    strip.placement = "outside",  # Place facet titles outside the panels
    panel.spacing = unit(1, "lines")  # Adjust space between facets
  ) +
  labs(
    x = "Effect of Treatment",
    y = "",
    color = "Group",
    shape = "Treatment Type"
  ) +
  guides(color = "none") +  # Remove the color legend
  facet_grid(
    model ~ .,  # Facet rows based on the 'type' variable
    scales = "free_y",  # Allow y-axis to vary across facets
    space = "free",  # Adjust facet spacing dynamically
    switch = "y"  # Move facet labels to the left
  )+
  theme(axis.text=element_text(size=12))


ggsave("output/figures/time_control.pdf", time_control)


