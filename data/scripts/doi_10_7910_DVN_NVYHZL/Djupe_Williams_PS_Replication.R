# An Undergraduate Degree in ‘Data for Political Research’: High in Demand and Student Value but Low in Supply
# Forthcoming in PS: Political Science and Politics
# Paul A. Djupe, Denison University
# Miles D. Williams, Denison University

#Installs####
install.packages("tidyverse")
install.packages("rio")
install.packages("ggthemes")
install.packages("remotes")
remotes::install_github("ryanburge/socsci")
remotes::install_github("milesdwilliams15/coolorrr")
install.packages("sjPlot")
install.packages("ggtext")
install.packages("showtext")
install.packages("patchwork")
install.packages("ggpubr")

#Libraries####
library(tidyverse)
library(ggthemes)
library(rio)
library(socsci)
library(coolorrr)
library(sjPlot)
library(ggtext)
library(showtext)
library(patchwork)
library(ggpubr)

#Data Loading####
psdir <- "INSERT YOUR FILE FOLDER LOCATION HERE"

ps <- import(paste0(psdir, "ps_dpr_replication.csv"))


#Theme####
theme_set(ggthemes::theme_fivethirtyeight(
  base_family = "serif"
))
theme_update(axis.title = element_text(hjust = 0.5),
             plot.title.position = "plot",
             panel.background = element_rect(fill = "white"),
             plot.background = element_rect(fill = "white"), 
             legend.key = element_rect(fill = "white"),
             panel.grid.major = element_line(
               color = "gray", linewidth = .2
             ))


#Recodes####
val7 <- c("Strongly\nDisapprove", "Disapprove", 
          "Somewhat\nDisapprove", "Neither",
          "Somewhat\nApprove", "Approve",
          "Strongly\nApprove")
val5 <- c("Strongly\nDisagree", "Disagree",
          "Neither", "Agree", "Strongly\nAgree")
qnam <- c("...be good for student jobs",
          "...drive students away",
          "...fix STEM's leaky pipeline",
          "...be better as a minor",
          "...be good for critical thinking",
          "...idealy be taught inhouse")
ps |>
  transmute(
    ## support for DPR (-3 lowest and +3 highest)
    pro_dpr = 4 - q26,
    
    ## followups (-2 lowest and +2 highest)
    good_jobs = 3 - q27_1,
    drive_away = 3 - q27_2,
    fix_leaky_pipe = 3 - q27_3,
    better_minor = 3 - q27_4,
    critical_thinking = 3 - q27_5,
    inhouse = 3 - q27_6,
    
    ## academic position:
    position = frcode(
      q28 == 1 ~ "Grad Student",
      q28 == 2 ~ "Post-doc",
      q28 == 3 ~ "Assistant Prof",
      q28 == 4 ~ "Associate Prof",
      q28 == 5 ~ "Full Prof",
      TRUE ~ "Other"
    ),
    
  
    institution = frcode(
      q30 == 1 ~ "R1",
      q30 == 2 ~ "R2",
      q30 == 3 ~ "R3",
      q30 == 4 ~ "Masters Granting",
      q30 == 5 ~ "Liberal Arts",
      q30 == 6 ~ "Community College",
      TRUE ~ "Other"
    ),
  
    ## methods used in research:
    methods = frcode(
      q35 %in% 1:2 ~ "Quantitative",
      q35 %in% 3:4 ~ "Qualitative",
      q35 %in% 5:6 ~ "Normative"
    ),
    
   
    ## gender?
    gender = frcode(
      q37 == 1 ~ "Male",
      q37 == 2 ~ "Female",
      q37 == 3 ~ "Non-binary"
    ),
    
    ## race?
    race = frcode(
      q38 == 1 ~ "White",
      q38 == 2 ~ "Black",
      q38 == 3 ~ "Latino/a",
      q38 == 4 ~ "Asian",
      q38 == 5 ~ "Other"
    ),
    
    ## age?
    age = 2024 - q39_4,
    wgt=wgt) -> newps

#Figure 1 -- There is Widespread Support for a DPR, Methods-Forward-Style Major####


## Summarize overall support for DPR
newps |>
  ct(pro_dpr, show_na = F, wt=wgt) |>
  mutate(
    label = val7
  ) |>
  ggplot() +
  aes(x = reorder(label, pro_dpr), y = pct) +
  geom_col(
    color = "black",
    fill = "skyblue"
  ) +
  geom_text(
    aes(
      label = scales::percent(round(pct, 2))
    ),
    family = "serif",
    vjust = 1.5
  ) +
  geom_bracket(
    xmin = 4.5,
    xmax = 7.5,
    y.position = .26,
    label = "61% approve",
    family = "serif"
  ) +
  geom_bracket(
    xmin = .5,
    xmax = 3.5,
    y.position = .14,
    label = "23% disapprove",
    family = "serif"
  ) +
  labs(
    x = NULL,
    y = NULL,
    #title = "Most faculty support DPR",
    #subtitle = "% approval/disapproval of a methods forward political science major"
  ) +
  scale_y_continuous(
    breaks = NULL
  ) +
  theme(
    panel.grid.major.x = element_blank()
  ) + 
  expand_limits(y=c(0, .3))
ggsave(file=paste0(psdir, "Fig1.tiff"), height=4, width=5, dpi=300)

#Figure 2 -- How Support for DPR is Linked to Beliefs about the Program####
newps |>
  pivot_longer(
    good_jobs:inhouse
  ) |>
  mutate(
    name = rep(
      qnam, len = n()
    )
  ) |>
  ggplot() +
  aes(
    x = pro_dpr,
    y = value
  ) +
  geom_smooth(
    method = "lm",
    aes(weight=wgt),
    formula = y ~ poly(x, 2),
    color = "steelblue"
  ) +
  facet_wrap(~ name) +
  scale_x_continuous(
    breaks = c(-3, 0, 3),
    labels = val7[c(1, 4, 7)]
  ) +
  scale_y_continuous(
    breaks = -2:2,
    labels = val5
  ) +
  labs(
    subtitle = paste0(
      "Respondent agreement with the idea that DPR would..."
    ),
    x = "\nOverall Approval of DPR",
    y = "Belief Agreement"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(
      hjust = c(0, .5, 1)
    )
  )

ggsave(file=paste0(psdir, "Fig2.tiff"), height=6, width=9, dpi=300)

#Figure 3 -- Support for DPR Shown by Faculty Institutions, Research Methods, and Demographics####

newps |>
  group_by(position) |>
  filter(wgt!="NA") |> 
  socsci::mean_ci(pro_dpr, ci = .83, wt=wgt) |>
  # mutate(
  #   upper = ifelse(
  #     upper > 2, 2, upper
  #   )
  # ) |>
  ggplot() +
  aes(
    x = mean,
    xmin = lower,
    xmax = upper,
    y = position
  ) +
  geom_pointrange(
    color = "steelblue"
  ) +
  geom_text(
    data = . %>% filter(position == "Post-doc"),
    aes(label = "Mean with 83% CI"),
    vjust = 1.75, hjust=.45,
    color = "steelblue",
    fontface = "bold",
    size=3,
    family = "serif"
  ) +
  geom_vline(
    xintercept = 0,
    lty = 2
  ) +
  scale_x_continuous(
    breaks = c(-2, 0, 2),
    labels = 
      c("Strongly\nDisapprove", "Neutral", "Strongly\nApprove"),
    limits = c(-2, 4)
  ) +
  labs(
    subtitle = paste0(
      "Support for DPR by academic position"
    ),
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.caption = element_markdown(),
    panel.grid.major.x = element_blank()
  ) -> p1

newps |>
  group_by(institution) |>
  filter(wgt!="NA") |> 
  socsci::mean_ci(pro_dpr, ci = .83, wt=wgt) |>
  ggplot() +
  aes(
    x = mean,
    xmin = lower,
    xmax = upper,
    y = institution
  ) +
  geom_pointrange(
    color = "steelblue"
  ) +
  geom_vline(
    xintercept = 0,
    lty = 2
  ) +
  scale_x_continuous(
    breaks = c(-2, 0, 2),
    labels = 
      c("Strongly\nDisapprove", "Neutral", "Strongly\nApprove"),
    limits = c(-2, 2)
  ) +
  labs(
    subtitle = paste0(
      "Support for DPR by academic institution"
    ),
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.caption = element_markdown(),
    panel.grid.major.x = element_blank()
  ) -> p2

newps |>
  group_by(methods) |>
  filter(wgt!="NA") |> 
  socsci::mean_ci(pro_dpr, ci = .83, wt=wgt) |>
  drop_na() |>
  ggplot() +
  aes(
    x = mean,
    xmin = lower,
    xmax = upper,
    y = methods
  ) +
  geom_pointrange(
    color = "steelblue"
  ) +
  geom_vline(
    xintercept = 0,
    lty = 2
  ) +
  scale_x_continuous(
    breaks = c(-3, 0, 3),
    labels = 
      c("Strongly\nDisapprove", "Neutral", "Strongly\nApprove"),
    limits = c(-3, 3)
  ) +
  labs(
    subtitle = paste0(
      "Support for DPR by research method"
    ),
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.caption = element_markdown(),
    axis.text.x = element_text(
      hjust = c(0, .5, 1)
    ),
    panel.grid.major.x = element_blank()
  ) -> p3


newps |>
  group_by(gender) |>
  filter(wgt!="NA") |> 
  socsci::mean_ci(pro_dpr, ci = .83, wt=wgt) |>
  drop_na() |>
  ggplot() +
  aes(
    x = mean,
    xmin = lower,
    xmax = upper,
    y = gender
  ) +
  geom_pointrange(
    color = "steelblue"
  ) +
  geom_vline(
    xintercept = 0,
    lty = 2
  ) +
  scale_x_continuous(
    breaks = c(-2, 0, 2),
    labels = 
      c("Strongly\nDisapprove", "Neutral", "Strongly\nApprove"),
    limits = c(-2, 2)
  ) +
  labs(
    subtitle = paste0(
      "Support for DPR by gender"
    ),
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.caption = element_markdown(),
    axis.text.x = element_text(
      hjust = c(0, .5, 1)
    ),
    panel.grid.major.x = element_blank()
  ) -> p4

newps |>
  group_by(race) |>
  filter(wgt!="NA") |> 
  socsci::mean_ci(pro_dpr, ci = .83, wt=wgt) |>
  drop_na() |>
  ggplot() +
  aes(
    x = mean,
    xmin = lower,
    xmax = upper,
    y = race
  ) +
  geom_pointrange(
    color = "steelblue"
  ) +
  geom_vline(
    xintercept = 0,
    lty = 2
  ) +
  scale_x_continuous(
    breaks = c(-2, 0, 2),
    labels = 
      c("Strongly\nDisapprove", "Neutral", "Strongly\nApprove"),
    limits = c(-2.5, 2.5)
  ) +
  labs(
    subtitle = paste0(
      "Support for DPR by race"
    ),
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.caption = element_markdown(),
    axis.text.x = element_text(
      hjust = c(0, .5, 1)
    ),
    panel.grid.major.x = element_blank()
  ) -> p5


newps |>
  filter(wgt!="NA") |> 
  ggplot() +
  aes(
    x = age,
    y = pro_dpr
  ) +
  geom_smooth(
    aes(weight=wgt),
    color = "steelblue",
    method = "gam"
  ) +
  geom_rug(
    color = "red3",
    sides = "b",
    alpha = .1,
    size = 1.5
  ) +
  geom_hline(
    yintercept = 0,
    lty = 2
  ) +
  scale_x_continuous(
    breaks = seq(25, 105, by = 5)
  ) +
  scale_y_continuous(
    breaks = c(-2, 0, 2),
    labels = c("Strongly\nDisapprove", "Neutral", "Strongly\nApprove"),
    limits = c(-2, 2.2)
  ) +
  annotate(
    "text",
    x = 60,
    y = -1.75,
    label = "Distribution of Faculty Ages",
    color = "red3",
    family = "serif"
  ) +
  labs(
    subtitle = paste0(
      "Approval for DPR by age"
    ),
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.caption = element_markdown(),
    panel.grid.major.x = element_blank()
  ) -> p6

(p1 + p2 + p3) / (p4 + p5 + p6) &
  theme(
    plot.subtitle = element_text(face = "bold.italic"),
    plot.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 1,
      linetype = 1
    )
  )
ggsave(file=paste0(psdir, "Fig3.tiff"), height=6, width=10, dpi=300)



#Removes####
rm(list=ls(pattern="p"))
rm(qnam, val5, val7)
detach("package:ggpubr", unload=T)

