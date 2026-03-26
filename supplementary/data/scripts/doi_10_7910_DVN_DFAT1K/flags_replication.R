library(haven)
library(tidyverse)
library(specr)
library(marginaleffects)
library(monochromeR)
library(colorblindr)
library(ggdist)
library(ggrepel)
library(distributional)
library(scales)
library(performance)

theme_nhsr_demo <- function(base_size = 12, 
                            dark_text = "#1A242F") {
  
  mid_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]
  
  theme_minimal(base_size = base_size) +
    theme(text = element_text(colour = mid_text, family = "BrandonText", lineheight = 1.1),
          plot.title = element_text(colour = dark_text, family = "EnriquetaSB", size = rel(1.6), margin = margin(12, 0, 8, 0)),
          plot.subtitle = element_text(size = rel(1.1), margin = margin(4, 0, 0, 0)),
          axis.text.y = element_text(colour = light_text, size = rel(0.8)),
          axis.title.y = element_text(size = 12, margin = margin(0, 4, 0, 0)),
          axis.text.x = element_text(colour = mid_text, size = 12),
          axis.title.x = element_blank(),
          legend.position = "top",
          legend.justification = 1,
          panel.grid = element_line(colour = "#F3F4F5"),
          plot.caption = element_text(size = rel(0.8), margin = margin(8, 0, 0, 0)),
          plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm"))
}

dark_text <- "#1A242F"
  
monochromeR::generate_palette(
  dark_text, 
  "go_lighter",
  n_colours = 5,
  view_palette = TRUE
)
mid_text <-  monochromeR::generate_palette(
  dark_text, "go_lighter",
  n_colours = 5)[2]

light_text <-  monochromeR::generate_palette(
  dark_text, "go_lighter",
  n_colours = 5)[3]

#### 0. cleaning ####
murcia_raw <- read_sav("/Users/aloport/Dropbox/Mac (3)/Downloads/BARÓMETRO CEMOP PRIMAVERA 2022.sav")

#view(dfSummary(murcia_raw))

murcia <- murcia_raw %>%
  mutate(across(c(P1:P15), 
                as.double)) |>
  mutate(across(c(P33:P44), 
                as.double)) |>
   transmute(edu_raw= case_when(P43>6 ~ NA_real_,
                                      TRUE ~P43), 
             edu= case_when(edu_raw %in% c(1,2) ~ "Primary education or less",
                            edu_raw %in% c(3,4) ~ "Secondary education",
                            edu_raw %in% c(5) ~ "Vocational education",
                            edu_raw %in% c(6) ~ "University or more",
                            
                            ),
             work_situation= factor(case_when(P44== 1 ~ "Private worker",
                                              P44== 2 ~ "Public worker",
                                              P44== 3 ~ "Freelance worker",
                                              P44== 4 ~ "Retired",
                                              P44== 5 ~ "Unemployed",
                                              P44== 6 ~ "Student")),
             age= factor(case_when(COD_EDAD== 1 ~ "18-30",
                                   COD_EDAD== 2 ~ "31-44",
                                   COD_EDAD== 3 ~ "45-64",
                                   COD_EDAD== 4 ~ ">64"),
                         levels= c("18-30", "31-44", "45-64", ">64")),
             gender= factor(case_when(SEXO== 1 ~ "Man",
                                      SEXO== 2 ~ "Woman")), 
             size_town= factor(case_when(HÁBITAT== 1 ~ "0-10k",
                                         HÁBITAT== 2 ~ "10-50k",
                                         HÁBITAT== 3 ~ "50-300k",
                                         HÁBITAT== 4 ~ ">300k")),
             anti_lgbtflags= case_when(P9 ==98 ~ NA_real_,
                                     P9 ==99 ~ NA_real_,
                                     TRUE ~ P9), 
             anti_lgbtflags_fac= factor(case_when(anti_lgbtflags == 1 ~ "Strongly agree",
                                           anti_lgbtflags == 2 ~ "Somewhat agree",
                                           anti_lgbtflags == 3 ~ "Somewhat disagree",
                                           anti_lgbtflags == 4 ~ "Strongly disagree"), 
                                        levels= c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")),
             anti_lgbtflags_reduced= case_when(anti_lgbtflags %in% c(1,2) ~ 1,
                                           TRUE~ 0),
             pp_evaluation= 6-P11, #8 and 9 remove
             psoe_evaluation=6-P12, 
             up_evaluation=6-P13,
             vox_evaluation=6-P14,
             cs_evaluation=6-P15,
             anti_lgbtflags = 5-anti_lgbtflags,
             abascal_evaluation=  as.double(P16N_VALORACION_1),
             arrimadas_evaluation=  as.double(P16N_VALORACION_3),
             feijoo_evaluation=  as.double(P16N_VALORACION_4),
             belarra_evaluation=  as.double(P16N_VALORACION_6),
             sanchez_evaluation=  as.double(P16N_VALORACION_10),

             vote_intention=  factor(case_when(P26== 1 ~ "PP",
                                               P26== 2 ~ "PSOE",
                                               P26== 3 ~ "Cs",
                                               P26== 4 ~ "UP",
                                               P26== 5 ~ "Vox",
                                               TRUE ~ NA_character_)),
             
         vote_recall=  factor(case_when(P42== 1 ~ "PP",
                                        P42== 2 ~ "PSOE",
                                        P42== 3 ~ "Cs",
                                        P42== 4 ~ "UP",
                                        P42== 5 ~ "Vox",
                                        TRUE ~ NA_character_)),
         left_right=  P39,
         left_right_dad=  P40,
         left_right_mum=  P41,
         
         info_socialmedia=  P33, #remove 6 and 9
         info_whatsapp=  P34,
         info_laverdad=  P35,
         info_laopinion=  P36,
         info_7tv=  P37,
         info_lasexta=  P38, provider= "CEMOP") |>
  
  mutate_at(.vars= vars(pp_evaluation:cs_evaluation),  funs(case_when(
    .==-3 ~ 1,
    .==-2 ~ 2,
    .==1 ~ 3,
    .==2 ~ 4,
    .==3 ~5,
    . >3 ~ NA_real_  
    ))) |>
  mutate_at(.vars= vars(abascal_evaluation:sanchez_evaluation),  funs(scales::rescale(., to=c(1,5)))) |>
  mutate_at(.vars= vars(info_socialmedia:info_lasexta),  funs(case_when(
    . >5 ~ NA_real_,
    TRUE ~ .
  ))) 



ess_murcia <- read_stata("/Users/aloport/Dropbox/Mac (3)/Downloads/ESS10SC.dta/ESS10SC.dta") |> 
  filter(region=="ES62")


ess_murcia_stats <- ess_murcia |>  
  mutate(adopt_bin= case_when(hmsacld %in% c(4,5) ~ 1,
                              hmsacld %in% c(1,2,3) ~ 0),
         freelife_bin= case_when(freehms %in% c(4,5) ~ 1,
                                 freehms %in% c(1,2,3) ~ 0),
         shame_bin= case_when(hmsfmlsh %in% c(1,2) ~ 1,
                              hmsfmlsh %in% c(3,4,5) ~ 0),
         shame= 6- hmsfmlsh,
         adopt= as.double(hmsacld),
         freelife= as.double(freehms),
         shame4=  rescale(shame, to = c(1, 4)),
         adopt4=  rescale(adopt, to = c(1, 4)),
         freelife4= rescale(freelife, to = c(1, 4)),
         left_right= as.double(lrscale),
         left_right= rescale(left_right, to= c(1,10)),
         provider= "ESS",
         gender= factor(gndr),
         age= case_when(agea < 31 ~ "18-30",
                        agea >30 & agea<45  ~ "31-44",
                        agea >44 & agea<65 ~ "45-64",
                        agea >64 ~ ">64"
                        
         ),
         edu= case_when(edulvlb %in% c(620, 720, 800) ~ "University or more",
                        edulvlb %in% c(322, 421, 520, 222) ~ "Vocational education",
                        edulvlb %in% c(213, 313, 311) ~ "Secondary education",
                        edulvlb %in% c(0, 113, 129) ~ "Primary education or less",
                        
                        )
         ) |> 
  dplyr::select(starts_with(c("adopt", "freelife", "shame")), gender, edu, age, left_right,provider) 



cemop_murcia_stats <- murcia |> 
  mutate(provider= "CEMOP") |> 
  dplyr::select(provider, anti_lgbtflags, anti_lgbtflags_reduced) |> 
  mutate()



murcijitters <- full_join(cemop_murcia_stats, ess_murcia_stats) |> 
  dplyr::select(ends_with("5"), anti_lgbtflags, provider) |> 
  pivot_longer(-provider, names_to = "issue", values_to = "value") |> 
  mutate(value= factor(case_when(value== 1 & provider== "ESS" ~ "Strongly disagree",
                                 value== 2 & provider== "ESS" ~ "Disagree",
                                 value== 3 & provider== "ESS" ~ "Neither agree nor disagree",
                                 value== 4 & provider== "ESS" ~ "Agree",
                                 value== 5 & provider== "ESS" ~ "Strongly agree",
                                 value== 1 & provider== "CEMOP" ~ "Strongly disagree",
                                 value== 2 & provider== "CEMOP" ~ "Disagree",
                                 value== 3 & provider== "CEMOP" ~ "Agree",
                                 value== 4 & provider== "CEMOP" ~ "Strongly agree"),
                       levels=c("Strongly disagree", "Disagree", 
                                "Neither agree nor disagree",
                                "Agree", "Strongly agree"))) |> 
  mutate(issue= factor(case_when(issue=="anti_lgbtflags" ~ "Anti flags (CEMOP, n=800)",
                                 issue=="adopt5" ~ "Anti adoption (ESS, n=74)",
                                 issue=="freelife5" ~ "Anti free life (ESS, n=74)",
                                 issue=="shame5" ~ "Shame (ESS, n=74)"),
                       levels=c("Anti flags (CEMOP, n=800)",
                                "Anti adoption (ESS, n=74)",
                                "Anti free life (ESS, n=74)",
                                "Shame (ESS, n=74)"
                       )))


#### 0.5 table CEMOP-ESS ####

#### 1. descriptives ####

#Figure 2

  full_join(cemop_murcia_stats, ess_murcia_stats) |> 
  dplyr::select(shame, adopt, freelife, anti_lgbtflags, provider) |> 
  pivot_longer(-provider, names_to = "issue", values_to = "value") |> 
  mutate(value= factor(case_when(value== 5 & provider== "ESS" ~ "Strongly disagree",
                          value== 4 & provider== "ESS" ~ "Disagree",
                          value== 3 & provider== "ESS" ~ "Neither agree nor disagree",
                          value== 2 & provider== "ESS" ~ "Agree",
                          value== 1 & provider== "ESS" ~ "Strongly agree",
                          value== 4 & provider== "CEMOP" ~ "Strongly disagree",
                          value== 3 & provider== "CEMOP" ~ "Disagree",
                          value== 2 & provider== "CEMOP" ~ "Agree",
                          value== 1 & provider== "CEMOP" ~ "Strongly agree"),
                       levels=c("Strongly disagree", "Disagree", 
                                "Neither agree nor disagree",
                                "Agree", "Strongly agree"))) |>
  drop_na(value) |> 
  group_by(provider, issue, value) |> 
  summarise(count= n()) |> 
  ungroup() |> 
  #add missing rows due to no values
  add_row(provider= "ESS", issue="freelife", value="Agree", count=0) |> 
  add_row(provider= "ESS", issue="freelife", value="Strongly agree", count=0) |> 
  add_row(provider= "ESS", issue="shame", value="Strongly agree", count=0) |> 
  group_by(provider, issue) |> 
  
  mutate(relative=round(count/sum(count)*100, 1),
         value= factor(value, levels= c("Strongly disagree", "Disagree", 
                                         "Neither agree nor disagree",
                                         "Agree", "Strongly agree")),
         issue= factor(case_when(issue=="anti_lgbtflags" ~ "Flags",
                                 issue=="adopt" ~ "Adoption",
                                 issue=="freelife" ~ "Free life",
                                 issue=="shame" ~ "No shame"),
                       levels=c("Flags",
                                "Adoption",
                                "Free life",
                                "No shame"
                                )),
          relative2= case_when(relative<2 ~ NA_real_,
                              TRUE ~ relative)
         )  |> 
  ggplot() +
  #geom_vline(xintercept = 50, color="grey") +
  #geom_jitter(width=0.1, height=0.2, size=3) +  # plot all the observations in the background
  geom_bar(aes(relative, issue, fill= stage(value, after_scale = alpha(fill, .9)), 
               color=stage(value, after_scale = alpha(color, .9)), label= relative2), position="stack", stat="identity")+
  scale_fill_brewer(type="seq", palette = "PRGn") +
  scale_color_brewer(type="seq", palette = "PRGn") +
  theme_nhsr_demo() + 
  guides(fill=guide_legend(title="")) +
  coord_cartesian(expand=FALSE)+
  geom_text(aes(relative, issue, fill= value, label= relative2), size = 5, position = position_stack(vjust = 0.5))+ 
  scale_x_continuous(labels = scales::percent_format(scale = 1))+
  #geom_jitter(data=murcijitters, aes(value, issue), width=0.1, height=0.2, alpha = 0.1, size=3) +  # plot all the observations in the background
  labs(y="") + 
  theme(text = element_text(size = 20)) 

ggsave("compare_flags.png", width =10, height =7)


#### 1.5 ORDINAL logit regressions ####

library(MASS)
library(ordinal)
library(ggeffects)

voteint_olr <- clm(
  anti_lgbtflags_fac ~ gender + edu + age + size_town + work_situation +
    left_right + left_right_dad + left_right_mum +
    info_socialmedia + info_whatsapp + info_laverdad + info_laopinion + info_7tv + info_lasexta+
    vote_intention,  data = murcia, link = "logit", 
)|> 
  #marginaleffects() |> 
  tidy() |> as.data.frame() |> 
  mutate(model= "Vote intention model")



voterec_olr <- clm(
  anti_lgbtflags_fac ~ gender + edu + age + size_town + work_situation +
    left_right + left_right_dad + left_right_mum +
    info_socialmedia + info_whatsapp + info_laverdad + info_laopinion + info_7tv + info_lasexta+
    vote_recall,  data = murcia, link = "logit"
) |> 
  #marginaleffects() |> 
  tidy() |> as.data.frame() |> 
  mutate(model= "Vote recall model")



partyeval_olr <- clm(
  anti_lgbtflags_fac ~ gender + edu + age + size_town + work_situation +
    left_right + left_right_dad + left_right_mum +
    info_socialmedia + info_whatsapp + info_laverdad + info_laopinion + info_7tv + info_lasexta +
    vox_evaluation + pp_evaluation + psoe_evaluation + up_evaluation + cs_evaluation,
  data = murcia,
  link = "logit"
)|> 
  #marginaleffects() |> 
  tidy() |> as.data.frame() |> 
  mutate(model= "Party model")


candeval_olr <- clm(
  anti_lgbtflags_fac ~ gender + edu + age + size_town + work_situation +
    left_right + left_right_dad + left_right_mum +
    info_socialmedia + info_whatsapp + info_laverdad + info_laopinion + info_7tv + info_lasexta +
    abascal_evaluation + feijoo_evaluation + sanchez_evaluation + belarra_evaluation + arrimadas_evaluation, 
  
  data = murcia,
  link = "logit"
)|> 
  #marginaleffects() |> 
  tidy() |> as.data.frame() |> 
  mutate(model= "Candidate model")

all_ordinal <- reduce(list(voteint_olr, voterec_olr,
                         candeval_olr, partyeval_olr), full_join) |> 
  mutate(significance= case_when(
    p.value<0.001 ~ "***",
    p.value<0.01 ~ "**",
    p.value<0.05 ~ "*",
    p.value<0.1 ~ "."
  ),  term = case_when(term == "info_7tv" ~ "info_siete", TRUE ~ term))  %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) |> 
  filter(!grepl("agree", term, ignore.case = TRUE)) |> 
  separate(term, into = c("term", "contrast"), sep = "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)|(?<=\\D)(?=[A-Z])|(?<=[A-Z])(?=\\D)|(?<=\\D)(?=\\W)|(?<=\\W)(?=\\D)", extra = "merge")

#Figure 3

age_logit1 <-all_ordinal |> 
  mutate(contrast= factor(contrast, levels= c(">64",
                                              "45-64",
                                              "31-44")),
         model= factor(model, levels=c("Vote recall model", "Vote intention model",
                                       "Party model", "Candidate model"))) |> 
  filter(term %in% c("age")) |> 
  mutate(term= case_when(term=="age" ~"Age")) |> 
  ggplot(aes(y = term, fill= contrast)) +
  geom_vline(xintercept = 0, size = 0.5, linetype = "24", color = "grey") +
  stat_halfeye(position = "dodge", alpha=0.6,
               aes(xdist = dist_normal(estimate, std.error)))+
  theme_nhsr_demo() +
  scale_fill_manual(values=c("#064789", "#427aa1","#ebf2fa"), name="") +
  facet_grid(~model) +
  labs(x="", y="") +
  theme(legend.position = "top",
        legend.justification = "left",     # Justifies the legend to the left
        legend.box.just = "left")+ 
  theme(axis.text.y = element_blank(),    # This removes the x-axis labels
        axis.ticks.y = element_blank())



party_logit1 <- all_ordinal |> 
  filter(term %in% c("vote_intention", "vote_recall")) |> 
  mutate(contrast= factor(contrast, levels= c("Vox",
                                              "PP",
                                              "PSOE",
                                              "UP")),
         model= factor(model, levels=c("Vote recall model", "Vote intention model")),
         type= "Vote") |> 
  ggplot(aes(fill= contrast, shape= contrast)) +
  geom_vline(xintercept = 0, size = 1.5, linetype = "24", color = "orange") +
  scale_fill_manual(values=c( "#76CE86", "#7985E0", "#F29496", "#D291BC"), name="") +
  stat_halfeye(position = "dodge", alpha=0.6,
               aes(xdist = dist_normal(estimate, std.error)))+
  theme_nhsr_demo() +
  # scale_fill_discrete_sequential(name="") +
  labs(x="", y="") +
  theme(legend.position = "top",
        legend.justification = "left",     # Justifies the legend to the left
        legend.box.just = "left")+ 
  guides(shape=guide_legend(title="")) +
  facet_grid(~model) +
  theme(axis.text.y = element_blank(),    # This removes the x-axis labels
        axis.ticks.y = element_blank())




party_logit2 <- all_ordinal |> 
  filter(grepl("evaluation",term)) |> 
  separate(term, into = c("term", NA), sep = "_") |> 
  mutate(term= factor(case_when(term %in% c("vox", "abascal") ~ "Vox",
                                term %in% c("pp", "feijoo") ~ "PP",
                                term %in% c("psoe", "sanchez") ~ "PSOE",
                                term %in% c("cs", "arrimadas") ~ "Cs",
                                term %in% c("up", "belarra") ~ "UP"), 
                      levels=c("Vox", "PP", "Cs", "PSOE", "UP")),
         model= factor(model, levels=c("Vote recall model", "Vote intention model",
                                       "Party model", "Candidate model"))) |>
  ggplot(aes(fill= term, shape= term)) +
  geom_vline(xintercept = 0, size = 0.5, linetype = "24", color = "grey") +
  scale_fill_manual(values=c( "#76CE86", "#7985E0", "orange",  "#F29496", "#D291BC"), name="") +
  stat_halfeye(position = "dodge", alpha=0.6,
               aes(xdist = dist_normal(estimate, std.error)))+
  theme_nhsr_demo() + 
  labs(x="", y="") +

  theme(legend.position = "none")+ 
  
  guides(shape=guide_legend(title="")) +
  facet_grid(~model)+
  theme(axis.text.y = element_blank(),    # This removes the x-axis labels
        axis.ticks.y = element_blank())


library(patchwork)
age_logit1 / (party_logit1 | party_logit2)

ggsave("logit_flags.png", width =10, height =10)

#### 2. APPENDIX ####

#Figure A1

murcia_percentages <- murcia %>%
  pivot_longer(cols = c(vote_intention, vote_recall),
               names_to = "type",
               values_to = "party") %>%
  mutate(type= case_when(type== "vote_intention" ~ "Vote intention",
                         TRUE ~ "Vote recall"),
         party= factor(party, levels= c("UP", "PSOE", "Cs", "PP", "Vox")),
         anti_lgbtflags = factor(anti_lgbtflags, levels = c("4", "3", "2", "1"),
                                 labels = c("Strongly agree", "Agree", "Disagree", "Strongly disagree"))) %>%
  drop_na(party) %>%
  count(party, type, anti_lgbtflags) %>%
  group_by(party, type) %>%
  drop_na(anti_lgbtflags) |> 
  mutate(percentage = n / sum(n)) %>%
  ungroup()

ggplot(murcia_percentages, aes(x = party, y = percentage, fill = anti_lgbtflags)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~type, ncol=1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(fill = "Position on banning state-sponsored LGBTQ+ flags",
       y = "Percentage",
       x = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("#7F27FF", "#9F70FD", "#FDBF60", "#FF8911"))

ggsave("party_flags_rev.png", width =10, height =7)


#Figure A2

murcia_age_percentages <- murcia %>%
  drop_na(age, vote_intention) %>%
  mutate(
    anti_lgbtflags = factor(anti_lgbtflags, levels = c("4", "3", "2", "1"),
                            labels = c("Strongly agree", "Agree", "Disagree", "Strongly disagree"))) %>%
  drop_na(age) %>%
  count(age, anti_lgbtflags) %>%
  group_by(age) %>%
  drop_na(anti_lgbtflags) |> 
  mutate(percentage = n / sum(n)) %>%
  ungroup()


ggplot(murcia_age_percentages, aes(x = age, y = percentage, fill = anti_lgbtflags))  +
  geom_bar(stat = "identity", position = "fill") +
  #facet_wrap(~type, ncol=1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(fill = "Position on banning state-sponsored LGBTQ+ flags",
       y = "Percentage",
       x = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("#7F27FF", "#9F70FD", "#FDBF60", "#FF8911"))



ggsave("age_flags_rev.png", width =10, height =7)




#Table A2

all_ordinal_tb <- all_ordinal %>%
  mutate(estimate = paste0(round(estimate, 3), significance),
         Estimate = paste0(estimate, " (", round(std.error, 3), ")")) |> 
  dplyr::select(Model= model, Term= term, Contrast= contrast, Estimate, Statistic=statistic, `P value`= p.value)%>%
  dplyr::select(Term, Contrast, Model, Estimate) %>%
  pivot_wider(names_from = Model, values_from = Estimate)

# Now you have a formatted table with each model type as a column and P values as stars

write_csv2(all_ordinal_tb, "all_ordinal.csv")


voteint_performance <- model_performance(clm(
  anti_lgbtflags_fac ~ gender + edu + age + size_town + work_situation +
    left_right + left_right_dad + left_right_mum +
    info_socialmedia + info_whatsapp + info_laverdad + info_laopinion + info_7tv + info_lasexta +
    vote_intention,  data = murcia, link = "logit", 
), type="ordinal") |>  as.data.frame() |> mutate(Model= "Vote intention model")


voterec_performance <- model_performance(clm(
  anti_lgbtflags_fac ~ gender + edu + age + size_town + work_situation +
    left_right + left_right_dad + left_right_mum +
    info_socialmedia + info_whatsapp + info_laverdad + info_laopinion + info_7tv + info_lasexta +
    vote_recall,  data = murcia, link = "logit", 
), type="ordinal") |>  as.data.frame() |> mutate(Model= "Vote recall model")

#Table A3


partyeval_performance <- model_performance(clm(
  anti_lgbtflags_fac ~ gender + edu + age + size_town + work_situation +
    left_right + left_right_dad + left_right_mum +
    info_socialmedia + info_whatsapp + info_laverdad + info_laopinion + info_7tv + info_lasexta +
    vox_evaluation + pp_evaluation + psoe_evaluation + up_evaluation + cs_evaluation, 
  data = murcia,
  link = "logit"
)) |>  as.data.frame() |> mutate(Model= "Party evaluation model")

candeval_performance <- model_performance(clm(
  anti_lgbtflags_fac ~ gender + edu + age + size_town + work_situation +
    left_right + left_right_dad + left_right_mum +
    info_socialmedia + info_whatsapp + info_laverdad + info_laopinion + info_7tv + info_lasexta +
    abascal_evaluation + feijoo_evaluation + sanchez_evaluation + belarra_evaluation + arrimadas_evaluation, 
  data = murcia,
  link = "logit"
)) |>  as.data.frame() |> mutate(Model= "Candidate evaluation model")


all_performance <- reduce(list(voteint_performance, voterec_performance,
                               candeval_performance, partyeval_performance), full_join)

write_csv2(all_performance, "all_performance.csv")

