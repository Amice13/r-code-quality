# LOAD DATA, using the R project or your own working directory -----------------------------------------------------------------

#Before starting create subfolders to save data, figures and tables. If you do not do that functions are not going to work to save it automatically in the folders.
#data/processed
#output/figures
#output/tables
#output/tables/baseline_results

library(tidyverse)
library(naniar)
library(readxl)


data <- read_excel("data.xlsx")%>%
  replace_with_na_all(~.x %in% c(96, 97, 98, 99, "N/A")) %>%
  mutate(
    id = CodPanelista)


# DATA WRANGLING ------------------------------------------------------------

#Create partisan groups for analysis
data<- data%>%
  mutate(
    antiptID=if_else(Q13==9, 1, 0, missing=0),
    antiptIDstrong=if_else(antiptID==1 & Q15==2,1,0, missing=0),
    ptID=if_else(Q10==9, 1, 0, missing=0),
    ptIDstrong=if_else(ptID==1 & Q15==1,1,0, missing=0),
    ptIDextreme=if_else(ptIDstrong==1 & Q11==1 | ptIDstrong==1 & Q11==2,1,0, missing=0),
    antiptIDextreme=if_else(antiptIDstrong==1 & Q14==1 | antiptIDstrong==1 & Q14==1,1,0, missing=0))


data <- data%>%
  mutate(ideo_group=if_else(ptID==1,"Petistas",
                            if_else(antiptID==1,"Antipetistas","Non-partisans")))%>%
  mutate(ideo_group_5=if_else(ptIDstrong==1&ptID==1,"petista.strong",
                              if_else(antiptIDstrong==1&antiptID==1,"antipetista.strong",ideo_group)))%>%
  mutate(ideo_group_7=if_else(ptIDextreme==1&ptIDstrong==1&ptID==1,"petista.extreme",
                              if_else(antiptIDextreme==1&antiptIDstrong==1&antiptID==1,"antipetista.extreme",ideo_group_5)))




data <- data%>%
  mutate(ideo_group_leaning=if_else(ideo_group=="Petistas","Petistas",
                                    if_else(ideo_group=="Antipetistas","Antipetistas",
                                            if_else(ideo_group=="Non-partisans" & Q15==1, "Non-partisans leaning PT",
                                                    if_else(ideo_group=="Non-partisans" & Q15==2, "Non-partisans leaning antiPT", "Pure Non-partisans")))))



data<-data%>%
  mutate(np_pt=ifelse(ideo_group_leaning=="Non-partisans leaning PT",1,0),
         np_antipt=ifelse(ideo_group_leaning=="Non-partisans leaning antiPT",1,0))




#Demographics and treatment variable

data <- data %>%
  mutate(
    treatment = factor(
      GRUPO,
      labels=c(
        "Journalistic",
        "Crude",
        "Control")),
    female = ifelse(panelistSex==2,1,0),
    gender = ifelse(panelistSex==1,"Male","Female"),
    region = factor(
      REGION,
      labels=c(
        "North",
        "North-East",
        "South-East",
        "South",
        "Center-East"
      )
    ),
    income = factor(
      Q33,
      labels=c(
        "From R$0 to R$1,254",
        "From R$1,255.00 to R$2,004",
        "From R$2,005 to R$8,640",
        "From R$8,641 to R$11,261",
        "More than R$11,262"
      )
    ),
    age = Q4,
    age_cat = factor(
      Q4R,
      labels=c(
        "16-24",
        "25-34",
        "35-44",
        "45-59",
        "60+"
      )
    ),
    state = factor(
      Q1,
      labels = c(
        "Acre",
        "Alagoas",
        "Amazonas",
        "Amapá",
        "Bahia",
        "Ceará",
        "Distrito Federal",
        "Espírito Santo",
        "Goiás",
        "Maranhão",
        "Minas Gerais",
        "Mato Grosso",
        "Mato Grosso do Sul",
        "Pará",
        "Paraíba",
        "Pernambuco",
        "Piauí",
        "Paraná",
        "Rio de Janeiro",
        "Rio Grande do Norte",
        "Rondônia",
        "Roraima",
        "Rio Grande do Sul",
        "Santa Catarina",
        "Sergipe",
        "São Paulo",
        "Tocantins"
        
      )
    ),
    intr_pol = factor(
      Q8,
      labels = c(
        "Extremamente interessado",
        "Muito interessado",
        "Moderadamente interessado",
        "Ligeiramente interessado",
        "Nada interessado"
        
      )
    ),
    
    intr_pol_dummy = ifelse(intr_pol=="Extremamente interessado" | intr_pol=="Muito interessado",1,0 ),
    partisan_identity = ifelse(Q9==1,1,0)
    ,
    less_liked_party = factor(
      Q13,
      labels = c(
        "MDB - Movimento Democrático Brasileiro",
        "PSDB - Partido da Social Democracia Brasileira",
        "NOVO",
        "Republicanos",
        "União Brasil",
        "PDT - Partido Democrático Trabalhista",
        "PL - Partido Liberal",
        "PSOL - Partido Socialismo e Liberdade",
        "PT - Partido dos Trabalhadores",
        "PSB - Partido Socialista Brasileiro",
        "PSD - Partido Social Democrático"
      )
    ),
    most_liked_party = factor(
      Q10,
      labels = c(
        "MDB - Movimento Democrático Brasileiro",
        "PSDB - Partido da Social Democracia Brasileira",
        "NOVO",
        "Republicanos",
        "União Brasil",
        "PDT - Partido Democrático Trabalhista",
        "PL - Partido Liberal",
        "PSOL - Partido Socialismo e Liberdade",
        "PT - Partido dos Trabalhadores",
        "PSB - Partido Socialista Brasileiro",
        "PSD - Partido Social Democrático"
      )
    ),
    edu_lvl = factor(
      Q32,
      labels = c(
        "Analfabeto / Primário incompleto (Analfabeto/ Até 3ª série Fundamental/ Até 3ª série 1º. Grau)",
        "Primário completo / Ginasial incompleto (Até 4ª série Fundamental / Até 4ª série 1º. Grau)",
        "Ginasial completo / Colegial incompleto (Fundamental completo/ 1º. Grau completo)",
        "Colegial completo / Superior incompleto (Médio completo/ 2º. Grau completo)",
        "Superior completo",
        "Pós-graduação",
        "Mestrado",
        "Doutorado"
        
      )
    ),
    duration_min = duration/60,
    coupons_given = QJUEGO,
    bolsa_familia_receiver = Q34,
    race = factor(
      Q35,
      labels = c(
        "Branca",
        "Parda",
        "Preta",
        "Amarela",
        "Indígena"
      )
    ),
    religion = factor(
      Q36,
      labels = c(
        "Católico",
        "Evangélico",
        "Crente sem religião",
        "Outra religião",
        "Agnóstico ou ateu"
      )
    ),
    volunteer = ifelse(Q37== 1,1,0),
    volunteer_type = Q38,
    vote_last_election = factor(
      Q39,
      labels = c(
        "Ciro Gomes",
        "Constituinte Eymael",
        "Felipe d'Avila",
        "Jair Bolsonaro",
        "Lula",
        "Léo Péricles",
        "Padre Kelmon",
        "Simone Tebet",
        "Sofia Manzano",
        "Soraya Thronicke",
        "Vera"
      )
    )
  )

#Attention level
checks <- c("Q16", "Q16_PLACEBO",
            "Q18", "Q18_PLACEBO",
            "Q20", "Q20_PLACEBO",
            "Q22", "Q22_PLACEBO",
            "Q24", "Q24_PLACEBO",
            "Q26", "Q26_PLACEBO")

data$excluded <- ifelse(rowSums(data[checks] != 1, na.rm = TRUE) > 0, 1, 0)



data<-data%>%
  mutate(high_attention=ifelse(rowSums(data[checks] != 1, na.rm = TRUE)== 0, 1, 0),
         mid_attention=ifelse(rowSums(data[checks] != 1, na.rm = TRUE)== 1 | rowSums(data[checks] != 1, na.rm = TRUE)== 2, 1, 0),
         low_attention=ifelse(rowSums(data[checks] != 1, na.rm = TRUE)== 3 | rowSums(data[checks] != 1, na.rm = TRUE)== 4, 1, 0),
         no_attention=ifelse(rowSums(data[checks] != 1, na.rm = TRUE)== 5 | rowSums(data[checks] != 1, na.rm = TRUE)== 6, 1, 0),
         attention_level=ifelse(high_attention==1,"High",
                                ifelse(mid_attention==1,"Mid",
                                       ifelse(low_attention==1,"Low",      
                                              "no_attention"))))


# Polarization DVs
data <- data %>%
  mutate(
    affective_polarization_voters = Q29_2 - Q29_1,
    affective_polarization_politicians = Q29_4 - Q29_3,
    affective_polarization_lula_bolsonaro=Q29_6-Q29_5)
    




# Labels:

Hmisc::label(data$id) <- 'Id'
Hmisc::label(data$treatment) <- 'Experimental group'
Hmisc::label(data$ideo_group) <- "Ideological group"
Hmisc::label(data$region) <- 'Region'
Hmisc::label(data$female) <- 'Female'
Hmisc::label(data$gender) <- 'Gender'
Hmisc::label(data$age) <- "Age"
Hmisc::label(data$age_cat) <- "Age"
Hmisc::label(data$income) <- "Income"
Hmisc::label(data$state) <- "State"
Hmisc::label(data$intr_pol) <- "Interest in politics"
Hmisc::label(data$edu_lvl) <- "Educational Level"
Hmisc::label(data$duration_min) <- "Survey duration (min)"
Hmisc::label(data$affective_polarization_voters) <- "Level of affective polarization (voters)"
Hmisc::label(data$affective_polarization_politicians) <- "Level of affective polarization (politicians)"
Hmisc::label(data$partisan_identity) <- "Partisan Identity"
Hmisc::label(data$vote_last_election) <- "Vote recall (last election)"
Hmisc::label(data$less_liked_party) <- "Less liked party"
Hmisc::label(data$most_liked_party) <- "Most liked party"
Hmisc::label(data$volunteer) <- "Was volunteer"
Hmisc::label(data$religion) <- "Religion"
Hmisc::label(data$bolsa_familia_receiver) <- "Bolsa Familia Receiver"
Hmisc::label(data$race) <- "Race"
Hmisc::label(data$edu_lvl) <- "Educational Level"
Hmisc::label(data$coupons_given) <- "Nr. of coupons given to outgroup"

# Target proportions based on the population distribution
target_props <- c(Petistas = 0.24, Antipetistas = 0.331, NonPartisans = 0.331)



# Calculate the observed proportions in the dataset
pj<-data%>%
  mutate(treatment2=ifelse(treatment=="Control","Control","Treatment"))%>%
  dplyr::group_by(ideo_group,treatment2)%>%
  dplyr::summarise(n=n())%>%
  dplyr::group_by(ideo_group)%>%
  mutate(total=sum(n))%>%
  filter(treatment2=="Treatment")%>%
  mutate(pj=n/total)%>%
  dplyr::select(ideo_group,pj,treatment2)


# Create inverse propensity scores
data<-data%>%
  left_join(pj, by=c("ideo_group"))%>%
  mutate(weight=ifelse(treatment2==1, 1/pj, 1/(1-pj)))


data$treatment<-relevel(data$treatment,ref="Control")

data %>%
  saveRDS("data/processed/data.rds")



data_specific_prompts_items <- data %>%
  dplyr::select(
    id,
    Q17,
    Q19,
    Q21,
    Q23,
    Q25,
    Q27,
    ideo_group,
    ideo_group_5,
    ideo_group_7,
    ideo_group_leaning,
    np_pt,
    np_antipt,
    attention_level,
    weight,
    treatment,
    excluded,
  )

data_specific_prompts <- data_specific_prompts_items %>%
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "prompt",
    values_to = "score",
    values_drop_na = TRUE
  )

data_general_rumors_items <- data %>%
  dplyr::select(
    id,
    Q28_1,
    Q28_2,
    Q28_3,
    Q28_4,
    Q28_5,
    Q28_6,
    ideo_group,
    ideo_group_5,
    ideo_group_7,
    attention_level,
    ideo_group_leaning,
    np_pt,
    np_antipt,
    weight,
    treatment,
    excluded
  )

data_general_rumors <- data_general_rumors_items %>%
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "prompt",
    values_to = "score",
    values_drop_na = TRUE
  )

data_polarization_attitudes_1_items <- data %>%
  mutate(Q30_1=ifelse(Q30_1==1 | Q30_1==2,1,0),
         Q30_2=ifelse(Q30_2==1 | Q30_2==2,1,0),
         Q30_3=ifelse(Q30_3==1 | Q30_3==2,1,0),
         Q30_4=ifelse(Q30_4==1 | Q30_4==2,1,0),
         Q30_5=ifelse(Q30_5==1 | Q30_5==2,1,0)) %>%
  dplyr::select(
    id,
    Q30_1:Q30_5,
    weight,
    ideo_group,
    ideo_group_5,
    ideo_group_7,
    ideo_group_leaning,
    np_pt,
    np_antipt,
    attention_level,
    treatment,
    excluded,
    intr_pol_dummy,
    vote_last_election,
    partido_jugador2,
    Q39
  )

data_polarization_attitudes_1 <- data_polarization_attitudes_1_items %>%
  pivot_longer(
    cols = starts_with("Q30"),
    names_to = "prompt",
    values_to = "score",
    values_drop_na = TRUE
  )


data_polarization_attitudes_2_items <- data %>%
  mutate(Q31_1=ifelse(Q31_1==4 | Q31_1==3,1,0),
         Q31_2=ifelse(Q31_2==4 | Q31_1==3 ,1,0),
         Q31_3=ifelse(Q31_3==1 | Q31_3==2 ,1,0),
         Q31_4=ifelse(Q31_4==4 | Q31_4==4 ,1,0))%>%
  dplyr::select(
    id,
    Q31_1:Q31_4,
    weight,
    ideo_group,
    ideo_group_5,
    ideo_group_7,
    ideo_group_leaning,
    np_pt,
    np_antipt,
    attention_level,
    treatment,
    excluded,
    intr_pol_dummy,
    vote_last_election,
    partido_jugador2,
    Q39
  )

data_polarization_attitudes_2 <- data_polarization_attitudes_2_items %>%
  pivot_longer(
    cols = starts_with("Q31"),
    names_to = "prompt",
    values_to = "score",
    values_drop_na = TRUE
  )


data_specific_prompts <- data_specific_prompts %>%
  rename(
    dv = score
  )

data_general_rumors <- data_general_rumors %>%
  rename(
    dv = score
  )

data_polarization_voters <- data %>%
  rename(
    dv = affective_polarization_voters
  )

data_polarization_politicians <- data %>%
  rename(
    dv = affective_polarization_politicians
  )

data_polarization_lula_bolsonaro<- data %>%
  rename(
    dv = affective_polarization_lula_bolsonaro
  )


data_polarization_attitudes_1 <- data_polarization_attitudes_1 %>%
  rename(
    dv = score
  )

data_polarization_attitudes_2 <- data_polarization_attitudes_2 %>%
  rename(
    dv = score
  )

data_polarization_game <- data %>%
  mutate(keep=10-QJUEGO,
         affective_polarization_game=keep-QJUEGO)

data_specific_prompts_items %>%
  saveRDS("data/processed/data_specific_prompts_items.rds")

data_specific_prompts %>%
  saveRDS("data/processed/data_specific_prompts.rds")

data_general_rumors_items %>%
  saveRDS("data/processed/data_general_rumors_items.rds")

data_general_rumors %>%
  saveRDS("data/processed/data_general_rumors.rds")

data_polarization_voters %>%
  saveRDS("data/processed/data_polarization_voters.rds")

data_polarization_politicians %>%
  saveRDS("data/processed/data_polarization_politicians.rds")

data_polarization_lula_bolsonaro %>%
  saveRDS("data/processed/data_polarization_lula_bolsonaro.rds")

data_polarization_attitudes_1_items %>%
  saveRDS("data/processed/data_polarization_attitudes_1_items.rds")

data_polarization_attitudes_1 %>%
  saveRDS("data/processed/data_polarization_attitudes_1.rds")

data_polarization_attitudes_2_items %>%
  saveRDS("data/processed/data_polarization_attitudes_2_items.rds")

data_polarization_attitudes_2 %>%
  saveRDS("data/processed/data_polarization_attitudes_2.rds")

data_polarization_game %>%
  saveRDS("data/processed/data_polarization_game.rds")