library(magrittr)
library(fastDummies)
library(ebal)

# load CES surveys
CES_2016 <- rio::import("2016_CES/CCES16_Common_OUTPUT_Feb2018_VV.dta")
CES_2020 <- rio::import("2020_CES/CES20_Common_OUTPUT_vv.dta")
CES_2022 <- rio::import("2022_CES/CCES22_Common_OUTPUT_vv_topost.dta", encoding="UTF-8")

# load ACS weights
ACS_2012 <- rio::import("weights/ACS_2012_margins.csv")
ACS_2016 <- rio::import("weights/ACS_2016_margins.csv")
ACS_2019 <- rio::import("weights/ACS_2019_margins.csv")

# 2016 CES 

# filter out non-Latinos
CES_2016 %<>%
  filter(race == 3 | hispanic == 1) 

# recode variables in 2016 CES to work with ACS margin format
CES_2016 %>%
  transmute(V101,
            weight = commonweight,
            age_group = plyr::mapvalues(cut(2016 - birthyr,
                                            c(18, 35, 55, 100),
                                            include.lowest = F,
                                            right = F), c("[18,35)", "[35,55)", "[55,100)"),
                                        c("age1834", "age3554", "age55plus")),
            
            age1834 = as.numeric(age_group == "age1834"),
            age3554 = as.numeric(age_group == "age3554"),
            age55plus = as.numeric(age_group == "age55plus"),
            SEX = as.numeric(gender==2),
            ForeignBorn = as.numeric(immstat < 3),
            Education = plyr::mapvalues(educ, 
                                        c(1:6, 8, 9),
                                        c("NoHS", "HSOnly", "SomeColl",
                                          "Coll2yr", "Coll4yr", "PostGrad",
                                          NA, NA)),
            
            NoHS = as.numeric(Education == "NoHS"),
            HSOnly = as.numeric(Education == "HSOnly"),
            SomeColl = as.numeric(Education == "SomeColl"),
            Coll2yr = as.numeric(Education == "Coll2yr"),
            Coll4yr = as.numeric(Education == "Coll4yr"),
            PostGrad = as.numeric(Education == "PostGrad"),
            
            Mex = CES_2016 %>% 
              dplyr::select(contains("Hispanic_origin")) %>% 
              unite("hisp") %>% 
              transmute(Mex = as.numeric(str_sub(as.numeric(gsub("_|NA", "", hisp)), 3, 3) == 1)) %>%
              dplyr::pull(Mex),
            
            Cub = CES_2016 %>% 
              dplyr::select(contains("Hispanic_origin")) %>% 
              unite("hisp") %>% 
              transmute(Cub = as.numeric(str_sub(as.numeric(gsub("_|NA", "", hisp)), 4, 4) == 1)) %>%
              dplyr::pull(Cub),
            
            PR = CES_2016 %>% 
              dplyr::select(contains("Hispanic_origin")) %>% 
              unite("hisp") %>% 
              transmute(PR = as.numeric(str_sub(as.numeric(gsub("_|NA", "", hisp)), 5, 5) == 1)) %>%
              dplyr::pull(PR),
            
            Oth = CES_2016 %>% dplyr::select(contains("Hispanic_origin")) %>% 
              unite("hisp") %>% 
              transmute(hisp_origin = as.numeric(gsub("2", "0", 
                                                      str_sub(as.numeric(gsub("_|NA", "", hisp)), 4, 12)))) %>% 
              transmute(Oth = as.numeric(hisp_origin > 0)) %>%
              dplyr::pull(Oth),
            
            State = plyr::mapvalues(inputstate, 
                                    sjlabelled::get_values(inputstate), 
                                    sjlabelled::get_labels(inputstate)),
            State = state.abb[match(State,state.name)],
            sample = 0) %>% dplyr::select(-age_group, -Education, State) -> CES_2016_recodes

CES_2016_recodes %>%
  fastDummies::dummy_cols(., select_columns = "State") %>%
  set_names(~ str_replace_all(., "State_", "")) %>% 
  as_tibble() -> CES_2016_recodes

CES_2016_recodes %>%
  bind_rows(ACS_2016 %>% slice(rep(1:n(), each = 20)) %>% bind_cols(weight = 1)) -> CES_ACS 

CES_ACS %<>%
  mutate(Mex = replace_na(Mex, 0),
         Cub = replace_na(Cub, 0),
         PR = replace_na(PR, 0),
         Oth = replace_na(Oth, 0))

CES_ACS <- CES_ACS[,-which(names(CES_ACS) == "NA")]

CES_na <- na.omit(CES_ACS[,1:68] %>% dplyr::select(-State, -Oth, -AK,
                                                   -NoHS, -age1834))

X <- as.matrix(CES_na %>% dplyr::select(-sample, -weight, -V101))
S <- as.matrix(CES_na[,"sample"])

eb.out <- ebalance(Treatment = S, X = X)

bind_rows(apply(X[S == 0,], 2, weighted.mean, w=eb.out$w) - apply(X[S == 1,], 2, mean),
          apply(X[S == 0,], 2, mean) - apply(X[S == 1,], 2, mean),
          apply(X[S == 0,], 2, weighted.mean, w=CES_na$weight[1:7423]) - apply(X[S == 1,], 2, mean))  %>%
  t() %>%
  as_tibble() %>%
  set_names(c("entropy", "unweighted", "CCES")) %>%
  gather(variable, estimate, entropy:CCES) %>%
  bind_cols(names = rep(names(CES_na %>% dplyr::select(-V101, -weight, -sample)), 3)) %>%
  ggplot(aes(x = forcats::fct_reorder(names, 
                                      estimate), y = estimate, color = variable)) +
  geom_point(size = 2) +
  theme_bw() +
  coord_flip() +
  theme(panel.grid.minor = element_blank()) +
  labs(y = "ACS Difference", x = "")

CES_na %>% 
  filter(sample == 0) %>%
  dplyr::select(V101) %>%
  bind_cols(eb_weights = eb.out$w) %>%
  write.csv(., "weights/CES_2016_LatWeights.csv")

# 2020 CES

CES_2020 %<>%
  filter(race == 3 | hispanic == 1) 

# recode variables in 2016 CES to work with ACS margin format
CES_2020 %>%
  transmute(caseid,
            weight = commonweight,
            age_group = plyr::mapvalues(cut(2016 - birthyr,
                                            c(18, 35, 55, 100),
                                            include.lowest = F,
                                            right = F), c("[18,35)", "[35,55)", "[55,100)"),
                                        c("age1834", "age3554", "age55plus")),
            
            age1834 = as.numeric(age_group == "age1834"),
            age3554 = as.numeric(age_group == "age3554"),
            age55plus = as.numeric(age_group == "age55plus"),
            SEX = as.numeric(gender==2),
            ForeignBorn = as.numeric(immstat < 3),
            Education = plyr::mapvalues(educ, 
                                        c(1:6, 8, 9),
                                        c("NoHS", "HSOnly", "SomeColl",
                                          "Coll2yr", "Coll4yr", "PostGrad",
                                          NA, NA)),
            
            NoHS = as.numeric(Education == "NoHS"),
            HSOnly = as.numeric(Education == "HSOnly"),
            SomeColl = as.numeric(Education == "SomeColl"),
            Coll2yr = as.numeric(Education == "Coll2yr"),
            Coll4yr = as.numeric(Education == "Coll4yr"),
            PostGrad = as.numeric(Education == "PostGrad"),
            
            Mex = as.numeric(CES_2020$CC20_hisp_3 == 1),
            
            Cub = as.numeric(CES_2020$CC20_hisp_4 == 1),
            
            PR = as.numeric(CES_2020$CC20_hisp_5 == 1),
            
            Oth = as.numeric(Mex == 0 & Cub == 0 & PR == 0),
            
            State = plyr::mapvalues(inputstate, 
                                    sjlabelled::get_values(inputstate), 
                                    sjlabelled::get_labels(inputstate)),
            State = state.abb[match(State,state.name)],
            sample = 0) %>% dplyr::select(-age_group, -Education, State) -> CES_2020_recodes

CES_2020_recodes %>%
  dummy_cols(., select_columns = "State") %>%
  set_names(~ str_replace_all(., "State_", "")) %>% 
  as_tibble() -> CES_2020_recodes

CES_2020_recodes %>%
  bind_rows(margins %>% slice(rep(1:n(), each = 20)) %>% bind_cols(weight = 1, 
                                                                   caseid = 1)) -> CES_ACS_2020

CES_ACS_2020 %<>%
  mutate(Mex = replace_na(Mex, 0),
         Cub = replace_na(Cub, 0),
         PR = replace_na(PR, 0),
         Oth = replace_na(Oth, 0))

CES_ACS_2020 <- CES_ACS_2020[,-which(names(CES_ACS_2020) == "NA")]

CES_na <- na.omit(CES_ACS_2020 %>% dplyr::select(-State, -Oth, -AK,
                                                   -NoHS, -age1834, -V101,
                                                 -DC))

X <- as.matrix(CES_na %>% dplyr::select(-sample, -weight, -caseid))
S <- as.matrix(CES_na[,"sample"])

eb.out <- ebalance(Treatment = S, X = X)

CES_na %>% 
  filter(sample == 0) %>%
  dplyr::select(caseid) %>%
  bind_cols(eb_weights = eb.out$w) %>%
  write.csv(., "CES_2020_LatWeights.csv")


# 2022 CES

CES_2022 %<>%
 filter(race == 3 | hispanic == 1) 

# recode variables in 2016 CES to work with ACS margin format
CES_2022 %>%
 transmute(caseid,
           weight = commonweight,
           age_group = plyr::mapvalues(cut(2016 - birthyr,
                                           c(18, 35, 55, 100),
                                           include.lowest = F,
                                           right = F), c("[18,35)", "[35,55)", "[55,100)"),
                                       c("age1834", "age3554", "age55plus")),
           
           age1834 = as.numeric(age_group == "age1834"),
           age3554 = as.numeric(age_group == "age3554"),
           age55plus = as.numeric(age_group == "age55plus"),
           
           SEX = as.numeric(gender4==2),
           
           ForeignBorn = as.numeric(immstat < 3),
           
           Education = plyr::mapvalues(educ, 
                                       c(1:6, 8, 9),
                                       c("NoHS", "HSOnly", "SomeColl",
                                         "Coll2yr", "Coll4yr", "PostGrad",
                                         NA, NA)),
           
           NoHS = as.numeric(Education == "NoHS"),
           HSOnly = as.numeric(Education == "HSOnly"),
           SomeColl = as.numeric(Education == "SomeColl"),
           Coll2yr = as.numeric(Education == "Coll2yr"),
           Coll4yr = as.numeric(Education == "Coll4yr"),
           PostGrad = as.numeric(Education == "PostGrad"),
           
           Mex = as.numeric(CES_2022$CC22_hisp_3 == 1),
           
           Cub = as.numeric(CES_2022$CC22_hisp_4 == 1),
           
           PR = as.numeric(CES_2022$CC22_hisp_5 == 1),
           
           Oth = as.numeric(Mex == 0 & Cub == 0 & PR == 0),
           
           State = plyr::mapvalues(inputstate, 
                                   sjlabelled::get_values(inputstate), 
                                   sjlabelled::get_labels(inputstate)),
           State = state.abb[match(State,state.name)],
           sample = 0) %>% dplyr::select(-age_group, -Education, State) -> CES_2022_recodes

CES_2022_recodes %>%
 dummy_cols(., select_columns = "State") %>%
 select(-State) -> CES_2022_recodes

names(CES_2022_recodes)[which(grepl("State", names(CES_2022_recodes)))] <- gsub("State_", "",
                  names(CES_2022_recodes)[which(grepl("State", names(CES_2022_recodes)))])

CES_2022_recodes %>%
 bind_rows(margins %>% slice(rep(1:n(), each = 20)) %>% bind_cols(weight = 1, 
                                                                  caseid = 1)) -> CES_ACS_2022

CES_ACS_2022 %<>%
 mutate(Mex = replace_na(Mex, 0),
        Cub = replace_na(Cub, 0),
        PR = replace_na(PR, 0),
        Oth = replace_na(Oth, 0))

CES_ACS_2022 <- CES_ACS_2022[,-which(names(CES_ACS_2022) == "NA")]

CES_na <- na.omit(CES_ACS_2022 %>% dplyr::select(-Oth, -AK,
                                                 -NoHS, -age1834, -V101,
                                                 -DC))

X <- as.matrix(CES_na %>% dplyr::select(-sample, -weight, -caseid))
S <- as.matrix(CES_na[,"sample"])

eb.out <- ebalance(Treatment = S, X = X)

CES_na %>% 
 filter(sample == 0) %>%
 dplyr::select(caseid) %>%
 bind_cols(eb_weights = eb.out$w) %>%
 write.csv(., "CES_2022_LatWeights.csv")

