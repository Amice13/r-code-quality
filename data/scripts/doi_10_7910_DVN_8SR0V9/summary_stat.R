library(psych)
summ <- function(data,outfile){
    sdata <- data %>% 
        select(uniqueID,
               groupID,
               treat,
               Gender,
               returnyear,
               Region_bf_rcrt,
               Doc_region,
               bc_overall_rank,
               phd_overall_rank,
               Doctoral_graduation_year) %>% 
        distinct() %>% 
        mutate(female = as.numeric(Gender == "Female"),
               Bachelor_cnrank = as.numeric(bc_overall_rank),
               Doc_WorldRank = as.numeric(phd_overall_rank),
               B1 = as.numeric(Bachelor_cnrank <= 10),
               B2 = as.numeric(Bachelor_cnrank <= 50 & Bachelor_cnrank > 10),
               B3 = as.numeric(Bachelor_cnrank <= 100 & Bachelor_cnrank > 50),
               B4 = as.numeric(Bachelor_cnrank <= 200 & Bachelor_cnrank > 100),
               B5 = as.numeric(Bachelor_cnrank > 200),
               D1 = as.numeric(Doc_WorldRank <= 10),
               D2 = as.numeric(Doc_WorldRank <= 50 & Doc_WorldRank > 10),
               D3 = as.numeric(Doc_WorldRank <= 100 & Doc_WorldRank > 50),
               D4 = as.numeric(Doc_WorldRank <= 200 & Doc_WorldRank > 100),
               D5 = as.numeric(Doc_WorldRank > 200),
               US = as.numeric(Region_bf_rcrt %in% c("USA","Usa")),
               phdUS = as.numeric(Doc_region %in% c("USA","Usa")))
              
    sdata$B1[is.na(sdata$Bachelor_university)] <- NA
    sdata$B2[is.na(sdata$Bachelor_university)] <- NA
    sdata$B3[is.na(sdata$Bachelor_university)] <- NA
    sdata$B4[is.na(sdata$Bachelor_university)] <- NA
    sdata$B5[is.na(sdata$Bachelor_university)] <- NA
    sdata_1 <- filter(sdata,treat == 1)
    sdata_2 <- filter(sdata,treat == 0)
    summ1 <- rbind(cbind(data.frame(psych ::describe(sdata_1$female))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$female))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$Doctoral_graduation_year))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$Doctoral_graduation_year))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$US))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$US))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$phdUS))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$phdUS))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$B1))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$B1))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$B2))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$B2))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$B3))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$B3))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$B4))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$B4))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$B5))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$B5))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$D1))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$D1))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$D2))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$D2))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$D3))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$D3))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$D4))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$D4))[c(2,3,4,8,9)]),
                   cbind(data.frame(psych ::describe(sdata_1$D5))[c(2,3,4,8,9)],
                         data.frame(psych ::describe(sdata_2$D5))[c(2,3,4,8,9)]))
    row.names(summ1) <- c("Female",
                          "Year of PhD", 
                          "Recruited from U.S.",
                          "Ph.D. of U.S.",
                          "Rank of Bachelor Program (1-10)",
                          "Rank of Bachelor Program (11-50)",
                          "Rank of Bachelor Program (51-100)",
                          "Rank of Bachelor Program (101-200)",
                          "Rank of Bachelor Program (200+)",
                          "Rank of Doctoral Program (1-10)",
                          "Rank of Doctoral Program (11-50)",
                          "Rank of Doctoral Program (51-100)",
                          "Rank of Doctoral Program (101-200)",
                          "Rank of Doctoral Program (200+)")
    stargazer(summ1,summary = FALSE,type = "html",out = outfile)
}
summ_v <-  function(data,outfile){
    sdata <- data %>% 
        select(treat,post,
               num_of_pub,
               Q12_num,
               Q1_num,
               top10_num,
               FA_pub,
               FA_Q12pub,
               FA_Q1pub,
               FA_top10pub,
               LA_pub,
               LA_Q12pub,
               LA_Q1pub,
               LA_top10pub,
               grant,
               teamsize) %>% 
        mutate(grant_nlg = grant_nlg/1000,
               grant = grant/1000)
    sdata_1_1 <- filter(sdata,treat == 1,post == 0) %>% 
        select(-treat,-post)
    sdata_1_2 <- filter(sdata,treat == 1,post == 1) %>% 
        select(-treat,-post)
    
    sdata_2_1 <- filter(sdata,treat == 0,post == 0) %>% 
        select(-treat,-post)
    sdata_2_2 <- filter(sdata,treat == 0,post == 1) %>% 
        select(-treat,-post)
    
    summ <- cbind(data.frame(psych ::describe(sdata_1_1))[c(2,3,4,8,9)],
                  data.frame(psych ::describe(sdata_1_2))[c(2,3,4,8,9)],
                  data.frame(psych ::describe(sdata_2_1))[c(2,3,4,8,9)],
                  data.frame(psych ::describe(sdata_2_2))[c(2,3,4,8,9)])
    
    row.names(summ) <- c("All Articles","Articles in Q1 Q2", 
                         "Articles in Q1", "Articles in cem.csv",
                         "First-Authored Articles",
                         "First-Authored Articles in Q1 Q2",
                         "First-Authored Articles in Q1",
                         "First-Authored Articles in cem.csv",
                         "Last-Authored Articles",
                         "Last-Authored Articles in Q1 Q2",
                         "Last-Authored Articles in Q1",
                         "Last-Authored Articles in top 10%",
                         "Grant",
                         "Team Size")
    stargazer(summ,summary = FALSE,type = "html",out = outfile)
}
