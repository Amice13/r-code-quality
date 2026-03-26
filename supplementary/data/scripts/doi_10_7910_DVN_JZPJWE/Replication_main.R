# Script for replication of the article "Sensitivity bias in attitudes towards lowering the votinge age"
## load the following packages for the replication
library(tidyverse)
library(patchwork)

################################### Figure 1 ####################################

##### 1. Data loading #####
df2020 <- read.csv("data2020.csv", fileEncoding = "CP932") 
re_df2020 <-
  df2020 |>
  mutate(C_sum = case_when(List1_agree >= 0 ~ List1_agree + List1_disagree, TRUE ~ NA_real_), ## Outcome of Con. 
         T_sum = case_when(List2_agree >= 0 ~ List2_agree + List2_disagree, TRUE ~ NA_real_), ## Outcome of Treatment
         Satis = case_when(C_sum == 4 ~ 0, T_sum == 5 ~ 0, TRUE ~ 1), ## Satisficers
         Out = case_when(Satis == 0 & C_sum >= 0 ~ List1_agree,
                         Satis == 0 & T_sum >= 0 ~ List2_agree,
                         TRUE ~ NA_real_), ## Outcome 
         Group = case_when(Satis == 0 & C_sum >= 0 ~ "Control",
                           Satis == 0 & T_sum >= 0 ~ "Treatment",
                           TRUE ~ NA_character_) ## Con. or Treat
         ) |>
  filter(deb == 1 & Satis == 0) ## excluding survey satisficers

##### 2. Bootstrapping estimation #####
sim2020 <- tibble(ID = 1:10000) 

re_df2020 <-
  re_df2020 |> 
  mutate(Age = substr(re_df2020$p, 3, 4) |> as.numeric(),
         Age_G = case_when(Age < 30 ~ 1, Age < 60 ~ 2, TRUE ~ 3)) ## Make age groups

re_df2020_l <- re_df2020 |> filter(Age_G == 1) ### Young
re_df2020_m <- re_df2020 |> filter(Age_G == 2) ### Middle
re_df2020_h <- re_df2020 |> filter(Age_G == 3) ### Older

for(i in 1:nrow(sim2020)){
  set.seed(i)
  sample_l <- sample_n(re_df2020_l, size = nrow(re_df2020_l), replace = T)
  sample_m <- sample_n(re_df2020_m, size = nrow(re_df2020_m), replace = T)
  sample_h <- sample_n(re_df2020_h, size = nrow(re_df2020_h), replace = T)
  DQ_l <- table(sample_l$DQ)[2]/nrow(sample_l)
  DQ_m <- table(sample_m$DQ)[2]/nrow(sample_m)
  DQ_h <- table(sample_h$DQ)[2]/nrow(sample_h)
  Dif_l <- t.test(sample_l$Out[sample_l$Group == "Control"],sample_l$Out[sample_l$Group == "Treatment"])
  Dif_m <- t.test(sample_m$Out[sample_m$Group == "Control"],sample_m$Out[sample_m$Group == "Treatment"])
  Dif_h <- t.test(sample_h$Out[sample_h$Group == "Control"],sample_h$Out[sample_h$Group == "Treatment"])
  dif_l <- Dif_l[["estimate"]][["mean of y"]] - Dif_l[["estimate"]][["mean of x"]]
  dif_m <- Dif_m[["estimate"]][["mean of y"]] - Dif_m[["estimate"]][["mean of x"]]
  dif_h <- Dif_h[["estimate"]][["mean of y"]] - Dif_h[["estimate"]][["mean of x"]]
  sens_l <- dif_l - DQ_l
  sens_m <- dif_m - DQ_m
  sens_h <- dif_h - DQ_h
  sim2020$dq_l[i] <- DQ_l
  sim2020$Lis_l[i] <- dif_l
  sim2020$sens_l[i] <- sens_l
  sim2020$dq_m[i] <- DQ_m
  sim2020$Lis_m[i] <- dif_m
  sim2020$sens_m[i] <- sens_m
  sim2020$dq_h[i] <- DQ_h
  sim2020$Lis_h[i] <- dif_h
  sim2020$sens_h[i] <- sens_h
  sim2020$dif1[i] <- sens_l - sens_m
  sim2020$dif2[i] <- sens_l - sens_h
  }

##### 3. Make Fig. 1  #####
Fig1_df <- tibble(
  est = c(mean(sim2020$dq_l), mean(sim2020$Lis_l), mean(sim2020$sens_l),
          mean(sim2020$dq_m), mean(sim2020$Lis_m), mean(sim2020$sens_m),
          mean(sim2020$dq_h), mean(sim2020$Lis_h), mean(sim2020$sens_h)),
  se =  c(sd(sim2020$dq_l), sd(sim2020$Lis_l), sd(sim2020$sens_l),
          sd(sim2020$dq_m), sd(sim2020$Lis_m), sd(sim2020$sens_m),
          sd(sim2020$dq_h), sd(sim2020$Lis_h), sd(sim2020$sens_h)),
  label = rep(c("DQ", "LE", "LE - DQ \n(Sensitivity bias)"), times = 3),
  Group = rep(c("Young (18-29)", "Middle (30-59)", "Older (60+)"), each = 3),
  num = rep(1:3, times = 3)) |>
  mutate(Group = factor(Group, levels = c("Young (18-29)", "Middle (30-59)", "Older (60+)")))

Fig1 <-
  ggplot(Fig1_df, aes(x = reorder(label, num))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_segment(aes(y = est - 1.96*se, yend = est + 1.96*se,
                   x = reorder(label, num), xend = reorder(label, num))) +
  geom_errorbar(aes(ymin = est - 1.64*se, ymax = est + 1.64*se), width = 0.2) +
  geom_point(aes(y = est)) +
  labs(x = "", y = "Est.") +
  ylim(-0.1, 0.6) +
  facet_wrap(~Group, nrow = 1) +
  geom_text(aes( x = reorder(label, num), y = est + 1.96*se, 
                 label = sprintf("%.3f", est)), vjust = -1,
            size = 2.5) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),  
        panel.grid.minor.y = element_blank()) 

## If you'd like to see Fig1, please type print(Fig1) in the R console. 
## If you'd like to see Fig1, please use ggsave() in the tidyverse package
## We saved Fig1 in JPEG format. ggsave(plot = Fig1, dpi = 900, width = 7, height = 4, filename = "Fig1.jpg")

################################### Figure 2 ####################################

##### 1. Data loading #####
df2024 <- read.csv("data2024.csv", fileEncoding = "CP932") 
re_df2024 <-
  df2024 |>
  mutate(List1C_sum = case_when(List1C_agree >= 0 ~ List1C_agree + List1C_disagree, TRUE ~ NA_real_), ## List A out of Con.
         List1T_sum = case_when(List1T_agree >= 0 ~ List1T_agree + List1T_disagree, TRUE ~ NA_real_), ## List A out of Treat
         List2C_sum = case_when(List2C_agree >= 0 ~ List2C_agree + List2C_disagree, TRUE ~ NA_real_), ## List B out of Con.
         List2T_sum = case_when(List2T_agree >= 0 ~ List2T_agree + List2T_disagree, TRUE ~ NA_real_),  ## List B out of Treat
         Satis_A = case_when(List1C_sum == 4 ~ 0, List1T_sum == 5 ~ 0, TRUE ~ 1), ## Satisficers in List A
         Satis_B = case_when(List2C_sum == 4 ~ 0, List2T_sum == 5 ~ 0, TRUE ~ 1), ## Satisficers in List B
         Out_A = case_when(Satis_A == 0 & List1C_sum >= 0 & List1C_sum < 5 ~ List1C_agree,
                           Satis_A == 0 & List1T_sum >= 0 & List1T_sum < 6 ~ List1T_agree,
                           TRUE ~ NA_real_),
         Out_A = case_when(Out_A == 2.5 ~ NA_real_, TRUE ~ Out_A),
         Out_B = case_when(Satis_B == 0 & List2C_sum >= 0 & List2C_sum < 5 ~ List2C_agree,
                           Satis_B == 0 & List2T_sum >= 0 & List2T_sum < 6 ~ List2T_agree,
                           TRUE ~ NA_real_),
         Group_A = case_when(Satis_A == 0 & List1C_sum >= 0 & List1C_sum < 5 ~ "Cont",
                             Satis_A == 0 & List1T_sum >= 0 & List1T_sum < 6 ~ "Treat",
                             TRUE ~ NA_character_),
         Group_B = case_when(Satis_B == 0 & List2C_sum >= 0 & List2C_sum < 5 ~ "Cont",
                             Satis_B == 0 & List2T_sum >= 0 & List2T_sum < 6 ~ "Treat",
                             TRUE ~ NA_character_)) |>
  filter(deb == 1 & Satis_A == 0 & Satis_B == 0)

##### 2. Bootstrapping estimation #####
sim2024 <- tibble(ID = 1:10000) 

re_df2024 |> filter(Esteem_1 == 4 | Esteem_1 == 5) -> df2024_O1_h ## Esteem_1 is self-esteem scale(1), high group
re_df2024 |> filter(Esteem_1 == 1 | Esteem_1 == 2) -> df2024_O1_l ## Est.1, low group 
re_df2024 |> filter(Esteem_2 == 4 | Esteem_2 == 5) -> df2024_O2_h ## Esteem_2 is self-esteem scale(2), high group
re_df2024 |> filter(Esteem_2 == 1 | Esteem_2 == 2) -> df2024_O2_l ##  Est.2, low group

for(i in 1:nrow(sim2024)){
  set.seed(i)
  sO1_l <- sample_n(df2024_O1_l, size = nrow(df2024_O1_l), replace = T)
  sO1_h <- sample_n(df2024_O1_h, size = nrow(df2024_O1_h), replace = T)
  sO2_l <- sample_n(df2024_O2_l, size = nrow(df2024_O2_l), replace = T)
  sO2_h <- sample_n(df2024_O2_h, size = nrow(df2024_O2_h), replace = T)
  DQ_O1l <- table(sO1_l$DQ)[2]/nrow(sO1_l)
  DQ_O1h <- table(sO1_h$DQ)[2]/nrow(sO1_h)
  DQ_O2l <- table(sO2_l$DQ)[2]/nrow(sO2_l)
  DQ_O2h <- table(sO2_h$DQ)[2]/nrow(sO2_h)
  DifL1_O1l <- t.test(sO1_l$Out_A[sO1_l$Group_A=="Cont"],sO1_l$Out_A[sO1_l$Group_A=="Treat"])
  DifL2_O1l <- t.test(sO1_l$Out_B[sO1_l$Group_B=="Cont"],sO1_l$Out_B[sO1_l$Group_B=="Treat"])
  DifL1_O1h <- t.test(sO1_h$Out_A[sO1_h$Group_A=="Cont"],sO1_h$Out_A[sO1_h$Group_A=="Treat"])
  DifL2_O1h <- t.test(sO1_h$Out_B[sO1_h$Group_B=="Cont"],sO1_h$Out_B[sO1_h$Group_B=="Treat"])
  DifL1_O2l <- t.test(sO2_l$Out_A[sO2_l$Group_A=="Cont"],sO2_l$Out_A[sO2_l$Group_A=="Treat"])
  DifL2_O2l <- t.test(sO2_l$Out_B[sO2_l$Group_B=="Cont"],sO2_l$Out_B[sO2_l$Group_B=="Treat"])
  DifL1_O2h <- t.test(sO2_h$Out_A[sO2_h$Group_A=="Cont"],sO2_h$Out_A[sO2_h$Group_A=="Treat"])
  DifL2_O2h <- t.test(sO2_h$Out_B[sO2_h$Group_B=="Cont"],sO2_h$Out_B[sO2_h$Group_B=="Treat"])
  mean_dif_L1_O1l <- DifL1_O1l[["estimate"]][["mean of y"]] - DifL1_O1l[["estimate"]][["mean of x"]]
  mean_dif_L2_O1l <- DifL2_O1l[["estimate"]][["mean of y"]] - DifL2_O1l[["estimate"]][["mean of x"]]
  est_O1ml <- (mean_dif_L1_O1l + mean_dif_L2_O1l)/2
  mean_dif_L1_O1h <- DifL1_O1h[["estimate"]][["mean of y"]] - DifL1_O1h[["estimate"]][["mean of x"]]
  mean_dif_L2_O1h <- DifL2_O1h[["estimate"]][["mean of y"]] - DifL2_O1h[["estimate"]][["mean of x"]]
  est_O1mh <- (mean_dif_L1_O1h + mean_dif_L2_O1h)/2
  mean_dif_L1_O2l <- DifL1_O2l[["estimate"]][["mean of y"]] - DifL1_O2l[["estimate"]][["mean of x"]]
  mean_dif_L2_O2l <- DifL2_O2l[["estimate"]][["mean of y"]] - DifL2_O2l[["estimate"]][["mean of x"]]
  est_O2ml <- (mean_dif_L1_O2l + mean_dif_L2_O2l)/2
  mean_dif_L1_O2h <- DifL1_O2h[["estimate"]][["mean of y"]] - DifL1_O2h[["estimate"]][["mean of x"]]
  mean_dif_L2_O2h <- DifL2_O2h[["estimate"]][["mean of y"]] - DifL2_O2h[["estimate"]][["mean of x"]]
  est_O2mh <- (mean_dif_L1_O2h + mean_dif_L2_O2h)/2
  sens_O1l <- est_O1ml - DQ_O1l
  sens_O1h <- est_O1mh - DQ_O1h
  sens_O2l <- est_O2ml - DQ_O2l
  sens_O2h <- est_O2mh - DQ_O2h
  sim2024$dq_O1l[i] <- DQ_O1l
  sim2024$L1_O1l[i] <- mean_dif_L1_O1l
  sim2024$L2_O1l[i] <- mean_dif_L2_O1l
  sim2024$est_O1l[i] <- est_O1ml
  sim2024$sens_O1l[i] <- sens_O1l
  sim2024$dq_O1h[i] <- DQ_O1h
  sim2024$L1_O1h[i] <- mean_dif_L1_O1h
  sim2024$L2_O1h[i] <- mean_dif_L2_O1h
  sim2024$est_O1h[i] <- est_O1mh
  sim2024$sens_O1h[i] <- sens_O1h
  sim2024$difO1[i] <-  sens_O1l - sens_O1h
  sim2024$dq_O2l[i] <- DQ_O2l
  sim2024$L1_O2l[i] <- mean_dif_L1_O2l
  sim2024$L2_O2l[i] <- mean_dif_L2_O2l
  sim2024$est_O2l[i] <- est_O2ml
  sim2024$sens_O2l[i] <- sens_O2l
  sim2024$dq_O2h[i] <- DQ_O2h
  sim2024$L1_O2h[i] <- mean_dif_L1_O2h
  sim2024$L2_O2h[i] <- mean_dif_L2_O2h
  sim2024$est_O2h[i] <- est_O2mh
  sim2024$sens_O2h[i] <- sens_O2h
  sim2024$difO2[i] <-  sens_O2l - sens_O2h
}

##### 3. Figure 2 #####
Fig2_1df <- tibble( ## Make a dataset for Fig2_1
  est = c(mean(sim2024$dq_O1l), mean(sim2024$est_O1l),  mean(sim2024$sens_O1l),
          mean(sim2024$dq_O1h), mean(sim2024$est_O1h), mean(sim2024$sens_O1h)),
  se = c(sd(sim2024$dq_O1l), sd(sim2024$est_O1l), sd(sim2024$sens_O1l),
         sd(sim2024$dq_O1h),  sd(sim2024$est_O1h), sd(sim2024$sens_O1h)),
  Group = c(rep("Low self-esteem", 3), rep("High self-esteem", 3)),
  label = c("DQ", "DLE", "DLE - DQ\n(Sensitivity bias)", "DQ", "DLE", "DLE - DQ\n(Sensitivity bias)"),
  num = 1:6) |>
  mutate(Group = factor(Group, levels = c("Low self-esteem", "High self-esteem")))

Fig2_1 <-  ## Make Fig2.1 (self-esteem scale 1)
  ggplot(Fig2_1df, aes(x = reorder(label, num))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_segment(aes(y = est - 1.96*se, yend = est + 1.96*se,
                   x = reorder(label, -num), xend = reorder(label, -num))) +
  geom_errorbar(aes(ymin = est - 1.64*se, ymax = est + 1.64*se), width = 0.2) +
  geom_point(aes(y = est)) +
  coord_flip() +
  labs(x = "", y = "Est.", title = "Opinion 1: I consider myself an adult") +
  ylim(-0.2, 0.6) +
  facet_wrap(~Group, scales = "free_x") +
  geom_text(aes( x = reorder(label, -num), 
                 y = est + 1.96*se, 
                 label = sprintf("%.3f", est)),
            vjust = -1,
            size = 2.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),  
        panel.grid.minor.x = element_blank()) 

Fig2_2df <- tibble( ## Make a dataset for Fig2_1
  est = c(mean(sim2024$dq_O2l), mean(sim2024$est_O2l),  mean(sim2024$sens_O2l),
          mean(sim2024$dq_O2h), mean(sim2024$est_O2h), mean(sim2024$sens_O2h)),
  se = c(sd(sim2024$dq_O2l), sd(sim2024$est_O2l), sd(sim2024$sens_O2l),
         sd(sim2024$dq_O2h),  sd(sim2024$est_O2h), sd(sim2024$sens_O2h)),
  Group = c(rep("Low self-esteem", 3), rep("High self-esteem", 3)),
  label = c("DQ", "DLE", "DLE - DQ\n(Sensitivity bias)", "DQ", "DLE", "DLE - DQ\n(Sensitivity bias)"),
  num = 1:6) |>
  mutate(Group = factor(Group, levels = c("Low self-esteem", "High self-esteem")))

Fig2_2 <-  ## Make Fig2.2 (self-esteem scale 2)
  ggplot(Fig2_2df, aes(x = reorder(label, num))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_segment(aes(y = est - 1.96*se, yend = est + 1.96*se,
                   x = reorder(label, -num), xend = reorder(label, -num))) +
  geom_errorbar(aes(ymin = est - 1.64*se, ymax = est + 1.64*se), width = 0.2) +
  geom_point(aes(y = est)) +
  coord_flip() +
  labs(x = "", y = "Est.", title = "Opinion 2: I think of myself as a responsible member of society") +
  ylim(-0.2, 0.6) +
  facet_wrap(~Group, scales = "free_x") +
  geom_text(aes( x = reorder(label, -num), 
                 y = est + 1.96*se, 
                 label = sprintf("%.3f", est)),
            vjust = -1,
            size = 2.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),  
        panel.grid.minor.x = element_blank()) 

Fig2 <- Fig2_1 + Fig2_2 + plot_layout(nrow = 2) ## Combine Fig2_1 and Fig2_2 to make Fig2

## If you'd like to see Fig2, please type print(Fig2) in the R console. 
## We saved Fig2 in JPEG format. ggsave(plot = Fig2, filename = "Fig2.jpeg", dpi = 900, width = 7, height = 5)

################################### Figure 3 ####################################

##### 1. Bootstrapping estimation #####

re_df2024 |> filter(Efficacy_1 > 2) -> df2024_E1_h  ## Efficacy_1 is political efficacy scale(1), high group 
re_df2024 |> filter(Efficacy_1 < 3) -> df2024_E1_l  ## Eff.1, high group 
re_df2024 |> filter(Efficacy_2 > 2) -> df2024_E2_h  ## Efficacy_2 is political efficacy scale(2), high group 
re_df2024 |> filter(Efficacy_2 < 3) -> df2024_E2_l  ##  Eff.2, low group 

for(i in 1:nrow(sim2024)){
  set.seed(i)
  sE1_l <- sample_n(df2024_E1_l, size = nrow(df2024_E1_l), replace = T)
  sE1_h <- sample_n(df2024_E1_h, size = nrow(df2024_E1_h), replace = T)
  sE2_l <- sample_n(df2024_E2_l, size = nrow(df2024_E2_l), replace = T)
  sE2_h <- sample_n(df2024_E2_h, size = nrow(df2024_E2_h), replace = T)
  DQ_E1l <- table(sE1_l$DQ)[2]/nrow(sE1_l)
  DQ_E1h <- table(sE1_h$DQ)[2]/nrow(sE1_h)
  DQ_E2l <- table(sE2_l$DQ)[2]/nrow(sE2_l)
  DQ_E2h <- table(sE2_h$DQ)[2]/nrow(sE2_h)
  DifL1_E1l <- t.test(sE1_l$Out_A[sE1_l$Group_A=="Cont"],sE1_l$Out_A[sE1_l$Group_A=="Treat"])
  DifL2_E1l <- t.test(sE1_l$Out_B[sE1_l$Group_B=="Cont"],sE1_l$Out_B[sE1_l$Group_B=="Treat"])
  DifL1_E1h <- t.test(sE1_h$Out_A[sE1_h$Group_A=="Cont"],sE1_h$Out_A[sE1_h$Group_A=="Treat"])
  DifL2_E1h <- t.test(sE1_h$Out_B[sE1_h$Group_B=="Cont"],sE1_h$Out_B[sE1_h$Group_B=="Treat"])
  DifL1_E2l <- t.test(sE2_l$Out_A[sE2_l$Group_A=="Cont"],sE2_l$Out_A[sE2_l$Group_A=="Treat"])
  DifL2_E2l <- t.test(sE2_l$Out_B[sE2_l$Group_B=="Cont"],sE2_l$Out_B[sE2_l$Group_B=="Treat"])
  DifL1_E2h <- t.test(sE2_h$Out_A[sE2_h$Group_A=="Cont"],sE2_h$Out_A[sE2_h$Group_A=="Treat"])
  DifL2_E2h <- t.test(sE2_h$Out_B[sE2_h$Group_B=="Cont"],sE2_h$Out_B[sE2_h$Group_B=="Treat"])
  mean_dif_L1_E1l <- DifL1_E1l[["estimate"]][["mean of y"]] - DifL1_E1l[["estimate"]][["mean of x"]]
  mean_dif_L2_E1l <- DifL2_E1l[["estimate"]][["mean of y"]] - DifL2_E1l[["estimate"]][["mean of x"]]
  est_E1ml <- (mean_dif_L1_E1l + mean_dif_L2_E1l)/2
  mean_dif_L1_E1h <- DifL1_E1h[["estimate"]][["mean of y"]] - DifL1_E1h[["estimate"]][["mean of x"]]
  mean_dif_L2_E1h <- DifL2_E1h[["estimate"]][["mean of y"]] - DifL2_E1h[["estimate"]][["mean of x"]]
  est_E1mh <- (mean_dif_L1_E1h + mean_dif_L2_E1h)/2
  mean_dif_L1_E2l <- DifL1_E2l[["estimate"]][["mean of y"]] - DifL1_E2l[["estimate"]][["mean of x"]]
  mean_dif_L2_E2l <- DifL2_E2l[["estimate"]][["mean of y"]] - DifL2_E2l[["estimate"]][["mean of x"]]
  est_E2ml <- (mean_dif_L1_E2l + mean_dif_L2_E2l)/2
  mean_dif_L1_E2h <- DifL1_E2h[["estimate"]][["mean of y"]] - DifL1_E2h[["estimate"]][["mean of x"]]
  mean_dif_L2_E2h <- DifL2_E2h[["estimate"]][["mean of y"]] - DifL2_E2h[["estimate"]][["mean of x"]]
  est_E2mh <- (mean_dif_L1_E2h + mean_dif_L2_E2h)/2
  sens_E1l <- est_E1ml - DQ_E1l
  sens_E1h <- est_E1mh - DQ_E1h
  sens_E2l <- est_E2ml - DQ_E2l
  sens_E2h <- est_E2mh - DQ_E2h
  sim2024$dq_E1l[i] <- DQ_E1l
  sim2024$L1_E1l[i] <- mean_dif_L1_E1l
  sim2024$L2_E1l[i] <- mean_dif_L2_E1l
  sim2024$est_E1l[i] <- est_E1ml
  sim2024$sens_E1l[i] <- sens_E1l
  sim2024$dq_E1h[i] <- DQ_E1h
  sim2024$L1_E1h[i] <- mean_dif_L1_E1h
  sim2024$L2_E1h[i] <- mean_dif_L2_E1h
  sim2024$est_E1h[i] <- est_E1mh
  sim2024$sens_E1h[i] <- sens_E1h
  sim2024$difE1[i] <-  sens_E1l - sens_E1h
  sim2024$dq_E2l[i] <- DQ_E2l
  sim2024$L1_E2l[i] <- mean_dif_L1_E2l
  sim2024$L2_E2l[i] <- mean_dif_L2_E2l
  sim2024$est_E2l[i] <- est_E2ml
  sim2024$sens_E2l[i] <- sens_E2l
  sim2024$dq_E2h[i] <- DQ_E2h
  sim2024$L1_E2h[i] <- mean_dif_L1_E2h
  sim2024$L2_E2h[i] <- mean_dif_L2_E2h
  sim2024$est_E2h[i] <- est_E2mh
  sim2024$sens_E2h[i] <- sens_E2h
  sim2024$difE2[i] <-  sens_E2l - sens_E2h
}
##### 2. Figure 3  #####
Fig3_1df <- tibble(
  est = c(mean(sim2024$dq_E1l), mean(sim2024$est_E1l),  mean(sim2024$sens_E1l),
          mean(sim2024$dq_E1h), mean(sim2024$est_E1h), mean(sim2024$sens_E1h)),
  se = c(sd(sim2024$dq_E1l), sd(sim2024$est_E1l), sd(sim2024$sens_E1l),
         sd(sim2024$dq_E1h),  sd(sim2024$est_E1h), sd(sim2024$sens_E1h)),
  Group = c(rep("Low internal efficacy", 3), rep("High internal efficacy", 3)),
  label = c("DQ", "DLE", "DLE - DQ\n(Sensitivity bias)", "DQ", "DLE", "DLE - DQ\n(Sensitivity bias)"),
  num = 1:6) |>
  mutate(Group = factor(Group, levels = c("Low internal efficacy", "High internal efficacy")))

Fig3_1 <-
  ggplot(Fig3_1df, aes(x = reorder(label, num))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_segment(aes(y = est - 1.96*se, yend = est + 1.96*se,
                   x = reorder(label, -num), xend = reorder(label, -num))) +
  geom_errorbar(aes(ymin = est - 1.64*se, ymax = est + 1.64*se), width = 0.2) +
  geom_point(aes(y = est)) +
  coord_flip() +
  labs(x = "", y = "Est.", title = "Efficacy 1: I believe I have no power to influence \n what the government does") +
  ylim(-0.2, 0.6) +
  facet_wrap(~Group, scales = "free_x") +
  geom_text(aes( x = reorder(label, -num), 
                 y = est + 1.96*se, 
                 label = sprintf("%.3f", est)),
            vjust = -1,
            size = 2.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),  
        panel.grid.minor.x = element_blank()) 

Fig3_2df <- tibble(
  est = c(mean(sim2024$dq_E2l), mean(sim2024$est_E2l),  mean(sim2024$sens_E2l),
          mean(sim2024$dq_E2h), mean(sim2024$est_E2h), mean(sim2024$sens_E2h)),
  se = c(sd(sim2024$dq_E2l), sd(sim2024$est_E2l), sd(sim2024$sens_E2l),
         sd(sim2024$dq_E2h),  sd(sim2024$est_E2h), sd(sim2024$sens_E2h)),
  Group = c(rep("Low internal efficacy", 3), rep("High internal efficacy", 3)),
  label = c("DQ", "DLE", "DLE - DQ\n(Sensitivity bias)", "DQ", "DLE", "DLE - DQ\n(Sensitivity bias)"),
  num = 1:6) |>
  mutate(Group = factor(Group, levels = c("Low internal efficacy", "High internal efficacy")))

Fig3_2 <-
  ggplot(Fig3_2df, aes(x = reorder(label, num))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_segment(aes(y = est - 1.96*se, yend = est + 1.96*se,
                   x = reorder(label, -num), xend = reorder(label, -num))) +
  geom_errorbar(aes(ymin = est - 1.64*se, ymax = est + 1.64*se), width = 0.2) +
  geom_point(aes(y = est)) +
  coord_flip() +
  labs(x = "", y = "Est.", title = "Efficacy 2: Sometimes politics and government seem so complicated \n that a person like me can’t really understand what’s going on.") +
  ylim(-0.2, 0.6) +
  facet_wrap(~Group, scales = "free_x") +
  geom_text(aes( x = reorder(label, -num), 
                 y = est + 1.96*se, 
                 label = sprintf("%.3f", est)),
            vjust = -1,
            size = 2.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),  
        panel.grid.minor.x = element_blank()) 


Fig3 <- Fig3_1 + Fig3_2 + plot_layout(nrow = 2)
## ggsave(plot = Fig3, filename = "Fig3.eps", dpi = 900, width = 7, height = 5)