library(tidyverse)
library(list)
library(broom)

################################### A. Descriptive statistics ####################################
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
                           TRUE ~ NA_character_), ## Con. or Treat
         treat = if_else(Group == "Treatment", 1, 0)
  ) |>
  filter(deb == 1 & Satis == 0) ## excluding survey satisficers

sim2020_ap <- tibble(ID = 1:10000) 

for(i in 1:nrow(sim2020_ap)){
  set.seed(i)
  sample <- sample_n(re_df2020, size = nrow(re_df2020), replace = T)
  DQ <- table(sample$DQ)[2]/nrow(sample)
  Dif_L <- t.test(sample$Out[sample$Group == "Control"],sample$Out[sample$Group == "Treatment"])
  mean_dif <- Dif_L[["estimate"]][["mean of y"]] - Dif_L[["estimate"]][["mean of x"]]
  sens <- mean_dif - DQ
  sim2020_ap$dq[i] <- DQ
  sim2020_ap$List[i] <- mean_dif
  sim2020_ap$sens[i] <- sens
  }

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

re_df2024 <- re_df2024[complete.cases(re_df2024$Out_A), ]

sim2024_sp <- tibble(ID = 1:10000) 

for(i in 1:nrow(sim2024_sp)){
  set.seed(i)
  sample <- sample_n(re_df2024, size = nrow(re_df2024), replace = T)
  DQ <- table(sample$DQ)[2]/nrow(sample)
  DifL1 <- t.test(sample$Out_A[sample$Group_A=="Cont"],sample$Out_A[sample$Group_A=="Treat"])
  DifL2 <- t.test(sample$Out_B[sample$Group_B=="Cont"],sample$Out_B[sample$Group_B=="Treat"])
  mean_dif_L1 <- DifL1[["estimate"]][["mean of y"]] - DifL1[["estimate"]][["mean of x"]]
  mean_dif_L2 <- DifL2[["estimate"]][["mean of y"]] - DifL2[["estimate"]][["mean of x"]]
  est_m <- (mean_dif_L1 + mean_dif_L2)/2
  sens <- est_m - DQ
  sim2024_sp$dq[i] <- DQ
  sim2024_sp$L1[i] <- mean_dif_L1
  sim2024_sp$L2[i] <- mean_dif_L2
  sim2024_sp$est[i] <- est_m
  sim2024_sp$sens[i] <- sens
  sim2024_sp$dif_m[i] <-  mean_dif_L1 - mean_dif_L2
  }

##### Table A.1 #####

DQ_20 <- re_df2020$DQ |> table() |> as.data.frame() |> mutate(N = sum(Freq), Per = sprintf("%.1f", Freq/N*100))
LE_20 <- re_df2020 |> with(table(Out, Group)) |> as.data.frame() |> mutate(N = rep(c(2103,1998), each = 6), Per = sprintf("%.1f", Freq/N*100))

print(DQ_20) ## 1 = No (Disagree with the enfranchisement to the 18-year-olds)
print(LE_20)

sens_df20 <- 
  tibble(
    label = c("DQ", "List", "Sensitivity bias \n(LE - DQ)"),
    est = c(mean(sim2020_ap$dq), mean(sim2020_ap$List), mean(sim2020_ap$sens)),
    se = c(sd(sim2020_ap$dq), sd(sim2020_ap$List), sd(sim2020_ap$sens)),
    num = 1:3
  )

print(sens_df20)

##### Figure A.1 #####
Fig_A1 <-
sens_df20 |>
  ggplot(aes(x = reorder(label, -num))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_segment(aes(y = est - 1.96*se, yend = est + 1.96*se,
                   x = reorder(label, num), xend = reorder(label, num))) +
  geom_errorbar(aes(ymin = est - 1.64*se, ymax = est + 1.64*se), width = 0.1) +
  geom_point(aes(y = est)) +
  labs(x = "", y = "Est.") +
  geom_text(aes(x = reorder(label, -num), y = est, 
                 label = sprintf("%.3f", est)), hjust = -0.5,
            size = 2.5) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),  
        panel.grid.minor.y = element_blank()) 

print(Fig_A1) ## ggsave(plot = Fig_A1, filename = "FigA1.jpeg", dpi = 800, width = 5, height = 3)

##### Table A.2 #####

DQ_24 <- re_df2024$DQ |> table() |> as.data.frame() |> mutate(N = sum(Freq), Per = sprintf("%.1f", Freq/N*100))
LE_A_24 <- re_df2024 |> with(table(Out_A, Group_A)) |> as.data.frame() |> mutate(N = rep(c(566,552), each = 6), Per = sprintf("%.1f", Freq/N*100))
LE_B_24 <- re_df2024 |> with(table(Out_B, Group_B)) |> as.data.frame() |> mutate(N = rep(c(552,566), each = 6), Per = sprintf("%.1f", Freq/N*100))

print(DQ_24) ## 2 = No (Disagree with the enfranchisement to the 18-year-olds)
print(LE_A_24)
print(LE_B_24)

sens_df24 <- 
  tibble(
    label = c("DQ", "ListA", "ListB", "DLE","Sensitivity bias \n(DLE - DQ)"),
    est = c(mean(sim2024_sp$dq), mean(sim2024_sp$L1), mean(sim2024_sp$L2), mean(sim2024_sp$est), mean(sim2024_sp$sens)),
    se = c(sd(sim2024_sp$dq), sd(sim2024_sp$L1), sd(sim2024_sp$L2), sd(sim2024_sp$est), sd(sim2024_sp$sens)),
    num = 1:5
  )

print(sens_df24)

##### Figure A.2 #####

Fig_A2 <-
  sens_df24 |>
  ggplot(aes(x = reorder(label, num))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_segment(aes(y = est - 1.96*se, yend = est + 1.96*se,
                   x = reorder(label, num), xend = reorder(label, num))) +
  geom_errorbar(aes(ymin = est - 1.64*se, ymax = est + 1.64*se), width = 0.1) +
  geom_point(aes(y = est)) +
  labs(x = "", y = "Est.") +
  geom_text(aes(x = reorder(label, num), y = est + se*1.96, 
                label = sprintf("%.3f", est)), vjust = -0.5,
            size = 2.5) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),  
        panel.grid.minor.y = element_blank()) 

print(Fig_A2)
## ggsave(plot = Fig_A2, filename = "FigA2.jpeg", dpi = 800, width = 5, height = 3.5)

################################### C. Assumption check ###################################
re_df2020 <- re_df2020 |> mutate(treat = if_else(Group == "Treatment", 1, 0))
re_df2024 <- re_df2024 |> mutate(treatA = if_else(Group_A == "Treat", 1, 0), treatB = if_else(Group_B == "Treat", 1, 0))
check_20 <- ict.test(y = re_df2020$Out, treat = re_df2020$treat, J = 4)
re_df2024 <- re_df2024 |> mutate(treatA = if_else(Group_A == "Treat", 1, 0),treatB = if_else(Group_B == "Treat", 1, 0))
check_24_A <- ict.test(y = re_df2024$Out_A, treat = re_df2024$treatA, J = 4)
check_24_B <- ict.test(y = re_df2024$Out_B, treat = re_df2024$treatB, J = 4)

print(check_20)
print(check_24_A)
print(check_24_B)
################################### D. Subgroup Analyses ###################################

##### Table D.1 #####
sim2020_sub <- tibble(ID = 1:10000) 

re_df2020 |> filter(p < 200000) -> df2020_O1_a ## men
re_df2020 |> filter(p > 200000) -> df2020_O1_b ## women
re_df2020 |> filter(Edu == 4) -> df2020_O1_c ## graduate or undergraduate
re_df2020 |> filter(Edu != 4) -> df2020_O1_d ## non undergraduates

for(i in 1:nrow(sim2020_sub)){
  set.seed(i)
  s20a <- sample_n(df2020_O1_a, size = nrow(df2020_O1_a), replace = T)
  s20b <- sample_n(df2020_O1_b, size = nrow(df2020_O1_b), replace = T)
  s20c <- sample_n(df2020_O1_c, size = nrow(df2020_O1_c), replace = T)
  s20d <- sample_n(df2020_O1_d, size = nrow(df2020_O1_d), replace = T)
  
  DQa <- table(s20a$DQ)[2]/nrow(s20a)
  DQb <- table(s20b$DQ)[2]/nrow(s20b)
  DQc <- table(s20c$DQ)[2]/nrow(s20c)
  DQd <- table(s20d$DQ)[2]/nrow(s20d)
  
  DifLa <- t.test(s20a$Out[s20a$treat==0], s20a$Out[s20a$treat==1])
  DifLb <- t.test(s20b$Out[s20b$treat==0], s20b$Out[s20b$treat==1])
  DifLc <- t.test(s20c$Out[s20c$treat==0], s20c$Out[s20c$treat==1])
  DifLd <- t.test(s20d$Out[s20d$treat==0], s20d$Out[s20d$treat==1])
  
  est_a <- DifLa[["estimate"]][["mean of y"]] - DifLa[["estimate"]][["mean of x"]]
  est_b <- DifLb[["estimate"]][["mean of y"]] - DifLb[["estimate"]][["mean of x"]]
  est_c <- DifLc[["estimate"]][["mean of y"]] - DifLc[["estimate"]][["mean of x"]]
  est_d <- DifLd[["estimate"]][["mean of y"]] - DifLd[["estimate"]][["mean of x"]]
  
  sens_a <- est_a - DQa
  sens_b <- est_b - DQb
  sens_c <- est_c - DQc
  sens_d <- est_d - DQd
  
  dif_gen <- sens_a - sens_b
  dif_edu <- sens_c - sens_d
  
  sim2020_sub$dq_a[i] <- DQa
  sim2020_sub$dq_b[i] <- DQb
  sim2020_sub$dq_c[i] <- DQc
  sim2020_sub$dq_d[i] <- DQd
  
  sim2020_sub$list_a[i] <- est_a
  sim2020_sub$list_b[i] <- est_b
  sim2020_sub$list_c[i] <- est_c
  sim2020_sub$list_d[i] <- est_d
  
  sim2020_sub$sens_a[i] <- sens_a
  sim2020_sub$sens_b[i] <- sens_b
  sim2020_sub$sens_c[i] <- sens_c
  sim2020_sub$sens_d[i] <- sens_d
  
  sim2020_sub$dif_gen[i] <- dif_gen
  sim2020_sub$dif_edu[i] <- dif_edu
  sim2020_sub$dif_sb1[i] <- sens_a - sens_b
  sim2020_sub$dif_sb2[i] <- sens_c - sens_d
}

TableD1_df <- tibble(
  est = c(mean(sim2020_sub$dq_a), mean(sim2020_sub$dq_b), mean(sim2020_sub$dq_c), mean(sim2020_sub$dq_d),
          mean(sim2020_sub$list_a), mean(sim2020_sub$list_b), mean(sim2020_sub$list_c), mean(sim2020_sub$list_d),
          mean(sim2020_sub$sens_a), mean(sim2020_sub$sens_b), mean(sim2020_sub$sens_c), mean(sim2020_sub$sens_d)),
  se = c(sd(sim2020_sub$dq_a), sd(sim2020_sub$dq_b), sd(sim2020_sub$dq_c), sd(sim2020_sub$dq_d),
         sd(sim2020_sub$list_a), sd(sim2020_sub$list_b), sd(sim2020_sub$list_c), sd(sim2020_sub$list_d),
         sd(sim2020_sub$sens_a), sd(sim2020_sub$sens_b), sd(sim2020_sub$sens_c), sd(sim2020_sub$sens_d)),
  label = rep(c("DQ", "List", "Bias"), each = 4),
  Group = rep(c("Men", "Women", "Graduate or undergraduate", "non undergraduate"), times = 3),
  Split = rep(rep(c("Gender", "Educational levels"), each = 2), 3)
  )

TableD1_df_sb <- tibble(
  est = c(mean(sim2020_sub$dif_sb1), mean(sim2020_sub$dif_sb2)),
  se = c(sd(sim2020_sub$dif_sb1), sd(sim2020_sub$dif_sb2)),
  label = rep(c("Gender","Edu."))
  )

##### Table D.2 #####
sim2024_sub <- tibble(ID = 1:10000) 

re_df2024 |> filter(Knowledge == 4) -> df2024_O1_a ## correct answer for quiz on politics
re_df2024 |> filter(Knowledge != 4) -> df2024_O1_b ## incorrect or DK for quiz on politics
re_df2024 |> filter(Edu == 4) -> df2024_O1_c ## graduate or undergraduate
re_df2024 |> filter(Edu != 4) -> df2024_O1_d ## non undergraduates

for(i in 1:nrow(sim2024_sub)){
  set.seed(i)
  s24a <- sample_n(df2024_O1_a, size = nrow(df2024_O1_a), replace = T)
  s24b <- sample_n(df2024_O1_b, size = nrow(df2024_O1_b), replace = T)
  s24c <- sample_n(df2024_O1_c, size = nrow(df2024_O1_c), replace = T)
  s24d <- sample_n(df2024_O1_d, size = nrow(df2024_O1_d), replace = T)
  
  DQa <- table(s24a$DQ)[2]/nrow(s24a)
  DQb <- table(s24b$DQ)[2]/nrow(s24b)
  DQc <- table(s24c$DQ)[2]/nrow(s24c)
  DQd <- table(s24d$DQ)[2]/nrow(s24d)
  
  La_A <- t.test(s24a$Out_A[s24a$Group_A=="Cont"], s24a$Out_A[s24a$Group_A=="Treat"])
  Lb_A <-  t.test(s24b$Out_A[s24b$Group_A=="Cont"], s24b$Out_A[s24b$Group_A=="Treat"])
  Lc_A <-  t.test(s24c$Out_A[s24c$Group_A=="Cont"], s24c$Out_A[s24c$Group_A=="Treat"])
  Ld_A <-  t.test(s24d$Out_A[s24d$Group_A=="Cont"], s24d$Out_A[s24d$Group_A=="Treat"])
  
  La_B <- t.test(s24a$Out_B[s24a$Group_B=="Cont"], s24a$Out_B[s24a$Group_B=="Treat"])
  Lb_B <-  t.test(s24b$Out_B[s24b$Group_B=="Cont"], s24b$Out_B[s24b$Group_B=="Treat"])
  Lc_B <-  t.test(s24c$Out_B[s24c$Group_B=="Cont"], s24c$Out_B[s24c$Group_B=="Treat"])
  Ld_B <-  t.test(s24d$Out_B[s24d$Group_B=="Cont"], s24d$Out_B[s24d$Group_B=="Treat"])
  
  est_Aa <- La_A[["estimate"]][["mean of y"]] - La_A[["estimate"]][["mean of x"]]
  est_Ab <- Lb_A[["estimate"]][["mean of y"]] - Lb_A[["estimate"]][["mean of x"]]
  est_Ac <- Lc_A[["estimate"]][["mean of y"]] - Lc_A[["estimate"]][["mean of x"]]
  est_Ad <- Ld_A[["estimate"]][["mean of y"]] - Ld_A[["estimate"]][["mean of x"]]
  
  est_Ba <- La_B[["estimate"]][["mean of y"]] - La_B[["estimate"]][["mean of x"]]
  est_Bb <- Lb_B[["estimate"]][["mean of y"]] - Lb_B[["estimate"]][["mean of x"]]
  est_Bc <- Lc_B[["estimate"]][["mean of y"]] - Lc_B[["estimate"]][["mean of x"]]
  est_Bd <- Ld_B[["estimate"]][["mean of y"]] - Ld_B[["estimate"]][["mean of x"]]
  
  est_a <- (est_Aa + est_Ba) /2
  est_b <- (est_Ab + est_Bb) /2
  est_c <- (est_Ac + est_Bc) /2
  est_d <- (est_Ad + est_Bd) /2
  
  sens_a <- est_a - DQa
  sens_b <- est_b - DQb
  sens_c <- est_c - DQc
  sens_d <- est_d - DQd
  
  dif_know <- sens_a - sens_b
  dif_edu <- sens_c - sens_d
  
  sim2024_sub$dq_a[i] <- DQa
  sim2024_sub$dq_b[i] <- DQb
  sim2024_sub$dq_c[i] <- DQc
  sim2024_sub$dq_d[i] <- DQd
  
  sim2024_sub$list_a[i] <- est_a
  sim2024_sub$list_b[i] <- est_b
  sim2024_sub$list_c[i] <- est_c
  sim2024_sub$list_d[i] <- est_d
  
  sim2024_sub$sens_a[i] <- sens_a
  sim2024_sub$sens_b[i] <- sens_b
  sim2024_sub$sens_c[i] <- sens_c
  sim2024_sub$sens_d[i] <- sens_d
  
  sim2024_sub$dif_know[i] <- dif_know
  sim2024_sub$dif_edu[i] <- dif_edu
  }

TableD2_df <- tibble(
  est = c(mean(sim2024_sub$dq_a), mean(sim2024_sub$dq_b), mean(sim2024_sub$dq_c), mean(sim2024_sub$dq_d),
          mean(sim2024_sub$list_a), mean(sim2024_sub$list_b), mean(sim2024_sub$list_c), mean(sim2024_sub$list_d),
          mean(sim2024_sub$sens_a), mean(sim2024_sub$sens_b), mean(sim2024_sub$sens_c), mean(sim2024_sub$sens_d)),
  se = c(sd(sim2024_sub$dq_a), sd(sim2024_sub$dq_b), sd(sim2024_sub$dq_c), sd(sim2024_sub$dq_d),
         sd(sim2024_sub$list_a), sd(sim2024_sub$list_b), sd(sim2024_sub$list_c), sd(sim2024_sub$list_d),
         sd(sim2024_sub$sens_a), sd(sim2024_sub$sens_b), sd(sim2024_sub$sens_c), sd(sim2024_sub$sens_d)),
  label = rep(c("DQ", "List", "Bias"), each = 4),
  Group = rep(c("Correct", "Incorrect", "Graduate or undergraduate", "non undergraduate"), times = 3),
  Split = rep(rep(c("Political knowledge", "Educational levels"), each = 2), 3)
)

TableD2_df_sb <- tibble(
  est = c(mean(sim2024_sub$dif_know), mean(sim2024_sub$dif_edu)),
  se = c(sd(sim2024_sub$dif_know), sd(sim2024_sub$dif_edu)),
  label = (c("Political knowledge","Edu."))
)
################################### E. Multivariate analyses ###################################
reg_df <-
re_df2020 |> 
  mutate(Age = substr(re_df2020$p, 3, 4) |> as.numeric(),
         Age_G = case_when(Age < 30 ~ "18-29", Age < 60 ~ "30-64", TRUE ~ "65+"),
         Age_G = factor(Age_G, levels = c("30-64","18-29", "65+")),
         Gender = if_else(p < 200000, 1, 0),
         Education = if_else(Edu == 4, 1, 0),
         treat = if_else(Group == "Treatment", 1, 0)
         )

glm_result <- glm(DQ ~ Age_G + Gender + Education, data = reg_df, family = binomial("logit")) %>% tidy()

reg_df %>% as.data.frame() %>%
  ictreg(Out ~ Age_G + Gender + Education, treat = "treat", data = reg_df, J=4, method = "ml", robust = T) -> ict_result_ml
reg_df %>% as.data.frame() %>%
  ictreg(Out ~ Age_G + Gender + Education, treat = "treat", data =., J=4, method = "nls", robust = T) -> ict_result_nls

summary(ict_result_ml)
summary(ict_result_nls)
