library(ggplot2)
library(dplyr)
library(patchwork)
library(summarytools)
library(CCTpack)
library(gridExtra)
library(grid)
library(AER)


###dataset#######
data_or<-read.csv("Voce_total.csv")

###dependent variables aggregation####

mapping <- c(-3, -2, -1, 1, 2, 3)
table(data_or$Q1_1)

# political violence support
data_or$violence <- rowSums(
  cbind(
    mapping[data_or$Q171_1],
    mapping[data_or$Q171_2],
    mapping[data_or$Q171_3]
  ),
  na.rm = FALSE  
)

# Verify the result
summary(data_or$violence)
range(data_or$violence, na.rm = TRUE)
table(data_or$violence)

# anti-migrant institutional action support
data_or$anti <- rowSums(
  cbind(
    mapping[data_or$Q170_1],
    mapping[data_or$Q170_2],
    mapping[data_or$Q170_3]
  ),
  na.rm = FALSE  
)
table(data_or$anti)

# protest mobilisation support
data_or$Mob <- rowSums(
  cbind(
    mapping[data_or$Q12_1],
    mapping[data_or$Q12_2],
    mapping[data_or$Q12_3]
  ),
  na.rm = FALSE  
)
table(data_or$Mob)



######subset data##############

data_or_RW <- subset(data_or, ideo > 7)#radical right cohort
data_or_LW<-subset(data_or,ideo<8)#other / Non-radical right-cohort

# Generate the descriptive statistics table
data_RW_ds<-data_or_RW[,30:33]
data_RW_ds
data_or_RW
data_or_RW

data_LW_ds<-data_or_LW[,30:33]
data_or_LW

#centering#
data_or_RW$ideo_centered <- scale(data_or_RW$ideo, center = TRUE, scale = FALSE)
data_or_LW$ideo_centered <- scale(data_or_LW$ideo, center = TRUE, scale = FALSE)

#treatment cohort#
data_c<-subset(data_or_RW, levels==0)#control radical right
data_tg<- subset(data_or_RW, levels ==1)# positive treatment radical
data_tb<-subset(data_or_RW, levels == 2)#negative treatment radical 

data_sc<-subset(data_or_LW, levels==0)#control others
data_sg<- subset(data_or_LW, levels ==1)#positive treatment others
data_sb<-subset(data_or_LW, levels == 2)#negative other

###descriptive statistic#####
#Table A3
data_or_ds<-data_or[,30:33]
data_or_ds
descr_table <- dfSummary(data_or_ds)#whole sample 
view(descr_table)

#Table A4
descr_table_rw <- dfSummary(data_RW_ds)#radical right
view(descr_table_rw)

#Table A5
descr_table_lw <- dfSummary(data_LW_ds)#other
view(descr_table_lw)


##testing differences across cohorts##
#Figure A3
treat_sex_table <- table(data_c$gender)
contr_sex_table <- table(data_tb$gender)

# Combine into a single contingency table
combined_table <- rbind(treat_sex_table, contr_sex_table)
rownames(combined_table) <- c("Treatment_Negative", "Control")
sex_diff <-fisher.test(combined_table)
ideo_diff<-wilcox.test(data_tb$ideo,data_c$ideo)
age_diff<-wilcox.test(data_c$age,data_tb$age)
edu_diff<-wilcox.test(data_c$edu,data_tb$edu)

p_value_table <- data.frame(
  Variable = c("Gender", "Ideology", "Age", "Education"),
  p_value_Treatment_Control = c(sex_diff$p.value, ideo_diff$p.value, age_diff$p.value, edu_diff$p.value)
)

table_grob <- tableGrob(p_value_table)

grid.newpage()
grid.draw(table_grob)

grid.text("Treatment Negative vs Control", y = unit(0.9, "npc"), 
          gp = gpar(fontsize = 14, fontface = "bold"))

#Figure A4
treat_sex_table <- table(data_c$gender)
contr_sex_table <- table(data_tg$gender)
combined_table <- rbind(treat_sex_table, contr_sex_table)
rownames(combined_table) <- c("Treatment_Positive", "Control")
sex_diff <-fisher.test(combined_table)
ideo_diff<-wilcox.test(data_tg$ideo,data_c$ideo)
age_diff<-wilcox.test(data_c$age,data_tg$age)
edu_diff<-wilcox.test(data_c$edu,data_tg$edu)

p_value_table <- data.frame(
  Variable = c("Gender", "Ideology", "Age", "Education"),
  p_value_Treatment_Control = c(sex_diff$p.value, ideo_diff$p.value, age_diff$p.value, edu_diff$p.value)
)

table_grob <- tableGrob(p_value_table)

grid.newpage()
grid.draw(table_grob)

grid.text("Treatment Positive vs Control", y = unit(0.9, "npc"), 
          gp = gpar(fontsize = 14, fontface = "bold"))

#Figures A5#
treat_sex_table <- table(data_tb$gender)
contr_sex_table <- table(data_tg$gender)
combined_table <- rbind(treat_sex_table, contr_sex_table)
rownames(combined_table) <- c("Treatment_Positive", "Negative")
sex_diff <-fisher.test(combined_table)
ideo_diff<-wilcox.test(data_tg$ideo,data_tb$ideo)
age_diff<-wilcox.test(data_tb$age,data_tg$age)
edu_diff<-wilcox.test(data_tb$edu,data_tg$edu)

p_value_table <- data.frame(
  Variable = c("Gender", "Ideology", "Age", "Education"),
  p_value_Treatment_Control = c(sex_diff$p.value, ideo_diff$p.value, age_diff$p.value, edu_diff$p.value)
)

table_grob <- tableGrob(p_value_table)

grid.newpage()
grid.draw(table_grob)

grid.text("Treatment Positive vs Negative", y = unit(0.9, "npc"), 
          gp = gpar(fontsize = 14, fontface = "bold"))

##Ceiling floor#
# table A6
# Proportion at minimum and maximum - control rightwing Mob
min_val <- min(data_c$Mob, na.rm = TRUE)
max_val <- max(data_c$Mob, na.rm = TRUE)

prop_min <- sum(data_c$Mob == min_val, na.rm = TRUE) / length(data_c$Mob)
prop_max <- sum(data_c$Mob == max_val, na.rm = TRUE) / length(data_c$Mob)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - control rightwing Anti
min_val <- min(data_c$anti, na.rm = TRUE)
max_val <- max(data_c$anti, na.rm = TRUE)

prop_min <- sum(data_c$anti == min_val, na.rm = TRUE) / length(data_c$anti)
prop_max <- sum(data_c$anti == max_val, na.rm = TRUE) / length(data_c$anti)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - control rightwing violence
min_val <- min(data_c$violence, na.rm = TRUE)
max_val <- max(data_c$violence, na.rm = TRUE)

prop_min <- sum(data_c$violence == min_val, na.rm = TRUE) / length(data_c$violence)
prop_max <- sum(data_c$violence == max_val, na.rm = TRUE) / length(data_c$violence)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - negative treatment Mob rightwing
min_val <- min(data_tb$Mob, na.rm = TRUE)
max_val <- max(data_tb$Mob, na.rm = TRUE)

prop_min <- sum(data_tb$Mob == min_val, na.rm = TRUE) / length(data_tb$Mob)
prop_max <- sum(data_tb$Mob == max_val, na.rm = TRUE) / length(data_tb$Mob)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - negative treatment anti rightwing
min_val <- min(data_tb$anti, na.rm = TRUE)
max_val <- max(data_tb$anti, na.rm = TRUE)

prop_min <- sum(data_tb$anti == min_val, na.rm = TRUE) / length(data_tb$anti)
prop_max <- sum(data_tb$anti == max_val, na.rm = TRUE) / length(data_tb$anti)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - - negative treatment rightwing violence
min_val <- min(data_tb$violence, na.rm = TRUE)
max_val <- max(data_tb$violence, na.rm = TRUE)

prop_min <- sum(data_tb$violence == min_val, na.rm = TRUE) / length(data_tb$violence)
prop_max <- sum(data_tb$violence == max_val, na.rm = TRUE) / length(data_tb$violence)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - positive treatment rightwing Mob
min_val <- min(data_tg$Mob, na.rm = TRUE)
max_val <- max(data_tg$Mob, na.rm = TRUE)

prop_min <- sum(data_tg$Mob == min_val, na.rm = TRUE) / length(data_tg$Mob)
prop_max <- sum(data_tg$Mob == max_val, na.rm = TRUE) / length(data_tg$Mob)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - positive treatment rightwing Anti
min_val <- min(data_tg$anti, na.rm = TRUE)
max_val <- max(data_tg$anti, na.rm = TRUE)

prop_min <- sum(data_tg$anti == min_val, na.rm = TRUE) / length(data_tg$anti)
prop_max <- sum(data_tg$anti == max_val, na.rm = TRUE) / length(data_tg$anti)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - positive treatment rightwing violence
min_val <- min(data_tg$violence, na.rm = TRUE)
max_val <- max(data_tg$violence, na.rm = TRUE)

prop_min <- sum(data_tg$violence == min_val, na.rm = TRUE) / length(data_tg$violence)
prop_max <- sum(data_tg$violence == max_val, na.rm = TRUE) / length(data_tg$violence)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - control non-rightwing Mob
min_val <- min(data_sc$Mob, na.rm = TRUE)
max_val <- max(data_sc$Mob, na.rm = TRUE)

prop_min <- sum(data_sc$Mob == min_val, na.rm = TRUE) / length(data_sc$Mob)
prop_max <- sum(data_sc$Mob == max_val, na.rm = TRUE) / length(data_sc$Mob)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - control non-ritghwing anti
min_val <- min(data_sc$anti, na.rm = TRUE)
max_val <- max(data_sc$anti, na.rm = TRUE)

prop_min <- sum(data_sc$anti == min_val, na.rm = TRUE) / length(data_sc$anti)
prop_max <- sum(data_sc$anti == max_val, na.rm = TRUE) / length(data_sc$anti)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - control non-rightwing violence
min_val <- min(data_sc$violence, na.rm = TRUE)
max_val <- max(data_sc$violence, na.rm = TRUE)

prop_min <- sum(data_sc$violence == min_val, na.rm = TRUE) / length(data_sc$violence)
prop_max <- sum(data_sc$violence == max_val, na.rm = TRUE) / length(data_sc$violence)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - negative treatment non-rightwing Mob
min_val <- min(data_sb$Mob, na.rm = TRUE)
max_val <- max(data_sb$Mob, na.rm = TRUE)

prop_min <- sum(data_sb$Mob == min_val, na.rm = TRUE) / length(data_sb$Mob)
prop_max <- sum(data_sb$Mob == max_val, na.rm = TRUE) / length(data_sb$Mob)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - negative treatment non-rightwing anti
min_val <- min(data_sb$anti, na.rm = TRUE)
max_val <- max(data_sb$anti, na.rm = TRUE)

prop_min <- sum(data_sb$anti == min_val, na.rm = TRUE) / length(data_sb$anti)
prop_max <- sum(data_sb$anti == max_val, na.rm = TRUE) / length(data_sb$anti)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - negative treatment non-rightwing violence
min_val <- min(data_sb$violence, na.rm = TRUE)
max_val <- max(data_sb$violence, na.rm = TRUE)

prop_min <- sum(data_sb$violence == min_val, na.rm = TRUE) / length(data_sb$violence)
prop_max <- sum(data_sb$violence == max_val, na.rm = TRUE) / length(data_sb$violence)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - positive treatment non-rightwing Mob
min_val <- min(data_sg$Mob, na.rm = TRUE)
max_val <- max(data_sg$Mob, na.rm = TRUE)

prop_min <- sum(data_sg$Mob == min_val, na.rm = TRUE) / length(data_sg$Mob)
prop_max <- sum(data_sg$Mob == max_val, na.rm = TRUE) / length(data_sg$Mob)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - positive treatment non-rightwing Anti
min_val <- min(data_sg$anti, na.rm = TRUE)
max_val <- max(data_sg$anti, na.rm = TRUE)

prop_min <- sum(data_sg$anti == min_val, na.rm = TRUE) / length(data_sg$anti)
prop_max <- sum(data_sg$anti == max_val, na.rm = TRUE) / length(data_sg$anti)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")

# Proportion at minimum and maximum - positive treatment non-rightwing violence
min_val <- min(data_sg$violence, na.rm = TRUE)
max_val <- max(data_sg$violence, na.rm = TRUE)

prop_min <- sum(data_sg$violence == min_val, na.rm = TRUE) / length(data_sg$violence)
prop_max <- sum(data_sg$violence == max_val, na.rm = TRUE) / length(data_sg$violence)

cat("Proportion of values at the minimum:", prop_min, "\n")
cat("Proportion of values at the maximum:", prop_max, "\n")


######Cultural Consensus Theory [CCT]###############

#######CCT analysiss#######

control_t_cct<-data_c[11:20]#control radical 
treat_bad<-data_tb[11:20]#negative radical
treat_good<-data_tg[11:20]#positive radical
left_good<-data_sg[11:20]#positive others
left_bad<-data_sb[11:20]#negative other
left_cont<-data_sc[11:20]#control others

contr_cct <- cctscree(control_t_cct)#on culture
treat_cct <- cctscree(treat_bad)#one culture
treat_good_cct<-cctscree(treat_good)#one culture
treat_good_NR<-cctscree(left_good)#one culture 
contr_NR <- cctscree(left_cont)#one culture
treat_bad_RR <- cctscree(left_bad)#one culture

###running CCT model###
#CCT model treatment negative radical right#
cctfit_t_CT<- cctapply(data = treat_bad
                       , clusters = 1, itemdiff = TRUE, samples = 4000,
                       chains = 4, thinning=10, parallel = TRUE, burnin = 3000, seed = 1, runchecks = FALSE )
cctfit_t_CT$Rhat#R-hat 2/481

#CCT model treatment negative other#
cctfit_lt_CT<- cctapply(data = left_bad
                       , clusters = 1, itemdiff = TRUE, samples = 4000,#simulation
                       chains = 3, thinning=10, parallel = TRUE, burnin = 3000, seed = 1, runchecks = FALSE )
cctfit_lt_CT$Rhat#R-hat36/478

#CCT model treatment positive radical right#
cctfit_tg_CT<- cctapply(data = treat_good
                       , clusters = 1, itemdiff = TRUE, samples = 4000,
                       chains = 2,  thinning=20, parallel = TRUE,burnin = 3000, seed = 1, runchecks = FALSE )
cctfit_tg_CT$Rhat#R-hat 27/532

#CCT model treatment positive other#
cctfit_tlg_CT<- cctapply(data = left_good
                        , clusters = 1, itemdiff = TRUE, samples = 4000,
                        chains = 3, thinning=10, parallel = TRUE, burnin = 3000, seed = 1, runchecks = FALSE )
cctfit_tlg_CT$Rhat#R-hat 43/448

#CCT model control radical right#
cctfit_c_CT<- cctapply(data = control_t_cct
                       , clusters = 1, itemdiff = TRUE, samples = 4000,
                       chains = 2, thinning=20, parallel = TRUE, burnin = 3000, seed = 1, runchecks = FALSE )

cctfit_c_CT$Rhat#R-hat 3/586

#CCT model control other#
cctfit_lc_CT<- cctapply(data = left_cont
                       , clusters = 1, itemdiff = TRUE, samples = 4500,
                       chains = 4,  thinning=10, parallel = TRUE,burnin = 3500, seed = 1, runchecks = FALSE )
cctfit_lc_CT$Rhat#R-hat 43/547



#fit measures - VDI - between 15-85#
cctfit_t_CT<-cctppc(cctfit_t_CT)
cctfit_t_CT$VDIperc

cctfit_c_CT <- cctppc(cctfit_c_CT)
cctfit_c_CT$VDIperc

cctfit_g_CT <- cctppc(cctfit_tg_CT)
cctfit_g_CT$VDIperc

cctfit_lt_CT<-cctppc(cctfit_lt_CT)
cctfit_lt_CT$VDIperc

cctfit_lc_CT <- cctppc(cctfit_lc_CT)
cctfit_lc_CT$VDIperc

cctfit_lg_CT <- cctppc(cctfit_tlg_CT)
cctfit_lg_CT$VDIperc

#plots overview#not reported
cctresults(cctfit_t_CT)
cctresults(cctfit_c_CT)
cctresults(cctfit_tg_CT)
cctresults(cctfit_lt_CT)
cctresults(cctfit_lc_CT)
cctresults(cctfit_lg_CT)

####consensus values###
item_CT_hfdi_t<-data.frame(cctitemhdi(cctfit_t_CT))
item_CT_hfdi_g<-data.frame(cctitemhdi(cctfit_tg_CT))
item_CT_hfdi_c<-data.frame(cctitemhdi(cctfit_c_CT))
item_CT_hfdi_lt<-data.frame(cctitemhdi(cctfit_lt_CT))
item_CT_hfdi_lg<-data.frame(cctitemhdi(cctfit_lg_CT))
item_CT_hfdi_lc<-data.frame(cctitemhdi(cctfit_lc_CT))

###save consensus values###
# Assuming you have a dataframe named 'my_data'
write.csv(item_CT_hfdi_t, "item_CT_hfdi_t.csv", row.names = FALSE)
# Assuming you have a dataframe named 'my_data'
write.csv(item_CT_hfdi_g, "item_CT_hfdi_g.csv", row.names = FALSE)
# Assuming you have a dataframe named 'my_data'
write.csv(item_CT_hfdi_c, "item_CT_hfdi_c.csv", row.names = FALSE)
# Assuming you have a dataframe named 'my_data'
write.csv(item_CT_hfdi_lt, "item_CT_hfdi_lt.csv", row.names = FALSE)
# Assuming you have a dataframe named 'my_data'
write.csv(item_CT_hfdi_lg, "item_CT_hfdi_lg.csv", row.names = FALSE)
# Assuming you have a dataframe named 'my_data'
write.csv(item_CT_hfdi_lc, "item_CT_hfdi_lc.csv", row.names = FALSE)

#consensus without CI#
item_CT_t<-data.frame(cctitem(cctfit_t_CT))
item_CT_c<-data.frame(cctitem(cctfit_c_CT))
item_CT_g<-data.frame(cctitem(cctfit_g_CT))
item_CT_lt<-data.frame(cctitem(cctfit_lt_CT))
item_CT_lc<-data.frame(cctitem(cctfit_lc_CT))
item_CT_lg<-data.frame(cctitem(cctfit_lg_CT))

#consensus plots - Figure 1#
treat_cct_truth<-ggplot(item_CT_hfdi_t, aes(x = factor(item))) +
  geom_point(aes(y = l_ans1), color = 'blue') +
  geom_point(aes(y = u_ans1), color = 'red') +
  geom_errorbar(aes(ymin = l_ans1, ymax = u_ans1), width = 0.1) +
  labs(title = "Negative News: Right Wing",
       x = "Questions",
       y = "Confidence Interval") +ylim(-8,9)+
  theme_minimal()+geom_hline(yintercept = 0)

Control_cct_truth<-ggplot(item_CT_hfdi_c, aes(x = factor(item))) +
  geom_point(aes(y = l_ans1), color = 'blue') +ylim(-8,9)+
  geom_point(aes(y = u_ans1), color = 'red') +
  geom_errorbar(aes(ymin = l_ans1, ymax = u_ans1), width = 0.1) +
  labs(title = "Control: Right Wing",
       x = "Questions",
       y = "Confidence Interval") +
  theme_minimal()+geom_hline(yintercept = 0)

good_cct_truth<-ggplot(item_CT_hfdi_g, aes(x = factor(item))) +
  geom_point(aes(y = l_ans1), color = 'blue') +ylim(-8,9)+
  geom_point(aes(y = u_ans1), color = 'red') +
  geom_errorbar(aes(ymin = l_ans1, ymax = u_ans1), width = 0.1) +
  labs(title = "Positive News: Right Wing",
       x = "Questions",
       y = "Confidence Interval") +
  theme_minimal()+geom_hline(yintercept = 0)


treat_cct_ltruth<-ggplot(item_CT_hfdi_lt, aes(x = factor(item))) +
  geom_point(aes(y = l_ans1), color = 'blue') +
  geom_point(aes(y = u_ans1), color = 'red') +
  geom_errorbar(aes(ymin = l_ans1, ymax = u_ans1), width = 0.1) +
  labs(title = "Negative News: Non-Right Wing",
       x = "Questions",
       y = "Confidence Interval") +ylim(-8,9)+
  theme_minimal()+geom_hline(yintercept = 0)

Control_cct_ltruth<-ggplot(item_CT_hfdi_lc, aes(x = factor(item))) +
  geom_point(aes(y = l_ans1), color = 'blue') +ylim(-8,9)+
  geom_point(aes(y = u_ans1), color = 'red') +
  geom_errorbar(aes(ymin = l_ans1, ymax = u_ans1), width = 0.1) +
  labs(title = "Control: Non-Right Wing",
       x = "Questions",
       y = "Confidence Interval") +
  theme_minimal()+geom_hline(yintercept = 0)

good_cct_ltruth<-ggplot(item_CT_hfdi_lg, aes(x = factor(item))) +
  geom_point(aes(y = l_ans1), color = 'blue') +ylim(-8,9)+
  geom_point(aes(y = u_ans1), color = 'red') +
  geom_errorbar(aes(ymin = l_ans1, ymax = u_ans1), width = 0.1) +
  labs(title = "Positive News: Non-Right Wing",
       x = "Questions",
       y = "Confidence Interval") +
  theme_minimal()+geom_hline(yintercept = 0)
(Control_cct_ltruth+treat_cct_ltruth+good_cct_ltruth)/(Control_cct_truth+treat_cct_truth+good_cct_truth)

###cultural competence###
#dataset extraction#

sbj_t_d_hdi<-data.frame(cctsubjhdi(cctfit_t_CT))
sbj_t_d<- data.frame(cctsubj(cctfit_t_CT))
sbj_c_d_hdi<-data.frame(cctsubjhdi(cctfit_c_CT))
sbj_c_d<- data.frame(cctsubj(cctfit_c_CT))
sbj_g_d_hdi<-data.frame(cctsubjhdi(cctfit_g_CT))
sbj_g_d<- data.frame(cctsubj(cctfit_g_CT))


sbj_lg_d_hdi<-data.frame(cctsubjhdi(cctfit_lg_CT))
sbj_lg_d<- data.frame(cctsubj(cctfit_lg_CT))
sbj_lc_d_hdi<-data.frame(cctsubjhdi(cctfit_lc_CT))
sbj_lc_d<- data.frame(cctsubj(cctfit_lc_CT))
sbj_lt_d_hdi<-data.frame(cctsubjhdi(cctfit_lt_CT))
sbj_lt_d<- data.frame(cctsubj(cctfit_lt_CT))


# save consensus data#
write.csv(sbj_t_d, "sbj_t_d.csv", row.names = FALSE)
write.csv(sbj_c_d, "sbj_c_d.csv", row.names = FALSE)
write.csv(sbj_g_d, "sbj_g_d.csv", row.names = FALSE)
write.csv(sbj_lt_d, "sbj_lt_d.csv", row.names = FALSE)
write.csv(sbj_lc_d, "sbj_lc_d.csv", row.names = FALSE)
write.csv(sbj_lg_d, "sbj_lg_d.csv", row.names = FALSE)

###link consensus data with origianal dataset#
cUlt_c<-(cctsubj(cctfit_c_CT))
cult_c2<-(cctsubjhdi(cctfit_c_CT))
cult_e2<-(cctsubjhdi(cctfit_t_CT))
cUlt_e<-(cctsubj(cctfit_t_CT))
cult_g2<-(cctsubjhdi(cctfit_g_CT))
cUlt_g<-(cctsubj(cctfit_g_CT))

cUlt_lc<-(cctsubj(cctfit_lc_CT))
cult_lc2<-(cctsubjhdi(cctfit_lc_CT))
cult_le2<-(cctsubjhdi(cctfit_lt_CT))
cUlt_le<-(cctsubj(cctfit_lt_CT))
cult_lg2<-(cctsubjhdi(cctfit_lg_CT))
cUlt_lg<-(cctsubj(cctfit_lg_CT))

knowledge_t_CT<-data.frame(cult_e2,cUlt_e)
knowledge_c_CT<-data.frame(cult_c2,cUlt_c)
knowledge_g_CT<-data.frame(cult_g2,cUlt_g)

knowledge_lt_CT<-data.frame(cult_le2,cUlt_le)
knowledge_lc_CT<-data.frame(cult_lc2,cUlt_lc)
knowledge_lg_CT<-data.frame(cult_lg2,cUlt_lg)


#plot competence radical right [not reported]#
data <- data.frame(
  score = c(knowledge_c_CT$comp_E, knowledge_t_CT$comp_E, knowledge_g_CT$comp_E),
  group = factor(c(rep("Control", length(knowledge_c_CT$comp_E)), 
                   rep("negative news", length(knowledge_t_CT$comp_E)),  # Correct closing of rep() function
                   rep("positive news", length(knowledge_g_CT$comp_E))))  # Correct closing of rep() function
)


p <- ggplot(data, aes(x = group, y = score, fill = group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") + # This adds color
  labs(title = "Cultural Knowledge Scores (SE Posterior mean value): Control vs. Treatment",
       x = "Group",
       y = "Knowledge Score") +
  theme_minimal()

print(p)

###plot competence others [not reported]#
datal <- data.frame(
  score = c(knowledge_lc_CT$comp_E, knowledge_lt_CT$comp_E, knowledge_lg_CT$comp_E),
  group = factor(c(rep("Control", length(knowledge_lc_CT$comp_E)), 
                   rep("negative news", length(knowledge_lt_CT$comp_E)),  # Correct closing of rep() function
                   rep("positive news", length(knowledge_lg_CT$comp_E))))  # Correct closing of rep() function
)

pl <- ggplot(datal, aes(x = group, y = score, fill = group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") + # This adds color
  labs(title = "Cultural Knowledge Scores (SE Posterior mean value): Control vs. Treatment",
       x = "Group",
       y = "Knowledge Score") +
  theme_minimal()

print(pl)

######link data with results####
#raw competence
data_tb$know_r<-knowledge_t_CT$comp_E
data_c$know_r<-knowledge_c_CT$comp_E
data_tg$know_r<-knowledge_g_CT$comp_E


data_sc$know_r<-knowledge_lc_CT$comp_E
data_sg$know_r<-knowledge_lg_CT$comp_E
data_sb$know_r<-knowledge_lt_CT$comp_E

##radical right subset {values 9,10,11}##
data_cr<-subset(data_c, ideo>8)#control radical right
data_tgr<- subset(data_tg, ideo>8)# positive treatment radical right
data_tbr<-subset(data_tb, ideo > 8)#negative treatment radical right

#######H1 and H2 hypotheses#####
###competence wilcoxon test #### H1 and H2########
##table 1##

#testing competence between positive, negative and control#
wilcox.test(data_cr$know_r, data_tbr$know_r, paired = FALSE, alternative = "greater")#H1 not corroborated -> control more consensus than negative exposure
wilcox.test(data_cr$know_r, data_tgr$know_r, paired = FALSE, alternative = "less")#corroborated against negative more consensus than positive treatment treatment
wilcox.test(data_tgr$know_r, data_tbr$know_r, paired = FALSE, alternative = "greater")#H2 corroborated: positive treatment more consensus than negative
mean(data_cr$know_r)
mean(data_tbr$know_r)
mean(data_tgr$know_r)
var(data_cr$know_r)
var(data_tbr$know_r)
var(data_tgr$know_r)

#non-radical right #
wilcox.test(data_sc$know_r, data_sb$know_r, paired = FALSE, alternative = "less")#in NR control and negative similar
wilcox.test(data_sc$know_r, data_sg$know_r, paired = FALSE, alternative = "greater")#in NR control and positive similar
wilcox.test(data_sg$know_r, data_sb$know_r, paired = FALSE, alternative = "less")#in NR positive and negative similar
mean(data_sc$know_r)
mean(data_sb$know_r)
mean(data_sg$know_r)
var(data_sc$know_r)
var(data_sb$know_r)
var(data_sg$know_r)

######violence, anti-migrant institution support, protest - table 2####
wilcox.test(data_cr$anti, data_tbr$anti, paired = FALSE, alternative = "less")#support for anti-migrant government similar across all three cohorts
wilcox.test(data_cr$anti, data_tgr$anti, paired = FALSE, alternative = "greater")#support here is bigger for positive treatment
wilcox.test(data_tgr$anti, data_tbr$anti, paired = FALSE, alternative = "less")
mean(data_cr$anti)
mean(data_tbr$anti)
mean(data_tgr$anti)
var(data_cr$anti)
var(data_tbr$anti)
var(data_tgr$anti)

wilcox.test(data_cr$violence, data_tbr$violence, paired = FALSE, alternative = "less")#support for political violence action more in negative exposure
wilcox.test(data_cr$violence, data_tgr$violence, paired = FALSE, alternative = "greater")
wilcox.test(data_tgr$violence, data_tbr$violence, paired = FALSE, alternative = "less")
mean(data_cr$violence)
mean(data_tbr$violence)
mean(data_tgr$violence)
var(data_cr$violence)
var(data_tbr$violence)
var(data_tgr$violence)

wilcox.test(data_cr$Mob, data_tbr$Mob, paired = FALSE, alternative = "less")#support for political protest more in negative exposure
wilcox.test(data_cr$Mob, data_tgr$Mob, paired = FALSE, alternative = "greater")
wilcox.test(data_tgr$Mob, data_tbr$Mob, paired = FALSE, alternative = "less")
mean(data_cr$Mob)
mean(data_tbr$Mob)
mean(data_tgr$Mob)
var(data_cr$Mob)
var(data_tbr$Mob)
var(data_tgr$Mob)


##Table A9##
wilcox.test(data_sc$anti, data_sb$anti, paired = FALSE, alternative = "less")#no differences
wilcox.test(data_sc$anti, data_sg$anti, paired = FALSE, alternative = "greater")
wilcox.test(data_sg$anti, data_sb$anti, paired = FALSE, alternative = "less")
mean(data_sc$anti)
mean(data_sb$anti)
mean(data_sg$anti)
var(data_sc$anti)
var(data_sb$anti)
var(data_sg$anti)

wilcox.test(data_sc$violence, data_sb$violence, paired = FALSE, alternative = "less")#no differences
wilcox.test(data_sc$violence, data_sg$violence, paired = FALSE, alternative = "greater")
wilcox.test(data_sg$violence, data_sb$violence, paired = FALSE, alternative = "less")
mean(data_sc$violence)
mean(data_sb$violence)
mean(data_sg$violence)
var(data_sc$violence)
var(data_sb$violence)
var(data_sg$violence)

wilcox.test(data_sc$Mob, data_sb$Mob, paired = FALSE, alternative = "less")#no differences
wilcox.test(data_sc$Mob, data_sg$Mob, paired = FALSE, alternative = "greater")
wilcox.test(data_sg$Mob, data_sb$Mob, paired = FALSE, alternative = "less")
mean(data_sc$Mob)
mean(data_sb$Mob)
mean(data_sg$Mob)
var(data_sc$Mob)
var(data_sb$Mob)
var(data_sg$Mob)

##########H3##############
####data competence inversion sign###

data_tb$know<--knowledge_t_CT$comp_E
data_c$know<--knowledge_c_CT$comp_E
data_tg$know<--knowledge_g_CT$comp_E

data_sc$know<--knowledge_lc_CT$comp_E
data_sg$know<--knowledge_lg_CT$comp_E
data_sb$know<--knowledge_lt_CT$comp_E

####Control Radical Right Regression#####table 3

model_control_RR<-lm(cbind(anti,violence,Mob) ~know*ideo_centered+edu+age+as.factor(gender), data = data_c)
summary(model_control_RR)

# Model tobit#table A7 
model_anti_C_RR <- tobit(anti ~ know * ideo_centered + edu + age + as.factor(gender), left=-Inf,right = 9, data = data_c)
model_violence_C_RR <- tobit(violence ~ know * ideo_centered + edu + age + as.factor(gender), left=-Inf,right = 9, data = data_c)
model_mob_C_RR <- tobit(Mob ~ know * ideo_centered + edu + age + as.factor(gender), left=-Inf,right = 9, data = data_c)
summary(model_anti_C_RR)
summary(model_violence_C_RR)
summary(model_mob_C_RR)


####Negative news treatment Radical Right####table 3

model_negative_RR<-lm(cbind(anti,violence,Mob) ~know*ideo_centered+edu+age+as.factor(gender), data = data_tb)
summary(model_negative_RR)

#model Tobit#table A7
model_anti_N_RR <- tobit(anti ~ know * ideo_centered + edu + age + as.factor(gender), left=-Inf,right = 9, data = data_tb)
model_violence_N_RR <- tobit(violence ~ know * ideo_centered + edu + age + as.factor(gender), left=-Inf,right = 9, data = data_tb)
model_mob_N_RR <- tobit(Mob ~ know * ideo_centered + edu + age + as.factor(gender), left=-Inf,right = 9, data = data_tb)
summary(model_anti_N_RR)
summary(model_violence_N_RR)
summary(model_mob_N_RR)


####Positive news treatment Radical Right####Table 3
model_positive_RR<-lm(cbind(anti,violence,Mob) ~know*ideo_centered+edu+age+as.factor(gender), data=data_tg)
summary(model_positive_RR)

#model tobit#table A7
model_anti_G_RR <- tobit(anti ~ know * ideo_centered + edu + age + as.factor(gender), left=-Inf,right = 9, data = data_tg)
model_violence_G_RR <- tobit(violence ~ know * ideo_centered + edu + age + as.factor(gender), left=-Inf,right = 9, data = data_tg)
model_mob_G_RR <- tobit(Mob ~ know * ideo_centered + edu + age + as.factor(gender), left=-Inf,right = 9, data = data_tg)
summary(model_anti_G_RR)
summary(model_violence_G_RR)
summary(model_mob_G_RR)

####non-radical-Right Models####
# Control Non-Radical Right#table A10
model_C_NR <- lm(cbind(anti,violence,Mob) ~ know * ideo_centered + edu + age + as.factor(gender), data = data_sc)
summary(model_C_NR)

#tobit#not reported
model_anti_C_NR <- tobit(anti ~ know * ideo_centered + edu + age + as.factor(gender), left=-9,right = Inf, data = data_sc)
model_violence_C_NR <- tobit(violence ~ know * ideo_centered + edu + age + as.factor(gender), left=-9,right = Inf, data = data_sc)
model_mob_C_NR <- tobit(Mob ~ know * ideo_centered + edu + age + as.factor(gender), left=-9,right = Inf, data = data_sc)
summary(model_anti_C_NR)
summary(model_violence_C_NR)
summary(model_mob_C_NR)

#Treatment negative Non-Radical Right#Table A10
model_N_NR <- lm(cbind(anti,violence,Mob) ~ know * ideo_centered + edu + age + as.factor(gender), data = data_sb)
summary(model_N_NR)
#tobit#not reported
model_anti_N_NR <- tobit(anti ~ know * ideo_centered + edu + age + as.factor(gender), left=-9,right = Inf, data = data_sb)
model_violence_N_NR <- tobit(violence ~ know * ideo_centered + edu + age + as.factor(gender), left=-9,right = Inf, data = data_sb)
model_mob_N_NR <- tobit(Mob ~ know * ideo_centered + edu + age + as.factor(gender), left=-9,right = Inf, data = data_sb)
summary(model_anti_N_NR)
summary(model_violence_N_NR)
summary(model_mob_N_NR)

#Treatment positive Non-Radical Right#Table A10
model_G_NR <- lm(cbind(anti,violence,Mob) ~ know * ideo_centered + edu + age + as.factor(gender), data = data_sg)
summary(model_G_NR)

#tobit#non-reported
model_anti_G_NR <- tobit(anti ~ know * ideo_centered + edu + age + as.factor(gender), left=-9,right = Inf, data = data_sg)
model_violence_G_NR <- tobit(violence ~ know * ideo_centered + edu + age + as.factor(gender), left=-9,right = Inf, data = data_sg)
model_mob_G_NR <- tobit(Mob ~ know * ideo_centered + edu + age + as.factor(gender), left=-9,right = Inf, data = data_sg)
summary(model_anti_G_NR)
summary(model_violence_G_NR)
summary(model_mob_G_NR)

