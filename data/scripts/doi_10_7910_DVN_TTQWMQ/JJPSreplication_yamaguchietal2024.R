
library(tidyverse)
library(marginaleffects)
library(ggeffects)
library(estimatr)
library(janitor)
library(ggsci)

data <- read.csv("replication_yamaguchietal2024.csv")

# experiment group
data <- data %>% 
  mutate(exp1 = 
           case_when(!is.na(Q9.1) ~ "EleEdu",
                     !is.na(Q10.1) ~ "LotoEdu",
                     !is.na(Q11.1) ~ "HalfLotoEdu",
                     !is.na(Q12.1) ~ "EpistEdu",
                     !is.na(Q13.1) ~ "EleTax",
                     !is.na(Q14.1) ~ "LotoTax",
                     !is.na(Q15.1) ~ "HalfLotoTax",
                     !is.na(Q16.1) ~ "EpistTax",
           ),
         exp = 
           case_when(!is.na(Q9.1) ~ "Group1",
                     !is.na(Q10.1) ~ "Group2",
                     !is.na(Q11.1) ~ "Group3",
                     !is.na(Q12.1) ~ "Group4",
                     !is.na(Q13.1) ~ "Group5",
                     !is.na(Q14.1) ~ "Group6",
                     !is.na(Q15.1) ~ "Group7",
                     !is.na(Q16.1) ~ "Group8",
           ),
         policy =
           case_when(exp1 == "EleEdu" | exp1 == "LotoEdu"|exp1 == "HalfLotoEdu" | exp1 == "EpistEdu" ~ "Education Policy",
                     exp1 == "EleTax" | exp1 == "LotoTax"|exp1 == "HalfLotoTax" | exp1 == "EpistTax" ~ "Environmental Tax",
           ),
         decision =
           case_when(exp1 == "EleEdu" | exp1 == "EleTax" ~ "Electoral democracy",
                     exp1 == "LotoEdu" | exp1 == "LotoTax" ~ "Lottocracy \n (Full)",
                     exp1 == "HalfLotoEdu" | exp1 == "HalfLotoTax" ~ "Lottocracy \n (Upper only)",
                     exp1 == "EpistEdu" | exp1 == "EpistTax" ~ "Epistocracy",
           ),
         decision = factor(decision, levels = c("Electoral democracy","Lottocracy \n (Full)",
                                                "Lottocracy \n (Upper only)","Epistocracy")))
 

# outcome variable
data <- data %>% 
  mutate(out = 
           case_when(Q9.1 == 1|Q10.1 == 1|Q9.1 == 1|Q11.1 == 1|Q12.1 == 1|Q13.1 == 1|Q14.1 == 1|Q15.1 == 1|Q16.1 == 1 ~ 5,
                     Q9.1 == 2|Q10.1 == 2|Q9.1 == 2|Q11.1 == 2|Q12.1 == 2|Q13.1 == 2|Q14.1 == 2|Q15.1 == 2|Q16.1 == 2 ~ 4,
                     Q9.1 == 3|Q10.1 == 3|Q9.1 == 3|Q11.1 == 3|Q12.1 == 3|Q13.1 == 3|Q14.1 == 3|Q15.1 == 3|Q16.1 == 3 ~ 3,
                     Q9.1 == 4|Q10.1 == 4|Q9.1 == 4|Q11.1 == 4|Q12.1 == 4|Q13.1 == 4|Q14.1 == 4|Q15.1 == 4|Q16.1 == 4 ~ 2,
                     Q9.1 == 5|Q10.1 == 5|Q9.1 == 5|Q11.1 == 5|Q12.1 == 5|Q13.1 == 5|Q14.1 == 5|Q15.1 == 5|Q16.1 == 5 ~ 1,
                     TRUE ~ NA),
         out.f = factor(out))

data <- data %>% 
  mutate(gender = sex,
         age = age,
         education = if_else(Q2.1 == 6, NA, Q2.1),
         income = if_else(Q2.2 == 22, NA, Q2.2),
         know1 = if_else(Q7.2 == 4, 1, 0),
         know2 = if_else(Q7.3 == 2, 1, 0),
         know3 = if_else(Q7.4 == 1, 1, 0),
         know = know1 + know2 + know3)

summary(data$know)


### Table 1

data %>% 
  group_by(exp) %>% 
  summarise(Gender = mean(gender,na.rm = T),
            Age = mean(age,na.rm = T),
            Educational_level = mean(education, na.rm = T),
            household_income = mean(income, na.rm = T),
            Political_knowledge = mean(know, na.rm = T))


### Table 2

data %>% 
  group_by(exp) %>% 
  summarise(ATE = mean(out, na.rm = T),
            sd = sd(out, na.rm = T),
            n = n())

TukeyHSD(aov(data$out ~ data$exp))


### Figure 1

fig1 <- data %>%
  drop_na(out.f) %>% 
  group_by(exp) %>% 
  count(out.f) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(out.f, prop)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~exp, nrow = 2) + 
  theme_bw(base_size = 14, base_family = "HiraginoSans-W3") +
  scale_y_continuous(labels = scales::percent, limits=c(0,.6)) + 
  geom_text(aes(out.f, prop, label=sprintf("%.2f", prop*100)),
            vjust = -.5) + 
  labs(y = "Response rate(percent)",
       x = "Not Acceptance　←　→　　Acceptance")


fig1

ggsave("fig1.pdf", fig1, dpi = 300, width = 12, height = 6, device = cairo_pdf)


####figure2
data <- data %>% 
  mutate(manu = 
           case_when(exp1 == "EleEdu" & Q17.1 == 1 & Q17.2 == 4 ~ 1,
                     exp1 == "LotoEdu" & Q17.1 == 1 & Q17.2 == 1 ~ 1,
                     exp1 == "HalfLotoEdu" & Q17.1 == 1 & Q17.2 == 2 ~ 1,
                     exp1 == "EpistEdu" & Q17.1 == 1 & Q17.2 == 3 ~ 1,
                     exp1 == "EleTax" & Q17.1 == 4 & Q17.2 == 4 ~ 1,
                     exp1 == "LotoTax" & Q17.1 == 4 & Q17.2 == 1 ~ 1,
                     exp1 == "HalfLotoTax" & Q17.1 == 4 & Q17.2 == 2 ~ 1,
                     exp1 == "EpistTax" & Q17.1 == 4 & Q17.2 == 3 ~ 1,
                     TRUE ~ 0
           ))

table(data$manu)

1013/2398

####Policy Model
policy.full <- data %>% 
  lm_robust(out ~ policy + gender + age + education + income + know, 
            data = .,
            se_type = "stata") %>% 
  tidy() %>% 
  mutate(group = "All respondents")

policy.correct <- data %>% 
  filter(manu == 1) %>% 
  lm_robust(out ~ policy + gender + age + education + income + know, 
            data = .,
            se_type = "stata") %>% 
  tidy() %>% 
  mutate(group = "Respondents who answered correctly")

policymodel <- rbind(policy.full,policy.correct) %>% 
  mutate(team = "Policy Model")


####Decision Making model
decisionmodel.full <- data %>% 
  lm_robust(out ~ decision + gender + age + education + income + know, 
            data = .,
            se_type = "stata") %>% 
  tidy() %>% 
  mutate(group = "All respondents")

decisionmodel.correct <- data %>% 
  filter(manu == 1) %>% 
  lm_robust(out ~ decision + gender + age + education + income + know, 
            data = .,
            se_type = "stata") %>% 
  tidy() %>% 
  mutate(group = "Respondents who answered correctly")

decisionmodel <- rbind(decisionmodel.full,decisionmodel.correct) %>% 
  mutate(team = "Decision Making Model")


####Interaction model
interactionmodel.full <- data %>% 
  lm_robust(out ~ policy * decision + gender + age + education + income + know, 
            data = .,
            se_type = "stata") %>% 
  tidy() %>% 
  mutate(group = "All respondents")

interactionmodel.correct <- data %>% 
  filter(manu == 1) %>% 
  lm_robust(out ~ policy * decision + gender + age + education + income + know, 
            data = .,
            se_type = "stata") %>% 
  tidy() %>% 
  mutate(group = "Respondents who answered correctly")

interactionmodel <- rbind(interactionmodel.full,interactionmodel.correct) %>% 
  mutate(team = "Interaction Term")

### combined model
model <- rbind(policymodel,decisionmodel,interactionmodel) 
model

model <- model %>% 
  mutate(sig = if_else(conf.low * conf.high > 0, "sig", "ns"),
         team = factor(team, levels = c("Policy Model","Decision Making Model","Interaction Term"))) %>% 
  mutate(term1 = 
           case_when(term == "policyEnvironmental Tax" ~ "Environmental Tax",
                     term == "decisionLottocracy \n (Full)" ~ "Lottocracy \n (Full)",
                     term == "decisionLottocracy \n (Upper only)" ~ "Lottocracy \n (Upper only)",
                     term == "decisionEpistocracy" ~ "Epistocracy",
                     term == "policyEnvironmental Tax:decisionLottocracy \n (Full)" ~ "Lottocracy \n (Full) * \n Environmental Tax",
                     term == "policyEnvironmental Tax:decisionLottocracy \n (Upper only)" ~ "Lottocracy \n (Upper only) * \n Environmental Tax",
                     term == "policyEnvironmental Tax:decisionEpistocracy" ~ "Epistocracy * \n Environmental Tax",
           ),
         term1 = factor(term1, levels = c("Environmental Tax",
                                          "Lottocracy \n (Full)",
                                          "Lottocracy \n (Upper only)",
                                          "Epistocracy",
                                          "Lottocracy \n (Full) * \n Environmental Tax",
                                          "Lottocracy \n (Upper only) * \n Environmental Tax",
                                          "Epistocracy * \n Environmental Tax")))

model1 <- model %>% 
  drop_na(term1)

fig2 <- model1 %>% 
  filter(!is.na(term1)) %>%
  mutate(term1 = fct_reorder(term1, desc(term1))) %>%
  ggplot() +
  geom_vline(xintercept =  0, color = "black",  linetype="dashed") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high,
                      y = term1, shape = group, color = sig),
                  position = position_dodge(width = 0.4) ) + 
  scale_color_manual(values = c("sig" = "black",
                                "ns" = "grey60")) +  
  theme_bw(base_size = 15, base_family = "HiraginoSans-W3") + 
  theme(axis.title.y = element_blank(),legend.position = "bottom") + 
  labs(x = "Point Estimation with 95% Confidence Interval",
       shape = "") +
  facet_wrap(~ team) + 
  guides(color = "none") + 
  geom_text(model1 %>% filter(group == "All respondents"),
            mapping = aes(x = estimate, y = term1, 
                          label=sprintf("%.2f", estimate),
                          vjust = 2.5)) + 
  geom_text(model1 %>% filter(group == "Respondents who answered correctly"),
            mapping = aes(x = estimate, y = term1, 
                          label=sprintf("%.2f", estimate),
                          vjust = - 1.5)) + 
  guides(shape = guide_legend(reverse = TRUE))


fig2

ggsave("fig2.pdf", fig2, dpi = 300, width = 11, height = 6.4, device = cairo_pdf)

### Figure 3

#Calculate the predicted value
interactionmodel.correct.pred <- data %>% 
  filter(manu == 1) %>% 
  lm_robust(out ~ policy * decision + gender + age + education + income + know, 
            data = .,
            se_type = "stata")

interactionmodel.prediction <- predictions(interactionmodel.correct.pred,
                                           newdata = datagrid(decision = c("Electoral democracy","Lottocracy \n (Full)","Lottocracy \n (Upper only)","Epistocracy"),
                                                              policy = c("Education Policy","Environmental Tax")))

fig3 <- interactionmodel.prediction %>% 
  ggplot(aes(decision, estimate)) +
  geom_point(position = position_dodge(.1),
             size = 2) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(.1),
    width = 0.1, 
    size = 0.5) +
  geom_text(aes(x = decision, y = estimate, label=sprintf("%.2f", estimate)), 
            hjust = -.5, size = 6) +
  labs(x = "", y = "Predicted Value") + 
  theme_bw(base_size = 14) + 
  facet_wrap(~ policy, nrow = 2) + 
  ylim(1,5)

fig3

ggsave("fig3.pdf", fig3, dpi = 300, width = 7, height = 5, device = cairo_pdf)


###################################################################################################


### Appendix:Table A

data %>% 
  tabyl(gender, show_na = F)


data <- data %>% 
  mutate(sedai = 
           case_when(age >= 18 & age < 30 ~ "18-29",
                     age >= 30 & age < 40 ~ "30-39",
                     age >= 40 & age < 50 ~ "40-49",
                     age >= 50 & age < 60 ~ "50-59",
                     age >= 60 & age < 70 ~ "60-69",
                     age >= 70 ~ "70-"))

data %>% 
  tabyl(sedai, show_na = F)

data %>% 
  tabyl(education, show_na = F)

data <- data %>% 
  mutate(hi = 
           case_when(income == 1 ~ "Less than 3,000k",
                     income == 2 | income == 3 ~ "3,000k-4,000k",
                     income == 4 | income == 5 ~ "4,000k-5,000k",
                     income == 6 | income == 7 ~ "5,000k-6,000k",
                     income == 8 | income == 9 ~ "6,000k-7,000k",
                     income == 10 | income == 11 ~ "7,000k-8,000k",
                     income == 12 | income == 13 ~ "8,000k-9,000k",
                     income == 14 | income == 15 ~ "9,000k-10,000k",
                     income == 16 | income == 17 ~ "10,000k-15,000k",
                     income == 18  ~ "15,000k-20,000k",
                     income == 19 | income == 20  ~ "More than 20,000k",
                     TRUE ~ NA))

data %>% 
  tabyl(hi, show_na = F)


### Appendix: Figure B

Q1 <- data %>% 
  tabyl(Q7.2, show_na = F) %>% 
  mutate(group = "Question 1") %>% 
  rename(know = Q7.2)
Q2 <- data %>% 
  tabyl(Q7.3, show_na = F) %>% 
  mutate(group = "Question 2")%>% 
  rename(know = Q7.3)
Q3 <- data %>% 
  tabyl(Q7.4, show_na = F) %>% 
  mutate(group = "Question 3")%>% 
  rename(know = Q7.4)

Q4 <- data %>% 
  tabyl(know, show_na = F) %>% 
  mutate(group = "Total number of correct answers")

pk <- rbind(Q1,Q2, Q3, Q4)

app1 <- pk %>% 
  ggplot(aes(know, percent)) + 
  geom_bar(stat = "identity") +
  geom_label(aes(x = know, y = percent, label=sprintf("%.2f", percent * 100))) +
  labs(x = "Choice/Total number", y = "Percent") + 
  theme_bw(base_size = 14) + 
  facet_wrap(~ group, nrow = 2) + 
  scale_y_continuous(breaks = seq(0,0.6, by=.1),limits=c(0,.6),  labels = scales::percent) + 
  scale_x_continuous(breaks = seq(0,5,1))

app1

ggsave("FigB.pdf", app1, dpi = 300, width = 9, height = 5, device = cairo_pdf)