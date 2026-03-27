# load packages
library(readr)
library(dplyr)
library(sandwich)
library(stargazer)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(ggsci)
library(ggthemes)

rm(list = ls())

#---------------------------------
# Table 1 
acceptor_rejector <- read_csv("./datafile/acceptor_rejector.csv")
data <- receiver_rejector %>% 
    mutate(treat = 1 - reject)

source("./code/bal.R")

tb1 <- data.frame(rbind(bal("top100",data)[c(1,2,3,6)],
             bal("avg_pub",data)[c(1,2,3,6)],
             bal("avg_FA_pub",data)[c(1,2,3,6)],
             bal("avg_FA_top10pub",data)[c(1,2,3,6)],
             bal("avg_LA_pub",data)[c(1,2,3,6)],
             bal("avg_LA_top10pub",data)[c(1,2,3,6)],
             bal("grant_nlg",data)[c(1,2,3,6)])) %>% 
    setNames(c("Acceptors","Rejectors","Diff","P_value"))

#---------------------------------
# Fig.1 
cem <- read_csv("./datafile/cem.csv")
source("./code/ceff_figure.R")
p1 <- ceff_figure(data = cem, 
                  output = "pub_cem",
                  dv = c("num_of_pub","Q12_num","Q1_num","top10_num"))

p2 <- ceff_figure(data = cem, 
                  output = "pub_cem_first",
                  dv = c("FA_pub","FA_Q12pub","FA_Q1pub","FA_top10pub"))

p3 <- ceff_figure(data = cem, 
                  output = "pub_cem_last",
                  dv = c("LA_pub","LA_Q12pub","LA_Q1pub","LA_top10pub")) 

p1 <- p1 + scale_y_continuous(breaks = seq(-4,4,1),limits = c(-4,4))
p2 <- p2 + scale_y_continuous(breaks = seq(-4,4,1),limits = c(-4,4))
p3 <- p3 + scale_y_continuous(breaks = seq(-4,4,1),limits = c(-4,4))

Fig1 <- ggarrange(p1, p2, p3, 
                  nrow = 1,
                  labels = c('     A.Publication Count',
                             '  B.First-Authored Publications',
                             '  C.Last-Authored Publications'),
                  hjust = -0.25,
                  common.legend = TRUE,
                  font.label = list(size = 12))
#---------------------------------
# Fig.2
p1 <- ceff_figure_c2(data = cem, 
                     output = "pub_cem",
                     dv = c("num_of_pub","Q12_num","Q1_num","top10_num"))

p2 <- ceff_figure_c2(data = cem, 
                      output = "pub_cem_first",
                      dv = c("FA_pub","FA_Q12pub","FA_Q1pub","FA_top10pub"))

p3 <- ceff_figure_c2(data = cem, 
                      output = "pub_cem_last",
                      dv = c("LA_pub","LA_Q12pub","LA_Q1pub","LA_top10pub"))

p1 <- p1 + scale_y_continuous(breaks = seq(-4,4,1),limits = c(-4,4))
p2 <- p2 + scale_y_continuous(breaks = seq(-4,4,1),limits = c(-4,4))
p3 <- p3 + scale_y_continuous(breaks = seq(-4,4,1),limits = c(-4,4))

Fig2 <- ggarrange(p1, p2, p3,
                  nrow = 1,
                  labels = c('     A.Publication Count',
                             '  B.First-Authored Publications',
                             '  C.Last-Authored Publications'),
                  hjust = -0.25,
                  common.legend = TRUE,
                  font.label = list(size = 12))

#---------------------------------
# Fig.3 is created in excel and its data are from table S16, S21, and S22

#---------------------------------
# Fig. S2 and Table S6
data <- read_csv("./datafile/cov_balance.csv")
tbS6 <- data.frame()
for (i in c("Doctoral_graduation_year",
            "B1","D1",
            "prior_pub",
            "prior_citation",
            "prior_Q12",
            "prior_Q1",
            "prior_top10",
            "prior_FLA_pub")){
    result <- c(i,bal(i,data))
    tbS6 <- rbind(tbS6,result)
}
names(tbS6) <- c("cov","meanx","meany","diff","min","max","p")

tbS6 <- tbS6 %>%   
    mutate(cov = factor(cov, levels = c("B1",
                                        "D1",
                                        "Doctoral_graduation_year",
                                        "prior_pub",
                                        "prior_Q12",
                                        "prior_Q1",
                                        "prior_natureindex_pub",
                                        "prior_FLA_pub",
                                        "prior_citation")[9:1])) %>% 
    arrange(cov) %>%
    mutate(cov = c("B.S. from top 10 univetisities of China",
                   "Ph.D. from top 50 global univetisities",
                   "Doctoral Graduation Year",
                   "Publicaions in all journals",
                   "Publications in top 50% journals",
                   "Publications in top 25% journals",
                   "Publications in top 10% journals",
                   "Firsr/last-authored publications",
                   "Citations")[9:1]) %>% 
    mutate(meanx = as.numeric(meanx),
           meany = as.numeric(meany),
           diff = as.numeric(diff),
           min = as.numeric(min),
           max = as.numeric(max),
           cov_y = 1:9)

FigS2 <- ggplot(data = tbS6[2:9,] %>% 
                       mutate(cov_y = cov_y -1), 
                   aes(x = diff, y = cov_y)) +
    geom_point(size = 1,color = "#3B4992FF") +
    theme_classic2() +
    geom_segment(aes(x = min, xend = max, 
                     y = cov_y, yend = cov_y),size = 0.8) +
    geom_segment(aes(x = min, xend = min, 
                     y = cov_y-0.1, yend = cov_y+0.1),
                 size = 0.8) + 
    geom_segment(aes(x = max, xend = max, 
                     y = cov_y-0.1, yend = cov_y+0.1),
                 size = 0.8) +
    scale_y_continuous(labels = bal1$cov[2:9],breaks = 1:8) +
    scale_x_continuous(breaks = seq(-2,2,0.5),limits = c(-2,2)) +
    geom_vline(xintercept = 0, lwd = 0.2) + 
    ylab("") + 
    xlab("Differences") + 
    labs(title = "Covariate Balance") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(color="black",face="bold",
                                     size=12,family="Times"),
          axis.text.x = element_text(color="black",family="Times",size=12),
          axis.title.x =  element_text(color="black", size=12,family="Times"),
          title = element_text(color="black",face="bold",size=16,
                               family="Times"))

#---------------------------------
# Fig.S3
full <- read_csv("./datafile/fullsample.csv")
rank_plot_raw <- full %>% 
    #filter(!is.na(overall_rank)) %>% 
    filter(treat == 1) %>% 
    select(uniqueID,phd_field_rank,phd_overall_rank) %>% 
    distinct() %>% 
    rename(rank = phd_field_rank,
           overall_rank = phd_overall_rank) %>% 
    tidyr::gather(key = "type",
                  value = "rank",
                  rank,overall_rank) 

rank_plot <- rank_plot_raw %>% 
    mutate(rank_group = case_when(rank <= 20 ~ "1",
                                  rank > 20 & rank <= 50 ~ "2",
                                  rank > 50 & rank <= 100 ~ "3",
                                  rank > 100 & rank <= 200 ~ "4",
                                  rank > 200 & rank <= 400~ "5",
                                  rank > 400~ "6",
                                  is.na(rank) ~ "7",
                                  T ~ as.character(rank)))

rank_plot <- rank_plot %>% 
    filter(!is.na(rank)) %>% 
    semi_join(y = full %>% 
                  filter(treat == 1) %>% 
                  select(uniqueID) %>% 
                  distinct(),
              by = "uniqueID") %>% 
    group_by(type,rank_group) %>% 
    summarise(count = n()) %>% 
    mutate(prop = count /sum(count))

FigS3 <- ggplot(data = rank_plot,
                 aes(x = rank_group,fill = type)) +
    geom_bar(position = "dodge",stat = "identity",
             aes(y = prop),width = 0.5) +
    scale_y_continuous(breaks = seq(0,0.25,0.05),
                       limits = c(0,0.25),
                       labels = c("0%","5%","10%","15%","20%","25%")) + 
    scale_x_discrete(labels=c("1-20","21-50","51-100","101-200",
                              "200-400","400+","not ranked")) + 
    theme_classic2() +
    ylab("Percentage") + 
    xlab("Scimago Ranking") +
    scale_fill_manual(values = c("darkgrey","black"),
                      labels = c("Overall Rank","Field Rank")) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          text = element_text(size = 14),
          legend.text = element_text(size = 14),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14))+
    ggtitle("The distribution of the YTT's doctoral program rankings, full sample")


#---------------------------------
# Fig.S4
full <- read_csv("./datafile/fullsample.csv")
p1 <- ceff_figure(data = full, 
                  output = "pub_full",
                  dv = c("num_of_pub","Q12_num","Q1_num","top10_num"))

p2 <- ceff_figure(data = full, 
                  output = "pub_full_first",
                  dv = c("FA_pub","FA_Q12pub","FA_Q1pub","FA_top10pub"))

p3 <- ceff_figure(data = full, 
                  output = "pub_full_last",
                  dv = c("LA_pub","LA_Q12pub","LA_Q1pub","LA_top10pub")) 

p1 <- p1 + scale_y_continuous(breaks = seq(-4,4,1),limits = c(-4,4))
p2 <- p2 + scale_y_continuous(breaks = seq(-4,4,1),limits = c(-4,4))
p3 <- p3 + scale_y_continuous(breaks = seq(-4,4,1),limits = c(-4,4))

FigS4 <- ggarrange(p1, p2, p3,
                    nrow = 1,
                    labels = c('     A.Publication Count',
                               '  B.First-Authored Publications',
                               '  C.Last-Authored Publications'),
                    hjust = -0.25,
                    common.legend = TRUE,
                    font.label = list(size = 12))


#---------------------------------
# Fig.S5 - S6

data <- cem %>% 
    mutate(diff = year - returnyear,
           dumm_12 = as.numeric(treat*(year - returnyear == -12)),
           dumm_11 = as.numeric(treat*(year - returnyear == -11)),
           dumm_10 = as.numeric(treat*(year - returnyear == -10)),
           dumm_9 = as.numeric(treat*(year - returnyear == -9)),
           dumm_8 = as.numeric(treat*(year - returnyear == -8)),
           dumm_7 = as.numeric(treat*(year - returnyear <= -7)),
           dumm_6 = as.numeric(treat*(year - returnyear == -6)),
           dumm_5 = as.numeric(treat*(year - returnyear == -5)),
           dumm_4 = as.numeric(treat*(year - returnyear == -4)),
           dumm_3 = as.numeric(treat*(year - returnyear == -3)),
           dumm_2 = as.numeric(treat*(year - returnyear == -2)),
           dumm_1 = as.numeric(treat*(year - returnyear == -1)),
           dumm_0 = as.numeric(treat*(year - returnyear == 0)),
           dumm1 = as.numeric(treat*(year - returnyear == 1)),
           dumm2 = as.numeric(treat*(year - returnyear == 2)),
           dumm3 = as.numeric(treat*(year - returnyear == 3)),
           dumm4 = as.numeric(treat*(year - returnyear == 4)),
           dumm5 = as.numeric(treat*(year - returnyear == 5)),
           dumm6 = as.numeric(treat*(year - returnyear == 6)),
           dumm7 = as.numeric(treat*(year - returnyear == 7)),
           dumm8 = as.numeric(treat*(year - returnyear == 8)),
           dumm9 = as.numeric(treat*(year - returnyear == 9)),
           grant_nlg = grant_nlg/1000)

f1 <- as.formula(paste0("grant_nlg"," ~ dumm_12 + dumm_11 + dumm_10 + dumm_9+dumm_8+dumm_7 +
                               dumm_6 + dumm_5 + dumm_4 + dumm_3 + dumm_2 + dumm_1+
                               dumm1 + dumm2 + dumm3 + dumm4 + dumm5 + dumm6 + dumm7 +
                               dumm8+ dumm9 + factor(year) + factor(uniqueID)+factor(diff)"))

mod <- lm(f1,data = data)
coef <- coeftest(mod, vcov = vcovCL(mod, type = "HC0",
                                    cluster = ~uniqueID))

df_tmp <- data.frame(coef[6:17,1:2]) %>% 
    rename(se = Std..Error) %>% 
    mutate(ymin = Estimate - 1.961*se,
           ymax = Estimate + 1.961*se,
           year = c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) #%>% 

FigS5  <- ggplot(data = df_tmp, 
            aes(x = year, y = Estimate)) + 
    geom_segment(aes(x = year, xend = year, 
                     y = ymin, yend = ymax)) +
    geom_segment(aes(x = year-0.06, xend = year+0.06, 
                     y = ymin, yend = ymin))+
    geom_segment(aes(x = year-0.06, xend = year+0.06, 
                     y = ymax, yend = ymax))+
    geom_point(color = "#3B4992FF") +
    scale_x_continuous(breaks = seq(-6,6)) + 
    theme_classic2() +
    geom_hline(yintercept = 0, lwd = 0.2) +
    geom_vline(xintercept = -0.5, lwd = 0.2) + 
    ylab("Research Grant in 1k PPP USD") + 
    xlab("Years to/After Return")


# 
f2 <- as.formula(paste0("teamsize_nlg"," ~ dumm_12 + dumm_11 + dumm_10 + dumm_9+dumm_8+dumm_7 +
                               dumm_6 + dumm_5 + dumm_4 + dumm_3 + dumm_2 + dumm_1+
                               dumm1 + dumm2 + dumm3 + dumm4 + dumm5 + dumm6 + dumm7 +
                               dumm8+ dumm9 + factor(year) + factor(uniqueID)+factor(diff)"))

mod <- lm(f2,data = data)
coef <- coeftest(mod, vcov = vcovCL(mod, type = "HC0",
                                    cluster = ~uniqueID))

df_tmp <- data.frame(coef[6:17,1:2]) %>% 
    rename(se = Std..Error) %>% 
    mutate(ymin = Estimate - 1.96*se,
           ymax = Estimate + 1.96*se,
           year = c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) 

FigS6 <- ggplot(data = df_tmp, 
            aes(x = year, y = Estimate)) + 
    geom_segment(aes(x = year, xend = year, 
                     y = ymin, yend = ymax))+
    geom_segment(aes(x = year-0.06, xend = year+0.06, 
                     y = ymin, yend = ymin))+
    geom_segment(aes(x = year-0.06, xend = year+0.06, 
                     y = ymax, yend = ymax))+
    geom_point(color = "#3B4992FF") +
    scale_x_continuous(breaks = seq(-6,6)) + 
    scale_y_continuous(breaks = seq(0,6)) + 
    theme_classic2() +
    geom_hline(yintercept = 0, lwd = 0.2) +
    geom_vline(xintercept = -0.5, lwd = 0.2) + 
    ylab("Team Size") + 
    xlab("Years to/After Return")

#---------------------------------
# Table S7
cem <- read_csv("./datafile/cem.csv")
tbS7_1 <- cem %>% 
    filter(treat == 1) %>% 
    select(Doctoral_university,uniqueID) %>% 
    distinct() %>% 
    group_by(Doctoral_university) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count))

tbS7_2 <- cem %>% 
    filter(treat == 0) %>% 
    select(Doctoral_university,uniqueID) %>% 
    distinct() %>% 
    group_by(Doctoral_university) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count))

# Table S8
tbS8 <- cem %>% 
    filter(treat == 1) %>% 
    select(Region_bf_rcrt,uniqueID) %>% 
    distinct() %>% 
    group_by(Region_bf_rcrt) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count))

# Table S9
inst_2019_treat <- read_csv("./datafile/inst_2019_treat.csv")
tbS9 <- inst_2019_treat %>% 
    select(uniqueID,treat,inst) %>% 
    distinct() %>% 
    group_by(treat, inst) %>% 
    summarise(no = n()) %>% 
    arrange(inst) %>% 
    select(no,inst) %>% 
    group_by(treat, no) %>% 
    summarise(inst = paste(inst, collapse = "; ")) %>% 
    arrange(desc(treat), desc(no))

#---------------------------------
# Table S11
data <- read_csv("./datafile/offfer_decision.csv")

mod1 <- glm(reject ~ top200+ chinaphd + usphd + 
                recruit_from_us + 
                factor(Discipline) + factor(Cohort),
            data = data,
            family = "binomial")

mod2 <- glm(reject ~ top200 + chinaphd + usphd + 
                recruit_from_us + avg_Q12_num + avg_Q1_num + avg_top10_num+
                factor(Discipline) + factor(Cohort),
            data = data, 
            family = "binomial")

mod3 <- glm(reject ~top200+ chinaphd + usphd + 
                recruit_from_us + 
                avg_FA_Q12pub + avg_FA_Q1pub + avg_FA_top10pub +
                factor(Discipline) + factor(Cohort),
            data = data,
            family = "binomial")

mod4 <- glm(reject ~ top200+ chinaphd + usphd + 
                recruit_from_us + 
                avg_LA_Q12pub + avg_LA_Q1pub + avg_LA_top10pub + 
                factor(Discipline) + factor(Cohort),
            data = data,
            family = "binomial")

mod5 <- glm(reject ~ top200+ chinaphd + usphd + 
                recruit_from_us + 
                avg_FA_Q12pub + avg_FA_Q1pub + avg_FA_top10pub +
                avg_LA_Q12pub + avg_LA_Q1pub + avg_LA_top10pub + 
                factor(Discipline) + factor(Cohort),
            data = data,
            family = "binomial")
mod6 <- glm(reject ~ top200+ chinaphd + usphd + 
                recruit_from_us + PI+ 
                factor(Discipline) + factor(Cohort),
            data = data,
            family = "binomial")

rs1 <- sqrt(diag(vcovCL(mod1, type = "HC0")))
rs2 <- sqrt(diag(vcovCL(mod2, type = "HC0")))
rs3 <- sqrt(diag(vcovCL(mod3, type = "HC0")))
rs4 <- sqrt(diag(vcovCL(mod4, type = "HC0")))
rs5 <- sqrt(diag(vcovCL(mod5, type = "HC0")))
rs6 <- sqrt(diag(vcovCL(mod6, type = "HC0")))

stargazer(mod1,mod2,mod3,mod4,mod5,mod6,
          omit = c("Cohort","Discipline",
                   "Recruit_year","Institution_before_recruit"),
          align = T,
          no.space = T,
          se = list(rs1,rs2,rs3,rs4,rs5,rs6,rs7,rs8,rs9),
          out = "TableS11.html",
          type = "html")

#---------------------------------
# Table S12
source("./code/summary_stat.R")
full <- read_csv("./datafile/fullsample.csv")
cem <- read_csv("./datafile/cem.csv")
summ(full,"TableS12A1.html")
summ(cem,"TableS12A2.html")
summ_v(cem,"TableS12B.html")

#---------------------------------
# Table S13
source("./code/regression.R")
outreg(full, name = "TableS13")

#---------------------------------
# Table S14
outreg(cem, name = "TableS14")

#---------------------------------
# Table S15
outreg(cem, name = "TableS15", 
       control = c("log(teamsize+1)","log(grant+1)"))

#---------------------------------
# Table S16
data <- cem %>% 
    filter(Discipline == "PHYSICS AND MATHEMATICS")
outreg(data,name = "TableS16_1")
data <- cem %>% 
    filter(Discipline %in% c("LIFE SCIENCE","CHEMISTRY"))
outreg(data,name = "TableS16_2")
data <- cem %>% 
    filter(Discipline %in% c("ENVIRONMENTAL AND EARTH SCIENCE",
                             "INFORMATION SCIENCE",
                             "ENGINEERING AND MATERIALS SCIENCE"))
outreg(data,name = "TableS16_3")

#---------------------------------
# Table S17
top20_groupID <- cem %>%
    filter(treat == 1, phd_field_rank <= 20) %>% 
    select(groupID) %>% 
    distinct()

data <- cem %>% 
    semi_join(y = top20_groupID, 
              by = "groupID")
outreg(data,name = "TableS17_1")

data <- cem %>% 
    anti_join(y = top20_groupID, 
              by = "groupID")
outreg(data,name = "TableS17_2")

#---------------------------------
# Table S18
group_alumni <- cem %>% 
    filter(Alumni == 1) %>% 
    select(groupID) %>% 
    distinct()

data <- cem %>% 
    semi_join(y = group_alumni, 
              by = "groupID")
outreg(data,name = "TableS18_1")

data <- cem %>% 
    anti_join(y = group_alumni, 
              by = "groupID")
outreg(data,name = "TableS18_2")

#---------------------------------
# Table S19
group_prof <- cem %>% 
    filter(recruit_position == "Professor") %>% 
    select(groupID) %>% 
    distinct()
data <- cem %>% 
    semi_join(y = group_prof, 
              by = "groupID")
outreg(data,name = "TableS19_1")

data <- cem %>% 
    anti_join(y = group_prof, 
              by = "groupID")
outreg(data,name = "TableS19_2")

#---------------------------------
# Table S20
data <- read_csv("./datafile/control_univ_ri.csv")
outreg(data,name = "TableS20_1")

data <- read_csv("./datafile/control_evertop200.csv")
outreg(data,name = "TableS20_2")

data <- read_csv("./datafile/control_top200.csv")
outreg(data,name = "TableS20_3")

#---------------------------------
# Table S21
group_employer <- cem %>% 
    filter(treat == 1) %>% 
    select(groupID,Recruit_type) %>% 
    distinct()

data <- cem %>% 
    semi_join(y = group_employer %>% filter(Recruit_type == "T2"), 
              by = "groupID")
outreg(data,name = "TableS21_1")

data <- cem %>% 
    semi_join(y = group_employer %>% filter(Recruit_type == "C9?CAS"), 
              by = "groupID")
outreg(data,name = "TableS21_2")

data <- cem %>% 
    semi_join(y = group_employer %>% filter(Recruit_type == "Other"), 
              by = "groupID")
outreg(data,name = "TableS21_3")

#---------------------------------
# Table S22
group_PI<- cem %>% 
    filter(treat == 1,PI == 1) %>% 
    select(groupID) %>% 
    distinct()

data <- cem %>% 
    semi_join(y = group_PI, 
              by = "groupID")
outreg(data,name = "TableS22_1")

data <- cem %>% 
    anti_join(y = group_PI, 
              by = "groupID")
outreg(data,name = "TableS22_2")

#---------------------------------
# Table S23
group_prod<- cem %>% 
    filter(treat == 1,top10_productivity == 1) %>% 
    select(groupID) %>% 
    distinct()

data <- cem %>% 
    semi_join(y = group_prod, 
              by = "groupID")
outreg(data,name = "TableS23_1")

data <- cem %>% 
    anti_join(y = group_prod, 
              by = "groupID")
outreg(data,name = "TableS23_2")

#---------------------------------
# Table S24
data <- read_csv("./datafile/cninclude.csv")
outreg(data,name = "TableS24")

#---------------------------------
# Table S25
ID_count <- full %>% 
    select(groupID,uniqueID) %>% 
    distinct() %>% 
    group_by(groupID) %>% 
    summarise(count = n()) %>% 
    filter(count>1)

data <- full %>% 
    filter(groupID %in% ID_count$groupID)
outreg(data,name = "TableS25")

#---------------------------------
# Table S26
data <- read_csv("./datafile/ytt_vs_stayer.csv")
outreg(data,name = "TableS26")

#---------------------------------
# Table S27
data <- read_csv("./datafile/gt.csv")
outreg(data,name = "TableS27")

#---------------------------------
# Table S28
data <- cem %>% 
    filter(!is.na(Bachelor_university))

ID_count <- data %>% 
    #filter(year <= returnyear) %>% 
    select(groupID,uniqueID) %>% 
    distinct() %>% 
    group_by(groupID) %>% 
    summarise(count = n()) %>% 
    filter(count>1)

data <- data %>% 
    filter(groupID %in% ID_count$groupID) %>% 
    distinct()
outreg(bs, name = "TableS28")

#---------------------------------
# Table S29
data <- cem %>% 
    filter(year >= 2009)
outreg(data, name = "TableS29")

#---------------------------------
# Table S30
data <- cem %>% 
    mutate(post = as.numeric(year > returnyear),
           treat_post = treat * post)
outreg(data, name = "TableS30")

#---------------------------------
# Table S31
data <- read_csv("./datafile/cem_grdy1.csv")
outreg(data, name = "TableS31-1")
data <- read_csv("./datafile/cem_grdy0.csv")
outreg(data, name = "TableS31-2")

#---------------------------------
# Table S32
data <- read_csv("./datafile/cemadvisor.csv")
outreg(data, name = "TableS32")

