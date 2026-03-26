#analyse expanded data 

setwd("D:/Data/workdata/707116/SIDS_RDD/Revision1/Robustness")

Sys.setlocale("LC_ALL","English")
Sys.setenv(LANG="English")


need <- c("tidyverse","glue","stargazer","broom","ggplot2", "extrafont",
          "readxl","here","haven","lfe","lubridate","gridExtra")

invisible(lapply(need,library,character.only=T))

rm(list = ls())

randomized.RD <- read_excel("D:/Data/workdata/707116/SIDS_RDD/Revision1/output/days/Rob_randomization.xlsx")
#randomized.RD <- read_dta("Rob_randomization.dta") 

coefs <- randomized.RD %>% 
  filter(coef_name=="b" | coef_name=="se") %>%
  select(v_scalar, coef_name,dif_relative,med_ym ) %>%
  spread(key=coef_name, value=v_scalar) %>%
  mutate(rd.cutoff = as.Date(med_ym))



g1 <- coefs %>%
  mutate(col.g = case_when(b-2.576*se>0 ~ "Reject Ho", 
                           b+2.576*se<0 ~ "Reject Ho", 
                           TRUE ~ "Fail to Reject Ho")) %>%
  ggplot() + 
  geom_errorbar(aes(ymin=b-2.576*se,ymax=b+2.576*se, x=rd.cutoff), 
                col="grey", linetype="solid",size=0.2, width = 0) +
  geom_point(aes(y=b,x=rd.cutoff,fill=col.g), shape=21,size=2) + 
  geom_vline(xintercept = ymd('1991-12-31'), linetype="dashed",size=0.5) +
  geom_hline(yintercept = 0, linetype="solid",size=0.1) +
  scale_fill_manual(values=c("white","black"), 
                    name="Test result at 1%") +
  ylab("Estimated RD coefficient and 99% CI") + 
  xlab("RD cutoff date") + 
  theme_bw() + 
  theme(text=element_text(family="Georgia", size=10))

ggsave(filename = "../output/days/rob_random.png", 
                   plot= g1, 
                   width=18,height=12, units=c("cm"), device="png",dpi=700)



