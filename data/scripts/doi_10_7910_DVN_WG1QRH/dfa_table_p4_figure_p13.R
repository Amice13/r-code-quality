#### purpose: producing figure n10 #### 

#### installing key packages #### 

list.of.packages = 
  c('readstata13', 'haven', 'tidyverse', 'dplyr', 'estimatr', 'texreg',
    'gridExtra', 'ggthemes', 'wCorr', 'questionr', 'xtable', 'sf', 
    'TAM', 'purrr', 'kable', 'kableExtra', 'wCorr', 'psych',
    'psychTools')
new.packages =  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#### libraries #### 

suppressPackageStartupMessages(
  
  {
    
    library(readstata13)    
    library(haven)
    library(tidyverse)
    library(dplyr)
    library(estimatr)
    library(texreg)
    library(gridExtra)
    library(ggthemes)
    library(wCorr)
    library(questionr)
    library(xtable)
    library(sf)
    library(TAM)
    library(purrr)
    library(kable)
    library(kableExtra)
    library(wCorr)
    library(psych)
    library(psychTools)
    library(knitr)
    
  }
  
)

#### experimental manipulation failure #### 

# reading in omnibus survey data

omni = read_dta("omnibus_survey/omnibus_S21.dta")
omni = omni %>% 
  mutate(treatment = ifelse(m4_RobRom == 2, 1, 0),
         latino = ifelse(Q5 == 3, 1, 0),
         age = abs(as.numeric(Q3) - 1),
         woman = ifelse(Q4 == 1, 1, 0),
         fobo = ifelse(Q17 == 2, 1, 0),
         no_mex = ifelse(Q21 == 13, 1, 0),
         ide = as.numeric(Q42) - 1,
         pi_dem = ifelse(Q43 == 1, 1, 0),
         pi_gop = ifelse(Q43 == 2, 1, 0),
         inc = as.numeric(Q47) - 1,
         worry = abs(as.numeric(Q99) - 7)) %>% 
  mutate(age = age / max(age, na.rm = TRUE),
         ide = ide / max(ide, na.rm = TRUE),
         inc = inc / max(inc, na.rm = TRUE))

omni$worry = omni$worry / max(omni$worry, na.rm = TRUE)
model_exp = lm_robust(worry ~ treatment + 
                        age + woman + fobo + no_mex + 
                        inc + pi_dem + pi_gop + ide,
                      data = omni, subset = latino == 1)
model_exp2 = lm_robust(worry ~ treatment + 
                         age + woman + fobo + no_mex + 
                         inc + pi_dem + pi_gop + ide,
                       data = omni, subset = latino == 1)


model_exp$coefficients[2]
sd(omni$worry, na.rm = TRUE)
mean(omni$worry, na.rm = TRUE)

texreg(l = model_exp, 
       include.ci = FALSE,
       custom.coef.map = list("treatment" = "Threat Treatment"),
       custom.model.names = c("Threat"),
       label = "table:threatexp",
       float.pos = "!htbp",
       caption = "An Experimental Manipulation of Threat Failed",
       caption.above = TRUE,
       include.rmse = FALSE,
       include.adjrs = FALSE)
1 - mean(omni$fobo[omni$latino == 1])
# balcheck 


outsbcheck = 
  c('age', 'woman', 'fobo', 'no_mex', 'ide', 'pi_dem', 'pi_gop', 'inc')
outsbchecklab = 
  c("Age", "Woman", "Foreign", "Mexican", "Ideology", "Democrat", "Republican",
    "Income")
outsbcheckmat = matrix(NA, ncol = 4, nrow = length(outsbcheck)) %>% 
  as.data.frame() %>% 
  `colnames<-` (c("est", "se", "pv", "cov"))

for (i in 1:length(outsbcheck)) {
  
  print(paste0("Iteration ", i))
  
  outmod = 
    lm_robust(as.formula(paste0(outsbcheck[i], "~treatment")),
              data = omni, subset = latino == 1)
  
  outsbcheckmat[i, 1] = outmod$coefficients[2]
  outsbcheckmat[i, 2] = outmod$std.error[2]
  outsbcheckmat[i, 3] = outmod$p.value[2]
  outsbcheckmat[i, 4] = outsbchecklab[i]
  
  
}


outsbcheckmat$cov = factor(outsbcheckmat$cov,
                           levels = rev(outsbchecklab))

outsbcheckmat %>% 
  ggplot() + 
  geom_point(aes(x = cov, y = est),
             size = 2.5) + 
  geom_errorbar(aes(x = cov, 
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se),
                width = 0) + 
  geom_hline(yintercept = 0) + 
  coord_flip() + 
  labs(title = "Balance Check", 
       y = "Coefficient\n(Threat Treatment)",
       x = "Covariate") + 
  theme_tufte()

ggsave(plot = last_plot(), filename = "balcheck.png", width = 4, height = 3)
