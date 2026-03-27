
# To install and open the R packages that you need for this code. 
need <- c('tidyverse','glue','rdrobust', 'stargazer', 'broom', 'ggthemes',
          'readstata13', 'rddensity', 'rdd', 'stringr',
          'ggplot2', 'readxl','viridis','dplyr', 'gridExtra', 'viridis','lubridate','zoo')
have <- need %in% rownames(installed.packages()) 
if(any(!have)) install.packages(need[!have]) 
invisible(lapply(need, library, character.only=T)) 
# Change path to whereever you place the models
# To set up the working directory. 
script_folder = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(glue('{script_folder}'))
rm(list = ls())

####################################################################

##Note: set the working directory to the one that includes the Excel files exported from the previous programs. 

#setwd("../../../manuscript/tables/rev1_days")

main.est <-  read_excel("DMainTables_rev1.xlsx") 


donut   <- read_excel("DRob_Donut.xlsx")
covar   <- read_excel("DRob_Tables_Covar_rev1.xlsx")
covar$col_index[is.na(covar$col_index==TRUE)] <- 1

tri     <- read_excel("DRobTables_Kernel_rev1.xlsx") 
quadcon <- read_excel("DRob_Tables_Quadratic_rev1.xlsx")



df1 <- main.est %>%
  filter(coef_name=="b" | coef_name=="se") %>%
  filter(vname=="dc_sids" | vname=="dc_sids_oth" | 
         vname=="mor1" | vname=="mor5" | 
         vname=="mor1_exc_sids" | vname=="mor5_exc_sids")   %>%
  spread(key="coef_name", value="v_scalar") %>%
  mutate(spec="1 linear + no control + uniform") 

df2 <- donut %>%
  filter(coef_name=="b" | coef_name=="se") %>%
  filter(vname=="dc_sids" | vname=="dc_sids_oth" | 
           vname=="mor1" | vname=="mor5" | 
           vname=="mor1_exc_sids" | vname=="mor5_exc_sids")   %>%
  spread(key="coef_name", value="v_scalar") %>%
  mutate(spec="2 linear + no control + uniform + donut") 

df3 <- quadcon %>%
  filter(coef_name=="b" | coef_name=="se") %>%
  spread(key="coef_name", value="v_scalar") %>%
  mutate(spec="5 quadratic + no control + uniform") 
  
df4 <- tri %>%
  filter(coef_name=="b" | coef_name=="se") %>%
  spread(key="coef_name", value="v_scalar") %>%
  mutate(spec="4 linear + no control +  triangle") 

df5 <- covar %>%
  filter(coef_name=="b" | coef_name=="se") %>%
  spread(key="coef_name", value="v_scalar") %>%
  mutate(spec="3 linear + with controls + uniform") 


lev.list <-  c("dc_sids","dc_sids_oth","mor1","mor5","mor1_exc_sids","mor5_exc_sids")                      

label.list<- c(
  `dc_sids` = "SIDS mortality",
  `dc_sids_oth` = "SIDS + all unclassified mortality",
  `mor1` = "Infant mortality",
  `mor5` = "Child mortality",
  `mor1_exc_sids` = "Infant mortality excluding SIDS",
  `mor5_exc_sids` = "Child mortality excluding SIDS"
)

g1 <- rbind(df1,df2,df3,df4,df5) %>%
  filter(col_index==2) %>%
  filter(bw==1000 | bw==1250 | bw==1500  | bw==1750 | bw==2000) %>% 
  mutate(vname=factor(vname, levels = lev.list))  %>%
  ggplot() + 
  geom_rect(aes(xmin=1334, xmax=1818, ymin=-Inf, ymax=Inf), 
            fill='#d9d9d9', alpha=0.1) + 
  geom_hline(yintercept=0, linetype="dashed") + 
  geom_linerange(aes(x=bw,  ymax=b+1.96*se, ymin=b-1.96*se, group=spec), color="#525252", 
               position=position_dodge(120)) +
  geom_point(aes(x=bw, y=b,colour=spec), size=3, 
             position=position_dodge(120)) +
  theme_bw() + 
  ylim(c(-40,40)) + 
  scale_x_continuous(breaks = c(1000,1250,1500,1750,2000)) + 
  scale_color_viridis(discrete = T, option = "H",
  #scale_color_manual(values=c("black", "red","blue","#31a354","purple"),
          labels = c("Linear control function \nNo covariates \nUniform kernel\n\n", 
                     "Linear control function \nNo covariates \nUniform kernel\nDonut\n\n",
                     "Linear control function \nWith covariates \nUniform kernel\n\n", 
                     "Linear control function \nNo covariates \nTriangular kernel\n\n", 
                     "Quadratic control function \nNo covariates \nUniform kernel"), 
                      name = "RD Specification") + 
  labs(x="Bandwidth", y="Estimated RD Coefficient + 95% CI") + 
  facet_wrap(vname~., ncol = 2,  labeller = labeller(vname=label.list)) + 
  theme(text=element_text(family="Georgia",  size=12)) + 
  theme(legend.position="right")


ggsave(filename="sensitivity_check.png", g1, width = 1650/170*1.5, height = 1650/170)
