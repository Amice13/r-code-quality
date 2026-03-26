#Title: Understanding Opposition to Apartment Buildings
#Author: Martin Vinæs Larsen and Niels Nyholt 

#File purpose: This code is makes the analysis for Understanding Opposition to Apartment Buildings
#NB: Set working directory to the folder where you store "kom_data.dta".

setwd("")
rm(list=ls())

figures.save <- "./results"

library(tidyverse) #version 2.0.0
library(haven)     #version 2.5.4
library(cregg)     #version 0.4.0
library(cowplot)   #version 1.1.2
library(ggh4x)     #version 0.2.7
library(survey)    #version 4.2.1
library(kableExtra)#version 1.3.4
library(fixest)    #version 0.11.2

#Read data####
kom_data <- read_dta("kom_data.dta")
##Recoding variables, transforming and tidying data for analysis####
kom_data <- kom_data %>% 
  mutate(
    gender2 = ifelse(gender == 2, 1,0),
    age = 2023-birthyear,
    edu = factor(ifelse(uddannelse==1, 1, ifelse(uddannelse %in% c(2, 3), 2, ifelse(uddannelse %in% c(4), 3, ifelse(uddannelse %in% c(5), 4, ifelse(uddannelse %in% c(6, 7), 5, ifelse(uddannelse %in% c(8, 9), 6, NA)))))), 
                 labels = c("Primary school", "High school", "Vocational education", "Short-cycle tertiary education", "Bachelors or equivalent level", "Masters or longer")),
    edu2 = ifelse(edu %in% c("Bachelors or equivalent level", "Masters or longer"), 1,0),
    homeowner=factor(ifelse(Q6 %in% c(1, 2), 1, 2), levels = c(1, 2), label = c("homeowner", "non-homeowner")),
    homeowner2=ifelse(Q6 %in% c(1, 2), 1, 0),
    rural=factor(ifelse(c_Kommune %in% c("Københavns Kommune", "Frederiksberg Kommune", "Tårnby Kommune", "Dragør Kommune","Gentofte Kommune", "Lyngby Kommune", "Gladsaxe Kommune", "Rødovre Kommune","Hvidovre Kommune", "Brøndby Kommune", "Taastrup Kommune","Ballerup Kommune", "Aarhus Kommune", "Odense Kommune", "Aalborg Kommune"), 1, 2), 
                 labels = c("Big city", "Not big city")),
    rural2=ifelse(rural=="Big city", 1,0),
    b.type = factor(V2_X_rand, labels = c("social housing", "own-occ housing", "rental housing", "office premises", "retail premises")),
    b.size = factor(V2_Y1_rand, labels = c("500", "1.000", "10.000")),
    b.height = factor(ifelse(V2_Y2_rand %in% c(1, 5), 1,
                             ifelse(V2_Y2_rand %in% c(2, 6), 2,
                                    ifelse(V2_Y2_rand %in% c(3, 7), 3, 
                                           ifelse(V2_Y2_rand %in% c(4, 8), 4, NA)))), labels = c("1", "3", "5", "7")),
    opp3=ifelse(V2==8, 0, ifelse(V2 %in% c(1,2,3), 1, 0)),
    conatt2=ifelse(V4_10_resp == 6, 0, ifelse(V4_10_resp %in% c(4, 5), 1, 0)),
    exatt2=ifelse(V4_13_resp == 6, 0, ifelse(V4_13_resp %in% c(4, 5), 1, 0)),
    fitatt2=ifelse(V4_16_resp == 6, 0, ifelse(V4_16_resp %in% c(4, 5), 1, 0)),
    hpratt2=ifelse(V4_1_resp == 6, 0, ifelse(V4_1_resp %in% c(4,5), 1, 0)),
    a.tall=factor(ifelse(bbr_etager_5>0, 1,0), labels = c("No 5+ story building in area", "5+ story building in area")),
    a.tall2=factor(ifelse(bbr_etager_7>0, 1,0), labels = c("No 7+ story building in area", "7+ story building in area")),
    pfitweight = ifelse(fivestories==0 | fivestories==1, 0, (1/fivestories)*ifelse(a.tall=="5+ story building in area",1,0)+(1/(1-fivestories))*(1-ifelse(a.tall=="5+ story building in area",1,0))),
    pfitweight7 = ifelse(sevenstories==0 | sevenstories==1, 0, (1/sevenstories)*ifelse(a.tall2=="7+ story building in area",1,0)+(1/(1-sevenstories))*(1-ifelse(a.tall2=="7+ story building in area",1,0))),
    weightdf7 = pfitweight7 * dweight,
    weightdf = pfitweight * dweight)

#Figure 2: Opposition across the type, size, and height of the development.####
##A. Estimate models####
fig2 <- cj(kom_data, opp3 ~ b.type + b.size + b.height, weight = ~ dweight, estimate = "mm") #Marginal Means of project attributes
fortext <- cj(kom_data, opp3 ~ b.type + b.size + b.height, weight = ~ dweight, estimate = "amce") #AMCE estimates for text

##B. Tidy data from model for plotting####
plot_fig2 <- fig2 %>% 
  mutate(level = factor(level, 
                        levels = c("office premises", "retail premises",
                                   "social housing", "rental housing",  "own-occ housing",
                                   "500", "1.000", "10.000",
                                   "1", "3", "5", "7"),     
                        labels = c("office premises", "retail premises",
                                   "social housing", "rental housing",  "own-occ housing",
                                   "500 sqm", "1.000 sqm", "10.000 sqm",
                                   "one story", "three stories", "five stories", "seven stories")))

##C. Create plot####
fig2_plot <- ggplot(data = plot_fig2, aes(x=estimate, y = level, xmin =  lower, xmax = upper))+
  geom_vline(xintercept = weighted.mean(kom_data$opp3, w=kom_data$dweight, na.rm = T), color = "grey")+
  geom_point(position = position_dodge(0.5))+
  geom_linerange(position = position_dodge(0.5))+
  facet_grid(rows = vars(feature), scales = "free_y", space='free')+
  scale_x_continuous(name="Opposition %", breaks= seq(0, 1, 0.05),  limits =c(0.20,0.5), labels = c(seq(0, 100, 5)))+
  scale_y_discrete(name=element_blank())+
  theme_classic()+
  theme(text = element_text(size = 9),
        strip.text.y = element_blank(),
        axis.text = element_text(size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.line = element_line(color = "black", lineend = 'square'),
        axis.ticks = element_line(color = "black", lineend = 'square')
        )

##D. Export plot for text####
ggsave(paste0(figures.save, "/fig2_w.pdf"), fig2_plot,
       dpi = 600,
       scale = 1,
       width = 8,
       height = 7,
       units = c("cm")
)
#Figure 3: Beliefs about the effects of the development on the local area across the type, size, and height of the development.####
##A. Estimate models####
conatt <- cj(kom_data, conatt2 ~ b.type + b.size + b.height, weight = ~ dweight, estimate = "mm") #Marginal Means of project attributes
exatt <- cj(kom_data, exatt2 ~ b.type + b.size + b.height, weight = ~ dweight, estimate = "mm") #Marginal Means of project attributes
fitatt <- cj(kom_data, fitatt2 ~ b.type + b.size + b.height, weight = ~ dweight, estimate = "mm") #Marginal Means of project attributes
##B. Tidy data from model for plotting####
plot_fig3 <- conatt %>% 
  bind_rows(exatt, fitatt) %>% 
  mutate(level = factor(level, 
                        levels = c("office premises", "retail premises",
                                   "social housing", "rental housing",  "own-occ housing",
                                   "500", "1.000", "10.000",
                                   "1", "3", "5", "7"),     
                        labels = c("office premises", "retail premises",
                                   "social housing", "rental housing",  "own-occ housing",
                                   "500 sqm", "1.000 sqm", "10.000 sqm",
                                   "one story", "three stories", "five stories", "seven stories")),
        test = factor(paste0(level, "&", feature),
         levels = c("one story&b.height", 
                "three stories&b.height",
                "five stories&b.height",
                "seven stories&b.height",
                "500 sqm&b.size" ,
                "1.000 sqm&b.size",
                "10.000 sqm&b.size", 
                "office premises&b.type",
                "retail premises&b.type",
                "social housing&b.type",
                "rental housing&b.type",
                "own-occ housing&b.type")),
        outcome2 = factor(outcome, levels = c("conatt2", "exatt2",  "fitatt2"), labels = c("Agree project will create\nmore congestion %",
                                                            "Agree project attracts\nundesirable residents %",
                                                            "Agree project fits\npoorly in the area %"))
         )

##C. Create plot (in this case we combine three plots to keep the same formatting)####
p1 <- ggplot(data = subset(plot_fig3, feature =="b.type"), aes(x=estimate, y =level , xmin =  lower, xmax = upper))+
  geom_vline(data = data.frame(xint= weighted.mean(kom_data$conatt2, w=kom_data$dweight, na.rm = T), outcome2 =factor("Agree project will create\nmore congestion %", levels = c("Agree project will create\nmore congestion %", "Agree project attracts\nundesirable residents %", "Agree project fits\npoorly in the area %"), labels = c("Agree project will create\nmore congestion %","Agree project attracts\nundesirable residents %","Agree project fits\npoorly in the area %"))),
             aes(xintercept = xint), color= "grey")+
  geom_vline(data = data.frame(xint= weighted.mean(kom_data$exatt2, w=kom_data$dweight, na.rm = T), outcome2 =factor("Agree project attracts\nundesirable residents %", levels = c("Agree project will create\nmore congestion %", "Agree project attracts\nundesirable residents %", "Agree project fits\npoorly in the area %"), labels = c("Agree project will create\nmore congestion %","Agree project attracts\nundesirable residents %","Agree project fits\npoorly in the area %"))),
             aes(xintercept = xint), color= "grey")+
  geom_vline(data = data.frame(xint= weighted.mean(kom_data$fitatt2, w=kom_data$dweight, na.rm = T), outcome2 =factor("Agree project fits\npoorly in the area %", levels = c("Agree project will create\nmore congestion %", "Agree project attracts\nundesirable residents %", "Agree project fits\npoorly in the area %"), labels = c("Agree project will create\nmore congestion %","Agree project attracts\nundesirable residents %","Agree project fits\npoorly in the area %"))),
             aes(xintercept = xint), color= "grey")+
  geom_point(position = position_dodge(0.5))+
  geom_linerange(position = position_dodge(0.5))+
  facet_wrap2(vars(outcome2), axes = "all", scales = "free", remove_labels = "y")+
  facetted_pos_scales(
    x = list(
    outcome2 == "Agree project will create\nmore congestion %" ~ scale_x_continuous(name=" ", breaks= seq(0, 1, 0.05), limits = c(0.27,0.53), labels = NULL),
    outcome2 == "Agree project attracts\nundesirable residents %" ~ scale_x_continuous(name=" ", breaks= seq(0, 1, 0.05), limits = c(0.07,0.33), labels = NULL),
    outcome2 == "Agree project fits\npoorly in the area %" ~ scale_x_continuous(name=" ", breaks= seq(0, 1, 0.05), limits = c(0.24,0.50), labels = NULL)),
    y = list(
      outcome2 == "Agree project will create\nmore congestion %" ~ scale_y_discrete(name=element_blank()),
      outcome2 == "Agree project attracts\nundesirable residents %" ~ scale_y_discrete(name=element_blank(), labels = NULL),
      outcome2 == "Agree project fits\npoorly in the area %" ~ scale_y_discrete(name=element_blank(),labels = NULL))
    )+
  theme_classic()+
  theme(text = element_text(size = 9),
        strip.text.y = element_blank(),
        axis.line.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.title.x=element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_text(size = 9),
        axis.ticks = element_line(color = "black", lineend = 'square'),
        plot.margin = margin(b = 0,  unit = "mm")) 


p2<- ggplot(data = subset(plot_fig3, feature =="b.size"), aes(x=estimate, y =level , xmin =  lower, xmax = upper))+
  geom_vline(data = data.frame(xint= weighted.mean(kom_data$conatt2, w=kom_data$dweight, na.rm = T), outcome2 =factor("Agree project will create\nmore congestion %", levels = c("Agree project will create\nmore congestion %", "Agree project attracts\nundesirable residents %", "Agree project fits\npoorly in the area %"), labels = c("Agree project will create\nmore congestion %","Agree project attracts\nundesirable residents %","Agree project fits\npoorly in the area %"))),
             aes(xintercept = xint), color= "grey")+
  geom_vline(data = data.frame(xint= weighted.mean(kom_data$exatt2, w=kom_data$dweight, na.rm = T), outcome2 =factor("Agree project attracts\nundesirable residents %", levels = c("Agree project will create\nmore congestion %", "Agree project attracts\nundesirable residents %", "Agree project fits\npoorly in the area %"), labels = c("Agree project will create\nmore congestion %","Agree project attracts\nundesirable residents %","Agree project fits\npoorly in the area %"))),
             aes(xintercept = xint), color= "grey")+
  geom_vline(data = data.frame(xint= weighted.mean(kom_data$fitatt2, w=kom_data$dweight, na.rm = T), outcome2 =factor("Agree project fits\npoorly in the area %", levels = c("Agree project will create\nmore congestion %", "Agree project attracts\nundesirable residents %", "Agree project fits\npoorly in the area %"), labels = c("Agree project will create\nmore congestion %","Agree project attracts\nundesirable residents %","Agree project fits\npoorly in the area %"))),
             aes(xintercept = xint), color= "grey")+
  geom_point(position = position_dodge(0.5))+
  geom_linerange(position = position_dodge(0.5))+
  facet_wrap2(vars(outcome2), axes = "all", scales = "free", remove_labels = "y")+
  facetted_pos_scales(
    x = list(
      outcome2 == "Agree project will create\nmore congestion %" ~ scale_x_continuous(name=" ", breaks= seq(0, 1, 0.05), limits = c(0.27,0.53), labels = NULL),
      outcome2 == "Agree project attracts\nundesirable residents %" ~ scale_x_continuous(name=" ", breaks= seq(0, 1, 0.05), limits = c(0.07,0.33), labels = NULL),
      outcome2 == "Agree project fits\npoorly in the area %" ~ scale_x_continuous(name=" ", breaks= seq(0, 1, 0.05), limits = c(0.24,0.50), labels = NULL)),
    y = list(
      outcome2 == "Agree project will create\nmore congestion %" ~ scale_y_discrete(name=element_blank()),
      outcome2 == "Agree project attracts\nundesirable residents %" ~ scale_y_discrete(name=element_blank(), labels = NULL),
      outcome2 == "Agree project fits\npoorly in the area %" ~ scale_y_discrete(name=element_blank(),labels = NULL))
    
  )+
  theme_classic()+
  theme(text = element_text(size = 9),
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.line.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.title.x=element_blank(),
        strip.text.x = element_blank(),
        axis.ticks = element_line(color = "black", lineend = 'square'),
        plot.margin = margin(t = 2, b = 2, unit = "pt")) 

p3 <- ggplot(data = subset(plot_fig3, feature =="b.height"), aes(x=estimate, y =level , xmin =  lower, xmax = upper))+
  geom_vline(data = data.frame(xint= weighted.mean(kom_data$conatt2, w=kom_data$dweight, na.rm = T), outcome2 =factor("Agree project will create\nmore congestion %", levels = c("Agree project will create\nmore congestion %", "Agree project attracts\nundesirable residents %", "Agree project fits\npoorly in the area %"), labels = c("Agree project will create\nmore congestion %","Agree project attracts\nundesirable residents %","Agree project fits\npoorly in the area %"))),
             aes(xintercept = xint), color= "grey")+
  geom_vline(data = data.frame(xint= weighted.mean(kom_data$exatt2, w=kom_data$dweight, na.rm = T), outcome2 =factor("Agree project attracts\nundesirable residents %", levels = c("Agree project will create\nmore congestion %", "Agree project attracts\nundesirable residents %", "Agree project fits\npoorly in the area %"), labels = c("Agree project will create\nmore congestion %","Agree project attracts\nundesirable residents %","Agree project fits\npoorly in the area %"))),
             aes(xintercept = xint), color= "grey")+
  geom_vline(data = data.frame(xint= weighted.mean(kom_data$fitatt2, w=kom_data$dweight, na.rm = T), outcome2 =factor("Agree project fits\npoorly in the area %", levels = c("Agree project will create\nmore congestion %", "Agree project attracts\nundesirable residents %", "Agree project fits\npoorly in the area %"), labels = c("Agree project will create\nmore congestion %","Agree project attracts\nundesirable residents %","Agree project fits\npoorly in the area %"))),
             aes(xintercept = xint), color= "grey")+
  geom_point(position = position_dodge(0.5))+
  geom_linerange(position = position_dodge(0.5))+
  facet_wrap2(vars(outcome2), axes = "all", scales = "free", remove_labels = "y", strip.position = "bottom")+
  facetted_pos_scales(
      x = list(
        outcome2 == "Agree project will create\nmore congestion %" ~ scale_x_continuous(name = element_blank(), breaks= seq(0, 1, 0.05), limits = c(0.27,0.53), labels = c(seq(0, 100, 5))),
        outcome2 == "Agree project attracts\nundesirable residents %" ~ scale_x_continuous( breaks= seq(0, 1, 0.05), limits = c(0.07,0.33), labels = c(seq(0, 100, 5))),
        outcome2 == "Agree project fits\npoorly in the area %" ~ scale_x_continuous(breaks= seq(0, 1, 0.05), limits = c(0.24,0.50), labels = c(seq(0, 100, 5)))),
      y = list(
        outcome2 == "Agree project will create\nmore congestion %" ~ scale_y_discrete(name=element_blank()),
        outcome2 == "Agree project attracts\nundesirable residents %" ~ scale_y_discrete(name=element_blank(), labels = NULL),
        outcome2 == "Agree project fits\npoorly in the area %" ~ scale_y_discrete(name=element_blank(),labels = NULL))
    )+
  theme_classic()+
  theme(text = element_text(size = 9),
        strip.text.y = element_blank(),
        strip.placement = "outside",
        strip.background=element_rect(fill="white", linetype = "blank"),
        strip.switch.pad.grid = unit(0, "pt"),
        strip.text.x = element_text(size = 9, face = "bold", margin = margin( b = 0, t = 0)),
        axis.text = element_text(size = 9),
        axis.line = element_line(color = "black", lineend = 'square'),
        axis.ticks = element_line(color = "black", lineend = 'square'),
        plot.margin = margin(t = 0, b = 0, unit = "mm")) 
        
fig3_plot <- plot_grid(p1, p2, p3, ncol =1 , align = c("v"), rel_heights =  c(0.8, 0.48,1))

##D. Export plot for text####
ggsave(paste0(figures.save, "/fig3_w.pdf"), fig3_plot,
       dpi = 600,
       scale = 1,
       width = 15,
       height = 7,
       units = c("cm")
)

#Figure 4: Opposition and neighborhood fit across the height of the project for areas where there is at least one 5+ story building and areas with no 5+ story buildings####
kom_data_fig4 <- kom_data %>% 
  subset(fivestories!=0) %>% #Need to remove never takers 6158
  subset(fivestories!=1)  #Need to remove allways takers 48

##A. Estimate models####
fig4_opp <- cj(kom_data_fig4, opp3 ~ b.height,  by = ~a.tall, weights = ~ weightdf, estimate = "mm") %>% #Marginal Means of project attributes
  mutate(name = "Weighted") #Using inverse probability weighting

fig4_opp_2 <- cj(kom_data_fig4, opp3 ~ b.height,  by = ~a.tall, weights = ~ dweight, estimate = "mm") %>% #Marginal Means of project attributes
  mutate(name = "Non-weighted") #Using standard design weights

fig4_fit <- cj(kom_data_fig4, fitatt2 ~ b.height,  by = ~a.tall, weights = ~ weightdf, estimate = "mm") %>% #Marginal Means of project attributes
  mutate(name = "Weighted") #Using inverse probability weighting

fig4_fit_2 <- cj(kom_data_fig4, fitatt2 ~ b.height,  by = ~a.tall, weights = ~ dweight, estimate = "mm") %>% #Marginal Means of project attributes
  mutate(name = "Non-weighted") #Using standard design weights

#For text analysis
amceopp <- cj(kom_data_fig4, opp3 ~ b.height,  by = ~a.tall, weights = ~ weightdf, estimate = "amce_difference") #Difference in AMCE over a.tall of project attributes
amceopp_2 <- cj(kom_data_fig4, opp3 ~ b.height,  by = ~a.tall, weights = ~ dweight, estimate = "amce_difference") #Difference in AMCE over a.tall of project attributes
amcefit <- cj(kom_data_fig4, fitatt2 ~ b.height,  by = ~a.tall, weights = ~ weightdf, estimate = "amce_difference") #Difference in AMCE over a.tall of project attributes
amcefit_2 <- cj(kom_data_fig4, fitatt2 ~ b.height,  by = ~a.tall, weights = ~ dweight, estimate = "amce_difference") #Difference in AMCE over a.tall of project attributes


##B. Tidy data from model for plotting####
plot_fig4_opp <- fig4_opp %>% 
  bind_rows(fig4_opp_2) %>% 
  mutate(height = factor(level, levels = c("1", "3", "5", "7"), labels = c("one story", "three stories", "five stories", "seven stories")),
         tall = factor(a.tall, levels = c("5+ story building in area", "No 5+ story building in area")))


plot_fig4_fit <- fig4_fit %>% 
  bind_rows(fig4_fit_2) %>% 
  mutate(height = factor(level, levels = c("1", "3", "5", "7"), labels = c("one story", "three stories", "five stories", "seven stories")),
         tall = factor(a.tall, levels = c("5+ story building in area", "No 5+ story building in area")))

##C. Create plots####
fig4_plot_opp <- ggplot(data = plot_fig4_opp , aes(x=estimate, y = height, xmin =  lower, xmax = upper, group = name, color = name))+
  geom_vline(xintercept =  weighted.mean(kom_data_fig4$opp3, w=kom_data_fig4$dweight, na.rm = T), color = "grey")+
  geom_point(position = position_dodge(0.5))+
  geom_linerange(position = position_dodge(0.5))+
  scale_colour_manual(values = c("black", "grey60"))+
  facet_wrap2(vars(tall), axes = "all", remove_labels = "y")+
  scale_x_continuous(name="Opposition %", breaks= seq(0, 1, 0.1), limits =c(0.11,0.63), labels = c(seq(0, 100, 10)))+
  scale_y_discrete(name=element_blank())+
  theme_classic()+
  theme(text = element_text(size = 9),
        strip.text.y = element_blank(),
        axis.text = element_text(size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        strip.background=element_rect(fill="white", linetype = "blank"),
        strip.text = element_text(size = 9, color = "black", face = "bold", hjust = 0),
        axis.line = element_line(color = "black", lineend = 'square'),
        axis.ticks = element_line(color = "black", lineend = 'square'),
        legend.position = "none"
  )

fig4_plot_fit <- ggplot(data = plot_fig4_fit , aes(x=estimate, y = height, xmin =  lower, xmax = upper, group = name, color = name))+
  geom_vline(xintercept =  weighted.mean(kom_data_fig4$fitatt2, w=kom_data_fig4$dweight, na.rm = T), color = "grey")+
  geom_point(position = position_dodge(0.5))+
  geom_linerange(position = position_dodge(0.5))+
  scale_colour_manual(values = c("black", "grey60"))+
  facet_wrap2(vars(tall), axes = "all", remove_labels = "y")+
  scale_x_continuous(name="Agree project fits poorly in area %", breaks= seq(0, 1, 0.1), limits =c(0.11,0.63), labels = c(seq(0, 100, 10)))+
  scale_y_discrete(name=element_blank())+
  theme_classic()+
  theme(text = element_text(size = 9),
        strip.text.y = element_blank(),
        axis.text = element_text(size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        strip.background=element_rect(fill="white", linetype = "blank"),
        strip.text = element_text(size = 9, color = "black", face = "bold", hjust = 0),
        axis.line = element_line(color = "black", lineend = 'square'),
        axis.ticks = element_line(color = "black", lineend = 'square'),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm")

  )
plot_grid(fig4_plot_opp, fig4_plot_fit, ncol =1 , rel_heights =  c(0.9, 1.1))

##D. Export plot for text####
ggsave(paste0(figures.save, "/fig4_w.pdf"), last_plot(),
       dpi = 600,
       scale = 1,
       width = 12,
       height = 11,
       units = c("cm")
)

#Figure 5: Beliefs about the effects of the development on home values across the type, size, and height of the development####
kom_data_fig5 <- kom_data %>% 
  subset(homeowner != "non-homeowner") 

##A. Estimate models####
opp_home_cheaper_amce <- cj(kom_data_fig5 , hpratt2 ~ b.type + b.size + b.height, weight = ~ dweight, estimate = "amce") #Marginal Means of project attributes
fig5      <- cj(kom_data_fig5 , hpratt2 ~ b.type + b.size + b.height, weight = ~ dweight, estimate = "mm") #Marginal Means of project attributes

#For text 
svyciprop(~hpratt2, svydesign(ids=~1, data=kom_data_fig5, weights = ~ dweight))

##B. Tidy data from model for plotting####
plot_fig5 <- fig5 %>% 
  mutate(level = factor(level, 
                        levels = c("office premises", "retail premises",
                                   "social housing", "rental housing",  "own-occ housing",
                                   "500", "1.000", "10.000",
                                   "1", "3", "5", "7"),     
                        labels = c("office premises", "retail premises",
                                   "social housing", "rental housing",  "own-occ housing",
                                   "500 sqm", "1.000 sqm", "10.000 sqm",
                                   "one story", "three stories", "five stories", "seven stories")))

##C. Create plot####
fig5_plot <- ggplot(data = plot_fig5, aes(x=estimate, y = level, xmin =  lower, xmax = upper))+
  geom_vline(xintercept = weighted.mean(kom_data_fig5$hpratt2, w=kom_data_fig5$dweight, na.rm = T), color = "grey")+
  geom_point(position = position_dodge(0.5))+
  geom_linerange(position = position_dodge(0.5))+
  facet_grid(rows = vars(feature), scales = "free_y", space='free')+
  scale_x_continuous(name="Agree project will decrease\nresp. home value %", breaks= seq(0, 1, 0.05),  limits =c(0.00,0.26), labels = c(seq(0, 100, 5)))+
  scale_y_discrete(name=element_blank())+
  theme_classic()+
  theme(text = element_text(size = 9),
        strip.text.y = element_blank(),
        axis.text = element_text(size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.line = element_line(color = "black", lineend = 'square'),
        axis.ticks = element_line(color = "black", lineend = 'square')
  )
##D. Export plot for text####
ggsave(paste0(figures.save, "/fig5_w.pdf"), fig5_plot,
       dpi = 600,
       scale = 1,
       width = 8,
       height = 7,
       units = c("cm")
)
#Figure 6: Opposition across height for homeowners and renters.####
##A. Estimate models####
fig6 <- cj(kom_data, opp3 ~ b.height,  by = ~ homeowner, weights = ~ dweight, estimate = "mm") #Marginal Means of project attributes

table(kom_data$homeowner)
cj(kom_data, opp3 ~  b.height, by = ~homeowner, weight = ~ dweight, estimate = "amce_difference") #Marginal Means of project attributes
##B. Tidy data from model for plotting####
plot_fig6 <- fig6 %>% 
  mutate(height = factor(level, levels = c("1", "3", "5", "7"), labels = c("one story", "three stories", "five stories", "seven stories")),
         homeowner2  =factor(homeowner, levels = c("non-homeowner", "homeowner"), labels = c("Do not own home", "Homeowners")))
##C. Create plot####
fig6_plot <- ggplot(data = plot_fig6, aes(x=estimate, y = height, xmin =  lower, xmax = upper))+
  geom_vline(xintercept =  weighted.mean(kom_data$opp3, w=kom_data$dweight, na.rm = T), color = "grey")+
  geom_point()+
  geom_linerange()+
  facet_wrap2(vars(homeowner2), axes = "all", remove_labels = "y")+
  scale_x_continuous(name="Opposition %", breaks= seq(0, 1, 0.05), limits =c(0.20,0.5), labels = c(seq(0, 100, 5)))+
  scale_y_discrete(name=element_blank())+
  theme_classic()+
  theme(text = element_text(size = 9),
        strip.text.y = element_blank(),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 9),
        strip.background=element_rect(fill="white", linetype = "blank"),
        strip.text = element_text(size = 9, color = "black", face = "bold", hjust = 0),
        axis.line = element_line(color = "black", lineend = 'square'),
        axis.ticks = element_line(color = "black", lineend = 'square')
  )
##D. Export plot for text####
ggsave(paste0(figures.save, "/fig6_w.pdf"), fig6_plot,
       dpi = 600,
       scale = 1,
       width = 12,
       height = 5,
       units = c("cm")
)

#Appendix A####
app_a_samp <- tibble()
app_a_samp[1,1] <- "Total sample"
app_a_samp[2,1] <- "No map"
app_a_samp[3,1] <- "With map and type of project:"
app_a_samp[4,1] <- paste0("   ", "a public institution")
app_a_samp[5,1] <- paste0("   ", "factory premises")
app_a_samp[6,1] <- paste0("   ", "a biogas plant")
app_a_samp[7,1] <- paste0("   ", "a sewage plant")
app_a_samp[8,1] <- paste0("   ", "own-occ housing")
app_a_samp[9,1] <- paste0("   ", "rental housing")
app_a_samp[10,1] <- paste0("   ", "social housing")
app_a_samp[11,1] <- paste0("   ", "retail premises")
app_a_samp[12,1] <- paste0("   ", "office premises")
app_a_samp[13,1] <- "Total with a map"
app_a_samp[14,1] <- "Total included in analysis"

app_a_samp[1,2] <- formatC(as.integer(28850) , big.mark = ",") #Inserted from full sample
app_a_samp[2,2] <- formatC(length(kom_data$dweight),big.mark = ",")
app_a_samp[3,2] <- ""
app_a_samp[4,2] <- formatC(as.integer(2530), big.mark = ",")
app_a_samp[5,2] <- formatC(as.integer(2549), big.mark = ",")
app_a_samp[6,2] <- formatC(as.integer(2611), big.mark = ",")
app_a_samp[7,2] <- formatC(as.integer(2531), big.mark = ",")
app_a_samp[8,2] <- formatC(length(subset(kom_data, b.type==levels(kom_data$b.type)[2])$dweight), big.mark = ",")
app_a_samp[9,2] <- formatC(length(subset(kom_data, b.type==levels(kom_data$b.type)[3])$dweight), big.mark = ",")
app_a_samp[10,2] <- formatC(length(subset(kom_data, b.type==levels(kom_data$b.type)[1])$dweight), big.mark = ",")
app_a_samp[11,2] <- formatC(length(subset(kom_data, b.type==levels(kom_data$b.type)[5])$dweight), big.mark = ",")
app_a_samp[12,2] <- formatC(length(subset(kom_data, b.type==levels(kom_data$b.type)[4])$dweight), big.mark = ",")
app_a_samp[13,2] <- formatC(as.integer(23261), big.mark = ",")
app_a_samp[14,2] <- formatC(length(kom_data$dweight) ,big.mark = ",")

app_a_samp[1,3] <- ""
app_a_samp[2,3] <- "Not included"
app_a_samp[3,3] <- ""
app_a_samp[4,3] <- "Not included"
app_a_samp[5,3] <- "Not included"
app_a_samp[6,3] <- "Not included"
app_a_samp[7,3] <- "Not included"
app_a_samp[8,3] <- "Included"
app_a_samp[9,3] <- "Included"
app_a_samp[10,3] <- "Included"
app_a_samp[11,3] <- "Included"
app_a_samp[12,3] <- "Included"
app_a_samp[13,3] <- ""
app_a_samp[14,3] <- "Included"

names(app_a_samp)[1] <- "Subset"
names(app_a_samp)[2] <- "N"
names(app_a_samp)[3] <- "Part of analysis"


kbl(app_a_samp, caption = "Sample and subsets by treatment status", label = "tab_sample",
    booktabs = T, 
    format = "latex",
    linesep = c("\\addlinespace",
                "\\addlinespace",
                "", "", "", "", "",
                "", "", "", "", 
                "\\addlinespace",
                "\\addlinespace"),
    align = "lrr") %>%
  kable_styling(latex_options = c("hold_position",  "condensed", "striped"),
                position = "center",
                
                font_size = 11) %>% 
  column_spec(1, width = paste0(0.30*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(2, width = paste0(0.06*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(3, width = paste0(0.16*452.9679,"pt"), latex_valign = "m") %>% 
  save_kable("tab_sample.tex")

#Appendix B####
app_b_discr <- tibble()
app_b_design <-  svydesign(ids=~1, data=kom_data, weights = ~ dweight)

app_b_discr[1,1] <- "Do you support or oppose the proposed development project? (0-1)"
app_b_discr[2,1] <- "The project will increase congestion (0-1)"
app_b_discr[3,1] <- "The project will attract people to my neighborhood I wish to avoid (0-1)"
app_b_discr[4,1] <- "The project would not fit well into the area (0-1)"
app_b_discr[5,1] <- "The project will decrease the value of my home (0-1)"
app_b_discr[6,1] <- "5+ story building in area (0-1)"
app_b_discr[7,1] <- "Share homeowners"

names(app_b_discr)[1] <- "Variable"

app_b_discr[1,2] <- formatC(length(kom_data$opp3), big.mark = ",")
app_b_discr[2,2] <- formatC(length(kom_data$conatt2), big.mark = ",")
app_b_discr[3,2] <- formatC(length(kom_data$exatt2), big.mark = ",")
app_b_discr[4,2] <- formatC(length(kom_data$fitatt2), big.mark = ",")
app_b_discr[5,2] <- formatC(length(subset(kom_data, F==is.na(hpratt2))$hpratt2), big.mark = ",")
app_b_discr[6,2] <- formatC(length(kom_data$a.tall), big.mark = ",")
app_b_discr[7,2] <- formatC(length(kom_data$homeowner), big.mark = ",")

names(app_b_discr)[2] <- "N"

app_b_discr[1,3] <- round(svymean(~opp3, design = app_b_design)[[1]], 3)
app_b_discr[2,3] <- round(svymean(~conatt2, design = app_b_design)[[1]], 3)
app_b_discr[3,3] <- round(svymean(~exatt2, design = app_b_design)[[1]], 3)
app_b_discr[4,3] <- round(svymean(~fitatt2, design = app_b_design)[[1]], 3)
app_b_discr[5,3] <- round(svymean(~hpratt2, design = svydesign(ids=~1, data=subset(kom_data, homeowner=="homeowner"), weights = ~ dweight))[[1]], 3)
app_b_discr[6,3] <- round(svymean(~i(a.tall, ref = "No 5+ story building in area") , design = app_b_design)[[1]], 3)
app_b_discr[7,3] <- round(svymean(~i(homeowner, ref = "non-homeowner") , design = app_b_design)[[1]], 3)

names(app_b_discr)[3] <- "Mean"

app_b_discr[1,4] <- round(sqrt(svyvar(~opp3, design = app_b_design)[[1]]), 3)
app_b_discr[2,4] <- round(sqrt(svyvar(~conatt2, design = app_b_design)[[1]]), 3)
app_b_discr[3,4] <- round(sqrt(svyvar(~exatt2, design = app_b_design)[[1]]), 3)
app_b_discr[4,4] <- round(sqrt(svyvar(~fitatt2, design = app_b_design)[[1]]), 3)
app_b_discr[5,4] <- round(sqrt(svyvar(~hpratt2, design = svydesign(ids=~1, data=subset(kom_data, homeowner=="homeowner"), weights = ~ dweight))[[1]]), 3)
app_b_discr[6,4] <- round(sqrt(svyvar(~i(a.tall, ref = "No 5+ story building in area") , design = app_b_design)[[1]]), 3)
app_b_discr[7,4] <- round(sqrt(svyvar(~i(homeowner, ref = "non-homeowner") , design = app_b_design)[[1]]), 3)

names(app_b_discr)[4] <- "SD"

kbl(app_b_discr, caption = "Descriptive statistics", label = "tab_discriptives",
    booktabs = T, 
    format = "latex",
    linesep = c("\\addlinespace",
                "\\addlinespace",
                "\\addlinespace",
                "\\addlinespace",
                "\\addlinespace",
                "\\addlinespace",
                "\\addlinespace",
                "\\addlinespace"
                ),
    align = "lccc") %>%
  kable_styling(latex_options = c("hold_position",  "condensed", "striped"),
                position = "center",
                font_size = 11) %>% 
  column_spec(1, width = paste0(0.50*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(2, width = paste0(0.06*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(3, width = paste0(0.06*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(4, width = paste0(0.06*452.9679,"pt"), latex_valign = "m") %>% 
  save_kable("tab_discriptives.tex")


#Appendix C####
##A. Estimate models####
figc_size_mm <- cj(kom_data, opp3 ~ b.height, by = ~ b.size, weight = ~ dweight, estimate = "mm") 
figc_type_mm <- cj(kom_data, opp3 ~ b.height, by = ~ b.type, weight = ~ dweight, estimate = "mm") 

figc_size_amce <- cj(kom_data, opp3 ~ b.height, by = ~ b.size, weight = ~ dweight, estimate = "amce") 
figc_type_amce <- cj(kom_data, opp3 ~ b.height, by = ~ b.type, weight = ~ dweight, estimate = "amce") 

##B. Tidy data from model for plotting####
plotc <- figc_size_mm %>% 
  bind_rows(figc_type_mm, figc_size_amce, figc_type_amce) %>% 
  mutate(level = factor(level, 
                        levels = c("office premises", "retail premises",
                                   "social housing", "rental housing",  "own-occ housing",
                                   "500", "1.000", "10.000",
                                   "1", "3", "5", "7"),     
                        labels = c("office premises", "retail premises",
                                   "social housing", "rental housing",  "own-occ housing",
                                   "500 sqm", "1.000 sqm", "10.000 sqm",
                                   "one story", "three stories", "five stories", "seven stories")),
         BY = factor(BY,
                     levels = c("office premises", "retail premises",
                                "social housing", "rental housing",  "own-occ housing",
                                "500", "1.000", "10.000"
                     ),
                     labels = c("office premises", "retail premises",
                                "social housing", "rental housing",  "own-occ housing",
                                "500 sqm", "1.000 sqm", "10.000 sqm")),
         vars.type.size = factor(ifelse(is.na(b.size), 1,0)),
         statistic = factor(statistic,levels = c("mm", "amce"), labels = c("MM", "AMCE")))

##C. Create plot####
plotc1 <- ggplot(data = plotc, aes(x=estimate, y = level, xmin =  lower, xmax = upper, group = BY, fill = BY, color = vars.type.size, shape = vars.type.size))+
  geom_vline(data = data.frame(xint= weighted.mean(kom_data$opp3, w=kom_data$dweight, na.rm = T), statistic =factor("mm", levels = c("mm", "amce"), labels = c("MM", "AMCE"))),
             aes(xintercept = xint), color= "grey")+
  geom_vline(data = data.frame(xint= 0, statistic =factor("amce",levels = c("mm", "amce"), labels = c("MM", "AMCE"))),
             aes(xintercept = xint), color= "grey")+
  geom_linerange(position = position_dodge(0.8))+
  geom_point(position = position_dodge(0.8)) +
  scale_shape_manual(values=c(21, 21))+
  scale_fill_manual(values=c("#d9f0a3", "#78c679", "#31a354", "#31a354", "#006837", "#fdcc8a", "#fc8d59", "#d7301f"))+
  scale_color_manual(values=c("grey50", "black"))+
  facet_wrap2(vars(statistic  ), scales = "free_x", axes = "all", remove_labels = "y")+
  geom_text(data = tibble(story= rep("one story",8), est = rep(0.08,8), statistic =factor("amce",levels = c("mm", "amce"), labels = c("MM", "AMCE")), varlab=factor(1:8, levels = 1:8, labels = c("office premises", "retail premises", "social housing", "rental housing",  "own-occ housing",
                                                                                                                                                                                                  "500 sqm",  "1.000 sqm", "10.000 sqm"))), aes(x=est, group = varlab, y = story, label = varlab), size = 2, color = "grey30", position = position_dodge(0.8), inherit.aes = F)+
  scale_x_continuous(name=element_blank(), breaks= seq(0, 1, 0.05), labels = c(seq(0, 100, 5)))+
  scale_y_discrete(name=element_blank())+
  theme_classic()+
  theme(text = element_text(size = 9),
        strip.text.y = element_blank(),
        axis.text = element_text(size = 9),
        strip.background=element_rect(fill="white", linetype = "blank"),
        strip.text.x = element_text(size = 9),
        legend.position = "none",
        axis.line = element_line(color = "black", lineend = 'square'),
        axis.ticks = element_line(color = "black", lineend = 'square')
  )

title_opp4 <- ggdraw() + 
  draw_label(
    "Opposition",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 10
  ) +
  theme(
    plot.background = element_rect(color = "white"),
    plot.margin = margin(0, 0, 0, 7)
  )

jpipe_c1 <- plot_grid(title_opp4, plotc1, ncol =1 , rel_heights =  c(0.1, 1))

##D. Export plot for text####
ggsave(paste0(figures.save, "/figc1_w.pdf"), jpipe_c1,
       dpi = 600,
       scale = 1,
       width = 12,
       height = 9.5,
       units = c("cm")
)



#Appendix D#########
appd_design_treat <- svydesign(ids=~1, data=kom_data_fig4, weights = ~ dweight)
appd_design_treat_w <- svydesign(ids=~1, data=kom_data_fig4, weights = ~weightdf)

balance_tab <- tibble() 
varname <- c("Age (mean)", "Gender (% women)", "Bachelors or more (%)", "Homeowner (%)", "Live in big city (%)")
varname2 <- c("age", "gender2", "edu2",  "homeowner2", "rural2")


for(i in 1:5){
  balance_tab[i,1] <- varname[i]
  balance_tab[i,2] <- paste0(round(svyby(~get(varname2[i]), na.rm=TRUE, by=~a.tall, design = appd_design_treat, svymean)[1,2]*ifelse(i>1, 100,1),1))
  balance_tab[i,3] <- paste0(round(svyby(~get(varname2[i]), na.rm=TRUE, by=~a.tall, design = appd_design_treat, svymean)[2,2]*ifelse(i>1, 100,1),1))
  balance_tab[i,4] <- paste0(round(svyttest(formula = formula(paste0(varname2[i], "~", "a.tall")), na.rm=TRUE, appd_design_treat)$estimate[[1]]*ifelse(i>1, 100,1),1), ifelse(svyttest(formula = formula(paste0(varname2[i], "~", "a.tall")), na.rm=TRUE, appd_design_treat)$p.value<0.05,"*",""))
  balance_tab[i,5] <- paste0(round(svyby(~get(varname2[i]), na.rm=TRUE, by=~a.tall, design = appd_design_treat_w, svymean)[1,2]*ifelse(i>1, 100,1),1))
  balance_tab[i,6] <- paste0(round(svyby(~get(varname2[i]), na.rm=TRUE, by=~a.tall, design = appd_design_treat_w, svymean)[2,2]*ifelse(i>1, 100,1),1))
  balance_tab[i,7] <- paste0(round(svyttest(formula = formula(paste0(varname2[i], "~", "a.tall")), na.rm=TRUE, appd_design_treat_w)$estimate[[1]]*ifelse(i>1, 100,1),1), ifelse(svyttest(formula = formula(paste0(varname2[i], "~", "a.tall")), na.rm=TRUE, appd_design_treat_w)$p.value<0.05,"*",""))
}

balance_tab[6,1] <- "N = "
balance_tab[6,2] <- paste0(format(length(subset(appd_design_treat$variables, a.tall=="No 5+ story building in area")$dweight), big.mark = ","))
balance_tab[6,3] <- paste0(format(length(subset(appd_design_treat$variables, a.tall!="No 5+ story building in area")$dweight), big.mark = ","))
balance_tab[6,4] <- ""
balance_tab[6,5] <- paste0(format(length(subset(appd_design_treat_w$variables, a.tall=="No 5+ story building in area")$dweight), big.mark = ","))
balance_tab[6,6] <- paste0(format(length(subset(appd_design_treat_w$variables, a.tall!="No 5+ story building in area")$dweight), big.mark = ","))
balance_tab[6,7] <- ""

names(balance_tab) <- c("", "Control", "Treatment", "Difference", "Control weighted", "Treatment weighted", "Difference weighted")

kbl(balance_tab, caption = "Relationship between assignment to a development project situated in an area where there was a five-story building in the development site and covariates", label = "tab_appfit",
    booktabs = T, 
    format = "latex",
    linesep = c("", "", "", "", "\\addlinespace", ""
    ),
    align = "lrrrrrr") %>%
  kable_styling(latex_options = c("hold_position",  "condensed", "striped"),
                position = "center",
                font_size = 11) %>% 
  column_spec(1, width = paste0(0.20*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(2, width = paste0(0.10*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(3, width = paste0(0.10*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(4, width = paste0(0.10*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(5, width = paste0(0.10*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(6, width = paste0(0.10*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(7, width = paste0(0.10*452.9679,"pt"), latex_valign = "m") %>% 
  footnote("* p \\< 0.05 from t-test. Big city includes the municipality of Aarhus, Odense, Aalborg, and Copenhagen and all its suburbs of Frederiksberg, Tårnby, Dragør, Gentofte, Lyngby, Gladsaxe, Rødovre, Hvidovre, Brøndby, Taastrup and Ballerup.", footnote_as_chunk = T, threeparttable = TRUE) %>% 
  save_kable("tab_appfit.tex")

#Appendix E####
kom_data_appe <- kom_data %>% 
  subset(sevenstories!=0) %>% #Need to remove never takers 6158
  subset(sevenstories!=1)  #Need to remove allways takers 48

appe_design_treat <- svydesign(ids=~1, data=kom_data_appe, weights = ~ dweight)
appe_design_treat_w <- svydesign(ids=~1, data=kom_data_appe, weights = ~weightdf7)

balance_tab <- tibble() 
varname <- c("Age (mean)", "Gender (% women)", "Bachelors or more (%)", "Homeowner (%)", "Live in big city (%)")
varname2 <- c("age", "gender2", "edu2",  "homeowner2", "rural2")

for(i in 1:5){
  balance_tab[i,1] <-  varname[i]
  balance_tab[i,2] <- paste0(round(svyby(~get(varname2[i]), na.rm=TRUE, by=~a.tall2, design = appe_design_treat, svymean)[1,2]*ifelse(i>1, 100,1),1))
  balance_tab[i,3] <- paste0(round(svyby(~get(varname2[i]), na.rm=TRUE, by=~a.tall2, design = appe_design_treat, svymean)[2,2]*ifelse(i>1, 100,1),1))
  balance_tab[i,4] <- paste0(round(svyttest(formula = formula(paste0(varname2[i], "~", "a.tall2")), na.rm=TRUE, appe_design_treat)$estimate[[1]]*ifelse(i>1, 100,1),1), ifelse(svyttest(formula = formula(paste0(varname2[i], "~", "a.tall2")), na.rm=TRUE, appe_design_treat)$p.value<0.05,"*",""))
  balance_tab[i,5] <- paste0(round(svyby(~get(varname2[i]), na.rm=TRUE, by=~a.tall2, design = appe_design_treat_w, svymean)[1,2]*ifelse(i>1, 100,1),1))
  balance_tab[i,6] <- paste0(round(svyby(~get(varname2[i]), na.rm=TRUE, by=~a.tall2, design = appe_design_treat_w, svymean)[2,2]*ifelse(i>1, 100,1),1))
  balance_tab[i,7] <- paste0(round(svyttest(formula = formula(paste0(varname2[i], "~", "a.tall2")), na.rm=TRUE, appe_design_treat_w)$estimate[[1]]*ifelse(i>1, 100,1),1), ifelse(svyttest(formula = formula(paste0(varname2[i], "~", "a.tall2")), na.rm=TRUE, appe_design_treat_w)$p.value<0.05,"*",""))
}

balance_tab[6,1] <- "N = "
balance_tab[6,2] <- paste0(format(length(subset(appe_design_treat$variables, a.tall2=="No 7+ story building in area")$Id), big.mark = ","))
balance_tab[6,3] <- paste0(format(length(subset(appe_design_treat$variables, a.tall2!="No 7+ story building in area")$Id), big.mark = ","))
balance_tab[6,4] <- ""
balance_tab[6,5] <- paste0(format(length(subset(appe_design_treat_w$variables, a.tall2=="No 7+ story building in area")$Id), big.mark = ","))
balance_tab[6,6] <- paste0(format(length(subset(appe_design_treat_w$variables, a.tall2!="No 7+ story building in area")$Id), big.mark = ","))
balance_tab[6,7] <- ""

names(balance_tab) <- c("", "Control", "Treatment", "Difference", "Control weighted", "Treatment weighted", "Difference weighted")

kbl(balance_tab, caption = "Relationship between assignment to a development project situated in an area where there was a seven-story building in the development site and covariates", label = "tab_appgfit",
    booktabs = T, 
    format = "latex",
    linesep = c("", "", "", "", "\\addlinespace", ""
    ),
    align = "lrrrrrr") %>%
  kable_styling(latex_options = c("hold_position",  "condensed", "striped"),
                position = "center",
                font_size = 11) %>% 
  column_spec(1, width = paste0(0.20*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(2, width = paste0(0.10*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(3, width = paste0(0.10*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(4, width = paste0(0.10*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(5, width = paste0(0.10*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(6, width = paste0(0.10*452.9679,"pt"), latex_valign = "m") %>% 
  column_spec(7, width = paste0(0.10*452.9679,"pt"), latex_valign = "m") %>% 
  footnote("* p \\< 0.05 from t-test. Big city includes the municipality of Aarhus, Odense, Aalborg, and Copenhagen and all its suburbs of Frederiksberg, Tårnby, Dragør, Gentofte, Lyngby, Gladsaxe, Rødovre, Hvidovre, Brøndby, Taastrup and Ballerup.", footnote_as_chunk = T, threeparttable = TRUE) %>% 
  save_kable("tab_appgfit.tex")

##A. Estimate models####
appe_opp <- cj(kom_data_appe, opp3 ~ b.height,  by = ~a.tall2, weights = ~ weightdf7, estimate = "mm") %>% #Marginal Means of project attributes
  mutate(name = "Weighted") #Using inverse probability weighting

appe_opp_2 <- cj(kom_data_appe, opp3 ~ b.height,  by = ~a.tall2, weights = ~ dweight, estimate = "mm") %>% #Marginal Means of project attributes
  mutate(name = "Non-weighted") #Using standard design weights

appe_fit <- cj(kom_data_appe, fitatt2 ~ b.height,  by = ~a.tall2, weights = ~ weightdf7, estimate = "mm") %>% #Marginal Means of project attributes
  mutate(name = "Weighted") #Using inverse probability weighting

appe_fit_2 <- cj(kom_data_appe, fitatt2 ~ b.height,  by = ~a.tall2, weights = ~ dweight, estimate = "mm") %>% #Marginal Means of project attributes
  mutate(name = "Non-weighted") #Using standard design weights


##B. Tidy data from model for plotting####
plot_appe_opp <- appe_opp %>% 
  bind_rows(appe_opp_2) %>% 
  mutate(height = factor(level, levels = c("1", "3", "5", "7"), labels = c("one story", "three stories", "five stories", "seven stories")),
         tall = factor(a.tall2, levels = c("7+ story building in area", "No 7+ story building in area")))


plot_appe_fit <- appe_fit %>% 
  bind_rows(appe_fit_2) %>% 
  mutate(height = factor(level, levels = c("1", "3", "5", "7"), labels = c("one story", "three stories", "five stories", "seven stories")),
         tall = factor(a.tall2, levels = c("7+ story building in area", "No 7+ story building in area")))

##C. Create plots####
appe_plot_opp <- ggplot(data = plot_appe_opp , aes(x=estimate, y = height, xmin =  lower, xmax = upper, group = name, color = name))+
  geom_vline(xintercept =  weighted.mean(kom_data_appe$opp3, w=kom_data_appe$dweight, na.rm = T), color = "grey")+
  geom_point(position = position_dodge(0.5))+
  geom_linerange(position = position_dodge(0.5))+
  scale_colour_manual(values = c("black", "grey60"))+
  facet_wrap2(vars(tall), axes = "all", remove_labels = "y")+
  scale_x_continuous(name="Opposition %", breaks= seq(0, 1, 0.1), limits =c(0.07,0.59), labels = c(seq(0, 100, 10)))+
  scale_y_discrete(name=element_blank())+
  theme_classic()+
  theme(text = element_text(size = 9),
        strip.text.y = element_blank(),
        axis.text = element_text(size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        strip.background=element_rect(fill="white", linetype = "blank"),
        strip.text = element_text(size = 9, color = "black", face = "bold", hjust = 0),
        axis.line = element_line(color = "black", lineend = 'square'),
        axis.ticks = element_line(color = "black", lineend = 'square'),
        legend.position = "none"
  )

appe_plot_fit <- ggplot(data = plot_appe_fit , aes(x=estimate, y = height, xmin =  lower, xmax = upper, group = name, color = name))+
  geom_vline(xintercept =  weighted.mean(kom_data_appe$fitatt2, w=kom_data_appe$dweight, na.rm = T), color = "grey")+
  geom_point(position = position_dodge(0.5))+
  geom_linerange(position = position_dodge(0.5))+
  scale_colour_manual(values = c("black", "grey60"))+
  facet_wrap2(vars(tall), axes = "all", remove_labels = "y")+
  scale_x_continuous(name="Agree project fits poorly in area %", breaks= seq(0, 1, 0.1), limits =c(0.07,0.59), labels = c(seq(0, 100, 10)))+
  scale_y_discrete(name=element_blank())+
  theme_classic()+
  theme(text = element_text(size = 9),
        strip.text.y = element_blank(),
        axis.text = element_text(size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        strip.background=element_rect(fill="white", linetype = "blank"),
        strip.text = element_text(size = 9, color = "black", face = "bold", hjust = 0),
        axis.line = element_line(color = "black", lineend = 'square'),
        axis.ticks = element_line(color = "black", lineend = 'square'),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm")
        
  )


plot_grid(appe_plot_opp, appe_plot_fit, ncol =1 , rel_heights =  c(0.9, 1.1))

##D. Export plot for text####
ggsave(paste0(figures.save, "/fige1_w.pdf"), last_plot(),
       dpi = 600,
       scale = 1,
       width = 12,
       height = 11,
       units = c("cm")
)


