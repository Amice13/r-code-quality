# ----------------------------------------------------------------------
#  Project: Diversity in Healthcare Provision Reduces Jewish Patients' Prejudice towards Arabs
#  Last updated: "Wed Feb 17 08:14:12 2021"
#  Input data: "replication_data/pnas_diversity_main.csv"
#  Purpose: Replication Materials -- Main Text
#  Outputs: Figure 4 and Figure 5 from the main text
#  Machine: Chagai's Macbook Pro
# ----------------------------------------------------------------------

#############################################################################
# Load relevant paackges      
#############################################################################
library("readxl")
library("tidyverse")
library("stringr")
library("xlsx")
library("estimatr")
library("gridExtra")
library("ggpubr")
library("texreg")
library("stargazer")
library("xtable")
library("lfe")
library("MASS")
library("multiwayvcov")
library("lmtest")

#############################################################################
# Read in main data
#############################################################################

# read data
data <- read_csv("pnas_diversity_main.csv")
nrow(data)

# subset to Jewish patients for main analyses
data_j <- data %>% 
 filter(.,
        jewish == 1)
nrow(data_j)


#############################################################################
# Create Figure 4 --- Demographic Balance
#############################################################################

# Regress Arab doctor indicator over religiosity variable
fe_diagnostic_r <- lm_robust(arab ~ religiosity_num, 
                             data = data_j,
                             fixed_effects = clinic_code + visit_date,
                             clusters = clinic_code, se_type = "CR0") %>% 
 tidy(., conf.int = T, robust = T) %>% mutate(Model = "FE",
                                  term = "Religiosity \nn=2058")


# Regress Arab doctor indicator over gender variable
fe_diagnostic_g <- lm_robust(arab ~ gender_num, 
                             data = data_j,
                             fixed_effects = clinic_code + visit_date,
                             clusters = clinic_code, se_type = "CR0") %>% 
 tidy(., conf.int = T, robust = T) %>% mutate(Model = "FE",
                                  term = "Gender\nn=2164")

# Regress Arab doctor indicator over education variable
fe_diagnostic_e <- lm_robust(arab ~ edu_num, 
                             data = data_j,
                             fixed_effects = clinic_code + visit_date,
                             clusters = clinic_code, se_type = "CR0") %>% 
 tidy(., conf.int = T, robust = T) %>% mutate(Model = "FE",
                                  term = "Education \nn=1914")

# Regress Arab doctor indicator over age variable
fe_diagnostic_a <- lm_robust(arab ~ age, 
                             data = data_j,
                             fixed_effects = clinic_code + visit_date,
                             clusters = clinic_code, se_type = "CR0") %>% 
 tidy(., conf.int = T, robust = T) %>% mutate(Model = "FE",
                                  term = "Age \nn=2164")



# Create balance plot
balance_models <- rbind(fe_diagnostic_r, fe_diagnostic_g, fe_diagnostic_e, fe_diagnostic_a)
ggplot(balance_models, aes(x = term, y = estimate)) +
 geom_hline(yintercept = 0, color = "gray50", linetype = 2, size = 0.2) +
 geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                 position = position_dodge(width = 0.4),
                 fill = "white", color = "dodgerblue4") +
 coord_flip()+
 # scale_color_manual(values = c("firebrick2", "dodgerblue2")) +
 # scale_fill_manual(values = c("firebrick3", "dodgerblue3")) +
 labs(x = "",
      y = "Effect Size") +
 scale_y_continuous(limits = c(-0.07, 0.055)) + 
 # scale_x_discrete(limits= order_x) +
 # caption = "*Coefficients from OLS models, in which outcomes are standardized."
 theme(text = element_text(size = 12, family = "Times"),
       legend.key=element_blank(),
       panel.grid.major = element_blank(), 
       axis.text.x = element_text(size = 12),
       plot.caption = element_text(size = 12, family = "Times",hjust = -.02),
       panel.grid.minor = element_blank(),
       panel.background = element_blank(), 
       axis.line = element_line(colour = "black"))




#############################################################################
# Create Figure 5 --- Main Effect of Contact on Prejudice
#############################################################################

#### Estimate models considering the effects of contact on 5 outcomes

# Social distance
sd_plot <- lm_robust(z_sd ~ arab, 
                data = data_j,
                fixed_effects = clinic_code + visit_date,
                clusters = clinic_code, se_type = "CR0") %>% 
 tidy(., conf.int = T, robust = T) %>%  mutate(., term = "Social \nDistance")

# Peace
peace_plot <- lm_robust(z_peace ~ arab, 
                   data = data_j,
                   fixed_effects = clinic_code + visit_date,
                   clusters = clinic_code, se_type = "CR0") %>% 
tidy(., conf.int = T, robust = T) %>%  mutate(., term = "Peace")


# Therm
therm_plot <-  lm_robust(z_therm ~ arab, 
                    data = data_j,
                    fixed_effects = clinic_code + visit_date,
                    clusters = clinic_code, se_type = "CR0") %>% 
 tidy(., conf.int = T, robust = T) %>%  mutate(., term = "Feeling \nThermometer")

# Trust
trust_plot <- lm_robust(z_trust ~ arab, 
                   data = data_j,
                   fixed_effects = clinic_code + visit_date,
                   clusters = clinic_code, se_type = "CR0") %>% 
tidy(., conf.int = T, robust = T) %>% mutate(., term = "Trust")

# Index
index_plot <- lm_robust(z_index ~ arab, 
                   data = data_j,
                   fixed_effects = clinic_code + visit_date,
                   clusters = clinic_code, se_type = "CR0") %>% 
tidy(., conf.int = T, robust = T)%>%  mutate(., term = "Index")


# Create plot of main results for paper





#### Additional Figure that includes a no cluster model as well as wild cluster bootstrap

# bind all models
main_models <- bind_rows(sd_plot, peace_plot, therm_plot, trust_plot, index_plot)


# Generate coefficient plot 
positions_main <- c("Social \nDistance", "Peace", "Feeling \nThermometer", "Trust", "Index")
ggplot(main_models, aes(x = term, y = estimate)) +
 geom_hline(yintercept = 0, color = "gray50", linetype = 2, size = 0.2) +
 geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                 position = position_dodge(width = 0.4),
                 fill = "white", color = "dodgerblue4") +
 scale_y_continuous(limits = c(-0.4, 0.5)) + 
 #coord_flip()+
 scale_color_manual(values = c("dodgerblue2")) +
 scale_fill_manual(values = c("dodgerblue2")) +
 scale_x_discrete(limits = positions_main)+ 
 labs(x = "",
      y = "Effect Size") +
 geom_text(aes(label= format(round(estimate, digits = 2), nsmall = 2)), hjust=-.85, vjust=-2.1, size = 3.3, color = "firebrick3",
           family = "Times") +
 geom_text(aes(label= paste("(",format(round(std.error, digits = 2), nsmall =2), ")")), hjust=-.33, vjust=-0.5, size = 3.3, color = "firebrick3", 
           family = "Times") +
 annotate("text", x = 1:5, y = -0.4, 
          label = c("N = 1,285","N = 1,456","N = 1,521",
                    "N = 1,461", "N = 1,234"),
          family = "Times", color = "gray30", size = 3.4)+
 theme(text = element_text(size = 12, family = "Times"),
       panel.grid.major = element_blank(), 
       legend.key = element_rect(colour = "transparent", fill = "white"),
       axis.text.x = element_text(size = 12),
       plot.caption = element_text(size = 12, family = "Times",hjust = -.02),
       panel.grid.minor = element_blank(),
       panel.background = element_blank(), 
       axis.line = element_line(colour = "black"))

