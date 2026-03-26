packages <- c("DeclareDesign","tidyverse","kableExtra","sf","magrittr","rio","stargazer","lfe",
              "car","scales","ggthemes","lubridate","survminer","survival","splitstackshape",
              "gridExtra","knitr","modelsummary","wesanderson","janitor","ggprism","sandwich",
              "lmtest","ri2","ggplot2","RColorBrewer","xtable","texreg","dplyr","tidyr","reshape2",
              "Hmisc","estimatr","patchwork","haven","scales","countrycode","schoolmath","Rmisc","broom","tidyr","ggstance")

ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#### Import Data ####
df.final <- read.csv("Figure_1a.csv", header = TRUE) 

#### Plot ####
regime <- c("Closed Autocracy", "Electoral Autocracy", "Electoral Democracy", "Liberal Democracy")

figure.1a <- ggplot(df.final[df.final$immigration_status==2,], aes(regime_des)) +
  geom_bar(stat = "count") +
  xlab("Regime Type") +
  ylab("Num. of Immigrants")+
  scale_x_continuous(labels= regime, breaks=c(0,1,2,3))

print(figure.1a)

ggsave(
  filename = "Figures/Figure_1a.pdf",
  width = 6,
  height = 4, 
  units = "in"
)


# Convert labelled variables to numeric
df <- read.csv("Figure_2.csv", header = TRUE)
df <- df %>%
  mutate(
    IoD = as.numeric(IoD),
    DemGood = as.numeric(DemGood)
  )

#### Figure 2a ####
model_IoD <- felm(IoD ~ factor(migrant) | iso_num | 0 | 0, data = df, subset = IoD > 0)

#### Figure 2b ####
model_DemGood <- felm(DemGood ~ factor(migrant) | iso_num | 0 | 0, data = df, subset = DemGood > 0)

# Function to plot model results
plot_coef_clean <- function(model, x_limits = NULL) {
  coef_df <- broom::tidy(model, conf.int = TRUE) %>%
    filter(!is.na(estimate), !is.na(conf.low), !is.na(conf.high)) %>%
    filter(term != "(Intercept)") %>%
    mutate(term = gsub("factor\\(migrant\\)", "", term),
           term = trimws(term),
           term = recode(term, "1" = "To Autocracy", "2" = "To Democracy"))
  
  ggplot(coef_df, aes(x = estimate, y = term)) +
    geom_pointrangeh(aes(xmin = conf.low, xmax = conf.high),
                     color = "black", size = 0.4) +
    geom_vline(xintercept = 0, color = "black") +
    scale_y_discrete(limits = rev(c("To Autocracy", "To Democracy"))) +
    scale_x_continuous(labels = function(x) gsub("^(-?)0\\.", "\\1.", format(x, trim = TRUE))) +
    {if (!is.null(x_limits)) coord_cartesian(xlim = x_limits) else NULL} +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid.major.y = element_line(color = "grey85", size = 0.3),  # only horizontal lines
      panel.grid.major.x = element_blank(),  # remove vertical lines
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(),
      axis.text.y = element_text(size = 11),
      axis.text.x = element_text(size = 11),
      axis.ticks.x = element_line(color = "black", size = 0.3),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
}

# Generate plots
p1 <- plot_coef_clean(model_IoD, x_limits = c(-0.2, 0.4))
print(p1)

ggsave("Figures/Figure_2a.pdf", p1, width = 6, height = 4)

p2 <- plot_coef_clean(model_DemGood, x_limits = c(-0.05, 0.15))
print(p2)
ggsave("Figures/Figure_2b.pdf", p2, width = 6, height = 4)