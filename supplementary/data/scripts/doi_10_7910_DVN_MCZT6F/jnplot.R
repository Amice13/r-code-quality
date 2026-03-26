library(interactions)
library(readxl)
my_data <- read_excel("data_jnplot.xlsx")
library(fixest)
library(jtools)

###without removing fixed effects###
reg_lm <- lm(elected ~ authoritarianism * polity2, data = subset(my_data, divi_polity == 1 & polity2 > 5))
reg_lm
johnson_neyman(reg_lm, pred = authoritarianism, modx = polity2, mod.range = c(0, 10))
library(ggplot2)
jn_plot <- johnson_neyman(reg_lm, pred = authoritarianism, modx = polity2, mod.range = c(0, 10))
jn_plot1 <- jn_plot$plot + labs(
  title = "Without removing fixed effects"
)
jn_plot1
ggsave(
  filename = "jn_plot.png",
  plot = jn_plot1,
  width = 6,
  height = 6.5, 
  dpi = 600
)

###removing fixed effects###
my_data <- subset(  my_data,  divi_polity == 1 & !is.na(authoritarianism) &
                      !is.na(elected) &
                      !is.na(polity2))
my_data$resid_auth <- feols(authoritarianism ~ 1 | id + pcode + year, data = my_data)$residuals
my_data$resid_elec <- feols(elected ~ 1 | id + pcode + year, data = my_data)$residuals
my_data$resid_pol  <- feols(polity2 ~ 1 | id + pcode + year, data = my_data)$residuals
reg_lm <- lm(resid_elec ~ resid_auth * resid_pol, data = subset(my_data, divi_polity == 1 & polity2 > 5))
jnplot2 <- johnson_neyman(reg_lm, pred = resid_auth, modx = resid_pol, mod.range = c(-7.5, 3.5))
jn_plot2 <- jnplot2$plot + labs(
  title = "Removing fixed effects"
)
ggsave(
  filename = "jn_plot2.png",   # 文件名
  plot = jn_plot2,        # 绘图对象
  width = 6,                  # 宽度（英寸）
  height = 6.5,                 # 高度（英寸）
  dpi = 600                   # 分辨率（常用学术期刊标准）
)