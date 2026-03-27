### META-ANALYSIS ###

# EFFECTS OF RESISTANCE TRAINING ON BONE MINERAL DENSITY AND BIOMARKERS OF BONE METABOLISM IN 
# POSTMENOPAUSAL WOMEN: SYSTEMATIC REVIEW AND META-ANALYSIS

### BONE MINERAL DENSITY ###

library(readxl)
BMD_Total <- read_excel("BMD_Total.xlsx")
View(BMD_Total)
library(meta)

BMD <- as.data.frame(BMD_Total)
BMD

BMD_Total <- metacont(t_n, t_mean, t_sd, c_n, c_mean, c_sd, data = BMD, method.tau="DL",
                      sm="MD", studlab = Study, comb.fixed=FALSE, xlim=c(-0.2, 0.2), 
                      label.e = "Strength exercise", lab.c="Control",
                      label.left = "Favors control", label.right = "Favors strength")                     
BMD_Total
forest(BMD_Total, fontfamily="Times", fontsize=12, xlim=c(-0.4, 0.4), digits = 3, digits.se = 3,
       col.diamond = "black")



### CTX-I ###

library(readxl)
CTX_I <- read_excel("CTX-I.xlsx")
View(CTX_I)
library(meta)

ctx <- as.data.frame(CTX_I)
ctx

ctx <- metacont(t_n, t_mean, t_sd, c_n, c_mean, c_sd, data = ctx, method.tau="DL",
                sm="MD", studlab = Study, comb.fixed=FALSE, xlim=c(-0.2, 0.2), 
                label.e = "Strength exercise", lab.c="Control",
                label.left = "Favors strength", label.right = "Favors control")                     
ctx
forest(ctx, fontfamily="Times", fontsize=12, xlim=c(-0.4, 0.4), digits = 3, digits.se = 3,
       col.diamond = "black")



### ALKALINE PHOSPHATASE ###

library(readxl)
alkaline_phosphatase <- read_excel("alkaline_phosphatase.xlsx")
View(alkaline_phosphatase)
library(meta)

phosphatase <- as.data.frame(alkaline_phosphatase)
phosphatase

phosphatase <- metacont(t_n, t_mean, t_sd, c_n, c_mean, c_sd, data = phosphatase, method.tau="DL",
                      sm="MD", studlab = Study, comb.fixed=FALSE, xlim=c(-50, 50), 
                      label.e = "Strength exercise", lab.c="Control",
                      label.left = "Favors control", label.right = "Favors strength")                     
phosphatase
forest(phosphatase, fontfamily="Times", fontsize=12, xlim=c(-50, 50), digits = 3, digits.se = 3,
       col.diamond = "black")

