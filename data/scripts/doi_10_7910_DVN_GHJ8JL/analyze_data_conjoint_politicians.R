### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file analyzes the politician conjoint experiment data, generating plots and tables
### R version, platform, and package versions reported at the end of the file

# Prepare the environment -------------------------------------------------

### This section of the code prepares the environment 

# Clean the environment
rm(list = ls())

# Install required packages if not previously installed
package_list <- c("tidyverse", "here", "xtable", "cjoint") 
packages_to_install <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_to_install)>0){
  install.packages(packages_to_install)
}

# Load required packages
library(tidyverse)
library(here)
library(xtable)
library(cjoint)

# Set Working Directory to wherever this file is located.
setwd(here())
# The directory where the code folder is located must also have a "plots" and a "tables" subdirectory

# Load analysis dataset ---------------------------------------------------

### This section loads the analysis dataset and prepares it for analysis with the "cjoint" package

d <- read_csv("../../datasets/analysis/survey_politicians/conjoint_politicians_analysis.csv")

# Transform attributes to factors
d$experience <- as.factor(d$experience)
d$education <- as.factor(d$education)
d$selection <- as.factor(d$selection)
d$political_connections <- as.factor(d$political_connections)
d$union_membership <- as.factor(d$union_membership)
d$gender <- as.factor(d$gender)

# Define baseline values for each of the attributes
d$experience <- relevel(d$experience, "3 years")
d$education <- relevel(d$education, "Undergraduate degree")
d$selection <- relevel(d$selection, "Civil service")
d$political_connections <- relevel(d$political_connections, "No connections with city hall")
d$union_membership <- relevel(d$union_membership, "Does not participate in a union")
d$gender <- relevel(d$gender, "Woman")

# Generate Figure 8 (politician conjoint experiment plot) -----------------------------------------------------------

### This section generates Figure 8 in the article (plot results for the politician conjoint experiment)

# Run regressions for each of the choice tasks (effort, performance, implementation, and communication)
# These regressions use profile attributes as predictors, and cluster standard errors by respondent
m1 <- amce(chosen_effort ~ experience + education + selection + political_connections + union_membership + gender,
                  data=d, respondent.id = "respondent_id", cluster=T)
m2 <- amce(chosen_performance ~ experience + education + selection + political_connections + union_membership + gender,
                       data=d, respondent.id = "respondent_id", cluster=T)
m3 <- amce(chosen_implementation ~ experience + education + selection + political_connections + union_membership + gender,
                          data=d, respondent.id = "respondent_id", cluster=T)
m4 <- amce(chosen_communication ~ experience + education + selection + political_connections + union_membership + gender,
                         data=d, respondent.id = "respondent_id", cluster=T)

# Generate list of attributes for the plot
attributes <- rev(c("Temporary contract \n (vs civil service)", 
                    "Has political connections \n (vs no connections)",
                    "Graduate degree \n (vs Bachelors degree)", 
                    "Has 10 years of \n experience (vs 3 years)", 
                    "Participates in a union \n (vs does not participate)",
                    "Man \n (vs woman)"))

# Define a critical value for confidence intervals
critical_value <- qt(.975, df=m1$samplesize_prof-8)

# For each of the choice tasks, create a table with AMCEs, standard errors, and confidence intervals 
### Effort 
amce <- rev(c(m1$estimates$selection[1,1], m1$estimates$politicalconnections[1,1], m1$estimates$experience[1,1], m1$estimates$education[1,1], m1$estimates$unionmembership[1,1], m1$estimates$gender[1,1]))
se <- rev(c(m1$estimates$selection[2,1], m1$estimates$politicalconnections[2,1], m1$estimates$experience[2,1], m1$estimates$education[2,1], m1$estimates$unionmembership[2,1], m1$estimates$gender[2,1]))
results_effort <- as_tibble(cbind(attributes, amce, se))
results_effort <- results_effort %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)
### Performance
amce <- rev(c(m2$estimates$selection[1,1], m2$estimates$politicalconnections[1,1], m2$estimates$experience[1,1], m2$estimates$education[1,1], m2$estimates$unionmembership[1,1], m2$estimates$gender[1,1]))
se <- rev(c(m2$estimates$selection[2,1], m2$estimates$politicalconnections[2,1], m2$estimates$experience[2,1], m2$estimates$education[2,1], m2$estimates$unionmembership[2,1], m2$estimates$gender[2,1]))
results_performance <- as_tibble(cbind(attributes, amce, se))
results_performance <- results_performance %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)
### Implementation
amce <- rev(c(m3$estimates$selection[1,1], m3$estimates$politicalconnections[1,1], m3$estimates$experience[1,1], m3$estimates$education[1,1], m3$estimates$unionmembership[1,1], m3$estimates$gender[1,1]))
se <- rev(c(m3$estimates$selection[2,1], m3$estimates$politicalconnections[2,1], m3$estimates$experience[2,1], m3$estimates$education[2,1], m3$estimates$unionmembership[2,1], m3$estimates$gender[2,1]))
results_implementation <- as_tibble(cbind(attributes, amce, se))
results_implementation <- results_implementation %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)
### Communication
amce <- rev(c(m4$estimates$selection[1,1], m4$estimates$politicalconnections[1,1], m4$estimates$experience[1,1], m4$estimates$education[1,1], m4$estimates$unionmembership[1,1], m4$estimates$gender[1,1]))
se <- rev(c(m4$estimates$selection[2,1], m4$estimates$politicalconnections[2,1], m4$estimates$experience[2,1], m4$estimates$education[2,1], m4$estimates$unionmembership[2,1], m4$estimates$gender[2,1]))
results_communication <- as_tibble(cbind(attributes, amce, se))
results_communication <- results_communication %>%
  mutate(amce = as.numeric(amce), se = as.numeric(se),
         cilow = amce-critical_value*se, ciup=amce+critical_value*se)

# Create the plot itself
pdf("../../plots/conjoint_politicians.pdf", width=7.5, height=6.5)
par(mar=c(5, 11, 2, 2)) 
plot(NULL, ylim = c(0.5,6.5), xlim = c(-0.2, 0.25), yaxt = "n",
     xlab = "Average marginal component effect", ylab = "", cex.main = 1)
grid() # Draw an empty plot
abline(v=0, col="gray", lty=1,lwd=1.5) # Add a vertical line at zero
axis(2, at=c(1:6),labels=results_effort$attributes, las=2, cex=.8) # Plot the attribute values on the left
segments(y0=c(1.15:6.15),x0 = results_communication$cilow, x1 = results_communication$ciup, col = "black", lwd = 2) # Draw AMCE confidence intervals for question on communication
points(y = c(1.15:6.15), x = results_communication$amce, pch = 15, col="black", cex=1) # Draw AMCE point estimates for question on communication
segments(y0=c(1.075:6.075),x0 = results_implementation$cilow, x1 = results_implementation$ciup, col = "red", lwd = 2) # Draw AMCE confidence intervals for question on implementation
points(y = c(1.075:6.075), x = results_implementation$amce, pch = 16, col="red", cex=1) # Draw AMCE point estimates for question on implementation
segments(y0=c(0.925:5.925),x0 = results_effort$cilow, x1 = results_effort$ciup, col = "blue", lwd = 2) # Draw AMCE confidence intervals for question on effort
points(y = c(0.925:5.925), x = results_effort$amce, pch = 17, col="blue", cex=1) # Draw AMCE point estimates for question on effort
segments(y0=c(0.85:5.85),x0 = results_performance$cilow, x1 = results_performance$ciup, col = "darkgreen", lwd = 2) # Draw AMCE confidence intervals for question on performance
points(y = c(0.85:5.85), x = results_performance$amce, pch = 18, col="darkgreen", cex=1) # Draw AMCE point estimates for question on performance
legend("bottomright", c("Communication", "Implementation", "Effort", "Performance"), lty=1, pch=c(15,16,17,18), col=c("black", "red", "blue", "darkgreen"), bty="n") # Add legend
dev.off()

# SUPLEMENTARY ANALYSES (reported in Online Appendix) ---------------------
# Generate Table 31 (politician conjoint experiment regression table) --------------------------------------------------------

### This section generates Table 31 (in Appendix F.3), which reports regression results for the politician conjoint experiment

# Define vector with row names in the regression table
vars <- c("Contract: Temporary (vs civil service)", "Political connections: Yes (vs no)","Education: Masters (vs Bachelors)", "Experience: 10 years (vs 3 years)", "Union membership: Yes (vs no)", "Gender: Male (vs female)",  "Number of respondents", "Number of valid profiles")

# Results for communication
### Each item in the vector collects the beta coefficient for each row, the corresponding stars according to its p-value, and (in brackets) its standard error
betas_comm <- c(paste0(paste0(round(m4$estimates$selection[1,1],3),ifelse(summary(m4)$amce[5,6]<0.001,"***", ifelse(summary(m4)$amce[5,6]<0.01,"**", ifelse(summary(m4)$amce[5,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$selection[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m4$estimates$politicalconnections[1,1],3),ifelse(summary(m4)$amce[4,6]<0.001,"***", ifelse(summary(m4)$amce[4,6]<0.01,"**", ifelse(summary(m4)$amce[4,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$politicalconnections[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m4$estimates$education[1,1],3),ifelse(summary(m4)$amce[1,6]<0.001,"***", ifelse(summary(m4)$amce[1,6]<0.01,"**", ifelse(summary(m4)$amce[1,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$education[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m4$estimates$experience[1,1],3),ifelse(summary(m4)$amce[2,6]<0.001,"***", ifelse(summary(m4)$amce[2,6]<0.01,"**", ifelse(summary(m4)$amce[2,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$experience[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m4$estimates$unionmembership[1,1],3),ifelse(summary(m4)$amce[6,6]<0.001,"***", ifelse(summary(m4)$amce[6,6]<0.01,"**", ifelse(summary(m4)$amce[6,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$unionmembership[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m4$estimates$gender[1,1],3),ifelse(summary(m4)$amce[3,6]<0.001,"***", ifelse(summary(m4)$amce[3,6]<0.01,"**", ifelse(summary(m4)$amce[3,6]<0.05,"*",""))),sep=""),paste0(" (",round(m4$estimates$gender[2,1],3),")",sep=""),sep=""),
                m4$numrespondents, m4$samplesize_prof)

# Results for implementation
### Each item in the vector collects the beta coefficient for each row, the corresponding stars according to its p-value, and (in brackets) its standard error
betas_impl <- c(paste0(paste0(round(m3$estimates$selection[1,1],3),ifelse(summary(m3)$amce[5,6]<0.001,"***", ifelse(summary(m3)$amce[5,6]<0.01,"**", ifelse(summary(m3)$amce[5,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$selection[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m3$estimates$politicalconnections[1,1],3),ifelse(summary(m3)$amce[4,6]<0.001,"***", ifelse(summary(m3)$amce[4,6]<0.01,"**", ifelse(summary(m3)$amce[4,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$politicalconnections[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m3$estimates$education[1,1],3),ifelse(summary(m3)$amce[1,6]<0.001,"***", ifelse(summary(m3)$amce[1,6]<0.01,"**", ifelse(summary(m3)$amce[1,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$education[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m3$estimates$experience[1,1],3),ifelse(summary(m3)$amce[2,6]<0.001,"***", ifelse(summary(m3)$amce[2,6]<0.01,"**", ifelse(summary(m3)$amce[2,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$experience[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m3$estimates$unionmembership[1,1],3),ifelse(summary(m3)$amce[6,6]<0.001,"***", ifelse(summary(m3)$amce[6,6]<0.01,"**", ifelse(summary(m3)$amce[6,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$unionmembership[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m3$estimates$gender[1,1],3),ifelse(summary(m3)$amce[3,6]<0.001,"***", ifelse(summary(m3)$amce[3,6]<0.01,"**", ifelse(summary(m3)$amce[3,6]<0.05,"*",""))),sep=""),paste0(" (",round(m3$estimates$gender[2,1],3),")",sep=""),sep=""),
                m3$numrespondents, m3$samplesize_prof)

# Results for effort
### Each item in the vector collects the beta coefficient for each row, the corresponding stars according to its p-value, and (in brackets) its standard error
betas_eff <- c(paste0(paste0(round(m1$estimates$selection[1,1],3),ifelse(summary(m1)$amce[5,6]<0.001,"***", ifelse(summary(m1)$amce[5,6]<0.01,"**", ifelse(summary(m1)$amce[5,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$selection[2,1],3),")",sep=""),sep=""),
               paste0(paste0(round(m1$estimates$politicalconnections[1,1],3),ifelse(summary(m1)$amce[4,6]<0.001,"***", ifelse(summary(m1)$amce[4,6]<0.01,"**", ifelse(summary(m1)$amce[4,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$politicalconnections[2,1],3),")",sep=""),sep=""),
               paste0(paste0(round(m1$estimates$education[1,1],3),ifelse(summary(m1)$amce[1,6]<0.001,"***", ifelse(summary(m1)$amce[1,6]<0.01,"**", ifelse(summary(m1)$amce[1,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$education[2,1],3),")",sep=""),sep=""),
               paste0(paste0(round(m1$estimates$experience[1,1],3),ifelse(summary(m1)$amce[2,6]<0.001,"***", ifelse(summary(m1)$amce[2,6]<0.01,"**", ifelse(summary(m1)$amce[2,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$experience[2,1],3),")",sep=""),sep=""),
               paste0(paste0(round(m1$estimates$unionmembership[1,1],3),ifelse(summary(m1)$amce[6,6]<0.001,"***", ifelse(summary(m1)$amce[6,6]<0.01,"**", ifelse(summary(m1)$amce[6,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$unionmembership[2,1],3),")",sep=""),sep=""),
               paste0(paste0(round(m1$estimates$gender[1,1],3),ifelse(summary(m1)$amce[3,6]<0.001,"***", ifelse(summary(m1)$amce[3,6]<0.01,"**", ifelse(summary(m1)$amce[3,6]<0.05,"*",""))),sep=""),paste0(" (",round(m1$estimates$gender[2,1],3),")",sep=""),sep=""),
               m1$numrespondents, m1$samplesize_prof)

# Results for performance
### Each item in the vector collects the beta coefficient for each row, the corresponding stars according to its p-value, and (in brackets) its standard error
betas_perf <- c(paste0(paste0(round(m2$estimates$selection[1,1],3),ifelse(summary(m2)$amce[5,6]<0.001,"***", ifelse(summary(m2)$amce[5,6]<0.01,"**", ifelse(summary(m2)$amce[5,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$selection[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m2$estimates$politicalconnections[1,1],3),ifelse(summary(m2)$amce[4,6]<0.001,"***", ifelse(summary(m2)$amce[4,6]<0.01,"**", ifelse(summary(m2)$amce[4,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$politicalconnections[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m2$estimates$education[1,1],3),ifelse(summary(m2)$amce[1,6]<0.001,"***", ifelse(summary(m2)$amce[1,6]<0.01,"**", ifelse(summary(m2)$amce[1,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$education[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m2$estimates$experience[1,1],3),ifelse(summary(m2)$amce[2,6]<0.001,"***", ifelse(summary(m2)$amce[2,6]<0.01,"**", ifelse(summary(m2)$amce[2,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$experience[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m2$estimates$unionmembership[1,1],3),ifelse(summary(m2)$amce[6,6]<0.001,"***", ifelse(summary(m2)$amce[6,6]<0.01,"**", ifelse(summary(m2)$amce[6,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$unionmembership[2,1],3),")",sep=""),sep=""),
                paste0(paste0(round(m2$estimates$gender[1,1],3),ifelse(summary(m2)$amce[3,6]<0.001,"***", ifelse(summary(m2)$amce[3,6]<0.01,"**", ifelse(summary(m2)$amce[3,6]<0.05,"*",""))),sep=""),paste0(" (",round(m2$estimates$gender[2,1],3),")",sep=""),sep=""),
                m2$numrespondents, m2$samplesize_prof)

# Bind it all in a table and name the columns
results_conjoint_politicians <- as_tibble(cbind(vars,betas_comm, betas_impl, betas_eff, betas_perf))
colnames(results_conjoint_politicians) <- c("", "Communication","Implementation","Effort","Performance")

# Export the table
print(file="../../tables/conjoint_politicians.tex",
      only.contents=TRUE,
      type="latex",
      xtable(results_conjoint_politicians,
             align=c("lccccc")),
      caption.placement="top", 
      include.rownames=FALSE, 
      comment=F,
      floating=F,
      booktabs=F)

# NOTES -- R version, platform, and loaded packages -------------------------

### sessionInfo(package = NULL)

# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS  10.16
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] cjoint_2.1.0    survey_4.0      survival_3.1-11 Matrix_1.2-18   lmtest_0.9-37   zoo_1.8-7       sandwich_2.5-1 
# [8] texreg_1.37.1   codebook_0.9.2  readxl_1.3.1    here_1.0.1      forcats_0.5.0   stringr_1.4.0   dplyr_1.0.0    
# [15] purrr_0.3.3     readr_1.4.0     tidyr_1.0.2     tibble_3.0.0    ggplot2_3.3.5   tidyverse_1.3.0
# 
# loaded via a namespace (and not attached):
#   [1] nlme_3.1-145      fs_1.4.1          lubridate_1.7.4   webshot_0.5.2     httr_1.4.1        rprojroot_2.0.2  
# [7] repr_1.1.0        tools_3.6.3       backports_1.1.5   utf8_1.1.4        R6_2.4.1          DT_0.13          
# [13] DBI_1.1.0         colorspace_1.4-1  withr_2.1.2       tidyselect_1.1.0  compiler_3.6.3    cli_2.3.0        
# [19] rvest_0.3.5       xml2_1.3.0        labeling_0.3      scales_1.1.0      digest_0.6.25     rmarkdown_2.6    
# [25] base64enc_0.1-3   pkgconfig_2.0.3   htmltools_0.4.0   labelled_2.5.0    fastmap_1.0.1     dbplyr_1.4.2     
# [31] highr_0.8         htmlwidgets_1.5.1 rlang_1.0.0       rstudioapi_0.11   shiny_1.4.0.2     farver_2.0.3     
# [37] generics_0.0.2    jsonlite_1.6.1    crosstalk_1.1.0.1 magrittr_1.5      Rcpp_1.0.7        munsell_0.5.0    
# [43] fansi_0.4.1       lifecycle_0.2.0   stringi_1.4.6     yaml_2.2.1        rmdpartials_0.5.8 plyr_1.8.6       
# [49] parallel_3.6.3    listenv_0.8.0     promises_1.1.0    crayon_1.3.4      lattice_0.20-40   haven_2.3.1      
# [55] splines_3.6.3     hms_0.5.3         knitr_1.28        pillar_1.4.3      codetools_0.2-16  reprex_0.3.0     
# [61] glue_1.3.2        evaluate_0.14     mitools_2.4       modelr_0.1.6      vctrs_0.3.1       httpuv_1.5.2     
# [67] cellranger_1.1.0  gtable_0.3.0      future_1.17.0     assertthat_0.2.1  xfun_0.20         mime_0.9         
# [73] skimr_2.1.3       xtable_1.8-4      broom_0.5.5       later_1.0.0       rsconnect_0.8.16  globals_0.12.5   
# [79] ellipsis_0.3.0  