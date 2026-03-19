
rm (list = ls(all=TRUE))

#set your working directory

setwd("")

#install necessary packages

install.packages("estimatr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("survey")
install.packages("dplyr")
install.packages("haven")

library(estimatr)
library(ggplot2)
library(GGally)
library(survey)
library(dplyr)
library(haven)

###Table4####

teacher_baseline <- 
  read.csv("teacher_2_baseline.csv", header=T, stringsAsFactors = F)

#arrange dataframe for statistical tests

teacher_baseline_r <- svydesign(id = ~0, 
                                strata=~
                                  department*urban.rural,
                                data = teacher_baseline, nest=T)

school_baseline <- 
  read.csv("school_principal_2_baseline.csv", header=T, stringsAsFactors = F)

#arrange dataframe for statistical tests

school_baseline_r <- svydesign(id = ~0, 
                                strata=~
                                  department*urban.rural,
                                data = school_baseline, nest=T)

###teacher section

teacher_baseline_t <- subset(teacher_baseline,
                             treatment_d==1)

teacher_baseline_c <- subset(teacher_baseline,
                             treatment_d==0)

#teacher sex

mean(teacher_baseline_t$Teacher_Sex, na.rm=T)*100
mean(teacher_baseline_c$Teacher_Sex, na.rm=T)*100

Diff_sex <- mean(teacher_baseline_t$Teacher_Sex, na.rm=T)*100-
  mean(teacher_baseline_c$Teacher_Sex, na.rm=T)*100

p_Sex <- svychisq(~Teacher_Sex+treatment_d, teacher_baseline_r,
                  statistic = c("Chisq"))

#teacher age

mean(teacher_baseline_t$Teacher_Age, na.rm=T)
mean(teacher_baseline_c$Teacher_Age, na.rm=T)

sd(teacher_baseline_t$Teacher_Age, na.rm=T)
sd(teacher_baseline_c$Teacher_Age, na.rm=T)

Diff_age <- mean(teacher_baseline_t$Teacher_Age, na.rm=T)-
  mean(teacher_baseline_c$Teacher_Age, na.rm=T)

p_Teacher_Age <- svyranktest(Teacher_Age~treatment_d, teacher_baseline_r,
                             test = c("wilcoxon"))

#teaching period

mean(teacher_baseline_t$Year_Start_teach_r, na.rm=T)
mean(teacher_baseline_c$Year_Start_teach_r, na.rm=T)

sd(teacher_baseline_t$Year_Start_teach_r, na.rm=T)
sd(teacher_baseline_c$Year_Start_teach_r, na.rm=T)

Diff_Year_Start_teach_r <- mean(teacher_baseline_t$Year_Start_teach_r, na.rm=T)-
  mean(teacher_baseline_c$Year_Start_teach_r, na.rm=T)

p_Year_Start_teach_r <- svyranktest(Year_Start_teach_r~treatment_d, teacher_baseline_r,
                                    test = c("wilcoxon"))

#teaching other subject

mean(teacher_baseline_t$Teaching_other_subject_G2, na.rm=T)*100
mean(teacher_baseline_c$Teaching_other_subject_G2, na.rm=T)*100

Diff_Teaching_other_subject_G2 <- 
  mean(teacher_baseline_t$Teaching_other_subject_G2, na.rm=T)*100-
  mean(teacher_baseline_c$Teaching_other_subject_G2, na.rm=T)*100

p_Teaching_other_subject_G2 <- 
  svychisq(~Teaching_other_subject_G2+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#highest degree: high school

mean(teacher_baseline_t$Highest_Degree_highschool, na.rm=T)*100
mean(teacher_baseline_c$Highest_Degree_highschool, na.rm=T)*100

Diff_Highest_Degree_highschool <- 
  mean(teacher_baseline_t$Highest_Degree_highschool, na.rm=T)*100-
  mean(teacher_baseline_c$Highest_Degree_highschool, na.rm=T)*100

p_Highest_Degree_highschool <- 
  svychisq(~Highest_Degree_highschool+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#highest degree: professorate

mean(teacher_baseline_t$Highest_Degree_professorate, na.rm=T)*100
mean(teacher_baseline_c$Highest_Degree_professorate, na.rm=T)*100

Diff_Highest_Degree_professorate <- 
  mean(teacher_baseline_t$Highest_Degree_professorate, na.rm=T)*100-
  mean(teacher_baseline_c$Highest_Degree_professorate, na.rm=T)*100

p_Highest_Degree_professorate <- 
  svychisq(~Highest_Degree_professorate+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#highest degree: bachelor

mean(teacher_baseline_t$Highest_Degree_bachelor, na.rm=T)*100
mean(teacher_baseline_c$Highest_Degree_bachelor, na.rm=T)*100

Diff_Highest_Degree_bachelor <- 
  mean(teacher_baseline_t$Highest_Degree_bachelor, na.rm=T)*100-
  mean(teacher_baseline_c$Highest_Degree_bachelor, na.rm=T)*100

p_Highest_Degree_bachelor <- 
  svychisq(~Highest_Degree_bachelor+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

mean(teacher_baseline$Highest_Degree_master, na.rm=T)
mean(teacher_baseline$Highest_Degree_doctor, na.rm=T)

#teacher qualification (1): Pedagogical Bachelor

mean(teacher_baseline_t$Teachr_Qualification__1, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Qualification__1, na.rm=T)*100

Diff_Teachr_Qualification__1 <- 
  mean(teacher_baseline_t$Teachr_Qualification__1, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Qualification__1, na.rm=T)*100

p_Teachr_Qualification__1 <- 
  svychisq(~Teachr_Qualification__1+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (1): Professorate

mean(teacher_baseline_t$Teachr_Qualification__2, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Qualification__2, na.rm=T)*100

Diff_Teachr_Qualification__2 <- 
  mean(teacher_baseline_t$Teachr_Qualification__2, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Qualification__2, na.rm=T)*100

p_Teachr_Qualification__2 <- 
  svychisq(~Teachr_Qualification__2+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (1): Bachelor in Education

mean(teacher_baseline_t$Teachr_Qualification__3, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Qualification__3, na.rm=T)*100

Diff_Teachr_Qualification__3 <- 
  mean(teacher_baseline_t$Teachr_Qualification__3, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Qualification__3, na.rm=T)*100

p_Teachr_Qualification__3 <- 
  svychisq(~Teachr_Qualification__3+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

mean(teacher_baseline$Teachr_Qualification__4, na.rm=T)
mean(teacher_baseline$Teachr_Qualification__5, na.rm=T)

#teacher qualification (1): Pedagogical Training Course

mean(teacher_baseline_t$Teachr_Qualification__6, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Qualification__6, na.rm=T)*100

Diff_Teachr_Qualification__6 <- 
  mean(teacher_baseline_t$Teachr_Qualification__6, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Qualification__6, na.rm=T)*100

p_Teachr_Qualification__6 <- 
  svychisq(~Teachr_Qualification__6+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (2): Basic Education Teacher (cycle 1 and 2) 

mean(teacher_baseline_t$Teachr_Apontmt_Post__1, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Apontmt_Post__1, na.rm=T)*100

Diff_Teachr_Apontmt_Post__1 <- 
  mean(teacher_baseline_t$Teachr_Apontmt_Post__1, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Apontmt_Post__1, na.rm=T)*100

p_Teachr_Apontmt_Post__1 <- 
  svychisq(~Teachr_Apontmt_Post__1+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (2): Mathematics Specialty Teacher (cycle 3)

mean(teacher_baseline_t$Teachr_Apontmt_Post__2, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Apontmt_Post__2, na.rm=T)*100

Diff_Teachr_Apontmt_Post__2 <- 
  mean(teacher_baseline_t$Teachr_Apontmt_Post__2, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Apontmt_Post__2, na.rm=T)*100

p_Teachr_Apontmt_Post__2 <- 
  svychisq(~Teachr_Apontmt_Post__2+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (2): Teacher specialized in other than math (cycle 3) 

mean(teacher_baseline_t$Teachr_Apontmt_Post__3, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Apontmt_Post__3, na.rm=T)*100

Diff_Teachr_Apontmt_Post__3 <- 
  mean(teacher_baseline_t$Teachr_Apontmt_Post__3, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Apontmt_Post__3, na.rm=T)*100

p_Teachr_Apontmt_Post__3 <- 
  svychisq(~Teachr_Apontmt_Post__3+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (2): others 

mean(teacher_baseline_t$Teachr_Apontmt_Post__4, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Apontmt_Post__4, na.rm=T)*100

Diff_Teachr_Apontmt_Post__4 <- 
  mean(teacher_baseline_t$Teachr_Apontmt_Post__4, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Apontmt_Post__4, na.rm=T)*100

p_Teachr_Apontmt_Post__4 <- 
  svychisq(~Teachr_Apontmt_Post__4+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

###school section in table 4

school_baseline_t <- subset(school_baseline,
                             treatment_d==1)

school_baseline_c <- subset(school_baseline,
                             treatment_d==0)

#total number of 2nd grade students AM shift

school_baseline_t$Number_G2_Students_AM_r <- 
  replace(school_baseline_t$Number_G2_Students_AM_r, 
          which(school_baseline_t$Number_G2_Students_AM_r==0), NA)

school_baseline_c$Number_G2_Students_AM_r <- 
  replace(school_baseline_c$Number_G2_Students_AM_r, 
          which(school_baseline_c$Number_G2_Students_AM_r==0), NA)

mean(school_baseline_t$Number_G2_Students_AM_r, na.rm=T)
mean(school_baseline_c$Number_G2_Students_AM_r, na.rm=T)

sd(school_baseline_t$Number_G2_Students_AM_r, na.rm=T)
sd(school_baseline_c$Number_G2_Students_AM_r, na.rm=T)

Diff_Number_G2_Students_AM <- mean(school_baseline_t$Number_G2_Students_AM_r, na.rm=T)-
  mean(school_baseline_c$Number_G2_Students_AM_r, na.rm=T)

p_Number_G2_Students_AM <- svyranktest(Number_G2_Students_AM_r~treatment_d, school_baseline_r,
                                    test = c("wilcoxon"))

#total number of 2nd grade students PM shift

school_baseline_t$Number_G2_Students_PM_r <- 
  replace(school_baseline_t$Number_G2_Students_PM_r, 
          which(school_baseline_t$Number_G2_Students_PM_r==0), NA)

school_baseline_c$Number_G2_Students_PM_r <- 
  replace(school_baseline_c$Number_G2_Students_PM_r, 
          which(school_baseline_c$Number_G2_Students_PM_r==0), NA)

mean(school_baseline_t$Number_G2_Students_PM_r, na.rm=T)
mean(school_baseline_c$Number_G2_Students_PM_r, na.rm=T)

sd(school_baseline_t$Number_G2_Students_PM_r, na.rm=T)
sd(school_baseline_c$Number_G2_Students_PM_r, na.rm=T)

Diff_Number_G2_Students_PM <- mean(school_baseline_t$Number_G2_Students_PM_r, na.rm=T)-
  mean(school_baseline_c$Number_G2_Students_PM_r, na.rm=T)

school_baseline$Number_G2_Students_PM_r <- 
  replace(school_baseline$Number_G2_Students_PM_r, 
          which(school_baseline$Number_G2_Students_PM_r==0), NA)

school_baseline_r <- svydesign(id = ~0, 
                               strata=~
                                 department*urban.rural,
                               data = school_baseline, nest=T)

p_Number_G2_Students_PM <- svyranktest(Number_G2_Students_PM_r~treatment_d, school_baseline_r,
                                       test = c("wilcoxon"))

#total number of students

mean(school_baseline_t$Number_total_Students, na.rm=T)
mean(school_baseline_c$Number_total_Students, na.rm=T)

sd(school_baseline_t$Number_total_Students, na.rm=T)
sd(school_baseline_c$Number_total_Students, na.rm=T)

Diff_Number_total_Students <- mean(school_baseline_t$Number_total_Students, na.rm=T)-
  mean(school_baseline_c$Number_total_Students, na.rm=T)

p_Number_total_Students <- svyranktest(Number_total_Students~treatment_d, school_baseline_r,
                                       test = c("wilcoxon"))

#repetition rate grade 2 AM shift

mean(school_baseline_t$repeat_rate_g2_am, na.rm=T)*100
mean(school_baseline_c$repeat_rate_g2_am, na.rm=T)*100

Diff_repeat_rate_g2_am <- mean(school_baseline_t$repeat_rate_g2_am, na.rm=T)*100-
  mean(school_baseline_c$repeat_rate_g2_am, na.rm=T)*100

p_repeat_rate_g2_am <- svyranktest(repeat_rate_g2_am~treatment_d, school_baseline_r,
                                       test = c("wilcoxon"))

#repetition rate grade 2 PM shift

mean(school_baseline_t$repeat_rate_g2_pm, na.rm=T)*100
mean(school_baseline_c$repeat_rate_g2_pm, na.rm=T)*100

Diff_repeat_rate_g2_pm <- mean(school_baseline_t$repeat_rate_g2_pm, na.rm=T)*100-
  mean(school_baseline_c$repeat_rate_g2_pm, na.rm=T)*100

p_repeat_rate_g2_pm <- svyranktest(repeat_rate_g2_pm~treatment_d, school_baseline_r,
                                   test = c("wilcoxon"))

#dropout rate grade 2 AM shift

mean(school_baseline_t$drop_out_rate_g2_am, na.rm=T)*100
mean(school_baseline_c$drop_out_rate_g2_am, na.rm=T)*100

Diff_drop_out_rate_g2_am <- mean(school_baseline_t$drop_out_rate_g2_am, na.rm=T)*100-
  mean(school_baseline_c$drop_out_rate_g2_am, na.rm=T)*100

p_drop_out_rate_g2_am <- svyranktest(drop_out_rate_g2_am~treatment_d, school_baseline_r,
                                     test = c("wilcoxon"))

#dropout rate grade 2 PM shift

mean(school_baseline_t$drop_out_rate_g2_pm, na.rm=T)*100
mean(school_baseline_c$drop_out_rate_g2_pm, na.rm=T)*100

Diff_drop_out_rate_g2_pm <- mean(school_baseline_t$drop_out_rate_g2_pm, na.rm=T)*100-
  mean(school_baseline_c$drop_out_rate_g2_pm, na.rm=T)*100

p_drop_out_rate_g2_pm <- svyranktest(drop_out_rate_g2_pm~treatment_d, school_baseline_r,
                                   test = c("wilcoxon"))

#number of teachers

mean(school_baseline_t$Number_of_Teachers, na.rm=T)
mean(school_baseline_c$Number_of_Teachers, na.rm=T)

sd(school_baseline_t$Number_of_Teachers, na.rm=T)
sd(school_baseline_c$Number_of_Teachers, na.rm=T)

Diff_Number_of_Teachers <- mean(school_baseline_t$Number_of_Teachers, na.rm=T)-
  mean(school_baseline_c$Number_of_Teachers, na.rm=T)

p_Number_of_Teachers <- svyranktest(Number_of_Teachers~treatment_d, school_baseline_r,
                                     test = c("wilcoxon"))

#school facility: Electricity

mean(school_baseline_t$School_Facility__1, na.rm=T)*100
mean(school_baseline_c$School_Facility__1, na.rm=T)*100

Diff_School_Facility__1 <- 
  mean(school_baseline_t$School_Facility__1, na.rm=T)*100-
  mean(school_baseline_c$School_Facility__1, na.rm=T)*100

p_School_Facility__1 <- 
  svychisq(~School_Facility__1+treatment_d, school_baseline_r,
           statistic = c("Chisq"))

#school facility: drinking water

mean(school_baseline_t$School_Facility__2, na.rm=T)*100
mean(school_baseline_c$School_Facility__2, na.rm=T)*100

Diff_School_Facility__2 <- 
  mean(school_baseline_t$School_Facility__2, na.rm=T)*100-
  mean(school_baseline_c$School_Facility__2, na.rm=T)*100

p_School_Facility__2 <- 
  svychisq(~School_Facility__2+treatment_d, school_baseline_r,
           statistic = c("Chisq"))

#school facility: computer

mean(school_baseline_t$School_Facility__3, na.rm=T)*100
mean(school_baseline_c$School_Facility__3, na.rm=T)*100

Diff_School_Facility__3 <- 
  mean(school_baseline_t$School_Facility__3, na.rm=T)*100-
  mean(school_baseline_c$School_Facility__3, na.rm=T)*100

p_School_Facility__3 <- 
  svychisq(~School_Facility__3+treatment_d, school_baseline_r,
           statistic = c("Chisq"))

#school facility: library

mean(school_baseline_t$School_Facility__5, na.rm=T)*100
mean(school_baseline_c$School_Facility__5, na.rm=T)*100

Diff_School_Facility__5 <- 
  mean(school_baseline_t$School_Facility__5, na.rm=T)*100-
  mean(school_baseline_c$School_Facility__5, na.rm=T)*100

p_School_Facility__5 <- 
  svychisq(~School_Facility__5+treatment_d, school_baseline_r,
           statistic = c("Chisq"))

#school facility: laboratory

mean(school_baseline_t$School_Facility__6, na.rm=T)*100
mean(school_baseline_c$School_Facility__6, na.rm=T)*100

Diff_School_Facility__6 <- 
  mean(school_baseline_t$School_Facility__6, na.rm=T)*100-
  mean(school_baseline_c$School_Facility__6, na.rm=T)*100

p_School_Facility__6 <- 
  svychisq(~School_Facility__6+treatment_d, school_baseline_r,
           statistic = c("Chisq"))

#donor support

mean(school_baseline_t$Donor_Project, na.rm=T)*100
mean(school_baseline_c$Donor_Project, na.rm=T)*100

Diff_Donor_Project <- 
  mean(school_baseline_t$Donor_Project, na.rm=T)*100-
  mean(school_baseline_c$Donor_Project, na.rm=T)*100

p_Donor_Project <- 
  svychisq(~Donor_Project+treatment_d, school_baseline_r,
           statistic = c("Chisq"))

###Table A-3 in Appendix A####

rm (list = ls(all=TRUE))

#teacher data

teacher_baseline_original <- 
  read.csv("teacher_2_baseline.csv", header=T, stringsAsFactors = F)

teacher_baseline_remaining <- subset(teacher_baseline_original,
                                    !(teacher_baseline_original$ID_IE==430323|
                                        teacher_baseline_original$ID_IE==430336|
                                        teacher_baseline_original$ID_IE==426534))

teacher_baseline_original$original <- c(1)
teacher_baseline_remaining$original <- c(0)

teacher_baseline_check <- rbind.data.frame(teacher_baseline_original,
                                           teacher_baseline_remaining)

#arrange dataframe for statistical tests

teacher_baseline_check_r <- svydesign(id = ~0, 
                                     strata=~
                                       department*urban.rural,
                                     data = teacher_baseline_check, nest=T)

#school data

school_baseline_original <- 
  read.csv("school_principal_2_baseline.csv", header=T, stringsAsFactors = F)

school_baseline_remaining <- subset(school_baseline_original,
                                    !(school_baseline_original$ID_IE==430323|
                                        school_baseline_original$ID_IE==430336|
                                        school_baseline_original$ID_IE==426534))

school_baseline_original$original <- c(1)
school_baseline_remaining$original <- c(0)

school_baseline_check <- rbind.data.frame(school_baseline_original,
                                          school_baseline_remaining)

#arrange dataframe for statistical tests

school_baseline_check_r <- svydesign(id = ~0, 
                               strata=~
                                 department*urban.rural,
                               data = school_baseline_check, nest=T)

###table A-3 in Appendix A

#teacher sex

mean(teacher_baseline_original$Teacher_Sex, na.rm=T)*100
mean(teacher_baseline_remaining$Teacher_Sex, na.rm=T)*100

Diff_sex <- mean(teacher_baseline_original$Teacher_Sex, na.rm=T)*100-
  mean(teacher_baseline_remaining$Teacher_Sex, na.rm=T)*100

p_Sex <- svychisq(~Teacher_Sex+original, teacher_baseline_check_r,
                  statistic = c("Chisq"))

#teacher age

mean(teacher_baseline_original$Teacher_Age, na.rm=T)
mean(teacher_baseline_remaining$Teacher_Age, na.rm=T)

Diff_age <- mean(teacher_baseline_original$Teacher_Age, na.rm=T)-
  mean(teacher_baseline_remaining$Teacher_Age, na.rm=T)

p_Teacher_Age <- svyranktest(Teacher_Age~original, teacher_baseline_check_r,
                             test = c("wilcoxon"))

#teaching period

mean(teacher_baseline_original$Year_Start_teach_r, na.rm=T)
mean(teacher_baseline_remaining$Year_Start_teach_r, na.rm=T)

Diff_Year_Start_teach_r <- mean(teacher_baseline_original$Year_Start_teach_r, na.rm=T)-
  mean(teacher_baseline_remaining$Year_Start_teach_r, na.rm=T)

p_Year_Start_teach_r <- svyranktest(Year_Start_teach_r~original, teacher_baseline_check_r,
                                    test = c("wilcoxon"))

#teaching other subject

mean(teacher_baseline_original$Teaching_other_subject_G2, na.rm=T)*100
mean(teacher_baseline_remaining$Teaching_other_subject_G2, na.rm=T)*100

Diff_Teaching_other_subject_G2 <- 
  mean(teacher_baseline_original$Teaching_other_subject_G2, na.rm=T)*100-
  mean(teacher_baseline_remaining$Teaching_other_subject_G2, na.rm=T)*100

p_Teaching_other_subject_G2 <- 
  svychisq(~Teaching_other_subject_G2+original, teacher_baseline_check_r,
           statistic = c("Chisq"))

#highest degree: high school

mean(teacher_baseline_original$Highest_Degree_highschool, na.rm=T)*100
mean(teacher_baseline_remaining$Highest_Degree_highschool, na.rm=T)*100

Diff_Highest_Degree_highschool <- 
  mean(teacher_baseline_original$Highest_Degree_highschool, na.rm=T)*100-
  mean(teacher_baseline_remaining$Highest_Degree_highschool, na.rm=T)*100

p_Highest_Degree_highschool <- 
  svychisq(~Highest_Degree_highschool+original, teacher_baseline_check_r,
           statistic = c("Chisq"))

#highest degree: professorate

mean(teacher_baseline_original$Highest_Degree_professorate, na.rm=T)*100
mean(teacher_baseline_remaining$Highest_Degree_professorate, na.rm=T)*100

Diff_Highest_Degree_professorate <- 
  mean(teacher_baseline_original$Highest_Degree_professorate, na.rm=T)*100-
  mean(teacher_baseline_remaining$Highest_Degree_professorate, na.rm=T)*100

p_Highest_Degree_professorate <- 
  svychisq(~Highest_Degree_professorate+original, teacher_baseline_check_r,
           statistic = c("Chisq"))

#highest degree: bachelor

mean(teacher_baseline_original$Highest_Degree_bachelor, na.rm=T)*100
mean(teacher_baseline_remaining$Highest_Degree_bachelor, na.rm=T)*100

Diff_Highest_Degree_bachelor <- 
  mean(teacher_baseline_original$Highest_Degree_bachelor, na.rm=T)*100-
  mean(teacher_baseline_remaining$Highest_Degree_bachelor, na.rm=T)*100

p_Highest_Degree_bachelor <- 
  svychisq(~Highest_Degree_bachelor+original, teacher_baseline_check_r,
           statistic = c("Chisq"))

mean(teacher_baseline_original$Highest_Degree_master, na.rm=T)
mean(teacher_baseline_original$Highest_Degree_doctor, na.rm=T)

#teacher qualification (1): Pedagogical Bachelor

mean(teacher_baseline_original$Teachr_Qualification__1, na.rm=T)*100
mean(teacher_baseline_remaining$Teachr_Qualification__1, na.rm=T)*100

Diff_Teachr_Qualification__1 <- 
  mean(teacher_baseline_original$Teachr_Qualification__1, na.rm=T)*100-
  mean(teacher_baseline_remaining$Teachr_Qualification__1, na.rm=T)*100

p_Teachr_Qualification__1 <- 
  svychisq(~Teachr_Qualification__1+original, teacher_baseline_check_r,
           statistic = c("Chisq"))

#teacher qualification (1): Professorate

mean(teacher_baseline_original$Teachr_Qualification__2, na.rm=T)*100
mean(teacher_baseline_remaining$Teachr_Qualification__2, na.rm=T)*100

Diff_Teachr_Qualification__2 <- 
  mean(teacher_baseline_original$Teachr_Qualification__2, na.rm=T)*100-
  mean(teacher_baseline_remaining$Teachr_Qualification__2, na.rm=T)*100

p_Teachr_Qualification__2 <- 
  svychisq(~Teachr_Qualification__2+original, teacher_baseline_check_r,
           statistic = c("Chisq"))

#teacher qualification (1): Bachelor in Education

mean(teacher_baseline_original$Teachr_Qualification__3, na.rm=T)*100
mean(teacher_baseline_remaining$Teachr_Qualification__3, na.rm=T)*100

Diff_Teachr_Qualification__3 <- 
  mean(teacher_baseline_original$Teachr_Qualification__3, na.rm=T)*100-
  mean(teacher_baseline_remaining$Teachr_Qualification__3, na.rm=T)*100

p_Teachr_Qualification__3 <- 
  svychisq(~Teachr_Qualification__3+original, teacher_baseline_check_r,
           statistic = c("Chisq"))

mean(teacher_baseline_original$Teachr_Qualification__4, na.rm=T)
mean(teacher_baseline_original$Teachr_Qualification__5, na.rm=T)

#teacher qualification (1): Pedagogical Training Course

mean(teacher_baseline_original$Teachr_Qualification__6, na.rm=T)*100
mean(teacher_baseline_remaining$Teachr_Qualification__6, na.rm=T)*100

Diff_Teachr_Qualification__6 <- 
  mean(teacher_baseline_original$Teachr_Qualification__6, na.rm=T)*100-
  mean(teacher_baseline_remaining$Teachr_Qualification__6, na.rm=T)*100

p_Teachr_Qualification__6 <- 
  svychisq(~Teachr_Qualification__6+original, teacher_baseline_check_r,
           statistic = c("Chisq"))

#teacher qualification (2): Basic Education Teacher (cycle 1 and 2) 

mean(teacher_baseline_original$Teachr_Apontmt_Post__1, na.rm=T)*100
mean(teacher_baseline_remaining$Teachr_Apontmt_Post__1, na.rm=T)*100

Diff_Teachr_Apontmt_Post__1 <- 
  mean(teacher_baseline_original$Teachr_Apontmt_Post__1, na.rm=T)*100-
  mean(teacher_baseline_remaining$Teachr_Apontmt_Post__1, na.rm=T)*100

p_Teachr_Apontmt_Post__1 <- 
  svychisq(~Teachr_Apontmt_Post__1+original, teacher_baseline_check_r,
           statistic = c("Chisq"))

#teacher qualification (2): Mathematics Specialty Teacher (cycle 3)

mean(teacher_baseline_original$Teachr_Apontmt_Post__2, na.rm=T)*100
mean(teacher_baseline_remaining$Teachr_Apontmt_Post__2, na.rm=T)*100

Diff_Teachr_Apontmt_Post__2 <- 
  mean(teacher_baseline_original$Teachr_Apontmt_Post__2, na.rm=T)*100-
  mean(teacher_baseline_remaining$Teachr_Apontmt_Post__2, na.rm=T)*100

p_Teachr_Apontmt_Post__2 <- 
  svychisq(~Teachr_Apontmt_Post__2+original, teacher_baseline_check_r,
           statistic = c("Chisq"))

#teacher qualification (2): Teacher specialized in other than math (cycle 3) 

mean(teacher_baseline_original$Teachr_Apontmt_Post__3, na.rm=T)*100
mean(teacher_baseline_remaining$Teachr_Apontmt_Post__3, na.rm=T)*100

Diff_Teachr_Apontmt_Post__3 <- 
  mean(teacher_baseline_original$Teachr_Apontmt_Post__3, na.rm=T)*100-
  mean(teacher_baseline_remaining$Teachr_Apontmt_Post__3, na.rm=T)*100

p_Teachr_Apontmt_Post__3 <- 
  svychisq(~Teachr_Apontmt_Post__3+original, teacher_baseline_check_r,
           statistic = c("Chisq"))

#teacher qualification (2): Teacher specialized in other than math (cycle 3) 

mean(teacher_baseline_original$Teachr_Apontmt_Post__4, na.rm=T)*100
mean(teacher_baseline_remaining$Teachr_Apontmt_Post__4, na.rm=T)*100

Diff_Teachr_Apontmt_Post__4 <- 
  mean(teacher_baseline_original$Teachr_Apontmt_Post__4, na.rm=T)*100-
  mean(teacher_baseline_remaining$Teachr_Apontmt_Post__4, na.rm=T)*100

p_Teachr_Apontmt_Post__4 <- 
  svychisq(~Teachr_Apontmt_Post__4+original, teacher_baseline_check_r,
           statistic = c("Chisq"))

###school section in table A-3

#total number of 2nd grade students AM shift

school_baseline_original$Number_G2_Students_AM_r <- 
  replace(school_baseline_original$Number_G2_Students_AM_r, 
          which(school_baseline_original$Number_G2_Students_AM_r==0), NA)

school_baseline_remaining$Number_G2_Students_AM_r <- 
  replace(school_baseline_remaining$Number_G2_Students_AM_r, 
          which(school_baseline_remaining$Number_G2_Students_AM_r==0), NA)

mean(school_baseline_original$Number_G2_Students_AM_r, na.rm=T)
mean(school_baseline_remaining$Number_G2_Students_AM_r, na.rm=T)

Diff_Number_G2_Students_AM <- mean(school_baseline_original$Number_G2_Students_AM, na.rm=T)-
  mean(school_baseline_remaining$Number_G2_Students_AM, na.rm=T)

p_Number_G2_Students_AM <- svyranktest(Number_G2_Students_AM~original, school_baseline_check_r,
                                       test = c("wilcoxon"))

#total number of 2nd grade students PM shift

school_baseline_original$Number_G2_Students_PM_r <- 
  replace(school_baseline_original$Number_G2_Students_PM_r, 
          which(school_baseline_original$Number_G2_Students_PM_r==0), NA)

school_baseline_remaining$Number_G2_Students_PM_r <- 
  replace(school_baseline_remaining$Number_G2_Students_PM_r, 
          which(school_baseline_remaining$Number_G2_Students_PM_r==0), NA)

mean(school_baseline_original$Number_G2_Students_PM_r, na.rm=T)
mean(school_baseline_remaining$Number_G2_Students_PM_r, na.rm=T)

Diff_Number_G2_Students_PM <- mean(school_baseline_original$Number_G2_Students_PM_r, na.rm=T)-
  mean(school_baseline_remaining$Number_G2_Students_PM_r, na.rm=T)

p_Number_G2_Students_PM <- svyranktest(Number_G2_Students_PM_r~original, school_baseline_check_r,
                                       test = c("wilcoxon"))

#total number of students

mean(school_baseline_original$Number_total_Students, na.rm=T)
mean(school_baseline_remaining$Number_total_Students, na.rm=T)

Diff_Number_total_Students <- mean(school_baseline_original$Number_total_Students, na.rm=T)-
  mean(school_baseline_remaining$Number_total_Students, na.rm=T)

p_Number_total_Students <- svyranktest(Number_total_Students~original, school_baseline_check_r,
                                       test = c("wilcoxon"))

#repetition rate grade 2 AM shift

mean(school_baseline_original$repeat_rate_g2_am, na.rm=T)*100
mean(school_baseline_remaining$repeat_rate_g2_am, na.rm=T)*100

Diff_repeat_rate_g2_am <- mean(school_baseline_original$repeat_rate_g2_am, na.rm=T)*100-
  mean(school_baseline_remaining$repeat_rate_g2_am, na.rm=T)*100

p_repeat_rate_g2_am <- svyranktest(repeat_rate_g2_am~original, school_baseline_check_r,
                                   test = c("wilcoxon"))

#repetition rate grade 2 PM shift

mean(school_baseline_original$repeat_rate_g2_pm, na.rm=T)*100
mean(school_baseline_remaining$repeat_rate_g2_pm, na.rm=T)*100

Diff_repeat_rate_g2_pm <- mean(school_baseline_original$repeat_rate_g2_pm, na.rm=T)*100-
  mean(school_baseline_remaining$repeat_rate_g2_pm, na.rm=T)*100

p_repeat_rate_g2_pm <- svyranktest(repeat_rate_g2_pm~original, school_baseline_check_r,
                                   test = c("wilcoxon"))

#dropout rate grade 2 AM shift

mean(school_baseline_original$drop_out_rate_g2_am, na.rm=T)*100
mean(school_baseline_remaining$drop_out_rate_g2_am, na.rm=T)*100

Diff_drop_out_rate_g2_am <- mean(school_baseline_original$drop_out_rate_g2_am, na.rm=T)*100-
  mean(school_baseline_remaining$drop_out_rate_g2_am, na.rm=T)*100

p_drop_out_rate_g2_am <- svyranktest(drop_out_rate_g2_am~original, school_baseline_check_r,
                                     test = c("wilcoxon"))

#dropout rate grade 2 PM shift

mean(school_baseline_original$drop_out_rate_g2_pm, na.rm=T)*100
mean(school_baseline_remaining$drop_out_rate_g2_pm, na.rm=T)*100

Diff_drop_out_rate_g2_pm <- mean(school_baseline_original$drop_out_rate_g2_pm, na.rm=T)*100-
  mean(school_baseline_remaining$drop_out_rate_g2_pm, na.rm=T)*100

p_drop_out_rate_g2_pm <- svyranktest(drop_out_rate_g2_pm~original, school_baseline_check_r,
                                     test = c("wilcoxon"))

#number of teachers

mean(school_baseline_original$Number_of_Teachers, na.rm=T)
mean(school_baseline_remaining$Number_of_Teachers, na.rm=T)

Diff_Number_of_Teachers <- mean(school_baseline_original$Number_of_Teachers, na.rm=T)-
  mean(school_baseline_remaining$Number_of_Teachers, na.rm=T)

p_Number_of_Teachers <- svyranktest(Number_of_Teachers~original, school_baseline_check_r,
                                    test = c("wilcoxon"))

#school facility: Electricity

mean(school_baseline_original$School_Facility__1, na.rm=T)*100
mean(school_baseline_remaining$School_Facility__1, na.rm=T)*100

Diff_School_Facility__1 <- 
  mean(school_baseline_original$School_Facility__1, na.rm=T)*100-
  mean(school_baseline_remaining$School_Facility__1, na.rm=T)*100

p_School_Facility__1 <- 
  svychisq(~School_Facility__1+original, school_baseline_check_r,
           statistic = c("Chisq"))

#school facility: drinking water

mean(school_baseline_original$School_Facility__2, na.rm=T)*100
mean(school_baseline_remaining$School_Facility__2, na.rm=T)*100

Diff_School_Facility__2 <- 
  mean(school_baseline_original$School_Facility__2, na.rm=T)*100-
  mean(school_baseline_remaining$School_Facility__2, na.rm=T)*100

p_School_Facility__2 <- 
  svychisq(~School_Facility__2+original, school_baseline_check_r,
           statistic = c("Chisq"))

#school facility: computer

mean(school_baseline_original$School_Facility__3, na.rm=T)*100
mean(school_baseline_remaining$School_Facility__3, na.rm=T)*100

Diff_School_Facility__3 <- 
  mean(school_baseline_original$School_Facility__3, na.rm=T)*100-
  mean(school_baseline_remaining$School_Facility__3, na.rm=T)*100

p_School_Facility__3 <- 
  svychisq(~School_Facility__3+original, school_baseline_check_r,
           statistic = c("Chisq"))

#school facility: library

mean(school_baseline_original$School_Facility__5, na.rm=T)*100
mean(school_baseline_remaining$School_Facility__5, na.rm=T)*100

Diff_School_Facility__5 <- 
  mean(school_baseline_original$School_Facility__5, na.rm=T)*100-
  mean(school_baseline_remaining$School_Facility__5, na.rm=T)*100

p_School_Facility__5 <- 
  svychisq(~School_Facility__5+original, school_baseline_check_r,
           statistic = c("Chisq"))

#school facility: laboratory

mean(school_baseline_original$School_Facility__6, na.rm=T)*100
mean(school_baseline_remaining$School_Facility__6, na.rm=T)*100

Diff_School_Facility__6 <- 
  mean(school_baseline_original$School_Facility__6, na.rm=T)*100-
  mean(school_baseline_remaining$School_Facility__6, na.rm=T)*100

p_School_Facility__6 <- 
  svychisq(~School_Facility__6+original, school_baseline_check_r,
           statistic = c("Chisq"))

#donor support

mean(school_baseline_original$Donor_Project, na.rm=T)*100
mean(school_baseline_remaining$Donor_Project, na.rm=T)*100

Diff_Donor_Project <- 
  mean(school_baseline_original$Donor_Project, na.rm=T)*100-
  mean(school_baseline_remaining$Donor_Project, na.rm=T)*100

p_Donor_Project <- 
  svychisq(~Donor_Project+original, school_baseline_check_r,
           statistic = c("Chisq"))

###Prepare dataframe to estimate the impact for teachers (Table 9, E-2, Figures 5 and 6)#####
###store the csv files in your working directory.

###read csv files 

teacher_pool <- 
  read.csv("teacher_2_pool.csv", header=T, stringsAsFactors = F)

teacher_pool$treatment_2018 <- c(teacher_pool$treatment_d*
                                   teacher_pool$timing_end)

teacher_pool$treatment_2019 <- c(teacher_pool$treatment_d*
                                   teacher_pool$timing_fus)

#coding variables for regression analysis

teacher_pool$Percentage_Time_Active_Learn_Students_min <-
  c((teacher_pool$Time_Acrtive_Learn_Students_min/teacher_pool$Duration_Lesson_min)*
      100)

teacher_pool$Percentage_Time_Active_Learn_Students_min <- 
  round(teacher_pool$Percentage_Time_Active_Learn_Students_min, digits=2)

teacher_pool <- subset(teacher_pool,
                       teacher_pool$Duration_Lesson_min>0)

teacher_pool$Teacher_Sex <- ifelse(teacher_pool$Teacher_Sex==2,1,0)

#

teacher_pool$Giving_Excercise <-
  ifelse(teacher_pool$Giving_Excercise==1,1,0)

teacher_pool$Frequency_Suggestion_Clues <-
  ifelse(teacher_pool$Frequency_Suggestion_Clues==1,1,0)

teacher_pool$Advice_Consultation_Students <-
  ifelse(teacher_pool$Advice_Consultation_Students==1,1,0)

teacher_pool$Instruction_Try_Wrong_Solutions <-
  ifelse(teacher_pool$Instruction_Try_Wrong_Solutions==1,1,0)

teacher_pool$Walk_through_Desks <-
  ifelse(teacher_pool$Walk_through_Desks==1,1,0)

teacher_pool$Giving_HW <-
  ifelse(teacher_pool$Giving_HW==1,1,0)

#balanced data of teachers in charge of grade 3 in year 2

#extract teachers who are in charge of third grade

teacher_pool_fus <- subset(teacher_pool,
                           timing_fus==1)

teacher_pool_end <- subset(teacher_pool,
                           timing_end==1)

teacher_pool_fus <- subset(teacher_pool_fus, 
                           
                           (Class_AM_G3_f__1==1|
                              Class_AM_G3_f__2==1|
                              Class_AM_G3_f__3==1|
                              Class_AM_G3_f__4==1|
                              Class_AM_G3_f__5==1|
                              Class_AM_G3_f__6==1|
                              
                              Class_PM_G3_f__1==1|
                              Class_PM_G3_f__2==1|
                              Class_PM_G3_f__3==1|
                              Class_PM_G3_f__4==1|
                              Class_PM_G3_f__5==1|
                              Class_PM_G3_f__6==1))

id_school_g3 <- cbind.data.frame(teacher_pool_fus$ID_IE)

names(id_school_g3) <- c("id_school_g3")

teacher_pool_end2 <- merge(teacher_pool_end, id_school_g3,
                           by.x=c("ID_IE"), 
                           by.y=c("id_school_g3"), all=F)

teacher_balanced_panel <- rbind.data.frame(teacher_pool_end2,
                                           teacher_pool_fus)

###Table 9#####

#percentage of student engaged time on learning math in a lesson
#column 1-1

reg_t9_1_1 <- lm_robust(Percentage_Time_Active_Learn_Students_min~
                          treatment_2018+
                          treatment_2019+
                          timing_fus+
                          urban.rural*department+
                          Teacher_Age+
                          Teacher_Sex+
                          Year_Start_teach_r+
                          Highest_Degree_highschool+
                          Highest_Degree_bachelor+
                          Teachr_Qualification__1+
                          Teachr_Qualification__3+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Total_No_Students+
                          Multigrade_G2+
                          Year_HM_ttl_e2+
                          Year_HM_this_school_e2+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_bachelor+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+
                          School_Facility__4+
                          School_Facility__5+
                          School_Facility__6+
                          School_Facility__7+
                          School_Facility__8+
                          Donor_Project, 
                        data=teacher_pool, se_type = "stata")

summary(reg_t9_1_1)

nrow(teacher_pool)

teacher_pool_c <- subset(teacher_pool,
                         treatment_2018==0&
                           timing_end==1)

mean(teacher_pool_c$Percentage_Time_Active_Learn_Students_min)

teacher_pool$Percentage_Time_Active_Learn_Students_min

linearHypothesis(reg_t9_1_1, "treatment_2018 - treatment_2019 = 0")

#9-1-2

reg_t9_1_2 <- lm_robust(Percentage_Time_Active_Learn_Students_min~
                          treatment_2018+
                          treatment_2019+
                          timing_fus+
                          urban.rural*department+
                          Teacher_Age+
                          Teacher_Sex+
                          Year_Start_teach_r+
                          Highest_Degree_highschool+
                          Highest_Degree_bachelor+
                          Teachr_Qualification__1+
                          Teachr_Qualification__3+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Total_No_Students+
                          Multigrade_G2+
                          Year_HM_ttl_e2+
                          Year_HM_this_school_e2+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_bachelor+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+
                          School_Facility__4+
                          School_Facility__5+
                          School_Facility__6+
                          School_Facility__7+
                          School_Facility__8+
                          Donor_Project, 
                        data=teacher_balanced_panel, se_type = "stata")

summary(reg_t9_1_2)

teacher_balanced_panel_c <- subset(teacher_balanced_panel,
                                   treatment_2018==0&
                                     timing_end==1)

mean(teacher_balanced_panel_c$Percentage_Time_Active_Learn_Students_min)

linearHypothesis(reg_t9_1_2, "treatment_2018 - treatment_2019 = 0")

#frequency of homework assignment in a week
#column 2-1

reg_t9_2_1 <- lm_robust(Homework_Given_G2~
                          treatment_2018+
                          treatment_2019+
                          timing_fus+
                          urban.rural*department+
                          Teacher_Age+
                          Teacher_Sex+
                          Year_Start_teach_r+
                          Highest_Degree_highschool+
                          Highest_Degree_bachelor+
                          Teachr_Qualification__1+
                          Teachr_Qualification__3+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Total_No_Students+
                          Multigrade_G2+
                          Year_HM_ttl_e2+
                          Year_HM_this_school_e2+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_bachelor+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+
                          School_Facility__4+
                          School_Facility__5+
                          School_Facility__6+
                          School_Facility__7+
                          School_Facility__8+
                          Donor_Project, 
                        data=teacher_pool, se_type = "stata")

summary(reg_t9_2_1)

teacher_pool_c <- subset(teacher_pool,
                               treatment_2018==0&
                                 timing_end==1)

nrow(teacher_pool)

teacher_pool$Homework_Given_G2
mean(teacher_pool_c$Homework_Given_G2)

linearHypothesis(reg_t9_2_1, "treatment_2018 - treatment_2019 = 0")

#column 2-2

reg_t9_2_2 <- lm_robust(Homework_Given_G2~
                          treatment_2018+
                          treatment_2019+
                          timing_fus+
                          urban.rural*department+
                          Teacher_Age+
                          Teacher_Sex+
                          Year_Start_teach_r+
                          Highest_Degree_highschool+
                          Highest_Degree_bachelor+
                          Teachr_Qualification__1+
                          Teachr_Qualification__3+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Total_No_Students+
                          Multigrade_G2+
                          Year_HM_ttl_e2+
                          Year_HM_this_school_e2+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_bachelor+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+
                          School_Facility__4+
                          School_Facility__5+
                          School_Facility__6+
                          School_Facility__7+
                          School_Facility__8+
                          Donor_Project, 
                        data=teacher_balanced_panel, se_type = "stata")

summary(reg_t9_2_2)

teacher_balanced_panel_c <- subset(teacher_balanced_panel,
                                   treatment_2018==0&
                                     timing_end==1)

teacher_balanced_panel$Homework_Given_G2
mean(teacher_balanced_panel_c$Homework_Given_G2)
nrow(teacher_balanced_panel)

linearHypothesis(reg_t9_2_2, "treatment_2018 - treatment_2019 = 0")

###Figures 5 and 6#### 
###impacts on instructional routines in a math lesson

#give exercise 

reg_t5_A <- lm_robust(Giving_Excercise~
                        treatment_2018+
                        treatment_2019+
                        timing_fus+
                        urban.rural*department+
                        Teacher_Age+
                        Teacher_Sex+
                        Year_Start_teach_r+
                        Highest_Degree_highschool+
                        Highest_Degree_bachelor+
                        Teachr_Qualification__1+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Total_No_Students+
                        Multigrade_G2+
                        Year_HM_ttl_e2+
                        Year_HM_this_school_e2+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_bachelor+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Donor_Project, 
                      data=teacher_balanced_panel, se_type = "stata")

summary(reg_t5_A)

#check notebooks

reg_t5_B <- lm_robust(How_to_check_solutn__3~
                        treatment_2018+
                        treatment_2019+
                        timing_fus+
                        urban.rural*department+
                        Teacher_Age+
                        Teacher_Sex+
                        Year_Start_teach_r+
                        Highest_Degree_highschool+
                        Highest_Degree_bachelor+
                        Teachr_Qualification__1+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Total_No_Students+
                        Multigrade_G2+
                        Year_HM_ttl_e2+
                        Year_HM_this_school_e2+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_bachelor+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Donor_Project, 
                      data=teacher_balanced_panel, se_type = "stata")

summary(reg_t5_B)

#walk in classroom to check notebook

reg_t5_C <- lm_robust(Walk_through_Desks~
                        treatment_2018+
                        treatment_2019+
                        timing_fus+
                        urban.rural*department+
                        Teacher_Age+
                        Teacher_Sex+
                        Year_Start_teach_r+
                        Highest_Degree_highschool+
                        Highest_Degree_bachelor+
                        Teachr_Qualification__1+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Total_No_Students+
                        Multigrade_G2+
                        Year_HM_ttl_e2+
                        Year_HM_this_school_e2+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_bachelor+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Donor_Project, 
                      data=teacher_balanced_panel, se_type = "stata")

summary(reg_t5_C)

#advice students to consult with each other

reg_t5_D <- lm_robust(Advice_Consultation_Students~
                        treatment_2018+
                        treatment_2019+
                        timing_fus+
                        urban.rural*department+
                        Teacher_Age+
                        Teacher_Sex+
                        Year_Start_teach_r+
                        Highest_Degree_highschool+
                        Highest_Degree_bachelor+
                        Teachr_Qualification__1+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Total_No_Students+
                        Multigrade_G2+
                        Year_HM_ttl_e2+
                        Year_HM_this_school_e2+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_bachelor+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Donor_Project, 
                      data=teacher_balanced_panel, se_type = "stata")

summary(reg_t5_D)

#tell students to check their own answers

reg_t5_E <- lm_robust(How_to_check_solutn__2~
                        treatment_2018+
                        treatment_2019+
                        timing_fus+
                        urban.rural*department+
                        Teacher_Age+
                        Teacher_Sex+
                        Year_Start_teach_r+
                        Highest_Degree_highschool+
                        Highest_Degree_bachelor+
                        Teachr_Qualification__1+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Total_No_Students+
                        Multigrade_G2+
                        Year_HM_ttl_e2+
                        Year_HM_this_school_e2+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_bachelor+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Donor_Project, 
                      data=teacher_balanced_panel, se_type = "stata")

summary(reg_t5_E)

#instruct students try again wrong answer

reg_t5_F <- lm_robust(Instruction_Try_Wrong_Solutions~
                        treatment_2018+
                        treatment_2019+
                        timing_fus+
                        urban.rural*department+
                        Teacher_Age+
                        Teacher_Sex+
                        Year_Start_teach_r+
                        Highest_Degree_highschool+
                        Highest_Degree_bachelor+
                        Teachr_Qualification__1+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Total_No_Students+
                        Multigrade_G2+
                        Year_HM_ttl_e2+
                        Year_HM_this_school_e2+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_bachelor+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Donor_Project, 
                      data=teacher_balanced_panel, se_type = "stata")

summary(reg_t5_F)

#assign homework

reg_t5_G <- lm_robust(Giving_HW~
                      treatment_2018+
                      treatment_2019+
                      timing_fus+
                      urban.rural*department+
                      Teacher_Age+
                      Teacher_Sex+
                      Year_Start_teach_r+
                      Highest_Degree_highschool+
                      Highest_Degree_bachelor+
                      Teachr_Qualification__1+
                      Teachr_Qualification__3+
                      Teachr_Qualification__6+
                      Teachr_Qualification__7+
                      Teachr_Apontmt_Post__2+
                      Teachr_Apontmt_Post__3+
                      Teachr_Apontmt_Post__4+
                      Total_No_Students+
                      Multigrade_G2+
                      Year_HM_ttl_e2+
                      Year_HM_this_school_e2+
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_bachelor+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_other+
                      School_Facility__2+
                      School_Facility__3+
                      School_Facility__4+
                      School_Facility__5+
                      School_Facility__6+
                      School_Facility__7+
                      School_Facility__8+
                      Donor_Project, 
                    data=teacher_balanced_panel, se_type = "stata")

summary(reg_t5_G)

#Draw Figure5

reg_t5_A_r <- tidy(reg_t5_A, conf.int = TRUE)
reg_t5_A_r2 <- reg_t5_A_r[2,]
reg_t5_A_r2$term <- c("(A)")

reg_t5_B_r <- tidy(reg_t5_B, conf.int = TRUE)
reg_t5_B_r2 <- reg_t5_B_r[2,]
reg_t5_B_r2$term <- c("(B)")

reg_t5_C_r <- tidy(reg_t5_C, conf.int = TRUE)
reg_t5_C_r2 <- reg_t5_C_r[2,]
reg_t5_C_r2$term <- c("(C)")

reg_t5_D_r <- tidy(reg_t5_D, conf.int = TRUE)
reg_t5_D_r2 <- reg_t5_D_r[2,]
reg_t5_D_r2$term <- c("(D)")

reg_t5_E_r <- tidy(reg_t5_E, conf.int = TRUE)
reg_t5_E_r2 <- reg_t5_E_r[2,]
reg_t5_E_r2$term <- c("(E)")

reg_t5_F_r <- tidy(reg_t5_F, conf.int = TRUE)
reg_t5_F_r2 <- reg_t5_F_r[2,]
reg_t5_F_r2$term <- c("(F)")

reg_t5_G_r <- tidy(reg_t5_G, conf.int = TRUE)
reg_t5_G_r2 <- reg_t5_G_r[2,]
reg_t5_G_r2$term <- c("(G)")

#
reg_t_f_5 <- rbind(reg_t5_A_r2,
                   reg_t5_B_r2,
                   reg_t5_C_r2,
                   reg_t5_D_r2,
                   reg_t5_E_r2,
                   reg_t5_F_r2,
                   reg_t5_G_r2)

#

g <- ggcoef(reg_t_f_5, conf.level=0.95)
g <- g + scale_x_continuous(breaks=seq(-0.4,0.4,by=0.1),limits=c(-0.4,0.45))
g <- g + ylab("") 
g <- g + coord_flip() 
plot(g)

ggsave(file = "estimate_instruction_e_check.png", dpi = 1000, 
       width = 8, height = 4, plot = g)

#Draw Figure6

reg_t5_A_r3 <- reg_t5_A_r[3,]
reg_t5_A_r3$term <- c("(A)")

reg_t5_B_r3 <- reg_t5_B_r[3,]
reg_t5_B_r3$term <- c("(B)")

reg_t5_C_r3 <- reg_t5_C_r[3,]
reg_t5_C_r3$term <- c("(C)")

reg_t5_D_r3 <- reg_t5_D_r[3,]
reg_t5_D_r3$term <- c("(D)")

reg_t5_E_r3 <- reg_t5_E_r[3,]
reg_t5_E_r3$term <- c("(E)")

reg_t5_F_r3 <- reg_t5_F_r[3,]
reg_t5_F_r3$term <- c("(F)")

reg_t5_G_r3 <- reg_t5_G_r[3,]
reg_t5_G_r3$term <- c("(G)")

#

reg_t_f_6 <- rbind(reg_t5_A_r3,
                   reg_t5_B_r3,
                   reg_t5_C_r3,
                   reg_t5_D_r3,
                   reg_t5_E_r3,
                   reg_t5_F_r3,
                   reg_t5_G_r3)

###

g <- ggcoef(reg_t_f_6, conf.level=0.95)
g <- g + scale_x_continuous(breaks=seq(-0.4,0.4,by=0.1),limits=c(-0.4,0.45))
g <- g + ylab("") 
g <- g + coord_flip() 
plot(g)

ggsave(file = "estimate_instruction_f_check.png", dpi = 1000, 
       width = 8, height = 4, plot = g)

###Table E-2 in Appendix 6####

#Frequency of lesson observation of school principal
#column 1-1

reg_t_e_2_1_1 <- lm_robust(HM_Observe~
                             treatment_2018+
                             treatment_2019+
                             timing_fus+
                             urban.rural*department+
                             Teacher_Age+
                             Teacher_Sex+
                             Year_Start_teach_r+
                             Highest_Degree_highschool+
                             Highest_Degree_bachelor+
                             Teachr_Qualification__1+
                             Teachr_Qualification__3+
                             Teachr_Qualification__6+
                             Teachr_Qualification__7+
                             Teachr_Apontmt_Post__2+
                             Teachr_Apontmt_Post__3+
                             Teachr_Apontmt_Post__4+
                             Total_No_Students+
                             Multigrade_G2+
                             Year_HM_ttl_e2+
                             Year_HM_this_school_e2+
                             HM_Highest_Degree_highschool+
                             HM_Highest_Degree_bachelor+
                             HM_Highest_Degree_master+
                             HM_Highest_Degree_other+
                             School_Facility__2+
                             School_Facility__3+
                             School_Facility__4+
                             School_Facility__5+
                             School_Facility__6+
                             School_Facility__7+
                             School_Facility__8+
                             Donor_Project, 
                           data=teacher_pool, se_type = "stata")

summary(reg_t_e_2_1_1)

teacher_pool_end_c <- subset(teacher_pool,
                             treatment_2018==0&
                               timing_end==1)

mean(teacher_pool_end_c$HM_Observe)

linearHypothesis(reg_t_e_2_1_1, "treatment_2018 - treatment_2019 = 0")

#column 1-2

reg_t_e_2_1_2 <- lm_robust(HM_Observe~
                             treatment_2018+
                             treatment_2019+
                             timing_fus+
                             urban.rural*department+
                             Teacher_Age+
                             Teacher_Sex+
                             Year_Start_teach_r+
                             Highest_Degree_highschool+
                             Highest_Degree_bachelor+
                             Teachr_Qualification__1+
                             Teachr_Qualification__3+
                             Teachr_Qualification__6+
                             Teachr_Qualification__7+
                             Teachr_Apontmt_Post__2+
                             Teachr_Apontmt_Post__3+
                             Teachr_Apontmt_Post__4+
                             Total_No_Students+
                             Multigrade_G2+
                             Year_HM_ttl_e2+
                             Year_HM_this_school_e2+
                             HM_Highest_Degree_highschool+
                             HM_Highest_Degree_bachelor+
                             HM_Highest_Degree_master+
                             HM_Highest_Degree_other+
                             School_Facility__2+
                             School_Facility__3+
                             School_Facility__4+
                             School_Facility__5+
                             School_Facility__6+
                             School_Facility__7+
                             School_Facility__8+
                             Donor_Project, 
                           data=teacher_balanced_panel, se_type = "stata")

summary(reg_t_e_2_1_2)

teacher_balanced_panel_end_c <- subset(teacher_balanced_panel,
                                       treatment_2018==0&
                                         timing_end==1)

mean(teacher_balanced_panel_end_c$HM_Observe)

linearHypothesis(reg_t_e_2_1_2, "treatment_2018 - treatment_2019 = 0")

#Frequency of suggestions in lesson observation of school principal
#column 2-1

reg_t_e_2_2_1 <- lm_robust(HM_Suggestion~
                             treatment_2018+
                             treatment_2019+
                             timing_fus+
                             urban.rural*department+
                             Teacher_Age+
                             Teacher_Sex+
                             Year_Start_teach_r+
                             Highest_Degree_highschool+
                             Highest_Degree_bachelor+
                             Teachr_Qualification__1+
                             Teachr_Qualification__3+
                             Teachr_Qualification__6+
                             Teachr_Qualification__7+
                             Teachr_Apontmt_Post__2+
                             Teachr_Apontmt_Post__3+
                             Teachr_Apontmt_Post__4+
                             Total_No_Students+
                             Multigrade_G2+
                             Year_HM_ttl_e2+
                             Year_HM_this_school_e2+
                             HM_Highest_Degree_highschool+
                             HM_Highest_Degree_bachelor+
                             HM_Highest_Degree_master+
                             HM_Highest_Degree_other+
                             School_Facility__2+
                             School_Facility__3+
                             School_Facility__4+
                             School_Facility__5+
                             School_Facility__6+
                             School_Facility__7+
                             School_Facility__8+
                             Donor_Project, 
                           data=teacher_pool, se_type = "stata")

summary(reg_t_e_2_2_1)

teacher_pool_end_c <- subset(teacher_pool,
                             treatment_2018==0&
                               timing_end==1)

mean(teacher_pool_end_c$HM_Suggestion)

linearHypothesis(reg_t_e_2_2_1, "treatment_2018 - treatment_2019 = 0")

#column 2-2

reg_t_e_2_2_2 <- lm_robust(HM_Suggestion~
                             treatment_2018+
                             treatment_2019+
                             timing_fus+
                             urban.rural*department+
                             Teacher_Age+
                             Teacher_Sex+
                             Year_Start_teach_r+
                             Highest_Degree_highschool+
                             Highest_Degree_bachelor+
                             Teachr_Qualification__1+
                             Teachr_Qualification__3+
                             Teachr_Qualification__6+
                             Teachr_Qualification__7+
                             Teachr_Apontmt_Post__2+
                             Teachr_Apontmt_Post__3+
                             Teachr_Apontmt_Post__4+
                             Total_No_Students+
                             Multigrade_G2+
                             Year_HM_ttl_e2+
                             Year_HM_this_school_e2+
                             HM_Highest_Degree_highschool+
                             HM_Highest_Degree_bachelor+
                             HM_Highest_Degree_master+
                             HM_Highest_Degree_other+
                             School_Facility__2+
                             School_Facility__3+
                             School_Facility__4+
                             School_Facility__5+
                             School_Facility__6+
                             School_Facility__7+
                             School_Facility__8+
                             Donor_Project, 
                           data=teacher_balanced_panel, se_type = "stata")

summary(reg_t_e_2_2_2)

teacher_balanced_panel_end_c <- subset(teacher_balanced_panel,
                                       treatment_2018==0&
                                         timing_end==1)

mean(teacher_balanced_panel_end_c$HM_Suggestion)

nrow(teacher_balanced_panel)

linearHypothesis(reg_t_e_2_2_2, "treatment_2018 - treatment_2019 = 0")