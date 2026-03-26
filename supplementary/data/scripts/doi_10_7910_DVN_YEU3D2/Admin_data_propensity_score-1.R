###how to load files at the NII
# pkgFilenames <- read.csv("pkgFilenames.csv", stringsAsFactors = FALSE)[,1]
# install.packages(pkgFilenames, repos = NULL, type = "win.binary")
##names of linraries to install from:
#pac_r/pac_files
##tomeR_r

(gc)
setwd("w:/02192835/incoming/maya_hisachon_chiild")

library(MASS)
library(Matching)
library(MatchIt)
library(stargazer)
library(lubridate)
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(pastecs)
library(Hmisc)
library(MASS)
library(plyr)
library(foreign)
library(rio)
library(data.table)
library(Matching)
library(MatchIt)
library(rmarkdown)
library(dplyr)
library(textclean)
library(substr)
library(lubridate)
library(MatchIt)

load("total_match_feb_only_sms")
##only SMS

feb_nearest_main_t <- matchit(sms_feb ~
                            child_male++
                            mother_academic+father_academic+parents_num_children+parents_arab+parents_hardi+gill_child+mother_wage+father_wage+parents_married,
                          data=total_match_feb_only_sms,  method= "nearest", ratio=1, replace= TRUE)


save(feb_nearest_main_t,file="feb_nearest_main_t_match")
feb_nearest_main_t_data <- match.data(feb_nearest_main_t,weights="weights")
save(feb_nearest_main_t_data,file="feb_nearest_main_t_data")
summary(feb_nearest_main_t)

# load("feb_nearest_main_t_match")
# plot (feb_nearest_main_t, type="jitter")
# plot (feb_nearest_main_t, type="hist")


######robust specifications:


feb_nearest_noreplace_t <- matchit(sms_feb ~
                                child_male++
                                mother_academic+father_academic+parents_num_children+parents_arab+parents_hardi+gill_child+mother_wage+father_wage+parents_married,
                              data=total_match_feb_only_sms,  method= "nearest", ratio=1, replace= FALSE)


save(feb_nearest_noreplace_t,file="feb_nearest_noreplace_t_match")
feb_nearest_noreplace_t_data <- match.data(feb_nearest_noreplace_t,weights="weights")
save(feb_nearest_noreplace_t_data,file="feb_nearest_noreplace_t_data")
summary(feb_nearest_noreplace_t)

# load("feb_nearest_noreplace_t_match")
# plot (feb_nearest_noreplace_t, type="jitter")
# plot (feb_nearest_noreplace_t, type="hist")

feb_nearest_caliper_t <- matchit(sms_feb ~
                                     child_male++
                                     mother_academic+father_academic+parents_num_children+parents_arab+parents_hardi+gill_child+mother_wage+father_wage+parents_married,
                                   data=total_match_feb_only_sms,  method= "nearest", ratio=1, replace= TRUE, caliper=0.2)


save(feb_nearest_caliper_t,file="feb_nearest_caliper_t_match")
feb_nearest_caliper_t_data <- match.data(feb_nearest_caliper_t,weights="weights")
save(feb_nearest_caliper_t_data,file="feb_nearest_caliper_t_data")
summary(feb_nearest_caliper_t)

# load("feb_nearest_caliper_t_match")
# plot (feb_nearest_caliper_t, type="jitter")
# plot (feb_nearest_caliper_t, type="hist")



feb_nearest_ratio_t <- matchit(sms_feb ~
                                   child_male++
                                   mother_academic+father_academic+parents_num_children+parents_arab+parents_hardi+gill_child+mother_wage+father_wage+parents_married,
                                 data=total_match_feb_only_sms,  method= "nearest", ratio=1:3, replace= TRUE)


save(feb_nearest_ratio_t,file="feb_nearest_ratio_t_match")
feb_nearest_ratio_t_data <- match.data(feb_nearest_ratio_t,weights="weights")
save(feb_nearest_ratio_t_data,file="feb_nearest_ratio_t_data")
summary(feb_nearest_ratio_t)

# load("feb_nearest_ratio_t_match")
# plot (feb_nearest_ratio_t, type="jitter")
# plot (feb_nearest_ratio_t, type="hist")
#############

a_total_feb_main=total_match_feb_only_sms[parents_arab==1,]
a_total_match_feb_main <- matchit(sms_feb ~
                                    child_male++
                                    mother_academic+father_academic+parents_num_children+gill_child+mother_wage+father_wage+parents_married,
                               data=a_total_feb_main, method= "nearest", ratio=1, replace= TRUE)


save(a_total_match_feb_main,file="a_total_match_feb_main_match")
a_total_match_feb_main_data <- match.data(a_total_match_feb_main,weights="weights")
save(a_total_match_feb_main_data,file="a_total_match_feb_main_data")

a_total_match_feb_noreplace <- matchit(sms_feb ~
                                    child_male++
                                    mother_academic+father_academic+parents_num_children+gill_child+mother_wage+father_wage+parents_married,
                                  data=a_total_feb_main, method= "nearest", ratio=1, replace= FALSE)


save(a_total_match_feb_noreplace,file="a_total_match_feb_noreplace_match")
a_total_match_feb_noreplace_data <- match.data(a_total_match_feb_noreplace,weights="weights")
save(a_total_match_feb_noreplace_data,file="a_total_match_feb_noreplace_data")

a_total_match_feb_caliper <- matchit(sms_feb ~
                                   child_male++
                                   mother_academic+father_academic+parents_num_children+parents_arab+parents_hardi+gill_child+mother_wage+father_wage+parents_married,
                                 data=a_total_feb_main,  method= "nearest", ratio=1, replace= TRUE, caliper=0.2)


save(a_total_match_feb_caliper,file="a_total_match_feb_caliper_match")
a_total_match_feb_caliper_data <- match.data(a_total_match_feb_caliper,weights="weights")
save(a_total_match_feb_caliper_data,file="a_total_match_feb_caliper_data")
summary(a_total_match_feb_caliper)


a_total_match_feb_ratio <- matchit(sms_feb ~
                                 child_male++
                                 mother_academic+father_academic+parents_num_children+parents_arab+parents_hardi+gill_child+mother_wage+father_wage+parents_married,
                               data=a_total_feb_main,  method= "nearest", ratio=1:3, replace= TRUE)


save(a_total_match_feb_ratio,file="a_total_match_feb_ratio_match")
a_total_match_feb_ratio_data <- match.data(a_total_match_feb_ratio,weights="weights")
save(a_total_match_feb_ratio_data,file="a_total_match_feb_ratio_data")
summary(a_total_match_feb_ratio)


####################
h_total_feb_main=total_match_feb_only_sms[parents_hardi==1,]

h_total_match_feb_main <- matchit(sms_feb ~
                                    child_male+
                                    mother_academic+father_academic+parents_num_children+gill_child+mother_wage+father_wage+parents_married,
                                  data=h_total_feb_main, method= "nearest", ratio=1, replace= TRUE)


save(h_total_match_feb_main,file="h_total_match_feb_main_match")
h_total_match_feb_main_data <- match.data(h_total_match_feb_main,weights="weights")
save(h_total_match_feb_main_data,file="h_total_match_feb_main_data")

h_total_match_feb_noreplace <- matchit(sms_feb ~
                                         child_male++
                                         mother_academic+father_academic+parents_num_children+gill_child+mother_wage+father_wage+parents_married,
                                       data=h_total_feb_main, method= "nearest", ratio=1, replace= FALSE)


save(h_total_match_feb_noreplace,file="h_total_match_feb_noreplace_match")
h_total_match_feb_noreplace_data <- match.data(h_total_match_feb_noreplace,weights="weights")
save(h_total_match_feb_noreplace_data,file="h_total_match_feb_noreplace_data")

h_total_match_feb_caliper <- matchit(sms_feb ~
                                       child_male++
                                       mother_academic+father_academic+parents_num_children+parents_arab+parents_hardi+gill_child+mother_wage+father_wage+parents_married,
                                     data=h_total_feb_main,  method= "nearest", ratio=1, replace= TRUE, caliper=0.2)


save(h_total_match_feb_caliper,file="h_total_match_feb_caliper_match")
h_total_match_feb_caliper_data <- match.data(h_total_match_feb_caliper,weights="weights")
save(h_total_match_feb_caliper_data,file="h_total_match_feb_caliper_data")
summary(h_total_match_feb_caliper)


h_total_match_feb_ratio <- matchit(sms_feb ~
                                     child_male++
                                     mother_academic+father_academic+parents_num_children+parents_arab+parents_hardi+gill_child+mother_wage+father_wage+parents_married,
                                   data=h_total_feb_main,  method= "nearest", ratio=1:3, replace= TRUE)


save(h_total_match_feb_ratio,file="h_total_match_feb_ratio_match")
h_total_match_feb_ratio_data <- match.data(h_total_match_feb_ratio,weights="weights")
save(h_total_match_feb_ratio_data,file="h_total_match_feb_ratio_data")
summary(h_total_match_feb_ratio)


j_total_feb_main=total_match_feb_only_sms[parents_arab==0 & parents_hardi==0,]
j_total_match_feb_main <- matchit(sms_feb ~
                                    child_male++
                                    mother_academic+father_academic+parents_num_children+gill_child+mother_wage+father_wage+parents_married,
                                  data=j_total_feb_main, method= "nearest", ratio=1, replace= TRUE)


save(j_total_match_feb_main,file="j_total_match_feb_main_match")
j_total_match_feb_main_data <- match.data(j_total_match_feb_main,weights="weights")
save(j_total_match_feb_main_data,file="j_total_match_feb_main_data")

j_total_match_feb_noreplace <- matchit(sms_feb ~
                                         child_male++
                                         mother_academic+father_academic+parents_num_children+gill_child+mother_wage+father_wage+parents_married,
                                       data=j_total_feb_main, method= "nearest", ratio=1, replace= FALSE)


save(j_total_match_feb_noreplace,file="j_total_match_feb_noreplace_match")
j_total_match_feb_noreplace_data <- match.data(j_total_match_feb_noreplace,weights="weights")
save(j_total_match_feb_noreplace_data,file="j_total_match_feb_noreplace_data")

j_total_match_feb_caliper <- matchit(sms_feb ~
                                       child_male++
                                       mother_academic+father_academic+parents_num_children+parents_arab+parents_hardi+gill_child+mother_wage+father_wage+parents_married,
                                     data=j_total_feb_main,  method= "nearest", ratio=1, replace= TRUE, caliper=0.2)


save(j_total_match_feb_caliper,file="j_total_match_feb_caliper_match")
j_total_match_feb_caliper_data <- match.data(j_total_match_feb_caliper,weights="weights")
save(j_total_match_feb_caliper_data,file="j_total_match_feb_caliper_data")
summary(j_total_match_feb_caliper)


j_total_match_feb_ratio <- matchit(sms_feb ~
                                     child_male++
                                     mother_academic+father_academic+parents_num_children+parents_arab+parents_hardi+gill_child+mother_wage+father_wage+parents_married,
                                   data=j_total_feb_main,  method= "nearest", ratio=1:3, replace= TRUE)


save(j_total_match_feb_ratio,file="j_total_match_feb_ratio_match")
j_total_match_feb_ratio_data <- match.data(j_total_match_feb_ratio,weights="weights")
save(j_total_match_feb_ratio_data,file="j_total_match_feb_ratio_data")
summary(j_total_match_feb_ratio)

##########################################Lamas match
####socio_econ_cluster+periphery_cluster

feb_nearest_lamas_t <- matchit(sms_feb ~
                                rural+socio_econ_cluster+periphery_cluster+
                                child_male+
                                mother_academic+father_academic+parents_num_children+parents_arab+parents_hardi+gill_child+mother_wage+father_wage+parents_married,
                              data=total_match_feb_only_sms,  method= "nearest", ratio=1, replace= TRUE)


save(feb_nearest_lamas_t,file="feb_nearest_lamas_t_match")
feb_nearest_lamas_t_data <- match.data(feb_nearest_lamas_t,weights="weights")
save(feb_nearest_lamas_t_data,file="feb_nearest_lamas_t_data")
summary(feb_nearest_lamas_t)

a_total_match_feb_lamas <- matchit(sms_feb ~
                                     rural+socio_econ_cluster+periphery_cluster+
                                    child_male++
                                    mother_academic+father_academic+parents_num_children+gill_child+mother_wage+father_wage+parents_married,
                                  data=a_total_feb_main, method= "nearest", ratio=1, replace= TRUE)


save(a_total_match_feb_lamas,file="a_total_match_feb_lamas_match")
a_total_match_feb_lamas_data <- match.data(a_total_match_feb_lamas,weights="weights")
save(a_total_match_feb_lamas_data,file="a_total_match_feb_lamas_data")

h_total_match_feb_lamas <- matchit(sms_feb ~
                                     rural+socio_econ_cluster+periphery_cluster+
                                    child_male++
                                    mother_academic+father_academic+parents_num_children+gill_child+mother_wage+father_wage+parents_married,
                                  data=h_total_feb_main, method= "nearest", ratio=1, replace= TRUE)


save(h_total_match_feb_lamas,file="h_total_match_feb_lamas_match")
h_total_match_feb_lamas_data <- match.data(h_total_match_feb_lamas,weights="weights")
save(h_total_match_feb_lamas_data,file="h_total_match_feb_lamas_data")

j_total_match_feb_lamas <- matchit(sms_feb ~
                                     rural+socio_econ_cluster+periphery_cluster+
                                    child_male++
                                    mother_academic+father_academic+parents_num_children+gill_child+mother_wage+father_wage+parents_married,
                                  data=j_total_feb_main, method= "nearest", ratio=1, replace= TRUE)


save(j_total_match_feb_lamas,file="j_total_match_feb_lamas_match")
j_total_match_feb_lamas_data <- match.data(j_total_match_feb_lamas,weights="weights")
save(j_total_match_feb_lamas_data,file="j_total_match_feb_lamas_data")

#########################################
