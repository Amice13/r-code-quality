
library(MatchIt)

feb_big_seker_1 <- matchit(sms_feb ~     child_male++
                             mother_academic+father_academic+parents_num_children+parents_arab+parents_hardi+gill_child+mother_wage+father_wage+parents_married,
                           data=cda_seker_new_match_feb_big, method= "nearest", ratio=1, replace= TRUE)

summary(feb_big_seker_1)


save(feb_big_seker_1,file="feb_big_seker_1_match")
feb_big_seker_data_1 <- match.data(feb_big_seker_1,weights="weights")
save(feb_big_seker_data_1,file="feb_big_seker_data_1")



###########################
load("cda_seker_new_match_feb_big")
a_feb_big_seker_data=subset(cda_seker_new_match_feb_big,cda_seker_new_match_feb_big$parents_arab==1)
a_feb_big_seker_1 <- matchit(sms_feb ~
                                    child_male+
                                    mother_academic+father_academic+parents_num_children+gill_child+mother_wage+father_wage+parents_married,
                                  data=a_feb_big_seker_data, method= "nearest", ratio=1, replace= TRUE)


save(a_feb_big_seker_1,file="a_feb_big_seker_1_match")
a_feb_big_seker_1_data <- match.data(a_feb_big_seker_1,weights="weights")
save(a_feb_big_seker_1_data,file="a_feb_big_seker_1_data")

# 
# load("a_feb_big_seker_1_match")
# plot(a_feb_big_seker_1,type="hist", standardize = TRUE)
# v<- summary(a_feb_big_seker_1, standardize = TRUE)
# plot (v, var.orger="unmatched")
# summary(a_feb_big_seker_1)

load("cda_seker_new_match_feb_big")
j_feb_big_seker_data=subset(cda_seker_new_match_feb_big,cda_seker_new_match_feb_big$parents_arab==0 & cda_seker_new_match_feb_big$parents_hardi==0)
j_feb_big_seker_1 <- matchit(sms_feb ~
                               child_male++
                               mother_academic+father_academic+parents_num_children+gill_child+mother_wage+father_wage+parents_married,
                             data=j_feb_big_seker_data, method= "nearest", ratio=1, replace= TRUE)


save(j_feb_big_seker_1,file="j_feb_big_seker_1_match")
j_feb_big_seker_1_data <- match.data(j_feb_big_seker_1,weights="weights")
save(j_feb_big_seker_1_data,file="j_feb_big_seker_1_data")


# load("j_feb_big_seker_1_match")
# plot(j_feb_big_seker_1,type="hist", standardize = TRUE)
# v<- summary(j_feb_big_seker_1, standardize = TRUE)
# plot (v, var.orger="unmatched")
# summary(j_feb_big_seker_1)


load("cda_seker_new_match_feb_big")
h_feb_big_seker_data=subset(cda_seker_new_match_feb_big,cda_seker_new_match_feb_big$parents_hardi==1)
h_feb_big_seker_1 <- matchit(sms_feb ~
                               child_male++
                               mother_academic+father_academic+parents_num_children+gill_child+mother_wage+father_wage+parents_married,
                             data=h_feb_big_seker_data, method= "nearest", ratio=1, replace= TRUE)


save(h_feb_big_seker_1,file="h_feb_big_seker_1_match")
h_feb_big_seker_1_data <- match.data(h_feb_big_seker_1,weights="weights")
save(h_feb_big_seker_1_data,file="h_feb_big_seker_1_data")


# load("h_feb_big_seker_1_match")
# plot(h_feb_big_seker_1,type="hist", standardize = TRUE)
# v<- summary(h_feb_big_seker_1, standardize = TRUE)
# plot (v, var.orger="unmatched")
# summary(h_feb_big_seker_1)


