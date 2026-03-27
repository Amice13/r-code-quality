#Jenny Gao & Aticus Peterson
#replication of carnes, lupa "what good is a college degree"
#key statistics from table 2

#because the data is in stata 2013 format, we need a special package called readstata13
library(readstata13)
library(gdata)
congress = read.dta13("leader/US Congress.dta")

#subset data to only include rows where first congress = 1
first = which(congress$first_congress == 1)
model.one = congress[first,]

#subset data to only include rows of those elected from 1901 on
v.46 = which(model.one$V46 >= 901)
model.one = model.one[v.46,]

#subset data so year left congress is >=0
v.47 = which(model.one$V47 >= 0)
model.one = model.one[v.47,]

#get rid of NA values in close
close = which(model.one$close >= 0)
model.one = model.one[close,]

#eliminate entries where there is no state information
no_state = which(is.na(model.one$state_1)==FALSE)
model.one = model.one[no_state,]

model.one[2417,5] = 932

#create our categorical variables
v46.f = factor(model.one$V46)
v47.f = factor(model.one$V47)

###
#Table 2, Column 1

bills_one = lm(model.one$bills_enacted_AVG ~ model.one$close + v47.f + v46.f + model.one$chamber + model.one$state_1 + model.one$state_2 +model.one$state_3+model.one$state_4+model.one$state_5+model.one$state_6+model.one$state_7+model.one$state_8+model.one$state_9+model.one$state_10+model.one$state_11+model.one$state_12+model.one$state_13+model.one$state_14+model.one$state_15+model.one$state_16+model.one$state_17+model.one$state_18+model.one$state_19+model.one$state_20+model.one$state_21+model.one$state_22+model.one$state_23+model.one$state_24+model.one$state_25+model.one$state_26+model.one$state_27+model.one$state_28+model.one$state_29+model.one$state_30+model.one$state_31+model.one$state_32+model.one$state_33+model.one$state_34+model.one$state_35+model.one$state_36+model.one$state_37+model.one$state_38+model.one$state_39+model.one$state_40+model.one$state_41+model.one$state_42+model.one$state_43+model.one$state_44+model.one$state_45+model.one$state_46+model.one$state_47+model.one$state_48+model.one$state_49+model.one$state_50)
summary(bills_one)

###
#Table 2, Column 2

bills_two = lm(model.one$bills_enacted_AVG ~ model.one$close_college + model.one$close_nocollege + v47.f + v46.f + model.one$chamber + model.one$state_1 + model.one$state_2 +model.one$state_3+model.one$state_4+model.one$state_5+model.one$state_6+model.one$state_7+model.one$state_8+model.one$state_9+model.one$state_10+model.one$state_11+model.one$state_12+model.one$state_13+model.one$state_14+model.one$state_15+model.one$state_16+model.one$state_17+model.one$state_18+model.one$state_19+model.one$state_20+model.one$state_21+model.one$state_22+model.one$state_23+model.one$state_24+model.one$state_25+model.one$state_26+model.one$state_27+model.one$state_28+model.one$state_29+model.one$state_30+model.one$state_31+model.one$state_32+model.one$state_33+model.one$state_34+model.one$state_35+model.one$state_36+model.one$state_37+model.one$state_38+model.one$state_39+model.one$state_40+model.one$state_41+model.one$state_42+model.one$state_43+model.one$state_44+model.one$state_45+model.one$state_46+model.one$state_47+model.one$state_48+model.one$state_49+model.one$state_50)
summary(bills_two)

###
#Table 2, Column 3

years_one = lm(model.one$V48 ~ model.one$close + v47.f + v46.f + model.one$chamber + model.one$state_1 + model.one$state_2 +model.one$state_3+model.one$state_4+model.one$state_5+model.one$state_6+model.one$state_7+model.one$state_8+model.one$state_9+model.one$state_10+model.one$state_11+model.one$state_12+model.one$state_13+model.one$state_14+model.one$state_15+model.one$state_16+model.one$state_17+model.one$state_18+model.one$state_19+model.one$state_20+model.one$state_21+model.one$state_22+model.one$state_23+model.one$state_24+model.one$state_25+model.one$state_26+model.one$state_27+model.one$state_28+model.one$state_29+model.one$state_30+model.one$state_31+model.one$state_32+model.one$state_33+model.one$state_34+model.one$state_35+model.one$state_36+model.one$state_37+model.one$state_38+model.one$state_39+model.one$state_40+model.one$state_41+model.one$state_42+model.one$state_43+model.one$state_44+model.one$state_45+model.one$state_46+model.one$state_47+model.one$state_48+model.one$state_49+model.one$state_50)
summary(years_one)

###
#Table 2, Column 4

years_two = lm(model.one$V48 ~ model.one$close_college + model.one$close_nocollege + v47.f + v46.f + model.one$chamber + model.one$state_1 + model.one$state_2 +model.one$state_3+model.one$state_4+model.one$state_5+model.one$state_6+model.one$state_7+model.one$state_8+model.one$state_9+model.one$state_10+model.one$state_11+model.one$state_12+model.one$state_13+model.one$state_14+model.one$state_15+model.one$state_16+model.one$state_17+model.one$state_18+model.one$state_19+model.one$state_20+model.one$state_21+model.one$state_22+model.one$state_23+model.one$state_24+model.one$state_25+model.one$state_26+model.one$state_27+model.one$state_28+model.one$state_29+model.one$state_30+model.one$state_31+model.one$state_32+model.one$state_33+model.one$state_34+model.one$state_35+model.one$state_36+model.one$state_37+model.one$state_38+model.one$state_39+model.one$state_40+model.one$state_41+model.one$state_42+model.one$state_43+model.one$state_44+model.one$state_45+model.one$state_46+model.one$state_47+model.one$state_48+model.one$state_49+model.one$state_50)
summary(years_two)

###
#Table 2, Column 5

lost_one = lm(model.one$lostelection ~ model.one$close + v47.f + v46.f + model.one$chamber + model.one$state_1 + model.one$state_2 +model.one$state_3+model.one$state_4+model.one$state_5+model.one$state_6+model.one$state_7+model.one$state_8+model.one$state_9+model.one$state_10+model.one$state_11+model.one$state_12+model.one$state_13+model.one$state_14+model.one$state_15+model.one$state_16+model.one$state_17+model.one$state_18+model.one$state_19+model.one$state_20+model.one$state_21+model.one$state_22+model.one$state_23+model.one$state_24+model.one$state_25+model.one$state_26+model.one$state_27+model.one$state_28+model.one$state_29+model.one$state_30+model.one$state_31+model.one$state_32+model.one$state_33+model.one$state_34+model.one$state_35+model.one$state_36+model.one$state_37+model.one$state_38+model.one$state_39+model.one$state_40+model.one$state_41+model.one$state_42+model.one$state_43+model.one$state_44+model.one$state_45+model.one$state_46+model.one$state_47+model.one$state_48+model.one$state_49+model.one$state_50)
summary(lost_one)

###
#Table 2, Column 6

lost_two = lm(model.one$lostelection ~ model.one$close_college + model.one$close_nocollege + v47.f + v46.f + model.one$chamber + model.one$state_1 + model.one$state_2 +model.one$state_3+model.one$state_4+model.one$state_5+model.one$state_6+model.one$state_7+model.one$state_8+model.one$state_9+model.one$state_10+model.one$state_11+model.one$state_12+model.one$state_13+model.one$state_14+model.one$state_15+model.one$state_16+model.one$state_17+model.one$state_18+model.one$state_19+model.one$state_20+model.one$state_21+model.one$state_22+model.one$state_23+model.one$state_24+model.one$state_25+model.one$state_26+model.one$state_27+model.one$state_28+model.one$state_29+model.one$state_30+model.one$state_31+model.one$state_32+model.one$state_33+model.one$state_34+model.one$state_35+model.one$state_36+model.one$state_37+model.one$state_38+model.one$state_39+model.one$state_40+model.one$state_41+model.one$state_42+model.one$state_43+model.one$state_44+model.one$state_45+model.one$state_46+model.one$state_47+model.one$state_48+model.one$state_49+model.one$state_50)
summary(lost_two)
