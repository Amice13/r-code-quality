###########################
### REGRESSION ANALYSIS ###
###########################

# Fractional binary models
# Suppress warning messages that occur because this is a fractional logit model
options(warn = -1)

qbin.sh.1 <- glm(sharedness ~ ministries_total * ministries_diff + attention_total,
                 family = binomial(),
                 data = Data_Party_Pos)
qbin.sh.1.cntry <- update(qbin.sh.1, .~. + country)
qbin.sh.1.gov <- update(qbin.sh.1, .~. + country)
qbin.sh.1.cab <- update(qbin.sh.1, .~. + cabinet)
qbin.sh.1.cntry_mjr <- update(qbin.sh.1, .~. + country + major)

qbin.sh.2 <- update(qbin.sh.1, . ~ . + position_diff)
qbin.sh.2.cntry <- update(qbin.sh.2, .~. + country)
qbin.sh.2.cab <- update(qbin.sh.2, .~. + cabinet)
qbin.sh.2.cntry_mjr <- update(qbin.sh.2, .~. + country + major)

qbin.sh.3 <- update(qbin.sh.1, . ~ . + position_diff * attention_total)
qbin.sh.3.cntry <- update(qbin.sh.3, .~. + country)
qbin.sh.3.cab <- update(qbin.sh.3, .~. + cabinet)
qbin.sh.3.cntry_mjr <- update(qbin.sh.3, .~. + country + major)

options(warn = defaultW)


# Beta models
beta.sh.1 <- betareg(sharedness_infl ~ ministries_total * ministries_diff + attention_total,
                     data = Data_Party_Pos)
beta.sh.1.cntry <- update(beta.sh.1, .~. + country)
beta.sh.1.cntry_mjr <- update(beta.sh.1, .~. + country + major)

beta.sh.2 <- update(beta.sh.1, . ~ . + position_diff)
beta.sh.2.cntry <- update(beta.sh.2, .~. + country)
beta.sh.2.cntry_mjr <- update(beta.sh.2, .~. + country + major)

beta.sh.3 <- update(beta.sh.1, . ~ . + position_diff * attention_total)
beta.sh.3.cntry <- update(beta.sh.3, .~. + country)
beta.sh.3.cntry_mjr <- update(beta.sh.3, .~. + country + major)
