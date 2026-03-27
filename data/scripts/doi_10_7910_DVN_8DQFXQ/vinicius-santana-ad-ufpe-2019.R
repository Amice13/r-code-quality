#############################################################################################
#############################################################################################
############################ DATA ANALYSIS ##################################################
############################ FINAL ACTIVITY #################################################
#############################################################################################
########################  VINICIUS SILVA SANTANA ############################################
#############################################################################################
#############################################################################################



# COMMENT: REMEMBER TO ADD HEADS AND CLEAR NAMES TO VARIABLES IN PLOTS

############################################################################################
############################ LOADING PACKAGES AND DATAFRAME ################################
############################################################################################

getwd()
setwd("./Curso R/QOG Data")

require(tidyverse)
require(ggplot2)

pov.aid <- read.csv("./vinicius-santana-ad-ufpe-2019.csv")

view(pov.aid)
names(pov.aid)
summary(pov.aid)

#########################################################################################
################################ EXPLORATORY DATA ANALYSIS ##############################
#########################################################################################

# how many countries for each of the relevant areas of study
ggplot(data = pov.aid) +
  geom_bar(mapping = aes(x = cregion)) + coord_flip() +
  labs(title = "Number of countries per basin area")

# checking the distribution of aid_capta within cregion 
ggplot(data = pov.aid, mapping = aes(x = cregion, y = aid_capta)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Distribution of International Aid per Capta")

# checking the distribution of aid_total within regions
ggplot(data = pov.aid, mapping = aes(x = cregion, y = aid_total)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Distribution of Total Aid")

# plotting aid total and aid capta in one plot
aid.total.plot <- ggplot(data = pov.aid, mapping = aes(x = cregion, y = aid_total)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Distribution of Total Aid")

aid.capta.plot <- ggplot(data = pov.aid, mapping = aes(x = cregion, y = aid_capta)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Distribution of International Aid per Capta")

require(gridExtra)
grid.arrange(aid.total.plot, aid.capta.plot, ncol = 2)

# calculating median aid per capta in the bay of bengal
summary(pov.aid$aid_capta[pov.aid$cregion == "Bay of Bengal"])

# distribution of pov_rate within regions
ggplot(data = pov.aid, mapping = aes(x = cregion, y = pov_rate)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Distribution of Poverty Rates within Regions")

# HIGHEST POVERTY RATES ARE ENCOUNTERED IN ALL THREE AFRICAN REGIONS

# checking mean income and aid_crsio
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = aid_crsio))

# mean income and aid_crsio log
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = log(aid_crsio)))

# median_inc e aid crsc
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = aid_crsc))

# log
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = log(aid_crsc)))

# median income and aid total
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = aid_total))

#log
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = log(aid_total)))
# IT DOESNT SEEM THAT MEDIAN INCOME MATTERS MUCH FOR THE SIZE OF AID

# mean income and aid capta
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = log(aid_capta)))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = aid_capta))

##########################################################################################
##########################################################################################
##########################################################################################

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_size, y = aid_total))
# BUT IT DOES SEEM THAT THE HIGHER THE GDP THE HIGHER THE AID TOTAL

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_size, y = aid_total)) +
  geom_smooth(mapping = aes(x = gdp_size, y = aid_total))
# OR MAYBE NOT MUCH

# what about if GDO is logged?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = log(gdp_size), y = aid_total)) # even clearer correlation

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = log(gdp_size), y = aid_total)) +
  geom_smooth(mapping = aes(x = log(gdp_size), y = aid_total))

# what if aid is logged?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_size, y = log(aid_total))) # not very conclusive, but
#it seems to be the case when the amount of aid gets higher.

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_size, y = log(aid_total))) +
  geom_smooth(mapping = aes(x = gdp_size, y = log(aid_total)))

# what if both are logged?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = log(gdp_size), y = log(aid_total))) +
  geom_point(mapping = aes(color = cregion))# VERY STRONG CORRELATION

ggplot(data = pov.aid, mapping = aes(x = log(gdp_size), y = log(aid_total))) + 
  geom_point(mapping = aes(color = cregion)) + 
  geom_smooth()

ggplot(data = pov.aid, mapping = aes(x = log(gdp_size), y = log(aid_total))) + 
  geom_point(mapping = aes(color = cregion)) +
  labs(title = "Relation between GDP and Aid - both logged")

ggplot(data = pov.aid, mapping = aes(x = log(gdp_size), y = log(aid_total))) + 
  geom_point(mapping = aes(color = cregion)) +
  geom_smooth(mapping = aes(x = log(gdp_size), y = log(aid_total)))
  labs(title = "Relation between GDP and Aid - both logged")
  
###########################################################################################

# checking it by gdp_capta
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gpd_capta, y = aid_total)) # not very conclusive
  
# logging aid
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gpd_capta, y = log(aid_total))) +
  geom_smooth(mapping = aes(x = gpd_capta, y = log(aid_total)))

#who gives more based on gdp size? IO or other donors?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_size, y = aid_crsio)) #international organizations

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_size, y = aid_crsc))

# does the same apply to gdp capta?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gpd_capta, y = aid_crsio))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gpd_capta, y = aid_crsc)) # inconclusive

############################################################################################

# WHAT ABOUT POVERTY RATES?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = pov_rate, y = aid_total))
# data suggests that countries that receive low aid_total tend to have their aid slightly
# increased as poverty rates are higher. High aid total was only given to countries with less
# than 40% pov rate

# logging it
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = pov_rate, y = log(aid_total))) +
  geom_smooth(mapping = aes(x = pov_rate, y = log(aid_total))) # when logged, data correlation
# seems to disappear but the line indicates a rather slight trend towards reducing aid as
# poverty rates get higher

# checking by aid_capta
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = pov_rate, y = aid_capta)) +
  geom_smooth(mapping = aes(x = pov_rate, y = aid_capta))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = pov_rate, y = aid_capta)) +
  geom_smooth(mapping = aes(x = pov_rate, y = aid_capta))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = pov_rate, y = aid_crsio)) 

# log crsio
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = pov_rate, y = log(aid_crsio))) 
# it seems the higher the poverty rate, the lower the aid donated by intl org

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gpd_capta, y = aid_crsc)) # same applies for other donors

# what about aid capta and aid total? i start to think there is a correlation here
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = aid_capta, y = aid_total))

# what if
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = aid_total, y = aid_capta))

#what about logging aidtotal
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = aid_capta, y = log(aid_total))) +
  geom_smooth(mapping = aes(x = aid_capta, y = log(aid_total)))
# apparently, the lower aid capta the higher aid total.. especially 

ggplot(data = pov.aid, mapping = aes(x = aid_capta, y = log(aid_total))) + 
  geom_point(mapping = aes(color = cregion)) +
  geom_smooth(mapping = aes(x = aid_capta, y= log(aid_total))) +
  labs(title = "Relation between Aid Total and Aid per capta")
# the lower aid capta, the higher aid total for the Bay of Bengal; Nile Basin has similar
# aid capta range, as seen also in the box plot, but aid total varies


# what about gdp growth?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_growth, y = aid_total))

#log aidtotal
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_growth, y = log(aid_total))) # when logged there seems to
# be a slight correlation

# adding geom_smooh
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_growth, y = log(aid_total))) +
  geom_smooth(mapping = aes(x = gdp_growth, y = log(aid_total))) # yes! the lower the gdp growth
# the less logged aid

#adding cregion
ggplot(data = pov.aid, mapping = aes(x = gdp_growth, y = log(aid_total))) + 
  geom_point(mapping = aes(color = cregion)) +
  geom_smooth(mapping = aes(x = gdp_growth, y= log(aid_total))) +
  labs(title = "Relation between Aid Total and GDP growth") +
  xlab(label = "GDP GROWTH") +
  ylab(label = "TOTAL AID - logged")

# gdp growth and aid crsio
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_growth, y = aid_crsio))

#log aidcrisio
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_growth, y = log(aid_crsio)))

ggplot(data = pov.aid, mapping = aes(x = gdp_growth, y = log(aid_crsio))) + 
  geom_point(mapping = aes(color = cregion)) +
  geom_smooth(mapping = aes(x = gdp_growth, y= log(aid_crsio)))

# if only aid to the poor is considered
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_growth, y = aid_poor)) +
  geom_smooth(mapping = aes(x = gdp_growth, y = aid_poor))

ggplot(data = pov.aid, mapping = aes(x = gdp_growth, y = log(aid_poor))) + 
  geom_point(mapping = aes(color = cregion)) +
  geom_smooth(mapping = aes(x = gdp_growth, y= log(aid_poor)))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_growth, y = aid_crsc))
# IT SEEMS AS GDP GROWTH IS HIGHER, THE AID ENDS TO INCREASE

# what about regions?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = cregion, y = log(aid_total))) + coord_flip() +
  geom_smooth(mapping = aes(x = cregion, y = log(aid_total))) + coord_flip()


ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = cregion, y = log(aid_crsio))) + coord_flip() #two countries
# in the caspian sea received significantly less from crsio (seen only when logged)

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = cregion, y = log(aid_crsc))) + coord_flip() #two countries
# in the amazon bay received significanly less from crsc

# checking relation between pov_rate and aid_total for each region
ggplot(data = pov.aid) + 
  geom_point(mapping = aes(x = pov_rate, y = log(aid_total))) + 
  facet_wrap(~ cregion, nrow = 2)

#same with aid capta
ggplot(data = pov.aid) + 
  geom_point(mapping = aes(x = pov_rate, y = log(aid_capta))) + 
  facet_wrap(~ cregion, nrow = 2) # considering aid capta there is less conclusive evidence

#same with aid poor
ggplot(data = pov.aid) + 
  geom_point(mapping = aes(x = pov_rate, y = log(aid_poor))) + 
  facet_wrap(~ cregion, nrow = 2) # considering aid poor

# without log
ggplot(data = pov.aid) + 
  geom_point(mapping = aes(x = pov_rate, y = aid_poor)) + 
  facet_wrap(~ cregion, nrow = 2) #NOT USE

# when I add the smooth line, something else happens and the correlation seems to disappear
# WHY????
ggplot(data = pov.aid) + 
  geom_point(mapping = aes(x = pov_rate, y = log(aid_total))) + 
  geom_smooth(mapping = aes(x = pov_rate, y = log(aid_total))) +
  facet_wrap(~ cregion, nrow = 2) #not use this.. warning say few values for degrees of freedom

# THE HIGHER GDP SIZE(logged), THE HIGHER AID TOTAL LOGGED. THE RELATIONSHIP IS LESS CLEAR IN
# the congo and niger basin and partially clear in the caspian sea.. for the other three groups
# the pattern is more evident, with one country in the Amazon region falling significantly off
# the curve.
ggplot(data = pov.aid) + 
  geom_point(mapping = aes(x = log(gdp_size), y = log(aid_total))) + 
  facet_wrap(~ cregion, nrow = 2) # info above

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = cregion, y = aid_capta)) + coord_flip()

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = cregion, y = aid_poor)) + coord_flip() # not use this

# gdp capta and aid total
ggplot(data = pov.aid) +
  geom_hex(mapping = aes(x = gpd_capta, y = aid_total))

#logging aid
ggplot(data = pov.aid) +
  geom_hex(mapping = aes(x = gpd_capta, y = log(aid_total)))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gpd_capta, y = log(aid_total))) +
  geom_smooth(mapping = aes(x = gpd_capta, y = log(aid_total)))

#loggin gdp capta
ggplot(data = pov.aid) +
  geom_hex(mapping = aes(x = log(gpd_capta), y = aid_total))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = log(gpd_capta), y = aid_total)) +
  geom_smooth(mapping = aes(x = log(gpd_capta), y = aid_total))

# logging both
ggplot(data = pov.aid) +
  geom_hex(mapping = aes(x = log(gpd_capta), y = log(aid_total)))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = log(gpd_capta), y = log(aid_total))) +
  geom_smooth(mapping = aes(x = log(gpd_capta), y = log(aid_total)))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = log(gpd_capta), y = log(aid_total)))

ggplot(data = pov.aid) +
  geom_hex(mapping = aes(x = gdp_size, y = aid_total)) # rather inconclusive with gdp capta

ggplot(data = pov.aid) +
  geom_hex(mapping = aes(x = pov_rate, y = aid_total))

ggplot(data = pov.aid) +
  geom_hex(mapping = aes(x = pov_rate, y = aid_capta))

# what about gdp size and aid_total when they are logged?
ggplot(data = pov.aid, aes(x = gdp_size, y = aid_total)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() # it also seems the higher the gdp size, the higher the aid

ggplot(data = pov.aid, aes(x = gdp_size, y = aid_total)) +
  geom_point() # without log

ggplot(data = pov.aid, aes(x = log(gdp_size), y = aid_total)) +
  geom_point() # log gdp size

ggplot(data = pov.aid, aes(x = gdp_size, y = log(aid_total))) +
  geom_point() # log aid total # no good

ggplot(data = pov.aid, aes(x = log(gdp_size), y = log(aid_total))) +
  geom_point() # log both (# use this rather than the other)

# adding line
ggplot(data = pov.aid, aes(x = log(gdp_size), y = log(aid_total))) +
  geom_point(mapping = aes(color = cregion)) +
  geom_smooth() # I have seen this graph before

# same for pov rate
ggplot(data = pov.aid, aes(x = pov_rate, y = log(aid_total))) +
  geom_point(mapping = aes(color = cregion)) +
  geom_smooth() # which in turn did not show as much correlation

# MODELLING (AVOID)
library(modelr)

mod.gdp.aid <- lm(log(aid_total) ~ log(gdp_size), data = pov.aid) # does gdp size explain amount of aid?
summary(mod.gdp.aid) # logged, yes, P relevant

mod.pov.aid <- lm(log(aid_total) ~ pov_rate, data = pov.aid) # does gdp size explain amount of aid?
summary(mod.pov.aid)

pov.aid2 <- pov.aid %>% 
  add_residuals(mod.gdp.aid) %>% 
  mutate(resid = exp(resid))

ggplot(data = pov.aid2) + 
  geom_point(mapping = aes(x = gdp_size, y = resid)) # I didnt quite understand this

ggplot(data = pov.aid2) + 
  geom_boxplot(mapping = aes(x = cregion, y = resid)) + coord_flip()

## DONT KNOW HOW TO USE THIS JUST YET

###########################################################################################
##################################### OTHER PLOT CHECKS ###################################
###########################################################################################


plot(pov.aid$aid_total, pov.aid$pov_rate)
# FROM THIS PLOT WE SEE THAT COUNTRIES THAT RECEIVED MORE TOTAL AID ARE COUNTRIES THAT
# HAVE LOWER POVERTY RATES.

plot(pov.aid$aid_crsc, pov.aid$pov_rate)
# THE SAME APPLIES IF ONLY DONORS, WITHOUT INTERNATIONAL ORGANIZATIONS ARE TAKEN INTO ACCOUNT

plot(pov.aid$aid_crsio, pov.aid$pov_rate)
# AND IF ONLY THE AID FROM INTERNATIONAL ORGANIZATIONS ARE TAKEN INTO ACCOUNT

plot(pov.aid$aid_total, pov.aid$gdp_size)
# AS ECONOMIES ARE HIGHER, HOWEVER THE TENDENCY SEEMS TO BE HIGHER AID

plot(pov.aid$aid_total, pov.aid$gdp_growth)
# IT SEEMS NOT TO BE ABOUT GDP GROWTH, BUT ON GDP SIZE ONLY

plot(pov.aid$gdp_size, pov.aid$aid_capta)
# ONLY COUNTRIES WITH LOW GDP SIZE HAVE RECEIVED HIGH PER CAPTA AID

plot(pov.aid$pov_rate, pov.aid$aid_capta)
# IT SEEMS TO HAVE PRETTY MUCH EQUAL DISTRIBUTION HERE

plot(pov.aid$gpd_capta, pov.aid$aid_capta)


##########################################################################################
################################ REGRESSION MODELS #######################################
##########################################################################################

# APPLYING A SIMPLE LINEAR REGRESSION USING POVERTY RATE AND TOTAL AID, TO SEE HOW THEY BEHAVE
povaidreg <- lm(data = pov.aid, log(aid_total) ~ pov_rate + log(gdp_size) + gpd_capta + median_inc + gdp_growth + log(pop_poor))
summary (povaidreg)

povaidreg2 <- lm(data = pov.aid, log(aid_total) ~ pov_rate + log(gdp_size) + gpd_capta + median_inc + gdp_growth + log(pop_poor) + cregion)
summary (povaidreg2)

#checking correlation between the two main variables
cor(pov.aid$pov_rate, log(pov.aid$gdp_size), method = "pearson")
# -.54 meaning only partially correlated

#testand covariancia entre a variavel dependente e independente
cor(log(pov.aid$aid_total), log(pov.aid$gdp_size), method = "pearson") #0.68
cor(log(pov.aid$aid_total), pov.aid$pov_rate, method = "pearson") #-.20 very low neg correlation

cov(log(pov.aid$aid_total), log(pov.aid$gdp_size), method = "pearson") #0.68
cov(log(pov.aid$aid_total), pov.aid$pov_rate, method = "pearson")

#SAME SEM O LOG
povaidregwithoutlogg <- lm(data = pov.aid, aid_total ~ pov_rate + gdp_size + gpd_capta + median_inc + gdp_growth + pop_poor + cregion)
summary (povaidregwithoutlogg)

# AS IT SEEMS, THE HIGHER THE POVERTY RATE, THE LOWER THE AID RECEIVED BY THE COUNTRIES, AS
# WITH A NEGATIVE SIGNAL IN THE ESTIMATE WITH A SIGNIFICANT P VALUE OF 0.04791.

regcregion <- lm(data = pov.aid, log(aid_total) ~ cregion + pov_rate + log(gdp_size) + gpd_capta + median_inc + gdp_growth + pop_poor)
summary(regcregion)

   plot(povaidreg1)

# WHAT CAN I INFER ABOUT THE FITTED AND RESIDUALS?

# APPLYING A LINEAR REGRESSION USING GDP SIZE TO ESTIMATE THE VALUE OF TOTAL AID
povaidreg3 <- lm(data = pov.aid, log(aid_total) ~ log(gdp_size) + pov_rate)
summary(povaidreg3)

# WITH A HIGH SIGNFICANT VALUE, AND A POSITIVE SIGNAL, IT SEEMS HIGHER GDP ALLOWS FOR MORE
# AID RECEIVED IN TOTAL

plot(povaidreg2)

# IF GDP ALLOWS MORE AID, HOW DOES IT WORK WHEN TAKING POVERTY RATE INTO ACCOUNT CONTROLLING
# THE OUTCOMES OF INTERNATIONAL COOPERATION?

povaidmulti <- lm(data = pov.aid, aid_total ~ gdp_size + pov_rate)
summary(povaidmulti)

# WITH THIS REGRESSION, POVERTY RATE BECOMES STATISTICALLY NON SIGNIFICANT HOWEVER IT STILL SHOWS
# A NEGATIVE INFLUENCE OVER THE TOTAL AMOUNT OF AID RECEIVED BY THE COUNTRIES

plot(povaidmulti)

# CHECKING IF THE REGION PLAYS A ROLL IN THE OUTCOMES OF TOTAL AID
plot(pov.aid$cregion, pov.aid$aid_total)
aid.regionreg1 <- lm(data = pov.aid, aid_total ~ cregion)
summary(aid.regionreg1)

# with log
aid.regionreg1log <- lm(data = pov.aid, log(aid_total) ~ cregion)
summary(aid.regionreg1log)

#aidcapta
aidcaptaregion <- lm(data = pov.aid, aid_capta ~ cregion)
summary(aidcaptaregion) # NOT USE; NOT IMPORTANT

# IN RELATION TO THE AMAZON BASIN, ONLY  THE BAY OF BENGAL HAS A SLIGHT STATISTICAL SIGNIFICANCE
# SHOWING IT PLAYS A ROLE IN INCREASING THE VALUE OF AID.

# ADDING TO THE REGRESSION THE GDP GROWTH, THE MEDIAN INCOME AND THE REGION OF COUNTRIES
full.pov.aid.reg <- lm(data = pov.aid, aid_total ~ pov_rate + gdp_size + gdp_growth + cregion)
summary(full.pov.aid.reg)

# CHECKING AN INVERTED REGRESSION WHERE POVERTY RATES CONTROLL THE INFLUENCE OF GDP IN AID RECEIVING
inverse.pov.aid.reg <- lm(data = pov.aid, aid_total ~ gdp_size + pov_rate + gdp_growth + median_inc + cregion)
summary(inverse.pov.aid.reg)

# AGAIN GDP SHOWS TO BE MORE SIGNIFICANT THAN POVERTY RATE

#BACK TO FULL MODEL TO BE ANALYSED

plot(log(pov.aid$pop_total), log(pov.aid$aid_total))
cor(log(pov.aid$pop_total), log(pov.aid$aid_total), method = "pearson") #.79 high correlation
cor(log(pov.aid$pop_total), log(pov.aid$gdp_size), method = "pearson") #.82 high correlation
# i will therefore take pop_total out of the model,let us check for pop_poor
plot(log(pov.aid$pop_poor), log(pov.aid$aid_total))
cor(log(pov.aid$pop_poor), log(pov.aid$aid_total), method = "pearson") #less correlated .43
cor(log(pov.aid$pop_poor), log(pov.aid$gdp_size), method = "pearson") #even less .19, include pop poor

########################################################################################
#FULL MODELS

###### MODEL ONE POVERTY RATE BEING CONTROLLED BY THE REST

fullmodel1 <- lm(data = pov.aid, log(aid_total) ~ pov_rate + log(gdp_size) + gdp_growth + median_inc + log(pop_poor) + cregion)
summary(fullmodel1)                 
# it explains 69% of the statistics (59 if R squared are adjusted).. controlled by the others,
# pov_rate presents to have a slightly negative impact on the total amount of aid received.
# this can be interpreted like this: since the poverty rate depends on the poverty rates of the
# previous years, then donor countries and organizations might interpret it as the aid is not
# being effectively used, and therefore, high rates of poverty, despite denouncing the need for
# aid will have a negative effect, although marginal on total aid. GDP size, however, not only
# is statistically significant to total aid, but represents a fair increase in total aid.

# checking if the model fits
plot(fullmodel1)
# graph 1 shows the residuals vs fitted, it can be seen that the model confirms the homoskedacist
# need as the residual values are reasonably distributed around zero and with similar amplitude.
# graph two shows that most of the data are not far away from the diagonal line, except for the
# two extreme values. the sclae location show the standardized residuals within the fitted values
#of the model. The last graph shows the Cook's distance, that does show most data fit well in the
# model, and there is one piece that gets closer to being an outlier

shapiro.test(fullmodel1$residuals) # this shows the residuals have normal distribution as its
# p value is higher than 0.05

plot(density(resid(fullmodel1))) # plot de densidade dos residuos, ok
qqnorm(resid(fullmodel1))
qqline(resid(fullmodel1))   # this is the same as with plot(lm)

library(sjPlot)
plot_model(fullmodel1, type = "pred", terms = c("pov_rate")) # PREDICTED VALUES OF TOTAL AID poverty
plot_model(fullmodel1, type = "pred", terms = c("gdp_size [exp]")) # PREDICTED VALUES OF TOTAL AID gdp size
plot_model(fullmodel1, type = "pred", terms = c("pop_poor"))
plot_model(fullmodel1, type = "pred", terms = c("pov_rate", "gdp_size"))
plot_model(fullmodel1, type = "pred", terms = c("gdp_size [exp]", "pov_rate"))
plot_model(fullmodel1, type = "pred", terms = c("pov_rate", "cregion"))

# creating the magnitude of coeficients graph
par(mfrow=c(1,1))
betas <- coefficients(fullmodel1)
IC <- confint(fullmodel1, level=0.95)
y.axis <- seq(from=1, to=length(betas))
plot(betas, y.axis, type="p", pch=19, xlab="Magnitude of Coeficients",
     ylab="", axes=F, xlim=c(min(IC-.4), max(IC+.4)), ylim=c(min(y.axis-.2),
                                                             max(y.axis+.2)), cex=1,yaxs="i",xaxs="i")
segments(IC[,1], y.axis, IC[,2], y.axis)
axis(1, at=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1),
     labels=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), tick=T,
     cex.axis=1, mgp=c(2,.7,0))
axis(2, at=y.axis, label=names(betas), las=1, tick=T, line=-.5,
     cex.axis=1, mgp=c(2,.7,0))
abline(v=0, lty=2, col="red")

       