##ReplicationFinal.R
###Replication script for Bucchianeri, et al 2021, 
###"What explains local policy cleavages? Examining the policy preferences of public officials at the municipal level"
###Creates all Tables and Figures in article and Appendix, except for Figure A1, which was made using ArcGIS
###Ryan D. Enos, January 2022

rm(list = ls()) ##clear environment

##load necessary libraries
library(tidyverse) 
library(basicspace)
library(xtable)
library(cowplot)
library(estimatr)
library(dotwhisker)
library(stargazer)


source("coeff_plot_functions.R") ##functions for creating Figures 2 and 3

##load data
dat = read.csv("MayorsCityData.csv")
ip_dat = read.csv("IPUMSData.csv")


####Main Article####


###Figure 1###

##create 1 plot for each tradeoff question
ineq <- qplot(tradeoff.ineq, data=dat, geom="histogram", binwidth = .5, fill=I("dodgerblue3"), alpha=I(.5), ylim=c(0,500)) + labs(x = "Income Inequality")
propval <- qplot(tradeoff.propval, data=dat, geom="histogram", binwidth = .5, fill=I("blue4"), alpha=I(.5), ylim=c(0,500)) + labs(x = "Property Values")
climate <- qplot(tradeoff.climate, data=dat, geom="histogram", binwidth = .5, fill=I("darkorchid4"), alpha=I(.5), ylim=c(0,500)) + labs(x = "Climate Change")
housing <- qplot(tradeoff.housing, data=dat, geom="histogram", binwidth = .5, fill=I("darkred"), alpha=I(.5), ylim=c(0,500)) + labs(x = "Affordable Housing")
charters <- qplot(tradeoff.charters, data=dat, geom="histogram", binwidth = .5, fill=I("darkgreen"), alpha=I(.5), ylim=c(0,500)) + labs(x = "Charter Schools")
minwage <- qplot(tradeoff.minwage, data=dat, geom="histogram", binwidth = .5, fill=I("deeppink4"), alpha=I(.5), ylim=c(0,500)) + labs(x = "Minimum Wage")
police <- qplot(tradeoff.police, data=dat, geom="histogram", binwidth = .5, fill=I("lightslateblue"), alpha=I(.5), ylim=c(0,500)) + labs(x = "Police/Body Cameras")
transit <- qplot(tradeoff.transit, data=dat, geom="histogram", binwidth = .5, fill=I("orangered2"), alpha=I(.5), ylim=c(0,500)) + labs(x = "Public Transit")
lgbt <- qplot(tradeoff.lgbt, data=dat, geom="histogram", binwidth = .5, fill=I("seagreen3"), alpha=I(.5), ylim=c(0,500)) + labs(x = "LGBT Anti-Discrimination")
private <- qplot(tradeoff.private, data=dat, geom="histogram", binwidth = .5, fill=I("turquoise4"), alpha=I(.5), ylim=c(0,500)) + labs(x = "Service Privatization")
schools <- qplot(tradeoff.schools, data=dat, geom="histogram", binwidth = .5, fill=I("violetred"), alpha=I(.5), ylim=c(0,500)) + labs(x = "Open Enrollment")
justice <- qplot(tradeoff.justice, data=dat, geom="histogram", binwidth = .5, fill=I("skyblue4"), alpha=I(.5), ylim=c(0,500)) + labs(x = "Underrep. Groups")

##use cowplot to create a grid of these plots
theme_set(theme_cowplot(font_size=12))
grid <- plot_grid(housing, charters, climate, ineq, lgbt, minwage, police, propval, transit, schools, private, justice)
ggsave('Figure_1.pdf',
       width = 8.5,
       height = 11)




###Table 1###

##Isolate Tradeoff variables
tradeoffs <- dat[, c(grep("tradeoff", colnames(dat)))] 
colnames(tradeoffs) <- gsub("tradeoff.", "", colnames(tradeoffs))

##Editing Columns with Strings + NAs --> Numeric
for(i in c(1,4,5,7)){
  tradeoffs[,i] <- ifelse(tradeoffs[,i] == "Disagree ", 2, tradeoffs[,i])
  tradeoffs[,i] <- ifelse(tradeoffs[,i] == "Agree ", 4, tradeoffs[,i])
}; rm(i)
tradeoffs[is.na(tradeoffs)] <- 999
tradeoffs <- apply(tradeoffs, 2, as.numeric)


##Poole's 'Basic Space' Scaling 
tradeoffs_out <- blackbox(tradeoffs, missing=999, dims=3, minscale=5, verbose=TRUE) 

tradeoffs_out$fits # Variance Explained by Dimension
sum(tradeoffs_out$fits$percent) # Total Variance Explained
tradeoffs_out$stimuli # Issue specific weights by dimension + R^2 

##Export
out <- tradeoffs_out$stimuli[[1]]
out$w2 <- tradeoffs_out$stimuli[[2]]$w2
out$R2_D2 <- tradeoffs_out$stimuli[[2]]$R2
out$R2_D2_gain <- out$R2_D2 - out$R2
out <- out[,-which(colnames(out) == 'c')]
out.table = xtable(out)
print(out.table, file = "Table_1.tex")




###Figure 2###

##regress tradeoff loadings on individual predictors
d1_preds <- lm_robust(tradeoffs_d1 ~ factor(partyID) + ideology + gender + white +  mayor + pnwhite + logInc + phis + logpop + log_debt_out_per_cap + pcol, 
                      data=dat, clusters = dat$NAME_E) 
d2_preds <- lm_robust(tradeoffs_d2 ~ factor(partyID) + ideology + gender + white +  mayor + pnwhite + logInc + phis+ logpop + log_debt_out_per_cap + pcol, 
                      data=dat, clusters = dat$NAME_E)


brackets <- list(c("Individual-Level", "Republican", "Mayor"), c("City-Level", "Percent Nonwhite", "Percent College Graduates"))

#dot and whisker plot function
{dwplot(bind_rows(bind_cols(term=names(d1_preds$coefficients),
                            estimate=d1_preds$coefficients,
                            std.error=d1_preds$std.error,
                            statistic=d1_preds$statistic,
                            p.value=d1_preds$p.value) %>% mutate(model = 'D1'), 
                  bind_cols(term=names(d2_preds$coefficients),
                            estimate=d2_preds$coefficients,
                            std.error=d2_preds$std.error,
                            statistic=d2_preds$statistic,
                            p.value=d2_preds$p.value) %>% mutate(model = 'D2')),
        by_2sd = FALSE,
        whisker_args = list(aes(linetype = model)),
        dot_args = list(size = 2)
) %>% 
    relabel_predictors(c(`factor(partyID)2` = "Republican",   
                         `factor(partyID)3` = "Independent",
                         ideology = "Self-Reported Ideology", 
                         gender = "Male", 
                         white = "White", 
                         mayor = "Mayor",
                         pnwhite = "Percent Nonwhite",
                         logInc = "log(Median Income)",
                         phis = "Percent Hispanic",
                         logpop = "log(Population)", 
                         log_debt_out_per_cap = "log(Debt Per Capita)",
                         pcol = 'Percent College Graduates'
    )) +
    theme_bw() + xlab("Coefficient Estimate") + ylab("") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    theme(legend.position = 'right',
          text=element_text(size=12))  +
    
    scale_color_manual(name = "Dependent Variable:", 
                       labels = c("D1 Ideal Point", "D2 Ideal Point"),
                       values = c('D1' = 'black', 'D2' = 'steelblue3')
    ) 
} %>% 
  add_brackets(brackets,face="plain") ##call from coeff_plot_functions.R

ggsave('Figure_2.pdf',
       height = 6,
       width = 10)




###Figure 3###

##regress tradeoff loadings on supporters
d1_support <- lm_robust(tradeoffs_d1 ~ support.i_c + support.n_c + support.o_c +support.h_c + support.k_c + support.l_c + 
                          support.m_c + support.a_c + support.p_c + support.e_c + support.c_c + support.b_c + support.j_c + 
                          support.f_c + support.q_c + support.g_c + support.d_c,
                        data=dat, clusters = dat$NAME_E)
d2_support <- lm_robust(tradeoffs_d2 ~ support.i_c +  support.n_c + support.o_c +support.h_c + support.k_c + support.l_c + 
                          support.m_c + support.a_c + support.p_c + support.e_c + support.c_c + support.b_c + support.j_c + 
                          support.f_c + support.q_c + support.g_c + support.d_c,
                        data=dat, clusters = dat$NAME_E)


brackets <- list(c("Party Orgs/ \nOfficials", "Local Republican Comm.", "Local Democratic Comm."), 
                 c("Unions", "Police/Firefighter Unions", "Other Unions"),
                 c("Industry", "Local Business Groups","Health Industry"),
                 c("Other","Civic/Fraternal Orgs.","Environemntal Groups"))

#dot and whisker plot function
{dwplot(bind_rows(bind_cols(term=names(d1_support$coefficients),
                            estimate=d1_support$coefficients,
                            std.error=d1_support$std.error,
                            statistic=d1_support$statistic,
                            p.value=d1_support$p.value) %>% mutate(model = 'D1'), 
                  bind_cols(term=names(d2_support$coefficients),
                            estimate=d2_support$coefficients,
                            std.error=d2_support$std.error,
                            statistic=d2_support$statistic,
                            p.value=d2_support$p.value) %>% mutate(model = 'D2')),
        by_2sd = FALSE,
        whisker_args = list(aes(linetype = model)),
        dot_args = list(size = 2)) %>% 
    relabel_predictors(c(support.i_c = "Local Republican Comm.",
                         support.n_c = "State Officials",
                         support.o_c = "Federal Officials",
                         support.h_c = "Local Democratic Comm.",
                         support.k_c = "Police/Firefighter Unions",
                         support.l_c = "Teacher's Unions",
                         support.m_c = "Other Unions",
                         support.a_c = "Local Business Groups",
                         support.p_c = 'Real Estate Industry', 
                         support.e_c = "Health Industry",
                         support.c_c = "Civic/Fraternal Orgs.",
                         support.b_c = "Faith-Based Orgs.",
                         support.j_c = 'Newspapers',
                         support.f_c = "Large Individual Donors",
                         support.q_c = "Women's Orgs.",
                         support.g_c = "LGBT Orgs.", 
                         support.d_c = "Environemntal Groups"                      
    )) +
    theme_bw() + xlab("Coefficient Estimate") + ylab("") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    theme(legend.position = 'right',
          text=element_text(size=12))  +
    scale_color_manual(name = "Dependent Variable:", 
                       labels = c("D1 Ideal Point", "D2 Ideal Point"),
                       values = c('D1' = 'black', 'D2' = 'steelblue3'))} %>% 
  add_brackets(brackets, face="plain") ##call from coeff_plot_functions.R

ggsave('Figure_3.pdf',
       height = 6,
       width = 10)



####Appendix####

##Figure A1 was made in ArcGIS


###Table A1###

#summarize relevant variables for cities in sample
samp_sub <- dat %>%
  select(pop, pmin, phis, Inc, pcol, pman) %>%
  distinct() %>%
  gather()%>% 
  group_by(key) %>% 
  summarise_all(funs(Minimum = min, Maximum = max,Median = median, Mean = mean, SD = sd), na.rm = TRUE)%>%
  mutate(sample = 'Sample')  

#summarize relevant variables for cities with more than 20 population
ip_sub <- ip_dat %>%
  select(pop, pmin, phis, Inc, pcol, pman) %>%
  distinct() %>%
  gather()%>% 
  group_by(key) %>% 
  summarise_all(funs(Minimum = min, Maximum = max,Median = median, Mean = mean, SD = sd), na.rm = TRUE)%>%
  mutate(sample = 'All Cities/Towns')  

#combine summaries
out = rbind(samp_sub,ip_sub)
##format
out$key = rep(c('Per Capita 12-mo Income','Percent with Bachelors Degree or Higher',
                       'Percent of Hispanic Origin','Percent Manufacturing',
                       'Percent Racial Minorities','Total Population'),2)

out.table = xtable(out)
print(out.table, file = "Table_A1.tex")




###Figure A2###



## All Cities, Subset to relevant variables
ip_sub <- mutate(ip_dat, 
                 sample = "All Cities/Towns"
) %>%
  select(city_name, logpop, pmin, phis, logInc, pcol, pman, sample)

### Sample Cities, Subset to relevant vars
comp_sub <- filter(ip_sub, logpop > log(10000)) %>%
  mutate(sample = 'Cities/Towns > 10,000')

##sample cities
samp_sub <- mutate(dat,
  sample = 'Cities/Towns in Sample') %>%
  select(city_name, logpop, pmin, phis, logInc, pcol, pman, sample) %>%
  distinct()

##combine the samples
both <- bind_rows(ip_sub, comp_sub, samp_sub) %>%
  tidyr::gather("variable", "measure", 2:7) %>%
  mutate(variable = factor(variable)) %>%
  arrange(variable)   
levels(both$variable)
levels(both$variable) <- c("log(MedianIncome)", "log(Total Popultion)", "% College Grad.", "% Hispanic",  "% Manufacturing Employment", "% Nonwhite") 

##plot
ggplot(both, aes(x=measure, fill=sample)) + 
  facet_wrap(~variable, nrow = 2, ncol = 3, scales = "free") + 
  geom_density(alpha=0.25) + labs(fill="") + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), strip.text.x = element_text(size = 12)) + 
  ggtitle("") + xlab("") + ylab("Density") 

ggsave("Figure_A2.pdf",
       width = 9, height = 6)




###Table A2###

##subset to mayors and councilors 
mayors <- dat[dat$type == "mayor", ]
council <- dat[dat$type == "councillor", ]

##create blank matrix to add rows
mayors.mat = matrix(nrow=0, ncol = 1)
council.mat =  matrix(nrow=0, ncol = 1)


#gender
gender <- as.matrix(table(mayors$gender))
mayors.mat = rbind(mayors.mat,as.matrix((gender[ ,1]/nrow(mayors))*100))
gender <- as.matrix(table(council$gender))
council.mat = rbind(council.mat,as.matrix((gender[ ,1]/nrow(council))*100))


#race
race <- as.matrix(table(mayors$race))
mayors.mat = rbind(mayors.mat,as.matrix((race[ ,1]/nrow(mayors))*100))
race <- as.matrix(table(council$race))
council.mat = rbind(council.mat, as.matrix((race[ ,1]/nrow(council))*100))


#party 
party <- as.matrix(table(mayors$party))
mayors.mat = rbind(mayors.mat,as.matrix((party[ ,1]/nrow(mayors))*100))
ind <- as.matrix(table(mayors$party2))
mayors.mat = rbind(mayors.mat,as.matrix((ind[ ,1]/nrow(mayors))*100))
party <- as.matrix(table(council$party))
council.mat = rbind(council.mat,as.matrix((party[ ,1]/nrow(council))*100))
ind <- as.matrix(table(council$party2))
council.mat = rbind(council.mat, as.matrix((ind[ ,1]/nrow(council))*100))

#ideology
mayors.mat = rbind(mayors.mat,mean(mayors$ideology,na.rm=T))
mayors.mat = rbind(mayors.mat,sd(na.omit(mayors$ideology)))
council.mat = rbind(council.mat,mean(council$ideology,na.rm=T))
council.mat = rbind(council.mat,sd(na.omit(council$ideology)))

#progressive
prog <- as.matrix(table(mayors$progressive))
mayors.mat = rbind(mayors.mat, as.matrix((prog[ ,1]/nrow(mayors))*100))
prog <- as.matrix(table(council$progressive))
council.mat = rbind(council.mat, as.matrix((prog[ ,1]/nrow(council))*100))

out = merge(mayors.mat,council.mat,
            by = 0, sort = F)
out = out[out$Row.names!='X.2',] #remove the 'not leaner' category
row.names(out) = c('Missing Gender','Female','Male',
                   'Missing Race','Asian','Black','Hispanic','Mixed','Native American','White',
                   'Democratic','Republican','Independent',
                   'D leaner','R Leaner', 'Neither',
                   'Ideology Mean','Ideology SD',
                   'Progressive Missing','Prog Def Not','Prog Def Yes','Prog Not Sure','Prog Prob Not','Prog Prop Yes')
out$Row.names=NULL #remove unneeded column
out.table = xtable(out)
print(out.table, file = "Table_A2.tex")



###Table C3###

##Need to fit with lm() first and add in SEs get stargazer to work here..
d1_preds_out <- lm(tradeoffs_d1 ~ factor(partyID) + ideology + white + gender + mayor + logpop + pnwhite + phis + pcol + logInc + log_debt_out_per_cap, data=dat)
d1_preds_mayors <- lm(tradeoffs_d1 ~ factor(partyID) + ideology + white + gender + logpop + pnwhite + phis + pcol + logInc + log_debt_out_per_cap, 
                      data=dat, subset = dat$mayor == 1)
d1_preds_CMs <- lm(tradeoffs_d1 ~ factor(partyID) + ideology + white + gender + logpop + pnwhite + phis + pcol + logInc + log_debt_out_per_cap, 
                   data=dat, subset = dat$mayor == 0)
d2_preds_out <- lm(tradeoffs_d2 ~ factor(partyID) + ideology + white + gender + mayor + logpop + pnwhite + phis + pcol + logInc + log_debt_out_per_cap, data=dat)
d2_preds_mayors <- lm(tradeoffs_d2 ~ factor(partyID) + ideology + white + gender + logpop + pnwhite + phis + pcol + logInc + log_debt_out_per_cap, 
                      data=dat, subset = dat$mayor == 1)
d2_preds_CMs <- lm(tradeoffs_d2 ~ factor(partyID) + ideology + white + gender + logpop + pnwhite + phis + pcol + logInc + log_debt_out_per_cap, 
                   data=dat, subset = dat$mayor == 0)

##subsets for SEs
mayors <- filter(dat, mayor == 1)
councilmembers <- filter(dat, mayor == 0)

##Clustered SEs
stargazer(d1_preds_out, d1_preds_mayors, d1_preds_CMs, d2_preds_out, d2_preds_mayors, d2_preds_CMs, 
          font.size = 'scriptsize',
          se = list(
            starprep(d1_preds_out, clusters = dat[-d1_preds_out$na.action,]$NAME_E)[[1]],
            starprep(d1_preds_mayors, clusters = mayors[-d1_preds_mayors$na.action,]$NAME_E)[[1]],
            starprep(d1_preds_CMs, clusters = councilmembers[-d1_preds_CMs$na.action,]$NAME_E)[[1]],
            starprep(d2_preds_out, clusters = dat[-d2_preds_out$na.action,]$NAME_E)[[1]],
            starprep(d2_preds_mayors, clusters = mayors[-d2_preds_mayors$na.action,]$NAME_E)[[1]],
            starprep(d2_preds_CMs, clusters = councilmembers[-d2_preds_CMs$na.action,]$NAME_E)[[1]]
          ),
          omit.stat = c('f', 'ser', 'adj.rsq'),
          covariate.labels = c("Republican", 'Independent', 'Ideology', 'White', 'Male', 'Mayor', 'log(Population)',
                               '\\% Nonwhite', '\\% Hispanic', '\\% College Grad.', 'log(Median Income)',  
                               'log(Debt Per Capita)', 'Constant'),
          dep.var.labels = c("D1 Ideal Point", "D2 Ideal Point"),
          column.labels = rep(c("All Officials", "Mayors", "Councilmembers"), 2),
          notes = c("All standard errors clustered at the city-level"),
          label = 'tab:predictors_regs',
          title = 'Individual/City-Level Characteristics and Ideological Placement by Office Type',
          out = "Table_C3.tex"
)


###Figure C3###

##create new data frame of cities
city <- data.frame(dat[,"city_name"]); colnames(city) <- "city"
city$city <- tools::toTitleCase(gsub("_", " ", city$city))
city$two_party_color <- ifelse(dat$dem == 1, "blue", ifelse(dat$dem == 0, "red", "yellow"))

##extract tradeoff data
t_d1 <- tradeoffs_out$individuals[[2]][,1]
t_d2 <- tradeoffs_out$individuals[[2]][,2]

##random names for labels
random <- which(abs(rnorm(length(t_d1))) > 2.4)
random <- c(random, 1099)

pdf("Figure_C3.pdf", height = 8, width = 9)
plot(t_d1, t_d2, main="", xlab="D1 (Liberal/Conservative)", ylab="D2 (Market Orientation)", cex=0, xlim=c(-.75, .75), ylim=c(-.75, .75))
points(t_d1, t_d2, pch=16, col=scales::alpha(city$two_party_color, .375), cex=.65) 
text(t_d1[random], t_d2[random], labels=city[random,1], cex=1, col=city[random,]$two_party_color) # ifelse(city[random,]$two_party_color == "red", "darkred", "darkblue")
dev.off()

##Table D4 
##Need to fit with lm() first and add in SEs get stargazer to work here..
d1_supp_out <- lm(tradeoffs_d1 ~ support.a_c + support.b_c + support.c_c + support.d_c + support.e_c + support.f_c + support.g_c + support.h_c + support.i_c + support.j_c + support.k_c + support.l_c + support.m_c + support.n_c + support.o_c + support.p_c + support.q_c, data=dat)
d1_supp_mayors <- lm(tradeoffs_d1 ~  support.a_c + support.b_c + support.c_c + support.d_c + support.e_c + support.f_c + support.g_c + support.h_c + support.i_c + support.j_c + support.k_c + support.l_c + support.m_c + support.n_c + support.o_c + support.p_c + support.q_c, 
                     data=dat, subset = dat$mayor == 1)
d1_supp_CMs <- lm(tradeoffs_d1 ~  support.a_c + support.b_c + support.c_c + support.d_c + support.e_c + support.f_c + support.g_c + support.h_c + support.i_c + support.j_c + support.k_c + support.l_c + support.m_c + support.n_c + support.o_c + support.p_c + support.q_c,
                  data=dat, subset = dat$mayor == 0)
d2_supp_out <- lm(tradeoffs_d2 ~  support.a_c + support.b_c + support.c_c + support.d_c + support.e_c + support.f_c + support.g_c + support.h_c + support.i_c + support.j_c + support.k_c + support.l_c + support.m_c + support.n_c + support.o_c + support.p_c + support.q_c, data=dat)
d2_supp_mayors <- lm(tradeoffs_d2 ~ support.a_c + support.b_c + support.c_c + support.d_c + support.e_c + support.f_c + support.g_c + support.h_c + support.i_c + support.j_c + support.k_c + support.l_c + support.m_c + support.n_c + support.o_c + support.p_c + support.q_c,
                     data=dat, subset = dat$mayor == 1)
d2_supp_CMs <- lm(tradeoffs_d2 ~  support.a_c + support.b_c + support.c_c + support.d_c + support.e_c + support.f_c + support.g_c + support.h_c + support.i_c + support.j_c + support.k_c + support.l_c + support.m_c + support.n_c + support.o_c + support.p_c + support.q_c, 
                  data=dat, subset = dat$mayor == 0)

# subsets for SEs 
mayors2 <- filter(dat, mayor == 1)
councilmembers2 <- filter(dat, mayor == 0)

stargazer(d1_supp_out, d1_supp_mayors, d1_supp_CMs, d2_supp_out, d2_supp_mayors, d2_supp_CMs, #type = 'text',
          font.size = 'scriptsize',
          se = list(
            starprep(d1_supp_out, clusters = dat[-d1_supp_out$na.action,]$NAME_E)[[1]],
            starprep(d1_supp_mayors, clusters = mayors2[-d1_supp_mayors$na.action,]$NAME_E)[[1]],
            starprep(d1_supp_CMs, clusters = councilmembers2[-d1_supp_CMs$na.action,]$NAME_E)[[1]],
            starprep(d2_supp_out, clusters = dat[-d2_supp_out$na.action,]$NAME_E)[[1]],
            starprep(d2_supp_mayors, clusters = mayors2[-d2_supp_mayors$na.action,]$NAME_E)[[1]],
            starprep(d2_supp_CMs, clusters = councilmembers2[-d2_supp_CMs$na.action,]$NAME_E)[[1]]
          ),
          omit.stat = c('f', 'ser', 'adj.rsq'),
          covariate.labels = c("Local Business Groups", "Faith-Based Orgs.", "Civic/Fraternal Orgs.", "Environemntal Groups", 
                               "Health Industry", "Large Individual Donors","LGBT Orgs.", "Local Democratic Comm.", 
                               "Local Republican Comm.", 'Newspapers', "Police/Firefighter Unions", "Teacher's Unions", 
                               "Other Unions", "State Officials", "Federal Officials", 'Real Estate Industry', "Women's Orgs."),
          dep.var.labels = c("D1 Ideal Point", "D2 Ideal Point"),
          column.labels = rep(c("All Officials", "Mayors", "Councilmembers"), 2),
          label = 'tab:support_regs',
          title = 'Constituency Support and and Ideological Placement by Office Type',
          out = "Table_D4.tex"
)


