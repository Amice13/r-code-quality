################################
# ANALYSIS: DESCRIPTIVES
################################

### Figure 1 in main text
#whole sample
cm_means <- aggregate(data[,c("dead")], by=list(data$year), FUN=mean, na.rm=T)
names(cm_means) <- c("Year","u1")
cm_means <- cm_means[cm_means$Year>1959,]
cm_means[1,]

q <- ggplot(data = cm_means, aes(x = Year, y = u1)) + geom_point(size=1.8) + theme_gray(base_size=18) + geom_smooth(colour="black",linetype=1, fill="grey35", method="loess") +
  scale_x_continuous(limits = c(1960, 2013), breaks = c(1960,1970,1980,1990,2000,2010))  +
  scale_y_continuous(limits = c(0, 20), breaks = c(5,10,15,20))  +
  labs(x = "Year", y = "Mean Infant Mortality Rate",
       #title = "Mean Annual Infant Mortality across all 22 Countries, 1960-2013",
       #subtitle = "Annual mean values, LOESS curve and 95% Confidence Interval",
       caption = "Data Source: Demographic and Health Surveys")
q
q + ggsave(paste0(fig.path,"Descriptive1_grey.pdf"),width=12,height=8.5)

### Figure 2 in main text
# prepare empty country-level datasets with nice names


countries <- unique(data[,c("cowcode"), drop = F])
countries$country <- countrycode(countries$cowcode,"cown","country.name")
countries$country <- as.character(countries$country)
row.names(countries) <- NULL
countries$country[countries$country == "Central African Republic"] <- "Centr. Afr. Rep."
countries$country[countries$country == "Congo - Kinshasa"] <- "Congo (DRC)"
countries$country[countries$country == "Côte d’Ivoire"] <- "Côte d'Ivoire"

# get country-level means and standard errors from our data
cdata <- ddply(data, "cowcode", summarise,
               N    = length(dead),
               mean = mean(dead, na.rm=T),
               sd   = sd(dead, na.rm=T),
               se   = sd / sqrt(N)
)

cdata <- join(cdata,countries,by="cowcode",type="left")

cdata$lb=cdata$mean - 1.96*cdata$se
cdata$ub=cdata$mean + 1.96*cdata$se

cdata <-cdata[order(-cdata$mean),]
cdata$order <- c(22:1)



# plot with ggplot2
p <- ggplot(cdata)
p + geom_point(size=2.7,aes(mean,-order)) + 
  geom_errorbarh(aes(x=mean,y=-order, xmin=lb, xmax=ub), size=1.2, height=0) +  
  geom_text(aes(x=lb, y=-order, label=country),hjust="right" ,nudge_x = -0.25, size=4.5) + 
  theme_grey(base_size=18) +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  scale_x_continuous(limits = c(3, 16)) + 
  labs(x = "Mean Infant Mortality per 100 Births (1960-2013)", y = "",
       #title = "Infant Mortality in 22 African Countries, 1960-2013",
       #subtitle = "All Countries in our Sample: Mean and 95% Confidence Interval",
       caption = "Data Source: Demographic and Health Surveys") 
# save 
p + ggsave(paste0(fig.path,"country_means.pdf"),width=12,height=8.5)




############# TABLE A1: SUMMARY STATISTICS
sample.data <- data[!is.na(data$dead) & !is.na(data$epr.incl.dist_l1) & !is.na(data$epr.incl.ind_l1),
                    c("dead","epr.incl.ind_l1","epr.incl.dist_l1","epr.senior.ind_l1","epr.senior.dist_l1","upgrade","downgrade",
                      "mother.educ.fac","mother.b.age",
                      "birthorder.num","female","twin_dummy",
                      "leadergroup.ind_l1","leadergroup.dist_l1","topgovpositions.ind_l1",
                      "topgovpositions.dist_l1","govpositions.ind_l1","govpositions.dist_l1",
                      "polity_iv_high_l1","vdem_polyarchy_index_high_l1","fptp_most_l1",
                      "prenatal_min_healthprof","prenatal_doctor","institutional.birth",
                      "assist_min_healthprof","assist_doctor","birthweight_low","year")]

names(sample.data)
sample.data$mother.educ.fac <-as.numeric(sample.data$mother.educ.fac)


fileConn<-file(paste0(tab.path,"sumstats.tex"))
writeLines(stargazer(sample.data, type = "latex",font.size = "scriptsize", title="Summary Statistics (DHS Data)",
                     covariate.labels=c("Infant Death", 
                                        "Government Co-Ethnic (t-1)", 
                                        "District Share Government Co-Ethnics (t-1)",
                                        "Senior Government Co-Ethnic (t-1)",
                                        "District Share Senior Government Co-Ethnics (t-1)",
                                        "Upgrade to Political Inclusion",
                                        "Downgrade to Political Exclusion",
                                        "Mother's education",
                                        "Mother's age",
                                        "Birthorder",
                                        "Female",
                                        "Twin or Higher Multiple Birth",
                                        "Co-Ethnic President (t-1)",
                                        "Dist. Share Co-Ethnics President (t-1)",
                                        "Co-Ethnic Top Minister (t-1)",
                                        "Dist. Share Co-Ethnics Top Min. (t-1)",
                                        "Co-Ethnic Any Minister (t-1)",
                                        "Dist. Share Co-Ethnics Any Min. (t-1)",
                                        "Polity IV $>$ Median (t-1)",
                                        "VDEM Polyarchy $>$ Median (t-1)",
                                        "FPTP Electoral System (t-1)",
                                        "Prenatal Assistance",
                                        "Prenatal Asst. by Doctor",
                                        "Institutional Birth",
                                        "Assisted Birth",
                                        "Birth assisted by Doctor",
                                        "Low Birthweight ($\\leq 2500$g)",
                                        "Year"),
                     column.sep.width = "-5pt",
                     label="summary_dhs", median = F, iqr = F, summary.stat = c("n","mean", "sd", "min", "max")), 
           fileConn)
close(fileConn)

?stargazer

## Clean up
rm(sample.data)







################################
# TABLE A2: Regressing Mothers Individual Co-Ethnicity with the government on district shares 

# define fixed effects and cluster variables
fe.spec3 <- c("| 0", "| cow.round", "| reg.round", "| dist.round") 
clust.var.b <- "| 0 | cow.round"

side.check <- list()
for(fe in fe.spec3){
  form.str <- paste("epr.incl.ind ~ epr.incl.dist", 
                    fe, clust.var.b)
  this.data <- data[,unlist(lapply(colnames(data), 
                                   function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                      !colnames(data) %in% c("age")] 
  this.data <- na.omit(this.data)    
  this.data$weights <- gen_weights(this.data$cowcode)
  m <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
  print(summary(m))
  side.check <- c(side.check, list(m))
}

## Output Table
mean(data$epr.incl.ind)

#mean(data$epr.incl.ind,na.rm=T)
m.list <- side.check

latex.country.fe <- function(entries){c("Country-Survey-Round FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.region.fe <- function(entries){c("Survey-Round-Region FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.district.fe <- function(entries){c("Survey-Round-District FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.notes.2 <-  "\\parbox[t]{.9\\textwidth}{\\textit{Notes:} OLS linear probability models.
The sample mean of the dependent variable is 0.583.   
Observations are weighted to ensure equal weights for each country.
Clustered standard errors in parentheses (country-survey-round).  
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}"



add.lines <- list(latex.country.fe(c("no","yes","--","--")), 
                  latex.region.fe(c("no","no","yes","--")),
                  latex.district.fe(c("no","no","no","yes")),
                  latex.controls(c("no","no","no","no")))
keep.lines <- which(grepl("epr.",rownames(m.list[[1]]$coefficients)))


fileConn<-file(paste0(tab.path,"SIDE_check.tex"))
writeLines(stargazer(m.list,
                     title="Regressing Mothers' Individual Co-Ethnicity on District Share",
                     keep=keep.lines,
                     multicolumn=F,# se = se,
                     covariate.labels=c("Share of Dist. Pop. Included"),
                     dep.var.caption = "Individual Government Co-Ethnicity",dep.var.labels = rep("",length(m.list)),
                     font.size = "scriptsize",
                     notes.align = "c",label="SIDE_check",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.2, notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)


