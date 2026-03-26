###########################################
# Ethnic favoritism: Afrobarometer
#
# Janina Beiser-McGrath, Carl Müller-Crepon & Yannick Pengl
#
# 
############################################
rm(list = ls())

# INIT #####################################
library(lfe)
library(stargazer)
library(plyr)
library(countrycode)

tab.path <- "tables/"
fig.path <- "figures/"

# FUNCTIONS ################################

# (function) Add line to latex table
latex.addline <- function(title,entries){c(title,paste0("\\multicolumn{1}{c}{",entries,"}"))}

# DATA

# Load data
load("data/ab_side.RData")
colnames(data)

# assign numeric cow country codes
data$cowcode <- countrycode(data$country,"country.name","cown")

# Drop individuals with bad geocoding
# http://docs.aiddata.org/ad4/pdfs/geocodingafrobarometer.pdf
# http://docs.aiddata.org/ad4/files/geocoding-methodology-updated-2017-06.pdf , page 9
data <- data[data$precision_code <= 3,]

# Coded dummy for individual inclusion
data$incl_lang <- ifelse(data$statusid_lang < 5, 1, 0)


# IDs

# ... country -rounds
data$cow.round <- as.numeric(as.factor(paste0(data$cowcode, ".", data$round)))


# ... GAUL District id
data$adm.id <- data$id.new
data$adm.id <- as.numeric(as.factor(data$adm.id))

# ... Language ids
data$lang.id <- as.numeric(as.factor(paste0(data$language, ".", data$country)))

# ... encode econ_employment to any employment
data$econ_employment <- ifelse(data$econ_employment == 0, 0, 1)

# Principal components for categories
categories <- c("econ_", "difficulty_")
names(categories) <- c("Economic hardship", "Perceived service accessibility")

## Get variables
cat.ls <- lapply(categories, function(c){colnames(data)[grepl(c, colnames(data))]})

## Drop econ variables not present in all rounds
cat.ls[[1]] <- cat.ls[[1]][!cat.ls[[1]]  %in% c("econ_remit", "econ_radio",
                                                "econ_tv","econ_motorveh")]

## Make labels for nice tables
cat.labels <- list()
cat.labels[[1]] <- c(paste0("How often gone without: ", "Food"),
                     paste0("--- ", c("Water", "Health Care", "Fuel","Income")), "Any employment")
cat.labels[[2]] <- c(paste0("Ease of accessing: ", "ID card"),
                     paste0("--- ", c("Primary school placement", "Household services", "Medical services","Police services")))


for(cat in c(1:length(categories))){
  
  # Estimate
  pca <- prcomp(na.omit(data[,cat.ls[[cat]]]),
                center = TRUE, 
                scale. = TRUE) 
  print(summary(pca))
  print(pca)
  
  # Save to data
  data[apply(data[,cat.ls[[cat]]], 1, function(x){!any(is.na(x))}), paste0(categories[cat], "pc")] <- 
    1 * pca$x[,1]
  
  #Save output to Latex

  # ... make data
  output <- data.frame(Component = paste0("Component ",c(1:length(cat.ls[[cat]]))),
                       Eigenvalue = round_any(pca$sdev^2, .01),
                       `Explained Variance` = round_any(summary(pca)$importance[2,], .01),
                       `Variable | Loadings` = paste0("",cat.labels[[cat]],""),
                       round_any(pca$rotation, .01),
                       stringsAsFactors = F)
  colnames(output) <- c("Component", "Eigenvalue", "Explained", "Variable", paste0("PC", c(1:length(cat.ls[[cat]]))))
  
  # ... header
  head <- c("\\begin{table}[!htbp] \\centering ",
            paste0("  \\caption{",paste("Principal component analysis:", names(categories)[cat]),"} "),
            paste0("  \\label{","afrobarometer_", categories[cat],"pca} ")," \\scriptsize " ,
            "\\begin{tabular}{@{\\extracolsep{0pt}} ",paste(rep("D{.}{.}{-2} ", 3), collapse = ""),
            paste(rep("D{.}{.}{-2} ", ncol(output)-3), collapse = "")," } ",
            "\\\\[-1.8ex]\\hline ",
            "\\hline \\\\[-1.8ex] ")
  
  # ... footer
  foot <- c("\\hline \\\\[-1.8ex] "    ,                                                                                                                                                                                                                                                                                                                                                                                                               
            "\\end{tabular} " ,                                                                                                                                                                                                                                                                                                                                                                                                                        
            "\\end{table} " )
  
  # ... column titles
  col.tit.1 <- paste(c(rep(" & ", 4), paste0("\\multicolumn{",length(cat.ls[[cat]]),"}{c}{Factor loadings}"),
                       "\\\\ \\cline{5-",4 + length(cat.ls[[cat]]),"} \\\\[-1ex] "), collapse = "")
  col.tit.2 <- paste0("\\multicolumn{1}{c}{",paste(c("","","Variance","", rep("", length(cat.ls[[cat]]))), collapse = "} & \\multicolumn{1}{c}{"),"} \\\\" ,
                      "")
  col.tit.3 <- paste0("\\multicolumn{1}{c}{",paste(colnames(output), collapse = "} & \\multicolumn{1}{c}{"),"} \\\\" ,
                      "\\hline \\\\[-1.8ex] ")
  
  # ... content
  allignment <- c("l", "c","c","l", rep("c", length(cat.ls[[cat]])))
  content <- paste0(paste(unlist(lapply(1:nrow(output),function(x){
    paste(c("\\multicolumn{1}{l}{",unlist(lapply(1:(ncol(output)), function(y){
      if(y != ncol(output)){
        paste0(output[x,y], "} & \\multicolumn{1}{",allignment[y+1],"}{")
      } else {
        paste0(output[x,y], "}")
      }
    })),""), collapse = "")
  })), collapse = " \\\\ \\\\[-1ex] "),"\\\\")
  
  
  # ... save
  fileConn<-file(paste0(tab.path,"afrobarometer_", categories[cat],"pca.tex"))
  writeLines(
    c(head, col.tit.1, col.tit.2, col.tit.3, content, foot), fileConn)
  close(fileConn)
}

# Covariates
data$female <- ifelse(data$sex == 1, 1, 0)
data$age <- as.numeric(data$age)
data$educ <- as.numeric(data$educ)


# ANALYSIS

# Specification

# ... components
main.expl <- c("incl_lang", "epr.incl.dist", "I(incl_lang*epr.incl.dist)")
main.cov <- c("female", "age", "I(age^2)", "urban","factor(educ)")

fe.main <- " factor(cow.round) "
se.cluster <- "lang.id + adm.id"

# ... labels
cov.labels <- c("Government Co-Ethnic (t-1)", "Dist. Share Gov. Co-Ethnics (t-1)",
                "Co-Ethnic $\\times$ Dist. Share Co-Ethnics  (t-1)")
latex.notes <- "\\parbox[t]{.95\\textwidth}{\\textit{Notes:} OLS linear models. 
Control variables include 4 levels of education, age and age squared, as well as a female dummy. 
Two-way clustered standard errors in parentheses (language group and district clusters).
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}"


# ... helper function to combine
make_form <- function(dv, expl, fe, iv = "0", se = "0" ){
  as.formula(paste(dv, "~", paste(expl, collapse = "+"), "|",
                   fe, "|", iv, "|", se))
}



# Summary Statistics ########################

# ... select variables
sum.vars <- c(main.expl, main.cov)[ c(main.expl, main.cov) %in% colnames(data)]
sum.vars <- c(sum.vars, "educ", 
              "econ_pc","econ_food","econ_water","econ_health","econ_fuel" , "econ_income" , "econ_employment",
              "difficulty_pc"  , "difficulty_id"  ,  "difficulty_school",  "difficulty_hhservice", "difficulty_medical", "difficulty_police")

sum.var.labels <- c("Government Co-Ethnic (t-1)", "Dist. Share Gov. Co-Ethnics (t-1)",
                    "Female", "Age", "Urban", "Education", 
                    "Economic hardship (principal component)",
                    paste0("How often gone without: ", "Food"),
                    paste0("--- ", c("Water", "Health Care", "Fuel","Income")), "Any employment",
                    "Service access (principal component)",
                    paste0("Ease of accessing: ", "ID card"),
                    paste0("--- ", c("Primary school placement", "Household services", "Medical services","Police services")))

fileConn<-file(paste0(tab.path,"afrobarometer_sumstats.tex"))
writeLines(stargazer(data[,sum.vars],
                     title="Afrobarometer: Summary statistics",
                     covariate.labels = sum.var.labels,
                     font.size = "scriptsize",
                     notes.align = "l",label= "afrobarometer_sumstats",align =T,
                     digits = 2, digit.separate = 0), 
           fileConn)
close(fileConn)


# Table 3: Economic hardship and difficulty to access public services ######################

# ... estimate
outcomes <- c("difficulty_pc", "difficulty_medical", "econ_pc", "econ_health")
model.ls <- lapply(outcomes, function(dv){
  felm(make_form(dv, c(main.expl, main.cov), fe.main, "0", se.cluster),
       data = data)
})


# ... prepare output
add.lines <- list(latex.addline("Individual-level covariates:", rep("yes",length(model.ls))),
                  latex.addline("Country-survey fixed effects:", rep("yes",length(model.ls))))
dep.var.labels.1 = c("PC", "Medical", "PC", 
                     "Medical")
dep.var.labels.2 = c("","services (1-4)"  , "", 
                     "treatment (0-4)")

# ... save
fileConn<-file(paste0(tab.path,"afrobarometer_main.tex"))
writeLines(stargazer(model.ls,
                     title="Economic hardship and public services: Cross-sectional OLS",
                     keep = main.expl,
                     multicolumn = F,# se = se,
                     column.labels = c("Ease of public service access",paste0( "Economic hardship",
                                                                               "}\\\\  \\cmidrule(lr{.75em}){2-3}  \\cmidrule(lr{.75em}){4-5}  \n  & \\multicolumn{1}{c}{", 
                                                                               paste(dep.var.labels.1, collapse = "} & \\multicolumn{1}{c}{"),
                                                                               "}\\\\ & \\multicolumn{1}{c}{", 
                                                                               paste(dep.var.labels.2, collapse = "} & \\multicolumn{1}{c}{"))),
                     column.separate = c(2,2), column.sep.width ="0pt",
                     dep.var.caption = "",dep.var.labels = rep("", length(model.ls)),
                     covariate.labels=cov.labels,
                     font.size = "scriptsize",
                     notes.align = "c",label="afrobarometer_main",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes, notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)

# Table A19: Economic hardship - disaggregated ############################

# ... estimate
outcomes <- c("econ_food","econ_water","econ_health","econ_fuel" , "econ_income" , "econ_employment")
model.ls <- lapply(outcomes, function(dv){
  felm(make_form(dv, c(main.expl, main.cov), fe.main, "0", se.cluster),
       data = data)
})


# ... prepare output
add.lines <- list(latex.addline("Individual-level covariates:", rep("yes",length(model.ls))),
                  latex.addline("Country-survey fixed effects:", rep("yes",length(model.ls))))
dep.var.labels = c("Food", "Water", "Medical treat.", "Fuel","Income", "")

# ... save
fileConn<-file(paste0(tab.path,"afrobarometer_hardship.tex"))
writeLines(stargazer(model.ls,
                     title="Economic hardship indicators: Cross-sectional OLS",
                     keep = main.expl,
                     multicolumn = F,# se = se,
                     column.labels = c("How often have you gone without (0--4):",paste0("Employment",
                                                                                        "}\\\\ \\cmidrule(lr{1.5em}){2-6} \\cmidrule(lr{1.5em}){7-7}  \n  & \\multicolumn{1}{c}{", 
                                                                                        paste(dep.var.labels, collapse = "} & \\multicolumn{1}{c}{"))),
                     column.separate = c(5,1),column.sep.width ="-15pt",
                     dep.var.caption = "",dep.var.labels = rep("", length(model.ls)),
                     covariate.labels=cov.labels,
                     font.size = "scriptsize",
                     notes.align = "c",label="afrobarometer_hardship",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes, notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)

# Table A20: Service Access Difficulty disaggregated #####

# ... estimate
outcomes <- c("difficulty_id"  ,  "difficulty_school",  "difficulty_hhservice", "difficulty_medical", "difficulty_police")
model.ls <- lapply(outcomes, function(dv){
  felm(make_form(dv, c(main.expl, main.cov), fe.main, "0", se.cluster),
       data = data)
})


# ... prepare output
add.lines <- list(latex.addline("Individual-level covariates:", rep("yes",length(model.ls))),
                  latex.addline("Country-survey fixed effects:", rep("yes",length(model.ls))))
dep.var.labels.1 = c("ID card", "Prim. school", "Household", "Medical","Police")
dep.var.labels.2 = c("", "placement", "services", "services","")

# ... save
fileConn<-file(paste0(tab.path,"afrobarometer_services.tex"))
writeLines(stargazer(model.ls,
                     title="Ease of accessing services: Cross-sectional OLS",
                     keep = main.expl,column.sep.width ="-6pt",
                     multicolumn = F,# se = se,
                     column.labels = c(paste0("Ease to access public serivces (1--4):",
                                              "}\\\\ \\cmidrule(lr{.75em}){2-6}   \n  & \\multicolumn{1}{c}{", 
                                              paste(dep.var.labels.1, collapse = "} & \\multicolumn{1}{c}{"),
                                              "}\\\\ \n  & \\multicolumn{1}{c}{", 
                                              paste(dep.var.labels.2, collapse = "} & \\multicolumn{1}{c}{"))),
                     column.separate = c(5),
                     dep.var.caption = "",dep.var.labels = rep("", length(model.ls)),
                     covariate.labels=cov.labels,
                     font.size = "scriptsize",
                     notes.align = "c",label="afrobarometer_services",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes, notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)



############################################
# END
###########################################