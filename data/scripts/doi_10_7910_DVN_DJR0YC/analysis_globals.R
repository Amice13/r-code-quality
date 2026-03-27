################################
# ANALYSIS: GLOBALS
################################


# Generate Weights function ####
gen_weights <- function(vec){
  tab <- table(vec)
  weights <- data.frame(cbind(weight = as.numeric(tab), unit = as.character(names(tab))),
                        stringsAsFactors = F)
  weight.vec <- 1/as.numeric(join(data.frame(unit = as.character(vec), stringsAsFactors = F),
                                  weights, type = "left", by = "unit")[,2])
  return(weight.vec)
  
}


### Define fixed effects specifications for main models (based on EPR ethnicity data)
fe.spec1 <- c("| factor(dist.round) + factor(ethnic.id) + factor(birth.reg.year)", # Survey-District & Survey-Ethnic & Survey-Region-Birthyear FE
              "| factor(dist.round) + factor(birth.ethnic.year) + factor(birth.reg.year)", # Survey-District & Survey-Ethnic-Birthyear FE
              "| factor(ethnic.id) + factor(birth.dist.year)", #Survey-Ethnic & Survey-District-Birthyear FE
              "| factor(birth.ethnic.year) + factor(birth.dist.year)") # Survey-Ethnic-Birthyear FE & Survey-District-Birthyear FE

### Define fixed effects specifications for main models (based on FRT ethnicity data)
fe.spec.frt <- c("| factor(dist.round) + factor(ethnic.id.frt) + factor(birth.reg.year)", # Survey-District & Survey-Ethnic & Survey-Region-Birthyear FE
                 "| factor(dist.round) + factor(birth.ethnic.year.frt) + factor(birth.reg.year)", # Survey-District & Survey-Ethnic-Birthyear FE
                 "| factor(ethnic.id.frt) + factor(birth.dist.year)", #Survey-Ethnic & Survey-District-Birthyear FE
                 "| factor(birth.ethnic.year.frt) + factor(birth.dist.year)") # Survey-Ethnic-Birthyear FE & Survey-District-Birthyear FE

### Two-way clustering on ethnic groups and districts within survey rounds
clust.var.a <- "| 0 | ethnic.id + dist.round" 
clust.var.frt <- "| 0 | ethnic.id.frt + dist.round"


### define control variables
controls.cm <- c("birthorder.num","birthorder.num.sq","female",
                 "twin_dummy", "mother.b.age", "mother.b.age.sq")
controls.str <- paste(controls.cm, collapse = " + ")


### Prepare Latex entries for nice regression tables and specify folders to save tables and figures
tab.path <- "tables/"
fig.path <- "figures/"


latex.controls <- function(entries){c("Controls",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.ethnic.id.fe <- function(entries){c("Survey-Ethnic FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.dist.fe <- function(entries){c("Survey-District FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.pts.fe <- function(entries){c("Survey-Cluster FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.pts.year.fe <- function(entries){c("Survey-Cluster-Birthyear FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.reg.year.fe <- function(entries){c("Survey-Region-Birthyear FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.ethnic.year.fe <- function(entries){c("Survey-Ethnic-Birthyear FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.ethnic.tt <- function(entries){c("Survey-Ethnic Time Trend",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.dist.year.fe <- function(entries){c("Survey-District-Birthyear FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.year.fe <- function(entries){c("Birthyear FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.cow.cl <- function(entries){c("Country-Clustered SE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.two.cl <- function(entries){c("Two-Way Clustered SE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.clusters <- function(entries){c("SE Clustering",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.uoa <- function(entries){c("Unit of Analysis",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.hh.fe <- function(entries){c("Household FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.mother.fe <- function(entries){c("Mother FE",paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.dv.means <- function(entries){c("Sample Mean DV",paste0("\\multicolumn{1}{c}{$",entries,"$}"))}

describe(data$dead)
latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textit{Notes:} OLS linear probability models. 
The sample mean of the dependent variable is 10.78 infant deaths per 100 live births.   
Observations are weighted to ensure equal weights for each country.
Control variables include mothers' age and age squared as well as infants' sex, a twin dummy, 
birth rank, and birth rank squared.
Two-way clustered standard errors in parentheses (survey-ethnic group and survey-district clusters).
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}

frt.sample <-unique(data$cowcode[!is.na(data$topgovpositions.dist)])
describe(data$dead[data$cowcode%in%frt.sample])

latex.notes.long.frt <- "\\parbox[t]{.95\\textwidth}{\\textit{Notes:} OLS linear probability models. 
The sample mean of the dependent variable is 10.04 infant deaths per 100 live births.   
Observations are weighted to ensure equal weights for each country.
Control variables include mothers' age and age squared as well as infants' sex, a twin dummy, 
birth rank, and birth rank squared.
Two-way clustered standard errors in parentheses (survey-ethnic group and survey-district clusters).
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}"


