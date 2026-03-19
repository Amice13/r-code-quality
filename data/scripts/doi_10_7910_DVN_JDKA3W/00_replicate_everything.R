rm(list = ls())

### define path to where the replication folder is stored
replication.path <- "~/Desktop/replication_io"

### set working directory to replication folder
setwd(replication.path)


#### load libraries
library(fixest) #v0.11.1
library(car) #v3.1-2
library(here) #v1.0.1
library(dplyr) #v1.1.3
library(ggplot2) #v3.4.3



##### define regression helper function for fixest models
mdlr.conflict <- function(dv=NULL,x=NULL,cntr=NULL,fe=NULL,model="ols",iv=NULL,dat=NULL){
  lhs <- paste(dv,"~")
  rhs <- paste(paste(x,collapse=" + "),"+",paste(cntr,collapse=" + "))
  if(is.null(fe) & is.null(iv)){
    form <- as.formula(paste(lhs,rhs))
  } else{ 
    if (!is.null(fe) & is.null(iv)){
      form <- as.formula(paste(lhs,rhs, "|",paste(fe,collapse=" + ")))
    } else{
      if (is.null(fe) & !is.null(iv)){
        form <- as.formula(paste(lhs,rhs, "|",iv))
      } else{
        if (is.null(fe) & !is.null(iv)){
          form <- as.formula(paste(lhs,rhs, "|",iv))
        } else{
          form <- as.formula(paste(lhs,rhs, "|",paste(fe,collapse=" + "),"|",iv))
        }
      }
    }
  }
  if(model=="ols"){
    m.out <- feols(form,data=dat)
  } else{
    m.out <- feglm(form,data=dat,family="binomial",glm.iter=50,fixef.rm="perfect")
  }
  return(m.out)
}


### set output folder
tab.path <- "output/"


### load segment data
segments.df <- readRDS("data/df_analysis_segments.rds")
segments.df.first <- readRDS("data/df_analysis_segments_first.rds")
segments.df.max <- readRDS("data/df_analysis_segments_max.rds")

### load dyad data
dyad.df <- readRDS("data/df_analysis_dyads.rds")
dyad.df.first <- readRDS("data/df_analysis_dyads_first.rds")
dyad.df.max <- readRDS("data/df_analysis_dyads_max.rds")


###### source individual analysis scripts
scripts <- list.files("scripts",full.names = T)
# subset to everything apart from the currently open one 
scripts <- scripts[!grepl("00_",scripts)]
# loop through all
for(script in scripts){
  source(script)
  print(paste(script, "completed."))
}


### clear workspace & quit R session
rm(list = ls())

q(save="no",status=0,runLast = T)


