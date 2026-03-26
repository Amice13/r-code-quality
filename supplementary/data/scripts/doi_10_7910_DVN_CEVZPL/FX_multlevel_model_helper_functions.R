# Function to carry out the tasks of processing fitted models contained in a list produced by 
#   run.multlev.model()

#anova.from.list()
#make.anova.list()
#make.anova.table()
#label.anova.table()
#compile.everything()

# These functions are called by a wrapper function
#   process.ML.model.wrapper()
# Which is saved in Fx_multlevel_model_wrapper_for_helper_functions.R

#function to run anova() on models contained in  list
#  this is called by make.anova.list()
anova.from.list <- function(model.list, m.nested, m2){
  anova.out <- anova(model.list[[m2]], model.list[[m.nested]])
  row.names(anova.out) <- c(m.nested,m2)
  return(anova.out)
}




#FUNCTION make.anova.list()
#function to run all of the anova and put them into a list
#  this function calls anova.from.list(), defined above
#  ACHTUNG: this hard coded so that only works 
#  ACHTUNG: for models that use MONTH!
#  ACHTUNG: different for latitudinal migrants
make.anova.list <- function(model.list){
  anova9.6v5a   <- anova.from.list(model.list = model.list, "m6.drp.yrXtrt", "m5a.drop.trt")
  anova8.6v5b   <- anova.from.list(model.list = model.list, "m6.drp.yrXtrt", "m5b.drop.mo")
  anova7.v6v5c  <- anova.from.list(model.list = model.list, "m6.drp.yrXtrt", "m5c.drop.yr")
  
  anova6.7av6   <- anova.from.list(model.list = model.list, "m7a.best",     "m6.drp.yrXtrt")
  #anova5.7av7c  <- anova.from.listmodel.list = model.list, "m7a.best",     "m7c.drop.trt")
  #anova4.7av7b  <- anova.from.list(model.list = model.list, "m7a.best",     "m7b.drop.mo")
  anova3.8v7a   <- anova.from.list(model.list = model.list, "m8.drop.moXyr","m7a.best")
  anova2.9v8    <- anova.from.list(model.list = model.list, "m9.all.2.term","m8.drop.moXyr")
  anova1.10v9   <- anova.from.list(model.list = model.list, "m10.FULL","m9.all.2.term")
  
  anova.list <- list(anova9.6v5a = anova9.6v5a,
                     anova8.6v5b = anova8.6v5b,
                     anova7.v6v5c = anova7.v6v5c,
                     anova6.7av6 = anova6.7av6,
                     anova3.8v7a = anova3.8v7a,
                     anova3.8v7a = anova3.8v7a,
                     anova2.9v8 = anova2.9v8,
                     anova1.10v9 = anova1.10v9)
  
  return(anova.list)
}





#function to compile anova objects from a list
#  ACHTUNG: this HARD CODED so that only works 
#  ACHTUNG: for models that use MONTH!
make.anova.table <- function(anova.list){
  anova.tab <- rbind(anova.extract(anova.list$anova9.6v5a)
                     ,anova.extract(anova.list$anova8.6v5b)
                     ,anova.extract(anova.list$anova7.v6v5c)
                     ,anova.extract(anova.list$anova6.7av6)
                     #,anova.extract(anova5.7av7c)
                     #,anova.extract(anova4.7av7b)
                     ,anova.extract(anova.list$anova3.8v7a)
                     ,anova.extract(anova.list$anova2.9v8)
                     ,anova.extract(anova.list$anova1.10v9))
  anova.tab <- data.frame(anova.tab)
  
  return(anova.tab)
}




#function to label an anova table
label.anova.table <- function(anova.tab, trait.name = trait.label){
  label.yearXtrait <- paste("Year*",trait.name,sep = "")
  label.monthXtrait <- paste("Month*",trait.name,sep = "")
  label.monthXyearXtrait <- paste("Month*Year*",trait.name,sep = "")
  
  tests <- c(trait.name
             ,"Month"
             ,"Year"
             ,label.yearXtrait
             #,"Trait"
             #,"Month"
             ,label.monthXtrait
             ,"Month*Year"
             ,label.monthXyearXtrait)
  
  df <- data.frame(tests, anova.tab)
  
  return(df)
}



#function to compile output of all previous function

#   compile everything into a list
compile.everything <- function(trait.name =trait.column.name,
                               anova.tab2,model.list,anova.list,df){
  list(trait = trait.name,
       anova.table = anova.tab2,
       when = date(),
       all.models = model.list,
       all.anova = anova.list)
}



