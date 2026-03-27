
#run everything
process.ML.model.wrapper <- function(model.list,        # list of fitted models from run.multlev.model()
                                     trait.column.name, # name of column actualy used for iftting models
                                     trait.label){      # "pretty" name to use for table labels
  
  #do anovas on nested modeld from model.list and put the anovas into list
  anova.list <- make.anova.list(model.list)
  
  #turn raw anovas into table of important infol
  anova.tab <- make.anova.table(anova.list)
  
  #label the anova table with the trait.label
  anova.tab2 <- label.anova.table(anova.tab, trait = trait.label)
  
  #compile everything
  everything <- compile.everything(trait.column.name,
                                   anova.tab2,
                                   model.list,
                                   anova.list)
  
  return(everything)
  
}

print("Fx_multlevel_model_wrapper_for_helper_functions.R loads process.ML.model.wrapper()")
