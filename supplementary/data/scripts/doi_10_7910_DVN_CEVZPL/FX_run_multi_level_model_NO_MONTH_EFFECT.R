# Function to run all of the models

# This model takes two arguements
#  data
#  name of the focal trait for model
# 
# Functions 1st runs "null" intercept only model
# Then up dates the  model for all relevant intractions
#
# Out puts a a list of all of the models
# This list is then processed by a set of functions
# currently saved in Fx_multlevel_model_helper_functions
# (which includes a wrapper for all of the functions)


#ACTHUNG: MONTH EFFECT REMOVED 
#ACHTUNG: SO THAT JANUARY DATA 
#ACHTUNG: CAN BE RUN SEPERATE

run.multlev.model.NO.MONTH <- function(df,trait.name){
  
  #rename focal covariate as "trait"
  j.trait <- which(names(df) == trait.name)
  
  names(df)[j.trait] <- "trait"
  
  
  #Run "null" model
  print("running null")
  m1.null <- glmer(N ~   1 +
                     #spp-level intercept
                     #(1|name) +    
                     
                     #slopes for each spp w/in each moth
                     (year.cent|name) + 
                     
                     #intercept for each year
                     (1|year)  +    

                     #ind level raned for pois-norm to reduce overdisp
                     (1|i),                   
                   family = poisson,
                   data = df)
  
  #Run models
  print("m5a.drop.trt")
  m5a.drop.trt   <- update(m1.null, . ~ .   +          # + month 
                             + year.cent                      )#trait
  
 # print("m5b.drop.mo")
 # m5b.drop.mo    <- update(m1.null, . ~ .   + trait +       + year.cent                      )#month
  
  print("m5c.drop.yr")
  m5c.drop.yr    <- update(m1.null, . ~ .   + trait 
                           #+ month 
                           )#year
  
  print("m6.drp.yrXtrt")
  m6.drp.yrXtrt  <- update(m1.null, . ~ .   + 
                             trait + 
                             #month + 
                             year.cent                      )#trait*year.cent
  #m7a.very.best       <- update(m1.null, . ~ .   + trait + year.cent + trait*year.cent)#trait*month 
  #m7c.drop.trt   <- update(m1.null, . ~ .   +             month + year.cent + trait*year.cent)#trait
  #m7b.drop.mo    <- update(m1.null, . ~ .   + trait +       + year.cent + trait*year.cent)#month
  
  
  print("m7a.best")
  m7a.best       <- update(m1.null, . ~ .   + trait + 
                             #month + year.cent + 
                             trait*year.cent)#trait*month 
  
  #print("m8.drop.moXyr")
  #m8.drop.moXyr  <- update(m1.null, . ~ .   + trait*month               + trait*year.cent)#month*year.cent
  
  #print("m9.all.2.term")
  #m9.all.2.term  <- update(m1.null, . ~ .   + trait*month               + trait*year.cent + month*year.cent)#3x
  
  #print("m10.FULL")
  #m10.FULL       <- update(m1.null, . ~ .   + trait*month*year.cent)
  
  
  #compile models into list
  model.list <- list(m5a.drop.trt  = m5a.drop.trt,
                   #  m5b.drop.mo   = m5b.drop.mo,
                     m5c.drop.yr   = m5c.drop.yr,
                     m6.drp.yrXtrt = m6.drp.yrXtrt,
                     m7a.best      = m7a.best #,
                    # m8.drop.moXyr = m8.drop.moXyr,
                    # m9.all.2.term = m9.all.2.term,
                    #m10.FULL      = m10.FULL
                    )
  
  return(model.list)
  }