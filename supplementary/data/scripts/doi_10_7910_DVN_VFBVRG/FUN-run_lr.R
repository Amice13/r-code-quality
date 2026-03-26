# FUNCTION TO RUN ANOVA, FORMAT RESULTS
run_lr <-  function(m1, # RATING SCALE MODEL
                    m2 # PARTIAL CREDIT MODEL
){
  anova(m1, m2) %>% unclass()  %>% tbl_df()
}
