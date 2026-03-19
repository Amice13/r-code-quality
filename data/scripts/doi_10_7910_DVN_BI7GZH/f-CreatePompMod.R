#######################################################################################################
# Create pomp object for the age-structured model of pertussis transmission, with vaccination and maternal immunization
# Time is measured in YEARS, all rates are PER YEAR
#######################################################################################################

CreatePompMod <- function(nages_mod = 76L, 
                          nages_cmat = 15L,
                          add_preschool_booster = F, 
                          dat_df,
                          covars_df,
                          dt = 0.0027, #0.0027=1 day 
                          debug_bool = F) {
  # Args: 
  # nages_mod: no of age groups in the model (integer)
  # nages_cmat: no of age groups in contact matrix data (integer) 
  # add_preschool_booster: should a preschool booster be included? (boolean)
  # dat_df: data, with simulation times (data frame)
  # covars_df: seasonal covariates, with simulation times (data frame)
  # dt: time step (in years) for stochastic model (currently not used, numeric)
  # debug_bool: should messages be displayed to help debug? (boolean)
  
  # Extract C code from file
  mod_code <- readLines("c-model-equations.c")
  components_nm <- c("globs", "dmeas", "rmeas", "skel", "rsim") #add rsim for the stochastic version
  components_l <- vector(mode = 'list', length = length(components_nm))
  names(components_l) <- components_nm
  
  for(nm in components_nm) {
    components_l[[nm]] <- mod_code[str_which(mod_code, paste0("start_", nm)):str_which(mod_code, paste0("end_", nm))] %>% 
      str_flatten(collapse = "\n")
    
    if(nm == "globs") {
      components_l[[nm]] <- paste(components_l[[nm]], 
                                  sprintf("static int nages_mod = %d;\nstatic int nages_cmat = %d;\nstatic int add_preschool_booster = %d;\nstatic int debug = %d;", 
                                          nages_mod, 
                                          nages_cmat, 
                                          as.integer(add_preschool_booster),
                                          as.integer(debug_bool)), 
                                  sep = "\n")
    }
    components_l[[nm]] <- Csnippet(text = components_l[[nm]])
  }
  
  po <- pomp(data = dat_df,
             times = "time",
             t0 = min(dat_df$time),
             covar = covariate_table(covars_df, times = "time"), 
             obsnames = paste0("CIobs_", 1:nages_mod), 
             statenames = c(
               paste0("VM_", 1:nages_mod),
               paste0("VMbar_", 1:nages_mod),
               paste0("V_", 1:nages_mod),
               paste0("M_", 1:nages_mod),
               paste0("MV_", 1:nages_mod),
               paste0("SMbar_", 1:nages_mod),
               paste0("S_", 1:nages_mod),
               paste0("E_", 1:nages_mod),
               paste0("I_", 1:nages_mod),
               paste0("SV_", 1:nages_mod),
               paste0("SVMbar_", 1:nages_mod),
               paste0("SVM_", 1:nages_mod),
               paste0("R_", 1:nages_mod),
               paste0("CI1_", 1:nages_mod),
               paste0("CI2_", 1:nages_mod),
               paste0("CI3_", 1:nages_mod),
               paste0("CI4_", 1:nages_mod),
               paste0("CI5_", 1:nages_mod) 
             ),
             accumvars = c(
               paste0("CI1_", 1:nages_mod),
               paste0("CI2_", 1:nages_mod),
               paste0("CI3_", 1:nages_mod),
               paste0("CI4_", 1:nages_mod),
               paste0("CI5_", 1:nages_mod)
             ),
             paramnames = c(
               paste0("CM_", 1:(nages_cmat * nages_cmat)), # Contact matrix
               paste0("q_", 1:nages_mod), # Age-specific susceptibility
               paste0("N_", 1:nages_mod), # Age-specific population sizes
               paste0("delta_", 1:nages_mod), # Age-specific aging rates
               paste0("rho_", 1:nages_mod), # Age-specific reporting probabilities
               "tV", # Start time of routine vaccination
               "v1", # Vaccine coverage for primary course of vaccination
               "v2", # Vaccine coverage for booster vaccines doses
               "epsilon", # Initial vaccine effectiveness
               "alphaV", # 1 / average duration of vaccine protection
               "tM", # Start time of maternal immunization
               "v0", # Maternal immunization coverage
               "epsilonM", # Effectiveness of maternal Ab
               "tau", # 1 / average duration of maternal Ab
               "b1", # Blunting effect of maternal Ab on initial vaccine effectiveness
               "b2", # Blunting effect of maternal Ab on duration of vaccine protection
               "sigma", # 1 / average latent period
               "gamma", # 1 / average infectious period
               #"theta", # Relative average transmissibility of post-vaccine infections
               "iota", # Imported no of cases
               "mu", # Birth rate
               "N_tot", # Total population size
               #"eta", # Relative reporting probability of post-vaccine infections
               "kC", # Case reporting over-dispersion, 
               as.character(sapply(paste0(c("VM", "VMbar", "V", "M", "MV","SMbar", "S", "E", "I", "SV", "SVMbar","SVM","R", "CI1", "CI2", "CI3", "CI4", "CI5"), "_"), 
                                   paste0, 1:nages_mod, ".0")) #this should be okay
             ), 
             params = c(
               setNames(object = rep(0, nages_cmat * nages_cmat), nm = paste0("CM_", 1:(nages_cmat * nages_cmat))),
               setNames(object = rep(0, nages_mod), paste0("q_", 1:nages_mod)),
               setNames(object = rep(0, nages_mod), paste0("N_", 1:nages_mod)),
               setNames(object = rep(0, nages_mod), paste0("delta_", 1:nages_mod)),
               setNames(object = rep(0, nages_mod), paste0("rho_", 1:nages_mod)),
               "tV" = 0,
               "v1" = 0.90, 
               "v2" = 0.90, 
               "epsilon" = 0.95,
               "alphaV" = 0, 
               "tM" = 0, # Start time of maternal immunization
               "v0" = 0, # Maternal immunization coverage
               "epsilonM" = 0, # Effectiveness of maternal Ab
               "tau" = log(2) / (180 / 365), # 1 / average duration of maternal Ab
               "b1" = 0, # Blunting effect of maternal Ab on initial vaccine effectiveness
               "b2" = 0, # Blunting effect of maternal Ab on duration of vaccine protection
               "sigma" = 365 / 8, # 1 / average latent period
               "gamma" = 365 / 15, # 1 / average infectious period
               #"theta" = 0, # Relative average transmissibility of post-vaccine infections
               "iota" = 1, # Imported no of cases
               "mu" = 1 / 75, # Birth rate
               "N_tot" = 1e7, # Total population size
               #"eta" = 0, # Relative reporting probability of post-vaccine infections
               "kC" = 1e-8,  # Case reporting over-dispersion
               setNames(object = rep(0, nages_mod * 18), 
                        nm = as.character(sapply(paste0(c("VM", "VMbar", "V", "M", "MV","SMbar", "S", "E", "I", "SV", "SVMbar","SVM","R", "CI1", "CI2", "CI3", "CI4", "CI5"), "_"), 
                                                 paste0, 1:nages_mod, ".0"))) 
             ),
             globals = components_l[["globs"]],
             dmeasure = components_l[["dmeas"]],
             rmeasure = components_l[["rmeas"]],
             skeleton = vectorfield(components_l[["skel"]]),
             rprocess = euler(step.fun = components_l[["rsim"]], delta.t = dt), #add this for the stochastic version; euler approximation see Pej book p201
             partrans = parameter_trans(logit = c("b1", "b2")),
             rinit = NULL
             # cdir = "_help/", 
             # cfile = "codes"
  )
  return(po)
}


