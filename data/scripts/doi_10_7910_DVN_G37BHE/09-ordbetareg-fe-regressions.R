# load data (saved in stata after running all data preparation commands there)
data <- haven::read_dta("replicationdata_prepped.dta")


## set parameters for ordered beta regression
## delta and treedepth not in use
delta <- 0.9
treedepth <- 12
ncores <- 6
nchains <- 4
niter <- 2000
nthreads <- 5


## set variables

## controls
# gl ictrls_2l_varying iwdi_pop_2l iwdi_oda_2l iwdi_gdppc_2l iunhcr_ref_idp_2l iwdi_literacy_2l iwdi_fuel_2l
ictrls <- c("ictrls_2l_varying", "iwdi_pop_2l", "iwdi_oda_2l", "iwdi_gdppc_2l", "iunhcr_ref_idp_2l", "iwdi_literacy_2l", "iwdi_fuel_2l")
# gl ictrls_3l_varying iwdi_pop_3l iwdi_oda_3l iwdi_gdppc_3l iunhcr_ref_idp_3l iwdi_literacy_3l iwdi_fuel_3l
ictrls_3l_varying <- c("iwdi_pop_3l", "iwdi_oda_3l", "iwdi_gdppc_3l", "iunhcr_ref_idp_3l", "iwdi_literacy_3l", "iwdi_fuel_3l")
# gl ictrls_4l_varying iwdi_pop_4l iwdi_oda_4l iwdi_gdppc_4l iunhcr_ref_idp_4l iwdi_literacy_4l iwdi_fuel_4l
ictrls_4l_varying <- c("iwdi_pop_4l", "iwdi_oda_4l", "iwdi_gdppc_4l", "iunhcr_ref_idp_4l", "iwdi_literacy_4l", "iwdi_fuel_4l")
# gl ictrls_5l_varying iwdi_pop_5l iwdi_oda_5l iwdi_gdppc_5l iunhcr_ref_idp_5l iwdi_literacy_5l iwdi_fuel_5l
ictrls_5l_varying <- c("iwdi_pop_5l", "iwdi_oda_5l", "iwdi_gdppc_5l", "iunhcr_ref_idp_5l", "iwdi_literacy_5l", "iwdi_fuel_5l")

## dependent variable
dv <- "v2x_polyarchy"
## fe
xtset <- c("country")


## replicate table 2
# indvar_separate_ctrls v2x_polyarchy, indvar(ipema_any_demo_assist_dum_2l) title(table2)
iv <- "ipema_any_demo_assist_dum_2l"
mod2.1 <- paste(dv, paste0(c(iv, ictrls_3l_varying), collapse = " + "), sep = " ~ ")
datafilter <- c(dv, xtset, iv, ictrls_3l_varying)

tab2.1 <- ordbetareg::ordbetareg(as.formula(paste0(mod2.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)
saveRDS(tab2.1, "rds/tab2.1.rds")
summary(tab2.1)

data2.2 <- data %>% filter(ucdp_0yrs == 1)
tab2.2 <- ordbetareg::ordbetareg(as.formula(paste0(mod2.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data2.2[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)
saveRDS(tab2.2, "rds/tab2.2.rds")
summary(tab2.2)

data2.3 <- data %>% filter(ucdp_1yrs == 1)
tab2.3 <- ordbetareg::ordbetareg(as.formula(paste0(mod2.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data2.3[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab2.3, "rds/tab2.3.rds")
#summary(tab2.3)

data2.4 <- data %>% filter(ucdp_2yrs == 1)
tab2.4 <- ordbetareg::ordbetareg(as.formula(paste0(mod2.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data2.4[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab2.4, "rds/tab2.4.rds")
#summary(tab2.4)

data2.5 <- data %>% filter(ucdp_3yrs == 1)
tab2.5 <- ordbetareg::ordbetareg(as.formula(paste0(mod2.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data2.5[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab2.5, "rds/tab2.5.rds")
#summary(tab2.5)

# tab2.1 <- readRDS("rds/tab2.1.rds")
# tab2.2 <- readRDS("rds/tab2.2.rds")
# tab2.3 <- readRDS("rds/tab2.3.rds")
# tab2.4 <- readRDS("rds/tab2.4.rds")
# tab2.5 <- readRDS("rds/tab2.5.rds")

# function to create texreg table output without \usepackage{rotating} line
create_texreg_table <- function(models, file_path, ...) {

  texreg::texreg(models, file = file_path, sideways = TRUE, ...)
  
  tex_content <- readLines(file_path)
  
  tex_content <- tex_content[!grepl("\\usepackage\\{rotating\\}", tex_content)]
  
  writeLines(tex_content, file_path)
}

models <- list(tab2.1, tab2.2, tab2.3, tab2.4, tab2.5)
file_path <- "tables/table2.obg.tex"

create_texreg_table(models, file_path, 
                    omit.coef = "(iwdi)|(iunhcr)|(as.factor)|(Intercept)",
                    custom.coef.names = "Democracy mandate",
                    caption = "Table 2 replication (ordered beta regression country fixed effects - scaled variables)",
                    label = "tab:tab2obg",
                    digits = 3)



## replicate table 4
# indvar_separate_ctrls v2x_polyarchy, indvar(itotal_compound_K_2l) title(table4)
iv <- "itotal_compound_K_2l"
mod4.1 <- paste(dv, paste0(c(iv, ictrls_3l_varying), collapse = " + "), sep = " ~ ")
datafilter <- c(dv, xtset, iv, ictrls_3l_varying)

tab4.1 <- ordbetareg::ordbetareg(as.formula(paste0(mod4.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab4.1, "rds/tab4.1.rds")
#summary(tab4.1)

data4.2 <- data %>% filter(ucdp_0yrs == 1)
tab4.2 <- ordbetareg::ordbetareg(as.formula(paste0(mod4.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data4.2[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab4.2, "rds/tab4.2.rds")
#summary(tab4.2)

data4.3 <- data %>% filter(ucdp_1yrs == 1)
tab4.3 <- ordbetareg::ordbetareg(as.formula(paste0(mod4.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data4.3[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab4.3, "rds/tab4.3.rds")
#summary(tab4.3)

data4.4 <- data %>% filter(ucdp_2yrs == 1)
tab4.4 <- ordbetareg::ordbetareg(as.formula(paste0(mod4.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data4.4[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab4.4, "rds/tab4.4.rds")
#summary(tab4.4)

data4.5 <- data %>% filter(ucdp_3yrs == 1)
tab4.5 <- ordbetareg::ordbetareg(as.formula(paste0(mod4.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data4.5[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab4.5, "rds/tab4.5.rds")
#summary(tab4.5)

# tab4.1 <- readRDS("rds/tab4.1.rds")
# tab4.2 <- readRDS("rds/tab4.2.rds")
# tab4.3 <- readRDS("rds/tab4.3.rds")
# tab4.4 <- readRDS("rds/tab4.4.rds")
# tab4.5 <- readRDS("rds/tab4.5.rds")

models <- list(tab4.1, tab4.2, tab4.3, tab4.4, tab4.5)
file_path <- "tables/table4.obg.tex"

create_texreg_table(models, file_path, 
                    omit.coef = "(iwdi)|(iunhcr)|(as.factor)|(Intercept)",
                    custom.coef.names = "\\# of uniformed personnel",
                    caption = "Table 4 replication (ordered beta regression country fixed effects - scaled variables)",
                    label = "tab:tab4obg",
                    digits = 3)



## replicate table 5
# indvar_separate_ctrls v2x_polyarchy, indvar(iactual_civilian_total_K_2l) title(table5)
iv <- "iactual_civilian_total_K_2l"
mod5.1 <- paste(dv, paste0(c(iv, ictrls_3l_varying), collapse = " + "), sep = " ~ ")
datafilter <- c(dv, xtset, iv, ictrls_3l_varying)

tab5.1 <- ordbetareg::ordbetareg(as.formula(paste0(mod5.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab5.1, "rds/tab5.1.rds")
#summary(tab5.1)

data5.2 <- data %>% filter(ucdp_0yrs == 1)
tab5.2 <- ordbetareg::ordbetareg(as.formula(paste0(mod5.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data5.2[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab5.2, "rds/tab5.2.rds")
#summary(tab5.2)

data5.3 <- data %>% filter(ucdp_1yrs == 1)
tab5.3 <- ordbetareg::ordbetareg(as.formula(paste0(mod5.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data5.3[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab5.3, "rds/tab5.3.rds")
#summary(tab5.3)

data5.4 <- data %>% filter(ucdp_2yrs == 1)
tab5.4 <- ordbetareg::ordbetareg(as.formula(paste0(mod5.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data5.4[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta, max_treedepth = 15),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab5.4, "rds/tab5.4.rds")
#summary(tab5.4)

data5.5 <- data %>% filter(ucdp_3yrs == 1)
tab5.5 <- ordbetareg::ordbetareg(as.formula(paste0(mod5.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data5.5[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab5.5, "rds/tab5.5.rds")
#summary(tab5.5)

# tab5.1 <- readRDS("rds/tab5.1.rds")
# tab5.2 <- readRDS("rds/tab5.2.rds")
# tab5.3 <- readRDS("rds/tab5.3.rds")
# tab5.4 <- readRDS("rds/tab5.4.rds")
# tab5.5 <- readRDS("rds/tab5.5.rds")

models <- list(tab5.1, tab5.2, tab5.3, tab5.4, tab5.5)
file_path <- "tables/table5.obg.tex"

create_texreg_table(models, file_path, 
                    omit.coef = "(iwdi)|(iunhcr)|(as.factor)|(Intercept)",
                    custom.coef.names = "\\# of civilian personnel",
                    caption = "Table 5 replication (ordered beta regression country fixed effects - scaled variables)",
                    label = "tab:tab5obg",
                    digits = 3)



## replicate table 6
# indvar_separate_ctrls v2x_polyarchy, indvar(iany_demo_all_max_dum_2l) title(table6)
iv <- "iany_demo_all_max_dum_2l"
mod6.1 <- paste(dv, paste0(c(iv, ictrls_3l_varying), collapse = " + "), sep = " ~ ")
datafilter <- c(dv, xtset, iv, ictrls_3l_varying)

tab6.1 <- ordbetareg::ordbetareg(as.formula(paste0(mod6.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab6.1, "rds/tab6.1.rds")
#summary(tab6.1)

data6.2 <- data %>% filter(ucdp_0yrs == 1)
tab6.2 <- ordbetareg::ordbetareg(as.formula(paste0(mod6.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data6.2[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab6.2, "rds/tab6.2.rds")
#summary(tab6.2)

data6.3 <- data %>% filter(ucdp_1yrs == 1)
tab6.3 <- ordbetareg::ordbetareg(as.formula(paste0(mod6.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data6.3[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab6.3, "rds/tab6.3.rds")
#summary(tab6.3)

data6.4 <- data %>% filter(ucdp_2yrs == 1)
tab6.4 <- ordbetareg::ordbetareg(as.formula(paste0(mod6.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data6.4[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab6.4, "rds/tab6.4.rds")
#summary(tab6.4)

data6.5 <- data %>% filter(ucdp_3yrs == 1)
tab6.5 <- ordbetareg::ordbetareg(as.formula(paste0(mod6.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data6.5[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab6.5, "rds/tab6.5.rds")
#summary(tab6.5)

# tab6.1 <- readRDS("rds/tab6.1.rds")
# tab6.2 <- readRDS("rds/tab6.2.rds")
# tab6.3 <- readRDS("rds/tab6.3.rds")
# tab6.4 <- readRDS("rds/tab6.4.rds")
# tab6.5 <- readRDS("rds/tab6.5.rds")

models <- list(tab6.1, tab6.2, tab6.3, tab6.4, tab6.5)
file_path <- "tables/table6.obg.tex"

create_texreg_table(models, file_path, 
                    omit.coef = "(iwdi)|(iunhcr)|(as.factor)|(Intercept)",
                    custom.coef.names = "Any dem. activities",
                    caption = "Table 6 replication (ordered beta regression country fixed effects - scaled variables)",
                    label = "tab:tab6obg",
                    digits = 3)



## replicate table 8
# indvar_together_ctrls v2x_polyarchy, indvar(iany_demo_engage_max_dum_2l iany_demo_bypass_max_dum_2l) title(table8)
iv <- c("iany_demo_engage_max_dum_2l", "iany_demo_bypass_max_dum_2l")
mod8.1 <- paste(dv, paste0(c(iv, ictrls_3l_varying), collapse = " + "), sep = " ~ ")
datafilter <- c(dv, xtset, iv, ictrls_3l_varying)

tab8.1 <- ordbetareg::ordbetareg(as.formula(paste0(mod8.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab8.1, "rds/tab8.1.rds")
#summary(tab8.1)

data8.2 <- data %>% filter(ucdp_0yrs == 1)
tab8.2 <- ordbetareg::ordbetareg(as.formula(paste0(mod8.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data8.2[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab8.2, "rds/tab8.2.rds")
#summary(tab8.2)

data8.3 <- data %>% filter(ucdp_1yrs == 1)
tab8.3 <- ordbetareg::ordbetareg(as.formula(paste0(mod8.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data8.3[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab8.3, "rds/tab8.3.rds")
#summary(tab8.3)

data8.4 <- data %>% filter(ucdp_2yrs == 1)
tab8.4 <- ordbetareg::ordbetareg(as.formula(paste0(mod8.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data8.4[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab8.4, "rds/tab8.4.rds")
#summary(tab8.4)

data8.5 <- data %>% filter(ucdp_3yrs == 1)
tab8.5 <- ordbetareg::ordbetareg(as.formula(paste0(mod8.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data8.5[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab8.5, "rds/tab8.5.rds")
#summary(tab8.5)

# tab8.1 <- readRDS("rds/tab8.1.rds")
# tab8.2 <- readRDS("rds/tab8.2.rds")
# tab8.3 <- readRDS("rds/tab8.3.rds")
# tab8.4 <- readRDS("rds/tab8.4.rds")
# tab8.5 <- readRDS("rds/tab8.5.rds")

models <- list(tab8.1, tab8.2, tab8.3, tab8.4, tab8.5)
file_path <- "tables/table8.obg.tex"

create_texreg_table(models, file_path, 
                    omit.coef = "(iwdi)|(iunhcr)|(as.factor)|(Intercept)",
                    custom.coef.names = c("Any dem. eng. with host state",
                                          "Any dem. byp. of host state"),
                    caption = "Table 8 replication (ordered beta regression country fixed effects - scaled variables)",
                    label = "tab:tab8obg",
                    digits = 3)



## replicate table 9
# indvar_together_ctrls v2x_polyarchy, indvar(idemo_all_max_dum_2l ielections_all_max_dum_2l iparties_all_max_dum_2l ivoters_all_max_dum_2l) title(table9)
iv <- c("idemo_all_max_dum_2l", "ielections_all_max_dum_2l", "iparties_all_max_dum_2l", "ivoters_all_max_dum_2l")
mod9.1 <- paste(dv, paste0(c(iv, ictrls_3l_varying), collapse = " + "), sep = " ~ ")
datafilter <- c(dv, xtset, iv, ictrls_3l_varying)

tab9.1 <- ordbetareg::ordbetareg(as.formula(paste0(mod9.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab9.1, "rds/tab9.1.rds")
#summary(tab9.1)

data9.2 <- data %>% filter(ucdp_0yrs == 1)
tab9.2 <- ordbetareg::ordbetareg(as.formula(paste0(mod9.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data9.2[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab9.2, "rds/tab9.2.rds")
#summary(tab9.2)

data9.3 <- data %>% filter(ucdp_1yrs == 1)
tab9.3 <- ordbetareg::ordbetareg(as.formula(paste0(mod9.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data9.3[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab9.3, "rds/tab9.3.rds")
#summary(tab9.3)

data9.4 <- data %>% filter(ucdp_2yrs == 1)
tab9.4 <- ordbetareg::ordbetareg(as.formula(paste0(mod9.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data9.4[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab9.4, "rds/tab9.4.rds")
#summary(tab9.4)

data9.5 <- data %>% filter(ucdp_3yrs == 1)
tab9.5 <- ordbetareg::ordbetareg(as.formula(paste0(mod9.1, paste(" + as.factor(", xtset, ")"))),
                                 data = na.omit(data9.5[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                                 # control = list(adapt_delta = delta),
                                 # control = list(max_treedepth = treedepth),
                                 cores = ncores, chains = nchains, iter = niter,
                                 ## the following two lines for parallel processing within chains
                                 threads = nthreads,
                                 ## error when using cmdstanr backend
                                 # Compiling Stan program...
                                 # Error in `process_initialize(self, private, command, args, stdin, stdout, ...`:
                                 #   ! Native call to `processx_exec` failed
                                 # Caused by error in `chain_call(c_processx_exec, command, c(command, args), pty, pty_options, ...`:
                                 #   ! Command 'make' not found @win/processx.c:982 (processx_exec)
                                 # backend = "cmdstanr",
                                 backend = "rstan",
                                 refresh = 0)

saveRDS(tab9.5, "rds/tab9.5.rds")
#summary(tab9.5)

# tab9.1 <- readRDS("rds/tab9.1.rds")
# tab9.2 <- readRDS("rds/tab9.2.rds")
# tab9.3 <- readRDS("rds/tab9.3.rds")
# tab9.4 <- readRDS("rds/tab9.4.rds")
# tab9.5 <- readRDS("rds/tab9.5.rds")

models <- list(tab9.1, tab9.2, tab9.3, tab9.4, tab9.5)
file_path <- "tables/table9.obg.tex"

create_texreg_table(models, file_path, 
                    omit.coef = "(iwdi)|(iunhcr)|(as.factor)|(Intercept)",
                    custom.coef.names = c("Any dem. institution act's",
                                          "Any election act's",
                                          "Any pol. party act's",
                                          "Any voter act's"),
                    caption = "Table 9 replication (ordered beta regression country fixed effects - scaled variables)",
                    label = "tab:tab9obg",
                    digits = 3)

