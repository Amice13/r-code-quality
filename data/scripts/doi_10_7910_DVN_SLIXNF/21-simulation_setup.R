##
## Simulation Study: Setup
##

n_obs    <- c(100, 250, 500, 1000)
v_type   <- c(1, 2, 3, 4, 5)  # 1 is small auto, 2 is large auto
trends   <- c(1, 2)  # 1 is PT, 2 is PTT
sim_grid <- expand.grid(n_obs, v_type, trends)
n_sim    <- 2000

# Fixed paramters
bias_term_base <- c(0.05, 0.1) # (PT and PTT)
tau_mean       <- 0.2
ep_sigma       <- 3 # 1
ep_sigma2      <- 0
e_auto         <- "AR1"
auto2          <- 0

time_length <- 5
auto_list   <- c(0, 0.2, 0.4, 0.6, 0.8)
id_try      <- nrow(sim_grid)

focus_trend  <- 2
focus_var    <- c(1)

# ## sim params
trends_vec <- c("PT", "PTT")