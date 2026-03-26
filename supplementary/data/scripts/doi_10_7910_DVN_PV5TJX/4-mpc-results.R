library("tidyverse")
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5, size = 15),
             plot.subtitle = element_text(hjust = 0.5, size = 10),
             legend.text = element_text(size = 10),
             legend.position = "bottom",
             strip.placement = "outside",
             strip.background = element_blank())
library("here")
library("lubridate")
library("forecast")
library("patchwork")

# Load Data -------------------------------------------------------------------
df_mpc <- read_csv(here("mpc-exp.csv"), col_types = "fffTf")

# Sequence length for each experiment (Hours)
df_mpc |> 
  filter(drain_valve == 0) |> 
  group_by(sequence, experiment) |> 
  summarize(hour = max(hr)-min(hr)) |> 
  pivot_wider(names_from = experiment, names_prefix = "exp_",
              values_from = hour) |> 
  view()

# Prepare data for modeling ---------------------------------------------------

# Separate sequences, remove drain phase & startup,
# remove data to make observations single second instead of quarter second.
n_trim <- 240

prep_data <- function(df) {
  df |> 
    filter(drain_valve == 0) |> 
    group_split(sequence) |> 
    map(\(x){
      x |> 
        slice(-c(1:n_trim))
    })
}

# NOTE: here we change the order of experiments to be inline with the paper

df_1 <- prep_data(filter(df_mpc, experiment == 1)) # No param updates
df_2 <- prep_data(filter(df_mpc, experiment == 2)) # param updates + no system excitation
df_3 <- prep_data(filter(df_mpc, experiment == 3)) # param updates + system excitation
df_4 <- prep_data(filter(df_mpc, experiment == 4)) # param updates + system excitation + additional constraints

# Calculate Forecasts ---------------------------------------------------------

# For experiments 2, 3, and 4 parameters were updated every sequence. Thus,
# we will refit our model and make forecasts based on the control signal.
# For experiment 1 we will estimate the parameters on sequence 1 and then
# forecast based on the control signal for all other sequences.

# Model type: 
# c_r: bilinear
# p_f: bilinear
# p_r: quadratic

make_forecast <- function(df_list, param_update = TRUE) {
  n_seq <- length(df_list)
  
  # Fit Models
  fit_cr <- df_list |> 
    map(\(x) {
      lm(c_r ~ lag(c_r)*lag(f_p), data = x)
    })
  
  fit_pf <- df_list |> 
    map(\(x) {
      lm(p_f ~ lag(f_p) + lag(c_r), data = x)
    })
  
  fit_pr <- df_list |> 
    map(\(x) {
      lm(p_r ~ lag(f_r) + I(lag(f_r)^2) + 0, data = x)
    })
  
  # Adjust if no parameter updates
  if(param_update == FALSE) {
    fit_cr <- rep(fit_cr[1], n_seq)
    fit_pf <- rep(fit_pf[1], n_seq)
    fit_pr <- rep(fit_pr[1], n_seq)
  }
  
  # Make forecasts based on fitted model and control signal
  pred_cr <- 
    map2(df_list[-1], fit_cr[-n_seq],
         \(x, m) {
           y <- x$c_r
           a <- m$coefficients
           
           for(i in 1:(nrow(x)-1)) {
             y[i+1] = a[1] + a[2]*y[i] + a[3]*x$u_fp[i] + a[4]*y[i]*x$u_fp[i]
           }
           
           y
         })
  
  pred_pf <- 
    pmap(list(df_list[-1], pred_cr, fit_pf[-n_seq]),
         \(x, y, m) {
           predict(m, mutate(x, c_r = y, f_p = u_fp)) |> as.numeric()
         })
  
  pred_pr <- 
    map2(df_list[-1], fit_pr[-n_seq], 
         \(x, m) {
           predict(m, mutate(x, f_r = u_fr)) |> as.numeric()
         })
  
  # Make new df with forecasts
  df_pred <-
    pmap_dfr(list(df_list[-1], pred_cr, pred_pf, pred_pr),
             \(x, pred_cr, pred_pf, pred_pr) {
               mutate(x,
                      .pred_cr = pred_cr,
                      .pred_pf = pred_pf,
                      .pred_pr = pred_pr)
             }) |> 
    filter(!is.na(.pred_pf))
}

df_pred_1 <- make_forecast(df_1, param_update = FALSE)
df_pred_2 <- make_forecast(df_2)
df_pred_3 <- make_forecast(df_3)
df_pred_4 <- make_forecast(df_4)

df_pred <- bind_rows(df_pred_1,
                     df_pred_2,
                     df_pred_3,
                     df_pred_4)

# Visualizations --------------------------------------------------------------

# Plot specifications
model_colors <- c("Observed" = "black",
                  "Forecast" = "red")

model_linetype <- c("Observed" = "solid",
                    "Forecast" = "dashed")

y_lims <- map2(c("c_r", "p_f", "p_r"),
               c(".pred_cr", ".pred_pf", ".pred_pr"),
               \(obs, pred) {
                 range(c(pull(df_pred, obs), pull(df_pred, pred)), na.rm = TRUE)
               }) |> 
  set_names(c("c_r", "p_f", "p_r"))
  

# Plot helper functions
mpc_plot <- function(df, n_exp, variable) {
  
  # Adjust to variable
  df <- df |> 
    mutate(Observed = pull(df, variable),
           Forecast = pull(df, paste0(".pred_", str_replace(variable, "_", ""))))
  
  # Set y_label
  if(variable == "c_r")      y_label <- "Conductivity (mS/cm)"
  else if(variable == "p_f") y_label <- "FPPC (kW)"
  else if(variable == "p_r") y_label <- "RPPC (kW)"
  else                       y_label <- ""
  
  # Set y_breaks
  if(variable == "c_r")      y_breaks <- seq(5, 25, 5)
  else if(variable == "p_f") y_breaks <- seq(.4, 1.2, .2)
  else if(variable == "p_r") y_breaks <- seq(.5, 1, .1)
  else                       y_breaks <- NULL
  
  # Set experiment label
  if(n_exp == 1)      exp_lab <- "Experiment 1: No Parameter Updates, System Excitation"
  else if(n_exp == 2) exp_lab <- "Experiment 2: Parameter Updates, No System Excitation"
  else if(n_exp == 3) exp_lab <- "Experiment 3: Parameter Updates, System Excitation"
  else                exp_lab <- "Experiment 4: Parameter Updates, System Excitation, Additional Constraints"
  
  # Determine time of salt change
  if(n_exp == 1)      salt_change_seq <- 13
  else if(n_exp == 2) salt_change_seq <- NA
  else if(n_exp == 3) salt_change_seq <- 7
  else                salt_change_seq <- 10
  
  change_time <- filter(df, sequence == salt_change_seq)$hr |> last()
  
  # Set x breaks
  x_breaks <- seq(0, max(df$hr), 3)
  
  # Draw plot
  df |>
    ggplot() +
    geom_line(aes(hr, Observed, group = sequence)) +
    geom_line(aes(hr, Forecast, group = sequence), color = "red", linetype = "dashed") +
    geom_vline(xintercept = change_time, linetype = "dotdash", linewidth = 1.5, color = "blue") +
    scale_y_continuous(breaks = y_breaks, limits = y_lims[[variable]]) + 
    scale_x_continuous(breaks = x_breaks, limits = c(0, max(df$hr))) +
    scale_color_manual(values = model_colors) +
    #guides(color = "none") +
    labs(x = "Time (Hours)", y = y_label,
         color = "", linetype = "",
         subtitle = exp_lab)
}

width <- 50*.8
height <- 15*.8

# c_r: Forecast 
df_annotate <- df_pred_4 |> 
  group_by(sequence) |> 
  summarise(x = mean(hr), y = 10)

mpc_plot(df_pred_1, 1, "c_r") +
  mpc_plot(df_pred_2, 2, "c_r") +
  mpc_plot(df_pred_3, 3, "c_r") +
  mpc_plot(df_pred_4, 4, "c_r") +
  annotate("text", x = df_annotate$x, y = df_annotate$y, label = df_annotate$sequence) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "MPC Forecast: Reject Conductivity")

ggsave(here("figures", "mpc-exp-cond.png"), units = "cm",
       width = width, height = height)

# p_f: Forecast
mpc_plot(df_pred_1, 1, "p_f") +
  mpc_plot(df_pred_2, 2, "p_f") +
  mpc_plot(df_pred_3, 3, "p_f") +
  mpc_plot(df_pred_4, 4, "p_f") +
  plot_layout(guides = "collect") +
  plot_annotation(title = "MPC Forecast: Feed Pump Power Consumption")

ggsave(here("figures", "mpc-exp-fppc.png"), units = "cm",
       width = width, height = height)

# p_r: Forecast
df_annotate <- df_pred_2 |> 
  group_by(sequence) |> 
  summarise(x = mean(hr), y = max(p_r) + .02)

mpc_plot(df_pred_1, 1, "p_r") +
  {mpc_plot(df_pred_2, 2, "p_r") +
  annotate("text", x = df_annotate$x, y = df_annotate$y, label = df_annotate$sequence)} +
  mpc_plot(df_pred_3, 3, "p_r") +
  mpc_plot(df_pred_4, 4, "p_r") +
  plot_layout(guides = "collect") +
  plot_annotation(title = "MPC Forecast: Recirculation Pump Power Consumption")

ggsave(here("figures", "mpc-exp-rppc.png"), units = "cm",
       width = width, height = height)
