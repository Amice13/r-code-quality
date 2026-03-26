library("tidyverse")
theme_set(cowplot::theme_cowplot())
theme_update(plot.title = element_text(hjust = 0.5, size = 15),
             plot.subtitle = element_text(hjust = 0.5, size = 10))
library("patchwork")
library("here")

# Model diagram ---------------------------------------------------------------
# R version 4.2.3

# Length
n <- 100

# AR models
model_0 <- list(ar = .8)

model_1 <- list(ar = .8)
model_2 <- list(ar = .4)
model_3 <- list(ar = .8)

model_4 <- list(ar = .8)
model_5 <- list(ar = .8)
model_6 <- list(ar = .8)

# 3 Trajectories: y
set.seed(123) #123

y_0 <- arima.sim(model_0, n/2 + 1, sd = .5)

y_1 <- arima.sim(model_1, n/2) + sqrt(seq(0, 8, length.out = n/2)) + 2
y_2 <- arima.sim(model_2, n/2) - .6
y_3 <- arima.sim(model_3, n/2) - seq(0, 6, length.out = n/2) - 1

y_1[1] <- last(y_0)
y_2[1] <- last(y_0)
y_3[1] <- last(y_0)

# 3 Trajectories: y 
set.seed(332) #112

u_0 <- rep(arima.sim(model_1, n/2), each = 5) |> head(n/2 + 1) #rep(.2, n/2 + 1)

u_2 <- head(rep(arima.sim(model_4, n/2), each = 5), n/2)*.5 + 1
u_3 <- head(rep(arima.sim(model_5, n/2), each = 5), n/2)*1.2 - 2.5
u_1 <- head(rep(arima.sim(model_6, n/2), each = 5), n/2) + 2

df <- tibble(t = seq(-n/2, n/2, 1),
             t_pos = t > 0,
             y1 = c(y_0, y_1),
             y2 = c(y_0, y_2),
             y3 = c(y_0, y_3),
             u1 = c(u_0, u_1),
             u2 = c(u_0, u_2),
             u3 = c(u_0, u_3)) |> 
  mutate(across(contains("y"), \(x){ runmed(x, k = 3, endrule = "keep")}))

y_lims <- range(select(df, matches("[y][123]")))
u_lims <- range(select(df, matches("[u][123]")))

plot_y <- df |> 
  pivot_longer(contains("y")) |> 
  ggplot(aes(t, value, color = name, linetype = t_pos)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = .6) +
  scale_y_continuous(limits = y_lims) +
  labs(title = "Forecast System State: y", y = "", color = "") +
  guides(linetype = "none") +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 13))

plot_u <- df |> 
  pivot_longer(contains("u")) |> 
  ggplot(aes(t, value, color = name, linetype = t_pos)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = .6) +
  scale_y_continuous(limits = u_lims) +
  labs(title = "Candidate Control Signal: u", y = "", color = "") +
  guides(linetype = "none") +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 13))

plot_u + plot_y + 
  plot_annotation(title = "Sample Control Signals and Forecast Diagram")

ggsave(here("figures", "mpc-sample.png"), units = "cm",
       width = 25, height = 10)

