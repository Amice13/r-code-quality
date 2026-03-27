#######
#######
####### Replication Data for: The Differential Impact of the Hong Kong National Security Law 
####### on Political Sensitivity Bias in Opinion Polls.
####### This file produces the figures seen in Appendix B.
####### Last Updated: May. 2023
#######
#######

rm(list=ls())
need <- c("ggplot2", "dplyr", "grid") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages



###############Figure B1##############################################
# Generate a sequence of numbers from -4 to 4 with increments of 0.1
x <- seq(-4, 4, by = 0.1)
y <- dnorm(x, mean = 0, sd = 1)
df <- data.frame(x = x, y = y)

# Create the plot using ggplot
ggplot(data = df, aes(x = x, y = y)) +
  geom_line() +
  scale_x_continuous(breaks = seq(-4, 4, by = 1), limits = c(-4, 4)) +
  scale_y_continuous(limits = c(0, 0.4)) +
  labs(x = "Public Opinion", y = "PDF", title = "Normal Distribution, μ=0, σ=1, γ=0.5") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
geom_vline(xintercept = 0.5, linetype = "solid", color = "red") +
  annotate("text", x = 2, y = 0.07, label = "x", hjust = 0)+
  annotate("text", x = 0.6, y = 0.02, label = "γ", color = "red", hjust = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue")+
  annotate("text", x = 4, y = 0.03, label = "Pro-Beijing", hjust = 1) +
  annotate("text", x = -4, y = 0.03, label = "Anti-Beijing", hjust = 0) +
geom_segment(aes(x = 0, y = 0.02, xend = 0.5, yend = 0.02), linetype = "dashed", color = "blue")+
  annotate("text", x = 0.04, y = 0.03, label = "(μ-γ)", color = "blue", hjust = 0)

#################Figure B2#####################
x2 <- seq(-4, 4, by = 0.1)
y2 <- dnorm(x2, mean = 1, sd = 1)
df2 <- data.frame(x = x2, y = y2)
ggplot() +
  geom_line(data = df, aes(x = x, y = y), color="black") +
  geom_line(data = df2, aes(x = x, y = y), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, by = 1), limits = c(-4, 4)) +
  scale_y_continuous(limits = c(0, 0.4)) +
  labs(x = "Public Opinion", y = "PDF", title = "Normal Distribution, μ=0→1, σ=1, γ=0.5") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0.5, linetype = "solid", color = "red") +
  annotate("text", x = 2, y = 0.07, label = "x1", hjust = 0)+
  annotate("text", x = 3, y = 0.07, label = "x2", hjust = 0, color="blue")+
  annotate("text", x = 0.6, y = 0.1, label = "γ", color = "red", hjust = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue")+
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue")+
  annotate("text", x = 4, y = 0.03, label = "Pro-Beijing", hjust = 1) +
  annotate("text", x = -4, y = 0.03, label = "Anti-Beijing", hjust = 0) +
  geom_segment(aes(x = 0, y = 0.02, xend = 1, yend = 0.02), linetype = "dashed", color = "blue")+
  annotate("text", x = 0.3, y = 0.03, label = "△(μ-γ)", color = "blue", hjust = 0)

##################Figure B3#########################
x3 <- seq(-2, 6, by = 0.1)
y3 <- dnorm(x3, mean = 2, sd = 1)
df3 <- data.frame(x = x3, y = y3)
x4 <- seq(-2, 6, by = 0.1)
y4 <- dnorm(x3, mean = 3, sd = 1)
df4 <- data.frame(x = x4, y = y4)
ggplot() +
  geom_line(data = df3, aes(x = x, y = y), color="black") +
  geom_line(data = df4, aes(x = x, y = y), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, by = 1), limits = c(-2, 6)) +
  scale_y_continuous(limits = c(0, 0.4)) +
  labs(x = "Public Opinion", y = "PDF", title = "Normal Distribution, μ=2→3, σ=1, γ=0.5") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0.5, linetype = "solid", color = "red") +
  annotate("text", x = 4, y = 0.07, label = "x1", hjust = 0)+
  annotate("text", x = 5, y = 0.07, label = "x2", hjust = 0, color="blue")+
  annotate("text", x = 0.6, y = 0.08, label = "γ", color = "red", hjust = 0) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "blue")+
  geom_vline(xintercept = 3, linetype = "dashed", color = "blue")+
  annotate("text", x = 6, y = 0.03, label = "Pro-Beijing", hjust = 1) +
  annotate("text", x = -2, y = 0.03, label = "Anti-Beijing", hjust = 0) +
  geom_segment(aes(x = 2, y = 0.02, xend = 3, yend = 0.02), linetype = "dashed", color = "blue")+
  annotate("text", x = 2.3, y = 0.03, label = "△(μ-γ)", color = "blue", hjust = 0)

##################Figure B4#########################
x2 <- seq(-3, 5, by = 0.1)
y2 <- dnorm(x2, mean = 1, sd = 1)
df2 <- data.frame(x = x2, y = y2)
x2s <- seq(-3, 5, by = 0.1)
y2s <- dnorm(x2s, mean = 1, sd = 0.5)
df2s <- data.frame(x = x2s, y = y2s)
ggplot() +
  geom_line(data = df2, aes(x = x, y = y), color="black") +
  geom_line(data = df2s, aes(x = x, y = y), color = "blue") +
  scale_x_continuous(breaks = seq(-3, 5, by = 1), limits = c(-3, 5)) +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(x = "Public Opinion", y = "PDF", title = "Normal Distribution, μ=1, σ=1→0.5, γ=0.5") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0.5, linetype = "solid", color = "red") +
  annotate("text", x = 3, y = 0.07, label = "x1", hjust = 0)+
  annotate("text", x = 2.3, y = 0.07, label = "x2", hjust = 0, color="blue")+
  annotate("text", x = 0.6, y = 0.07, label = "γ", color = "red", hjust = 0) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue")+
  annotate("text", x = 4, y = 0.03, label = "Pro-Beijing", hjust = 1) +
  annotate("text", x = -4, y = 0.03, label = "Anti-Beijing", hjust = 0)

#################Figure B5############################
x4 <- seq(-2, 6, by = 0.1)
y4 <- dnorm(x3, mean = 3, sd = 1)
df4 <- data.frame(x = x4, y = y4)
x4s <- seq(-2, 6, by = 0.1)
y4s <- dnorm(x3, mean = 3, sd = 0.5)
df4s <- data.frame(x = x4s, y = y4s)
ggplot() +
  geom_line(data = df4, aes(x = x, y = y), color = "black") +
  geom_line(data = df4s, aes(x = x, y = y), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, by = 1), limits = c(-2, 6)) +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(x = "Public Opinion", y = "PDF", title = "Normal Distribution, μ=3, σ=1→0.5, γ=0.5") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0.5, linetype = "solid", color = "red") +
  annotate("text", x = 5, y = 0.07, label = "x1", hjust = 0)+
  annotate("text", x = 4.3, y = 0.07, label = "x2", hjust = 0, color="blue")+
  annotate("text", x = 0.6, y = 0.08, label = "γ", color = "red", hjust = 0) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "blue")+
  annotate("text", x = 6, y = 0.03, label = "Pro-Beijing", hjust = 1) +
  annotate("text", x = -2, y = 0.03, label = "Anti-Beijing", hjust = 0)