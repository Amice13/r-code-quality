
library(ggplot2) # Figure 1 was created using ggplot 2 version 2.2.1 and cowplot version 0.9.1
library(cowplot)
library(dplyr)

# Read data from your directory
dat <- read.csv("C:/PATH/.../data.csv")
dat2 <- read.csv("C:/PATH/.../socsp.csv")

# Aggregate cantonal social expenditure as well as the coefficient of variation for log-normal distributed data

dat2 <- dat2 %>% filter(year>1929 & year<2001) %>% group_by(year) %>% summarize(total = sum(exp(socsppc)), cvar = sqrt(exp(sd(socsppc))-1))


p1 <- ggplot(data = dat2, aes(x=year, y=total))+geom_line(color = "black")+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "Social Spending of the Swiss Cantons 1930-2000", 
                                             x = "Year",
                                             y = "Social Spending per capita")
p2 <- ggplot(data = dat2, aes(x=year, y=cvar))+geom_line(color = "black")+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "Convergence of Social Spending of the Swiss Cantons 1930-2000", 
                                             x = "Year",
                                             y = "Coefficient of Variation")

plot <- plot_grid(p1,p2, nrow = 2, align = "v")

# Save figure in your directory
ggsave("C:/PATH/.../devsoc.eps", height=5, width=10, units='in',device=cairo_ps)