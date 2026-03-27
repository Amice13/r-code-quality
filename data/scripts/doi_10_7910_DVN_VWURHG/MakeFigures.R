#########
# Figure 1 in the main manuscript
#########

library("ggplot2")
eq1 = function(x){10*(x/10)^1}
eq2 = function(x){10*(x/10)^0.1}
eq6 = function(x){10*(x/10)^0.2} 
eq7 = function(x){10*(x/10)^5}
eq3 = function(x){10*(x/10)^10}
eq4 = function(x){10*(x/10)^100}
eq5 = function(x){10*(x/10)^0.01}

Para.Plot <- ggplot(data.frame(x = c(0, 10)), aes(x=x)) + stat_function(fun = eq1, aes(colour = " 1")) +
  stat_function(fun = eq4, aes(colour = "100")) +
  stat_function(fun = eq5, aes(colour = " 0.01")) +
  stat_function(fun = eq6, aes(colour = " 0.2")) +
  stat_function(fun = eq7, aes(colour = " 5")) +
  stat_function(fun = eq2, aes(colour = " 0.1")) +
  stat_function(fun = eq3, aes(colour = "10")) + 
  theme_bw() +
  labs(x = "Y", y = "T(Y)") + 
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  scale_y_continuous(breaks=seq(0, 10, 5)) +
  scale_x_continuous(breaks=seq(0, 10, 5)) +
  scale_colour_manual("Sigma", values = c("Dark gray", "black", "gray", "Light gray", "gray", "black", "Dark gray")) 
Para.Plot
ggsave("/results/Figure_1.png", scale = 1, width = 4.8, height = 3.5)

##############
# Summary Simulation Figure
# Aghion et al. (2016)
#############

library("ggplot2")
linear = function(x){10*(x/10)^1}
bound1 = function(x){10*(x/10)^0.1}
bound2 = function(x){10*(x/10)^10}

# Prediction One
sign1a = function(x){10*(x/10)^.12}
sign1b = function(x){10*(x/10)^10}
inf1a = function(x){10*(x/10)^0.78}
inf1b = function(x){10*(x/10)^10}

x <- seq(0, 10, 1)
sign.ymax <- sign1a(x)
sign.ymin <- sign1b(x)
inf.ymax <- inf1a(x)
inf.ymin <- inf1b(x)
df <- data.frame(x, sign.ymin, sign.ymax, inf.ymin, inf.ymax)

Summary.Plot1 <- ggplot(data.frame(x = c(0, 10)), aes(x=x)) + 
  geom_ribbon(aes(x=x, ymin=sign.ymin, ymax=sign.ymax), data = df, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=x, ymin=inf.ymin, ymax=inf.ymax), data = df, fill = "black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Prediction 1: Job Turnover", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = linear, aes(colour = "1")) +
  stat_function(fun = bound1, aes(colour = "0.1")) +
  stat_function(fun = bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 10, 5)) +
  scale_x_continuous(breaks=seq(0, 10, 5)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot1

# Prediction Two (Job Creation)
sign2a.c = function(x){10*(x/10)^0.1}
sign2b.c = function(x){10*(x/10)^10}
inf2a.c = function(x){10*(x/10)^0.3}
inf2b.c = function(x){10*(x/10)^10}

x2c <- seq(0, 10, 1)
sign2c.ymax <- sign2a.c(x)
sign2c.ymin <- sign2b.c(x)
inf2c.ymax <- inf2a.c(x)
inf2c.ymin <- inf2b.c(x)
df2c <- data.frame(x2c, sign2c.ymin, sign2c.ymax, inf2c.ymin, inf2c.ymax)

Summary.Plot2c <- ggplot(data.frame(x = c(0, 10)), aes(x=x))+
  geom_ribbon(aes(x=x, ymin=sign2c.ymin, ymax=sign2c.ymax), data = df2c, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=x, ymin=inf2c.ymin, ymax=inf2c.ymax), data = df2c, fill ="black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Prediction 2: Job Creation", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = linear, aes(colour = "1")) +
  stat_function(fun = bound1, aes(colour = "0.1")) +
  stat_function(fun = bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 10, 5)) +
  scale_x_continuous(breaks=seq(0, 10, 5)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot2c

# Prediction Two (Job Destruction)
sign2a.d = function(x){10*(x/10)^0.1}
sign2b.d = function(x){10*(x/10)^10}
inf2a.d = function(x){10*(x/10)^0.16}
inf2b.d = function(x){10*(x/10)^1.86}

x2d <- seq(0, 10, 1)
sign2d.ymax <- sign2a.d(x)
sign2d.ymin <- sign2b.d(x)
inf2d.ymax <- inf2a.d(x)
inf2d.ymin <- inf2b.d(x)
df2d <- data.frame(x2d, sign2d.ymin, sign2d.ymax, inf2d.ymin, inf2d.ymax)

Summary.Plot2d <- ggplot(data.frame(x = c(0, 10)), aes(x=x))+
  geom_ribbon(aes(x=x, ymin=sign2d.ymin, ymax=sign2d.ymax), data = df2d, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=x, ymin=inf2d.ymin, ymax=inf2d.ymax), data = df2d, fill ="black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Prediction 2: Job Destruction", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = linear, aes(colour = "1")) +
  stat_function(fun = bound1, aes(colour = "0.1")) +
  stat_function(fun = bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 10, 5)) +
  scale_x_continuous(breaks=seq(0, 10, 5)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot2d

# Prediction Three (Turnover Rate)
sign3a.t = function(x){10*(x/10)^0.1}
sign3b.t = function(x){10*(x/10)^10}
inf3a.t = function(x){10*(x/10)^0.1}
inf3b.t = function(x){10*(x/10)^0.67}

x3t <- seq(0, 10, 1)
sign3t.ymax <- sign3a.t(x)
sign3t.ymin <- sign3b.t(x)
inf3t.ymax <- inf3a.t(x)
inf3t.ymin <- inf3b.t(x)
df3t <- data.frame(x3t, sign3t.ymin, sign3t.ymax, inf3t.ymin, inf3t.ymax)

Summary.Plot3t <- ggplot(data.frame(x = c(0, 10)), aes(x=x))+
  geom_ribbon(aes(x=x, ymin=sign3t.ymin, ymax=sign3t.ymax), data = df3t, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=x, ymin=inf3t.ymin, ymax=inf3t.ymax), data = df3t, fill ="black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Prediction 3: Job Turnover x \n Unemployment Insurance Generosity", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = linear, aes(colour = "1")) +
  stat_function(fun = bound1, aes(colour = "0.1")) +
  stat_function(fun = bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 10, 5)) +
  scale_x_continuous(breaks=seq(0, 10, 5)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot3t

# Prediction Three (Destruction Rate)
sign3a.d = function(x){10*(x/10)^0.1}
sign3b.d = function(x){10*(x/10)^10}
inf3a.d = function(x){10*(x/10)^0.1}
inf3b.d = function(x){10*(x/10)^0.68}

x3d <- seq(0, 10, 1)
sign3d.ymax <- sign3a.d(x)
sign3d.ymin <- sign3b.d(x)
inf3d.ymax <- inf3a.d(x)
inf3d.ymin <- inf3b.d(x)
df3d <- data.frame(x3d, sign3d.ymin, sign3d.ymax, inf3d.ymin, inf3d.ymax)

Summary.Plot3d <- ggplot(data.frame(x = c(0, 10)), aes(x=x))+
  geom_ribbon(aes(x=x, ymin=sign3d.ymin, ymax=sign3d.ymax), data = df3d, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=x, ymin=inf3d.ymin, ymax=inf3d.ymax), data = df3d, fill ="black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Prediction 3: Job Destruction x \n Unemployment Insurance Generosity", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = linear, aes(colour = "1")) +
  stat_function(fun = bound1, aes(colour = "0.1")) +
  stat_function(fun = bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 10, 5)) +
  scale_x_continuous(breaks=seq(0, 10, 5)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot3d

## Combine Summary Plots 1 + 2c + 2d + 3t + 3d ##
library(cowplot)
Figure1 <- plot_grid(Summary.Plot1, Summary.Plot2c, Summary.Plot2d, Summary.Plot3t, Summary.Plot3d, labels = c("A", "B", "C", "D", "E"), nrow = 2) 
ggsave("/results/Summary_Plot.png", scale = 2, width = 4.8, height = 3.25)

##############
# Summary Simulation Figure
# Nunn and Wantcheckon (2011)
#############

library("ggplot2")
nw.linear = function(x){4*(x/4)^1}
nw.bound1 = function(x){4*(x/4)^0.1}
nw.bound2 = function(x){4*(x/4)^10}

# Trust of Relatives
signa.rel = function(x){4*(x/4)^0.1}
signb.rel = function(x){4*(x/4)^10}
infa.rel = function(x){4*(x/4)^0.1}
infb.rel = function(x){4*(x/4)^10}

nw.1 <- seq(0, 4, 1)
nw.sign1.ymax <- signa.rel(nw.1)
nw.sign1.ymin <- signb.rel(nw.1)
nw.inf1.ymax <- infa.rel(nw.1)
nw.inf1.ymin <- infb.rel(nw.1)
nw.1.df <- data.frame(nw.1, nw.sign1.ymin, nw.sign1.ymax, nw.inf1.ymin, nw.inf1.ymax)

Summary.Plot.nw1 <- ggplot(data.frame(nw.1 = c(0, 4)), aes(x=nw.1)) + 
  geom_ribbon(aes(x=nw.1, ymin=nw.sign1.ymin, ymax=nw.sign1.ymax), data = nw.1.df, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=nw.1, ymin=nw.inf1.ymin, ymax=nw.inf1.ymax), data = nw.1.df, fill = "black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Trust of Relatives", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = nw.linear, aes(colour = "1")) +
  stat_function(fun = nw.bound1, aes(colour = "0.1")) +
  stat_function(fun = nw.bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 4, 1)) +
  scale_x_continuous(breaks=seq(0, 4, 1)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot.nw1

# Trust of Neighbors
signa.nei = function(x){4*(x/4)^0.1}
signb.nei = function(x){4*(x/4)^10}
infa.nei = function(x){4*(x/4)^0.1}
infb.nei = function(x){4*(x/4)^10}

nw.2 <- seq(0, 4, 1)
nw.sign2.ymax <- signa.nei(nw.2)
nw.sign2.ymin <- signb.nei(nw.2)
nw.inf2.ymax <- infa.nei(nw.2)
nw.inf2.ymin <- infb.nei(nw.2)
nw.2.df <- data.frame(nw.2, nw.sign2.ymin, nw.sign2.ymax, nw.inf2.ymin, nw.inf2.ymax)

Summary.Plot.nw2 <- ggplot(data.frame(nw.2 = c(0, 4)), aes(x=nw.2)) + 
  geom_ribbon(aes(x=nw.2, ymin=nw.sign2.ymin, ymax=nw.sign2.ymax), data = nw.2.df, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=nw.2, ymin=nw.inf2.ymin, ymax=nw.inf2.ymax), data = nw.2.df, fill = "black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Trust of Neighbors", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = nw.linear, aes(colour = "1")) +
  stat_function(fun = nw.bound1, aes(colour = "0.1")) +
  stat_function(fun = nw.bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 4, 1)) +
  scale_x_continuous(breaks=seq(0, 4, 1)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot.nw2

# Trust of Local Council
signa.loc = function(x){4*(x/4)^0.1}
signb.loc = function(x){4*(x/4)^10}
infa.loc = function(x){4*(x/4)^0.1}
infb.loc = function(x){4*(x/4)^7.6}

nw.3 <- seq(0, 4, 1)
nw.sign3.ymax <- signa.loc(nw.3)
nw.sign3.ymin <- signb.loc(nw.3)
nw.inf3.ymax <- infa.loc(nw.3)
nw.inf3.ymin <- infb.loc(nw.3)
nw.3.df <- data.frame(nw.3, nw.sign3.ymin, nw.sign3.ymax, nw.inf3.ymin, nw.inf3.ymax)

Summary.Plot.nw3 <- ggplot(data.frame(nw.3 = c(0, 4)), aes(x=nw.3)) + 
  geom_ribbon(aes(x=nw.3, ymin=nw.sign3.ymin, ymax=nw.sign3.ymax), data = nw.3.df, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=nw.3, ymin=nw.inf3.ymin, ymax=nw.inf3.ymax), data = nw.3.df, fill = "black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Trust of Local Council", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = nw.linear, aes(colour = "1")) +
  stat_function(fun = nw.bound1, aes(colour = "0.1")) +
  stat_function(fun = nw.bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 4, 1)) +
  scale_x_continuous(breaks=seq(0, 4, 1)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot.nw3

# Intra-group Trust
signa.intra = function(x){4*(x/4)^0.1}
signb.intra = function(x){4*(x/4)^10}
infa.intra = function(x){4*(x/4)^0.1}
infb.intra = function(x){4*(x/4)^10}

nw.4 <- seq(0, 4, 1)
nw.sign4.ymax <- signa.intra(nw.4)
nw.sign4.ymin <- signb.intra(nw.4)
nw.inf4.ymax <- infa.intra(nw.4)
nw.inf4.ymin <- infb.intra(nw.4)
nw.4.df <- data.frame(nw.4, nw.sign4.ymin, nw.sign4.ymax, nw.inf4.ymin, nw.inf4.ymax)

Summary.Plot.nw4 <- ggplot(data.frame(nw.4 = c(0, 4)), aes(x=nw.4)) + 
  geom_ribbon(aes(x=nw.4, ymin=nw.sign4.ymin, ymax=nw.sign4.ymax), data = nw.4.df, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=nw.4, ymin=nw.inf4.ymin, ymax=nw.inf4.ymax), data = nw.4.df, fill = "black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Intra-group Trust", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = nw.linear, aes(colour = "1")) +
  stat_function(fun = nw.bound1, aes(colour = "0.1")) +
  stat_function(fun = nw.bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 4, 1)) +
  scale_x_continuous(breaks=seq(0, 4, 1)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot.nw4

# Inter-group Trust
signa.inter = function(x){4*(x/4)^0.1}
signb.inter = function(x){4*(x/4)^10}
infa.inter = function(x){4*(x/4)^0.1}
infb.inter = function(x){4*(x/4)^1.45}

nw.5 <- seq(0, 4, 1)
nw.sign5.ymax <- signa.inter(nw.5)
nw.sign5.ymin <- signb.inter(nw.5)
nw.inf5.ymax <- infa.inter(nw.5)
nw.inf5.ymin <- infb.inter(nw.5)
nw.5.df <- data.frame(nw.5, nw.sign5.ymin, nw.sign5.ymax, nw.inf5.ymin, nw.inf5.ymax)

Summary.Plot.nw5 <- ggplot(data.frame(nw.5 = c(0, 4)), aes(x=nw.5)) + 
  geom_ribbon(aes(x=nw.5, ymin=nw.sign5.ymin, ymax=nw.sign5.ymax), data = nw.5.df, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=nw.4, ymin=nw.inf5.ymin, ymax=nw.inf5.ymax), data = nw.5.df, fill = "black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Inter-group Trust", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = nw.linear, aes(colour = "1")) +
  stat_function(fun = nw.bound1, aes(colour = "0.1")) +
  stat_function(fun = nw.bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 4, 1)) +
  scale_x_continuous(breaks=seq(0, 4, 1)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot.nw5

## Combine Summary Plots nw1+ nw2 + nw3 + nw4 + nw5 ##
library(cowplot)
Figure1 <- plot_grid(Summary.Plot.nw1, Summary.Plot.nw2, Summary.Plot.nw3, Summary.Plot.nw4, Summary.Plot.nw5, labels = c("A", "B", "C", "D", "E"), nrow = 2) 
ggsave("/results/Summary_Plot_NW.png", scale = 2, width = 4.8, height = 3.25)

##############
# Summary Simulation Figure
# Bond and Lang (2013)
#############

library("ggplot2")
bl.linear = function(x){180*(x/180)^1}
bl.bound1 = function(x){180*(x/180)^0.1}
bl.bound2 = function(x){180*(x/180)^10}

# Test Score Gap, Fall Kindergarten
signa.fk = function(x){180*(x/180)^0.1}
signb.fk = function(x){180*(x/180)^10}
infa.fk = function(x){180*(x/180)^0.1}
infb.fk = function(x){180*(x/180)^9.07}

bl.fk <- seq(0, 180, 1)
bl.signfk.ymax <- signa.fk(bl.fk)
bl.signfk.ymin <- signb.fk(bl.fk)
bl.inffk.ymax <- infa.fk(bl.fk)
bl.inffk.ymin <- infb.fk(bl.fk)
bl.fk.df <- data.frame(bl.fk, bl.signfk.ymin, bl.signfk.ymax, bl.inffk.ymin, bl.inffk.ymax)

Summary.Plot.blfk <- ggplot(data.frame(bl.fk = c(0, 180)), aes(x=bl.fk)) + 
  geom_ribbon(aes(x=bl.fk, ymin=bl.signfk.ymin, ymax=bl.signfk.ymax), data = bl.fk.df, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=bl.fk, ymin=bl.inffk.ymin, ymax=bl.inffk.ymax), data = bl.fk.df, fill = "black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Test Score Gap \n Fall Kindergarten", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = bl.linear, aes(colour = "1")) +
  stat_function(fun = bl.bound1, aes(colour = "0.1")) +
  stat_function(fun = bl.bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 180, 20)) +
  scale_x_continuous(breaks=seq(0, 180, 20)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot.blfk

# Test Score Gap, Spring Kindergarten
signa.sk = function(x){180*(x/180)^0.1}
signb.sk = function(x){180*(x/180)^10}
infa.sk = function(x){180*(x/180)^0.1}
infb.sk = function(x){180*(x/180)^10}

bl.sk <- seq(0, 180, 1)
bl.signsk.ymax <- signa.sk(bl.fk)
bl.signsk.ymin <- signb.sk(bl.fk)
bl.infsk.ymax <- infa.sk(bl.fk)
bl.infsk.ymin <- infb.sk(bl.fk)
bl.sk.df <- data.frame(bl.sk, bl.signsk.ymin, bl.signsk.ymax, bl.infsk.ymin, bl.infsk.ymax)

Summary.Plot.blsk <- ggplot(data.frame(bl.sk = c(0, 180)), aes(x=bl.sk)) + 
  geom_ribbon(aes(x=bl.sk, ymin=bl.signsk.ymin, ymax=bl.signsk.ymax), data = bl.sk.df, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=bl.sk, ymin=bl.infsk.ymin, ymax=bl.infsk.ymax), data = bl.sk.df, fill = "black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Test Score Gap \n Spring Kindergarten", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = bl.linear, aes(colour = "1")) +
  stat_function(fun = bl.bound1, aes(colour = "0.1")) +
  stat_function(fun = bl.bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 180, 20)) +
  scale_x_continuous(breaks=seq(0, 180, 20)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot.blsk

# Test Score Gap, Spring 1st Grade
signa.s1 = function(x){180*(x/180)^0.1}
signb.s1 = function(x){180*(x/180)^10}
infa.s1 = function(x){180*(x/180)^0.1}
infb.s1 = function(x){180*(x/180)^10}

bl.s1 <- seq(0, 180, 1)
bl.signs1.ymax <- signa.s1(bl.s1)
bl.signs1.ymin <- signb.s1(bl.s1)
bl.infs1.ymax <- infa.s1(bl.s1)
bl.infs1.ymin <- infb.s1(bl.s1)
bl.s1.df <- data.frame(bl.s1, bl.signs1.ymin, bl.signs1.ymax, bl.infs1.ymin, bl.infs1.ymax)

Summary.Plot.bls1 <- ggplot(data.frame(bl.s1 = c(0, 180)), aes(x=bl.s1)) + 
  geom_ribbon(aes(x=bl.s1, ymin=bl.signs1.ymin, ymax=bl.signs1.ymax), data = bl.s1.df, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=bl.s1, ymin=bl.infs1.ymin, ymax=bl.infs1.ymax), data = bl.s1.df, fill = "black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Test Score Gap \n Spring 1st Grade", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = bl.linear, aes(colour = "1")) +
  stat_function(fun = bl.bound1, aes(colour = "0.1")) +
  stat_function(fun = bl.bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 180, 20)) +
  scale_x_continuous(breaks=seq(0, 180, 20)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot.bls1

# Test Score Gap, Spring 1st Grade
signa.s3 = function(x){180*(x/180)^0.1}
signb.s3 = function(x){180*(x/180)^10}
infa.s3 = function(x){180*(x/180)^0.1}
infb.s3 = function(x){180*(x/180)^10}

bl.s3 <- seq(0, 180, 1)
bl.signs3.ymax <- signa.s3(bl.s3)
bl.signs3.ymin <- signb.s3(bl.s3)
bl.infs3.ymax <- infa.s3(bl.s3)
bl.infs3.ymin <- infb.s3(bl.s3)
bl.s3.df <- data.frame(bl.s3, bl.signs3.ymin, bl.signs3.ymax, bl.infs3.ymin, bl.infs3.ymax)

Summary.Plot.bls3 <- ggplot(data.frame(bl.s3 = c(0, 180)), aes(x=bl.s3)) + 
  geom_ribbon(aes(x=bl.s3, ymin=bl.signs3.ymin, ymax=bl.signs3.ymax), data = bl.s3.df, fill = "black", alpha = 0.25) +
  geom_ribbon(aes(x=bl.s3, ymin=bl.infs3.ymin, ymax=bl.infs3.ymax), data = bl.s3.df, fill = "black", alpha = 0.5) +
  theme_bw() +
  labs(x = "Y \n Test Score Gap \n Spring 3rd Grade", y = "T(Y)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  stat_function(fun = bl.linear, aes(colour = "1")) +
  stat_function(fun = bl.bound1, aes(colour = "0.1")) +
  stat_function(fun = bl.bound2, aes(colour = "10")) + 
  scale_y_continuous(breaks=seq(0, 180, 20)) +
  scale_x_continuous(breaks=seq(0, 180, 20)) +
  scale_colour_manual("Sigma", values = c("black", "black", "black"))
Summary.Plot.bls3

## Combine Summary Plots blfk + blsk + bls1 + bls3 ##
library(cowplot)
Figure1 <- plot_grid(Summary.Plot.blfk, Summary.Plot.blsk, Summary.Plot.bls1, Summary.Plot.bls3, labels = c("A", "B", "C", "D"), nrow = 2) 
ggsave("/results/Summary_Plot_BL.png", scale = 2, width = 4.5, height = 3.75)

#########
# CDF - Figure 2
#########

library("ggplot2")
cdf1 = function(x)pnorm(x, mean = 5, sd = 2.5)
cdf5 = function(x)pnorm(x, mean = 5, sd = 5)
cdf0 = function(x)pnorm(x, mean = 5, sd = 0.1)

cdf.plot <- ggplot(data.frame(x = c(0, 10)), aes(x=x)) + stat_function(fun = cdf1, aes(colour = "2.5")) +
  stat_function(fun = cdf5, aes(colour = "5")) +
  stat_function(fun = cdf0, aes(colour = "0.1")) +
  theme_bw() +
  labs(x = "Y", y = "T(Y)") + 
  scale_y_continuous(breaks=seq(0, 10, 5)) +
  scale_x_continuous(breaks=seq(0, 10, 5)) +
  scale_colour_manual("Sigma", values = c("light gray", "dark gray", "black"))
cdf.plot
ggsave("/results/Figure_2.png", scale = 1, width = 4.8, height = 3.5)

#####################
# AADR Prediction 1 #
#####################

## Plot 1.1 ##

# Read .csv into R
Pred1.1 <- read.csv(file="/data/pred1_1.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor1.1 <- Pred1.1$factor
log.factor1.1 <- log10(factor1.1)
beta1.1 <- Pred1.1$beta1
low_bound1.1 <- Pred1.1$low_bound
up_bound1.1 <- Pred1.1$up_bound

# Organized data frame
FigureData1.1<-rbind(data.frame(log.factor1.1,Key="Point Estimate",low_bound1.1=beta1.1),
                     data.frame(log.factor1.1,Key="Confidence Interval",low_bound1.1),
                     data.frame(log.factor1.1,Key="Confidence Interval",low_bound1.1=up_bound1.1))

# Make plot
library(ggplot2)
Fig1.1 <- ggplot(FigureData1.1)+
  geom_point(aes(log.factor1.1, low_bound1.1, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Turnover Rate \n (unemployment \n omitted)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
  #labs(caption = "Note: Gray lines represent 95% confidence interval")
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig1.1 <- Fig1.1 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig1.1

## Plot 1.2 ##

# Read in .csv into R
Pred1.2 <- read.csv(file="/data/pred1_2.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor1.2 <- Pred1.2$factor
log.factor1.2 <- log10(factor1.2)
beta1.2 <- Pred1.2$beta1
low_bound1.2 <- Pred1.2$low_bound
up_bound1.2 <- Pred1.2$up_bound

# Organized data frame
FigureData1.2<-rbind(data.frame(log.factor1.2,Key="Point Estimate",low_bound1.2=beta1.2),
                     data.frame(log.factor1.2,Key="Confidence Interval",low_bound1.2),
                     data.frame(log.factor1.2,Key="Confidence Interval",low_bound1.2=up_bound1.2))

# Make plot
library(ggplot2)
Fig1.2 <- ggplot(FigureData1.2)+
  geom_point(aes(log.factor1.2, low_bound1.2, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Turnover Rate \n (with unemployment)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
  #labs(caption = "Note: Gray lines represent 95% confidence interval")
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig1.2 <- Fig1.2 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig1.2

## Plot 1.3 ##

# Read .csv into R
Pred1.3 <- read.csv(file="/data/pred1_3.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor1.3 <- Pred1.3$factor
log.factor1.3 <- log10(factor1.3)
beta1.3 <- Pred1.3$beta1
low_bound1.3 <- Pred1.3$low_bound
up_bound1.3 <- Pred1.3$up_bound

# Organize data 
FigureData1.3<-rbind(data.frame(log.factor1.3,Key="Point Estimate",low_bound1.3=beta1.3),
                     data.frame(log.factor1.3,Key="Confidence Interval",low_bound1.3),
                     data.frame(log.factor1.3,Key="Confidence Interval",low_bound1.3=up_bound1.3))

# Make plot
library(ggplot2)
Fig1.3 <- ggplot(FigureData1.3)+
  geom_point(aes(log.factor1.3, low_bound1.3, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Turnover Rate \n (with unemployment \n and MSA controls)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
  #labs(caption = "Note: Gray lines represent 95% confidence interval")
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig1.3 <- Fig1.3 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig1.3

## Combine figures 1.1 + 1.2 + 1.3 ##
library(cowplot)
Figure1 <- plot_grid(Fig1.1, Fig1.2, Fig1.3, labels = c("A", "B", "C"), nrow = 2) 
ggsave("/results/Figure_A1.png", scale = 2)

#####################
# AADR Prediction 2 #
#####################

## Plot 2.1a ##

# Read .csv into R
Pred2.1a <- read.csv(file="/data/pred2_1a.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor2.1a <- Pred2.1a$factor
log.factor2.1a <- log10(factor2.1a)
beta2.1a <- Pred2.1a$beta1
low_bound2.1a <- Pred2.1a$low_bound
up_bound2.1a <- Pred2.1a$up_bound

# Organized data
FigureData2.1a<-rbind(data.frame(log.factor2.1a,Key="Point Estimate",low_bound2.1a=beta2.1a),
                      data.frame(log.factor2.1a,Key="Confidence Interval",low_bound2.1a),
                     data.frame(log.factor2.1a,Key="Confidence Interval",low_bound2.1a=up_bound2.1a))

# Make plot
library(ggplot2)
Fig2.1a <- ggplot(FigureData2.1a)+
  geom_point(aes(log.factor2.1a, low_bound2.1a, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Creation Rate") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0)) 
Fig2.1a <- Fig2.1a + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig2.1a

## Plot 2.1b ##

# Read .csv into R
Pred2.1b <- read.csv(file="/data/pred2_1b.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor2.1b <- Pred2.1b$factor
log.factor2.1b <- log10(factor2.1b)
beta2.1b <- Pred2.1b$beta1
low_bound2.1b <- Pred2.1b$low_bound
up_bound2.1b <- Pred2.1b$up_bound

# Organized data
FigureData2.1b<-rbind(data.frame(log.factor2.1b,Key="Point Estimate",low_bound2.1b=beta2.1b),
                      data.frame(log.factor2.1b,Key="Confidence Interval",low_bound2.1b),
                      data.frame(log.factor2.1b,Key="Confidence Interval",low_bound2.1b=up_bound2.1b))

# Make plot
library(ggplot2)
Fig2.1b <- ggplot(FigureData2.1b)+
  geom_point(aes(log.factor2.1b, low_bound2.1b, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Destruction Rate") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig2.1b <- Fig2.1b + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig2.1b

## Plot 2.2a ##

# Read .csv into R
Pred2.2a <- read.csv(file="/data/pred2_2a.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor2.2a <- Pred2.2a$factor
log.factor2.2a <- log10(factor2.2a)
beta2.2a <- Pred2.2a$beta1
low_bound2.2a <- Pred2.2a$low_bound
up_bound2.2a <- Pred2.2a$up_bound

# Organized data
FigureData2.2a<-rbind(data.frame(log.factor2.2a,Key="Point Estimate",low_bound2.2a=beta2.2a),
                      data.frame(log.factor2.2a,Key="Confidence Interval",low_bound2.2a),
                      data.frame(log.factor2.2a,Key="Confidence Interval",low_bound2.2a=up_bound2.2a))

# Make plot
library(ggplot2)
Fig2.2a <- ggplot(FigureData2.2a)+
  geom_point(aes(log.factor2.2a, low_bound2.2a, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Creation Rate \n (with MSA controls)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig2.2a <- Fig2.2a + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig2.2a

## Plot 2.2b ##

# Read .csv into R
Pred2.2b <- read.csv(file="/data/pred2_2b.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor2.2b <- Pred2.2b$factor
log.factor2.2b <- log10(factor2.2b)
beta2.2b <- Pred2.2b$beta1
low_bound2.2b <- Pred2.2b$low_bound
up_bound2.2b <- Pred2.2b$up_bound

# Organized data
FigureData2.2b<-rbind(data.frame(log.factor2.2b,Key="Point Estimate",low_bound2.2b=beta2.2b),
                      data.frame(log.factor2.2b,Key="Confidence Interval",low_bound2.2b),
                      data.frame(log.factor2.2b,Key="Confidence Interval",low_bound2.2b=up_bound2.2b))

# Make plot
library(ggplot2)
Fig2.2b <- ggplot(FigureData2.2b)+
  geom_point(aes(log.factor2.2b, low_bound2.2b, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Destruction Rate \n (with MSA controls)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig2.2b <- Fig2.2b + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig2.2b

## Combine figures 2.1a + 2.1b + 2.2a + 2.2b ##
library(cowplot)
Figure2 <- plot_grid(Fig2.1a, Fig2.1b, Fig2.2a, Fig2.2b, labels = c("A", "B", "C", "D"), nrow = 2) 
ggsave("/results/Simulation_Figure2.png", scale = 2)

#####################
# AADR Prediction 3 #
#####################

## Plot 3.1 ##

# Read .csv into R
Pred3.1 <- read.csv(file="/data/pred3_1.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor3.1 <- Pred3.1$factor
log.factor3.1 <- log10(factor3.1)
beta3.1 <- Pred3.1$beta1
low_bound3.1 <- Pred3.1$low_bound
up_bound3.1 <- Pred3.1$up_bound

# Organized data frame
FigureData3.1<-rbind(data.frame(log.factor3.1,Key="Point Estimate",low_bound3.1=beta3.1),
                     data.frame(log.factor3.1,Key="Confidence Interval",low_bound3.1),
                     data.frame(log.factor3.1,Key="Confidence Interval",low_bound3.1=up_bound3.1))

# Make plot
library(ggplot2)
Fig3.1 <- ggplot(FigureData3.1)+
  geom_point(aes(log.factor3.1, low_bound3.1, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Turnover Rate \n x \n UI Generosity") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig3.1 <- Fig3.1 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig3.1

## Plot 3.2 ##

# Read .csv into R
Pred3.2 <- read.csv(file="/data/pred3_2.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor3.2 <- Pred3.2$factor
log.factor3.2 <- log10(factor3.2)
beta3.2 <- Pred3.2$beta1
low_bound3.2 <- Pred3.2$low_bound
up_bound3.2 <- Pred3.2$up_bound

# Organized data frame
FigureData3.2<-rbind(data.frame(log.factor3.2,Key="Point Estimate",low_bound3.2=beta3.2),
                     data.frame(log.factor3.2,Key="Confidence Interval",low_bound3.2),
                     data.frame(log.factor3.2,Key="Confidence Interval",low_bound3.2=up_bound3.2))

# Make plot
library(ggplot2)
Fig3.2 <- ggplot(FigureData3.2)+
  geom_point(aes(log.factor3.2, low_bound3.2, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Turnover Rate \n x \n UI Generosity \n (with MSA controls)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig3.2 <- Fig3.2 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig3.2

## Plot 3.3 ##

# Read .csv into R
Pred3.3 <- read.csv(file="/data/pred3_3.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor3.3 <- Pred3.3$factor
log.factor3.3 <- log10(factor3.3)
beta3.3 <- Pred3.3$beta1
low_bound3.3 <- Pred3.3$low_bound
up_bound3.3 <- Pred3.3$up_bound

# Organized data frame
FigureData3.3<-rbind(data.frame(log.factor3.3,Key="Point Estimate",low_bound3.3=beta3.3),
                     data.frame(log.factor3.3,Key="Confidence Interval",low_bound3.3),
                     data.frame(log.factor3.3,Key="Confidence Interval",low_bound3.3=up_bound3.3))

# Make plot
library(ggplot2)
Fig3.3 <- ggplot(FigureData3.3)+
  geom_point(aes(log.factor3.3, low_bound3.3, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Destruction Rate \n x \n UI Generosity") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig3.3 <- Fig3.3 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig3.3

## Plot 3.4 ##

# Read .csv into R
Pred3.4 <- read.csv(file="/data/pred3_4.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor3.4 <- Pred3.4$factor
log.factor3.4 <- log10(factor3.4)
beta3.4 <- Pred3.4$beta1
low_bound3.4 <- Pred3.4$low_bound
up_bound3.4 <- Pred3.4$up_bound

# Organized data frame
FigureData3.4<-rbind(data.frame(log.factor3.4,Key="Point Estimate",low_bound3.4=beta3.4),
                     data.frame(log.factor3.4,Key="Confidence Interval",low_bound3.4),
                     data.frame(log.factor3.4,Key="Confidence Interval",low_bound3.4=up_bound3.4))

# Make plot
library(ggplot2)
Fig3.4 <- ggplot(FigureData3.4)+
  geom_point(aes(log.factor3.4, low_bound3.4, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Destruction Rate \n x \n UI Generosity \n (with MSA controls)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig3.4 <- Fig3.4 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig3.4

# Combine plots
library(cowplot)
Figure3 <- plot_grid(Fig3.1, Fig3.2, Fig3.3, Fig3.4, labels = c("A", "B", "C", "D"), nrow = 2) 
ggsave("/results/Simulation_Figure3.png", scale = 2)

#######################
# Nunn and Wantchekon #
# Table 5             #
#######################

## Plot 5.1 ##

# Read .csv into R
Table5.1 <- read.csv(file="/data/NW_Table5_1.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor5.1 <- Table5.1$factor
log.factor5.1 <- log10(factor5.1)
beta5.1 <- Table5.1$beta1
low_bound5.1 <- Table5.1$low_bound
up_bound5.1 <- Table5.1$up_bound

# Organized data frame
FigureData5.1<-rbind(data.frame(log.factor5.1,Key="Point Estimate",low_bound5.1=beta5.1),
                     data.frame(log.factor5.1,Key="Confidence Interval",low_bound5.1),
                     data.frame(log.factor5.1,Key="Confidence Interval",low_bound5.1=up_bound5.1))

# Make plot
library(ggplot2)
Fig5.1 <- ggplot(FigureData5.1)+
  geom_point(aes(log.factor5.1, low_bound5.1, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "ln(Exports/Area) \n on Trust of \n Relatives") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig5.1 <- Fig5.1 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig5.1

## Plot 5.2 ##

# Read .csv into R
Table5.2 <- read.csv(file="/data/NW_Table5_2.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor5.2 <- Table5.2$factor
log.factor5.2 <- log10(factor5.2)
beta5.2 <- Table5.2$beta1
low_bound5.2 <- Table5.2$low_bound
up_bound5.2 <- Table5.2$up_bound

# Organized data frame
FigureData5.2<-rbind(data.frame(log.factor5.2,Key="Point Estimate",low_bound5.2=beta5.2),
                     data.frame(log.factor5.2,Key="Confidence Interval",low_bound5.2),
                     data.frame(log.factor5.2,Key="Confidence Interval",low_bound5.2=up_bound5.2))

# Make plot
library(ggplot2)
Fig5.2 <- ggplot(FigureData5.2)+
  geom_point(aes(log.factor5.2, low_bound5.2, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "ln(Exports/Area) \n on Trust of \n Neighbors") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig5.2 <- Fig5.2 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig5.2

## Plot 5.3 ##

# Read .csv into R
Table5.3 <- read.csv(file="/data/NW_Table5_3.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor5.3 <- Table5.3$factor
log.factor5.3 <- log10(factor5.3)
beta5.3 <- Table5.3$beta1
low_bound5.3 <- Table5.3$low_bound
up_bound5.3 <- Table5.3$up_bound

# Organized data frame
FigureData5.3<-rbind(data.frame(log.factor5.3,Key="Point Estimate",low_bound5.3=beta5.3),
                     data.frame(log.factor5.3,Key="Confidence Interval",low_bound5.3),
                     data.frame(log.factor5.3,Key="Confidence Interval",low_bound5.3=up_bound5.3))

# Make plot
library(ggplot2)
Fig5.3 <- ggplot(FigureData5.3)+
  geom_point(aes(log.factor5.3, low_bound5.3, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "ln(Exports/Area) \n on Trust of \n Local Council") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig5.3 <- Fig5.3 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig5.3

## Plot 5.4 ##

# Read .csv into R
Table5.4 <- read.csv(file="/data/NW_Table5_4.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor5.4 <- Table5.4$factor
log.factor5.4 <- log10(factor5.4)
beta5.4 <- Table5.4$beta1
low_bound5.4 <- Table5.4$low_bound
up_bound5.4 <- Table5.4$up_bound

# Organized data frame
FigureData5.4<-rbind(data.frame(log.factor5.4,Key="Point Estimate",low_bound5.4=beta5.4),
                     data.frame(log.factor5.4,Key="Confidence Interval",low_bound5.4),
                     data.frame(log.factor5.4,Key="Confidence Interval",low_bound5.4=up_bound5.4))

# Make plot
library(ggplot2)
Fig5.4 <- ggplot(FigureData5.4)+
  geom_point(aes(log.factor5.4, low_bound5.4, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "ln(Exports/Area) \n on Intra-group \n Trust") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig5.4 <- Fig5.4 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig5.4

## Plot 5.5 ##

# Read .csv into R
Table5.5 <- read.csv(file="/data/NW_Table5_5.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor5.5 <- Table5.5$factor
log.factor5.5 <- log10(factor5.5)
beta5.5 <- Table5.5$beta1
low_bound5.5 <- Table5.5$low_bound
up_bound5.5 <- Table5.5$up_bound

# Organized data frame
FigureData5.5<-rbind(data.frame(log.factor5.5,Key="Point Estimate",low_bound5.5=beta5.5),
                     data.frame(log.factor5.5,Key="Confidence Interval",low_bound5.5),
                     data.frame(log.factor5.5,Key="Confidence Interval",low_bound5.5=up_bound5.5))

# Make plot
library(ggplot2)
Fig5.5 <- ggplot(FigureData5.5)+
  geom_point(aes(log.factor5.5, low_bound5.5, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "ln(Exports/Area) \n on Inter-group \n Trust") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig5.5 <- Fig5.5 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig5.5

# Combine plots
library(cowplot)
Figure4 <- plot_grid(Fig5.1, Fig5.2, Fig5.3, Fig5.4, Fig5.5, labels = c("A", "B", "C", "D", "E"), nrow = 3) 
ggsave("/results/Figure_4.png", scale = 2)


#######################
# Nunn and Wantchekon #
# Table 2             #
#######################

## Plot 2.1 ##

# Read .csv into R
Table2.1 <- read.csv(file="/data/NW_Table2_1.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor2.1 <- Table2.1$factor
log.factor2.1 <- log10(factor2.1)
beta2.1 <- Table2.1$beta1
low_bound2.1 <- Table2.1$low_bound
up_bound2.1 <- Table2.1$up_bound

# Organized data frame
FigureData2.1<-rbind(data.frame(log.factor2.1,Key="Point Estimate",low_bound2.1=beta2.1),
                     data.frame(log.factor2.1,Key="Confidence Interval",low_bound2.1),
                     data.frame(log.factor2.1,Key="Confidence Interval",low_bound2.1=up_bound2.1))

# Make plot
library(ggplot2)
Fig2.1 <- ggplot(FigureData2.1)+
  geom_point(aes(log.factor2.1, low_bound2.1, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "ln(Exports/Area) \n on Trust of \n Relatives") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig2.1 <- Fig2.1 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig2.1

## Plot 2.2 ##

# Read .csv into R
Table2.2 <- read.csv(file="/data/NW_Table2_2.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor2.2 <- Table2.2$factor
log.factor2.2 <- log10(factor2.2)
beta2.2 <- Table2.2$beta1
low_bound2.2 <- Table2.2$low_bound
up_bound2.2 <- Table2.2$up_bound

# Organized data frame
FigureData2.2<-rbind(data.frame(log.factor2.2,Key="Point Estimate",low_bound2.2=beta2.2),
                     data.frame(log.factor2.2,Key="Confidence Interval",low_bound2.2),
                     data.frame(log.factor2.2,Key="Confidence Interval",low_bound2.2=up_bound2.2))

# Make plot
library(ggplot2)
Fig2.2 <- ggplot(FigureData2.2)+
  geom_point(aes(log.factor2.2, low_bound2.2, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "ln(Exports/Area) \n on Trust of \n Neighbors") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig2.2 <- Fig2.2 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig2.2

## Plot 2.3 ##

# Read .csv into R
Table2.3 <- read.csv(file="/data/NW_Table2_3.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor2.3 <- Table2.3$factor
log.factor2.3 <- log10(factor2.3)
beta2.3 <- Table2.3$beta1
low_bound2.3 <- Table2.3$low_bound
up_bound2.3 <- Table2.3$up_bound

# Organized data frame
FigureData2.3<-rbind(data.frame(log.factor2.3,Key="Point Estimate",low_bound2.3=beta2.3),
                     data.frame(log.factor2.3,Key="Confidence Interval",low_bound2.3),
                     data.frame(log.factor2.3,Key="Confidence Interval",low_bound2.3=up_bound2.3))

# Make plot
library(ggplot2)
Fig2.3 <- ggplot(FigureData2.3)+
  geom_point(aes(log.factor2.3, low_bound2.3, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "ln(Exports/Area) \n on Trust of \n Local Council") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig2.3 <- Fig2.3 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig2.3

## Plot 2.4 ##

# Read .csv into R
Table2.4 <- read.csv(file="/data/NW_Table2_4.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor2.4 <- Table2.4$factor
log.factor2.4 <- log10(factor2.4)
beta2.4 <- Table2.4$beta1
low_bound2.4 <- Table2.4$low_bound
up_bound2.4 <- Table2.4$up_bound

# Organized data frame
FigureData2.4<-rbind(data.frame(log.factor2.4,Key="Point Estimate",low_bound2.4=beta2.4),
                     data.frame(log.factor2.4,Key="Confidence Interval",low_bound2.4),
                     data.frame(log.factor2.4,Key="Confidence Interval",low_bound2.4=up_bound2.4))

# Make plot
library(ggplot2)
Fig2.4 <- ggplot(FigureData2.4)+
  geom_point(aes(log.factor2.4, low_bound2.4, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "ln(Exports/Area) \n on Intra-group \n Trust") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig2.4 <- Fig2.4 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig2.4

## Plot 2.5 ##

# Read .csv into R
Table2.5 <- read.csv(file="/data/NW_Table2_5.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor2.5 <- Table2.5$factor
log.factor2.5 <- log10(factor2.5)
beta2.5 <- Table2.5$beta1
low_bound2.5 <- Table2.5$low_bound
up_bound2.5 <- Table2.5$up_bound

# Organized data frame
FigureData2.5<-rbind(data.frame(log.factor2.5,Key="Point Estimate",low_bound2.5=beta2.5),
                     data.frame(log.factor2.5,Key="Confidence Interval",low_bound2.5),
                     data.frame(log.factor2.5,Key="Confidence Interval",low_bound2.5=up_bound2.5))

# Make plot
library(ggplot2)
Fig2.5 <- ggplot(FigureData2.5)+
  geom_point(aes(log.factor2.5, low_bound2.5, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "ln(Exports/Area) \n on Inter-group \n Trust") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig2.5 <- Fig2.5 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig2.5

# Combine plots
library(cowplot)
Figure4 <- plot_grid(Fig2.1, Fig2.2, Fig2.3, Fig2.4, Fig2.5, labels = c("A", "B", "C", "D", "E"), nrow = 3) 
ggsave("/results/Figure_A8.png", scale = 2)

####################
# Bond and Lang    #
# ECLS no controls #
####################

## Plot ECLS Fall Kindergarten ##

# Read .csv into R
Table.ecls.fk <- read.csv(file="/data/BL_ecls_fallk.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.ecls.fk <- Table.ecls.fk$factor
log.factor.ecls.fk <- log10(factor.ecls.fk)
beta.ecls.fk <- Table.ecls.fk$beta1
low_bound.ecls.fk <- Table.ecls.fk$low_bound
up_bound.ecls.fk <- Table.ecls.fk$up_bound

# Organized data frame
FigureData.ecls.fk<-rbind(data.frame(log.factor.ecls.fk,Key="Point Estimate",low_bound.ecls.fk=beta.ecls.fk),
                      data.frame(log.factor.ecls.fk,Key="Confidence Interval",low_bound.ecls.fk),
                     data.frame(log.factor.ecls.fk,Key="Confidence Interval",low_bound.ecls.fk=up_bound.ecls.fk))

# Make plot
library(ggplot2)
Fig.ecls.fk <- ggplot(FigureData.ecls.fk)+
  geom_point(aes(log.factor.ecls.fk, low_bound.ecls.fk, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Test Score Gap, \n Fall \n Kindergarten") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.ecls.fk <- Fig.ecls.fk + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.ecls.fk

## Plot ECLS Spring Kindergarten ##

# Read .csv into R
Table.ecls.sk <- read.csv(file="/data/BL_ecls_springk.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.ecls.sk <- Table.ecls.sk$factor
log.factor.ecls.sk <- log10(factor.ecls.sk)
beta.ecls.sk <- Table.ecls.sk$beta1
low_bound.ecls.sk <- Table.ecls.sk$low_bound
up_bound.ecls.sk <- Table.ecls.sk$up_bound

# Organized data frame
FigureData.ecls.sk<-rbind(data.frame(log.factor.ecls.sk,Key="Point Estimate",low_bound.ecls.sk=beta.ecls.sk),
                          data.frame(log.factor.ecls.sk,Key="Confidence Interval",low_bound.ecls.sk),
                         data.frame(log.factor.ecls.sk,Key="Confidence Interval",low_bound.ecls.sk=up_bound.ecls.sk))

# Make plot
library(ggplot2)
Fig.ecls.sk <- ggplot(FigureData.ecls.sk)+
  geom_point(aes(log.factor.ecls.sk, low_bound.ecls.sk, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Test Score Gap, \n Spring \n Kindergarten") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.ecls.sk <- Fig.ecls.sk + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.ecls.sk

## Plot ECLS Spring First Grade ##

# Read .csv into R
Table.ecls.s1 <- read.csv(file="/data/BL_ecls_spring1.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.ecls.s1 <- Table.ecls.s1$factor
log.factor.ecls.s1 <- log10(factor.ecls.s1)
beta.ecls.s1 <- Table.ecls.s1$beta1
low_bound.ecls.s1 <- Table.ecls.s1$low_bound
up_bound.ecls.s1 <- Table.ecls.s1$up_bound

# Organized data frame
FigureData.ecls.s1<-rbind(data.frame(log.factor.ecls.s1,Key="Point Estimate",low_bound.ecls.s1=beta.ecls.s1),
                          data.frame(log.factor.ecls.s1,Key="Confidence Interval",low_bound.ecls.s1),
                          data.frame(log.factor.ecls.s1,Key="Confidence Interval",low_bound.ecls.s1=up_bound.ecls.s1))

# Make plot
library(ggplot2)
Fig.ecls.s1 <- ggplot(FigureData.ecls.s1)+
  geom_point(aes(log.factor.ecls.s1, low_bound.ecls.s1, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Test Score Gap, \n Spring \n 1st Grade") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.ecls.s1 <- Fig.ecls.s1 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.ecls.s1

## Plot ECLS Spring Third Grade ##

# Read .csv into R
Table.ecls.s3 <- read.csv(file="/data/BL_ecls_spring3.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.ecls.s3 <- Table.ecls.s3$factor
log.factor.ecls.s3 <- log10(factor.ecls.s3)
beta.ecls.s3 <- Table.ecls.s3$beta1
low_bound.ecls.s3 <- Table.ecls.s3$low_bound
up_bound.ecls.s3 <- Table.ecls.s3$up_bound

# Organized data frame
FigureData.ecls.s3<-rbind(data.frame(log.factor.ecls.s3,Key="Point Estimate",low_bound.ecls.s3=beta.ecls.s3),
                          data.frame(log.factor.ecls.s3,Key="Confidence Interval",low_bound.ecls.s3),
                          data.frame(log.factor.ecls.s3,Key="Confidence Interval",low_bound.ecls.s3=up_bound.ecls.s3))

# Make plot
library(ggplot2)
Fig.ecls.s3 <- ggplot(FigureData.ecls.s3)+
  geom_point(aes(log.factor.ecls.s3, low_bound.ecls.s3, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Test Score Gap, \n Spring \n 3rd Grade") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.ecls.s3 <- Fig.ecls.s3 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.ecls.s3

# Combine plots
library(cowplot)
Figure4 <- plot_grid(Fig.ecls.fk, Fig.ecls.sk, Fig.ecls.s1, Fig.ecls.s3, labels = c("A", "B", "C", "D"), nrow = 2) 
ggsave("/results/Figure_A2.png", scale = 2)

####################
# Bond and Lang    #
# ECLS w/ controls #
####################

## Plot ECLS Fall Kindergarten (with controls)##

# Read .csv into R
Table.ecls.fkc <- read.csv(file="/data/BL_ecls_fallk_controls.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.ecls.fkc <- Table.ecls.fkc$factor
log.factor.ecls.fkc <- log10(factor.ecls.fkc)
beta.ecls.fkc <- Table.ecls.fkc$beta1
low_bound.ecls.fkc <- Table.ecls.fkc$low_bound
up_bound.ecls.fkc <- Table.ecls.fkc$up_bound

# Organized data frame
FigureData.ecls.fkc<-rbind(data.frame(log.factor.ecls.fkc,Key="Point Estimate",low_bound.ecls.fkc=beta.ecls.fkc),
                           data.frame(log.factor.ecls.fkc,Key="Confidence Interval",low_bound.ecls.fkc),
                          data.frame(log.factor.ecls.fkc,Key="Confidence Interval",low_bound.ecls.fkc=up_bound.ecls.fkc))

# Make plot
library(ggplot2)
Fig.ecls.fkc <- ggplot(FigureData.ecls.fkc)+
  geom_point(aes(log.factor.ecls.fkc, low_bound.ecls.fkc, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Test Score Gap, \n Fall \n Kindergarten") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.ecls.fkc <- Fig.ecls.fkc + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.ecls.fkc

## Plot ECLS Spring Kindergarten (with controls) ##

# Read .csv into R
Table.ecls.skc <- read.csv(file="/data/BL_ecls_springk_controls.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.ecls.skc <- Table.ecls.skc$factor
log.factor.ecls.skc <- log10(factor.ecls.skc)
beta.ecls.skc <- Table.ecls.skc$beta1
low_bound.ecls.skc <- Table.ecls.skc$low_bound
up_bound.ecls.skc <- Table.ecls.skc$up_bound

# Organized data frame
FigureData.ecls.skc<-rbind(data.frame(log.factor.ecls.skc,Key="Point Estimate",low_bound.ecls.skc=beta.ecls.skc),
                           data.frame(log.factor.ecls.skc,Key="Confidence Interval",low_bound.ecls.skc),
                          data.frame(log.factor.ecls.skc,Key="Confidence Interval",low_bound.ecls.skc=up_bound.ecls.skc))

# Make plot
library(ggplot2)
Fig.ecls.skc <- ggplot(FigureData.ecls.skc)+
  geom_point(aes(log.factor.ecls.skc, low_bound.ecls.skc, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Test Score Gap, \n Spring \n Kindergarten") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.ecls.skc <- Fig.ecls.skc + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.ecls.skc

## Plot ECLS Spring First Grade (with controls) ##

# Read .csv into R
Table.ecls.s1c <- read.csv(file="/data/BL_ecls_spring1_controls.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.ecls.s1c <- Table.ecls.s1c$factor
log.factor.ecls.s1c <- log10(factor.ecls.s1c)
beta.ecls.s1c <- Table.ecls.s1c$beta1
low_bound.ecls.s1c <- Table.ecls.s1c$low_bound
up_bound.ecls.s1c <- Table.ecls.s1c$up_bound

# Organized data frame
FigureData.ecls.s1c<-rbind(data.frame(log.factor.ecls.s1c,Key="Point Estimate",low_bound.ecls.s1c=beta.ecls.s1c),
                           data.frame(log.factor.ecls.s1c,Key="Confidence Interval",low_bound.ecls.s1c),
                          data.frame(log.factor.ecls.s1c,Key="Confidence Interval",low_bound.ecls.s1c=up_bound.ecls.s1c))

# Make plot
library(ggplot2)
Fig.ecls.s1c <- ggplot(FigureData.ecls.s1c)+
  geom_point(aes(log.factor.ecls.s1c, low_bound.ecls.s1c, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Test Score Gap, \n Spring \n 1st Grade") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.ecls.s1c <- Fig.ecls.s1c + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.ecls.s1c

## Plot ECLS Spring Third Grade (with controls) ##

# Read .csv into R
Table.ecls.s3c <- read.csv(file="/data/BL_ecls_spring3_controls.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.ecls.s3c <- Table.ecls.s3c$factor
log.factor.ecls.s3c <- log10(factor.ecls.s3c)
beta.ecls.s3c <- Table.ecls.s3c$beta1
low_bound.ecls.s3c <- Table.ecls.s3c$low_bound
up_bound.ecls.s3c <- Table.ecls.s3c$up_bound

# Organized data frame
FigureData.ecls.s3c<-rbind(data.frame(log.factor.ecls.s3c,Key="Point Estimate",low_bound.ecls.s3c=beta.ecls.s3c),
                           data.frame(log.factor.ecls.s3c,Key="Confidence Interval",low_bound.ecls.s3c),
                          data.frame(log.factor.ecls.s3c,Key="Confidence Interval",low_bound.ecls.s3c=up_bound.ecls.s3c))

# Make plot
library(ggplot2)
Fig.ecls.s3c <- ggplot(FigureData.ecls.s3c)+
  geom_point(aes(log.factor.ecls.s3c, low_bound.ecls.s3c, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Test Score Gap, \n Spring \n 3rd Grade") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.ecls.s3c <- Fig.ecls.s3c + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.ecls.s3c

# Combine plots
library(cowplot)
Figure4 <- plot_grid(Fig.ecls.fkc, Fig.ecls.skc, Fig.ecls.s1c, Fig.ecls.s3c, labels = c("A", "B", "C", "D"), nrow = 2) 
ggsave("/results/Figure_A6.png", scale = 2)

####################
# Bond and Lang    #
# PIAT no controls #
####################

## Plot PIAT Kindergarten ##

# Read .csv into R
Table.piat.k <- read.csv(file="/data/BL_piat_k.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.piat.k <- Table.piat.k$factor
log.factor.piat.k <- log10(factor.piat.k)
beta.piat.k <- Table.piat.k$beta1
low_bound.piat.k <- Table.piat.k$low_bound
up_bound.piat.k <- Table.piat.k$up_bound

# Organized data frame
FigureData.piat.k<-rbind(data.frame(log.factor.piat.k,Key="Point Estimate",low_bound.piat.k=beta.piat.k),
                         data.frame(log.factor.piat.k,Key="Confidence Interval",low_bound.piat.k),
                          data.frame(log.factor.piat.k,Key="Confidence Interval",low_bound.piat.k=up_bound.piat.k))

# Make plot
library(ggplot2)
Fig.piat.k <- ggplot(FigureData.piat.k)+
  geom_point(aes(log.factor.piat.k, low_bound.piat.k, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Test Score Gap, \n Kindergarten") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.piat.k <- Fig.piat.k + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.piat.k

## Plot PIAT Grade 1 ##

# Read .csv into R
Table.piat.1 <- read.csv(file="/data/BL_piat_1.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.piat.1 <- Table.piat.1$factor
log.factor.piat.1 <- log10(factor.piat.1)
beta.piat.1 <- Table.piat.1$beta1
low_bound.piat.1 <- Table.piat.1$low_bound
up_bound.piat.1 <- Table.piat.1$up_bound

# Organized data frame
FigureData.piat.1<-rbind(data.frame(log.factor.piat.1,Key="Point Estimate",low_bound.piat.1=beta.piat.1),
                         data.frame(log.factor.piat.1,Key="Confidence Interval",low_bound.piat.1),
                         data.frame(log.factor.piat.1,Key="Confidence Interval",low_bound.piat.1=up_bound.piat.1))

# Make plot
library(ggplot2)
Fig.piat.1 <- ggplot(FigureData.piat.1)+
  geom_point(aes(log.factor.piat.1, low_bound.piat.1, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Test Score Gap, \n Grade 1") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.piat.1 <- Fig.piat.1 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.piat.1

## Plot PIAT Grade 2 ##

# Read .csv into R
Table.piat.2 <- read.csv(file="/data/BL_piat_2.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.piat.2 <- Table.piat.2$factor
log.factor.piat.2 <- log10(factor.piat.2)
beta.piat.2 <- Table.piat.2$beta1
low_bound.piat.2 <- Table.piat.2$low_bound
up_bound.piat.2 <- Table.piat.2$up_bound

# Organized data frame
FigureData.piat.2<-rbind(data.frame(log.factor.piat.2,Key="Point Estimate",low_bound.piat.2=beta.piat.2),
                         data.frame(log.factor.piat.2,Key="Confidence Interval",low_bound.piat.2),
                         data.frame(log.factor.piat.2,Key="Confidence Interval",low_bound.piat.2=up_bound.piat.2))

# Make plot
library(ggplot2)
Fig.piat.2 <- ggplot(FigureData.piat.2)+
  geom_point(aes(log.factor.piat.2, low_bound.piat.2, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Test Score Gap, \n Grade 2") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.piat.2 <- Fig.piat.2 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.piat.2

## Plot PIAT Grade 3 ##

# Read .csv into R
Table.piat.3 <- read.csv(file="/data/BL_piat_3.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.piat.3 <- Table.piat.3$factor
log.factor.piat.3 <- log10(factor.piat.3)
beta.piat.3 <- Table.piat.3$beta1
low_bound.piat.3 <- Table.piat.3$low_bound
up_bound.piat.3 <- Table.piat.3$up_bound

# Organized data frame
FigureData.piat.3<-rbind(data.frame(log.factor.piat.3,Key="Point Estimate",low_bound.piat.3=beta.piat.3),
                         data.frame(log.factor.piat.3,Key="Confidence Interval",low_bound.piat.3),
                         data.frame(log.factor.piat.3,Key="Confidence Interval",low_bound.piat.3=up_bound.piat.3))

# Make plot
library(ggplot2)
Fig.piat.3 <- ggplot(FigureData.piat.3)+
  geom_point(aes(log.factor.piat.3, low_bound.piat.3, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Test Score Gap, \n Grade 3") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.piat.3 <- Fig.piat.3 + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.piat.3

# Combine plots
library(cowplot)
Figure4 <- plot_grid(Fig.piat.k, Fig.piat.1, Fig.piat.2, Fig.piat.3, labels = c("A", "B", "C", "D"), nrow = 2) 
ggsave("/results/Figure_A7.png", scale = 2)

#####################
# AADR Prediction 1 #
# 0 - 5 Scale       #
#####################

## Plot A1.1 ##

# Read .csv into R
Pred1.1A <- read.csv(file="/data/pred1_1_A05.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor1.1A <- Pred1.1A$factor
log.factor1.1A <- log10(factor1.1A)
beta1.1A <- Pred1.1A$beta1
low_bound1.1A <- Pred1.1A$low_bound
up_bound1.1A <- Pred1.1A$up_bound

# Organized data frame
FigureData1.1A<-rbind(data.frame(log.factor1.1A,Key="Point Estimate",low_bound1.1A=beta1.1A),
                      data.frame(log.factor1.1A,Key="Confidence Interval",low_bound1.1A),
                     data.frame(log.factor1.1A,Key="Confidence Interval",low_bound1.1A=up_bound1.1A))

# Make plot
library(ggplot2)
Fig1.1A <- ggplot(FigureData1.1A)+
  geom_point(aes(log.factor1.1A, low_bound1.1A, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Turnover Rate \n (unemployment \n omitted)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
#labs(caption = "Note: Gray lines represent 95% confidence interval")
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig1.1A <- Fig1.1A + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig1.1A

## Plot 1.2 ##

# Read in .csv into R
Pred1.2A <- read.csv(file="/data/pred1_2_A05.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor1.2A <- Pred1.2A$factor
log.factor1.2A <- log10(factor1.2A)
beta1.2A <- Pred1.2A$beta1
low_bound1.2A <- Pred1.2A$low_bound
up_bound1.2A <- Pred1.2A$up_bound

# Organized data frame
FigureData1.2A<-rbind(data.frame(log.factor1.2A,Key="Point Estimate",low_bound1.2A=beta1.2A),
                      data.frame(log.factor1.2A,Key="Confidence Interval",low_bound1.2A),
                     data.frame(log.factor1.2A,Key="Confidence Interval",low_bound1.2A=up_bound1.2A))

# Make plot
library(ggplot2)
Fig1.2A <- ggplot(FigureData1.2A)+
  geom_point(aes(log.factor1.2A, low_bound1.2A, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Turnover Rate \n (with unemployment)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
#labs(caption = "Note: Gray lines represent 95% confidence interval")
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig1.2A <- Fig1.2A + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig1.2A

## Plot 1.3 ##

# Read .csv into R
Pred1.3A <- read.csv(file="/data/pred1_3_A05.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor1.3A <- Pred1.3A$factor
log.factor1.3A <- log10(factor1.3A)
beta1.3A <- Pred1.3A$beta1
low_bound1.3A <- Pred1.3A$low_bound
up_bound1.3A <- Pred1.3A$up_bound

# Organize data 
FigureData1.3A<-rbind(data.frame(log.factor1.3A,Key="Point Estimate",low_bound1.3A=beta1.3A),
                      data.frame(log.factor1.3A,Key="Confidence Interval",low_bound1.3A),
                     data.frame(log.factor1.3A,Key="Confidence Interval",low_bound1.3A=up_bound1.3A))

# Make plot
library(ggplot2)
Fig1.3A <- ggplot(FigureData1.3A)+
  geom_point(aes(log.factor1.3A, low_bound1.3A, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "log(Sigma)") + labs(y = "Job Turnover Rate \n (with unemployment \n and MSA controls)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(-1, 1, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
#labs(caption = "Note: Gray lines represent 95% confidence interval")
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig1.3A <- Fig1.3A + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig1.3A

## Combine figures 1.1 + 1.2 + 1.3 ##
library(cowplot)
Figure1 <- plot_grid(Fig1.1A, Fig1.2A, Fig1.3A, labels = c("A", "B", "C"), nrow = 2) 
ggsave("/results/Simulation_Figure1_A05.png", scale = 2)

#######################
# CDF Transformations #
#######################

######################
# AADR Prediction 1  #
# CDF Transformation #
######################

## Plot A1.1 ##

# Read .csv into R
Pred1.1cdf <- read.csv(file="/data/pred1_1_cdf.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor1.1cdf <- Pred1.1cdf$factor
beta1.1cdf <- Pred1.1cdf$beta1
low_bound1.1cdf <- Pred1.1cdf$low_bound
up_bound1.1cdf <- Pred1.1cdf$up_bound

# Organized data frame
FigureData1.1cdf<-rbind(data.frame(factor1.1cdf,Key="Point Estimate",low_bound1.1cdf=beta1.1cdf),
                        data.frame(factor1.1cdf,Key="Confidence Interval",low_bound1.1cdf),
                      data.frame(factor1.1cdf,Key="Confidence Interval",low_bound1.1cdf=up_bound1.1cdf))

# Make plot
library(ggplot2)
Fig1.1cdf <- ggplot(FigureData1.1cdf)+
  geom_point(aes(factor1.1cdf, low_bound1.1cdf, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "Sigma") + labs(y = "Job Turnover Rate \n (unemployment \n omitted)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 5, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
#labs(caption = "Note: Gray lines represent 95% confidence interval")
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig1.1cdf <- Fig1.1cdf + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig1.1cdf

## Plot 1.2 ##

# Read in .csv into R
Pred1.2cdf <- read.csv(file="/data/pred1_2_cdf.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor1.2cdf <- Pred1.2cdf$factor
beta1.2cdf <- Pred1.2cdf$beta1
low_bound1.2cdf <- Pred1.2cdf$low_bound
up_bound1.2cdf <- Pred1.2cdf$up_bound

# Organized data frame
FigureData1.2cdf<-rbind(data.frame(factor1.2cdf,Key="Point Estimate",low_bound1.2cdf=beta1.2cdf),
                        data.frame(factor1.2cdf,Key="Confidence Interval",low_bound1.2cdf),
                      data.frame(factor1.2cdf,Key="Confidence Interval",low_bound1.2cdf=up_bound1.2cdf))

# Make plot
library(ggplot2)
Fig1.2cdf <- ggplot(FigureData1.2cdf)+
  geom_point(aes(factor1.2cdf, low_bound1.2cdf, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "Sigma") + labs(y = "Job Turnover Rate \n (with unemployment)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 5, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
#labs(caption = "Note: Gray lines represent 95% confidence interval")
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig1.2cdf <- Fig1.2cdf + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig1.2cdf

## Plot 1.3 ##

# Read .csv into R
Pred1.3cdf <- read.csv(file="/data/pred1_3_cdf.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor1.3cdf <- Pred1.3cdf$factor
beta1.3cdf <- Pred1.3cdf$beta1
low_bound1.3cdf <- Pred1.3cdf$low_bound
up_bound1.3cdf <- Pred1.3cdf$up_bound

# Organize data 
FigureData1.3cdf<-rbind(data.frame(factor1.3cdf,Key="Point Estimate",low_bound1.3cdf=beta1.3cdf),
                        data.frame(factor1.3cdf,Key="Confidence Interval",low_bound1.3cdf),
                      data.frame(factor1.3cdf,Key="Confidence Interval",low_bound1.3cdf=up_bound1.3cdf))

# Make plot
library(ggplot2)
Fig1.3cdf <- ggplot(FigureData1.3cdf)+
  geom_point(aes(factor1.3cdf, low_bound1.3cdf, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "Sigma") + labs(y = "Job Turnover Rate \n (with unemployment \n and MSA controls)") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 5, 1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
#labs(caption = "Note: Gray lines represent 95% confidence interval")
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig1.3cdf <- Fig1.3cdf + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig1.3cdf

## Combine figures 1.1 + 1.2 + 1.3 ##
library(cowplot)
Figure1 <- plot_grid(Fig1.1cdf, Fig1.2cdf, Fig1.3cdf, labels = c("A", "B", "C"), nrow = 2) 
ggsave("Simulation_Figure1_cdf.png", scale = 2)

#######################
# Nunn and Wantchekon #
# Table 5 - CDF       #
#######################

## Plot 5.1 ##

# Read .csv into R
Table5.1cdf <- read.csv(file="/data/NW_Table5_1_cdf.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor5.1cdf <- Table5.1cdf$factor
beta5.1cdf <- Table5.1cdf$beta1
low_bound5.1cdf <- Table5.1cdf$low_bound
up_bound5.1cdf <- Table5.1cdf$up_bound

# Organized data frame
FigureData5.1cdf<-rbind(data.frame(factor5.1cdf,Key="Point Estimate",low_bound5.1cdf=beta5.1cdf),
                        data.frame(factor5.1cdf,Key="Confidence Interval",low_bound5.1cdf),
                     data.frame(factor5.1cdf,Key="Confidence Interval",low_bound5.1cdf=up_bound5.1cdf))

# Make plot
library(ggplot2)
Fig5.1cdf <- ggplot(FigureData5.1cdf)+
  geom_point(aes(factor5.1cdf, low_bound5.1cdf, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "Sigma") + labs(y = "ln(Exports/Area) \n on Trust of \n Relatives") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 1.5, 0.5)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig5.1cdf <- Fig5.1cdf + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig5.1cdf

## Plot 5.2 ##

# Read .csv into R
Table5.2cdf <- read.csv(file="/data/NW_Table5_2_cdf.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor5.2cdf <- Table5.2cdf$factor
beta5.2cdf <- Table5.2cdf$beta1
low_bound5.2cdf <- Table5.2cdf$low_bound
up_bound5.2cdf <- Table5.2cdf$up_bound

# Organized data frame
FigureData5.2cdf<-rbind(data.frame(factor5.2cdf,Key="Point Estimate",low_bound5.2cdf=beta5.2cdf),
                        data.frame(factor5.2cdf,Key="Confidence Interval",low_bound5.2cdf),
                     data.frame(factor5.2cdf,Key="Confidence Interval",low_bound5.2cdf=up_bound5.2cdf))

# Make plot
library(ggplot2)
Fig5.2cdf <- ggplot(FigureData5.2cdf)+
  geom_point(aes(factor5.2cdf, low_bound5.2cdf, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "Sigma") + labs(y = "ln(Exports/Area) \n on Trust of \n Neighbors") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 1.5, 0.5)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig5.2cdf <- Fig5.2cdf + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig5.2cdf

## Plot 5.3 ##

# Read .csv into R
Table5.3cdf <- read.csv(file="/data/NW_Table5_3_cdf.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor5.3cdf <- Table5.3cdf$factor
beta5.3cdf <- Table5.3cdf$beta1
low_bound5.3cdf <- Table5.3cdf$low_bound
up_bound5.3cdf <- Table5.3cdf$up_bound

# Organized data frame
FigureData5.3cdf<-rbind(data.frame(factor5.3cdf,Key="Point Estimate",low_bound5.3cdf=beta5.3cdf),
                        data.frame(factor5.3cdf,Key="Confidence Interval",low_bound5.3cdf),
                     data.frame(factor5.3cdf,Key="Confidence Interval",low_bound5.3cdf=up_bound5.3cdf))

# Make plot
library(ggplot2)
Fig5.3cdf <- ggplot(FigureData5.3cdf)+
  geom_point(aes(factor5.3cdf, low_bound5.3cdf, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "Sigma") + labs(y = "ln(Exports/Area) \n on Trust of \n Local Council") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 1.5, 0.5)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig5.3cdf <- Fig5.3cdf + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig5.3cdf

## Plot 5.4 ##

# Read .csv into R
Table5.4cdf <- read.csv(file="/data/NW_Table5_4_cdf.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor5.4cdf <- Table5.4cdf$factor
beta5.4cdf <- Table5.4cdf$beta1
low_bound5.4cdf <- Table5.4cdf$low_bound
up_bound5.4cdf <- Table5.4cdf$up_bound

# Organized data frame
FigureData5.4cdf<-rbind(data.frame(factor5.4cdf,Key="Point Estimate",low_bound5.4cdf=beta5.4cdf),
                        data.frame(factor5.4cdf,Key="Confidence Interval",low_bound5.4cdf),
                     data.frame(factor5.4cdf,Key="Confidence Interval",low_bound5.4cdf=up_bound5.4cdf))

# Make plot
library(ggplot2)
Fig5.4cdf <- ggplot(FigureData5.4cdf)+
  geom_point(aes(factor5.4cdf, low_bound5.4cdf, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "Sigma") + labs(y = "ln(Exports/Area) \n on Intra-group \n Trust") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 1.5, 0.5)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig5.4cdf <- Fig5.4cdf + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig5.4cdf

## Plot 5.5 ##

# Read .csv into R
Table5.5cdf <- read.csv(file="/data/NW_Table5_5_cdf.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor5.5cdf <- Table5.5cdf$factor
beta5.5cdf <- Table5.5cdf$beta1
low_bound5.5cdf <- Table5.5cdf$low_bound
up_bound5.5cdf <- Table5.5cdf$up_bound

# Organized data frame
FigureData5.5cdf<-rbind(data.frame(factor5.5cdf,Key="Point Estimate",low_bound5.5cdf=beta5.5cdf),
                        data.frame(factor5.5cdf,Key="Confidence Interval",low_bound5.5cdf),
                     data.frame(factor5.5cdf,Key="Confidence Interval",low_bound5.5cdf=up_bound5.5cdf))

# Make plot
library(ggplot2)
Fig5.5cdf <- ggplot(FigureData5.5cdf)+
  geom_point(aes(factor5.5cdf, low_bound5.5cdf, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "Sigma") + labs(y = "ln(Exports/Area) \n on Inter-group \n Trust") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 1.5, 0.5)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig5.5cdf <- Fig5.5cdf + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig5.5cdf

# Combine plots
library(cowplot)
Figure4 <- plot_grid(Fig5.1cdf, Fig5.2cdf, Fig5.3cdf, Fig5.4cdf, Fig5.5cdf, labels = c("A", "B", "C", "D", "E"), nrow = 3) 
ggsave("/results/Figure_5.png", scale = 2)

#######################
# Bond and Lang       #
# ECLS no controls    #
# CDF Transformations #
####################### 

## Plot ECLS Fall Kindergarten ##

# Read .csv into R
Table.ecls.fkcdf <- read.csv(file="/data/BL_ecls_fallk_cdf.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.ecls.fkcdf <- Table.ecls.fkcdf$factor
beta.ecls.fkcdf <- Table.ecls.fkcdf$beta1
low_bound.ecls.fkcdf <- Table.ecls.fkcdf$low_bound
up_bound.ecls.fkcdf <- Table.ecls.fkcdf$up_bound

# Organized data frame
FigureData.ecls.fkcdf<-rbind(data.frame(factor.ecls.fkcdf,Key="Point Estimate",low_bound.ecls.fkcdf=beta.ecls.fkcdf),
                             data.frame(factor.ecls.fkcdf,Key="Confidence Interval",low_bound.ecls.fkcdf),
                          data.frame(factor.ecls.fkcdf,Key="Confidence Interval",low_bound.ecls.fkcdf=up_bound.ecls.fkcdf))

# Make plot
library(ggplot2)
Fig.ecls.fkcdf <- ggplot(FigureData.ecls.fkcdf)+
  geom_point(aes(factor.ecls.fkcdf, low_bound.ecls.fkcdf, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "Sigma") + labs(y = "Test Score Gap, \n Fall \n Kindergarten") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 90, 10)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.ecls.fkcdf <- Fig.ecls.fkcdf + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.ecls.fkcdf

## Plot ECLS Spring Kindergarten ##

# Read .csv into R
Table.ecls.skcdf <- read.csv(file="/data/BL_ecls_springk_cdf.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.ecls.skcdf <- Table.ecls.skcdf$factor
beta.ecls.skcdf <- Table.ecls.skcdf$beta1
low_bound.ecls.skcdf <- Table.ecls.skcdf$low_bound
up_bound.ecls.skcdf <- Table.ecls.skcdf$up_bound

# Organized data frame
FigureData.ecls.skcdf<-rbind(data.frame(factor.ecls.skcdf,Key="Point Estimate",low_bound.ecls.skcdf=beta.ecls.skcdf),
                             data.frame(factor.ecls.skcdf,Key="Confidence Interval",low_bound.ecls.skcdf),
                          data.frame(factor.ecls.skcdf,Key="Confidence Interval",low_bound.ecls.skcdf=up_bound.ecls.skcdf))

# Make plot
library(ggplot2)
Fig.ecls.skcdf <- ggplot(FigureData.ecls.skcdf)+
  geom_point(aes(factor.ecls.skcdf, low_bound.ecls.skcdf, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "Sigma") + labs(y = "Test Score Gap, \n Spring \n Kindergarten") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 90, 10)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.ecls.skcdf <- Fig.ecls.skcdf + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.ecls.skcdf

## Plot ECLS Spring First Grade ##

# Read .csv into R
Table.ecls.s1cdf <- read.csv(file="/data/BL_ecls_spring1_cdf.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.ecls.s1cdf <- Table.ecls.s1cdf$factor
beta.ecls.s1cdf <- Table.ecls.s1cdf$beta1
low_bound.ecls.s1cdf <- Table.ecls.s1cdf$low_bound
up_bound.ecls.s1cdf <- Table.ecls.s1cdf$up_bound

# Organized data frame
FigureData.ecls.s1cdf<-rbind(data.frame(factor.ecls.s1cdf,Key="Point Estimate",low_bound.ecls.s1cdf=beta.ecls.s1cdf),
                             data.frame(factor.ecls.s1cdf,Key="Confidence Interval",low_bound.ecls.s1cdf),
                          data.frame(factor.ecls.s1cdf,Key="Confidence Interval",low_bound.ecls.s1cdf=up_bound.ecls.s1cdf))

# Make plot
library(ggplot2)
Fig.ecls.s1cdf <- ggplot(FigureData.ecls.s1cdf)+
  geom_point(aes(factor.ecls.s1cdf, low_bound.ecls.s1cdf, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "Sigma") + labs(y = "Test Score Gap, \n Spring \n 1st Grade") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 90, 10)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.ecls.s1cdf <- Fig.ecls.s1cdf + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.ecls.s1cdf

## Plot ECLS Spring Third Grade ##

# Read .csv into R
Table.ecls.s3cdf <- read.csv(file="/data/BL_ecls_spring3_cdf.csv", header = TRUE, sep = ",")

# Pull components of .csv file
factor.ecls.s3cdf <- Table.ecls.s3cdf$factor
beta.ecls.s3cdf <- Table.ecls.s3cdf$beta1
low_bound.ecls.s3cdf <- Table.ecls.s3cdf$low_bound
up_bound.ecls.s3cdf <- Table.ecls.s3cdf$up_bound

# Organized data frame
FigureData.ecls.s3cdf<-rbind(data.frame(factor.ecls.s3cdf,Key="Point Estimate",low_bound.ecls.s3cdf=beta.ecls.s3cdf),
                             data.frame(factor.ecls.s3cdf,Key="Confidence Interval",low_bound.ecls.s3cdf),
                          data.frame(factor.ecls.s3cdf,Key="Confidence Interval",low_bound.ecls.s3cdf=up_bound.ecls.s3cdf))

# Make plot
library(ggplot2)
Fig.ecls.s3cdf <- ggplot(FigureData.ecls.s3cdf)+
  geom_point(aes(factor.ecls.s3cdf, low_bound.ecls.s3cdf, color=Key)) +
  scale_color_grey() + theme_minimal() +
  labs(x = "Sigma") + labs(y = "Test Score Gap, \n Spring \n 3rd Grade") + theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 90, 10)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.75))
cutoff <- data.frame(yintercept=0, cutoff=factor(0))
Fig.ecls.s3cdf <- Fig.ecls.s3cdf + 
  geom_hline(mapping = NULL, data = NULL, size  = 1, yintercept=0, na.rm = FALSE, show.legend = NA)
Fig.ecls.s3cdf

# Combine plots
library(cowplot)
Figure4 <- plot_grid(Fig.ecls.fkcdf, Fig.ecls.skcdf, Fig.ecls.s1cdf, Fig.ecls.s3cdf, labels = c("A", "B", "C", "D"), nrow = 2) 
ggsave("/results/Figure_A3.png", scale = 2)
