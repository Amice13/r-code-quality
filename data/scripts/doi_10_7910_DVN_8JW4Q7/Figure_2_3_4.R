
library(ggplot2) # Figure 2-4 was created using plm version 1.6-6, ggplot 2 version 2.2.1 and cowplot version 0.9.1
library(cowplot)
library(plm)

# Read data from your directory
dat <- read.csv("C:/PATH/.../data.csv")

dat2 <- pdata.frame(dat, index = c("cod","year"))

# Load script for interaction terms with continuous variables

source("C:/PATH/.../interaction_plot.R")

est6 <- plm(lesocsppc~diflesocsppc+as.factor(year)+conc*concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag | 
              socsppc+as.factor(year)+conc*concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag
            , dat2, model = "pooling")
cova1 <- vcovBK(est6, cluster = "time")


est9 <- plm(lesocsppc~diflesocsppc+as.factor(year)+factor(concalt)*concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag | 
              socsppc+as.factor(year)+factor(concalt)*concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag
            , dat2, model = "pooling")
cova2 <- vcovBK(est9, cluster = "time")

konkeff <- interaction_plot_continuous(est6, effect = "conc", moderator = "concmigou", interaction = "conc:concmigou", varcov = cova1)
konkeff$effect <- (exp(konkeff$delta_1)-1)*100
rug <- model.frame(est6)

konkme <- ggplot(data = konkeff)+geom_line(aes(x=x_2, y=delta_1))+theme_bw()+geom_line(aes(x=x_2, y=lower_bound), color = "grey")+
  geom_line(aes(x=x_2, y=upper_bound), color = "grey")+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "Effect of Concordat on Social Expenditure", 
                                             x = "",
                                             y = "Effect of Concordat (First Difference)")+ geom_hline(yintercept = 0, size = 0.2)+
  geom_rug(data = rug, aes(x = concmigou))

histdia2 <- qplot(concmigou, data=rug, xlab = "% Outward Migration", xlim = c(0,220))

plot <- plot_grid(konkme,histdia2, nrow = 2, align = "v", rel_heights = c(2, 1))

# Save figure in your directory
ggsave("C:/PATH/.../konkeff.eps", height=7, width=10, units='in',device=cairo_ps)

# Load script for interaction terms with binary variables

source("C:/PATH/.../binary_interaction_plot.R")

migeff <- interaction_plot_binary(est6, effect = "concmigou", moderator = "conc", interaction = "conc:concmigou", varcov = cova1)
migeff$effect <- (exp(abs(migeff$delta_1)))*40

migme <- ggplot(data = migeff)+geom_point(aes(x=x_2, y=delta_1))+theme_bw()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound,x=x_2), width = .2,position=position_dodge(0.05))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "Effect of Outward Migration on Social Expenditure", x = "",
                                             y = "Marginal Effect of Outward Migration")+ geom_hline(yintercept = 0, size = 0.2)+
  scale_x_continuous(limits = c(0, 1.03),breaks = c(0,0.25,0.5,0.75,1))

histdia1 <- ggplot(data=rug, aes(rug$conc)) + 
  geom_histogram() + labs(x="Membership Concordat", y="Count")+scale_x_continuous(breaks=c(0,1)) 

plot <- plot_grid(migme,histdia1, nrow = 2, align = "v", rel_heights = c(2, 1))

# Save figure in your directory

ggsave("C:/PATH/.../migeff.eps", height=7, width=15, units='in',device=cairo_ps)

## Concordat different Reforms

konkeff23 <- interaction_plot_continuous(est9, effect = "factor(concalt)2", moderator = "concmigou", interaction = "factor(concalt)2:concmigou", varcov = cova2)
konkeff23$effect <- ((exp(konkeff23$delta_1)-1)*100)
konkeff37 <- interaction_plot_continuous(est9, effect = "factor(concalt)3", moderator = "concmigou", interaction = "factor(concalt)3:concmigou", varcov = cova2)
konkeff37$effect <- ((exp(konkeff37$delta_1)-1)*100)
konkeff59 <- interaction_plot_continuous(est9, effect = "factor(concalt)4", moderator = "concmigou", interaction = "factor(concalt)4:concmigou", varcov = cova2)
konkeff59$effect <- ((exp(konkeff59$delta_1)-1)*100)
rug <- model.frame(est9)

rug$`as.factor(year)` <- as.numeric(rug$`as.factor(year)`)
rug$concalt <- as.numeric(levels(rug$`factor(concalt)`))[rug$`factor(concalt)`]
rug23 <- subset(rug, rug$`as.factor(year)` < 8)
rug37 <- subset(rug, rug$`as.factor(year)` > 8 & rug$`as.factor(year)` <30)
rug59 <- subset(rug, rug$`as.factor(year)` > 29)

konkme23 <- ggplot(data = konkeff23)+geom_line(aes(x=x_2, y=delta_1))+theme_bw()+geom_line(aes(x=x_2, y=lower_bound), color = "grey")+
  geom_line(aes(x=x_2, y=upper_bound), color = "grey")+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "Effect of Concordat on Social Expenditure", 
                                             x = "% Outward Migration",
                                             y = "Effect of Concordat (First Difference)")+ geom_hline(yintercept = 0, size = 0.2)+
  geom_rug(data = rug23, aes(x = concmigou))+ expand_limits(y=c(-5,17))

konkme37 <- ggplot(data = konkeff37)+geom_line(aes(x=x_2, y=delta_1))+theme_bw()+geom_line(aes(x=x_2, y=lower_bound), color = "grey")+
  geom_line(aes(x=x_2, y=upper_bound), color = "grey")+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "Effect of Concordat on Social Expenditure", 
                                             x = "% Outward Migration",
                                             y = "Effect of Concordat (First Difference)")+ geom_hline(yintercept = 0, size = 0.2)+
  geom_rug(data = rug37, aes(x = concmigou))+ expand_limits(y=c(-5,17))

konkme59 <- ggplot(data = konkeff59)+geom_line(aes(x=x_2, y=delta_1))+theme_bw()+geom_line(aes(x=x_2, y=lower_bound), color = "grey")+
  geom_line(aes(x=x_2, y=upper_bound), color = "grey")+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "Effect of Concordat on Social Expenditure", 
                                             x = "% Outward Migration",
                                             y = "Effect of Concordat (First Difference)")+ geom_hline(yintercept = 0, size = 0.2)+
  geom_rug(data = rug59, aes(x = concmigou))+ expand_limits(y=c(-5,17))

histdia23 <- qplot(concmigou, data=rug23, xlab = "% Outward Migration", xlim = c(0,220))
histdia37 <- qplot(concmigou, data=rug37, xlab = "% Outward Migration", xlim = c(0,220))
histdia59 <- qplot(concmigou, data=rug59, xlab = "% Outward Migration", xlim = c(0,220))

plot <- plot_grid(konkme23,histdia23, nrow = 2, align = "v", rel_heights = c(2, 1))

# Save figure in your directory

ggsave("C:/PATH/.../konkeff23.eps", height=7, width=10, units='in',device=cairo_ps)

plot <- plot_grid(konkme37,histdia37, nrow = 2, align = "v", rel_heights = c(2, 1))

ggsave("C:/PATH/.../konkeff37.eps", height=7, width=10, units='in',device=cairo_ps)

plot <- plot_grid(konkme59,histdia59, nrow = 2, align = "v", rel_heights = c(2, 1))
plot
ggsave("C:/PATH/.../konkeff59.eps", height=7, width=10, units='in',device=cairo_ps)


migeff23 <- interaction_plot_binary(est9, effect = "concmigou", moderator = "factor(concalt)2", interaction = "factor(concalt)2:concmigou", varcov = cova2)

migeff37 <- interaction_plot_binary(est9, effect = "concmigou", moderator = "factor(concalt)3", interaction = "factor(concalt)3:concmigou", varcov = cova2)

migeff59 <- interaction_plot_binary(est9, effect = "concmigou", moderator = "factor(concalt)4", interaction = "factor(concalt)4:concmigou", varcov = cova2)

mieffall <- rbind(migeff23,migeff37,migeff59) 

mieffall <- mieffall[-c(3,5),]

mieffall$effect <- ((exp(mieffall$delta_1)-1)*100)

mieffall$conc <- c("No Concordat","Concordat 1923-36","Concordat 1937-58","Concordat 1959-75") 

mieffall$conc <- factor(mieffall$conc, levels = c("No Concordat","Concordat 1923-36","Concordat 1937-58","Concordat 1959-75"))

rug <- subset(rug, concalt!=1)
rug$concalt <- ifelse(rug$concalt >0, rug$concalt-1,rug$concalt)

miall <- ggplot(data = mieffall)+geom_point(aes(x=conc, y=delta_1))+theme_bw()+
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound,x=conc), width = 0,position=position_dodge(0.05))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "Effect of Outward Migration on Social Expenditure", x = "",
                                             y = "Marginal Effect of Outward Migration")+ geom_hline(yintercept = 0, size = 0.2)

histdia <- ggplot(data=rug, aes(rug$concalt)) + 
  geom_histogram() + labs(x="Membership Concordat", y="Count")+scale_x_continuous(breaks=c(0,1,2,3), limits = c(-.4,3.4)) 

plot <- plot_grid(miall,histdia, nrow = 2, align = "v", rel_heights = c(2, 1))
plot

# Save figure in your directory

ggsave("C:/PATH/.../mieffall.eps", height=7, width=15, units='in',device=cairo_ps)
