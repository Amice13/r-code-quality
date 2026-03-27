rm(list = ls())
library(btergm)
library(ggplot2)
library(dplyr)


################################################################
## Figure 7
################################################################
load('Data/make_tergm.RData')
#Put model estimates into temporary data.frames:
modelmake_tergm <- data.frame(Variable = c("Edges(Intercept)", "Popularity", 
                                           "Sociality", "Reciprocity",
                                           "Transitivity",
                                          "Distance in State Preferences",
                                          "Distance in Democracy",
                                          "Alliance",
                                          "Minimum Distance(log)",
                                          "Trade Imports(log)",
                                          "Difference in Population (log)",
                                          "Difference in GDP/Capita (log)",
                                          "Difference in Military Expenditure", 
                                          "Democracy(j)", "Population (log,j)",
                                          "GDP/Capita(j)", "UNSC P5(j)", "Civil War(j)",
                                          "Democracy(i)", "Population (log,i)",
                                          "GDP/Capita(i)", "UNSC P5(i)", "Civil War(i)", 
                                          "Tie Stability"),
                             Coefficient = summary(make_mod)[,1],
                             CIL = summary(make_mod)[,2],
                             CIU = summary(make_mod)[,3])

modelmake_tergm <- modelmake_tergm %>% 
        mutate(labelchn = c("截距", "欢迎度", 
                            "社会化", "对等性",
                            "传递性",
                            "外交立场差距",
                            "民主选举差异",
                            "同盟",
                            "最短距离(对数)",
                            "贸易进口(对数)",
                            "人口数量差距 (对数)",
                            "人均GDP差距 (对数)",
                            "军费开支比例差距", 
                            "民主选举(j)", "人口 (对数,j)",
                            "人均GDP(j)", "安理会常任理事国(j)", "内战(j)",
                            "民主选举(i)", "人口 (对数,i)",
                            "人均GDP(i)", "安理会常任理事国(i)", "内战(i)", 
                            "联系稳定性"))
# make var order the same as in the model

modelmake_tergm$Variable <- factor(modelmake_tergm$Variable, levels = unique(rev(modelmake_tergm$Variable)))
modelmake_tergm$labelchn <- factor(modelmake_tergm$labelchn, levels = unique(rev(modelmake_tergm$labelchn)))

# create sig variable to give different colors to significant/insignificant coeffs
modelmake_tergm$sig <- 1
modelmake_tergm$sig[modelmake_tergm$CIL<0 & modelmake_tergm$CIU>0] <- 0
modelmake_tergm$sig <- factor(modelmake_tergm$sig)

# Plot (B&W)
ggplot(modelmake_tergm, aes(x=labelchn, fill = sig, colour = sig)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(ymin = CIL, ymax = CIU), colour = "black")+ 
  geom_point(aes(x = labelchn, y = Coefficient), shape = 21, size = 1.2) +
  coord_flip() + theme_bw() + 
  scale_fill_manual(values = c("white", "black")) +
  scale_colour_manual(values = c("black", "black")) +
  geom_vline(xintercept=c(6.5, 11.5, 19.5), size=.25) +
  labs(x = "", y = "")+
  theme(legend.position="none",text = element_text(size=14,family ='KaiTi'))+ 
  annotate("text", x = 5, y = 2, label = "发送者层次变量", size=2.7,family = "KaiTi") +
  annotate("text", x = 10, y = 2, label = "接受者层次变量", size=2.7,family = "KaiTi") +
  annotate("text", x = 16.5, y = 2, label = "双边层次变量", size=2.7,family = "KaiTi") +
  annotate("text", x = 22.5, y = 2, label = "网络效应", size=2.7,family = "KaiTi") +
  theme(axis.text.y = element_text(size=7.6)) +
  theme(axis.text.x = element_text(size=7.6)) + labs(x = "")
ggsave("figs/makevisit_tergm.pdf", width=6.5, height=5.2)


################################################################
## Figure 8b
################################################################
load('Data/host_tergm.RData')
#Put model estimates into temporary data.frames:
modelhost_tergm <- data.frame(Variable = c("Edges(Intercept)", "Popularity", 
                                           "Sociality", "Reciprocity",
                                           "Transitivity",
                                           "Distance in State Preferences",
                                           "Distance in Democracy",
                                           "Alliance",
                                           "Minimum Distance(log)",
                                           "Trade Imports(log)",
                                           "Difference in Population (log)",
                                           "Difference in GDP/Capita (log)",
                                           "Difference in Military Expenditure", 
                                           "Democracy(j)", "Population (log,j)",
                                           "GDP/Capita(j)", "UNSC P5(j)", "Civil War(j)",
                                           "Democracy(i)", "Population (log,i)",
                                           "GDP/Capita(i)", "UNSC P5(i)", "Civil War(i)", 
                                           "Tie Stability"),
                              Coefficient = summary(host_mod)[,1],
                              CIL = summary(host_mod)[,2],
                              CIU = summary(host_mod)[,3])

modelhost_tergm <- modelhost_tergm %>% 
  mutate(labelchn = c("截距", "欢迎度", 
                      "社会化", "对等性",
                      "传递性",
                      "外交立场差距",
                      "民主选举差异",
                      "同盟",
                      "最短距离(对数)",
                      "贸易进口(对数)",
                      "人口数量差距 (对数)",
                      "人均GDP差距 (对数)",
                      "军费开支比例差距", 
                      "民主选举(j)", "人口 (对数,j)",
                      "人均GDP(j)", "安理会常任理事国(j)", "内战(j)",
                      "民主选举(i)", "人口 (对数,i)",
                      "人均GDP(i)", "安理会常任理事国(i)", "内战(i)", 
                      "联系稳定性"))
# make var order the same as in the model

modelhost_tergm$Variable <- factor(modelhost_tergm$Variable, levels = unique(rev(modelhost_tergm$Variable)))
modelhost_tergm$labelchn <- factor(modelhost_tergm$labelchn, levels = unique(rev(modelhost_tergm$labelchn)))

# create sig variable to give different colors to significant/insignificant coeffs
modelhost_tergm$sig <- 1
modelhost_tergm$sig[modelhost_tergm$CIL<0 & modelhost_tergm$CIU>0] <- 0
modelhost_tergm$sig <- factor(modelhost_tergm$sig)

# Plot (B&W)
ggplot(modelhost_tergm, aes(x=labelchn, fill = sig, colour = sig)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(ymin = CIL, ymax = CIU), colour = "black")+ 
  geom_point(aes(x = labelchn, y = Coefficient), shape = 21, size = 1.2) +
  coord_flip() + theme_bw() + 
  scale_fill_manual(values = c("white", "black")) +
  scale_colour_manual(values = c("black", "black")) +
  geom_vline(xintercept=c(6.5, 11.5, 19.5), size=.25) +
  labs(x = "", y = "")+
  theme(legend.position="none",text = element_text(size=14,family ='KaiTi'))+ 
  annotate("text", x = 5, y = 2, label = "发送者层次变量", size=2.7,family = "KaiTi") +
  annotate("text", x = 10, y = 2, label = "接受者层次变量", size=2.7,family = "KaiTi") +
  annotate("text", x = 16.5, y = 2, label = "双边层次变量", size=2.7,family = "KaiTi") +
  annotate("text", x = 22.5, y = 2, label = "网络效应", size=2.7,family = "KaiTi") +
  theme(axis.text.y = element_text(size=7.6)) +
  theme(axis.text.x = element_text(size=7.6)) + labs(x = "")
ggsave("figs/hostvisit_tergm.pdf", width=6.5, height=5.2)
