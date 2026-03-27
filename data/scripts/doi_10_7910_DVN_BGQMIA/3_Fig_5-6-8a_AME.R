library(amen)
library(ggplot2)
library(dplyr)
library(showtext)
library(ggthemes)
library(scales)
library(extrafont)
showtext_auto(enable = TRUE)
font_add('KaiTi', 'KaiTi.ttf')

load("Data/AME_makevisiList.RData")
load("Data/AME_hostvisiList.RData")
source("Rcode/ggCirc.R")


################################################################
## Figure 5
################################################################

### make a visit
beta_vc <- AME_makevisiList$VC%>% as.data.frame()
beta_vc$ve <- NULL
beta_vc <- data.frame(Coefficient = apply(beta_vc, 2, mean),
                      se = apply(beta_vc, 2, sd),
                      variable = colnames(beta_vc)) 

beta_vc <- beta_vc %>% 
  dplyr::mutate(labels = c('Within Sender Variance',
                           'Sender Receiver Covariance',
                           'Within Receiver Variance',
                           'Reciprocity') ,
                labelschn = c("发送者方差",
                              "发送者-接受者协方差",
                              "接受者方差",
                              "对等性"))

beta <- AME_makevisiList$BETA %>% as.data.frame()

make_df <- data.frame(Coefficient = apply(beta, 2, mean),
                      se = apply(beta, 2, sd),
                      variable = colnames(beta)) %>% 
  dplyr::mutate(labels = c("Intercept",
                           "Democracy(j)", "Population (log,j)",
                           "GDP/Capita(j)", "UNSC P5(j)", "Civil War(j)",
                           "Democracy(i)", "Population (log,i)",
                           "GDP/Capita(i)", "UNSC P5(i)", "Civil War(i)",
                           "Distance in State Preferences",
                           "Distance in Democracy",
                           "Alliance",
                           "Minimum Distance(log)",
                           "Trade Imports(log)",
                           "Difference in Population (log)",
                           "Difference in GDP/Capita (log)",
                           "Difference in Military Expenditure"),
                labelschn = c("截距",
                              "民主选举(j)", "人口 (对数,j)",
                              "人均GDP(j)", "安理会常任理事国(j)", "内战(j)",
                              "民主选举(i)", "人口 (对数,i)",
                              "人均GDP(i)", "安理会常任理事国(i)", "内战(i)",
                              "外交立场差距",
                              "民主选举差异",
                              "同盟",
                              "最短距离(对数)",
                              "贸易进口(对数)",
                              "人口数量差距 (对数)",
                              "人均GDP差距 (对数)",
                              "军费开支比例差距"))
make_df <- bind_rows(host_df,beta_vc)
# make var order the same as in the model

make_df$labels <- factor(make_df$labels, levels = unique(rev(make_df$labels)))
make_df$labelschn <- factor(make_df$labelschn, levels = unique(rev(make_df$labelschn)))

make_df <- make_df %>% 
  dplyr::mutate(CIL = Coefficient - 1.96*se,
                CIU = Coefficient +1.96*se) %>%
  dplyr::mutate(sig = ifelse(CIL < 0 & CIU > 0, 0, 1)) %>% 
  dplyr::mutate(sig = factor(sig))

# Plot (B&W)
ggplot(make_df, aes(x=labelschn, fill = sig, colour = sig)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(ymin = CIL, ymax = CIU), colour = "black")+ 
  geom_point(aes(x = labelschn, y = Coefficient), shape = 21, size = 1.2) +
  coord_flip() + theme_bw() +   labs(x = "", y = "")+
  scale_fill_manual(values = c("white", "black")) +
  scale_colour_manual(values = c("black", "black")) +
  geom_vline(xintercept=c(4.5, 12.5, 17.5), size=.25) +
  theme(legend.position="none",text = element_text(size=14,family ='KaiTi')) + 
  annotate("text", x = 20, y = -0.8, label = "接受者层次变量", size=2.7,family = "KaiTi") +
  annotate("text", x = 15, y = -0.8, label = "发送者层次变量", size=2.7,family = "KaiTi") +
  annotate("text", x = 8, y = -0.8, label = "双边层次变量", size=2.7,family = "KaiTi") +
  annotate("text", x = 2, y = -0.8, label = "SRRM参数", size=2.7,family = "KaiTi") +
  theme(axis.text.y = element_text(size=7.6)) +
  theme(axis.text.x = element_text(size=7.6)) 
ggsave("figs/makevisit_amen.pdf", width=6.5, height=5.2)



################################################################
## Figure 6
################################################################

#### cirplot
yrs <- as.character(seq(1979, 2014, by =1))
#################
load("Data/makevisiList.RData")
source("Rcode/listToArray.R")


yArr = listToArray(actors=sort(unique(unlist(lapply(makevisiList,rownames)))), 
                   Y=makevisiList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y

yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE)
diag(yArrSumm) = 0



################
ySimp = yArrSumm
uSimp = AME_hostvisiList$U
vSimp = AME_hostvisiList$V
library(RColorBrewer)
uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
facet_labeller = function(string){ TeX(string) }
set.seed(6886)

##get a unique actor list across time

ccode_list <- list()

for ( i in 1: length(yrs)){
  ccode_list[[i]] <-  rownames(makevisiList[[i]])
}    
##keep the common ccode      
ccode_list <- Reduce(intersect, ccode_list)


ySimp <- ySimp[colnames(ySimp)%in%ccode_list, rownames(ySimp)%in%ccode_list]
uSimp <- uSimp[rownames(uSimp) %in%ccode_list, ]
vSimp <- vSimp[rownames(vSimp) %in%ccode_list,]


circPlot=ggCirc(
  Y=ySimp, U=uSimp, V=vSimp, vscale=.7, force=.5,lsize=.4,
  uLabel='相同发送模式的国家',
  vLabel='相同接受模式的国家',
  removeIsolates=FALSE, showActLinks=FALSE) +
  scale_color_manual(values=uvCols) +
  facet_wrap(~eff, 
             ncol=2)+ 
  # labeller=as_labeller(facet_labeller, default = label_parsed) ) +
  theme_bw()+
  theme(
    legend.position='none',
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    axis.text=element_blank(),
    panel.border=element_blank(),
    text = element_text(size=14,family ='KaiTi'),
    panel.grid=element_blank(),
    strip.text.x = element_text(size = 12, color='white',
                                angle=0, hjust=.2),
    strip.background = element_rect(fill = "#525252", color='#525252'))+
  xlab("") + ylab("")

ggsave("figs/makingvisit_circPlot.pdf", plot = circPlot, width = 10, height = 5)


################################################################
## Figure 8-a
################################################################


beta_vc <- AME_hostvisiList$VC%>% as.data.frame()
beta_vc$ve <- NULL
beta_vc <- data.frame(Coefficient = apply(beta_vc, 2, mean),
                       se = apply(beta_vc, 2, sd),
                       variable = colnames(beta_vc)) 

beta_vc <- beta_vc %>% 
  dplyr::mutate(labels = c('Within Sender Variance',
                           'Sender Receiver Covariance',
                           'Within Receiver Variance',
                           'Reciprocity'),
                labelschn = c("发送者方差",
                              "发送者-接受者协方差",
                              "接受者方差",
                              "对等性"))

beta <- AME_hostvisiList$BETA %>% as.data.frame()

host_df <- data.frame(Coefficient = apply(beta, 2, mean),
                  se = apply(beta, 2, sd),
                  variable = colnames(beta)) 
host_df <- host_df %>% 
    dplyr::mutate(labels = c("Intercept",
                             "Democracy(j)", "Population (log,j)",
                             "GDP/Capita(j)", "UNSC P5(j)", "Civil War(j)",
                             "Democracy(i)", "Population (log,i)",
                             "GDP/Capita(i)", "UNSC P5(i)", "Civil War(i)",
                             "Distance in State Preferences",
                             "Distance in Democracy",
                             "Alliance",
                             "Minimum Distance(log)",
                             "Trade Imports(log)",
                             "Difference in Population (log)",
                             "Difference in GDP/Capita (log)",
                             "Difference in Military Expenditure"),
                  labelschn = c("截距",
                                "民主选举(j)", "人口 (对数,j)",
                                "人均GDP(j)", "安理会常任理事国(j)", "内战(j)",
                                "民主选举(i)", "人口 (对数,i)",
                                "人均GDP(i)", "安理会常任理事国(i)", "内战(i)",
                                "外交立场差距",
                                "民主选举差异",
                                "同盟",
                                "最短距离(对数)",
                                "贸易进口(对数)",
                                "人口数量差距 (对数)",
                                "人均GDP差距 (对数)",
                                "军费开支比例差距"))

host_df <- bind_rows(host_df,beta_vc)
# make var order the same as in the model

host_df$labels <- factor(host_df$labels, levels = unique(rev(host_df$labels)))
host_df$labelschn <- factor(host_df$labelschn, levels = unique(rev(host_df$labelschn)))


host_df <- host_df %>% 
    dplyr::mutate(CIL = Coefficient - 1.96*se,
                  CIU = Coefficient +1.96*se) %>%
    dplyr::mutate(sig = ifelse(CIL < 0 & CIU > 0, 0, 1)) %>% 
      dplyr::mutate(sig = factor(sig))

# Plot (B&W)

ggplot(host_df, aes(x=labelschn, fill = sig, colour = sig)) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
    geom_linerange(aes(ymin = CIL, ymax = CIU), colour = "black")+ 
    geom_point(aes(x = labelschn, y = Coefficient), shape = 21, size = 1.2) +
    coord_flip() + theme_bw() + 
    scale_fill_manual(values = c("white", "black")) +
    scale_colour_manual(values = c("black", "black")) +
    geom_vline(xintercept=c(4.5, 12.5, 17.5), size=.25) +
  labs(x = "", y = "")+
  theme(legend.position="none",text = element_text(size=14,family ='KaiTi'))+
    annotate("text", x = 20, y = -0.8, label = "接受者层次变量", size=2.7,family = "KaiTi") +
    annotate("text", x = 15, y = -0.8, label = "发送者层次变量", size=2.7,family = "KaiTi") +
    annotate("text", x = 8, y = -0.8, label = "双边层次变量", size=2.7,family = "KaiTi") +
    annotate("text", x = 2, y = -0.8, label = "SRRM参数", size=2.7,family = "KaiTi") +
    theme(axis.text.y = element_text(size=7.6)) +
    theme(axis.text.x = element_text(size=7.6)) 
ggsave("figs/hostvisit_amen.pdf", width=6.5, height=5.2)



########################################
### trace plot not in the paper
######################################

beta <- AME_makevisiList$BETA %>% as.data.frame()
trace_df <- beta %>% dplyr::mutate(index = row_number())
library(tidyr)
names(trace_df) <- c("Intercept",
                     "Democracy(j)", "Population (log,j)",
                     "GDP/Capita(j)", "UNSC P5(j)", "Civil War(j)",
                     "Democracy(i)", "Population (log,i)",
                     "GDP/Capita(i)", "UNSC P5(i)", "Civil War(i)",
                     "Distance in State Preferences",
                     "Distance in Democracy",
                     "Alliance",
                     "Minimum Distance(log)",
                     "Trade Imports(log)",
                     "Difference in Population (log)",
                     "Difference in GDP/Capita (log)",
                     "Difference in Military Expenditure","index")
trace_df_wd <- trace_df %>% gather(Vars, Value, `Intercept`:`Difference in Military Expenditure`)
trace_df_wd$Vars <- factor(trace_df_wd$Vars, levels = unique(rev(trace_df_wd$Vars)))


ggplot(trace_df_wd, aes(x = index, y = Value)) + 
  geom_line() + 
  facet_wrap(~Vars, ncol = 4, scales = "free") + 
  xlab('') + ylab('') + theme_bw()+
  theme(legend.position="none",
        legend.title=element_blank(),
        axis.text = element_text(size=10),
        text = element_text(size=10),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        strip.text.x = element_text(size = 8, color='white', angle=0, hjust=.5),
        strip.background = element_rect(fill = "#525252", color='#525252'),
        plot.title = element_text(hjust = .5, size = 8, face = "bold"))
ggsave("figs/AME_makevisit_trace.pdf", width = 14, height = 7)
#### host trace
beta <- AME_hostvisiList$BETA %>% as.data.frame()
trace_df <- beta %>% dplyr::mutate(index = row_number())
library(tidyr)
names(trace_df) <- c("Intercept",
                     "Democracy(j)", "Population (log,j)",
                     "GDP/Capita(j)", "UNSC P5(j)", "Civil War(j)",
                     "Democracy(i)", "Population (log,i)",
                     "GDP/Capita(i)", "UNSC P5(i)", "Civil War(i)",
                     "Distance in State Preferences",
                     "Distance in Democracy",
                     "Alliance",
                     "Minimum Distance(log)",
                     "Trade Imports(log)",
                     "Difference in Population (log)",
                     "Difference in GDP/Capita (log)",
                     "Difference in Military Expenditure","index")
trace_df_wd <- trace_df %>% gather(Vars, Value, `Intercept`:`Difference in Military Expenditure`)
trace_df_wd$Vars <- factor(trace_df_wd$Vars, levels = unique(rev(trace_df_wd$Vars)))


ggplot(trace_df_wd, aes(x = index, y = Value)) + 
  geom_line() + 
  facet_wrap(~Vars, ncol = 4, scales = "free") + 
  xlab('') + ylab('') + theme_bw()+
  theme(legend.position="none",
        legend.title=element_blank(),
        axis.text = element_text(size=10),
        text = element_text(size=10),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        strip.text.x = element_text(size = 8, color='white', angle=0, hjust=.5),
        strip.background = element_rect(fill = "#525252", color='#525252'),
        plot.title = element_text(hjust = .5, size = 8, face = "bold"))
ggsave("figs/AME_hostvisit_trace.pdf", width = 14, height = 7)

