#Aim: create PR-curves, ROC for all models and regions 
list.of.packages <- c("readr","precrec","palettetown", "ggthemes", "ggplot2","ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)

#evaluation file paths
evalpath <- paste0(getwd(),"/results/evaluation/")
modelnames <- c("AR1","AR1X","AR2","GAM","RF","XGB")
#check prevalence of terrorism (percentage of positive week-cells) for each region
regionnames <- c("A", "B","C","K","L","J", "I","D","H","E","G","F","M")
#A NAm, B CAm, C SAm, K EAs, L SEAs, J SAs, I CAs, D: EU, H Rus, E: MENA, G SubAf, F WAf, M: Oce
#names for plots with correct order A-M
regionAM <- c("A", "B","C","D","E","F","G","H","I","J","K","L","M")
nbAM <- c(1,2,3,8,10,12,11,9,7,6,4,5,13)

#load prevalence data
load(paste0(getwd(),'/results/evaluation/prevalence.Rdata'))

#plot themes
sizeline <- 1 # size of line 
mytheme <-    
  ggplot2::theme(plot.margin = unit(c(0.1, 0.05, 0, 0.05), "cm"),
   #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
   line = element_line(size=sizeline),
   aspect.ratio=1)

#list of ROC and PR-curve plots
rocl <- prcl <- aucl <- list()
#loop over all regions

for (mm in 1:length(regionnames)) {
#make sure that the file names are properly saved (with _ before region number)
f2 <- list.files(evalpath, full.names = TRUE, pattern = paste0('_',mm,'\\.csv$'))
f2 <- f2[!grepl('extraction', f2)]
preds <- lapply(f2, readr::read_csv)

# #rename for plot
preds[[1]]$method <- modelnames[1]
preds[[2]]$method <- modelnames[2]
preds[[3]]$method <- modelnames[3]
preds[[4]]$method <- modelnames[4]
preds[[5]]$method <- modelnames[5]
preds[[6]]$method <- modelnames[6]
preds <- do.call(rbind, preds)
preds$modnames <-preds$method 

#join scores and labels for each method
score <- label <- list()
for (i in 1:length(modelnames))
{
pred <- preds[preds$modnames == modelnames[i],]
  score[[i]] <- pred$prediction
  label[[i]] <- pred$terror
}
myscores <- precrec::join_scores(score)
mylabels <- precrec::join_labels(label)
mmdat <- precrec::mmdata(myscores,mylabels,modnames = modelnames,
                           dsids = c(1, 2,3,4,5,6))
mscurves <- precrec::evalmod(mmdat,cb_alpha = 0.05)

#ROC
roc1 <- ggplot2::autoplot(mscurves,curvetype = c("ROC"),show_cb = TRUE)#,ret_grob=TRUE)
rocall <- roc1 +
  ggthemes::geom_rangeframe(color="black") + ggthemes::theme_tufte(base_size = 14)+
  ggplot2::scale_colour_manual(values = c(palettetown::pokepal('blastoise', spread = 5)[c(2, 1, 3)],
      palettetown::pokepal('charizard')[7],palettetown::pokepal('venusaur')[c(2, 3)])) + mytheme
rocl[[mm]] <- rocall
#PR curve

# Explicitly fortify mmcurves
prc1 <- fortify(mscurves, raw_curves = FALSE)
# Plot average Precision-Recall curve
prc1 <- subset(prc1, curvetype == "PRC")
prc1 <- ggplot(prc1, aes(x = x, y = y, ymin = ymin, ymax = ymax))
prcall <- prc1 + geom_smooth(aes(color = modname), stat = "identity",size = sizeline/2)+
  ggthemes::geom_rangeframe(color="black") +
  ggthemes::theme_tufte(base_size = 14)+
  ggplot2::scale_colour_manual(values = c(palettetown::pokepal('blastoise', spread = 5)[c(2, 1, 3)],
                                          palettetown::pokepal('charizard')[7],palettetown::pokepal('venusaur')[c(2, 3)])) +
  mytheme
 prcl[[mm]] <- prcall
  }
#END loop over regions

#plot ROC
mylegend <- ggpubr::get_legend(rocall,position = 'right')
plot.new()

rall <- cowplot::plot_grid(rocl[[nbAM[1]]],rocl[[nbAM[2]]],rocl[[nbAM[3]]],rocl[[nbAM[4]]], rocl[[nbAM[5]]],rocl[[nbAM[6]]],
                            rocl[[nbAM[7]]],rocl[[nbAM[8]]],rocl[[nbAM[9]]], rocl[[nbAM[10]]],rocl[[nbAM[11]]],rocl[[nbAM[12]]],rocl[[nbAM[13]]],
                            mylegend, NULL, align = "hv",labels =regionAM, nrow = 3,
                           rel_widths = c(1,1,1,1,1))#
ggsave(paste0(evalpath,"roc.pdf"),rall,dpi = 450,width = 14.5,height = 10)

#plot PR Curves
plot.new()
pall <- cowplot::plot_grid(prcl[[nbAM[1]]],prcl[[nbAM[2]]],prcl[[nbAM[3]]],prcl[[nbAM[4]]], prcl[[nbAM[5]]],prcl[[nbAM[6]]],
                           prcl[[nbAM[7]]],prcl[[nbAM[8]]],prcl[[nbAM[9]]], prcl[[nbAM[10]]],prcl[[nbAM[11]]],prcl[[nbAM[12]]],prcl[[nbAM[13]]],
                           mylegend, NULL, align = "hv",labels =regionAM, nrow = 3,
                           rel_widths = c(1,1,1,1,1))#
ggsave(paste0(evalpath,"pr.pdf"),pall,dpi = 450,width = 14.0,height = 10)
#END