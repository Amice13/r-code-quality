# This R-file is generating the Figure SI-3

setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")
library(ggplot2)
library(lemon)

load("Data/dtms_all_pages.rdata")

which( colnames(dtms_all_pages$dtm_ih_1g)=="הסתנן")
histanen_colnumber_ih = 5499

which( colnames(dtms_all_pages$dtm_yed_1g)=="הסתנן" )
histanen_colnumber_yed = 4835

which( colnames(dtms_all_pages$dtm_ih_2g)=="ביקש.מקלט" )
bikesh_miklat_colnumber_ih = 686584

which( colnames(dtms_all_pages$dtm_yed_2g)=="ביקש.מקלט" )
bikesh_miklat_colnumber_yed = 234869


ih_mistanen_mean = mean(dtms_all_pages$dtm_ih_1g[, histanen_colnumber_ih])
yed_mistanen_mean = mean(dtms_all_pages$dtm_yed_1g[, histanen_colnumber_yed])
ih_mevakesh_miklat_mean = mean(dtms_all_pages$dtm_ih_2g[, bikesh_miklat_colnumber_ih])
yed_mevakesh_miklat_mean = mean(dtms_all_pages$dtm_yed_2g[, bikesh_miklat_colnumber_yed])

mistanen_mevakesh_miklat = data.frame( "Newspaper" = rep(c("Israel Hayom", "Yediot Ahronot"),2),
                                       "Mean" = c(ih_mistanen_mean, yed_mistanen_mean, ih_mevakesh_miklat_mean, yed_mevakesh_miklat_mean),
                                       "Word" = c("Invaders", "Invaders", "Asylum seekers", "Asylum seekers"))

mistanen_plot = ggplot(mistanen_mevakesh_miklat[1:2,], aes(Word, Mean)) +
  geom_bar(aes(fill = Newspaper), width = 0.4, position = position_dodge(width=0.5), stat="identity") +
  theme_bw()+ theme(legend.position="top", legend.title =
                      element_blank(),axis.title.x=element_blank(),
                    axis.title.y=element_blank())

mevakesh_miklat_plot = ggplot(mistanen_mevakesh_miklat[3:4,], aes(Word, Mean)) +
  geom_bar(aes(fill = Newspaper), width = 0.4, position = position_dodge(width=0.5), stat="identity") +
  theme_bw()+ theme(legend.position="top", legend.title =
                      element_blank(),axis.title.x=element_blank(),
                    axis.title.y=element_blank())

fig_SI3 = grid_arrange_shared_legend(mistanen_plot, mevakesh_miklat_plot, ncol = 2, nrow = 1)
ggsave(file="Figures/Fig_SI-3.pdf", fig_SI3, width=9, height=4)
