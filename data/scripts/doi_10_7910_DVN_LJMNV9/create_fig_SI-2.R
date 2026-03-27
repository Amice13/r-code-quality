# This R-file is generating the Figure SI-2

setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")
library(ggplot2)
library(lemon)

load("Data/dtms_all_pages.rdata")

which( colnames(dtms_all_pages$dtm_ih_2g)=="יהודה.שומרון" )
yehuda_shomron_colnumber_ih = 2624173

which( colnames(dtms_all_pages$dtm_yed_2g)=="יהודה.שומרון" )
yehuda_shomron_colnumber_yed = 854579

which( colnames(dtms_all_pages$dtm_ih_1g)=="שטחים" )
shtachim_colnumber_ih = 18516

which( colnames(dtms_all_pages$dtm_yed_1g)=="שטחים" )
shtachim_colnumber_yed = 16505

ih_judea_samaria_mean = mean(dtms_all_pages$dtm_ih_2g[,yehuda_shomron_colnumber_ih])
yed_judea_samaria_mean = mean(dtms_all_pages$dtm_yed_2g[,yehuda_shomron_colnumber_yed])
ih_settlements_mean = mean(dtms_all_pages$dtm_ih_1g[,shtachim_colnumber_ih])
yed_settlements_mean = mean(dtms_all_pages$dtm_yed_1g[,shtachim_colnumber_yed])

judea_samaria_settlements = data.frame( "Newspaper" = rep(c("Israel Hayom", "Yediot Ahronot"),2),
                                        "Mean" = c(ih_judea_samaria_mean, yed_judea_samaria_mean, ih_settlements_mean, yed_settlements_mean),
                                        "Word" = c("Judea and Samaria", "Judea and Samaria", "Settlements", "Settlements"))

judea_samaria_plot = ggplot(judea_samaria_settlements[1:2,], aes(Word, Mean)) +
  geom_bar(aes(fill = Newspaper), width = 0.4, position = position_dodge(width=0.5), stat="identity") +
  theme_bw()+ theme(legend.position="top", legend.title =
                      element_blank(),axis.title.x=element_blank(),
                    axis.title.y=element_blank())

mitnachalim_plot = ggplot(judea_samaria_settlements[3:4,], aes(Word, Mean)) +
  geom_bar(aes(fill = Newspaper), width = 0.4, position = position_dodge(width=0.5), stat="identity") +
  theme_bw()+ theme(legend.position="top", legend.title =
                      element_blank(),axis.title.x=element_blank(),
                    axis.title.y=element_blank())

fig_SI2 = grid_arrange_shared_legend(judea_samaria_plot, mitnachalim_plot, ncol = 2, nrow = 1)
ggsave(file="Figures/Fig_SI-2.pdf", fig_SI2, width=9, height=4)
