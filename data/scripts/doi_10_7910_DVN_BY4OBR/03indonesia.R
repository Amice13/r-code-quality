library(ggplot2)
library(reshape)
theme.arg <-  theme(panel.background=element_blank(),
                    panel.border=element_rect(colour = "black", fill=NA),
                    axis.text=element_text(color="black"),
                    text=element_text(size=14))
load("analysis-data-bmr.Rdata")
data <- data.bmr

#############################################
############### Data on Indonesia #########
#############################################
data.indo <- data[country == "Indonesia", list(year, 
                                         # `Regional Government Index`=v2xel_regelec, 
                                         `(elected) local Government Index`=v2xel_locelec, 
                                         `(elected) local offices relative power`=v2ellocpwr,
                                         stateunknown, armedconf,
                                         `Electoral Democracy`=v2x_polyarchy)] 
data.indo <- data.indo[order(year),]
data.indo[, stateunknown := ifelse(armedconf==1,0, stateunknown)]
plot.data <- melt(data.indo, id=c("year", "stateunknown", "armedconf"))

#############################################
############### Journalist killings ##########
#############################################
df <- data.table(read.csv("indonesia.csv", sep=",", stringsAsFactors=FALSE))
df$country <-  trimws(df$country, which = c("both"))
indo <- df[country=="Indonesia" & conflict.setting=="no" & year > 2003, list(year, name, region, city.town.village)]
indo <- indo[, list(region=paste(region, collapse="\n")), by=list(year)]

indo.plot <- ggplot(data=plot.data) +
  geom_line(aes(x=year, y=value, linetype=variable), size=1, color="black")+
  geom_segment(data = plot.data[stateunknown!=0, ], 
               aes(x = year, y = -.5, xend = year, yend = .2, size=factor(stateunknown)))+
  geom_text(data=indo, aes(x=year, y=.4, label=region))+
  scale_linetype_manual("", values = c("solid","dashed","dotted"))+
  scale_size_manual("Number of Journalists killed\noutside of armed conflict\n(and the regions they were killed in)", values = c(3,8))+
  xlab("") + ylab("Election indicators (V-Dem)")+
  coord_cartesian(ylim = c(0, 1.5))+
  scale_x_continuous(breaks = unique(plot.data$year) )+
  theme(legend.position = "bottom",
        legend.box="vertical", legend.margin=margin()) + theme.arg 
indo.plot
ggsave(plot=indo.plot, file="fig5-indonesia.png", 
       width = 9, height = 6, dpi = 500)

