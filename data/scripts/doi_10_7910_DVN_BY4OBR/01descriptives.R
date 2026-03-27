library(ggplot2)
library(gridExtra)
library(reshape)
library(data.table)
library(xtable)
load("analysis-data-fullsample.Rdata")
load("analysis-data-bmr.Rdata")
theme.arg <-  theme(panel.background=element_blank(),
                    panel.border=element_rect(colour = "black", fill=NA),
                    axis.text=element_text(color="black"),
                    text=element_text(size=14))

#############################################
############### Full sample ###############
#############################################

plot.data <- data

## outside of major conflict
plot.data <- plot.data[!(major==1),]
plot.data$regime <- factor(plot.data$bmr_democracy)
levels(plot.data$regime) <- c("Autocracy", "Democracy")

## all journalists killed by state/unknown, by democracy (BMR measure)
all <- plot.data[!(is.na(regime)), list(state=sum(state),
                              unknown=sum(unknown)),
                        by=list(regime)]

all <- melt(all, id=c("regime"))                       
all <- all[order(regime),]

regime.plot <- ggplot(data=all, aes(x=regime, y=value)) +
   geom_bar(aes(fill=variable), alpha=.65, color="black",
            position="dodge",stat="identity", width=.3)+
   coord_flip() +
   scale_fill_manual("Perpetrator", labels=c("state",
                                             "unconfirmed"),
                     values=c("grey", "white"))+
   xlab("")+ ylab("Journalists killed") +
   theme.arg + theme(legend.position="bottom",
                     text = element_text(size=16)) 
regime.plot
ggsave(plot=regime.plot, file="fig1-BMR-perpetrator-barplot-stacked.png", 
       width=6, height=4, dpi = 500)


#############################################
############### Democracies only ###############
################################################ 
## regime and capital

plot.data.democ <- data.bmr
regime.by.capital <- plot.data.democ[, list(state=sum(state),
                                      unknown=sum(unknown),
                                      capital.state=sum(capital.state),
                                      capital.unknown=sum(capital.unknown))]

regime.by.capital[, nocapital.state := state-capital.state]
regime.by.capital[, nocapital.unknown := unknown-capital.unknown]

regime.by.capital <- melt(regime.by.capital, id=c("state", "unknown"))

regime.by.capital$capital <- "capital"
regime.by.capital$capital[grep("nocapital", regime.by.capital$variable)] <-
   "nocapital"

regime.by.capital$perp <- "State"
regime.by.capital$perp[grep("unknown", regime.by.capital$variable)] <- "Unconfirmed"

regime.cap.plot <- ggplot(data=regime.by.capital, aes(x=perp, y=value)) +
       geom_bar(aes(fill=capital), alpha=.65, color="black",
                position="dodge", stat="identity", width=.4)  +
    scale_fill_manual("Killed in captial?\n\n", labels=c("yes", "no"),
                              values=c("grey", "white"))+
        xlab("")+ ylab("Journalists killed") +
   theme.arg + theme(legend.position="bottom")

## worked for what type of media?

regime.by.mediareach <- plot.data.democ[,
                       list(id="id", regional.media.state=sum(regional.media.state),
                            national.media.state=sum(national.media.state),
                            international.media.state=sum(international.media.state),
                            regional.media.unknown=sum(regional.media.unknown),
                            national.media.unknown=sum(national.media.unknown),
                            international.media.unknown=sum(international.media.unknown)
                            )]

regime.by.mediareach <- melt(regime.by.mediareach, id="id")

regime.by.mediareach$reach <- "regional"
regime.by.mediareach$reach[grep("^national", regime.by.mediareach$variable)] <- "national"
regime.by.mediareach$reach[grep("international", regime.by.mediareach$variable)] <- "international"

regime.by.mediareach$perp <- "State"
regime.by.mediareach$perp[grep("unknown", regime.by.mediareach$variable)] <- "Unconfirmed"
regime.reach.plot <- ggplot(data=regime.by.mediareach,
                            aes(x=perp, y=value)) +
       geom_bar(aes(fill=reach), alpha=.65, color="black",
                position="dodge", stat="identity", width=.4)+
    scale_fill_manual("Worked for\nwhat type\nof media?", labels=c("international", "national", "regional"),
                              values=c("black", "grey", "white"))+
        xlab("")+ ylab("Journalists killed") +
  guides(fill = guide_legend(nrow = 2))+
   theme.arg + theme(legend.position="bottom")


fig2 <- grid.arrange(regime.cap.plot, regime.reach.plot, nrow = 1)
ggsave(plot=fig2, file="fig2-visibility-reach-democracies.png", 
       width=8, height=4, dpi = 500)

#############################################
#############################################
#### timeline of journalist killings included in analysis
#############################################
#############################################

journ.timeline <- plot.data.democ[, list(State=sum(state), Unconfirmed=sum(unknown),
                                         num.obs = sum(stateunknown.d)), by=list(year=year)]
journ.timeline <- melt(journ.timeline, id=c("year", "num.obs"))
journ.timeline$num.obs <- ifelse(journ.timeline$variable=="State", journ.timeline$num.obs, NA)

time.plot <- ggplot(data = journ.timeline, aes(x=year, y=value, fill=variable)) +
  geom_bar(position = "stack", stat = "identity", color="black") +
  scale_fill_manual("Perpetrator", values=c("grey","white")) +
  #geom_text(aes(label=num.obs, y=5))+
  xlab("") +ylab("Count")+
  theme.arg+
  theme(legend.position = "bottom")
time.plot
ggsave(plot=time.plot, file="figA1-journ-over-time.pdf", width=7, height=5)






