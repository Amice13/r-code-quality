library("ggplot2")
library("dplyr")
library("zoo")
library("reshape2")

### READ DATA ###
dfnew <- read.csv('results_bySize.csv')
dfnew$end_date <- as.Date(dfnew$end_date)

## Table 4
as.data.frame(summarise(group_by(dfnew, Size), 
                        mean(Concentration_Average),
                        mean(Similarity_Average),
                        sd(Concentration_Average),
                        sd(Similarity_Average)
))


colnames(dfnew)[13:16] <- c("Cash", "Securities", "Repo", "Loans")

pdf("Fig2.pdf", width = 6.5, height = 8)
dfm <- melt(dfnew, id.vars = c("end_date", "Size"), measure.vars = c("Cash", "Securities", "Repo", "Loans", "Trade", "Bkprem", "Ore", "Intan", "Idoa"))
dfm$Variable <- dfm$variable
print(ggplot(dfm, aes(end_date, value)) + 
        geom_vline(xintercept = as.Date("2000-03-10"), colour = "red") + 
        geom_vline(xintercept = as.Date("2000-10-09"), colour = "red") + 
        geom_vline(xintercept = as.Date("2007-08-07"), colour = "red") + 
        geom_vline(xintercept = as.Date("2008-10-03"), colour = "red") + 
        geom_vline(xintercept = as.Date("2010-07-10"), colour = "red") + 
        geom_vline(xintercept = as.Date("2020-03-01"), colour = "red") + 
        geom_vline(xintercept = as.Date("2023-03-08"), colour = "red") + 
        geom_line() +
        facet_grid(Variable~Size, scales = "free_y") + 
        theme_bw() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1)) +  
        guides(colour=guide_legend(nrow=2, byrow=TRUE)) + 
        ylab("Estimated Average Percentage Holdings") + xlab("Date")
)
dev.off()

#dfnew$Concentration_Average <- 3.2*dfnew$Concentration_Average
pdf("Fig3a.pdf", width = 6.5, height = 4.5)
print(ggplot(dfnew, aes(end_date, Concentration_Average)) + 
  # geom_smooth(se = FALSE) +
  facet_wrap(~Size, ncol = 1, scales = "fixed") + 
  geom_vline(xintercept = as.Date("2000-03-10"), colour = "red") + 
  geom_vline(xintercept = as.Date("2000-10-09"), colour = "red") + 
  geom_vline(xintercept = as.Date("2007-08-07"), colour = "red") + 
  geom_vline(xintercept = as.Date("2008-10-03"), colour = "red") + 
  geom_vline(xintercept = as.Date("2010-07-10"), colour = "red") + 
    geom_vline(xintercept = as.Date("2020-03-01"), colour = "red") + 
    geom_vline(xintercept = as.Date("2023-03-08"), colour = "red") + 
    geom_line() +
  theme_bw() + theme(legend.position = "bottom") + ylab("Estimated Average Concentration") + xlab("Date")
  )
dev.off()


pdf("Fig3b.pdf", width = 6.5, height = 4.5)
print(ggplot(dfnew, aes(end_date, Similarity_Average)) + 
  # geom_smooth(se = FALSE) +
  geom_vline(xintercept = as.Date("2000-03-10"), colour = "red") + 
  geom_vline(xintercept = as.Date("2000-10-09"), colour = "red") + 
  geom_vline(xintercept = as.Date("2007-08-07"), colour = "red") + 
  geom_vline(xintercept = as.Date("2008-10-03"), colour = "red") + 
  geom_vline(xintercept = as.Date("2010-07-10"), colour = "red") + 
  geom_vline(xintercept = as.Date("2020-03-01"), colour = "red") + 
    geom_vline(xintercept = as.Date("2023-03-08"), colour = "red") + 
    geom_line() +
    facet_wrap(~Size, ncol = 1, scales = "fixed") + 
    theme_bw() + theme(legend.position = "bottom") + ylab("Estimated Average Similarity") + xlab("Date")
  )
dev.off()

