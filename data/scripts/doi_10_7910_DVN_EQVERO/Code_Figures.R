

#### Figure 1 ####

mysample <- read_dta("Parallel_Trend.dta")

try <- mysample %>% group_by(treated, relative_year) %>% dplyr::summarise(R1=mean(R1, na.rm=T))

plot(try[try$treated==1,]$R1 ~ try[try$treated==1,]$relative_year, type='b', ylim=c(-0.3,0.3), col=2, lwd=2.5, cex=1.8, pch=17, lty=1,
     main="", ylab= list("Residualized Patent Count (log)", cex=1.5), xlab="")
lines(try[try$treated==0,]$R1 ~ try[try$treated==0,]$relative_year, type='b', col=3, lwd=2.5, cex=1.8, pch=15, lty=2)
legend("bottom", legend = c("Partisan Aligned Group", "Control Group"),
       col = c(2, 3), lty = 1:2, pch = c(17,15), cex = 1.5, lwd=2)

try <- mysample %>% group_by(treated, relative_year) %>% dplyr::summarise(R2=mean(R2, na.rm=T))

plot(try[try$treated==1,]$R2 ~ try[try$treated==1,]$relative_year, type='b', ylim=c(-0.3,0.3), col=2, lwd=2.5, cex=1.8, pch=17, lty=1,
     main="", ylab= list("Residualized Patent Citation (log)", cex=1.5), xlab="")
lines(try[try$treated==0,]$R2 ~ try[try$treated==0,]$relative_year, type='b', col=3, lwd=2.5, cex=1.8, pch=15, lty=2)
legend("bottom", legend = c("Partisan Aligned Group", "Control Group"),
       col = c(2, 3), lty = 1:2, pch = c(17,15), cex = 1.5, lwd=2)

try <- mysample %>% group_by(treated, relative_year) %>% dplyr::summarise(R3=mean(R3, na.rm=T))

plot(try[try$treated==1,]$R3 ~ try[try$treated==1,]$relative_year, type='b', ylim=c(-0.3,0.3), col=2, lwd=2.5, cex=1.8, pch=17, lty=1,
     main="", ylab= list("Residualized Patent Value (log)", cex=1.5), xlab="")
lines(try[try$treated==0,]$R3 ~ try[try$treated==0,]$relative_year, type='b', col=3, lwd=2.5, cex=1.8, pch=15, lty=2)
legend("bottom", legend = c("Partisan Aligned Group", "Control Group"),
       col = c(2, 3), lty = 1:2, pch = c(17,15), cex = 1.5, lwd=2)



#### Figure A1 ####

MYDATA <- read_dta("FIGURE_A1A.dta")

ggplot(MYDATA, aes(fill=factor(Y11, levels=c("No Leaning","Democratic","Republican")), y=N, x=YEAR )) + 
  theme(plot.title = element_text(lineheight=0, face="bold", hjust = 0.5, size = rel(1.2)), legend.position = "bottom") + 
  geom_bar(position="fill", stat="identity", width = 0.7) +
  labs(fill = "", y = 'Share', x = 'YEAR') +
  scale_fill_manual(values = c("grey", "blue", "red")) +
  scale_x_continuous(breaks = seq(1993, 2018, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))


MYDATA <- read_dta("FIGURE_A1B.dta")

ggplot(MYDATA, aes(fill=factor(Y11, levels=c("No Leaning","Democratic","Republican")), y=N, x=YEAR )) + 
  theme(plot.title = element_text(lineheight=0, face="bold", hjust = 0.5, size = rel(1.2)), legend.position = "bottom") + 
  geom_bar(position="fill", stat="identity", width = 0.7) +
  labs(fill = "", y = 'Share', x = 'YEAR') +
  scale_fill_manual(values = c("grey", "blue", "red")) +
  scale_x_continuous(breaks = seq(1993, 2018, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))



#### Figure A2 ####

MYDATA <- read_dta("FIGURE_A2A.dta")

mycolors <- c("No Leaning"="green", "Democratic"="blue", "Republican"="red")

ggplot(EXE_PARTISAN_PLOT_1, aes(x=CONG, y=V1, group=TYPE, color=TYPE)) +
  ggtitle("(a) Since 107th Congress") + 
  theme(plot.title = element_text(lineheight=0, face="bold", hjust = 0.5, size = rel(1.2)), legend.position = "bottom") + 
  geom_path() +
  geom_point() +
  coord_cartesian(ylim = c(-0.3, 0.5)) + 
  labs(x = 'Election Cycle', y = 'Partisanship of Executives') +
  scale_x_continuous(breaks = seq(107, 115, by = 1)) +
  scale_y_continuous(breaks = seq(-0.3, 0.5, by = 0.2)) +
  scale_color_manual(name="", values = mycolors)


MYDATA <- read_dta("FIGURE_A2B.dta")

mycolors <- c("No Leaning"="green", "Democratic"="blue", "Republican"="red")

ggplot(EXE_PARTISAN_PLOT_1, aes(x=CONG, y=V1, group=TYPE, color=TYPE)) +
  ggtitle("(b) Since 111th Congress") + 
  theme(plot.title = element_text(lineheight=0, face="bold", hjust = 0.5, size = rel(1.2)), legend.position = "bottom") + 
  geom_path() +
  geom_point() +
  coord_cartesian(ylim = c(-0.3, 0.5)) + 
  labs(x = 'Election Cycle', y = 'Partisanship of Executives') +
  scale_x_continuous(breaks = seq(111, 115, by = 1)) +
  scale_y_continuous(breaks = seq(-0.3, 0.5, by = 0.2)) +
  scale_color_manual(name="", values = mycolors)



