#Figure 4

#Please run figure_4.do in Stata before running this file


###################################################
setwd(pathData)
temp <- read_dta("nmax_dur.dta")

p <- ggplot(temp, aes(x=time_to_treat, y=point_estimate, ymin = lb_CI_95, ymax = up_CI_95)) +
		#ylim(-0.25,0.5)+
		geom_pointrange()+
			labs(title = "",
             x = "Time to and since Treatment",
             y = "Estimated Effect of Treatment")+ theme_bw() + geom_hline(yintercept=0,linetype="dashed")
 
setwd(pathFig)            
ggsave("did_nmax_dur.pdf",p,width = 10*1.63, height = 10, units = c("cm"))


###################################################
setwd(pathData)
temp <- read_dta("ncal_dur.dta")

p <- ggplot(temp, aes(x=time_to_treat, y=point_estimate, ymin = lb_CI_95, ymax = up_CI_95)) +
		#ylim(-0.25,0.5)+
		geom_pointrange()+
			labs(title = "",
             x = "Time to and since Treatment",
             y = "Estimated Effect of Treatment")+ theme_bw() + geom_hline(yintercept=0,linetype="dashed")
 
setwd(pathFig)             
ggsave("did_ncal_dur.pdf",p,width = 10*1.63, height = 10, units = c("cm"))

###################################################
setwd(pathData)
temp <- read_dta("nmean_dur.dta")

p <- ggplot(temp, aes(x=time_to_treat, y=point_estimate, ymin = lb_CI_95, ymax = up_CI_95)) +
		#ylim(-0.25,0.5)+
		geom_pointrange()+
			labs(title = "",
             x = "Time to and since Treatment",
             y = "Estimated Effect of Treatment")+ theme_bw() + geom_hline(yintercept=0,linetype="dashed")

setwd(pathFig)             
ggsave("did_nmean_dur.pdf",p,width = 10*1.63, height = 10, units = c("cm"))

###################################################
setwd(pathData)
temp <- read_dta("nmax_after_dur.dta")

p <- ggplot(temp, aes(x=time_to_treat, y=point_estimate, ymin = lb_CI_95, ymax = up_CI_95)) +
		#ylim(-0.25,0.5)+
		geom_pointrange()+
			labs(title = "",
             x = "Time to and since Treatment",
             y = "Estimated Effect of Treatment")+ theme_bw() + geom_hline(yintercept=0,linetype="dashed")

setwd(pathFig)              
ggsave("did_nmax_after_dur.pdf",p,width = 10*1.63, height = 10, units = c("cm"))


###################################################
setwd(pathData)
temp <- read_dta("ncal_after_dur.dta")

p <- ggplot(temp, aes(x=time_to_treat, y=point_estimate, ymin = lb_CI_95, ymax = up_CI_95)) +
		#ylim(-0.25,0.5)+
		geom_pointrange()+
			labs(title = "",
             x = "Time to and since Treatment",
             y = "Estimated Effect of Treatment")+ theme_bw() + geom_hline(yintercept=0,linetype="dashed")

setwd(pathFig)              
ggsave("did_ncal_after_dur.pdf",p,width = 10*1.63, height = 10, units = c("cm"))

###################################################
setwd(pathData)
temp <- read_dta("nmean_after_dur.dta")

p <- ggplot(temp, aes(x=time_to_treat, y=point_estimate, ymin = lb_CI_95, ymax = up_CI_95)) +
		#ylim(-0.25,0.5)+
		geom_pointrange()+
			labs(title = "",
             x = "Time to and since Treatment",
             y = "Estimated Effect of Treatment")+ theme_bw() + geom_hline(yintercept=0,linetype="dashed")

setwd(pathFig)             
ggsave("did_nmean_after_dur.pdf",p,width = 10*1.63, height = 10, units = c("cm"))

###################################################
setwd(pathData)
temp <- read_dta("nmax_after_all.dta")

p <- ggplot(temp, aes(x=time_to_treat, y=point_estimate, ymin = lb_CI_95, ymax = up_CI_95)) +
		#ylim(-0.25,0.5)+
		geom_pointrange()+
			labs(title = "",
             x = "Time to and since Treatment",
             y = "Estimated Effect of Treatment")+ theme_bw() + geom_hline(yintercept=0,linetype="dashed")

setwd(pathFig)          
ggsave("did_nmax_after_all.pdf",p,width = 10*1.63, height = 10, units = c("cm"))


###################################################
setwd(pathData)
temp <- read_dta("ncal_after_all.dta")

p <- ggplot(temp, aes(x=time_to_treat, y=point_estimate, ymin = lb_CI_95, ymax = up_CI_95)) +
		#ylim(-0.25,0.5)+
		geom_pointrange()+
			labs(title = "",
             x = "Time to and since Treatment",
             y = "Estimated Effect of Treatment")+ theme_bw() + geom_hline(yintercept=0,linetype="dashed")
 
setwd(pathFig)             
ggsave("did_ncal_after_all.pdf",p,width = 10*1.63, height = 10, units = c("cm"))

###################################################
setwd(pathData)
temp <- read_dta("nmean_after_all.dta")

p <- ggplot(temp, aes(x=time_to_treat, y=point_estimate, ymin = lb_CI_95, ymax = up_CI_95)) +
		#ylim(-0.25,0.5)+
		geom_pointrange()+
			labs(title = "",
             x = "Time to and since Treatment",
             y = "Estimated Effect of Treatment")+ theme_bw() + geom_hline(yintercept=0,linetype="dashed")

setwd(pathFig)              
ggsave("did_nmean_after_all.pdf",p,width = 10*1.63, height = 10, units = c("cm"))

