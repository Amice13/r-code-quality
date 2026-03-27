
#Country Table for nightlight max
setwd(pathFig)
plot.country <- model.data %>%
					dplyr::group_by(country_name,year,un.yes,duringUN,afterUN) %>%
						dplyr::summarize(mean.light = mean(nlights_max),y.min = quantile(nlights_max,probs=c(0,0.05,0.95,1))[1],y.max = quantile(nlights_max,probs=c(0,0.05,0.95,1))[4],y.05 = quantile(nlights_max,probs=c(0,0.05,0.95,1))[2],y.95 = quantile(nlights_max,probs=c(0,0.05,0.95,1))[3])
							
							plot.country$un.status <- NA
								plot.country$un.status[plot.country$un.yes==0&plot.country$duringUN==0&plot.country$afterUN==0] <- "Never"
									plot.country$un.status[plot.country$un.yes==1&plot.country$duringUN==0&plot.country$afterUN==0] <- "Before"
										plot.country$un.status[plot.country$un.yes==1&plot.country$duringUN==1&plot.country$afterUN==0] <- "During"
											plot.country$un.status[plot.country$un.yes==1&plot.country$duringUN==0&plot.country$afterUN==1] <- "After"


for(i in unique(plot.country$country_name)){
p <- ggplot(plot.country[plot.country$country_name==i,],aes(year,mean.light,group=un.status,color=un.status,ymin=y.05,ymax=y.95)) + geom_pointrange(position=position_dodge(0.5),fatten=0.5) + xlab('Years') + ylab('Nightlights') + scale_colour_discrete(name="UN deployment")+theme(legend.position="bottom")#geom_pointrange(aes(ymin=y.min,ymax=y.max),position=position_dodge(0.5),fatten=0.1,linetype="dashed") 

file.temp <- paste(gsub(" ", "", i, fixed = TRUE),"countryMax.pdf",sep="")
ggsave(file.temp,plot=p, width = 7*1.65, height = 7, units = c("cm"))
}



#Country Table for nightlight calibrated mean

plot.country <- model.data %>%
					dplyr::group_by(country_name,year,un.yes,duringUN,afterUN) %>%
						dplyr::summarize(mean.light = mean(nlights_calib_mean_orig),y.min = quantile(nlights_calib_mean_orig,probs=c(0,0.05,0.95,1))[1],y.max = quantile(nlights_calib_mean_orig,probs=c(0,0.05,0.95,1))[4],y.05 = quantile(nlights_calib_mean_orig,probs=c(0,0.05,0.95,1))[2],y.95 = quantile(nlights_calib_mean_orig,probs=c(0,0.05,0.95,1))[3])
							
								plot.country$un.status <- NA
								plot.country$un.status[plot.country$un.yes==0&plot.country$duringUN==0&plot.country$afterUN==0] <- "Never"
									plot.country$un.status[plot.country$un.yes==1&plot.country$duringUN==0&plot.country$afterUN==0] <- "Before"
										plot.country$un.status[plot.country$un.yes==1&plot.country$duringUN==1&plot.country$afterUN==0] <- "During"
											plot.country$un.status[plot.country$un.yes==1&plot.country$duringUN==0&plot.country$afterUN==1] <- "After"
												#plot.country <- plot.country[,c("country_name","year","mean.light","y.min","y.max",,"un.status")]
							

for(i in unique(plot.country$country_name)){
p <- ggplot(plot.country[plot.country$country_name==i,],aes(year,mean.light,group=un.status,color=un.status,ymin=y.05,ymax=y.95)) + geom_pointrange(position=position_dodge(0.5),fatten=0.5) + xlab('Years') + ylab('Nightlights') + scale_colour_discrete(name="UN deployment")+theme(legend.position="bottom")#geom_pointrange(aes(ymin=y.min,ymax=y.max),position=position_dodge(0.5),fatten=0.1,linetype="dashed") +scale_colour_discrete(name="UN deployment",labels=c("After","Before","During","Never"))
file.temp <- paste(gsub(" ", "", i, fixed = TRUE),"countryMean.pdf",sep="")
ggsave(file.temp,plot=p, width = 7*1.65, height = 7, units = c("cm"))
}


#Country Table for nightlight mean

plot.country <- model.data %>%
					dplyr::group_by(country_name,year,un.yes,duringUN,afterUN) %>%
						dplyr::summarize(mean.light = mean(nlights_mean),y.min = quantile(nlights_mean,probs=c(0,0.05,0.95,1))[1],y.max = quantile(nlights_mean,probs=c(0,0.05,0.95,1))[4],y.05 = quantile(nlights_mean,probs=c(0,0.05,0.95,1))[2],y.95 = quantile(nlights_mean,probs=c(0,0.05,0.95,1))[3])
							
								plot.country$un.status <- NA
								plot.country$un.status[plot.country$un.yes==0&plot.country$duringUN==0&plot.country$afterUN==0] <- "Never"
									plot.country$un.status[plot.country$un.yes==1&plot.country$duringUN==0&plot.country$afterUN==0] <- "Before"
										plot.country$un.status[plot.country$un.yes==1&plot.country$duringUN==1&plot.country$afterUN==0] <- "During"
											plot.country$un.status[plot.country$un.yes==1&plot.country$duringUN==0&plot.country$afterUN==1] <- "After"
												#plot.country <- plot.country[,c("country_name","year","mean.light","y.min","y.max",,"un.status")]
							

for(i in unique(plot.country$country_name)){
p <- ggplot(plot.country[plot.country$country_name==i,],aes(year,mean.light,group=un.status,color=un.status,ymin=y.05,ymax=y.95)) + geom_pointrange(position=position_dodge(0.5),fatten=0.5) + xlab('Years') + ylab('Nightlights') + scale_colour_discrete(name="UN deployment")+theme(legend.position="bottom")#geom_pointrange(aes(ymin=y.min,ymax=y.max),position=position_dodge(0.5),fatten=0.1,linetype="dashed") +scale_colour_discrete(name="UN deployment",labels=c("After","Before","During","Never"))
file.temp <- paste(gsub(" ", "", i, fixed = TRUE),"countryMeanNoCalib.pdf",sep="")
ggsave(file.temp,plot=p, width = 7*1.65, height = 7, units = c("cm"))
}


