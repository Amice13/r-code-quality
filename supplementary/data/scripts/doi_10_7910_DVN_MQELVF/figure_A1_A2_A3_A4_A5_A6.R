setwd(pathFig)
#plot outcome variable distribution total and by country.
p <- ggplot(model.data,aes(nlights_calib_mean_orig)) +geom_histogram()
p <- p + facet_wrap(~country_name,ncol=5,scales="free_y") + xlab('') + ylab('') + theme(legend.position="bottom")
ggsave("histMean.pdf", plot = p, width = 1.63*15, height = 15, units = "cm")

p <- ggplot(model.data,aes(nlights_max)) +geom_histogram()
p <- p + facet_wrap(~country_name,ncol=5,scales="free_y") + xlab('') + ylab('') + theme(legend.position="bottom")
ggsave("histMax.pdf", plot = p, width = 1.63*15, height = 15, units = "cm")


p <- ggplot(model.data,aes(nlights_mean)) +geom_histogram()
p <- p + facet_wrap(~country_name,ncol=5,scales="free_y") + xlab('') + ylab('') + theme(legend.position="bottom")
ggsave("histMeanNoCalib.pdf", plot = p, width = 1.63*15, height = 15, units = "cm")


p <- ggplot(model.data[mat2$matched==TRUE,],aes(nlights_calib_mean_orig)) +geom_histogram()
p <- p + facet_wrap(~country_name,ncol=5,scales="free_y") + xlab('') + ylab('') + theme(legend.position="bottom")
ggsave("histMeanMatch.pdf", plot = p, width = 1.63*15, height = 15, units = "cm")
    		
p <- ggplot(model.data[mat2$matched==TRUE,],aes(nlights_max)) +geom_histogram()
p <- p + facet_wrap(~country_name,ncol=5,scales="free_y") + xlab('') + ylab('') + theme(legend.position="bottom")
ggsave("histMaxMatch.pdf", plot = p, width = 1.63*15, height = 15, units = "cm")


p <- ggplot(model.data[mat2$matched==TRUE,],aes(nlights_mean)) +geom_histogram()
p <- p + facet_wrap(~country_name,ncol=5,scales="free_y") + xlab('') + ylab('') + theme(legend.position="bottom")
ggsave("histMeanNoCalibMatch.pdf", plot = p, width = 1.63*15, height = 15, units = "cm")
    	
