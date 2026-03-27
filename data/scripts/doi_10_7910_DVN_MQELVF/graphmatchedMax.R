####################    
#Number of troops in Grid
####################    
  
values <- apply(table.data[,c("lag.dv","No.troops.country","No.troops","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country","afterUN.period.spell","duringUN.period.spell")],2,FUN=mean)
	values[length(values)+1] <- values["duringUN.period.spell"]^2
		values <- rbind(values,values,values,values,values,values,values,values,values,values,values)
			colnames(values)[dim(values)[2]] <- "duringUN.period.spell.2"
				values[,"No.troops"] <- seq(from=min(table.data$No.troops),to=max(table.data$No.troops),length.out=11)

	source('sim_helper.R', chdir = TRUE)

		data.2$Var2 <- rep(seq(from=min(table.data$No.troops),to=max(table.data$No.troops),length.out=11),each=7)
		
n.p3 <- ggplot(data.2[data.2$Var1=="2.5%"|data.2$Var1=="97.5%"|data.2$Var1=="50%",], aes(x=Var2, y=value,fill=as.factor(Var1)))+
    geom_line(aes(linetype=Var1))+scale_linetype_manual(values=c("dashed","solid" ,"dashed"))+xlab("Number of Troops")+ ylab("Maximum Nightlight in Grid")+
    theme(legend.position="none")

setwd(pathFig)
ggsave("predMatchNumberMax.pdf",n.p3, width = 9, height = 9, units = c("cm"))

