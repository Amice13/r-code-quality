
####################  
# After
####################  
values <- apply(model.data[,c("nlights_mean.l1","No.troops.country","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country","afterUN.period.spell","duringUN.period.spell")],2,FUN=mean)
	values[length(values)+1] <- values["duringUN.period.spell"]^2
		values <- rbind(values,values,values,values,values,values,values,values,values,values)
			colnames(values)[dim(values)[2]] <- "duringUN.period.spell.2"
				values[,"afterUN.period.spell"] <- seq(from=min(model.data$afterUN.period.spell),to=max(model.data$afterUN.period.spell),length.out=10)


	source('sim_helper.R', chdir = TRUE)

					data.2$Var2 <- rep(seq(from=min(model.data$afterUN.period.spell),to=max(model.data$afterUN.period.spell),length.out=10),each=7)


n.p1 <- ggplot(data.2[data.2$Var1=="2.5%"|data.2$Var1=="97.5%"|data.2$Var1=="50%",], aes(x=Var2, y=value,fill=as.factor(Var1)))+
    geom_line(aes(linetype=Var1))+scale_linetype_manual(values=c("dashed","solid" ,"dashed"))+xlab("Years")+ ylab("Mean Nightlight in Grid")+
    theme(legend.position="none")


####################    
#During    
####################

values <- apply(model.data[,c("nlights_mean.l1","No.troops.country","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country","afterUN.period.spell","duringUN.period.spell")],2,FUN=mean)
	values[length(values)+1] <- values["duringUN.period.spell"]^2
		values <- rbind(values,values,values,values,values,values,values,values,values,values)
			colnames(values)[dim(values)[2]] <- "duringUN.period.spell.2"
				values[,"duringUN.period.spell"] <- seq(from=min(model.data$duringUN.period.spell),to=max(model.data$duringUN.period.spell),length.out=10)
				values[,"duringUN.period.spell.2"] <- seq(from=min(model.data$duringUN.period.spell),to=max(model.data$duringUN.period.spell),length.out=10)^2

	source('sim_helper.R', chdir = TRUE)

					data.2$Var2 <- rep(seq(from=min(model.data$duringUN.period.spell),to=max(model.data$duringUN.period.spell),length.out=10),each=7)


n.p2 <- ggplot(data.2[data.2$Var1=="2.5%"|data.2$Var1=="97.5%"|data.2$Var1=="50%",], aes(x=Var2, y=value,fill=as.factor(Var1)))+
    geom_line(aes(linetype=Var1))+scale_linetype_manual(values=c("dashed","solid" ,"dashed"))+xlab("Years")+ ylab("Mean Nightlight in Grid")+
    theme(legend.position="none")



setwd(pathFig)
ggsave("predMatchAfterNoCalibMeanNT.pdf",n.p1,width = 9, height = 9, units = c("cm"))
ggsave("predMatchDuringNoCalibMeanNT.pdf",n.p2, width = 9, height = 9, units = c("cm"))

