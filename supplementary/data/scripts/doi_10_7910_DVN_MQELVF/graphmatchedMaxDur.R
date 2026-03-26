
####################    
#During    
####################

values <- apply(matched.table.data.dur[,c("lag.dv","No.troops.country","No.troops","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country","duringUN.period.spell")],2,FUN=mean)
	values[length(values)+1] <- values["duringUN.period.spell"]*values["No.troops"]
		values <- rbind(values,values,values,values,values,values,values,values,values,values)
		#	colnames(values)[dim(values)[2]] <- "duringUN.period.spell.2"
				values[,"duringUN.period.spell"] <- seq(from=min(matched.table.data.dur$duringUN.period.spell),to=max(matched.table.data.dur$duringUN.period.spell),length.out=10)
			values[,dim(values)[2]] <- values[,"duringUN.period.spell"] * values[,"No.troops"]

	source('sim_helper.R', chdir = TRUE)

					data.2$Var2 <- rep(seq(from=min(matched.table.data.dur$duringUN.period.spell),to=max(matched.table.data.dur$duringUN.period.spell),length.out=10),each=7)


n.p2 <- ggplot(data.2[data.2$Var1=="2.5%"|data.2$Var1=="97.5%"|data.2$Var1=="50%",], aes(x=Var2, y=value,fill=as.factor(Var1)))+
    geom_line(aes(linetype=Var1))+scale_linetype_manual(values=c("dashed","solid" ,"dashed"))+xlab("Years")+ ylab("Maximum Nightlight in Grid")+
    theme(legend.position="none")


####################    
#Number of troops in Grid
####################    
  
values <- apply(matched.table.data.dur[,c("lag.dv","No.troops.country","No.troops","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country","duringUN.period.spell")],2,FUN=mean)
values[length(values)+1] <- values["duringUN.period.spell"]*values["No.troops"]
	#values[length(values)+1] <- values["duringUN.period.spell"]^2
		values <- rbind(values,values,values,values,values,values,values,values,values,values,values)
		#	colnames(values)[dim(values)[2]] <- "duringUN.period.spell.2"
				values[,"No.troops"] <- seq(from=min(matched.table.data.dur$No.troops),to=max(matched.table.data.dur$No.troops),length.out=11)

	source('sim_helper.R', chdir = TRUE)

		data.2$Var2 <- rep(seq(from=min(matched.table.data.dur$No.troops),to=max(matched.table.data.dur$No.troops),length.out=11),each=7)
		
n.p3 <- ggplot(data.2[data.2$Var1=="2.5%"|data.2$Var1=="97.5%"|data.2$Var1=="50%",], aes(x=Var2, y=value,fill=as.factor(Var1)))+
    geom_line(aes(linetype=Var1))+scale_linetype_manual(values=c("dashed","solid" ,"dashed"))+xlab("Number of Troops")+ ylab("Maximum Nightlight in Grid")+
    theme(legend.position="none")


setwd(pathFig)
ggsave("predMatchDuringMaxDur.pdf",n.p2, width = 9, height = 9, units = c("cm"))
ggsave("predMatchNumberMaxDur.pdf",n.p3, width = 9, height = 9, units = c("cm"))

