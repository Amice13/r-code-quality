

####################  
# After
####################  
values <- apply(matched.table.data.after[,c("lag.dv","No.troops.country","afterUN.period.spell","best.ged","No.troops_sp","time.since.last.best.gid","time.since.last.best.country","max.No.troops")],2,FUN=mean)
		values <- rbind(values,values,values,values,values,values,values,values,values,values)
				values[,"afterUN.period.spell"] <- seq(from=min(matched.table.data.after$afterUN.period.spell),to=max(matched.table.data.after$afterUN.period.spell),length.out=10)
					values[,"max.No.troops"] <- values[,"max.No.troops"] * values[,"afterUN.period.spell"]

	source('sim_helper.R', chdir = TRUE)

					data.2$Var2 <- rep(seq(from=min(matched.table.data.after$afterUN.period.spell),to=max(matched.table.data.after$afterUN.period.spell),length.out=10),each=7)


n.p1 <- ggplot(data.2[data.2$Var1=="2.5%"|data.2$Var1=="97.5%"|data.2$Var1=="50%",], aes(x=Var2, y=value,fill=as.factor(Var1)))+
    geom_line(aes(linetype=Var1))+scale_linetype_manual(values=c("dashed","solid" ,"dashed"))+xlab("Years")+ ylab("Maximum Nightlight in Grid")+
    theme(legend.position="none")



setwd(pathFig)
ggsave("predMatchAfterMaxAfter.pdf",n.p1,width = 9, height = 9, units = c("cm"))


