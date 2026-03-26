
setwd(pathData)
load("spatial_temp_list.rda")
library(data.table)

#spatial.temp.list

grid.sim <- c((138664:138673),(138664:138673)-720,(138664:138673)-720*2,(138664:138673)-720*3,(138664:138673)-720*4,(138664:138673)-720*5,(138664:138673)-720*6,(138664:138673)-720*7,(138664:138673)-720*8,(138664:138673)-720*9)

sample.sim <- spatial.temp.list[[2014]][as.character(grid.sim),as.character(grid.sim)]


####################    
#Number of troops in Grid
####################    
  
temp.List <- list() 
  
 for(j in 1:10){
 	
 	
values <- apply(temp.data[,c("nlights_max.l1","No.troops.country","No.troops","afterUN.period.spell","duringUN.period.spell","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country")],2,FUN=mean)
	values[length(values)+1] <- values["duringUN.period.spell"]^2
		values <- rbind(values,values,values,values,values,values,values,values,values,values,values)
			colnames(values)[dim(values)[2]] <- "duringUN.period.spell.2"
				values[,"No.troops"] <- seq(from=min(temp.data$No.troops),to=max(temp.data$No.troops),length.out=11)
				values[,"No.troops.country"] <- seq(from=min(temp.data$No.troops),to=max(temp.data$No.troops),length.out=11)*j

#repeat for each sim grid


values.100 <- do.call("rbind", replicate(100, values, simplify = FALSE))

row.names(values.100) <- rep(as.character(grid.sim), each=11)

temp.grid.a <- as.character((138664:138673)-720*4)
temp.grid.b <- as.character((138664:138673)-720*5)
#135788 is pretty much in the middle of sample.sim 
values.100[,"No.troops"][!rownames(values.100)%in%c(temp.grid.a[1:j],temp.grid.b[1:j])] <- 0

values.100 <- cbind(values.100,rep(1:11,100))
	colnames(values.100)[dim(values.100)[2]] <- "iteration"


for(i in 1:11){
	values.100[values.100[,"iteration"]==i,"No.troops_sp"] <- sample.sim%*%values.100[values.100[,"iteration"]==i,"No.troops"]
}



iteration.i <- values.100[,dim(values.100)[2]]

values <- values.100[,-dim(values.100)[2]]

setwd(pathR)
	source('sim_helper.R', chdir = TRUE)
	
	

data.2$Var2 <- rep(iteration.i,each=7)
data.2$grid <- rep(row.names(values.100),each=7)
		
data.plot <- data.2[data.2$Var1=="50%",]

data.plot$x.axis <- rep(rep(1:10,each=11),10)
data.plot$y.axis <- rep(rep(1:10,each=11),each=10)

temp.List[[j]] <- data.plot
}


temp.df <- data.frame(matrix(NA,11,11))

temp.df[,11] <- 1:11

for(j in 1:10){
temp.dt <- temp.List[[j]]	
	temp.dt <- temp.dt %>%
					dplyr::group_by(Var2) %>%
						dplyr::summarise(mean.value=mean(value))
	temp.df[,j] <- temp.dt[,2]					
}

names(temp.df) <- c("Grid 1","Grid 2","Grid 3","Grid 4","Grid 5","Grid 6","Grid 7","Grid 8","Grid 9","Grid 10","Troops")

long.df <- melt(setDT(temp.df), id.vars = c("Troops"))
long.df$Troops <- (long.df$Troops/10)-.1

p <- ggplot(long.df,aes(Troops,value,group=variable,color=variable)) + geom_line() + xlab('Percentile Troops') + ylab('Nightlights')#geom_pointrange(aes(ymin=y.min,ymax=y.max),position=position_dodge(0.5),fatten=0.1,linetype="dashed") +scale_colour_discrete(name="UN deployment",labels=c("After","Before","During","Never"))

p <- p + scale_colour_discrete(name="Number of Grids with Troops")

setwd(pathFig)
  ggsave("spatialLoop.pdf",plot=p, width = 15, height = 10, units = c("cm"))











