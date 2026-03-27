setwd(pathData)
load("spatial_temp_list.rda")

#spatial.temp.list

grid.sim <- c((138664:138673),(138664:138673)-720,(138664:138673)-720*2,(138664:138673)-720*3,(138664:138673)-720*4,(138664:138673)-720*5,(138664:138673)-720*6,(138664:138673)-720*7,(138664:138673)-720*8,(138664:138673)-720*9)

sample.sim <- spatial.temp.list[[2014]][as.character(grid.sim),as.character(grid.sim)]


####################    
#Number of troops in Grid
####################    
  
 
  
values <- apply(temp.data[,c("nlights_max.l1","No.troops.country","No.troops","afterUN.period.spell","duringUN.period.spell","best.ged","No.troops_sp","redeploy.period","time.since.last.best.gid","time.since.last.best.country")],2,FUN=mean)
	values[length(values)+1] <- values["duringUN.period.spell"]^2
		values <- rbind(values,values,values,values,values,values,values,values,values,values,values)
			colnames(values)[dim(values)[2]] <- "duringUN.period.spell.2"
				values[,"No.troops"] <- seq(from=min(temp.data$No.troops),to=max(temp.data$No.troops),length.out=11)
				values[,"No.troops.country"] <- seq(from=min(temp.data$No.troops),to=max(temp.data$No.troops),length.out=11)

#repeat for each sim grid
values

values.100 <- do.call("rbind", replicate(100, values, simplify = FALSE))

row.names(values.100) <- rep(as.character(grid.sim), each=11)

#135788 is pretty much in the middle of sample.sim 
values.100[,"No.troops"][rownames(values.100)!="135788"] <- 0

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



library(plotly)
library(processx)

predict.mat <- list()

for(i in 1:11){
predict.mat[[i]] <- matrix(data.plot$value[data.plot$Var2==i],10,10,byrow = TRUE)
}



axx <- list(

  title = "X-Prio-Grid"

)


axy <- list(

  title = "Y-Prio-Grid"

)


axz <- list(

  title = "Predicted Nightlight"

)


fig <- plot_ly(showscale = FALSE)

fig <- fig %>% add_surface(z = ~predict.mat[[2]],opacity = 0.4)
fig <- fig %>% add_surface(z = ~predict.mat[[5]],opacity = 0.3)
fig <- fig %>% add_surface(z = ~predict.mat[[10]],opacity = 0.2)
fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))


fig




