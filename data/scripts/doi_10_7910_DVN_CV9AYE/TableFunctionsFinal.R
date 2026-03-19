##creating some table functions


source('TableWriteFunctions.R')

##writing some function to create the information necessary 

create_subs<- function(object){
	pis<- matrix(NA, nrow= nrow(object$thetas), ncol = ncol(object$thetas))
	for(z in 1:nrow(pis)){
		pis[z,]<- object$thetas[z,]/sum(object$thetas[z,])
		}
	return(pis)
	}
	
create_sups<- function(object, pis){
	sup_pis<- matrix(NA, nrow = nrow(object$thetas), ncol=ncol(object$cs))
	class_tops<- apply(object$cs, 1, which.max)
	for(z in 1:nrow(sup_pis)){
		for(g in 1:ncol(sup_pis)){
			sup_pis[z,g]<- sum(pis[z, which(class_tops==g)])
		}
		}
		return(sup_pis)
		}
		

##now let's put the topics together for each kind of topic

create_sub_topics<- function(object){
	tops<- matrix(NA, nrow=ncol(object$mus), ncol=10)
	for(z in 1:nrow(tops)){
		part1<- object$mus[,z] - apply(object$mus[,-z], 1, mean)
		tops[z,]<- rownames(object$gammas)[order(part1, decreasing=T)[1:10]]
		}
	return(tops)
	}

create_super_topics<- function(object){
	tops<- matrix(NA, nrow=ncol(object$gammas), ncol=10)
	for(z in 1:nrow(tops)){
		part1<- object$gammas[,z] - apply(object$gammas[,-z], 1, mean)
		tops[z,]<- rownames(object$gammas)[order(part1, decreasing=T)[1:10]]
		}
	return(tops)
	}	


create_output<- function(object, sup, sub){
	sub_pis<- create_subs(object)
	sup_pis<- create_sups(object, sub_pis)
	s_average<- apply(sup_pis, 2, mean)
	ords<- order(s_average, decreasing=T)
	
	super_tops<- create_super_topics(object)
	sub_tops<- create_sub_topics(object)
	topic.set<- apply(object$cs, 1, which.max)
	cat('\\begin{table}[hbt!]', '\n')
	caps<- paste(paste('\\caption{', paste(paste(sup, 'Coarse', sep =' '), paste(sub, 'Granular', sep=' '), sep=','), sep=''), '}', sep='')
	cat(caps, '\n')
	cat('\\begin{center}', '\n')
	cat('\\begin{footnotesize}', '\n')
	cat('\\begin{tabular}{lll}', '\n')
	table.func(ords, sup_pis, sub_pis, super_tops, sub_tops, topic.set)	
	cat('\\end{tabular}', '\n')
	cat('\\end{footnotesize}', '\n')
	cat('\\end{center}', '\n')
	cat('\\end{table}', '\n')
	}