table.func<- function(ords, sub.pis, pis, super.top, sub.top, topic.set){
	##first we want to reorder the topics
	for(z in 1:len(ords)){
		keys<- super.top[ords[z], 1:10]
		nums<- mean(sub.pis[,ords[z]])
		ee<- paste(z, keys[1], sep='&')
		for(m in 2:10){
			ee<- paste(ee, keys[m], sep=',')
			}
		cat('\\hline', '\n')	
		cat(paste(paste(ee, round(nums,3), sep='&'), '\\\\'), '\n')
		cat('\\hline', '\n')
		other<- which(topic.set==ords[z])
		if(len(other)>1){
		ert<- apply(pis[, other], 2, mean)}
		if(len(other)==1){
			ert<- mean(pis[,other])}
		new_ords<- other[order(ert, decreasing=T)]
		props<- ert[order(ert, decreasing=T)]
		for(h in 1:len(new_ords)){
			n_key<- sub.top[new_ords[h],1:10]
			ff<- paste(h, n_key[1], sep='&')
			for(g in 2:10){
				ff<- paste(ff, n_key[g], sep =',')}
			cat(paste(paste(ff, round(props[h],3), sep='&'), '\\\\', sep=''), '\n')
			}
		cat('\\hline', '\n')
		}
		}