##########################################
#Code to make t-sne graphs
##########################################

#uses a dictionary of words (in paper, found collocates) to first make a Term Document Matrix
#then uses t-sne to visualize the high dimensional relationships within a set of two dimensional coordinates
#first run "makeTable" on vector of files (which must include the full path)
#then run "makeTSNEPlot" on the matrix created by "makeTable", which will output a plot, and return the coordinates



cleanTexts<-function(raw.text, dictionary){
  clean.text<-unlist(strsplit(raw.text, ""))
  clean.text<-tolower(clean.text)
  clean.text<-clean.text[which(clean.text %in% c(letters, LETTERS, " ", "-"))]
  clean.text<-paste(clean.text, collapse="")
  return(clean.text)
}

makeTable<-function(file.list, dictionary){
  all.files<-lapply(file.list, function(x) scan(x, what='character', sep=" ", quiet=T))
  all.files<-lapply(all.files, function(x) paste(x, collapse=" "))
  all.files<-lapply(all.files, function(x) cleanTexts(x))
  all.corpus<-Corpus(VectorSource(unlist(all.files)))
  all.dtm<-DocumentTermMatrix(all.corpus, control=list(wordLengths=c(1,Inf)))
  all.dtm<-all.dtm[,which(colnames(all.dtm) %in% dictionary)]
  all.dtm<-as.matrix(all.dtm)
  all.dtm<-t(all.dtm)
  return(all.dtm)
}


makeTSNEPlot<-function(data.table, perplex, max.iter, output.filename, group.table){
  tsne.plot<-Rtsne(data.table, perplexity=perplex, max_iter=max.iter)
  tsne.coords<-tsne.plot$Y
  tsne.table<-data.frame(row.names(data.table), tsne.coords, stringsAsFactors=F)
  colnames(tsne.table)<-c("Term", "tnse_X", "tsne_Y")
  group.table<-group.table[order(group.table$NObsScale, decreasing=T),]
  group.table<-group.table[-which(duplicated(group.table$Term)),]
  group.table<-group.table[which(group.table$Term %in% tsne.table$Term),]
  group.table<-group.table[order(group.table$Term),]
  tsne.table<-tsne.table[order(tsne.table$Term),]
  tsne.table$Group<-group.table$Group
  write.csv(tsne.table, output.filename, row.names=F)
  return(tsne.table)
}





