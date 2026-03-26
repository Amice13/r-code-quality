##############
##############
##
##
## Replication file for Blaydes, Grimmer, and McQueen "Mirrors for Princes and Sultans" 
##
##
##
###############


##set working directory to downloaded folder
##initial preprocessing happens in PreProcess.py

tdm<- read.delim('/Users/justingrimmer/dropbox/mirror/AbsoluteFinalRound/Preprocessed/ShortTermDoc.csv', sep=',')
comb_final<- c('god', 'god', 'priest', 'minister', 'king', 'minister', 'state')
tdm<- tdm[,-which(colnames(tdm)=='X0')]



god_combine<- which(colnames(tdm)=='god')
gods<- apply(tdm[,which(colnames(tdm)=='lord_cap'|colnames(tdm)=='allah'| colnames(tdm)=='god')], 1, sum)

tdm2<- tdm
tdm2[,which(colnames(tdm2)=='god')]<- gods
tdm2<- tdm2[,-which(colnames(tdm2)=='lord_cap'|colnames(tdm2)=='allah')]

priest_combine<- which(colnames(tdm2)=='priest')

priests<- apply(tdm2[, c(priest_combine, which(colnames(tdm2)=='imam'))], 1, sum)


tdm3<- tdm2
tdm3[,which(colnames(tdm3)=='priest')]<- priests
tdm3<- tdm3[,-which(colnames(tdm3)=='imam')]


king_combine<- which(colnames(tdm3)=='king')
kings<- apply(tdm3[,which(colnames(tdm3)=='king'| colnames(tdm3)=='malik')], 1, sum)

tdm4<- tdm3
tdm4[,which(colnames(tdm4)=='king')]<- kings
tdm4<- tdm4[,-which(colnames(tdm4)=='malik')]


extra<- c('hanlfa', 'dirham', 'cabd', 'dimna', 'shafici', 'mu', 'oh', 'schanzabeh', 'culama', 'll', 'got', 'telemachu', 'ulyss', 'idomeneu', 'itali', 'ithaca', 'england', 'french', 'philip', 'adrastu', 'troy', 'thoma', 'paul', 'cicero', 'ye', 'whereof', 'loui', 'henri', 'israel', 'st', 'aforesaid') 

tdm5<- tdm4[,-which(colnames(tdm4) %in% extra)]

new_rems<- read.delim('NewMirrorRemTerms.csv', sep=',')




rem_term<- ifelse(as.character(new_rems[,3])=='AM'|as.character(new_rems[,4])=='X', 1, 0)
rem_words<- gsub('\\W', '', as.character(new_rems[which(rem_term==1),1]))

tdm6<- tdm5[, -which(colnames(tdm5) %in% rem_words)]

##putting together the synonym set.  


syns<- strsplit(as.character(new_rems[,2]), split=';')
syn_set<- list()
a<- 0 
for(z in 1:len(syns)){
	if(len(syns[[z]])>0){
	a<- a + 1
	syn_set[[a]]<- c(as.character(new_rems[z,1]), syns[[z]])
	}
}

syn_set2<- syn_set[-9]


##identifies the redundant synonym sets
combs<- list()
for(z in 1:len(syn_set2)){
	ert<- z
	for(k in 1:len(syn_set2)){
		ee<- any(syn_set2[[z]] %in% syn_set2[[k]])
		if(ee==T & k!= z){
			ert<- c(ert, k)}
		}
	combs[[z]]<- ert
	}

##we'll use this to put together the final synonym set
use<- 1:76
syn_final<- list()
a<- 0
for(z in 1:76){
	if(z %in% use){
		a<- a + 1
		ee<- combs[[z]]
		out<- unique(c(unlist(syn_set2[ee])))
		syn_final[[a]]<- out
		use<- use[-which(use %in% ee)]
		}
	}
syn_abs<- syn_final[-c(4, 8, 12, 15, 21, 24,25,  29, 33, 34, 35, 45)]

tdm7<- tdm6

for(z in 1:len(syn_abs)){
	ee<- syn_abs[[z]]
	p1<- which(colnames(tdm7)==ee[1])
	p2<- which(colnames(tdm7)==ee[2])
	if(len(p1)>0 & len(p2)>0){
		combs<- apply(tdm7[,c(p1, p2)], 1, sum)
		tdm7[,p1]<- combs
		tdm7<- tdm7[,-p2]
		}
		}
		
tdm8<- tdm7[,-which(colnames(tdm7) %in% c('chapter', 'year', 'franc', 'think'))]


un_docs<- unique(as.character(book_info[,1]))
authors<- matrix(NA, nrow=len(un_docs), ncol=2)
for(z in 1:nrow(authors)){
	authors[z,1]<- min(which(book_info[,1]==un_docs[z]))
	authors[z,2]<- max(which(book_info[,1]==un_docs[z]))
	}


##this is our final term document matrix
tdm8<- as.matrix(tdm8)

##norming the document for preparation in statistical model
normed<- tdm8

for(z in 1:nrow(normed)){
	normed[z,]<- normed[z,]/(sqrt(normed[z,]%*%normed[z,]))
	}


##loading the code for the nested topics
source('nestedtopics.R')


##Replicating the model run
set.seed(3991260)
g_60_c_4<- exp.agenda.vonmon(normed, authors, 4, 60, verbose=T, kappa= 1000)
save(g_60_c_4, file='Model_4_60_1000_1.RData')

##loading the data from the preprocessing, already completed
load('Normed.Rdata')


##loading functions for making tables
source('TableFunctionsFinal.R')

##loading the final run of the nested topic model. 
load('FinalModel.RData')

##defining a length function
len<- length

##############
##############
###Table 1
##############
##############

create_output(g_60_c_4, 4, 60)


#############
#############
##Figure 1
#############
#############


##first organizing the output to correspond with the order in the table.
sub_pis<- create_subs(g_60_c_4)
sup_pis<- create_sups(g_60_c_4, sub_pis)
s_average<- apply(sup_pis, 2, mean)
ords<- order(s_average, decreasing=T)

super_tops<- create_super_topics(g_60_c_4)
sub_tops<- create_sub_topics(g_60_c_4)
##let's put together the corresponding texts for each 
##putting together the topics for each section

##
sub_topic_doc<- apply(g_60_c_4$rs, 1, which.max)
super_topic<- apply(g_60_c_4$cs, 1, which.max)

sup_pis<- sup_pis[,ords]
super_tops<- super_tops[ords,]

new_super<- c()
for(z in 1:60){
	new_super[z]<- ords[super_topic[z]]
	}
	





diff1<- lm(sup_pis[,1]~christ)
diff2<- lm(sup_pis[,2]~christ)
diff3<- lm(sup_pis[,3]~christ)
diff4<- lm(sup_pis[,4]~christ)

##creating a plot to compare the super topics 

par(mar = c(5, 6, 3, 2))
plot(c(0,1)~c(0,1), xlim=c(0,0.5), ylim=c(1, 12), xlab='Proportion of Books', ylab = '', axes= F, frame.plot=T)
par(mar= c(5, 6, 3, 2))
par(las = 1)
axis(1, c(0, 0.1, 0.2, 0.3, 0.4, 0.5))
axis(2, c(1.5, 2, 2.5), c('', 'Topic 4', '')) 
axis(2, c(4.5, 5, 5.5), c('', 'Topic 3', ''))
axis(2, c(7.5, 8, 8.5), c('', 'Topic 2', ''))
axis(2, c(10.5, 11, 11.5), c('', 'Topic 1', ''))

create.points<- function(obj, y1, y2){
  part1<- obj$coef[1]
  part2<- obj$coef[1] + obj$coef[2]
  se1<- sqrt(diag(vcov(obj)))[1]
  se2<- sqrt(diag(vcov(obj))[1] + diag(vcov(obj))[2] + 2*vcov(obj)[1,2])
  arrows(part1 - 1.96*se1, y1, part1 + 1.96*se1, y1, len= 0, lwd = 2.5, col=gray(0.5))
  arrows(part2- 1.96*se2, y2, part2 + 1.96*se2, y2, len = 0 , col='black', lwd = 2.5)
  points(part1, y1, col=gray(0.5), pch = 20 )
  points(part2, y2, col='black', pch = 20 )
}

create.arrows<- function(point){
  arrows(-1, point, 1, point, lty = 2, col=gray(0.2), len = 0)
}

seqs<- c(1.5, 2.5, 4.5, 5.5, 7.5, 8.5, 10.5, 11.5)

for(z in seqs){
  
  create.arrows(z)
  
}



create.points(diff1, 10.5, 11.5)  
create.points(diff2,  7.5, 8.5 )  
create.points(diff3, 4.5, 5.5)  
create.points(diff4, 1.5, 2.5)

text(diff1$coef[1] + diff1$coef[2], 12, labels = 'Christian',col='black' )
text(diff1$coef[1], 11, labels= 'Muslim', col=gray(0.5))

title(main = 'Comparing Super Topic Emphases')


################
################
##Figure 2
################
################



par(mfrow=c(2,2))
par(mar=c(4, 4, 2, 2))
par(las = 1)
for(z in 1:4){
	if(z==2|z==4){
		par(mar = c(4, 2, 2, 2))
		}
	if(z==3){
		par(mar = c(4, 4, 2, 2))
		}
		##creating the over time plots
	plot(sup_pis[,z]~muslim_label[,3], pch='', cex=0.8, col=ifelse(muslim_label[,2]==1, 'black', 'red'), main = paste('Super ', z, sep=''), xlab = 'Year', ylab = 'Proportion', ylim=c(0,0.7))
	christ<- ifelse(muslim_label[,2]==1, 1, 0)
	if(z ==1){
	legend(c(600, 600), c(0.7, 0.6), pch = 20, col=c(gray(0.5), 'black'), legend = c('Muslim', 'Christian'), text.col = c(gray(0.5), 'black'), bty = 'n')}
	points(sup_pis[,z]~muslim_label[,3], pch=20, col=ifelse(muslim_label[,2]==1, 'black', grey(0.5)))

	lines(lowess(sup_pis[which(christ==1), z]~muslim_label[which(christ==1),3], iter = 0, f = 7/8), col='black', lwd = 3)
	lines(lowess(sup_pis[which(christ==0), z]~muslim_label[which(christ==0),3], iter = 0, f = 7/8), col=gray(0.5), lwd = 3)
	}



################
################
##Figure 3
################
################

return_pis<- function(z, y){
	subset<- which(new_super==z)
	ordered_subset<- subset[order(apply(sub_pis[,subset], 2, mean), decreasing=T)]
	use<- sub_pis[,ordered_subset[y]]
	return(use)
	}
	
one.one<- return_pis(1, 1)
three.one<- return_pis(3,1)
three.three<- return_pis(3,3)
three.five<- return_pis(3, 5)

create.plot<- function(obj, super, sub){

label<- paste(paste('Super', super), paste('Sub', sub) , sep = ', ')

plot(obj~muslim_label[,3], pch=20, cex=0.8, col=ifelse(muslim_label[,2]==1, 'black', gray(0.5)), main = label ,  xlab = 'Year', ylab = 'Proportion', ylim=c(0,0.2))
lines(lowess(obj[which(christ==1)]~muslim_label[which(christ==1),3], iter = 0, f = 7/8), col='black', lwd = 3)
lines(lowess(obj[which(christ==0)]~muslim_label[which(christ==0),3], iter = 0, f = 7/8), col=gray(0.5), lwd = 3)
}
one.two<- return_pis(1, 2)

create.plot(one.two, super = 1, sub = 2)


one.eight<- return_pis(1, 8)
create.plot(one.eight)



two.one<- return_pis(2, 1)
create.plot(two.one)


################
################
##Figure 4
################
################


par(mfrow=c(3,1))
par(las = 1)
par(mar = c(2, 4, 2, 2))
create.plot(one.one, 1, 1)
legend(c(575, 575), c(0.2, 0.15), legend = c('Muslim', 'Christian'), col=c(grey(0.5), 'black'), text.col=c(grey(0.5), 'black'), bty = 'n', pch = 20 )
par(mar = c(3, 4, 2.5, 2))
create.plot(one.two, 1, 2)
par(mar = c(3, 4, 2.5, 2))
#create.plot(one.eight, 1, 8)
#par(mar = c(4, 4, 2, 2))
create.plot(two.one, 2, 1)


three.one<- return_pis(3, 1)
three.three<- return_pis(3, 3)
three.five<- return_pis(3, 5)
three.four<- return_pis(3, 4)

create_plot_christ<- function(obj, super, sub){

label<- paste(paste('Super', super), paste('Sub', sub) , sep = ', ')
subs<- which(christ==1 & muslim_label[,3]>1158)
plot(obj[subs]~muslim_label[subs,3], pch=20, cex=0.8, main = label ,  xlab = 'Year', ylab = 'Proportion', ylim=c(0,0.15))
lines(lowess(obj[subs]~muslim_label[subs,3], iter = 0, f = 7/8), col='black', lwd = 3)
}

##the remaining three 
par(mfrow= c(2, 2))
par(mar = c(2, 4, 2, 2))
create.plot(three.one, 3, 1)
legend(c(575, 575), c(0.2, 0.15), legend = c('Muslim', 'Christian'), col=c(grey(0.5), 'black'), text.col=c(grey(0.5), 'black'), bty = 'n', pch = 20 )
create.plot(three.three, 3, 3)
create.plot(three.four, 3, 4)
create.plot(three.five, 3, 5)




