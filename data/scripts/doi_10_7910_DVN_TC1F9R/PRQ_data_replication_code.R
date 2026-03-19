require(tm)
require(proxy)
require(dplyr)
require(plyr)
require(stringr)
require(data.table)
require(quanteda)
require(readtext)
require(ggplot2)
require(igraph)
require(ergm)
require(devtools)
#install development version of NetworkInference package
devtools::install_github('desmarais-lab/NetworkInference')
require(NetworkInference)
require(RNewsflow)

#set to your working directory (where replication data is stored on your computer)
setwd("/replication_data_and_code")

#####################################
## 1. Load and Pre-Process Corpus ##
#####################################

#load corpus as RData
load("corpus_workspace.RData")

#remove whitespace, numbers, punctuation, stopwords, corpus specific stopwords, make lower case,
#stem words to their root, and transform "/" and "-" to spaces
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corp <- tm_map(corp, toSpace, "/")
corp <- tm_map(corp, toSpace, "-")
corp <- tm_map(corp,content_transformer(tolower))
corp<- tm_map(corp, removeWords, c("^http.*","waxman","markey",
                                   "american clean energy", "security act","aces","mr. chairman","united states","draft","bill",
                                   "chairman", "committee","look forward","cap", "trade","thank you","washington","legislation",
                                   "week","hello","house","lieberman","senator","representative","“","’",""))           #remove websites and other common words
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords('english'))       #remove common words
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, toSpace, '[[:punct:] ]+') #remove additional punctuation 
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, stemDocument)


########################################
## 2. Create Bigram Tokenizer and DTM ##
########################################
#the bigram tokenizer focuses on bigrams (two-word phrases) rather than single terms
BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

#create a document term matrix, from bigrams, for each document
#weight these bigrams according to the Term Frequency-Inverse Document Frequency weighting
dtm <- DocumentTermMatrix(corp,control=list(tokenize=BigramTokenizer, weighting = weightTfIdf)) 


#remove sparse terms (in less than 1% of of documents)
removeSparseTerms(dtm, 0.99)

#transform to matrix
dtm_mat <- as.matrix(dtm)


#remove botton 95% of bigrams (based on sum of TF-IDF scores across all bigrams) 
#by calculating sum for each column and removing those that fall below top 5%
#(this helps focus attention on most meaningful bigrams, or those that best distinguish
#between texts)
mn<-as.numeric(quantile(colSums(dtm_mat),0.95)) #identify top 5% of terms 

#subset to those above mean (or quartile)
dtm_mat<-dtm_mat[,colSums(dtm_mat)>mn]


#Replace TF-IDF score with date for temporal analysis using NetInf package
#extract date and add to matrix
dts <- as.Date(str_extract(rownames(dtm_mat), "\\d{2}-\\d{2}-\\d{2}"),"%m-%d-%y")

##replace tf-idf scores of 0 with NA's
dtm_mat[as.numeric(as.character(dtm_mat))==0]<-NA

#replace non-NA's with date on which term was used
#Exclude usages after 2011 (date range = 2001-2011)
for(i in 1:nrow(dtm_mat)){
  dtm_mat[i,] <- ifelse(is.na(dtm_mat[i,]) | dts[[i]] > as.Date("2011-12-31"), NA, as.character(dts[[i]]))
}

#remove columns with all NA's
dtm_mat <- dtm_mat[,colSums(is.na(dtm_mat))<nrow(dtm_mat)] #remove rows with all NA

#add actor names to matrix
dtm_mat <- as.matrix(cbind.data.frame(actor=unlist(meta(corp, "author")),dtm_mat))


#FIND FIRST USAGE OF TERM BY EACH GROUP
DTM<-as.data.frame(matrix(ncol=ncol(dtm_mat),nrow=length(unique(dtm_mat[,1]))))
colnames(DTM)<-colnames(dtm_mat)
DTM[,1]<-unique(dtm_mat[,1])

for(i in 2:ncol(dtm_mat)){
  d = aggregate(dtm_mat[,i] ~ dtm_mat[,1],FUN=min)
  DTM[,i]<- d[,2][match(DTM[,1],d[,1])]
}

#rename rows and remove actor name column
rownames(DTM)<-DTM[,1]
DTM$actor<-NULL

rm(list=c("d","dtm_mat","corp","dtm","dts","i","mn","toSpace","BigramTokenizer"))


########################################
## 3. Identify Cascades of Bigrams in DTM ##
########################################
#change class from character to date
DTM[ , 1:ncol(DTM)] <- lapply(DTM[ , 1:ncol(DTM)], function(x) as.Date(x, format="%Y-%m-%d"))

#include only terms used by at least 3 actors (netinf tracks diffusion of bigrams,
#which can't diffuse if used by only 1 actor; 3+ is a slightly more stringent indicator of importance)
used <- lapply(DTM,function(col) length(unique(col))>=3)
used <- data.frame(unlist(subset(used, used==TRUE)))

DTM <- subset(DTM, select = rownames(used))


#transform date to days since Jan 1 2001
func<- function(x) { difftime(x, as.Date("2001-01-01"), units="days")}
DTM<-as.data.frame(apply(DTM,MARGIN = 2, func))

#make sure there are no rows with all NA's, remove if so
table(rowSums(is.na(DTM))!=ncol(DTM)) #false = number w/ all NAs
DTM<-DTM[rowSums(is.na(DTM))!=ncol(DTM), ]


#deal with duplicate rows for David Vitter
t <- DTM[c(85,105),]
for(i in 1:ncol(t)){
  t[1,i] <- ifelse(is.na(t[1,i]), t[2,i], t[1,i])}
DTM <- DTM[-c(85,105),]
DTM <- rbind(DTM, t[2,])

#change MC names to short name, using vertex dataset
v<-read.csv("vertex_data.csv")
rownames(DTM)<-ifelse(nchar(rownames(DTM))>7,as.character(v$Short_Name[match(rownames(DTM),v$Name)]),rownames(DTM))

#create vector of row names
node_names <- unique(row.names(DTM))

##Calculate cascade of bigrams
cascades <- as_cascade_wide(DTM, node_names = node_names)
summary(cascades)


##### Figure 1 ####
#cascade of select bigrams
#add color
cascades$col<-as.character(v$Color[match(cascades$node_names,v$Short_Name)])

#get bigram names
cascade_ids <- names(cascades[[1]])

#select bigrams to plot (can change to any bigram in cascade_ids)
bigrams<-c("energi tax","climat chang")
for(i in bigrams){
  #find index for bigram
  t<-which(cascade_ids %in% i)
  
  #get cascade node names for this bigram, then add color
  y<-as.data.frame(cascades$cascade_nodes[t])
  colnames(y)<-"node_names"
  y$col<-as.character(v$Color[match(y$node_names,v$Short_Name)])
  
  Y<-as.vector(y$col)
  names(Y)<-y$node_names
  
  selection <- cascade_ids[t] 

  p<-plot(cascades, label_nodes = T, selection = i)+
    scale_color_manual(values=Y)+
    theme(legend.position = "none")+theme(axis.text.y = element_blank(),axis.text=element_text(size=12))+
    ggtitle(paste0("",i," Bigram"))+ylab("")+xlab("Days Since January 1st, 2001")
  
  print(p)}


########################################
## 4. Infer Diffusion Ties Using NetInf ##
########################################
#run netinf
result <- netinf(cascades, trans_mod = "exponential",  p_value_cutoff = 0.01)

#create network
g <- graph_from_data_frame(d = result[, 1:2])

#find out degree of each node, add to vertex data
edges<-as.data.frame(table(result[,1]))
v$out_degree<-as.numeric(edges$Freq[match(v$Short_Name,edges$Var1)])

#find various degrees, add to network
V(g)$out_degree<-degree(g,mode="out")
V(g)$in_degree<-degree(g,mode="in")
V(g)$degree<-degree(g,mode="all")

#add vertex data to network
V(g)$shape<-as.character(v$Shape[match(V(g)$name,v$Short_Name)]) #shape
V(g)$col<-as.character(v$Color[match(V(g)$name,v$Short_Name)]) #color
V(g)$type<-as.character(v$Grp_Type[match(V(g)$name,v$Short_Name)]) #type
V(g)$ideo<-as.character(v$Party[match(V(g)$name,v$Short_Name)]) #party

#number of cascades involved in
casc<-as.data.frame(cascades)
casc <- as.data.frame(table(casc$node_name))
V(g)$cascades<-as.numeric(as.character(casc$Freq[match(V(g)$name,casc$Var1)]))

#environmental focus?
V(g)$env<-as.character(v$issue_area_env[match(V(g)$name,v$Short_Name)])


#PLOT NETWORK
#Figure 2:
#Note: If using RStudio, expand Plot tab to fit this graph
set.seed(1000)

par(mar=c(1,1,1,1))
plot(g, vertex.color = V(g)$col,rescale=T,margin=-0.1,
     layout=layout_with_kk,vertex.size=ifelse(V(g)$degree<22,3,V(g)$degree/7),edge.color="grey87",
     vertex.label.cex=ifelse(V(g)$degree>6 & V(g)$type!="MC",(V(g)$degree/(V(g)$degree+30)),.7),
     vertex.label.dist=ifelse(V(g)$degree>6 & V(g)$type!="MC",0,.6),
     vertex.label.degree=-pi/2,vertex.label.color=ifelse(V(g)$degree>6 & V(g)$type!="MC", "white" ,V(g)$col),
     vertex.label=ifelse(substr(V(g)$name,1,3) %in% c("Sen","Rep"),paste(substr(V(g)$name,1,4),"\n",substr(V(g)$name,5,nchar(V(g)$name))),V(g)$name),
     edge.arrow.size=.25, vertex.shape=ifelse(is.na(V(g)$shape), "none", V(g)$shape))


#Most influential/influenced nodes:
#Table 1:
influencers <- data.frame(actor=V(g)$name, type = V(g)$type,
                          party = V(g)$ideo, outdegree = V(g)$out_degree,
                          indegree = V(g)$in_degree, influence_ratio = (V(g)$out_degree/V(g)$in_degree))

#sort by number of sent ties
influencers <- influencers[order(influencers$outdegree,decreasing = T),]


#Table 2:
influenced_mcs <- subset(influencers, influencers$type=="MC")
influenced_mcs <- influenced_mcs[order(influenced_mcs$indegree,decreasing = T),]

#calculate % in and out to co-partisans
mc_ties<-network.aggregate(g,by=c("name","col"),return.df = T)

#is tie from copartisan?
mc_ties$same <- ifelse(mc_ties$from.col == mc_ties$to.col, 1, 0)

#total ties from copartisans
ties_sum<-aggregate(mc_ties$same ~ mc_ties$to.name, FUN=sum)
influenced_mcs$num_from_copart <- as.numeric(as.character(ties_sum[,2][match(influenced_mcs$actor,ties_sum[,1])]))

#percentage from copartisans
influenced_mcs$percent_from_copart <- (influenced_mcs$num_from_copart/influenced_mcs$indegree)*100

#is tie from opposing party?
mc_ties$opposing <- ifelse(paste0(mc_ties$from.col,mc_ties$to.col) == "grey50grey4" |
                             paste0(mc_ties$from.col,mc_ties$to.col) == "grey4grey50", 1, 0)

#total ties from opposing partisans
ties_sum<-aggregate(mc_ties$opposing ~ mc_ties$to.name, FUN=sum)
influenced_mcs$num_from_opposing <- as.numeric(as.character(ties_sum[,2][match(influenced_mcs$actor,ties_sum[,1])]))

#percentage from copartisans
influenced_mcs$percent_from_opposing <- (influenced_mcs$num_from_opposing/influenced_mcs$indegree)*100

########################################
##### 5. Run ERGM model on network #####
########################################
#Change from Igraph object to Network object for ERGM
summary(g)

num_nodes<-122 #set network parameters based on g
num_edges<-683

net<-network.initialize(num_nodes)
network.vertex.names(net)<-V(g)$name
net[as.matrix(result[,1:2])]<-1

set.vertex.attribute(net,"type",V(g)$type)
set.vertex.attribute(net,"ideo",V(g)$ideo)
set.vertex.attribute(net,"cascades",V(g)$cascades)
set.vertex.attribute(net,"env",V(g)$env)

eg<-ergm(net ~ edges+esp(0)+ nodematch("env",diff=T,keep=2) +
               nodematch("type",diff=T,keep=c(1,3))+
           absdiff("cascades") +  mutual+ nodematch("ideo",diff=T,keep=c(2,3))+
           mutual(same="ideo",diff=T,keep=c(2,3)),
         control = control.ergm(drop=T,seed=22))
summary(eg)


#To change log odds to predicted probabilities, first calculate
#the baseline probability using the edges term
plogis(coef(eg)[['edges']])

#Then add the baseline to other terms to see the probability given
#values for particular variables (change mutual to any model term)
plogis(coef(eg)[['edges']] + coef(eg)[['mutual']])

#To calculate the percent increase (again change mutual to any
#model term)
((plogis(coef(eg)[['edges']] + coef(eg)[['mutual']]))-plogis(coef(eg)[['edges']]))/plogis(coef(eg)[['edges']])*100


########################################
## 6. Check model fit and convergence ##
########################################
#Model diagnostics:
mcmc.diagnostics(eg)

#goodness of fit
go<-gof(eg,GOF = ~model) #p values closer to 1 = better
go

#plot GOF results
par(las=1, mar = c(7,5,3,1),mgp = c(10, 1, 0))
plot(go,cex.axis=0.6,main="",xaxt="n",xlab=NULL,ylab="Simulated Quantiles")
axis(1,at=seq(1,11,1),labels=c("Edges","Edgewise Shared\nPartners",
                               "Both Environmental","Both Interest\nGroups",
                               "Both Think\nTanks","Absolute Cascade\nDifference",
                               "Mutual Reciprocity","Both Democrats","Both Republicans",
                               "Mutual X\nBoth Democratic","Mutual X\nBoth Republican"),cex.axis=0.75,las=2)
corners<-par("usr")
par(xpd=T)
text(x=corners[1]-1.2, y=mean(corners[3:4]),"Simulated Quantiles",srt=90)



########################################
# 7. Qualitative Analysis - Top Bigrams #
########################################
df <- DTM

#create binary indicator for whether or not term was used
df[is.na(df)]<-0
df[df!=0]<-1

#find total usage of each bigram
tot_use <-as.data.frame(colSums(df))
tot_use$name<-rownames(tot_use)

#and percentage of actors who used
tot_use$pct<-tot_use[,1]/nrow(df)


#Percentage of actors from each partisan background using terms
#add party data
df$party <- as.character(v$Party[match(rownames(df),v$Short_Name)])

#calculate total usage by party
pty_use <- by(df[1:(ncol(df)-1)], df$party, colSums)

#add to tot_use
tot_use$centrist_tot <- pty_use$Centrist
tot_use$republican_tot <- pty_use$Republican
tot_use$democratic_tot <- pty_use$Democrat

colnames(tot_use) <- c("total","actor_name","total_pct","centrist_total","republican_total",
                       "democratic_total")
tot_use <- tot_use[,c(2,1,3:6)]

#calculate percentage by party
table(df$party) #find # of each party type

tot_use$centrist_pct <- (tot_use$centrist_total/12)*100
tot_use$democratic_pct <- (tot_use$democratic_total/48)*100
tot_use$republican_pct <- (tot_use$republican_total/62)*100

#order with most used terms first (regardless of party)
tot_use<-tot_use[order(tot_use$total,decreasing=T),]

#or order by party usage
tot_use<-tot_use[order(tot_use$centrist_pct,decreasing=T),]

tot_use<-tot_use[order(tot_use$democratic_pct,decreasing=T),]

tot_use<-tot_use[order(tot_use$republican_pct,decreasing=T),]
