library(segmented)
library(readxl)
library(changepoint)
library(changepoint.np)

library(tidyverse) #data manipulation
library(cluster) #clustering algorithms
library(factoextra) #clustering visualization
library(dendextend) #compare twd dendrograms
library(ggpubr)
library(FactoMineR)
library(corrplot)

library(igraph)

library(Kendall)
library(trend)



ds <- read_excel("/Data summary.xlsx", sheet = "Publication in time", col_names = TRUE) 

head(ds)


######change point by PELT######
tiff("Transition periods-publication of water resources.tiff", units="in", width = 12, height = 15, res=300)

ds.man1 = cpt.var(ds$`No. of publications`, method = "PELT", penalty = "Manual", pen.value = "2*log(n)")
cpts(ds.man1)
param.est(ds.man1)
plot(ds.man1,cpt.width=3)
dev.off()

#How many change point?
#"CROPS" for changepoints for a range of penalties, specify minimum and maximum penalty values and it returns all segmentations for any penalty between these values
ds.man=cpt.var(ds$`No. of publications`, method = "PELT", penalty = "CROPS", pen.value = c(1,500))
cpts.full(ds.man)
#retrieve the penalty boundary points where the segmentation switches from a smaller to larger number of changepoints
pen.value.full(ds.man)
#ncpts select the number of change we want un the segmentation to be plotted
plot(ds.man, ncpts=2)
#or a diagnostic plot using elbow method (akin to the scree plot in principal component analysis)
#when a true changepoint is added the cost increases or decreases rapidly, but when a changpoinr due to noise is added the change is small
plot(ds.man, diagnostic=TRUE)
dev.off()


######Network analysis######

tab_names<- excel_sheets(path = ".xlsx")
lst<- lapply(tab_names, function(i) read_excel(".xlsx", sheet = i, col_names=c("From", "TO", "YEAR","3 Periods", "10-yr Periods","77-17 Periods", "WEIGHTS")))

write.csv(tab_names, "lst of basin names-temporal.csv")
attr(lst, tab_names) <- lst

extract.year<- function(df){
  g<- df$YEAR
  return(g)
}
lst.year<- lapply(lst,extract.year)

#https://community.rstudio.com/t/the-condition-has-length-1-and-only-the-first-element-will-be-used/16737/3
#https://datascience.stackexchange.com/questions/33351/what-is-the-problem-with-the-condition-has-length-1-and-only-the-first-elemen
trans.period<- function(df){
  sapply(df, function (x) if (x<=1993)
     1993
     else if (x>1993 && x <= 2005) 
     2005
       else 
     2017)
}

trans.df<- lapply(lst.year, trans.period)

#https://stackoverflow.com/questions/36111521/getting-error-using-cbind-in-lapply-r
trans.df2<- Map(cbind, lst, trans.df)

#assigning column names to the new data set
#https://stackoverflow.com/questions/28648513/changing-column-names-in-a-list-of-data-frames-in-r
New.colnames<- c("From", "TO", "YEAR","3 Periods", "10-yr Periods","77-17 Periods", "WEIGHTS", "Hydro Periods")
trans.df3<- lapply(trans.df2, setNames, New.colnames)

#to split the list by hydro periods
split.byperiod<- function(df){
  g<- split(df, df$"Hydro Periods")
  return(g)
}
lst.byperiod<- lapply(trans.df3, split.byperiod)


#function to calculate the required network measures
cal.measures<- function(df){
  deg<- degree(df)
  clos<- closeness(df)
  
  measures<- cbind(deg, clos)
  
  return(measures)}

#function to transform the two-mode edglist to 1-mode igraph, and calculate the required network measures
get.1modegraph.kw<- function(df){
  
  #need to remove overlapping kw and disp: ecology, remote sensing (so lucky there are only two), according to:
  #https://stackoverflow.com/questions/44687623/bipartite-graph-projection-error-igraph-rstudio
  df[which(df[,1]=="Ecology"), 1]<- "Ecology-kw"
  df[which(df[,2]=="Ecology"), 2]<- "Ecology-dis"
  df[which(df[,1]=="remote sensing"), 1]<- "remote sensing-kw"
  df[which(df[,2]=="Remote Sensing"), 2]<- "Remote Sensing-dis"
  df[which(df[,1]=="Management"), 1]<- "Management-kw"
  df[which(df[,2]=="Management"), 2]<- "Management-dis"
  
  g<- graph_from_data_frame(df)
  
  
  g_el<- as_edgelist(g)
  colnames(g_el)<- c("kw", "disp")
  V(g)$type<- ifelse(V(g)$name %in% g_el[,"kw"], TRUE, FALSE)
  E(g)$weight<- E(g)$"WEIGHTS"
  
  gproj<- bipartite.projection(g, V(g)$type, multiplicity = TRUE)

  gproj.kw<- gproj$proj2
  
  return(measures.kw)
}

get.1modegraph.cat<- function(df){
  
  df[which(df[,1]=="Ecology"), 1]<- "Ecology-kw"
  df[which(df[,2]=="Ecology"), 2]<- "Ecology-dis"
  df[which(df[,1]=="remote sensing"), 1]<- "remote sensing-kw"
  df[which(df[,2]=="Remote Sensing"), 2]<- "Remote Sensing-dis"
  df[which(df[,1]=="Management"), 1]<- "Management-kw"
  df[which(df[,2]=="Management"), 2]<- "Management-dis"
  
  g<- graph_from_data_frame(df, vertices = NULL)
  g_el<- as_edgelist(g)
  colnames(g_el)<- c("kw", "disp")
  V(g)$type<- ifelse(V(g)$name %in% g_el[,"kw"], TRUE, FALSE)
  E(g)$weight<- E(g)$"WEIGHTS"
  
  gproj<- bipartite.projection(g, V(g)$type, multiplicity = TRUE)
  
  gproj.cat<- gproj$proj1
 
  measures.cat<- cal.measures(gproj$proj1)
  
  return(measures.cat)
}

#network analysis when all rivers are combined
lst.merge <- read_excel(".xlsx", col_names = TRUE)
lst.merge.byperiod<- split(lst.merge, lst.merge$"Hydro period")
lst.merge.cat<- lapply(lst.merge.byperiod, get.1modegraph.cat)
lst.merge.kw<- lapply(lst.merge.byperiod, get.1modegraph.kw)

#export the discipline matrix from igraph object
mat1993<- as.matrix(lst.merge.cat[["1993"]][])
#plot(lst.merge.cat[["1987"]],)
write.csv(mat1993,"1-mode all rivers by perid-cat-adjacency matrix-1993.csv", row.names=TRUE)

mat2005<- as.matrix(lst.merge.cat[["2005"]][])
write.csv(mat2005,"1-mode all rivers by perid-cat-adjacency matrix-2005.csv", row.names=TRUE)

mat.2017<- as.matrix(lst.merge.cat[["2017"]][])
write.csv(mat.2017,"1-mode all rivers by perid-cat-adjacency matrix-2017-transition.csv", row.names=TRUE)

#export the keyword matrix from igraph object
mat1993<- as.matrix(lst.merge.kw[["1993"]][])
#plot(lst.merge.cat[["1987"]],)
write.csv(mat1993,"1-mode all rivers by perid-kw-adjacency matrix-1993.csv", row.names=TRUE)

mat2005<- as.matrix(lst.merge.kw[["2005"]][])
write.csv(mat2005,"1-mode all rivers by perid-kw-adjacency matrix-2005.csv", row.names=TRUE)

mat.2017<- as.matrix(lst.merge.kw[["2017"]][])
write.csv(mat.2017,"1-mode all rivers by perid-kw-adjacency matrix-2017-transition.csv", row.names=TRUE)



#calculate and export network measures for each river by three periods
for (i in 1:95){
  lst.byperiod[[i]]<- lapply(lst.byperiod[[i]], get.1modegraph.cat)
}
for (i in 1:95){
  lapply(lst.byperiod[[i]], function(x) write.table(data.frame(x),file= paste("1 mode-cat-HydroPeriod by river",i,".csv"), append= T, sep=',', row.names=TRUE, col.names = TRUE))
}

