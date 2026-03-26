#REVERSE_TRUE_FALSE_SCRIPT

TRUE_FALSE_COUNTS<- function (x,y)
{
  #x<-AfricaRising_Ethiopia_Raw_multiplecols
  #y<- "where_heard_of_grp"
  input_dat<- grep(y, colnames(x), value=TRUE)
  input_dat<- x[,input_dat]
  colnames(input_dat)<- gsub(".*\\.", "", colnames(input_dat)) #Remove everything before a full stop
  name_list<-names(input_dat)
  original_names<- unique(names(input_dat))
  
  for(i in 1:ncol(input_dat))
  {
    name_list[i]<- paste0(name_list[i],"_", i)
    
  }
  names(input_dat)<- name_list
  
  
 # list_subset<-subset(why_not_more, select= grepl("Africa_Rising", names(why_not_more)))
#  all_colls<- as.data.frame(apply(X=list_subset,2,FUN=function(x) length(which(x=='True'))))
#  final_sum<- sum(all_colls[,1])
  
  empty<- c()
  j<-0
  for(i in original_names)
  {
    j<- j+1
    list_subset<-subset(input_dat, select= grepl(paste0(i), names(input_dat)))
    all_colls<- as.data.frame(apply(X=list_subset,2,FUN=function(x) length(which(x=='True'))))
    final_sum<- sum(all_colls[,1])
    
    assign(paste0(y,i),sum(all_colls[,1]))
    empty[j]<-sum(all_colls[,1])
  }
  output_dat<- data.frame(original_names, empty)
  
  colnames(output_dat)<- c(y, "Count")

return(output_dat)
  
}



