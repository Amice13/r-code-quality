
#This is a function to convert certain data from the format in the mother data set, to the format in the old data set.
#This means that old R scripts can be used to analyse these pieces of data
#For example, in th old format data appeared in the format

#     Jan           Feb         Mar         ...
# 1   TRUE          FALSE       TRUE        ...
# 2   FALSE         TRUE        TRUE        ...
# 3   FALSE         FALSE       TRUE        ...

#In the new format this would appear as

#     Months          
# 1   jan mar ...                 
# 2   feb mar ...         
# 3   mar ...

#To call this function enter  for example:

#wild_food_true_false<- List_to_True_False(dat_all$wildfood_collect_when)

#and this should return a TRUE FALSE data frame assigned to wild_food_true_false
#---------------------------------------------------------------------------------------------------------------------#

List_to_True_False<- function(x)
  
{
 x[is.na(x)]<-"NA" 
   
  x<- strsplit(x, " ")
  x_names<-unique(unlist(x))
  x_names<-x_names[!is.na(x_names)]


  n <- length(x_names)
  x <-lapply(x, `length<-`, n) 
  x_df<-data.frame(matrix(unlist(x), nrow= length(x), byrow=T),stringsAsFactors=FALSE)

  for (i in 1: length(x))
  {
    for (j in 1:n)
    {
      y<- as.character(x_df[[j]][i])
      if (is.na(y)==TRUE)
      {
        x_df[[j]][i]<- "NA"
      }
    }
  }

  x_true_false<-as.data.frame(matrix(ncol = n, nrow =  length(x)))
  colnames(x_true_false) <- c(x_names)
  x_true_false[is.na(x_true_false)] <- FALSE

  for (i in 1: length(x))
  {
    for (j in 1:n)
    {
      
      y<- as.character(x_df[[j]][i])
      c<- 0
      for (k in x_names)
      {
       
        c<-c+1
        if (y == k){x_true_false[[c]][i] <- TRUE}
      }
    }
  }

  return(x_true_false)
}




