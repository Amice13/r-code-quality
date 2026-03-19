# Generation of the random treatments

#This script generates 1000 randomized treatments at the zip level for both Thanksgiving and Christmas campaigns, 
#the same way it was originally done to assign treatments. 
#The output is in Data/randomized_zip.csv and is used in the randomization inference scripts.

# Import treatment data for both campaigns 

thanksgiving_data = read_excel("../Data/randomized_sample_thanksgiving.xlsx")

thanksgiving_data = thanksgiving_data %>% group_by(county) %>% mutate(
  
  share_urban = mean(urban)
  
)

thanksgiving_data = thanksgiving_data[,c("county","high_county","zip","treat","share_urban","urban")]
colnames(thanksgiving_data)=c("user_loc","high_county_T1","zip","treated_T1","share_urban","urban")

christmas_data = read_excel("../Data/randomized_sample_christmas.xlsx")
christmas_data = christmas_data[,c("fips","high_county","zip","treat")]
colnames(christmas_data)=c("user_loc","high_county_X","zip","treated_X")


# Creation of random permutations of the treatment for thanksgiving campaign

set.seed(3167858) 
nb_randomization=1000

counties_T1 = unique(thanksgiving_data$user_loc)


pb <- txtProgressBar(min = 0, max = nb_randomization, style = 3)

for (i in c(1:nb_randomization)){
  
  #permutation for T1
  permutation = sample(c(rep(0,410),rep(1,410)))
  
  df = as.data.frame(cbind(counties_T1,permutation))
  colnames(df)=c("user_loc",paste0("high_county_T1_",i))
  df= merge(df,thanksgiving_data[,c("user_loc","zip")],by="user_loc")
  
  df$rand = runif(nrow(df),0,1)
  df <-df[order(df$rand),]
  df$rand=NULL
  first_county=TRUE
  for (u in counties_T1){
    temp = df %>% filter(user_loc==u)
    temp[[paste0("treated_zip_T1_",i)]]=0
    temp[c(1:(nrow(temp)/4)),paste0("treated_zip_T1_",i)]=1
    if (nrow(temp)>=(4*(nrow(temp)%/%4)+1)){
    temp[(4*(nrow(temp)%/%4)+1):nrow(temp),paste0("treated_zip_T1_",i)]=as.numeric(runif(nrow(temp)-(4*(nrow(temp)%/%4)+1)+1,0,1)<0.25)
    }
    if (mean(temp[[paste0("high_county_T1_",i)]])==1){
      temp[[paste0("treated_zip_T1_",i)]]=1-temp[[paste0("treated_zip_T1_",i)]]
    }
    if(first_county==TRUE){
      new_df=temp
      first_county=FALSE
    }else{
      new_df=bind_rows(new_df,temp)
    }
  }
  if (i==1){
    results = new_df
  }else{
    results = merge(results,new_df,by=c("zip","user_loc"))
  }
  setTxtProgressBar(pb, i)
  }
close(pb)

results_T1=results

# Creation of random permutations of the treatment for christmas campaign


counties_X = unique(christmas_data$user_loc)


pb <- txtProgressBar(min = 0, max = nb_randomization, style = 3)

for (i in c(1:nb_randomization)){
  
  #permutation for X
  permutation = sample(c(rep(0,381),rep(1,386)))
  
  df = as.data.frame(cbind(counties_X,permutation))
  colnames(df)=c("user_loc",paste0("high_county_X_",i))
  df= merge(df,christmas_data[,c("user_loc","zip")],by="user_loc")
  
  df$rand = runif(nrow(df),0,1)
  df <-df[order(df$rand),]
  df$rand=NULL
  first_county=TRUE
  for (u in counties_X){
    temp = df %>% filter(user_loc==u)
    temp[[paste0("treated_zip_X_",i)]]=0
    temp[c(1:(nrow(temp)/4)),paste0("treated_zip_X_",i)]=1
    if (nrow(temp)>=(4*(nrow(temp)%/%4)+1)){
      temp[(4*(nrow(temp)%/%4)+1):nrow(temp),paste0("treated_zip_X_",i)]=as.numeric(runif(nrow(temp)-(4*(nrow(temp)%/%4)+1)+1,0,1)<0.25)
    }
    if (mean(temp[[paste0("high_county_X_",i)]])==1){
      temp[[paste0("treated_zip_X_",i)]]=1-temp[[paste0("treated_zip_X_",i)]]
    }
    if(first_county==TRUE){
      new_df=temp
      first_county=FALSE
    }else{
      new_df=bind_rows(new_df,temp)
    }
  }
  if (i==1){
    results = new_df
  }else{
    results = merge(results,new_df,by=c("zip","user_loc"))
  }
  setTxtProgressBar(pb, i)
}
close(pb)

results_X=results

results= merge(results_T1,results_X,by=c("user_loc","zip"),all=TRUE)

write.csv(results,"../Data/randomized_zip.csv") # save data
