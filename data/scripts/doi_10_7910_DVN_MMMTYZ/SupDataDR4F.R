####TITLE: Fossilization potential of marine assemblages and environments
####AUTHOR: Jack O. Shaw
####PERMANENT EMAIL: jackolivershaw@gmail.com

############INSTRUCTIONS

#Run all analyses in SupDataDR4C.R and SupDataDR4E.R to pass to the following script

############INSTRUCTIONS


####Conditional inference
#install.packages("partykit")


#Plot as mean 
ob_durations_stats_gbm <- ob_durations  %>%  filter(pb_presence == 1) %>%
  group_by(dataset.par, substrate, environment, LatBin, LongBin, Remaining_Tax, Remaining_Env_match) %>%
  summarise(
    taxon_duration_mean = mean(taxonRange, na.rm = T),
    taxon_duration_sd = sd(taxonRange, na.rm = T),
    strat_range_mean = mean(stratRange, na.rm = T),
    strat_range_sd = sd(stratRange, na.rm = T),
  )

datfra<-joined_col8 %>% filter(taxlev=="Genus") %>%
  left_join(ob_durations_stats_gbm[,c("taxon_duration_mean","taxon_duration_sd",
                                      #"strat_range_sd","strat_range_mean",
                                      "dataset.par","Remaining_Tax")],by=c("dataset.par","Remaining_Tax")) %>%
  filter(!is.na(taxon_duration_mean))


library(partykit)
library(dplyr)

runs<-1000

# Function replaces NA by mean: 
replace_by_mean <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}

# A function imputes NA observations for categorical variables: 

replace_na_categorical <- function(x) {
  x %>% 
    table() %>% 
    as.data.frame() %>% 
    arrange(-Freq) ->> my_df
  
  n_obs <- sum(my_df$Freq)
  pop <- my_df$. %>% as.character()
  x[is.na(x)] <- sample(pop, sum(is.na(x)), replace = TRUE, prob = my_df$Freq)
  return(x)
}


ct_stats<-c()

for(i in c("Remaining_Tax","Remaining_Env_match")){
  
  selectresponse<-i

  df<-datfra[,c("NumTotal","depth.slice",
              "LongBin","LatBin",
              "id_habitat",
              "taxon_duration_mean",#"taxon_duration_sd",
              #"strat_range_sd","strat_range_mean",
              #"cell",
              #"cell_lat_round","cell_lon_round",
              "environment","substrate",selectresponse)]

      df$Remaining_lev<-df[,c(selectresponse)]
      df[,c(selectresponse)]<-NULL
      
      if(selectresponse=="Remaining_Env_match"){
        df<-subset(df,environment=="Shallow" |environment=="Deep" |environment=="Coral reef" | environment=="Pelagic" | environment=="Seamount" )
      }else{
        df<-df
      }

      
      for(j in 1:runs){
      
      # Use the two functions: 
      df2 <- df %>% 
        mutate_if(is.factor, as.character) %>% 
        mutate(id_habitat = case_when(id_habitat == "" ~ NA_character_, TRUE ~ id_habitat), 
               environment = case_when(environment == "" ~ NA_character_, TRUE ~ environment), 
               substrate = case_when(substrate == "" ~ NA_character_, TRUE ~ substrate)) %>%
        mutate_if(is.character, as.factor) %>% 
        mutate_if(is.numeric, replace_by_mean) %>% 
        mutate_if(is.factor, replace_na_categorical)
      
      #str(df)
      
      sampn<-sample(1:nrow(df2),(0.7*nrow(df2)))
      df_train<-df2[sampn,]
      df_test<-df2[-sampn,]
      
      #gtree <- ctree(Remaining_lev ~ ., data = df_train,control = ctree_control(alpha=0.05,testtype="Teststatistic"))
      
      gtree <- ctree(Remaining_lev ~ ., data = df_train,control = ctree_control(alpha=0.05,testtype="MonteCarlo",nresample=10))
      
      
      tv<-varimp(gtree,conditional=TRUE)
      tv2<-as.data.frame(tv)
      tvvar <- cbind(tv2,tv2 %>% mutate(normval = tv / max(tv,na.rm=T))) 
      tvvar <- tvvar[order(tvvar$normval),]
      tvvar[,1]<-NULL
      tvvar[,1]<-NULL
      tvvar$var<-rownames(tvvar)
      rownames(tvvar)<-NULL

      #tvvar
      
      tv3<-predict(gtree,newdata=df_test)
      tv4<-cbind(tv3,df_test[,c("Remaining_lev")])
      
      rmseval<-tryCatch(100*sqrt(sum((tv4[,1] - tv4[,2])^2) / nrow(tv4)), error=function(e) NA)
      
      varexval<-tryCatch(100*(1-(var(tv4[,2]-tv4[,1])/var(tv4[,2]))), error=function(e) NA)
      
      
      ct_stats<-rbind(ct_stats,cbind(tvvar,lev=i,run=j,r2=varexval,rmse=rmseval))

 
      
      }
      
      print(i)
      
}
      
      
ct_stats2<-as.data.frame(ct_stats) %>% group_by(lev,var) %>% 
  summarise(normval=mean(normval,na.rm=T),
            r2=mean(r2,na.rm=T),
            rmse=mean(rmse,na.rm=T)) %>% filter(var!="id_habitat")

pm_order<-subset(ct_stats2,lev=="Remaining_Tax")
pm_order<-pm_order[order(pm_order$normval),]
pm_order<-as.character(pm_order$var)

ct_stats2$lev<-factor(ct_stats2$lev,levels=c("Remaining_Tax","Remaining_Env_match"))
ct_stats2$lev<-recode(ct_stats2$lev, 
                      Remaining_Tax="Taxon fossilization potential",
                      Remaining_Env_match="Within-environment fossilization potential")
ct_stats2$var<-factor(ct_stats2$var,levels=pm_order)
ct_stats2$var<-recode(ct_stats2$var, 
                                taxon_duration_sd = "Genus duration SD",
                                taxon_duration_mean = "Genus duration mean",
                                strat_range_sd = "Strat. range SD",
                                strat_range_mean="Strat. range mean",
                                LatBin = "Latitude", 
                                depth.slice = "Depth", 
                                LongBin = "Longitude", 
                                NumTotal = "Alpha diversity", 
                                substrate = "Substrate", 
                                environment = "Environment", 
                                id_habitat = "Realm")

ct_stats2$r2<-paste("R2 = ",round(ct_stats2$r2,0),"%",sep="")
ct_stats2$rmse<-paste("RMSE = ",round(ct_stats2$rmse,0),"%",sep="")

ggplot(ct_stats2)+
  geom_point(aes(x=var,y=normval),show.legend=F)+
  xlab("")+ylab("Scaled variable importance")+
  theme_bw()+
  coord_flip(ylim=c(0,1))+facet_grid(.~lev+r2+rmse)
