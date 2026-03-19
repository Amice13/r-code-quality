####TITLE: Fossilization potential of marine assemblages and environments
####AUTHOR: Jack O. Shaw
####PERMANENT EMAIL: jackolivershaw@gmail.com


############INSTRUCTIONS

#Use this script to run Sup Data DR4A and DR4B
#This script takes the fossilization potential values generated in DR4B and permits plotting/analyses
#Note: Within-environment fossilization potential is sometimes referred to here as "assemblage fossilization potential"

############INSTRUCTIONS


##Source all data
source("SupDataDR4A.R")
sitedat_joined<-read.csv("SupDataDR2B.csv")
source("SupDataDR4B.R")

library(patchwork)

#Code 4 imports interpolation model and predicts
joined_col8<-merge(joined_col7,t_final[,c("prob_select","dataset.par","taxlev","substrate")],by=c("dataset.par","taxlev"),all.x=TRUE)

####Plotting joined graphs
library(tidyverse)

joined_col5$taxlev<-gsub("order","Order",joined_col5$taxlev)
joined_col5$taxlev<-gsub("family","Family",joined_col5$taxlev)
joined_col5$taxlev<-gsub("genus","Genus",joined_col5$taxlev)
joined_col5$taxlev<-factor(joined_col5$taxlev,levels=c("Order","Family","Genus"))

joined_col8$substrate = factor(joined_col8$substrate, levels=c("Mud","Sand","Gravel","Rock"))
joined_col8$taxlev<-gsub("order","Order",joined_col8$taxlev)
joined_col8$taxlev<-gsub("family","Family",joined_col8$taxlev)
joined_col8$taxlev<-gsub("genus","Genus",joined_col8$taxlev)
joined_col8$taxlev<-factor(joined_col8$taxlev,levels=c("Order","Family","Genus"))


####FIGURE 1: map and summary----

dist_dat5<-subset(joined_col5,taxlev=="Genus")
dist_dat5<-dist_dat5[,c("Remaining_Tax","dataset.par","environment")]
colnames(dist_dat5)[which(names(dist_dat5) == "Remaining_Tax")] <- "Value"

dist_dat8<-subset(joined_col8,taxlev=="Genus")
dist_dat8<-dist_dat8[,c("Remaining_Env_match","dataset.par","environment")]
colnames(dist_dat8)[which(names(dist_dat8) == "Remaining_Env_match")] <- "Value"

dist_dat_join<-rbind(cbind(dist_dat5,ValType="Taxon"),
                     cbind(dist_dat8,ValType="Assemblage"))
dist_dat_join$ValType<-factor(dist_dat_join$ValType,levels=c("Taxon","Assemblage"))

d_stat_tax<-as.data.frame(dist_dat_join)  %>%
  group_by(ValType) %>%
  summarise(
    TAX_mean = mean(Value,na.rm=T), nums=n_distinct(dataset.par))
d_stat_labels<-as.character(paste(d_stat_tax$ValType," (n = ",d_stat_tax$nums,")",sep=""))


bw<-0.05

f1<-ggplot(dist_dat_join)+
  geom_density( aes(x=Value, group=ValType,color=ValType, y=bw * ..count..), show.legend=F) +
  geom_histogram(aes(x=Value, group=ValType,fill=ValType),binwidth = bw, position="identity",show.legend=T,alpha=0.3,boundary=0)+
  geom_vline(data=d_stat_tax,aes(xintercept=TAX_mean,color=ValType),linetype="dashed",show.legend=F,size=1)+
  scale_y_continuous("Number of assemblages",expand = expansion(mult = c(0, .05)))+
  scale_x_continuous("Fossilization potential",
                     labels = function(x) scales::percent(x, accuracy = 1),
                     breaks=c(0,0.25,0.5,0.75,1),
                     expand = c(0, 0)) +
  #theme(plot.margin=unit(c(0,0,0,0),"cm"))+
  guides(color = FALSE) +
  scale_fill_discrete(name = "Fossilization potential", labels = d_stat_labels) +
  theme(
    legend.position = c(.99, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    #legend.box.background = element_rect(color="black", size=1),
    legend.background = element_rect(colour = 'transparent', fill = 'transparent'),
    legend.title=element_blank()
  )

library("rnaturalearth")
library("rnaturalearthdata")
library(ggthemes)
library(ggspatial)
world <- ne_countries(scale = "medium", returnclass = "sf")
world_map <- map_data("world")

f2<-ggplot(data = world) +
  geom_polygon(data=world_map,fill="lightgrey", aes(x = long, y = lat,group=group),show.legend=F) +
  scale_x_continuous("Longitude",expand = c(0, 0))+scale_y_continuous("Latitude",expand = c(0, 0)) +
  geom_point(data=subset(joined_col8,taxlev=="Genus"),aes(x=LongBin,y=LatBin,fill=Remaining_Tax*100,color=Remaining_Tax*100),shape=21,alpha=0.5,show.legend=T)+
  scale_color_gradient2(name="Assemblage F.P.", guide = guide_colourbar(direction = "horizontal",title.position="top"), low="chartreuse3",mid="darkgrey",high="darkorchid3",midpoint=35,limits=c(0,70))+
  scale_fill_gradient2(low="chartreuse3",mid="darkgrey",high="darkorchid3",midpoint=35,limits=c(0,70))+
  theme_bw()+
  guides(fill = FALSE, linetype = FALSE, shape = FALSE)+
  theme(panel.border = element_rect(colour = "black", fill="transparent", size=1),
        #legend.title=element_blank(),
        legend.background = element_rect(colour = 'transparent', fill = 'transparent'),
        legend.position = c(.82, .85),
        legend.justification = c("right", "top"),
        legend.box.just = "top"
  ) +
  #annotation_scale() +
  annotation_north_arrow(location = "bl", which_north = "true",   pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) 

#4.5 x 11
f1+f2+plot_layout(ncol=2, widths=c(1,2)) & theme(strip.placement = NULL)


#Plot environments
ggplot(data = world) +
  geom_polygon(data=world_map,fill="lightgrey", aes(x = long, y = lat,group=group),show.legend=F) +
  scale_x_continuous("Longitude",expand = c(0, 0))+scale_y_continuous("Latitude",expand = c(0, 0)) +
  geom_point(data=subset(joined_col8,taxlev=="Genus"),aes(x=LongBin,y=LatBin,fill=environment,color=environment),shape=21,alpha=0.5,show.legend=T)+
  theme_bw()+
  guides(fill = FALSE, linetype = FALSE, shape = FALSE)+
  theme(panel.border = element_rect(colour = "black", fill="transparent", size=1),
        #legend.title=element_blank(),
        legend.background = element_rect(colour = 'transparent', fill = 'transparent'),
        legend.position = c(.82, .85),
        legend.justification = c("right", "top"),
        legend.box.just = "top"
  ) +
  #annotation_scale() +
  annotation_north_arrow(location = "bl", which_north = "true",   pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) 


####FIGURE 2: Substrate plot----

joined_col9<-joined_col8[,c("dataset.par","taxlev","id_habitat","environment","Remaining_Tax","Remaining_Env_match","Remaining_Env_any","substrate")]
joined_col9<-gather(joined_col9,"Remaining_Tax","Remaining_Env_match","Remaining_Env_any",key="ValType",value="Remaining_Value")

joined_col9$substrate<-factor(joined_col9$substrate,levels=rev(levels(joined_col9$substrate)))

joined_col9$ValType<-recode(joined_col9$ValType, Remaining_Tax = "Taxon", Remaining_Env_match="Assemblage",
                            Remaining_Env_any = "Env. info. available")

joined_col9<-subset(joined_col9,ValType=="Taxon" | ValType=="Assemblage")
joined_col9$ValType<-factor(joined_col9$ValType,levels=c("Taxon","Assemblage"))

joined_col9_means_all<-joined_col9 %>%
  group_by(taxlev,ValType) %>%
  summarise(Mean=mean(Remaining_Value,na.rm=T),
            SD=sd(Remaining_Value,na.rm=T))
joined_col9_means_all$type<-"All data"


########################Substrate
joined_col9_substrate<-subset(joined_col9,ValType=="Taxon" & environment=="Shallow" & taxlev=="Genus")


joined_col9_means_substrate<-joined_col9_substrate %>%
  group_by(taxlev,ValType,substrate) %>%
  summarise(Mean=mean(Remaining_Value,na.rm=T),
            SD=sd(Remaining_Value,na.rm=T))
joined_col9_means_substrate<-joined_col9_means_substrate%>%drop_na(substrate)

jc9_sub_nums<-joined_col9_substrate %>% group_by(taxlev,ValType,substrate) %>% summarise(nums=n()) %>% drop_na(substrate)
jc9_sub_nums<-unique(jc9_sub_nums[,c("taxlev","substrate","nums")])
jc9_sub_nums$nums<-paste("n = ",jc9_sub_nums$nums,sep="")

ggplot(joined_col9_substrate%>%drop_na(substrate))+
  geom_density( aes(x=Remaining_Value, group=taxlev,color=taxlev, y=bw * ..count..), show.legend=F) +
  geom_histogram( aes(x=Remaining_Value, group=taxlev,fill=taxlev),binwidth = bw, position="identity",show.legend=F,alpha=0.3,boundary=0)+
  geom_vline(data=joined_col9_means_substrate,aes(xintercept=Mean,color=taxlev,),linetype="dashed",size=1,show.legend=F)+
  geom_hline(yintercept=0,color="white")+
  scale_y_continuous("Number of assemblages",expand = expansion(mult = c(0, .05)))+
  scale_x_continuous("Taxon fossilization potential",
                     labels = function(x) scales::percent(x, accuracy = 1),
                     breaks=c(0,0.25,0.5,0.75,1),
                     expand = c(0, 0)) +
  theme(strip.background = element_rect(fill="white")) +
  theme(strip.background = element_blank(), strip.text = element_blank())+
  geom_text(data=jc9_sub_nums,x=0,y=0,aes(label=nums),vjust=0,hjust=0) +
  geom_text(data=jc9_sub_nums,x=0.9,y=0,aes(label=substrate),vjust=0,hjust="middle",size=5) +
  #theme(strip.text.x = element_text(angle = 0, hjust = 0))+
  facet_grid(substrate~environment,scales="free")

#Plot 3.5x6

####Sup figure, substrate
joined_col9_substrate<-subset(joined_col9,ValType=="Taxon" & environment=="Shallow")

joined_col9_means_substrate<-joined_col9_substrate %>%
  group_by(taxlev,ValType,substrate) %>%
  summarise(Mean=mean(Remaining_Value,na.rm=T),
            SD=sd(Remaining_Value,na.rm=T))
joined_col9_means_substrate<-joined_col9_means_substrate%>%drop_na(substrate)

jc9_sub_nums<-joined_col9_substrate %>% group_by(taxlev,ValType,substrate) %>% summarise(nums=n()) %>% drop_na(substrate)
jc9_sub_nums<-unique(jc9_sub_nums[,c("taxlev","substrate","nums")])
jc9_sub_nums$nums<-paste("n = ",jc9_sub_nums$nums,sep="")

ggplot(joined_col9_substrate%>%drop_na(substrate))+
  geom_density( aes(x=Remaining_Value, group=taxlev, y=bw * ..count..), color="#F8766D", show.legend=F) +
  geom_histogram( aes(x=Remaining_Value, group=taxlev),binwidth = bw, position="identity",fill="#F8766D",show.legend=F,alpha=0.3,boundary=0)+
  geom_vline(data=joined_col9_means_substrate,aes(xintercept=Mean),linetype="dashed",color="#F8766D",size=1,show.legend=F)+
  geom_hline(yintercept=0,color="white")+
  scale_y_continuous("Number of assemblages",expand = expansion(mult = c(0, .05)))+
  scale_x_continuous("Taxon fossilization potential",
                     labels = function(x) scales::percent(x, accuracy = 1),
                     breaks=c(0,0.25,0.5,0.75,1),
                     expand = c(0, 0)) +
  theme(strip.background = element_rect(fill="white")) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())+
  geom_text(data=jc9_sub_nums,x=0,y=0,aes(label=nums),vjust=0,hjust=0) +
  geom_text(data=jc9_sub_nums,x=0.8,y=0,aes(label=substrate),vjust=0,hjust="middle",size=5) +
  #theme(strip.text.x = element_text(angle = 0, hjust = 0))+
  theme(panel.spacing.x=unit(2, "lines"))+
  facet_grid(substrate~taxlev,scales="free")


####FIGURE 3: Environment plot----
joined_col9_environment<-subset(joined_col9, taxlev=="Genus")
joined_col9_environment$environment<-factor(joined_col9_environment$environment,levels=c("Shallow","Coral reef","Deep","Pelagic","Seamount"))


joined_col9_means_environment<-joined_col9_environment %>%
  group_by(taxlev,ValType,environment) %>%
  summarise(Mean=mean(Remaining_Value,na.rm=T),
            SD=sd(Remaining_Value,na.rm=T))


jc9_env_nums<-joined_col9_environment %>% group_by(taxlev,ValType,environment) %>% summarise(nums=n()) 
jc9_env_nums<-unique(jc9_env_nums[,c("taxlev","environment","nums")])
jc9_env_nums$nums<-paste("n = ",jc9_env_nums$nums,sep="")

joined_col9_environment_den<-joined_col9_environment %>%
  filter(!(environment=="Seamount" & ValType=="Assemblage")) %>%
  filter(!(environment=="Pelagic" & ValType=="Assemblage"))

ggplot(joined_col9_environment)+
  geom_density(data=joined_col9_environment_den, aes(x=Remaining_Value, group=ValType,color=ValType, y=bw * ..count..), show.legend=F) +
  geom_histogram(aes(x=Remaining_Value, group=ValType,fill=ValType),binwidth = bw, position="identity",show.legend=T,alpha=0.3,boundary=0)+
  geom_vline(data=joined_col9_means_environment,aes(xintercept=Mean,color=ValType,),size=1,linetype="dashed",show.legend=F)+
  geom_hline(yintercept=0,color="white")+
  scale_y_continuous("Number of assemblages",expand = expansion(mult = c(0, .05)))+
  scale_x_continuous("Fossilization potential",
                     labels = function(x) scales::percent(x, accuracy = 1),
                     breaks=c(0,0.25,0.5,0.75,1),
                     expand = c(0, 0)) +
  theme(strip.background = element_rect(fill="transparent")) +
  geom_text(data=jc9_env_nums,x=0,y=0,aes(label=nums),vjust=0,hjust=0) +
  #theme(strip.text.x = element_text(angle = 0, hjust = 0))+
  theme(
    legend.position = c(1, 0),
    legend.justification = c("right", "bottom"),
    #legend.box.background = element_rect(color="black", size=1),
    legend.title=element_blank(),
    legend.box.just = "right",
    legend.background = element_rect(colour = 'transparent', fill = 'transparent'),
    strip.background = element_blank(), strip.text = element_blank()
  )+
  guides(color = FALSE,fill=guide_legend(label.position = "left",
                                         label.hjust = 1)) +
  scale_fill_discrete(name = "Fossilization potential") +
  facet_grid(environment~.,scales="free") +
  geom_text(data=jc9_env_nums,x=0.85,y=0,aes(label=environment),vjust=0,hjust="middle",size=5) 


####Sup fig, environment
joined_col9_environment<-joined_col9
joined_col9_environment$environment<-factor(joined_col9_environment$environment,levels=c("Shallow","Coral reef","Deep","Pelagic","Seamount"))


joined_col9_means_environment<-joined_col9_environment %>%
  group_by(taxlev,ValType,environment) %>%
  summarise(Mean=mean(Remaining_Value,na.rm=T),
            SD=sd(Remaining_Value,na.rm=T))


jc9_env_nums<-joined_col9_environment %>% group_by(taxlev,ValType,environment) %>% summarise(nums=n()) 
jc9_env_nums<-unique(jc9_env_nums[,c("taxlev","environment","nums")])
jc9_env_nums$nums<-paste("n = ",jc9_env_nums$nums,sep="")

joined_col9_environment_den<-joined_col9_environment %>%
  filter(!(environment=="Seamount" & ValType=="Assemblage")) %>%
  filter(!(environment=="Pelagic" & ValType=="Assemblage"))

ggplot(joined_col9_environment)+
  geom_density(data=joined_col9_environment_den, aes(x=Remaining_Value, group=ValType,color=ValType, y=bw * ..count..), show.legend=F) +
  geom_histogram(aes(x=Remaining_Value, group=ValType,fill=ValType),binwidth = bw, position="identity",show.legend=T,alpha=0.3,boundary=0)+
  geom_vline(data=joined_col9_means_environment,aes(xintercept=Mean,color=ValType,),size=1,linetype="dashed",show.legend=F)+
  geom_hline(yintercept=0,color="white")+
  scale_y_continuous("Number of assemblages",expand = expansion(mult = c(0, .05)))+
  scale_x_continuous("Fossilization potential",
                     labels = function(x) scales::percent(x, accuracy = 1),
                     breaks=c(0,0.25,0.5,0.75,1),
                     expand = c(0, 0)) +
  theme(strip.background = element_rect(fill="transparent")) +
  geom_text(data=jc9_env_nums,x=0,y=0,aes(label=nums),vjust=0,hjust=0) +
  #theme(strip.text.x = element_text(angle = 0, hjust = 0))+
  theme(
    legend.position = c(1, 0),
    legend.justification = c("right", "bottom"),
    #legend.box.background = element_rect(color="black", size=1),
    legend.title=element_blank(),
    legend.box.just = "right",
    legend.background = element_rect(colour = 'transparent', fill = 'transparent'),
    strip.background = element_blank(), strip.text.y = element_blank()
  )+
  guides(color = FALSE,fill=guide_legend(label.position = "left",
                                         label.hjust = 1)) +
  scale_fill_discrete(name = "Fossilization potential") +
  facet_grid(environment~taxlev,scales="free") +
  theme(panel.spacing.x=unit(2, "lines"))+
  geom_text(data=jc9_env_nums,x=0.8,y=0,aes(label=environment),vjust=0,hjust="middle",size=5) 




####PHYLUM FOSSILIZABILITY ----

ordg<-EurDat_mstr[,c("phylum","order","genus","kingdom")]
ordg<-subset(ordg,kingdom=="Animalia")
ordg<-unique(ordg[,])
ordg$OBIS_pres<-1

pb3<-pb2[,c("phylum","order","genus")]
pb3<-unique(pb3[,])
pb3<-subset(pb3,genus!="" & phylum!="" & phylum!="NO_PHYLUM_SPECIFIED" & genus!="NO_GENUS_SPECIFIED"
            & order!=""& order!="NO_ORDER_SPECIFIED")
pb3$PBDB_pres<-1
orddat2<-pb3

orddat3<-merge(ordg,orddat2,by=c("phylum","genus"),all.x=TRUE)
orddat3[is.na(orddat3)]<-0

orddat4<-orddat3 %>%
  group_by(phylum) %>%
  summarise(num_OBIS=sum(OBIS_pres),
            num_both=sum(PBDB_pres))
orddat4$perc_foss<-(orddat4$num_both/orddat4$num_OBIS)*100
orddat4$label<-paste(orddat4$phylum," (n = ",orddat4$num_OBIS,")",sep="")

ggplot(orddat4,aes(x=label,y=perc_foss))+
  geom_bar(position="stack",stat='identity',fill="coral")+
  scale_x_discrete("",limits = rev(unique(orddat4$label))) +
  scale_y_continuous("Percentage of OBIS genera with fossil record",expand = c(0, 1)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_flip()




####TAXON SPECIFIC FOSSILIZABILITY----

#Combine faunal data with site data
joined_col_temp<-Moddata_joined2[,c("phylum","class","order","family","genus",
                                    "taxon","dataset.par")]
joined_col_temp2<-merge(joined_col_temp,sitedat_joined,by=c("dataset.par"),all.x=TRUE)

#Drop data without taxonomic info
joined_col_temp2<-joined_col_temp2[!is.na(joined_col_temp2$genus),]
colnames(joined_col_temp2)[which(names(joined_col_temp2) == "id_environment")] <- "environment"

joined_col8_names<-joined_col8
joined_col8_names$taxlev<-gsub("Order","order",joined_col8_names$taxlev)
joined_col8_names$taxlev<-gsub("Family","family",joined_col8_names$taxlev)
joined_col8_names$taxlev<-gsub("Genus","genus",joined_col8_names$taxlev)

#Select final communities
tax_nums<-c()
for(i in unique(joined_col8_names$taxlev)) {
  
  joined_col8_names2<-subset(joined_col8_names,taxlev==i)
  
  finalgen<-as.data.frame(joined_col_temp2[joined_col_temp2$dataset.par %in% unique(as.character(joined_col8_names2$dataset.par)),,drop=FALSE])
  finalgen$taxlev<-finalgen[,i]
  finalgen<-finalgen[,c("dataset.par","class","phylum","taxlev")]
  finalgen<-unique(finalgen[,])
  
  
  finalgen_sums<-finalgen %>%
    group_by(dataset.par) %>%
    summarise(NumChor=sum(phylum=="Chordata"),
              NumCnid=sum(phylum=="Cnidaria"),
              NumGen=n_distinct(taxlev))
  finalgen_sums$PercChor<-finalgen_sums$NumChor/finalgen_sums$NumGen
  finalgen_sums$PercCnid<-finalgen_sums$NumCnid/finalgen_sums$NumGen
  
  finalgen_sums2<-merge(joined_col8_names2[,c("dataset.par","NumTotal","Remaining_Tax","Remaining_Env_match","environment","substrate")],finalgen_sums,by="dataset.par",all=TRUE)
  
  tax_nums<-rbind(tax_nums,cbind(finalgen_sums2,taxlev=i))
  print(i)
  
}

tax_nums$taxlev<-recode(tax_nums$taxlev, order = "Order", family="Family",genus="Genus")
colnames(tax_nums)[which(names(tax_nums) == "environment")] <- "Environment"

library(broom)

tax_nums2<-tax_nums[,c("dataset.par","Remaining_Tax","Remaining_Env_match","PercChor","taxlev","Environment")]
tax_nums2<-tax_nums2 %>%
  rename(Taxon=Remaining_Tax, Assemblage=Remaining_Env_match) %>%
  gather(key=type,value=FP, Taxon,Assemblage) %>%
  mutate(type=factor(type,levels=c("Taxon","Assemblage")))

tax_nums2$FP<-tax_nums2$FP*100
tax_nums2$PercChor<-tax_nums2$PercChor*100

tax_nums2$ValType<-factor(tax_nums2$ValType,levels=c("Remaining_Tax","Remaining_Env_match"))
tax_nums2$taxlev<-factor(tax_nums2$taxlev,levels=c("Genus","Family","Order"))


fitted_models = tax_nums2 %>% group_by(taxlev,Environment,type) %>% do(model = lm(PercChor ~ FP, data = .))
fitted_models<-fitted_models %>% tidy(model)

fitted_models<-subset(fitted_models,term=="FP")
fitted_models_labels<-fitted_models
fitted_models_labels$estimate<-round(fitted_models_labels$estimate,4)
fitted_models_labels$p.value<-round(fitted_models_labels$p.value,6)
fitted_models_labels$yas<-ifelse(fitted_models_labels$p.value<0.05,"Significant","Insignificant")
fitted_models_labels$labels<-paste("Slope =",fitted_models_labels$estimate)
fitted_models_labels$labels<-ifelse(fitted_models_labels$p.value<0.05,fitted_models_labels$labels,"")

tax_nums3<-merge(tax_nums2,fitted_models_labels,by=c("taxlev","Environment","type"))
tax_nums3$taxlev<-factor(tax_nums3$taxlev,levels=c("Order","Family","Genus"))

ggplot(tax_nums3)+
  geom_point(aes(x=FP,y=PercChor,fill=Environment),shape=21,color="transparent",alpha=0.2,show.legend=F)+
  scale_fill_manual(values=c("red","gold","darkgreen","blue","hotpink"))+
  geom_smooth(data=(tax_nums3),aes(x=FP,y=PercChor,color=Environment,group=Environment,linetype=yas),method="lm",se=F,show.legend=T,size=1.5)+
  scale_linetype_manual(values=c("dashed","solid"))+
  scale_x_continuous("Fossilization potential") +
  scale_y_continuous("Percentage of taxa assigned to Chordata",limits=c(-0.001,100)) + 
  facet_grid(type~taxlev) +
  guides(linetype = FALSE) +
  theme(legend.title=element_blank())




####BREAKDOWN PELAGIC DATASETS----

pel<-subset(joined_col8, environment=="Pelagic" & taxlev=="Genus")

pel2 <- pel %>%
  left_join(obis_id_envs,c("dataset_id"="id"))

ggplot(pel2)+
  geom_histogram(aes(x=Remaining_Tax, fill=title),binwidth = bw, position="identity",show.legend=T,boundary=0)+
  scale_x_continuous("Taxon fossilization potential",labels = function(x) scales::percent(x, accuracy = 1),breaks=c(0,0.25,0.5,0.75,1)) +
  scale_y_continuous("Number of assemblages",breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
  theme(strip.text.y = element_blank()) +
  guides(fill=guide_legend(title="Dataset title"))+
  facet_grid(title~., scales="free")



####BIOCEAN COMPARISON----

library(broom)

depth_pre3<-subset(joined_col8, dataset_id=="b0a7add2-dd9e-4020-9ca4-5df048c8f6a2")
length(unique(depth_pre3$dataset.par))
depth_pre3$Remaining_Tax<-depth_pre3$Remaining_Tax*100
depth_pre3$Remaining_Env_match<-depth_pre3$Remaining_Env_match*100
depth_pre3<-depth_pre3 %>% gather(Remaining_Tax,Remaining_Env_match,key="ValType",value="Value")

depth_pre3$ValType<-factor(depth_pre3$ValType,levels=c("Remaining_Tax","Remaining_Env_match"))
depth_pre3$taxlev<-factor(depth_pre3$taxlev,levels=c("Genus","Family","Order"))

fitted_models = depth_pre3 %>% group_by(taxlev,ValType) %>% do(model = lm(Value ~ depth.slice, data = .))
fitted_models<-fitted_models %>% tidy(model)

fitted_models<-subset(fitted_models,term=="depth.slice")
fitted_models_labels<-fitted_models
fitted_models_labels$estimate<-round(fitted_models_labels$estimate,4)
fitted_models_labels$p.value<-round(fitted_models_labels$p.value,6)
fitted_models_labels$yas<-ifelse(fitted_models_labels$p.value<0.05,"Sig.","Insig.")
fitted_models_labels$labels<-paste("Slope =",fitted_models_labels$estimate)
fitted_models_labels$labels<-ifelse(fitted_models_labels$p.value<0.05,fitted_models_labels$labels,"")
fitted_models_labels$dataset_id<-NA


depth_pre3$ValType<-recode(depth_pre3$ValType, Remaining_Tax = "Taxon", Remaining_Env_match="Assemblage")
fitted_models_labels$ValType<-recode(fitted_models_labels$ValType, Remaining_Tax = "Taxon", Remaining_Env_match="Assemblage")

depth_pre3$taxlev<-factor(depth_pre3$taxlev,levels=c("Order","Family","Genus"))

ggplot(depth_pre3,aes(x=depth.slice,y=Value,group=1,fill=ValType,color=ValType),show.legend=F)+
  xlab("Depth (m)") + ylab("Fossilization potential") +
  geom_point(show.legend=F,alpha=0.5)+
  coord_cartesian(ylim=c(0,100)) +
  geom_smooth(data=(depth_pre3 %>%
                      filter(!(taxlev=="Genus" & ValType=="Taxon"))),method="lm",show.legend=F,color="black")+
  geom_text(data=fitted_models_labels, aes(label=labels),x=2000, y = 95,hjust=0,show.legend=F,color="black") +
  ggtitle("BIOOCEAN") +facet_grid(ValType~taxlev)

####SUMMARY STATS----

#All data - taxon foss
table_col5<-joined_col5[,c("dataset.par","taxlev","Remaining_Env_match","Remaining_Tax")]
table_col5$Remaining_Tax<-table_col5$Remaining_Tax*100
table_col5<-unique(table_col5[,])
table_col5_means_all_tax<-table_col5 %>%
  group_by(taxlev) %>%
  summarise(tax_mean=mean(Remaining_Tax,na.rm=T),
            tax_sd=sd(Remaining_Tax,na.rm=T),
            tax_n=n_distinct(dataset.par))
table_col5_means_all_tax$type<-"All data"
table_col5_means_all_tax$ValType<-"Taxon"

#All data - taxon+env foss
table_col8<-joined_col8[,c("dataset.par","taxlev","Remaining_Env_match","Remaining_Tax","environment")]
table_col8$Remaining_Env_match<-table_col8$Remaining_Env_match*100
table_col8_means_all_env<-table_col8 %>%
  group_by(taxlev) %>%
  summarise(tax_mean=mean(Remaining_Env_match,na.rm=T),
            tax_sd=sd(Remaining_Env_match,na.rm=T),
            tax_n=n_distinct(dataset.par))
table_col8_means_all_env$type<-"All data"
table_col8_means_all_env$ValType<-"Taxon + Env."

#Environment - taxon
table_col8<-joined_col8[,c("dataset.par","taxlev","Remaining_Env_match","Remaining_Tax","environment")]
table_col8$Remaining_Tax<-table_col8$Remaining_Tax*100
table_col8<-unique(table_col8[,])
table_col8_means_env_tax<-table_col8 %>%
  group_by(taxlev,environment) %>%
  summarise(tax_mean=mean(Remaining_Tax,na.rm=T),
            tax_sd=sd(Remaining_Tax,na.rm=T),
            tax_n=n_distinct(dataset.par))
table_col8_means_env_tax$type<-"Environment"
table_col8_means_env_tax$ValType<-"Taxon"

#Environment - taxon + env
table_col8<-joined_col8[,c("dataset.par","taxlev","Remaining_Env_match","Remaining_Tax","environment")]
#table_col8<-subset(table_col8,environment=="Shallow" | environment=="Deep" | environment=="Coral reef" )
table_col8$Remaining_Env_match<-table_col8$Remaining_Env_match*100
table_col8<-unique(table_col8[,])
table_col8_means_env_env<-table_col8 %>%
  group_by(taxlev,environment) %>%
  summarise(
    tax_mean=mean(Remaining_Env_match,na.rm=T),
    tax_sd=sd(Remaining_Env_match,na.rm=T),
    tax_n=n_distinct(dataset.par))
table_col8_means_env_env$type<-"Environment"
table_col8_means_env_env$ValType<-"Taxon + Env."

#Substrate - taxon
table_col8<-as.data.frame(joined_col8[,c("dataset.par","taxlev","Remaining_Env_match","Remaining_Tax","environment","substrate")])
table_col8<-subset(table_col8,environment=="Shallow")
table_col8<-unique(table_col8[,])
table_col8$Remaining_Tax<-table_col8$Remaining_Tax*100
table_col8_means_sub_tax<-table_col8 %>%
  group_by(taxlev,substrate) %>%
  summarise(tax_mean=mean(Remaining_Tax,na.rm=T),
            tax_sd=sd(Remaining_Tax,na.rm=T),
            tax_n=n_distinct(dataset.par)) %>%
  drop_na(substrate)
table_col8_means_sub_tax$type<-"Substrate"
table_col8_means_sub_tax$ValType<-"Taxon"



#Merge all dataframes
t<-as.data.frame(bind_rows(table_col5_means_all_tax,table_col8_means_all_env,table_col8_means_env_tax,table_col8_means_env_env,table_col8_means_sub_tax))
t$tax_mean<-round(t$tax_mean,0)
t$tax_sd<-round(t$tax_sd,0)

t[is.na(t)] <- ""
t$envtype<-paste(t$environment,t$substrate,sep="")
t$envtype<-gsub("NA","",t$envtype)
t$substrate<-NULL
t$environment<-NULL
tt<-dcast(setDT(t), type+envtype~taxlev+ValType, value.var=c('tax_mean', 'tax_sd',"tax_n"))

#Sort column names
lab1<-c("tax_mean","tax_sd","tax_n")
lab2<-unique(t$taxlev)
lab3<-unique(t$ValType)

lab_join<-expand.grid(lab1,lab2,lab3)
lab_join_order<-lab_join[order(lab_join$Var2,lab_join$Var3,lab_join$Var1),]
lab_join_order_names<-paste(lab_join_order[,c(1)],lab_join_order[,c(2)],lab_join_order[,c(3)],sep="_")
lab_join_order_names<-c("type","envtype",as.character(lab_join_order_names))

ttt<-setcolorder(tt, as.character(lab_join_order_names))

#Row order
row_order<-c("","Shallow","Coral reef","Deep","Pelagic","Seamount","Rock","Gravel","Sand","Mud")
tttt<-ttt[match(row_order, ttt$envtype),]

#write.csv(tttt,"/Users/jos23/Downloads/testsummarytable2.csv")

####ALPHA DIVERSITY----
library(tidyverse)
library(patchwork)
datfra_alp<-joined_col8 %>%
  dplyr::select(NumTotal,Remaining_Tax,Remaining_Env_match,environment,substrate) %>%
  pivot_longer(cols=c(Remaining_Tax,Remaining_Env_match),names_to="valtype",values_to="value")

datfra_alp$environment<-factor(datfra_alp$environment, levels=c("Shallow","Coral reef","Deep","Pelagic","Seamount"))
datfra_alp$substrate<-factor(datfra_alp$substrate, levels=c("Rock","Gravel","Sand","Mud"))
datfra_alp$valtype<-recode(datfra_alp$valtype, 
                           Remaining_Tax="Taxon fossilization potential",
                           Remaining_Env_match="Within-environment fossilization potential")

p1<-ggplot(datfra_alp)+geom_point(aes(x=NumTotal,y=value*100),alpha=0.4)+facet_grid(environment~valtype)+xlab("Genus alpha diversity")+ylab("Fossilization potential")
p2<-ggplot(datfra_alp %>% drop_na(substrate))+geom_point(aes(x=NumTotal,y=value*100),alpha=0.4)+facet_grid(substrate~valtype)+xlab("Genus alpha diversity")+ylab("Fossilization potential")

p1+p2



####FINAL FAUNAL LIST----

####Taxon FP
t<-joined_col5
finalgen<-as.data.frame(Moddata_joined2[Moddata_joined2$dataset.par %in% unique(as.character(t$dataset.par)),,drop=FALSE])
finalgen<-finalgen[,c("dataset.par","phylum","order","family","genus","dataset_id")]
finalgen<-unique(finalgen[,])
#Number of taxa in final dataset
length(unique(na.omit(finalgen$dataset.par)))
length(unique(na.omit(finalgen$dataset_id)))
length(unique(na.omit(finalgen$genus)))
length(unique(na.omit(finalgen$family)))
length(unique(na.omit(finalgen$order)))
#Number of taxa genus info
sum(is.na(finalgen$genus))
sum(is.na(finalgen$family))
sum(is.na(finalgen$order))

####Asssemblage FP
t<-joined_col8
finalgen8<-as.data.frame(Moddata_joined2[Moddata_joined2$dataset.par %in% unique(as.character(t$dataset.par)),,drop=FALSE])
finalgen8<-finalgen8[,c("dataset.par","phylum","order","family","genus","dataset_id")]
finalgen8<-unique(finalgen[,])
#Number of taxa in final dataset
length(unique(na.omit(finalgen8$dataset.par)))
length(unique(na.omit(finalgen8$dataset_id)))
length(unique(na.omit(finalgen8$genus)))
length(unique(na.omit(finalgen8$family)))
length(unique(na.omit(finalgen8$order)))
#Number of taxa genus info
sum(is.na(finalgen8$genus))
sum(is.na(finalgen8$family))
sum(is.na(finalgen8$order))

####ADDITIONAL ANALYSES----

####PBDB size----
length(unique(na.omit(pb$genus)))
length(unique(na.omit(pb2$family)))
length(unique(na.omit(pb2$order)))


####Num of taxa with env data----
taxnam<-paste("NO_",toupper("genus"),"_SPECIFIED",sep="")
try<-merge(pb,pb_baz,by=c("environment"),all.x=TRUE)
try2<-try[,c("genus","OBISenvs","phylum")]
try2<-subset(try2,genus!="" & phylum!="" & phylum!="NO_PHYLUM_SPECIFIED" & genus!="NO_GENUS_SPECIFIED")
try2<-unique(try2[,])
try3<-try2[!is.na(try2$OBISenvs),]
try3<-subset(try3,genus!="" & OBISenvs!="terrestrial")
#How many genera altogether
length(unique(tolower(na.omit(pb$genus))))
#How many genera with env data
length(unique(tolower(na.omit(try3$genus))))
#How many genus+env combos
nrow(try3)
#How many of each environment
table(try3$OBISenvs,exclude=NULL)

####Number of Lagerstatten taxa----
laglist2<-subset(laglist,Lagerstatten=="1" | Lagerstatten=="?")
lagtest<-as.data.frame(pb[pb$formation %in% laglist2$`Formation Name`,,drop=FALSE])
nrow(lagtest)
length(unique(na.omit(lagtest$genus)))
#How many Lag taxa recorded in OBIS
length(intersect(unique(na.omit(lagtest$genus)),unique(na.omit(finalgen$genus))))

#How many taxa only recorded in lags and not shelly----
nonlagnames<-as.data.frame(pb[!pb$formation %in% laglist2$`Formation Name`,,drop=FALSE])
pbtlag<-as.data.frame(pb[pb$formation %in% laglist2$`Formation Name`,,drop=FALSE])
length(setdiff(unique(na.omit(pbtlag$genus)),unique(na.omit(nonlagnames$genus))))

####Count number of lag taxa altogether and compare to OBIS----
uni_lag <- c()
for (i in c("order", "family", "genus")) {
  
  nonlagnames <- as.data.frame(pb[!pb$formation %in% laglist2$`Formation Name`, , drop = FALSE])
  
  nonlagnames <- unique(tolower(as.character(nonlagnames[, c(i)])))
  
  pbtlag <- as.data.frame(pb[pb$formation %in% laglist2$`Formation Name`, , drop = FALSE])
  pbtlag$taxlev <- tolower(pbtlag[, c(i)])
  
  pbtlag <- pbtlag[, c("phylum", "taxlev")]
  pbtlag <- unique(pbtlag[, ])
  
  all_tax <- length(na.omit(unique(pb[, c(i)])))
  
  # Lagerstatten only genera
  pbtlag <- as.data.frame(pbtlag[!pbtlag$taxlev %in% nonlagnames, , drop = FALSE])
  lag_tax <- length(na.omit(unique(pbtlag$taxlev)))
  
  #Lagerstatten only genera in OBIS
  eurmin <- as.data.frame(EurDat_mstr)
  eurmin$taxlev <- tolower(eurmin[, c(i)])
  
  lagmin <- as.data.frame(pbtlag[(pbtlag$taxlev) %in% (eurmin$taxlev), , drop = FALSE])
  obis_lag_tax<- length(na.omit(unique(lagmin$taxlev)))
  
  uni_lag <- rbind(uni_lag, cbind(taxlev = i, all = all_tax, lag = lag_tax, obis_lag_tax=obis_lag_tax))
  print(i)
}
uni_lag <- as.data.frame(uni_lag)


####Origination ages of OBIS subset taxa that are recorded in PBDB----

pbmin <- as.data.frame(pb)
pbmin$genus <- ((tolower(pbmin[, c("genus")])))

pbmin2 <- pbmin %>%
  group_by(genus) %>%
  summarise(FAD=max(max_ma,na.rm=T)) %>%
  drop_na(genus)

# Calculate origination ages
eurmin <- as.data.frame(finalgen)
obisnames <- unique(na.omit(tolower(eurmin[, c("genus")])))

obmin <- as.data.frame(pbmin2[(pbmin2$genus) %in% obisnames, , drop = FALSE])

obmin2 <- obmin %>%
  mutate(era = cut(FAD, breaks = c(0, 66, 252, 541))) %>%
  group_by(era) %>%
  count(EraOriginations = n_distinct(genus)) %>%
  drop_na(era) %>%
  group_by()%>%
  mutate(EraOriginationsPerc = 100 * EraOriginations / sum(EraOriginations))

####Number of pelagic and seamount taxa----
pelse<-subset(pb2,environment=="Seamount")
length(unique(na.omit(pelse$formation)))
length(unique(na.omit(pelse$genus)))
#How many pelagic taxa in obis
pel<-subset(joined_col8,environment=="Seamount")
notpel<-subset(joined_col8,environment!="Seamount")
tax_pel<-as.data.frame(Moddata_joined2[Moddata_joined2$dataset.par %in% unique(as.character(pel$dataset.par)),,drop=FALSE])
tax_nonpel<-as.data.frame(Moddata_joined2[Moddata_joined2$dataset.par %in% unique(as.character(notpel$dataset.par)),,drop=FALSE])
tax_all<-as.data.frame(Moddata_joined2[Moddata_joined2$dataset.par %in% unique(as.character(joined_col8$dataset.par)),,drop=FALSE])
length(setdiff(unique(na.omit(tax_pel$genus)),unique(na.omit(tax_nonpel$genus)))) #Number of taxa exclusive to pelagic
length(unique(na.omit(tax_all$genus))) #Number of taxa in assemblages used for assemblage fossilization
#And of the taxa that are exclusive to pelagic OBIS, how many are in pbdb
t<-(setdiff(unique(na.omit(tax_pel$genus)),unique(na.omit(tax_nonpel$genus)))) #Number of taxa exclusive to pelagic
length(setdiff(unique(t),unique(na.omit(pb$genus)))) #Number of taxa exclusive to pelagic and in the PBDB


####Analysis of species durations----

abms<-tax_durs3
#abms<-read.csv("SupDataDR6_ABM_OBIS_ages.csv")

abms$origFAD<-abms$ogFAD
abms$infFAD<-abms$meanFAD
abms<-abms[,c("genus","origFAD","infFAD")]
abms$pb_presence<-1

# Need to replace Moddata with properly culled version
ob_durations <- finalgen8[, c("phylum", "genus", "dataset.par")] %>% distinct() %>%
  left_join(abms, by = c("genus")) %>%
  # If the taxon is present today, then LAD must be 0
  mutate(LAD = 0) %>%
  mutate(stratRange = origFAD - LAD) %>%
  mutate(stratRange = replace_na(stratRange, 0)) %>%
  mutate(taxonRange = infFAD - LAD) %>%
  mutate(taxonRange = replace_na(taxonRange, 0)) %>%
  mutate(pb_presence = replace_na(pb_presence, 0)) %>%
  left_join(joined_col8 %>% filter(taxlev == "Genus") %>% distinct(), by = c("dataset.par")) %>%
  filter(!is.na(environment))

ob_durations$environment<-factor(ob_durations$environment,levels=rev(c("Shallow","Coral reef","Deep","Pelagic","Seamount")))
ob_durations$substrate = factor(ob_durations$substrate, levels=(c("Mud","Sand","Gravel","Rock")))

ggplot(data=ob_durations)+ geom_histogram(aes(x = taxonRange),fill="red",alpha=0.2) + geom_histogram(aes(x = stratRange),fill="blue",alpha=0.2) 

#Plot by environment
p1<-ggplot(data=ob_durations %>% dplyr::select(genus, environment, taxonRange,pb_presence) %>% filter(pb_presence==1) %>% distinct()) + 
  geom_boxplot(aes(x = environment,y=taxonRange),alpha=0.2,outlier.color=NA) +
  coord_flip(ylim=c(0,300))+ xlab("Environment") + ylab("Taxon duration (Myrs)")
p2<-ggplot(data=ob_durations %>% dplyr::select(genus, substrate, taxonRange,pb_presence) %>% filter(pb_presence==1 & substrate!="NA") %>% distinct()) + 
  geom_boxplot(aes(x = substrate,y=taxonRange),alpha=0.2,outlier.color=NA) +
  coord_flip(ylim=c(0,300)) + xlab("Substrate") + ylab("Taxon duration (Myrs)")
p1+p2


#Generate stats
d1<-ob_durations %>% dplyr::select(genus, environment, taxonRange,pb_presence) %>% filter(pb_presence==1) %>% distinct() %>%
  group_by(environment) %>%
  summarise(boxplot= list( boxplot.stats(taxonRange)$stats), stat = list( c('lower_whisker','lower_hinge','median','upper_hinge','upper_whisker') ) ) %>%
  unnest(c(stat, boxplot)) %>%
  spread(stat, boxplot)
d2<-ob_durations %>% dplyr::select(genus, substrate, taxonRange,pb_presence) %>% filter(pb_presence==1 & substrate!="NA") %>% distinct() %>%
  group_by(substrate) %>%
  summarise(boxplot= list( boxplot.stats(taxonRange)$stats), stat = list( c('lower_whisker','lower_hinge','median','upper_hinge','upper_whisker') ) ) %>%
  unnest(c(stat, boxplot)) %>%
  spread(stat, boxplot)
bind_rows(d1,d2)


#Plot as mean 
ob_durations_stats <- ob_durations  %>%  filter(pb_presence == 1) %>%
  group_by(dataset.par, substrate, environment, LatBin, LongBin, Remaining_Tax, Remaining_Env_match) %>%
  summarise(
    duration_mean = mean(taxonRange, na.rm = T),
    duration_sd = sd(taxonRange, na.rm = T)
  )

ob_durations_stats$environment<-factor(ob_durations_stats$environment,levels=rev(c("Shallow","Coral reef","Deep","Pelagic","Seamount")))
ob_durations_stats$substrate = factor(ob_durations_stats$substrate, levels=(c("Mud","Sand","Gravel","Rock")))

p1<-ggplot(ob_durations_stats) + geom_boxplot(aes(y=environment,x = duration_mean),show.legend=F,outlier.color=NA)+ xlab("Mean genus duration by assemblage (Myrs)")+ylab("")+coord_cartesian(xlim=c(0,450))
p2<-ggplot(ob_durations_stats) + geom_boxplot(aes(y=environment,x = duration_sd),show.legend=F,outlier.color=NA)+ xlab("Std. dev. of genus duration by assemblage (Myrs)")+ylab("")+coord_cartesian(xlim=c(0,450))
p3<-ggplot(ob_durations_stats %>% filter(!is.na(substrate))) + geom_boxplot(aes(y=substrate,x = duration_mean),show.legend=F,outlier.color=NA)+ xlab("Mean genus duration by assemblage (Myrs)")+ylab("")+coord_cartesian(xlim=c(0,450))
p4<-ggplot(ob_durations_stats %>% filter(!is.na(substrate))) + geom_boxplot(aes(y=substrate,x = duration_sd),show.legend=F,outlier.color=NA)+ xlab("Std. dev. of genus duration by assemblage (Myrs)")+ylab("")+coord_cartesian(xlim=c(0,450))
(p1|p3)/(p2|p4)


#Generate stats
d1<-ob_durations_stats %>% 
  group_by(environment) %>%
  summarise(boxplot= list( boxplot.stats(duration_mean)$stats), stat = list( c('lower_whisker','lower_hinge','median','upper_hinge','upper_whisker') ) ) %>%
  unnest(c(stat, boxplot)) %>%
  spread(stat, boxplot) %>% mutate(type="mean")
d2<-ob_durations_stats %>% 
  group_by(environment) %>%
  summarise(boxplot= list( boxplot.stats(duration_sd)$stats), stat = list( c('lower_whisker','lower_hinge','median','upper_hinge','upper_whisker') ) ) %>%
  unnest(c(stat, boxplot)) %>%
  spread(stat, boxplot) %>% mutate(type="sd")
d3<-ob_durations_stats %>% filter(!is.na(substrate)) %>%
  group_by(substrate) %>%
  summarise(boxplot= list( boxplot.stats(duration_mean)$stats), stat = list( c('lower_whisker','lower_hinge','median','upper_hinge','upper_whisker') ) ) %>%
  unnest(c(stat, boxplot)) %>%
  spread(stat, boxplot) %>% mutate(type="mean")
d4<-ob_durations_stats %>% filter(!is.na(substrate))%>% 
  group_by(substrate) %>%
  summarise(boxplot= list( boxplot.stats(duration_sd)$stats), stat = list( c('lower_whisker','lower_hinge','median','upper_hinge','upper_whisker') ) ) %>%
  unnest(c(stat, boxplot)) %>%
  spread(stat, boxplot) %>% mutate(type="sd")

bind_rows(d1,d2,d3,d4)

#Run linear regressions for each
library(dplyr)
library(broom)

summary(lm(Remaining_Tax*100~duration_mean, data=ob_durations_stats))
summary(lm(Remaining_Env_match*100~duration_mean, data=ob_durations_stats))


ob_durations_regr_tax<-ob_durations_stats %>% 
  group_by(environment)  %>% 
  do(tidy(lm(Remaining_Tax~duration_mean, .))) %>% 
  filter(term=="duration_mean") %>%
  mutate(p.value=round(p.value,4)) %>%
  mutate(sig=ifelse(p.value<=0.05,"Sig","Insig"))

ob_durations_regr_ass<-ob_durations_stats %>% 
  group_by(environment)  %>% 
  do(tidy(lm(Remaining_Env_match~duration_mean, .))) %>% 
  filter(term=="duration_mean") %>%
  mutate(p.value=round(p.value,4)) %>%
  mutate(sig=ifelse(p.value<=0.05,"Sig","Insig"))

ob_durations_stats2<-ob_durations_stats %>%
  left_join(ob_durations_regr_tax[,c("environment","sig")],by=c("environment")) %>%rename("sig_Tax"="sig")%>%
  left_join(ob_durations_regr_ass[,c("environment","sig")],by=c("environment")) %>%rename("sig_Env_match"="sig")

ob_durations_stats2$sig_Tax<-as.factor(ob_durations_stats2$sig_Tax)

#Plot only significant regressions
ggplot(ob_durations_stats2)+geom_point(aes(x=LongBin,y=LatBin,color=log(duration_mean)))

ggplot(ob_durations_stats2,aes(x = duration_mean, y = Remaining_Tax, color = environment)) +
  geom_point(alpha = 0.1)+
  stat_smooth(data=ob_durations_stats2,aes(alpha=sig_Tax),size=2,method="lm",se=F,geom="line")+
  scale_alpha_manual(values=c(0,1))
ggplot(ob_durations_stats2,aes(x = duration_mean, y = Remaining_Env_match, color = environment)) +
  geom_point(alpha = 0.1)+
  stat_smooth(data=ob_durations_stats2,aes(alpha=sig_Env_match),size=2,method="lm",se=F,geom="line")+
  scale_alpha_manual(values=c(0,1))

ggplot(ob_durations_stats2,aes(x = duration_sd, y = Remaining_Tax, color = environment)) +
  geom_point(alpha = 0.1)+
  stat_smooth(data=ob_durations_stats2,aes(alpha=sig_Tax),size=2,method="lm",se=F,geom="line")+
  scale_alpha_manual(values=c(0,1))
ggplot(ob_durations_stats2,aes(x = duration_sd, y = Remaining_Env_match, color = environment)) +
  geom_point(alpha = 0.1)+
  stat_smooth(data=ob_durations_stats2,aes(alpha=sig_Env_match),size=2,method="lm",se=F,geom="line")+
  scale_alpha_manual(values=c(0,1))

ggplot(ob_durations_stats2,aes(x = duration_mean, y = Remaining_Tax, color = environment)) +
  geom_point(alpha = 0.1)+
  stat_smooth(data=ob_durations_stats2,aes(alpha=sig_Tax),size=2,method="lm",se=F,geom="line")+
  scale_alpha_manual(values=c(1,0))+facet_wrap(.~environment,scales="free")
ggplot(ob_durations_stats2,aes(x = duration_mean, y = Remaining_Env_match, color = environment)) +
  geom_point(alpha = 0.1)+
  stat_smooth(data=ob_durations_stats2,aes(alpha=sig_Env_match),size=2,method="lm",se=F,geom="line")+
  scale_alpha_manual(values=c(1,0))+facet_wrap(.~environment,scales="free")

####Cenozoic PBDB map----
t<-fread("SupDataDR3A.csv", header = T, sep = ',',select=c("environment","genus","formation","max_ma","lat","lng")) 

mar_envs<-c("fluvial indet.", "fluvial-deltaic indet.", "fluvial-lacustrine indet.", "lacustrine - large", "lacustrine - small", "lacustrine delta front", "lacustrine delta plain", "lacustrine deltaic indet.", "lacustrine indet.", "delta front", "prodelta", "delta plain", "deltaic indet.", "estuary/bay", "foreshore", "lagoonal/restricted shallow subtidal", "marginal marine indet.", "paralic indet.", "peritidal", "interdistributary bay", "lagoonal", "coastal indet.", "intrashelf/intraplatform reef", "open shallow subtidal", "sand shoal", "shallow subtidal indet.", "shoreface", "transition zone/lower shoreface", "submarine fan", "deep subtidal indet.", "deep subtidal ramp", "deep subtidal shelf", "perireef or subreef", "platform/shelf-margin reef", "reef, buildup or bioherm", "slope/ramp reef", "offshore", "offshore indet.", "offshore shelf", "offshore ramp", "slope", "basinal (carbonate)", "basinal (siliceous)", "basinal (siliciclastic)", "basin reef", "deep-water indet.", "carbonate indet.", "marine indet.")
t<-as.data.frame(t[t$environment %in% mar_envs,,drop=FALSE])

t2<-t%>% filter(max_ma<=66) %>% distinct()

round_any = function(x, accuracy, f=ceiling){f(x/ accuracy) * accuracy}
t2$latbin<-round_any(t2$lat,accuracy=5)
t2$lngbin<-round_any(t2$lng,accuracy=5)

t3 <- t2 %>%
  group_by(latbin,lngbin) %>%
  summarise(alphagenus=n()) # %>% filter(alphagenus>=5)

world_map <- map_data("world")

ggplot() +
  geom_polygon(data=world_map,fill="lightgrey", aes(x = long, y = lat,group=group),show.legend=F) +
  geom_tile(data=t3,aes(x=lngbin,y=latbin,fill=log(alphagenus)),alpha=0.5,show.legend=T)+
  scale_fill_gradientn(colours = rev(rainbow(4)),
                       guide = guide_colourbar(direction = "horizontal"),
                       name="Log number of genus occurrences")+
  theme_void()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = c("bottom")) 

