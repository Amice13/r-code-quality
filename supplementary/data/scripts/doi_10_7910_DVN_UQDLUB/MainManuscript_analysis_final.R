rm(list=ls())
packages <- c("mediation","foreign","DirectEffects","lmtest","lfe","dplyr","broom","ggplot2","stargazer","sandwich","conleyreg","maps","ggstance")

package_installed <-
  sapply(packages, function(pack)
    pack %in% rownames(installed.packages()))
if (any(!package_installed)) {
  sapply(packages[!package_installed], install.packages)
}

sapply(packages, require, character.only = TRUE)
rm(packages,package_installed)


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# FIGURE 1: FlowMap, 1995-2015.
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

county<- map_data("county")

county  <- county %>%
  mutate(polyname = paste(region,subregion,sep=",")) %>%
  left_join(county.fips, by="polyname")

countymap_all <- read.csv("flowmap_all.csv")

a<-ggplot() + 
  geom_polygon(data= county, aes(long,lat, group=group), fill="gray60") +
  geom_point(data=countymap_all, aes(x=long.x, y=lat.x), color="gray1", size=1, alpha=0.5) +
  geom_point(data=countymap_all, aes(x=long.end, y=lat.end), color="gray1", size=1, alpha=0.5) +
  geom_segment(data = countymap_all, aes(x = long.x, y = lat.x, xend = long.end, yend = lat.end),
               size=0.1,
               arrow = arrow( type="closed", length = unit(0.015, "npc"))) +
  theme(axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  coord_equal()
a
ggsave(a, file="flowmap_final_final.pdf", family="sans", width=40, height=17, units= "cm", dpi=800)


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
#MAIN Table and coefficient plots
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

data1<-read.csv("maindata.csv")

main_czfe_noint<-felm(DemVotesMajorPercent~ d_cz_HQexo  + d_cz_HQin | stateyear + czone_state | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
main_czfe<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin | stateyear + czone_state | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
main_czfer<-felm(DemVotesMajorPercent~ d_cz_HQexo + intd_d_cz_HQexo + d_cz_HQin + intd_d_cz_HQin | stateyear + czone_state | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
main_cfe_noint<-felm(DemVotesMajorPercent~ d_cz_HQexo + d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
main_cfe<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
main_cfer<-felm(DemVotesMajorPercent~ d_cz_HQexo + intd_d_cz_HQexo + d_cz_HQin + intd_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

#-----------------------------------------------------------------------#
#TABLE 1 - main table
#-----------------------------------------------------------------------#

stargazer(main_czfe_noint, main_czfe, main_cfe_noint, main_cfe,
          #dep.var.caption = "\\emph{Dependent Variable=Dem.Gov.Vote Share}",
          dep.var.labels.include = FALSE,
          #dep.var.labels = c("President", "Senate","House", "Federal Average"),
          omit = "^DemVotesMajorPercent",
          order = c("d_cz_HQin","int_d_cz_HQin","d_cz_HQexo","int_d_cz_HQexo"),
          covariate.labels = c("Headquarters Inflow", "Headquarters Inflow X Republican Governor",
                               "Headquarters Outflow", "Headquarters Outflow X Republican Governor"
          ),
          float=F,
          add.lines = list(c("State-year fixed effects","Yes","Yes","Yes","Yes"),c("Commuting zone fixed effects","Yes","Yes","No","No"),c("County fixed effects","No","No","Yes","Yes")),
          keep.stat = "n",
          star.cutoffs = c(0.1,0.05,0.01),
          star.char = c("†", "*", "**"),
          omit.table.layout = "n",
          column.sep.width = "10pt",
          notes = "Standard errors clustered by commuting zone.",notes.align = "l",
          out = "Tab_main_final.tex")

#-----------------------------------------------------------------------#
#FIGURE 2 - main coefficient plots
#-----------------------------------------------------------------------#

main_cfe<-tidy(main_cfe, conf.int = TRUE, se.type = "cluster")
main_cfe<-main_cfe %>% mutate(type="Democratic Governor") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.645*std.error) %>% mutate(upper90=estimate+1.645*std.error)
main_cfer<-tidy(main_cfer, conf.int = TRUE, se.type = "cluster")
main_cfer<-main_cfer %>% mutate(type="Republican Governor") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(estimate= -1*estimate) %>% mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)  %>%
  mutate(lower90=estimate-1.645*std.error) %>% mutate(upper90=estimate+1.645*std.error)

all<-rbind(main_cfe, main_cfer)
all$term[all$term=="d_cz_HQin"] <- "Headquarters Inflow"
all$term[all$term=="d_cz_HQexo"] <- "Headquarters Outflow"

all$ord <-factor(all$term, levels = c("Headquarters Outflow","Headquarters Inflow"))
all$ord2 <-factor(all$type, levels = c("Republican Governor","Democratic Governor"))
#all<- all[order(all$ord),]

all %>% 
  filter(estimate!=0) %>% 
  ggplot(aes(estimate, ord, shape=as.factor(ord2))) +
  scale_colour_manual(values=c("red", "blue"))+
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  geom_text(aes(label=round(estimate,2)),position=position_dodge(width = .8),size=10)+
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.6) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1.4) +
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Effect on Incumbent Party Vote Share", expand=c(0,0), limits = c(-4,4)) +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    axis.text.x = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=17),
    legend.text = element_text(size = 20, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position = c(0.22, 0.90),
    #legend.position="right",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  #  theme_bw()+
  guides(shape=guide_legend(reverse = TRUE, override.aes = list(size = 1.95)))-> p
p


ggsave(p, file="Fig_Main_final.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# DIVIDED GOVERNMENT
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

ddd<-felm(DemVotesMajorPercent~ d_cz_HQin+in_rrr+in_rd+in_dr+d_cz_HQexo+exo_rrr+exo_rd+exo_dr | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
rrr<-felm(DemVotesMajorPercent~ d_cz_HQin+in_ddd+in_rd+in_dr+d_cz_HQexo+exo_ddd+exo_rd+exo_dr | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
rd<-felm(DemVotesMajorPercent~ d_cz_HQin+in_ddd+in_rrr+in_dr+d_cz_HQexo+exo_ddd+exo_rrr+exo_dr | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
dr<-felm(DemVotesMajorPercent~ d_cz_HQin+in_ddd+in_rrr+in_rd+d_cz_HQexo+exo_ddd+exo_rrr+exo_rd | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

#-----------------------------------------------------------------------#
#FIGURE 3 Divided government 
#-----------------------------------------------------------------------#

ddd<-tidy(ddd, conf.int = TRUE, se.type = "cluster")
ddd<-ddd %>% mutate(type="Democratic Control") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.645*std.error) %>% mutate(upper90=estimate+1.645*std.error)
dr<-tidy(dr, conf.int = TRUE, se.type = "cluster")
dr<-dr %>% mutate(type="Divided (Democratic Governor)") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)%>%
  mutate(lower90=estimate-1.645*std.error) %>% mutate(upper90=estimate+1.645*std.error)

rrr<-tidy(rrr, conf.int = TRUE, se.type = "cluster")
rrr<-rrr %>% mutate(type="Republican Control") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(estimate= -1*estimate) %>% mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.645*std.error) %>% mutate(upper90=estimate+1.645*std.error)
rd<-tidy(rd, conf.int = TRUE, se.type = "cluster")
rd<-rd %>% mutate(type="Divided (Republican Governor)") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(estimate= -1*estimate) %>% mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.645*std.error) %>% mutate(upper90=estimate+1.645*std.error)

all<-rbind(ddd, dr, rd, rrr)
all$term[all$term=="d_cz_HQin"] <- "Headquarters Inflow"
all$term[all$term=="d_cz_HQexo"] <- "Headquarters Outflow"

all$ord <-factor(all$term, levels = c("Headquarters Outflow","Headquarters Inflow"))
all$ord2 <-factor(all$type, levels = c("Republican Control","Divided (Republican Governor)","Divided (Democratic Governor)","Democratic Control"))
#all<- all[order(all$ord),]


all %>% 
  filter(estimate!=0) %>% 
  ggplot(aes(estimate, ord, shape=as.factor(ord2))) +
  scale_colour_manual(values=c("red", "orange", "skyblue","blue"))+
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.6) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1.4) +
  #geom_text(aes(label=round(estimate,2)),position=position_dodge(width = .1),size=5)+
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Effect on Incumbent Party Vote Share", expand=c(0,0), limits = c(-5,4.9)) +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    axis.text.x = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=17),
    legend.text = element_text(size = 18, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position = c(0.25, 0.9),
    #legend.position="right",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  #  theme_bw()+
  guides(shape=guide_legend(reverse = TRUE,override.aes = list(size = 1.6)))-> p
p

ggsave(p, file="Fig_Divided_final.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)



#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# CCES PANEL ANALYSIS
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

data_cces <- read.csv("ccespaneldata.csv")

#-----------------------------------------------------------------------#
#TABLE 2 CCES PANEL
#-----------------------------------------------------------------------#

datad_cces <-subset(data_cces, data_cces$inc_party_d==1 & data_cces$move==0)
datar_cces <-subset(data_cces, data_cces$inc_party_r==1 & data_cces$move==0)

estd_noc_cid<-felm(govapp ~ cz_b_hq_exo + cz_b_hq_in  | sy+cid | 0 |czone_state, weights=datad_cces$weight, data=datad_cces)
estr_noc_cid<-felm(govapp ~ cz_b_hq_exo + cz_b_hq_in  | sy+cid | 0 |czone_state, weights=datar_cces$weight, data=datar_cces)

estd_noc_caseid<-felm(govapp ~ cz_b_hq_exo + cz_b_hq_in  | sy+caseid | 0 |czone_state, weights=datad_cces$weight, data=datad_cces)
estr_noc_caseid<-felm(govapp ~ cz_b_hq_exo + cz_b_hq_in  | sy+caseid | 0 |czone_state, weights=datar_cces$weight, data=datar_cces)

estd_c <-felm(govapp ~ cz_b_hq_exo + cz_b_hq_in + birthyr_ + pid7_ + ba  + unempl + married + faminc_ | sy | 0 |czone_state, weights=datad_cces$weight, data=datad_cces)
estr_c <-felm(govapp ~ cz_b_hq_exo + cz_b_hq_in + birthyr_ + pid7_ + ba  + unempl + married + faminc_ | sy | 0 |czone_state, weights=datar_cces$weight, data=datar_cces)

estd_cid <-felm(govapp ~ cz_b_hq_exo + cz_b_hq_in + birthyr_ + pid7_ + ba  + unempl + married + faminc_ | sy+cid | 0 |czone_state, weights=datad_cces$weight, data=datad_cces)
estr_cid <-felm(govapp ~ cz_b_hq_exo + cz_b_hq_in + birthyr_ + pid7_ + ba  + unempl + married + faminc_ | sy+cid | 0 |czone_state, weights=datar_cces$weight, data=datar_cces)

estd_caseid <-felm(govapp ~ cz_b_hq_exo + cz_b_hq_in + birthyr_ + pid7_ + ba  + unempl + married + faminc_ | sy+caseid | 0 |czone_state, weights=datad_cces$weight, data=datad_cces)
estr_caseid <-felm(govapp ~ cz_b_hq_exo + cz_b_hq_in + birthyr_ + pid7_ + ba  + unempl + married + faminc_ | sy+caseid | 0 |czone_state, weights=datar_cces$weight, data=datar_cces)


stargazer(estd_noc_cid, estd_noc_caseid, estd_c, estd_cid, estd_caseid, estr_noc_cid, estr_noc_caseid,estr_c, estr_cid, estr_caseid,
          #dep.var.caption = "\\emph{Dependent Variable=Approval of the Governor’s Performance}",
          column.labels = c("Democratic Governor", "Republican Governor"),
          column.separate = c(5,5),
          #dep.var.labels = c("Under Democratic Governor", "Under Republican Governor"),
          #dep.var.labels.include = FALSE,
          #dep.var.labels = c("President", "Senate","House", "Federal Average"),
          keep = c("cz_b_hq_exo","cz_b_hq_in"),
          order = c("cz_b_hq_in","cz_b_hq_exo"),
          covariate.labels = c("Headquarters Inflow",
                               "Headquarters Outflow"
          ),
          float=F,
          add.lines = list(c("Controls","No","No","Yes","Yes","Yes","No","No","Yes","Yes","Yes"), c("County fixed effects","Yes","No","No","Yes","No","Yes","No","No","Yes","No"),c("Respondent fixed effects","No","Yes","No","No","Yes","No","Yes","No","No","Yes")),
          keep.stat = "n",
          star.cutoffs = c(0.1,0.05,0.01),
          star.char = c("†", "*", "**"),
          omit.table.layout = "n",
          column.sep.width = "5pt",
          notes = c("Standard errors clustered by commuting zone.", "State-year fixed effects and sampling weights are included throughout the models."), 
          notes.align = "l",
          out = "Tab_cces_panel_final.tex")


