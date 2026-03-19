


################################
###         Figures         ###


# load data on activities
library(readxl)
library(reshape)
library(ggplot2)

##### Figure 1 #####

df.C24_SOI <- read_excel(file.choose()) # choose data set "organizations_and_activities"

# aggregate categories
df.C24_SOI$other2 <- with(df.C24_SOI, other+`statement of authorities`+
                            `mention of member of organization in other context`+
                            `deliberate mobilization`+festivities)
df.C24_SOI$technical <- with(df.C24_SOI, `composition of body / organizational questions` + 
                               `founding of organization`)
df.C24_SOI$statement <- with(df.C24_SOI, analysis + `utterance / publication` + `press conference`)

### reshape data for plotting

# new data frame with aggregated categories
df.act <- data.frame(cbind(df.C24_SOI$other2,
                           df.C24_SOI$technical,
                           df.C24_SOI$`roundtable / seminar / discussion`,
                           df.C24_SOI$statement,
                           df.C24_SOI$`cooperation with authorities / parliamentary activity`,
                           df.C24_SOI$`addressing officials`,
                           df.C24_SOI$`election monitoring`,
                           df.C24_SOI$protesting,
                           df.C24_SOI$org))
colnames(df.act) <- c("other", "technical", "debate etc", "statement",
                      "contact with auth.", "addressing auth.", "election monitoring",
                      "protest", "org")

# bring data into long format, name, and sort
df.act_melt <- melt(df.act, id = "org")
df.act_melt$percent <- ifelse(df.act_melt$org=="Council of 24 Dec",
                              as.numeric(as.character(df.act_melt$value))/232,
                              ifelse(df.act_melt$org=="SOI",
                                     as.numeric(as.character(df.act_melt$value))/83,NA))

df.act_melt$order <- rep(c(1,2),8)
df.act_melt$org  <- with(df.act_melt, reorder(org, -order))
levels(df.act_melt$org)[levels(df.act_melt$org)=="Council of 24 Dec"] <- "Council of 24 Dec\n(Perm)"
levels(df.act_melt$org)[levels(df.act_melt$org)=="SOI"] <- "SOI\n(Saratov)"

### plot

ggplot(df.act_melt) +
  geom_bar(aes(x=org, y=percent, fill=variable),
           stat = "identity") +
  xlab("Organization") +
  ylab("") +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey80")) +
  scale_fill_brewer(name="main reported\nactivities",
                    palette = "Set2",
                    guide = guide_legend(reverse = TRUE,
                                         ncol=2)) +
  coord_flip()
ggsave("Figure1.pdf", height = 3, width = 6, dpi = 1200)


##### Figure A1 ######

# load and prepare data
df.party <- read_excel(file.choose()) # choose file "Party_involvement"

# plot
ggplot(df.party, aes(x=city, y=percent, fill=factor(party)))+ # change data to party for abs. numbers
  geom_bar(stat = "identity")+
  xlab("City") +
  ylab("relative frequency") + # change to frequency for abs. numbers
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("grey80", "grey40"),
                    name="Party present",
                    breaks=c(0,1),
                    labels=c("no", "yes")) +
  theme_minimal()
ggsave("2011_party_involvement.pdf", height = 5, width = 6, dpi = 1200)


##### Figure A2 #####

data <- read_excel(file.choose()) # choose file "from folder"Case_selection"
data$cityshow <- ifelse(data$city=="Sverdlovsk" |
                          data$city=="Perm",1,
                        ifelse(data$city=="Saratov" |
                                 data$city=="Rostov",2,0))

# linear regression of the two indices (to aqcuire R^2)

m.1 <- lm(formula = econ ~ pol, data = data)
summary(m.1)

# plot

p.1 <- ggplot(data=data, aes(x=pol, y=econ)) +
  geom_rect(data=data, mapping=aes(xmin=mean(pol), xmax=max(pol)+0.01, 
                                   ymin=mean(econ), ymax=max(econ)+0.01), alpha=0.01)+
  geom_rect(data=data, mapping=aes(xmin=min(pol)-0.01, xmax=mean(pol), 
                                   ymin=min(econ)-0.01, ymax=mean(econ)), alpha=0.01)+
  annotate("text", x=0.785, y=0.68, size = 4.5,
           label = "most-likely cases") +
  annotate("text", x=0.43, y=0.384, size = 4.5,
           label = "least-likely cases") +
  geom_smooth(method = "lm", se = FALSE, size = 0.5)+
  geom_point() +
  geom_text(aes(label=ifelse(cityshow==1,as.character(city),'')),hjust=0.9,vjust=-1) +
  geom_text(aes(label=ifelse(cityshow==2,as.character(city),'')),hjust=0.4,vjust=-1) +
  annotate("text", x=0.74, y=0.12, size = 5,
           label = "R^2 = 0.32") +
  xlab("political index") +
  ylab("economic index") +
  theme_minimal(); p.1
ggsave("case-selection.pdf",width = 6, height = 4.5,  dpi = 600)


##### Figure A3 #####

vdem <- data.frame(readRDS(file.choose())) # load v-dem data

# demonstrate that Russia's value has not changed in the given period
vdem1 <- vdem[vdem$year>=2006 & vdem$year<=2010,]
table(vdem1$v2elsnlsff[vdem1$country_name=="Russia"],
      vdem1$year[vdem1$country_name=="Russia"])
table(vdem1$v2elsnlsff_osp[vdem1$country_name=="Russia"],
      vdem1$year[vdem1$country_name=="Russia"])
table(vdem1$v2elsnlsff_ord[vdem1$country_name=="Russia"],
      vdem1$year[vdem1$country_name=="Russia"])

# plot VDEM's Linearized Original Scale Posterior Prediction
p.kernel <- ggplot(vdem[vdem$v2x_regime==1 & vdem$year>=1991,], aes(x=v2elsnlsff_osp)) +
  geom_density(adjust = 0.5) +
  xlab("Subnational election unevenness in electoral autocracies (OSP), 1991-2019") +
  geom_vline(xintercept = 0.335, color = "red") +
  theme_minimal(); p.kernel
ggsave("Subnational_el_unevenness_VDEM-OSP.pdf", width = 6, height = 3, dpi = 1200)


## estimate area under the curve for Russia's value on v2elsnlsff_osp
# Create empirical cumulative distribution function from sample data
d_fun <- ecdf(vdem$v2elsnlsff_osp[vdem$v2x_regime==1 & vdem$year>=1991])

#Assume a value for the "red vertical line"
x0 <- 0.335

#Area under curve greater than x0
1 - d_fun(x0)

