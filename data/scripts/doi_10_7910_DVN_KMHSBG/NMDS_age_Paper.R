
#https://www.youtube.com/watch?v=QljEeBei-JA Vegan R Library Tutorial

library(dplyr)
library(vegan)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(shadowtext)
library(ggforce)

datos1 <- read.delim('datos_limpios.csv', sep = ';', header = TRUE)

# Edu
datos1$cl_edu <- factor(datos1$cl_education,
                       labels = c('Uneducated', 'Elementary School', 'Middle School',
                                  'High School', 'Vocational Education', 'University degree'))
# Status
datos1$cl_status_factor <- factor(datos1$cl_status,
                                 labels = c('UMI', 'MI', 'LMI',
                                            'SW', 'W'))
# Locality
datos1$cl_locality_factor <- factor(datos1$cl_locality,
                                   labels = c('- 2 K', '2 - 10 K', '10 - 50 K', '50 - 100 K',
                                              '100 - 400 K', '400 - 1,000 K', '1,000 K +'))

# Descriptives
aaa <- as.data.frame.matrix(table(datos1$cl_age_interval, datos1$cl_sex_factor))
bbb <- as.data.frame.matrix(table(datos1$cl_status_factor, datos1$cl_sex_factor))

## Attitude toward robots, age and sex
datos <- read.delim('age_sex_t_1.csv', sep = ';', header = TRUE, fileEncoding="latin1")

#colnames(datos)[4:8] <- c('Very negative', 'Negative', 'Neutral','Positive','Very positive')
colnames(datos)[4:8] <- c('VN', 'N', 'NE','P','VP')
row.names(datos) <- datos$cl_group

aa <- datos1 %>%
  group_by(cl_age_interval, cl_sex_factor) %>%
  summarise_each(list(mean))
a <- aa %>% 
  filter(., cl_sex_factor == 'Female')
b <- aa %>% 
  filter(., cl_sex_factor == 'Male')

datos.means <- bind_rows(a,b)

model <- select(datos, -starts_with('cl'))
model1 <- select(datos.means, starts_with('in'))

#ordination by NMDS
NMDS <- metaMDS(model, distance = "bray", k = 2)
## ggplot
nmds.plot <- data.frame(name = rownames(model), axis1 = NMDS$points[,1], axis2 = NMDS$points[,2])
nmds.species <- data.frame(name = colnames(model), axis1 = NMDS$species[,1], axis2 = NMDS$species[,2])

stressplot(NMDS)

##### Environmental variables
ef <- envfit(NMDS, model1[2:11])
ef

#Get the vectors for env.fit
df_envfit<-scores(ef,display=c("vectors"))
df_envfit<-df_envfit*vegan:::ordiArrowMul(df_envfit)
#size adjusted by R2
df_envfit<-as.data.frame(df_envfit)*ef$vectors$r
#####
# rownames(df_envfit) <- c('LCL', 'ADAP', 'UNC','JLOSS', 'WF2F',
#                          'EEG', 'ICC', 'IQL', 'IAPS',
#                          'SUST')
rownames(df_envfit) <- c('Loss of traditions', 'Difficulties to adapt', 'Unnecessary \nconsumption','Job losses', 'Worsens f2f \n communication',
                         'Essential for growth', 'Improves \ncompetitiveness', 'Improves \nquality of life', 'Improves access \n to products',
                         'Improves \nsustainability')
#add p-values
df_envfit <- cbind(df_envfit, data.frame(pvals = ef$vectors$pvals))
#Only significant, less th
#df_envfit <- df_envfit[-c(1,2), ]
df_envfit <- dplyr::filter(df_envfit, pvals < 0.03)




gof <- goodness(NMDS)
gof
# plot(NMDS, display = "sites", type = "n")
# points(NMDS, display = "sites", cex = 2*gof/mean(gof))

nmds.plot$grp<-rep(c("Female","Male"),each=6)
nmds.plot$gof <- 2*gof/mean(gof)
nmds.species$color <- c('n', 'n', 'ne', 'p', 'p')

## NEW Change Format Plot
#png(file="plot1.png", width=400, height=400)

ggplot() +
  geom_text_repel(data=as.data.frame(df_envfit*0.6),
                  mapping = aes(NMDS1 * 1.95, NMDS2 * 1.95, label = rownames(df_envfit)),  size=4, ## Environment
                  color="black", alpha = 0.4, inherit.aes = FALSE) +
  geom_text(nmds.plot, mapping = aes(axis1, axis2, label = name, color = grp),
            alpha = 0.9, inherit.aes = FALSE,  fontface = "bold", size = 4) + ## Sites
  theme(legend.title=element_blank(),legend.position = 'none') +
  geom_point(nmds.plot, mapping = aes(axis1, axis2, label = name, color  = grp, size=1*gof), ## Circle
             inherit.aes = FALSE, shape = 21, fill = NA, stroke = 1, alpha = 0.6) +
  scale_size_continuous(range = c(3, 12)) +
  geom_text(data = nmds.species, mapping = aes(axis1, axis2, label = name, color = color),
            color = c("firebrick3", "firebrick3", "gray48","springgreen3","springgreen3"),
            inherit.aes = FALSE,  alpha = 1,  size=6, fontface = "bold") + ## Species
  geom_segment(data=df_envfit, mapping = aes(x = 0, y = 0, xend = NMDS1*0.8, yend = NMDS2*0.8), ## Arrow
               inherit.aes = FALSE, arrow = arrow(length = unit(0.2, "cm")),color="#4C005C",
               alpha=0.9, lineend = 'round', linejoin = 'round', size = 0.7) +
  xlim(-0.22, 0.24) +
  ylim(-0.15, 0.15) +
  geom_hline(yintercept = 0, alpha = 0.2) +
  geom_vline(xintercept = 0, alpha = 0.2) +
  labs(title = "", x = '', y = '', color = NULL) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA)) +
  guides(x = "none", y = "none")


## End Saving
#dev.off()



#####################
#Bootstrapping and testing for differences between the groups, should be small, the groups are very different from each other
fit <- adonis(model ~ cl_age + cl_sex , data=datos, permutations=9999, method="bray")
fit



#####################
#Check assumption of homogeneity of multivariate dispersion P should be > 0.05 to have no problems with the assumption
distances_data <- vegdist(model)
anova(betadisper(distances_data, datos$cl_sex))
anova(betadisper(distances_data, datos$cl_age))
anova(betadisper(distances_data, datos$cl_group))
### Stress
NMDS[["stress"]]

