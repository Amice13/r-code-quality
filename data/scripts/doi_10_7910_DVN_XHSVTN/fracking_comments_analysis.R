#clear variables
rm(list=ls(all=TRUE))
gc()

library(tidyverse)
library(broom)
library(geosphere)
library(factoextra)
library(rgdal)
library(rgeos)
library(stringr)
library(gridExtra)
library(ggpubr)
library(DescTools)
library(forcats)


## read comment data 
k = 30
k_folder_path <-  file.path("data", paste0("mallet_lda_",k))


## topic proportions and covariates
comment_data <- read.csv(file = file.path(k_folder_path, "comment_topics&covars.csv"),
                        header = TRUE, sep = ",",  stringsAsFactors = FALSE)

### topic labels and other topic-related information
topic_data <- read.table(file.path(k_folder_path,"topic_labels.txt"), 
                         header = TRUE, sep="\t", stringsAsFactors = FALSE)

# calculate average topic proportion and re-order factors from low to high
topic_data$topic.prop <- apply(comment_data[, 2:(k+1)], 2, mean)
topic_data <- within(topic_data, numbered.label <- factor(numbered.label, levels=numbered.label[order(topic.prop, decreasing=FALSE)], ordered=TRUE))
topic_data$primary.cat = factor(topic_data$primary.cat,levels=c("enviro","health", "socio-econ","political", "0"))
print(levels(topic_data$primary.cat))

ordered_top5 <- colnames(comment_data[,2:(k+1)])
topic_data$primary_cat_colors <- ifelse(topic_data$primary.cat == "enviro", "#19A979",
                                        ifelse(topic_data$primary.cat == "health", "#525DF4",
                                               ifelse(topic_data$primary.cat == "political", "#6C8893",
                                                      ifelse(topic_data$primary.cat == "socio-econ", "#EE6868", NA
                                                      ))))
# calculate total by primary category
comment_data$total.enviro <- apply(comment_data[,which(topic_data$primary.cat == 'enviro')+1], 1, sum)
comment_data$total.health <- apply(comment_data[,which(topic_data$primary.cat == 'health')+1,drop=F], 1, sum)
comment_data$total.political <- apply(comment_data[,which(topic_data$primary.cat == 'political')+1], 1, sum)
comment_data$total.socio.econ <- apply(comment_data[,which(topic_data$primary.cat == 'socio-econ')+1], 1, sum)
comment_data$total.other <- apply(comment_data[,which(topic_data$primary.cat == '0')+1], 1, sum)


#########################################################
## 1. Overall topic distribution (excluding throw-aways)
## Figure 2 and Supplementary Figure 23
#########################################################

#proportions of all topics (except throwaway)
si_fig23 <- ggplot(topic_data[topic_data$throwaway ==0,], aes(x = numbered.label, y =topic.prop)) +
  geom_bar(stat="identity", width = .5, aes(fill=primary.cat)) +
  scale_fill_manual(values= c("#19A979", "#525DF4","#EE6868", "#6C8893", "black"), labels = c("Environmental", "Health", "Social/economic", "Political/institutional", "Not related to\nimpact/concern")) +
  ylab("Average topic proportion") +
  theme_classic() +
  theme( axis.text.x=element_text(size=7, colour = "black"),
         axis.text.y=element_text(size=7, colour = "black"),
         axis.title.y=element_blank(),
         axis.title.x=element_text(size=7, colour = "black"),
         legend.title = element_blank(),
         legend.text = element_text(size =6, colour = "black"),
         legend.key.size = unit(3, "mm"),
         legend.spacing.y = unit(1, 'mm'),
         legend.position = c(.65,.15)
  ) +
  coord_flip()
print(si_fig23)
ggsave("si_fig23.pdf",
       si_fig23, width=88, height=90, units="mm", dpi = 500)

#proportions of concern topics
fig2 <- ggplot(topic_data[which(topic_data$concern ==1),], aes(x = numbered.label, y =topic.prop)) +
  geom_bar(stat="identity", width = .5, aes(fill=primary.cat)) +
  scale_fill_manual(values= c("#19A979", "#525DF4", "#EE6868","#6C8893"), labels = c("Environmental", "Health", "Social/Economic", "Political/institutional")) +
  ylab("Average topic proportion") +
  theme_classic() +
  theme( axis.text.x=element_text(size=7, colour = "black"),
         axis.text.y=element_text(size=7, colour = "black"),
         axis.title.y=element_blank(),
         axis.title.x=element_text(size=7, colour = "black"),
         legend.title = element_blank(),
         legend.text = element_text(size =6, colour = "black"),
         legend.key.size = unit(3, "mm"),
         legend.spacing.y = unit(1, 'mm'),
         legend.position = c(.65,.15)
  ) +
  coord_flip()
print(fig2)
ggsave("fig2.pdf",
       fig2, width=88, height=90, units="mm", dpi = 500)


#########################################################
## 2. Relationship between distance to proposed well and topic proportion
## (Supplementary Figure 20)
#########################################################
complete_ny_data <- comment_data[which(!is.na(comment_data$long) & comment_data$state == 'ny'),]
rownames(complete_ny_data) <- complete_ny_data$comment_id

topic_data <- topic_data[order(topic_data$topic),]
for ( i in c( 1:k ) ) { #var in colnames(data)[6:32]) {
  
  fit <- lm(paste0( topic_data$top5[i], " ~",  "well_dist"), data = complete_ny_data)
  topic_data$slope[i] <- summary(fit)$coefficients[2]
  
}
topic_data <- topic_data[order(topic_data$slope),] #order by slope from smallest to largest

plot.list <- list()
list.index = 1
scaleFUN <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) }
for (i in topic_data$topic[which(topic_data$concern ==1)] ) { # Loop over loop.vector
  
  fit <- lm(paste0(topic_data$top5[which(topic_data$topic ==i)], " ~",  "well_dist"), data = complete_ny_data)
  
  topic.plot <- ggplot( complete_ny_data, aes_string( x="well_dist", y= topic_data$top5[topic_data$topic ==i] ) ) + 
    stat_smooth(method = "lm", formula = y ~ x, color = 'deepskyblue4', na.rm = TRUE, se = TRUE) + 
    ggtitle(topic_data$short_label[which(topic_data$topic == i)]) +
    xlab("Miles to nearest HVHF well") +
    coord_cartesian(ylim=c(0, .085)) + #sets y limits without throwing away data outside limits
    scale_y_continuous( labels = scaleFUN ) +
    scale_x_continuous( limits = c(0, 110), breaks = seq(0,100, 50), labels = c("0 mi", "50 mi", "100 mi") ) +
    theme_classic() +
    theme( axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank(),
           axis.ticks.x =element_blank(),
           axis.ticks.y =element_blank(),
           plot.title=element_text(size=14, colour = "black")
    )
  if(coef(summary(fit))["well_dist", "Pr(>|t|)"] < .01) {
    topic.plot <- topic.plot + 
      ggplot2::annotate("text", x = 50, y = .06, label = "*", colour = "black", size = 10)  
    
  }
  
  if(list.index %in% c( 17:21) ) {
    topic.plot <- topic.plot + 
      theme( axis.text.x=element_text(size=14, colour = "black")
      )
    
  }
  
  if(list.index %in% c( 1,6, 11, 16,21) ) {
    topic.plot <- topic.plot + 
      theme( axis.text.y=element_text(size=14, colour = "black")
      )
    
  }
  plot.list <- c(plot.list, list(topic.plot))
  list.index <- list.index +1
}

si_fig20 <-grid.arrange(grobs = plot.list, ncol = 5)
print(si_fig20)
ggsave("si_fig20.pdf",
       si_fig20, width=42, height=30, units="cm", dpi = 500)


#########################################################
## 4. Differences in topic proportions by Marcellus Status
## (Figure 3)
#########################################################


# function to get t-test result
get.t.result <- function(a.vector, marcellus.yes, marcellus.no ) {
  marcellus.vector <- a.vector[marcellus.yes]
  non.marcellus.vector <-a.vector[marcellus.no]
  test <- t.test(marcellus.vector, non.marcellus.vector)
  test[1]
}

# t < 3.291 is p < .001
t.test.results.topics <- apply(complete_ny_data[, c(ordered_top5)],
                        2, FUN=get.t.result, marcellus.yes = which(complete_ny_data$marcellus ==1), marcellus.no = which(complete_ny_data$marcellus ==0))
topic_data <- topic_data[order(topic_data$topic),]

topics_by_marcellus <- data.frame()
for ( i in 1:k ) {
  topics_by_marcellus[i, 1] <- topic_data$label[i]
  topics_by_marcellus[i, 2] <- topic_data$concern[i]
  topics_by_marcellus[i, 3] <- mean(complete_ny_data[,(i+1)], na.rm = TRUE)
  topics_by_marcellus[i, 4] <- mean(complete_ny_data[complete_ny_data$marcellus==1,(i+1)], na.rm = TRUE) 
  topics_by_marcellus[i, 5] <- mean(complete_ny_data[complete_ny_data$marcellus==0,(i+1)], na.rm = TRUE)
  topics_by_marcellus[i, 6] <- topic_data$primary.cat[i]
  topics_by_marcellus[i,7] <- ifelse(t.test.results.topics[[i]]$statistic >3.291, 1,ifelse(t.test.results.topics[[i]]$statistic < -3.2911, -1, 0) )
  
}

colnames(topics_by_marcellus) <- c("topic", "concern", "overall.rate", "marcellus.rate", "non.marcellus.rate", 'primary.category', "stat.sig")

# filter out non-concern topics and re-order levels of primary.category (for plot)
topics_by_marcellus <- topics_by_marcellus %>%
  filter(concern ==1)
topics_by_marcellus$primary.category <- factor(topics_by_marcellus$primary.category, levels=c("enviro","health", "socio-econ","political"))

##create a similar category-level dataframe (to be merged to use in slope graphs below)
t.test.results.cats <- apply(complete_ny_data[, c("total.enviro", "total.health", "total.political", "total.socio.econ")],
                               2, FUN=get.t.result, marcellus.yes = which(complete_ny_data$marcellus ==1), marcellus.no = which(complete_ny_data$marcellus ==0))
cats_by_marcellus <- data.frame() #create dataframe with same columns as topics, but for categories
for (i in 1:length( levels(topics_by_marcellus$primary.category) ) ) {
  cats_by_marcellus[i, 1] <- paste0(levels(topics_by_marcellus$primary.category)[i], ".overall")
  cats_by_marcellus[i, 2] <- sum(topics_by_marcellus$overall.rate[which(topics_by_marcellus$primary.category == levels(topics_by_marcellus$primary.category)[i])], na.rm = TRUE)
  cats_by_marcellus[i, 3] <- sum(topics_by_marcellus$marcellus.rate[which(topics_by_marcellus$primary.category == levels(topics_by_marcellus$primary.category)[i])], na.rm = TRUE)
  cats_by_marcellus[i, 4] <- sum(topics_by_marcellus$non.marcellus.rate[which(topics_by_marcellus$primary.category == levels(topics_by_marcellus$primary.category)[i])], na.rm = TRUE)
  cats_by_marcellus[i, 5] <- levels(topics_by_marcellus$primary.category)[i]
  cats_by_marcellus[i,6] <- ifelse(t.test.results.cats[[i]]$statistic >3.291, 1,ifelse(t.test.results.cats[[i]]$statistic < -3.2911, -1, 0) )
}
colnames(cats_by_marcellus) <- c("topic", "overall.rate", "marcellus.rate", "non.marcellus.rate", 'primary.category', "stat.sig")

## calculate diffs
topics_by_marcellus$diff <- round(topics_by_marcellus$marcellus.rate- topics_by_marcellus$non.marcellus.rate, 3)
topics_by_marcellus$diff.type <- ifelse(topics_by_marcellus$diff > 0.000000, "More common on Marcellus", "More common off Marcellus")
topics_by_marcellus$diff.std <- ifelse(topics_by_marcellus$marcellus.rate > topics_by_marcellus$non.marcellus.rate, round( (topics_by_marcellus$marcellus.rate-topics_by_marcellus$overall.rate)/topics_by_marcellus$overall.rate, 3), -round( (topics_by_marcellus$non.marcellus.rate-topics_by_marcellus$overall.rate)/topics_by_marcellus$overall.rate, 3) )

#topics_by_marcellus <- topics_by_marcellus[order(topics_by_marcellus$diff, decreasing = FALSE),]
#topics_by_marcellus$`topic` <- factor(topics_by_marcellus$topic, levels = topics_by_marcellus$`topic`)

cats_by_marcellus$diff <- cats_by_marcellus$marcellus.rate - cats_by_marcellus$non.marcellus.rate

### ### ### ### ### ### ### ### ### ### ### ### ### 
## Differences between on Marcellus and off
### ### ### ### ### ### ### ### ### ### ### ### ### 
topics_by_marcellus <- topics_by_marcellus[order(topics_by_marcellus$primary.category, topics_by_marcellus$diff, decreasing = FALSE),]
topics_by_marcellus$`topic` <- factor(topics_by_marcellus$topic, levels = topics_by_marcellus$`topic`)
levels(topics_by_marcellus$topic)

#topic-level
topic_diffs <- ggplot(topics_by_marcellus, aes(x = topic, y= diff, label = diff)) +
  geom_bar(stat="identity", aes(fill=primary.category), width = .75) +
  scale_fill_manual(values= c("#19A979", "#525DF4","#EE6868","#6C8893"), labels = c("Environmental", "Health", "Social/Economic", "Political/institutional")) +
  geom_vline( xintercept=10.5, linetype = "dashed",  show.legend = FALSE ) + 
  geom_vline( xintercept=12.5, linetype = "dashed", show.legend = FALSE ) +
  geom_vline( xintercept=17.5, linetype = "dashed" ,show.legend = FALSE ) +
  scale_y_continuous(position = "right", limits = c(-.032, .032), sec.axis = dup_axis(name = "Difference in topic proportions")) +
  geom_text(aes(y = diff + .001 * sign(stat.sig), label = ifelse(stat.sig != 0,"*", "") ), 
            size = 5, nudge_x = -.22 ) +
  geom_hline(yintercept = 0.0, colour = "black", size = 1, linetype = "solid") +
  ggplot2::annotate(geom = "text",x=19.5, y=.025, label="Political/institutional", colour = "#6C8893", size = 3) +
  ggplot2::annotate(geom = "text",x=15.5, y=.026, label="Social/economic", colour = "#EE6868", size = 3) +
  ggplot2::annotate(geom = "text",x=11.5, y=.025, label="Health", colour = "#525DF4", size = 3) +
  ggplot2::annotate(geom = "text",x=5.5, y=.025, label="Environmental", colour = "#19A979", size = 3) +
  theme_classic() +
  theme( axis.text.y=element_text(size=7, colour = "black"),
         axis.text.x.top=element_blank(),
         axis.title.y=element_blank(),
         axis.title.x.top= element_blank(),
         axis.title.x.bottom =element_text(size=7, colour = "black"),
         axis.title.y.right = element_blank(),
         axis.text.x.bottom = element_text(size=7, colour = "black"),
         legend.position = "none", 
         panel.border = element_rect(fill = NA)
         
  ) +
  coord_flip()
print(topic_diffs)

#category-level
category_diffs <- ggplot(cats_by_marcellus[which(cats_by_marcellus$topic != "0.overall"),], aes(x = topic, y= diff, label = diff)) +
  geom_bar(stat="identity", aes(fill=topic), width = .75) +
  scale_fill_manual(values= c("#19A979", "#525DF4", "#EE6868","#6C8893")) +
  ylab("More prevalent OFF Marcellus                                    More prevalent ON Marcellus") +
  scale_y_continuous(position = "right", limits = c(-.0585, .0585), sec.axis = dup_axis(name = "Difference in average topic proportions")) +
  scale_x_discrete(labels= c("Environmental", "Health", "Social/Economic", "Political/institutional")) +
  theme_classic() +
  geom_hline(yintercept = 0.0, colour = "black", size = 1, linetype = "solid") +
  geom_text(aes(y = diff + .0015 * sign(stat.sig), label = ifelse(stat.sig != 0,"*", "") ), 
            size = 5, nudge_x = -.25 ) +
  theme( axis.text.y=element_text(size=7, colour = "black"),
         axis.text.x.top=element_blank(),
         axis.title.y=element_blank(),
         axis.title.x.top= element_text(size=7, colour = "black"),
         axis.title.x.bottom =element_blank(),
         axis.text.x.bottom = element_text(size=7, colour = "black"),
         legend.position = "none",
         panel.border = element_rect(fill = NA)
         
  ) +
  coord_flip()
print(category_diffs)

#combined plots
fig3 <- ggarrange(category_diffs, topic_diffs, 
                                      labels = c("A.", "B."), heights = c(1, 4), align = "v",
                                      ncol = 1, nrow = 2)
print(fig3)
ggsave("fig3.pdf",
       fig3, width=180, height=150, units="mm", dpi = 500)

#########################################################
## 5. Discourse diversity analysis
## (Figure 4)
#########################################################

### Define functions for bootstrapping Entropy
resampled.Entrop <- function(resample.ids, data) {
  resample.ave <- apply(data[resample.ids, ], 2, FUN=mean)
  resample.E <- Entropy( resample.ave )
  resample.E
}

boot.Entrop <- function(data, num, sample.size) {
  resamples.ids <- lapply(1:num, function(i) sample(nrow(data), size = sample.size, replace = T) )
  r.Entropies <- sapply(resamples.ids, function(i) resampled.Entrop(i, data) )
  std.Entrop <- sqrt(var(r.Entropies))
  list(std.Herf = std.Entrop, r.Entropies = r.Entropies, ave.Entropies = mean(r.Entropies))
}

counties_over30 <- complete_ny_data %>%
  group_by(county_fips) %>%
  filter( n() >30) %>%
  ungroup()
  
by.cnty.E <- lapply(split(counties_over30, counties_over30$county_fips), function(x) boot.Entrop(x[, 2:(k+1)], 1000, 30))

county_comments <- complete_ny_data %>%
  group_by(county_fips, county_name) %>%
  dplyr::summarize(n = n(),
                   cnty.marcellus.share = sum(marcellus==1)/n(),
                   cnty.ave.dist = mean(well_dist))
county_comments$county_fips <- fct_explicit_na(county_comments$county_fips, na_level = "(Missing)")

cnty.measures <- data.frame()
for ( i in 1:length(by.cnty.E) ) {
  cnty.measures[i, 1] <- names(by.cnty.E)[i]
  cnty.measures[i, 2] <- median(by.cnty.E[[i]]$r.Entropies, na.rm = T)
  cnty.measures[i, 3] <- quantile(by.cnty.E[[i]]$r.Entropies, .975, na.rm = T)
  cnty.measures[i, 4] <- quantile(by.cnty.E[[i]]$r.Entropies, .025, na.rm = T)
  cnty.measures[i, 5] <- county_comments[ county_comments$county_fips == names(by.cnty.E)[i], 'county_name' ]
  cnty.measures[i, 6] <- county_comments[ county_comments$county_fips == names(by.cnty.E)[i], "cnty.marcellus.share"]
  cnty.measures[i, 7] <- county_comments[ county_comments$county_fips == names(by.cnty.E)[i],'n']
  cnty.measures[i, 8] <- county_comments[ county_comments$county_fips == names(by.cnty.E)[i], "cnty.ave.dist"]
}
colnames(cnty.measures) <- c("cnty.fips", "E.estimate", "E.upper", "E.lower", 'cnty.name', "cnty.marcellus.share", "n.comments", "ave.well_dist")
cnty.measures$marcellus.any <- as.factor( ifelse(cnty.measures$cnty.marcellus.share > 0, 1, 0) )

fig4 <- ggplot(cnty.measures[which(!is.na(cnty.measures$E.estimate)),], aes(x=ave.well_dist, y=E.estimate)) +
  geom_point(aes(size = n.comments, fill = marcellus.any), colour = "grey45", pch = 21) +
  geom_text(aes(label=ifelse(cnty.name == "Tompkins" | cnty.name == "Westchester", cnty.name, "") ), nudge_y = .015, size = 2.5 )  +
  geom_smooth(method=lm, color = "black") +
  scale_x_continuous(limits = c(0, 132)) +
  ylab( "Discourse diversity (Shannon Entropy)") +
  xlab( "Average distance to the nearest proposed HVHF well") +
  scale_fill_manual(values = c("white","gray70"), name = "", labels = c("Not on Marcellus","On Marcellus")) +
  scale_size_continuous(name= "# of comments", breaks = c(30, 100, 500), range = c(1,10), limits = c(25,1000)) +
  theme_classic() +
  theme( axis.text.x=element_text(size=7, colour = "black"),
         axis.text.y=element_text(size=7, colour = "black"),
         axis.title.y=element_text(size=7, colour = "black"),
         axis.title.x=element_text(size=7, colour = "black"),
         
         legend.title = element_text(size =6, colour = "black"),
         legend.text = element_text(size =6, colour = "black"),
         legend.key.size = unit(3, "mm"),
         legend.spacing.y = unit(1, 'mm'),
         
         #legend.spacing.x = unit(1, 'mm'),
         legend.position = "bottom",
         legend.box = "horizontal",
         legend.box.just = "top"
  )  +
  guides(fill = guide_legend(override.aes = list(size=5), nrow = 2),
         size = guide_legend(nrow = 1)
  )
print(fig4)
ggsave("fig4.pdf",
       fig4, width=88, height=88, units="mm", dpi = 500)


#########################################################
## 6. PLACENESS ANALYSIS
## (Figure 5)
#########################################################

############# PLACENESS ANALYSIS

####Functions for boostrapping weighted standard distance by topic

##(1) Function for calculating weighted standard distance from a sample of comments
resampled_w_std_dist <- function( resample.ids, coord_data_weights) {
  coord_sample <- coord_data_weights[resample.ids,]
  mean_center <- apply(coord_sample[,1:2], 2, weighted.mean, w = coord_sample[,3])
  std_dist <- sqrt(
    sum(
      #squared horizontal deviation in KMs
      coord_sample[,3]*(
        distm( 
          cbind(coord_sample[,1], rep(mean_center[2], length(coord_sample[,1]))),
          mean_center
        )
      )^2 +
        #squared vertical deviation in KMs
        coord_sample[,3]*(
          distm( 
            cbind(rep(mean_center[1], length(coord_sample[,2])), coord_sample[,2] ),
            mean_center 
          )
        )^2
      
    ) / sum(coord_sample[,3])
  )
  std_dist/1000 #express in KMs, instead of meters
}

## unweighted (0/1) version
resampled_std_dist <- function( resample.ids, coord_data) {
  sample_size <- length(resample.ids)
  coord_sample <- coord_data[resample.ids,]
  mean_center <- apply(coord_sample, 2, mean)
  std_dist <- sqrt(
    sum(
      #squared horizontal deviation in KMs
      distm(
        cbind(coord_sample[,1], rep(mean_center[2], length(coord_sample[,1]))),
        mean_center
        )^2 +
        #squared vertical deviation in KMs
        distm( 
            cbind(rep(mean_center[1], length(coord_sample[,2])), coord_sample[,2] ),
            mean_center
            )^2
      
    ) / sample_size
  )
  std_dist/1000 #express in KMs, instead of meters
}

##(2) Function for executing the bootstrap of the weighted standard distance
boot_w_std_dist <- function(coord_data, num, sample_size) {
  resamples_ids <- lapply(1:num, function(i) sample(nrow(coord_data), size = sample_size, replace = T) )
  ave_dists <- sapply(resamples_ids, function(i) resampled_w_std_dist(i, coord_data) )
  std_aveDist <- sqrt(var(ave_dists))
  list(ave_dists = ave_dists, mean_ave_dists = mean(ave_dists) )
}

## Commenting out the bootstrap, which is computationally expensive. Can load
## a pre-computed dataframe below.

# by.topic.aveDist.original <- list()
# for (j in 1:k ) {
#   message(paste0("started topic: " , j, " of ", k ))
#   
#   by.topic.aveDist.original[[j]] = list()
#   topic_coord_data_weights <- complete_ny_data[c('long', 'lat', ordered_top5[j])]
 #   
 #   ## calculate the standard distance for complete dataset
 #   std_dist_estmate <- resampled_w_std_dist(
 #     seq(1:nrow(topic_coord_data_weights)), topic_coord_data_weights
 #   )
 #   by.topic.aveDist.original[[j]]$estimate <- std_dist_estmate
 #   
 #   ## calculate weighted mean center for complete dataset
 #   mean_center <- apply(topic_coord_data_weights[,1:2], 2, weighted.mean, w = topic_coord_data_weights[,3])
 #   by.topic.aveDist.original[[j]]$mean_center <- mean_center
 #   
 #   ## run bootstrap
 #   topic_mean_dist <- boot_w_std_dist(topic_coord_data_weights, 1000, nrow(complete_ny_data))
 #   by.topic.aveDist.original[[j]]$bootstrap <- topic_mean_dist
 #   message(paste0("finished topic: " , j, " of ", k,"\n" ))
 # }
 # 
 # topic_distances <- data.frame()
 # for ( i in 1:length(by.topic.aveDist.original) ) {
 #   topic_distances[i, 1] <- topic_data$short_label[i]
 #   topic_distances[i, 2] <- median(by.topic.aveDist.original[[i]]$bootstrap$ave_dists, na.rm = T)
 #   topic_distances[i, 3] <- quantile(by.topic.aveDist.original[[i]]$bootstrap$ave_dists, .975, na.rm = T)
 #   topic_distances[i, 4] <- quantile(by.topic.aveDist.original[[i]]$bootstrap$ave_dists, .025, na.rm = T)
 #   topic_distances[i, 5] <- by.topic.aveDist.original[[i]]$estimate
 #   topic_distances[i, 6] <- by.topic.aveDist.original[[i]]$mean_center[1]
 #   topic_distances[i, 7] <- by.topic.aveDist.original[[i]]$mean_center[2]
 #   topic_distances[i, 8] <- topic_data$primary.cat[i]
 #   
 # }
 # colnames(topic_distances) <- c("topic_label", "aveDist_median", "aveDist_upper", "aveDist_lower", "overall_estimate", "mean_center_long", "mean_center_lat", "primary_cat")

topic_distances_path <- file.path("data", paste0("topic_distances_",k,".csv"))
#  write.csv(topic_distances, topic_distances_path)

## Load the pre-computed dataframe of topic standard distances and bootstrapped CIs
topic_distances <- read.csv(topic_distances_path, header = TRUE, sep = ",",  stringsAsFactors = FALSE,
                            colClasses=c('character', rep('numeric', 6), "character"))

topic_distances$topic_label <- as.factor(topic_distances$topic_label)

topic_distances <- topic_distances[order(topic_distances$overall_estimate),]
topic_distances$topic_label <- as.factor(topic_distances$topic_label)
topic_distances$topic_label <- factor( topic_distances$topic_label, levels = topic_distances$topic_label)


# selected_topics determines which topics to inclulde in calculation of overall_estimate
## including all topics
selected_topics <- seq(1,k)
overall_weights <- rowSums(complete_ny_data[, seq(2,k+1)][,selected_topics])
coord_data_weights_overall <- cbind(complete_ny_data[c('long', 'lat')], overall_weights)

std_dist_estmate_overall <- resampled_w_std_dist(
  seq(1:nrow(coord_data_weights_overall)), coord_data_weights_overall
)

#Plot standard distance by concern
fig5 <- ggplot(topic_distances[which(topic_distances$primary_cat != "0"),], aes(x=topic_label, y=overall_estimate*0.621371)) + #converting kms to miles
  geom_hline (yintercept = std_dist_estmate_overall*0.621371, linetype = "solid", colour = "grey30") +
  geom_errorbar(aes(ymin=aveDist_lower*0.621371, ymax=aveDist_upper*0.621371, color= factor(primary_cat)), width=.25) +
  geom_point(aes(fill= factor(primary_cat)), size = 2, shape = 21) +
  scale_fill_manual(values= c("#19A979", "#525DF4", "#6C8893", "#EE6868"), labels = c("Environmental", "Health", "Political/institutional","Social/Economic")) +
  scale_color_manual(values= c("#19A979", "#525DF4", "#6C8893", "#EE6868"), labels = c("Environmental", "Health", "Political/institutional","Social/Economic")) +
  ylab( "Weighted standard distance (in miles)") +
  theme_classic() +
  theme( axis.text.x=element_text(size=7, colour = "black"),
         axis.text.y=element_text(size=7, colour = "black"),
         axis.title.y=element_blank(),
         axis.title.x=element_text(size=7, colour = "black"),
         legend.title = element_blank(),
         legend.text = element_text(size =6, colour = "black"),
         legend.key.size = unit(3, "mm"),
         legend.spacing.x = unit(0, 'mm'),
         legend.spacing.y = unit(0, 'mm'),
         legend.box.margin = margin(-4, -4, -4, -4),
         legend.position = c(.30,.90),
         legend.box = "vertical",
         legend.box.just = "top",
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         panel.border = element_rect(fill = NA)
  ) +
  coord_flip()
print(fig5)
ggsave("fig5.pdf", fig5, 
       width = 88, height = 88, units = "mm", dpi = 500)

#########################################################
## 7. Create maps
## Figure 6 (top concerns by county) and Figure 1 (distribution of comments)
#########################################################
concerns <- topic_data$top5[which(topic_data$concern == 1)]
names(concerns) <- concerns #to automatically name list items


topic_means <- complete_ny_data %>%
  dplyr::summarize(
    across(all_of(concerns), list(mean), .names = "{.col}")
  ) %>%
  pivot_longer(cols = all_of(concerns), names_to = "top5", values_to = "topic_mean")

concern_data <- topic_data %>%
  filter(concern == 1) %>%
  select(topic, short_label, primary.cat, top5, primary_cat_colors) %>%
  left_join(topic_means, by= "top5") %>%
  left_join(topic_distances, by = c("short_label" = "topic_label")) %>%
  mutate(
    std_dist_color = ifelse(
      overall_estimate > std_dist_estmate_overall, "yellow", primary_cat_colors), #Placeless concerns are yellow
    mean_prop_among_concerns = topic_mean/ sum(topic_mean)
  )

placeless_concerns <- concern_data$top5[which(concern_data$overall_estimate > std_dist_estmate_overall)]

#function for finding n'th largest 
which_nth_highest <- function(x, n)
{
  for(i in seq_len(n - 1L)) x[x == max(x)] <- -Inf
  which(x == max(x))
}

min_comments <- 30
## create county summaries
county_stats <- complete_ny_data %>%
  group_by(county_fips) %>%
  dplyr::summarize(n_comments = n(),
            across(all_of(concerns), list(mean), .names = "{.col}"),
            pop_2010 = first(pop_2010)) %>%
  rowwise() %>%
  dplyr::mutate(
    comments_per10K = n_comments/pop_2010*10000,
    first = names(all_of(concerns))[which_nth_highest(rank(c_across(all_of(concerns))),1)],
    second = names(all_of(concerns))[which_nth_highest(rank(c_across(all_of(concerns))),2)],
    third = names(all_of(concerns))[which_nth_highest(rank(c_across(all_of(concerns))),3)][1], #selects random one, if there's a tie for third
    
    first_short_all = ifelse(n_comments < min_comments, "Too few comments",
                             topic_data$short_label[which(topic_data$top5 == first)]
    ),
    
    first_short_emplaced = ifelse(n_comments < min_comments, "Too few comments",
                                  ifelse(first %in% placeless_concerns,
                                  "Placeless",
                                  topic_data$short_label[which(topic_data$top5 == first)])
    ),
    
    placeless_sum = sum(c_across(all_of(placeless_concerns))),
    placeless_prop_among_concerns = sum(c_across(all_of(placeless_concerns)))/sum(c_across(all_of(concerns))),
    
    ave_stdDist = sum((concern_data$overall_estimate)*c_across(all_of(concerns))/sum(c_across(all_of(concerns))))
  ) 

#most frequent top topics
county_stats %>%
  group_by(first_short_all, first_short_emplaced) %>%
  dplyr::summarize(count = n()) %>%
  arrange(desc(count))

## read the counties shapefile
counties_shapefile_path <- "data/new_york_counties_geography/"
counties_geo <- readOGR(
  dsn = counties_shapefile_path,
  layer =  "new_york_counties",
  verbose = FALSE
)

#find county centroids for placing dots
county_stats$county_fips <- as.character(county_stats$county_fips) #to facilitate join
county_centroids <- as.data.frame(SpatialPointsDataFrame(gCentroid(counties_geo, byid=TRUE), 
                                           counties_geo@data, match.ID=FALSE)) %>%
  left_join(county_stats, by= c("GEOID" = "county_fips")) %>%
  left_join(topic_data[,c("top5", "short_label")], by = c("first" = "top5"))


tidy_counties_geo <- tidy(counties_geo, region = "GEOID")

tidy_counties_geo <- tidy_counties_geo %>%
  left_join(county_stats, by= c("id" = "county_fips"))

## read the marcellus shale boundary shapefile
marcellus_shapefile <- "data/shale_geography/"
marcellus_geo <- readOGR(
  dsn = marcellus_shapefile,
  layer =  "devonian",
  verbose = FALSE
)
marcellus_geo <- tidy(marcellus_geo, region = "prov_name")

#Read the well locations and identify coordinates of hvhf wells
hvhf_wells_file <- "data/wellspublic.csv"
hvhf_wells <- read.csv(hvhf_wells_file,
         header = TRUE, sep = ",",  stringsAsFactors = FALSE)

hvhf_wells <- hvhf_wells %>%
  filter(Slant == "Horizontal", Objective_formation %in% c("Marcellus", "Utica"))

hvhf_coords <- hvhf_wells %>%
  select(Surface_Longitude, Surface_latitude) %>%
  dplyr::rename(long = Surface_Longitude,
         lat = Surface_latitude)
hvhf_wells <- hvhf_wells %>%
  select(-c(Surface_Longitude, Surface_latitude))

hvhf_wells <- as.data.frame(
  SpatialPointsDataFrame(coords = hvhf_coords, data = hvhf_wells))

## Map with labeled top topic for each county (where topic is not placeless)
top_topic_short_labels <- levels(as.factor(tidy_counties_geo$first_short_emplaced[which(!(tidy_counties_geo$first_short_emplaced %in% c("Placeless", "Too few comments")))])) #add "placeless" and "too few" at the end and manually color with grey25
top_topic_short_colors <- RColorBrewer::brewer.pal(length(top_topic_short_labels), "Set2")
top_topic_short_labels <- c(top_topic_short_labels, c("Placeless", "Too few comments"))
top_topic_short_colors <- c(top_topic_short_colors, c("grey25", "white"))

tidy_counties_geo$first_short_emplaced <- factor(tidy_counties_geo$first_short_emplaced, levels = top_topic_short_labels)

fig6 <-    ggplot() +
  geom_polygon(data = tidy_counties_geo[which(tidy_counties_geo$n_comments >= 0),], 
               aes(x = long, y = lat, group = group, fill=first_short_emplaced), color="black") +
  scale_fill_manual(values = top_topic_short_colors,
                    labels = top_topic_short_labels,
                    name = "Concern",
                    guide = guide_legend( 
                      keyheight = unit(5, units = "mm"), keywidth=unit(5, units = "mm"), 
                      label.position = "left", title.position = 'top', nrow=8) ) +

  geom_point(data = hvhf_wells, aes(x = long, y = lat, shape = "HVHF well app."), color = "black", size = 2) +
  scale_shape_manual(name = "", values=17, label="HVHF well\napplication") +
  geom_polygon(data = marcellus_geo, 
               aes(x = long, y = lat, group = group, color="Marcellus Shale"), alpha = .25) +
  scale_color_manual(name = "", values="grey30", label="Marcellus\nShale",
                     guide = guide_legend( 
                       keyheight = unit(5, units = "mm"), keywidth=unit(8, units = "mm"), 
                       label.position = "left") 
  ) +
  coord_cartesian(xlim = c(-79.46215, -72.05621),ylim = c(40.59610, 45.01585)) +
  
  theme_void() +
  theme(
    legend.direction = "vertical",
    legend.margin = margin(0,0,0,0),
    legend.text = element_text(size =9, colour = "black"),
    legend.key.height = unit(.25, "cm"),
    legend.key.width = unit(1, "cm"),
    legend.position =c(.30,.20),
    legend.box = "horizontal",
    legend.box.just = "top",
    legend.box.margin = margin(0,0,0,0)
  ) +
  guides(fill = guide_legend(order = 1),
         size = guide_legend(order = 2),
         color = guide_legend(order = 3),
         shape = guide_legend(order = 4))

print(fig6)
ggsave("fig6.pdf", fig6, 
       width = 20, height = 15, units = "cm", dpi = 500)

### Plot overall comment distribution
color.scheme <-RColorBrewer::brewer.pal(8,"Purples") # generate the color scheme to use

fig1 <- ggplot() +
  geom_polygon(data = tidy_counties_geo[which(tidy_counties_geo$n_comments >= 0),], 
               aes(x = long, y = lat, group = group, fill=comments_per10K), color="black") +
  geom_point(data = county_centroids, aes(x = x, y = y, size = n_comments), alpha = .35, color = "red") +
  scale_size(breaks = c(10,100, 500), range = c(.5,10), name = "Total number\nof comments") +
  scale_fill_gradientn(colors=color.scheme, limits = c(min(tidy_counties_geo$comments_per10K),max(tidy_counties_geo$comments_per10K)), breaks = c(1,10,30, 65),
                       trans = "sqrt",
                       guide = guide_legend( 
                         keyheight = unit(5, units = "mm"), keywidth=unit(8, units = "mm"), 
                         label.position = "left", title.position = 'top', nrow=8),
                       name = "Comments per\n10,000 residents") +
  geom_point(data = hvhf_wells, aes(x = long, y = lat, shape = "HVHF well app."), color = "black", size = 2) +
  scale_shape_manual(name = "", values=17, label="HVHF well\napplication") +
  geom_polygon(data = marcellus_geo, 
               aes(x = long, y = lat, group = group, color="Marcellus Shale"), alpha = .25) +
  scale_color_manual(name = "", values="grey30", label="Marcellus\nShale",
                     guide = guide_legend( 
                       keyheight = unit(5, units = "mm"), keywidth=unit(8, units = "mm"), 
                       label.position = "left") 
  ) +
  coord_cartesian(xlim = c(-79.46215, -72.05621),ylim = c(40.59610, 45.01585)) +
  theme_void() +
  theme(
    legend.direction = "vertical",
    legend.margin = margin(0,0,0,0),
    legend.text = element_text(size =9, colour = "black"),
    legend.key.height = unit(.25, "cm"),
    legend.key.width = unit(1, "cm"),
    #legend.spacing.x = unit(.1, 'cm'),
    legend.position =c(.30,.20),
    legend.box = "horizontal",
    legend.box.just = "top",
    legend.box.margin = margin(0,0,0,0)
  ) +
  guides(fill = guide_legend(order = 1),
         size = guide_legend(order = 2),
         color = guide_legend(order = 3),
         shape = guide_legend(order = 4))
print(fig1)
ggsave("fig1.pdf", fig1, 
       width = 20, height = 15, units = "cm", dpi = 500)
