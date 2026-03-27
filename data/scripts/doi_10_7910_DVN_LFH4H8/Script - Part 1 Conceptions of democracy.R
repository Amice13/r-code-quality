        

# Script for Conceptualizing and Measuring Citizens' Preferences for Democracy - Taking Stock of Three Decades of Research in a Fragmented Field

# Part 1: Analyze data table on coded articles dealing with conceptions of democracy

# Version: 8 November 2021


#### read data ####

library(readxl)
library(ggplot2)

df <- read_excel("Spreadsheet - Conceptions of democracy 10-2021.xlsx")
df.full <- data.frame(df)



#### How many conceptions predefined ####

table(df.full$A08.Number.of.concepts.predefined)

df.full$Study[df.full$A08.Number.of.concepts.predefined == 0 ]



#### Figure 2: Which conceptions are covered to what extent in the literature? ####

tab.1 <- data.frame(
                conception = c("Liberal democracy",
                   "Substantive democracy",
                   "Direct democracy",
                   "Authoritarian democracy",
                   "Stealth democracy",
                   "Populist democracy",
                   "None predefined") , 
                Freq = c(
                        sum( df.full[, 5] ),
                        sum( df.full[, 6] ),
                        sum( df.full[, 7] ),
                        sum( df.full[, 8] ),
                        sum( df.full[, 9] ),
                        sum( df.full[, 10] ) ,
                        sum( df.full[, 11] ))
                   )

tab.1 <- tab.1[order(c(1,5,2,6,4,3,7) ), ]


tab.1$conception <- factor(tab.1$conception, levels = tab.1$conception)  
                           

png(paste0("Figure_2_Freq_Conceptions.png"), width=12, height=6,units="cm", res=400)

ggplot() + 
        geom_bar(data = tab.1, aes(x = conception, y = Freq), stat = "identity") + scale_x_discrete(limits = rev(levels(tab.1$conception))) + 

        theme_bw() + 
        xlab("") + ylab("") +
        coord_flip()

dev.off()


pdf(paste0("Figure_2_Freq_Conceptions.pdf"), width=6, height=3)

ggplot() + 
        geom_bar(data = tab.1, aes(x = conception, y = Freq), stat = "identity") + scale_x_discrete(limits = rev(levels(tab.1$conception))) + 
        
        theme_bw() + 
        xlab("") + ylab("") +
        coord_flip()

dev.off()





#### Figure 3: Number of predefined conceptions per study ####

tab.2 <- as.data.frame( table(df.full$A08.Number.of.concepts.predefined) )

colnames(tab.2) <- c("Number",
                     "Frequency")


png(paste0("Figure_3_Num_Predef.png"), width=12, height=6,units="cm", res=400)

ggplot() + 
        geom_bar(data = tab.2, aes(x = Number, y = Frequency), stat = "identity") + 
        scale_x_discrete(limits = rev(levels(tab.2$Number))) + 
        theme_bw() + 
        xlab("# Predefined conceptions") + ylab("") +
        coord_flip()

dev.off()


pdf(paste0("Figure_3_Num_Predef.pdf"), width=6, height=3)

ggplot() + 
        geom_bar(data = tab.2, aes(x = Number, y = Frequency), stat = "identity") + 
        scale_x_discrete(limits = rev(levels(tab.2$Number))) + 
        theme_bw() + 
        xlab("# Predefined conceptions") + ylab("") +
        coord_flip()

dev.off()





#### Network analysis for describing affinities in the joint treatment of conceptions of democracy ####

df <- df.full[df.full$A07.None == 0 , 5:10] # subset columns and rows for network analysis

colnames(df) <- c("Liberal",
                  "Substantive",
                  "Direct",
                  "Authoritarian",
                  "Stealth",
                  "Populist")


library(proxy)

mat <- dist(t(df), method ="Russel", diag = T)
mat <- (1-mat)*98
mat


library(igraph)

g  <- graph.adjacency(as.matrix(mat), weighted=TRUE, mode = "undirected")

g <- graph_from_adjacency_matrix(as.matrix(mat), weighted=TRUE, mode = "undirected")

E(g)

E(g)$width <- E(g)$weight
E(g)$width <- E(g)$weight/8
E(g)$edge.color <- "gray80"

plot(g)

#### Figure 4: Joint coverage of conceptions ####

seed <- 1234

png(paste0("Figure_4_Network.png"), width=15, height=15,units="cm", res=400)

plot(g,edge.width=(edge_attr(g)$weight),
     vertex.size = colSums(df)*1,
     edge.arrow.size= 10, vertex.color="gray", edge.color = "grey70",
     vertex.frame.color="grey20", vertex.label.color="black", 
     vertex.label.cex=0.8, 
     vertex.label.dist=c(6,4,5,4,4,5)
)

dev.off()



pdf(paste0("Figure_4_Network.pdf"), width=10, height=6)

plot(g,edge.width=(edge_attr(g)$weight),
     vertex.size = colSums(df)*1,
     edge.arrow.size= 10, vertex.color="gray", edge.color = "grey70",
     vertex.frame.color="grey20", vertex.label.color="black", 
     vertex.label.cex=0.8, 
     vertex.label.dist=c(6,4,5,4,4,5)
)

dev.off()









#### Which conception with which other conception: select associations ####

table(df$Liberal, df$Substantive)

table(df$Liberal, df$Direct)

table(df$Liberal, df$Stealth)

table(df$Direct, df$Stealth)




#### Inductive versus deductive approach and predefined conceptions ####

table(df.full$A09.Ex.ante.conceptualization.of.specific.democracy.conceptions, df.full$B01.inductive.dimension.creation) # inductive vs. deductive in columns

table(df.full$A09.Ex.ante.conceptualization.of.specific.democracy.conceptions)




#### Figure 5: Item numbers user per concept ####

df.i <- df.full[df.full$A09.Ex.ante.conceptualization.of.specific.democracy.conceptions == "yes" , c(1, 18:23) ]

colnames(df.i) <- c("study",
                    "liberal",
                    "substantive",
                    "participatory",
                    "authoritarian",
                    "stealth",
                    "populist")

library(ggplot2)


df.i2 <- reshape(df.i, varying=c("liberal",
                                 "substantive",
                                 "participatory",
                                 "authoritarian",
                                 "stealth",
                                 "populist"),
                   direction="long", idvar=c("study"),
                   v.names="DV", timevar="cat")

df.i2$cat2 <- df.i2$cat

df.i2$cat2[df.i2$cat == 1] <- "Liberal \n democracy"
df.i2$cat2[df.i2$cat == 2] <- "Substantive \n democracy"
df.i2$cat2[df.i2$cat == 3] <- "Direct \n democracy"
df.i2$cat2[df.i2$cat == 4] <- "Authorit. \n democracy"
df.i2$cat2[df.i2$cat == 5] <- "Stealth \n democracy"
df.i2$cat2[df.i2$cat == 6] <- "Populist democracy"


df.i2$cat2 <- as.factor(df.i2$cat2)

df.i2$cat2 <- factor(df.i2$cat2 , c("Liberal \n democracy",
                                   "Substantive \n democracy",
                                   "Direct \n democracy",
                                   "Authorit. \n democracy",
                                   "Stealth \n democracy",
                                   "Populist democracy")
)



png(paste0("Figure_5_Num_Items.png"), width=16, height=6,units="cm", res=400)

ggplot() + geom_boxplot(data = df.i2, fill = "grey90", aes(group = cat2, x = cat2, y = DV)) +
        xlab("") + ylab("# items") + theme_bw()

dev.off()
   


pdf(paste0("Figure_5_Num_Items.pdf"), width=8, height=3)

ggplot() + geom_boxplot(data = df.i2, fill = "grey90", aes(group = cat2, x = cat2, y = DV)) +
        xlab("") + ylab("# items") + theme_bw()

dev.off()





#### How many in the inductive approach ####

df.induct <- df.full[  , c(1, 25) ]

summary(df.induct[ , 2]) # count NAs - these are inductive (no items used for deductive measurement)




##### Aggregration strategies by inductive vs. deductive measurement approach #### 

t(table(df.full$B01.inductive.dimension.creation, df.full$B10.Operationalization.of.the.conceptions.of.democracy))










##### Annex: Concepts over time #### 

df.full$decade <- "None"
df.full$decade[df.full$Year > 1989] <- "1990s"
df.full$decade[df.full$Year > 1999] <- "2000s"
df.full$decade[df.full$Year > 2009] <- "2010s"

table(df.full$decade)



tab.3 <- data.frame(conception = c("Liberal democracy",
                                   "Substantive democracy",
                                   "Direct democracy",
                                   "Authoritarian democracy",
                                   "Stealth democracy",
                                   "Populist democracy",
                                   "None predefined") )


tab.3 <- cbind(tab.3, rbind(table(df.full$decade, df.full[, 5])[, 2],
                                table(df.full$decade, df.full[, 6])[, 2],
                                table(df.full$decade, df.full[, 7])[, 2],
                                table(df.full$decade, df.full[, 8])[, 2],
                                table(df.full$decade, df.full[, 9])[, 2],
                                table(df.full$decade, df.full[, 10])[, 2],
                                table(df.full$decade, df.full[, 11])[, 2])
                                )


tab.3 <- cbind(tab.3, t(rbind(tab.3$`1990s`/sum(tab.3$`1990s`),
                              tab.3$`2000s`/sum(tab.3$`2000s`),
                              tab.3$`2010s`/sum(tab.3$`2010s`))
                              )
)

tab.3





##### Annex: Methods over time #### 


table(df.full$decade, df.full[, "B10.Operationalization.of.the.conceptions.of.democracy" ]  )


df.full$both <- 0
df.full$both[  df.full[, "C04.Scaling.techniques.used"] == "yes" &  df.full[, "C06.Combinatorial.approach"] == "yes"   ] <- 1


table(df.full$decade, df.full[, "C04.Scaling.techniques.used"]  )
table(df.full$decade, df.full[, "C06.Combinatorial.approach"]  )
table(df.full$decade, df.full[, "both"]  )











##### Annex: Geographic scope #### 

tab.regions <- data.frame(
        Region = c("Africa",
                       "Americas",
                       "Asia",
                       "Europe",
                       "Oceania"),
        Freq = c(
                sum( df.full$C03a.Africa, na.rm = T ),
                sum( df.full$C03b.Americas, na.rm = T),
                sum( df.full$C03c.Asia, na.rm = T ),
                sum( df.full$C03d.Europe, na.rm = T ),
                sum( df.full$C03e.Oceania, na.rm = T )
                )
)



# Declare region variable as factor for the plot
tab.regions$Region <- factor(tab.regions$Region, 
                             levels = c("Africa",
                                        "Americas",
                                        "Asia",
                                        "Europe",
                                        "Oceania") )



png(paste0("Figure_A2_Regions_covered.png"), width=12, height=12,units="cm", res=400)

ggplot() + 
        geom_bar(data = tab.regions, aes(x = Region, y = Freq), stat = "identity") + 
        theme_bw() + scale_x_discrete(limits = rev(levels(tab.regions$Region))) + 
        xlab("Region") + ylab("Frequency") +
        coord_flip()

dev.off()






##### Annex: How many countries - categorized #### 

df.full$C01...countries

df.full$C02...countries.coded

unique(df.full$C02...countries.coded)
df.full$C02...countries.coded <- factor(df.full$C02...countries.coded, 
                                        levels = c("1", "2", "3 to 5", "6 to 9",
                                                   "10 to 19", "20 to 50", "more than 50"))  

png(paste0("Figure_A1_Num_countries.png"), width=16, height=12,units="cm", res=400)

ggplot() + 
        geom_bar(data = df.full, aes(y = C02...countries.coded)) + 
        theme_bw() +  scale_x_continuous(breaks = c(0,10,20,30,40,50)) + 
        xlab("# of covered countries") + ylab("") +
        coord_flip() 

dev.off()
