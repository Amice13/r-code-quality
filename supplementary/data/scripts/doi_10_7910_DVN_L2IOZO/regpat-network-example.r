# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# This is the R code to reproduce the results presented in the article 
# Regional Innovator Networks - A review and an application with R 
# by Holger Graf
# Date: 2017-09-21
#
# You can download data samples along with this file at <http://dx.doi.org/10.7910/DVN/L2IOZO>. 
# The sample includes data on the four regions covered in the article and some helper files.
#
# If you want to perform the analysis for other regions, you can download the REGPAT and HAN databases 
# from the OECD: http://www.oecd.org/sti/intellectual-property-statistics-and-analysis.htm#ipdata 
# You need to fill in a short on-line form to get access to the data.
# 
# It is expected that you have a running installation of R (https://cran.r-project.org/) 
# In addition, I recommend to use RStudio as IDE for R 
#
# You will need the following R packages: 
#              data.table, stringr, Matrix, igraph, xtable, intergraph, sna, network
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Set your working directory, i.e. where your codefiles and output are stored. 

# setwd("C:/vusers/Forschung/Papers/Handbook_Methods-networks/Regpat-example")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                             -------- 1a Read sample data --------
#         Skip this part if you read the original data and continue in line 51
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Set a path variable which points to where the Sample REGPAT and HAN data is stored
path <- "Path to your data/" 
# path <- "C:/vusers/Forschung/Papers/Handbook_Methods-networks/Regpat-example/" # in my case

# Read 201602_EPO_IPC.txt 
EPO.IPC.201602 <- read.table(paste(path,"EPO.IPC.201602.SMBH.txt",sep=""),sep="|",encoding="UTF-8",header=T,stringsAsFactors=FALSE)

# Read 201602_EPO_Inv_reg.txt 
EPO.INV.REG.201602 <- read.table(paste(path,"EPO.INV.REG.201602.SMBH.txt",sep=""),sep="|",encoding="UTF-8",header=T, stringsAsFactors = FALSE)

# Read data on Regions 
REGPAT.REGIONS <- read.table(paste(path,"REGPAT.REGIONS.txt",sep=""),sep="|",encoding="UTF-8",header=T, stringsAsFactors = FALSE)

# Read 201609_HAN_Name.txt
HAN.Name.201609 <- read.table(paste(path,"HAN.Name.201609.SMBH.txt",sep=""),sep="|",encoding="UTF-8",header=T, stringsAsFactors = FALSE)

# Read 201609_HAN_Patents.txt 
HAN.Patents.201609 <- read.table(paste(path,"HAN.Patents.201609.SMBH.txt",sep=""),sep="|",encoding="UTF-8",header=T, stringsAsFactors = FALSE)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                             -------- 1b Read original data --------
#             If you use the example files, skip this part and continue in line 100
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Set a path variable which points to where the REGPAT data is stored
path <- "Path to your data/REGPAT_201602/201602_EPO_REGPAT/" # i.e. the 201602_EPO_REGPAT folder 
# path <- "C:/vusers/Forschung/Daten/OECD-patents/201602/REGPAT_201602/201602_EPO_REGPAT/" # in my case

# 'fread' in package "data.table" for faster reading of data as compared to 'read.table', however, 
# column classes are not identified the same way and behavior of class data.table is a bit different 
# in some instances, so we convert to data.frame right after import. Example data below is read in
# the traditional way, so you can try both ways.

library(data.table)
# Read 201602_EPO_IPC.txt 
EPO.IPC.201602 <- fread(paste(path,"201602_EPO_IPC.txt",sep=""),sep="|",encoding="UTF-8",header=T,stringsAsFactors=FALSE)
EPO.IPC.201602 <- as.data.frame(EPO.IPC.201602)

# Read 201602_EPO_Inv_reg.txt 
EPO.INV.REG.201602 <- fread(paste(path,"201602_EPO_Inv_reg.txt",sep=""),sep="|",encoding="UTF-8",header=T, stringsAsFactors = FALSE)
EPO.INV.REG.201602 <- as.data.frame(EPO.INV.REG.201602)
mode(EPO.INV.REG.201602$Appln_id) <- "integer" # to convert Application IDs from character to integer
mode(EPO.INV.REG.201602$Person_id) <- "integer"
mode(EPO.INV.REG.201602$Reg_share) <- "numeric"
mode(EPO.INV.REG.201602$Inv_share) <- "numeric"

# Read data on Regions 
path <- "Path to your data/REGPAT_201602/" # i.e. the REGPAT_201602 folder 
# path <- "C:/vusers/Forschung/Daten/OECD-patents/201602/REGPAT_201602/"

REGPAT.REGIONS <- fread(paste(path,"REGPAT_Regions.txt",sep=""),sep="|",encoding="UTF-8",header=T, stringsAsFactors = FALSE)
REGPAT.REGIONS <- as.data.frame(REGPAT.REGIONS)

# Set  a path variable which points to where the HAN data is stored
path <- "Path to your data/HAN_201609/" # i.e. the HAN_201609 folder 
# path <- "C:/vusers/Forschung/Daten/OECD-patents/201609/HAN_201609/"

# Read 201609_HAN_Name.txt 
HAN.Name.201609 <- fread(paste(path,"201609_HAN_Name.txt",sep=""),sep="|",encoding="UTF-8",header=T, stringsAsFactors = FALSE)
HAN.Name.201609 <- as.data.frame(HAN.Name.201609)
mode(HAN.Name.201609$HAN_ID) <- "integer"
mode(HAN.Name.201609$Matched) <- "integer"

# Read 201609_HAN_Patents.txt 
HAN.Patents.201609 <- fread(paste(path,"201609_HAN_Patents.txt",sep=""),sep="|",encoding="UTF-8",header=T, stringsAsFactors = FALSE)
HAN.Patents.201609 <- as.data.frame(HAN.Patents.201609)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                             ---- 2 Choose focal regions ----
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# In the article that was Stuttgart, Munich, Berlin, and Hamburg
regions <- c("DE111","DE212","DE300","DE600") # these are their regional codes (Reg_Code in REGPAT.REGIONS)
nregs <- length(regions)
regions <- data.frame(Reg_Code = regions, City = c("Stuttgart","Munich","Berlin","Hamburg"))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#              -------- 3 Generate edgelists per region and year --------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ~ 3.1 Generate list of Application IDs (Appln_id) for each region ----
Appln_id.reg <- list()

# Application IDs of focal regions with at least one inventor located in focal region
# In the following, lists have 'nregs' elements which are indexed with [[r]] in loops and if  
# we have different periods, these time windows are indexed with [[i]] (not before code section 3.9).

for (r in 1:nregs){
     Appln_id.reg[[r]] <- unique(EPO.INV.REG.201602$Appln_id[EPO.INV.REG.201602$Reg_code %in% regions[r,1]])
}

names(Appln_id.reg) <- regions[,2]


# ~ 3.2 Create Appln_id and Prio_Year combinations for each region ----
# Prio_Year is the priority year, i.e. the year of the first filing of a patent
EPO.IPC.reg <- list()

for (r in 1:nregs){
     EPO.IPC.reg[[r]] <- unique(data.frame(ID = EPO.IPC.201602$Appln_id[EPO.IPC.201602$Appln_id %in% Appln_id.reg[[r]]], 
                                           Prio_Year = EPO.IPC.201602$Prio_Year[EPO.IPC.201602$Appln_id %in% Appln_id.reg[[r]]]))
}

names(EPO.IPC.reg) <- regions[,2]


# ~ 3.3 Create Appln_id and inventor names (INV) combinations for each region ----
INV.ID.reg <- list()

for (r in 1:nregs){
     INV.ID.reg[[r]] <- data.frame(ID = EPO.INV.REG.201602$Appln_id[EPO.INV.REG.201602$Appln_id %in% Appln_id.reg[[r]]], 
                                   Inv_name = EPO.INV.REG.201602$Inv_name[EPO.INV.REG.201602$Appln_id %in% Appln_id.reg[[r]]])
}


# ~  3.4 Create Appln_id and Applicant combinations for each region ----
HAN.ID.reg <- list()

for (r in 1:nregs){
     HAN.ID.reg[[r]] <- data.frame(ID = HAN.Patents.201609$Appln_id[HAN.Patents.201609$Appln_id %in% Appln_id.reg[[r]]], 
                                   HAN_ID = HAN.Patents.201609$HAN_ID[HAN.Patents.201609$Appln_id %in% Appln_id.reg[[r]]])
}

for (r in 1:nregs){
     HAN.ID.reg[[r]] <- merge(HAN.ID.reg[[r]],HAN.Name.201609[,1:2])
     names(HAN.ID.reg[[r]])[3] <- "HAN"
}

# ~ 3.5 Clean inventor names ----

library(stringr)
string_merge <- function(s){str_c(s[1],s[2],sep=", ")}

# Read concordance tables for string replacement
# We use theses tables to search for strings in the first column and replace them with the respective strings in the second column
accents <- read.table(paste(path,"accents.txt",sep=""),header = T,encoding="UTF-8",sep="|",colClasses="character")
corrupted <- read.table(paste(path,"corrupted.txt",sep=""),header = T,encoding="UTF-8",sep="|",colClasses="character")
foreign <- read.table(paste(path,"foreign.txt",sep=""),header = T,encoding="UTF-8",sep="|",colClasses="character")

# Modifiers are titles, used in Germany, that will be deleted for name matching. 
# In other countries, titles might be different so you will have to add them accordingly 
modifiers <- read.table(paste(path,"modifiers.txt",sep=""),header = T,encoding="UTF-8",sep="|",colClasses="character")


for (r in 1:nregs){
     INV.ID.reg[[r]]$INV <- INV.ID.reg[[r]]$Inv_name
     INV.ID.reg[[r]]$INV <- toupper(INV.ID.reg[[r]]$INV) # Change to upper case
     INV.ID.reg[[r]]$INV <- gsub(","," ",INV.ID.reg[[r]]$INV) # delete colons
}


# Now, go through all inventor names and remove accents, corrupted strings and so on according to the four concordance tables
for (r in 1:nregs){
     for (i in 1:dim(accents)[1]){
          INV.ID.reg[[r]]$INV <- gsub(accents[i,1],accents[i,2],INV.ID.reg[[r]]$INV,fixed=TRUE) 
     }
     for (i in 1:dim(corrupted)[1]){
          INV.ID.reg[[r]]$INV <- gsub(corrupted[i,1],corrupted[i,2],INV.ID.reg[[r]]$INV,fixed=TRUE) 
     }
     for (i in 1:dim(foreign)[1]){
          INV.ID.reg[[r]]$INV <- gsub(foreign[i,1],foreign[i,2],INV.ID.reg[[r]]$INV,fixed=TRUE) 
     }
     for (i in 1:dim(modifiers)[1]){
          INV.ID.reg[[r]]$INV <- gsub(modifiers[i,1],"",INV.ID.reg[[r]]$INV,fixed=TRUE) 
     }
}


# Remove extra blanks
# before  R 3.2.0 use 
# trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# instead of trimws()

for (r in 1:nregs){
     INV.ID.reg[[r]]$INV <- trimws(INV.ID.reg[[r]]$INV)
     while(sum(str_detect(INV.ID.reg[[r]]$INV,"  ")) > 0){INV.ID.reg[[r]]$INV <- gsub("\\s+"," ",INV.ID.reg[[r]]$INV)}
}


# add comma to separate surname and name
for (r in 1:nregs){
     INV.ID.reg[[r]]$INV <- sapply(str_split(INV.ID.reg[[r]]$INV," "),string_merge) 
}


# ~ 3.6 Shorten names of Applicants if needed ----
# This is especially useful for plotting networks with node names

for (r in 1:nregs){
     HAN.ID.reg[[r]]$HAN[HAN.ID.reg[[r]]$HAN == "FRAUNHOFER GESELLSCHAFT ZUR FOERDERUNG DER ANGEWANDTEN FORSCHUNG EV"] <- "FRAUNHOFER"
     HAN.ID.reg[[r]]$HAN[HAN.ID.reg[[r]]$HAN == "MAX PLANCK GESELLSCHAFT ZUR FOERDERUNG DER WISSENSCHAFTEN EV"] <- "MAX PLANCK"
     HAN.ID.reg[[r]]$HAN[HAN.ID.reg[[r]]$HAN == "DEUTSCHES ZENTRUM FUER LUFT & RAUMFAHRT EV"] <- "DLR"
     HAN.ID.reg[[r]]$HAN[HAN.ID.reg[[r]]$HAN == "DEUTSCHES KREBSFORSCHUNGSZENTRUM STIFTUNG DES OEFFENTLICHEN RECHTS"] <- "DKFZ"
}


# ~ 3.7 HAN.INV.reg - list of inventor applicant combinations ----
# Merge applicants, inventors, and priority year by the patent ID
HAN.INV.reg <- list()

for (r in 1:nregs){
     HAN.INV.reg[[r]] <- merge(HAN.ID.reg[[r]][,c("ID","HAN")],INV.ID.reg[[r]][,c("ID","INV")],by="ID")
     HAN.INV.reg[[r]] <- merge(HAN.INV.reg[[r]],EPO.IPC.reg[[r]][,c("ID","Prio_Year")],by="ID")
}

# ~ 3.8 Choose time windows ----

library(Matrix)
# The chosen time window is seven years, to change this, you need to manipulate yrs.window accordingly.
# The start and end years are more or less arbitrary. You can start earlier and end later, depending on the REGPAT version.
yrs.window <- cbind(start = c(1990:2007),end = c(1996:2013))
wind.len <- yrs.window[1,2]-yrs.window[1,1]+1 # length of the windows


# ~ 3.9 Patent IDs per Region and Period ----
PatID.reg <- list()
for (r in 1:nregs)  PatID.reg[[r]] <- list()
names(PatID.reg) <- regions[,2]

for (r in 1:length(PatID.reg)){
     for (i in 1:dim(yrs.window)[1]){
          PatID.reg[[r]][[i]] <- EPO.IPC.reg[[r]][EPO.IPC.reg[[r]][["Prio_Year"]] %in% c(yrs.window[i,1]:yrs.window[i,2]),"ID"]
          PatID.reg[[r]][[i]] <- unique(PatID.reg[[r]][[i]])
          names(PatID.reg[[r]])[i]  <- paste("y",yrs.window[i,1],yrs.window[i,2],sep=".")
     }
}

# ~ 3.10 Create 2-mode edgelists for every period per region ----
HAN.INV <- list()
for (r in 1:nregs)  HAN.INV[[r]] <- list()
names(HAN.INV) <- regions[,2]

for (r in 1:length(HAN.INV)){
     for (i in 1:dim(yrs.window)[1]){
          HAN.INV[[r]][[i]] <- HAN.INV.reg[[r]][HAN.INV.reg[[r]][["Prio_Year"]] %in% c(yrs.window[i,1]:yrs.window[i,2]),]
          HAN.INV[[r]][[i]]  <- unique(HAN.INV[[r]][[i]][,c('HAN','INV')])
          names(HAN.INV[[r]])[i]  <- paste("y",yrs.window[i,1],yrs.window[i,2],sep=".")
     }
}

# ~ 3.11 Create the 2-mode matrix in sparse format ----
two.mode <- list()
for (r in 1:nregs)  two.mode[[r]] <- list()
names(two.mode) <- regions[,2]

for (r in 1:nregs){
     for (i in 1:dim(yrs.window)[1]){
          two.mode[[r]][[i]]  <- xtabs(~ HAN.INV[[r]][[i]]$HAN + HAN.INV[[r]][[i]]$INV,sparse=TRUE)
          names(two.mode[[r]])[i]  <- paste("y",yrs.window[i,1],yrs.window[i,2],sep=".")
     }
}


# ~ 3.12 Create adjacency matrix ----

adj.mat <- list()
for (r in 1:nregs)  adj.mat[[r]] <- list()
names(adj.mat) <- regions[,2]

for (r in 1:nregs){
     for (i in 1:dim(yrs.window)[1]){
          adj.mat[[r]][[i]]  <- two.mode[[r]][[i]] %*% t(two.mode[[r]][[i]]) 
          names(adj.mat[[r]])[i]  <- paste("y",yrs.window[i,1],yrs.window[i,2],sep=".")
     }
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                           -------- 4 Networks with igraph --------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ~ 4.1 Conversion into igraph format ----
# To convert the sparse adjacency matrix into a format for network analysis, we use the igraph package
library(igraph)

network.ig <- list()
for (r in 1:nregs)  network.ig[[r]] <- list()
names(network.ig) <- regions[,2]

for (r in 1:nregs){
     for (i in 1:dim(yrs.window)[1]){
          network.ig[[r]][[i]] <- graph.adjacency(adj.mat[[r]][[i]],mode="undirected",weighted="weight",diag=FALSE)
          names(network.ig[[r]])[i]  <- paste("y",yrs.window[i,1],yrs.window[i,2],sep=".")
     }
}

# ~ 4.2 Extract main component ----
# useful for some measures 
network.ig.mc <- list()
for (r in 1:nregs)  network.ig.mc[[r]] <- list()
names(network.ig.mc) <- regions[,2]

for (r in 1:nregs){
     for (i in 1:dim(yrs.window)[1]){ 
          g.temp <- network.ig[[r]][[i]]
          g.temp.mc <- decompose.graph(g.temp, min.vertices=max(clusters(g.temp,mode="strong")$csize)) # A graph object is generated for every component (cluster) with at least two members
          g.temp.mc <- g.temp.mc[[1]]
          network.ig.mc[[r]][[i]] <- g.temp.mc
     }
}

# ~ 4.3 Results: network statistics ----
stats <- list()
for (r in 1:nregs)  stats[[r]] <- list()
names(stats) <- regions[,2]

# Some functions don't exist in igraph, so we could define them

reachability.igraph <- function(graph){
     graph.reach <- shortest.paths(graph)
     graph.reach[graph.reach == "Inf"] <- 0
     graph.reach[graph.reach > 0] <- 1
     graph.reach
}

connectedness.igraph <- function(graph){
     sum(reachability.igraph(graph))/(vcount(graph)*(vcount(graph)-1))
}

# All of the network functions are applied over the list of networks for each region in a loop.
# For more explanation of the functions consult the help, e.g. ?clusters 
for (r in 1:nregs){
     # 'length' of the vector of patent IDs gives the number of patents
     temp.patcount <- lapply(PatID.reg[[r]],length)
     # 'vcount': number of nodes
     temp.vcount <- lapply(network.ig[[r]],vcount)
     # 'ecount': number of edges
     temp.ecount <- lapply(network.ig[[r]],ecount)
     # 'graph.density': density
     temp.density <- lapply(network.ig[[r]],graph.density)
     # To calculate the mean degree, we have to nest two functions, calculate the degree of each vertex (inner), then take the mean (outer)
     temp.mean.deg <- lapply(lapply(network.ig[[r]], function(x) degree(x, mode="all", loops = FALSE, normalized = FALSE)),mean) 
     # Similar, just use graph-strength, which is the weighted mean degree
     temp.mean.strength <- lapply(lapply(network.ig[[r]], function(x) graph.strength(x)),mean)
     # 'components': finds connected components in a graph.  
     temp.no.comp <- lapply(network.ig[[r]], function(x) components(x,mode="strong")$no)
     temp.max.comp <- lapply(lapply(network.ig[[r]], function(x) components(x,mode="strong")$csize),max)
     temp.isolates <- lapply(lapply(network.ig[[r]], function(x) components(x,mode="strong")$csize == 1),sum)
     # 'connectedness.igraph' is the function, we declared above
     temp.connect <- lapply(network.ig[[r]],connectedness.igraph) 
     # 'centralization': several measures for ego centrality and graph centralization
     temp.cent.deg <- lapply(network.ig[[r]], function(x) centralization.degree(x,mode="all")$centralization)
     temp.cent.bet <- lapply(network.ig[[r]], function(x) centralization.betweenness(x,directed=FALSE)$centralization)
     temp.cent.ev <- lapply(network.ig[[r]], function(x) centralization.evcent(x,directed=FALSE)$centralization)
     temp.cent.ev.mc <- lapply(network.ig.mc[[r]], function(x) centralization.evcent(x,directed=FALSE)$centralization)
     temp.cent.clo <- lapply(network.ig.mc[[r]], function(x) centralization.closeness(x,mode="all")$centralization)
     # 'diameter': diameter of the *main component* of the graph
     temp.diameter <- lapply(network.ig.mc[[r]], function(x) diameter(x,weights=NA))
     # 'mean_distance': mean distance between nodes or average path length, average among reachable nodes 
     temp.path.length <- lapply(network.ig[[r]],mean_distance)
     # 'transitivity': transitivity or clustering coefficient
     temp.trans <- lapply(network.ig[[r]],transitivity)
     # Combine everything
     stats[[r]]$results <- data.frame(patcount = unlist(temp.patcount),
                                      vcount = unlist(temp.vcount), 
                                      ecount = unlist(temp.ecount), 
                                      density = unlist(temp.density),
                                      mean_deg = unlist(temp.mean.deg),
                                      mean_strength = unlist(temp.mean.strength), 
                                      no_comp = unlist(temp.no.comp), 
                                      max_comp = unlist(temp.max.comp), 
                                      share_mc = unlist(temp.max.comp)/unlist(temp.vcount),
                                      isolates = unlist(temp.isolates),
                                      share_isolates = unlist(temp.isolates)/unlist(temp.vcount),
                                      connect = unlist(temp.connect),
                                      cent_deg = unlist(temp.cent.deg),
                                      cent_bet = unlist(temp.cent.bet),
                                      cent_ev = unlist(temp.cent.ev),
                                      cent_ev_mc = unlist(temp.cent.ev.mc),
                                      cent_clo = unlist(temp.cent.clo),
                                      diameter = unlist(temp.diameter),
                                      path_length = unlist(temp.path.length),
                                      trans = unlist(temp.trans)  )
}

rm(list = ls(pattern="temp")) # remove all temporary files

# Convert the list 'stats' into a three dimensional array (period, statistic, region)
stats1 <- array(NA,dim=c(dim(yrs.window)[1],dim(stats[[1]]$results)[2],nregs))
dimnames(stats1) <- list(c(yrs.window[,2]),colnames(stats[[1]]$results),regions[,2])

for (r in 1:nregs){
     stats1[,,r] <- as.matrix(stats[[r]]$results)
}


# Now we have a three dimensional array with all network statistics that we calculated.
dimnames(stats1)
# We can compare a specific measure for all regions over time
round(stats1[,"connect",],3)

# or look at all measures across all regions in any given period
round(stats1["2013",,],2)

# or look at one region only
round(stats1[,,"Stuttgart"],2)



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                             -------- 5 Output --------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

path.out <- "results_20171021/" # this way it will be relative to your working directory
dir.create(path.out) # Creates the directory for output if it does not exist

regs <- as.character(regions$City)


# ~Figure 1 - No of patents and nodes ---- 
# The following characters are set to be able to refer to them in the plot and to reuse the 
# code with other statistics later on

name.stat1 <- "patcount"
name.stat.print1 <- "No. of patents"
main.print1 <- "No. of patents"
name.stat2 <- "vcount"
name.stat.print2 <- "No. of nodes"
main.print2 <- "No. of nodes"

# Make a 16x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-","figure_1",".png",sep=""), width=16*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1),mfcol=c(1,2))
matplot(yrs.window[,2],stats1[,name.stat1,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print1,ylim=c(min(stats1[,name.stat1,regs]),max(stats1[,name.stat1,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.275),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
par(xpd=FALSE)
matplot(yrs.window[,2],stats1[,name.stat2,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print2,ylim=c(min(stats1[,name.stat2,regs]),max(stats1[,name.stat2,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.275),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()


# ~Plot networks ----

# I personally prefer the layout algorithm of the network package over than the one implemented in igraph
# Using the 'asNetwork' command of the intergraph package, we can convert the igraph object into a network object.
# There will be a warning that some of the objects (functions) are 'masked' from ‘package:igraph’ 
# To invoke the function of the desired package, use e.g. igraph::betweenness or sna::betweenness
 
library(intergraph)
library(sna)
library(network)

# This loop will plot all networks and save them as pdf files in the "path.out" folder 

# We want node size to be proportional to betweenness centrality and display the names of the five most central actors.
# Apart from that, we only change colors and line widths. 

# Plotting the main components of each network (region and period)

for (r in 1:nregs){                          # first loop - regions
     for (i in 1:length(yrs.window[,2])){    # second loop - periods
          
          pdf(paste(path.out,"nw-mc-",wind.len,"y-",regions[r,2],yrs.window[i,2],".pdf",sep=""), width=8, height=8)
          par(mar=c(1,1,1,1))
          nw <- asNetwork(network.ig.mc[[r]][[i]]) # This line selects the main components
          nw.bet <- sna::betweenness(nw,gmode="graph")
          v.size <- 2*sqrt(nw.bet+2)/sqrt(max(nw.bet)+2)+.25
          min.bet.top5 <- min(head(nw.bet[order(nw.bet,decreasing=TRUE)],5))
          plot(nw,
               label = ifelse(nw.bet >= min.bet.top5,network.vertex.names(nw),NA), # Label only the top five central actors
               label.cex = .4,
               label.col = "black",
               edge.col = "grey60",
               edge.lwd = .25,
               label.pos = 5,
               label.lwd = 1,
               boxed.labels = FALSE,
               vertex.border = "white",
               vertex.cex = v.size,
               vertex.col = "cyan",
               main=paste(regions[r,2],"_",yrs.window[i,1],"-",yrs.window[i,2],sep=""))
          
          dev.off()
     }
}


# Plotting complete networks for each region and period

for (r in 1:nregs){                          # first loop - regions
     for (i in 1:length(yrs.window[,2])){    # second loop - periods
          
          pdf(paste(path.out,"nw-",wind.len,"y-",regions[r,2],yrs.window[i,2],".pdf",sep=""), width=8, height=8)
          par(mar=c(1,1,1,1))
          nw <- asNetwork(network.ig[[r]][[i]])
          nw.bet <- sna::betweenness(nw,gmode="graph")
          v.size <- 2*sqrt(nw.bet+2)/sqrt(max(nw.bet)+2)+.25
          min.bet.top5 <- min(head(nw.bet[order(nw.bet,decreasing=TRUE)],5))
          plot(nw,
               label = ifelse(nw.bet >= min.bet.top5,network.vertex.names(nw),NA), # Label only the top five central actors
               label.cex = .4,
               label.col = "black",
               edge.col = "grey60",
               edge.lwd = .25,
               label.pos = 5,
               label.lwd = 1,
               boxed.labels = FALSE,
               vertex.border = "white",
               vertex.cex = v.size,
               vertex.col = "cyan",
               main=paste(regions[r,2],"_",yrs.window[i,1],"-",yrs.window[i,2],sep=""))
          
          dev.off()
     }
}



# ~Table 3 - Patents per region ----

library(xtable)

temp <- lapply(Appln_id.reg,length)

tab.temp <- as.data.frame(temp)
rownames(tab.temp) <- "EPO patent applications"
xtab.temp <- xtable(tab.temp,caption = "Number of patents per region", label = "tab-patents",align = rep("c",ncol(tab.temp)+1))
print(xtab.temp, caption.placement="top", booktabs=TRUE)


# ~Table A.1 - Most central actors ----

i <- which(yrs.window[,2] == 2012) # the 2006--2012 period

cent <- list()
for (r in 1:nregs)  cent[[r]] <- list()
names(cent) <- regions[,2]
for (r in 1:nregs){
     tmp <- data.frame(HAN = V(network.ig.mc[[r]][[i]])$name,degree = igraph::degree(network.ig.mc[[r]][[i]]),betweenness = igraph::betweenness(network.ig.mc[[r]][[i]], normalized = TRUE,weights = NA))
    cent[[r]] <- head(tmp[order(tmp$betweenness,decreasing=TRUE),"HAN"],10)
# cent[[r]] <- head(tmp[order(tmp$betweenness,decreasing=TRUE),],10)
# rownames(cent[[r]]) <- 1:10
}

temp.tab <- xtable(as.data.frame(cent),caption = paste("Top 10 nodes in the ",yrs.window[i,1],"--",yrs.window[i,2]," innovator networks according to betweenness centrality",sep=""))
print(temp.tab, caption.placement="top", booktabs=TRUE,hline.after=-1:10)

rm(list = ls(pattern="temp")) # remove all temporary files


# ~Figure 3 - selected network statistics on cohesion ---- 

# dimnames(stats1)[2] provides the available names 

name.stat <- "vcount"
name.stat.print <- "No. of nodes"
main.print <- "No. of nodes"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

name.stat <- "density"
name.stat.print <- "Density"
main.print <- "Density"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

name.stat <- "mean_deg"
name.stat.print <- "Mean degree"
main.print <- "Mean degree"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

name.stat <- "mean_strength"
name.stat.print <- "Weighted mean degree"
main.print <- "Weighted mean degree"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

name.stat <- "share_mc"
name.stat.print <- "Share in main component"
main.print <- "Share in main component"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

name.stat <- "connect"
name.stat.print <- "Connectedness"
main.print <- "Connectedness"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

name.stat <- "share_isolates"
name.stat.print <- "Share of isolates"
main.print <- "Share of isolates"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

name.stat <- "diameter"
name.stat.print <- "Diameter"
main.print <- "Diameter"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

name.stat <- "path_length"
name.stat.print <- "Average distance"
main.print <- "Average distance"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()


name.stat <- "trans"
name.stat.print <- "Transitivity / Clustering coefficient"
main.print <- "Transitivity / Clustering coefficient"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()


# ~Figure 4 - Centralization measures ---- 

name.stat <- "cent_deg"
name.stat.print <- "Centralization (degree)"
main.print <- "Centralization (degree)"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

name.stat <- "cent_bet"
name.stat.print <- "Centralization (betweenness)"
main.print <- "Centralization (betweenness)"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

name.stat <- "cent_ev"
name.stat.print <- "Centralization (eigenvector)"
main.print <- "Centralization (eigenvector)"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

name.stat <- "cent_ev_mc"
name.stat.print <- "Centralization (eigenvector)"
main.print <- "Centralization (eigenvector)"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

name.stat <- "cent_clo"
name.stat.print <- "Centralization (closeness)"
main.print <- "Centralization (closeness)"
# Make a 6x6 inch image at 300dpi
ppi <- 300
png(paste(path.out,wind.len,"y-",paste(substr(regs,1,1),sep="",collapse=""),"-",name.stat,".png",sep=""), width=10*ppi, height=6*ppi, res=ppi)
par(xpd=FALSE,mar=c(6,4,2,1))
matplot(yrs.window[,2],stats1[,name.stat,regs],type="l",xlab=paste(wind.len,"-year period",sep=""),ylab=name.stat.print,ylim=c(min(stats1[,name.stat,regs]),max(stats1[,name.stat,regs])),lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)))
grid()
par(xpd=TRUE)
legend("bottomright", inset=c(0,-0.25),legend = regs,lwd=2,lty=c(1:length(regs)),col=c(1:length(regs)),bty="n")
dev.off()

