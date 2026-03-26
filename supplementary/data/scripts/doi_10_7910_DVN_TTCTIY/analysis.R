# Replication script for Leifeld, Philip and Dana R. Fisher: Membership 
# Nominations in International Scientific Assessments. Nature Climate Change.

################################################################################
# Load packages and set random seed
################################################################################

library("statnet")
library("xergm")
library("texreg")
library("xtable")

sessionInfo()  # display version numbers of packages for replication
# R version 3.3.0 (2016-05-03)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 16.04.2 LTS
# 
# locale:
#  [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
#  [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
#  [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
#  [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
#  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
# [11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] xtable_1.8-0         texreg_1.36.23       xergm_1.8.2         
#  [4] GERGM_0.11.2         rem_1.1.2            tnam_1.6.5          
#  [7] btergm_1.9.0         ggplot2_2.0.0        xergm.common_1.7.7  
# [10] statnet_2016.4       sna_2.4              ergm.count_3.2.0    
# [13] tergm_3.4.0          networkDynamic_0.9.0 ergm_3.7.1          
# [16] network_1.13.0       statnet.common_3.3.0
# 
# loaded via a namespace (and not attached):
#  [1] deSolve_1.12        gtools_3.5.0        lpSolve_5.6.13     
#  [4] splines_3.3.0       lattice_0.20-33     mstate_0.2.8       
#  [7] colorspace_1.2-6    flexsurv_0.7        stats4_3.3.0       
# [10] mgcv_1.8-12         survival_2.39-4     nloptr_1.0.4       
# [13] RColorBrewer_1.1-2  muhaz_1.2.6         speedglm_0.3-1     
# [16] trust_0.1-7         plyr_1.8.3          robustbase_0.92-5  
# [19] munsell_0.4.2       gtable_0.1.2        caTools_1.17.1     
# [22] mvtnorm_1.0-5       coda_0.18-1         permute_0.8-4      
# [25] parallel_3.3.0      DEoptimR_1.0-4      Rcpp_0.12.11       
# [28] KernSmooth_2.23-15  ROCR_1.0-7          scales_0.4.1       
# [31] gdata_2.17.0        vegan_2.3-1         RcppParallel_4.3.20
# [34] lme4_1.1-10         gplots_2.17.0       grid_3.3.0         
# [37] quadprog_1.5-5      tools_3.3.0         bitops_1.0-6       
# [40] magrittr_1.5        RSiena_1.1-232      cluster_2.0.4      
# [43] MASS_7.3-45         Matrix_1.2-6        minqa_1.2.4        
# [46] boot_1.3-17         igraph_1.0.1        nlme_3.1-128       

ncpus <- 3
parallel <- "snow"
set.seed(12345)

################################################################################
# Data preparation: nominations
################################################################################

nominations <- read.csv2("nominations.csv", stringsAsFactors = FALSE)
names <- read.csv2("names.csv", stringsAsFactors = FALSE)
chapters <- read.csv2("chapters.csv", stringsAsFactors = FALSE)

# extract names who answered the survey + including external nominations
survey.names <- names$name[names$survey.name == 1]
survey.names.extended <- unique(c(nominations$sender, nominations$receiver, 
    names$name[names$survey.name == 1]))

unique(chapters[chapters$role %in% c("lead author", 
    "coordinating lead author"), ]$person)  # survey population = 493

# create matrix and network object of nomination patterns within survey
nomin.short <- nominations[nominations$sender %in% survey.names & 
    nominations$receiver %in% survey.names, ]
nomin.mat <- matrix(0, nrow = length(survey.names), ncol = length(survey.names))
for (i in 1:nrow(nomin.short)) {
  send <- which(survey.names == nomin.short$sender[i])
  rec <- which(survey.names == nomin.short$receiver[i])
  nomin.mat[send, rec] <- nomin.mat[send, rec] + 1
}
rownames(nomin.mat) <- colnames(nomin.mat) <- survey.names

# create network of nomination patterns including external nominations
nomin.mat.extended <- matrix(0, nrow = length(survey.names.extended), ncol = 
    length(survey.names.extended))
for (i in 1:nrow(nominations)) {
  send <- which(survey.names.extended == nominations$sender[i])
  rec <- which(survey.names.extended == nominations$receiver[i])
  nomin.mat.extended[send, rec] <- nomin.mat.extended[send, rec] + 1
}
rownames(nomin.mat.extended) <- survey.names.extended
colnames(nomin.mat.extended) <- survey.names.extended
extended.in.short <- survey.names.extended %in% survey.names


################################################################################
# Data preparation: institutions
################################################################################

institutions <- read.csv2("institutions.csv", stringsAsFactors = FALSE)
inst.unique <- unique(institutions$institution)
inst.mat <- matrix(0, nrow = nrow(nomin.mat), ncol = length(inst.unique))
rownames(inst.mat) <- survey.names
colnames(inst.mat) <- inst.unique

for (i in 1:nrow(institutions)) {
  row.index <- which(survey.names == institutions$name[i])
  col.index <- which(inst.unique == institutions$institution[i])
  inst.mat[row.index, col.index] <- 1
}

inst.num <- rowSums(inst.mat)  # number of institutional memberships
inst.cooc <- inst.mat %*% t(inst.mat)  # co-occurrence matrix
inst.cooc.sq <- inst.cooc^2  # squared co-occurrence matrix


################################################################################
# Data preparation: chapter collaboration
################################################################################

# create networks of chapters (only survey respondents)
chapters.short <- chapters[chapters$person %in% survey.names, ]
chapterlist <- unique(chapters$chapter)
chapters.mat.bip <- matrix(0, nrow = length(survey.names), ncol = 
    length(chapterlist))
for (i in 1:nrow(chapters.short)) {
  pers <- which(survey.names == chapters.short$person[i])
  chap <- which(chapterlist == chapters.short$chapter[i])
  chapters.mat.bip[pers, chap] <- chapters.mat.bip[pers, chap] + 1
}
chapters.mat.bip <- chapters.mat.bip[, colSums(chapters.mat.bip) > 0]
chapters.mat <- chapters.mat.bip %*% t(chapters.mat.bip)
chapters.mat.squared <- chapters.mat^2  # for ERGM edgecov
chapters.mat.binary <- 1 * (chapters.mat > 0)  # for ERGM edgecov
rownames(chapters.mat.binary) <- survey.names
colnames(chapters.mat.binary) <- survey.names
  
# create networks of chapters (extended to external nominations)
chapters.extended <- chapters[chapters$person %in% survey.names.extended, ]
chapterlist.extended <- unique(chapters.extended$chapter)
chapters.mat.bip.extended <- matrix(0, nrow = length(survey.names.extended), 
    ncol = length(chapterlist.extended))
for (i in 1:nrow(chapters.extended)) {
  pers <- which(survey.names.extended == chapters.extended$person[i])
  chap <- which(chapterlist.extended == chapters.extended$chapter[i])
  chapters.mat.bip.extended[pers, chap] <- chapters.mat.bip.extended[pers, 
      chap] + 1
}
chapters.mat.extended <- chapters.mat.bip.extended %*% 
    t(chapters.mat.bip.extended)

# create networks of chapters (all persons)
chapterlist.all <- unique(chapters$chapter)
persons.all <- unique(chapters$person)
chapters.mat.bip.all <- matrix(0, nrow = length(persons.all), ncol = 
    length(chapterlist.all))
for (i in 1:nrow(chapters)) {
  pers <- which(persons.all == chapters$person[i])
  chap <- which(chapterlist.all == chapters$chapter[i])
  chapters.mat.bip.all[pers, chap] <- chapters.mat.bip.all[pers, chap] + 1
}
chapters.mat.all <- chapters.mat.bip.all %*% t(chapters.mat.bip.all)


################################################################################
# Data preparation: covariates/attributes
################################################################################

# create covariates from names table (only survey respondents)
names.short <- names[names$name %in% survey.names, ]
socsci <- character(length(survey.names))
gender <- socsci
nationality <- socsci
school <- socsci
expertise <- socsci
education <- socsci
country <- socsci
employer <- socsci
authortype <- socsci
leader <- numeric(length(survey.names))
for (i in 1:nrow(names.short)) {
  index <- which(survey.names == names.short$name[i])
  if (names.short$socsci[i] == "") {
    socsci[index] <- NA
  } else {
    socsci[index] <- names.short$socsci[i]
  }
  if (names.short$gender[i] == "") {
    gender[index] <- NA
  } else {
    gender[index] <- names.short$gender[i]
  }
  if (names.short$nationality[i] == "") {
    nationality[index] <- NA
  } else {
    nationality[index] <- names.short$nationality[i]
  }
  if (names.short$school[i] == "") {
    school[index] <- NA
  } else {
    school[index] <- names.short$school[i]
  }
  if (names.short$expertise[i] == "") {
    expertise[index] <- NA
  } else {
    expertise[index] <- names.short$expertise[i]
  }
  if (names.short$education[i] == "") {
    education[index] <- NA
  } else {
    education[index] <- names.short$education[i]
  }
  if (names.short$country[i] == "") {
    country[index] <- NA
  } else {
    country[index] <- names.short$country[i]
  }
  if (names.short$employer[i] == "") {
    employer[index] <- NA
  } else {
    employer[index] <- names.short$employer[i]
  }
  if (names.short$director[index] == "yes" || 
      names.short$assessment_panel[index] == "yes" || 
      names.short$wg_editor[index] == "yes") {
    leader[index] <- 1
  } else {
    leader[index] <- 0
  }
}

chap.indices <- match(names.short$name, chapters$person)
for (i in 1:length(names.short$name)) {
  authortype[i] <- chapters$role[chap.indices[i]]
}
authortype[is.na(authortype)] <- "NA"

attributes.short <- data.frame(name = survey.names, socsci = socsci, gender = 
    gender, nationality = nationality, school = school, expertise = expertise, 
    education = education, country = country, employer = employer, 
    authortype = authortype, stringsAsFactors = FALSE)

# create covariates from names table (extended names from survey)
names.extended <- names[names$name %in% survey.names.extended, ]
socsci <- character(length(survey.names.extended))
gender <- socsci
nationality <- socsci
school <- socsci
expertise <- socsci
education <- socsci
country <- socsci
employer <- socsci
director <- logical(length(survey.names.extended))
wg.editor <- director
asspanel <- director
cla <- director
la <- director
for (i in 1:nrow(names.extended)) {
  index <- which(survey.names.extended == names.extended$name[i])
  if (names.extended$socsci[i] == "") {
    socsci[index] <- NA
  } else {
    socsci[index] <- names.extended$socsci[i]
  }
  if (names.extended$gender[i] == "") {
    gender[index] <- NA
  } else {
    gender[index] <- names.extended$gender[i]
  }
  if (names.extended$nationality[i] == "") {
    nationality[index] <- NA
  } else {
    nationality[index] <- names.extended$nationality[i]
  }
  if (names.extended$school[i] == "") {
    school[index] <- NA
  } else {
    school[index] <- names.extended$school[i]
  }
  if (names.extended$expertise[i] == "") {
    expertise[index] <- NA
  } else {
    expertise[index] <- names.extended$expertise[i]
  }
  if (names.extended$education[i] == "") {
    education[index] <- NA
  } else {
    education[index] <- names.extended$education[i]
  }
  if (names.extended$country[i] == "") {
    country[index] <- NA
  } else {
    country[index] <- names.extended$country[i]
  }
  if (names.extended$employer[i] == "") {
    employer[index] <- NA
  } else {
    employer[index] <- names.extended$employer[i]
  }
  if (names.extended$director[i] == "yes") {
    director[index] <- TRUE
  } else {
    director[index] <- FALSE
  }
  if (names.extended$assessment_panel[i] == "yes") {
    asspanel[index] <- TRUE
  } else {
    asspanel[index] <- FALSE
  }
  if (names.extended$wg_editor[i] == "yes") {
    wg.editor[index] <- TRUE
  } else {
    wg.editor[index] <- FALSE
  }
}

attributes.extended <- data.frame(name = survey.names.extended, 
    socsci = socsci, gender = gender, nationality = nationality, 
    school = school, expertise = expertise, education = education, 
    country = country, employer = employer, stringsAsFactors = FALSE)

# compile CLA and LA roles for cross-table
chap.indices <- match(survey.names.extended, chapters$person)
for (i in 1:length(survey.names.extended)) {
  if (!is.na(chap.indices[i]) && 
      chapters$role[chap.indices[i]] == "coordinating lead author") {
    cla[i] <- TRUE
  } else {
    cla[i] <- FALSE
  }
  if (!is.na(chap.indices[i]) && 
      chapters$role[chap.indices[i]] == "lead author") {
    la[i] <- TRUE
  } else {
    la[i] <- FALSE
  }
}


################################################################################
# Data preparation: create network objects and covariates for ERGM
################################################################################

# create network objects with nodal attributes
nomin.short.nw <- network(nomin.mat, directed = TRUE, bipartite = FALSE)
set.vertex.attribute(nomin.short.nw, "socsci", attributes.short$socsci)
set.vertex.attribute(nomin.short.nw, "gender", attributes.short$gender)
set.vertex.attribute(nomin.short.nw, "nationality", 
    attributes.short$nationality)
set.vertex.attribute(nomin.short.nw, "school", attributes.short$school)
set.vertex.attribute(nomin.short.nw, "expertise", attributes.short$expertise)
set.vertex.attribute(nomin.short.nw, "education", attributes.short$education)
set.vertex.attribute(nomin.short.nw, "country", attributes.short$country)
set.vertex.attribute(nomin.short.nw, "employer", attributes.short$employer)
set.vertex.attribute(nomin.short.nw, "inst.num", inst.num)
set.vertex.attribute(nomin.short.nw, "authortype", authortype)
set.vertex.attribute(nomin.short.nw, "leader", leader)

# create network object with edge attribute for extended matrix
nomin.extended.nw <- network(nomin.mat.extended, directed = TRUE, 
    bipartite = FALSE)
set.vertex.attribute(nomin.extended.nw, "socsci", attributes.extended$socsci)
set.vertex.attribute(nomin.extended.nw, "gender", attributes.extended$gender)
set.vertex.attribute(nomin.extended.nw, "nationality", 
    attributes.extended$nationality)
set.vertex.attribute(nomin.extended.nw, "school", attributes.extended$school)
set.vertex.attribute(nomin.extended.nw, "expertise", 
    attributes.extended$expertise)
set.vertex.attribute(nomin.extended.nw, "education", 
    attributes.extended$education)
set.vertex.attribute(nomin.extended.nw, "country", attributes.extended$country)
set.vertex.attribute(nomin.extended.nw, "employer", 
    attributes.extended$employer)
diag(nomin.mat.extended) <- 0
edgeattrib.mat <- matrix(0, nrow = nrow(nomin.mat.extended), 
    ncol = ncol(nomin.mat.extended))
edgeattrib.vector <- numeric(0)
for (i in 1:nrow(nomin.mat.extended)) {
  for (j in 1:ncol(nomin.mat.extended)) {
    if (nomin.mat.extended[i, j] > 0) {
      if (survey.names.extended[i] %in% survey.names && 
          survey.names.extended[j] %in% survey.names) {
        edgeattrib.vector <- c(edgeattrib.vector, 1)
      } else {
        edgeattrib.vector <- c(edgeattrib.vector, 0)
      }
    }
  }
}
set.edge.attribute(nomin.extended.nw, "respondent", edgeattrib.vector)

# create country-nationality nodematch covariate matrix
country.mat <- matrix(0, nrow = nrow(nomin.mat), ncol = ncol(nomin.mat))
for (i in 1:nrow(country.mat)) {
  for (j in 1:ncol(country.mat)) {
    ci <- attributes.short$country[i]
    cj <- attributes.short$country[j]
    ni <- attributes.short$nationality[i]
    nj <- attributes.short$nationality[j]
    if (!is.na(ci) && !is.na(cj) && ci == cj) {
      country.mat[i, j] <- 1
    }
    if (!is.na(ni) && !is.na(nj) && ni == nj) {
      country.mat[i, j] <- 1
    }
    if (!is.na(ci) && !is.na(nj) && ci == nj) {
      country.mat[i, j] <- 1
    }
    if (!is.na(ni) && !is.na(cj) && ni == cj) {
      country.mat[i, j] <- 1
    }
  }
}

# recode education into science/PhD versus non-science
science <- rep(0, nrow(attributes.short))
for (i in 1:nrow(attributes.short)) {
  edu <- attributes.short$education[i]
  if (edu %in% c("Doctorate/PhD", "M.D./PhD")) {
    science[i] <- 1
  }
}
nomin.short.nw <- set.vertex.attribute(nomin.short.nw, "phd", science)

# reachability centrality in the extended network
reach <- rowSums(reachability(nomin.mat.extended))
names(reach) <- survey.names.extended
reach.extended <- reach
reach <- reach[names(reach) %in% survey.names]
nomin.short.nw <- set.vertex.attribute(nomin.short.nw, "reach", reach)

# indegree outfactor (because those who receive many nominations give this back)
idegree <- degree(nomin.short.nw, cmode = "indegree")
nomin.short.nw <- set.vertex.attribute(nomin.short.nw, "ideg", idegree)

# look at reciprocal dyads
table(nomin.mat * t(nomin.mat))

# network object for chapters
chapters.nw <- network(chapters.mat, bipartite = FALSE, directed = FALSE, 
    ignore.eval = FALSE, names.eval = "weight", loops = FALSE)
set.vertex.attribute(chapters.nw, "socsci", attributes.short$socsci)
set.vertex.attribute(chapters.nw, "gender", attributes.short$gender)
set.vertex.attribute(chapters.nw, "nationality", attributes.short$nationality)
set.vertex.attribute(chapters.nw, "school", attributes.short$school)
set.vertex.attribute(chapters.nw, "expertise", attributes.short$expertise)
set.vertex.attribute(chapters.nw, "education", attributes.short$education)
set.vertex.attribute(chapters.nw, "country", attributes.short$country)
set.vertex.attribute(chapters.nw, "employer", attributes.short$employer)
set.vertex.attribute(chapters.nw, "phd", science)
set.vertex.attribute(chapters.nw, "reach", reach)

# matrix for CLA sender
cla.sender <- matrix(0, nrow = length(authortype), ncol = length(authortype))
for (i in 1:nrow(cla.sender)) {
  for (j in 1:ncol(cla.sender)) {
    if (authortype[i] == "coordinating lead author") {
      cla.sender[i, j] <- 1
    }
  }
}
cla.interaction <- cla.sender * inst.cooc

# matrix for leader sender
leader.sender <- matrix(0, nrow = length(authortype), ncol = length(authortype))
for (i in 1:nrow(leader.sender)) {
  for (j in 1:ncol(leader.sender)) {
    if (leader[i] == 1) {
      leader.sender[i, j] <- 1
    }
  }
}
leader.interaction <- leader.sender * inst.cooc



################################################################################
# Network diagrams
################################################################################

coords.core <- plot(nomin.short.nw)
dev.off()

# network diagram (Figure 1): all people in the survey
pdf("output/figure_1.pdf", width = 7, height = 7)
par(mar = c(0, 0, 1, 0))

ext.col <- extended.in.short
ext.col[extended.in.short == TRUE] <- "white"
ext.col[extended.in.short == FALSE] <- "black"

bothresp <- extended.in.short %*% t(extended.in.short)
bothresp[bothresp == 1] <- 8  # gray
bothresp[bothresp == 0] <- 1  # black

plot(nomin.extended.nw, edge.col = bothresp, arrowhead.cex = 0.5, 
    edge.lwd = 1.0, vertex.cex = 0.6, vertex.col = ext.col
)

dev.off()

# network diagram: only respondents; elite memberships
pdf("output/figure_2.pdf", width = 7, height = 7)
plot(network(inst.mat, bipartite = TRUE), 
     vertex.col = c(rep("white", nrow(inst.mat)), rep("gray", ncol(inst.mat))), 
     vertex.cex = c(rep(0.7, nrow(inst.mat)), rep(1.0, ncol(inst.mat))), 
     displayisolates = TRUE)
dev.off()

# network diagram: only respondents; joint organizational memberships
pdf("output/figure_3.pdf", width = 7, height = 7)
mat.col <- inst.cooc
mat.col[inst.cooc > 2] <- "#000000"
mat.col[inst.cooc == 2] <- "#8B8B8B"
mat.col[inst.cooc == 1] <- "#BFBFBF"
mat.col[inst.cooc == 0] <- "#E6E6E6"
plot(nomin.short.nw, edge.col = mat.col, vertex.col = "white", 
    displayisolates = TRUE, coord = coords.core, 
    arrowhead.cex = 0.5, edge.lwd = 1.0, vertex.cex = 0.7)
dev.off()


################################################################################
# Outdegree centrality of reachability matrix and cross-table
################################################################################

ocrm <- rowSums(reachability(nomin.mat.extended))
names(ocrm) <- survey.names.extended
ocrm <- sort(ocrm, decreasing = TRUE)[1:30]
write.csv2(ocrm, file = "output/reachability.csv")

crosstable <- matrix(NA, nrow = 6, ncol = 5)
rownames(crosstable) <- c("All members", 
                          "Coordinating Lead Authors", 
                          "Lead Authors", 
                          "Director", 
                          "Assessment Panel", 
                          "Working Group Editors")
colnames(crosstable) <- c("N", 
                          "Nomination outdegree", 
                          "SD", 
                          "Reachability outdegree", 
                          "SD")

crosstable[1, 1] <- length(survey.names.extended)
crosstable[1, 2] <- mean(degree(nomin.extended.nw, cmode = "outdegree"))
crosstable[1, 3] <- sd(degree(nomin.extended.nw, cmode = "outdegree"))
crosstable[1, 4] <- mean(reach.extended)
crosstable[1, 5] <- sd(reach.extended)

crosstable[2, 1] <- length(survey.names.extended[cla])
crosstable[2, 2] <- mean(degree(nomin.extended.nw, cmode = "outdegree")[cla])
crosstable[2, 3] <- sd(degree(nomin.extended.nw, cmode = "outdegree")[cla])
crosstable[2, 4] <- mean(reach.extended[cla])
crosstable[2, 5] <- sd(reach.extended[cla])

crosstable[3, 1] <- length(survey.names.extended[la])
crosstable[3, 2] <- mean(degree(nomin.extended.nw, cmode = "outdegree")[la])
crosstable[3, 3] <- sd(degree(nomin.extended.nw, cmode = "outdegree")[la])
crosstable[3, 4] <- mean(reach.extended[la])
crosstable[3, 5] <- sd(reach.extended[la])

crosstable[4, 1] <- length(survey.names.extended[director])
crosstable[4, 2] <- mean(degree(nomin.extended.nw, cmode = "outdegree")[director])
crosstable[4, 3] <- 0
crosstable[4, 4] <- mean(reach.extended[director])
crosstable[4, 5] <- 0

crosstable[5, 1] <- length(survey.names.extended[asspanel])
crosstable[5, 2] <- mean(degree(nomin.extended.nw, cmode = "outdegree")[asspanel])
crosstable[5, 3] <- sd(degree(nomin.extended.nw, cmode = "outdegree")[asspanel])
crosstable[5, 4] <- mean(reach.extended[asspanel])
crosstable[5, 5] <- sd(reach.extended[asspanel])

crosstable[6, 1] <- length(survey.names.extended[wg.editor])
crosstable[6, 2] <- mean(degree(nomin.extended.nw, cmode = "outdegree")[wg.editor])
crosstable[6, 3] <- sd(degree(nomin.extended.nw, cmode = "outdegree")[wg.editor])
crosstable[6, 4] <- mean(reach.extended[wg.editor])
crosstable[6, 5] <- sd(reach.extended[wg.editor])

sink("output/crosstable.tex")
xtable(crosstable, 
       caption = "Nomination outdegree and reachability outdegree of leading members", 
       label = "crosstable", 
       align = "lrrrrr")
sink()


################################################################################
# ERGM estimation and GOF assessment
################################################################################

# ERGM: only intercept; null model
model1 <- ergm(nomin.short.nw ~ 
    edges, 
    control = control.ergm(MCMC.interval = 2000, MCMC.burnin = 30000, 
    MCMC.samplesize = 15000)
)
summary(model1)

# ERGM: only intercept and covariates; no endogenous processes
model2 <- ergm(nomin.short.nw ~ 
    edges + 
    nodematch("nationality") + 
    nodematch("employer") + 
    nodeofactor("gender") + 
    nodeifactor("gender") + 
    nodemix("gender", base = -4) + 
    nodeicov("inst.num") + 
    nodeocov("inst.num") + 
    edgecov(inst.cooc) + 
    edgecov(inst.cooc.sq) + 
    nodematch("expertise") + 
    edgecov(chapters.mat.binary) + 
    nodematch("education") + 
    nodeifactor("phd") + 
    nodeofactor("phd") + 
    nodeifactor("socsci") + 
    nodeofactor("socsci") + 
    nodematch("socsci") + 
    nodeofactor("authortype", base = -2) + 
    edgecov(cla.interaction) + 
    nodeofactor("leader") + 
    edgecov(leader.interaction), 
    control = control.ergm(MCMC.interval = 2000, MCMC.burnin = 30000, 
    MCMC.samplesize = 15000)
)
summary(model2)

# ERGM: full specification
model3 <- ergm(nomin.short.nw ~ 
    edges + 
    istar(2) + 
    ostar(2:3) + 
    twopath + 
    isolates + 
    nodematch("nationality") + 
    nodematch("employer") + 
    nodeofactor("gender") + 
    nodeifactor("gender") + 
    nodemix("gender", base = -4) + 
    nodeicov("inst.num") + 
    nodeocov("inst.num") + 
    edgecov(inst.cooc) + 
    edgecov(inst.cooc.sq) + 
    nodematch("expertise") + 
    edgecov(chapters.mat.binary) + 
    nodematch("education") + 
    nodeifactor("phd") + 
    nodeofactor("phd") + 
    nodeifactor("socsci") + 
    nodeofactor("socsci") + 
    nodematch("socsci") + 
    nodeofactor("authortype", base = -2) + 
    edgecov(cla.interaction) + 
    nodeofactor("leader") + 
    edgecov(leader.interaction)
    , control = control.ergm(MCMC.interval = 2000, MCMC.burnin = 30000, 
    MCMC.samplesize = 15000)
)
summary(model3)

# ERGM: without elite club variables
model4 <- ergm(nomin.short.nw ~ 
    edges + 
    istar(2) + 
    ostar(2:3) + 
    twopath + 
    isolates + 
    nodematch("nationality") + 
    nodematch("employer") + 
    nodeofactor("gender") + 
    nodeifactor("gender") + 
    nodemix("gender", base = -4) + 
    #nodeicov("inst.num") + 
    #nodeocov("inst.num") + 
    #edgecov(inst.cooc) + 
    #edgecov(inst.cooc.sq) + 
    nodematch("expertise") + 
    edgecov(chapters.mat.binary) + 
    nodematch("education") + 
    nodeifactor("phd") + 
    nodeofactor("phd") + 
    nodeifactor("socsci") + 
    nodeofactor("socsci") + 
    nodematch("socsci") + 
    nodeofactor("authortype", base = -2) + 
    nodeofactor("leader")
    , control = control.ergm(MCMC.interval = 2000, MCMC.burnin = 30000, 
    MCMC.samplesize = 15000)
)
summary(model4)


################################################################################
# Degeneracy check and goodness-of-fit assessment
################################################################################

pdf("output/mcmc-diagnostics.pdf", width = 10, height = 13)
par(mfrow = c(7, 4), mar = c(2, 2, 2, 2))
plot(model3$sample, auto.layout = FALSE, ask = FALSE)
dev.off()

gof3 <- gof(model3, nsim = 1000, MCMC.interval = 10000, MCMC.burnin = 30000,
    ncpus = ncpus, parallel = parallel, statistics = c(dsp, esp, ideg, odeg, 
    geodesic, triad.directed, rocpr), roc = FALSE)
gof4 <- gof(model4, nsim = 1000, MCMC.interval = 10000, MCMC.burnin = 30000,
    ncpus = ncpus, parallel = parallel, statistics = c(rocpr), roc = FALSE)
gof.dyadindep <- gof(model2, nsim = 1000, MCMC.interval = 10000, 
    MCMC.burnin = 30000, ncpus = ncpus, parallel = parallel, 
    statistics = c(dsp, esp, ideg, odeg, geodesic, triad.directed, rocpr), 
    roc = FALSE)

gof3boxplots <- gof3[1:6]
class(gof3boxplots) <- "gof"
pdf("output/gof-boxplot-full.pdf", width = 10, height = 6)
plot(gof3boxplots, transform = log1p)
dev.off()

gof.di.bp <- gof.dyadindep[1:6]
class(gof.di.bp) <- "gof"
pdf("output/gof-boxplot-dyadindep.pdf", width = 10, height = 6)
par(mfrow = c(2, 3))
plot(gof.di.bp, transform = log1p)
dev.off()

pdf("output/figure_4.pdf", width = 4, height = 4)
plot(gof3[[7]], rgraph = TRUE, col = "#000000", random.col = "#E6E6E6")
plot(gof4, add = TRUE, pr.add = TRUE, col = "#A7A7A7")
legend(0.1, 0.97, col = c("#000000", "#A7A7A7", "#E6E6E6"), 
    legend = c("Full model", "Without memberships", "Null model"), 
    lty = "solid", lwd = 3)
dev.off()


################################################################################
# Regression tables
################################################################################

map <- list("nodeicov.inst.num" = "Institutional memberships (receiver)", 
            "nodeocov.inst.num" = "Institutional memberships (sender)", 
            "edgecov.inst.cooc" = "Institutional co-memberships", 
            "edgecov.inst.cooc.sq" = "Institutional co-memberships$^2$", 
            "nodematch.nationality" = "Same nationality", 
            "nodematch.employer" = "Same employer/university affiliation", 
            "nodeofactor.gender.Male" = "Sender male", 
            "nodeifactor.gender.Male" = "Receiver male", 
            "mix.gender.Male.Male" = "Sender male, receiver male", 
            "nodematch.expertise" = "Same area of expertise", 
            "edgecov.chapters.mat.binary" = "Joint chapter(s) in the report", 
            "nodematch.education" = "Same type and level of degree", 
            "nodeifactor.phd.1" = "Receiver has a PhD or MD", 
            "nodeofactor.phd.1" = "Sender has a PhD or MD", 
            "nodeifactor.socsci.yes" = "Receiver is a social scientist", 
            "nodeofactor.socsci.yes" = "Sender is a social scientist", 
            "nodematch.socsci" = "Both are social/natural scientists", 
            "nodeofactor.authortype.coordinating lead author" = "Sender is a CLA", 
            "edgecov.cla.interaction" = "Sender CLA $\\times$ Institutional co-memberships", 
            "nodeofactor.leader.1" = "Sender is a Leader", 
            "edgecov.leader.interaction" = "Leader $\\times$ Institutional co-memberships", 
            "edges" = "Edges", 
            "istar2" = "Two-stars (incoming)", 
            "ostar2" = "Two-stars (outgoing)", 
            "ostar3" = "Three-stars (outgoing)", 
            "twopath" = "Two-paths", 
            "isolates" = "Isolates"
            )

screenreg(model3, single.row = TRUE, custom.model.names = "ERGM", 
    groups = list("Institutional elite memberships" = 1:4, 
    "Exogenous controls" = 5:21, "Endogenous dependencies" = 22:27), 
    custom.coef.map = map, include.aic = FALSE, include.bic = FALSE, 
    include.loglik = FALSE, file = "output/table.txt"
)

texreg(model3, single.row = TRUE, custom.model.names = "ERGM", 
    groups = list("Institutional elite memberships" = 1:4, 
    "Exogenous controls" = 5:21, "Endogenous dependencies" = 22:27), 
    custom.coef.map = map, include.aic = FALSE, include.bic = FALSE, 
    include.loglik = FALSE, file = "output/table.tex", booktabs = TRUE, 
    dcolumn = TRUE, use.packages = FALSE, caption = "ERGM results"
)

htmlreg(model3, single.row = TRUE, custom.model.names = "ERGM", 
    groups = list("Institutional elite memberships" = 1:4, 
    "Exogenous controls" = 5:21, "Endogenous dependencies" = 22:27), 
    custom.coef.map = map, include.aic = FALSE, include.bic = FALSE, 
    include.loglik = FALSE, file = "output/table.html", 
    caption = "ERGM results"
)

# save everything to a file for later use
save(list = ls(), file = "output/workspace.RData")


################################################################################
# Marginal effects plot
################################################################################

ep <- edgeprob(model3)

mp <- marginalplot(model3, 
                   var1 = "edgecov.inst.cooc", 
                   var2 = "edgecov.inst.cooc", 
                   inter = "edgecov.inst.cooc.sq", 
                   xlab = "Institutional co-memberships (conditioning variable)", 
                   ylab = "Institutional co-memberships (coefficient)") + 
  theme_bw() + 
  labs(title = "Marginal effects for squared model term")

tab <- data.frame(table(ep$`edgecov.inst.cooc[[i]]`))
fr <- ggplot(tab[tab$Var1 != 0, ], aes(Var1, Freq)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  xlab("Institutional co-memberships") + 
  ylab("Frequency") + 
  labs(title = "Frequency and range of institutional co-memberships (> 0)")

pp <- ggplot(ep[ep$`edgecov.inst.cooc[[i]]` %in% 0:4, ], 
       aes(x = `edgecov.inst.cooc[[i]]`, y = probability)) + 
  theme_bw() + 
  stat_summary(geom = "ribbon", 
               fun.data = mean_cl_normal, 
               fun.args = (conf.int=1), 
               fill = "lightgray") + 
  stat_summary(geom = "line", fun.y = mean) + 
  ylab("Predicted probability") +
  xlab("Institutional co-memberships") + 
  labs(title = "Predicted probabilities for highest-frequency values")

require("gridExtra")
pdf("output/interpretation.pdf", width = 18, height = 6)
grid.arrange(mp, fr, pp, ncol = 3)
dev.off()

pdf("output/marginaleffect.pdf")
plot(mp)
dev.off()

pdf("output/predictedprob.pdf")
plot(pp)
dev.off()

pdf("output/instfreq.pdf")
plot(fr)
dev.off()
