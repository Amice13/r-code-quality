# last change: 2017-05-18


# ==============================================================================
# Prepare workspace
# ==============================================================================

library("network")
library("sna")
library("ergm")
library("xergm")
library("texreg")
library("inline")
library("Rcpp")
library("ggplot2")
library("reshape2")

sessionInfo()
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
#  [1] reshape2_1.4.1       Rcpp_0.12.3          inline_0.3.14       
#  [4] texreg_1.36.23       xergm_1.8.2          GERGM_0.11.2        
#  [7] rem_1.1.2            tnam_1.6.5           btergm_1.9.0        
# [10] ggplot2_2.0.0        xergm.common_1.7.7   ergm_3.6.0          
# [13] statnet.common_3.3.0 sna_2.3-2            network_1.13.0      
# 
# loaded via a namespace (and not attached):
#  [1] deSolve_1.12         gtools_3.5.0         lpSolve_5.6.13      
#  [4] ergm.count_3.2.0     splines_3.3.0        lattice_0.20-33     
#  [7] mstate_0.2.8         colorspace_1.2-6     flexsurv_0.7        
# [10] stats4_3.3.0         tergm_3.4.0          mgcv_1.8-12         
# [13] survival_2.39-4      nloptr_1.0.4         RColorBrewer_1.1-2  
# [16] muhaz_1.2.6          speedglm_0.3-1       trust_0.1-7         
# [19] plyr_1.8.3           stringr_1.0.0        robustbase_0.92-5   
# [22] munsell_0.4.2        gtable_0.1.2         caTools_1.17.1      
# [25] mvtnorm_1.0-5        coda_0.18-1          permute_0.8-4       
# [28] parallel_3.3.0       DEoptimR_1.0-4       KernSmooth_2.23-15  
# [31] ROCR_1.0-7           networkDynamic_0.9.0 statnet_2016.4      
# [34] scales_0.3.0         gdata_2.17.0         vegan_2.3-1         
# [37] RcppParallel_4.3.20  lme4_1.1-10          gplots_2.17.0       
# [40] stringi_1.0-1        grid_3.3.0           quadprog_1.5-5      
# [43] tools_3.3.0          bitops_1.0-6         magrittr_1.5        
# [46] RSiena_1.1-232       cluster_2.0.4        MASS_7.3-45         
# [49] Matrix_1.2-6         minqa_1.2.4          boot_1.3-17         
# [52] igraph_1.0.1         nlme_3.1-128

burnin <- 10000      # MCMC burnin
sampsize <- 10000    # MCMC sample size
maxit <- 200         # number of MCMC MLE iterations
nsim <- 1000         # number of simulated networks for the GOF assessment
cores <- 3           # number of computing cores for parallel processing
seed <- 12345        # random seed for exact reproducibility
set.seed(seed)


# ==============================================================================
# Read CSV files and transform/manage data
# ==============================================================================

# leadership network
leader <- as.matrix(read.csv("Coalition_Leadership.csv", header = TRUE, 
    row.names = 1, stringsAsFactors = FALSE))

# coalition non-membership matrix
nonmem <- as.matrix(read.csv("Coalition_Nonmembership.csv", header = TRUE, 
    row.names = 1, stringsAsFactors = FALSE))
mem <- (nonmem * -1) + 1  # membership matrix

# several nodal attributes
attrib <- read.csv("Coalition_Node_Attributes.csv", header = TRUE)

# communication network: any kind of communication
comm.any <- as.matrix(read.table("Communication_Any.csv", 
    stringsAsFactors = FALSE, sep = ",", header = TRUE, row.names = 1))

# communication network: occasional communication
comm.occ <- as.matrix(read.table("Communication_Occasional.csv", 
    stringsAsFactors = FALSE, sep = ",", header = TRUE, row.names = 1))

# communication network: regular communication
comm.reg <- as.matrix(read.table("Communication_Regular.csv", 
    stringsAsFactors = FALSE, sep = ",", header = TRUE, row.names = 1))

# attributes contain both groups and coalitions; they need to be separated
attrib.grp <- attrib[1:nrow(mem), ]
attrib.coal <- attrib[(nrow(mem) + 1): nrow(attrib), ]

# who founded which coalition?
founded <- as.matrix(read.csv("Founded.csv", header = TRUE, row.names = 1, 
    stringsAsFactors = FALSE))


# ==============================================================================
# Create new model terms for multiplexity and diversity
# ==============================================================================


# impute NA values in communication
for (i in 1:nrow(comm.any)) {
  for (j in 1:ncol(comm.any)) {
    if (is.na(comm.any[i, j]) && !is.na(comm.any[j, i])) {
      comm.any[i, j] <- comm.any[j, i]  # impute from reciprocal dyad
    } else if (is.na(comm.any[j, i])) {
      comm.any[i, j] <- 0  # zero-impute if reciprocal dyad also NA
    }
    if (is.na(comm.reg[i, j]) && !is.na(comm.reg[j, i])) {
      comm.reg[i, j] <- comm.reg[j, i]
    } else if (is.na(comm.reg[j, i])) {
      comm.reg[i, j] <- 0
    }
    if (is.na(comm.occ[i, j]) && !is.na(comm.occ[j, i])) {
      comm.occ[i, j] <- comm.occ[j, i]
    } else if (is.na(comm.occ[j, i])) {
      comm.occ[i, j] <- 0
    }
  }
}


# H1: network embeddedness

# compute co-occurrence of coalition membership among coalition members
cpp.comember.strong <- cxxfunction(signature(mat = "matrix"), plugin = "Rcpp", 
    body = '
  IntegerMatrix mem = as<IntegerMatrix>(mat);
  int rows = mem.nrow();
  int cols = mem.ncol();
  Rcpp::NumericMatrix comemb = NumericMatrix(rows, cols);
  int realized;
  int possible;
  for (int i = 0; i < cols; i++) {
    for (int j = 0; j < rows; j++) {
      realized = 0;
      possible = 0;
      for (int k = 0; k < rows; k++) {
        for (int l = 0; l < cols; l++) {
          if (j != k && i != l && mem(j, i) == 1 && mem(k, i) == 1) {
            possible++;
            if (mem(k, l) == 1 && mem(j, l) == 1) {
              realized++;
            }
          }
        }
      }
      //std::cout << i << " " << j << " " << realized << " " << possible << "\\n";
      if (possible == 0.0) {
        comemb(j, i) = 0.0;
      } else {
        comemb(j, i) = double(realized) / double(possible);
      }
    }
  }
  return(wrap(comemb));
')

Network_Embeddedness_Strong <- cpp.comember.strong(mem)

# compute share of other members with whom i has at least one co-membership
cpp.comember.weak <- cxxfunction(signature(mat = "matrix"), plugin = "Rcpp", 
    body = '
  IntegerMatrix mem = as<IntegerMatrix>(mat);
  int rows = mem.nrow();
  int cols = mem.ncol();
  Rcpp::NumericMatrix comemb = NumericMatrix(rows, cols);
  int realized;
  int nummembers;
  for (int i = 0; i < cols; i++) {
    nummembers = 0;
    for (int k = 0; k < rows; k++) {
      if (mem(k, i) == 1) {
        nummembers++;
      }
    }
    for (int j = 0; j < rows; j++) {
      realized = 0;
      bool kiscomem;
      for (int k = 0; k < rows; k++) {
        kiscomem = false;
        for (int l = 0; l < cols; l++) {
          if (j != k && i != l && mem(j, i) == 1 && mem(k, i) == 1) {
            if (mem(k, l) == 1 && mem(j, l) == 1) {
              kiscomem = true;
              realized++;
              break;
            }
          }
        }
      }
      //std::cout << i << " " << j << " " << realized << " " << nummembers << "\\n";
      if (nummembers < 2.0) {
        comemb(j, i) = 0.0;
      } else {
        comemb(j, i) = double(realized) / double(nummembers - 1);
      }
    }
  }
  return(wrap(comemb));
')

Network_Embeddedness_Weak <- cpp.comember.weak(mem)


# communication density of others in current coalition
cpp.commdensity <- cxxfunction(signature(mat = "matrix", comm = "matrix"), 
    plugin = "Rcpp", body = '
  IntegerMatrix mem = as<IntegerMatrix>(mat);
  IntegerMatrix com = as<IntegerMatrix>(comm);
  int rows = mem.nrow();
  int cols = mem.ncol();
  Rcpp::NumericMatrix cd = NumericMatrix(rows, cols);
  int realized;
  int possible;
  for (int i = 0; i < cols; i++) {
    for (int j = 0; j < rows; j++) {
      realized = 0;
      possible = 0;
      for (int k = 0; k < rows; k++) {
        if (j != k && mem(j, i) == 1 && mem(k, i) == 1) {
          possible++;
          if (com(j, k) == 1) {
            realized++;
          }
        }
      }
      // std::cout << realized << " " << possible << "\\n";
      if (possible == 0.0) {
        cd(j, i) = 0.0;
      } else {
        cd(j, i) = double(realized) / double(possible);
      }
    }
  }
  return(wrap(cd));
')

commdensity <- cpp.commdensity(mem, comm.any)


# H2: diversity

cpp.diversity <- cxxfunction(signature(mat = "matrix", attribute = "integer"), 
    plugin = "Rcpp", body = '
  IntegerMatrix mem = as<IntegerMatrix>(mat);
  IntegerVector at = as<IntegerVector>(attribute);
  int rows = mem.nrow();
  int cols = mem.ncol();
  Rcpp::NumericMatrix diversity = NumericMatrix(rows, cols);
  for (int i = 0; i < cols; i++) {
    for (int j = 0; j < rows; j++) {
      double sum = 0.0;
      int counter = 0;
      for (int k = 0; k < rows; k++) {
        if (mem(j, i) == 1 && mem(k, i) == 1) {
          counter++;
          sum = sum + at[k];
        }
      }
      double mean = sum / counter;
      double sqsum = 0.0;
      for (int k = 0; k < rows; k++) {
        if (mem(j, i) == 1 && mem(k, i) == 1) {
          sqsum = sqsum + ((mean - at[k]) * (mean - at[k]));
        }
      }
      if (counter == 0) {
        diversity(j, i) = 0;
      } else {
        diversity(j, i) = sqrt(sqsum / counter);
      }
    }
  }
  return(wrap(diversity));
')

# diversity measures
Partisan_Diversity <- cpp.diversity(mat = mem, 
    attribute = attrib.grp$Conservative_Lean_of_Organization)
diversity.lobspend <- cpp.diversity(mat = mem, 
    attribute = attrib.grp$Lobbying_Spending_by_Organization)
diversity.infrep <- cpp.diversity(mat = mem, 
    attribute = attrib.grp$Organizations_Influence_Reputation)
diversity.outshealth <- cpp.diversity(mat = mem, 
    attribute = attrib.grp$Organization_Identified_Primarily_Outside_Health)
diversity.citadv <- cpp.diversity(mat = mem, 
    attribute = attrib.grp$Organization_is_Citizens_Advocacy_Organization)
diversity.age <- cpp.diversity(mat = mem, 
    attribute = attrib.grp$Years_Since_Founding_of_Organization_Coalition)

# interaction terms comembership * diversity
Diversity_X_Embeddedness_Strong <- 
    Network_Embeddedness_Strong * Partisan_Diversity
Diversity_X_Embeddedness_Weak <- 
    Network_Embeddedness_Weak * Partisan_Diversity
commdensity.diversity <- commdensity * Partisan_Diversity

# dependent variable and structural zeros and ones
leader <- network(leader, directed = FALSE, bipartite = TRUE)  # DV
# model 1: non-members are structural zeros:
nonmem <- network(nonmem, directed = FALSE, bipartite = TRUE)
nonmem2 <- as.matrix(nonmem)
nonmem2[founded == 1] <- 1  # model 2, non-members and founders = struct. zeros
sum(mem * founded)  # 136 additional structural zeros
sum((mem * founded) * as.matrix(leader))  # 102 leadership ties are removed
# model 3: in addition to nonmem as structural zeros, model founders who are 
# also leaders as structural ones, i.e., constrain founders to be leaders:
founderleader <- as.matrix(leader) * founded


# ==============================================================================
# Create model terms for control variables
# ==============================================================================

# issue controversy
Controversial <- matrix(rep(attrib.coal$Issue_is_Highly_Controversial, 
    nrow(as.matrix(leader))), nrow = nrow(as.matrix(leader)), byrow = TRUE)
Diversity_X_Controversial <- Partisan_Diversity * Controversial

# coalition visibility
Visibility <- matrix(rep(attrib.coal$Coalition_in_Public, 
    nrow(as.matrix(leader))), nrow = nrow(as.matrix(leader)), byrow = TRUE)
Diversity_X_Visibility <- Partisan_Diversity * Visibility

# three-way interaction: diversity x embeddedness within or across parties
cpp.comember.strong.party <- cxxfunction(signature(mat = "matrix", 
    cl = "IntegerVector", cross = "bool"), plugin = "Rcpp", body = '
  IntegerMatrix mem = as<IntegerMatrix>(mat);
  IntegerVector conslean = as<IntegerVector>(cl);
  bool crossparty = as<bool>(cross);
  int rows = mem.nrow();
  int cols = mem.ncol();
  Rcpp::NumericMatrix comemb = NumericMatrix(rows, cols);
  int realized;
  int possible;
  for (int i = 0; i < cols; i++) {
    for (int j = 0; j < rows; j++) {
      realized = 0;
      possible = 0;
      for (int k = 0; k < rows; k++) {
        if ((crossparty == false && (conslean(j) < 0 && conslean(k) < 0) || 
                                    (conslean(j) >= 0 && conslean(k) >= 0)) ||
            (crossparty == true && (conslean(j) < 0 && conslean(k) >= 0) || 
                                    (conslean(j) >= 0 && conslean(k) < 0))) {
          for (int l = 0; l < cols; l++) {
            if (j != k && i != l && mem(j, i) == 1 && mem(k, i) == 1) {
              possible++;
              if (mem(k, l) == 1 && mem(j, l) == 1) {
                realized++;
              }
            }
          }
        }
      }
      // std::cout << realized << " " << possible << "\\n";
      if (possible == 0.0) {
        comemb(j, i) = 0.0;
      } else {
        comemb(j, i) = double(realized) / double(possible);
      }
    }
  }
  return(wrap(comemb));
')

cpp.comember.weak.party <- cxxfunction(signature(mat = "matrix", 
    cl = "IntegerVector", cross = "bool"), plugin = "Rcpp", body = '
  IntegerMatrix mem = as<IntegerMatrix>(mat);
  IntegerVector conslean = as<IntegerVector>(cl);
  bool crossparty = as<bool>(cross);
  int rows = mem.nrow();
  int cols = mem.ncol();
  Rcpp::NumericMatrix comemb = NumericMatrix(rows, cols);
  int realized;
  int nummembers;
  for (int i = 0; i < cols; i++) {
    nummembers = 0;
    for (int k = 0; k < rows; k++) {
      if (mem(k, i) == 1) {
        nummembers++;
      }
    }
    for (int j = 0; j < rows; j++) {
      realized = 0;
      bool kiscomem;
      for (int k = 0; k < rows; k++) {
        kiscomem = false;
        if ((crossparty == false && (conslean(j) < 0 && conslean(k) < 0) || 
                                    (conslean(j) >= 0 && conslean(k) >= 0)) ||
            (crossparty == true && (conslean(j) < 0 && conslean(k) >= 0) || 
                                    (conslean(j) >= 0 && conslean(k) < 0))) {
          for (int l = 0; l < cols; l++) {
            if (j != k && i != l && mem(j, i) == 1 && mem(k, i) == 1) {
              if (mem(k, l) == 1 && mem(j, l) == 1) {
                kiscomem = true;
                realized++;
                break;
              }
            }
          }
        }
      }
      //std::cout << i << " " << j << " " << realized << " " << nummembers << "\\n";
      if (nummembers < 2.0) {
        comemb(j, i) = 0.0;
      } else {
        comemb(j, i) = double(realized) / double(nummembers - 1);
      }
    }
  }
  return(wrap(comemb));
')

Network_Embeddedness_Weak_SameParty <- cpp.comember.weak.party(mem, 
    attrib.grp$Conservative_Lean_of_Organization_Coalition, FALSE)
Network_Embeddedness_Weak_CrossParty <- cpp.comember.weak.party(mem, 
    attrib.grp$Conservative_Lean_of_Organization_Coalition, TRUE)
Network_Embeddedness_Strong_SameParty <- cpp.comember.strong.party(mem, 
    attrib.grp$Conservative_Lean_of_Organization_Coalition, FALSE)
Network_Embeddedness_Strong_CrossParty <- cpp.comember.strong.party(mem, 
    attrib.grp$Conservative_Lean_of_Organization_Coalition, TRUE)
Diversity_X_Embeddedness_Weak_SameParty <- 
    Partisan_Diversity * Network_Embeddedness_Weak_SameParty
Diversity_X_Embeddedness_CrossParty <- 
    Partisan_Diversity * Network_Embeddedness_Weak_CrossParty

# Number_of_Coalition_Memberships: outdegree centrality of groups in the 
# membership network
rs <- rowSums(mem)
Number_of_Coalition_Memberships <- matrix(NA, nrow = nrow(mem), 
    ncol = ncol(mem))
for (i in 1:nrow(Number_of_Coalition_Memberships)) {
  Number_of_Coalition_Memberships[i, ] <- rs[i]
}

# Coalition_Size: indegree centrality of coalitions in the membership network
cs <- colSums(mem)
Coalition_Size <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
for (i in 1:ncol(Coalition_Size)) {
  Coalition_Size[, i] <- cs[i]
}

# commpart.indeg: indegree centrality in the communication network; count number
# of comm. partners in same coal. and divide by num. of coal. members excl. ego
# (notes: NAs need to be replaced first; the matrix is transposed, i.e., 
# communication flows from columns to rows, so this needs to be transposed)
for (i in 1:nrow(comm.any)) {
  for (j in 1:ncol(comm.any)) {
    if (is.na(comm.any[i, j]) && !is.na(comm.any[j, i])) {
      comm.any[i, j] <- comm.any[j, i]  # impute from reciprocal dyad
    } else if (is.na(comm.any[j, i])) {
      comm.any[i, j] <- 0  # zero-impute if reciprocal dyad also NA
    }
    if (is.na(comm.reg[i, j]) && !is.na(comm.reg[j, i])) {
      comm.reg[i, j] <- comm.reg[j, i]
    } else if (is.na(comm.reg[j, i])) {
      comm.reg[i, j] <- 0
    }
    if (is.na(comm.occ[i, j]) && !is.na(comm.occ[j, i])) {
      comm.occ[i, j] <- comm.occ[j, i]
    } else if (is.na(comm.occ[j, i])) {
      comm.occ[i, j] <- 0
    }
  }
}
commpart.outdeg.any <- matrix(0, nrow = nrow(mem), ncol = ncol(mem))  # any com.
commpart.indeg.any <- commpart.outdeg.any  # indegree, any type of communication
commpart.outdeg.reg <- commpart.outdeg.any # outdegree, regular communication
commpart.indeg.reg <- commpart.outdeg.any  # indegree, regular communication
for (i in 1:nrow(mem)) {
  for (j in 1:ncol(mem)) {
    if (mem[i, j] == 1) {
      members <- which(mem[, j] == 1)  # all members of this coalition
      
      # any communication
      comm.subset <- comm.any[members, members]  # comm. partners in this coal.
      
      groupi <- which(rownames(comm.subset) == rownames(mem)[i])
      indeg.coal <- sum(comm.subset[groupi, ])  # indegree of group i in coal.
      commpart.indeg.any[i, j] <- indeg.coal / (sum(mem[, j]) - 1)
      
      outdeg.coal <- sum(comm.subset[, groupi])  # outdegree of group i in coal.
      commpart.outdeg.any[i, j] <- outdeg.coal / (sum(mem[, j]) - 1)
      
      # regular communication
      comm.subset <- comm.reg[members, members]  # comm. partners in this coal.
      
      groupi <- which(rownames(comm.subset) == rownames(mem)[i])
      indeg.coal <- sum(comm.subset[groupi, ])  # indegree of group i in coal.
      commpart.indeg.reg[i, j] <- indeg.coal / (sum(mem[, j]) - 1)
      
      outdeg.coal <- sum(comm.subset[, groupi])  # outdegree of group i in coal.
      commpart.outdeg.reg[i, j] <- outdeg.coal / (sum(mem[, j]) - 1)
    }
  }
}

# Interest_Group_Coalition_Partisan_Differential: absolute difference in 
# conservatism group vs. coalition
# set the node attribute for re-use with the absdiff term
set.vertex.attribute(leader, "Partisanship", 
    attrib$Conservative_Lean_of_Organization_Coalition)
Interest_Group_Coalition_Partisan_Differential <- matrix(NA, 
    nrow = nrow(as.matrix(leader)), ncol = ncol(as.matrix(leader)))
cl <- attrib$Conservative_Lean_of_Organization_Coalition
cl.ig <- cl[1:nrow(as.matrix(leader))]
cl.coal <- cl[(nrow(as.matrix(leader)) + 1):length(cl)]
for (i in 1:length(cl.ig)) {
  for (j in 1:length(cl.coal)) {
    Interest_Group_Coalition_Partisan_Differential[i, j] <- abs(cl.ig[i] - 
        cl.coal[j])
  }
}

# Interest_Group_Partisanship: conservatism of the group
Interest_Group_Partisanship <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
for (i in 1:ncol(mem)) {
  Interest_Group_Partisanship[, i] <- 
      attrib.grp$Conservative_Lean_of_Organization_Coalition
}

# Coalition_Partisanship: conservatism of the coalition
Coalition_Partisanship <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
for (i in 1:nrow(mem)) {
  Coalition_Partisanship[i, ] <- 
      attrib.coal$Conservative_Lean_of_Organization_Coalition
}

# Lobbying_Expenditures: lobbying expenditure of organization
Lobbying_Expenditures <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
for (i in 1:ncol(mem)) {
  Lobbying_Expenditures[, i] <- attrib.grp$Lobbying_Spending_by_Organization
}

# Coalition_Dues: does the coalition collect dues?
Coalition_Dues <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
for (i in 1:nrow(mem)) {
  Coalition_Dues[i, ] <- attrib.coal$Coalition_Collects_Dues
}

# Coalition_Faces_Legislative_Threat: coalition responding to legislative threat
Coalition_Faces_Legislative_Threat <- matrix(NA, nrow = nrow(mem), 
    ncol = ncol(mem))
for (i in 1:nrow(mem)) {
  Coalition_Faces_Legislative_Threat[i, ] <- 
      attrib.coal$Coalition_Responding_to_Legislative_Threat
}

# Coalition_Focuses_on_Authorizing_Legislation
Coalition_Focuses_on_Authorizing_Legislation <- matrix(NA, nrow = nrow(mem), 
    ncol = ncol(mem))
for (i in 1:nrow(mem)) {
  Coalition_Focuses_on_Authorizing_Legislation[i, ] <- 
      attrib.coal$Coalition_Focuses_on_Authorizing_Legislation
}

# Interest_Group_Crosses_Issue_Boundary: 
# organization primarily active outside health domain
Interest_Group_Crosses_Issue_Boundary <- matrix(NA, nrow = nrow(mem), 
    ncol = ncol(mem))
for (i in 1:ncol(mem)) {
  Interest_Group_Crosses_Issue_Boundary[, i] <- 
      attrib.grp$Organization_Identified_Primarily_Outside_Health
}

# Citizens_Advocacy_Group: organization is citizens' advocacy organization
Citizens_Advocacy_Group <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
for (i in 1:ncol(mem)) {
  Citizens_Advocacy_Group[, i] <- 
      attrib.grp$Organization_is_Citizens_Advocacy_Organization
}

# Coalition_Steering_Committee: coalition has a steering committee
Coalition_Steering_Committee <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
for (i in 1:nrow(mem)) {
  Coalition_Steering_Committee[i, ] <- 
      attrib.coal$Coalition_Has_Steering_Committee
}

# Interest_Group_Age: centuries since organization was founded
Interest_Group_Age <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
for (i in 1:ncol(mem)) {
  Interest_Group_Age[, i] <- 0.01 * 
      attrib.grp$Years_Since_Founding_of_Organization_Coalition
}

# Coalition_Age: centuries since coalition was founded
Coalition_Age <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
for (i in 1:nrow(mem)) {
  Coalition_Age[i, ] <- 0.01 * 
      attrib.coal$Years_Since_Founding_of_Organization_Coalition
}


# ==============================================================================
# Estimate ERGMs
# ==============================================================================

# model 1: non-members as structural zeros
model.1 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 2: non-members as structural zeros, founders who are leaders as 
# structural ones
model.2 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)) + offset(edgecov(founderleader)), 
    offset.coef = c(-Inf, Inf), eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 3: same as model 1, but with diversity x embeddedness interaction
model.3 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong)
    + edgecov(Diversity_X_Embeddedness_Strong)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 4: same as model 2, but with diversity x embeddedness interaction
model.4 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong)
    + edgecov(Diversity_X_Embeddedness_Strong)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)) + offset(edgecov(founderleader)), 
    offset.coef = c(-Inf, Inf), eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# models in the main manuscript
htmlreg(list(model.1, model.2, model.3, model.4), single.row = TRUE, 
    file = "Models 1-4.html")


# model 5: non-members and founders as structural zeros
model.5 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem2)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 6: no structural zeros at all
model.6 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    , eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 7: substituting strong for weak embeddedness in model 1
model.7 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Weak)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 8: substituting strong for weak embeddedness in model 3
model.8 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Weak)
    + edgecov(Diversity_X_Embeddedness_Weak)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

htmlreg(list(model.5, model.6, model.7, model.8), single.row = TRUE, 
    custom.model.names = paste("Model", 5:8), file = "Models 5-8.html")

# model 9: like model 1, but using communication density instead of network 
# embeddedness
model.9 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(commdensity)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 10: like model 3, but using communication density instead of network 
# embeddedness
model.10 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(commdensity)
    + edgecov(commdensity.diversity)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 11: network embeddedness only in the same party (within-party ties)
model.11 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong_SameParty)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 12: network embeddedness only across parties (cross-party ties)
model.12 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong_CrossParty)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

htmlreg(list(model.9, model.10, model.11, model.12), single.row = TRUE, 
    custom.model.names = paste("Model", 9:12), file = "Models 9-12.html")

# model 13: only main effects and endogenous model terms
model.13 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong)
    # controls
    + b1star(2)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 14: like model 13, but with number of coalition memberships
model.14 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong)
    # controls
    + b1star(2)
    + edgecov(Number_of_Coalition_Memberships)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 15: add five other diversity variables to model 1
model.15 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong)
    + edgecov(diversity.age)
    + edgecov(diversity.lobspend)
    + edgecov(diversity.citadv)
    + edgecov(diversity.outshealth)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 16: like model 1, but interaction between visibility and diversity
model.16 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Diversity_X_Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

htmlreg(list(model.13, model.14, model.15, model.16), single.row = TRUE, 
    custom.model.names = paste("Model", 13:16), file = "Models 13-16.html")

# model 17: like model 1, but interaction between controversial and diversity
model.17 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong)
    # controls
    + b1star(2)
    + edgecov(Visibility)
    + edgecov(Diversity_X_Controversial)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Number_of_Coalition_Memberships)
    + edgecov(Coalition_Size)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

# model 18: coalition two-stars instead of number of coalition memberships and 
# coalition size (as these are collinear)
model.18 <- ergm(
    leader ~ 
    + edges
    # main effects
    + edgecov(Partisan_Diversity)
    + edgecov(Network_Embeddedness_Strong)
    # controls
    + b1star(2)
    + b2star(2)
    + edgecov(Visibility)
    + edgecov(Controversial)
    + edgecov(Interest_Group_Coalition_Partisan_Differential)
    + edgecov(Interest_Group_Partisanship)
    + edgecov(Coalition_Partisanship)
    + edgecov(Interest_Group_Age)
    + edgecov(Coalition_Age)
    + edgecov(Citizens_Advocacy_Group)
    + edgecov(Coalition_Dues)
    + edgecov(Lobbying_Expenditures)
    + edgecov(Interest_Group_Crosses_Issue_Boundary)
    + edgecov(Coalition_Faces_Legislative_Threat)
    + edgecov(Coalition_Focuses_on_Authorizing_Legislation)
    + edgecov(Coalition_Steering_Committee)
    + offset(edgecov(nonmem)), 
    offset.coef = -Inf, eval.loglik = FALSE, 
    control = control.ergm(MCMC.burnin = burnin, MCMC.samplesize = sampsize, 
        seed = seed, MCMLE.maxit = maxit)
)

htmlreg(list(model.17, model.18), single.row = TRUE,  custom.model.names =
    paste("Model", 17:18), file = "Models 17-18.html")


# ==============================================================================
# Assess goodness of fit
# ==============================================================================

# boxplot diagrams
mygof <- function(model, number) {
  gf <- gof(model, nsim = nsim, statistics = c(nsp, b1deg, b2deg, geodesic, 
      b1star, b2star, rocpr), ncpus = cores, parallel = "multicore", 
      roc = FALSE)
  temp <- gf[1:6]
  class(temp) <- "gof"
  pdf(paste0("gof.", number, ".pdf"), width = 9, height = 6)
  plot(temp)
  dev.off()
  return(gf)
}

gof.1 <- mygof(model.1, 1)
gof.2 <- mygof(model.2, 2)
gof.2 <- mygof(model.3, 3)
gof.2 <- mygof(model.4, 4)
gof.2 <- mygof(model.5, 5)
gof.2 <- mygof(model.6, 6)
gof.2 <- mygof(model.7, 7)
gof.2 <- mygof(model.8, 8)
gof.2 <- mygof(model.9, 9)
gof.2 <- mygof(model.10, 10)
gof.2 <- mygof(model.11, 11)
gof.2 <- mygof(model.12, 12)
gof.2 <- mygof(model.13, 13)
gof.2 <- mygof(model.14, 14)
gof.2 <- mygof(model.15, 15)
gof.2 <- mygof(model.16, 16)
gof.2 <- mygof(model.17, 17)
gof.2 <- mygof(model.18, 18)

# precision-recall curves
gof.1[[7]]$auc.pr
gof.2[[7]]$auc.pr
pdf("pr.pdf")
plot(gof.2[[7]], col = "gray50", rgraph = FALSE, lwd = 3, 
    main = "Precision-recall curves")
plot(gof.1[[7]], col = "black", rgraph = TRUE, random.col = "gray90", lwd = 3, 
    add = TRUE)
legend("topright", legend = c("Model 1", "Model 2", "Random graph"), 
    col = c("black", "gray50", "gray90"), lty = 1, lwd = 3)
dev.off()

# MCMC trace plots
pdf("mcmcdiag.pdf")
mcmc.diagnostics(model.1)
dev.off()


# ==============================================================================
# Estimate a random or fixed effects model
# ==============================================================================

rows <- nrow(as.matrix(leader))
cols <- ncol(as.matrix(leader))
ig <- matrix(rep(1:rows, cols), nrow = rows)
coal <- matrix(rep(1:cols, rows), ncol = cols, byrow = TRUE)

# create data frame
nm <- as.matrix(nonmem)
dat <- data.frame(
    leader = as.matrix(leader)[nm != 1], 
    absdiff = Interest_Group_Coalition_Partisan_Differential[nm != 1], 
    Partisan_Diversity = Partisan_Diversity[nm != 1], 
    Network_Embeddedness_Strong = Network_Embeddedness_Strong[nm != 1], 
    Interest_Group_Partisanship = Interest_Group_Partisanship[nm != 1], 
    Coalition_Partisanship = Coalition_Partisanship[nm != 1], 
    Interest_Group_Age = Interest_Group_Age[nm != 1], 
    Coalition_Age = Coalition_Age[nm != 1], 
    Citizens_Advocacy_Group = Citizens_Advocacy_Group[nm != 1], 
    Coalition_Dues = Coalition_Dues[nm != 1], 
    Lobbying_Expenditures = Lobbying_Expenditures[nm != 1], 
    Interest_Group_Crosses_Issue_Boundary = 
        Interest_Group_Crosses_Issue_Boundary[nm != 1], 
    Coalition_Faces_Legislative_Threat = Coalition_Faces_Legislative_Threat[nm 
        != 1], 
    Coalition_Focuses_on_Authorizing_Legislation = 
        Coalition_Focuses_on_Authorizing_Legislation[nm != 1], 
    Coalition_Steering_Committee = Coalition_Steering_Committee[nm != 1], 
    ig = ig[nm != 1], 
    coal = coal[nm != 1]
)
dat$coal2 <- factor(dat$coal)  # fixed effect: create factor

# random effect in lme4: estimation does not converge
library("lme4")
model.19 <- glmer(
    leader
    ~ absdiff 
    + Partisan_Diversity
    + Network_Embeddedness_Strong
    + Interest_Group_Partisanship 
    + Coalition_Partisanship 
    + Interest_Group_Age 
    + Coalition_Age 
    + Citizens_Advocacy_Group 
    + Coalition_Dues 
    + Lobbying_Expenditures 
    + Interest_Group_Crosses_Issue_Boundary 
    + Coalition_Faces_Legislative_Threat 
    + Coalition_Focuses_on_Authorizing_Legislation 
    + Coalition_Steering_Committee 
    + (1 | coal), 
    data = dat[, 1:17], family = binomial, nAGQ = 10
)
summary(model.19)

# random effect with glmmPQL: estimation converges, but don't trust the results;
# e.g., no model fit is reported... did it really converge?
library("MASS")
model.20 <- glmmPQL(
    leader
    ~ absdiff 
    + Partisan_Diversity
    + Network_Embeddedness_Strong
    + Interest_Group_Partisanship 
    + Coalition_Partisanship 
    + Interest_Group_Age 
    + Coalition_Age 
    + Citizens_Advocacy_Group 
    + Coalition_Dues 
    + Lobbying_Expenditures 
    + Interest_Group_Crosses_Issue_Boundary 
    + Coalition_Faces_Legislative_Threat 
    + Coalition_Focuses_on_Authorizing_Legislation 
    + Coalition_Steering_Committee 
    , random = ~ 1|coal
    , data = dat[, 1:17], family = binomial
)
summary(model.20)

# use GLM and fixed effect: model does not converge
model.21 <- glm(
    leader
    ~ absdiff 
    + Partisan_Diversity
    + Network_Embeddedness_Strong
    + Interest_Group_Partisanship 
    + Coalition_Partisanship 
    + Interest_Group_Age 
    + Coalition_Age 
    + Citizens_Advocacy_Group 
    + Coalition_Dues 
    + Lobbying_Expenditures 
    + Interest_Group_Crosses_Issue_Boundary 
    + Coalition_Faces_Legislative_Threat 
    + Coalition_Focuses_on_Authorizing_Legislation 
    + Coalition_Steering_Committee
    + coal2
    , data = dat, family = binomial
)
summary(model.21)


# ==============================================================================
# Micro-level interpretation (= predicted probabilities)
# ==============================================================================

# create dyadic datasets
edgeprob.3 <- edgeprob(model.3)
edgeprob.4 <- edgeprob(model.4)
edgeprob.10 <- edgeprob(model.10)
edgeprob.16 <- edgeprob(model.16)
edgeprob.17 <- edgeprob(model.17)


# function for plotting facets with variable 1 conditional on variable 2
facets <- function(edgeprobs, mem, number, var1, var2, varname1, varname2) {
  # keep only those dyadic probabilities where there is no structural zero
  include <- logical(nrow(edgeprobs))
  for (r in 1:length(include)) {
    if (mem[edgeprobs$i[r], edgeprobs$j[r] - nrow(mem)] == 1) {
      include[r] <- TRUE
    } else {
      include[r] <- FALSE
    }
  }
  dyads <- edgeprobs[include == TRUE, ]
  
  # cut network embeddedness into slices
  dyads$v1 <- c(dyads[var1])[[1]]
  v2 <- c(dyads[var2])[[1]]
  v2.quantiles <- quantile(v2)
  v2 <- cut(v2, v2.quantiles, labels = names(v2.quantiles)[2:5])
  v2[is.na(v2)] <- "25%"
  dta <- transform(dyads, v2 = v2)

  # plot conditional probabilities with facets
  pdf(paste0("facets.", number, ".", varname1, ".pdf"))
  gp <- ggplot(data = dta, aes(x = v1, y = probability))
  print(gp + stat_smooth(method = "lm", fullrange = TRUE, color = "black") + facet_wrap( ~ v2, 
      ncol = 2) + xlab(varname1) + ylab("Probability") + ggtitle(
      paste0("Model ", number, ": ", varname1, " effect conditional on ", 
      varname2)))
  dev.off()
}

facets(edgeprobs = edgeprob.3, mem = mem, number = 3, 
       var1 = "edgecov.Partisan_Diversity[[i]]", 
       var2 = "edgecov.Network_Embeddedness_Strong[[i]]", 
       varname1 = "Partisan diversity", varname2 = "Network embeddedness")

facets(edgeprobs = edgeprob.3, mem = mem, number = 3, 
       var2 = "edgecov.Partisan_Diversity[[i]]", 
       var1 = "edgecov.Network_Embeddedness_Strong[[i]]", 
       varname2 = "Partisan diversity", varname1 = "Network embeddedness")

facets(edgeprobs = edgeprob.4, mem = mem, number = 4, 
       var1 = "edgecov.Partisan_Diversity[[i]]", 
       var2 = "edgecov.Network_Embeddedness_Strong[[i]]", 
       varname1 = "Partisan diversity", varname2 = "Network embeddedness")

facets(edgeprobs = edgeprob.4, mem = mem, number = 4, 
       var2 = "edgecov.Partisan_Diversity[[i]]", 
       var1 = "edgecov.Network_Embeddedness_Strong[[i]]", 
       varname2 = "Partisan diversity", varname1 = "Network embeddedness")

facets(edgeprobs = edgeprob.10, mem = mem, number = 10, 
       var1 = "edgecov.Partisan_Diversity[[i]]", 
       var2 = "edgecov.commdensity[[i]]", varname1 = "Partisan diversity", 
       varname2 = "Communication density")

facets(edgeprobs = edgeprob.10, mem = mem, number = 10, 
       var2 = "edgecov.Partisan_Diversity[[i]]", 
       var1 = "edgecov.commdensity[[i]]", varname2 = "Partisan diversity", 
       varname1 = "Communication density")


# predicted probabilities for visibility interaction (model 16)
include.16 <- logical(nrow(edgeprob.16))
for (r in 1:length(include.16)) {
  if (mem[edgeprob.16$i[r], edgeprob.16$j[r] - nrow(mem)] == 1) {
    include.16[r] <- TRUE
  } else {
    include.16[r] <- FALSE
  }
}
dyads.16 <- edgeprob.16[include.16 == TRUE, ]

dta <- data.frame(prob = dyads.16$probability,
                  pd = dyads.16$`edgecov.Partisan_Diversity[[i]]`,
                  vis = dyads.16$`edgecov.Visibility[[i]]`)

pdf(paste0("interaction.visibility.16.pdf"))
gp <- ggplot(data = dta, aes(x = pd, y = prob, linetype = factor(vis))) +
    stat_smooth(method = "lm", fullrange = TRUE, colour = "black")
gp + labs(linetype = "Visibility") + xlab("Partisan diversity") +
    ylab("Probability") + ggtitle(paste("Partisan diversity conditional on", 
    "visibility of the coalition"))
dev.off()


# predicted probabilities for controversy interaction (model 17)
include.17 <- logical(nrow(edgeprob.17))
for (r in 1:length(include.17)) {
  if (mem[edgeprob.17$i[r], edgeprob.17$j[r] - nrow(mem)] == 1) {
    include.17[r] <- TRUE
  } else {
    include.17[r] <- FALSE
  }
}
dyads.17 <- edgeprob.17[include.17 == TRUE, ]

dta <- data.frame(prob = dyads.17$probability,
                  pd = dyads.17$`edgecov.Partisan_Diversity[[i]]`,
                  cv = dyads.17$`edgecov.Controversial[[i]]`)

pdf(paste0("interaction.controversy.17.pdf"))
gp <- ggplot(data = dta, aes(x = pd, y = prob, linetype = factor(cv))) +
    stat_smooth(method = "lm", fullrange = TRUE, colour = "black")
gp + labs(linetype = "Controversy") + xlab("Partisan diversity") +
    ylab("Probability") + ggtitle(paste("Partisan diversity conditional on",
                                        "controversialness of the coalition"))
dev.off()


# ==============================================================================
# Marginal effects plot using btergm and interplot
# ==============================================================================

pdf("marginal-effects-model3a.pdf", width = 6, height = 4)
marginalplot(model.3, 
             var1 = "edgecov.Partisan_Diversity", 
             var2 = "edgecov.Network_Embeddedness_Strong", 
             inter = "edgecov.Diversity_X_Embeddedness_Strong", 
             structzeromat = as.matrix(nonmem), 
             ylab = "Partisan diversity", 
             xlab = "Network embeddedness", 
             rug = TRUE) + ggtitle("Model 3") + theme_bw()
dev.off()

pdf("marginal-effects-model3b.pdf", width = 6, height = 4)
marginalplot(model.3, 
             var1 = "edgecov.Network_Embeddedness_Strong", 
             var2 = "edgecov.Partisan_Diversity", 
             inter = "edgecov.Diversity_X_Embeddedness_Strong", 
             structzeromat = as.matrix(nonmem), 
             xlab = "Partisan diversity", 
             ylab = "Network embeddedness", 
             rug = TRUE) + ggtitle("Model 3") + theme_bw()
dev.off()

pdf("marginal-effects-model4a.pdf", width = 6, height = 4)
marginalplot(model.4, 
             var1 = "edgecov.Partisan_Diversity", 
             var2 = "edgecov.Network_Embeddedness_Strong", 
             inter = "edgecov.Diversity_X_Embeddedness_Strong", 
             structzeromat = as.matrix(nonmem), 
             ylab = "Partisan diversity", 
             xlab = "Network embeddedness", 
             rug = TRUE) + ggtitle("Model 4") + theme_bw()
dev.off()

pdf("marginal-effects-model4b.pdf", width = 6, height = 4)
marginalplot(model.4, 
             var1 = "edgecov.Network_Embeddedness_Strong", 
             var2 = "edgecov.Partisan_Diversity", 
             inter = "edgecov.Diversity_X_Embeddedness_Strong", 
             structzeromat = as.matrix(nonmem), 
             xlab = "Partisan diversity", 
             ylab = "Network embeddedness", 
             rug = TRUE) + ggtitle("Model 4") + theme_bw()
dev.off()

pdf("marginal-effects-model10a.pdf", width = 6, height = 4)
marginalplot(model.10, 
             var1 = "edgecov.Partisan_Diversity", 
             var2 = "edgecov.commdensity", 
             inter = "edgecov.commdensity.diversity", 
             structzeromat = as.matrix(nonmem), 
             ylab = "Partisan diversity", 
             xlab = "Communication density", 
             rug = TRUE) + ggtitle("Model 10") + theme_bw()
dev.off()

pdf("marginal-effects-model10b.pdf", width = 6, height = 4)
marginalplot(model.10, 
             var1 = "edgecov.commdensity", 
             var2 = "edgecov.Partisan_Diversity", 
             inter = "edgecov.commdensity.diversity", 
             structzeromat = as.matrix(nonmem), 
             xlab = "Partisan diversity", 
             ylab = "Communication density", 
             rug = TRUE) + ggtitle("Model 10") + theme_bw()
dev.off()

pdf("marginal-effects-model16.pdf", width = 6, height = 4)
marginalplot(model.16, 
             var1 = "edgecov.Partisan_Diversity", 
             var2 = "edgecov.Visibility", 
             inter = "edgecov.Diversity_X_Visibility", 
             structzeromat = as.matrix(nonmem), 
             ylab = "Partisan diversity", 
             xlab = "Visibility of the coalition", 
             point = TRUE, 
             rug = FALSE) + 
  ggtitle("Model 16") + 
  theme_bw()
dev.off()

pdf("marginal-effects-model17.pdf", width = 6, height = 4)
marginalplot(model.17, 
             var1 = "edgecov.Partisan_Diversity", 
             var2 = "edgecov.Controversial", 
             inter = "edgecov.Diversity_X_Controversial", 
             structzeromat = as.matrix(nonmem), 
             ylab = "Partisan diversity", 
             xlab = "Controversy", 
             point = TRUE, 
             rug = FALSE) + 
  ggtitle("Model 17") + 
  theme_bw()
dev.off()


# save workspace to a file for later use
save.image(file = "leadership-lobbying.RData")
