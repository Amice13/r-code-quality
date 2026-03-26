###################################################################################
###                                                                             ###
###                                   Code for                                  ###
###     "The State and the Emergence of the First American Party System"        ###
###                         American Sociological Review                        ###
###                                     2025                                    ###
###                                                                             ###
###################################################################################


# Make sure you have the following libraries installed before running the code.
library(dplyr)
library(tidyr)
library(scrime)
library(igraph)
library(ggplot2)
library(ggpubr)
library(ggforce)
library(wnominate)
library(purrr)



#========== DATA PREPARATION ==========#

# Change the working directory to the folder where you stored the file replication_data.RData
setwd("...")

# Create output directory
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

# Load the data
# The file replication_data.RData contains 
# (a) a file containing the votes (votes)
# (b) a file containing legislator information (assemblymen)
# (c) a file containing polarities for the W_NOMINATE model (polarities). See the wnominate documentation for an explanation of polarity. Polarities were chosen so that Republicans tend to be placed on the right side of the graph and legislators from the southern district at the bottom.
# (d) a file containing our codings of the votes (codings)
# (e) three files containing figures for the US House
load("replication_data.rdata")

# Prepare the data
assemblymen$party <- factor(assemblymen$party, levels = c("F", "R", "both", "none"))
assemblymen$party2 <- factor(assemblymen$party2, levels = c("F", "R", "both", "none"))

votes <- read.csv("data/votes.csv", header = T, stringsAsFactors = F)
vote_list <- votes %>%
  group_split(session_number)

# Create list of roll call voting matrices
rcv_list <- lapply(vote_list, function(v){
  sn <- unique(v$session_number)
  message("Session: ", sn)
 
  v2 <- v %>%
    rowwise() %>%
    mutate(vote_number = paste("V", sprintf("%03d", vote_number), sep = "")) %>%
    dplyr::select(session_number, id, name, county, district, vote_number, vote) %>%
    arrange(session_number, vote_number) %>%
    spread(vote_number, vote)

  # Delete assemblymen with less than X votes
  v2 <- v2 %>%
    rowwise() %>%
    filter(sum(c_across(starts_with("V")) %in% c(0, 1), na.rm = TRUE) >= 1) %>%
    ungroup()
  print(paste("Number of assemblymen with at least 1 vote:", nrow(v2)))
  
  v2 <- v2 %>%
    rowwise() %>%
    filter(sum(c_across(starts_with("V")) %in% c(0, 1), na.rm = TRUE) >= 20) %>%
    ungroup()
  print(paste("Number of assemblymen with at least 20 votes:", nrow(v2)))
  
  # Delete lopsided votes (minority < 2.5%)
  print(paste("Number of all roll calls:", length(grep("^V", names(v2)))))
  
  vote_columns <- grep("^V", names(v2), value = TRUE)
  lopsided_votes <- sapply(vote_columns, function(vote_column) {
    vote_counts <- table(v2[[vote_column]], useNA = "no")  # Count 0s and 1s, ignoring NAs
    if (length(vote_counts) < 2) return(TRUE)  # If only one category exists, it's lopsided
    min(vote_counts) / sum(vote_counts) < 0.025  # Check if minority proportion is < 2.5%
  })
  v2 <- v2[, !(names(v2) %in% vote_columns[lopsided_votes])]
  print(paste("Number of all roll calls where minority >= 2.5%:", length(grep("^V", names(v2)))))
  
  return(v2)
})

# The dates when the assembly met
dates <- data.frame("sn" = seq(11,26,1),
                    "date" = c("Jan 1788 - Mar 1788", "Dec 1788 - Mar 1789", "Jul 1789 - Apr 1790", 
                               "Jan 1791 - Mar 1791", "Jan 1792 - Apr 1792", "Nov 1792 - Mar 1793", 
                               "Jan 1794 - Mar 1794", "Jan 1795 - Apr 1795", "Jan 1796 - Apr 1796", 
                               "Nov 1796 - Apr 1797", "Jan 1798 - Apr 1798", "Aug 1798 - Apr 1799", 
                               "Jan 1800 - Apr 1800", "Nov 1800 - Apr 1801", "Jan 1802 - Apr 1802", 
                               "Jan 1803 - Apr 1803"))



#========== TABLE 3 ==========#

assemblymen_sub <- assemblymen %>%
  filter(served_12_to_25 == 1)

table(assemblymen_sub$party)

ct <- assemblymen_sub %>%
  group_by(party) %>%
  summarise(dob_mean = mean(dob, na.rm = T),
            dob_median = median(dob, na.rm = T),
            
            prop_merchant = mean(merchant, na.rm = T) * 100,
            prop_lawyer = mean(lawyer, na.rm = T) * 100,
            prop_capitalist = mean(capitalist, na.rm = T) * 100,
            prop_landowner = mean(landowner, na.rm = T) * 100,
            prop_farmer = mean(farmer, na.rm = T) * 100,
            prop_mechanic = mean(mechanic, na.rm = T) * 100,
            prop_professional = mean(professional, na.rm = T) * 100,
            prop_storekeeper = mean(storekeeper, na.rm = T) * 100,
            prop_tavernkeeper = mean(tavernkeeper, na.rm = T) * 100,

            real_all_av = mean(real_all, na.rm = T),
            real_all_median = median(real_all, na.rm = T),
            personal_all_av = mean(personal_all, na.rm = T),
            personal_all_median = median(personal_all, na.rm = T),
            real_primary_av = mean(real_primary, na.rm = T),
            real_primary_median = median(real_primary, na.rm = T),
            personal_primary_av = mean(personal_primary, na.rm = T),
            personal_primary_median = median(personal_primary, na.rm = T),
            
            av_nr_slaves = mean(nrslaves),
            prop_with_slaves = mean(slave_yn) * 100,
            
            prop_college = mean(college, na.rm = T) * 100,
            
            prop_soc = mean(soc, na.rm = T) * 100,
            prop_soc_plus_honorary = mean(soc_plus_honorary, na.rm = T) * 100
  ) %>%
  as.data.frame()

ct2 <- ct
rownames(ct2) <- ct2$party
ct2 <- t(ct2[, -1])
ct2 <- round(ct2, 1)
ct2



#========== TABLE 4 ==========#

codings %>%
  group_by(type, level1) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(type) %>%
  mutate(pct = round(count / sum(count) * 100, 1)) %>%
  dplyr::select(-count) %>%
  spread(type, pct, fill = 0) 



#========== FIGURE 1 ==========#

# This can take 10-30 minutes to run...

out1 <- lapply(rcv_list, function(rcv) {
  sn <- unique(rcv$session_number)
  message("Session: ", sn)
  
  legData <- dplyr::select(rcv, id, name, county, district)
  votes <- rcv %>%
    dplyr::select(starts_with("V")) %>%
    as.matrix()
  
  # Calculate simple matching distance
  s <- smc(votes + 1, dist = FALSE)
  rownames(s) <- rcv$id
  colnames(s) <- rcv$id

  G <- graph_from_adjacency_matrix(s, weighted = TRUE, mode = "undirected")
  
  # Find the optimal number of clusters based on modularity maximization
  optimal_number_of_clusters <- cluster_optimal(G, weights = E(G)$weight)
  
  out2 <- data.frame("session_number" = sn,
                     "optimal_number_of_clusters" = length(optimal_number_of_clusters),
                     "cluster" = seq(1, length(optimal_number_of_clusters), 1),
                     "cluster_size" = as.vector(sort(table(optimal_number_of_clusters$membership), decreasing = T))
  )
  return(out2)
}) 
out3 <- bind_rows(out1)

# Create figure
ggplot(out3, aes(x = session_number, y = cluster_size, fill =  forcats::fct_rev(factor(cluster)))) +
  geom_bar(position = "fill", stat = "identity") + # or position = "stack"
  scale_fill_grey(start = .95, end = .15) +
  labs(x = "Session Number", y = "Proportion in Cluster", fill = "Cluster") +
  scale_x_continuous(breaks = c(12, 16, 20, 24), sec.axis = sec_axis(~.+1777, breaks = c(1789, 1793, 1797, 1801))) +
  theme_bw() +
  theme(text = element_text(size = 15))
ggsave("output/figure1.tiff", width = 5, height = 4, scale = 1.3)



#========== FIGURES 2 AND 3 ==========#

# The function that does all the work
produce_figure <- function(session) {
  # Get data ready
  rcv <- rcv_list[[session-10]]
  sn <- unique(rcv$session_number)
  message("Session: ", sn)
  
  legData <- rcv %>%
    dplyr::select(id, session_number, district)
  
  # Merge in party data
  if (sn < 26) {
    legData <- legData %>%
      left_join(dplyr::select(assemblymen, id, party), by = "id") %>%
      as.data.frame()
  } else {
    legData <- legData %>%
      left_join(dplyr::select(assemblymen, id, party2), by = "id") %>%
      rename(party = party2) %>%
      as.data.frame()
  }
  legData$party[is.na(legData$party)] <- "none"
  
  votes <- rcv %>%
    dplyr::select(starts_with("V")) %>%
    as.matrix()
  
  # Calculate WNOMINATE scores
  rc <- rollcall(votes, legis.names = legData$id, legis.data = legData, vote.names = names(votes)) # , vote.data = rcData
  result <- wnominate(rc, 
                      polarity = c(polarities$D1[polarities$session_number == sn], polarities$D2[polarities$session_number == sn]), 
                      minvotes = 20, lop = .025)
  
  # Normalize eigenvalues
  evs <- result$eigenvalues / sqrt(sum(result$eigenvalues^2))
  sp <- ggplot() +
    geom_point(aes(x = seq(1,5,1), y = evs[1:5]), size = 2) +
    geom_line(aes(x = seq(1,5,1), y = evs[1:5])) +
    lims(y = c(0, 1)) +
    theme_classic() +
    labs(x = "Dimension", y = "Eigenvalue")
  
  # Calculate centroids
  D <- result$legislators %>%
    group_by(district) %>%
    summarise(X = mean(coord1D, na.rm = T),
              Y = mean(coord2D, na.rm = T))
  D$district <- as.character(D$district)
  D$district[D$district == "Southern"] <- "S"
  D$district[D$district == "Middle"] <- "M"
  D$district[D$district == "Eastern"] <- "E"
  D$district[D$district == "Western"] <- "W"
  
  # Plot
  p <- ggplot() +
    geom_point(data = result$legislators,
               aes(x = coord1D, y = coord2D, group = party, color = party, shape = party), 
               size = 3) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 1), size = .01) +
    geom_point(data = D, aes(x = X, y = Y), size = 12, color = "gray", alpha = .2) +  
    geom_text(data = D, aes(x = X, y = Y, label = district), size=3, alpha = .9) +
    scale_color_manual(values = c("#08306B", "#FD8D3C", "#FFEDA0", "#D4B9DA")) + 
    scale_shape_manual(values = c(19, 15, 17, 18)) +
    theme_classic() +
    coord_fixed() +
    labs(x = "First Dimension", y = "Second Dimension", title = paste0("Correct Classifiction: ", round(result$fits[1], 1), "%, ", round(result$fits[2], 1), "% | ", "APRE: ", round(result$fits[3], 2), ", ", round(result$fits[4], 2), " | GMP: ", round(result$fits[5], 2), ", ", round(result$fits[6], 2))) +
    annotation_custom(ggplotGrob(sp), xmin = .85, xmax = 1.45, 
                      ymin = -1.1, ymax = -.55)
  return(p)
}

# Produce W-NOMINATE plot for different sessions
# (session 24 for Figure 2 and session 12 for Figure 3)
f2 <- produce_figure(session = 24)
f3 <- produce_figure(session = 12)

# Create figures
ggarrange(p_house_7, f2,
          labels = c("A: US House, Session 7 (Mar 1801 - Mar 1803)", "B: NY Assembly, Session 24 (Nov 1800 - Apr 1801)"),
          ncol = 2, nrow = 1,
          common.legend = FALSE,
          label.x = c(-.25, -.28),
          widths = c(1, 1.02)
) 
ggsave("output/figure2.tiff", scale = 2.65, width = 5, height = 2.5, bg = "white")

ggarrange(p_house_1, f3,
          labels = c("A: US House, Session 1 (Mar 1789 - Mar 1791)", "B: NY Assembly, Session 12 (Dec 1788 - Mar 1789)"),
          ncol = 2, nrow = 1,
          common.legend = FALSE,
          #legend.grob = get_legend(p_assembly_24, position = "bottom"), legend = "bottom",
          label.x = c(-.255, -.28)
) 
ggsave("output/figure3.tiff", scale = 2.65, width = 5, height = 2.5, bg = "white")



#========== FIGURE 4 ==========#

process_nominate_session <- function(rcv, polarities) {
  sn <- unique(rcv$session_number)
  message("Session: ", sn)
  
  # Prepare data
  legData <- rcv %>%
    dplyr::select(id, session_number)
  
  # Merge in party data
  if (sn < 26) {
    legData <- legData %>%
      left_join(dplyr::select(assemblymen, id, party), by = "id") %>%
      as.data.frame()
  } else {
    legData <- legData %>%
      left_join(dplyr::select(assemblymen, id, party2), by = "id") %>%
      rename(party = party2) %>%
      as.data.frame()
  }
  legData$party[is.na(legData$party)] <- "none"
  
  votes <- rcv %>%
    dplyr::select(starts_with("V")) %>%
    as.matrix()

  # Compute NOMINATE scores
  rc <- rollcall(votes, legis.names = legData$id, legis.data = legData)
  result <- wnominate(rc, 
                      polarity = c(polarities$D1[polarities$session_number == sn], polarities$D2[polarities$session_number == sn]), 
                      minvotes = 10, lop = .025)
  
  # Get data
  dta <- result$legislators %>%
    mutate(party_binary = ifelse(party == "R", 1, ifelse(party == "F", 0, NA))) %>%
    dplyr::select(party_binary, coord1D, coord2D, session_number) %>%
    drop_na()
    
  # Function to calculate variance explained for each dimension
  compute_rsq <- function(coord_var, dim_label) {
    sdx <- sd(dta$party_binary)
    sdy <- sd(dta[[coord_var]])
    
    lm_model <- lm(as.formula(paste(coord_var, "~ party_binary")), data = dta)
    b1 <- summary(lm_model)$coefficients[2,1]
    se1 <- summary(lm_model)$coefficients[2,2]
    
    b_low <- b1 - 2 * se1 * sign(b1)
    b_high <- b1 + 2 * se1 * sign(b1)
    
    r_sq1 <- (b1 * (sdx / sdy))^2
    if (b_low * b_high < 0) {
      r_sq_high <- max((b_low * (sdx / sdy))^2, (b_high * (sdx / sdy))^2)
      r_sq_low <- 0
    } else {
      r_sq_low <- (b_low * (sdx / sdy))^2
      r_sq_high <- (b_high * (sdx / sdy))^2
    }
    
    return(data.frame("session_number" = sn,
                      "Dimension" = dim_label,
                      "value" = 100 * r_sq1,
                      "ll" = 100 * r_sq_low,
                      "ul" = min(100, 100 * r_sq_high)))
  }
  
  # Compute results for both dimensions
  out_dim1 <- compute_rsq("coord1D", "Dimension 1")
  out_dim2 <- compute_rsq("coord2D", "Dimension 2")
  
  return(rbind(out_dim1, out_dim2))
}
out1 <- lapply(rcv_list, process_nominate_session, polarities = polarities)
out1 <- bind_rows(out1)

# Create figure
ggplot(out1) +
  geom_line(aes(x = session_number, y = value, linetype = Dimension, color = Dimension), linewidth = 1, color = "black") +
  geom_ribbon(data = filter(out1, Dimension == "Dimension 1"), aes(x = session_number, ymin = ll, ymax = ul), alpha = 0.3) +
  labs(x = "Session Number", y = "% Variance Explained", linetype = NULL) +
  scale_x_continuous(breaks = c(12, 16, 20, 24), sec.axis = sec_axis(~.+1777, breaks = c(1789, 1793, 1797, 1801))) +
  theme_classic() +
  theme(text = element_text(size = 15), 
        legend.position="bottom")
ggsave("output/figure4.tiff", width = 5, height = 4, scale = 1.3)



#========== FIGURE 5 ==========#

variances <- lapply(rcv_list, function(rcv) {
  sn <- unique(rcv$session_number)
  message("Session: ", sn)
  
  # Merge in party data
  if (sn < 26) {
    rcv <- rcv %>%
      left_join(dplyr::select(assemblymen, id, party), by = "id") %>%
      as.data.frame()
  } else {
    rcv <- rcv %>%
      left_join(dplyr::select(assemblymen, id, party2), by = "id") %>%
      rename(party = party2) %>%
      as.data.frame()
  }
  rcv$party[is.na(rcv$party)] <- "none"
  
  # Reshape to long format
  rcv_long <- rcv %>%
    pivot_longer(cols = starts_with("V"), names_to = "vote_id", values_to = "vote") %>%
    drop_na(vote)
  
  # Run ANOVA for each vote_id and compute ratios
  anova_results <- rcv_long %>%
    group_by(vote_id) %>%
    summarise({
      anova1 <- anova(lm(vote ~ district + party, data = pick(vote, district, party)))
      sum_squares <- sum(anova1[, 2])
      
      tibble(
        session_number = sn,
        district_variance = anova1[1, 2] / sum_squares,
        party_variance = anova1[2, 2] / sum_squares,
        residual_variance = anova1[3, 2] / sum_squares
      )
    }, .groups = "drop")
  
  return(anova_results)
}) %>%
  bind_rows() %>%
  group_by(session_number) %>%
  summarise(
    Residual = mean(residual_variance) * 100,
    District = mean(district_variance) * 100,
    Party = mean(party_variance) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -session_number, names_to = "type", values_to = "value") %>%
  mutate(type = factor(type, levels = c("Residual", "Party", "District")))

# Create figure
ggplot(variances, aes(x = session_number, y = value, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_grey(start = .8, end = .2) +
  labs(x = "Session Number", y = "% of Total Variance", fill = NULL) +
  scale_x_continuous(limits = c(10.5, 26.5), breaks = c(12, 16, 20, 24),
                     sec.axis = sec_axis(~.+1777, breaks = c(1789, 1793, 1797, 1801))) +
  theme_bw() +
  theme(text = element_text(size = 15))
ggsave("output/figure5.tiff", width = 5, height = 3.5, scale = 1.3)




#========== FIGURES FOR ONLINE SUPPLEMENT ==========#

library(cluster)
library(reshape2)
library(dendextend)
library(grid)
library(GGally)



#========== FIGURES B-1 AND B-2 ==========#

out1 <- lapply(rcv_list, function(rcv) {
  sn <- unique(rcv$session_number)
  message("Session: ", sn)
  
  # Prepare data
  ids <- rcv$id
  legData <- rcv %>%
    dplyr::select(id, session_number)

  # Merge in party data
  if (sn < 26) {
    legData <- legData %>%
      left_join(dplyr::select(assemblymen, id, party), by = "id") %>%
      as.data.frame()
  } else {
    legData <- legData %>%
      left_join(dplyr::select(assemblymen, id, party2), by = "id") %>%
      rename(party = party2) %>%
      as.data.frame()
  }
  legData$party[is.na(legData$party)] <- "none"
  
  votes <- rcv %>%
    dplyr::select(starts_with("V")) %>%
    as.matrix()
    
  d <- smc(as.matrix(votes + 1), dist = TRUE)
  d <- as.matrix(d)
  rownames(d) <- ids
  colnames(d) <- ids
  
  pam1 <- pam(as.dist(d), diss = TRUE, k = 2)
  sil <- silhouette(pam1)
  
  sil_df <- data.frame(
    id = names(sil[, 1]),
    cluster = sil[, 1],
    sil_width = sil[, 3]
  )
  
  legData <- legData %>%
    left_join(sil_df, by = "id")
  
  return(legData)
}) 
out2 <- bind_rows(out1)

out3 <- out2 %>%
  group_by(session_number, cluster) %>%
  summarise(ASW = mean(sil_width, na.rm = TRUE), .groups = "drop") %>%
  group_by(session_number) %>%
  arrange(-ASW) %>%
  mutate(cluster_label = c(1, 2))

# Create figure
p_ASW_assembly <- ggplot(out3) +
  geom_line(aes(x = session_number, y = ASW, group = as.factor(cluster_label), linetype = as.factor(cluster_label)), linewidth = 1) +
  labs(x = "Session Number", y = "ASW", linetype = "Cluster") +
  scale_x_continuous(breaks = c(12, 16, 20, 24), sec.axis = sec_axis(~.+1777, breaks = c(1789, 1793, 1797, 1801))) +
  theme_bw() +
  theme(text = element_text(size = 15),
        legend.position="bottom")

ggarrange(NULL, p_ASW_assembly, NULL, p_ASW_house,
          labels = c("", "A: NY Assembly", "", "B: US House"),
          ncol = 1, nrow = 4,
          common.legend = TRUE, legend = "bottom",
          label.y = 1.1,
          hjust = c(0, -0.41, 0, -0.51),
          heights = c(0.2, 1, 0.2, 1)
) 
ggsave("output/figureB-1.tiff", width = 9, height = 11, bg = "white")

out4 <- out2 %>%
  filter(party %in% c("F", "R")) %>%
  group_by(session_number, cluster) %>%
  summarise(N_F = sum(party == "F", na.rm = T),
            N_R = sum(party == "R", na.rm = T), .groups = "drop") %>%
  arrange(session_number, -N_F) %>%
  group_by(session_number) %>%
  mutate(cluster_label = c(1, 2)) %>%
  summarise(correctly_classified = (N_F[cluster_label == 1] + N_R[cluster_label == 2]) / (sum(N_F) + sum(N_R)) )

ggplot(out4) +
  geom_line(aes(x = session_number, y = correctly_classified), linewidth = 1) +
  labs(x = "Session Number", y = "Overlap") +
  scale_x_continuous(breaks = c(12, 16, 20, 24), sec.axis = sec_axis(~.+1777, breaks = c(1789, 1793, 1797, 1801))) +
  theme_bw() +
  theme(text = element_text(size = 15))
ggsave("output/figureB-2.tiff", height = 7, width = 9)



#========== FIGURE B-3 ==========#

out1 <- lapply(rcv_list, function(rcv) {
  sn <- unique(rcv$session_number)
  message("Session: ", sn)
  
  # Prepare data
  ids <- rcv$id
  legData <- rcv %>%
    dplyr::select(id, session_number, district)
  
  # Merge in data on party, occupation, and year of birth
  if (sn < 26) {
    legData <- legData %>%
      left_join(dplyr::select(assemblymen, id, party, oclass, dob), by = "id") %>%
      as.data.frame()
  } else {
    legData <- legData %>%
      left_join(dplyr::select(assemblymen, id, party2, oclass, dob), by = "id") %>%
      rename(party = party2) %>%
      as.data.frame()
  }
  legData$party[is.na(legData$party)] <- "none"
  
  votes <- rcv %>%
    dplyr::select(starts_with("V")) %>%
    as.matrix()
  
  d <- smc(as.matrix(votes + 1), dist = TRUE)
  s <- as.matrix(1 - d)
  rownames(s) <- ids
  colnames(s) <- ids
  s[upper.tri(s, diag = T)] <- NA
  
  s2 <- s %>%
    melt() %>%
    filter(!is.na(value)) %>%
    rename("id.x" = "Var1", "id.y" = "Var2", "agreement" = "value")

  s2 <- merge(s2, legData, by.x = "id.x", by.y = "id", all.x = T, all.y = F)
  s2 <- merge(s2, legData, by.x = "id.y", by.y = "id", all.x = T, all.y = F)

  s2 <- s2 %>%
    mutate(
      same_party = ifelse((party.x == "F" & party.y == "F") | (party.x == "R" & party.y == "R"), 1, 0),
      same_district = ifelse(district.x == district.y, 1, 0),
      dob_diff = abs(dob.x - dob.y),  # 10-year difference
      same_occ = map2_chr(strsplit(oclass.x, ","), strsplit(oclass.y, ","), 
                          ~ paste(intersect(.x, .y), collapse=", ")),
      same_occ2 = ifelse(same_occ != "" & same_occ != "NA", 1, 0)
    )
  
  s2 %>%
    dplyr::select(party.x, party.y, same_party)
  
  # Run regression
  m1 <- lm(agreement ~ same_district + same_occ2 + same_party + dob_diff, data = s2)
  out2 <- data.frame("session_number" = sn, 
                     "type" = c("same_district", "same_occ", "same_party", "dob_diff"), 
                     "coeff" = c(m1$coefficients[2], m1$coefficients[3], m1$coefficients[4], m1$coefficients[5]),
                     "ll" = c(m1$coefficients[2] - 1.96* summary(m1)$coefficients[2,2], m1$coefficients[3] - 1.96* summary(m1)$coefficients[3,2], m1$coefficients[4] - 1.96* summary(m1)$coefficients[4,2], m1$coefficients[5] - 1.96* summary(m1)$coefficients[5,2]),
                     "ul" = c(m1$coefficients[2] + 1.96* summary(m1)$coefficients[2,2], m1$coefficients[3] + 1.96* summary(m1)$coefficients[3,2], m1$coefficients[4] + 1.96* summary(m1)$coefficients[4,2], m1$coefficients[5] + 1.96* summary(m1)$coefficients[5,2])
                     )
  
  return(out2)
})
out3 <- bind_rows(out1)

f.labs <- c("Difference in date of birth", "Same district", "Same occupation", "Same party")
names(f.labs) <- c("dob_diff", "same_district", "same_occ", "same_party")

# Create figure
ggplot(out3) +
  geom_line(aes(x = session_number, y = coeff), linewidth = 1) +
  geom_ribbon(aes(x = session_number, ymin = ll, ymax = ul), alpha = 0.2, fill = "black") +
  facet_wrap(~type, scales = "free_y", labeller = labeller(type = f.labs)) +
  #facet_wrap(~type, scales = "free_y") +
  labs(x = "Session Number", y = "Coefficient", linetype = NULL) +
  scale_x_continuous(breaks = c(12, 16, 20, 24), sec.axis = sec_axis(~.+1777, breaks = c(1789, 1793, 1797, 1801))) +
  theme_bw() +  
  theme(text = element_text(size = 15))
ggsave("output/figureB-3.tiff", height = 6, width = 9)



#========== FIGURE B-4 ==========#

for (rcv in rcv_list) {
  sn <- unique(rcv$session_number)
  message("Session: ", sn)
  
  # Prepare vote matrix
  ids <- rcv$id
  votes <- rcv %>%
    dplyr::select(starts_with("V")) %>%
    as.matrix()
  
  # Compute distance matrix
  d <- smc(votes + 1, dist = TRUE)
  rownames(d) <- ids
  colnames(d) <- ids
  d <- as.dist(d)
  
  # Hierarchical clustering
  hc1 <- hclust(d, method = "ward.D2")
  d_dendro <- as.dendrogram(hc1)
  d_order <- order.dendrogram(d_dendro)
  
  # Prepare long format for heatmap
  d_long <- melt(as.matrix(d))
  labs <- d_long$Var1
  
  d_long$Var1 <- factor(d_long$Var1, levels = labs[d_order], ordered = TRUE)
  d_long$Var2 <- factor(d_long$Var2, levels = labs[rev(d_order)], ordered = TRUE)
  
  # Customize dendrogram
  n_leaves <- length(labels(d_dendro))
  dend <- d_dendro %>%
    set("labels", rep("", n_leaves))
  
  # Convert dendrogram for ggplot
  ggd1 <- as.ggdend(dend)
  ggd2 <- ggplot(ggd1, theme = theme_dendro()) +
    coord_flip() +
    theme(axis.text.x = element_text(size = 10))

  # Heatmap
  heatmap_plot <- ggplot(data = d_long, aes(x = Var2, y = Var1)) +
    geom_tile(aes(fill = 1-value)) +
    scale_fill_gradient2(midpoint = .5, low = "#3A3A98", high = "#832424") +
    labs(x = NULL, y = NULL, fill = "Agreement") +
    theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 6),
          legend.position = "bottom")
  
  png(file = paste0("output/figureB-4_session_", sn, ".png"), width = 1200, height = 900)
  grid.newpage()
  print(heatmap_plot, 
        vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
  print(ggd2, 
        vp = viewport(x = 0.90, y = 0.535, width = 0.2, height = .99))
  dev.off()
  }



#========== FIGURE C-1 ==========#

for (rcv in rcv_list) {
  sn <- unique(rcv$session_number)
  message("Session: ", sn)
  
  # Prepare data
  votes <- rcv %>%
    dplyr::select(starts_with("V")) %>%
    as.matrix()
  legData <- rcv %>%
    dplyr::select(id, session_number)
  
  # Merge in party data
  if (sn < 26) {
    legData <- legData %>%
      left_join(dplyr::select(assemblymen, id, party), by = "id") %>%
      as.data.frame()
  } else {
    legData <- legData %>%
      left_join(dplyr::select(assemblymen, id, party2), by = "id") %>%
      rename(party = party2) %>%
      as.data.frame()
  }
  legData$party[is.na(legData$party)] <- "none"
  
  # calculate WNOMINATE coordinates
  rc <- rollcall(votes, legis.names = legData$id, legis.data = legData)
  result <- wnominate(rc, polarity = c(polarities$D1[polarities$session_number == sn], polarities$D2[polarities$session_number == sn]), minvotes = 10, lop = .025)
  coords <- result$legislators[, c("coord1D", "coord2D")]
  
  # get edge list with % agreement
  s <- smc(votes + 1, dist = FALSE)
  rownames(s) <- legData$id
  colnames(s) <- legData$id
  s[upper.tri(s, diag = TRUE)] <- NA
  e <- melt(s, na.rm = TRUE)
  names(e) <- c("Source", "Target", "weight")
  e$edge_width <- .1
  e$edge_width[e$weight >= mean(e$weight) + sd(e$weight)] <- .8
  e$edge_width[e$weight >= mean(e$weight) + 2 * sd(e$weight)] <- 2
  e$edge_alpha <- .5
  e$edge_alpha[e$weight >= mean(e$weight) + sd(e$weight)] <- .9
  e$edge_alpha[e$weight >= mean(e$weight) + 2 * sd(e$weight)] <- .9
  e <- filter(e, weight > .6)
  
  G <- graph_from_data_frame(e, directed = F, vertices = legData)
  
  # sort people in coords file so that same order as in igraph object
  coords_ordered <- as.matrix(coords[match(V(G)$name, rownames(coords)),])
  
  p1 <- ggnet2(G,
               mode = coords_ordered,
               size = 4,
               edge.size = "edge_width", edge.alpha = E(G)$edge_alpha,
               color = "party", palette = c("F" = "#08306B", "R" = "#FD8D3C", "both" = "#FFEDA0", "none" = "#D4B9DA"),
               shape = "party", shape.palette = c("F" = 19, "R" = 15, "both" = 17, "none" = 18)
  ) +
    ggtitle(paste0("Session ", sn, " (", dates[dates$sn == sn, 2], ")")) +
    labs(color = "Party", shape = "Party")
  ggsave(paste0("output/figureC-1_session_", sn, ".tiff"), p1, width = 7, height = 7)
}



#========== FIGURE C-2 ==========#

p_assembly_20 <- produce_figure(session = 20)
p_assembly_21 <- produce_figure(session = 21)

ggarrange(p_assembly_20, p_assembly_21,
          labels = c("A: NY Assembly, Session 20 (Nov 1796 - Apr 1797)", "B: NY Assembly, Session 21 (Jan 1798 - Apr 1798)"),
          ncol = 2, nrow = 1,
          common.legend = FALSE,
          #legend.grob = get_legend(p_assembly_24, position = "bottom"), legend = "bottom",
          label.x = c(-.255, -.28)
) 
ggsave("output/figureC-2.tiff", scale = 1.15, width = 12, height = 6, bg = "white")

