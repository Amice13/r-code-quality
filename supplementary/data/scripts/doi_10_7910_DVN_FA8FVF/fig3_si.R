###############################################################################################
# Appendix Tables for Appendix Summarizing the Measures on Whole Data Set of Districts
###############################################################################################

print("Running fig3_si.R...")

###############################################################################################
# Setup
###############################################################################################
# Functions
  splitit <- function(x,splitchar,n) sapply(strsplit(as.character(x), splitchar), "[", n)
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  
# Packages
  require(data.table)
  require(ggplot2)
  require(maps)
  
  data(state.fips)
  
# Load Data
 load("../results/preds.RData")

###############################################################################################
# Formatting
###############################################################################################
# Break off Chamber, District Year, State
  finalpreds$year <- splitit(finalpreds$district, "_", 4)
  finalpreds$statefip <-splitit(finalpreds$district, "_", 1)
  finalpreds$chamber <- splitit(finalpreds$district, "_", 2)
  
###############################################################################################
# Summary Tables/Plots
###############################################################################################
# Looking for Interesting Trends or Facts to Present
  
# Starting by Looking at Time Trends in Canonically Gerrymandered States
# So what if I show that trends in the canonically most gerrymandered states are actually stable
# and their ranks are rising? This isn't what Steve and Max do because they are all about (1) 
# adjusting estimates to reflect coastlines, etc. - and we don't do that because people
# can ostensibly perceive this and (2) they don't show you statewide results at all

# 37 = NC, 24 = MD, 42 = PA, 54 = WV
  
  gerrys <- finalpreds[finalpreds$statefip %in% c("37", "24", "42", "39") & finalpreds$chamber == "C", 
                     c("district", "compactness", "year", "statefip", "chamber")]
  
# Bootstrap compactness in narrow bandwidths to get "sampling distributions" for the means
  set.seed(02138)
  bdists <- NULL
  for(i in 1:nrow(gerrys)){
    temp <- sample(finalpreds$compactness[finalpreds$compactness >= (gerrys$compactness[i] - 5) & 
                                             finalpreds$compactness <= (gerrys$compactness[i] + 5)], 100, replace = T)
    bdists <- rbind(bdists, temp)
  }
  
  gerrys <- data.frame(cbind(gerrys, bdists))
  
  out <- data.table(gerrys)
  
  out <- out[year %in% names(table(gerrys$year)),
             lapply(.SD, mean), .SDcols = 6:ncol(out),
             by = list(year, statefip)]
  
  out <- as.data.frame(out)
  out$pointest <- rowMeans(out[,3:ncol(out)])
  out$lbound95 <- apply(out[,3:ncol(out)], 1,function(x) quantile(x, 0.025))
  out$ubound95 <- apply(out[,3:ncol(out)], 1,function(x) quantile(x, 0.975))
  out$lbound90 <- apply(out[,3:ncol(out)], 1,function(x) quantile(x, 0.25))
  out$ubound90 <- apply(out[,3:ncol(out)], 1,function(x) quantile(x, 0.75))
  
  
  out$state <- ifelse(out$statefip == 24, "Maryland",
                      ifelse(out$statefip == 37, "North Carolina", 
                             ifelse(out$statefip == 42, "Pennsylvania", "Ohio")))
  
  yearlabs <- as.character(sort(as.numeric(unique(out$year))))
  yearlabs <- ifelse(splitit(yearlabs, '', 4) != "3", "", yearlabs)
  
# Plot Average Compactness

  ggplot(data = out, aes(x = year, y = pointest, group = state, colour = state)) +
    geom_errorbar(data = out, mapping = aes(x = year, ymin = lbound95, ymax = ubound95), width = 0, size = 0.75) +
    geom_errorbar(data = out, mapping = aes(x = year, ymin = lbound90, ymax = ubound90), width = 0, size = 1.75) +
    geom_point(data = out, mapping = aes(x = year, y = pointest, fill = state, color = state), size = 3, shape = 20) + 
    geom_line(data = out, mapping = aes(x = year, y = pointest, fill = state, color = state)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(panel.background = element_rect(fill = "white", colour = 'white'))+
    scale_x_discrete(breaks = sort(as.numeric(unique(out$year))), labels = yearlabs) + 
    scale_color_manual(values = c("dodgerblue2", "mediumpurple3", "orange1", "palegreen3")) + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_text(size = rel(1.3)), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
    ylab("Noncompactness") +
    ggplot2::annotate("text", x = "1901", y = 70, label = "Maryland", colour = "dodgerblue2") +
    ggplot2::annotate("text", x = "1901", y = 55, label = "North Carolina", colour = "mediumpurple3") +
    ggplot2::annotate("text", x = "1901", y = 38, label = "Ohio", colour = "orange1") +
    ggplot2::annotate("text", x = "1901", y = 45, label = "Pennsylvania", colour = "palegreen3")
ggsave("../results/gerrys_ranks.pdf")