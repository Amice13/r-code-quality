library(ggridges)

mayor_v_mayor <- read.csv("data/mayor_vs_mayor_topic_space_4th_no_timelimit.csv")
mayor_v_mayor <- mayor_v_mayor[,-1]
mayor_v_mayor <- unlist(mayor_v_mayor, use.names = TRUE)

gov_v_gov <- read.csv("data/governor_vs_governor_topic_space_4th_no_timelimit.csv")
gov_v_gov <- gov_v_gov[,-1]
gov_v_gov <- unlist(gov_v_gov, use.names = FALSE)

cong_v_cong <- read.csv("data/congress_vs_congress_topic_space_4th_no_timelimit.csv")
cong_v_cong <- cong_v_cong[,-1]
cong_v_cong <- unlist(cong_v_cong, use.names = FALSE)

cong_v_mayor <- read.csv("data/congress_vs_mayor_topic_space_4th_no_timelimit.csv")
cong_v_mayor <- cong_v_mayor[,-1]
cong_v_mayor <- unlist(cong_v_mayor, use.names = FALSE)

cong_v_gov <- read.csv("data/congress_vs_governor_topic_space_4th_no_timelimit.csv")
cong_v_gov <- cong_v_gov[,-1]
cong_v_gov <- unlist(cong_v_gov, use.names = FALSE)


dist_dat <- data.frame(dist = c(mayor_v_mayor, gov_v_gov, cong_v_cong,
                                cong_v_mayor, cong_v_gov),
                       office = c(rep("Mayor vs. Mayor", length(mayor_v_mayor)),
                                  rep("Gov vs. Gov", length(gov_v_gov)),
                                  rep("Cong vs. Cong", length(cong_v_cong)),
                                  rep("Cong vs. Mayor", length(cong_v_mayor)),
                                  rep("Cong vs. Gov", length(cong_v_gov))))

median_diffs <- dist_dat %>%
  group_by(office) %>%
  summarize(median_diffs = round(median(dist, na.rm=T), digits = 3))


distplot <- ggplot(dist_dat, aes(dist, office)) + geom_density_ridges() +
  xlab("Tweets' distances in topic space") + ylab("Comparison") +
  theme_classic() + annotate("text", x = .45, y = 1.2, 
                             label = paste("Median:", 
                                           median_diffs$median_diffs[median_diffs$office=="Cong vs. Cong"], sep = " ")) +
  annotate("text", x = .45, y = 2.2,
           label = paste("Median:", 
                         median_diffs$median_diffs[median_diffs$office=="Cong vs. Gov"],
           sep = " ")) +
  annotate("text", x = .45, y = 3.2,
           label = paste("Median:", 
                         median_diffs$median_diffs[median_diffs$office=="Cong vs. Mayor"],
                         sep = " ")) +
  annotate("text", x = .45, y = 4.2,
           label = paste("Median:", 
                         median_diffs$median_diffs[median_diffs$office=="Gov vs. Gov"],
                         sep = " ")) +
  annotate("text", x = .45, y = 5.2,
           label = paste("Median:", 
                         median_diffs$median_diffs[median_diffs$office=="Mayor vs. Mayor"],
                         sep = " "))

print(distplot)

                                  