# Replication Files for Sacred Speech: Analyzing the Influence of Congressional Leadership on Religious Rhetoric

# 02_descriptive_analysis
# This file takes the "speeches" and "dcinbox" CSVs 
# It includes code to replicate Table 1 in the paper and Table 1 in the Appendix,
# as well as Figures 1 and 2 in the paper.
# It also includes any code used to determine significance in the "Descriptive
# Analysis" portion of the paper.

# Last updated July 29, 2024

# Initial settings -------------------------------------------------------------
my_packages <- c("dplyr","ggplot2","stargazer","lubridate") 
lapply(my_packages, require, character.only = TRUE) 

# Upload the data --------------------------------------------------------------
{df <- read.csv("R data/speeches.csv")
df_dc <- read.csv("R data/dcinbox.csv")}

# Table 1 (Paper) --------------------------------------------------------------

# Number & Percent of religious terms used in speeches
# Republicans
{tab1 = table(df$all_terms_binary[df$party == "R" & df$Year == 2023]);
table1 = rbind(tab1[2],prop.table(tab1)[2]*100);
tab2 = table(df$god_terms_binary[df$party == "R" & df$Year == 2023]);
table2 = rbind(tab2[2],prop.table(tab2)[2]*100);
tab3 = table(df$blessings_terms_binary[df$party == "R" & df$Year == 2023]);
table3 = rbind(tab3[2],prop.table(tab3)[2]*100);
tab4 = table(df$scripture_terms_binary[df$party == "R" & df$Year == 2023]);
table4 = rbind(tab4[2],prop.table(tab4)[2]*100);
tab5 = table(df$religion_terms_binary[df$party == "R" & df$Year == 2023]);
table5 = rbind(tab5[2],prop.table(tab5)[2]*100);
tab6 = table(df$political_terms_binary[df$party == "R" & df$Year == 2023]);
table6 = rbind(tab6[2],prop.table(tab6)[2]*100);
tab7 = table(df$whistles_terms_binary[df$party == "R" & df$Year == 2023]);
table7 = rbind(tab7[2],prop.table(tab7)[2]*100);
# Democrats
tab1_D = table(df$all_terms_binary[df$party == "D" & df$Year == 2023]);
table1_D = rbind(tab1_D[2],prop.table(tab1_D)[2]*100);
tab2_D = table(df$god_terms_binary[df$party == "D" & df$Year == 2023]);
table2_D = rbind(tab2_D[2],prop.table(tab2_D)[2]*100);
tab3_D = table(df$blessings_terms_binary[df$party == "D" & df$Year == 2023]);
table3_D = rbind(tab3_D[2],prop.table(tab3_D)[2]*100);
tab4_D = table(df$scripture_terms_binary[df$party == "D" & df$Year == 2023]);
table4_D = rbind(tab4_D[2],prop.table(tab4_D)[2]*100);
tab5_D = table(df$religion_terms_binary[df$party == "D" & df$Year == 2023]);
table5_D = rbind(tab5_D[2],prop.table(tab5_D)[2]*100);
tab6_D = table(df$political_terms_binary[df$party == "D" & df$Year == 2023]);
table6_D = rbind(tab6_D[2],prop.table(tab6_D)[2]*100);
tab7_D = table(df$whistles_terms_binary[df$party == "D" & df$Year == 2023]);
table7_D = rbind(tab7_D[2],prop.table(tab7_D)[2]*100);

rep_table_speeches <- cbind(table2,table3,table4,table5,table6,table7,table1);
dem_table_speeches <- cbind(table2_D,table3_D,table4_D,table5_D,table6_D,table7_D,table1_D);
}

# Number & Percent of religious terms used in House speeches
df_reps <- df[df$Year == 2023,] %>%
  group_by(bioguide_id, rep) %>%
  summarize(all_terms_binary = max(all_terms_binary),
            god_terms_binary = max(god_terms_binary),
            blessings_terms_binary = max(blessings_terms_binary),
            scripture_terms_binary = max(scripture_terms_binary),
            religion_terms_binary = max(religion_terms_binary),
            political_terms_binary = max(political_terms_binary),
            whistles_terms_binary = max(whistles_terms_binary)) 

{# Republicans
  tab1 = table(df_reps$all_terms_binary[df_reps$rep == 1]);
  table1 = rbind(tab1[2],prop.table(tab1)[2]*100);
  tab2 = table(df_reps$god_terms_binary[df_reps$rep == 1]);
  table2 = rbind(tab2[2],prop.table(tab2)[2]*100);
  tab3 = table(df_reps$blessings_terms_binary[df_reps$rep == 1]);
  table3 = rbind(tab3[2],prop.table(tab3)[2]*100);
  tab4 = table(df_reps$scripture_terms_binary[df_reps$rep == 1]);
  table4 = rbind(tab4[2],prop.table(tab4)[2]*100);
  tab5 = table(df_reps$religion_terms_binary[df_reps$rep == 1]);
  table5 = rbind(tab5[2],prop.table(tab5)[2]*100);
  tab6 = table(df_reps$political_terms_binary[df_reps$rep == 1]);
  table6 = rbind(tab6[2],prop.table(tab6)[2]*100);
  tab7 = table(df_reps$whistles_terms_binary[df_reps$rep == 1]);
  table7 = rbind(tab7[2],prop.table(tab7)[2]*100);
  # Democrats
  tab1_D = table(df_reps$all_terms_binary[df_reps$rep == 0]);
  table1_D = rbind(tab1_D[2],prop.table(tab1_D)[2]*100);
  tab2_D = table(df_reps$god_terms_binary[df_reps$rep == 0]);
  table2_D = rbind(tab2_D[2],prop.table(tab2_D)[2]*100);
  tab3_D = table(df_reps$blessings_terms_binary[df_reps$rep == 0]);
  table3_D = rbind(tab3_D[2],prop.table(tab3_D)[2]*100);
  tab4_D = table(df_reps$scripture_terms_binary[df_reps$rep == 0]);
  table4_D = rbind(tab4_D[2],prop.table(tab4_D)[2]*100);
  tab5_D = table(df_reps$religion_terms_binary[df_reps$rep == 0]);
  table5_D = rbind(tab5_D[2],prop.table(tab5_D)[2]*100);
  tab6_D = table(df_reps$political_terms_binary[df_reps$rep == 0]);
  table6_D = rbind(tab6_D[2],prop.table(tab6_D)[2]*100);
  tab7_D = table(df_reps$whistles_terms_binary[df_reps$rep == 0]);
  table7_D = rbind(tab7_D[2],prop.table(tab7_D)[2]*100);
  
  rep_table_reps <- cbind(table2,table3,table4,table5,table6,table7,table1);
  dem_table_reps <- cbind(table2_D,table3_D,table4_D,table5_D,table6_D,table7_D,table1_D);
  }

# Combining the two tables
{tots_table <- rbind(rep_table_speeches,rep_table_reps,
                     dem_table_speeches,dem_table_reps);
  
  colnames(tots_table) <- c("God","Blessings","Scripture","Religion","Political",
                            "Whistles","Total");
  rownames(tots_table) <- c("Speeches","%","Representatives","%",
                            "Speeches","%","Representatives","%");
  tots_table_rounded <- round(tots_table,0);
}

# Creating the table 
stargazer(tots_table_rounded)


# Table 1 (Appendix) -----------------------------------------------------------

# Number & Percent of religious terms used in newsletters
# Republicans
{tab1 = table(df_dc$all_terms_binary[df_dc$rep == 1 & df_dc$Year == 2023]);
  table1 = rbind(tab1[2],prop.table(tab1)[2]*100);
  tab2 = table(df_dc$god_terms_binary[df_dc$rep == 1 & df_dc$Year == 2023]);
  table2 = rbind(tab2[2],prop.table(tab2)[2]*100);
  tab3 = table(df_dc$blessings_terms_binary[df_dc$rep == 1 & df_dc$Year == 2023]);
  table3 = rbind(tab3[2],prop.table(tab3)[2]*100);
  tab4 = table(df_dc$scripture_terms_binary[df_dc$rep == 1 & df_dc$Year == 2023]);
  table4 = rbind(tab4[2],prop.table(tab4)[2]*100);
  tab5 = table(df_dc$religion_terms_binary[df_dc$rep == 1 & df_dc$Year == 2023]);
  table5 = rbind(tab5[2],prop.table(tab5)[2]*100);
  tab6 = table(df_dc$political_terms_binary[df_dc$rep == 1 & df_dc$Year == 2023]);
  table6 = rbind(tab6[2],prop.table(tab6)[2]*100);
  tab7 = table(df_dc$whistles_terms_binary[df_dc$rep == 1 & df_dc$Year == 2023]);
  table7 = rbind(tab7[2],prop.table(tab7)[2]*100);
  # Democrats
  tab1_D = table(df_dc$all_terms_binary[df_dc$rep == 0 & df_dc$Year == 2023]);
  table1_D = rbind(tab1_D[2],prop.table(tab1_D)[2]*100);
  tab2_D = table(df_dc$god_terms_binary[df_dc$rep == 0 & df_dc$Year == 2023]);
  table2_D = rbind(tab2_D[2],prop.table(tab2_D)[2]*100);
  tab3_D = table(df_dc$blessings_terms_binary[df_dc$rep == 0 & df_dc$Year == 2023]);
  table3_D = rbind(tab3_D[2],prop.table(tab3_D)[2]*100);
  tab4_D = table(df_dc$scripture_terms_binary[df_dc$rep == 0 & df_dc$Year == 2023]);
  table4_D = rbind(tab4_D[2],prop.table(tab4_D)[2]*100);
  tab5_D = table(df_dc$religion_terms_binary[df_dc$rep == 0 & df_dc$Year == 2023]);
  table5_D = rbind(tab5_D[2],prop.table(tab5_D)[2]*100);
  tab6_D = table(df_dc$political_terms_binary[df_dc$rep == 0 & df_dc$Year == 2023]);
  table6_D = rbind(tab6_D[2],prop.table(tab6_D)[2]*100);
  tab7_D = table(df_dc$whistles_terms_binary[df_dc$rep == 0 & df_dc$Year == 2023]);
  table7_D = rbind(tab7_D[2],prop.table(tab7_D)[2]*100);
  
  rep_table_speeches <- cbind(table2,table3,table4,table5,table6,table7,table1);
  dem_table_speeches <- cbind(table2_D,table3_D,table4_D,table5_D,table6_D,table7_D,table1_D);
}

# Looking at the number of Representatives who used terms in newsletters
df_dc_reps <- df_dc[df_dc$Year == 2023,] %>%
  group_by(BioGuide.ID, rep) %>%
  summarize(all_terms_binary = max(all_terms_binary),
            god_terms_binary = max(god_terms_binary),
            blessings_terms_binary = max(blessings_terms_binary),
            scripture_terms_binary = max(scripture_terms_binary),
            religion_terms_binary = max(religion_terms_binary),
            political_terms_binary = max(political_terms_binary),
            whistles_terms_binary = max(whistles_terms_binary)) 

{# Making the table
  ## Republicans
  tab1 = table(df_dc_reps$all_terms_binary[df_dc_reps$rep == 1]);
  table1 = rbind(tab1[2],prop.table(tab1)[2]*100);
  tab2 = table(df_dc_reps$god_terms_binary[df_dc_reps$rep == 1]);
  table2 = rbind(tab2[2],prop.table(tab2)[2]*100);
  tab3 = table(df_dc_reps$blessings_terms_binary[df_dc_reps$rep == 1]);
  table3 = rbind(tab3[2],prop.table(tab3)[2]*100);
  tab4 = table(df_dc_reps$scripture_terms_binary[df_dc_reps$rep == 1]);
  table4 = rbind(tab4[2],prop.table(tab4)[2]*100);
  tab5 = table(df_dc_reps$religion_terms_binary[df_dc_reps$rep == 1]);
  table5 = rbind(tab5[2],prop.table(tab5)[2]*100);
  tab6 = table(df_dc_reps$political_terms_binary[df_dc_reps$rep == 1]);
  table6 = rbind(tab6[2],prop.table(tab6)[2]*100);
  tab7 = table(df_dc_reps$whistles_terms_binary[df_dc_reps$rep == 1]);
  table7 = rbind(tab7[2],prop.table(tab7)[2]*100);
  # Democrats
  tab1_D = table(df_dc_reps$all_terms_binary[df_dc_reps$rep == 0]);
  table1_D = rbind(tab1_D[2],prop.table(tab1_D)[2]*100);
  tab2_D = table(df_dc_reps$god_terms_binary[df_dc_reps$rep == 0]);
  table2_D = rbind(tab2_D[2],prop.table(tab2_D)[2]*100);
  tab3_D = table(df_dc_reps$blessings_terms_binary[df_dc_reps$rep == 0]);
  table3_D = rbind(tab3_D[2],prop.table(tab3_D)[2]*100);
  tab4_D = table(df_dc_reps$scripture_terms_binary[df_dc_reps$rep == 0]);
  table4_D = rbind(tab4_D[2],prop.table(tab4_D)[2]*100);
  tab5_D = table(df_dc_reps$religion_terms_binary[df_dc_reps$rep == 0]);
  table5_D = rbind(tab5_D[2],prop.table(tab5_D)[2]*100);
  tab6_D = table(df_dc_reps$political_terms_binary[df_dc_reps$rep == 0]);
  table6_D = rbind(tab6_D[2],prop.table(tab6_D)[2]*100);
  tab7_D = table(df_dc_reps$whistles_terms_binary[df_dc_reps$rep == 0]);
  table7_D = rbind(tab7_D[2],prop.table(tab7_D)[2]*100);
  
  rep_table_reps <- cbind(table2,table3,table4,table5,table6,table7,table1);
  dem_table_reps <- cbind(table2_D,table3_D,table4_D,table5_D,table6_D,table7_D,table1_D);
  }


# Combining the two tables
{tots_table <- rbind(rep_table_speeches,rep_table_reps,
                     dem_table_speeches,dem_table_reps);
  
  colnames(tots_table) <- c("God","Blessings","Scripture","Religion","Political",
                            "Whistles","Total");
  rownames(tots_table) <- c("Newsletters","X","Representatives","X",
                            "Newsletters","X","Representatives","X");
  tots_table_rounded <- round(tots_table,0);
}

# Making the table
stargazer(tots_table_rounded)


# Statistical Findings ---------------------------------------------------------
# Difference between Republican and Democratic use of religious terms
t.test(all_terms_binary ~ rep, data = df[df$Year == 2023,])
t.test(all_terms_binary ~ rep, data = df_reps)

# Comparing religious usage between Republicans and Democrats
# Aggregate the counts by person
total_counts <- df[df$Year == 2023, ] %>%
  dplyr::group_by(bioguide_id, rep) %>%
  dplyr::summarise(total_count = sum(all_terms_count)) %>%
  dplyr::ungroup()

t.test(total_count ~ rep, data = total_counts)


# Comparing Dates --------------------------------------------------------------

# Checking which date in 2023 had the most religious terms
total_counts_date <- df[df$Year == 2023,] %>%
  dplyr::group_by(full_dates) %>%
  dplyr::summarise(total_count = sum(all_terms_count)) %>%
  dplyr::ungroup()

# Arrange in descending order and select the top 10
top_10 <- total_counts_date %>%
  arrange(desc(total_count)) %>%
  head(20)
top_10


# Figure 1 ---------------------------------------------------------------------
# Setting up the data
{# Limit data to only 2023
  plot_df <- df[df$Year == 2023, ]
  plot_df$full_dates <- as.Date(plot_df$date, format = "%m/%d/%y")

  # Add a new column for weeks
  plot_df <- plot_df %>%
    mutate(month = lubridate::floor_date(full_dates, unit = "month"))
}

# Creating the plot
p <- ggplot(plot_df %>%
              group_by(party, month) %>%
              summarise(terms_total = mean(all_terms_count)),
            aes(x = month, y = terms_total, fill = party)) 

p + geom_bar(stat = "identity", position = "dodge", width = 20, color = "black") +
  theme_minimal() +
  scale_fill_manual(values = c("gray90","gray20"), 
                    labels = c("Democrat","Republican")) +
  labs(title = "Use of Religious Terms, House Speeches", 
       x = "Date", y = "Average Religious Terms per Speech",
       fill = "Party") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.2)) +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA))


# Figure 2 ---------------------------------------------------------------------
{# Setting up data
  plot_dc <- df_dc[df_dc$Year == 2023, ]
  plot_dc$party <- ifelse(plot_dc$Party == "Republican", "R", 
                          ifelse(plot_dc$Party == "Democrat", "D", NA))
  # Fixing issue with the date
  plot_dc$Unix.Timestamp <- as.numeric(plot_dc$Unix.Timestamp)
  plot_dc$full_dates <- as.Date(as.POSIXct(plot_dc$Unix.Timestamp/1000, origin="1970-01-01"))
  
  # Add a new column for weeks (or bi-weeks)
  plot_dc <- plot_dc %>%
    mutate(month = lubridate::floor_date(full_dates, unit = "month"))
  }

# Making the histogram: average religious newsletters
p1 <- ggplot(plot_dc %>%
               group_by(party, month) %>%
               summarise(terms_total = mean(all_terms_count)),
             aes(x = month, y = terms_total, fill = party)) 

p1 + geom_bar(stat = "identity", position = "dodge", width = 20, color = "black") +
  theme_minimal() +
  scale_fill_manual(values = c("gray90","gray20"), 
                    labels = c("Democrat","Republican")) +
  labs(title = "Use of Religious Terms, Representatives' Newsletters", 
       x = "Date", y = "Average Religious Terms per Newsletter",
       fill = "Party") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.2)) +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA))



