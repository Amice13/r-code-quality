#########################################################
#                                                       #
#Date created:  November 28, 2021                       #
#Created by: Chinemelu Okafor and Shivram Viswanathan   #
#Date updated:                                          #
#Updated by:                                            #
#Notes:                                                 #
#This code uses the cleaned Afrobarometer data          #
# "ReplicationPaper_Afrobarometer_Cleaned.rds" to create#
# ELF, CF and FST measures. It returns both a dataset of# 
# measures for all countries and of Benin and Senegal   #
# separately.                                           #
#                                                       #
#########################################################

####File Inputs####
input_Benin_Senegal_Measures_rds <- 'data/clean/Benin_Senegal_Measures.rds'  
input_All_Country_Measures_rds <- 'data/clean/All_Country_Measures.rds' 


####File Outputs####
output_All_Country_Scatterplot <- 'output/All_Country_Scatterplot.png'
output_All_country_bargraph_EF <- 'output/All_country_bargraph_EF.png'
output_All_country_bargraph_CF <- 'output/All_country_bargraph_CF.png'

####Reading in Files####
All_Country_measures <- readRDS(input_All_Country_Measures_rds)
Benin_Senegal_measure <- readRDS(input_Benin_Senegal_Measures_rds)

#3. Hypothesis test
####Defining our lists####
#All Country Names
countname <- c("Benin", "Botswana", "Burkina Faso", "Cape Verde", "Ghana", "Kenya",
               "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique",
               "Namibia", "Nigeria", "Senegal", "South Africa", "Tanzania", "Uganda",
               "Zambia", "Zimbabwe", "Mauritius", "Sierra Leona", "Niger", "Togo",
               "Burundi", "Cameroon", "Cote d'Ivoire", "Guinea", "Ethiopia",
               "Swaziland", "Algeria", "Egypt", "Morocco", "Sudan", "Tunisia")

####Country Scatter plot####
#Collapsing data
All_Country_measuresf <- All_Country_measures %>% select(c(country, countryname, countrycode, ELF, CF, FST))
All_Country_measuresf <- All_Country_measuresf %>%
  group_by(countryname, countrycode) %>%
  summarise(FST = mean(FST), CF = mean(CF), ELF = mean(ELF))

#Plotting data
scatterplot <- All_Country_measuresf %>%
  ggplot(aes(x = ELF, y = FST)) +
  geom_point(data = All_Country_measuresf[All_Country_measuresf$countryname %in% countname,], color = "black", size = 1) +
  geom_point(data = All_Country_measuresf[All_Country_measuresf$countryname %in% c("Benin", "Senegal"),], color = "red", size = 1) +
  geom_text_repel(aes(label = countryname), size = 2) +
  theme_apa() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

scatterplot <- scatterplot + 
  labs(y = "Fixtation Index", x = "Ethnolinguistic Fractionalization Index")

print(scatterplot)

#Saving scatterplot (4x6)
ggsave(output_All_Country_Scatterplot,   #The directory you want to save the file in
    width = 6, #The width of the plot
    height = 4 #The height of the plot
    ) 

####ELF bar graph####
bargraph_EF <- All_Country_measuresf %>% ggplot(aes(x = reorder(countryname, ELF), y = ELF, fill = countryname)) + 
  geom_bar(color = "gray13", fill = "gray13", stat = "identity", width = 0.6,) +
  scale_fill_manual(values = c("SEN" = "red")) +
  theme_apa() +
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle=45, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank()) 

bargraph_EF <- bargraph_EF + 
  labs(y = "Ethnolinguistic Fractionalization Index")

print(bargraph_EF)

#Save bargraph (6x10)
ggsave(output_All_country_bargraph_EF,   # The directory you want to save the file in
    width = 10, #The width of the plot
    height = 6 #The height of the plot
    ) 
#Note: Senegal and Bening bars highlighted using Photoshop

####CF bar graph####
bargraph_CF <- All_Country_measuresf %>% ggplot(aes(x = reorder(countryname, CF), y = CF, fill = countryname)) + 
  geom_bar(color = "gray13", fill = "gray13", stat = "identity", width = 0.6,) +
  scale_fill_manual(values = c("SEN" = "red")) +
  theme_apa() +
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle=45, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank()) 

bargraph_CF <- bargraph_CF + 
  labs(y = "Cultural Fractionalization Index")

print(bargraph_CF)

#Save bargraph (6x10)
ggsave(output_All_country_bargraph_CF,   # The directory you want to save the file in
    width = 10, #The width of the plot
    height = 6 #The height of the plot
    ) 

#Note: Senegal and Benin bars highlighted using Photoshop

####Benin and Senegal Hypothesis tests####
#Split Benin and senegal dataframes
Ben_measures <- Benin_Senegal_measure %>% filter(country == 1) #192 observations
Sen_measures <- Benin_Senegal_measure %>% filter(country == 15) #192 observations

#Testing difference in mean ELF, CF and FST
ELF_ttest <- t.test(Ben_measures$ELF, Sen_measures$ELF) 
CF_ttest <- t.test(Ben_measures$cultelf, Sen_measures$cultelf)
FST_ttest <- t.test(Ben_measures$fst, Sen_measures$fst)

measures_ben_sen <- tibble(Country = c("Benin", "Senegal"),
                           ELF = c(Ben_measures[1,9],Sen_measures[1,9]),
                           CF = c(Ben_measures[1,10],Sen_measures[1,10]),
                           FST = c(Ben_measures[1,11], Sen_measures[1,11])) 
Ben_Sen_ttest <- xtable(measures_ben_sen)
