# Title: The R code for generating recommendation stats
# Author: Raghvendra Jain
# Maintainer: jain@nii.ac.jp
# Description: Functions for generating different statistics. 

library(readr)
#library(colorspace)


stats <- function(data){
      str(data)
      head(data)
      
      # changing all the classes to the factor
      data[] <- lapply(data, function(x) as.factor(as.character(x)))
      attach(data)
      
      ## "clicked data"
      data$clicked <- as.character(data$clicked)
      data$clicked <- gsub("\\N", NA, data$clicked)
      
      ## Recommendation id should be integer
      
      # data$recommendation_id <- as.integer(data$recommendation_id)
      
      ## Lets see whats inside the data
      
      names(data)
      
      # [1] "recommendation_id"           "requested_document_language" "abstract_language"          
      # [4] "algorithm_id"                "recommendation_class"        "cbf_text_fields"            
      # [7] "cbf_feature_type"            "cbf_feature_count"           "language_filter"            
      # [10] "category"                    "bibliometric_id"             "reRankingMethod"            
      # [13] "metric"                      "reRankedCandidates"          "clicked"                    
      # [16] "processingTime"             
      
      
      ## Subset the data based on recommendation if
      
      # data <- subset(data, recommendation_id > 12500000)
      
      
      ## There are 4 recommendation classes
      recommendation_class
      table(recommendation_class)
      table(cbf_feature_type)
      
      #fallback
      df_fallback    <- subset(data, recommendation_class == "fallback")
      fallback_Count <- nrow(df_fallback)
      fallback_clicks <- sum(!is.na(df_fallback$clicked))
      
      #cbf
      
      df_cbf <- subset(data, recommendation_class == "cbf")
      cbf_Count <- nrow(df_cbf)
      
      ##cbf and terms 
      df_cbf_terms<-subset(data, recommendation_class == "cbf" & cbf_feature_type == "terms")
      term_cbf_Count <- nrow(df_cbf_terms)
      term_cbf_clicks <- sum(!is.na(df_cbf_terms$clicked))
      
      ##cbf and keyphrase
      
      df_keyphrases_cbf <- df_cbf[grep("keyphrase_", df_cbf$cbf_feature_type),]
      keyphrases_cbf_Count <- nrow(df_keyphrases_cbf)
      keyphrases_cbf_clicks <- sum(!is.na(df_keyphrases_cbf$clicked))
      
      
      
      ## cbf overall average
      
      cbf_all <- cbind(fallback_Count, term_cbf_Count,keyphrases_cbf_Count)
      cbf_clicks <- cbind(fallback_clicks, term_cbf_clicks, keyphrases_cbf_clicks)
      
      
      # random 
      df_random   <- subset(data, recommendation_class == "random")
      random_Count <- nrow(df_random)
      random_clicks <- sum(!is.na(df_random$clicked))
      
      # stereotype
      df_stereotype  <- subset(data, recommendation_class == "stereotype")
      stereotype_Count <- nrow(df_stereotype)
      
      
      
      ## stereotypes and categories
      # 1. 
      table(df_stereotype$category)
      
      academic_writing_df <- subset(df_stereotype, category == "academic_writing")
      academic_writing_Count <- nrow(academic_writing_df)
      academic_writing_clicks <- sum(!is.na(academic_writing_df$clicked))
      
      research_methods_df <- subset(df_stereotype, category == "research_methods")
      research_methods_Count <- nrow(research_methods_df)
      research_methods_clicks <- sum(!is.na(research_methods_df$clicked))
      
      
      research_evaluation_df <- subset(df_stereotype, category == "research_evaluation_and_peer_review")
      research_evaluation_Count <- nrow(research_evaluation_df)
      research_evaluation_clicks <- sum(!is.na(research_evaluation_df$clicked))
      
      
      sterotype_all <- cbind(academic_writing_Count, research_methods_Count, research_evaluation_Count)
      sterotype_clicks <- cbind(academic_writing_clicks, research_methods_clicks, research_evaluation_clicks)
      
      ## Top exported
      top_exported_df <- subset(df_stereotype, category == "top_exported")
      top_exported_Count <- nrow(top_exported_df)
      top_exported_clicks <- sum(!is.na(top_exported_df$clicked))
      
      
      
      ## Top Views
      
      top_views_df <- subset(df_stereotype, category == "top_views")
      top_views_Count <- nrow(top_views_df)
      top_views_clicks <- sum(!is.na(top_views_df$clicked))
      
      
      ## Total Most Popular
      mostPopular_all <- cbind(top_exported_Count, top_views_Count)
      mostPopular_clicks <- cbind(top_exported_clicks, top_views_clicks)
      
      
      ### Getting all the stats
      
      df_stats <- function(df1, df2){
        df <- data.frame()
        df <- rbind(df1, df2)
        df <- as.data.frame(df)
        df$overall <- rowSums(df)
        row.names(df) <- c("Views", "Clicks")
        df["CTR", ] <- (df["Clicks", ]/ df["Views", ]) * 100.0
        df
      }
      
      options(digits=3)
      
      
      ## for Content based Filtering
      cbf_stats <- df_stats(cbf_all, cbf_clicks)
      colnames(cbf_stats) <- c("Fallback", "CBF", "KeyPhrases", "overall")
      write.csv(cbf_stats, "cbf_stats.csv", sep=";")
      
      
      ## for Stereotypes
      stereotype_stats <- df_stats(sterotype_all, sterotype_clicks)
      colnames(stereotype_stats) <- c("academic_writing", "research_methods", "research_evaluation_and_peer_review", "overall")
      write.csv(stereotype_stats, "stereotype_stats.csv", sep=";")
      
      ## Popular Stats
      popular_stats <- df_stats(mostPopular_all, mostPopular_clicks)
      colnames(popular_stats) <- c("top_exported", "top_views", "overall")
      write.csv(popular_stats, "popular_stats.csv", sep=";")
      
      
      random_stats <- df_stats(random_Count, random_clicks)
      random_stats$overall <- NULL
      colnames(random_stats) <- "Random"
      write.csv(random_stats, "random_stats.csv", sep=";")
      
      
      cbf_stats$Fallback <- NULL
      cbf_stats <- as.matrix(cbf_stats)
      stereotype_stats <- as.matrix(stereotype_stats)
      popular_stats  <- as.matrix(popular_stats)
      random_stats <- as.matrix(random_stats)
      
      
      
      overall <- c(cbf_stats[3,"overall"], stereotype_stats[3,"overall"], popular_stats[3,"overall"], 
                   random_stats[3,"Random"] )
      
  
      png("overall_fig.png", width=1280, height=580, res=120)
      barplot(overall, names.arg = c("Content Based Filtering", "Stereotype", "Most Popular", "Random" ),
              col=c("cyan","blue","green","orange"),
              density=c(10,20,25,30),
              angle=c(0,45,135,90),
              ylim = c(0,0.2),
              ylab = "CTR (%)",
              main = "Overall")
      text(0.7, overall[1] + 0.01, paste0(round(overall[1],3),"%"))
      text(1.9, overall[2] + 0.01, paste0(round(overall[2],3),"%"))
      text(3.1, overall[3] + 0.01, paste0(round(overall[3],3),"%"))
      text(4.3, overall[4] + 0.01, paste0(round(overall[4],3),"%"))
      dev.off()
      
      
      png("cbf_fig.png", width=1280, height=740, res=120)
      barplot(cbf_stats[3,],
              names.arg = c("CBF", "KeyPhrases", "Overall"),
              col = c("cyan1", "cyan4", "darkcyan"),
              density=c(10,20,25),
              angle=c(45,90,135),
              ylim = c(0, 0.20 ),
              ylab = "CTR (%)",
              main = "Content Based Filtering" )
      text(0.7, cbf_stats[3,1] + 0.01, paste0(round(cbf_stats[3,1],3),"%"))
      text(1.9, cbf_stats[3,2] + 0.01, paste0(round(cbf_stats[3,2],3),"%"))
      text(3.1, cbf_stats[3,3] + 0.01, paste0(round(cbf_stats[3,3],3),"%"))
      dev.off()
      
      
      png("stereotype_fig.png", width=1280, height=420, res=120)
      barplot(stereotype_stats[3,], 
              names.arg = c("Academic Writing", 
                            "Research Methods", 
                            "Research Evaluation & Peer Review",
                            "Overall"),
              col = c("blue1", "blue3", "blue4", "darkblue"),
              density=c(10,20,25, 30),
              angle=c(0,45,90,135),
              ylim = c(0, 0.175),
              ylab = "CTR (%)",
              main = "Stereotype")
      text(0.7, stereotype_stats[3,1] + 0.01, paste0(round(stereotype_stats[3,1],3),"%"))
      text(1.9, stereotype_stats[3,2] + 0.01, paste0(round(stereotype_stats[3,2],3),"%"))
      text(3.1, stereotype_stats[3,3] + 0.01, paste0(round(stereotype_stats[3,3],3),"%"))
      text(4.3, stereotype_stats[3,4] + 0.01, paste0(round(stereotype_stats[3,4],3),"%"))
      dev.off()
      
      
      png("popular_fig.png", width=1280, height=420, res=120)
      barplot(popular_stats[3,],  
              names.arg = c("Top Exported", 
                            "Top Views",
                            "Overall"),
              col = c("chartreuse2", "chartreuse3", "darkgreen"),
              density=c(10,20,25),
              angle=c(45,90,135),
              ylim = c(0, 0.15),
              ylab = "CTR (%)",
              main = "Most Popular")
      text(0.7, popular_stats[3,1] + 0.01, paste0(round(popular_stats[3,1],3), "%"))
      text(1.9, popular_stats[3,2] + 0.01, paste0(round(popular_stats[3,2],3), "%"))
      text(3.1, popular_stats[3,3] + 0.01, paste0(round(popular_stats[3,3],3), "%"))
      dev.off()
      
      
      entireStats <- list(cbf_stats, stereotype_stats, popular_stats, random_stats)
      entireStats

}


full_data <- read_delim("full-data.csv", ";", escape_double = FALSE, trim_ws = TRUE)

before <- nrow(full_data) ## The number of entries with all processing times
cat("The number of entries with all processing times:", before)

reduced_data <- subset(full_data, processingTime <= 3) ## 
after <- nrow(reduced_data ) ## The no of entries with reduced time 
cat("The number of entries with all processing times less than 4 seconds: ", after)


data <- as.data.frame(reduced_data)
listOfStats <-  stats(data)

## The range of recommendation ids

minRecoId <- min(reduced_data$recommendation_id)
cat("Minimum Recommendation id is:", minRecoId)

maxRecId <- max(reduced_data$recommendation_id)
cat("Maximum Recommendation id is:", maxRecId)

## The response time variable 
minResTime <- min(reduced_data$response_delivered)
paste0("The earliest response was delivered on: ", minResTime)

latestResTime <- min(reduced_data$response_delivered)
paste0("The latest response was delivered on: ", latestResTime)




