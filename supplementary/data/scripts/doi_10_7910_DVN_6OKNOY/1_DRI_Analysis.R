####### DRI Analysis ##########

### Preparation ###

  library(dplyr)
  library(ggplot2)
  library(viridis)
  library(grid)

  # DRI Formulas & Plot Function#
  
  dri_calc <- function(data, v1, v2){
    
    lambda <- 1 - (sqrt(2) / 2)
    dri <- 2 * (((1 - mean(abs((data[[v1]] - data[[v2]]) / sqrt(2)))) - (lambda))/(1 - (lambda))) - 1
    
    return(dri)
  }
  
  
  #Create Output folder existing files are overwritten
  dir.create('Output', showWarnings = FALSE)
  
  # Data #
  data_orig <- read.csv("Data1_Raw_Input.csv", fileEncoding="UTF-8-BOM")
  MLM <- read.csv("Data2_MLM_Input.csv")
  pre_mid_post <- read.csv("Data3_Pre-Mid-Post.csv")

### Data Editing ###
  
  Study.U <- unique(data_orig[c("StudyID", "CaseID")])
  Study.U <- Study.U[order(Study.U$StudyID), ]
  
  for(row in 1:nrow(Study.U)){
    
    #Select data
    
    study <- Study.U[row, "StudyID"]
    case <- Study.U[row, "CaseID"]
    data_study_case <- data_orig %>% filter (StudyID == study & CaseID == case)
    
    #Loop through analysis stages
    for(stage in 1:max(data_orig$StageID)){
      
      #Select specific data
      data_analyse <- data_study_case %>% filter(StageID == stage)
      
      if(nrow(data_analyse) > 0){
        
        #get participant numbers
        PNums <- data_analyse$PNum
        
        #variables for reading COLUMN data
        #' We should probably explain what Q and R stand for
        Q <- data_analyse %>% dplyr::select(F1:F50)
        R <- data_analyse %>% dplyr::select(Pref1:Pref10)
        
        #remove all NA columns
        Q <- Q[, colSums(is.na(Q)) != nrow(Q)]
        R <- R[, colSums(is.na(R)) != nrow(R)]
        
        #transpose data
        Q <- t(Q)
        R <- t(R)
        
        Q <- as.data.frame(Q)
        R <- as.data.frame(R)
        
        colnames(Q) <- PNums
        colnames(R) <- PNums
        
        #Obtain a list of correlations without duplicates
        QWrite <- subset(as.data.frame(as.table(cor(Q, method="spearman"))),
                         match(Var1, names(Q)) > match(Var2, names(Q)))
        
        RWrite <- subset(as.data.frame(as.table(cor(R, method="spearman"))),
                         match(Var1, names(R)) > match(Var2, names(R)))
        
        #Initialize the final df
        if(stage == 1){
          IC <- data.frame("P_P" = paste0(QWrite$Var1,'-',QWrite$Var2))
          IC$P1 <- as.numeric(QWrite$Var1)
          IC$P2 <- as.numeric(QWrite$Var2)
        }
        
        #Prepare QWrite
        QWrite <- as.data.frame(QWrite$Freq)
        names(QWrite)<- paste0("Q", stage)
        
        #Prepare RWrite for merge
        RWrite <- as.data.frame(RWrite$Freq)
        names(RWrite) <- paste0('R', stage)
        
        #merge
        IC <- cbind(IC, QWrite, RWrite)
      }
      
      #This is to make it robust to missing cases so the global merge still works
      if(nrow(data_analyse) == 0){
        QWrite <- as.data.frame(matrix(0, ncol = 3, nrow = nrow(IC)))
      }
    }
    
    #Add case & study
    IC$CaseId <- case 
    IC$StudyID <- study
    IC$Study <- data_study_case$Study[1]
    IC$Case <- data_study_case$Case[1]
    
    #Add data_status
    IC$Data_Status <- 1
    data_for_status <- data_analyse$PNum[data_analyse$Datacheck == 3]
    IC$Data_Status[IC$P1 %in% data_for_status | IC$P2 %in% data_for_status] <- 3
    
### Analysis ### 
   
    ## IC Points calculations ##
    IC$IC_PRE <- 1 - abs((IC$R1-IC$Q1)/sqrt(2))
    IC$IC_POST <- 1 - abs((IC$R2-IC$Q2)/sqrt(2))
    
    ## Group DRI level ##
    DRI_PRE <- dri_calc(data = IC, v1 = 'R1', v2 = 'Q1')
    DRI_POST <- dri_calc(data = IC, v1 = 'R2', v2 = 'Q2')
    
    #CaseDRI Dataframe
    DRI.Case <- data.frame(StudyID = study, Study = data_study_case$Study[1], 
                           CaseID = case, Case = data_study_case$Case[1], 
                           DRI_PRE, DRI_POST)
    
    #Tests for groups
    DRIOverallSig <- wilcox.test(IC$IC_POST, IC$IC_PRE, paired = TRUE, alternative = "greater")
    DRIOverallSig_twoside <- wilcox.test(IC$IC_POST, IC$IC_PRE, paired = TRUE, alternative = "two.side")
    #cumdist_pre_post <- cvm_test(IC$IC_PRE, IC$IC_POST, nboots = 1000)not necessary
    
    #Adding the results to case data
    DRI.Case$DRI_one_tailed_p <- DRIOverallSig$p.value 
    DRI.Case$DRI_twoside_p <- DRIOverallSig_twoside$p.value
    
    ## INDIVIDUAL DRI ##
  
    Plist <- unique(c(IC$P1, IC$P2))
    
    Plist <- Plist[order(Plist)]

    DRIInd <- data.frame('Participant' = Plist)
    DRIInd$StudyID <- study
    DRIInd$Study <- data_study_case$Study[1]
    DRIInd$CaseID <- case
    DRIInd$Case <- data_study_case$Case[1]
    
    DRIInd <- DRIInd[c("StudyID", "Study", "CaseID", "Case", "Participant")]
    
    #Add individual-level metrics
    for(i in 1:length(Plist)){
      DRIInd$DRIPre[i] <- dri_calc(data = IC  %>% filter(P1 == Plist[i] | P2 == Plist[i]), 
                                   v1 = 'R1', v2 = 'Q1')
      DRIInd$DRIPost[i] <- dri_calc(data = IC  %>% filter(P1 == Plist[i] | P2 == Plist[i]), 
                                    v1 = 'R2', v2 = 'Q2')
    }
    
    #Global dataframes for depositing results
    
    if(row == 1){
      IC.Global <- IC
      DRIInd.Global <- DRIInd
      DRI.Global <- DRI.Case
    }
    
    if(row > 1){
      IC.Global <- rbind(IC.Global, IC)
      DRIInd.Global<-rbind(DRIInd.Global, DRIInd)  
      DRI.Global<-rbind(DRI.Global, DRI.Case)   
    }
    
  }
  
  # Produces Data file without control groups #
    DRI0 <- data_orig %>% filter(StageID =="1")
    DRI0 <- DRI0 %>% filter(CaseID > "0.9")
    DRI0 <- DRI0[order(Study.U$StudyID)] #ID participant list without control groups
    
    ## DRI Total ##      
    DRIInd.Global_1 <- DRIInd.Global %>% filter(CaseID > "0.9")#dataset without control groups
    DRIInd.Global_2 <- cbind(DRIInd.Global_1, DRI0$PNum)
  
  # Save data writes data output files in the the "Output" folder overwrites any existing files#
  write.csv(DRI.Global, file = "Output/DRIGlobal.csv") 
  write.csv(DRIInd.Global_2, file = "Output/DRI_Individual_Global_NO_Control.csv") 
  write.csv(DRIInd.Global, file = "Output/DRI_Individual_Global.csv")

### Plots and Tables###
  
  ## Table 3 Main Text ##
  
  for(studyID in unique(DRI.Global$StudyID)){
    for(caseID in unique(DRI.Global$CaseID[DRI.Global$StudyID == studyID])){
      dri_study_case <- DRI.Global[DRI.Global$StudyID == studyID & DRI.Global$CaseID == caseID,]
      MLM_study_case <- MLM %>% 
        select(Group_building, Decision_impact, Complexity, Duration, StudyID, CaseID) %>%
        filter(StudyID == studyID & CaseID == caseID)
      MLM_study_case <- MLM_study_case[1,]
      table_3_temp <- merge(dri_study_case, MLM_study_case, by = c('StudyID', 'CaseID'), all.x = T)
      table_3_temp$n <- length(DRIInd.Global$StudyID[
        DRIInd.Global$StudyID == studyID & DRIInd.Global$CaseID == caseID])
      if(studyID == unique(DRI.Global$StudyID)[1] & caseID == unique(DRI.Global$CaseID)[1]){
        table_3 <- table_3_temp
      }
      if(!(studyID == unique(DRI.Global$StudyID)[1] & caseID == unique(DRI.Global$CaseID))[1]){
        table_3 <- rbind(table_3, table_3_temp)
      }
    }
  }
  
  table3 <- table_3 %>% select(-Study, -StudyID, -CaseID, -DRI_twoside_p)
  table3$DRI_one_tailed_p <- format(round(table3$DRI_one_tailed_p, 2), nsmall = 2)
  table3$DRI_Change <- format(round(table3$DRI_POST - table3$DRI_PRE, 2), nsmall = 2)
  table3$DRI_PRE <- round(table3$DRI_PRE, 2)
  table3$DRI_POST <- round(table3$DRI_POST, 2)
  table3$DRI_Change[table3$DRI_one_tailed_p < 0.01] <- paste0(
    table3$DRI_Change[table3$DRI_one_tailed_p < 0.01], '***')
  table3$DRI_Change[table3$DRI_one_tailed_p >= 0.01 & table3$DRI_one_tailed_p < 0.05] <- paste0(
    table3$DRI_Change[table3$DRI_one_tailed_p >= 0.01 & table3$DRI_one_tailed_p < 0.05], '**')
  table3$DRI_Change[table3$DRI_one_tailed_p >= 0.05 & table3$DRI_one_tailed_p < 0.1] <- paste0(
    table3$DRI_Change[table3$DRI_one_tailed_p >= 0.05 & table3$DRI_one_tailed_p < 0.1], '*')
  
  table3 <- table3 %>% select(Case, DRI_PRE, DRI_POST, DRI_Change, Group_building, Decision_impact,
                              Complexity, Duration, n)
  
  #Creates a file containing Table in Output/Tables folder existing files are overwritten
  dir.create('Output/Tables', showWarnings = FALSE)
  write.csv(table3, file = "Output/Tables/Table_3.csv") 
  
  ## Table D.1.1. (Online Appendix Information vs Formal Deliberation Effects) ##
  names(pre_mid_post)[1] <- 'APSR_CaseID'
  
  Study.U <- unique(pre_mid_post[c("APSR_StudyID", "APSR_CaseID")])
  Study.U <- Study.U[order(Study.U$APSR_StudyID), ]
  
  for(row in 1:nrow(Study.U)){
    
    #Select data
    
    study <- Study.U[row, "APSR_StudyID"]
    case <- Study.U[row, "APSR_CaseID"]
    data_study_case <- pre_mid_post %>% filter (APSR_StudyID == study & APSR_CaseID == case)
    
    #Loop through analysis stages
    for(stage in 1:max(pre_mid_post$StageID_Analysis)){
      
      #Select specific data
      data_analyse <- data_study_case %>% filter(StageID_Analysis == stage)
      
      if(nrow(data_analyse) > 0){
        
        #get participant numbers
        PNums <- data_analyse$PNum
        
        #variables for reading COLUMN data
        #' We should probably explain what Q and R stand for
        Q <- data_analyse %>% dplyr::select(F01:F50)
        R <- data_analyse %>% dplyr::select(RF1:RF10)
        
        #remove all NA columns
        Q <- Q[, colSums(is.na(Q)) != nrow(Q)]
        R <- R[, colSums(is.na(R)) != nrow(R)]
        
        #transpose data
        Q <- t(Q)
        R <- t(R)
        
        Q <- as.data.frame(Q)
        R <- as.data.frame(R)
        
        colnames(Q) <- PNums
        colnames(R) <- PNums
        
        #Obtain a list of correlations without duplicates
        QWrite <- subset(as.data.frame(as.table(cor(Q, method="spearman"))),
                         match(Var1, names(Q)) > match(Var2, names(Q)))
        
        RWrite <- subset(as.data.frame(as.table(cor(R, method="spearman"))),
                         match(Var1, names(R)) > match(Var2, names(R)))
        
        #Initialize the final df
        if(stage == 1){
          IC_PMP <- data.frame("P_P" = paste0(QWrite$Var1,'-',QWrite$Var2))
          IC_PMP$P1 <- as.numeric(QWrite$Var1)
          IC_PMP$P2 <- as.numeric(QWrite$Var2)
        }
        
        #Prepare QWrite
        QWrite <- as.data.frame(QWrite$Freq)
        names(QWrite)<- paste0("Q", stage)
        
        #Prepare RWrite for merge
        RWrite <- as.data.frame(RWrite$Freq)
        names(RWrite) <- paste0('R', stage)
        
        #merge
        IC_PMP <- cbind(IC_PMP, QWrite, RWrite)
      }
      #This is to make it robust to missing cases so the global merge still works
      if(nrow(data_analyse) == 0){
        QWrite <- as.data.frame(matrix(0, ncol = 3, nrow = nrow(IC)))
      }
    }
    
    ## Group DRI level ##
    DRI_PRE <- dri_calc(data = IC_PMP, v1 = 'R1', v2 = 'Q1')
    DRI_MID <- dri_calc(data = IC_PMP, v1 = 'R2', v2 = 'Q2')
    DRI_POST <- dri_calc(data = IC_PMP, v1 = 'R3', v2 = 'Q3')
    
    #CaseDRI Dataframe
    DRI_pre_mid_post <- data.frame(StudyID = study, Study = data_study_case$APSR_StudyName[1], 
                           CaseID = case, Case = data_study_case$APSR_CaseName[1], 
                           DRI_PRE, DRI_MID, DRI_POST, 
                           n = length(unique(data_study_case$PNum[data_study_case$APSR_CaseID == case])))
    
    table_3_temp$n <- length(DRIInd.Global$StudyID[
      DRIInd.Global$StudyID == studyID & DRIInd.Global$CaseID == caseID])
    ## IC Points calculations ##
    IC_PMP$IC_PRE <- 1 - abs((IC_PMP$R1-IC_PMP$Q1)/sqrt(2))
    IC_PMP$IC_MID <- 1 - abs((IC_PMP$R2-IC_PMP$Q2)/sqrt(2))
    IC_PMP$IC_POST <- 1 - abs((IC_PMP$R3-IC_PMP$Q3)/sqrt(2))
    
    #Tests for groups
    DRI_mid_pre_twoside <- wilcox.test(IC_PMP$IC_MID, IC_PMP$IC_PRE, paired = TRUE, alternative = "two.side")
    DRI_post_mid_twoside <- wilcox.test(IC_PMP$IC_POST, IC_PMP$IC_MID, paired = TRUE, alternative = "two.side")
    #cumdist_pre_post <- cvm_test(IC_PMP$IC_PRE, IC_PMP$IC_POST, nboots = 1000)not necessary
    
    #Adding the results to case data
    DRI_pre_mid_post$DRI_mid_pre_twoside <- DRI_mid_pre_twoside$p.value
    DRI_pre_mid_post$DRI_post_mid_twoside <- DRI_post_mid_twoside$p.value
    
    #Global dataframes for depositing results
    
    if(row == 1){
      IC.Global_PMP <- IC_PMP
      DRI_pre_mid_post_global <- DRI_pre_mid_post
    }
    
    if(row > 1){
      IC.Global_PMP <- rbind(IC.Global_PMP, IC_PMP)
      DRI_pre_mid_post_global <- rbind(DRI_pre_mid_post_global, DRI_pre_mid_post)   
    }
    
  }
  
    # Table D.1.1 (online appendix)
    table_d11 <- DRI_pre_mid_post_global[order(DRI_pre_mid_post_global$CaseID), ]
    #table_d11$Case_t <- paste0(table_d11$Study, ' ', table_d11$Case)
    table_d11$Case_t <- paste0(table_d11$Case)
    
    table_d11$Information <- format(round(table_d11$DRI_MID - table_d11$DRI_PRE, 2), nsmall = 2)
    table_d11$Information[table_d11$DRI_mid_pre_twoside < 0.01] <- paste0(
      table_d11$Information[table_d11$DRI_mid_pre_twoside < 0.01], '***')
    table_d11$Information[table_d11$DRI_mid_pre_twoside >= 0.01 & table_d11$DRI_mid_pre_twoside < 0.05] <- paste0(
      table_d11$Information[table_d11$DRI_mid_pre_twoside >= 0.01 & table_d11$DRI_mid_pre_twoside < 0.05], '**')
    table_d11$Information[table_d11$DRI_mid_pre_twoside >= 0.05 & table_d11$DRI_mid_pre_twoside < 0.1] <- paste0(
      table_d11$Information[table_d11$DRI_mid_pre_twoside >= 0.05 & table_d11$DRI_mid_pre_twoside < 0.1], '*')
    
    table_d11$Formal_Deliberation <- format(round(table_d11$DRI_POST - table_d11$DRI_MID, 2), nsmall = 2)
    table_d11$Formal_Deliberation[
      table_d11$DRI_post_mid_twoside < 0.01] <- paste0(
      table_d11$Formal_Deliberation[table_d11$DRI_post_mid_twoside < 0.01], '***')
    table_d11$Formal_Deliberation[
      table_d11$DRI_post_mid_twoside >= 0.01 & table_d11$DRI_post_mid_twoside < 0.05] <- paste0(
      table_d11$Formal_Deliberation[table_d11$DRI_post_mid_twoside >= 0.01 & table_d11$DRI_post_mid_twoside < 0.05], '**')
    table_d11$Formal_Deliberation[
      table_d11$DRI_post_mid_twoside >= 0.05 & table_d11$DRI_post_mid_twoside < 0.1] <- paste0(
      table_d11$Formal_Deliberation[table_d11$DRI_post_mid_twoside >= 0.05 & table_d11$DRI_post_mid_twoside < 0.1], '*')
    
    table_d11 <- table_d11 %>% select(CaseID, Case_t, Information, Formal_Deliberation, n)
    
    #Creates a file containing Table in Output/Tables folder, existing files are overwritten
    write.csv(table_d11, file = "Output/Tables/Table_D_1_1.csv") 
    
  ## DRI Plot function Used for producing DRI plots in figures 2 and 3 ## 
  dri_plot <- function(data, x, y, title, suffix, DRI){
    
    grob <- grobTree(textGrob(paste0("DRI = ", round(DRI, 2)), x=0.1,  y=0.9, hjust=0,
                              gp=gpar(col="red", fontsize=13, fontface="italic")))
    
    plot <-
      ggplot(data, aes(x = get(x), y = get(y))) +
      geom_jitter(width = 0.02, height = 0.02, show.legend = TRUE, col = rgb(0.1, 0, 1, 0.6), lwd = 3) +
      xlim(-1.1, 1.1) + ylim(-1.1, 1.1) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      geom_abline(intercept = 0, slope = 1, colour="white", lwd = 1) +
      geom_hline(yintercept = 0, color = "white", lwd = 1) +
      geom_vline(xintercept = 0, color = "white", lwd = 1) +
      labs(x = "Intersubjective Agreement - Considerations", y = "Intersubjective Agreement - Preferences") +
      ggtitle(paste0(title, ": ", suffix)) + 
      annotation_custom(grob) + 
      geom_density_2d_filled(inherit.aes = T, alpha = 0.3) +
      geom_density_2d(linewidth = 0.25, colour = "black") +
      theme(legend.position="none")
    
    return(plot)
  }

  ## Figure 2 ##
  
  dir.create('Output/Figures', showWarnings = FALSE)

    #Pre-deliberation
    fig2_pre <- dri_plot(data = IC.Global[IC.Global$StudyID == 2,], 
           x = 'Q1', y = 'R1', title = 'Figure 2. DRI Plots: FNQCJ Case', suffix = 'PRE',
           DRI = DRI.Global[DRI.Global$StudyID == 2,]$DRI_PRE)
    fig2_pre
    
    ggsave('Output/Figures/Fig2_a_Pre.pdf', fig2_pre, device = 'pdf')
    
    #Post-deliberation
    fig2_post <- dri_plot(data = IC.Global[IC.Global$StudyID == 2,], 
             x = 'Q2', y = 'R2', title = 'Figure 2. DRI Plots: FNQCJ Case', suffix = 'POST',
             DRI = DRI.Global[DRI.Global$StudyID == 2,]$DRI_POST)
    fig2_post
    
    ggsave('Output/Figures/Fig2_b_Post.pdf', fig2_post, device = 'pdf')
    
  ## Figure 3 writes the three pre- and post-deliberation plots for the three cases as separate files ## 
    
    #Figure 3 Part 1 Control group (no deliberation)
   
      #Pre
      fig3_control_pre <- dri_plot(data = IC.Global[IC.Global$StudyID == 1 & IC.Global$CaseId == 0.1,], 
             x = 'Q1', y = 'R1', title = 'Figure 3. DRI Plots: Uppsala Speaks Study', suffix = 'PRE',
             DRI = DRI.Global[DRI.Global$StudyID == 1 & DRI.Global$CaseID == 0.1,]$DRI_PRE)
      ggsave('Output/Figures/Fig3_1Control_a_Pre.pdf', fig3_control_pre, device = 'pdf')
      
      fig3_control_pre
      
      #Post
      fig3_control_post <- dri_plot(data = IC.Global[IC.Global$StudyID == 1 & IC.Global$CaseId == 0.1,], 
               x = 'Q2', y = 'R2', title = 'Figure 3. DRI Plots: Uppsala Speaks Study', suffix = 'POST',
               DRI = DRI.Global[DRI.Global$StudyID == 1 & DRI.Global$CaseID == 0.1,]$DRI_POST)
      ggsave('Output/Figures/Fig3_1Control_b_Post.pdf', fig3_control_post, device = 'pdf')
      
      fig3_control_post
    #Figure 3 Part 2 Group Briefing
      
      #Pre
      fig3_brief_pre <- dri_plot(data = IC.Global[IC.Global$StudyID == 1 & IC.Global$CaseId == 1,], 
               x = 'Q1', y = 'R1', title = 'Figure 3. DRI Plots: Uppsala Speaks Study', suffix = 'PRE',
               DRI = DRI.Global[DRI.Global$StudyID == 1 & DRI.Global$CaseID == 1,]$DRI_PRE)
      ggsave('Output/Figures/Fig3_2Brief_a_Pre.pdf', fig3_brief_pre, device = 'pdf')
      
      fig3_brief_pre
      
      #Post
      fig3_brief_post <- dri_plot(data = IC.Global[IC.Global$StudyID == 1 & IC.Global$CaseId == 1,], 
               x = 'Q2', y = 'R2', title = 'Figure 3. DRI Plots: Uppsala Speaks Study', suffix = 'POST',
               DRI = DRI.Global[DRI.Global$StudyID == 1 & DRI.Global$CaseID == 1,]$DRI_POST)
      ggsave('Output/Figures/Fig3_2Brief_b_Post.pdf', fig3_brief_post, device = 'pdf')
      
      fig3_brief_post
    #Figure 3 Part 3 Group Building Plus
      
      #Pre
      fig3_building_pre <- dri_plot(data = IC.Global[IC.Global$StudyID == 1 & IC.Global$CaseId == 2,], 
               x = 'Q1', y = 'R1', title = 'Figure 3. DRI Plots: Uppsala Speaks Study', suffix = 'PRE',
               DRI = DRI.Global[DRI.Global$StudyID == 1 & DRI.Global$CaseID == 2,]$DRI_PRE)
      
      fig3_building_pre
      
      ggsave('Output/Figures/Fig3_3Building_a_Pre.pdf', fig3_building_pre, device = 'pdf')
    
      #Post
      fig3_building_post <- dri_plot(data = IC.Global[IC.Global$StudyID == 1 & IC.Global$CaseId == 2,], 
               x = 'Q2', y = 'R2', title = 'Figure 3. DRI Plots: Uppsala Speaks Study', suffix = 'POST',
               DRI = DRI.Global[DRI.Global$StudyID == 1 & DRI.Global$CaseID == 2,]$DRI_POST)
      
      fig3_building_post
      
      ggsave('Output/Figures/Fig3_3Building_b_Post.pdf', fig3_building_post, device = 'pdf')
      
      