    ##############################################################################################
    ##############################################################################################
    ### R code for the 17th Assembly Data
    ### By Gyung-Ho Jeong
    ### 03/03/2024
    ##############################################################################################
    ##############################################################################################


    require(MASS)
    require(pscl) # for ZIP models

    Data <- read.csv("Data_L17.csv", header=T)
    
    # distribution of ideal points and brawlers: Figure 2 - Panel A and Figure S2 - Panel A
    
    pdf("fig2_panal_A.pdf", height=5, width=5)
    par(mfrow=c(1,1))
    plot(density(Data$ideology), main="", xlab="")
    segments(Data$ideology[Data$brawling_censure==1], 0, Data$ideology[Data$brawling_censure==1], 0.2, lty=2)
    dev.off()

    pdf("fig_S2_panal_A.pdf", height=5, width=5)
    par(mfrow=c(1,1))
    plot(density(Data$nominate, na.rm=T), main="", xlab="")
    segments(Data$nominate[Data$brawling_censure==1], 0, Data$nominate[Data$brawling_censure==1], 0.2, lty=2)
    dev.off()

   
    # Table 1: 17th 

    # censure for brawling: brawling_censure
    Model1 <- glm(brawling_censure ~ ideology + seniority + PR, data=Data, family='binomial')
    summary(Model1)
    Model2 <- glm(brawling_censure ~ ideology + seniority + margin + stronghold, data=Data, family='binomial')
    summary(Model2)

    # censure for non-brawling: nonbrawling_censure
    Model3 <- glm(nonbrawling_censure ~ ideology + seniority + PR, data=Data, family='binomial')
    summary(Model3)
    Model4 <- glm(nonbrawling_censure ~ ideology + seniority + margin + stronghold, data=Data, family='binomial')
    summary(Model4)


    # Table S1: 17th

    S1_Model1 <- glm(brawling_confirmed ~ ideology + seniority + PR, data=Data, family='binomial')
    summary(S1_Model1)
    S1_Model2 <- glm(brawling_confirmed ~ ideology + seniority + margin + stronghold, data=Data, family='binomial')
    summary(S1_Model2)


    # Table S2: 17th
    
    S2_Model1 <- glm(brawling_news ~ ideology + seniority + PR, data=Data, family='binomial')
    summary(S2_Model1)
    S2_Model2 <- glm(brawling_news ~ ideology + seniority + margin + stronghold, data=Data, family='binomial')
    summary(S2_Model2)

    # Table S3: 17th
    
    S3_Model1 <- glm(brawling_news ~ nominate + seniority + PR, data=Data, family='binomial')
    summary(S3_Model1)
    S3_Model2 <- glm(brawling_news ~ nominate + seniority + margin + stronghold, data=Data, family='binomial')
    summary(S3_Model2)
    S3_Model3 <- glm(nonbrawling_censure ~ nominate + seniority + PR, data=Data, family='binomial')
    summary(S3_Model3)
    S3_Model4 <- glm(nonbrawling_censure ~ nominate + seniority + margin + stronghold, data=Data, family='binomial')
    summary(S3_Model4)


