    ##############################################################################################
    ##############################################################################################
    ### R code for the 18th Assembly Data
    ### By Gyung-Ho Jeong
    ### 03/03/2024
    ##############################################################################################
    ##############################################################################################


    require(MASS)
    require(pscl) # for ZIP models

    Data <- read.csv("Data_L18.csv", header=T)
    
    # distribution of ideal points and brawlers
    
    pdf("fig2_panal_B.pdf", height=5, width=5)
    par(mfrow=c(1,1))
    plot(density(Data$ideology), main="", xlab="")
    segments(Data$ideology[Data$brawling_censure==1], 0, Data$ideology[Data$brawling_censure==1], 0.2, lty=2)
    dev.off()

    
    pdf("fig_S2_panal_B.pdf", height=5, width=5)
    par(mfrow=c(1,1))
    plot(density(Data$nominate, na.rm=T), main="", xlab="")
    segments(Data$nominate[Data$brawling_censure==1], 0, Data$nominate[Data$brawling_censure==1], 0.2, lty=2)
    dev.off()



   
    # Table 1: 18th 

    Model5 <- glm(brawling_censure ~ ideology + seniority + PR, data=Data, family='binomial')
    summary(Model5)
    Model6 <- glm(brawling_censure ~ ideology + seniority + margin + stronghold, data=Data, family='binomial')
    summary(Model6)

    Model7 <- glm(nonbrawling_censure ~ ideology + seniority + PR, data=Data, family='binomial')
    summary(Model7)
    Model8 <- glm(nonbrawling_censure ~ ideology + seniority + margin + stronghold, data=Data, family='binomial')
    summary(Model8)


    # Table S1: 18th

    S1_Model5 <- glm(brawling_confirmed ~ ideology + seniority + PR, data=Data, family='binomial')
    summary(S1_Model5)
    S1_Model6 <- glm(brawling_confirmed ~ ideology + seniority + margin + stronghold, data=Data, family='binomial')
    summary(S1_Model6)

    # Table S2: 18th
    
    S2_Model5 <- glm(brawling_news ~ ideology + seniority + PR, data=Data, family='binomial')
    summary(S2_Model5)
    S2_Model6 <- glm(brawling_news ~ ideology + seniority + margin + stronghold, data=Data, family='binomial')
    summary(S2_Model6)

    # Table S3: 18th
    
    S3_Model5 <- glm(brawling_news ~ nominate + seniority + PR, data=Data, family='binomial')
    summary(S3_Model5)
    S3_Model6 <- glm(brawling_news ~ nominate + seniority + margin + stronghold, data=Data, family='binomial')
    summary(S3_Model6)
    S3_Model7 <- glm(nonbrawling_censure ~ nominate + seniority + PR, data=Data, family='binomial')
    summary(S3_Model7)
    S3_Model8 <- glm(nonbrawling_censure ~ nominate + seniority + margin + stronghold, data=Data, family='binomial')
    summary(S3_Model8)
