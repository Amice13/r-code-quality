    ##############################################################################################
    ##############################################################################################
    ### R code for the 20th Assembly Data
    ### By Gyung-Ho Jeong
    ### 03/03/2024
    ##############################################################################################
    ##############################################################################################

    require(MASS)
    require(AER) # for Tobit models
    require(pscl) # for ZIP models

    Data <- read.csv("Data_L20.csv", header=T)

    # distribution of ideal points and filibusters

    pdf("fig2_panel_D.pdf", height=5, width=5)
    par(mfrow=c(1,1))
    plot(density(Data$ideology), main="", xlab="Ideal points")
    segments(Data$ideology, 0, Data$ideology, Data$talk_rescaled, lty=2)
    dev.off()

    pdf("fig_S2_panel_D.pdf", height=5, width=5)
    par(mfrow=c(1,1))
    plot(density(Data$nominate), main="", xlab="Ideal points")
    segments(Data$nominate, 0, Data$nominate, Data$talk_rescaled, lty=2)
    dev.off()


    # Table 2: 20th 
    Model13 <- zeroinfl(talk ~ ideology + seniority + PR  | ideology, data=Data)
    summary(Model13)
    Model14 <- zeroinfl(talk ~ ideology + seniority + margin + stronghold | ideology, data=Data)
    summary(Model14)

    Data$censure2 <- ifelse(Data$censure > 0, 1, 0)
    Model15 <- glm(censure2 ~ ideology + seniority + PR, data=Data, family='binomial')
    summary(Model15)
    Model16 <- glm(censure2 ~ ideology + seniority + margin + stronghold, data=Data, family='binomial')
    summary(Model16)


    # Table S4: 19th 
    S4_Model13 <- zeroinfl(talk ~ nominate + seniority + PR  | nominate, data=Data)
    summary(S4_Model13)
    S4_Model14 <- zeroinfl(talk ~ nominate + seniority + margin + stronghold | nominate, data=Data)
    summary(S4_Model14)

    S4_Model15 <- glm(censure2 ~ nominate + seniority + PR, data=Data, family='binomial')
    summary(S4_Model15)
    S4_Model16 <- glm(censure2 ~ nominate + seniority + margin + stronghold, data=Data, family='binomial')
    summary(S4_Model16)



    # Table S5: 20th
    S5_Model13 <- zeroinfl(talk ~ ideology + seniority + PR | ideology + seniority + PR, data=Data)
    summary(S5_Model13)
    S5_Model14 <- zeroinfl(talk ~ ideology + seniority + margin + stronghold | ideology + seniority + margin + stronghold, data=Data)
    summary(S5_Model14)



    # Table S6: 20th 
    S6_Model13 <- tobit(talk ~ ideology + seniority + PR, left=0, right=Inf, data=Data)
    summary(S6_Model13)
    S6_Model14 <- tobit(talk ~ ideology + seniority + margin + stronghold, left=0, right=Inf, data=Data)
    summary(S6_Model14)

