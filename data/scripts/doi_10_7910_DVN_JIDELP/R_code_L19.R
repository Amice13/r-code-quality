    ##############################################################################################
    ##############################################################################################
    ### R code for the 19th Assembly Data
    ### By Gyung-Ho Jeong
    ### 03/03/2024
    ##############################################################################################
    ##############################################################################################


    require(MASS)
    require(AER) # for Tobit models
    require(pscl) # for ZIP models

    Data <- read.csv("Data_L19.csv", header=T)

    # distribution of ideal points and filibusters

    pdf("fig2_panel_C.pdf", height=5, width=5)
    par(mfrow=c(1,1))
    plot(density(Data$ideology), main="", xlab="Ideal points")
    #points(density(Data$ideology), type='l')
    segments(Data$ideology, 0, Data$ideology, Data$talk_rescaled, lty=2)
    dev.off()

    pdf("fig_S2_panel_C.pdf", height=5, width=5)
    par(mfrow=c(1,1))
    plot(density(Data$nominate), main="", xlab="Ideal points")
    #points(density(Data$nominate), type='l')
    segments(Data$nominate, 0, Data$nominate, Data$talk_rescaled, lty=2)
    dev.off()


    # Table 2: 19th 
    Model9 <- zeroinfl(talk ~ ideology + seniority + PR  | ideology, data=Data)
    summary(Model9)
    Model10 <- zeroinfl(talk ~ ideology + seniority + margin + stronghold | ideology, data=Data)
    summary(Model10)

    Data$censure <- ifelse(Data$censure > 0, 1, 0)
    Model11 <- glm(censure ~ ideology + seniority + PR, data=Data, family='binomial')
    summary(Model11)
    Model12 <- glm(censure ~ ideology + seniority + margin + stronghold, data=Data, family='binomial')
    summary(Model12)


    # Table S4: 19th 
    S4_Model9 <- zeroinfl(talk ~ nominate + seniority + PR  | nominate, data=Data)
    summary(S4_Model9)
    S4_Model10 <- zeroinfl(talk ~ nominate + seniority + margin + stronghold | nominate, data=Data)
    summary(S4_Model10)

    S4_Model11 <- glm(censure ~ nominate + seniority + PR, data=Data, family='binomial')
    summary(S4_Model11)
    S4_Model12 <- glm(censure ~ nominate + seniority + margin + stronghold, data=Data, family='binomial')
    summary(S4_Model12)



    # Table S5: 19th
    S5_Model9 <- zeroinfl(talk ~ ideology + seniority + PR | ideology + seniority + PR, data=Data)
    summary(S5_Model9)
    S5_Model10 <- zeroinfl(talk ~ ideology + seniority + margin + stronghold | ideology + seniority + margin + stronghold, data=Data)
    summary(S5_Model10)



    # Table S6: 19th 
    S6_Model9 <- tobit(talk ~ ideology + seniority + PR, left=0, right=Inf, data=Data)
    summary(S6_Model9)
    S6_Model10 <- tobit(talk ~ ideology + seniority + margin + stronghold, left=0, right=Inf, data=Data)
    summary(S6_Model10)

