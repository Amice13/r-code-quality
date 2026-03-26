
###JOP_table 3

data<- read.csv(file.choose())

attach(data)

Symbolic.data<-data.frame(Symbolic_Racisim, Genetic_Attr, Female , Age , White , Religious_Attendance , 
                          Education , Income ,Conservative_Ideology, Republican_Partisanship)

Symbolic.data <- na.omit(Symbolic.data)

pcor(Symbolic.data)


Affirm_Action.data <- data.frame(Affirm_Action, Genetic_Attr, Female , Age , White , Religious_Attendance , 
                                 Education , Income ,Conservative_Ideology, Republican_Partisanship)
Affirm_Action.data<-na.omit(Affirm_Action.data)

pcor(Affirm_Action.data)


Racial_Tolerance.data <- data.frame(Racial_Tolerance, Genetic_Attr, Female , Age , White , Religious_Attendance , 
                                    Education , Income ,Conservative_Ideology, Republican_Partisanship)

Racial_Tolerance.data<-na.omit(Racial_Tolerance.data)

pcor(Racial_Tolerance.data)

Racial_Differences.data <- data.frame(Racial_Differences, Genetic_Attr, Female , Age , White , Religious_Attendance , 
                                      Education , Income ,Conservative_Ideology, Republican_Partisanship)

Racial_Differences.data<-na.omit(Racial_Differences.data)

pcor(Racial_Differences.data)

Black_Homogeneity.data <-data.frame(Black_Homogeneity, Genetic_Attr, Female , Age , White , Religious_Attendance , 
                                    Education , Income ,Conservative_Ideology, Republican_Partisanship)

Black_Homogeneity.data<-na.omit(Black_Homogeneity.data)

pcor(Black_Homogeneity.data)

