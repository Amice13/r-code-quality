# replication script: for A short scale for measuring political secularism 

  # libraries
  library(tidyverse)
  library(haven)
  library(car)
  library(mosaic)
  library(MplusAutomation)
  library(texreg)
  library(tidyverse)
  library(rio)
  library(polycor)
  library(psych)
  # pid <- read_dta("/home/kai/Work/bioethics/data/umfrage-infratest-2016/pid-2016.dta",encoding = "latin1")
  data2016  <- as_tibble(import("replication-data-2016.dta"))
  data2012  <- as_tibble(import("replication-data-2012.dta"))
  
  # split into four different datasets (year * region)
  
  east2016  <- data2016 %>% filter(east == 1)
  west2016  <- data2016 %>% filter(east == 0)
  east2012  <- data2012 %>% filter(east == 1)
  west2012  <- data2012 %>% filter(east == 0)

# run main models in MPlus 

    # West Germany 2016 ordinal

    pathmodelwest2016.5.ordinal  <- mplusObject(
      TITLE = "Scaling West Germany 2016, five items, ML, ordinal;",
      VARIABLE = "CATEGORICAL = ALL ;",
      MODEL = "
        SEC BY nosymbols* nolimsci noreleduc norelipol nogodeu ;
        SEC@1 ;",
      rdata = west2016) 


    fit.west2016.5.ordinal  <- mplusModeler(pathmodelwest2016.5.ordinal, run = 1L,modelout = "scaling-west-2016-5-ordinal.inp")





    pathmodelwest2016.42.ordinal  <- mplusObject(
      TITLE = "Scaling West Germany 2016, four items (2 rel education), ML, ordinal;",
      VARIABLE = "CATEGORICAL = ALL ;",
      MODEL = "
        SEC BY nolimsci* noreleduc norelipol nogodeu ;
        SEC@1 ;",
      rdata = west2016) 


    fit.west2016.42.ordinal  <- mplusModeler(pathmodelwest2016.42.ordinal, run = 1L,modelout = "scaling-west-2016-42-ordinal.inp")



    # East Germany 2016 ordinal

     pathmodeleast2016.5.ordinal  <- mplusObject(
      TITLE = "Scaling East Germany 2016, five items, ML, ordinal;",
      VARIABLE = "CATEGORICAL = ALL ;",
      MODEL = "
        SEC BY nosymbols* nolimsci noreleduc norelipol nogodeu ;
        SEC@1 ;",
      rdata = east2016) 


    fit.east2016.5.ordinal  <- mplusModeler(pathmodeleast2016.5.ordinal, run = 1L,modelout = "scaling-east-2016-5-ordinal.inp")





    pathmodeleast2016.42.ordinal  <- mplusObject(
      TITLE = "Scaling East Germany 2016, four items (2 rel education), ML, ordinal;",
      VARIABLE = "CATEGORICAL = ALL ;",
      MODEL = "
        SEC BY nolimsci* noreleduc norelipol nogodeu ;
        SEC@1 ;",
      rdata = east2016) 


    fit.east2016.42.ordinal  <- mplusModeler(pathmodeleast2016.42.ordinal, run = 1L,modelout = "scaling-east-2016-42-ordinal.inp")


    # West Germany 2012 ordinal

    pathmodelwest2012.5.ordinal  <- mplusObject(
      TITLE = "Scaling West Germany 2012, five items, ML, ordinal;",
      VARIABLE = "CATEGORICAL = ALL ;",
      MODEL = "
        SEC BY nosymbols* nolimsci noreleduc norelipol nogodeu ;
        SEC@1 ;",
      rdata = west2012) 


    fit.west2012.5.ordinal  <- mplusModeler(pathmodelwest2012.5.ordinal, run = 1L,modelout = "scaling-west-2012-5-ordinal.inp")





    pathmodelwest2012.42.ordinal  <- mplusObject(
      TITLE = "Scaling West Germany 2012, four items (2 rel education), ML, ordinal;",
      VARIABLE = "CATEGORICAL = ALL ;",
      MODEL = "
        SEC BY nolimsci* noreleduc norelipol nogodeu ;
        SEC@1 ;",
      rdata = west2012) 


    fit.west2012.42.ordinal  <- mplusModeler(pathmodelwest2012.42.ordinal, run = 1L,modelout = "scaling-west-2012-42-ordinal.inp")



    # East Germany 2012 ordinal

     pathmodeleast2012.5.ordinal  <- mplusObject(
      TITLE = "Scaling East Germany 2012, five items, ML, ordinal;",
      VARIABLE = "CATEGORICAL = ALL ;",
      MODEL = "
        SEC BY nosymbols* nolimsci noreleduc norelipol nogodeu ;
        SEC@1 ;",
      rdata = east2012) 


    fit.east2012.5.ordinal  <- mplusModeler(pathmodeleast2012.5.ordinal, run = 1L,modelout = "scaling-east-2012-5-ordinal.inp")




    pathmodeleast2012.42.ordinal  <- mplusObject(
      TITLE = "Scaling East Germany 2012, four items (2 rel education), ML, ordinal;",
      VARIABLE = "CATEGORICAL = ALL ;",
      MODEL = "
        SEC BY nolimsci* noreleduc norelipol nogodeu ;
        SEC@1 ;",
      rdata = east2012) 


    fit.east2012.42.ordinal  <- mplusModeler(pathmodeleast2012.42.ordinal, run = 1L,modelout = "scaling-east-2012-42-ordinal.inp")






  # West 2016 
      validity.religiosity.west2016.2fac  <- mplusObject(
        TITLE = "Secularism + Religiosity, 2 factors ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu ;",
        MODEL = "
          SEC BY nolimsci* noreleduc norelipol nogodeu ;
          SEC@1 ;
          RELIGIOSITY BY religintensity* prayer  churchattendance ;
          RELIGIOSITY@1 ;",
        rdata = west2016)

    fit.validity.religiosity.west2016.2fac  <- mplusModeler(validity.religiosity.west2016.2fac, run = 1L,modelout = "validity-religiosity-2fac-west-2016.inp")

      validity.religiosity.west2016.1fac  <- mplusObject(
        TITLE = "Secularism + Religiosity, 1 factor ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu ;",
        MODEL = "
        COMMON BY nolimsci* noreleduc norelipol nogodeu
        religintensity prayer  churchattendance;
        COMMON@1 ;",
        rdata = west2016)

    fit.validity.religiosity.west2016.1fac  <- mplusModeler(validity.religiosity.west2016.1fac, run = 1L,modelout = "validity-religiosity-1fac-west-2016.inp")

  # Ost 2016 
      validity.religiosity.east2016.2fac  <- mplusObject(
        TITLE = "Secularism + Religiosity, 2 factors ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu ;",
        MODEL = "
          SEC BY nolimsci* noreleduc norelipol nogodeu ;
          SEC@1 ;
          RELIGIOSITY BY religintensity* prayer  churchattendance ;
          RELIGIOSITY@1 ;",
        rdata = east2016)

    fit.validity.religiosity.east2016.2fac  <- mplusModeler(validity.religiosity.east2016.2fac, run = 1L,modelout = "validity-religiosity-2fac-east-2016.inp")

      validity.religiosity.east2016.1fac  <- mplusObject(
        TITLE = "Secularism + Religiosity, 1 factor ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu ;",
        MODEL = "
        COMMON BY nolimsci* noreleduc norelipol nogodeu
        religintensity prayer  churchattendance;
        COMMON@1 ;",
        rdata = east2016)

    fit.validity.religiosity.east2016.1fac  <- mplusModeler(validity.religiosity.east2016.1fac, run = 1L,modelout = "validity-religiosity-1fac-east-2016.inp")


  # West 2012 
      validity.religiosity.west2012.2fac  <- mplusObject(
        TITLE = "Secularism + Religiosity, 2 factors ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu ;",
        MODEL = "
          SEC BY nolimsci* noreleduc norelipol nogodeu ;
          SEC@1 ;
          RELIGIOSITY BY religintensity* prayer  churchattendance ;
          RELIGIOSITY@1 ;",
        rdata = west2012)

    fit.validity.religiosity.west2012.2fac  <- mplusModeler(validity.religiosity.west2012.2fac, run = 1L,modelout = "validity-religiosity-2fac-west-2012.inp")

      validity.religiosity.west2012.1fac  <- mplusObject(
        TITLE = "Secularism + Religiosity, 1 factor ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu ;",
        MODEL = "
        COMMON BY nolimsci* noreleduc norelipol nogodeu
        religintensity prayer  churchattendance;
        COMMON@1 ;",
        rdata = west2012)

    fit.validity.religiosity.west2012.1fac  <- mplusModeler(validity.religiosity.west2012.1fac, run = 1L,modelout = "validity-religiosity-1fac-west-2012.inp")

  # Ost 2012 
      validity.religiosity.east2012.2fac  <- mplusObject(
        TITLE = "Secularism + Religiosity, 2 factors ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu ;",
        MODEL = "
          SEC BY nolimsci* noreleduc norelipol nogodeu ;
          SEC@1 ;
          RELIGIOSITY BY religintensity* prayer  churchattendance ;
          RELIGIOSITY@1 ;",
        rdata = east2012)

    fit.validity.religiosity.east2012.2fac  <- mplusModeler(validity.religiosity.east2012.2fac, run = 1L,modelout = "validity-religiosity-2fac-east-2012.inp")

      validity.religiosity.east2012.1fac  <- mplusObject(
        TITLE = "Secularism + Religiosity, 1 factor ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu ;",
        MODEL = "
        COMMON BY nolimsci* noreleduc norelipol nogodeu
        religintensity prayer  churchattendance;
        COMMON@1 ;",
        rdata = east2012)

fit.validity.religiosity.east2012.1fac  <- mplusModeler(validity.religiosity.east2012.1fac, run = 1L,modelout = "validity-religiosity-1fac-east-2012.inp")

  # Ok. We do the same:  a) a two-factor model (secularism with abortion and their correlation)
  # vs a single-factor model
  
  # West 2016 
      validity.abortion.west2016.2fac  <- mplusObject(
        TITLE = "Secularism + Abortion, 2 factors ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu 
        handicapabortionok healthabortionok rapeabortionok choiceabortionok;",
        MODEL = "
          SEC BY nolimsci* noreleduc norelipol nogodeu ;
          SEC@1 ;
          ABORTION BY handicapabortionok* healthabortionok 
                   rapeabortionok choiceabortionok  ;
          ABORTION@1 ;",
        rdata = west2016)
  
    fit.validity.abortion.west2016.2fac  <- mplusModeler(validity.abortion.west2016.2fac, run = 1L,modelout = "validity-abortion-2fac-west-2016.inp")
  
      validity.abortion.west2016.1fac  <- mplusObject(
        TITLE = "Secularism + Abortion, 1 factor ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu
        handicapabortionok healthabortionok rapeabortionok choiceabortionok;",
        MODEL = "
        COMMON BY nolimsci* noreleduc norelipol nogodeu
        handicapabortionok healthabortionok rapeabortionok choiceabortionok ;
        COMMON@1 ;",
        rdata = west2016)
  
    fit.validity.abortion.west2016.1fac  <- mplusModeler(validity.abortion.west2016.1fac, run = 1L,modelout = "validity-abortion-1fac-west-2016.inp")
  
  # Ost 2016 
      validity.abortion.east2016.2fac  <- mplusObject(
        TITLE = "Secularism + Abortion, 2 factors ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu 
        handicapabortionok healthabortionok rapeabortionok choiceabortionok;",
        MODEL = "
          SEC BY nolimsci* noreleduc norelipol nogodeu ;
          SEC@1 ;
          ABORTION BY handicapabortionok* healthabortionok 
          rapeabortionok choiceabortionok  ;
          ABORTION@1 ;",
        rdata = east2016)
  
    fit.validity.abortion.east2016.2fac  <- mplusModeler(validity.abortion.east2016.2fac, run = 1L,modelout = "validity-abortion-2fac-east-2016.inp")
  
      validity.abortion.east2016.1fac  <- mplusObject(
        TITLE = "Secularism + Abortion, 1 factor ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu
        handicapabortionok healthabortionok rapeabortionok choiceabortionok;",
        MODEL = "
        COMMON BY nolimsci* noreleduc norelipol nogodeu
        handicapabortionok healthabortionok rapeabortionok choiceabortionok ;
        COMMON@1 ;",
        rdata = east2016)
  
    fit.validity.abortion.east2016.1fac  <- mplusModeler(validity.abortion.east2016.1fac, run = 1L,modelout = "validity-abortion-1fac-east-2016.inp")
  
  # West 2012 
      validity.abortion.west2012.2fac  <- mplusObject(
        TITLE = "Secularism + Abortion, 2 factors ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu 
        handicapabortionok healthabortionok rapeabortionok choiceabortionok;",
        MODEL = "
          SEC BY nolimsci* noreleduc norelipol nogodeu ;
          SEC@1 ;
          ABORTION BY handicapabortionok* healthabortionok 
          rapeabortionok choiceabortionok  ;
          ABORTION@1 ;",
        rdata = west2012)
  
    fit.validity.abortion.west2012.2fac  <- mplusModeler(validity.abortion.west2012.2fac, run = 1L,modelout = "validity-abortion-2fac-west-2012.inp")
  
      validity.abortion.west2012.1fac  <- mplusObject(
        TITLE = "Secularism + Abortion, 1 factor ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu
        handicapabortionok healthabortionok rapeabortionok choiceabortionok;",
        MODEL = "
        COMMON BY nolimsci* noreleduc norelipol nogodeu
        handicapabortionok healthabortionok rapeabortionok choiceabortionok ;
        COMMON@1 ;",
        rdata = west2012)
  
    fit.validity.abortion.west2012.1fac  <- mplusModeler(validity.abortion.west2012.1fac, run = 1L,modelout = "validity-abortion-1fac-west-2012.inp")
  
  # Ost 2012 
      validity.abortion.east2012.2fac  <- mplusObject(
        TITLE = "Secularism + Abortion, 2 factors ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu 
        handicapabortionok healthabortionok rapeabortionok choiceabortionok;",
        MODEL = "
          SEC BY nolimsci* noreleduc norelipol nogodeu ;
          SEC@1 ;
          ABORTION BY handicapabortionok* healthabortionok 
          rapeabortionok choiceabortionok  ;
          ABORTION@1 ;",
        rdata = east2012)
  
    fit.validity.abortion.east2012.2fac  <- mplusModeler(validity.abortion.east2012.2fac, run = 1L,modelout = "validity-abortion-2fac-east-2012.inp")
  
      validity.abortion.east2012.1fac  <- mplusObject(
        TITLE = "Secularism + Abortion, 1 factor ",
        VARIABLE = "CATEGORICAL = nolimsci noreleduc norelipol nogodeu
        handicapabortionok healthabortionok rapeabortionok choiceabortionok;",
        MODEL = "
        COMMON BY nolimsci* noreleduc norelipol nogodeu
        handicapabortionok healthabortionok rapeabortionok choiceabortionok ;
        COMMON@1 ;",
        rdata = east2012)
  
  fit.validity.abortion.east2012.1fac  <- mplusModeler(validity.abortion.east2012.1fac, run = 1L,modelout = "validity-abortion-1fac-east-2012.inp")

# make tables 

  # Coefficient names are messed up. Replace them with something nicer
  custom.coeff.names.5  <- c(
  "Loading: Symbols",
  "Loading: Science",
  "Loading: Education",
  "Loading: Debates",
  "Loading: God EU",
  "Threshold 1: Symbols ",
  "Threshold 2: Symbols ",
  "Threshold 3: Symbols ",
  "Threshold 4: Symbols ",
  "Threshold 1: Science ",
  "Threshold 2: Science ",
  "Threshold 3: Science ",
  "Threshold 4: Science ",
  "Threshold 1: Education ",
  "Threshold 2: Education ",
  "Threshold 3: Education ",
  "Threshold 4: Education ",
  "Threshold 1: Debates ",
  "Threshold 2: Debates ",
  "Threshold 3: Debates ",
  "Threshold 4: Debates ",
  "Threshold 1: God EU ",
  "Threshold 2: God EU ",
  "Threshold 3: God EU ",
  "Threshold 4: God EU ",
  NA)


  custom.coeff.names.4  <- c(
  "Loading: Science",
  "Loading: Education",
  "Loading: Debates",
  "Loading: God EU",
  "Threshold 1: Science ",
  "Threshold 2: Science ",
  "Threshold 3: Science ",
  "Threshold 4: Science ",
  "Threshold 1: Education ",
  "Threshold 2: Education ",
  "Threshold 3: Education ",
  "Threshold 4: Education ",
  "Threshold 1: Debates ",
  "Threshold 2: Debates ",
  "Threshold 3: Debates ",
  "Threshold 4: Debates ",
  "Threshold 1: God EU ",
  "Threshold 2: God EU ",
  "Threshold 3: God EU ",
  "Threshold 4: God EU ",
  NA)

  
texreg(list(fit.validity.abortion.west2016.2fac,fit.validity.abortion.west2016.1fac,fit.validity.abortion.east2016.2fac,fit.validity.abortion.east2016.1fac,fit.validity.abortion.west2012.2fac,fit.validity.abortion.west2012.1fac,fit.validity.abortion.east2012.2fac,fit.validity.abortion.east2012.1fac), summaries=c("Observations","CFI","TLI","RMSEA_Estimate","SRMR","ChiSqM_Value","Parameters"),caption="Secularism and abortion",label="validation-abortion-comparison-table",custom.coef.map=list(" ABORTION<->SEC" = "$\\phi$"),custom.header = list("West 2016" = 1:2, "East 2016" = 3:4,"West 2012" = 5:6, "East 2012" = 7:8),custom.model.names = c(rep(c("two factors","one factor"),4)), sideways=TRUE,use.packages=FALSE,dcolumn=FALSE,threeparttable=FALSE)

  texreg(list(fit.validity.religiosity.west2016.2fac,fit.validity.religiosity.west2016.1fac,fit.validity.religiosity.east2016.2fac,fit.validity.religiosity.east2016.1fac,fit.validity.religiosity.west2012.2fac,fit.validity.religiosity.west2012.1fac,fit.validity.religiosity.east2012.2fac,fit.validity.religiosity.east2012.1fac), summaries=c("Observations","CFI","TLI","RMSEA_Estimate","SRMR","ChiSqM_Value","Parameters"),caption="Secularism and religiosity",label="validation-religiosity-comparison-table",custom.coef.map=list(" RELIGIOS<->SEC" = "$\\phi$"),custom.header = list("West 2016" = 1:2, "East 2016" = 3:4,"West 2012" = 5:6, "East 2012" = 7:8),custom.model.names = c(rep(c("two factors","one factor"),4)), sideways=TRUE,use.packages=FALSE,dcolumn=FALSE,threeparttable=FALSE)


  texreg(list(fit.west2016.5.ordinal,fit.east2016.5.ordinal,fit.west2012.5.ordinal,fit.east2012.5.ordinal),custom.model.names = c("West 2016", "East 2016", "West 2012", "East 2012"), summaries=c("Observations"),caption="Treshold parameters for the measurement of political secularism by five items",label="measurement-5-items-thresholds",custom.coef.names = custom.coeff.names.5,omit.coef="SEC",use.packages=FALSE,longtable=FALSE,dcolumn=FALSE,custom.note="%stars. This table shows the threshold parameters that were omitted from Table \\ref{measurement-5-items-loadings}",threeparttable=FALSE)

texreg(list(fit.west2016.42.ordinal,fit.east2016.42.ordinal,fit.west2012.42.ordinal,fit.east2012.42.ordinal),custom.model.names = c("West 2016", "East 2016", "West 2012", "East 2012"), summaries=c("Observations"),caption="Treshold parameters for the measurement of political secularism by four items",label="measurement-4-items-thresholds",custom.coef.names = custom.coeff.names.4,omit.coef="SEC",use.packages=FALSE,longtable=FALSE,dcolumn=FALSE,custom.note="%stars. This table shows the threshold parameters that were omitted from Table \\ref{measurement-4-items-loadings}",threeparttable=FALSE)

  abortion.measurement.west.2016  <- mplusObject(
    TITLE = "Measurement of abortion attitude, West 2016",
    VARIABLE = "CATEGORICAL = handicapabortionok healthabortionok rapeabortionok choiceabortionok ;",
    MODEL = "
      ABORTION BY handicapabortionok* healthabortionok rapeabortionok choiceabortionok ;
      ABORTION@1 ;",
    rdata = west2016) 

  fit.abortion.measurement.west.2016  <- mplusModeler(abortion.measurement.west.2016, run = 1L,modelout = "abortion-measurement-west-2016.inp")

  # East 2016 Abortion 
  abortion.measurement.east.2016  <- mplusObject(
    TITLE = "Measurement of abortion attitude, East 2016",
    VARIABLE = "CATEGORICAL = handicapabortionok healthabortionok rapeabortionok choiceabortionok ;",
    MODEL = "
      ABORTION BY handicapabortionok* healthabortionok rapeabortionok choiceabortionok ;
      ABORTION@1 ;",
    rdata = east2016) 

  fit.abortion.measurement.east.2016  <- mplusModeler(abortion.measurement.east.2016, run = 1L,modelout = "abortion-measurement-east-2016.inp")

  # West 2012 Abortion 
  abortion.measurement.west.2012  <- mplusObject(
    TITLE = "Measurement of abortion attitude, West 2012",
    VARIABLE = "CATEGORICAL = handicapabortionok healthabortionok rapeabortionok choiceabortionok ;",
    MODEL = "
      ABORTION BY handicapabortionok* healthabortionok rapeabortionok choiceabortionok ;
      ABORTION@1 ;",
    rdata = west2012) 

  fit.abortion.measurement.west.2012  <- mplusModeler(abortion.measurement.west.2012, run = 1L,modelout = "abortion-measurement-west-2012.inp")

  # East 2012 Abortion 
  abortion.measurement.east.2012  <- mplusObject(
    TITLE = "Measurement of abortion attitude, East 2012",
    VARIABLE = "CATEGORICAL = handicapabortionok healthabortionok rapeabortionok choiceabortionok ;",
    MODEL = "
      ABORTION BY handicapabortionok* healthabortionok rapeabortionok choiceabortionok ;
      ABORTION@1 ;",
    rdata = east2012) 

  fit.abortion.measurement.east.2012  <- mplusModeler(abortion.measurement.east.2012, run = 1L,modelout = "abortion-measurement-east-2012.inp")

    custom.coeff.names.abortionmeasurement  <- c(
    "Loading: Birth Defect",
    "Loading: Health",
    "Loading: Rape",
    "Loading: Choice",
    "Threshold: Birth Defect ",
    "Threshold: Health",
    "Threshold: Rape",
    "Threshold: Choice")




  library(lavaan)
  library(semTools)
  prepareMplusData(west2016 %>% select(starts_with("no")),"west2016.dat")
  prepareMplusData(east2016 %>% select(starts_with("no")),"east2016.dat")
  
  prepareMplusData(west2012 %>% select(starts_with("no")),"west2012.dat")
  prepareMplusData(east2012 %>% select(starts_with("no")),"east2012.dat")
  
  # Not run, b/c input needs to be tweaked
  ## pathmodelwest2016.5.ordinal.std  <- mplusObject(
  ##   TITLE = "Scaling West Germany 2016, five items, stdx, ordinal;",
  ##   VARIABLE = "
  ##     NAMES = nosymbols nolimsci noreleduc norelipol nogodeu; 
  ##     MISSING=.;
  ##     CATEGORICAL = nosymbols nolimsci noreleduc norelipol nogodeu ;",
  ##   SAVEDATA = "DIFFTEST = deriv1.dat ;",
  ##   OUTPUT = "stdyx  tech1;", 
  ##   MODEL = "
  ##     SEC BY nosymbols* nolimsci noreleduc norelipol nogodeu ;
  ##     SEC@1 ;",
  ##   rdata = west2016) 
  
  ## fit.west2016.5.ordinal.std  <- mplusModeler(pathmodelwest2016.5.ordinal.std, run = 1L,modelout = "scaling-west-2016-5-ordinal-std.inp")
  
  # initialise vectors for collecting alphas and omegas 
  # for models with five items 
  ordinalalpha.5  <- rep(NA,4)
  omega3.5  <- rep(NA,4)
  
  # for models with four items 
  ordinalalpha.4  <- rep(NA,4)
  omega3.4  <- rep(NA,4)
  
  # West 2016, 5 Items
  # re-run MPlus Model in lavaan
  congeneric5  <- mplus2lavaan("scaling-west-2016-5-ordinal-std.inp",run=TRUE)
  myreliability  <- reliability(congeneric5)
  ordinalalpha.5[1]  <- myreliability[1,1]
  omega3.5[1]  <- myreliability[4,1]
  
  # even for the 5-item model, the results are good 
  
  # East 2016, 5 Items
  # re-run MPlus Model in lavaan
  congeneric5  <- mplus2lavaan("scaling-east-2016-5-ordinal-std.inp",run=TRUE)
  myreliability  <- reliability(congeneric5)
  ordinalalpha.5[2]  <- myreliability[1,1]
  omega3.5[2]  <- myreliability[4,1]
  
  
  # West 2012, 5 Items
  # re-run MPlus Model in lavaan
  congeneric5  <- mplus2lavaan("scaling-west-2012-5-ordinal-std.inp",run=TRUE)
  myreliability  <- reliability(congeneric5)
  # even for the 5-item model, the results are good 
  ordinalalpha.5[3]  <- myreliability[1,1]
  omega3.5[3]  <- myreliability[4,1]
  
  # East 2012, 5 Items
  # re-run MPlus Model in lavaan
  congeneric5  <- mplus2lavaan("scaling-east-2012-5-ordinal-std.inp",run=TRUE)
  myreliability  <- reliability(congeneric5)
  ordinalalpha.5[4]  <- myreliability[1,1]
  omega3.5[4]  <- myreliability[4,1]
  
  
  # West 2016, 4 Item
  congeneric4  <- mplus2lavaan("scaling-west-2016-4-ordinal-std.inp",run=TRUE)
  myreliability  <- reliability(congeneric4)
  # slightly worse findings for the 4-item model 
  ordinalalpha.4[1]  <- myreliability[1,1]
  omega3.4[1]  <- myreliability[4,1]
  
  
  
  # East 2016, 4 Item
  congeneric4  <- mplus2lavaan("scaling-east-2016-4-ordinal-std.inp",run=TRUE)
  myreliability  <- reliability(congeneric4)
  ordinalalpha.4[2]  <- myreliability[1,1]
  omega3.4[2]  <- myreliability[4,1]
  
  
  # West 2012, 4 Item
  congeneric4  <- mplus2lavaan("scaling-west-2012-4-ordinal-std.inp",run=TRUE)
  myreliability  <- reliability(congeneric4)
  ordinalalpha.4[3]  <- myreliability[1,1]
  omega3.4[3]  <- myreliability[4,1]
  
  
  # East 2012, 4 Item
  congeneric4  <- mplus2lavaan("scaling-east-2012-4-ordinal-std.inp",run=TRUE)
  myreliability  <- reliability(congeneric4)
  ordinalalpha.4[4]  <- myreliability[1,1]
  omega3.4[4]  <- myreliability[4,1]
  



  prepareMplusData(west2016 %>% select(c("religintensity","prayer","churchattendance")),"west2016-relig.dat") 
  prepareMplusData(east2016 %>% select(c("religintensity","prayer","churchattendance")),"east2016-relig.dat") 

  prepareMplusData(west2012 %>% select(c("religintensity","prayer","churchattendance")),"west2012-relig.dat") 
  prepareMplusData(east2012 %>% select(c("religintensity","prayer","churchattendance")),"east2012-relig.dat") 

  alpharelig <- rep(NA,4)
  omegarelig <- rep(NA,4)

  #re-run MPlus Model in lavaan 
  # West 2016
  congeneric.relig <-  mplus2lavaan("relig-alpha-west-2016.inp",run=TRUE)
  myreliability <- reliability(congeneric.relig)
  alpharelig[1]  <- myreliability[1,1]
  omegarelig[1]  <- myreliability[2,1]

  # East 2016
  congeneric.relig <-  mplus2lavaan("relig-alpha-east-2016.inp",run=TRUE)
  myreliability <- reliability(congeneric.relig)
  alpharelig[2]  <- myreliability[1,1]
  omegarelig[2]  <- myreliability[2,1]

  # West 2012
  congeneric.relig <-  mplus2lavaan("relig-alpha-west-2012.inp",run=TRUE)
  myreliability <- reliability(congeneric.relig)
  alpharelig[3]  <- myreliability[1,1]
  omegarelig[3]  <- myreliability[2,1]

  # East 2012
  congeneric.relig <-  mplus2lavaan("relig-alpha-east-2012.inp",run=TRUE)
  myreliability <- reliability(congeneric.relig)
  alpharelig[4]  <- myreliability[1,1]
  omegarelig[4]  <- myreliability[2,1]

  prepareMplusData(west2016 %>% select(ends_with("abortionok")),"west2016-abortion.dat") 
  prepareMplusData(east2016 %>% select(ends_with("abortionok")),"east2016-abortion.dat") 

  prepareMplusData(west2012 %>% select(ends_with("abortionok")),"west2012-abortion.dat") 
  prepareMplusData(east2012 %>% select(ends_with("abortionok")),"east2012-abortion.dat") 

  ordinalalphaabortion <- rep(NA,4)
  ordinalomegaabortion <- rep(NA,4)

  ### West 2016
  congeneric.abortion <-  mplus2lavaan("abortion-alpha-west-2016.inp",run=TRUE)
  myreliability <- reliability(congeneric.abortion)
  ordinalalphaabortion[1]  <- myreliability[1,1]
  ordinalomegaabortion[1]  <- myreliability[4,1]

  ### East 2016
  congeneric.abortion <-  mplus2lavaan("abortion-alpha-east-2016.inp",run=TRUE)
  myreliability <- reliability(congeneric.abortion)
  ordinalalphaabortion[2]  <- myreliability[1,1]
  ordinalomegaabortion[2]  <- myreliability[4,1]

  ### West 2012
  congeneric.abortion <-  mplus2lavaan("abortion-alpha-west-2012.inp",run=TRUE)
  myreliability <- reliability(congeneric.abortion)
  ordinalalphaabortion[3]  <- myreliability[1,1]
  ordinalomegaabortion[3]  <- myreliability[4,1]

  ### East 2012
  congeneric.abortion <-  mplus2lavaan("abortion-alpha-east-2012.inp",run=TRUE)
  myreliability <- reliability(congeneric.abortion)
  ordinalalphaabortion[4]  <- myreliability[1,1]
  ordinalomegaabortion[4]  <- myreliability[4,1]

texreg(list(fit.abortion.measurement.west.2016,fit.abortion.measurement.east.2016,fit.abortion.measurement.west.2012,fit.abortion.measurement.east.2012),custom.model.names = c("West 2016", "East 2016", "West 2012", "East 2012"), summaries=c("Observations","CFI","TLI","RMSEA_Estimate","SRMR","ChiSqM_Value","Parameters"),caption="Measurement of abortion attitude",label="measurement-abortion",use.packages=FALSE,longtable=TRUE,dcolumn=FALSE,custom.note="%stars.",threeparttable=FALSE,omit.coef="ABORTION<->",custom.coef.names=custom.coeff.names.abortionmeasurement,custom.gof.rows = list("Ordinal $\\alpha$ " = ordinalalphaabortion,"Ordinal $\\omega_3$ ($\\rho_{NL}$)" = ordinalomegaabortion),reorder.gof = c(3:9,1:2))


  # West 2016 
  religiosity.measurement.west.2016  <- mplusObject(
    TITLE = "Measurement of religiosity, West 2016",
    MODEL = "
      RELIGIOSITY BY religintensity* prayer  churchattendance ;
      RELIGIOSITY@1 ;",
    rdata = west2016) 

  fit.religiosity.measurement.west.2016  <- mplusModeler(religiosity.measurement.west.2016, run = 1L,modelout = "religiosity-measurement-west-2016.inp")

  # Perfect model fit. Is there a problem? No there is not, as such: the model is just identified (b/c of the intercepts, there are more DFs)

  # East 2016

  religiosity.measurement.east.2016  <- mplusObject(
    TITLE = "Measurement of religiosity, East 2016",
    ANALYSIS = "ESTIMATOR = WLSMV ;",
    MODEL = "
      RELIGIOSITY BY religintensity* prayer churchattendance ;
      RELIGIOSITY@1 ;",
    rdata = east2016) 

  fit.religiosity.measurement.east.2016  <- mplusModeler(religiosity.measurement.east.2016, run = 1L,modelout = "religiosity-measurement-east-2016.inp")

  # Try 2012

  # West 2012 
  religiosity.measurement.west.2012  <- mplusObject(
    TITLE = "Measurement of religiosity, West 2012",
    ANALYSIS = "ESTIMATOR = WLSMV ;",
    MODEL = "
      RELIGIOSITY BY religintensity* prayer churchattendance ;
      RELIGIOSITY@1 ;",
    rdata = west2012) 

  fit.religiosity.measurement.west.2012  <- mplusModeler(religiosity.measurement.west.2012, run = 1L,modelout = "religiosity-measurement-west-2012.inp")

  # Perfect model fit. Is there a problem?

  # East 2012

  religiosity.measurement.east.2012  <- mplusObject(
    TITLE = "Measurement of religiosity, East 2012",
    ANALYSIS = "ESTIMATOR = WLSMV ;",
    MODEL = "
      RELIGIOSITY BY religintensity* prayer churchattendance ;
      RELIGIOSITY@1 ;",
    rdata = east2012) 

  fit.religiosity.measurement.east.2012  <- mplusModeler(religiosity.measurement.east.2012, run = 1L,modelout = "religiosity-measurement-east-2012.inp")



  texreg(list(fit.west2016.5.ordinal,fit.east2016.5.ordinal,fit.west2012.5.ordinal,fit.east2012.5.ordinal),custom.model.names = c("West 2016", "East 2016", "West 2012", "East 2012"), summaries=c("Observations","CFI","TLI","RMSEA_Estimate","SRMR","Parameters"),custom.gof.rows = list("Ordinal $\\alpha$ " = ordinalalpha.5,"Ordinal $\\omega_3$ ($\\rho_{NL}$)" = omega3.5),reorder.gof = c(3:8,1:2),caption="Measurement of political secularism by five items (factor loadings only)",label="measurement-5-items-loadings",custom.coef.names = custom.coeff.names.5,omit.coef="SEC<->SEC|Threshold",use.packages=FALSE,longtable=FALSE,dcolumn=FALSE,custom.note="%stars. Threshold parameters are omitted from the table for better legibility. Full set of parameters in Table \\ref{measurement-5-items-thresholds} in the appendix",threeparttable=FALSE)



  texreg(list(fit.west2016.42.ordinal,fit.east2016.42.ordinal,fit.west2012.42.ordinal,fit.east2012.42.ordinal),custom.model.names = c("West 2016", "East 2016", "West 2012", "East 2012"), summaries=c("Observations","CFI","TLI","RMSEA_Estimate","SRMR","Parameters"),custom.gof.rows = list("Ordinal $\\alpha$ " = ordinalalpha.4,"Ordinal $\\omega_3$ ($\\rho_{NL}$)" = omega3.4),reorder.gof = c(3:8,1:2),caption="Measurement of political secularism by four items (factor loadings only)",label="measurement-4-items-loadings",custom.coef.names = custom.coeff.names.4,omit.coef="SEC<->SEC|Threshold",use.packages=FALSE,longtable=FALSE,dcolumn=FALSE,custom.note="%stars. Threshold parameters are omitted from the table for better legibility. Full set of parameters in Table \\ref{measurement-4-items-thresholds} in the appendix",threeparttable=FALSE)




  custom.coeff.names.religmeasurement  <- c(
  "Loading: Intensity",
  "Loading: Prayer",
  "Loading: Church Attendance",
  "Intercept: Intensity ",
  "Intercept: Church Attendance ",
  "Intercept: Prayer ",
  "Variance: Intensity",
  "Variance: Church Attendance",
  "Variance: Prayer")



  texreg(list(fit.religiosity.measurement.west.2016,fit.religiosity.measurement.east.2016,fit.religiosity.measurement.west.2012,fit.religiosity.measurement.east.2012),custom.model.names = c("West 2016", "East 2016", "West 2012", "East 2012"), summaries=c("Observations","CFI","TLI","RMSEA_Estimate","SRMR","ChiSqM_Value","Parameters"),caption="Measurement of religiosity",label="measurement-religiosity",use.packages=FALSE,longtable=TRUE,dcolumn=FALSE,custom.note="%stars.",threeparttable=FALSE,omit.coef="RELIGIOSIT",custom.coef.names=custom.coeff.names.religmeasurement,reorder.coef=c(1,2,3,4,6,5,7,9,8),custom.gof.rows = list("$\\alpha$" = alpharelig,"$\\omega$"=omegarelig),reorder.gof = c(3:9,1:2))


  custom.coeff.names.validity.abortion.2fac <- c(
  "Loading: Science",
  "Loading: Education",
  "Loading: Debates",
  "Loading: God EU",
  "Loading: Birth Defect",
  "Loading: Health",
  "Loading: Rape",
  "Loading: Choice",
  "$\\phi$ Abortion, Secularism",
  "Threshold 1: Science ",
  "Threshold 2: Science ",
  "Threshold 3: Science ",
  "Threshold 4: Science ",
  "Threshold 1: Education ",
  "Threshold 2: Education ",
  "Threshold 3: Education ",
  "Threshold 4: Education ",
  "Threshold 1: Debates ",
  "Threshold 2: Debates ",
  "Threshold 3: Debates ",
  "Threshold 4: Debates ",
  "Threshold 1: God EU ",
  "Threshold 2: God EU ",
  "Threshold 3: God EU ",
  "Threshold 4: God EU ",
  "Threshold: Birth Defect ",
  "Threshold: Health",
  "Threshold: Rape",
  "Threshold: Choice")


    texreg(list(fit.validity.abortion.west2016.2fac,fit.validity.abortion.east2016.2fac,fit.validity.abortion.west2012.2fac,fit.validity.abortion.east2012.2fac),custom.model.names = c("West 2016", "East 2016", "West 2012", "East 2012"), summaries=c("Observations","CFI","TLI","RMSEA_Estimate","SRMR","Parameters"),caption="Political secularism and abortion: two-factor model",custom.coef.names=custom.coeff.names.validity.abortion.2fac,omit.coef="SEC<->SEC|ABORTION<->ABORTION",label="validation-abortion-two-factor-table",use.packages=FALSE,longtable=TRUE,dcolumn=FALSE,threeparttable=FALSE)


  custom.coeff.names.validity.abortion.1fac <- c(
  "Loading: Science",
  "Loading: Education",
  "Loading: Debates",
  "Loading: God EU",
  "Loading: Birth Defect",
  "Loading: Health",
  "Loading: Rape",
  "Loading: Choice",
  "Threshold 1: Science ",
  "Threshold 2: Science ",
  "Threshold 3: Science ",
  "Threshold 4: Science ",
  "Threshold 1: Education ",
  "Threshold 2: Education ",
  "Threshold 3: Education ",
  "Threshold 4: Education ",
  "Threshold 1: Debates ",
  "Threshold 2: Debates ",
  "Threshold 3: Debates ",
  "Threshold 4: Debates ",
  "Threshold 1: God EU ",
  "Threshold 2: God EU ",
  "Threshold 3: God EU ",
  "Threshold 4: God EU ",
  "Threshold: Birth Defect ",
  "Threshold: Health",
  "Threshold: Rape",
  "Threshold: Choice")


    texreg(list(fit.validity.abortion.west2016.1fac,fit.validity.abortion.east2016.1fac,fit.validity.abortion.west2012.1fac,fit.validity.abortion.east2012.1fac),custom.model.names = c("West 2016", "East 2016", "West 2012", "East 2012"), summaries=c("Observations","CFI","TLI","RMSEA_Estimate","SRMR","Parameters"),caption="Political secularism and abortion: one-factor model",custom.coef.names=custom.coeff.names.validity.abortion.1fac, omit.coef="COMMON<->COMMON",label="validation-abortion-one-factor-table",use.packages=FALSE,longtable=TRUE,dcolumn=FALSE,threeparttable=FALSE)


  custom.coeff.names.validity.religiosity.2fac <- c(
  "Loading: Science",
  "Loading: Education",
  "Loading: Debates",
  "Loading: God EU",
  "Loading: Intensity",
  "Loading: Prayer",
  "Loading: Church Attendance",
  "$\\phi$ Religiousness, Secularism",
  "Intercept: Intensity ",
  "Intercept: Church Attendance ",
  "Intercept: Prayer ",
  "Threshold 1: Science ",
  "Threshold 2: Science ",
  "Threshold 3: Science ",
  "Threshold 4: Science ",
  "Threshold 1: Education ",
  "Threshold 2: Education ",
  "Threshold 3: Education ",
  "Threshold 4: Education ",
  "Threshold 1: Debates ",
  "Threshold 2: Debates ",
  "Threshold 3: Debates ",
  "Threshold 4: Debates ",
  "Threshold 1: God EU ",
  "Threshold 2: God EU ",
  "Threshold 3: God EU ",
  "Threshold 4: God EU ",
  NA,
  NA, 
  "Variance: Intensity",
  "Variance: Church Attendance",
  "Variance: Prayer")


    texreg(list(fit.validity.religiosity.west2016.2fac,fit.validity.religiosity.east2016.2fac,fit.validity.religiosity.west2012.2fac,fit.validity.religiosity.east2012.2fac),custom.model.names = c("West 2016", "East 2016", "West 2012", "East 2012"), summaries=c("Observations","CFI","TLI","RMSEA_Estimate","SRMR","Parameters"),caption="Political secularism and religiousness: two-factor model",label="validation-religiosity-two-factor-table",use.packages=FALSE,longtable=TRUE,dcolumn=FALSE,omit.coef="SEC<->SEC|RELIGIOSIT<->RELIGIOSIT",custom.coef.names=custom.coeff.names.validity.religiosity.2fac,threeparttable=FALSE)

   custom.coeff.names.validity.religiosity.1fac <- c(
   "Loading: Science",
   "Loading: Education",
   "Loading: Debates",
   "Loading: God EU",
   "Loading: Intensity",
   "Loading: Prayer",
   "Loading: Church Attendance",
    "Intercept: Intensity ",
   "Intercept: Church Attendance ",
   "Intercept: Prayer ",
   "Threshold 1: Science ",
   "Threshold 2: Science ",
   "Threshold 3: Science ",
   "Threshold 4: Science ",
   "Threshold 1: Education ",
   "Threshold 2: Education ",
   "Threshold 3: Education ",
   "Threshold 4: Education ",
   "Threshold 1: Debates ",
   "Threshold 2: Debates ",
   "Threshold 3: Debates ",
   "Threshold 4: Debates ",
   "Threshold 1: God EU ",
   "Threshold 2: God EU ",
   "Threshold 3: God EU ",
   "Threshold 4: God EU ",
   "Variance: Intensity",
   "Variance: Church Attendance",
   "Variance: Prayer"
   )
  texreg(list(fit.validity.religiosity.west2016.1fac,fit.validity.religiosity.east2016.1fac,fit.validity.religiosity.west2012.1fac,fit.validity.religiosity.east2012.1fac),custom.model.names = c("West 2016", "East 2016", "West 2012", "East 2012"), summaries=c("Observations","CFI","TLI","RMSEA_Estimate","SRMR","Parameters"),caption="Political secularism and religiousness: one-factor model model",custom.coef.names=custom.coeff.names.validity.religiosity.1fac,omit.coef="COMMON<->COMMON",label="validation-religiosity-one-factor-table",use.packages=FALSE,longtable=TRUE,dcolumn=FALSE,threeparttable=FALSE)


