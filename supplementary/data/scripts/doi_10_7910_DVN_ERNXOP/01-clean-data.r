
  # IMPORT DATA FOR LEFT-RIGHT PROBING QUESTIONS
    d <- read.dta("za4605_a08.dta", convert.underscore=TRUE,
                  convert.factors=FALSE)
    # d <- read.csv("za4605_a08_UTF8.csv", stringsAsFactors=F)
    d$left <- d$f29
    d$right <- d$f30

  # if (!bauer) d$left <- iconv(d$left, from="latin1", to="UTF-8")
  # if (!bauer) d$right <- iconv(d$right, from="latin1", to="UTF-8")

    d$left <- sub("^\\s+", "", d$left)   # trim leading white space
    d$left <- sub("^\\s+", "", d$left)   # again
    d$left <- sub("^\\s+", "", d$left)   # again
    d$right <- sub("^\\s+", "", d$right) # trim leading white space
    d$right <- sub("^\\s+", "", d$right)   # again
    d$right <- sub("^\\s+", "", d$right)   # again
    # if you trim matching with wordstems does not work for single responses
    d <- d[, c("v2", "v3", "left", "right")]
    names(d) <- c("id", "east", "left", "right")


  # IMPORT ALLBUS 2008 DATA (sociodemographics etc.)
    ZA4600_A08 <- read.csv("ZA4600_A08.txt", sep="#")
    names(ZA4600_A08)[2] <- "id"

  # MERGE PROBING DATA WITH ALLBUS 2008
    d <- left_join(d, ZA4600_A08, by="id") # changed from left_join


  # RECODE VARIABLES
    # left-right scale - v106 - 1 = left; 10 = right
      d$leftright <- mapvalues(d$v106, from = c(99), to = c(NA))
      table(d$leftright,d$v106)

    # east: Interview in east germany or not
      d$east <- mapvalues(d$east, from = c(1,2), to = c(0,1))
     

    # education - v173
      d$education <- mapvalues(d$v173, from = c(1,2,3,4,5,6,7,99), to = c(0,0,1,1,2,2,NA,NA))
      table(d$education,d$v173)

    # age: 18-29 JAHRE; 30-44 JAHRE; 45-59 JAHRE; 60-74 JAHRE; 75-89 JAHRE;
    #     UEBER 89 JAHRE
      d$age <- mapvalues(d$v155, from = c(9), to = c(NA))
      d$age <- d$age-1
      table(d$age, d$v155)

      
    # sex
      d$male <- mapvalues(d$v151, from = c(2), to = c(0))
      table(d$male, d$v151)

    # political interest: 2 = Very strong/strong, 1 = Middle, 0 = little/notatall
      d$pol.interest <- mapvalues(d$v100, from = c(1,2,3,4,5), to = c(2,2,1,0,0))
      table(d$pol.interest, d$v100)

    # income: higher values, higher income
      d$income <- NA
      d$income[d$v389>=0 & d$v389<=8] <- 0 # <= 999
      d$income[d$v389>=9 & d$v389<=14] <- 1 # 1000 <= x <= 1999
      d$income[d$v389>=15 & d$v389<=18] <- 2 # 2000 <= x <= 2999
      d$income[d$v389>=19 & d$v389<=22] <- 3 # 3000 <= x
      table(d$income, d$v389)

    # party.preference
      d$party.preference <- mapvalues(d$v70, from = c(0,1,2,3,4,5,6,7,8,97,99), 
                                      to = c("none", "cdu", "spd", "fdp"
                                             , "gruene", "other.parties", "other.parties", 
                                             "linke"
                                             , "other.parties", NA, NA))
      d$factor.party.preference <- factor(d$party.preference,
                                          levels = c("none", "cdu", "spd",
                                                     "gruene", "linke", "fdp",
                                                     "other.parties"))


      
      # Attitudindal scales
      vars <- paste("v", seq(21,32,1),sep="")
      for(i in vars){
        original <- d[,i]
        d[,i] <- mapvalues(d[,i], from = c(1,2,3,4,5, 8, 9), to = c(4,3,2,1,0,NA, NA))
        new <- d[,i]
        #print(table(new, original))
      }
      # TOTALLY AGREE = 4; TOTALLY DISAAGREE = 0, 2 = neither..nor
      

     
      d <- dplyr::select(d, id, east, left, right, leftright, education, age, male, pol.interest, income, 
             factor.party.preference, immigrant.adapt = v21, politics.abstain = v22, measures.environment = v23,
             samesexmarriage= v24, menwomen.equal = v25, stiffer.sentences = v26,
             social.security = v27, income.redist = v28, immigrant.good = v29,
             war.terrorism = v30, ind.rights = v31, open.globalmarkets = v32)


#########################################################################################################
## LEFTRIGHT: CLEAN DATA -> DON'T KNOWS
#########################################################################################################


  # HOW MANY GAVE NO REPONSE AT ALL? (page 11)
    sum(d$left=="", na.rm=TRUE)/nrow(d) # 713 = 21%
    sum(d$right=="", na.rm=TRUE)/nrow(d) # 664 = 19%

    # What is the overlap in this group? (page 11)
      sum(d$left=="" & d$right=="", na.rm=TRUE)/nrow(d) # 17%

  # HOW MANY DON'T KNOWS ARE THERE?
  # LEFT
    d <- d[order(d$left),]


  # DELETE DEFINITIONAL STARTINGS
    cbind(d$id[str_detect(d$left, "^sind ")], d$left[str_detect(d$left, "^sind ")]) # check
    for (i in 1:3469){ # replace definitional startings..
      d$left[i] <-  str_replace(d$left[i], "^sind ", "")
      d$left[i] <-  str_replace(d$left[i], "^links ist", "")
      d$left[i] <-  str_replace(d$left[i], "^links sind", "")
    }
  # DON't KNOWS - LEFT (p.11)
    answers <- c("gar nichts", "garnichts", "ich kann dazu nichts sagen",
                 "ich kann es nicht beschreiben", "ich verstehe das nicht",
                 "ich weiss es nicht", "kann dazu nichts sagen",
                 "kann ich nicht erklären", "kann ich nicht genau sagen",
                 "kann ich nicht sagen", "keine ahnung", "keine angabe",
                 "keine antwort", "keine meinung", "kenne ich mich nicht aus",
                 "nichts bestimmtes", "weiss nicht",
                 "weiss ich nicht", "weis nicht")
    # Answers that include these phrases are excluded
    # SHOW ANSWERS
    for (i in answers){print(cbind(d$id[str_detect(d$left, i)], d$left[str_detect(d$left, i)]))}
    for (i in answers){d$left[str_detect(d$left, i)] <- ""}
    # reinstate answers that have been wrongly coded by the above loop
      d$left[d$id==1749] <- "die linkspartei"
      d$left[d$id==527] <- "die linken eben, die pds"
    # check again for different words
      # x <- cbind(d$id[str_detect(d$left, "nichts")],
      # d$left[str_detect(d$left, "nichts")]) #  detect different terms such as "nichts", "wn" etc.

      additional.dontknows <- c(2806, 717, 2544, 1627, 1346, 1279, 3331, 1280,
                              224, 225, 643, 475, 1875, 2457, 1929, 68, 2261,
                              1801, 1380, 444,2833, 1293,116,1068,1407,3206,
                               222,533,358,210,2027,358,1231,790,2717,673,15,
                              191,564,730,1057,1073,1448,1816,1858,1883,2576,
                              2607,2770,3258,2713,2732,64,1693,1558,576,888,
                              1438,843,2387,58,1166,2498)
    for (i in additional.dontknows){print(d$left[d$id==i]);
                                    d$left[d$id==i] <- ""} # CHECK AND REPLACE
    (sum(d$left=="", na.rm=TRUE)-713)/nrow(d) # dont know: 195 = 6%, 713 was calc. above


  # DON't KNOWS - RIGHT
    d <- d[order(d$right),]
    # DEFINITIONAL STARTINGS
    for (i in 1:3469){ # replace definitional startings..
      d$right[i] <-  str_replace(d$right[i], "^sind ", "")
      d$right[i] <-  str_replace(d$right[i], "^rechts ist", "")
      d$right[i] <-  str_replace(d$right[i], "^rechts sind", "")
    }
    # DONT KNOWS
      answers <- c("gar nichts", "garnichts", "ich kann dazu nichts sagen",
                   "ich kann es nicht beschreiben", "ich verstehe das nicht",
                   "ich weiss es nicht", "kann dazu nichts sagen",
                   "kann ich nicht erklären", "kann ich nicht genau sagen",
                   "kann ich nicht sagen", "keine ahnung", "keine angabe",
                   "keine antwort", "keine meinung", "kenne ich mich nicht aus",
                   "nichts bestimmtes", "weiss nicht", "weiss ich nicht",
                   "weis nicht")
    # Answers that include these phrases are exclude
      for (i in answers){print(cbind(d$id[str_detect(d$right, i)],
                                     d$right[str_detect(d$right, i)]))}
      for (i in answers){d$right[str_detect(d$right, i)] <- ""}
    # reinstate answers that have been wrongly coded by the above loop
      d$right[d$id==2264] <- "rechtsradikal"
      d$right[d$id==1683] <- "nazis"
      d$right[d$id==544] <- "neonazis"
      d$right[d$id==2306] <- "ausländerfeindlichkeit"
      d$right[d$id==1853] <- "npd"
    # additional don't knows
    additional.nonresponses <- c(2849, 179, 1842, 2041, 1377, 3070, 486, 1227,
                                 2484, 3099, 72, 2294, 3206, 222, 2831, 2845,
                                 1498, 210, 1313, 2027, 358, 3331, 224, 1085,
                                 1222, 541, 691, 1665, 1231, 250, 1708, 3447,
                                 790, 2361, 55, 3403, 3317, 2952, 2572, 1473,
                                 2031, 1117, 3468, 2359, 1875, 1729, 3216, 105,
                                 191, 564, 730, 1057, 1073, 1448, 1816, 1858,
                                 1883, 2576, 2607, 2770, 3258, 3193, 2301, 1433,
                                 3143, 2713, 2981, 272, 1716, 576, 3301, 280,
                                 3418, 2343, 2387, 2254, 1817, 2721, 2261, 394,
                                 58, 1166, 2498)
    for (i in additional.nonresponses){print(d$right[d$id==i]);
                                       d$right[d$id==i] <- ""} #
    (sum(d$right=="", na.rm=TRUE)-664)/nrow(d) # dont know: 177 = 5%

    # PAGE 9: Non-respondents - What is the overlap now and how are the
    # distributed on the left-right scale? (p.11-12)
      sum(d$left=="" & d$right=="", na.rm=TRUE)/nrow(d)
      sum(d$left!="" & d$right!="", na.rm=TRUE)

      mean(d$leftright[d$left=="" & d$right=="" & !is.na(d$leftright)], na.rm=T) # numbers in text
      var(d$leftright[d$left=="" & d$right=="" & !is.na(d$leftright)], na.rm=T) # numbers in text


      mean(d$leftright[d$left!="" & d$right!="" & !is.na(d$leftright)], na.rm=T)
      var(d$leftright[d$left!="" & d$right!="" & !is.na(d$leftright)], na.rm=T)
      
      
      
# No association people and their don't know answers to policy questions  
      # Create dummy for association don't knows
      d$dk.dummy <- 0
      d$dk.dummy[d$left=="" & d$right==""] <- 1
      
      d$dk.immigrant.adapt <- 0
      d$dk.immigrant.adapt[is.na(d$immigrant.adapt)] <- 1     
      table(d$dk.immigrant.adapt, d$immigrant.adapt, useNA = "always")
      table(d$dk.immigrant.adapt, d$dk.dummy)
      
      vars <- c("immigrant.adapt", "politics.abstain", "measures.environment",
                "samesexmarriage", "menwomen.equal", "stiffer.sentences",
                "social.security", "income.redist", "immigrant.good",
                "war.terrorism", "ind.rights", "open.globalmarkets")
      NAs <- NULL
      for(i in vars){
      #print(i)
      NAs.i <- summary(d[,i])[7]
      #print(NAs.i)
      NAs <- c(NAs, NAs.i)
      
      # generate dummy vars
      d[,paste("dk.", i, sep="")] <- 0
      d[,paste("dk.", i, sep="")][is.na(d[,i])] <- 1
      cr.table <- table(d[,paste("dk.", i, sep="")], d$dk.dummy)
      print(summary(lm(paste(paste("d$dk.", i, sep=""), " ~ ", "d$dk.dummy", sep=""))))
      #print(cr.table)
      }
      sum(NAs)/length(NAs)

      

