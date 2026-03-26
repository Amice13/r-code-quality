err <- 0.5
# Set the characteristics of the population you want to mimic. Here are: 1) The proportion of the voting population of Denmark who intended to vote for each party(vec1);
# 2) The proportion who are a) male and b) female; 3) The proportion of the voting population of Denmark who
# a) 51 or over with a degree; b) 51 or over with no degree; c) were 18 to 50 with a degree; d) 18 to 50 with no degree (vec3); 4) The proportion who have a) high interest and b) low interest in politics (vec4)
vec1 <- c(0.2129,0.1529,0.1335,0.0876,0.0732,0.0646,0.0523,0.0023,0.2000)
vec2 <- c(0.485,0.515)
vec3 <- c(0.130,0.370,0.184,0.316)
vec4 <- c(0.674,0.326)
# Continue the iteration until the total error is below 0.03
while (err>0.03) {
  i <- 0
  n <- 100000
  while (n>4){
    ndb <- dfr
    # seg1 ... 7 includes users who voted 1) SNP 2) Tory, 3) Labour, 4) LibDem, 5) Green; 6) UKIP; 7) Others
    seg1 <- dfr[dfr[,114]=='O',]
    seg2 <- dfr[dfr[,114]=='A',]
    seg3 <- dfr[dfr[,114]=='V',]
    seg4 <- dfr[dfr[,114]=='F',]
    seg5 <- dfr[dfr[,114]=='C',]
    seg6 <- dfr[dfr[,114]=='N',]
    seg7 <- dfr[dfr[,114]=='B',]
    seg8 <- dfr[dfr[,114]=='I',]
    seg9 <- dfr[dfr[,114]=='Undec',]
    coeff <- min(nrow(seg1)/vec1[1],nrow(seg2)/vec1[2],nrow(seg3)/vec1[3],nrow(seg4)/vec1[4],nrow(seg5)/vec1[5],nrow(seg6)/vec1[6],nrow(seg7)/vec1[7],nrow(seg8)/vec1[8],nrow(seg9)/vec1[9])
    s1 <- as.integer(vec1[1]*coeff); s2 <- as.integer(vec1[2]*coeff); s3 <- as.integer(vec1[3]*coeff); s4 <- as.integer(vec1[4]*coeff); s5 <- as.integer(vec1[5]*coeff); s6 <- as.integer(vec1[6]*coeff); s7 <- as.integer(vec1[7]*coeff); s8 <- as.integer(vec1[8]*coeff); s9 <- as.integer(vec1[9]*coeff)
    seg1 <- seg1[sample(nrow(seg1),s1,replace=FALSE),]
    seg2 <- seg2[sample(nrow(seg2),s2,replace=FALSE),]
    seg3 <- seg3[sample(nrow(seg3),s3,replace=FALSE),]
    seg4 <- seg4[sample(nrow(seg4),s4,replace=FALSE),]
    seg5 <- seg5[sample(nrow(seg5),s5,replace=FALSE),]
    seg6 <- seg6[sample(nrow(seg6),s6,replace=FALSE),]
    seg7 <- seg7[sample(nrow(seg7),s7,replace=FALSE),]
    seg8 <- seg8[sample(nrow(seg8),s8,replace=FALSE),]
    seg9 <- seg9[sample(nrow(seg9),s9,replace=FALSE),]
    dfr <- rbind(seg1,seg2,seg3,seg4,seg5,seg6,seg7,seg8,seg9)
    
    ref1 <- dfr[dfr[,115]=='Male',]
    ref2 <- dfr[dfr[,115]=='Female',]
    coeff <- min(nrow(ref1)/vec2[1],nrow(ref2)/vec2[2])
    r1 <- as.integer(vec2[1]*coeff); r2 <- as.integer(vec2[2]*coeff)
    ref1 <- ref1[sample(nrow(ref1),r1,replace=FALSE),]
    ref2 <- ref2[sample(nrow(ref2),r2,replace=FALSE),]
    dfr <- rbind(ref1,ref2)
    
    par1 <- dfr[dfr[,117]=='degreeHigh',]
    par2 <- dfr[dfr[,117]=='noDegreeHigh',]
    par3 <- dfr[dfr[,117]=='degreeLow',]
    par4 <- dfr[dfr[,117]=='noDegreeLow',]
    coeff <- min(nrow(par1)/vec3[1],nrow(par2)/vec3[2],nrow(par3)/vec3[3],nrow(par4)/vec3[4])
    p1 <- as.integer(vec3[1]*coeff); p2 <- as.integer(vec3[2]*coeff); p3 <- as.integer(vec3[3]*coeff); p4 <- as.integer(vec3[4]*coeff)
    par1 <- par1[sample(nrow(par1),p1,replace=FALSE),]
    par2 <- par2[sample(nrow(par2),p2,replace=FALSE),]
    par3 <- par3[sample(nrow(par3),p3,replace=FALSE),]
    par4 <- par4[sample(nrow(par4),p4,replace=FALSE),]
    dfr <- rbind(par1,par2,par3,par4)
    
    sref1 <- dfr[dfr[,116]=='interest',]
    sref2 <- dfr[dfr[,116]=='LowInterest',]
    coeff <- min(nrow(sref1)/vec4[1],nrow(sref2)/vec4[2])
    sr1 <- as.integer(vec4[1]*coeff); sr2 <- as.integer(vec4[2]*coeff)
    sref1 <- sref1[sample(nrow(sref1),sr1,replace=FALSE),]
    sref2 <- sref2[sample(nrow(sref2),sr2,replace=FALSE),]
    dfr <- rbind(sref1,sref2)
    
    if (i==0) dummy <- dfr
    if (i==1) dummy <- rbind(dummy,dfr)
    
    ndb <- rbind(ndb,dfr)
    
    mat1 <- duplicated(ndb)
    mat2 <- duplicated(ndb, fromLast = TRUE)
    mat <- mat1|mat2
    ndb <- cbind(ndb,mat)
    n <- nrow(dfr)
    dfr <- ndb[ndb$mat==FALSE,]
    dfr$mat <- NULL
    ndb <- dfr
    i <- 1
  }
  
  dfr <- dummy  
  
  propest1a <- nrow(dfr[dfr[,114]=='O',])/nrow(dfr); propest1b <- nrow(dfr[dfr[,114]=='A',])/nrow(dfr); propest1c <- nrow(dfr[dfr[,114]=='V',])/nrow(dfr); propest1d <- nrow(dfr[dfr[,114]=='F',])/nrow(dfr); propest1e <- nrow(dfr[dfr[,114]=='C',])/nrow(dfr); propest1f <- nrow(dfr[dfr[,114]=='N',])/nrow(dfr); propest1g <- nrow(dfr[dfr[,114]=='B',])/nrow(dfr); propest1h <- nrow(dfr[dfr[,114]=='I',])/nrow(dfr); propest1i <- nrow(dfr[dfr[,114]=='Undec',])/nrow(dfr)
  propest2a <- nrow(dfr[dfr[,115]=='Male',])/nrow(dfr); propest2b <- nrow(dfr[dfr[,115]=='Female',])/nrow(dfr)
  propest3a <- nrow(dfr[dfr[,117]=='degreeHigh',])/nrow(dfr); propest3b <- nrow(dfr[dfr[,117]=='noDegreeHigh',])/nrow(dfr); propest3c <- nrow(dfr[dfr[,117]=='degreeLow',])/nrow(dfr); propest3d <- nrow(dfr[dfr[,117]=='noDegreeLow',])/nrow(dfr)
  propest4a <- nrow(dfr[dfr[,116]=='interest',])/nrow(dfr); propest4b <- nrow(dfr[dfr[,116]=='LowInterest',])/nrow(dfr)
  err1 <- abs(vec1[1]-propest1a) + abs(vec1[2]-propest1b) + abs(vec1[3]-propest1c) + abs(vec1[4]-propest1d) + abs(vec1[5]-propest1e) + abs(vec1[6]-propest1f) + abs(vec1[7]-propest1g) + abs(vec1[8]-propest1h) + abs(vec1[9]-propest1i)
  err2 <- abs(vec2[1]-propest2a) + abs(vec2[2]-propest2b)
  err3 <- abs(vec3[1]-propest3a) + abs(vec3[2]-propest3b) + abs(vec3[3]-propest3c) + abs(vec3[4]-propest3d)
  err4 <- abs(vec4[1]-propest4a) + abs(vec4[2]-propest4b)
  err <- err1 + err2 + err3
}