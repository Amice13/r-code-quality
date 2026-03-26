########### This file analyzes ESS data on support for democracy for supporters of parties in government and in opposition
########### and replicates the analysis in Mazepus and Toshkov (2021), Comparative Political Studies
########### Part 1 of 2. Last update: 1 September 2021

### Libraries and functions --------------------------------------------
library(essurvey) # to access ESS data
library(labelled) 
library(tidyverse)
library(countrycode)
library(wbstats)
sum.na<-function (x) sum(x, na.rm=T) #sum that aviods NAs, returns 0 if all NAs
sum.allna <- function(x) if (all(is.na(x))) NA else sum(x,na.rm=T) #sum that avoids NAs but returns NA if all NAs
rowsum.na<-function (x) rowSums(x, na.rm=T) #sum by row that avoids NAs 
rowsum.allna <- function(x) rowSums(x, na.rm=TRUE) * ifelse(rowSums(is.na(x)) == ncol(x), NA, 1) #sum by row that avoids NAs but gives NA for a row with all NAs

#set_email("xxx@xxx.xxx") # set your registered email to access the ESS data

### Load the data --------------------------------------------
dat6 <- import_rounds(6, format = 'spss') # ESS Round 6, 2012
# Alternatively, ESS data can be downloaded from https://www.europeansocialsurvey.org/data/download.html?r=6
### Recode variables --------------------------------------------

## Date of interview
dat6$date <- as.Date(paste(dat6$inwyys, dat6$inwmms, dat6$inwdds, sep='-'))

## Demographics
dat6$age <- dat6$agea
dat6$sex <- ifelse(dat6$gndr==1, 'male' ,'female')
dat6$edu <- as.numeric(dat6$eisced)
dat6$edu <- ifelse (dat6$edu == 55, NA, dat6$edu)
dat6$empl <- ifelse (dat6$mnactic==1, 'paid work', ifelse (dat6$mnactic==2, 'studying', 'not_working')  ) 

## Political variables
# Voted
dat6$voted<- ifelse(dat6$vote==1, 1,0) # 

# Satisfaction with democracy, government, and policy outcomes
dat6$sat.democracy <- as.numeric(as.character(dat6$stfdem)) # Satisfaction with democracy ( "How satisfied with the way democracy works in country", from 'Extremely dissatisfied' to 'Extremely satisfied', 11 categories)
dat6$sat.government <- as.numeric(dat6$stfgov)
dat6$sat.economy <- as.numeric(dat6$stfeco)

# Ideological positions
dat6$lr <- dat6$lrscale
dat6$lr.ex<- abs(5 - dat6$lrscale)

# Party attachment
dat6$party.close <-as.numeric(dat6$prtdgcl)  # 1 is very close
dat6$party.close.any<- ifelse(dat6$clsprty==1, 1,0)  # close to any party

# these are about importance in general. There are questions that are asked in different ways (best for democract, responsivenss, single party)
dat6$imp.democracy <- as.numeric(dat6$implvdm)
dat6$imp.fair.elections<-as.numeric(dat6$fairelc)
dat6$imp.opposition.free<-as.numeric(dat6$oppcrgv)
dat6$imp.media.free<-as.numeric(dat6$medcrgv)
dat6$imp.courts.stop<-as.numeric(dat6$ctstogv)
dat6$imp.dem.checks <- rowsum.allna (dat6[,c('imp.opposition.free', 'imp.media.free', 'imp.courts.stop')])/3

### Recode parties / votes ----------------------------------------------------------
# Albania, 2012
dat6$party.maingov.al <- ifelse ((dat6$prtvtal == 2  ), 1, 0) # main party in government: PD
dat6$party.gov.al <- ifelse ((dat6$prtvtal == 2 | dat6$prtvtal == 3), 1 ,0) # all parties in government:  PD and Socialist Movement for Integration

# Kosovo, 2012
dat6$party.maingov.ks <- ifelse ((dat6$prtvtxk == 2  ), 1, 0) # main party in government: PDK
dat6$party.gov.ks <- ifelse ((dat6$prtvtxk == 2), 1 ,0) # all parties in government: PDK

# Iceland, 2012
dat6$party.maingov.is <- ifelse ((dat6$prtvtais == 1  ), 1, 0) # main party in government: SDA
dat6$party.gov.is <- ifelse ((dat6$prtvtais  == 1 | dat6$prtvtais  == 4 ), 1 ,0) # all parties in government: SDA LGM

# Russia, 2012
dat6$party.maingov.ru <- ifelse ((dat6$prtvtcru== 1  ), 1, 0) # main party in government: United Russia
dat6$party.gov.ru <- ifelse ((dat6$prtvtcru == 1 ), 1 ,0) # all parties in government: United Russia

# Ukraine, 2012
dat6$party.maingov.ua <- ifelse ((dat6$prtvtcua== 5  ), 1, 0) # main party in government: President Party of regions
dat6$party.gov.ua <- ifelse ((dat6$prtvtcua == 5 ), 1 ,0) # all parties in government: President Party of regions

# Denmark, 2012
dat6$party.maingov.dk <- ifelse ((dat6$prtvtcdk == 1  ), 1, 0) # main party in government: SocDem
dat6$party.gov.dk <- ifelse ((dat6$prtvtcdk == 1 | dat6$prtvtcdk == 2 | dat6$prtvtcdk == 4), 1 ,0) # all parties in government:  SocDem, SocLib, and PopSoc

# Switzerland, 2012
dat6$party.maingov.ch <- ifelse ((dat6$prtvtdch == 1  ), 1, 0) # main party in government:
dat6$party.gov.ch <- ifelse ((dat6$prtvtdch == 1 | dat6$prtvtdch == 2 | dat6$prtvtdch == 4), 1 ,0) # all parties in government:  

# Slovakia, 2012
dat6$party.maingov.sk <- ifelse ((dat6$prtvtcsk== 3  ), 1, 0) # main party in government: SMER
dat6$party.gov.sk <- ifelse ((dat6$prtvtcsk == 3 ), 1 ,0) # all parties in government: SMER
dat6$party.sk.smer <- ifelse ((dat6$prtvtcsk == 3 ), 1 ,0) # SMER
dat6$party.sk.sns <- ifelse ((dat6$prtvtcsk == 7 ), 1 ,0) # sns, in fact other because sns is missing
dat6$party.sk.most <- ifelse ((dat6$prtvtcsk == 4 ), 1 ,0) # most

# Sweden, 2012
dat6$party.maingov.se <- ifelse ((dat6$prtvtbse == 5  ), 1, 0) # main party in government: Modearates
dat6$party.gov.se <- ifelse ((dat6$prtvtbse == 5 | dat6$prtvtbse == 1 | dat6$prtvtbse == 2 | dat6$prtvtbse == 3), 1 ,0) # all parties in government:  Moderate Party, Centre Party, Liberal People's Party and the Christian Democrats.
dat6$party.se.sd <- ifelse ((dat6$prtvtbse ==  6), 1, 0) # cdu
dat6$party.se.green <- ifelse ((dat6$prtvtbse == 4 ), 1, 0) # fdp
dat6$party.se.moder <- ifelse ((dat6$prtvtbse ==  5), 1, 0) # moderates
dat6$party.se.center <- ifelse ((dat6$prtvtbse ==  1), 1, 0) # center
dat6$party.se.lib <- ifelse ((dat6$prtvtbse ==  2), 1, 0) # liberal
dat6$party.se.cd <- ifelse ((dat6$prtvtbse ==  3), 1, 0) # chr dem

# Portugal, 2012 
dat6$party.maingov.pt <- ifelse ((dat6$prtvtbpt == 10  ), 1, 0) # main party in government: PSD
dat6$party.gov.pt <- ifelse ((dat6$prtvtbpt == 10 | dat6$prtvtbpt == 2), 1 ,0) # all parties in government:  PSD and CDS/PP
dat6$party.pt.sp <- ifelse ((dat6$prtvtbpt == 11  ), 1, 0) # sp
dat6$party.pt.psd <- ifelse ((dat6$prtvtbpt == 10  ), 1, 0) # psd
dat6$party.pt.cds <- ifelse ((dat6$prtvtbpt == 2  ), 1, 0) # cds/pp

# Spain, 2012
dat6$party.maingov.sp <- ifelse ((dat6$prtvtces == 1  ), 1, 0) # main party in government: PP
dat6$party.gov.sp <- ifelse ((dat6$prtvtces == 1 | dat6$prtvtces == 1), 1 ,0) # all parties in government:  PP
dat6$party.sp.pp <- ifelse ((dat6$prtvtces == 1  ), 1, 0) # PP
dat6$party.sp.psoe <- ifelse ((dat6$prtvtces == 2  ), 1, 0) # PP

# Germany, 2012 (I)
dat6$party.maingov.de <- ifelse ((dat6$prtvdde1 == 2  ), 1, 0) # main party in government: CDU
dat6$party.gov.de <- ifelse ((dat6$prtvdde1 == 2 | dat6$prtvdde1 == 4), 1 ,0) # all parties in government:  CDU/FDP
dat6$party.de.cdu <- ifelse ((dat6$prtvdde1 ==  2), 1, 0) # cdu
dat6$party.de.fdp <- ifelse ((dat6$prtvdde1 == 4 ), 1, 0) # fdp
dat6$party.de.spd <- ifelse ((dat6$prtvdde1 == 1 ), 1, 0) # spd

# Germany, 2012 (II)
dat6$party.maingov.de2 <- ifelse ((dat6$prtvdde2 == 2  ), 1, 0) # main party in government: CDU
dat6$party.gov.de2 <- ifelse ((dat6$prtvdde2 == 2 | dat6$prtvdde2 == 4), 1 ,0) # all parties in government:  CDU/FDP
dat6$party.de2.cdu <- ifelse ((dat6$prtvdde2 ==  2), 1, 0) # cdu
dat6$party.de2.fdp <- ifelse ((dat6$prtvdde2 == 4 ), 1, 0) # fdp
dat6$party.de2.spd <- ifelse ((dat6$prtvdde2 == 1 ), 1, 0) # spd

# Estonia, 2012
dat6$party.maingov.ee <- ifelse ((dat6$prtvtdee == 3  ), 1, 0) # main party in government: Estonian reform party
dat6$party.gov.ee <- ifelse ((dat6$prtvtdee == 3 | dat6$prtvtdee == 1), 1 ,0) # all parties in government:  Estonian reform party and ISAMAA
dat6$party.ee.ere <- ifelse ((dat6$prtvtdee ==  3), 1, 0) # ERE
dat6$party.ee.irl <- ifelse ((dat6$prtvtdee == 1 ), 1, 0) # ISAMAA
dat6$party.ee.eke <- ifelse ((dat6$prtvtdee == 2 ), 1, 0) # EKE
dat6$party.ee.sde <- ifelse ((dat6$prtvtdee == 5 ), 1, 0) # SDE

# Belgium, 2012
dat6$party.maingov.be <- ifelse ((dat6$prtvtcbe == 13 | dat6$prtvtcbe == 5 ), 1, 0) # main party in government: Sp.A/PS
dat6$party.gov.be <- ifelse ((dat6$prtvtcbe == 13 | dat6$prtvtcbe == 5  | dat6$prtvtcbe == 2  | dat6$prtvtcbe == 9  | dat6$prtvtcbe == 8 | dat6$prtvtcbe == 12), 1, 0) # all parties in government:  Sp.A/PS and VLD/MR and CD&V/CDH
dat6$party.be.soc <- ifelse ((dat6$prtvtcbe == 13 |  dat6$prtvtcbe == 5), 1, 0) # Socialist: PS and SP.A
dat6$party.be.cdv <- ifelse ((dat6$prtvtcbe == 2 |  dat6$prtvtcbe == 9), 1, 0) # CD&V and CDH
dat6$party.be.vld <- ifelse ((dat6$prtvtcbe == 8 |  dat6$prtvtcbe == 12), 1, 0) # VLD and MR

# Hungary, 2012
dat6$party.maingov.hu <- ifelse ((dat6$prtvtdhu == 3), 1, 0) # main party in government: FIDESZ
dat6$party.gov.hu <- ifelse ((dat6$prtvtdhu == 3 ), 1, 0) # all parties in government: FIDESZ
dat6$party.hu.fidesz <- ifelse ((dat6$prtvtdhu == 3), 1, 0) # FIDESZ
dat6$party.hu.mszp <- ifelse ((dat6$prtvtdhu == 8), 1, 0) # MSZP (socialist)
dat6$party.hu.jobbik <- ifelse ((dat6$prtvtdhu == 4), 1, 0) #  Jobbik

# France, 2012 (08.02.13-30.06.13)
dat6$party.maingov.fr <- ifelse ((dat6$prtvtcfr == 9), 1, 0) # main party in government: Socialist
dat6$party.gov.fr <- ifelse ((dat6$prtvtcfr == 9 | dat6$prtvtcfr == 12 | dat6$prtvtcfr==7), 1, 0) # all parties in government: Socialist, EELV, Radical Party of the Left
dat6$party.fr.soc <- ifelse ((dat6$prtvtcfr == 9), 1, 0) # Socialist
dat6$party.fr.eelv <- ifelse ((dat6$prtvtcfr == 12), 1, 0) # EELV
dat6$party.fr.modem <- ifelse ((dat6$prtvtcfr == 11), 1, 0) # Mov Dem
dat6$party.fr.ump <- ifelse ((dat6$prtvtcfr == 10), 1, 0) # UMP

# UK, 2012
dat6$party.maingov.uk <- ifelse ((dat6$prtvtgb == 1), 1, 0) # main party in government: Conservative
dat6$party.gov.uk <- ifelse ((dat6$prtvtgb == 1 | dat6$prtvtgb == 3), 1, 0) # all parties in government: Conservatice and LibDem
dat6$party.uk.cons <- ifelse (dat6$prtvtgb == 1, 1, 0) # Conservative
dat6$party.uk.lab <- ifelse (dat6$prtvtgb == 2, 1, 0) # Labour
dat6$party.uk.libdem <- ifelse (dat6$prtvtgb == 3, 1, 0) # Liberal Democrats

# Poland, 2012
dat6$party.maingov.pl <- ifelse ((dat6$prtvtcpl == 2), 1, 0) # main party in government: Civic Platform
dat6$party.gov.pl <- ifelse ((dat6$prtvtcpl == 2 | dat6$prtvtcpl == 5), 1, 0) # all parties in government: Civil Platform and Polish Peasants Party
dat6$party.pl.pis <- ifelse (dat6$prtvtcpl == 6, 1, 0) # Law and Justice
dat6$party.pl.cp <- ifelse (dat6$prtvtcpl == 2, 1, 0) # Civic Platform
dat6$party.pl.psl <- ifelse (dat6$prtvtcpl == 5, 1, 0) # Polish Peasants Party

# Czechia, 2012
dat6$party.maingov.cz <- ifelse ((dat6$prtvtccz == 5), 1, 0) # main party in government: ODS
dat6$party.gov.cz <- ifelse ((dat6$prtvtccz == 5 | dat6$prtvtccz == 3 | dat6$prtvtccz == 4), 1, 0) # all parties in government: ODS, TOP-09 and VV
dat6$party.cz.ods <- ifelse (dat6$prtvtccz == 5, 1, 0) # ODS
dat6$party.cz.top09 <- ifelse (dat6$prtvtccz == 3, 1, 0) # TOP-09
dat6$party.cz.vv <- ifelse (dat6$prtvtccz == 4, 1, 0) # VV
dat6$party.cz.cssd <- ifelse (dat6$prtvtccz == 2, 1, 0) # CSSD

# Slovenia, 2012
dat6$party.maingov.si <- ifelse ((dat6$prtvtdsi == 4), 1, 0) # main party in government: Slovenian Democratic Party - SDS 
dat6$party.gov.si <- ifelse ((dat6$prtvtdsi == 4 | dat6$prtvtdsi == 3 | dat6$prtvtdsi == 8 | dat6$prtvtdsi == 5), 1, 0) # main party in government: Slovenian Democratic Party - SDS, New Slovenia - Christian Peoples Party - NSi, Civic List - DL, Slovene Peoples Party - SLS   
dat6$party.si.sds <- ifelse (dat6$prtvtdsi == 4, 1, 0) # SDS
dat6$party.si.sls <- ifelse (dat6$prtvtdsi == 5, 1, 0) # SLS
dat6$party.si.desus <- ifelse (dat6$prtvtdsi == 1, 1, 0) # DESUS
dat6$party.si.sd <- ifelse (dat6$prtvtdsi == 6, 1, 0) # SD
dat6$party.si.nsi <- ifelse (dat6$prtvtdsi == 3, 1, 0) # NSI

# Bulgaria, 2012 (Data collection was right in the middle of a government change [Feb - March 2013] with PM from GERB resigning)
dat6$party.maingov.bg <- ifelse (dat6$prtvtcbg == 1, 1, 0) # main party in government: GERB
dat6$party.gov.bg <- ifelse (dat6$prtvtcbg == 1, 1, 0) # all parties in government: GERB
dat6$party.bg.gerb <- ifelse (dat6$prtvtcbg == 1, 1, 0)
dat6$party.bg.vmro <- ifelse (dat6$prtvtcbg == 5, 1, 0)
dat6$party.bg.nfsb <- ifelse (dat6$prtvtcbg == 7, 1, 0)
dat6$party.bg.bsp <- ifelse (dat6$prtvtcbg == 2, 1, 0)

# Italy, 2012 (in fact second semester of 2013)
dat6$party.maingov.it <- ifelse (dat6$prtvtbit == 1, 1, 0) # main party in government: Partido Democratico (PD) 
dat6$party.gov.it <- ifelse ((dat6$prtvtbit == 1 | dat6$prtvtbit == 8 | dat6$prtvtbit == 5 | dat6$prtvtbit == 6 | dat6$prtvtbit == 11 ) , 1, 0) # all parties in government: Partido Democratico (PD), Popolo delle Libertà (PdL), Scelta Civica (con Monti), Union of the Centre (UDC), Radicali Italiani (Amnistia giustizia e libertà)    
dat6$party.it.pd <- ifelse (dat6$prtvtbit == 1, 1, 0) # PD
dat6$party.it.pdl <- ifelse (dat6$prtvtbit == 8, 1, 0) # PdL
dat6$party.it.sc <- ifelse (dat6$prtvtbit == 5, 1, 0) # scelta civica
dat6$party.it.udc <- ifelse (dat6$prtvtbit == 6, 1, 0) # UDC
dat6$party.it.ri <- ifelse (dat6$prtvtbit == 11, 1, 0) # Radicali Italiani (nobody voted for them)!
dat6$party.it.ms5 <- ifelse (dat6$prtvtbit == 4, 1, 0) # Movimento 5 stelle
dat6$party.it.lega <- ifelse (dat6$prtvtbit == 9, 1, 0) # Lega Nord

# Norway, 2012 (August 2012 - February 2013)
dat6$party.maingov.no <- ifelse (dat6$prtvtano == 3, 1, 0) # main party in government: Labour Party (A)
dat6$party.gov.no <- ifelse ((dat6$prtvtano == 3 | dat6$prtvtano == 2 | dat6$prtvtano == 6 ) , 1, 0) # all parties in government: Labour Party (A), Socialist Left Party (SV), Centre Party (SP) 
dat6$party.no.a <- ifelse (dat6$prtvtano == 3, 1, 0) # Labour Party (A)
dat6$party.no.sv <- ifelse (dat6$prtvtano == 2, 1, 0) #  Socialist Left Party (SV) 
dat6$party.no.sp <- ifelse (dat6$prtvtano == 6, 1, 0) # Centre Party (SP) 
dat6$party.no.h <- ifelse (dat6$prtvtano == 7, 1, 0) # Conservative Party (H)
dat6$party.no.frp <- ifelse (dat6$prtvtano == 8, 1, 0) # Progress Party (FRP)

# The Netherlands, 2012 (28.08.12-30.03.13). Bad period: Rutte I (with PVV) decommissioned from April 2012. New gov (VVD and PvdA) from November 2012.
dat6$party.maingov.nl <- ifelse (dat6$prtvtenl == 1, 1, 0) # main party in government: VVD
dat6$party.gov.nl <- ifelse ((dat6$prtvtenl == 1 | dat6$prtvtenl == 2) , 1, 0) # all parties in government: VVD and PvdA
dat6$party.nl.vvd <- ifelse (dat6$prtvtenl == 1, 1, 0) # VVD
dat6$party.nl.pvda <- ifelse (dat6$prtvtenl == 2, 1, 0) # PvdA
dat6$party.nl.pvv <- ifelse (dat6$prtvtenl == 3, 1, 0) # PVV
dat6$party.nl.cda <- ifelse (dat6$prtvtenl == 4, 1, 0) # CDA
dat6$party.nl.sp <- ifelse (dat6$prtvtenl == 5, 1, 0) # SP
dat6$party.nl.d66 <- ifelse (dat6$prtvtenl == 6, 1, 0) # D66
dat6$party.nl.cu <- ifelse (dat6$prtvtenl == 8, 1, 0) # CU

# Ireland, 2012 (10.12-09.02.13)
dat6$party.maingov.ie <- ifelse (dat6$prtvtaie == 2, 1, 0) # main party in government: FG
dat6$party.gov.ie <- ifelse ((dat6$prtvtaie == 2 | dat6$prtvtaie == 5) , 1, 0) # all parties in government: FG and Labour
dat6$party.ie.fg <- ifelse (dat6$prtvtaie == 2, 1, 0) # Fine Gael
dat6$party.ie.ff <- ifelse (dat6$prtvtaie == 1, 1, 0) # Fianna Fáil
dat6$party.ie.lab <- ifelse (dat6$prtvtaie == 5, 1, 0) # Labour

# Cyprus, 2012 
dat6$party.maingov.cy <- ifelse (dat6$prtvtacy == 1, 1, 0) # main party in government: AKEL
dat6$party.gov.cy <- ifelse ((dat6$prtvtacy == 1) , 1, 0) # all parties in government: AKEL minority
dat6$party.cy.akel <- ifelse (dat6$prtvtacy == 1, 1, 0) # AKEL
dat6$party.cy.diko <- ifelse (dat6$prtvtacy == 2, 1, 0) # DIKO
dat6$party.cy.disy <- ifelse (dat6$prtvtacy == 3, 1, 0) # DISY

# Finland, 2012 
dat6$party.maingov.fi <- ifelse (dat6$prtvtcfi == 1, 1, 0) # main party in government: National Coalition Party
dat6$party.gov.fi <- ifelse ((dat6$prtvtcfi == 1 | dat6$prtvtcfi == 13 | dat6$prtvtcfi == 14 | dat6$prtvtcfi == 12 |dat6$prtvtcfi == 2) , 1, 0) # all parties in government: AKEL minority
dat6$party.fi.ncp <- ifelse (dat6$prtvtcfi == 1, 1, 0) # NCP
dat6$party.fi.sdp <- ifelse (dat6$prtvtcfi == 13, 1, 0) # SDP
dat6$party.fi.la <- ifelse (dat6$prtvtcfi == 14, 1, 0) # Left Alliance
dat6$party.fi.gl <- ifelse (dat6$prtvtcfi == 12, 1, 0) # Green League
dat6$party.fi.spp <- ifelse (dat6$prtvtcfi == 2, 1, 0) # Swedish pp
dat6$party.fi.cp <- ifelse (dat6$prtvtcfi == 3, 1, 0) # Center party

### Recode parties / attachment ------------------------------------------------
dat6$attach.party.gov.fi <- ifelse ((dat6$prtclcfi == 1 | dat6$prtclcfi == 13 | dat6$prtclcfi == 14 | dat6$prtclcfi == 12 |dat6$prtclcfi == 2) , 1, 0) # all parties in government: AKEL minority
dat6$attach.party.maingov.fi <- ifelse (dat6$prtclcfi == 1, 1, 0) # main party in government: National Coalition Party

dat6$attach.party.gov.cy <- ifelse ((dat6$prtclacy == 1) , 1, 0) # all parties in government: AKEL minority
dat6$attach.party.maingov.cy <- ifelse (dat6$prtclacy == 1, 1, 0) # main party in government: AKEL

dat6$attach.party.gov.ie <- ifelse ((dat6$prtclcie == 2 | dat6$prtclcie == 5) , 1, 0) # all parties in government: FG and Labour
dat6$attach.party.maingov.ie <- ifelse (dat6$prtclcie == 2, 1, 0) # main party in government: FG

dat6$attach.party.gov.no <- ifelse ((dat6$prtclano == 3 | dat6$prtclano == 2 | dat6$prtclano == 6 ) , 1, 0) # all parties in government: Labour Party (A), Socialist Left Party (SV), Centre Party (SP)
dat6$attach.party.maingov.no <- ifelse (dat6$prtclano == 3, 1, 0) # main party in government: Labour Party (A)

dat6$attach.party.gov.it <- ifelse ((dat6$prtclbit == 1 | dat6$prtclbit == 8 | dat6$prtclbit == 5 | dat6$prtclbit == 6 | dat6$prtclbit == 11 ) , 1, 0) # all parties in government: Partido Democratico (PD), Popolo delle Libertà (PdL), Scelta Civica (con Monti), Union of the Centre (UDC), Radicali Italiani (Amnistia giustizia e libertà)
dat6$attach.party.maingov.it <- ifelse (dat6$prtclbit == 1, 1, 0) # main party in government: Partido Democratico (PD)

dat6$attach.party.gov.bg <- ifelse (dat6$prtclcbg == 1, 1, 0) # all parties in government: GERB
dat6$attach.party.maingov.bg <- ifelse (dat6$prtclcbg == 1, 1, 0) # main party in government: GERB

dat6$attach.party.gov.si <- ifelse ((dat6$prtcldsi == 4 | dat6$prtcldsi == 3 | dat6$prtcldsi == 8 | dat6$prtcldsi == 5), 1, 0) # main party in government: Slovenian Democratic Party - SDS, New Slovenia - Christian Peoples Party - NSi, Civic List - DL, Slovene Peoples Party - SLS
dat6$attach.party.maingov.si <- ifelse ((dat6$prtcldsi == 4), 1, 0) # main party in government: Slovenian Democratic Party - SDS

dat6$attach.party.gov.cz <- ifelse ((dat6$prtclccz == 5 | dat6$prtclccz == 3 | dat6$prtclccz == 4), 1, 0) # all parties in government: ODS, TOP-09 and VV
dat6$attach.party.maingov.cz <- ifelse ((dat6$prtclccz == 5), 1, 0) # main party in government: ODS

dat6$attach.party.gov.pl <- ifelse ((dat6$prtclepl == 2 | dat6$prtclepl == 5), 1, 0) # all parties in government: Civil Platform and Polish Peasants Party
dat6$attach.party.maingov.pl <- ifelse ((dat6$prtclepl == 2), 1, 0) # main party in government: Civic Platform

dat6$attach.party.gov.uk <- ifelse ((dat6$prtclgb == 1 | dat6$prtclgb == 3), 1, 0) # all parties in government: Conservatice and LibDem
dat6$attach.party.maingov.uk <- ifelse ((dat6$prtclgb == 1), 1, 0) # main party in government: Conservative

dat6$attach.party.gov.fr <- ifelse ((dat6$prtcldfr == 9 | dat6$prtcldfr == 12 | dat6$prtcldfr==7), 1, 0) # all parties in government: Socialist, EELV, Radical Party of the Left
dat6$attach.party.maingov.fr <- ifelse ((dat6$prtcldfr == 9), 1, 0) # main party in government: Socialist

dat6$attach.party.gov.hu <- ifelse ((dat6$prtcldhu == 3 ), 1, 0) # all parties in government: FIDESZ
dat6$attach.party.maingov.hu <- ifelse ((dat6$prtcldhu == 3), 1, 0) # main party in government: FIDESZ

dat6$attach.party.gov.be <- ifelse ((dat6$prtclcbe == 13 | dat6$prtclcbe == 5  | dat6$prtclcbe == 2  | dat6$prtclcbe == 9  | dat6$prtclcbe == 8 | dat6$prtclcbe == 12), 1, 0) # all parties in government:  Sp.A/PS and VLD/MR and CD&V/CDH
dat6$attach.party.maingov.be <- ifelse ((dat6$prtclcbe == 13 | dat6$prtclcbe == 5 ), 1, 0) # main party in government: Sp.A/PS

dat6$attach.party.gov.ee <- ifelse ((dat6$prtcldee == 3 | dat6$prtcldee == 1), 1 ,0) # all parties in government:  Estonian reform party and ISAMAA
dat6$attach.party.maingov.ee <- ifelse ((dat6$prtcldee == 3  ), 1, 0) # main party in government: Estonian reform party

dat6$attach.party.gov.de <- ifelse ((dat6$prtcldde == 2 | dat6$prtcldde == 4), 1 ,0) # all parties in government:  CDU/FDP
dat6$attach.party.maingov.de <- ifelse ((dat6$prtcldde == 2  ), 1, 0) # main party in government: CDU

dat6$attach.party.gov.al <- ifelse ((dat6$prtclal == 2 | dat6$prtclal == 3), 1 ,0) # all parties in government:  PD and Socialist Movement for Integration
dat6$attach.party.maingov.al <- ifelse ((dat6$prtclal == 2  ), 1, 0) # main party in government: PD

dat6$attach.party.gov.sp <- ifelse ((dat6$prtclces == 1 | dat6$prtclces == 1), 1 ,0) # all parties in government:  PP
dat6$attach.party.maingov.sp <- ifelse ((dat6$prtclces == 1  ), 1, 0) # main party in government: PP

dat6$attach.party.gov.nl <- ifelse ((dat6$prtcldnl == 1 | dat6$prtcldnl == 2) , 1, 0) # all parties in government: VVD and PvdA
dat6$attach.party.maingov.nl <- ifelse (dat6$prtcldnl == 1, 1, 0) # main party in government: VVD

dat6$attach.party.gov.pt <- ifelse ((dat6$prtclcpt == 10 | dat6$prtclcpt == 2), 1 ,0) # all parties in government:  PSD and CDS/PP
dat6$attach.party.maingov.pt <- ifelse ((dat6$prtclcpt == 10  ), 1, 0) # main party in government: PSD

dat6$attach.party.gov.dk <- ifelse ((dat6$prtclcdk == 1 | dat6$prtclcdk == 2 | dat6$prtclcdk == 4), 1 ,0) # all parties in government:  SocDem, SocLib, and PopSoc
dat6$attach.party.maingov.dk <- ifelse ((dat6$prtclcdk == 1  ), 1, 0) # main party in government: SocDem

dat6$attach.party.gov.se <- ifelse ((dat6$prtclbse == 5 | dat6$prtclbse == 1 | dat6$prtclbse == 2 | dat6$prtclbse == 3), 1 ,0) # all parties in government:  Moderate Party, Centre Party, Liberal People's Party and the Christian Democrats.
dat6$attach.party.maingov.se <- ifelse ((dat6$prtclbse == 5  ), 1, 0) # main party in government: Modearates

dat6$attach.party.gov.sk <- ifelse ((dat6$prtclcsk == 3 ), 1 ,0) # all parties in government: SMER
dat6$attach.party.maingov.sk <- ifelse ((dat6$prtclcsk== 3  ), 1, 0) # main party in government: SMER

dat6$attach.party.gov.ua <- ifelse ((dat6$prtcldua == 5 ), 1 ,0) # all parties in government: President Party of regions
dat6$attach.party.maingov.ua <- ifelse ((dat6$prtcldua== 5  ), 1, 0) # main party in government: President Party of regions

dat6$attach.party.gov.ru <- ifelse ((dat6$prtclcru == 1 ), 1 ,0) # all parties in government: United Russia
dat6$attach.party.maingov.ru <- ifelse ((dat6$prtclcru== 1  ), 1, 0) # main party in government: United Russia

dat6$attach.party.gov.is <- ifelse ((dat6$prtclais  == 1 | dat6$prtclais  == 4 ), 1 ,0) # all parties in government: SDA LGM
dat6$attach.party.maingov.is <- ifelse ((dat6$prtclais == 1  ), 1, 0) # main party in government: SDA

dat6$attach.party.gov.ks <- ifelse ((dat6$prtclxk == 2), 1 ,0) # all parties in government: PDK
dat6$attach.party.maingov.ks <- ifelse ((dat6$prtclxk == 2  ), 1, 0) # main party in government: PDK

### Add country-level variables: election dates, polarization and proportionality --------------------------------
dat6$country <- dat6$cntry

# List of countries
countries <- c( 'PL' ,'BG' , 'CZ' , 'IE' , 'IT' , 'NL' , 'NO' , 'SI' , 'CY' , 'FI', 'DE', 'FR', 'GB', 'HU', 'BE' , 'EE' ,
               'PT' , 'ES' , 'AL' , 'DK' , 'SE' , 'SK' , 'IS' , 'XK', 'UA' , 'RU') 

# Load election data from ParlGov
cab <- read.csv('./data - external/view_election.csv')
cab$election_date <- ifelse (cab$election_date=='2009-09-27' & cab$country_name=='Portugal', NA, cab$election_date)
cab <- cab %>%
  mutate (country = countrycode(country_name_short, origin='iso3c', destination = 'iso2c')) %>%
  filter (election_type=='parliament', election_date < '2013-04-01', election_date > '2009-04-01',
          election_date != '2010-06-12', election_date != '2012-09-12', 
          country %in% countries) %>%
  mutate (election_date = as.Date(election_date)) %>%
  dplyr::select (country, election_date) %>% 
  group_by(country) %>% 
  slice(1) %>%  
  ungroup()

dat6 <- left_join (dat6, cab, by='country')

dat6 <- dat6  %>%
  mutate (election_date = case_when(country=='AL' ~ '2009-06-28',
                                     country=='UA' ~ '2012-10-28',
                                     country=='RU' ~ '2012-03-04',
                                     country=='XK' ~ '2010-12-12',
                                     TRUE ~ as.character(election_date))) %>%
  mutate (election_date = as.Date(election_date))


dat6$time <- as.numeric((dat6$date - dat6$election_date))/365

# Load polarization data from Digital Society Project
polar<-read.csv('./data - external/DigitalSocietyProject-v2-R/DSP-Dataset-v2.csv')
polar <- polar %>%
  dplyr::filter (year == 2012) %>%
  dplyr::mutate (polarization = v2smpolsoc,
                 country = countrycode(country_name, origin = 'country.name', destination = 'iso2c')) %>%
  dplyr::select ('country', 'polarization')

polar$country = ifelse(polar$country=='Kosovo', 'XK', polar$country)

dat6 <-left_join(dat6, polar, by='country')

# Code new/old democracies
dat6$newdem<-ifelse ((dat6$country == 'PL' |  dat6$country == 'BG' | dat6$country == 'CZ' | 
                          dat6$country == 'SI' | dat6$country == 'CY' | dat6$country == 'HU'| dat6$country == 'EE' |
                          dat6$country == 'AL' |  dat6$country == 'SK' |  dat6$country == 'XK' |
                          dat6$country == 'UA' | dat6$country == 'RU'), 'new', 'old')

dat6$newdem3<-ifelse ((dat6$country == 'PL' |  dat6$country == 'BG' | dat6$country == 'CZ' | 
                           dat6$country == 'SI' | dat6$country == 'CY' | dat6$country == 'HU'| dat6$country == 'EE' |
                           dat6$country == 'SK' ), 'new', 
                        ifelse ((dat6$country == 'AL' |  dat6$country == 'XK' | dat6$country == 'UA' | dat6$country == 'RU'), 'non', 'old'))


# Load economic measures from World Bank
gdp_data <- wb_data(indicator = c("NY.GDP.PCAP.KD.ZG", 'SL.UEM.TOTL.ZS'), start_date = 2013, end_date = 2013) %>% 
  filter (iso2c %in% countries) %>% 
  mutate (gdp.pc.change = NY.GDP.PCAP.KD.ZG, unempl = SL.UEM.TOTL.ZS, country = iso2c) %>% 
  dplyr::select (country, gdp.pc.change, unempl)

dat6 <- left_join (dat6, gdp_data, by='country')

### Pool and subset -------------------------------------------------------------------------
dat6.s<-dat6 %>% filter (country %in% countries)  %>% droplevels()

dat6.s$party.gov.all <- ifelse ((dat6.s$party.gov.pl  %in% 1 | dat6.s$party.gov.bg  %in% 1 | dat6.s$party.gov.cz  %in% 1 | dat6.s$party.gov.si  %in% 1 |
                                   dat6.s$party.gov.ie  %in% 1 | dat6.s$party.gov.it  %in% 1 | dat6.s$party.gov.nl  %in% 1 | dat6.s$party.gov.no  %in% 1 |
                                   dat6.s$party.gov.cy  %in% 1 | dat6.s$party.gov.fi  %in% 1 |
                                   dat6.s$party.gov.de  %in% 1 | dat6.s$party.gov.de2  %in% 1 | dat6.s$party.gov.fr  %in% 1 | dat6.s$party.gov.uk  %in% 1 | 
                                   dat6.s$party.gov.be  %in% 1 |dat6.s$party.gov.hu  %in% 1 | dat6.s$party.gov.ee  %in% 1 |
                                   dat6.s$party.gov.pt  %in% 1 | dat6.s$party.gov.al  %in% 1 | dat6.s$party.gov.sp  %in% 1 | dat6.s$party.gov.dk  %in% 1 |
                                   dat6.s$party.gov.se  %in% 1 | dat6.s$party.gov.sk  %in% 1 | dat6.s$party.gov.is  %in% 1 | dat6.s$party.gov.ks  %in% 1 |
                                   dat6.s$party.gov.ua  %in% 1 | dat6.s$party.gov.ru  %in% 1), 1, 0)

dat6.s$attach.party.gov.all <- ifelse ((dat6.s$attach.party.gov.pl  %in% 1 | dat6.s$attach.party.gov.bg  %in% 1 | dat6.s$attach.party.gov.cz  %in% 1 | dat6.s$attach.party.gov.si  %in% 1 |
                                          dat6.s$attach.party.gov.ie  %in% 1 | dat6.s$attach.party.gov.it  %in% 1 | dat6.s$attach.party.gov.nl  %in% 1 | dat6.s$attach.party.gov.no  %in% 1 |
                                          dat6.s$attach.party.gov.cy  %in% 1 | dat6.s$attach.party.gov.fi  %in% 1 |
                                          dat6.s$attach.party.gov.de  %in% 1 | dat6.s$attach.party.gov.fr  %in% 1 | dat6.s$attach.party.gov.uk  %in% 1 | 
                                          dat6.s$attach.party.gov.be  %in% 1 |dat6.s$attach.party.gov.hu  %in% 1 | dat6.s$attach.party.gov.ee  %in% 1 |
                                          dat6.s$attach.party.gov.pt  %in% 1 | dat6.s$attach.party.gov.al  %in% 1 | dat6.s$attach.party.gov.sp  %in% 1 | dat6.s$attach.party.gov.dk  %in% 1 |
                                          dat6.s$attach.party.gov.se  %in% 1 | dat6.s$attach.party.gov.sk  %in% 1 | dat6.s$attach.party.gov.is  %in% 1 | dat6.s$attach.party.gov.ks  %in% 1 |
                                          dat6.s$attach.party.gov.ua  %in% 1 | dat6.s$attach.party.gov.ru  %in% 1), 1, 0)

dat6.s <- dat6.s %>%
  mutate (status = case_when (party.gov.all == 1 & attach.party.gov.all == 1 ~ 'vote.closeto.winner',
                              party.gov.all == 1 ~ 'vote.winner',
                              party.gov.all == 0 & party.close.any==1 & voted ==1 ~ 'vote.closeto.loser',
                              party.gov.all == 0 & voted ==1 ~ 'vote.loser',
                              voted == 0 ~ 'no vote'
  ))
dat6.s$status= factor(dat6.s$status, levels =c('vote.closeto.loser','vote.loser','no vote','vote.winner','vote.closeto.winner'))

dat6.s <- dat6.s %>%
  mutate (status2 = case_when(status == 'vote.closeto.winner' | status == 'vote.winner' ~ 'winner',
                              status == 'vote.closeto.loser' | status == 'vote.loser' ~ 'loser',
                              TRUE ~ 'no vote'))

dat6.s <- dat6.s %>%
  mutate (status3 = case_when(status == 'vote.closeto.winner' ~ 'winner',
                              status == 'vote.closeto.loser'  ~ 'loser',
                              TRUE ~ 'no attachment'))

dat6.s <- dat6.s[, 625:dim(dat6.s)[2]]

### Save this subset file -------------------------------------------------------------------------
write_csv(dat6.s, './data - output/replication_dat6_subset_new.csv')
save (dat6.s, file = './data - output/replication_dat6_subset_new.RData')
### The end of Part 1 of 2 ------------------------------------------------------