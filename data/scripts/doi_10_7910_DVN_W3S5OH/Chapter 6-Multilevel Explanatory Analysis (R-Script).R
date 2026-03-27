### Chapter 6
rm(list=ls(all=TRUE))

### TASK 1: Data Preparation (NATIONAL)
### TASK 2: The Estimation of the Multilevel Models with the ESEB (NATIONAL)
### TASK 3: The Estimation of Interaction Effects of Controlling Variables on Equal Shares and Equal Changes (Post-Hoc Analysis-A)
### TASK 4: Data Preparation (LOCAL)
### TASK 5: The Estimation of the Multilevel Models with the ESEB (LOCAL: Post-Hoc Analysis-B)

### TASK 1: Data Preparation (NATIONAL)
rm(list=ls(all=TRUE))

ESEB02<- subset(ESEB2002, select = c(    uf, #Electoral District
                                         p19, #Satisfaction to the functioning of democracy in Brazil
                                         p20, #Who governs the country makes a difference
                                         p21, #Your vote influences whay will happen in the country
                                         p22, #The best form of government
                                         p31, #Is there a political party that represents you?
                                         p32, #Which party represents best the way you think?
                                         p35, #Is there a political party that you like?
                                         p36a, #Which party do you like (the first mentioned)?
                                         p108b, #The government must say everything that companies have to do, like how many bathrooms they have to have.
                                         p04, #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                                         p159, #Up to which grade did you study?
                                         p176, #Adding the income of all the people who live in your house, what is the family income?
                                         p50v1, #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
                                         #p50v2, #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
                                         p13, #Who did you vote for FEDERAL DEPUTY?
                                         p14  #Who did you vote for STATE DEPUTY?
))

ESEB14 <- subset(ESEB2014, select = c(ESTADO, #Electoral District
                                      Q15, #Satisfaction to the functioning of democracy in Brazil
                                      Q7, #Who governs the country makes a difference
                                      Q8, #Your vote influences whay will happen in the country
                                      PC6, #The best form of government
                                      Q16, #Is there a political party that represents you?
                                      D9B, #Which party represents best the way you think?
                                      Q16A, #Is there a political party that you like?
                                      Q16B, #Which party do you like (the first mentioned)?
                                      PC12B, #The government must say everything that companies have to do, like how many bathrooms they have to have.
                                      Q17, #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                                      D3_ESCOLA, #Up to which grade did you study?
                                      D20A_FXRENDFAM, #Adding the income of all the people who live in your house, what is the family income?
                                      Q12, #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
                                      Q5CDA, #Who did you vote for FEDERAL DEPUTY?
                                      Q5ALA  #Who did you vote for STATE DEPUTY?
                                      
))

### Creating the variable Party, based on voting intentions in the 2002 legislative elections
### and also the district-level lists of candidates for the election.
TSE2002 <- rbind(consulta_cand_2002_AC, consulta_cand_2002_AL, consulta_cand_2002_AM, 
                 consulta_cand_2002_AP, consulta_cand_2002_BA, consulta_cand_2002_CE,
                 consulta_cand_2002_DF, consulta_cand_2002_ES, consulta_cand_2002_GO,
                 consulta_cand_2002_MA, consulta_cand_2002_MG, consulta_cand_2002_MS,
                 consulta_cand_2002_MT, consulta_cand_2002_PA, consulta_cand_2002_PB,
                 consulta_cand_2002_PE, consulta_cand_2002_PI, consulta_cand_2002_PR,
                 consulta_cand_2002_RJ, consulta_cand_2002_RN, consulta_cand_2002_RO,
                 consulta_cand_2002_RR, consulta_cand_2002_RS, consulta_cand_2002_SC,
                 consulta_cand_2002_SE, consulta_cand_2002_SP, consulta_cand_2002_TO) #The district-level lists of candidates for the 2002 lower-house election.
TSEFD2002 <- TSE2002[ which(TSE2002$V10 == "DEPUTADO FEDERAL"),]

library(openxlsx)
write.xlsx(TSEFD2002, file = "Candidacy2002.xlsx",
           sheetName = "Candidacy2002", append = FALSE)

## Assigning party labels, based on voting intentions at the 2002 lower-house election.
# Acre
BESAC <- ESEB02[ which(ESEB02$uf == "12"),]
TSEAC <- TSEFD2002[ which(TSEFD2002$V8 == "ACRE"),]

BESAC$Party <- NA
BESAC$Party[ BESAC$p13 == "ANTÔNIO JOSÉ                                                                                                                                                                                                                                                   "] <- "PSB"
BESAC$Party[ BESAC$p13 == "CARLOS BASA                                                                                                                                                                                                                                                    "] <- "PMN"
BESAC$Party[ BESAC$p13 == "CARLOS DO BASA                                                                                                                                                                                                                                                 "] <- "PMN"
BESAC$Party[ BESAC$p13 == "CARLOS DO BOSA                                                                                                                                                                                                                                                 "] <- "PMN"
BESAC$Party[ BESAC$p13 == "CARLOS DO BOSA                                                                                                                                                                                                                                                 "] <- "PMN"
BESAC$Party[ BESAC$p13 == "CHICÃO BRÍGIDO                                                                                                                                                                                                                                                 "] <- "PMDB"
BESAC$Party[ BESAC$p13 == "JOÃO DO CORREIO                                                                                                                                                                                                                                                "] <- "PMDB"
BESAC$Party[ BESAC$p13 == "JOÃO CORREIA                                                                                                                                                                                                                                                   "] <- "PMDB"
BESAC$Party[ BESAC$p13 == "JOÃO DOS CORREIOS                                                                                                                                                                                                                                              "] <- "PMDB"
BESAC$Party[ BESAC$p13 == "JOÃO DOS CORREIOS                                                                                                                                                                                                                                              "] <- "PMDB"
BESAC$Party[ BESAC$p13 == "JOSÉ ANTONIO                                                                                                                                                                                                                                                   "] <- "PSB"
BESAC$Party[ BESAC$p13 == "JULINHO                                                                                                                                                                                                                                                        "] <- "PV"
BESAC$Party[ BESAC$p13 == "JULINHO                                                                                                                                                                                                                                                        "] <- "PV"
BESAC$Party[ BESAC$p13 == "MONTEIRO                                                                                                                                                                                                                                                       "] <- "PT"
BESAC$Party[ BESAC$p13 == "NARCISO MENDES                                                                                                                                                                                                                                                 "] <- "PP"
BESAC$Party[ BESAC$p13 == "NORMANDO SALES                                                                                                                                                                                                                                                 "] <- "PSDB"
BESAC$Party[ BESAC$p13 == "RONIVON SANTIAGO                                                                                                                                                                                                                                               "] <- "PP"
BESAC$Party[ BESAC$p13 == "NORMANDO SALES                                                                                                                                                                                                                                                 "] <- "PSDB"
BESAC$Party[ BESAC$p13 == "SAID FILHO                                                                                                                                                                                                                                                     "] <- "PPS"
BESAC$Party[ BESAC$p13 == "SORMANDO SALES                                                                                                                                                                                                                                                 "] <- "PSDB"

# Alagoas
BESAL <- ESEB02[ which(ESEB02$uf == "27"),]
TSEAL <- TSEFD2002[ which(TSEFD2002$V8 == "ALAGOAS"),]

BESAL$Party <- NA
BESAL$Party[ BESAL$p13 == "ARNON DE MELLO                                                                                                                                                                                                                                                 "] <- "PRTB"
BESAL$Party[ BESAL$p13 == "BENEDITO DE LIRA                                                                                                                                                                                                                                               "] <- "PTB"
BESAL$Party[ BESAL$p13 == "BENEDITO FR LIRA                                                                                                                                                                                                                                               "] <- "PTB"
BESAL$Party[ BESAL$p13 == "DR.ROGÉRIO                                                                                                                                                                                                                                                     "] <- "PFL"
BESAL$Party[ BESAL$p13 == "ELENILDO RIBEIRO                                                                                                                                                                                                                                               "] <- "PSDB"
BESAL$Party[ BESAL$p13 == "ELENILDO RIBEIRO                                                                                                                                                                                                                                               "] <- "PSDB"
BESAL$Party[ BESAL$p13 == "JURANDIR BOIA                                                                                                                                                                                                                                                  "] <- "PSB"
BESAL$Party[ BESAL$p13 == "LUIS DANTAS                                                                                                                                                                                                                                                    "] <- "PTB"
BESAL$Party[ BESAL$p13 == "MAURÍCIO QUINTELA                                                                                                                                                                                                                                              "] <- "PSB"
BESAL$Party[ BESAL$p13 == "OLAVO CALHEIROS                                                                                                                                                                                                                                                "] <- "PMDB"
BESAL$Party[ BESAL$p13 == "ROGERIO TEÓFILO                                                                                                                                                                                                                                                "] <- "PFL"
BESAL$Party[ BESAL$p13 == "ROGÉRIO TEÓFILO                                                                                                                                                                                                                                                "] <- "PFL"
BESAL$Party[ BESAL$p13 == "SEVERINO LEÃO                                                                                                                                                                                                                                                  "] <- "PL"
BESAL$Party[ BESAL$p13 == "ZÉ MUNIZ                                                                                                                                                                                                                                                       "] <- "PMDB"

# Amazonas
BESAM <- ESEB02[ which(ESEB02$uf == "13"),]
TSEAM <- TSEFD2002[ which(TSEFD2002$V8 == "AMAZONAS"),]

BESAM$Party <- NA
BESAM$Party[ BESAM$p13 == "CARLOS SOUZA                                                                                                                                                                                                                                                   "] <- "PL"
BESAM$Party[ BESAM$p13 == "DR. GOMES                                                                                                                                                                                                                                                      "] <- "PFL"
BESAM$Party[ BESAM$p13 == "FRANCISCO GARCIA                                                                                                                                                                                                                                               "] <- "PFL"
BESAM$Party[ BESAM$p13 == "LUIZ FERNANDO COSTA                                                                                                                                                                                                                                            "] <- "PST"
BESAM$Party[ BESAM$p13 == "VANESSA                                                                                                                                                                                                                                                        "] <- "PC do B"

# Amapá
BESAP <- ESEB02[ which(ESEB02$uf == "16"),]
TSEAP <- TSEFD2002[ which(TSEFD2002$V8 == "AMAPA"),]

BESAP$Party <- NA
BESAP$Party[ BESAP$p13 == "BENEDITO DIAS                                                                                                                                                                                                                                                  "] <- "PP"
BESAP$Party[ BESAP$p13 == "JANETE                                                                                                                                                                                                                                                         "] <- "PSB"
BESAP$Party[ BESAP$p13 == "NOGUEIRA                                                                                                                                                                                                                                                       "] <- "PT"
BESAP$Party[ BESAP$p13 == "SERGIO BARCELOS                                                                                                                                                                                                                                                "] <- "PFL"

# Bahia
BESBA <- ESEB02[ which(ESEB02$uf == "29"),]
TSEBA <- TSEFD2002[ which(TSEFD2002$V8 == "BAHIA"),]

BESBA$Party <- NA
BESBA$Party[ BESBA$p13 == "1415-JAIRO CARNEIRO                                                                                                                                                                                                                                            "] <- "PFL"
BESBA$Party[ BESBA$p13 == "A.C.M. NETO                                                                                                                                                                                                                                                    "] <- "PFL"
BESBA$Party[ BESBA$p13 == "ALELUIA                                                                                                                                                                                                                                                        "] <- "PFL"
BESBA$Party[ BESBA$p13 == "BENITO GAMA                                                                                                                                                                                                                                                    "] <- "PMDB"
BESBA$Party[ BESBA$p13 == "CAJADO                                                                                                                                                                                                                                                         "] <- "PFL"
BESBA$Party[ BESBA$p13 == "COLBERT MARTINS                                                                                                                                                                                                                                                "] <- "PPS"
BESBA$Party[ BESBA$p13 == "FELIX MENDONÇA                                                                                                                                                                                                                                                 "] <- "PTB"
BESBA$Party[ BESBA$p13 == "FÉLIX MENDONÇA                                                                                                                                                                                                                                                 "] <- "PTB"
BESBA$Party[ BESBA$p13 == "FERNANDO DE FABINHO                                                                                                                                                                                                                                            "] <- "PFL"
BESBA$Party[ BESBA$p13 == "GEDEL                                                                                                                                                                                                                                                          "] <- "PMDB"
BESBA$Party[ BESBA$p13 == "GEDEL VIEIRA                                                                                                                                                                                                                                                   "] <- "PMDB"
BESBA$Party[ BESBA$p13 == "GEDEL VIEIRA NÃO LEMBRA                                                                                                                                                                                                                                        "] <- "PMDB"
BESBA$Party[ BESBA$p13 == "GERSON GABRIELLLI                                                                                                                                                                                                                                              "] <- "PFL"
BESBA$Party[ BESBA$p13 == "GILBERTO CERQUEIRA                                                                                                                                                                                                                                             "] <- "PSB"
BESBA$Party[ BESBA$p13 == "JOÃO ALMEIDA                                                                                                                                                                                                                                                   "] <- "PSDB"
BESBA$Party[ BESBA$p13 == "JOSE ROCHA                                                                                                                                                                                                                                                     "] <- "PFL"
BESBA$Party[ BESBA$p13 == "JOSÉ ROCHA                                                                                                                                                                                                                                                     "] <- "PFL"
BESBA$Party[ BESBA$p13 == "LEÃO                                                                                                                                                                                                                                                           "] <- "PP"
BESBA$Party[ BESBA$p13 == "MARCELO GUIMARÃES FILHO                                                                                                                                                                                                                                        "] <- "PFL"
BESBA$Party[ BESBA$p13 == "MARCELO SANTANA                                                                                                                                                                                                                                                "] <- "PDT"
BESBA$Party[ BESBA$p13 == "PAULO MAGALHÃES                                                                                                                                                                                                                                                "] <- "PFL"
BESBA$Party[ BESBA$p13 == "PINHEIRO                                                                                                                                                                                                                                                       "] <- "PT"
BESBA$Party[ BESBA$p13 == "ROBERTO MUNIZ                                                                                                                                                                                                                                                  "] <- "PFL"
BESBA$Party[ BESBA$p13 == "SANTANA                                                                                                                                                                                                                                                        "] <- "PDT"
BESBA$Party[ BESBA$p13 == "SÉRGIO CARNEIRO                                                                                                                                                                                                                                                "] <- "PT"
BESBA$Party[ BESBA$p13 == "VALDIR                                                                                                                                                                                                                                                         "] <- "PT do B"
BESBA$Party[ BESBA$p13 == "ZELINDA NOVAEWS                                                                                                                                                                                                                                                "] <- "PFL"
BESBA$Party[ BESBA$p13 == "ZELINDA NOVAIS                                                                                                                                                                                                                                                 "] <- "PFL"

# Ceará
BESCE <- ESEB02[ which(ESEB02$uf == "23"),]
TSECE <- TSEFD2002[ which(TSEFD2002$V8 == "CEARA"),]

BESCE$Party <- NA
BESCE$Party[ BESCE$p13 == "ALMEIDA DE JESUS                                                                                                                                                                                                                                               "] <- "PL"
BESCE$Party[ BESCE$p13 == "ANÍBAL GOMES                                                                                                                                                                                                                                                   "] <- "PMDB"
BESCE$Party[ BESCE$p13 == "BISMARCK                                                                                                                                                                                                                                                       "] <- "PSDB"
BESCE$Party[ BESCE$p13 == "BISMARCK MAIA                                                                                                                                                                                                                                                  "] <- "PSDB"
BESCE$Party[ BESCE$p13 == "BISMARK                                                                                                                                                                                                                                                        "] <- "PSDB"
BESCE$Party[ BESCE$p13 == "BISMARK MAIA                                                                                                                                                                                                                                                   "] <- "PSDB"
BESCE$Party[ BESCE$p13 == "CÂNDIDA FIGUEIREDO                                                                                                                                                                                                                                             "] <- "PDT"
BESCE$Party[ BESCE$p13 == "EUNICIO                                                                                                                                                                                                                                                        "] <- "PP"
BESCE$Party[ BESCE$p13 == "EUNÍCIO OLIVEIRA                                                                                                                                                                                                                                               "] <- "PP"
BESCE$Party[ BESCE$p13 == "GONZAGA MOTA                                                                                                                                                                                                                                                   "] <- "PMDB"
BESCE$Party[ BESCE$p13 == "INÁCIO ARRUDA                                                                                                                                                                                                                                                  "] <- "PC do B"
BESCE$Party[ BESCE$p13 == "INÊS ARRUDA                                                                                                                                                                                                                                                    "] <- "PC do B"
BESCE$Party[ BESCE$p13 == "JOÃO ALFREDO                                                                                                                                                                                                                                                   "] <- "PT"
BESCE$Party[ BESCE$p13 == "JOSÉ GERALDO                                                                                                                                                                                                                                                   "] <- "PMDB"
BESCE$Party[ BESCE$p13 == "LEO ALCANTARA                                                                                                                                                                                                                                                  "] <- "PSDB"
BESCE$Party[ BESCE$p13 == "LÉO ALCÂNTARA                                                                                                                                                                                                                                                  "] <- "PSDB"
BESCE$Party[ BESCE$p13 == "LÉO ALCANTERA                                                                                                                                                                                                                                                  "] <- "PSDB"
BESCE$Party[ BESCE$p13 == "MORONI                                                                                                                                                                                                                                                         "] <- "PFL"
BESCE$Party[ BESCE$p13 == "MORONI TORGAN                                                                                                                                                                                                                                                  "] <- "PFL"
BESCE$Party[ BESCE$p13 == "PASTOR PEDRO RIBEIRO                                                                                                                                                                                                                                           "] <- "PL"
BESCE$Party[ BESCE$p13 == "PIMENTEL                                                                                                                                                                                                                                                       "] <- "PT"
BESCE$Party[ BESCE$p13 == "RAIMUNDO MATOS                                                                                                                                                                                                                                                 "] <- "PSDB"
BESCE$Party[ BESCE$p13 == "RINOCO LONA                                                                                                                                                                                                                                                    "] <- "PC do B"
BESCE$Party[ BESCE$p13 == "VICENTE ARRUDA                                                                                                                                                                                                                                                 "] <- "PSDB"

# Distrito Federal
BESDF <- ESEB02[ which(ESEB02$uf == "53"),]
TSEDF <- TSEFD2002[ which(TSEFD2002$V8 == "DISTRITO FEDERAL"),]

BESDF$Party <- NA
BESDF$Party[ BESDF$p13 == "AGNELO                                                                                                                                                                                                                                                         "] <- "PC do B"
BESDF$Party[ BESDF$p13 == "ARRUDA                                                                                                                                                                                                                                                         "] <- "PFL"
BESDF$Party[ BESDF$p13 == "CHICO VIGILANTE                                                                                                                                                                                                                                                "] <- "PSC"
BESDF$Party[ BESDF$p13 == "CINGMARINGA                                                                                                                                                                                                                                                    "] <- "PT"
BESDF$Party[ BESDF$p13 == "FELIPELLI                                                                                                                                                                                                                                                      "] <- "PMDB"
BESDF$Party[ BESDF$p13 == "FELLIPE ARRUDA                                                                                                                                                                                                                                                 "] <- "PFL"
BESDF$Party[ BESDF$p13 == "FRAGA                                                                                                                                                                                                                                                          "] <- "PMDB"
BESDF$Party[ BESDF$p13 == "MANINHA                                                                                                                                                                                                                                                        "] <- "PT"
BESDF$Party[ BESDF$p13 == "PEDRO CELSO                                                                                                                                                                                                                                                    "] <- "PT"
BESDF$Party[ BESDF$p13 == "RENALT                                                                                                                                                                                                                                                         "] <- "PPS"
BESDF$Party[ BESDF$p13 == "SIG MARINGA                                                                                                                                                                                                                                                    "] <- "PT"
BESDF$Party[ BESDF$p13 == "SIGMARINGA                                                                                                                                                                                                                                                     "] <- "PT"
BESDF$Party[ BESDF$p13 == "TATICO                                                                                                                                                                                                                                                         "] <- "PSD"

# Espírito Santo
BESES <- ESEB02[ which(ESEB02$uf == "32"),]
TSEES <- TSEFD2002[ which(TSEFD2002$V8 == "ESPIRITO SANTO"),]

BESES$Party <- NA
BESES$Party[ BESES$p13 == "ACIR                                                                                                                                                                                                                                                           "] <- "PFL"
BESES$Party[ BESES$p13 == "ACIR MENDONÇA                                                                                                                                                                                                                                                  "] <- "PFL"
BESES$Party[ BESES$p13 == "ADEMIR CARDOSO                                                                                                                                                                                                                                                 "] <- "PSDB"
BESES$Party[ BESES$p13 == "AMADEU BOROTO                                                                                                                                                                                                                                                  "] <- "PSC"
BESES$Party[ BESES$p13 == "CASA GRANDE                                                                                                                                                                                                                                                    "] <- "PSB"
BESES$Party[ BESES$p13 == "CHARLES - PMN                                                                                                                                                                                                                                                  "] <- "PMN"
BESES$Party[ BESES$p13 == "ARCI                                                                                                                                                                                                                                                           "] <- "PFL"
BESES$Party[ BESES$p13 == "ELIAS KIEFER                                                                                                                                                                                                                                                   "] <- "PDT"
BESES$Party[ BESES$p13 == "ELIAS KIFFER                                                                                                                                                                                                                                                   "] <- "PDT"
BESES$Party[ BESES$p13 == "IRINY LOPES                                                                                                                                                                                                                                                    "] <- "PT"
BESES$Party[ BESES$p13 == "JOSÉ CARLOS FONSECA JÚNIOR                                                                                                                                                                                                                                     "] <- "PFL"
BESES$Party[ BESES$p13 == "JOTA NETO                                                                                                                                                                                                                                                      "] <- "PGT"
BESES$Party[ BESES$p13 == "NILTON BAIANO                                                                                                                                                                                                                                                  "] <- "PP"
BESES$Party[ BESES$p13 == "RENATO CASAGRANDE                                                                                                                                                                                                                                              "] <- "PSB"
BESES$Party[ BESES$p13 == "ROSE DE FREITAS                                                                                                                                                                                                                                                "] <- "PSDB"
BESES$Party[ BESES$p13 == "TRINY LOPES                                                                                                                                                                                                                                                    "] <- "PT"

# Goiás
BESGO <- ESEB02[ which(ESEB02$uf == "52"),]
TSEGO <- TSEFD2002[ which(TSEFD2002$V8 == "GOIAS"),]

BESGO$Party <- NA
BESGO$Party[ BESGO$p13 == "ADEMAR SANTILO                                                                                                                                                                                                                                                 "] <- "PMDB"
BESGO$Party[ BESGO$p13 == "ALDO ARANTES                                                                                                                                                                                                                                                   "] <- "PC do B"
BESGO$Party[ BESGO$p13 == "BARBOSA NETO                                                                                                                                                                                                                                                   "] <- "PMDB"
BESGO$Party[ BESGO$p13 == "HENRIQUE MEIRELLES                                                                                                                                                                                                                                             "] <- "PSDB"
BESGO$Party[ BESGO$p13 == "JOSÉ LOPES                                                                                                                                                                                                                                                     "] <- "PRP"
BESGO$Party[ BESGO$p13 == "JOVAIR ARANTES                                                                                                                                                                                                                                                 "] <- "PSDB"
BESGO$Party[ BESGO$p13 == "MEIRELLES                                                                                                                                                                                                                                                      "] <- "PSDB"
BESGO$Party[ BESGO$p13 == "PEDRO CANEDO                                                                                                                                                                                                                                                   "] <- "PSDB"
BESGO$Party[ BESGO$p13 == "PEDRO CARNEIRO                                                                                                                                                                                                                                                 "] <- "PSDB"
BESGO$Party[ BESGO$p13 == "RAQUEL TEIXEIRA                                                                                                                                                                                                                                                "] <- "PSDB"
BESGO$Party[ BESGO$p13 == "RONALDO CAIADO                                                                                                                                                                                                                                                 "] <- "PFL"
BESGO$Party[ BESGO$p13 == "SANDRO MABEL                                                                                                                                                                                                                                                   "] <- "PFL"
BESGO$Party[ BESGO$p13 == "VILMAR ROCHA                                                                                                                                                                                                                                                   "] <- "PFL"

# Maranhão
BESMA <- ESEB02[ which(ESEB02$uf == "21"),]
TSEMA <- TSEFD2002[ which(TSEFD2002$V8 == "MARANHAO"),]

BESMA$Party <- NA
BESMA$Party[ BESMA$p13 == "CHICO COELHO                                                                                                                                                                                                                                                   "] <- "PFL"
BESMA$Party[ BESMA$p13 == "CIOVES FECURY                                                                                                                                                                                                                                                  "] <- "PFL"
BESMA$Party[ BESMA$p13 == "CLOVES FECURY                                                                                                                                                                                                                                                  "] <- "PFL"
BESMA$Party[ BESMA$p13 == "CLOVIS FECRY                                                                                                                                                                                                                                                   "] <- "PFL"
BESMA$Party[ BESMA$p13 == "CLÓVIS FECURY                                                                                                                                                                                                                                                  "] <- "PFL"
BESMA$Party[ BESMA$p13 == "COSTA FERREIRA                                                                                                                                                                                                                                                 "] <- "PFL"
BESMA$Party[ BESMA$p13 == "DR. JOSÉ CARLOS                                                                                                                                                                                                                                                "] <- "PSB"
BESMA$Party[ BESMA$p13 == "FROTA                                                                                                                                                                                                                                                          "] <- "PFL"
BESMA$Party[ BESMA$p13 == "JO!AO CASTELO                                                                                                                                                                                                                                                  "] <- "PSDB"
BESMA$Party[ BESMA$p13 == "JOÃO CASTELO                                                                                                                                                                                                                                                   "] <- "PSDB"
BESMA$Party[ BESMA$p13 == "JULIO CESAR                                                                                                                                                                                                                                                    "] <- "PSDB"
BESMA$Party[ BESMA$p13 == "LUIS ROCHA FILHO                                                                                                                                                                                                                                               "] <- "PSDB"
BESMA$Party[ BESMA$p13 == "LUIZ ROCHA FILHO                                                                                                                                                                                                                                               "] <- "PSDB"
BESMA$Party[ BESMA$p13 == "MAURO FECURY                                                                                                                                                                                                                                                   "] <- "PFL"
BESMA$Party[ BESMA$p13 == "NICE LOBÃO                                                                                                                                                                                                                                                     "] <- "PFL"
BESMA$Party[ BESMA$p13 == "PEDRO FERNANDES                                                                                                                                                                                                                                                "] <- "PFL"
BESMA$Party[ BESMA$p13 == "PEDRO NOVAIS                                                                                                                                                                                                                                                   "] <- "PMDB"
BESMA$Party[ BESMA$p13 == "TEREZINHA FERNANDES                                                                                                                                                                                                                                            "] <- "PT"
BESMA$Party[ BESMA$p13 == "ZAQUEU BARROS                                                                                                                                                                                                                                                  "] <- "PSB"

# Minas Gerais
BESMG <- ESEB02[ which(ESEB02$uf == "31"),]
TSEMG <- TSEFD2002[ which(TSEFD2002$V8 == "MINAS GERAIS"),]

BESMG$Party <- NA
BESMG$Party[ BESMG$p13 == "\"JAIMINHO MARTINS (DIVINÓPOLIS) FAMÍLIA STO ANTÔNIO MONTE                                                                                                                                                                                                      "] <- "PFL"
BESMG$Party[ BESMG$p13 == "1111                                                                                                                                                                                                                                                           "] <- "PP"
BESMG$Party[ BESMG$p13 == "13                                                                                                                                                                                                                                                             "] <- "PT"
BESMG$Party[ BESMG$p13 == "4560                                                                                                                                                                                                                                                           "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "ADMAR MOREIRA                                                                                                                                                                                                                                                  "] <- "PP"
BESMG$Party[ BESMG$p13 == "BONIFÁCIO ANDRADA                                                                                                                                                                                                                                              "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "CABO JÚLIO                                                                                                                                                                                                                                                     "] <- "PST"
BESMG$Party[ BESMG$p13 == "CARMINHA                                                                                                                                                                                                                                                       "] <- "PT"
BESMG$Party[ BESMG$p13 == "CARTRUS ANANIAS                                                                                                                                                                                                                                                "] <- "PT"
BESMG$Party[ BESMG$p13 == "CUSTÓDIO                                                                                                                                                                                                                                                       "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "CUSTÓDIO DE MATOS                                                                                                                                                                                                                                              "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "CUSTODIO MATTOS                                                                                                                                                                                                                                                "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "DANILO DE CASTRO                                                                                                                                                                                                                                               "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "EDIMAR MOREIRA                                                                                                                                                                                                                                                 "] <- "PP"
BESMG$Party[ BESMG$p13 == "EDMAR MOREIRA                                                                                                                                                                                                                                                  "] <- "PP"
BESMG$Party[ BESMG$p13 == "EDUARDO BARBOSA                                                                                                                                                                                                                                                "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "ELIAS MURAD                                                                                                                                                                                                                                                    "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "ELISEU RESENDE                                                                                                                                                                                                                                                 "] <- "PFL"
BESMG$Party[ BESMG$p13 == "ELISEU RESENDE 2525                                                                                                                                                                                                                                            "] <- "PFL"
BESMG$Party[ BESMG$p13 == "ELIZEU RESENDE                                                                                                                                                                                                                                                 "] <- "PFL"
BESMG$Party[ BESMG$p13 == "FERNANDO DINIS                                                                                                                                                                                                                                                 "] <- "PFL"
BESMG$Party[ BESMG$p13 == "FERNANDO DINIZ                                                                                                                                                                                                                                                 "] <- "PFL"
BESMG$Party[ BESMG$p13 == "HÉLIO COSTA                                                                                                                                                                                                                                                    "] <- "PL"
BESMG$Party[ BESMG$p13 == "HERCULANO                                                                                                                                                                                                                                                      "] <- "PP"
BESMG$Party[ BESMG$p13 == "IVO JOSÉ                                                                                                                                                                                                                                                       "] <- "PT"
BESMG$Party[ BESMG$p13 == "JAIMINHO                                                                                                                                                                                                                                                       "] <- "PFL"
BESMG$Party[ BESMG$p13 == "JAIMINHO (DIVINÓPOLIS)                                                                                                                                                                                                                                         "] <- "PFL"
BESMG$Party[ BESMG$p13 == "JOAO MAGNO                                                                                                                                                                                                                                                     "] <- "PT"
BESMG$Party[ BESMG$p13 == "JOÃO MAGNO                                                                                                                                                                                                                                                     "] <- "PT"
BESMG$Party[ BESMG$p13 == "JOÃO MAGNO (PT)                                                                                                                                                                                                                                                "] <- "PT"
BESMG$Party[ BESMG$p13 == "JOAO MAGNO DE MOURA                                                                                                                                                                                                                                            "] <- "PT"
BESMG$Party[ BESMG$p13 == "JOSÉ REIS                                                                                                                                                                                                                                                      "] <- "PSB"
BESMG$Party[ BESMG$p13 == "JOSÉ SANTANA                                                                                                                                                                                                                                                   "] <- "PFL"
BESMG$Party[ BESMG$p13 == "LINCOLN PORTELA                                                                                                                                                                                                                                                "] <- "PSL"
BESMG$Party[ BESMG$p13 == "LUIS CARLOS DE MIRANDA                                                                                                                                                                                                                                         "] <- "PTB"
BESMG$Party[ BESMG$p13 == "MARCELO CIQUEIRA                                                                                                                                                                                                                                               "] <- "PCO"
BESMG$Party[ BESMG$p13 == "MÁRCIO REINALDO                                                                                                                                                                                                                                                "] <- "PP"
BESMG$Party[ BESMG$p13 == "MARCIO RODRIGUES                                                                                                                                                                                                                                               "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "MÁRCIO RODRIGUES                                                                                                                                                                                                                                               "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "MARCIO RODRIGUES DA SILVEIRA                                                                                                                                                                                                                                   "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "MARCOS LIMA                                                                                                                                                                                                                                                    "] <- "PMDB"
BESMG$Party[ BESMG$p13 == "MARIA DO CARMO                                                                                                                                                                                                                                                 "] <- "PT"
BESMG$Party[ BESMG$p13 == "MARIA DO CARMO LARA                                                                                                                                                                                                                                            "] <- "PT"
BESMG$Party[ BESMG$p13 == "MÁRIO ASSAD JUNIOR                                                                                                                                                                                                                                             "] <- "PL"
BESMG$Party[ BESMG$p13 == "MÁRIO UNIOR                                                                                                                                                                                                                                                    "] <- "PL"
BESMG$Party[ BESMG$p13 == "MAURO LOPES                                                                                                                                                                                                                                                    "] <- "PMDB"
BESMG$Party[ BESMG$p13 == "MAURO LOPEZ                                                                                                                                                                                                                                                    "] <- "PMDB"
BESMG$Party[ BESMG$p13 == "MILITÃO                                                                                                                                                                                                                                                        "] <- "PTB"
BESMG$Party[ BESMG$p13 == "MOACIR DA CEMIG                                                                                                                                                                                                                                                "] <- "PST"
BESMG$Party[ BESMG$p13 == "NARCIO RODRIGUES                                                                                                                                                                                                                                               "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "NÁRCIO RODRIGUES                                                                                                                                                                                                                                               "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "NARCIO                                                                                                                                                                                                                                                         "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "PASTOR ANTONIO CARLOS                                                                                                                                                                                                                                          "] <- "PMDB"
BESMG$Party[ BESMG$p13 == "PASTOR JORGE (PL)                                                                                                                                                                                                                                              "] <- "PL"
BESMG$Party[ BESMG$p13 == "PATRUS ANANIAS                                                                                                                                                                                                                                                 "] <- "PT"
BESMG$Party[ BESMG$p13 == "PATRUS ANANIAS                                                                                                                                                                                                                                                 "] <- "PT"
BESMG$Party[ BESMG$p13 == "PAULO LOPES                                                                                                                                                                                                                                                    "] <- "PFL"
BESMG$Party[ BESMG$p13 == "PAULO LOPEZ                                                                                                                                                                                                                                                    "] <- "PFL"
BESMG$Party[ BESMG$p13 == "RAFAEL GUERRA                                                                                                                                                                                                                                                  "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "RAFAEL GUERRA (DIVINÓPOLIS)                                                                                                                                                                                                                                    "] <- "PSDB"
BESMG$Party[ BESMG$p13 == "REGILNALDO LOPES                                                                                                                                                                                                                                               "] <- "PT"
BESMG$Party[ BESMG$p13 == "REGINALDO                                                                                                                                                                                                                                                      "] <- "PT"
BESMG$Party[ BESMG$p13 == "REGINALDO LOPES                                                                                                                                                                                                                                                "] <- "PT"
BESMG$Party[ BESMG$p13 == "REGINALDO PT                                                                                                                                                                                                                                                   "] <- "PT"
BESMG$Party[ BESMG$p13 == "ROBERTO BRANT                                                                                                                                                                                                                                                  "] <- "PFL"
BESMG$Party[ BESMG$p13 == "RONALDO GONTIJO                                                                                                                                                                                                                                                "] <- "PPS"
BESMG$Party[ BESMG$p13 == "RONALDO VASCONCELOS                                                                                                                                                                                                                                            "] <- "PL"
BESMG$Party[ BESMG$p13 == "SARAIVA                                                                                                                                                                                                                                                        "] <- "PMDB"
BESMG$Party[ BESMG$p13 == "TONINHO TIMBIRA                                                                                                                                                                                                                                                "] <- "PTB"
BESMG$Party[ BESMG$p13 == "VIRGILINO DE GUIMARÃES                                                                                                                                                                                                                                         "] <- "PT"
BESMG$Party[ BESMG$p13 == "VIRGILIO GUIMARÃES                                                                                                                                                                                                                                             "] <- "PT"

# Mato Grosso do Sul
BESMS <- ESEB02[ which(ESEB02$uf == "50"),]
TSEMS <- TSEFD2002[ which(TSEFD2002$V8 == "MATO GROSSO DO SUL"),]

BESMS$Party <- NA
BESMS$Party[ BESMS$p13 == "BIFFI                                                                                                                                                                                                                                                          "] <- "PT"
BESMS$Party[ BESMS$p13 == "BRANCO                                                                                                                                                                                                                                                         "] <- "PMDB"
BESMS$Party[ BESMS$p13 == "MARIA HELENA                                                                                                                                                                                                                                                   "] <- "PPS"
BESMS$Party[ BESMS$p13 == "MOKA                                                                                                                                                                                                                                                           "] <- "PMDB"
BESMS$Party[ BESMS$p13 == "MURILO                                                                                                                                                                                                                                                         "] <- "PFL"
BESMS$Party[ BESMS$p13 == "PEDROSSIAN FILHO                                                                                                                                                                                                                                               "] <- "PP"
BESMS$Party[ BESMS$p13 == "VANDER                                                                                                                                                                                                                                                         "] <- "PT"
BESMS$Party[ BESMS$p13 == "VANDER LOUBET                                                                                                                                                                                                                                                  "] <- "PT"

# Mato Grosso
BESMT <- ESEB02[ which(ESEB02$uf == "51"),]
TSEMT <- TSEFD2002[ which(TSEFD2002$V8 == "MATO GROSSO"),]

BESMT$Party <- NA
BESMT$Party[ BESMT$p13 == "AGOSTINHO                                                                                                                                                                                                                                                      "] <- "PP"
BESMT$Party[ BESMT$p13 == "ÁGUA AGOSTINHO                                                                                                                                                                                                                                                 "] <- "PP"
BESMT$Party[ BESMT$p13 == "CARLOS ABICALIL                                                                                                                                                                                                                                                "] <- "PT"
BESMT$Party[ BESMT$p13 == "MURILO DOMINGOS                                                                                                                                                                                                                                                "] <- "PTB"
BESMT$Party[ BESMT$p13 == "WELINTON FAGUNDES                                                                                                                                                                                                                                              "] <- "PL"
BESMT$Party[ BESMT$p13 == "WELLINGTON FAGUNDES                                                                                                                                                                                                                                            "] <- "PL"
BESMT$Party[ BESMT$p13 == "WELLINTON FAGUNDES                                                                                                                                                                                                                                             "] <- "PL"
BESMT$Party[ BESMT$p13 == "WELLITON                                                                                                                                                                                                                                                       "] <- "PL"
BESMT$Party[ BESMT$p13 == "WELLITON FAGUNDES                                                                                                                                                                                                                                              "] <- "PL"

# Pará
BESPA <- ESEB02[ which(ESEB02$uf == "15"),]
TSEPA <- TSEFD2002[ which(TSEFD2002$V8 == "PARA"),]

BESPA$Party <- NA
BESPA$Party[ BESPA$p13 == "1456                                                                                                                                                                                                                                                           "] <- "PTB"
BESPA$Party[ BESPA$p13 == "BETINHO                                                                                                                                                                                                                                                        "] <- "PSB"
BESPA$Party[ BESPA$p13 == "CARLOS KAIATY                                                                                                                                                                                                                                                  "] <- "PTB"
BESPA$Party[ BESPA$p13 == "DR. SANTINO                                                                                                                                                                                                                                                    "] <- "PP"
BESPA$Party[ BESPA$p13 == "JADER BARBALHO                                                                                                                                                                                                                                                 "] <- "PMDB"
BESPA$Party[ BESPA$p13 == "PAULO ROCHA                                                                                                                                                                                                                                                    "] <- "PT"
BESPA$Party[ BESPA$p13 == "PRIANTE                                                                                                                                                                                                                                                        "] <- "PMDB"
BESPA$Party[ BESPA$p13 == "RAIMUNDO SANTOS                                                                                                                                                                                                                                                "] <- "PL"
BESPA$Party[ BESPA$p13 == "SILVANA                                                                                                                                                                                                                                                        "] <- "PV"
BESPA$Party[ BESPA$p13 == "VIC PIRES                                                                                                                                                                                                                                                      "] <- "PFL"
BESPA$Party[ BESPA$p13 == "WLAD                                                                                                                                                                                                                                                           "] <- "PMDB"
BESPA$Party[ BESPA$p13 == "WLADIMIR                                                                                                                                                                                                                                                       "] <- "PMDB"
BESPA$Party[ BESPA$p13 == "WLADIMIR SEDW                                                                                                                                                                                                                                                  "] <- "PMDB"
BESPA$Party[ BESPA$p13 == "WLADMIR COSTA                                                                                                                                                                                                                                                  "] <- "PMDB"

# Paraná
BESPR <- ESEB02[ which(ESEB02$uf == "41"),]
TSEPR <- TSEFD2002[ which(TSEFD2002$V8 == "PARANA"),]

BESPR$Party <- NA
BESPR$Party[ BESPR$p13 == "1145                                                                                                                                                                                                                                                           "] <- "PP"
BESPR$Party[ BESPR$p13 == "1333-TIGRÃO                                                                                                                                                                                                                                                    "] <- "PT"
BESPR$Party[ BESPR$p13 == "ALCENI                                                                                                                                                                                                                                                         "] <- "PFL"
BESPR$Party[ BESPR$p13 == "ALCENI GUERRA                                                                                                                                                                                                                                                  "] <- "PFL"
BESPR$Party[ BESPR$p13 == "ALEX CANZIANI                                                                                                                                                                                                                                                  "] <- "PSDB"
BESPR$Party[ BESPR$p13 == "ALMIR GUERRA                                                                                                                                                                                                                                                   "] <- "PFL"
BESPR$Party[ BESPR$p13 == "CENI GERRA                                                                                                                                                                                                                                                     "] <- "PFL"
BESPR$Party[ BESPR$p13 == "CHICO DA PRINCESA                                                                                                                                                                                                                                              "] <- "PSDB"
BESPR$Party[ BESPR$p13 == "DR. OSMAR                                                                                                                                                                                                                                                      "] <- "PMDB"
BESPR$Party[ BESPR$p13 == "DR. ROSINHA                                                                                                                                                                                                                                                    "] <- "PT"
BESPR$Party[ BESPR$p13 == "DRº OSMAR                                                                                                                                                                                                                                                      "] <- "PMDB"
BESPR$Party[ BESPR$p13 == "FRANGÃO                                                                                                                                                                                                                                                        "] <- "PMDB"
BESPR$Party[ BESPR$p13 == "FERNANDO GIACOBO                                                                                                                                                                                                                                               "] <- "PPS"
BESPR$Party[ BESPR$p13 == "FRUET                                                                                                                                                                                                                                                          "] <- "PMDB"
BESPR$Party[ BESPR$p13 == "GERRA (O ALCENI)                                                                                                                                                                                                                                               "] <- "PFL"
BESPR$Party[ BESPR$p13 == "GIACOB                                                                                                                                                                                                                                                         "] <- "PPS"
BESPR$Party[ BESPR$p13 == "GUSTAVO FLUEK                                                                                                                                                                                                                                                  "] <- "PMDB"
BESPR$Party[ BESPR$p13 == "GUSTAVO FRUET                                                                                                                                                                                                                                                  "] <- "PMDB"
BESPR$Party[ BESPR$p13 == "HAULI                                                                                                                                                                                                                                                          "] <- "PSDB"
BESPR$Party[ BESPR$p13 == "HAULY                                                                                                                                                                                                                                                          "] <- "PSDB"
BESPR$Party[ BESPR$p13 == "HENRIQUE BARROS                                                                                                                                                                                                                                                "] <- "PDT"
BESPR$Party[ BESPR$p13 == "JANENE                                                                                                                                                                                                                                                         "] <- "PP"
BESPR$Party[ BESPR$p13 == "LEGENDA (PT)                                                                                                                                                                                                                                                   "] <- "PT"
BESPR$Party[ BESPR$p13 == "LUIS CARLOS HAULY                                                                                                                                                                                                                                              "] <- "PSDB"
BESPR$Party[ BESPR$p13 == "LUPION                                                                                                                                                                                                                                                         "] <- "PFL"
BESPR$Party[ BESPR$p13 == "LÚPIOS                                                                                                                                                                                                                                                         "] <- "PFL"
BESPR$Party[ BESPR$p13 == "LUPTON                                                                                                                                                                                                                                                         "] <- "PFL"
BESPR$Party[ BESPR$p13 == "MAS ROSEMAN                                                                                                                                                                                                                                                    "] <- "PMDB"
BESPR$Party[ BESPR$p13 == "MAX ROSEMAN                                                                                                                                                                                                                                                    "] <- "PMDB"
BESPR$Party[ BESPR$p13 == "MAX ROSENNAM                                                                                                                                                                                                                                                   "] <- "PMDB"
BESPR$Party[ BESPR$p13 == "MAX ROSSEMAN                                                                                                                                                                                                                                                   "] <- "PMDB"
BESPR$Party[ BESPR$p13 == "OSMAR SERALIO                                                                                                                                                                                                                                                  "] <- "PMDB"
BESPR$Party[ BESPR$p13 == "OSMAR SERRAGLIO                                                                                                                                                                                                                                                "] <- "PMDB"
BESPR$Party[ BESPR$p13 == "PAULO BERNARDI                                                                                                                                                                                                                                                 "] <- "PT"
BESPR$Party[ BESPR$p13 == "PT                                                                                                                                                                                                                                                             "] <- "PT"
BESPR$Party[ BESPR$p13 == "TAKAYAMA                                                                                                                                                                                                                                                       "] <- "PTB"
BESPR$Party[ BESPR$p13 == "ZECA                                                                                                                                                                                                                                                           "] <- "PT"
BESPR$Party[ BESPR$p13 == "ZECA DO PT                                                                                                                                                                                                                                                     "] <- "PT"

# Paraíba
BESPB <- ESEB02[ which(ESEB02$uf == "25"),]
TSEPB <- TSEFD2002[ which(TSEFD2002$V8 == "PARAIBA"),]

BESPB$Party <- NA
BESPB$Party[ BESPB$p13 == "ÁLVARO NETO                                                                                                                                                                                                                                                    "] <- "PFL"
BESPB$Party[ BESPB$p13 == "ARMANDO ABILIO                                                                                                                                                                                                                                                 "] <- "PSDB"
BESPB$Party[ BESPB$p13 == "ARMANDO ABÍLIO                                                                                                                                                                                                                                                 "] <- "PSDB"
BESPB$Party[ BESPB$p13 == "BANJAMIM MARANHÃO                                                                                                                                                                                                                                              "] <- "PMDB"
BESPB$Party[ BESPB$p13 == "BENJAMIM MARANHÃO                                                                                                                                                                                                                                              "] <- "PMDB"
BESPB$Party[ BESPB$p13 == "BENJAMIM MARANHÃO                                                                                                                                                                                                                                              \001"] <- "PMDB"
BESPB$Party[ BESPB$p13 == "BENJAMIN MARANHÃO                                                                                                                                                                                                                                              \001"] <- "PMDB"
BESPB$Party[ BESPB$p13 == "DOMICIANO CABRAL                                                                                                                                                                                                                                               "] <- "PSDB"
BESPB$Party[ BESPB$p13 == "DR. DAMIÃO                                                                                                                                                                                                                                                     "] <- "PMDB"
BESPB$Party[ BESPB$p13 == "EDGAR MALAGODI                                                                                                                                                                                                                                                 \001"] <- "PT"
BESPB$Party[ BESPB$p13 == "FERNANDO CUNHA LIMA                                                                                                                                                                                                                                            "] <- "PSDB"
BESPB$Party[ BESPB$p13 == "LUCIA BRAGA                                                                                                                                                                                                                                                    "] <- "PSD"
BESPB$Party[ BESPB$p13 == "LUIZ COUTO                                                                                                                                                                                                                                                     "] <- "PT"
BESPB$Party[ BESPB$p13 == "LUIZ COUTO                                                                                                                                                                                                                                                     \001"] <- "PT"
BESPB$Party[ BESPB$p13 == "RICARDO RIQUE                                                                                                                                                                                                                                                  "] <- "PSDB"
BESPB$Party[ BESPB$p13 == "RONALDO CUNHA LIMA                                                                                                                                                                                                                                             "] <- "PSDB"
BESPB$Party[ BESPB$p13 == "RONALDO CUNHA LIMA                                                                                                                                                                                                                                             \001"] <- "PSDB"
BESPB$Party[ BESPB$p13 == "WELINGTON ROBERTO                                                                                                                                                                                                                                              "] <- "PTB"
BESPB$Party[ BESPB$p13 == "WELLINGTON ROBERTO                                                                                                                                                                                                                                             "] <- "PTB"
BESPB$Party[ BESPB$p13 == "WELLINGTON ROBRETO                                                                                                                                                                                                                                             "] <- "PTB"
BESPB$Party[ BESPB$p13 == "WILSON SANTIAGO                                                                                                                                                                                                                                                "] <- "PMDB"

# Pernambuco
BESPE <- ESEB02[ which(ESEB02$uf == "26"),]
TSEPE <- TSEFD2002[ which(TSEFD2002$V8 == "PERNAMBUCO"),]

BESPE$Party <- NA
BESPE$Party[ BESPE$p13 == "ALBERTO FEITOSA                                                                                                                                                                                                                                                "] <- "PL"
BESPE$Party[ BESPE$p13 == "ARMANDO MONTEIRO                                                                                                                                                                                                                                               "] <- "PMDB"
BESPE$Party[ BESPE$p13 == "CABO ARNALDO                                                                                                                                                                                                                                                   "] <- "PV"
BESPE$Party[ BESPE$p13 == "CADOCA                                                                                                                                                                                                                                                         "] <- "PMDB"
BESPE$Party[ BESPE$p13 == "CHARLES LUCENA                                                                                                                                                                                                                                                 "] <- "PSDB"
BESPE$Party[ BESPE$p13 == "DR. INOCÊNCIO                                                                                                                                                                                                                                                  "] <- "PFL"
BESPE$Party[ BESPE$p13 == "INOCÊNCIO                                                                                                                                                                                                                                                      "] <- "PFL"
BESPE$Party[ BESPE$p13 == "INOCÊNCIO OLIVEIRA                                                                                                                                                                                                                                             "] <- "PFL"
BESPE$Party[ BESPE$p13 == "JOSÉ CHAVES                                                                                                                                                                                                                                                    "] <- "PMDB"
BESPE$Party[ BESPE$p13 == "LUIZ PIAUHY LINO                                                                                                                                                                                                                                               "] <- "PSDB"
BESPE$Party[ BESPE$p13 == "MARCO DE JESUS                                                                                                                                                                                                                                                 "] <- "PL"
BESPE$Party[ BESPE$p13 == "MIGUEL ARRAES                                                                                                                                                                                                                                                  "] <- "PSB"
BESPE$Party[ BESPE$p13 == "OSWALDO COELHJO                                                                                                                                                                                                                                                "] <- "PFL"
BESPE$Party[ BESPE$p13 == "OSWALDO COELHO                                                                                                                                                                                                                                                 "] <- "PFL"
BESPE$Party[ BESPE$p13 == "PASTOR MARCOS DE JESUS                                                                                                                                                                                                                                         "] <- "PL"
BESPE$Party[ BESPE$p13 == "PAULO RUBENS SANTIAGO                                                                                                                                                                                                                                          "] <- "PT"
BESPE$Party[ BESPE$p13 == "PEDRO EUGÊNIO                                                                                                                                                                                                                                                  "] <- "PT"
BESPE$Party[ BESPE$p13 == "PIAUHY LINO                                                                                                                                                                                                                                                    "] <- "PSDB"
BESPE$Party[ BESPE$p13 == "PIAUILINO                                                                                                                                                                                                                                                      "] <- "PSDB"
BESPE$Party[ BESPE$p13 == "PIAVILINO                                                                                                                                                                                                                                                      "] <- "PSDB"
BESPE$Party[ BESPE$p13 == "PIDUINO                                                                                                                                                                                                                                                        "] <- "PSDB"
BESPE$Party[ BESPE$p13 == "RAUL JUGMAN                                                                                                                                                                                                                                                    "] <- "PMDB"
BESPE$Party[ BESPE$p13 == "RAUL JUNGERMAN                                                                                                                                                                                                                                                 "] <- "PMDB"
BESPE$Party[ BESPE$p13 == "RAUL JUNGUEMAN                                                                                                                                                                                                                                                 "] <- "PMDB"
BESPE$Party[ BESPE$p13 == "RENILDO CALHEIROS                                                                                                                                                                                                                                              "] <- "PC do B"
BESPE$Party[ BESPE$p13 == "RENILDO CALMEIRO                                                                                                                                                                                                                                               "] <- "PC do B"
BESPE$Party[ BESPE$p13 == "ROBERTO FREIRE                                                                                                                                                                                                                                                 "] <- "PPS"
BESPE$Party[ BESPE$p13 == "ROBERTO MAGALHÃES                                                                                                                                                                                                                                              "] <- "PSDB"

# Piauí
BESPI <- ESEB02[ which(ESEB02$uf == "22"),]
TSEPI <- TSEFD2002[ which(TSEFD2002$V8 == "PIAUI"),]

BESPI$Party <- NA
BESPI$Party[ BESPI$p13 == "AFONSO GIL                                                                                                                                                                                                                                                     "] <- "PC do B"
BESPI$Party[ BESPI$p13 == "ATILA                                                                                                                                                                                                                                                          "] <- "PSDB"
BESPI$Party[ BESPI$p13 == "FRANCISCA TRINDADE                                                                                                                                                                                                                                             "] <- "PT"
BESPI$Party[ BESPI$p13 == "JULIO CESAR                                                                                                                                                                                                                                                    "] <- "PFL"
BESPI$Party[ BESPI$p13 == "JULÍO CESAR                                                                                                                                                                                                                                                    "] <- "PFL"
BESPI$Party[ BESPI$p13 == "JÚLIO CESAR                                                                                                                                                                                                                                                    "] <- "PFL"
BESPI$Party[ BESPI$p13 == "MORAIS SOUZA                                                                                                                                                                                                                                                   "] <- "PMDB"
BESPI$Party[ BESPI$p13 == "TRINDADE                                                                                                                                                                                                                                                       "] <- "PT"

# Rio de Janeiro
BESRJ <- ESEB02[ which(ESEB02$uf == "33"),]
TSERJ <- TSEFD2002[ which(TSEFD2002$V8 == "RIO DE JANEIRO"),]

BESRJ$Party <- NA
BESRJ$Party[ BESRJ$p13 == "2233                                                                                                                                                                                                                                                           "] <- "PL"
BESRJ$Party[ BESRJ$p13 == "4004                                                                                                                                                                                                                                                           "] <- "PSB"
BESRJ$Party[ BESRJ$p13 == "4070                                                                                                                                                                                                                                                           "] <- "PSB"
BESRJ$Party[ BESRJ$p13 == "4545 RONALDO CEZAR                                                                                                                                                                                                                                             "] <- "PSDB"
BESRJ$Party[ BESRJ$p13 == "4545 RONALDO CEZAR COELHO                                                                                                                                                                                                                                      "] <- "PSDB"
BESRJ$Party[ BESRJ$p13 == "ALEXANDRE NOVAES                                                                                                                                                                                                                                               "] <- "PDT"
BESRJ$Party[ BESRJ$p13 == "ALEXANDRE SANTOS                                                                                                                                                                                                                                               "] <- "PSDB"
BESRJ$Party[ BESRJ$p13 == "ALEXANDRTE SANTOS                                                                                                                                                                                                                                              "] <- "PSDB"
BESRJ$Party[ BESRJ$p13 == "ALGUM CANDIDATO DO PT NÃO LEMBRA O NOME                                                                                                                                                                                                                        "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "ARILDO                                                                                                                                                                                                                                                         "] <- "PSC"
BESRJ$Party[ BESRJ$p13 == "ARNALDO CESAR COELHO                                                                                                                                                                                                                                           "] <- "PSDB"
BESRJ$Party[ BESRJ$p13 == "BISPO RODRIGUES                                                                                                                                                                                                                                                "] <- "PL"
BESRJ$Party[ BESRJ$p13 == "BITTAR                                                                                                                                                                                                                                                         "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "CAJU                                                                                                                                                                                                                                                           "] <- "PV"
BESRJ$Party[ BESRJ$p13 == "CHACON                                                                                                                                                                                                                                                         "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "CHICO ALENCAR                                                                                                                                                                                                                                                  "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "CHICO NALENCAR                                                                                                                                                                                                                                                 "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "CORONEL ARACATI                                                                                                                                                                                                                                                "] <- "PL"
BESRJ$Party[ BESRJ$p13 == "DENISE FRASSARD                                                                                                                                                                                                                                                "] <- "PSDB"
BESRJ$Party[ BESRJ$p13 == "DENISE FRONSARD                                                                                                                                                                                                                                                "] <- "PSDB"
BESRJ$Party[ BESRJ$p13 == "DENISE FROSSAD                                                                                                                                                                                                                                                 "] <- "PSDB"
BESRJ$Party[ BESRJ$p13 == "DENISE FROSSARD                                                                                                                                                                                                                                                "] <- "PSDB"
BESRJ$Party[ BESRJ$p13 == "DR. BALBI Nº 2013                                                                                                                                                                                                                                              "] <- "PSC"
BESRJ$Party[ BESRJ$p13 == "EDUARDO DO PV                                                                                                                                                                                                                                                  "] <- "PV"
BESRJ$Party[ BESRJ$p13 == "EDUARDO PAES                                                                                                                                                                                                                                                   "] <- "PFL"
BESRJ$Party[ BESRJ$p13 == "ESPOSA DO FINADO JOCA - MARIA LÚCIA (BELFORD ROXO) - MARIA LÚCIA                                                                                                                                                                                               "] <- "PMDB"
BESRJ$Party[ BESRJ$p13 == "FERNANDO LOPES                                                                                                                                                                                                                                                 "] <- "PSB"
BESRJ$Party[ BESRJ$p13 == "FOI ALGUM CANDIDATO DO PT (NÃO FOI NA LEGENDA)                                                                                                                                                                                                                 "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "FROSSARD                                                                                                                                                                                                                                                       "] <- "PSDB"
BESRJ$Party[ BESRJ$p13 == "GABEIRA                                                                                                                                                                                                                                                        "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "GLÁURIO                                                                                                                                                                                                                                                        "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "GLÁURIO DOS SANTOS (PT)                                                                                                                                                                                                                                        "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "IÉDIO ROSA                                                                                                                                                                                                                                                     "] <- "PFL"
BESRJ$Party[ BESRJ$p13 == "JAIR BOLSONARO                                                                                                                                                                                                                                                 "] <- "PP"
BESRJ$Party[ BESRJ$p13 == "JANDIRA FEGALI                                                                                                                                                                                                                                                 "] <- "PC do B"
BESRJ$Party[ BESRJ$p13 == "JANDIRA FEGALLI                                                                                                                                                                                                                                                "] <- "PC do B"
BESRJ$Party[ BESRJ$p13 == "JANDIRA FEGHALI                                                                                                                                                                                                                                                "] <- "PC do B"
BESRJ$Party[ BESRJ$p13 == "JANDIRA FEGHALI 6565                                                                                                                                                                                                                                           "] <- "PC do B"
BESRJ$Party[ BESRJ$p13 == "JANDIRTA FEGALI                                                                                                                                                                                                                                                "] <- "PC do B"
BESRJ$Party[ BESRJ$p13 == "JOÃO MENDES                                                                                                                                                                                                                                                    "] <- "PFL"
BESRJ$Party[ BESRJ$p13 == "JORGE BITTAR                                                                                                                                                                                                                                                   "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "JORGE BITTAR 1331                                                                                                                                                                                                                                              "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "JUNINHO CAJU                                                                                                                                                                                                                                                   "] <- "PV"
BESRJ$Party[ BESRJ$p13 == "LINDBERG                                                                                                                                                                                                                                                       "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "LUIS ROGÉRIO                                                                                                                                                                                                                                                   "] <- "PSB"
BESRJ$Party[ BESRJ$p13 == "LUÍS ROGÉRIO                                                                                                                                                                                                                                                   "] <- "PSB"
BESRJ$Party[ BESRJ$p13 == "LUIZ MEATO                                                                                                                                                                                                                                                     "] <- "PFL"
BESRJ$Party[ BESRJ$p13 == "LUIZ ROGERIO                                                                                                                                                                                                                                                   "] <- "PSB"
BESRJ$Party[ BESRJ$p13 == "LUIZ ROGÉRIO                                                                                                                                                                                                                                                   "] <- "PSB"
BESRJ$Party[ BESRJ$p13 == "MARQUINHO PEÇANHA                                                                                                                                                                                                                                              "] <- "PSDC"
BESRJ$Party[ BESRJ$p13 == "MIRIAM                                                                                                                                                                                                                                                         "] <- "PSB"
BESRJ$Party[ BESRJ$p13 == "MIRO TEIXEIRA                                                                                                                                                                                                                                                  "] <- "PDT"
BESRJ$Party[ BESRJ$p13 == "MOREIRA FRANCO                                                                                                                                                                                                                                                 "] <- "PMDB"
BESRJ$Party[ BESRJ$p13 == "N. 2210                                                                                                                                                                                                                                                        "] <- "PL"
BESRJ$Party[ BESRJ$p13 == "PAULO FEIJÓ                                                                                                                                                                                                                                                    "] <- "PSDB"
BESRJ$Party[ BESRJ$p13 == "PETRA                                                                                                                                                                                                                                                          "] <- "PDT"
BESRJ$Party[ BESRJ$p13 == "PT                                                                                                                                                                                                                                                             "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "PT (VOTOU NA LEGENDA DO PT)                                                                                                                                                                                                                                    "] <- "PT"
BESRJ$Party[ BESRJ$p13 == "RICARDO MARANHÃO                                                                                                                                                                                                                                               "] <- "PSB"
BESRJ$Party[ BESRJ$p13 == "RODRIGO MAIA                                                                                                                                                                                                                                                   "] <- "PFL"
BESRJ$Party[ BESRJ$p13 == "RONALDO CESAR COELHO                                                                                                                                                                                                                                           "] <- "PSDB"
BESRJ$Party[ BESRJ$p13 == "TUCA NASCIMENTO                                                                                                                                                                                                                                                "] <- "PSC"
BESRJ$Party[ BESRJ$p13 == "WAGNER SIQUEIRA                                                                                                                                                                                                                                                "] <- "PMDB"

# Rio Grande do Norte
BESRN <- ESEB02[ which(ESEB02$uf == "24"),]
TSERN <- TSEFD2002[ which(TSEFD2002$V8 == "RIO GRANDE DO NORTE"),]

BESRN$Party <- NA
BESRN$Party[ BESRN$p13 == "ALEXANDRE DANTAS                                                                                                                                                                                                                                               "] <- "PP"
BESRN$Party[ BESRN$p13 == "ALEXANDRE DANTAS                                                                                                                                                                                                                                               \001"] <- "PP"
BESRN$Party[ BESRN$p13 == "ALVARO DIAS                                                                                                                                                                                                                                                    "] <- "PMDB"
BESRN$Party[ BESRN$p13 == "BETINHO ROSADO                                                                                                                                                                                                                                                 "] <- "PFL"
BESRN$Party[ BESRN$p13 == "FÁTIMA BEZERRA                                                                                                                                                                                                                                                 "] <- "PT"
BESRN$Party[ BESRN$p13 == "FÁTIMA BEZERRA                                                                                                                                                                                                                                                 \001"] <- "PT"
BESRN$Party[ BESRN$p13 == "HENRIQUE ALVES                                                                                                                                                                                                                                                 "] <- "PMDB"
BESRN$Party[ BESRN$p13 == "JUTAÍ MENEZES                                                                                                                                                                                                                                                  "] <- "PDT"
BESRN$Party[ BESRN$p13 == "LAVOSIER                                                                                                                                                                                                                                                       "] <- "PFL"
BESRN$Party[ BESRN$p13 == "TEREZINHA                                                                                                                                                                                                                                                      \001"] <- "PDT"

# Rondônia
BESRO <- ESEB02[ which(ESEB02$uf == "11"),]
TSERO <- TSEFD2002[ which(TSEFD2002$V8 == "RONDONIA"),]

BESRO$Party <- NA
BESRO$Party[ BESRO$p13 == "SÉRGIO CARVALHIO                                                                                                                                                                                                                                               "] <- "PSDB"

# Roraima
BESRR <- ESEB02[ which(ESEB02$uf == "14"),]
TSERR <- TSEFD2002[ which(TSEFD2002$V8 == "RORAIMA"),]

BESRR$Party <- NA
BESRR$Party[ BESRR$p13 == "DR. CAMPELO                                                                                                                                                                                                                                                    "] <- "PFL"
BESRR$Party[ BESRR$p13 == "LUIZINHO                                                                                                                                                                                                                                                       "] <- "PFL"
BESRR$Party[ BESRR$p13 == "ROBÉRIO ARAÚJO                                                                                                                                                                                                                                                 "] <- "PL"

# Rio Grande do Sul
BESRS <- ESEB02[ which(ESEB02$uf == "43"),]
TSERS <- TSEFD2002[ which(TSEFD2002$V8 == "RIO GRANDE DO SUL"),]

BESRS$Party <- NA
BESRS$Party[ BESRS$p13 == "13 CANDIDATO DO PARTIDO                                                                                                                                                                                                                                        "] <- "PT"
BESRS$Party[ BESRS$p13 == "13 PARTIDO DO CANDIDATO                                                                                                                                                                                                                                        "] <- "PT"
BESRS$Party[ BESRS$p13 == "13 SÓ LEMBRA O PARTIDO DO CANDIDATO                                                                                                                                                                                                                            "] <- "PT"
BESRS$Party[ BESRS$p13 == "22 PARTIDO DO CANDIDATO                                                                                                                                                                                                                                        "] <- "PL"
BESRS$Party[ BESRS$p13 == "40 LEGENDA                                                                                                                                                                                                                                                     "] <- "PSB"
BESRS$Party[ BESRS$p13 == "ANA CORSO - PT                                                                                                                                                                                                                                                 "] <- "PT"
BESRS$Party[ BESRS$p13 == "ANA CORSO                                                                                                                                                                                                                                                      "] <- "PT"
BESRS$Party[ BESRS$p13 == "BETO ALBUQUERQUE                                                                                                                                                                                                                                               "] <- "PSB"
BESRS$Party[ BESRS$p13 == "BIOLKI PMDB                                                                                                                                                                                                                                                    "] <- "PMDB"
BESRS$Party[ BESRS$p13 == "CARRION                                                                                                                                                                                                                                                        "] <- "PC do B"
BESRS$Party[ BESRS$p13 == "CARRION JUNIOR                                                                                                                                                                                                                                                 "] <- "PC do B"
BESRS$Party[ BESRS$p13 == "COVATTI (DE ENCANTADO)                                                                                                                                                                                                                                         "] <- "PMDB"
BESRS$Party[ BESRS$p13 == "COZATTI                                                                                                                                                                                                                                                        "] <- "PMDB"
BESRS$Party[ BESRS$p13 == "EDIR DE OLIVEIRA                                                                                                                                                                                                                                               "] <- "PTB"
BESRS$Party[ BESRS$p13 == "ELISEU PADILHA                                                                                                                                                                                                                                                 "] <- "PMDB"
BESRS$Party[ BESRS$p13 == "ENIO BACCI                                                                                                                                                                                                                                                     "] <- "PDT"
BESRS$Party[ BESRS$p13 == "ÉRICO RIBEIRO                                                                                                                                                                                                                                                  "] <- "PP"
BESRS$Party[ BESRS$p13 == "FETTER JÚNIOR                                                                                                                                                                                                                                                  "] <- "PP"
BESRS$Party[ BESRS$p13 == "FORTUNATI-PDT                                                                                                                                                                                                                                                  "] <- "PDT"
BESRS$Party[ BESRS$p13 == "FRANCISCO APIO                                                                                                                                                                                                                                                 "] <- "PP"
BESRS$Party[ BESRS$p13 == "FRANCISCO TURRA                                                                                                                                                                                                                                                "] <- "PP"
BESRS$Party[ BESRS$p13 == "IBSEN PINHEIRO                                                                                                                                                                                                                                                 "] <- "PMDB"
BESRS$Party[ BESRS$p13 == "JOSE IVO SORTORI                                                                                                                                                                                                                                               "] <- "PMDB"
BESRS$Party[ BESRS$p13 == "JOSE SARTORE-PMDB                                                                                                                                                                                                                                              "] <- "PMDB"
BESRS$Party[ BESRS$p13 == "LEGENDA DO PT                                                                                                                                                                                                                                                  "] <- "PT"
BESRS$Party[ BESRS$p13 == "LUÍZ CARLOS ENGE (PMDB)                                                                                                                                                                                                                                        "] <- "PMDB"
BESRS$Party[ BESRS$p13 == "MARIA DO ROSÁRIO                                                                                                                                                                                                                                               "] <- "PT"
BESRS$Party[ BESRS$p13 == "MARIA DO ROSÁRIO (PT)                                                                                                                                                                                                                                          "] <- "PT"
BESRS$Party[ BESRS$p13 == "MIKI BREIER                                                                                                                                                                                                                                                    "] <- "PT"
BESRS$Party[ BESRS$p13 == "ONIX LORENZONE                                                                                                                                                                                                                                                 "] <- "PFL"
BESRS$Party[ BESRS$p13 == "ONIX LORENZONI                                                                                                                                                                                                                                                 "] <- "PFL"
BESRS$Party[ BESRS$p13 == "PAULO ODONE                                                                                                                                                                                                                                                    "] <- "PPS"
BESRS$Party[ BESRS$p13 == "PAULO PIMENTA                                                                                                                                                                                                                                                  "] <- "PT"
BESRS$Party[ BESRS$p13 == "POMPEO DE MATTOS                                                                                                                                                                                                                                               "] <- "PDT"
BESRS$Party[ BESRS$p13 == "POMPEU DE MATTOS                                                                                                                                                                                                                                               "] <- "PDT"
BESRS$Party[ BESRS$p13 == "PT-NÃO LEMBRA O NOME                                                                                                                                                                                                                                           "] <- "PT"
BESRS$Party[ BESRS$p13 == "RAUL CORRION                                                                                                                                                                                                                                                   "] <- "PMDB"
BESRS$Party[ BESRS$p13 == "ROBERTO ARGENTA-PHS                                                                                                                                                                                                                                            "] <- "PHS"
BESRS$Party[ BESRS$p13 == "SARTORI - PMDB                                                                                                                                                                                                                                                 "] <- "PMDB"
BESRS$Party[ BESRS$p13 == "SARTORI                                                                                                                                                                                                                                                        "] <- "PMDB"
BESRS$Party[ BESRS$p13 == "SÉRGIO MOREIRA                                                                                                                                                                                                                                                 "] <- "PTB"
BESRS$Party[ BESRS$p13 == "TARCÍSIO ZIMMERMAN                                                                                                                                                                                                                                             "] <- "PT"
BESRS$Party[ BESRS$p13 == "TURRA                                                                                                                                                                                                                                                          "] <- "PP"
BESRS$Party[ BESRS$p13 == "VALDEMAR RIBEIRO - PL                                                                                                                                                                                                                                          "] <- "PL"
BESRS$Party[ BESRS$p13 == "YEDA CRUSIUS                                                                                                                                                                                                                                                   "] <- "PSDB"

# Santa Catarina
BESSC <- ESEB02[ which(ESEB02$uf == "42"),]
TSESC <- TSEFD2002[ which(TSEFD2002$V8 == "SANTA CATARINA"),]

BESSC$Party <- NA
BESSC$Party[ BESSC$p13 == "AFRÂNIO BOBRÉ PT                                                                                                                                                                                                                                               "] <- "PT"
BESSC$Party[ BESSC$p13 == "AFRÂNIO PT                                                                                                                                                                                                                                                     "] <- "PT"
BESSC$Party[ BESSC$p13 == "BALDOINO 104 PTB                                                                                                                                                                                                                                               "] <- "PTB"
BESSC$Party[ BESSC$p13 == "CORUJA                                                                                                                                                                                                                                                         "] <- "PDT"
BESSC$Party[ BESSC$p13 == "CORUJA 1236                                                                                                                                                                                                                                                    "] <- "PDT"
BESSC$Party[ BESSC$p13 == "FERNANDO AGUSTINHO CORUJA                                                                                                                                                                                                                                      "] <- "PDT"
BESSC$Party[ BESSC$p13 == "FERNANDO CORUJA                                                                                                                                                                                                                                                "] <- "PDT"
BESSC$Party[ BESSC$p13 == "GERVÁSIO JOSÉ SILVA                                                                                                                                                                                                                                            "] <- "PFL"
BESSC$Party[ BESSC$p13 == "IVAN RANZOLIN                                                                                                                                                                                                                                                  "] <- "PP"
BESSC$Party[ BESSC$p13 == "IVAN RIZOLIN                                                                                                                                                                                                                                                   "] <- "PP"
BESSC$Party[ BESSC$p13 == "JOÃO MATOS                                                                                                                                                                                                                                                     "] <- "PMDB"
BESSC$Party[ BESSC$p13 == "JORGE BUEIRA                                                                                                                                                                                                                                                   "] <- "PT"
BESSC$Party[ BESSC$p13 == "LUCI 1333                                                                                                                                                                                                                                                      "] <- "PT"
BESSC$Party[ BESSC$p13 == "MAURO PASSOS                                                                                                                                                                                                                                                   "] <- "PT"
BESSC$Party[ BESSC$p13 == "PISOLATTE                                                                                                                                                                                                                                                      "] <- "PP"
BESSC$Party[ BESSC$p13 == "RANSOLIN 1171                                                                                                                                                                                                                                                  "] <- "PP"
BESSC$Party[ BESSC$p13 == "RANZOLIM                                                                                                                                                                                                                                                       "] <- "PP"
BESSC$Party[ BESSC$p13 == "RANZOLIN                                                                                                                                                                                                                                                       "] <- "PP"
BESSC$Party[ BESSC$p13 == "SERGE 1308 (PT)                                                                                                                                                                                                                                                "] <- "PT"

# Sergipe
BESSE <- ESEB02[ which(ESEB02$uf == "28"),]
TSESE <- TSEFD2002[ which(TSEFD2002$V8 == "SERGIPE"),]

BESSE$Party <- NA
BESSE$Party[ BESSE$p13 == "BOSCO COSTA                                                                                                                                                                                                                                                    "] <- "PSDB"
BESSE$Party[ BESSE$p13 == "DUDU                                                                                                                                                                                                                                                           "] <- "PT"
BESSE$Party[ BESSE$p13 == "JOÃO FONTES                                                                                                                                                                                                                                                    "] <- "PT"
BESSE$Party[ BESSE$p13 == "JOÃO FORTES                                                                                                                                                                                                                                                    "] <- "PT"
BESSE$Party[ BESSE$p13 == "JOSÉ CARLOS MACHADO                                                                                                                                                                                                                                            "] <- "PFL"
BESSE$Party[ BESSE$p13 == "JOSÉ CARLOS MACHADO                                                                                                                                                                                                                                            "] <- "PFL"
BESSE$Party[ BESSE$p13 == "LUIZA RIBEIRO                                                                                                                                                                                                                                                  "] <- "PT do B"
BESSE$Party[ BESSE$p13 == "PASTOR HELENO                                                                                                                                                                                                                                                  "] <- "PL"
BESSE$Party[ BESSE$p13 == "PEDRINHO VALADARES                                                                                                                                                                                                                                             "] <- "PSB"
BESSE$Party[ BESSE$p13 == "SAMARONE                                                                                                                                                                                                                                                       "] <- "PT"
BESSE$Party[ BESSE$p13 == "TÂNIA SOARES                                                                                                                                                                                                                                                   "] <- "PC do B"

# São Paulo
BESSP <- ESEB02[ which(ESEB02$uf == "35"),]
YSESP <- TSEFD2002[ whcih(TSEFD2002$V8 == "SAO PAULO"),]

BESSP$Party <- NA
BESSP$Party[ BESSP$p13 == "1116                                                                                                                                                                                                                                                           "] <- "PP"
BESSP$Party[ BESSP$p13 == "13                                                                                                                                                                                                                                                             "] <- "PT"
BESSP$Party[ BESSP$p13 == "1313                                                                                                                                                                                                                                                           "] <- "PT"
BESSP$Party[ BESSP$p13 == "4010                                                                                                                                                                                                                                                           "] <- "PSB"
BESSP$Party[ BESSP$p13 == "43                                                                                                                                                                                                                                                             "] <- "PV"
BESSP$Party[ BESSP$p13 == "43 (PV-LEGENDA)                                                                                                                                                                                                                                                "] <- "PV"
BESSP$Party[ BESSP$p13 == "45                                                                                                                                                                                                                                                             "] <- "PSDB"
BESSP$Party[ BESSP$p13 == "4560- FERNANDO FUADI                                                                                                                                                                                                                                           "] <- "PSDB"
BESSP$Party[ BESSP$p13 == "AMORIM                                                                                                                                                                                                                                                         "] <- "PV"
BESSP$Party[ BESSP$p13 == "ANDRÉ MONTORO                                                                                                                                                                                                                                                  "] <- "PSDB"
BESSP$Party[ BESSP$p13 == "ANGELA GUADAGUAMIM                                                                                                                                                                                                                                             "] <- "PT"
BESSP$Party[ BESSP$p13 == "ANTONIO CARLOS DADO                                                                                                                                                                                                                                            "] <- "PCO"
BESSP$Party[ BESSP$p13 == "ARNALDO FARIA DE SÁ                                                                                                                                                                                                                                            "] <- "PTB"
BESSP$Party[ BESSP$p13 == "AROUCA                                                                                                                                                                                                                                                         "] <- "PL"
BESSP$Party[ BESSP$p13 == "CARLOS SAMPAIO                                                                                                                                                                                                                                                 "] <- "PSDB"
BESSP$Party[ BESSP$p13 == "CARMEM -PARTIDO VERDE                                                                                                                                                                                                                                          "] <- "PV"
BESSP$Party[ BESSP$p13 == "CELSO  RUSSOMANO                                                                                                                                                                                                                                               "] <- "PP"
BESSP$Party[ BESSP$p13 == "CELSO RUSSO                                                                                                                                                                                                                                                    "] <- "PP"
BESSP$Party[ BESSP$p13 == "CELSO RUSSO MANO                                                                                                                                                                                                                                               "] <- "PP"
BESSP$Party[ BESSP$p13 == "CELSO RUSSOMANO                                                                                                                                                                                                                                                "] <- "PP"
BESSP$Party[ BESSP$p13 == "CESAR COLIGARI                                                                                                                                                                                                                                                 "] <- "PSB"
BESSP$Party[ BESSP$p13 == "CHICÃO                                                                                                                                                                                                                                                         "] <- "PGT"
BESSP$Party[ BESSP$p13 == "CLÁUDIO MAGRÃO                                                                                                                                                                                                                                                 "] <- "PPS"
BESSP$Party[ BESSP$p13 == "CLEAIN FERRARI                                                                                                                                                                                                                                                 "] <- "PL"
BESSP$Party[ BESSP$p13 == "CLEMENTE                                                                                                                                                                                                                                                       "] <- "PSB"
BESSP$Party[ BESSP$p13 == "DEVANIR RIBEIRO                                                                                                                                                                                                                                                "] <- "PT"
BESSP$Party[ BESSP$p13 == "DIOGO DO PT                                                                                                                                                                                                                                                    "] <- "PT"
BESSP$Party[ BESSP$p13 == "DO PT                                                                                                                                                                                                                                                          "] <- "PT"
BESSP$Party[ BESSP$p13 == "DR. EDNO MACEDO                                                                                                                                                                                                                                                "] <- "PTB"
BESSP$Party[ BESSP$p13 == "DR. LUIS TEOFILO                                                                                                                                                                                                                                               "] <- "PGT"
BESSP$Party[ BESSP$p13 == "EDCLEI                                                                                                                                                                                                                                                         "] <- "PRTB"
BESSP$Party[ BESSP$p13 == "EDICIEI                                                                                                                                                                                                                                                        "] <- "PRTB"
BESSP$Party[ BESSP$p13 == "EDICLEI                                                                                                                                                                                                                                                        "] <- "PRTB"
BESSP$Party[ BESSP$p13 == "EDINHO MONTEMOR                                                                                                                                                                                                                                                "] <- "PSB"
BESSP$Party[ BESSP$p13 == "EDNA MACEDO                                                                                                                                                                                                                                                    "] <- "PTB"
BESSP$Party[ BESSP$p13 == "EDUARDO CARDOZO                                                                                                                                                                                                                                                "] <- "PT"
BESSP$Party[ BESSP$p13 == "ELIAS FERNANDES DE CARVALHO                                                                                                                                                                                                                                    "] <- "PL"
BESSP$Party[ BESSP$p13 == "ENEAS                                                                                                                                                                                                                                                          "] <- "PRONA"
BESSP$Party[ BESSP$p13 == "ENÉAS                                                                                                                                                                                                                                                          "] <- "PRONA"
BESSP$Party[ BESSP$p13 == "ÉNEAS                                                                                                                                                                                                                                                          "] <- "PRONA"
BESSP$Party[ BESSP$p13 == "ËNEAS                                                                                                                                                                                                                                                          "] <- "PRONA"
BESSP$Party[ BESSP$p13 == "ENEIAS                                                                                                                                                                                                                                                         "] <- "PRONA"
BESSP$Party[ BESSP$p13 == "ENÉIAS                                                                                                                                                                                                                                                         "] <- "PRONA"
BESSP$Party[ BESSP$p13 == "ERUNDINA                                                                                                                                                                                                                                                       "] <- "PSB"
BESSP$Party[ BESSP$p13 == "ESTIMA                                                                                                                                                                                                                                                         "] <- "PL"
BESSP$Party[ BESSP$p13 == "FANTAZINI (PT)                                                                                                                                                                                                                                                 "] <- "PT"
BESSP$Party[ BESSP$p13 == "GERALDO NASCIMENTO                                                                                                                                                                                                                                             "] <- "PSB"
BESSP$Party[ BESSP$p13 == "GILBRETO KASSAB                                                                                                                                                                                                                                                "] <- "PFL"
BESSP$Party[ BESSP$p13 == "GOLDAMAN                                                                                                                                                                                                                                                       "] <- "PSDB"
BESSP$Party[ BESSP$p13 == "GOLDMAN                                                                                                                                                                                                                                                        "] <- "PSDB"
BESSP$Party[ BESSP$p13 == "GUILHERME GORTI                                                                                                                                                                                                                                                "] <- "PV"
BESSP$Party[ BESSP$p13 == "GUILHERME JOSKAI                                                                                                                                                                                                                                               "] <- "PV"
BESSP$Party[ BESSP$p13 == "JOÃO GILBERTO                                                                                                                                                                                                                                                  "] <- "PPS"
BESSP$Party[ BESSP$p13 == "JOÃO PAULO                                                                                                                                                                                                                                                     "] <- "PT"
BESSP$Party[ BESSP$p13 == "JOAS PAULO - PT                                                                                                                                                                                                                                                "] <- "PT"
BESSP$Party[ BESSP$p13 == "JOSÉ DE ALENCAR                                                                                                                                                                                                                                                "] <- "PMDB"
BESSP$Party[ BESSP$p13 == "JOSE DIRCEU                                                                                                                                                                                                                                                    "] <- "PT"
BESSP$Party[ BESSP$p13 == "JOSÉ DIRCEU                                                                                                                                                                                                                                                    "] <- "PT"
BESSP$Party[ BESSP$p13 == "JOSE EDUARDO CARDOSO                                                                                                                                                                                                                                           "] <- "PT"
BESSP$Party[ BESSP$p13 == "JOSÉ EDUARDO CARDOSO                                                                                                                                                                                                                                           "] <- "PT"
BESSP$Party[ BESSP$p13 == "JOSÉ MENDUR                                                                                                                                                                                                                                                    "] <- "PT"
BESSP$Party[ BESSP$p13 == "JOSÉ MENSOR                                                                                                                                                                                                                                                    "] <- "PT"
BESSP$Party[ BESSP$p13 == "JOSÉ ONÉRIO                                                                                                                                                                                                                                                    "] <- "PDT"
BESSP$Party[ BESSP$p13 == "JOVINO - PV                                                                                                                                                                                                                                                    "] <- "PV"
BESSP$Party[ BESSP$p13 == "KASSAB                                                                                                                                                                                                                                                         "] <- "PFL"
BESSP$Party[ BESSP$p13 == "LEGENDA (13)                                                                                                                                                                                                                                                   "] <- "PT"
BESSP$Party[ BESSP$p13 == "LEGENDA 45                                                                                                                                                                                                                                                     "] <- "PSDB"
BESSP$Party[ BESSP$p13 == "LEGENDA DO PT                                                                                                                                                                                                                                                  "] <- "PT"
BESSP$Party[ BESSP$p13 == "LEGENDA PT                                                                                                                                                                                                                                                     "] <- "PT"
BESSP$Party[ BESSP$p13 == "LEGENDA PT (13)                                                                                                                                                                                                                                                "] <- "PT"
BESSP$Party[ BESSP$p13 == "LEGENDA PT 13                                                                                                                                                                                                                                                  "] <- "PT"
BESSP$Party[ BESSP$p13 == "LOLO                                                                                                                                                                                                                                                           "] <- "PL"
BESSP$Party[ BESSP$p13 == "LUIZ CARLOS SANTOS                                                                                                                                                                                                                                             "] <- "PFL"
BESSP$Party[ BESSP$p13 == "LUIZ EDUARDO                                                                                                                                                                                                                                                   "] <- "PT"
BESSP$Party[ BESSP$p13 == "LUIZA ERONDINA                                                                                                                                                                                                                                                 "] <- "PSB"
BESSP$Party[ BESSP$p13 == "MADEIRA                                                                                                                                                                                                                                                        "] <- "PSDB"
BESSP$Party[ BESSP$p13 == "MARCELO BARBEIRO                                                                                                                                                                                                                                               "] <- "PMDB"
BESSP$Party[ BESSP$p13 == "MARCELO ORTIZ                                                                                                                                                                                                                                                  "] <- "PV"
BESSP$Party[ BESSP$p13 == "MARCOS ABRÃO                                                                                                                                                                                                                                                   "] <- "PFL"
BESSP$Party[ BESSP$p13 == "MAURICIO HAH                                                                                                                                                                                                                                                   "] <- "PDT"
BESSP$Party[ BESSP$p13 == "MEDEIROS                                                                                                                                                                                                                                                       "] <- "PL"
BESSP$Party[ BESSP$p13 == "NELTON LIMA                                                                                                                                                                                                                                                    "] <- "PFL"
BESSP$Party[ BESSP$p13 == "NEUTON LIMA                                                                                                                                                                                                                                                    "] <- "PFL"
BESSP$Party[ BESSP$p13 == "NEWTON LIMA                                                                                                                                                                                                                                                    "] <- "PFL"
BESSP$Party[ BESSP$p13 == "NILTON LIMA                                                                                                                                                                                                                                                    "] <- "PFL"
BESSP$Party[ BESSP$p13 == "NO PARTIDO DO GAROTINHO, Nº 40                                                                                                                                                                                                                                 "] <- "PSB"
BESSP$Party[ BESSP$p13 == "NO PT                                                                                                                                                                                                                                                          "] <- "PT"
BESSP$Party[ BESSP$p13 == "ORLANDO FANTARINI                                                                                                                                                                                                                                              "] <- "PT"
BESSP$Party[ BESSP$p13 == "PAMPA                                                                                                                                                                                                                                                          "] <- "PFL"
BESSP$Party[ BESSP$p13 == "PAMPA                                                                                                                                                                                                                                                          "] <- "PFL"
BESSP$Party[ BESSP$p13 == "PARTIDO DO SERRA 4545                                                                                                                                                                                                                                          "] <- "PSDB"
BESSP$Party[ BESSP$p13 == "PINOTI                                                                                                                                                                                                                                                         "] <- "PMDB"
BESSP$Party[ BESSP$p13 == "PINOTTI                                                                                                                                                                                                                                                        "] <- "PMDB"
BESSP$Party[ BESSP$p13 == "PSDB                                                                                                                                                                                                                                                           "] <- "PSDB"
BESSP$Party[ BESSP$p13 == "PT                                                                                                                                                                                                                                                             "] <- "PT"
BESSP$Party[ BESSP$p13 == "RENE                                                                                                                                                                                                                                                           "] <- "PL"
BESSP$Party[ BESSP$p13 == "RICARDO INGA                                                                                                                                                                                                                                                   "] <- "PTB"
BESSP$Party[ BESSP$p13 == "RICARDO IZAC                                                                                                                                                                                                                                                   "] <- "PTB"
BESSP$Party[ BESSP$p13 == "RICARDO IZAR                                                                                                                                                                                                                                                   "] <- "PTB"
BESSP$Party[ BESSP$p13 == "RICARDO IZZAR                                                                                                                                                                                                                                                  "] <- "PTB"
BESSP$Party[ BESSP$p13 == "ROBERTO GOUVEA                                                                                                                                                                                                                                                 "] <- "PT"
BESSP$Party[ BESSP$p13 == "ROBSON TUMA                                                                                                                                                                                                                                                    "] <- "PFL"
BESSP$Party[ BESSP$p13 == "ROINELSON                                                                                                                                                                                                                                                      "] <- "PV"
BESSP$Party[ BESSP$p13 == "ROY NELSON                                                                                                                                                                                                                                                     "] <- "PV"
BESSP$Party[ BESSP$p13 == "ROY. NELSON                                                                                                                                                                                                                                                    "] <- "PV"
BESSP$Party[ BESSP$p13 == "SARA BERNARDES                                                                                                                                                                                                                                                 "] <- "PTB"
BESSP$Party[ BESSP$p13 == "VALDEMAR COSTA NETO                                                                                                                                                                                                                                            "] <- "PL"
BESSP$Party[ BESSP$p13 == "VALDEMAR COSTA NETO (BAY)                                                                                                                                                                                                                                      "] <- "PL"
BESSP$Party[ BESSP$p13 == "VALSEMAR COSTA NETO (BOY)                                                                                                                                                                                                                                      "] <- "PL"
BESSP$Party[ BESSP$p13 == "VICENTINHO                                                                                                                                                                                                                                                     "] <- "PT"
BESSP$Party[ BESSP$p13 == "VISCENTINHO                                                                                                                                                                                                                                                    "] <- "PT"
BESSP$Party[ BESSP$p13 == "ZULAIE COBRA                                                                                                                                                                                                                                                   "] <- "PSDB"
BESSP$Party[ BESSP$p13 == "ZULOÉ COBRA RIBEIRO                                                                                                                                                                                                                                            "] <- "PSDB"

# Tocantins
BESTO <- ESEB02[ which(ESEB02$uf == "17"),]
TSETO <- TSEFD2002[ which(TSEFD2002$V8 == "TOCANTINS"),]

BESTO$Party <- NA
BESTO$Party[ BESTO$p13 == "EDMUNDO GOLDINO                                                                                                                                                                                                                                                "] <- "PSDB"

## Merging district-level datasets.
ESEB02NEW <- rbind(BESAC, BESAL, BESAM, BESAP, BESBA, BESCE, BESDF, BESES, BESGO, 
                   BESMA, BESMG, BESMS, BESMT, BESPA, BESPB, BESPE, BESPI, BESPR,
                   BESRJ, BESRN, BESRO, BESRR, BESRS, BESSC, BESSE, BESSP, BESTO)

BES2002NATIONAL <- na.omit(ESEB02NEW) #Clean dataset



### Creating the variable Party, based on voting intentions in the 2014 legislative elections
### and also the district-level lists of candidates for the election.
TSE2014 <- consulta_cand_2014_BRASIL #The district-level lists of candidates for the 2014 lower-house election.
TSEFD2014 <- TSE2014[ which(TSE2014$DS_CARGO == "DEPUTADO FEDERAL"),]

write.xlsx(TSEFD2014, file = "Candidacy2014.xlsx",
           sheetName = "Candidacy2014", append = FALSE)

## Assigning party labels, based on on voting intentions at the 2014 lower-house election
# Acre
BESAC14 <- ESEB14[ which(ESEB14$ESTADO == "12"),]
TSEAC14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "ACRE"),]

BESAC14$Party <- NA
BESAC14$Party[ BESAC14$Q5CDA == "20590"] <- "PT"

# Alagoas
BESAL14 <- ESEB14[ which(ESEB14$ESTADO == "27"),]
TSEAL14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "ALAGOAS"),]

BESAL14$Party <- NA
BESAL14$Party[ BESAL14$Q5CDA == "662"] <- "PP"
BESAL14$Party[ BESAL14$Q5CDA == "666"] <- "PMDB"
BESAL14$Party[ BESAL14$Q5CDA == "678"] <- "PMDB"
BESAL14$Party[ BESAL14$Q5CDA == "682"] <- "PROS"
BESAL14$Party[ BESAL14$Q5CDA == "693"] <- "PSD"
BESAL14$Party[ BESAL14$Q5CDA == "698"] <- "PRTB"
BESAL14$Party[ BESAL14$Q5CDA == "740"] <- "PT"
BESAL14$Party[ BESAL14$Q5CDA == "751"] <- "PDT"
BESAL14$Party[ BESAL14$Q5CDA == "20687"] <- "PSDB"
BESAL14$Party[ BESAL14$Q5CDA == "20705"] <- "PTB"
BESAL14$Party[ BESAL14$Q5CDA == "20724"] <- "PSD"
BESAL14$Party[ BESAL14$Q5CDA == "20932"] <- "PPS"
BESAL14$Party[ BESAL14$Q5CDA == "20944"] <- "PSC"

# Amazonas
BESAM14 <- ESEB14[ which(ESEB14$ESTADO == "13"),]
TSEAM14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "AMAZONAS"),]

BESAM14$Party <- NA
BESAM14$Party[ BESAM14$Q5CDA == "1402"] <- "PR"
BESAM14$Party[ BESAM14$Q5CDA == "1413"] <- "PSD"
BESAM14$Party[ BESAM14$Q5CDA == "1420"] <- "PT"
BESAM14$Party[ BESAM14$Q5CDA == "1427"] <- "PC do B"
BESAM14$Party[ BESAM14$Q5CDA == "1438"] <- "PT"
BESAM14$Party[ BESAM14$Q5CDA == "1453"] <- "PP"
BESAM14$Party[ BESAM14$Q5CDA == "21763"] <- "PSDC"

# Amapá
BESAP14 <- ESEB14[ which(ESEB14$ESTADO == "16"),]
TSEAP14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "AMAPÁ"),]

BESAP14$Party <- NA
BESAP14$Party[ BESAP14$Q5CDA == "1017"] <- "PDT"
BESAP14$Party[ BESAP14$Q5CDA == "1056"] <- "PSB"
BESAP14$Party[ BESAP14$Q5CDA == "1084"] <- "PT"
BESAP14$Party[ BESAP14$Q5CDA == "21177"] <- "PP"
BESAP14$Party[ BESAP14$Q5CDA == "21358"] <- "PRB"

# Bahia
BESBA14 <- ESEB14[ which(ESEB14$ESTADO == "29"),]
TSEBA14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "BAHIA"),]

BESBA14$Party <- NA
BESBA14$Party[ BESBA14$Q5CDA == "13"] <- "PT"
BESBA14$Party[ BESBA14$Q5CDA == "2052"] <- "PSB"
BESBA14$Party[ BESBA14$Q5CDA == "2088"] <- "PTB"
BESBA14$Party[ BESBA14$Q5CDA == "2093"] <- "PSB"
BESBA14$Party[ BESBA14$Q5CDA == "2110"] <- "DEM"
BESBA14$Party[ BESBA14$Q5CDA == "2153"] <- "PT"
BESBA14$Party[ BESBA14$Q5CDA == "2217"] <- "PSDB"
BESBA14$Party[ BESBA14$Q5CDA == "2240"] <- "DEM"
BESBA14$Party[ BESBA14$Q5CDA == "2254"] <- "PSDB"
BESBA14$Party[ BESBA14$Q5CDA == "2272"] <- "PMDB"
BESBA14$Party[ BESBA14$Q5CDA == "2298"] <- "PSD"
BESBA14$Party[ BESBA14$Q5CDA == "2341"] <- "PT"
BESBA14$Party[ BESBA14$Q5CDA == "2357"] <- "DEM"
BESBA14$Party[ BESBA14$Q5CDA == "2379"] <- "PT"
BESBA14$Party[ BESBA14$Q5CDA == "2382"] <- "PP"
BESBA14$Party[ BESBA14$Q5CDA == "2394"] <- "PSD"
BESBA14$Party[ BESBA14$Q5CDA == "2425"] <- "PT"
BESBA14$Party[ BESBA14$Q5CDA == "2427"] <- "PT"
BESBA14$Party[ BESBA14$Q5CDA == "15036"] <- "PR"
BESBA14$Party[ BESBA14$Q5CDA == "22059"] <- "PSDB"
BESBA14$Party[ BESBA14$Q5CDA == "22341"] <- "PV"
BESBA14$Party[ BESBA14$Q5CDA == "22401"] <- "PT"
BESBA14$Party[ BESBA14$Q5CDA == "22406"] <- "PT"
BESBA14$Party[ BESBA14$Q5CDA == "22464"] <- "PMDB"
BESBA14$Party[ BESBA14$Q5CDA == "22507"] <- "PSDB"
BESBA14$Party[ BESBA14$Q5CDA == "22575"] <- "PSL"
BESBA14$Party[ BESBA14$Q5CDA == "22694"] <- "PV"

# Ceará
BESCE14 <- ESEB14[ which(ESEB14$ESTADO == "23"),]
TSECE14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "CEARÁ"),]

BESCE14$Party <- NA
BESCE14$Party[ BESCE14$Q5CDA == "50"] <- "PSOL"
BESCE14$Party[ BESCE14$Q5CDA == "2762"] <- "PDT"
BESCE14$Party[ BESCE14$Q5CDA == "2771"] <- "PT"
BESCE14$Party[ BESCE14$Q5CDA == "2778"] <- "PMDB"
BESCE14$Party[ BESCE14$Q5CDA == "2829"] <- "PV"
BESCE14$Party[ BESCE14$Q5CDA == "2831"] <- "PC do B"
BESCE14$Party[ BESCE14$Q5CDA == "2837"] <- "PSD"
BESCE14$Party[ BESCE14$Q5CDA == "2868"] <- "PROS"
BESCE14$Party[ BESCE14$Q5CDA == "2874"] <- "PT"
BESCE14$Party[ BESCE14$Q5CDA == "2890"] <- "PT"
BESCE14$Party[ BESCE14$Q5CDA == "2908"] <- "PR"
BESCE14$Party[ BESCE14$Q5CDA == "2917"] <- "PMDB"
BESCE14$Party[ BESCE14$Q5CDA == "2920"] <- "DEM"
BESCE14$Party[ BESCE14$Q5CDA == "2921"] <- "PPS"
BESCE14$Party[ BESCE14$Q5CDA == "2932"] <- "PT do B"
BESCE14$Party[ BESCE14$Q5CDA == "2933"] <- "PP"
BESCE14$Party[ BESCE14$Q5CDA == "22892"] <- "PSD"
BESCE14$Party[ BESCE14$Q5CDA == "23133"] <- "PROS"
BESCE14$Party[ BESCE14$Q5CDA == "23279"] <- "PSL"
BESCE14$Party[ BESCE14$Q5CDA == "23379"] <- "PR"

# Distrito Federal
BESDF14 <- ESEB14[ which(ESEB14$ESTADO == "53"),]
TSEDF14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "DISTRITO FEDERAL"),]

BESDF14$Party <- NA
BESDF14$Party[ BESDF14$Q5CDA == "3023"] <- "PPS"
BESDF14$Party[ BESDF14$Q5CDA == "3028"] <- "PT"
BESDF14$Party[ BESDF14$Q5CDA == "3047"] <- "PSDB"
BESDF14$Party[ BESDF14$Q5CDA == "3053"] <- "DEM"
BESDF14$Party[ BESDF14$Q5CDA == "3124"] <- "PMDB"
BESDF14$Party[ BESDF14$Q5CDA == "3126"] <- "PMDB"
BESDF14$Party[ BESDF14$Q5CDA == "3133"] <- "PP"

# Espírito Santo
BESES14 <- ESEB14[ which(ESEB14$ESTADO == "32"),]
TSEES14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "ESPÍRITO SANTO"),]

BESES14$Party <- NA
BESES14$Party[ BESES14$Q5CDA == "4075"] <- "PMDB"
BESES14$Party[ BESES14$Q5CDA == "4212"] <- "PSB"
BESES14$Party[ BESES14$Q5CDA == "24746"] <- "PRP"
BESES14$Party[ BESES14$Q5CDA == "24774"] <- "PHS"
BESES14$Party[ BESES14$Q5CDA == "25037"] <- "DEM"

# Goiás
BESGO14 <- ESEB14[ which(ESEB14$ESTADO == "52"),]
TSEGO14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "GOIÁS"),]

BESGO14$Party <- NA
BESGO14$Party[ BESGO14$Q5CDA == "4728"] <- "PMDB"
BESGO14$Party[ BESGO14$Q5CDA == "4757"] <- "PDT"
BESGO14$Party[ BESGO14$Q5CDA == "4758"] <- "PMDB"
BESGO14$Party[ BESGO14$Q5CDA == "4759"] <- "PSDB"
BESGO14$Party[ BESGO14$Q5CDA == "4818"] <- "PPS"
BESGO14$Party[ BESGO14$Q5CDA == "4838"] <- "PT"
BESGO14$Party[ BESGO14$Q5CDA == "4860"] <- "PSD"
BESGO14$Party[ BESGO14$Q5CDA == "25894"] <- "PHS"
BESGO14$Party[ BESGO14$Q5CDA == "4728"] <- "PMDB"

# Maranhão
BESMA14 <- ESEB14[ which(ESEB14$ESTADO == "21"),]
TSEMA14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "MARANHÃO"),]

BESMA14$Party <- NA
BESMA14$Party[ BESMA14$Q5CDA == "5647"] <- "PRB"
BESMA14$Party[ BESMA14$Q5CDA == "5665"] <- "PPS"
BESMA14$Party[ BESMA14$Q5CDA == "5720"] <- "PMDB"
BESMA14$Party[ BESMA14$Q5CDA == "5725"] <- "PT"
BESMA14$Party[ BESMA14$Q5CDA == "5748"] <- "PV"
BESMA14$Party[ BESMA14$Q5CDA == "5753"] <- "PDT"
BESMA14$Party[ BESMA14$Q5CDA == "5849"] <- "PC do B"
BESMA14$Party[ BESMA14$Q5CDA == "5871"] <- "pdt"
BESMA14$Party[ BESMA14$Q5CDA == "26148"] <- "PSL"
BESMA14$Party[ BESMA14$Q5CDA == "26181"] <- "PSDC"
BESMA14$Party[ BESMA14$Q5CDA == "26289"] <- "PV"
BESMA14$Party[ BESMA14$Q5CDA == "26453"] <- "PMDB"
BESMA14$Party[ BESMA14$Q5CDA == "26491"] <- "PTC"
BESMA14$Party[ BESMA14$Q5CDA == "26569"] <- "PT"

# Minas Gerais
BESMG14 <- ESEB14[ which(ESEB14$ESTADO == "31"),]
TSEMG14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "MINAS GERAIS"),]

BESMG14$Party <- NA
BESMG14$Party[ BESMG14$Q5CDA == "13"] <- "PT"
BESMG14$Party[ BESMG14$Q5CDA == "45"] <- "PSDB"
BESMG14$Party[ BESMG14$Q5CDA == "7107"] <- "PROS"
BESMG14$Party[ BESMG14$Q5CDA == "7118"] <- "PR"
BESMG14$Party[ BESMG14$Q5CDA == "7195"] <- "PSDB"
BESMG14$Party[ BESMG14$Q5CDA == "7244"] <- "PMN"
BESMG14$Party[ BESMG14$Q5CDA == "7262"] <- "PSDB"
BESMG14$Party[ BESMG14$Q5CDA == "7298"] <- "PTB"
BESMG14$Party[ BESMG14$Q5CDA == "7315"] <- "PV"
BESMG14$Party[ BESMG14$Q5CDA == "7334"] <- "PRB"
BESMG14$Party[ BESMG14$Q5CDA == "7343"] <- "PSD"
BESMG14$Party[ BESMG14$Q5CDA == "7347"] <- "PSC"
BESMG14$Party[ BESMG14$Q5CDA == "7389"] <- "PSD"
BESMG14$Party[ BESMG14$Q5CDA == "7452"] <- "PSD"
BESMG14$Party[ BESMG14$Q5CDA == "7469"] <- "PMDB"
BESMG14$Party[ BESMG14$Q5CDA == "7474"] <- "PMDB"
BESMG14$Party[ BESMG14$Q5CDA == "7497"] <- "PT do B"
BESMG14$Party[ BESMG14$Q5CDA == "7503"] <- "PP"
BESMG14$Party[ BESMG14$Q5CDA == "7521"] <- "PHS"
BESMG14$Party[ BESMG14$Q5CDA == "7522"] <- "PRP"
BESMG14$Party[ BESMG14$Q5CDA == "7544"] <- "PSDB"
BESMG14$Party[ BESMG14$Q5CDA == "7563"] <- "PC do B"
BESMG14$Party[ BESMG14$Q5CDA == "7586"] <- "PSOL"
BESMG14$Party[ BESMG14$Q5CDA == "7596"] <- "PT"
BESMG14$Party[ BESMG14$Q5CDA == "7599"] <- "DEM"
BESMG14$Party[ BESMG14$Q5CDA == "7612"] <- "PT"
BESMG14$Party[ BESMG14$Q5CDA == "7621"] <- "PT"
BESMG14$Party[ BESMG14$Q5CDA == "7622"] <- "PP"
BESMG14$Party[ BESMG14$Q5CDA == "7632"] <- "PT"
BESMG14$Party[ BESMG14$Q5CDA == "7663"] <- "PT"
BESMG14$Party[ BESMG14$Q5CDA == "7686"] <- "PSDB"
BESMG14$Party[ BESMG14$Q5CDA == "7786"] <- "PT"
BESMG14$Party[ BESMG14$Q5CDA == "28003"] <- "PV"
BESMG14$Party[ BESMG14$Q5CDA == "28024"] <- "PTB"
BESMG14$Party[ BESMG14$Q5CDA == "28319"] <- "PP"
BESMG14$Party[ BESMG14$Q5CDA == "28573"] <- "PT do B"
BESMG14$Party[ BESMG14$Q5CDA == "28971"] <- "PSB"

# Mato Grosso do Sul
BESMS14 <- ESEB14[ which(ESEB14$ESTADO == "50"),]
TSEMS14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "MATO GROSSO DO SUL"),]

BESMS14$Party <- NA
BESMS14$Party[ BESMS14$Q5CDA == "6654"] <- "PMDB"
BESMS14$Party[ BESMS14$Q5CDA == "6663"] <- "PT"
BESMS14$Party[ BESMS14$Q5CDA == "6717"] <- "PSB"
BESMS14$Party[ BESMS14$Q5CDA == "27585"] <- "PT"
BESMS14$Party[ BESMS14$Q5CDA == "27621"] <- "PSC"
BESMS14$Party[ BESMS14$Q5CDA == "27669"] <- "PSDB"

# Mato Grosso
BESMT14 <- ESEB14[ which(ESEB14$ESTADO == "51"),]
TSEMT14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "MATO GROSSO"),]

BESMT14$Party <- NA
BESMT14$Party[ BESMT14$Q5CDA == "6216"] "PMDB"
BESMT14$Party[ BESMT14$Q5CDA == "6235"] "PSB"
BESMT14$Party[ BESMT14$Q5CDA == "6275"] "PSOL"
BESMT14$Party[ BESMT14$Q5CDA == "6308"] "PROS"
BESMT14$Party[ BESMT14$Q5CDA == "6310"] "PSOL"

# Pará
BESPA14 <- ESEB14[ which(ESEB14$ESTADO == "15"),]
TSEPA14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "PARÁ"),]

BESPA14$Party <- NA
BESPA14$Party[ BESPA14$Q5CDA == "8355"] <- "PV"
BESPA14$Party[ BESPA14$Q5CDA == "8360"] <- "PDT"
BESPA14$Party[ BESPA14$Q5CDA == "8372"] <- "PSB"
BESPA14$Party[ BESPA14$Q5CDA == "8375"] <- "PT"
BESPA14$Party[ BESPA14$Q5CDA == "8394"] <- "PSD"
BESPA14$Party[ BESPA14$Q5CDA == "8397"] <- "PSOL"
BESPA14$Party[ BESPA14$Q5CDA == "8405"] <- "PT"
BESPA14$Party[ BESPA14$Q5CDA == "8442"] <- "PMDB"
BESPA14$Party[ BESPA14$Q5CDA == "8457"] <- "PT"
BESPA14$Party[ BESPA14$Q5CDA == "8465"] <- "PSD"
BESPA14$Party[ BESPA14$Q5CDA == "8469"] <- "PTB"
BESPA14$Party[ BESPA14$Q5CDA == "8523"] <- "PSDB"
BESPA14$Party[ BESPA14$Q5CDA == "8525"] <- "PSDB"
BESPA14$Party[ BESPA14$Q5CDA == "8543"] <- "PROS"
BESPA14$Party[ BESPA14$Q5CDA == "8555"] <- "PMDB"
BESPA14$Party[ BESPA14$Q5CDA == "29370"] <- "PMDB"
BESPA14$Party[ BESPA14$Q5CDA == "29546"] <- "PMDB"
BESPA14$Party[ BESPA14$Q5CDA == "29630"] <- "PMDB"
BESPA14$Party[ BESPA14$Q5CDA == "29907"] <- "PTB"

# Paraíba
BESPB14 <- ESEB14[ which(ESEB14$ESTADO == "25"),]
TSEPB14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "PARAÍBA"),]

BESPB14$Party <- NA
BESPB14$Party[ BESPB14$Q5CDA == "9201"] <- "PP"
BESPB14$Party[ BESPB14$Q5CDA == "9276"] <- "PT"
BESPB14$Party[ BESPB14$Q5CDA == "9294"] <- "PDT"
BESPB14$Party[ BESPB14$Q5CDA == "9295"] <- "PSDB"
BESPB14$Party[ BESPB14$Q5CDA == "9303"] <- "PSD"
BESPB14$Party[ BESPB14$Q5CDA == "9312"] <- "PMDB"
BESPB14$Party[ BESPB14$Q5CDA == "30058"] <- "PSDB"
BESPB14$Party[ BESPB14$Q5CDA == "30120"] <- "PSB"
BESPB14$Party[ BESPB14$Q5CDA == "30260"] <- "PSD"
BESPB14$Party[ BESPB14$Q5CDA == "30377"] <- "PMDB"

# Pernambuco
BESPE14 <- ESEB14[ which(ESEB14$ESTADO == "26"),]
TSEPE14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "PERNAMBUCO"),]

BESPE14$Party <- NA
BESPE14$Party[ BESPE14$Q5CDA == "10322"] <- "PSDB"
BESPE14$Party[ BESPE14$Q5CDA == "10334"] <- "PSDB"
BESPE14$Party[ BESPE14$Q5CDA == "10335"] <- "PSB"
BESPE14$Party[ BESPE14$Q5CDA == "10343"] <- "PP"
BESPE14$Party[ BESPE14$Q5CDA == "10353"] <- "PSB"
BESPE14$Party[ BESPE14$Q5CDA == "10354"] <- "PSB"
BESPE14$Party[ BESPE14$Q5CDA == "10359"] <- "PSB"
BESPE14$Party[ BESPE14$Q5CDA == "10360"] <- "PSB"
BESPE14$Party[ BESPE14$Q5CDA == "10366"] <- "PSDB"
BESPE14$Party[ BESPE14$Q5CDA == "10382"] <- "PT"
BESPE14$Party[ BESPE14$Q5CDA == "10408"] <- "PC do B"
BESPE14$Party[ BESPE14$Q5CDA == "10460"] <- "PSB"
BESPE14$Party[ BESPE14$Q5CDA == "31459"] <- "PMDB"
BESPE14$Party[ BESPE14$Q5CDA == "31502"] <- "PP"
BESPE14$Party[ BESPE14$Q5CDA == "31604"] <- "PDT"
BESPE14$Party[ BESPE14$Q5CDA == "31609"] <- "PR"
BESPE14$Party[ BESPE14$Q5CDA == "31666"] <- "PV"
BESPE14$Party[ BESPE14$Q5CDA == "31824"] <- "PRB"

# Piauí
BESPI14 <- ESEB14[ which(ESEB14$ESTADO == "22"),]
TSEPI14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "PIAUÍ"),]

BESPI14$Party <- NA
BESPI14$Party[ BESPI14$Q5CDA == "10937"] <- "PTB"
BESPI14$Party[ BESPI14$Q5CDA == "10945"] <- "PT"
BESPI14$Party[ BESPI14$Q5CDA == "10957"] <- "PP"
BESPI14$Party[ BESPI14$Q5CDA == "10978"] <- "PSD"
BESPI14$Party[ BESPI14$Q5CDA == "10990"] <- "PMDB"
BESPI14$Party[ BESPI14$Q5CDA == "10995"] <- "PC do B"
BESPI14$Party[ BESPI14$Q5CDA == "11002"] <- "PSB"
BESPI14$Party[ BESPI14$Q5CDA == "11007"] <- "PR"
BESPI14$Party[ BESPI14$Q5CDA == "32028"] <- "PTB"
BESPI14$Party[ BESPI14$Q5CDA == "32086"] <- "PSD"
BESPI14$Party[ BESPI14$Q5CDA == "32126"] <- "PTB"
BESPI14$Party[ BESPI14$Q5CDA == "32240"] <- "PSB"
BESPI14$Party[ BESPI14$Q5CDA == "32244"] <- "PMDB"

# Paraná
BESPR14 <- ESEB14[ which(ESEB14$ESTADO == "41"),]
TSEPR14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "PARANÁ"),]

BESPR14$Party <- NA
BESPR14$Party[ BESPR14$Q5CDA == "13"] <- "PT"
BESPR14$Party[ BESPR14$Q5CDA == "9409"] <- "PTB"
BESPR14$Party[ BESPR14$Q5CDA == "9425"] <- "PMDB"
BESPR14$Party[ BESPR14$Q5CDA == "9439"] <- "PMN"
BESPR14$Party[ BESPR14$Q5CDA == "9460"] <- "PV"
BESPR14$Party[ BESPR14$Q5CDA == "9479"] <- "PSC"
BESPR14$Party[ BESPR14$Q5CDA == "9508"] <- "PSD"
BESPR14$Party[ BESPR14$Q5CDA == "9509"] <- "PR"
BESPR14$Party[ BESPR14$Q5CDA == "9527"] <- "PSC"
BESPR14$Party[ BESPR14$Q5CDA == "9539"] <- "PTB"
BESPR14$Party[ BESPR14$Q5CDA == "9554"] <- "PMDB"
BESPR14$Party[ BESPR14$Q5CDA == "9567"] <- "PT"
BESPR14$Party[ BESPR14$Q5CDA == "9587"] <- "PSB"
BESPR14$Party[ BESPR14$Q5CDA == "9588"] <- "PSB"
BESPR14$Party[ BESPR14$Q5CDA == "9591"] <- "PSDB"
BESPR14$Party[ BESPR14$Q5CDA == "8640"] <- "PP"
BESPR14$Party[ BESPR14$Q5CDA == "9647"] <- "PRB"
BESPR14$Party[ BESPR14$Q5CDA == "9651"] <- "DEM"
BESPR14$Party[ BESPR14$Q5CDA == "9679"] <- "PSD"
BESPR14$Party[ BESPR14$Q5CDA == "9680"] <- "PSD"
BESPR14$Party[ BESPR14$Q5CDA == "9681"] <- "PP"
BESPR14$Party[ BESPR14$Q5CDA == "9688"] <- "PT"
BESPR14$Party[ BESPR14$Q5CDA == "9695"] <- "PPS"
BESPR14$Party[ BESPR14$Q5CDA == "9699"] <- "PPS"
BESPR14$Party[ BESPR14$Q5CDA == "9723"] <- "PT"
BESPR14$Party[ BESPR14$Q5CDA == "9729"] <- "PSDB"
BESPR14$Party[ BESPR14$Q5CDA == "30508"] <- "PSDB"
BESPR14$Party[ BESPR14$Q5CDA == "30590"] <- "PSC"
BESPR14$Party[ BESPR14$Q5CDA == "30644"] <- "PSC"
BESPR14$Party[ BESPR14$Q5CDA == "30923"] <- "PSB"
BESPR14$Party[ BESPR14$Q5CDA == "31170"] <- "PSDB"
BESPR14$Party[ BESPR14$Q5CDA == "31184"] <- "DEM"
BESPR14$Party[ BESPR14$Q5CDA == "31327"] <- "PSC"
BESPR14$Party[ BESPR14$Q5CDA == "31339"] <- "PTC"
BESPR14$Party[ BESPR14$Q5CDA == "31342"] <- "PSB"

# Rio de Janeiro
BESRJ14 <- ESEB14[ which(ESEB14$ESTADO == "33"),]
TSERJ14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "RIO DE JANEIRO"),]

BESRJ14$Party <- NA
BESRJ14$Party[ BESRJ14$Q5CDA == "11340"] <- "PSD"
BESRJ14$Party[ BESRJ14$Q5CDA == "11404"] <- "PMDB"
BESRJ14$Party[ BESRJ14$Q5CDA == "11416"] <- "PR"
BESRJ14$Party[ BESRJ14$Q5CDA == "11450"] <- "PSOL"
BESRJ14$Party[ BESRJ14$Q5CDA == "11456"] <- "PR"
BESRJ14$Party[ BESRJ14$Q5CDA == "11502"] <- "PMDB"
BESRJ14$Party[ BESRJ14$Q5CDA == "11565"] <- "PSD"
BESRJ14$Party[ BESRJ14$Q5CDA == "11572"] <- "PSD"
BESRJ14$Party[ BESRJ14$Q5CDA == "11591"] <- "PROS"
BESRJ14$Party[ BESRJ14$Q5CDA == "11597"] <- "PSOL"
BESRJ14$Party[ BESRJ14$Q5CDA == "11663"] <- "PP"
BESRJ14$Party[ BESRJ14$Q5CDA == "11776"] <- "PP"
BESRJ14$Party[ BESRJ14$Q5CDA == "11794"] <- "PMDB"
BESRJ14$Party[ BESRJ14$Q5CDA == "11906"] <- "PT"
BESRJ14$Party[ BESRJ14$Q5CDA == "11909"] <- "PDT"
BESRJ14$Party[ BESRJ14$Q5CDA == "12044"] <- "PMDB"
BESRJ14$Party[ BESRJ14$Q5CDA == "12067"] <- "PR"
BESRJ14$Party[ BESRJ14$Q5CDA == "12106"] <- "PDT"
BESRJ14$Party[ BESRJ14$Q5CDA == "12257"] <- "PR"
BESRJ14$Party[ BESRJ14$Q5CDA == "12260"] <- "PTB"
BESRJ14$Party[ BESRJ14$Q5CDA == "12274"] <- "PMDB"
BESRJ14$Party[ BESRJ14$Q5CDA == "32642"] <- "PSOL"
BESRJ14$Party[ BESRJ14$Q5CDA == "32830"] <- "PT"
BESRJ14$Party[ BESRJ14$Q5CDA == "32942"] <- "PT do B"
BESRJ14$Party[ BESRJ14$Q5CDA == "33013"] <- "PMDB"
BESRJ14$Party[ BESRJ14$Q5CDA == "33050"] <- "PR"
BESRJ14$Party[ BESRJ14$Q5CDA == "33424"] <- "PMDB"
BESRJ14$Party[ BESRJ14$Q5CDA == "34208"] <- "PRB"
BESRJ14$Party[ BESRJ14$Q5CDA == "34487"] <- "PSD"
BESRJ14$Party[ BESRJ14$Q5CDA == "34508"] <- "PTB"

# Rio Grande do Norte
BESRN14 <- ESEB14[ which(ESEB14$ESTADO == "24"),]
TSERN14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "RIO GRANDE DO NORTE"),]

BESRN14$Party <- NA
BESRN14$Party[ BESRN14$Q5CDA == "13309"] <- "PP"
BESRN14$Party[ BESRN14$Q5CDA == "13362"] <- "PMDB"
BESRN14$Party[ BESRN14$Q5CDA == "13372"] <- "PROS"
BESRN14$Party[ BESRN14$Q5CDA == "13380"] <- "PSDB"
BESRN14$Party[ BESRN14$Q5CDA == "13386"] <- "PSB"
BESRN14$Party[ BESRN14$Q5CDA == "13390"] <- "PSL"
BESRN14$Party[ BESRN14$Q5CDA == "13399"] <- "PR"
BESRN14$Party[ BESRN14$Q5CDA == "34705"] <- "PMDB"

# Rondônia
BESRO14 <- ESEB14[ which(ESEB14$ESTADO == "11"),]
TSERO14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "RONDÔNIA"),]

BESRO14$Party <- NA
#Here, unfortunately, there existed no valid responses on voting intentions.

# Roraima
BESRR14 <- ESEB14[ which(ESEB14$ESTADO == "14"),]
TSERR14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "RORAIMA"),]

BESRR14$Party <- NA
BESRR14$Party[ BESRR14$Q5CDA == "14124"] <- "PSDB"
BESRR14$Party[ BESRR14$Q5CDA == "14167"] <- "PRTB"
BESRR14$Party[ BESRR14$Q5CDA == "14171"] <- "PR"
BESRR14$Party[ BESRR14$Q5CDA == "14177"] <- "PSDB"

# Rio Grande do Sul
BESRS14 <- ESEB14[ which(ESEB14$ESTADO == "43"),]
TSERS14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "RIO GRANDE DO SUL"),]

BESRS14$Party <- NA
BESRS14$Party[ BESRS14$Q5CDA == "11"] <- "PP"
BESRS14$Party[ BESRS14$Q5CDA == "12"] <- "PDT"
BESRS14$Party[ BESRS14$Q5CDA == "13632"] <- "PTB"
BESRS14$Party[ BESRS14$Q5CDA == "13676"] <- "PSD"
BESRS14$Party[ BESRS14$Q5CDA == "13678"] <- "PMDB"
BESRS14$Party[ BESRS14$Q5CDA == "13699"] <- "PT"
BESRS14$Party[ BESRS14$Q5CDA == "13707"] <- "PT"
BESRS14$Party[ BESRS14$Q5CDA == "13710"] <- "PMDB"
BESRS14$Party[ BESRS14$Q5CDA == "13718"] <- "PT"
BESRS14$Party[ BESRS14$Q5CDA == "13733"] <- "PP"
BESRS14$Party[ BESRS14$Q5CDA == "13750"] <- "PSB"
BESRS14$Party[ BESRS14$Q5CDA == "13756"] <- "PP"
BESRS14$Party[ BESRS14$Q5CDA == "13792"] <- "PP"
BESRS14$Party[ BESRS14$Q5CDA == "13795"] <- "PTB"
BESRS14$Party[ BESRS14$Q5CDA == "13804"] <- "PMDB"
BESRS14$Party[ BESRS14$Q5CDA == "13807"] <- "PT"
BESRS14$Party[ BESRS14$Q5CDA == "13846"] <- "DEM"
BESRS14$Party[ BESRS14$Q5CDA == "13926"] <- "PSDB"
BESRS14$Party[ BESRS14$Q5CDA == "34906"] <- "PTB"
BESRS14$Party[ BESRS14$Q5CDA == "35109"] <- "PC do B"
BESRS14$Party[ BESRS14$Q5CDA == "35407"] <- "DEM"

# Santa Catarina
BESSC14 <- ESEB14[ which(ESEB14$ESTADO == "42"),]
TSESC14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "SANTA CATARINA"),]

BESSC14$Party <- NA
BESSC14$Party[ BESSC14$Q5CDA == "14222"] <- "PMDB"
BESSC14$Party[ BESSC14$Q5CDA == "14236"] <- "PMDB"
BESSC14$Party[ BESSC14$Q5CDA == "14248"] <- "PSDB"
BESSC14$Party[ BESSC14$Q5CDA == "14259"] <- "PT"
BESSC14$Party[ BESSC14$Q5CDA == "14269"] <- "PSD"
BESSC14$Party[ BESSC14$Q5CDA == "14271"] <- "PP"
BESSC14$Party[ BESSC14$Q5CDA == "14273"] <- "PR"
BESSC14$Party[ BESSC14$Q5CDA == "14279"] <- "PT"
BESSC14$Party[ BESSC14$Q5CDA == "14309"] <- "PMDB"
BESSC14$Party[ BESSC14$Q5CDA == "14312"] <- "PT do B"
BESSC14$Party[ BESSC14$Q5CDA == "14327"] <- "PMDB"
BESSC14$Party[ BESSC14$Q5CDA == "36856"] <- "PMDB"
BESSC14$Party[ BESSC14$Q5CDA == "37020"] <- "PSDB"
BESSC14$Party[ BESSC14$Q5CDA == "37184"] <- "PP"
BESSC14$Party[ BESSC14$Q5CDA == "37185"] <- "PT"

# Sergipe
BESSE14 <- ESEB14[ which(ESEB14$ESTADO == "28"),]
TSESE14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "SERGIPE"),]

BESSE14$Party <- NA
BESSE14$Party[ BESSE14$Q5CDA == "16000"] <- "PTB"
BESSE14$Party[ BESSE14$Q5CDA == "16006"] <- "PSB"

# São Paulo
BESSP14 <- ESEB14[ which(ESEB14$ESTADO == "35"),]
TSESP14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "SÃO PAULO"),]

BESSP14$Party <- NA
BESSP14$Party[ BESSP14$Q5CDA == "13"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "15"] <- "PMDB"
BESSP14$Party[ BESSP14$Q5CDA == "29"] <- "PCO"
BESSP14$Party[ BESSP14$Q5CDA == "45"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "50"] <- "PSOL"
BESSP14$Party[ BESSP14$Q5CDA == "14529"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "14530"] <- "DEM"
BESSP14$Party[ BESSP14$Q5CDA == "14561"] <- "PPS"
BESSP14$Party[ BESSP14$Q5CDA == "14570"] <- "DEM"
BESSP14$Party[ BESSP14$Q5CDA == "14596"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "14602"] <- "PSB"
BESSP14$Party[ BESSP14$Q5CDA == "14609"] <- "PR"
BESSP14$Party[ BESSP14$Q5CDA == "14628"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "14655"] <- "PRB"
BESSP14$Party[ BESSP14$Q5CDA == "14689"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "14692"] <- "PTB"
BESSP14$Party[ BESSP14$Q5CDA == "14703"] <- "PRP"
BESSP14$Party[ BESSP14$Q5CDA == "14710"] <- "PRB"
BESSP14$Party[ BESSP14$Q5CDA == "14716"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "14718"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "14733"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "14746"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "14766"] <- "PRB"
BESSP14$Party[ BESSP14$Q5CDA == "14803"] <- "PRB"
BESSP14$Party[ BESSP14$Q5CDA == "14874"] <- "PPS"
BESSP14$Party[ BESSP14$Q5CDA == "14899"] <- "PMDB"
BESSP14$Party[ BESSP14$Q5CDA == "14911"] <- "PSC"
BESSP14$Party[ BESSP14$Q5CDA == "14914"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "14944"] <- "PMDB"
BESSP14$Party[ BESSP14$Q5CDA == "14947"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "14955"] <- "PSC"
BESSP14$Party[ BESSP14$Q5CDA == "15036"] <- "PR"
BESSP14$Party[ BESSP14$Q5CDA == "15078"] <- "PROS"
BESSP14$Party[ BESSP14$Q5CDA == "15128"] <- "PSOL"
BESSP14$Party[ BESSP14$Q5CDA == "15141"] <- "PSL"
BESSP14$Party[ BESSP14$Q5CDA == "15152"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "15216"] <- "PSB"
BESSP14$Party[ BESSP14$Q5CDA == "15221"] <- "PROS"
BESSP14$Party[ BESSP14$Q5CDA == "15229"] <- "PSB"
BESSP14$Party[ BESSP14$Q5CDA == "15241"] <- "PMDB"
BESSP14$Party[ BESSP14$Q5CDA == "15250"] <- "PC do B"
BESSP14$Party[ BESSP14$Q5CDA == "15278"] <- "PP"
BESSP14$Party[ BESSP14$Q5CDA == "15284"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "15293"] <- "PSOL"
BESSP14$Party[ BESSP14$Q5CDA == "15320"] <- "PSD"
BESSP14$Party[ BESSP14$Q5CDA == "15381"] <- "PMDB"
BESSP14$Party[ BESSP14$Q5CDA == "15389"] <- "PMDB"
BESSP14$Party[ BESSP14$Q5CDA == "15394"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "15398"] <- "PSB"
BESSP14$Party[ BESSP14$Q5CDA == "15443"] <- "PR"
BESSP14$Party[ BESSP14$Q5CDA == "15446"] <- "PV"
BESSP14$Party[ BESSP14$Q5CDA == "15451"] <- "PSC"
BESSP14$Party[ BESSP14$Q5CDA == "15455"] <- "PSB"
BESSP14$Party[ BESSP14$Q5CDA == "15474"] <- "PRB"
BESSP14$Party[ BESSP14$Q5CDA == "15561"] <- "PR"
BESSP14$Party[ BESSP14$Q5CDA == "15652"] <- "PSD"
BESSP14$Party[ BESSP14$Q5CDA == "15654"] <- "PR"
BESSP14$Party[ BESSP14$Q5CDA == "15655"] <- "PRB"
BESSP14$Party[ BESSP14$Q5CDA == "15658"] <- "PP"
BESSP14$Party[ BESSP14$Q5CDA == "15666"] <- "PSDC"
BESSP14$Party[ BESSP14$Q5CDA == "15692"] <- "PSD"
BESSP14$Party[ BESSP14$Q5CDA == "15725"] <- "PV"
BESSP14$Party[ BESSP14$Q5CDA == "15767"] <- "PRB"
BESSP14$Party[ BESSP14$Q5CDA == "15769"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "15811"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "15833"] <- "PRB"
BESSP14$Party[ BESSP14$Q5CDA == "15838"] <- "PDT"
BESSP14$Party[ BESSP14$Q5CDA == "15867"] <- "PV"
BESSP14$Party[ BESSP14$Q5CDA == "15920"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "15935"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "37259"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "37319"] <- "PTB"
BESSP14$Party[ BESSP14$Q5CDA == "37344"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "37374"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "37387"] <- "PP"
BESSP14$Party[ BESSP14$Q5CDA == "37395"] <- "PTB"
BESSP14$Party[ BESSP14$Q5CDA == "37452"] <- "PSTU"
BESSP14$Party[ BESSP14$Q5CDA == "37717"] <- "PC do B"
BESSP14$Party[ BESSP14$Q5CDA == "37748"] <- "DEM"
BESSP14$Party[ BESSP14$Q5CDA == "37889"] <- "PSL"
BESSP14$Party[ BESSP14$Q5CDA == "37895"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "38082"] <- "PMDB"
BESSP14$Party[ BESSP14$Q5CDA == "38183"] <- "PMDB"
BESSP14$Party[ BESSP14$Q5CDA == "38193"] <- "PRB"
BESSP14$Party[ BESSP14$Q5CDA == "38209"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "38241"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "38296"] <- "PSD"
BESSP14$Party[ BESSP14$Q5CDA == "38350"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "38468"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "38470"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "38474"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "38545"] <- "PSC"
BESSP14$Party[ BESSP14$Q5CDA == "38561"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "38562"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "38576"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "38588"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "38747"] <- "PHS"
BESSP14$Party[ BESSP14$Q5CDA == "38748"] <- "DEM"
BESSP14$Party[ BESSP14$Q5CDA == "38825"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "38866"] <- "PSDB"
BESSP14$Party[ BESSP14$Q5CDA == "38888"] <- "PSTU"
BESSP14$Party[ BESSP14$Q5CDA == "38964"] <- "PMDB"
BESSP14$Party[ BESSP14$Q5CDA == "39005"] <- "PT do B"
BESSP14$Party[ BESSP14$Q5CDA == "39196"] <- "PT"
BESSP14$Party[ BESSP14$Q5CDA == "39254"] <- "PMDB"
BESSP14$Party[ BESSP14$Q5CDA == "39278"] <- "PT"

# Tocantins
BESTO14 <- ESEB14[ which(ESEB14$ESTADO == "17"),]
TSETO14 <- TSEFD2014[ which(TSEFD2014$NM_UE == "TOCANTINS"),]

BESTO14$Party <- NA
#Here, unfortunately, there existed no valid responses on voting intentions.

## Merging district-level datasets.
ESEB14NATIONAL <- rbind(BESAC14, BESAL14, BESAM14, BESAP14, BESBA14, BESCE14, BESDF14, BESES14, BESGO14, 
                        BESMA14, BESMG14, BESMS14, BESMT14, BESPA14, BESPB14, BESPE14, BESPI14, BESPR14,
                        BESRJ14, BESRN14, BESRO14, BESRR14, BESRS14, BESSC14, BESSE14, BESSP14, BESTO14)

BES2014NATIONAL <- na.omit(ESEB14NATIONAL) #Clean dataset

## Cleaning nationa datasets
BES2002NATIONAL$STATE <- NA
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "11"] <- "Rondônia"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "12"] <- "Acre"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "13"] <- "Amazonas"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "14"] <- "Roraima"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "15"] <- "Pará"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "16"] <- "Amapá"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "17"] <- "Tocantins"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "21"] <- "Maranhão"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "22"] <- "Piauí"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "23"] <- "Ceará"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "24"] <- "Rio Grande do Norte"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "25"] <- "Paraíba"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "26"] <- "Pernambuco"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "27"] <- "Alagoas"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "28"] <- "Sergipe"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "29"] <- "Bahia"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "31"] <- "Minas Gerais"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "32"] <- "Espírito Santo"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "33"] <- "Rio de Janeiro"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "35"] <- "São Paulo"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "41"] <- "Paraná"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "42"] <- "Santa Catarina"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "43"] <- "Rio Grande do Sul"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "50"] <- "Mato Grosso do Sul"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "51"] <- "Mato Grosso"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "52"] <- "Goiás"
BES2002NATIONAL$STATE[ BES2002NATIONAL$uf == "53"] <- "Distrito Federal"

BES2014NATIONAL$ESTADO
BES2014NATIONAL$STATE <- NA
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "11"] <- "Rondônia"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "12"] <- "Acre"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "13"] <- "Amazonas"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "14"] <- "Roraima"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "15"] <- "Pará"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "16"] <- "Amapá"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "17"] <- "Tocantins"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "21"] <- "Maranhão"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "22"] <- "Piauí"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "23"] <- "Ceará"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "24"] <- "Rio Grande do Norte"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "25"] <- "Paraíba"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "26"] <- "Pernambuco"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "27"] <- "Alagoas"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "28"] <- "Sergipe"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "29"] <- "Bahia"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "31"] <- "Minas Gerais"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "32"] <- "Espírito Santo"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "33"] <- "Rio de Janeiro"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "35"] <- "São Paulo"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "41"] <- "Paraná"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "42"] <- "Santa Catarina"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "43"] <- "Rio Grande do Sul"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "50"] <- "Mato Grosso do Sul"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "51"] <- "Mato Grosso"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "52"] <- "Goiás"
BES2014NATIONAL$STATE[ BES2014NATIONAL$ESTADO == "53"] <- "Distrito Federal"

BES2002NATIONAL$p19[ BES2002NATIONAL$p19 == "6"] <- NA
BES2002NATIONAL$p22[ BES2002NATIONAL$p22 == "6"] <- NA
BES2002NATIONAL$p50v1[ BES2002NATIONAL$p50v1 == "66"] <- NA
BES2002NATIONAL$p50v1[ BES2002NATIONAL$p50v1 == "88"] <- NA

BES2014NATIONAL$Q15[ BES2014NATIONAL$Q15 == "8"] <- NA
BES2014NATIONAL$Q15[ BES2014NATIONAL$Q15 == "9"] <- NA
BES2014NATIONAL$Q7[ BES2014NATIONAL$Q7 == "8"] <- NA
BES2014NATIONAL$Q7[ BES2014NATIONAL$Q7 == "9"] <- NA
BES2014NATIONAL$Q8[ BES2014NATIONAL$Q8 == "8"] <- NA
BES2014NATIONAL$Q8[ BES2014NATIONAL$Q8 == "9"] <- NA
BES2014NATIONAL$PC6[ BES2014NATIONAL$PC6 == "8"] <- NA
BES2014NATIONAL$PC6[ BES2014NATIONAL$PC6 == "9"] <- NA
BES2014NATIONAL$Q16[ BES2014NATIONAL$Q16 == "8"] <- NA
BES2014NATIONAL$Q16[ BES2014NATIONAL$Q16 == "9"] <- NA
BES2014NATIONAL$Q16A[ BES2014NATIONAL$Q16A == "8"] <- NA
BES2014NATIONAL$Q16A[ BES2014NATIONAL$Q16A == "9"] <- NA
BES2014NATIONAL$PC12B[ BES2014NATIONAL$PC12B == "98"] <- NA
BES2014NATIONAL$PC12B[ BES2014NATIONAL$PC12B == "99"] <- NA
BES2014NATIONAL$Q17[ BES2014NATIONAL$Q17 == "8"] <- NA
BES2014NATIONAL$Q17[ BES2014NATIONAL$Q17 == "9"] <- NA
BES2014NATIONAL$D20A_FXRENDFAM[ BES2014NATIONAL$D20A_FXRENDFAM == "98"] <- NA
BES2014NATIONAL$D20A_FXRENDFAM[ BES2014NATIONAL$D20A_FXRENDFAM == "99"] <- NA
BES2014NATIONAL$D20A_FXRENDFAM[ BES2014NATIONAL$D20A_FXRENDFAM == "9999"] <- NA
BES2014NATIONAL$Q12[ BES2014NATIONAL$Q12 == "95"] <- NA
BES2014NATIONAL$Q12[ BES2014NATIONAL$Q12 == "98"] <- NA
BES2014NATIONAL$Q12[ BES2014NATIONAL$Q12 == "99"] <- NA

BES2002NATIONAL$p22New <- NA
BES2002NATIONAL$p22New[ BES2002NATIONAL$p22 == "1"] <- 1
BES2002NATIONAL$p22New[ BES2002NATIONAL$p22 == "2"] <- 1
BES2002NATIONAL$p22New[ BES2002NATIONAL$p22 == "3"] <- 2
BES2002NATIONAL$p22New[ BES2002NATIONAL$p22 == "4"] <- 3
BES2002NATIONAL$p22New[ BES2002NATIONAL$p22 == "5"] <- 3

BES2014NATIONAL$PC6New <- NA
BES2014NATIONAL$PC6New[ BES2014NATIONAL$PC6 == "1"] <- 3
BES2014NATIONAL$PC6New[ BES2014NATIONAL$PC6 == "2"] <- 1
BES2014NATIONAL$PC6New[ BES2014NATIONAL$PC6 == "3"] <- 2

BES2002NATIONAL$p31New <- NA
BES2002NATIONAL$p31New[ BES2002NATIONAL$p31 == "0"] <- 2
BES2002NATIONAL$p31New[ BES2002NATIONAL$p31 == "1"] <- 1

BES2002NATIONAL$p35New <- NA
BES2002NATIONAL$p35New[ BES2002NATIONAL$p35 == "0"] <- 2
BES2002NATIONAL$p35New[ BES2002NATIONAL$p35 == "1"] <- 1

BES2014NATIONAL$PC12BNew <- NA
BES2014NATIONAL$PC12BNew[ BES2014NATIONAL$PC12B == "1"] <- 1
BES2014NATIONAL$PC12BNew[ BES2014NATIONAL$PC12B == "2"] <- 1
BES2014NATIONAL$PC12BNew[ BES2014NATIONAL$PC12B == "3"] <- 2
BES2014NATIONAL$PC12BNew[ BES2014NATIONAL$PC12B == "4"] <- 2
BES2014NATIONAL$PC12BNew[ BES2014NATIONAL$PC12B == "5"] <- 3
BES2014NATIONAL$PC12BNew[ BES2014NATIONAL$PC12B == "6"] <- 3
BES2014NATIONAL$PC12BNew[ BES2014NATIONAL$PC12B == "7"] <- 4
BES2014NATIONAL$PC12BNew[ BES2014NATIONAL$PC12B == "8"] <- 4
BES2014NATIONAL$PC12BNew[ BES2014NATIONAL$PC12B == "9"] <- 5
BES2014NATIONAL$PC12BNew[ BES2014NATIONAL$PC12B == "10"] <- 5

BES2002NATIONAL$p04New <- NA
BES2002NATIONAL$p04New[ BES2002NATIONAL$p04 == "0"] <- 2
BES2002NATIONAL$p04New[ BES2002NATIONAL$p04 == "1"] <- 1

BES2002NATIONAL$p159New <- NA
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "1"] <- 1
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "2"] <- 2
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "3"] <- 2
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "4"] <- 2
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "5"] <- 2
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "6"] <- 2
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "7"] <- 3
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "8"] <- 3
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "9"] <- 3
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "10"] <- 3
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "11"] <- 3
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "12"] <- 4
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "13"] <- 4
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "14"] <- 4
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "15"] <- 4
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "16"] <- 5
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "17"] <- 5
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "18"] <- 5
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "19"] <- 5
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "20"] <- 5
BES2002NATIONAL$p159New[ BES2002NATIONAL$p159 == "21"] <- 5

BES2014NATIONAL$D3_ESCOLANew <- NA
BES2014NATIONAL$D3_ESCOLANew[ BES2014NATIONAL$D3_ESCOLA == "0"] <- 1
BES2014NATIONAL$D3_ESCOLANew[ BES2014NATIONAL$D3_ESCOLA == "1"] <- 2
BES2014NATIONAL$D3_ESCOLANew[ BES2014NATIONAL$D3_ESCOLA == "2"] <- 2
BES2014NATIONAL$D3_ESCOLANew[ BES2014NATIONAL$D3_ESCOLA == "3"] <- 3
BES2014NATIONAL$D3_ESCOLANew[ BES2014NATIONAL$D3_ESCOLA == "4"] <- 3
BES2014NATIONAL$D3_ESCOLANew[ BES2014NATIONAL$D3_ESCOLA == "5"] <- 4
BES2014NATIONAL$D3_ESCOLANew[ BES2014NATIONAL$D3_ESCOLA == "6"] <- 4
BES2014NATIONAL$D3_ESCOLANew[ BES2014NATIONAL$D3_ESCOLA == "7"] <- 5
BES2014NATIONAL$D3_ESCOLANew[ BES2014NATIONAL$D3_ESCOLA == "8"] <- 5
BES2014NATIONAL$D3_ESCOLANew[ BES2014NATIONAL$D3_ESCOLA == "9"] <- 5

BES2002NATIONAL$p176New <- NA
BES2002NATIONAL$p176New[ BES2002NATIONAL$p176 < 725] <- 1
BES2002NATIONAL$p176New[ BES2002NATIONAL$p176 >= 725 & BES2002NATIONAL$p176 < 1449] <- 2
BES2002NATIONAL$p176New[ BES2002NATIONAL$p176 >= 1449 & BES2002NATIONAL$p176 < 3621] <- 3
BES2002NATIONAL$p176New[ BES2002NATIONAL$p176 >= 3621 & BES2002NATIONAL$p176 < 7241] <- 4
BES2002NATIONAL$p176New[ BES2002NATIONAL$p176 >= 7241 & BES2002NATIONAL$p176 < 10861] <- 5
BES2002NATIONAL$p176New[ BES2002NATIONAL$p176 >= 10861 & BES2002NATIONAL$p176 < 14481] <- 6
BES2002NATIONAL$p176New[ BES2002NATIONAL$p176 >= 14481] <- 7

BES02CLEAN<- subset(BES2002NATIONAL, select = c(Party,    #Voting Intention
                                                STATE,#Electoral District
                                                p19,     #Satisfaction to the functioning of democracy in Brazil
                                                p20,     #Who governs the country makes a difference
                                                p21,     #Your vote influences whay will happen in the country
                                                p22New,  #The best form of government
                                                p31New,  #Is there a political party that represents you?
                                                p35New,  #Is there a political party that you like?
                                                p108b,   #The government must say everything that companies have to do, like how many bathrooms they have to have.
                                                p04New,  #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                                                p159New, #Up to which grade did you study?
                                                p176New, #Adding the income of all the people who live in your house, what is the family income?
                                                p50v1    #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
))

BES14CLEAN <- subset(BES2014NATIONAL, select = c(Party,           #Voting Intention
                                                 STATE,          #Electoral District
                                                 Q15,             #Satisfaction to the functioning of democracy in Brazil
                                                 Q7,              #Who governs the country makes a difference
                                                 Q8,              #Your vote influences whay will happen in the country
                                                 PC6New,          #The best form of government
                                                 Q16,             #Is there a political party that represents you?
                                                 Q16A,            #Is there a political party that you like?
                                                 PC12BNew,        #The government must say everything that companies have to do, like how many bathrooms they have to have.
                                                 Q17,             #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                                                 D3_ESCOLANew,   #Up to which grade did you study?
                                                 D20A_FXRENDFAM, #Adding the income of all the people who live in your house, what is the family income?
                                                 Q12              #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
))

colnames(BES02CLEAN) <- c("PARTY",           #Voting Intention
                          "STATE",           #Electoral District
                          "SATISFACTION",    #Satisfaction to the functioning of democracy in Brazil
                          "DIFFERENCE",      #Who governs the country makes a difference
                          "INFLUENCE",       #Your vote influences whay will happen in the country
                          "GOVERNMENT",      #The best form of government
                          "IDENTIFICATION",  #Is there a political party that represents you?
                          "PREFERENCE",      #Is there a political party that you like?
                          "ECONOMY",         #The government must say everything that companies have to do, like how many bathrooms they have to have.
                          "CONTACT",         #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                          "EDUCATION",      #Up to which grade did you study?
                          "INCOME",         #Adding the income of all the people who live in your house, what is the family income?
                          "LEFTRIGHT"        #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
)

colnames(BES14CLEAN) <- c("PARTY",           #Voting Intention
                          "STATE",           #Electoral District
                          "SATISFACTION",    #Satisfaction to the functioning of democracy in Brazil
                          "DIFFERENCE",      #Who governs the country makes a difference
                          "INFLUENCE",       #Your vote influences whay will happen in the country
                          "GOVERNMENT",      #The best form of government
                          "IDENTIFICATION",  #Is there a political party that represents you?
                          "PREFERENCE",      #Is there a political party that you like?
                          "ECONOMY",         #The government must say everything that companies have to do, like how many bathrooms they have to have.
                          "CONTACT",         #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                          "EDUCATION",      #Up to which grade did you study?
                          "INCOME",         #Adding the income of all the people who live in your house, what is the family income?
                          "LEFTRIGHT"        #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
)

## Coding controlling variables: Effective number of electoral parties at the district level
BES02CLEAN$ENP <- NA
BES14CLEAN$ENP <- NA

BES02CLEAN$ENP[ BES02CLEAN$STATE == "Acre"] <- 7.638647556
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Alagoas"] <- 8.71644783
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Amapá"] <- 8.825015068
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Amazonas"] <- 5.035866737
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Bahia"] <- 4.363955975
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Ceará"] <- 5.772770405
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Distrito Federal"] <- 4.542345524
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Espírito Santo"] <- 9.814389229
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Goiás"] <- 5.690371513
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Maranhão"] <- 6.468179703
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Mato Grosso"] <- 6.453672886
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Mato Grosso do Sul"] <- 6.185633183
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Minas Gerais"] <- 8.567997932
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Pará"] <- 6.131930828
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Paraíba"] <- 7.194557667
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Paraná"] <- 8.157027245
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Pernambuco"] <- 7.922866344
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Piauí"] <- 4.992351532
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Rio de Janeiro"] <- 11.25372219
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Rio Grande do Norte"] <- 5.650588512
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Rio Grande do Sul"] <- 6.867096901
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Roraima"] <- 6.532809258
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Santa Catarina"] <- 5.626364231
BES02CLEAN$ENP[ BES02CLEAN$STATE == "São Paulo"] <- 7.990907669
BES02CLEAN$ENP[ BES02CLEAN$STATE == "Sergipe"] <- 10.05707836

BES14CLEAN$ENP[ BES14CLEAN$STATE == "Acre"] <- 9.582177162
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Alagoas"] <- 10.66336823
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Amapá"] <- 16.97726822
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Amazonas"] <- 9.540507317
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Bahia"] <- 11.26440239
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Ceará"] <- 11.1743679
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Distrito Federal"] <- 13.33987427
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Espírito Santo"] <- 10.88947773
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Goiás"] <- 7.073312283
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Maranhão"] <- 15.80080318
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Mato Grosso do Sul"] <- 6.20756197
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Minas Gerais"] <- 11.86017895
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Pará"] <- 11.28019333
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Paraíba"] <- 9.882687569
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Paraná"] <- 13.19906235
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Pernambuco"] <- 8.2119627
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Piauí"] <- 8.169262886
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Rio de Janeiro"] <- 12.57965183
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Rio Grande do Norte"] <- 11.67064571
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Rio Grande do Sul"] <- 7.650769711
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Roraima"] <- 12.54011726
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Santa Catarina"] <- 7.420486927
BES14CLEAN$ENP[ BES14CLEAN$STATE == "São Paulo"] <- 10.95555878
BES14CLEAN$ENP[ BES14CLEAN$STATE == "Sergipe"] <- 12.02227306

## Coding controlling variables: Ethnic fractionalization at the district level
BES02CLEAN$EthnicFractionalization <- NA
BES14CLEAN$EthnicFractionalization <- NA

BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Acre"] <- 0.453240539
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Alagoas"] <- 0.477337742
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Amapá"] <- 0.479379318
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Amazonas"] <- 0.495866425
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Bahia"] <- 0.505351996
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Ceará"] <- 0.467237287
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Distrito Federal"] <- 0.558951824
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Espírito Santo"] <- 0.566425024
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Goiás"] <- 0.544794099
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Maranhão"] <- 0.466620383
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Mato Grosso"] <- 0.543363361
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Mato Grosso do Sul"] <- 0.53606619
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Minas Gerais"] <- 0.563377028
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Pará"] <- 0.46398909
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Paraíba"] <- 0.513359889
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Paraná"] <- 0.383342418
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Pernambuco"] <- 0.527347128
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Piauí"] <- 0.411050816
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Rio de Janeiro"] <- 0.53348731
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Rio Grande do Norte"] <- 0.510246425
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Rio Grande do Sul"] <- 0.254322213
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Roraima"] <- 0.350324844
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Santa Catarina"] <- 0.194943298
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "São Paulo"] <- 0.441620435
BES02CLEAN$EthnicFractionalization[ BES02CLEAN$STATE == "Sergipe"] <- 0.426528646

BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Acre"] <- 0.52573943
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Alagoas"] <- 0.496422978
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Amapá"] <- 0.487806675
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Amazonas"] <- 0.439574396
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Bahia"] <- 0.567721115
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Ceará"] <- 0.474894927
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Distrito Federal"] <- 0.590219757
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Espírito Santo"] <- 0.591882618
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Goiás"] <- 0.566132455
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Maranhão"] <- 0.478299106
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Mato Grosso do Sul"] <- 0.56919817
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Minas Gerais"] <- 0.588085536
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Pará"] <- 0.452128108
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Paraíba"] <- 0.549188452
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Paraná"] <- 0.466570352
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Pernambuco"] <- 0.535604271
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Piauí"] <- 0.482847688
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Rio de Janeiro"] <- 0.611700847
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Rio Grande do Norte"] <- 0.537729333
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Rio Grande do Sul"] <- 0.341876955
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Roraima"] <- 0.51664
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Santa Catarina"] <- 0.28024018
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "São Paulo"] <- 0.527226627
BES14CLEAN$EthnicFractionalization[ BES14CLEAN$STATE == "Sergipe"] <- 0.498480495

## Coding controlling variables: Class fractionalization at the district level
BES02CLEAN$ClassFractionalization <- NA
BES14CLEAN$ClassFractionalization <- NA

BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Acre"] <- 0.882466056
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Alagoas"] <- 0.772341136
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Amapá"] <- 0.872555387
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Amazonas"] <- 0.853809027
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Bahia"] <- 0.793661277
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Ceará"] <- 0.817797178
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Distrito Federal"] <- 0.878768712
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Espírito Santo"] <- 0.85552996
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Goiás"] <- 0.869993989
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Maranhão"] <- 0.747518538
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Mato Grosso"] <- 0.831127271
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Mato Grosso do Sul"] <- 0.867818529
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Minas Gerais"] <- 0.84653928
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Pará"] <- 0.859028883
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Paraíba"] <- 0.81276936
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Paraná"] <- 0.852314537
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Pernambuco"] <- 0.835473978
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Piauí"] <- 0.712735236
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Rio de Janeiro"] <- 0.878295119
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Rio Grande do Norte"] <- 0.856390707
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Rio Grande do Sul"] <- 0.823557749
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Roraima"] <- 0.875144398
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Santa Catarina"] <- 0.804128252
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "São Paulo"] <- 0.840409943
BES02CLEAN$ClassFractionalization[ BES02CLEAN$STATE == "Sergipe"] <- 0.855665236

BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Acre"] <- 0.870672474
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Alagoas"] <- 0.862511128
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Amapá"] <- 0.880175781
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Amazonas"] <- 0.869996241
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Bahia"] <- 0.863686573
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Ceará"] <- 0.851882426
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Distrito Federal"] <- 0.876253376
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Espírito Santo"] <- 0.875626094
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Goiás"] <- 0.870148686
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Maranhão"] <- 0.803179669
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Mato Grosso do Sul"] <- 0.877090116
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Minas Gerais"] <- 0.870513625
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Pará"] <- 0.863616254
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Paraíba"] <- 0.867523599
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Paraná"] <- 0.849048106
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Pernambuco"] <- 0.873792725
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Piauí"] <- 0.816788016
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Rio de Janeiro"] <- 0.881312196
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Rio Grande do Norte"] <- 0.870497884
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Rio Grande do Sul"] <- 0.854516514
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Roraima"] <- 0.885629397
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Santa Catarina"] <- 0.818458994
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "São Paulo"] <- 0.850122651
BES14CLEAN$ClassFractionalization[ BES14CLEAN$STATE == "Sergipe"] <- 0.859916151

## Coding controlling variables: Regional Authority Index
BES02CLEAN$RAI <- 19.5
BES14CLEAN$RAI <- 19.5

ESEB <- rbind(BES02CLEAN, BES14CLEAN)

ESEBNATIONAL <- na.omit(ESEB)

ESEBNATIONAL$PARTY[ ESEBNATIONAL$PARTY == "PFL"] <- "DEM"
ESEBNATIONAL$PARTY[ ESEBNATIONAL$PARTY == "PL"] <- "PR"
ESEBNATIONAL$PARTY[ ESEBNATIONAL$PARTY == "PRONA"] <- "PR"
ESEBNATIONAL$PARTY[ ESEBNATIONAL$PARTY == "PRB"] <- "PR"

## Coding the degree of party nationalization

unique(sort(MyData$PARTY))
#PP2003: 4.304939  (Equal Share) and 27.0367  (Equal Change)
#PDT:    8.897054  (Equal Share) and 17.1212  (Equal Change)
#PT:    20.74353   (Equal Share) and 11.9215  (Equal Change)
#PTB:    2.724552  (Equal Share) and 11.20572 (Equal Change)
#PMDB:  36.88386   (Equal Share) and 35.67332 (Equal Change)
#PSC:    0.3628846 (Equal Share) and 5.347685 (Equal Change)
#PR:     3.930946  (Equal Share) and 9.936525 (Equal Change)
#PPS:    5.38e-22  (Equal Share) and 1.063595 (Equal Change)
#DEM:   47.35181   (Equal Share) and 43.30902 (Equal Change)
#PMN:    0.4869283 (Equal Share) and 1.646082 (Equal Change)
#PSB:   14.99037   (Equal Share) and 9.719556 (Equal Change)
#PV:     1.61627   (Equal Share) and 1.994438 (Equal Change)
#PSDB:  25.88173   (Equal Share) and 29.34851 (Equal Change)
#PSOL:   1.182162  (Equal Share) and 1.009469 (Equal Change)
#PCdoB:  0.8503498 (Equal Share) and 2.863987 (Equal Change)

ESEBNATIONAL$ES <- NA
ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PP"] <- 4.304939
#ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PDT"] <- 8.897054
ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PT"] <- 20.74353
#ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PTB"] <- 2.724552
ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PMDB"] <- 36.88386
#ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PSC"] <- 0.3628846
ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PR"] <- 3.930946
#ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PPS"] <- 5.38e-22
#ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "DEM"] <- 47.35181
#ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PMN"] <- 0.4869283
ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PSB"] <- 14.99037
#ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PV"] <- 1.61627
ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PSDB"] <- 25.88173
#ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PSOL"] <- 1.182162
#ESEBNATIONAL$ES[ ESEBNATIONAL$PARTY == "PC do B"] <- 0.8503498

ESEBNATIONAL$EC <- NA
ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PP"] <- 27.0367
#ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PDT"] <- 17.1212
ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PT"] <- 11.9215
#ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PTB"] <- 11.20572
ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PMDB"] <- 35.67332
#ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PSC"] <- 5.347685
ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PR"] <- 9.936525
#ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PPS"] <- 1.063595
#ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "DEM"] <- 43.30902
#ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PMN"] <- 1.646082
ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PSB"] <- 9.719556
#ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PV"] <- 1.994438
ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PSDB"] <- 29.34851
#ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PSOL"] <- 1.009469
#ESEBNATIONAL$EC[ ESEBNATIONAL$PARTY == "PC do B"] <- 2.863987

## Coding controlling variables: District Magnitude
ESEBNATIONAL$DISTRICTMAGNITUDE <- NA
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Acre"] <- 8
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Alagoas"] <- 9
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Amapá"] <- 8
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Amazonas"] <- 8
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Bahia"] <- 39
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Ceará" ] <- 22 
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Distrito Federal"] <- 8
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Espírito Santo"] <- 10
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Goiás"] <- 17
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Maranhão"] <- 18
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Minas Gerais"] <- 53
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Mato Grosso do Sul"] <- 8
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Mato Grosso"] <- 8
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Pará"] <- 17
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Paraíba"] <- 12
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Pernambuco"] <- 25
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Piauí"] <- 10
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Paraná"] <- 30
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Rio de Janeiro"] <- 46
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Rio Grande do Sul"] <- 31
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Rio Grande do Norte"] <- 8
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Roraima"] <- 8
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Santa Catarina"] <- 16
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "Sergipe"] <- 8
ESEBNATIONAL$DISTRICTMAGNITUDE[ ESEBNATIONAL$STATE == "São Paulo"] <- 70



### TASK 2: The Estimation of the Multilevel Models with the ESEB (NATIONAL)
library(MASS)
library(rcompanion)
library(nlme)
library(r2glmm)
library(ggpubr)
library(psych)

describe(ESEBNATIONAL)

ESEBNATIONALNEW <- na.omit(ESEBNATIONAL)


EqualShare <- lme(ES ~ SATISFACTION + DIFFERENCE + INFLUENCE + GOVERNMENT + IDENTIFICATION + PREFERENCE +
                    ECONOMY +
                    CONTACT + 
                    EDUCATION + INCOME +
                    LEFTRIGHT + 
                    DISTRICTMAGNITUDE + ENP + EthnicFractionalization + ClassFractionalization,
                  random = ~ SATISFACTION + INFLUENCE + IDENTIFICATION + CONTACT | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(EqualShare)
R2ES <- r2beta(EqualShare, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

EqualChange <- lme(EC ~ SATISFACTION + DIFFERENCE + INFLUENCE + GOVERNMENT + IDENTIFICATION + PREFERENCE +
                    ECONOMY +
                    CONTACT + 
                    EDUCATION + INCOME +
                    LEFTRIGHT + 
                    DISTRICTMAGNITUDE + ENP + EthnicFractionalization + ClassFractionalization,
                  random = ~ SATISFACTION + DIFFERENCE + IDENTIFICATION + ECONOMY + EDUCATION | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(EqualChange)
R2EC <- r2beta(EqualChange, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

ModelES <- plot(x = R2ES) +
  theme_classic() +
  xlab("") +
  ggtitle("R-squared for Equal Shares Model") +
  ylab("R-squared (0 - 1)") +
  theme(text=element_text(size=12, 
                          #       family="Comic Sans MS"))
                          #       family="CM Roman"))
                          #       family="TT Times New Roman"))
                          #       family="Sans"))
                          family="serif"))

ModelEC <- plot(x = R2EC) +
  theme_classic() +
  xlab("") +
  ggtitle("R-squared for Equal Changes Model") +
  ylab("R-squared (0 - 1)") +
  theme(text=element_text(size=12, 
                          #       family="Comic Sans MS"))
                          #       family="CM Roman"))
                          #       family="TT Times New Roman"))
                          #       family="Sans"))
                          family="serif"))

ggarrange(ModelES, ModelEC,
          ncol = 1, nrow = 2) ### Plotting estimated R-squared for both equal shares and equal changes models (Figure 4.1)



### TASK 3: The Estimation of Interaction Effects of Controlling Variables on Equal Shares and Equal Changes (Post-Hoc Analysis-A)
# Specifying interactive models
library(lme4)
library(nlme)
library(interplot)
library(ggplot2)
library(ggpubr)

DIFFERENCE1 <- lme(ES ~ DIFFERENCE + DISTRICTMAGNITUDE + DIFFERENCE*DISTRICTMAGNITUDE, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(DIFFERENCE1)
R2ES <- r2beta(DIFFERENCE1, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE2 <- lme(ES ~ DIFFERENCE + ENP + DIFFERENCE*ENP, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(DIFFERENCE2)
R2ES <- r2beta(DIFFERENCE2, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE3 <- lme(ES ~ DIFFERENCE + EthnicFractionalization + DIFFERENCE*EthnicFractionalization, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(DIFFERENCE3)
R2ES <- r2beta(DIFFERENCE3, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE4 <- lme(ES ~ DIFFERENCE + ClassFractionalization + DIFFERENCE*ClassFractionalization, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(DIFFERENCE4)
R2ES <- r2beta(DIFFERENCE4, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

IDENTIFICATION1 <- lme(ES ~ IDENTIFICATION + DISTRICTMAGNITUDE + IDENTIFICATION*DISTRICTMAGNITUDE, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(IDENTIFICATION1)
R2ES <- r2beta(IDENTIFICATION1, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

IDENTIFICATION2 <- lme(ES ~ IDENTIFICATION + ENP + IDENTIFICATION*ENP, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(IDENTIFICATION2)
R2ES <- r2beta(IDENTIFICATION2, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

IDENTIFICATION3 <- lme(ES ~ IDENTIFICATION + EthnicFractionalization + IDENTIFICATION*EthnicFractionalization, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(IDENTIFICATION3)
R2ES <- r2beta(IDENTIFICATION3, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

IDENTIFICATION4 <- lme(ES ~ IDENTIFICATION + ClassFractionalization + IDENTIFICATION*ClassFractionalization, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(IDENTIFICATION4)
R2ES <- r2beta(IDENTIFICATION4, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

SATISFACTION1 <- lme(EC ~ SATISFACTION + DISTRICTMAGNITUDE + SATISFACTION*DISTRICTMAGNITUDE, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(SATISFACTION1)
R2ES <- r2beta(SATISFACTION1, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

SATISFACTION2 <- lme(EC ~ SATISFACTION + ENP + SATISFACTION*ENP, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(SATISFACTION2)
R2ES <- r2beta(SATISFACTION2, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

SATISFACTION3 <- lme(EC ~ SATISFACTION + EthnicFractionalization + SATISFACTION*EthnicFractionalization, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(SATISFACTION3)
R2ES <- r2beta(SATISFACTION3, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

SATISFACTION4 <- lme(EC ~ SATISFACTION + ClassFractionalization + SATISFACTION*ClassFractionalization, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(SATISFACTION4)
R2ES <- r2beta(SATISFACTION4, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE10 <- lme(EC ~ DIFFERENCE + DISTRICTMAGNITUDE + DIFFERENCE*DISTRICTMAGNITUDE, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(DIFFERENCE10)
R2ES <- r2beta(DIFFERENCE10, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE20 <- lme(EC ~ DIFFERENCE + ENP + DIFFERENCE*ENP, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(DIFFERENCE20)
R2ES <- r2beta(DIFFERENCE20, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE30 <- lme(EC ~ DIFFERENCE + EthnicFractionalization + DIFFERENCE*EthnicFractionalization, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(DIFFERENCE30)
R2ES <- r2beta(DIFFERENCE30, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE40 <- lme(EC ~ DIFFERENCE + ClassFractionalization + DIFFERENCE*ClassFractionalization, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(DIFFERENCE40)
R2ES <- r2beta(DIFFERENCE40, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

LEFTRIGHT1 <- lme(EC ~ LEFTRIGHT + DISTRICTMAGNITUDE + LEFTRIGHT*DISTRICTMAGNITUDE, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(LEFTRIGHT1)
R2ES <- r2beta(LEFTRIGHT1, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

LEFTRIGHT2 <- lme(EC ~ LEFTRIGHT + ENP + LEFTRIGHT*ENP, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(LEFTRIGHT2)
R2ES <- r2beta(LEFTRIGHT2, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

LEFTRIGHT3 <- lme(EC ~ LEFTRIGHT + EthnicFractionalization + LEFTRIGHT*EthnicFractionalization, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(LEFTRIGHT3)
R2ES <- r2beta(LEFTRIGHT3, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

LEFTRIGHT4 <- lme(EC ~ LEFTRIGHT + ClassFractionalization + LEFTRIGHT*ClassFractionalization, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(LEFTRIGHT4)
R2ES <- r2beta(LEFTRIGHT4, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

LEFTRIGHT5 <- lme(EC ~ LEFTRIGHT + EDUCATION + LEFTRIGHT*EDUCATION, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(LEFTRIGHT5)
R2ES <- r2beta(LEFTRIGHT5, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

LEFTRIGHT6 <- lme(EC ~ LEFTRIGHT + INCOME + LEFTRIGHT*INCOME, random = ~ 1 | STATE, data = ESEBNATIONALNEW, method = "ML")
summary(LEFTRIGHT6)
R2ES <- r2beta(LEFTRIGHT6, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

# Models for plotting
DIFFERENCE1 <- lmer(ES ~ DIFFERENCE + DISTRICTMAGNITUDE + DIFFERENCE*DISTRICTMAGNITUDE + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(DIFFERENCE1)
R2ES <- r2beta(DIFFERENCE1, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE2 <- lmer(ES ~ DIFFERENCE + ENP + DIFFERENCE*ENP + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(DIFFERENCE2)
R2ES <- r2beta(DIFFERENCE2, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE3 <- lmer(ES ~ DIFFERENCE + EthnicFractionalization + DIFFERENCE*EthnicFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(DIFFERENCE3)
R2ES <- r2beta(DIFFERENCE3, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE4 <- lmer(ES ~ DIFFERENCE + ClassFractionalization + DIFFERENCE*ClassFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(DIFFERENCE4)
R2ES <- r2beta(DIFFERENCE4, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

IDENTIFICATION1 <- lmer(ES ~ IDENTIFICATION + DISTRICTMAGNITUDE + IDENTIFICATION*DISTRICTMAGNITUDE + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(DIFFERENCE1)
R2ES <- r2beta(IDENTIFICATION1, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

IDENTIFICATION2 <- lmer(ES ~ IDENTIFICATION + ENP + IDENTIFICATION*ENP + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(IDENTIFICATION2)
R2ES <- r2beta(IDENTIFICATION, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

IDENTIFICATION3 <- lmer(ES ~ IDENTIFICATION + EthnicFractionalization + IDENTIFICATION*EthnicFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(IDENTIFICATION3)
R2ES <- r2beta(IDENTIFICATION3, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

IDENTIFICATION4 <- lmer(ES ~ IDENTIFICATION + ClassFractionalization + IDENTIFICATION*ClassFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(IDENTIFICATION4)
R2ES <- r2beta(IDENTIFICATION4, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

SATISFACTION1 <- lmer(EC ~ SATISFACTION + DISTRICTMAGNITUDE + SATISFACTION*DISTRICTMAGNITUDE + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(SATISFACTION1)
R2ES <- r2beta(SATISFACTION1, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

SATISFACTION2 <- lmer(EC ~ SATISFACTION + ENP + SATISFACTION*ENP + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(SATISFACTION2)
R2ES <- r2beta(SATISFACTION2, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

SATISFACTION3 <- lmer(EC ~ SATISFACTION + EthnicFractionalization + SATISFACTION*EthnicFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(SATISFACTION3)
R2ES <- r2beta(SATISFACTION3, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

SATISFACTION4 <- lmer(EC ~ SATISFACTION + ClassFractionalization + SATISFACTION*ClassFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(SATISFACTION4)
R2ES <- r2beta(SATISFACTION4, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE10 <- lmer(EC ~ DIFFERENCE + DISTRICTMAGNITUDE + DIFFERENCE*DISTRICTMAGNITUDE + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(DIFFERENCE10)
R2ES <- r2beta(DIFFERENCE10, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE20 <- lmer(EC ~ DIFFERENCE + ENP + DIFFERENCE*ENP + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(DIFFERENCE20)
R2ES <- r2beta(DIFFERENCE20, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE30 <- lmer(EC ~ DIFFERENCE + EthnicFractionalization + DIFFERENCE*EthnicFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(DIFFERENCE30)
R2ES <- r2beta(DIFFERENCE30, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE40 <- lmer(EC ~ DIFFERENCE + ClassFractionalization + DIFFERENCE*ClassFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(DIFFERENCE40)
R2ES <- r2beta(DIFFERENCE40, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

LEFTRIGHT1 <- lmer(EC ~ LEFTRIGHT + DISTRICTMAGNITUDE + LEFTRIGHT*DISTRICTMAGNITUDE + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(LEFTRIGHT1)
R2ES <- r2beta(LEFTRIGHT1, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

LEFTRIGHT2 <- lmer(EC ~ LEFTRIGHT + ENP + LEFTRIGHT*ENP + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(LEFTRIGHT2)
R2ES <- r2beta(LEFTRIGHT2, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

LEFTRIGHT3 <- lmer(EC ~ LEFTRIGHT + EthnicFractionalization + LEFTRIGHT*EthnicFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(LEFTRIGHT3)
R2ES <- r2beta(LEFTRIGHT3, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

LEFTRIGHT4 <- lmer(EC ~ LEFTRIGHT + ClassFractionalization + LEFTRIGHT*ClassFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(LEFTRIGHT4)
R2ES <- r2beta(LEFTRIGHT4, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

LEFTRIGHT5 <- lmer(EC ~ LEFTRIGHT + EDUCATION + LEFTRIGHT*EDUCATION + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(LEFTRIGHT5)
R2ES <- r2beta(LEFTRIGHT5, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

LEFTRIGHT6 <- lmer(EC ~ LEFTRIGHT + INCOME + LEFTRIGHT*INCOME + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
summary(LEFTRIGHT6)
R2ES <- r2beta(LEFTRIGHT6, partial = TRUE, method = "nsj", data = ESEBNATIONALNEW)

DIFFERENCE1 <- lmer(ES ~ DIFFERENCE + DISTRICTMAGNITUDE + DIFFERENCE*DISTRICTMAGNITUDE + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C1 <- interplot(m = DIFFERENCE1, var1 = "DIFFERENCE", var2 = "DISTRICTMAGNITUDE", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("District Magnitude") +
  ylab("Estimated Coefficient for DIFFERENCE") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of DIFFERENCE \nby District Magnitude") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

DIFFERENCE2 <- lmer(ES ~ DIFFERENCE + ENP + DIFFERENCE*ENP + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C2 <- interplot(m = DIFFERENCE2, var1 = "DIFFERENCE", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Effective Number of Electoral Parties at the District-Level") +
  ylab("Estimated Coefficient for DIFFERENCE") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of DIFFERENCE \nby Effective Number of Electoral Parties at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

DIFFERENCE3 <- lmer(ES ~ DIFFERENCE + EthnicFractionalization + DIFFERENCE*EthnicFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C3 <- interplot(m = DIFFERENCE3, var1 = "DIFFERENCE", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Ethnic Fractionalization at the District-Level") +
  ylab("Estimated Coefficient for DIFFERENCE") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of DIFFERENCE \nby Ethnic Fractionalization at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

DIFFERENCE4 <- lmer(ES ~ DIFFERENCE + ClassFractionalization + DIFFERENCE*ClassFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C4 <- interplot(m = DIFFERENCE4, var1 = "DIFFERENCE", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Class Fractionalization at the District-Level") +
  ylab("Estimated Coefficient for DIFFERENCE") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of DIFFERENCE \nby Class Fractionalization at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4,
          ncol = 2, nrow = 2) # Figure 6.2

DIFFERENCE10 <- lmer(EC ~ DIFFERENCE + DISTRICTMAGNITUDE + DIFFERENCE*DISTRICTMAGNITUDE + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C10 <- interplot(m = DIFFERENCE10, var1 = "DIFFERENCE", var2 = "DISTRICTMAGNITUDE", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("District Magnitude") +
  ylab("Estimated Coefficient for DIFFERENCE") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of DIFFERENCE \nby District Magnitude") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

DIFFERENCE20 <- lmer(EC ~ DIFFERENCE + ENP + DIFFERENCE*ENP + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C20 <- interplot(m = DIFFERENCE20, var1 = "DIFFERENCE", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Effective Number of Electoral Parties at the District-Level") +
  ylab("Estimated Coefficient for DIFFERENCE") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of DIFFERENCE \nby Effective Number of Electoral Parties at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

DIFFERENCE30 <- lmer(EC ~ DIFFERENCE + EthnicFractionalization + DIFFERENCE*EthnicFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C30 <- interplot(m = DIFFERENCE30, var1 = "DIFFERENCE", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Ethnic Fractionalization at the District-Level") +
  ylab("Estimated Coefficient for DIFFERENCE") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of DIFFERENCE \nby Ethnic Fractionalization at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

DIFFERENCE40 <- lmer(EC ~ DIFFERENCE + ClassFractionalization + DIFFERENCE*ClassFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C40 <- interplot(m = DIFFERENCE40, var1 = "DIFFERENCE", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Class Fractionalization at the District-Level") +
  ylab("Estimated Coefficient for DIFFERENCE") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of DIFFERENCE \nby Class Fractionalization at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C10, C20, C30, C40,
          ncol = 2, nrow = 2) # Figure 6.2

IDENTIFICATION1 <- lmer(ES ~ IDENTIFICATION + DISTRICTMAGNITUDE + IDENTIFICATION*DISTRICTMAGNITUDE + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C1 <- interplot(m = IDENTIFICATION1, var1 = "IDENTIFICATION", var2 = "DISTRICTMAGNITUDE", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("District Magnitude") +
  ylab("Estimated Coefficient for IDENTIFICATION") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of IDENTIFICATION \nby District Magnitude") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 8), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

IDENTIFICATION2 <- lmer(ES ~ IDENTIFICATION + ENP + IDENTIFICATION*ENP + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C2 <- interplot(m = IDENTIFICATION2, var1 = "IDENTIFICATION", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Effective Number of Electoral Parties at the District-Level") +
  ylab("Estimated Coefficient for IDENTIFICATION") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of IDENTIFICATION \nby Effective Number of Electoral Parties at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 8), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

IDENTIFICATION3 <- lmer(ES ~ IDENTIFICATION + EthnicFractionalization + IDENTIFICATION*EthnicFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C3 <- interplot(m = IDENTIFICATION3, var1 = "IDENTIFICATION", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Ethnic Fractionalization at the District-Level") +
  ylab("Estimated Coefficient for IDENTIFICATION") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of IDENTIFICATION \nby Ethnic Fractionalization at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 8), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

IDENTIFICATION4 <- lmer(ES ~ IDENTIFICATION + ClassFractionalization + IDENTIFICATION*ClassFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C4 <- interplot(m = IDENTIFICATION4, var1 = "IDENTIFICATION", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Class Fractionalization at the District-Level") +
  ylab("Estimated Coefficient for IDENTIFICATION") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of IDENTIFICATION \nby Class Fractionalization at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 8), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4,
          ncol = 2, nrow = 2) # Figure E.2

SATISFACTION1 <- lmer(EC ~ SATISFACTION + DISTRICTMAGNITUDE + SATISFACTION*DISTRICTMAGNITUDE + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C1 <- interplot(m = SATISFACTION1, var1 = "SATISFACTION", var2 = "DISTRICTMAGNITUDE", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("District Magnitude") +
  ylab("Estimated Coefficient for SATISFACTION") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SATISFACTION \nby District Magnitude") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SATISFACTION2 <- lmer(EC ~ SATISFACTION + ENP + SATISFACTION*ENP + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C2 <- interplot(m = SATISFACTION2, var1 = "SATISFACTION", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Effective Number of Electoral Parties at the District-Level") +
  ylab("Estimated Coefficient for SATISFACTION") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SATISFACTION \nby Effective Number of Electoral Parties at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SATISFACTION3 <- lmer(EC ~ SATISFACTION + EthnicFractionalization + SATISFACTION*EthnicFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C3 <- interplot(m = SATISFACTION3, var1 = "SATISFACTION", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Ethnic Fractionalization at the District-Level") +
  ylab("Estimated Coefficient for SATISFACTION") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SATISFACTION \nby Ethnic Fractionalization at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SATISFACTION4 <- lmer(EC ~ SATISFACTION + ClassFractionalization + SATISFACTION*ClassFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C4 <- interplot(m = SATISFACTION4, var1 = "SATISFACTION", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Class Fractionalization at the District-Level") +
  ylab("Estimated Coefficient for SATISFACTION") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SATISFACTION \nby Class Fractionalization at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4,
          ncol = 2, nrow = 2) # Figure E.2

LEFTRIGHT1 <- lmer(EC ~ LEFTRIGHT + DISTRICTMAGNITUDE + LEFTRIGHT*DISTRICTMAGNITUDE + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C1 <- interplot(m = LEFTRIGHT1, var1 = "LEFTRIGHT", var2 = "DISTRICTMAGNITUDE", hist = TRUE, adjCI = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("District Magnitude") +
  ylab("Estimated Coefficient for LEFT RIGHT") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LEFT RIGHT \nby District Magnitude") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LEFTRIGHT2 <- lmer(EC ~ LEFTRIGHT + ENP + LEFTRIGHT*ENP + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C2 <- interplot(m = LEFTRIGHT2, var1 = "LEFTRIGHT", var2 = "ENP", hist = TRUE, adjCI = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Effective Number of Electoral Parties at the District-Level") +
  ylab("Estimated Coefficient for LEFT RIGHT") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LEFT RIGHT \nby Effective Number of Electoral Parties at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LEFTRIGHT3 <- lmer(EC ~ LEFTRIGHT + EthnicFractionalization + LEFTRIGHT*EthnicFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C3 <- interplot(m = LEFTRIGHT3, var1 = "LEFTRIGHT", var2 = "EthnicFractionalization", hist = TRUE, adjCI = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Ethnic Fractionalization at the District-Level") +
  ylab("Estimated Coefficient for LEFT RIGHT") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LEFT RIGHT \nby Ethnic Fractionalization at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LEFTRIGHT4 <- lmer(EC ~ LEFTRIGHT + ClassFractionalization + LEFTRIGHT*ClassFractionalization + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C4 <- interplot(m = LEFTRIGHT4, var1 = "LEFTRIGHT", var2 = "ClassFractionalization", hist = TRUE, adjCI = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Class Fractionalization at the District-Level") +
  ylab("Estimated Coefficient for LEFT RIGHT") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LEFT RIGHT \nby Class Fractionalization at the District-Level") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LEFTRIGHT5 <- lmer(EC ~ LEFTRIGHT + EDUCATION + LEFTRIGHT*EDUCATION + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C5 <- interplot(m = LEFTRIGHT5, var1 = "LEFTRIGHT", var2 = "EDUCATION", hist = TRUE, adjCI = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("The Educational Level of Mass Respondents") +
  ylab("Estimated Coefficient for LEFT RIGHT") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LEFT RIGHT \nby The Educational Level of Mass Respondents") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LEFTRIGHT6 <- lmer(EC ~ LEFTRIGHT + INCOME + LEFTRIGHT*INCOME + (1 | STATE), data = ESEBNATIONALNEW, REML = FALSE)
C6 <- interplot(m = LEFTRIGHT6, var1 = "LEFTRIGHT", var2 = "INCOME", hist = TRUE, adjCI = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("The Monthly Household Income Level of Mass Respondents") +
  ylab("Estimated Coefficient for LEFT RIGHT") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LEFT RIGHT \nby The Monthly Household Income Level of Mass Respondents") +
  theme(plot.title = element_text(size = 9), axis.title.y = element_text(size = 9), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure E.2

ggarrange( C5, C6,
          ncol = 2, nrow = 1) # Figure E.2


### TASK 4: Data Preparation (LOCAL)
rm(list=ls(all=TRUE))

ESEB02<- subset(ESEB2002, select = c(    uf, #E11lectoral District
                                         p19, #Satisfaction to the functioning of democracy in Brazil
                                         p20, #Who governs the country makes a difference
                                         p21, #Your vote influences whay will happen in the country
                                         p22, #The best form of government
                                         p31, #Is there a political party that represents you?
                                         p35, #Is there a political party that you like?
                                         p108b, #The government must say everything that companies have to do, like how many bathrooms they have to have.
                                         p04, #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                                         p159, #Up to which grade did you study?
                                         p176, #Adding the income of all the people who live in your house, what is the family income?
                                         p50v1, #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
                                         p13, #Who did you vote for FEDERAL DEPUTY?
                                         p14  #Who did you vote for STATE DEPUTY?
))

ESEB14 <- subset(ESEB2014, select = c(ESTADO, #Electoral District
                                      Q15, #Satisfaction to the functioning of democracy in Brazil
                                      Q7, #Who governs the country makes a difference
                                      Q8, #Your vote influences whay will happen in the country
                                      PC6, #The best form of government
                                      Q16, #Is there a political party that represents you?
                                      Q16A, #Is there a political party that you like?
                                      PC12B, #The government must say everything that companies have to do, like how many bathrooms they have to have.
                                      Q17, #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                                      D3_ESCOLA, #Up to which grade did you study?
                                      D20A_FXRENDFAM, #Adding the income of all the people who live in your house, what is the family income?
                                      Q12, #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
                                      Q5CDA, #Who did you vote for FEDERAL DEPUTY?
                                      Q5ALA
                                      
))

## Assigning party labels, based on voting intentions at the 2002 state-deputy elections.
# Acre
BESAC <- ESEB02[ which(ESEB02$uf == "12"),]

BESAC$Party2 <- NA
BESAC$Party2[ BESAC$p14 == "AFONSO GEBER                                                                                                                                                                                                                                                   "] <- "PL"
BESAC$Party2[ BESAC$p14 == "ANGELIM                                                                                                                                                                                                                                                        "] <- "PSB"
BESAC$Party2[ BESAC$p14 == "BESTENE                                                                                                                                                                                                                                                        "] <- "PSB"
BESAC$Party2[ BESAC$p14 == "JOSE VIEIRA                                                                                                                                                                                                                                                    "] <- "PSB"
BESAC$Party2[ BESAC$p14 == "JOSÉ VIEIRA                                                                                                                                                                                                                                                    "] <- "PSB"
BESAC$Party2[ BESAC$p14 == "NILSON AREAL                                                                                                                                                                                                                                                   "] <- "PSB"
BESAC$Party2[ BESAC$p14 == "NILSON AREIAL                                                                                                                                                                                                                                                  "] <- "PSB"
BESAC$Party2[ BESAC$p14 == "PASTOR JONAS                                                                                                                                                                                                                                                   "] <- "PPB"
BESAC$Party2[ BESAC$p14 == "POLANCO                                                                                                                                                                                                                                                        "] <- "PSB"
BESAC$Party2[ BESAC$p14 == "RAIMUNDO SALES                                                                                                                                                                                                                                                 "] <- "PSDC"
BESAC$Party2[ BESAC$p14 == "ZÉ VIEIRA                                                                                                                                                                                                                                                      "] <- "PSB"
BESAC$Party2[ BESAC$p14 == "NALÍ GOUVEIA                                                                                                                                                                                                                                                   "] <- "PMDB"
BESAC$Party2[ BESAC$p14 == "NALU                                                                                                                                                                                                                                                           "] <- "PMDB"
BESAC$Party2[ BESAC$p14 == "HELIO LOPES                                                                                                                                                                                                                                                    "] <- "PPS"
BESAC$Party2[ BESAC$p14 == "GLÁCIO                                                                                                                                                                                                                                                         "] <- "PSDB"

# Alagoas
BESAL <- ESEB02[ which(ESEB02$uf == "27"),]

BESAL$Party2 <- NA
BESAL$Party2[ BESAL$p14 == "CELSO LUIZ                                                                                                                                                                                                                                                     "] <- "PL"
BESAL$Party2[ BESAL$p14 == "CICERO AMÉLIO                                                                                                                                                                                                                                                  "] <- "PPS"
BESAL$Party2[ BESAL$p14 == "GERVASIO RAIMUNDO                                                                                                                                                                                                                                              "] <- "PTB"
BESAL$Party2[ BESAL$p14 == "JOÃO DOS SANTOS                                                                                                                                                                                                                                                "] <- "PSB"
BESAL$Party2[ BESAL$p14 == "MARCOS BARBOSA                                                                                                                                                                                                                                                 "] <- "PTdoB"
BESAL$Party2[ BESAL$p14 == "MAURICIO QUINTELA                                                                                                                                                                                                                                              "] <- "PTdoB"
BESAL$Party2[ BESAL$p14 == "SILVIO CAMELO                                                                                                                                                                                                                                                  "] <- "PMN"
BESAL$Party2[ BESAL$p14 == "TRACÍSIO FREIRE                                                                                                                                                                                                                                                "] <- "PTdoB"
BESAL$Party2[ BESAL$p14 == "USIEL MARIANO                                                                                                                                                                                                                                                  "] <- "PRONA"
BESAL$Party2[ BESAL$p14 == "ZIANE COSTA                                                                                                                                                                                                                                                    "] <- "PTB"

# Amazonas
BESAM <- ESEB02[ which(ESEB02$uf == "13"),]

BESAM$Party2 <- NA
BESAM$Party2[ BESAM$p14 == "ARTHUR BISNETO                                                                                                                                                                                                                                                 "] <- "PSDB"
BESAM$Party2[ BESAM$p14 == "DR.VICENTE                                                                                                                                                                                                                                                     "] <- "PMDB"
BESAM$Party2[ BESAM$p14 == "DRA. VERA LUCIA                                                                                                                                                                                                                                                "] <- "PL"
BESAM$Party2[ BESAM$p14 == "MIGUEL CARRATO                                                                                                                                                                                                                                                 "] <- "PFL"
BESAM$Party2[ BESAM$p14 == "MIGUEL CARRETE                                                                                                                                                                                                                                                 "] <- "PFL"
BESAM$Party2[ BESAM$p14 == "NELSON AZEDO                                                                                                                                                                                                                                                   "] <- "PFL"
BESAM$Party2[ BESAM$p14 == "ROSA CAVALCANTE                                                                                                                                                                                                                                                "] <- "PST"
BESAM$Party2[ BESAM$p14 == "WALLACE SOUZA                                                                                                                                                                                                                                                  "] <- "PL"

# Amapá
BESAP <- ESEB02[ which(ESEB02$uf == "16"),]

BESAP$Party2 <- NA
BESAP$Party2[ BESAP$p14 == "JORGE SALOMÃO                                                                                                                                                                                                                                                  "] <- "PL"
BESAP$Party2[ BESAP$p14 == "PAULO JOSÉ                                                                                                                                                                                                                                                     "] <- "PTB"
BESAP$Party2[ BESAP$p14 == "RIBAMAR                                                                                                                                                                                                                                                        "] <- "PT"

# Bahia
BESBA <- ESEB02[ which(ESEB02$uf == "29"),]

BESBA$Party2 <- NA
BESBA$Party2[ BESBA$p14 == "ADERBAL                                                                                                                                                                                                                                                        "] <- "PPB"
BESBA$Party2[ BESBA$p14 == "ADERBAL CALDAS                                                                                                                                                                                                                                                 "] <- "PPB"
BESBA$Party2[ BESBA$p14 == "ADOLFO MENEZES                                                                                                                                                                                                                                                 "] <- "PST"
BESBA$Party2[ BESBA$p14 == "ALDOLFO MENEZES                                                                                                                                                                                                                                                "] <- "PST"
BESBA$Party2[ BESBA$p14 == "BATISTA NEVES                                                                                                                                                                                                                                                  "] <- "PMDB"
BESBA$Party2[ BESBA$p14 == "ELIANA BOAVENTURA                                                                                                                                                                                                                                              "] <- "PPB"
BESBA$Party2[ BESBA$p14 == "ELMAR                                                                                                                                                                                                                                                          "] <- "PMDB"
BESBA$Party2[ BESBA$p14 == "ELMAR NASCIMENTO                                                                                                                                                                                                                                               "] <- "PMDB"
BESBA$Party2[ BESBA$p14 == "FERNANDO ANDRADE                                                                                                                                                                                                                                               "] <- "PFL"
BESBA$Party2[ BESBA$p14 == "FERNANDO ANDRADE                                                                                                                                                                                                                                               "] <- "PFL"
BESBA$Party2[ BESBA$p14 == "JOÃO HENRIQUE                                                                                                                                                                                                                                                  "] <- "PDT"
BESBA$Party2[ BESBA$p14 == "JONAS ALVES                                                                                                                                                                                                                                                    "] <- "PTC"
BESBA$Party2[ BESBA$p14 == "JOSÉ ANTÕNIO                                                                                                                                                                                                                                                   "] <- "PT"
BESBA$Party2[ BESBA$p14 == "JOSSE MARI                                                                                                                                                                                                                                                     "] <- "PCO"
BESBA$Party2[ BESBA$p14 == "JUSMARI                                                                                                                                                                                                                                                        "] <- "PFL"
BESBA$Party2[ BESBA$p14 == "LUIZ HENRIQUE                                                                                                                                                                                                                                                  "] <- "PPB"
BESBA$Party2[ BESBA$p14 == "MARCELO GUIMARÃES                                                                                                                                                                                                                                              "] <- "PL"
BESBA$Party2[ BESBA$p14 == "MOEMA GRAMACHO                                                                                                                                                                                                                                                 "] <- "PT"
BESBA$Party2[ BESBA$p14 == "ROBERTO MUNIZ                                                                                                                                                                                                                                                  "] <- "PPB"
BESBA$Party2[ BESBA$p14 == "TARCÍSIO PIMENTA                                                                                                                                                                                                                                               "] <- "PTB"

# Ceará
BESCE <- ESEB02[ which(ESEB02$uf == "23"),]

BESCE$Party2 <- NA
BESCE$Party2[ BESCE$p14 == "ARTHUR BRUNO                                                                                                                                                                                                                                                   "] <- "PT"
BESCE$Party2[ BESCE$p14 == "ARTUR BRUNO                                                                                                                                                                                                                                                    "] <- "PT"
BESCE$Party2[ BESCE$p14 == "CAETANO                                                                                                                                                                                                                                                        "] <- "PPB"
BESCE$Party2[ BESCE$p14 == "CAETANO GUEDES                                                                                                                                                                                                                                                 "] <- "PPB"
BESCE$Party2[ BESCE$p14 == "CARLOMANO MARQUES                                                                                                                                                                                                                                              "] <- "PMDB"
BESCE$Party2[ BESCE$p14 == "CARLOMANO MARQUES                                                                                                                                                                                                                                              "] <- "PMDB"
BESCE$Party2[ BESCE$p14 == "DR PAULO AFONSO                                                                                                                                                                                                                                                "] <- "PTB"
BESCE$Party2[ BESCE$p14 == "DR. GERARDINHO SOARES                                                                                                                                                                                                                                          "] <- "PHS"
BESCE$Party2[ BESCE$p14 == "DR. HUGO                                                                                                                                                                                                                                                       "] <- "PSDB"
BESCE$Party2[ BESCE$p14 == "FERNANDO HUGO                                                                                                                                                                                                                                                  "] <- "PSDB"
BESCE$Party2[ BESCE$p14 == "FROTA CAVALCANTE                                                                                                                                                                                                                                               "] <- "PHS"
BESCE$Party2[ BESCE$p14 == "GERALDO SOARES                                                                                                                                                                                                                                                 "] <- "PHS"
BESCE$Party2[ BESCE$p14 == "GOMES FARIAS                                                                                                                                                                                                                                                   "] <- "PST"
BESCE$Party2[ BESCE$p14 == "HEITOR FERRER                                                                                                                                                                                                                                                  "] <- "PDT"
BESCE$Party2[ BESCE$p14 == "JOÃO JAIME                                                                                                                                                                                                                                                     "] <- "PSDB"
BESCE$Party2[ BESCE$p14 == "REGINA                                                                                                                                                                                                                                                         "] <- "PL"
BESCE$Party2[ BESCE$p14 == "REGINA CARDOSO                                                                                                                                                                                                                                                 "] <- "PL"
BESCE$Party2[ BESCE$p14 == "ROGÉRIO AGUIAR                                                                                                                                                                                                                                                 "] <- "PSDB"
BESCE$Party2[ BESCE$p14 == "RONALDO MARTINS                                                                                                                                                                                                                                                "] <- "PL"
BESCE$Party2[ BESCE$p14 == "TADEU PONTES                                                                                                                                                                                                                                                   "] <- "PCdoB"

# Distrito Federal
BESDF <- ESEB02[ which(ESEB02$uf == "53"),]

BESDF$Party2 <- NA
#Here, unfortunately, there existed no valid responses on voting intentions.

# Espírito Santo
BESES <- ESEB02[ which(ESEB02$uf == "32"),]

BESES$Party2 <- NA
BESES$Party2[ BESES$p14 == "BENEDITO ENÉAS                                                                                                                                                                                                                                                 "] <- "PFL"
BESES$Party2[ BESES$p14 == "CÉSAR COLGNAGO                                                                                                                                                                                                                                                 "] <- "PPS"
BESES$Party2[ BESES$p14 == "CLAUDIO VEREZA                                                                                                                                                                                                                                                 "] <- "PT"
BESES$Party2[ BESES$p14 == "CORONEL RUBIM                                                                                                                                                                                                                                                  "] <- "PSB"
BESES$Party2[ BESES$p14 == "DÉLIO PARRINI                                                                                                                                                                                                                                                  "] <- "PSC"
BESES$Party2[ BESES$p14 == "EDIVAL PETRI                                                                                                                                                                                                                                                   "] <- "PSDB"
BESES$Party2[ BESES$p14 == "EUCLÉRIO SAMPAIO                                                                                                                                                                                                                                               "] <- "PTB"
BESES$Party2[ BESES$p14 == "GERALDINHO - PMN                                                                                                                                                                                                                                               "] <- "PMN"
BESES$Party2[ BESES$p14 == "GILSINHO LOPES                                                                                                                                                                                                                                                 "] <- "PFL"
BESES$Party2[ BESES$p14 == "GRATZ                                                                                                                                                                                                                                                          "] <- "PFL"
BESES$Party2[ BESES$p14 == "HELDER SALOMÃO                                                                                                                                                                                                                                                 "] <- "PT"
BESES$Party2[ BESES$p14 == "JORGE ANDERS                                                                                                                                                                                                                                                   "] <- "PFL"
BESES$Party2[ BESES$p14 == "JOSÉ CARLOS GRATZ                                                                                                                                                                                                                                              "] <- "PFL"
BESES$Party2[ BESES$p14 == "LUCIANO REZENDE                                                                                                                                                                                                                                                "] <- "PPS"
BESES$Party2[ BESES$p14 == "MARIAZINHA VELLOSO                                                                                                                                                                                                                                             "] <- "PSDB"
BESES$Party2[ BESES$p14 == "MARIAZINHA VELOSO LUCAS                                                                                                                                                                                                                                        "] <- "PSDB"
BESES$Party2[ BESES$p14 == "ROBSON VAILANT                                                                                                                                                                                                                                                 "] <- "PL"
BESES$Party2[ BESES$p14 == "WALTER DEPRA                                                                                                                                                                                                                                                   "] <- "PFL"

# Goiás
BESGO <- ESEB02[ which(ESEB02$uf == "52"),]

BESGO$Party2 <- NA
BESGO$Party2[ BESGO$p14 == "AFRÊNIO GONÇALVES                                                                                                                                                                                                                                              "] <- "PSDB"
BESGO$Party2[ BESGO$p14 == "CARLA SANTILO                                                                                                                                                                                                                                                  "] <- "PSDB"
BESGO$Party2[ BESGO$p14 == "DARCI OCORSI                                                                                                                                                                                                                                                   "] <- "PTB"
BESGO$Party2[ BESGO$p14 == "JOSÉ ANTÔNIO                                                                                                                                                                                                                                                   "] <- "PL"
BESGO$Party2[ BESGO$p14 == "JOSÉ ANTÔNIO (DE S. MIGUEL DO ARAGUAIA)                                                                                                                                                                                                                        "] <- "PL"
BESGO$Party2[ BESGO$p14 == "JOSÉ LOPES                                                                                                                                                                                                                                                     "] <- "PSDB"
BESGO$Party2[ BESGO$p14 == "JOSÉ NARCÍSIO                                                                                                                                                                                                                                                  "] <- "PSDB"
BESGO$Party2[ BESGO$p14 == "KENEDY                                                                                                                                                                                                                                                         "] <- "PSB"
BESGO$Party2[ BESGO$p14 == "KENNEDY                                                                                                                                                                                                                                                        "] <- "PSB"
BESGO$Party2[ BESGO$p14 == "ROBERTO CURI                                                                                                                                                                                                                                                   "] <- "PPB"
BESGO$Party2[ BESGO$p14 == "SEBASTIÃO TEJOTA                                                                                                                                                                                                                                               "] <- "PSDB"
BESGO$Party2[ BESGO$p14 == "WALTER INÁCIO                                                                                                                                                                                                                                                  "] <- "PL"
BESGO$Party2[ BESGO$p14 == "WELLINGTON CAMARGO                                                                                                                                                                                                                                             "] <- "PSDB"
BESGO$Party2[ BESGO$p14 == "WOLNEY MARTINS                                                                                                                                                                                                                                                 "] <- "PPB"

# Maranhão
BESMA <- ESEB02[ which(ESEB02$uf == "21"),]

BESMA$Party2 <- NA
BESMA$Party2[ BESMA$p14 == "ARNALDO MELO                                                                                                                                                                                                                                                   "] <- "PFL"
BESMA$Party2[ BESMA$p14 == "CANDIDINUO                                                                                                                                                                                                                                                     "] <- "PSDB"
BESMA$Party2[ BESMA$p14 == "CANTIDIANO                                                                                                                                                                                                                                                     "] <- "PSDB"
BESMA$Party2[ BESMA$p14 == "CARLOS BRAID                                                                                                                                                                                                                                                   "] <- "PMDB"
BESMA$Party2[ BESMA$p14 == "DUTRA                                                                                                                                                                                                                                                          "] <- "PT"
BESMA$Party2[ BESMA$p14 == "HOLANDA                                                                                                                                                                                                                                                        "] <- "PSD"
BESMA$Party2[ BESMA$p14 == "JOAQUIM NAGIB                                                                                                                                                                                                                                                  "] <- "PTB"
BESMA$Party2[ BESMA$p14 == "LOURIVAL MENDES                                                                                                                                                                                                                                                "] <- "PTdoB"
BESMA$Party2[ BESMA$p14 == "LUÍS PEDRO                                                                                                                                                                                                                                                     "] <- "PDT"
BESMA$Party2[ BESMA$p14 == "MANOEL RIBEIRO                                                                                                                                                                                                                                                 "] <- "PSD"
BESMA$Party2[ BESMA$p14 == "MARLY ABDALLA                                                                                                                                                                                                                                                  "] <- "PFL"
BESMA$Party2[ BESMA$p14 == "NAGIB AYKEL                                                                                                                                                                                                                                                    "] <- "PTB"
BESMA$Party2[ BESMA$p14 == "NAGIB HAICKEL                                                                                                                                                                                                                                                  "] <- "PTB"
BESMA$Party2[ BESMA$p14 == "PEDRO VELOSO                                                                                                                                                                                                                                                   "] <- "PSDC"
BESMA$Party2[ BESMA$p14 == "PONTES AGUIAR                                                                                                                                                                                                                                                  "] <- "PSD"
BESMA$Party2[ BESMA$p14 == "STENIO                                                                                                                                                                                                                                                         "] <- "PSD"
BESMA$Party2[ BESMA$p14 == "STÊNIO                                                                                                                                                                                                                                                         "] <- "PSD"

# Minas Gerais
BESMG <- ESEB02[ which(ESEB02$uf == "31"),]

BESMG$Party2 <- NA
BESMG$Party2[ BESMG$p14 == "ADACLEVER                                                                                                                                                                                                                                                      "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "ADALCLEVER                                                                                                                                                                                                                                                     "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "ADAU CLÉVER                                                                                                                                                                                                                                                    "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "ADAUCLEVER                                                                                                                                                                                                                                                     "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "AGOSTINHO PATRUS                                                                                                                                                                                                                                               "] <- "PTB"
BESMG$Party2[ BESMG$p14 == "ALENCAR DA SILVEIRA                                                                                                                                                                                                                                            "] <- "PDT"
BESMG$Party2[ BESMG$p14 == "ALTAIR VILAR                                                                                                                                                                                                                                                   "] <- "PT"
BESMG$Party2[ BESMG$p14 == "AMBRÓSIO PINTO                                                                                                                                                                                                                                                 "] <- "PTB"
BESMG$Party2[ BESMG$p14 == "ANA MARIA                                                                                                                                                                                                                                                      "] <- "PSDB"
BESMG$Party2[ BESMG$p14 == "ANDERSON MAURÃO                                                                                                                                                                                                                                                "] <- "PT"
BESMG$Party2[ BESMG$p14 == "ANDERSON MOURÃO                                                                                                                                                                                                                                                "] <- "PT"
BESMG$Party2[ BESMG$p14 == "ANTONIO ROBERTO                                                                                                                                                                                                                                                "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "BILAC PINTO                                                                                                                                                                                                                                                    "] <- "PFL"
BESMG$Party2[ BESMG$p14 == "CARLOS GOMES                                                                                                                                                                                                                                                   "] <- "PT"
BESMG$Party2[ BESMG$p14 == "CARLOS PIMENTA                                                                                                                                                                                                                                                 "] <- "PPS"
BESMG$Party2[ BESMG$p14 == "CECÍLIA                                                                                                                                                                                                                                                        "] <- "PT"
BESMG$Party2[ BESMG$p14 == "CECÍLIA FERRAMENTA                                                                                                                                                                                                                                             "] <- "PT"
BESMG$Party2[ BESMG$p14 == "DIMAR FABIANO                                                                                                                                                                                                                                                  "] <- "PPB"
BESMG$Party2[ BESMG$p14 == "DIMAS FABIANO                                                                                                                                                                                                                                                  "] <- "PPB"
BESMG$Party2[ BESMG$p14 == "DIMAS RODRIGUES                                                                                                                                                                                                                                                "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "DINIS PINHEIRO                                                                                                                                                                                                                                                 "] <- "PL"
BESMG$Party2[ BESMG$p14 == "DINIZ PINHEIRO                                                                                                                                                                                                                                                 "] <- "PL"
BESMG$Party2[ BESMG$p14 == "DINIZ PINHEIRO 22141                                                                                                                                                                                                                                           "] <- "PL"
BESMG$Party2[ BESMG$p14 == "DJALMA                                                                                                                                                                                                                                                         "] <- "PSDB"
BESMG$Party2[ BESMG$p14 == "DR. ANTONIO ROBERTO                                                                                                                                                                                                                                            "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "DR. LAÉRCIO                                                                                                                                                                                                                                                    "] <- "PT"
BESMG$Party2[ BESMG$p14 == "DR. LEOPOLDO (MÉDICO CARDIOLOGISTA - DIVINÓPOLIS)                                                                                                                                                                                                              "] <- "PFL"
BESMG$Party2[ BESMG$p14 == "DR. LEOPOLDO GRECO                                                                                                                                                                                                                                             "] <- "PFL"
BESMG$Party2[ BESMG$p14 == "DR. RONALDO DE SOUSA                                                                                                                                                                                                                                           "] <- "PL"
BESMG$Party2[ BESMG$p14 == "DURVAL ANGELO                                                                                                                                                                                                                                                  "] <- "PT"
BESMG$Party2[ BESMG$p14 == "ELBE BRANDÃO                                                                                                                                                                                                                                                   "] <- "PSDB"
BESMG$Party2[ BESMG$p14 == "EUGÊNIO MANSUR                                                                                                                                                                                                                                                 "] <- "PPS"
BESMG$Party2[ BESMG$p14 == "EVAIR NOGUEIRA                                                                                                                                                                                                                                                 "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "EVAIS NOGUEIRA                                                                                                                                                                                                                                                 "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "FÁBIO AVELA                                                                                                                                                                                                                                                    "] <- "PTB"
BESMG$Party2[ BESMG$p14 == "IVAIR NOGUEIRA                                                                                                                                                                                                                                                 "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "JOÃO CESAR                                                                                                                                                                                                                                                     "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "JOÃO CÉSAR                                                                                                                                                                                                                                                     "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "JOÃO PAULO                                                                                                                                                                                                                                                     "] <- "PPB"
BESMG$Party2[ BESMG$p14 == "JOSÉ HENRIQUE                                                                                                                                                                                                                                                  "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "JOSE MAIA                                                                                                                                                                                                                                                      "] <- "PSDB"
BESMG$Party2[ BESMG$p14 == "LEONARDO MOREIRA                                                                                                                                                                                                                                               "] <- "PL"
BESMG$Party2[ BESMG$p14 == "LÚCIA CACÍFICO                                                                                                                                                                                                                                                 "] <- "PTB"
BESMG$Party2[ BESMG$p14 == "LÚCIA PACÍFICO (BH) FAMÍLIA DE STO. ANT.                                                                                                                                                                                                                       "] <- "PTB"
BESMG$Party2[ BESMG$p14 == "LUÍS VIEIRA                                                                                                                                                                                                                                                    "] <- "PRTB"
BESMG$Party2[ BESMG$p14 == "LUIZ TADEU LEITE                                                                                                                                                                                                                                               "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "MARAI TEREZA LARA                                                                                                                                                                                                                                              "] <- "PT"
BESMG$Party2[ BESMG$p14 == "MÁRCIO CUNHA                                                                                                                                                                                                                                                   "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "MARIA OLÍVIA                                                                                                                                                                                                                                                   "] <- "PSDB"
BESMG$Party2[ BESMG$p14 == "MARIA TEREZA                                                                                                                                                                                                                                                   "] <- "PT"
BESMG$Party2[ BESMG$p14 == "MARÍLIA CAMPOS                                                                                                                                                                                                                                                 "] <- "PT"
BESMG$Party2[ BESMG$p14 == "MAURO LOBO                                                                                                                                                                                                                                                     "] <- "PSB"
BESMG$Party2[ BESMG$p14 == "MIGUEL MARTINE                                                                                                                                                                                                                                                 "] <- "PSB"
BESMG$Party2[ BESMG$p14 == "PAULO PEDRAS                                                                                                                                                                                                                                                   "] <- "PRTB"
BESMG$Party2[ BESMG$p14 == "RENÉ GUIMARÃES                                                                                                                                                                                                                                                 "] <- "PMDB"
BESMG$Party2[ BESMG$p14 == "ROBERTO DE CARVALHO                                                                                                                                                                                                                                            "] <- "PT"
BESMG$Party2[ BESMG$p14 == "SEBASTIÃO COSTA                                                                                                                                                                                                                                                "] <- "PFL"
BESMG$Party2[ BESMG$p14 == "SEBASTIÃO ELVÉCIO                                                                                                                                                                                                                                              "] <- "PDT"
BESMG$Party2[ BESMG$p14 == "SEBASTIÃO HELVÉCIO                                                                                                                                                                                                                                             "] <- "PDT"
BESMG$Party2[ BESMG$p14 == "TONINHO ANDRADA                                                                                                                                                                                                                                                "] <- "PSDB"
BESMG$Party2[ BESMG$p14 == "TONINHO PINHEIRO                                                                                                                                                                                                                                               "] <- "PL"

# Mato Grosso do Sul
BESMS <- ESEB02[ which(ESEB02$uf == "50"),]

BESMS$Party2 <- NA
BESMS$Party2[ BESMS$p14 == "ALUÍSIO BORGES                                                                                                                                                                                                                                                 "] <- "PPB"
BESMS$Party2[ BESMS$p14 == "GERSON DOMINGOS                                                                                                                                                                                                                                                "] <- "PSL"
BESMS$Party2[ BESMS$p14 == "MELO IRMÃO                                                                                                                                                                                                                                                     "] <- "PMN"
BESMS$Party2[ BESMS$p14 == "SILVEIRA                                                                                                                                                                                                                                                       "] <- "PSDB"
BESMS$Party2[ BESMS$p14== "SIMONE                                                                                                                                                                                                                                                         "] <- "PMDB"
BESMS$Party2[ BESMS$p14 == "SIMONE TEBET                                                                                                                                                                                                                                                   "] <- "PMDB"

# Mato Grosso
BESMT <- ESEB02[ which(ESEB02$uf == "51"),]

BESMT$Party2 <- NA
BESMT$Party2[ BESMT$p14 == "ANA CARLA                                                                                                                                                                                                                                                      "] <- "PPS"
BESMT$Party2[ BESMT$p14 == "ANA CARLA MUNIZ                                                                                                                                                                                                                                                "] <- "PPS"
BESMT$Party2[ BESMT$p14 == "APARECIDO ALVES                                                                                                                                                                                                                                                "] <- "PSDB"
BESMT$Party2[ BESMT$p14 == "CARLA MUNIZ                                                                                                                                                                                                                                                    "] <- "PPS"
BESMT$Party2[ BESMT$p14 == "CELIO TIBES                                                                                                                                                                                                                                                    "] <- "PSB"
BESMT$Party2[ BESMT$p14 == "CELIO TIBES                                                                                                                                                                                                                                                    "] <- "PSB"
BESMT$Party2[ BESMT$p14 == "CLÓVIS ROBERTO                                                                                                                                                                                                                                                 "] <- "PSDB"
BESMT$Party2[ BESMT$p14 == "CLÓVIS ROBERTO                                                                                                                                                                                                                                                 "] <- "PSDB"
BESMT$Party2[ BESMT$p14 == "EDMILSON PRATES                                                                                                                                                                                                                                                "] <- "PTB"
BESMT$Party2[ BESMT$p14 == "J EMILIO BARRETO                                                                                                                                                                                                                                               "] <- "PL"
BESMT$Party2[ BESMT$p14 == "J. BARRETO                                                                                                                                                                                                                                                     "] <- "PL"
BESMT$Party2[ BESMT$p14 == "JOÃO MALHEIROS                                                                                                                                                                                                                                                 "] <- "PPS"
BESMT$Party2[ BESMT$p14 == "JOAQUIM CURVO                                                                                                                                                                                                                                                  "] <- "PPS"
BESMT$Party2[ BESMT$p14 == "JOSÉ CARLOS DE FREIRE                                                                                                                                                                                                                                          "] <- "PMDB"
BESMT$Party2[ BESMT$p14 == "SEBASTIÃO RESENDE                                                                                                                                                                                                                                              "] <- "PTB"

# Pará
BESPA <- ESEB02[ which(ESEB02$uf == "15"),]

BESPA$Party2 <- NA
BESPA$Party2[ BESPA$p14 == "ANA CUNHA                                                                                                                                                                                                                                                      "] <- "PMDB"
BESPA$Party2[ BESPA$p14 == "HERDER BARBALHO                                                                                                                                                                                                                                                "] <- "PMDB"
BESPA$Party2[ BESPA$p14 == "HERDER BARBALHO                                                                                                                                                                                                                                                "] <- "PMDB"
BESPA$Party2[ BESPA$p14 == "IRAN LIMA                                                                                                                                                                                                                                                      "] <- "PTB"
BESPA$Party2[ BESPA$p14 == "MÁRIO CORRÊA                                                                                                                                                                                                                                                   "] <- "PSDB"

# Paraná
BESPR <- ESEB02[ which(ESEB02$uf == "41"),]

BESPR$Party2 <- NA
BESPR$Party2[ BESPR$p14 == "ANI BELI                                                                                                                                                                                                                                                       "] <- "PMDB"
BESPR$Party2[ BESPR$p14 == "ANIBELI                                                                                                                                                                                                                                                        "] <- "PMDB"
BESPR$Party2[ BESPR$p14 == "ANIBELLI                                                                                                                                                                                                                                                       "] <- "PMDB"
BESPR$Party2[ BESPR$p14 == "AUGUSTINHO ZICHI                                                                                                                                                                                                                                               "] <- "PDT"
BESPR$Party2[ BESPR$p14 == "BARBOSA NETO                                                                                                                                                                                                                                                   "] <- "PDT"
BESPR$Party2[ BESPR$p14 == "CAITO QUINTANA                                                                                                                                                                                                                                                 "] <- "PMDB"
BESPR$Party2[ BESPR$p14 == "CANTINI                                                                                                                                                                                                                                                        "] <- "PPS"
BESPR$Party2[ BESPR$p14 == "CARLOS TORTATO                                                                                                                                                                                                                                                 "] <- "PT"
BESPR$Party2[ BESPR$p14 == "CARTARI                                                                                                                                                                                                                                                        "] <- "PSL"
BESPR$Party2[ BESPR$p14 == "CARTÁRIO                                                                                                                                                                                                                                                       "] <- "PSL"
BESPR$Party2[ BESPR$p14 == "EDSON PRATIZIQUE                                                                                                                                                                                                                                               "] <- "PL"
BESPR$Party2[ BESPR$p14 == "ELI GUELERI (FOS DO IGUAÇU)                                                                                                                                                                                                                                    "] <- "PDT"
BESPR$Party2[ BESPR$p14 == "GUILHERME CAROLO                                                                                                                                                                                                                                               "] <- "PSB"
BESPR$Party2[ BESPR$p14 == "HERMAS BRANDÃO                                                                                                                                                                                                                                                 "] <- "PSDB"
BESPR$Party2[ BESPR$p14 == "HERMES BRANDÃO                                                                                                                                                                                                                                                 "] <- "PSDB"
BESPR$Party2[ BESPR$p14 == "IVANA GUERRA                                                                                                                                                                                                                                                   "] <- "PFL"
BESPR$Party2[ BESPR$p14 == "IVANIO GUERRA                                                                                                                                                                                                                                                  "] <- "PFL"
BESPR$Party2[ BESPR$p14 == "JOEL COIMBRA                                                                                                                                                                                                                                                   "] <- "PTB"
BESPR$Party2[ BESPR$p14 == "LEGENDA (PT)                                                                                                                                                                                                                                                   "] <- "PT"
BESPR$Party2[ BESPR$p14 == "LEGENDA DO PT                                                                                                                                                                                                                                                  "] <- "PT"
BESPR$Party2[ BESPR$p14 == "MARCELO PIRES                                                                                                                                                                                                                                                  "] <- "PDT"
BESPR$Party2[ BESPR$p14 == "MOZART LOPES                                                                                                                                                                                                                                                   "] <- "PDT"
BESPR$Party2[ BESPR$p14 == "NELSON GARCIA                                                                                                                                                                                                                                                  "] <- "PFL"
BESPR$Party2[ BESPR$p14 == "NELSON JUSTUS                                                                                                                                                                                                                                                  "] <- "PFL"
BESPR$Party2[ BESPR$p14 == "NEREU MOURA                                                                                                                                                                                                                                                    "] <- "PMDB"
BESPR$Party2[ BESPR$p14 == "PT                                                                                                                                                                                                                                                             "] <- "PT"
BESPR$Party2[ BESPR$p14 == "RICARDO MAIA                                                                                                                                                                                                                                                   "] <- "PSDB"
BESPR$Party2[ BESPR$p14 == "ROSSONI                                                                                                                                                                                                                                                        "] <- "PTB"
BESPR$Party2[ BESPR$p14 == "SATIO KAYUKANA                                                                                                                                                                                                                                                 "] <- "PFL"
BESPR$Party2[ BESPR$p14 == "ZUCCHI                                                                                                                                                                                                                                                         "] <- "PDT"
BESPR$Party2[ BESPR$p14 == "ZUCCKE                                                                                                                                                                                                                                                         "] <- "PDT"
BESPR$Party2[ BESPR$p14 == "ZUCHI                                                                                                                                                                                                                                                          "] <- "PDT"
BESPR$Party2[ BESPR$p14 == "ZUQUE                                                                                                                                                                                                                                                          "] <- "PDT"
BESPR$Party2[ BESPR$p14 == "ZUQUE (12345)                                                                                                                                                                                                                                                  "] <- "PDT"

# Paraíba
BESPB <- ESEB02[ which(ESEB02$uf == "25"),]

BESPB$Party2 <- NA
BESPB$Party2[ BESPB$p14 == "ANTÔNIO PEREIRA                                                                                                                                                                                                                                                "] <- "PSDB"
BESPB$Party2[ BESPB$p14 == "ESTEFANIA                                                                                                                                                                                                                                                      "] <- "PMDB"
BESPB$Party2[ BESPB$p14 == "FABIANO LUCENA                                                                                                                                                                                                                                                 "] <- "PSB"
BESPB$Party2[ BESPB$p14 == "FÁBIO NOGUEIRA                                                                                                                                                                                                                                                 "] <- "PSDB"
BESPB$Party2[ BESPB$p14 == "GILVAN FREIRE                                                                                                                                                                                                                                                  "] <- "PSB"
BESPB$Party2[ BESPB$p14 == "JOÃO PAULO                                                                                                                                                                                                                                                     \001"] <- "PFL"
BESPB$Party2[ BESPB$p14 == "JÚLIO RAFAEL                                                                                                                                                                                                                                                   \001"] <- "PT"
BESPB$Party2[ BESPB$p14 == "OLENKA MARANHÃO                                                                                                                                                                                                                                                "] <- "PMDB"
BESPB$Party2[ BESPB$p14 == "RICARDO COUTINHO                                                                                                                                                                                                                                               \001"] <- "PT"
BESPB$Party2[ BESPB$p14 == "RODRIGO                                                                                                                                                                                                                                                        "] <- "PT"
BESPB$Party2[ BESPB$p14 == "RODRIGO DO LAR                                                                                                                                                                                                                                                 "] <- "PT"
BESPB$Party2[ BESPB$p14 == "RÔMULO                                                                                                                                                                                                                                                         "] <- "PSDB"
BESPB$Party2[ BESPB$p14 == "ROMULO GOUVEIA                                                                                                                                                                                                                                                 "] <- "PSDB"
BESPB$Party2[ BESPB$p14 == "RÔMULO GOUVEIA                                                                                                                                                                                                                                                 "] <- "PSDB"
BESPB$Party2[ BESPB$p14 == "RÔMULO GOUVEIA                                                                                                                                                                                                                                                 \001"] <- "PSDB"
BESPB$Party2[ BESPB$p14 == "WALDECIR AMORIM                                                                                                                                                                                                                                                "] <- "PFL"
BESPB$Party2[ BESPB$p14 == "WALTER BRITO                                                                                                                                                                                                                                                   "] <- "PMDB"

# Pernambuco
BESPE <- ESEB02[ which(ESEB02$uf == "26"),]

BESPE$Party2 <- NA
BESPE$Party2[ BESPE$p14 == "AFONSO AUGUSTO                                                                                                                                                                                                                                                 "] <- "PSDB"
BESPE$Party2[ BESPE$p14 == "AFONSO FERRAZ                                                                                                                                                                                                                                                  "] <- "PSDB"
BESPE$Party2[ BESPE$p14 == "AUGUSTO CÉSAR                                                                                                                                                                                                                                                  "] <- "PSDB"
BESPE$Party2[ BESPE$p14 == "AGRAILSON JUNIOR                                                                                                                                                                                                                                               "] <- "PSB"
BESPE$Party2[ BESPE$p14 == "BRUNO ARAUJO                                                                                                                                                                                                                                                   "] <- "PSDB"
BESPE$Party2[ BESPE$p14 == "BRUNO RODRIGUES                                                                                                                                                                                                                                                "] <- "PPB"
BESPE$Party2[ BESPE$p14 == "DILMA LINS                                                                                                                                                                                                                                                     "] <- "PL"
BESPE$Party2[ BESPE$p14 == "DJALMA SEIXA                                                                                                                                                                                                                                                   "] <- "PSB"
BESPE$Party2[ BESPE$p14 == "EDUARDO MELO                                                                                                                                                                                                                                                   "] <- "PSD"
BESPE$Party2[ BESPE$p14 == "FRANCISMA                                                                                                                                                                                                                                                      "] <- "PSB"
BESPE$Party2[ BESPE$p14 == "GERALDO MELO                                                                                                                                                                                                                                                   "] <- "PMDB"
BESPE$Party2[ BESPE$p14 == "JOÃO BRAGA                                                                                                                                                                                                                                                     "] <- "PV"
BESPE$Party2[ BESPE$p14 == "JOÃO LEMOS                                                                                                                                                                                                                                                     "] <- "PCdoB"
BESPE$Party2[ BESPE$p14 == "PASTOR VALDIR                                                                                                                                                                                                                                                  "] <- "PSD"
BESPE$Party2[ BESPE$p14 == "RAU HENRY                                                                                                                                                                                                                                                      "] <- "PMDB"
BESPE$Party2[ BESPE$p14 == "RICARDO TEOBALDO                                                                                                                                                                                                                                               "] <- "PMDB"
BESPE$Party2[ BESPE$p14 == "SEBASTIÃO JUNIOR                                                                                                                                                                                                                                               "] <- "PSD"
BESPE$Party2[ BESPE$p14 == "SEBASTIÃO OLIVEIRA                                                                                                                                                                                                                                             "] <- "PSD"
BESPE$Party2[ BESPE$p14 == "SILVIO COSTA                                                                                                                                                                                                                                                   "] <- "PSD"
BESPE$Party2[ BESPE$p14 == "TERTULIANO                                                                                                                                                                                                                                                     "] <- "PST"

# Piauí
BESPI <- ESEB02[ which(ESEB02$uf == "22"),]

BESPI$Party2 <- NA
BESPI$Party2[ BESPI$p14 == "ADALFO NUNES                                                                                                                                                                                                                                                   "] <- "PPB"
BESPI$Party2[ BESPI$p14 == "ADOLFO                                                                                                                                                                                                                                                         "] <- "PPB"
BESPI$Party2[ BESPI$p14 == "ADOLFO NUNES                                                                                                                                                                                                                                                   "] <- "PPB"
BESPI$Party2[ BESPI$p14 == "JOÃO DE DEUS                                                                                                                                                                                                                                                   "] <- "PT"
BESPI$Party2[ BESPI$p14 == "MARGARIDA BONA                                                                                                                                                                                                                                                 "] <- "PDT"
BESPI$Party2[ BESPI$p14 ==  "OLAVO REBELO                                                                                                                                                                                                                                                   "] <- "PCdoB"
BESPI$Party2[ BESPI$p14 ==  "RAIMUNDO LIMA                                                                                                                                                                                                                                                  "] <- "PV"
BESPI$Party2[ BESPI$p14 ==  "WILSON BRANDÃO                                                                                                                                                                                                                                                 "] <- "PFL"

# Rio de Janeiro
BESRJ <- ESEB02[ which(ESEB02$uf == "33"),]

BESRJ$Party2 <- NA
BESRJ$Party2[ BESRJ$p14 == "ADRAULTO PEIXOTO                                                                                                                                                                                                                                               "] <- "PSB"
BESRJ$Party2[ BESRJ$p14 == "ALBANO REIS                                                                                                                                                                                                                                                    "] <- "PMDB"
BESRJ$Party2[ BESRJ$p14 == "ALEXANDRE FURLANETO                                                                                                                                                                                                                                            "] <- "PSB"
BESRJ$Party2[ BESRJ$p14 == "ALGUÉM DO PT (NÃO FOI LEGENDA)                                                                                                                                                                                                                                 "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "ALGUM CANDIDATO DO PT NÃO LEMBRA O NOME                                                                                                                                                                                                                        "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "ANDRÉ DO PV                                                                                                                                                                                                                                                    "] <- "PV"
BESRJ$Party2[ BESRJ$p14 == "ANDRÉ DO PV                                                                                                                                                                                                                                                    "] <- "PV"
BESRJ$Party2[ BESRJ$p14 == "ANTÔNIO CARLOS RANGEL                                                                                                                                                                                                                                          "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "ARTHUR MESSIAS                                                                                                                                                                                                                                                 "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "ARY BRUM                                                                                                                                                                                                                                                       "] <- "PMDB"
BESRJ$Party2[ BESRJ$p14 == "ÁTILA NUNES                                                                                                                                                                                                                                                    "] <- "PMDB"
BESRJ$Party2[ BESRJ$p14 == "BISPO CAETANO                                                                                                                                                                                                                                                  "] <- "PL"
BESRJ$Party2[ BESRJ$p14 == "BISPO JODENIR                                                                                                                                                                                                                                                  "] <- "PTdoB"
BESRJ$Party2[ BESRJ$p14 == "CANDIDATO DO PARTIDO DO LULA                                                                                                                                                                                                                                   "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "CARLOS MINC                                                                                                                                                                                                                                                    "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "DIOGO CUOCO                                                                                                                                                                                                                                                    "] <- "PMDB"
BESRJ$Party2[ BESRJ$p14 == "DR. JUVANDI                                                                                                                                                                                                                                                    "] <- "PRONA"
BESRJ$Party2[ BESRJ$p14 == "EDSON ALBERTASSI                                                                                                                                                                                                                                               "] <- "PSB"
BESRJ$Party2[ BESRJ$p14 == "EDSON ALBERTAZ                                                                                                                                                                                                                                                 "] <- "PSB"
BESRJ$Party2[ BESRJ$p14 == "EIDER DANTAS                                                                                                                                                                                                                                                   "] <- "PFL"
BESRJ$Party2[ BESRJ$p14 == "FÁBIO SILVA                                                                                                                                                                                                                                                    "] <- "PPB"
BESRJ$Party2[ BESRJ$p14 == "FAUSTO ALVES                                                                                                                                                                                                                                                   "] <- "PSC"
BESRJ$Party2[ BESRJ$p14 == "FAUSTO ALVES 20456                                                                                                                                                                                                                                             "] <- "PSC"
BESRJ$Party2[ BESRJ$p14 == "FAUSTO ALVES 2456                                                                                                                                                                                                                                              "] <- "PSC"
BESRJ$Party2[ BESRJ$p14 == "FLAVIO BOLSONARO                                                                                                                                                                                                                                               "] <- "PPB"
BESRJ$Party2[ BESRJ$p14 == "FLÁVIO BOLSONARO                                                                                                                                                                                                                                               "] <- "PPB"
BESRJ$Party2[ BESRJ$p14 == "GALLO                                                                                                                                                                                                                                                          "] <- "PMDB"
BESRJ$Party2[ BESRJ$p14 == "GILBERTO PALMEIRAS                                                                                                                                                                                                                                             "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "GILBERTO PALMEIRAS                                                                                                                                                                                                                                             "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "GILBERTO SALARINE                                                                                                                                                                                                                                              "] <- "PPB"
BESRJ$Party2[ BESRJ$p14 == "GRAÇA PEREIRA                                                                                                                                                                                                                                                  "] <- "PTdoB"
BESRJ$Party2[ BESRJ$p14 == "JOÃO PEIXOTO                                                                                                                                                                                                                                                   "] <- "PSB"
BESRJ$Party2[ BESRJ$p14 == "JORGE MANTILHA                                                                                                                                                                                                                                                 "] <- "PV"
BESRJ$Party2[ BESRJ$p14 == "JOSÉ CLAUDIO                                                                                                                                                                                                                                                   "] <- "PSB"
BESRJ$Party2[ BESRJ$p14 == "LUIS GOMES                                                                                                                                                                                                                                                     "] <- "PFL"
BESRJ$Party2[ BESRJ$p14 == "LULA RODRIGUES (PPS)                                                                                                                                                                                                                                           "] <- "PPS"
BESRJ$Party2[ BESRJ$p14 == "MARCIO CORRÊA                                                                                                                                                                                                                                                  "] <- "PSB"
BESRJ$Party2[ BESRJ$p14 == "MÁRCIO CORREA                                                                                                                                                                                                                                                  "] <- "PSB"
BESRJ$Party2[ BESRJ$p14 == "MARCIO GUIMARÃES                                                                                                                                                                                                                                               "] <- "PDT"
BESRJ$Party2[ BESRJ$p14 == "MÁRCIO GUIMARÃES                                                                                                                                                                                                                                               "] <- "PDT"
BESRJ$Party2[ BESRJ$p14 == "MARCIO GUMARÃES                                                                                                                                                                                                                                                "] <- "PDT"
BESRJ$Party2[ BESRJ$p14 == "MARIO RAMOS                                                                                                                                                                                                                                                    "] <- "PGT"
BESRJ$Party2[ BESRJ$p14 == "NATALIN JOSÉ                                                                                                                                                                                                                                                   "] <- "PFL"
BESRJ$Party2[ BESRJ$p14 == "NATALINA 25234                                                                                                                                                                                                                                                 "] <- "PFL"
BESRJ$Party2[ BESRJ$p14 == "NATALINO                                                                                                                                                                                                                                                       "] <- "PFL"
BESRJ$Party2[ BESRJ$p14 == "NOEL DE CARVALHO 40280                                                                                                                                                                                                                                         "] <- "PSB"
BESRJ$Party2[ BESRJ$p14 == "OTÁVIO LEITE                                                                                                                                                                                                                                                   "] <- "PSDB"
BESRJ$Party2[ BESRJ$p14 == "OTAVIOMLEITE 45678                                                                                                                                                                                                                                             "] <- "PSDB"
BESRJ$Party2[ BESRJ$p14 == "PASTOR EBER SILVA                                                                                                                                                                                                                                              "] <- "PST"
BESRJ$Party2[ BESRJ$p14 == "PASTOR ÉBER SILVA                                                                                                                                                                                                                                              "] <- "PST"
BESRJ$Party2[ BESRJ$p14 == "PAULO CÉSAR                                                                                                                                                                                                                                                    "] <- "PFL"
BESRJ$Party2[ BESRJ$p14 == "PAULO MAFRA                                                                                                                                                                                                                                                    "] <- "PSB"
BESRJ$Party2[ BESRJ$p14 == "PAULO PINHEIRO                                                                                                                                                                                                                                                 "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "PAULO SÉRGIO                                                                                                                                                                                                                                                   "] <- "PDT"
BESRJ$Party2[ BESRJ$p14 == "PT (VOTOU NA LEGENDA DO PT)                                                                                                                                                                                                                                    "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "PT (VOTOU NA LEGENDA)                                                                                                                                                                                                                                          "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "RENATO DE JESUS                                                                                                                                                                                                                                                "] <- "PMDB"
BESRJ$Party2[ BESRJ$p14 == "ROBERTO BRAGA                                                                                                                                                                                                                                                  "] <- "PSB"
BESRJ$Party2[ BESRJ$p14 == "ROBERTO DINAMITE                                                                                                                                                                                                                                               "] <- "PMDB"
BESRJ$Party2[ BESRJ$p14 == "ROBERTO RAUL VAGNER                                                                                                                                                                                                                                            "] <- "PHS"
BESRJ$Party2[ BESRJ$p14 == "RODRIGO (PT)                                                                                                                                                                                                                                                   "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "RODRIGO DO PT                                                                                                                                                                                                                                                  "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "SALARINI                                                                                                                                                                                                                                                       "] <- "PPB"
BESRJ$Party2[ BESRJ$p14 == "SONELI                                                                                                                                                                                                                                                         "] <- "PT"
BESRJ$Party2[ BESRJ$p14 == "VERÔNICA COSTA                                                                                                                                                                                                                                                 "] <- "PL"
BESRJ$Party2[ BESRJ$p14 == "WASCHITON REIS                                                                                                                                                                                                                                                 "] <- "PMDB"

# Rio Grande do Norte
BESRN <- ESEB02[ which(ESEB02$uf == "24"),]

BESRN$Party2 <- NA
BESRN$Party2[ BESRN$p14 == "BRANCO                                                                                                                                                                                                                                                         "] <- "PSC"
BESRN$Party2[ BESRN$p14 == "JOACYR PASCOAL                                                                                                                                                                                                                                                 "] <- "PPB"
BESRN$Party2[ BESRN$p14 == "LUIZ ALMIR                                                                                                                                                                                                                                                     "] <- "PPB"
BESRN$Party2[ BESRN$p14 == "LUIZ ALMIR                                                                                                                                                                                                                                                     \001"] <- "PPB"
BESRN$Party2[ BESRN$p14 == "MARCIA MAIA                                                                                                                                                                                                                                                    "] <- "PSB"
BESRN$Party2[ BESRN$p14 == "RAIMUNDO PINHEIRO                                                                                                                                                                                                                                              \001"] <- "PST"
BESRN$Party2[ BESRN$p14 == "RUTH CIARLINI                                                                                                                                                                                                                                                  "] <- "PFL"
BESRN$Party2[ BESRN$p14 == "VIVALDO COSTA                                                                                                                                                                                                                                                  "] <- "PL"
BESRN$Party2[ BESRN$p14 == "VIVALDO COSTA                                                                                                                                                                                                                                                  \001"] <- "PL"

# Rondônia
BESRO <- ESEB02[ which(ESEB02$uf == "11"),]

BESRO$Party2 <- NA
BESRO$Party2[ BESRO$p14 == "HELLEN RUTH                                                                                                                                                                                                                                                    "] <- "PSDB"

# Roraima
BESRR <- ESEB02[ which(ESEB02$uf == "14"),]

BESRR$Party2 <- NA
#Here, unfortunately, there existed no valid responses on voting intentions.

# Rio Grande do Sul
BESRS <- ESEB02[ which(ESEB02$uf == "43"),]

BESRS$Party2 <- NA
BESRS$Party2[ BESRS$p14 == "13 CANDIDATO DO PARTIDO                                                                                                                                                                                                                                        "] <- "PT"
BESRS$Party2[ BESRS$p14 == "13 PARTIDO DO CANDIDATO                                                                                                                                                                                                                                        "] <- "PT"
BESRS$Party2[ BESRS$p14 == "13 SÓ LEMBRA O PARTIDO DO CANDIDATO                                                                                                                                                                                                                            "] <- "PT"
BESRS$Party2[ BESRS$p14 == "40 LEGENDA                                                                                                                                                                                                                                                     "] <- "PSB"
BESRS$Party2[ BESRS$p14 == "ABILIO ALVES DOS SANTOS                                                                                                                                                                                                                                        "] <- "PTB"
BESRS$Party2[ BESRS$p14 == "ABÍLIO ALVES DOS SANTOS                                                                                                                                                                                                                                        "] <- "PTB"
BESRS$Party2[ BESRS$p14 == "ADOLFO BRITO                                                                                                                                                                                                                                                   "] <- "PPB"
BESRS$Party2[ BESRS$p14 == "ADOLFO BRITO PPB                                                                                                                                                                                                                                               "] <- "PPB"
BESRS$Party2[ BESRS$p14 == "ALAOR ( TRABALHA NA RÁDIO - PARTIDO DO RIGOTTO PMDB )                                                                                                                                                                                                          "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "ALAOR DE OLIVEIRA                                                                                                                                                                                                                                              "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "ALEXANDRE BRITTO                                                                                                                                                                                                                                               "] <- "PSDB"
BESRS$Party2[ BESRS$p14 == "ANTONIO CAMULERA                                                                                                                                                                                                                                               "] <- "PPB"
BESRS$Party2[ BESRS$p14 == "BERNARDO DE SOUZA                                                                                                                                                                                                                                              "] <- "PPS"
BESRS$Party2[ BESRS$p14 == "BIOLKI PMDB-FILHO                                                                                                                                                                                                                                              "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "CHERIN                                                                                                                                                                                                                                                         "] <- "PDT"
BESRS$Party2[ BESRS$p14 == "CHERINI - 12152                                                                                                                                                                                                                                                "] <- "PDT"
BESRS$Party2[ BESRS$p14 == "DÉO GOMES                                                                                                                                                                                                                                                      "] <- "PCdoB"
BESRS$Party2[ BESRS$p14 == "EDSON - PT                                                                                                                                                                                                                                                     "] <- "PT"
BESRS$Party2[ BESRS$p14 == "ELISEU SANTOS                                                                                                                                                                                                                                                  "] <- "PTB"
BESRS$Party2[ BESRS$p14 == "ELVINO BONGAZ (PT)                                                                                                                                                                                                                                             "] <- "PT"
BESRS$Party2[ BESRS$p14 == "ESTILAC                                                                                                                                                                                                                                                        "] <- "PT"
BESRS$Party2[ BESRS$p14 == "FLAVIO KOUTZI                                                                                                                                                                                                                                                  "] <- "PT"
BESRS$Party2[ BESRS$p14 == "FRANCISCO BECH                                                                                                                                                                                                                                                 "] <- "PFL"
BESRS$Party2[ BESRS$p14 == "GIOVANI SCHERINE                                                                                                                                                                                                                                               "] <- "PDT"
BESRS$Party2[ BESRS$p14 == "IRTON FELLER                                                                                                                                                                                                                                                   "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "JAIR SOARES                                                                                                                                                                                                                                                    "] <- "PPB"
BESRS$Party2[ BESRS$p14 == "JOÃO FISCHER                                                                                                                                                                                                                                                   "] <- "PPB"
BESRS$Party2[ BESRS$p14 == "JUSSARA CONY                                                                                                                                                                                                                                                   "] <- "PCdoB"
BESRS$Party2[ BESRS$p14 == "LARA                                                                                                                                                                                                                                                           "] <- "PTB"
BESRS$Party2[ BESRS$p14 == "LARA (14113)                                                                                                                                                                                                                                                   "] <- "PTB"
BESRS$Party2[ BESRS$p14 == "LEGENDA DO PT                                                                                                                                                                                                                                                  "] <- "PT"
BESRS$Party2[ BESRS$p14 == "LUÍZ FERNANDO ZACHIA                                                                                                                                                                                                                                           "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "MANOEL NEI                                                                                                                                                                                                                                                     "] <- "PPB"
BESRS$Party2[ BESRS$p14 == "MANUEL NEI                                                                                                                                                                                                                                                     "] <- "PPB"
BESRS$Party2[ BESRS$p14 == "MARCO ALBA                                                                                                                                                                                                                                                     "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "MARCOS ALBA                                                                                                                                                                                                                                                    "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "MARIA HELENA (PMDB)                                                                                                                                                                                                                                            "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "MARIA HELENA (PMDB)                                                                                                                                                                                                                                            "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "MARIA HELENA SARTORE-PMDB                                                                                                                                                                                                                                      "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "MARIA HELENA SARTORI - PMDB                                                                                                                                                                                                                                    "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "MARIA HELENA SARTORI                                                                                                                                                                                                                                           "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "MARIO BERND                                                                                                                                                                                                                                                    "] <- "PPS"
BESRS$Party2[ BESRS$p14 == "NELSON (P.V.)                                                                                                                                                                                                                                                  "] <- "PV"
BESRS$Party2[ BESRS$p14 == "OSMAR SEVERO                                                                                                                                                                                                                                                   "] <- "PTB"
BESRS$Party2[ BESRS$p14 == "PAULO AZEREDO                                                                                                                                                                                                                                                  "] <- "PDT"
BESRS$Party2[ BESRS$p14 == "PIMENTA PT                                                                                                                                                                                                                                                     "] <- "PT"
BESRS$Party2[ BESRS$p14 == "POSTAL 15150                                                                                                                                                                                                                                                   "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "PT NÃO LEMBRA O NOME                                                                                                                                                                                                                                           "] <- "PT"
BESRS$Party2[ BESRS$p14 == "RAUL PORT                                                                                                                                                                                                                                                      "] <- "PT"
BESRS$Party2[ BESRS$p14 == "REGINALDO PUJOL                                                                                                                                                                                                                                                "] <- "PFL"
BESRS$Party2[ BESRS$p14 == "REI PAULETT                                                                                                                                                                                                                                                    "] <- "PSDB"
BESRS$Party2[ BESRS$p14 == "ROQUE GRACIOTIM-PT                                                                                                                                                                                                                                             "] <- "PT"
BESRS$Party2[ BESRS$p14 == "ROQUE GRAZIOTIM-PT                                                                                                                                                                                                                                             "] <- "PT"
BESRS$Party2[ BESRS$p14 == "SEBASTIÃO MELO                                                                                                                                                                                                                                                 "] <- "PMDB"
BESRS$Party2[ BESRS$p14 == "SÉRGIO STAZINSKI                                                                                                                                                                                                                                               "] <- "PT"
BESRS$Party2[ BESRS$p14 == "VAINER MACHADO                                                                                                                                                                                                                                                 "] <- "PSB"
BESRS$Party2[ BESRS$p14 == "VÂNIA MACHADO (PT)                                                                                                                                                                                                                                             "] <- "PSB"
BESRS$Party2[ BESRS$p14 == "VILSON COVATTI                                                                                                                                                                                                                                                 "] <- "PPB"
BESRS$Party2[ BESRS$p14 == "VITOR HUGO - PT                                                                                                                                                                                                                                                "] <- "PT"
BESRS$Party2[ BESRS$p14 == "WAYNER (40444)                                                                                                                                                                                                                                                 "] <- "PSB"

# Santa Catarina
BESSC <- ESEB02[ which(ESEB02$uf == "42"),]

BESSC$Party2 <- NA
BESSC$Party2[ BESSC$p14 == "ANDRINO                                                                                                                                                                                                                                                        "] <- "PFL"
BESSC$Party2[ BESSC$p14 == "ANTONIO CEROMN                                                                                                                                                                                                                                                 "] <- "PFL"
BESSC$Party2[ BESSC$p14 == "CERON                                                                                                                                                                                                                                                          "] <- "PFL"
BESSC$Party2[ BESSC$p14 == "CÉSAR SOUZA                                                                                                                                                                                                                                                    "] <- "PFL"
BESSC$Party2[ BESSC$p14 == "DJALMA 25639                                                                                                                                                                                                                                                   "] <- "PFL"
BESSC$Party2[ BESSC$p14 == "DJALMA BERG                                                                                                                                                                                                                                                    "] <- "PFL"
BESSC$Party2[ BESSC$p14 == "DJALMA BERGER                                                                                                                                                                                                                                                  "] <- "PFL"
BESSC$Party2[ BESSC$p14 == "DR. CÉSAR 13100                                                                                                                                                                                                                                                "] <- "PT"
BESSC$Party2[ BESSC$p14 == "DUDUCO PFL                                                                                                                                                                                                                                                     "] <- "PFL"
BESSC$Party2[ BESSC$p14 == "GILMAR                                                                                                                                                                                                                                                         "] <- "PPB"
BESSC$Party2[ BESSC$p14 == "GODINHO                                                                                                                                                                                                                                                        "] <- "PTB"
BESSC$Party2[ BESSC$p14 == "JOÃO HENRIQUE BLASI                                                                                                                                                                                                                                            "] <- "PMDB"
BESSC$Party2[ BESSC$p14 == "JOÃO PAULO KLEINUBING                                                                                                                                                                                                                                          "] <- "PFL"
BESSC$Party2[ BESSC$p14 == "JOSE PAULO SERAFIM                                                                                                                                                                                                                                             "] <- "PT"
BESSC$Party2[ BESSC$p14 == "LUCI PT                                                                                                                                                                                                                                                        "] <- "PT"
BESSC$Party2[ BESSC$p14 == "LUCI PT 13130                                                                                                                                                                                                                                                  "] <- "PT"
BESSC$Party2[ BESSC$p14 == "NELSON GOETHE                                                                                                                                                                                                                                                  "] <- "PPB"
BESSC$Party2[ BESSC$p14 == "ONOFRE                                                                                                                                                                                                                                                         "] <- "PFL"
BESSC$Party2[ BESSC$p14 == "ONOFRE AGOSTINHO                                                                                                                                                                                                                                               "] <- "PFL"
BESSC$Party2[ BESSC$p14 == "ONOFRE SANTO AUGUSTINHO                                                                                                                                                                                                                                        "] <- "PFL"
BESSC$Party2[ BESSC$p14 == "PARIZOTO 14104 PTB                                                                                                                                                                                                                                             "] <- "PTB"
BESSC$Party2[ BESSC$p14 == "SANDRO PMDB (15011)                                                                                                                                                                                                                                            "] <- "PMDB"
BESSC$Party2[ BESSC$p14 == "SANDRO TARZAN 11992                                                                                                                                                                                                                                            "] <- "PPB"
BESSC$Party2[ BESSC$p14 == "SERGIO GODINHO                                                                                                                                                                                                                                                 "] <- "PTB"
BESSC$Party2[ BESSC$p14 == "SÉRGIO GODINHO                                                                                                                                                                                                                                                 "] <- "PTB"

# Sergipe
BESSE <- ESEB02[ which(ESEB02$uf == "28"),]

BESSE$Party2 <- NA
BESSE$Party2[ BESSE$p14 == "ADELSON BARRETO                                                                                                                                                                                                                                                "] <- "PMN"
BESSE$Party2[ BESSE$p14 == "ANA LUCIA                                                                                                                                                                                                                                                      "] <- "PT"
BESSE$Party2[ BESSE$p14 == "ANA LÚCIA                                                                                                                                                                                                                                                      "] <- "PT"
BESSE$Party2[ BESSE$p14 == "ANTÔNIO DOS SANTOS                                                                                                                                                                                                                                             "] <- "PV"
BESSE$Party2[ BESSE$p14 == "DOMINGOS                                                                                                                                                                                                                                                       "] <- "PT"
BESSE$Party2[ BESSE$p14 == "MARIETA                                                                                                                                                                                                                                                        "] <- "PMN"
BESSE$Party2[ BESSE$p14 == "PASCOAL NABUCO                                                                                                                                                                                                                                                 "] <- "PSB"
BESSE$Party2[ BESSE$p14 == "ZÉ MILTON                                                                                                                                                                                                                                                      "] <- "PMN"

# São Paulo
BESSP <- ESEB02[ which(ESEB02$uf == "35"),]

BESSP$Party2 <- NA
BESSP$Party2[ BESSP$p14 == " MARIA LUCIA                                                                                                                                                                                                                                                   "] <- "PT"
BESSP$Party2[ BESSP$p14 == "13                                                                                                                                                                                                                                                             "] <- "PT"
BESSP$Party2[ BESSP$p14 == "23110                                                                                                                                                                                                                                                          "] <- "PPS"
BESSP$Party2[ BESSP$p14 == "45                                                                                                                                                                                                                                                             "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "45660 DEUSINHO                                                                                                                                                                                                                                                 "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "ADEMIR CHEDI                                                                                                                                                                                                                                                   "] <- "PFL"
BESSP$Party2[ BESSP$p14 == "ADILSON ROSSI                                                                                                                                                                                                                                                  "] <- "PTB"
BESSP$Party2[ BESSP$p14 == "AFANASIO                                                                                                                                                                                                                                                       "] <- "PFL"
BESSP$Party2[ BESSP$p14 == "ALBERTÃO                                                                                                                                                                                                                                                       "] <- "PT"
BESSP$Party2[ BESSP$p14 == "ALMEIDA - PT                                                                                                                                                                                                                                                   "] <- "PT"
BESSP$Party2[ BESSP$p14 == "ANA DO CARMO                                                                                                                                                                                                                                                   "] <- "PT"
BESSP$Party2[ BESSP$p14 == "ANA MARTINS                                                                                                                                                                                                                                                    "] <- "PCdoB"
BESSP$Party2[ BESSP$p14 == "ANTONIO CRESCENTE                                                                                                                                                                                                                                              "] <- "PT"
BESSP$Party2[ BESSP$p14 == "ANTÔNIO LINO                                                                                                                                                                                                                                                   "] <- "PPS"
BESSP$Party2[ BESSP$p14 == "AVANI                                                                                                                                                                                                                                                          "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "AVANIR                                                                                                                                                                                                                                                         "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "BALEIA                                                                                                                                                                                                                                                         "] <- "PMDB"
BESSP$Party2[ BESSP$p14 == "BILAFOM                                                                                                                                                                                                                                                        "] <- "PFL"
BESSP$Party2[ BESSP$p14 == "BILAFON                                                                                                                                                                                                                                                        "] <- "PFL"
BESSP$Party2[ BESSP$p14 == "BILLAFON                                                                                                                                                                                                                                                       "] <- "PFL"
BESSP$Party2[ BESSP$p14== "CARLOS NELSON - PMDB                                                                                                                                                                                                                                           "] <- "PMDB"
BESSP$Party2[ BESSP$p14 == "CARUSO                                                                                                                                                                                                                                                         "] <- "PMDB"
BESSP$Party2[ BESSP$p14 == "CARUZO                                                                                                                                                                                                                                                         "] <- "PMDB"
BESSP$Party2[ BESSP$p14 == "CELSO TANUAIR                                                                                                                                                                                                                                                  "] <- "PTB"
BESSP$Party2[ BESSP$p14 == "CONTE LOPES                                                                                                                                                                                                                                                    "] <- "PPB"
BESSP$Party2[ BESSP$p14 == "CORONEL EDSON FENONINI                                                                                                                                                                                                                                         "] <- "PTB"
BESSP$Party2[ BESSP$p14 == "CORONEL FERRARINI                                                                                                                                                                                                                                              \001"] <- "PTB"
BESSP$Party2[ BESSP$p14 == "CORONEL FERROLINI                                                                                                                                                                                                                                              "] <- "PTB"
BESSP$Party2[ BESSP$p14 == "CRESPO                                                                                                                                                                                                                                                         "] <- "PFL"
BESSP$Party2[ BESSP$p14 == "D. MARIA DE JESUS                                                                                                                                                                                                                                              "] <- "PTdoB"
BESSP$Party2[ BESSP$p14 == "DARCI VERA                                                                                                                                                                                                                                                     "] <- "PFL"
BESSP$Party2[ BESSP$p14 == "DAVI TORRES                                                                                                                                                                                                                                                    "] <- "PPS"
BESSP$Party2[ BESSP$p14 == "DO PT                                                                                                                                                                                                                                                          "] <- "PT"
BESSP$Party2[ BESSP$p14 == "DO PT NÃO LEMBRA                                                                                                                                                                                                                                               "] <- "PT"
BESSP$Party2[ BESSP$p14 == "DONZETE-PT                                                                                                                                                                                                                                                     "] <- "PT"
BESSP$Party2[ BESSP$p14 == "DR JORGE LARDELO                                                                                                                                                                                                                                               "] <- "PL"
BESSP$Party2[ BESSP$p14 == "DR. HAVANIR                                                                                                                                                                                                                                                    "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "DRª EVANIR                                                                                                                                                                                                                                                     "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "DRA. AVANIR                                                                                                                                                                                                                                                    "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "DRA. EVANIR                                                                                                                                                                                                                                                    "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "DRA. HAVANIR                                                                                                                                                                                                                                                   "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "DUARTE NOGUEIRA                                                                                                                                                                                                                                                "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "EDGAR SOUZA                                                                                                                                                                                                                                                    "] <- "PPB"
BESSP$Party2[ BESSP$p14 == "EDIMIR CHEDI                                                                                                                                                                                                                                                   "] <- "PFL"
BESSP$Party2[ BESSP$p14 == "EDMIR CHIDIA                                                                                                                                                                                                                                                   "] <- "PFL"
BESSP$Party2[ BESSP$p14 == "EDSON FERRORINI                                                                                                                                                                                                                                                "] <- "PTB"
BESSP$Party2[ BESSP$p14 == "ELI CORREIA JUNIOR                                                                                                                                                                                                                                             "] <- "PDT"
BESSP$Party2[ BESSP$p14 == "ENIO TATO                                                                                                                                                                                                                                                      "] <- "PT"
BESSP$Party2[ BESSP$p14 == "ESTELA                                                                                                                                                                                                                                                         "] <- "PT"
BESSP$Party2[ BESSP$p14 == "FÁTIMA PIRES                                                                                                                                                                                                                                                   "] <- "PTdoB"
BESSP$Party2[ BESSP$p14 == "FERRARINI                                                                                                                                                                                                                                                      "] <- "PTB"
BESSP$Party2[ BESSP$p14 == "FERREIRA JR                                                                                                                                                                                                                                                    "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "FRANCISCO(PRONA)                                                                                                                                                                                                                                               "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "GERALDO LOPES                                                                                                                                                                                                                                                  "] <- "PMDB"
BESSP$Party2[ BESSP$p14 == "GILDÃO                                                                                                                                                                                                                                                         "] <- "PDT"
BESSP$Party2[ BESSP$p14 == "HAMILTON                                                                                                                                                                                                                                                       "] <- "PT"
BESSP$Party2[ BESSP$p14 == "HAVANIR                                                                                                                                                                                                                                                        "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "JOÃO CARAMES                                                                                                                                                                                                                                                   "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "JORGE CARUZO                                                                                                                                                                                                                                                   "] <- "PMDB"
BESSP$Party2[ BESSP$p14 == "JORGE JORDELO                                                                                                                                                                                                                                                  "] <- "PL"
BESSP$Party2[ BESSP$p14 == "JORGE LORDELLO                                                                                                                                                                                                                                                 "] <- "PL"
BESSP$Party2[ BESSP$p14 == "JORGE LOURDELO                                                                                                                                                                                                                                                 "] <- "PL"
BESSP$Party2[ BESSP$p14 == "JOSÉ HENRIQUE                                                                                                                                                                                                                                                  "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "LEGENDA (13)                                                                                                                                                                                                                                                   "] <- "PT"
BESSP$Party2[ BESSP$p14 == "LEGENDA 45                                                                                                                                                                                                                                                     "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "LEGENDA DO PSB (40)                                                                                                                                                                                                                                            "] <- "PSB"
BESSP$Party2[ BESSP$p14 == "LEGENDA DO PT                                                                                                                                                                                                                                                  "] <- "PT"
BESSP$Party2[ BESSP$p14 == "LEGENDA PT                                                                                                                                                                                                                                                     "] <- "PT"
BESSP$Party2[ BESSP$p14 == "LEGENDA PT (13)                                                                                                                                                                                                                                                "] <- "PT"
BESSP$Party2[ BESSP$p14 == "LEGENDA PT 13                                                                                                                                                                                                                                                  "] <- "PT"
BESSP$Party2[ BESSP$p14 == "LEONEL DAMO                                                                                                                                                                                                                                                    "] <- "PV"
BESSP$Party2[ BESSP$p14 == "LEONEL DAMO                                                                                                                                                                                                                                                    "] <- "PV"
BESSP$Party2[ BESSP$p14 == "LILI AIMAR                                                                                                                                                                                                                                                     "] <- "PL"
BESSP$Party2[ BESSP$p14 == "LINO BEZERRA                                                                                                                                                                                                                                                   "] <- "PPS"
BESSP$Party2[ BESSP$p14 == "LUIZ STEFANI                                                                                                                                                                                                                                                   "] <- "PPS"
BESSP$Party2[ BESSP$p14 == "MARCELO CANDIDO                                                                                                                                                                                                                                                "] <- "PT"
BESSP$Party2[ BESSP$p14 == "MARCOS VINICIOS                                                                                                                                                                                                                                                "] <- "PPS"
BESSP$Party2[ BESSP$p14 == "MARCOS VINÍCIOS                                                                                                                                                                                                                                                "] <- "PPS"
BESSP$Party2[ BESSP$p14 == "MARIA DE JESUS                                                                                                                                                                                                                                                 "] <- "PTdoB"
BESSP$Party2[ BESSP$p14 == "MARIA LUCIA AMORY                                                                                                                                                                                                                                              "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "MARIO BRAGATO                                                                                                                                                                                                                                                  "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "MARIO LUIZ GUIDE                                                                                                                                                                                                                                               "] <- "PSB"
BESSP$Party2[ BESSP$p14 == "MELÃO                                                                                                                                                                                                                                                          "] <- "PV"
BESSP$Party2[ BESSP$p14 == "MELÃO 4302                                                                                                                                                                                                                                                     "] <- "PV"
BESSP$Party2[ BESSP$p14 == "MIRO CORREIA                                                                                                                                                                                                                                                   "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "NABI                                                                                                                                                                                                                                                           "] <- "PSD"
BESSP$Party2[ BESSP$p14 == "NASI ABI CHEDI                                                                                                                                                                                                                                                 "] <- "PSD"
BESSP$Party2[ BESSP$p14 == "NASI ABI CHEID                                                                                                                                                                                                                                                 "] <- "PSD"
BESSP$Party2[ BESSP$p14 == "NICOLAU HOLE                                                                                                                                                                                                                                                   "] <- "PTB"
BESSP$Party2[ BESSP$p14 == "NILSON AMARAL                                                                                                                                                                                                                                                  "] <- "PSB"
BESSP$Party2[ BESSP$p14 == "NO PARTIDO (PSDB)                                                                                                                                                                                                                                              "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "NO PT                                                                                                                                                                                                                                                          "] <- "PT"
BESSP$Party2[ BESSP$p14 == "OSEALDO CAMARGO                                                                                                                                                                                                                                                "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "OSWALDO CAMARGO                                                                                                                                                                                                                                                "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "PAULINHO 11333                                                                                                                                                                                                                                                 "] <- "PPB"
BESSP$Party2[ BESSP$p14 == "PAULINHO MARAMDIM DO PV                                                                                                                                                                                                                                        "] <- "PV"
BESSP$Party2[ BESSP$p14 == "PAULO MARCENDIM                                                                                                                                                                                                                                                "] <- "PV"
BESSP$Party2[ BESSP$p14 == "PAULINHO MORANDIM                                                                                                                                                                                                                                              "] <- "PV"
BESSP$Party2[ BESSP$p14 == "PAULO MORANDI                                                                                                                                                                                                                                                  "] <- "PV"
BESSP$Party2[ BESSP$p14 == "PAULO MORANDIM                                                                                                                                                                                                                                                 "] <- "PV"
BESSP$Party2[ BESSP$p14 == "PAULO MORANDIN                                                                                                                                                                                                                                                 "] <- "PV"
BESSP$Party2[ BESSP$p14 == "PAULO MORAUDENIR                                                                                                                                                                                                                                               "] <- "PV"
BESSP$Party2[ BESSP$p14 == "PEDRO TOBIAS                                                                                                                                                                                                                                                   "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "PEDRO TOBÍAS                                                                                                                                                                                                                                                   "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "PROFESSOR MARINO                                                                                                                                                                                                                                               "] <- "PRTB"
BESSP$Party2[ BESSP$p14 == "PRONA                                                                                                                                                                                                                                                          "] <- "PRONA"
BESSP$Party2[ BESSP$p14 == "PSDB                                                                                                                                                                                                                                                           "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "PT                                                                                                                                                                                                                                                             "] <- "PT"
BESSP$Party2[ BESSP$p14 == "PV 43                                                                                                                                                                                                                                                          "] <- "PV"
BESSP$Party2[ BESSP$p14 == "RAMIRO                                                                                                                                                                                                                                                         "] <- "PL"
BESSP$Party2[ BESSP$p14 == "RAUL MARCELO                                                                                                                                                                                                                                                   "] <- "PT"
BESSP$Party2[ BESSP$p14 == "RODRIGO GARCÍA                                                                                                                                                                                                                                                 "] <- "PFL"
BESSP$Party2[ BESSP$p14 == "ROGÉRIO NOGUEIRA                                                                                                                                                                                                                                               "] <- "PDT"
BESSP$Party2[ BESSP$p14 == "ROGERIO NOUGUEIRA                                                                                                                                                                                                                                              "] <- "PDT"
BESSP$Party2[ BESSP$p14 == "ROGÉRIO NOUGUEIRA                                                                                                                                                                                                                                              "] <- "PDT"
BESSP$Party2[ BESSP$p14 == "ROMEU TUMA JR                                                                                                                                                                                                                                                  "] <- "PPS"
BESSP$Party2[ BESSP$p14 == "RUBNES DUARTE                                                                                                                                                                                                                                                  "] <- "PPS"
BESSP$Party2[ BESSP$p14 == "SALMÃO LIMA                                                                                                                                                                                                                                                    "] <- "PSB"
BESSP$Party2[ BESSP$p14 == "SIMÃO PEDRO (PT)                                                                                                                                                                                                                                               "] <- "PT"
BESSP$Party2[ BESSP$p14 == "TOBIAS                                                                                                                                                                                                                                                         "] <- "PSDB"
BESSP$Party2[ BESSP$p14 == "TUGA ANGIRAME                                                                                                                                                                                                                                                  "] <- "PSB"
BESSP$Party2[ BESSP$p14 == "WALTER CAVEANHA                                                                                                                                                                                                                                                "] <- "PTB"

# Tocantins
BESTO <- ESEB02[ which(ESEB02$uf == "17"),]

BESTO$Party2 <- NA
#Here, unfortunately, there existed no valid responses on voting intentions.

## Merging district-level datasets.
ESEB02LOCAL <- rbind(BESAC, BESAL, BESAM, BESAP, BESBA, BESCE, BESDF, BESES, BESGO, 
                     BESMA, BESMG, BESMS, BESMT, BESPA, BESPB, BESPE, BESPI, BESPR,
                     BESRJ, BESRN, BESRO, BESRR, BESRS, BESSC, BESSE, BESSP, BESTO)

BES2002LOCAL <- na.omit(ESEB02LOCAL) #Clean dataset

## Assigning party labels, based on voting intentions at the 2014 state-deputy elections.
# Acre
BESAC14 <- ESEB14[ which(ESEB14$ESTADO == "12"),]

BESAC14$Party2 <- NA

# Alagoas
BESAL14 <- ESEB14[ which(ESEB14$ESTADO == "27"),]

BESAL14$Party2 <- NA
BESAL14$Party2[ BESAL14$Q5ALA == "20694"] <- "PRTB"
BESAL14$Party2[ BESAL14$Q5ALA == "20705"] <- "PTB"
BESAL14$Party2[ BESAL14$Q5ALA == "20763"] <- "PROS"
BESAL14$Party2[ BESAL14$Q5ALA == "20779"] <- "PRTB"
BESAL14$Party2[ BESAL14$Q5ALA == "20785"] <- "DEM"
BESAL14$Party2[ BESAL14$Q5ALA == "20795"] <- "DEM"
BESAL14$Party2[ BESAL14$Q5ALA == "20810"] <- "PTB"
BESAL14$Party2[ BESAL14$Q5ALA == "20915"] <- "PSDB"

# Amazonas
BESAM14 <- ESEB14[ which(ESEB14$ESTADO == "13"),]

BESAM14$Party2 <- NA
BESAM14$Party2[ BESAM14$Q5ALA == "1427"] <- "PC do B"
BESAM14$Party2[ BESAM14$Q5ALA == "1431"] <- "PMDB"
BESAM14$Party2[ BESAM14$Q5ALA == "21409"] <- "PSDB"

# Amapá
BESAP14 <- ESEB14[ which(ESEB14$ESTADO == "16"),]

BESAP14$Party2 <- NA
BESAP14$Party2[ BESAP14$Q5ALA == "1006"] <- "PP"
BESAP14$Party2[ BESAP14$Q5ALA == "21105"] <- "PROS"
BESAP14$Party2[ BESAP14$Q5ALA == "21173"] <- "PT"
BESAP14$Party2[ BESAP14$Q5ALA == "21257"] <- "PDT"
BESAP14$Party2[ BESAP14$Q5ALA == "21271"] <- "PDT"
BESAP14$Party2[ BESAP14$Q5ALA == "21308"] <- "PSC"
BESAP14$Party2[ BESAP14$Q5ALA == "21353"] <- "PT"

# Bahia
BESBA14 <- ESEB14[ which(ESEB14$ESTADO == "29"),]

BESBA14$Party2 <- NA
BESBA14$Party2[ BESBA14$Q5ALA == "13"] <- "PT"
BESBA14$Party2[ BESBA14$Q5ALA == "2119"] <- "PMDB"
BESBA14$Party2[ BESBA14$Q5ALA == "2253"] <- "PSD"
BESBA14$Party2[ BESBA14$Q5ALA == "2277"] <- "PT"
BESBA14$Party2[ BESBA14$Q5ALA == "22059"] <- "PSDB"
BESBA14$Party2[ BESBA14$Q5ALA == "22201"] <- "PSL"
BESBA14$Party2[ BESBA14$Q5ALA == "22276"] <- "PSDB"
BESBA14$Party2[ BESBA14$Q5ALA == "22327"] <- "PMDB"
BESBA14$Party2[ BESBA14$Q5ALA == "22363"] <- "PC do B"
BESBA14$Party2[ BESBA14$Q5ALA == "22385"] <- "PT"
BESBA14$Party2[ BESBA14$Q5ALA == "22413"] <- "PDT"
BESBA14$Party2[ BESBA14$Q5ALA == "22415"] <- "PRP"
BESBA14$Party2[ BESBA14$Q5ALA == "22464"] <- "PMDB"
BESBA14$Party2[ BESBA14$Q5ALA == "22494"] <- "PSC"
BESBA14$Party2[ BESBA14$Q5ALA == "22507"] <- "PSDB"
BESBA14$Party2[ BESBA14$Q5ALA == "22508"] <- "PV"
BESBA14$Party2[ BESBA14$Q5ALA == "22564"] <- "PSD"
BESBA14$Party2[ BESBA14$Q5ALA == "22575"] <- "PSL"
BESBA14$Party2[ BESBA14$Q5ALA == "22587"] <- "PT"
BESBA14$Party2[ BESBA14$Q5ALA == "22600"] <- "PT"
BESBA14$Party2[ BESBA14$Q5ALA == "22682"] <- "DEM"

# Ceará
BESCE14 <- ESEB14[ which(ESEB14$ESTADO == "23"),]

BESCE14$Party2 <- NA
BESCE14$Party2[ BESCE14$Q5ALA == "22755"] <- "PMDB"
BESCE14$Party2[ BESCE14$Q5ALA == "22758"] <- "PHS"
BESCE14$Party2[ BESCE14$Q5ALA == "22770"] <- "PP"
BESCE14$Party2[ BESCE14$Q5ALA == "22897"] <- "PDT"
BESCE14$Party2[ BESCE14$Q5ALA == "22956"] <- "PSDC"
BESCE14$Party2[ BESCE14$Q5ALA == "22994"] <- "PV"
BESCE14$Party2[ BESCE14$Q5ALA == "22995"] <- "PDT"
BESCE14$Party2[ BESCE14$Q5ALA == "23037"] <- "PMDB"
BESCE14$Party2[ BESCE14$Q5ALA == "23042"] <- "PROS"
BESCE14$Party2[ BESCE14$Q5ALA == "23079"] <- "PT do B"
BESCE14$Party2[ BESCE14$Q5ALA == "23128"] <- "PSD"
BESCE14$Party2[ BESCE14$Q5ALA == "23133"] <- "PROS"
BESCE14$Party2[ BESCE14$Q5ALA == "23278"] <- "PT"
BESCE14$Party2[ BESCE14$Q5ALA == "23279"] <- "PSL"
BESCE14$Party2[ BESCE14$Q5ALA == "23316"] <- "PDT"
BESCE14$Party2[ BESCE14$Q5ALA == "23317"] <- "PP"
BESCE14$Party2[ BESCE14$Q5ALA == "23326"] <- "PSOL"
BESCE14$Party2[ BESCE14$Q5ALA == "23379"] <- "PR"

# Distrito Federal
BESDF14 <- ESEB14[ which(ESEB14$ESTADO == "53"),]

BESDF14$Party2 <- NA
BESDF14$Party2[ BESDF14$Q5ALA == "13213"] <- "PT"
BESDF14$Party2[ BESDF14$Q5ALA == "23497"] <- "PRTB"
BESDF14$Party2[ BESDF14$Q5ALA == "23654"] <- "PPS"
BESDF14$Party2[ BESDF14$Q5ALA == "24235"] <- "PDT"
BESDF14$Party2[ BESDF14$Q5ALA == "40193"] <- "PSB"
BESDF14$Party2[ BESDF14$Q5ALA == "51678"] <- "PEN"

# Espírito Santo
BESES14 <- ESEB14[ which(ESEB14$ESTADO == "32"),]

BESES14$Party2 <- NA
BESES14$Party2[ BESES14$Q5ALA == "24500"] <- "PV"
BESES14$Party2[ BESES14$Q5ALA == "24539"] <- "PPS"
BESES14$Party2[ BESES14$Q5ALA == "24672"] <- "PMDB"
BESES14$Party2[ BESES14$Q5ALA == "24733"] <- "PMDB"

# Goiás
BESGO14 <- ESEB14[ which(ESEB14$ESTADO == "52"),]

BESGO14$Party2 <- NA
BESGO14$Party2[ BESGO14$Q5ALA == "4728"] <- "PMDB"
BESGO14$Party2[ BESGO14$Q5ALA == "25144"] <- "PR"
BESGO14$Party2[ BESGO14$Q5ALA == "25145"] <- "PMDB"
BESGO14$Party2[ BESGO14$Q5ALA == "25423"] <- "PHS"
BESGO14$Party2[ BESGO14$Q5ALA == "25426"] <- "DEM"
BESGO14$Party2[ BESGO14$Q5ALA == "25470"] <- "PSL"
BESGO14$Party2[ BESGO14$Q5ALA == "25597"] <- "PSD"
BESGO14$Party2[ BESGO14$Q5ALA == "25639"] <- "PP"
BESGO14$Party2[ BESGO14$Q5ALA == "25693"] <- "PHS"
BESGO14$Party2[ BESGO14$Q5ALA == "25741"] <- "PT"
BESGO14$Party2[ BESGO14$Q5ALA == "25752"] <- "PMDB"
BESGO14$Party2[ BESGO14$Q5ALA == "25873"] <- "PTB"
BESGO14$Party2[ BESGO14$Q5ALA == "25908"] <- "PSDB"
BESGO14$Party2[ BESGO14$Q5ALA == "25944"] <- "PMDB"

# Maranhão
BESMA14 <- ESEB14[ which(ESEB14$ESTADO == "21"),]

BESMA14$Party2 <- NA
BESMA14$Party2[ BESMA14$Q5ALA == "5720"] <- "PMDB"
BESMA14$Party2[ BESMA14$Q5ALA == "5726"] <- "PMDB"
BESMA14$Party2[ BESMA14$Q5ALA == "5743"] <- "PRB"
BESMA14$Party2[ BESMA14$Q5ALA == "5808"] <- "PSDC"
BESMA14$Party2[ BESMA14$Q5ALA == "26024"] <- "PMDB"
BESMA14$Party2[ BESMA14$Q5ALA == "26140"] <- "PV"
BESMA14$Party2[ BESMA14$Q5ALA == "26148"] <- "PSL"
BESMA14$Party2[ BESMA14$Q5ALA == "26267"] <- "PRB"
BESMA14$Party2[ BESMA14$Q5ALA == "26289"] <- "PV"
BESMA14$Party2[ BESMA14$Q5ALA == "26325"] <- "PMDB"
BESMA14$Party2[ BESMA14$Q5ALA == "26351"] <- "PSC"
BESMA14$Party2[ BESMA14$Q5ALA == "26393"] <- "PRB"
BESMA14$Party2[ BESMA14$Q5ALA == "26453"] <- "PMDB"
BESMA14$Party2[ BESMA14$Q5ALA == "26491"] <- "PTC"
BESMA14$Party2[ BESMA14$Q5ALA == "26514"] <- "PV"

# Minas Gerais
BESMG14 <- ESEB14[ which(ESEB14$ESTADO == "31"),]

BESMG14$Party2 <- NA
BESMG14$Party2[ BESMG14$Q5ALA == "45"] <- "PSDB"
BESMG14$Party2[ BESMG14$Q5ALA == "50"] <- "PSOL"
BESMG14$Party2[ BESMG14$Q5ALA == "7192"] <- "PTC"
BESMG14$Party2[ BESMG14$Q5ALA == "7204"] <- "PSDB"
BESMG14$Party2[ BESMG14$Q5ALA == "7275"] <- "PT do B"
BESMG14$Party2[ BESMG14$Q5ALA == "7507"] <- "PPS"
BESMG14$Party2[ BESMG14$Q5ALA == "7585"] <- "PDT"
BESMG14$Party2[ BESMG14$Q5ALA == "7599"] <- "DEM"
BESMG14$Party2[ BESMG14$Q5ALA == "7786"] <- "PT"
BESMG14$Party2[ BESMG14$Q5ALA == "27911"] <- "PR"
BESMG14$Party2[ BESMG14$Q5ALA == "27987"] <- "PR"
BESMG14$Party2[ BESMG14$Q5ALA == "27992"] <- "PTC"
BESMG14$Party2[ BESMG14$Q5ALA == "28014"] <- "PRP"
BESMG14$Party2[ BESMG14$Q5ALA == "28024"] <- "PTB"
BESMG14$Party2[ BESMG14$Q5ALA == "28093"] <- "PSDB"
BESMG14$Party2[ BESMG14$Q5ALA == "28114"] <- "PTB"
BESMG14$Party2[ BESMG14$Q5ALA == "28117"] <- "PPS"
BESMG14$Party2[ BESMG14$Q5ALA == "28135"] <- "PSDB"
BESMG14$Party2[ BESMG14$Q5ALA == "28138"] <- "PT"
BESMG14$Party2[ BESMG14$Q5ALA == "28255"] <- "PDT"
BESMG14$Party2[ BESMG14$Q5ALA == "28319"] <- "PP"
BESMG14$Party2[ BESMG14$Q5ALA == "28386"] <- "DEM"
BESMG14$Party2[ BESMG14$Q5ALA == "28423"] <- "PT"
BESMG14$Party2[ BESMG14$Q5ALA == "28454"] <- "PSDB"
BESMG14$Party2[ BESMG14$Q5ALA == "28460"] <- "PSDB"
BESMG14$Party2[ BESMG14$Q5ALA == "28466"] <- "PTC"
BESMG14$Party2[ BESMG14$Q5ALA == "28529"] <- "PT do B"
BESMG14$Party2[ BESMG14$Q5ALA == "28561"] <- "PSB"
BESMG14$Party2[ BESMG14$Q5ALA == "28620"] <- "PMDB"
BESMG14$Party2[ BESMG14$Q5ALA == "28644"] <- "PMDB"
BESMG14$Party2[ BESMG14$Q5ALA == "28723"] <- "PT"
BESMG14$Party2[ BESMG14$Q5ALA == "28776"] <- "PMN"
BESMG14$Party2[ BESMG14$Q5ALA == "28784"] <- "PP"
BESMG14$Party2[ BESMG14$Q5ALA == "28814"] <- "PV"
BESMG14$Party2[ BESMG14$Q5ALA == "28828"] <- "PT"
BESMG14$Party2[ BESMG14$Q5ALA == "28849"] <- "PT do B"
BESMG14$Party2[ BESMG14$Q5ALA == "28910"] <- "PT"
BESMG14$Party2[ BESMG14$Q5ALA == "28916"] <- "PSDB"
BESMG14$Party2[ BESMG14$Q5ALA == "28930"] <- "PROS"
BESMG14$Party2[ BESMG14$Q5ALA == "29021"] <- "PSDB"
BESMG14$Party2[ BESMG14$Q5ALA == "29060"] <- "PT do B"
BESMG14$Party2[ BESMG14$Q5ALA == "29075"] <- "PDT"
BESMG14$Party2[ BESMG14$Q5ALA == "29089"] <- "PT do B"

# Mato Grosso do Sul
BESMS14 <- ESEB14[ which(ESEB14$ESTADO == "50"),]

BESMS14$Party2 <- NA
BESMS14$Party2[ BESMS14$Q5ALA == "6678"] <- "PMDB"
BESMS14$Party2[ BESMS14$Q5ALA == "27572"] <- "PP"
BESMS14$Party2[ BESMS14$Q5ALA == "27585"] <- "PT"
BESMS14$Party2[ BESMS14$Q5ALA == "27603"] <- "PR"
BESMS14$Party2[ BESMS14$Q5ALA == "27733"] <- "PTC"

# Mato Grosso
BESMT14 <- ESEB14[ which(ESEB14$ESTADO == "51"),]

BESMT14$Party2 <- NA
BESMT14$Party2[ BESMT14$Q5ALA == "6307"] <- "PHS"
BESMT14$Party2[ BESMT14$Q5ALA == "27145"] <- "PSD"
BESMT14$Party2[ BESMT14$Q5ALA == "27167"] <- "PSB"
BESMT14$Party2[ BESMT14$Q5ALA == "27234"] <- "PR"
BESMT14$Party2[ BESMT14$Q5ALA == "27280"] <- "PMDB"

# Pará
BESPA14 <- ESEB14[ which(ESEB14$ESTADO == "15"),]

BESPA14$Party2 <- NA
BESPA14$Party2[ BESPA14$Q5ALA == "8394"] <- "PSD"
BESPA14$Party2[ BESPA14$Q5ALA == "8400"] <- "PMDB"
BESPA14$Party2[ BESPA14$Q5ALA == "29216"] <- "PT"
BESPA14$Party2[ BESPA14$Q5ALA == "29235"] <- "PSDB"
BESPA14$Party2[ BESPA14$Q5ALA == "29311"] <- "PSB"
BESPA14$Party2[ BESPA14$Q5ALA == "29351"] <- "PT"
BESPA14$Party2[ BESPA14$Q5ALA == "29383"] <- "PSD"
BESPA14$Party2[ BESPA14$Q5ALA == "29403"] <- "PMDB"
BESPA14$Party2[ BESPA14$Q5ALA == "29467"] <- "DEM"
BESPA14$Party2[ BESPA14$Q5ALA == "29546"] <- "PMDB"
BESPA14$Party2[ BESPA14$Q5ALA == "29577"] <- "PT"
BESPA14$Party2[ BESPA14$Q5ALA == "29585"] <- "PR"
BESPA14$Party2[ BESPA14$Q5ALA == "29594"] <- "PSB"
BESPA14$Party2[ BESPA14$Q5ALA == "29655"] <- "PSC"
BESPA14$Party2[ BESPA14$Q5ALA == "29672"] <- "PSC"
BESPA14$Party2[ BESPA14$Q5ALA == "29755"] <- "PSDC"
BESPA14$Party2[ BESPA14$Q5ALA == "29819"] <- "PDT"
BESPA14$Party2[ BESPA14$Q5ALA == "29820"] <- "PROS"
BESPA14$Party2[ BESPA14$Q5ALA == "29907"] <- "PTB"
BESPA14$Party2[ BESPA14$Q5ALA == "29913"] <- "PSB"

# Paraíba
BESPB14 <- ESEB14[ which(ESEB14$ESTADO == "25"),]

BESPB14$Party2 <- NA
BESPB14$Party2[ BESPB14$Q5ALA == "9295"] <- "PSDB"
BESPB14$Party2[ BESPB14$Q5ALA == "30056"] <- "PR"
BESPB14$Party2[ BESPB14$Q5ALA == "30058"] <- "PSDB"
BESPB14$Party2[ BESPB14$Q5ALA == "30062"] <- "PROS"
BESPB14$Party2[ BESPB14$Q5ALA == "30120"] <- "PSB"
BESPB14$Party2[ BESPB14$Q5ALA == "30377"] <- "PMDB"

# Pernambuco
BESPE14 <- ESEB14[ which(ESEB14$ESTADO == "26"),]

BESPE14$Party2 <- NA
BESPE14$Party2[ BESPE14$Q5ALA == "10326"] <- "PRB"
BESPE14$Party2[ BESPE14$Q5ALA == "10334"] <- "PSDB"
BESPE14$Party2[ BESPE14$Q5ALA == "10354"] <- "PSB"
BESPE14$Party2[ BESPE14$Q5ALA == "10381"] <- "PMN"
BESPE14$Party2[ BESPE14$Q5ALA == "31416"] <- "PR"
BESPE14$Party2[ BESPE14$Q5ALA == "31417"] <- "PSDB"
BESPE14$Party2[ BESPE14$Q5ALA == "31419"] <- "PC do B"
BESPE14$Party2[ BESPE14$Q5ALA == "31465"] <- "PP"
BESPE14$Party2[ BESPE14$Q5ALA == "31502"] <- "PP"
BESPE14$Party2[ BESPE14$Q5ALA == "31566"] <- "PSOL"
BESPE14$Party2[ BESPE14$Q5ALA == "31609"] <- "PR"
BESPE14$Party2[ BESPE14$Q5ALA == "31664"] <- "PHS"
BESPE14$Party2[ BESPE14$Q5ALA == "31667"] <- "PSB"
BESPE14$Party2[ BESPE14$Q5ALA == "31687"] <- "PTC"
BESPE14$Party2[ BESPE14$Q5ALA == "31698"] <- "PSD"
BESPE14$Party2[ BESPE14$Q5ALA == "31723"] <- "PSB"
BESPE14$Party2[ BESPE14$Q5ALA == "31732"] <- "PSB"
BESPE14$Party2[ BESPE14$Q5ALA == "31813"] <- "PSB"
BESPE14$Party2[ BESPE14$Q5ALA == "31824"] <- "PRB"
BESPE14$Party2[ BESPE14$Q5ALA == "31840"] <- "PDT"
BESPE14$Party2[ BESPE14$Q5ALA == "31903"] <- "PMN"
BESPE14$Party2[ BESPE14$Q5ALA == "31907"] <- "PSB"
BESPE14$Party2[ BESPE14$Q5ALA == "31912"] <- "PP"
BESPE14$Party2[ BESPE14$Q5ALA == "31930"] <- "PT do B"
BESPE14$Party2[ BESPE14$Q5ALA == "31937"] <- "PPS"
BESPE14$Party2[ BESPE14$Q5ALA == "31948"] <- "PSB"

# Piauí
BESPI14 <- ESEB14[ which(ESEB14$ESTADO == "22"),]

BESPI14$Party2 <- NA
BESPI14$Party2[ BESPI14$Q5ALA == "10910"] <- "PSB"
BESPI14$Party2[ BESPI14$Q5ALA == "10937"] <- "PTB"
BESPI14$Party2[ BESPI14$Q5ALA == "10945"] <- "PT"
BESPI14$Party2[ BESPI14$Q5ALA == "10954"] <- "PSB"
BESPI14$Party2[ BESPI14$Q5ALA == "10966"] <- "PSD"
BESPI14$Party2[ BESPI14$Q5ALA == "32028"] <- "PTB"
BESPI14$Party2[ BESPI14$Q5ALA == "32029"] <- "PV"
BESPI14$Party2[ BESPI14$Q5ALA == "32042"] <- "PSDB"
BESPI14$Party2[ BESPI14$Q5ALA == "32062"] <- "PSDB"
BESPI14$Party2[ BESPI14$Q5ALA == "32064"] <- "PDT"
BESPI14$Party2[ BESPI14$Q5ALA == "32091"] <- "PSB"
BESPI14$Party2[ BESPI14$Q5ALA == "32126"] <- "PTB"
BESPI14$Party2[ BESPI14$Q5ALA == "32127"] <- "PT do B"
BESPI14$Party2[ BESPI14$Q5ALA == "32131"] <- "PSD"
BESPI14$Party2[ BESPI14$Q5ALA == "32204"] <- "PMDB"
BESPI14$Party2[ BESPI14$Q5ALA == "32237"] <- "PMDB"
BESPI14$Party2[ BESPI14$Q5ALA == "32240"] <- "PSB"
BESPI14$Party2[ BESPI14$Q5ALA == "32244"] <- "PMDB"

# Paraná
BESPR14 <- ESEB14[ which(ESEB14$ESTADO == "41"),]

BESPR14$Party2 <- NA
BESPR14$Party2[ BESPR14$Q5ALA == "9513"] <- "PR"
BESPR14$Party2[ BESPR14$Q5ALA == "9554"] <- "PMDB"
BESPR14$Party2[ BESPR14$Q5ALA == "9640"] <- "PP"
BESPR14$Party2[ BESPR14$Q5ALA == "30508"] <- "PSDB"
BESPR14$Party2[ BESPR14$Q5ALA == "30548"] <- "PMDB"
BESPR14$Party2[ BESPR14$Q5ALA == "30556"] <- "PSDC"
BESPR14$Party2[ BESPR14$Q5ALA == "30576"] <- "PT"
BESPR14$Party2[ BESPR14$Q5ALA == "30605"] <- "PT"
BESPR14$Party2[ BESPR14$Q5ALA == "30644"] <- "PSC"
BESPR14$Party2[ BESPR14$Q5ALA == "30698"] <- "PPS"
BESPR14$Party2[ BESPR14$Q5ALA == "30790"] <- "PSD"
BESPR14$Party2[ BESPR14$Q5ALA == "30870"] <- "PPS"
BESPR14$Party2[ BESPR14$Q5ALA == "30912"] <- "PSB"
BESPR14$Party2[ BESPR14$Q5ALA == "30913"] <- "PSD"
BESPR14$Party2[ BESPR14$Q5ALA == "30918"] <- "PV"
BESPR14$Party2[ BESPR14$Q5ALA == "30923"] <- "PSB"
BESPR14$Party2[ BESPR14$Q5ALA == "30978"] <- "PSD"
BESPR14$Party2[ BESPR14$Q5ALA == "30986"] <- "PMDB"
BESPR14$Party2[ BESPR14$Q5ALA == "30990"] <- "PSD"
BESPR14$Party2[ BESPR14$Q5ALA == "31007"] <- "PMN"
BESPR14$Party2[ BESPR14$Q5ALA == "31022"] <- "PSC"
BESPR14$Party2[ BESPR14$Q5ALA == "31062"] <- "PSDB"
BESPR14$Party2[ BESPR14$Q5ALA == "31077"] <- "PSD"
BESPR14$Party2[ BESPR14$Q5ALA == "31081"] <- "PDT"
BESPR14$Party2[ BESPR14$Q5ALA == "31088"] <- "PMDB"
BESPR14$Party2[ BESPR14$Q5ALA == "31091"] <- "PSDB"
BESPR14$Party2[ BESPR14$Q5ALA == "31112"] <- "PTB"
BESPR14$Party2[ BESPR14$Q5ALA == "31137"] <- "PMDB"
BESPR14$Party2[ BESPR14$Q5ALA == "31184"] <- "DEM"
BESPR14$Party2[ BESPR14$Q5ALA == "31238"] <- "PSD"
BESPR14$Party2[ BESPR14$Q5ALA == "31243"] <- "DEM"
BESPR14$Party2[ BESPR14$Q5ALA == "31254"] <- "PTB"
BESPR14$Party2[ BESPR14$Q5ALA == "31298"] <- "PPS"
BESPR14$Party2[ BESPR14$Q5ALA == "31340"] <- "PSC"

# Rio de Janeiro
BESRJ14 <- ESEB14[ which(ESEB14$ESTADO == "33"),]

BESRJ14$Party2 <- NA
BESRJ14$Party2[ BESRJ14$Q5ALA == "11502"] <- "PMDB"
BESRJ14$Party2[ BESRJ14$Q5ALA == "11906"] <- "PT"
BESRJ14$Party2[ BESRJ14$Q5ALA == "12260"] <- "PTB"
BESRJ14$Party2[ BESRJ14$Q5ALA == "32642"] <- "PSOL"
BESRJ14$Party2[ BESRJ14$Q5ALA == "32661"] <- "PR"
BESRJ14$Party2[ BESRJ14$Q5ALA == "32720"] <- "PMDB"
BESRJ14$Party2[ BESRJ14$Q5ALA == "32749"] <- "PMDB"
BESRJ14$Party2[ BESRJ14$Q5ALA == "32827"] <- "PMDB"
BESRJ14$Party2[ BESRJ14$Q5ALA == "32830"] <- "PT"
BESRJ14$Party2[ BESRJ14$Q5ALA == "33173"] <- "PP"
BESRJ14$Party2[ BESRJ14$Q5ALA == "33332"] <- "PMDB"
BESRJ14$Party2[ BESRJ14$Q5ALA == "33424"] <- "PMDB"
BESRJ14$Party2[ BESRJ14$Q5ALA == "33481"] <- "PPS"
BESRJ14$Party2[ BESRJ14$Q5ALA == "33503"] <- "PSD"
BESRJ14$Party2[ BESRJ14$Q5ALA == "33624"] <- "PSDB"
BESRJ14$Party2[ BESRJ14$Q5ALA == "33636"] <- "PTB"
BESRJ14$Party2[ BESRJ14$Q5ALA == "33705"] <- "PR"
BESRJ14$Party2[ BESRJ14$Q5ALA == "33746"] <- "PSOL"
BESRJ14$Party2[ BESRJ14$Q5ALA == "34014"] <- "PSD"
BESRJ14$Party2[ BESRJ14$Q5ALA == "34054"] <- "PR"
BESRJ14$Party2[ BESRJ14$Q5ALA == "34508"] <- "PTB"

# Rio Grande do Norte
BESRN14 <- ESEB14[ which(ESEB14$ESTADO == "24"),]

BESRN14$Party2 <- NA
BESRN14$Party2[ BESRN14$Q5ALA == "13380"] <- "PSDB"
BESRN14$Party2[ BESRN14$Q5ALA == "34673"] <- "PSC"
BESRN14$Party2[ BESRN14$Q5ALA == "34683"] <- "PRP"
BESRN14$Party2[ BESRN14$Q5ALA == "34696"] <- "PR"
BESRN14$Party2[ BESRN14$Q5ALA == "34705"] <- "PMDB"
BESRN14$Party2[ BESRN14$Q5ALA == "34743"] <- "DEM"
BESRN14$Party2[ BESRN14$Q5ALA == "34744"] <- "PSD"
BESRN14$Party2[ BESRN14$Q5ALA == "34747"] <- "PSD"
BESRN14$Party2[ BESRN14$Q5ALA == "34773"] <- "DEM"
BESRN14$Party2[ BESRN14$Q5ALA == "34787"] <- "PHS"

# Rondônia
BESRO14 <- ESEB14[ which(ESEB14$ESTADO == "11"),]

BESRO14$Party2 <- NA
#Here, unfortunately, there existed no valid responses on voting intentions.

# Roraima
BESRR14 <- ESEB14[ which(ESEB14$ESTADO == "14"),]

BESRR14$Party2 <- NA
BESRR14$Party2[ BESRR14$Q5ALA == "36287"] <- "PRP"
BESRR14$Party2[ BESRR14$Q5ALA == "36335"] <- "PSDB"
BESRR14$Party2[ BESRR14$Q5ALA == "36339"] <- "PDT"
BESRR14$Party2[ BESRR14$Q5ALA == "36414"] <- "PSDB"
BESRR14$Party2[ BESRR14$Q5ALA == "36574"] <- "PSB"

# Rio Grande do Sul
BESRS14 <- ESEB14[ which(ESEB14$ESTADO == "43"),]

BESRS14$Party2 <- NA
BESRS14$Party2[ BESRS14$Q5ALA == "11"] <- "PP"
BESRS14$Party2[ BESRS14$Q5ALA == "12"] <- "PDT"
BESRS14$Party2[ BESRS14$Q5ALA == "13606"] <- "PDT"
BESRS14$Party2[ BESRS14$Q5ALA == "13661"] <- "PT"
BESRS14$Party2[ BESRS14$Q5ALA == "13908"] <- "PDT"
BESRS14$Party2[ BESRS14$Q5ALA == "34907"] <- "PP"
BESRS14$Party2[ BESRS14$Q5ALA == "34908"] <- "PP"
BESRS14$Party2[ BESRS14$Q5ALA == "34988"] <- "PMDB"
BESRS14$Party2[ BESRS14$Q5ALA == "35054"] <- "PDT"
BESRS14$Party2[ BESRS14$Q5ALA == "35081"] <- "PMDB"
BESRS14$Party2[ BESRS14$Q5ALA == "35088"] <- "PSD"
BESRS14$Party2[ BESRS14$Q5ALA == "35113"] <- "PSOL"
BESRS14$Party2[ BESRS14$Q5ALA == "35118"] <- "PP"
BESRS14$Party2[ BESRS14$Q5ALA == "35221"] <- "PSB"
BESRS14$Party2[ BESRS14$Q5ALA == "35246"] <- "PTB"
BESRS14$Party2[ BESRS14$Q5ALA == "35250"] <- "PP"
BESRS14$Party2[ BESRS14$Q5ALA == "35251"] <- "PT"
BESRS14$Party2[ BESRS14$Q5ALA == "35267"] <- "PMDB"
BESRS14$Party2[ BESRS14$Q5ALA == "35327"] <- "PT"
BESRS14$Party2[ BESRS14$Q5ALA == "35344"] <- "PC do B"
BESRS14$Party2[ BESRS14$Q5ALA == "35393"] <- "PP"
BESRS14$Party2[ BESRS14$Q5ALA == "35419"] <- "PT"
BESRS14$Party2[ BESRS14$Q5ALA == "35514"] <- "PT"

# Santa Catarina
BESSC14 <- ESEB14[ which(ESEB14$ESTADO == "42"),]

BESSC14$Party2 <- NA
BESSC14$Party2[ BESSC14$Q5ALA == "14230"] <- "PT"
BESSC14$Party2[ BESSC14$Q5ALA == "14242"] <- "PSB"
BESSC14$Party2[ BESSC14$Q5ALA == "14315"] <- "PSB"
BESSC14$Party2[ BESSC14$Q5ALA == "14327"] <- "PMDB"
BESSC14$Party2[ BESSC14$Q5ALA == "36754"] <- "PT do B"
BESSC14$Party2[ BESSC14$Q5ALA == "36790"] <- "PMDB"
BESSC14$Party2[ BESSC14$Q5ALA == "36797"] <- "PMDB"
BESSC14$Party2[ BESSC14$Q5ALA == "36858"] <- "PSD"
BESSC14$Party2[ BESSC14$Q5ALA == "36934"] <- "PSDB"
BESSC14$Party2[ BESSC14$Q5ALA == "36998"] <- "PMDB"
BESSC14$Party2[ BESSC14$Q5ALA == "37111"] <- "PPS"
BESSC14$Party2[ BESSC14$Q5ALA == "37168"] <- "PMDB"
BESSC14$Party2[ BESSC14$Q5ALA == "37184"] <- "PP"

# Sergipe
BESSE14 <- ESEB14[ which(ESEB14$ESTADO == "28"),]

BESSE14$Party2 <- NA
BESSE14$Party2[ BESSE14$Q5ALA == "39522"] <- "PSB"
BESSE14$Party2[ BESSE14$Q5ALA == "39540"] <- "PP"

# São Paulo
BESSP14 <- ESEB14[ which(ESEB14$ESTADO == "35"),]

BESSP14$Party2 <- NA
BESSP14$Party2[ BESSP14$Q5ALA == "13"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "15"] <- "PMDB"
BESSP14$Party2[ BESSP14$Q5ALA == "29"] <- "PCO"
BESSP14$Party2[ BESSP14$Q5ALA == "40"] <- "PSB"
BESSP14$Party2[ BESSP14$Q5ALA == "45"] <- "PSDB"
BESSP14$Party2[ BESSP14$Q5ALA == "65"] <- "PC do B"
BESSP14$Party2[ BESSP14$Q5ALA == "14530"] <- "DEM"
BESSP14$Party2[ BESSP14$Q5ALA == "14561"] <- "PPS"
BESSP14$Party2[ BESSP14$Q5ALA == "14562"] <- "PMDB"
BESSP14$Party2[ BESSP14$Q5ALA == "14570"] <- "DEM"
BESSP14$Party2[ BESSP14$Q5ALA == "14574"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "14596"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "14766"] <- "PRB"
BESSP14$Party2[ BESSP14$Q5ALA == "14803"] <- "PRB"
BESSP14$Party2[ BESSP14$Q5ALA == "15036"] <- "PR"
BESSP14$Party2[ BESSP14$Q5ALA == "15078"] <- "PROS"
BESSP14$Party2[ BESSP14$Q5ALA == "15103"] <- "PSD"
BESSP14$Party2[ BESSP14$Q5ALA == "15152"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "15225"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "15250"] <- "PC do B"
BESSP14$Party2[ BESSP14$Q5ALA == "15293"] <- "PSOL"
BESSP14$Party2[ BESSP14$Q5ALA == "15369"] <- "PRB"
BESSP14$Party2[ BESSP14$Q5ALA == "15389"] <- "PMDB"
BESSP14$Party2[ BESSP14$Q5ALA == "15563"] <- "PSDB"
BESSP14$Party2[ BESSP14$Q5ALA == "15652"] <- "PSD"
BESSP14$Party2[ BESSP14$Q5ALA == "15872"] <- "PHS"
BESSP14$Party2[ BESSP14$Q5ALA == "15917"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "15958"] <- "PSD"
BESSP14$Party2[ BESSP14$Q5ALA == "37273"] <- "PSD"
BESSP14$Party2[ BESSP14$Q5ALA == "37311"] <- "PSD"
BESSP14$Party2[ BESSP14$Q5ALA == "37344"] <- "PSDB"
BESSP14$Party2[ BESSP14$Q5ALA == "37357"] <- "PR"
BESSP14$Party2[ BESSP14$Q5ALA == "37362"] <- "PSC"
BESSP14$Party2[ BESSP14$Q5ALA == "37431"] <- "PP"
BESSP14$Party2[ BESSP14$Q5ALA == "37457"] <- "PR"
BESSP14$Party2[ BESSP14$Q5ALA == "37496"] <- "PSTU"
BESSP14$Party2[ BESSP14$Q5ALA == "37514"] <- "PSOL"
BESSP14$Party2[ BESSP14$Q5ALA == "37545"] <- "PMDB"
BESSP14$Party2[ BESSP14$Q5ALA == "37600"] <- "PMDB"
BESSP14$Party2[ BESSP14$Q5ALA == "37759"] <- "PTB"
BESSP14$Party2[ BESSP14$Q5ALA == "37839"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "37979"] <- "PRB"
BESSP14$Party2[ BESSP14$Q5ALA == "38007"] <- "PSDC"
BESSP14$Party2[ BESSP14$Q5ALA == "38052"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "38081"] <- "PMDB"
BESSP14$Party2[ BESSP14$Q5ALA == "38082"] <- "PMDB"
BESSP14$Party2[ BESSP14$Q5ALA == "38110"] <- "PDT"
BESSP14$Party2[ BESSP14$Q5ALA == "38117"] <- "PSB"
BESSP14$Party2[ BESSP14$Q5ALA == "38137"] <- "PSDB"
BESSP14$Party2[ BESSP14$Q5ALA == "38155"] <- "PC do B"
BESSP14$Party2[ BESSP14$Q5ALA == "38157"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "38176"] <- "PTC"
BESSP14$Party2[ BESSP14$Q5ALA == "38185"] <- "PC do B"
BESSP14$Party2[ BESSP14$Q5ALA == "38186"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "38193"] <- "PRB"
BESSP14$Party2[ BESSP14$Q5ALA == "38196"] <- "PV"
BESSP14$Party2[ BESSP14$Q5ALA == "38209"] <- "PSDB"
BESSP14$Party2[ BESSP14$Q5ALA == "38241"] <- "PSDB"
BESSP14$Party2[ BESSP14$Q5ALA == "38269"] <- "PMDB"
BESSP14$Party2[ BESSP14$Q5ALA == "38287"] <- "PSD"
BESSP14$Party2[ BESSP14$Q5ALA == "38288"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "38296"] <- "PSD"
BESSP14$Party2[ BESSP14$Q5ALA == "38324"] <- "PSD"
BESSP14$Party2[ BESSP14$Q5ALA == "38342"] <- "PRP"
BESSP14$Party2[ BESSP14$Q5ALA == "38367"] <- "PSC"
BESSP14$Party2[ BESSP14$Q5ALA == "38388"] <- "PC do B"
BESSP14$Party2[ BESSP14$Q5ALA == "38454"] <- "PSD"
BESSP14$Party2[ BESSP14$Q5ALA == "38470"] <- "PSDB"
BESSP14$Party2[ BESSP14$Q5ALA == "38545"] <- "PSC"
BESSP14$Party2[ BESSP14$Q5ALA == "38561"] <- "PSDB"
BESSP14$Party2[ BESSP14$Q5ALA == "38566"] <- "PSD"
BESSP14$Party2[ BESSP14$Q5ALA == "38567"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "38588"] <- "PT"
BESSP14$Party2[ BESSP14$Q5ALA == "38663"] <- "PV"
BESSP14$Party2[ BESSP14$Q5ALA == "38747"] <- "PHS"
BESSP14$Party2[ BESSP14$Q5ALA == "38748"] <- "DEM"
BESSP14$Party2[ BESSP14$Q5ALA == "38813"] <- "PMDB"
BESSP14$Party2[ BESSP14$Q5ALA == "38825"] <- "PSDB"
BESSP14$Party2[ BESSP14$Q5ALA == "38866"] <- "PSDB"
BESSP14$Party2[ BESSP14$Q5ALA == "38901"] <- "PSDB"
BESSP14$Party2[ BESSP14$Q5ALA == "38949"] <- "PSB"
BESSP14$Party2[ BESSP14$Q5ALA == "39005"] <- "PT do B"
BESSP14$Party2[ BESSP14$Q5ALA == "39014"] <- "PPS"
BESSP14$Party2[ BESSP14$Q5ALA == "39022"] <- "PSC"
BESSP14$Party2[ BESSP14$Q5ALA == "39025"] <- "PSL"
BESSP14$Party2[ BESSP14$Q5ALA == "39079"] <- "PSDB"
BESSP14$Party2[ BESSP14$Q5ALA == "39105"] <- "PRB"
BESSP14$Party2[ BESSP14$Q5ALA == "39166"] <- "PSD"
BESSP14$Party2[ BESSP14$Q5ALA == "39228"] <- "PSD"
BESSP14$Party2[ BESSP14$Q5ALA == "39254"] <- "PMDB"
BESSP14$Party2[ BESSP14$Q5ALA == "39283"] <- "PV"

# Tocantins
BESTO14 <- ESEB14[ which(ESEB14$ESTADO == "17"),]

BESTO14$Party2 <- NA
BESTO14$Party2[ BESTO14$Q5ALA == "39612"] <- "PT"

## Merging district-level datasets.
ESEB14LOCAL <- rbind(BESAC14, BESAL14, BESAM14, BESAP14, BESBA14, BESCE14, BESDF14, BESES14, BESGO14, 
                     BESMA14, BESMG14, BESMS14, BESMT14, BESPA14, BESPB14, BESPE14, BESPI14, BESPR14,
                     BESRJ14, BESRN14, BESRO14, BESRR14, BESRS14, BESSC14, BESSE14, BESSP14, BESTO14)

BES2014LOCAL <- na.omit(ESEB14LOCAL) #Clean dataset

## Cleaning local datasets
BES2002LOCAL$STATE <- NA
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "11"] <- "Rondônia"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "12"] <- "Acre"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "13"] <- "Amazonas"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "14"] <- "Roraima"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "15"] <- "Pará"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "16"] <- "Amapá"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "17"] <- "Tocantins"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "21"] <- "Maranhão"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "22"] <- "Piauí"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "23"] <- "Ceará"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "24"] <- "Rio Grande do Norte"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "25"] <- "Paraíba"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "26"] <- "Pernambuco"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "27"] <- "Alagoas"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "28"] <- "Sergipe"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "29"] <- "Bahia"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "31"] <- "Minas Gerais"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "32"] <- "Espírito Santo"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "33"] <- "Rio de Janeiro"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "35"] <- "São Paulo"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "41"] <- "Paraná"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "42"] <- "Santa Catarina"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "43"] <- "Rio Grande do Sul"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "50"] <- "Mato Grosso do Sul"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "51"] <- "Mato Grosso"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "52"] <- "Goiás"
BES2002LOCAL$STATE[ BES2002LOCAL$uf == "53"] <- "Distrito Federal"

BES2014LOCAL$ESTADO
BES2014LOCAL$STATE <- NA
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "11"] <- "Rondônia"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "12"] <- "Acre"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "13"] <- "Amazonas"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "14"] <- "Roraima"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "15"] <- "Pará"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "16"] <- "Amapá"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "17"] <- "Tocantins"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "21"] <- "Maranhão"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "22"] <- "Piauí"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "23"] <- "Ceará"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "24"] <- "Rio Grande do Norte"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "25"] <- "Paraíba"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "26"] <- "Pernambuco"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "27"] <- "Alagoas"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "28"] <- "Sergipe"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "29"] <- "Bahia"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "31"] <- "Minas Gerais"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "32"] <- "Espírito Santo"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "33"] <- "Rio de Janeiro"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "35"] <- "São Paulo"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "41"] <- "Paraná"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "42"] <- "Santa Catarina"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "43"] <- "Rio Grande do Sul"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "50"] <- "Mato Grosso do Sul"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "51"] <- "Mato Grosso"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "52"] <- "Goiás"
BES2014LOCAL$STATE[ BES2014LOCAL$ESTADO == "53"] <- "Distrito Federal"

BES2002LOCAL$p19[ BES2002LOCAL$p19 == "6"] <- NA
BES2002LOCAL$p22[ BES2002LOCAL$p22 == "6"] <- NA
BES2002LOCAL$p50v1[ BES2002LOCAL$p50v1 == "66"] <- NA
BES2002LOCAL$p50v1[ BES2002LOCAL$p50v1 == "88"] <- NA

BES2014LOCAL$Q15[ BES2014LOCAL$Q15 == "8"] <- NA
BES2014LOCAL$Q15[ BES2014LOCAL$Q15 == "9"] <- NA
BES2014LOCAL$Q7[ BES2014LOCAL$Q7 == "8"] <- NA
BES2014LOCAL$Q7[ BES2014LOCAL$Q7 == "9"] <- NA
BES2014LOCAL$Q8[ BES2014LOCAL$Q8 == "8"] <- NA
BES2014LOCAL$Q8[ BES2014LOCAL$Q8 == "9"] <- NA
BES2014LOCAL$PC6[ BES2014LOCAL$PC6 == "8"] <- NA
BES2014LOCAL$PC6[ BES2014LOCAL$PC6 == "9"] <- NA
BES2014LOCAL$Q16[ BES2014LOCAL$Q16 == "8"] <- NA
BES2014LOCAL$Q16[ BES2014LOCAL$Q16 == "9"] <- NA
BES2014LOCAL$Q16A[ BES2014LOCAL$Q16A == "8"] <- NA
BES2014LOCAL$Q16A[ BES2014LOCAL$Q16A == "9"] <- NA
BES2014LOCAL$PC12B[ BES2014LOCAL$PC12B == "98"] <- NA
BES2014LOCAL$PC12B[ BES2014LOCAL$PC12B == "99"] <- NA
BES2014LOCAL$Q17[ BES2014LOCAL$Q17 == "8"] <- NA
BES2014LOCAL$Q17[ BES2014LOCAL$Q17 == "9"] <- NA
BES2014LOCAL$D20A_FXRENDFAM[ BES2014LOCAL$D20A_FXRENDFAM == "98"] <- NA
BES2014LOCAL$D20A_FXRENDFAM[ BES2014LOCAL$D20A_FXRENDFAM == "99"] <- NA
BES2014LOCAL$D20A_FXRENDFAM[ BES2014LOCAL$D20A_FXRENDFAM == "9999"] <- NA
BES2014LOCAL$Q12[ BES2014LOCAL$Q12 == "95"] <- NA
BES2014LOCAL$Q12[ BES2014LOCAL$Q12 == "98"] <- NA
BES2014LOCAL$Q12[ BES2014LOCAL$Q12 == "99"] <- NA

BES2002LOCAL$p22New <- NA
BES2002LOCAL$p22New[ BES2002LOCAL$p22 == "1"] <- 1
BES2002LOCAL$p22New[ BES2002LOCAL$p22 == "2"] <- 1
BES2002LOCAL$p22New[ BES2002LOCAL$p22 == "3"] <- 2
BES2002LOCAL$p22New[ BES2002LOCAL$p22 == "4"] <- 3
BES2002LOCAL$p22New[ BES2002LOCAL$p22 == "5"] <- 3

BES2014LOCAL$PC6New <- NA
BES2014LOCAL$PC6New[ BES2014LOCAL$PC6 == "1"] <- 3
BES2014LOCAL$PC6New[ BES2014LOCAL$PC6 == "2"] <- 1
BES2014LOCAL$PC6New[ BES2014LOCAL$PC6 == "3"] <- 2

BES2002LOCAL$p31New <- NA
BES2002LOCAL$p31New[ BES2002LOCAL$p31 == "0"] <- 2
BES2002LOCAL$p31New[ BES2002LOCAL$p31 == "1"] <- 1

BES2002LOCAL$p35New <- NA
BES2002LOCAL$p35New[ BES2002LOCAL$p35 == "0"] <- 2
BES2002LOCAL$p35New[ BES2002LOCAL$p35 == "1"] <- 1

BES2014LOCAL$PC12BNew <- NA
BES2014LOCAL$PC12BNew[ BES2014LOCAL$PC12B == "1"] <- 1
BES2014LOCAL$PC12BNew[ BES2014LOCAL$PC12B == "2"] <- 1
BES2014LOCAL$PC12BNew[ BES2014LOCAL$PC12B == "3"] <- 2
BES2014LOCAL$PC12BNew[ BES2014LOCAL$PC12B == "4"] <- 2
BES2014LOCAL$PC12BNew[ BES2014LOCAL$PC12B == "5"] <- 3
BES2014LOCAL$PC12BNew[ BES2014LOCAL$PC12B == "6"] <- 3
BES2014LOCAL$PC12BNew[ BES2014LOCAL$PC12B == "7"] <- 4
BES2014LOCAL$PC12BNew[ BES2014LOCAL$PC12B == "8"] <- 4
BES2014LOCAL$PC12BNew[ BES2014LOCAL$PC12B == "9"] <- 5
BES2014LOCAL$PC12BNew[ BES2014LOCAL$PC12B == "10"] <- 5

BES2002LOCAL$p04New <- NA
BES2002LOCAL$p04New[ BES2002LOCAL$p04 == "0"] <- 2
BES2002LOCAL$p04New[ BES2002LOCAL$p04 == "1"] <- 1

BES2002LOCAL$p159New <- NA
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "1"] <- 1
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "2"] <- 2
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "3"] <- 2
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "4"] <- 2
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "5"] <- 2
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "6"] <- 2
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "7"] <- 3
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "8"] <- 3
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "9"] <- 3
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "10"] <- 3
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "11"] <- 3
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "12"] <- 4
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "13"] <- 4
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "14"] <- 4
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "15"] <- 4
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "16"] <- 5
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "17"] <- 5
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "18"] <- 5
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "19"] <- 5
BES2002LOCAL$p159New[ BES2002LOCAL$p159 == "20"] <- 5

BES2014LOCAL$D3_ESCOLANew <- NA
BES2014LOCAL$D3_ESCOLANew[ BES2014LOCAL$D3_ESCOLA == "0"] <- 1
BES2014LOCAL$D3_ESCOLANew[ BES2014LOCAL$D3_ESCOLA == "1"] <- 2
BES2014LOCAL$D3_ESCOLANew[ BES2014LOCAL$D3_ESCOLA == "2"] <- 2
BES2014LOCAL$D3_ESCOLANew[ BES2014LOCAL$D3_ESCOLA == "3"] <- 3
BES2014LOCAL$D3_ESCOLANew[ BES2014LOCAL$D3_ESCOLA == "4"] <- 3
BES2014LOCAL$D3_ESCOLANew[ BES2014LOCAL$D3_ESCOLA == "5"] <- 4
BES2014LOCAL$D3_ESCOLANew[ BES2014LOCAL$D3_ESCOLA == "6"] <- 4
BES2014LOCAL$D3_ESCOLANew[ BES2014LOCAL$D3_ESCOLA == "7"] <- 5
BES2014LOCAL$D3_ESCOLANew[ BES2014LOCAL$D3_ESCOLA == "8"] <- 5
BES2014LOCAL$D3_ESCOLANew[ BES2014LOCAL$D3_ESCOLA == "9"] <- 5

BES2002LOCAL$p176New <- NA
BES2002LOCAL$p176New[ BES2002LOCAL$p176 < 725] <- 1
BES2002LOCAL$p176New[ BES2002LOCAL$p176 >= 725 & BES2002LOCAL$p176 < 1449] <- 2
BES2002LOCAL$p176New[ BES2002LOCAL$p176 >= 1449 & BES2002LOCAL$p176 < 3621] <- 3
BES2002LOCAL$p176New[ BES2002LOCAL$p176 >= 3621 & BES2002LOCAL$p176 < 7241] <- 4
BES2002LOCAL$p176New[ BES2002LOCAL$p176 >= 7241 & BES2002LOCAL$p176 < 10861] <- 5
BES2002LOCAL$p176New[ BES2002LOCAL$p176 >= 10861 & BES2002LOCAL$p176 < 14481] <- 6
BES2002LOCAL$p176New[ BES2002LOCAL$p176 >= 14481] <- 7

BES02LOCALCLEAN<- subset(BES2002LOCAL, select = c(Party2,   #Voting Intention
                                                  STATE,    #Electoral District
                                                  p19,      #Satisfaction to the functioning of democracy in Brazil
                                                  p20,      #Who governs the country makes a difference
                                                  p21,      #Your vote influences whay will happen in the country
                                                  p22New,   #The best form of government
                                                  p31New,   #Is there a political party that represents you?
                                                  p35New,   #Is there a political party that you like?
                                                  p108b,    #The government must say everything that companies have to do, like how many bathrooms they have to have.
                                                  p04New,   #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                                                  p159New,  #Up to which grade did you study?
                                                  p176New,  #Adding the income of all the people who live in your house, what is the family income?
                                                  p50v1     #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
))  

BES14LOCALCLEAN <- subset(BES2014LOCAL, select = c(Party2,         #Voting Intention
                                                   STATE,          #Electoral District
                                                   Q15,            #Satisfaction to the functioning of democracy in Brazil
                                                   Q7,             #Who governs the country makes a difference
                                                   Q8,             #Your vote influences whay will happen in the country
                                                   PC6New,         #The best form of government
                                                   Q16,            #Is there a political party that represents you?
                                                   Q16A,           #Is there a political party that you like?
                                                   PC12BNew,       #The government must say everything that companies have to do, like how many bathrooms they have to have.
                                                   Q17,            #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                                                   D3_ESCOLANew,   #Up to which grade did you study?
                                                   D20A_FXRENDFAM, #Adding the income of all the people who live in your house, what is the family income?
                                                   Q12             #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
))

colnames(BES02LOCALCLEAN) <- c("PARTY",           #Voting Intention
                               "STATE",           #Electoral District
                               "SATISFACTION",    #Satisfaction to the functioning of democracy in Brazil
                               "DIFFERENCE",      #Who governs the country makes a difference
                               "INFLUENCE",       #Your vote influences whay will happen in the country
                               "GOVERNMENT",      #The best form of government
                               "IDENTIFICATION",  #Is there a political party that represents you?
                               "PREFERENCE",      #Is there a political party that you like?
                               "ECONOMY",         #The government must say everything that companies have to do, like how many bathrooms they have to have.
                               "CONTACT",         #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                               "EDUCATION",       #Up to which grade did you study?
                               "INCOME",          #Adding the income of all the people who live in your house, what is the family income?
                               "LEFTRIGHT"        #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
)

colnames(BES14LOCALCLEAN) <- c("PARTY",           #Voting Intention
                               "STATE",           #Electoral District
                               "SATISFACTION",    #Satisfaction to the functioning of democracy in Brazil
                               "DIFFERENCE",      #Who governs the country makes a difference
                               "INFLUENCE",       #Your vote influences whay will happen in the country
                               "GOVERNMENT",      #The best form of government
                               "IDENTIFICATION",  #Is there a political party that represents you?
                               "PREFERENCE",      #Is there a political party that you like?
                               "ECONOMY",        #The government must say everything that companies have to do, like how many bathrooms they have to have.
                               "CONTACT",         #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                               "EDUCATION",       #Up to which grade did you study?
                               "INCOME",          #Adding the income of all the people who live in your house, what is the family income?
                               "LEFTRIGHT"        #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
)


## Coding controlling variables: Effective number of electoral parties at the district level
BES02LOCALCLEAN$ENP <- NA
BES14LOCALCLEAN$ENP <- NA

BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Acre"] <- 7.638647556
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Alagoas"] <- 8.71644783
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Amapá"] <- 8.825015068
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Amazonas"] <- 5.035866737
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Bahia"] <- 4.363955975
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Ceará"] <- 5.772770405
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Distrito Federal"] <- 4.542345524
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Espírito Santo"] <- 9.814389229
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Goiás"] <- 5.690371513
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Maranhão"] <- 6.468179703
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Mato Grosso"] <- 6.453672886
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Mato Grosso do Sul"] <- 6.185633183
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Minas Gerais"] <- 8.567997932
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Pará"] <- 6.131930828
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Paraíba"] <- 7.194557667
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Paraná"] <- 8.157027245
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Pernambuco"] <- 7.922866344
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Piauí"] <- 4.992351532
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Rio de Janeiro"] <- 11.25372219
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Rio Grande do Norte"] <- 5.650588512
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Rio Grande do Sul"] <- 6.867096901
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Roraima"] <- 6.532809258
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Santa Catarina"] <- 5.626364231
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "São Paulo"] <- 7.990907669
BES02LOCALCLEAN$ENP[ BES02LOCALCLEAN$STATE == "Sergipe"] <- 10.05707836

BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Acre"] <- 9.582177162
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Alagoas"] <- 10.66336823
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Amapá"] <- 16.97726822
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Amazonas"] <- 9.540507317
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Bahia"] <- 11.26440239
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Ceará"] <- 11.1743679
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Distrito Federal"] <- 13.33987427
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Espírito Santo"] <- 10.88947773
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Goiás"] <- 7.073312283
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Maranhão"] <- 15.80080318
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Mato Grosso do Sul"] <- 6.20756197
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Minas Gerais"] <- 11.86017895
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Pará"] <- 11.28019333
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Paraíba"] <- 9.882687569
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Paraná"] <- 13.19906235
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Pernambuco"] <- 8.2119627
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Piauí"] <- 8.169262886
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Rio de Janeiro"] <- 12.57965183
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Rio Grande do Norte"] <- 11.67064571
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Rio Grande do Sul"] <- 7.650769711
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Roraima"] <- 12.54011726
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Santa Catarina"] <- 7.420486927
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "São Paulo"] <- 10.95555878
BES14LOCALCLEAN$ENP[ BES14LOCALCLEAN$STATE == "Sergipe"] <- 12.02227306

## Coding controlling variables: Ethnic fractionalization at the district level
BES02LOCALCLEAN$EthnicFractionalization <- NA
BES14LOCALCLEAN$EthnicFractionalization <- NA

BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Acre"] <- 0.453240539
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Alagoas"] <- 0.477337742
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Amapá"] <- 0.479379318
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Amazonas"] <- 0.495866425
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Bahia"] <- 0.505351996
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Ceará"] <- 0.467237287
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Distrito Federal"] <- 0.558951824
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Espírito Santo"] <- 0.566425024
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Goiás"] <- 0.544794099
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Maranhão"] <- 0.466620383
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Mato Grosso"] <- 0.543363361
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Mato Grosso do Sul"] <- 0.53606619
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Minas Gerais"] <- 0.563377028
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Pará"] <- 0.46398909
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Paraíba"] <- 0.513359889
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Paraná"] <- 0.383342418
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Pernambuco"] <- 0.527347128
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Piauí"] <- 0.411050816
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Rio de Janeiro"] <- 0.53348731
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Rio Grande do Norte"] <- 0.510246425
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Rio Grande do Sul"] <- 0.254322213
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Roraima"] <- 0.350324844
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Santa Catarina"] <- 0.194943298
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "São Paulo"] <- 0.441620435
BES02LOCALCLEAN$EthnicFractionalization[ BES02LOCALCLEAN$STATE == "Sergipe"] <- 0.426528646

BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Acre"] <- 0.52573943
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Alagoas"] <- 0.496422978
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Amapá"] <- 0.487806675
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Amazonas"] <- 0.439574396
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Bahia"] <- 0.567721115
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Ceará"] <- 0.474894927
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Distrito Federal"] <- 0.590219757
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Espírito Santo"] <- 0.591882618
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Goiás"] <- 0.566132455
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Maranhão"] <- 0.478299106
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Mato Grosso do Sul"] <- 0.56919817
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Minas Gerais"] <- 0.588085536
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Pará"] <- 0.452128108
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Paraíba"] <- 0.549188452
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Paraná"] <- 0.466570352
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Pernambuco"] <- 0.535604271
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Piauí"] <- 0.482847688
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Rio de Janeiro"] <- 0.611700847
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Rio Grande do Norte"] <- 0.537729333
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Rio Grande do Sul"] <- 0.341876955
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Roraima"] <- 0.51664
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Santa Catarina"] <- 0.28024018
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "São Paulo"] <- 0.527226627
BES14LOCALCLEAN$EthnicFractionalization[ BES14LOCALCLEAN$STATE == "Sergipe"] <- 0.498480495

## Coding controlling variables: Class fractionalization at the district level
BES02LOCALCLEAN$ClassFractionalization <- NA
BES14LOCALCLEAN$ClassFractionalization <- NA

BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Acre"] <- 0.882466056
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Alagoas"] <- 0.772341136
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Amapá"] <- 0.872555387
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Amazonas"] <- 0.853809027
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Bahia"] <- 0.793661277
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Ceará"] <- 0.817797178
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Distrito Federal"] <- 0.878768712
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Espírito Santo"] <- 0.85552996
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Goiás"] <- 0.869993989
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Maranhão"] <- 0.747518538
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Mato Grosso"] <- 0.831127271
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Mato Grosso do Sul"] <- 0.867818529
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Minas Gerais"] <- 0.84653928
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Pará"] <- 0.859028883
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Paraíba"] <- 0.81276936
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Paraná"] <- 0.852314537
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Pernambuco"] <- 0.835473978
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Piauí"] <- 0.712735236
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Rio de Janeiro"] <- 0.878295119
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Rio Grande do Norte"] <- 0.856390707
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Rio Grande do Sul"] <- 0.823557749
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Roraima"] <- 0.875144398
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Santa Catarina"] <- 0.804128252
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "São Paulo"] <- 0.840409943
BES02LOCALCLEAN$ClassFractionalization[ BES02LOCALCLEAN$STATE == "Sergipe"] <- 0.855665236

BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Acre"] <- 0.870672474
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Alagoas"] <- 0.862511128
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Amapá"] <- 0.880175781
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Amazonas"] <- 0.869996241
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Bahia"] <- 0.863686573
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Ceará"] <- 0.851882426
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Distrito Federal"] <- 0.876253376
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Espírito Santo"] <- 0.875626094
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Goiás"] <- 0.870148686
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Maranhão"] <- 0.803179669
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Mato Grosso do Sul"] <- 0.877090116
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Minas Gerais"] <- 0.870513625
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Pará"] <- 0.863616254
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Paraíba"] <- 0.867523599
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Paraná"] <- 0.849048106
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Pernambuco"] <- 0.873792725
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Piauí"] <- 0.816788016
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Rio de Janeiro"] <- 0.881312196
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Rio Grande do Norte"] <- 0.870497884
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Rio Grande do Sul"] <- 0.854516514
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Roraima"] <- 0.885629397
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Santa Catarina"] <- 0.818458994
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "São Paulo"] <- 0.850122651
BES14LOCALCLEAN$ClassFractionalization[ BES14LOCALCLEAN$STATE == "Sergipe"] <- 0.859916151

## Coding controlling variables: Regional Authority Index
BES02LOCALCLEAN$RAI <- 19.5
BES14LOCALCLEAN$RAI <- 19.5

ESEB <- rbind(BES02LOCALCLEAN, BES14LOCALCLEAN)

ESEBLOCAL <- na.omit(ESEB)

ESEBLOCAL$PARTY[ ESEBLOCAL$PARTY == "PFL"] <- "DEM"
ESEBLOCAL$PARTY[ ESEBLOCAL$PARTY == "PL"] <- "PR"
ESEBLOCAL$PARTY[ ESEBLOCAL$PARTY == "PRONA"] <- "PR"
ESEBLOCAL$PARTY[ ESEBLOCAL$PARTY == "PRB"] <- "PR"

## Coding the degree of party nationalization

#PP2003: 4.304939  (Equal Share) and 27.0367  (Equal Change)
#PDT:    8.897054  (Equal Share) and 17.1212  (Equal Change)
#PT:    20.74353   (Equal Share) and 11.9215  (Equal Change)
#PTB:    2.724552  (Equal Share) and 11.20572 (Equal Change)
#PMDB:  36.88386   (Equal Share) and 35.67332 (Equal Change)
#PSC:    0.3628846 (Equal Share) and 5.347685 (Equal Change)
#PR:     3.930946  (Equal Share) and 9.936525 (Equal Change)
#PPS:    5.38e-22  (Equal Share) and 1.063595 (Equal Change)
#DEM:   47.35181   (Equal Share) and 43.30902 (Equal Change)
#PMN:    0.4869283 (Equal Share) and 1.646082 (Equal Change)
#PSB:   14.99037   (Equal Share) and 9.719556 (Equal Change)
#PV:     1.61627   (Equal Share) and 1.994438 (Equal Change)
#PSDB:  25.88173   (Equal Share) and 29.34851 (Equal Change)
#PSOL:   1.182162  (Equal Share) and 1.009469 (Equal Change)
#PCdoB:  0.8503498 (Equal Share) and 2.863987 (Equal Change)

ESEBLOCAL$ES <- NA
ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PP"] <- 4.304939
#ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PDT"] <- 8.897054
ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PT"] <- 20.74353
#ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PTB"] <- 2.724552
ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PMDB"] <- 36.88386
#ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PSC"] <- 0.3628846
ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PR"] <- 3.930946
#ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PPS"] <- 5.38e-22
#ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "DEM"] <- 47.35181
#ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PMN"] <- 0.4869283
ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PSB"] <- 14.99037
#ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PV"] <- 1.61627
ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PSDB"] <- 25.88173
#ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PSOL"] <- 1.182162
#ESEBLOCAL$ES[ ESEBLOCAL$PARTY == "PC do B"] <- 0.8503498

ESEBLOCAL$EC <- NA
ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PP"] <- 27.0367
#ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PDT"] <- 17.1212
ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PT"] <- 11.9215
#ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PTB"] <- 11.20572
ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PMDB"] <- 35.67332
#ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PSC"] <- 5.347685
ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PR"] <- 9.936525
#ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PPS"] <- 1.063595
#ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "DEM"] <- 43.30902
#ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PMN"] <- 1.646082
ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PSB"] <- 9.719556
#ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PV"] <- 1.994438
ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PSDB"] <- 29.34851
#ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PSOL"] <- 1.009469
#ESEBLOCAL$EC[ ESEBLOCAL$PARTY == "PC do B"] <- 2.863987

## Coding controlling variables: District Magnitude
ESEBLOCAL$DistrictMagnitude <- NA
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Acre"] <- 8
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Alagoas"] <- 9
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Amapá"] <- 8
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Amazonas"] <- 8
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Bahia"] <- 39
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Ceará" ] <- 22 
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Distrito Federal"] <- 8
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Espírito Santo"] <- 10
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Goiás"] <- 17
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Maranhão"] <- 18
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Minas Gerais"] <- 53
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Mato Grosso do Sul"] <- 8
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Mato Grosso"] <- 8
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Pará"] <- 17
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Paraíba"] <- 12
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Pernambuco"] <- 25
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Piauí"] <- 10
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Paraná"] <- 30
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Rio de Janeiro"] <- 46
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Rio Grande do Sul"] <- 31
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Rio Grande do Norte"] <- 8
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Roraima"] <- 8
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Santa Catarina"] <- 16
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "Sergipe"] <- 8
ESEBLOCAL$DistrictMagnitude[ ESEBLOCAL$STATE == "São Paulo"] <- 70



### TASK 5: The Estimation of the Multilevel Models with the ESEB (LOCAL)
library(MASS)
library(rcompanion)
library(nlme)
library(r2glmm)
library(ggpubr)
library(Hmisc)

describe(ESEBLOCAL)

ESEBLOCALNEW <- na.omit(ESEBLOCAL)

EqualShareLocal <- lme(ES ~ SATISFACTION + DIFFERENCE + INFLUENCE + GOVERNMENT + IDENTIFICATION + PREFERENCE +
                    ECONOMY +
                    CONTACT + 
                    EDUCATION + INCOME +
                    LEFTRIGHT + 
                    DistrictMagnitude + ENP + EthnicFractionalization + ClassFractionalization,
                  random = ~ SATISFACTION + IDENTIFICATION + CONTACT + EDUCATION | STATE, data = ESEBLOCALNEW, method = "ML")
summary(EqualShareLocal)
R2ES <- r2beta(EqualShareLocal, partial = TRUE, method = "nsj", data = ESEBLOCALNEW)

EqualChangeLocal <- lme(EC ~ SATISFACTION + DIFFERENCE + INFLUENCE + GOVERNMENT + IDENTIFICATION + PREFERENCE +
                     ECONOMY +
                     CONTACT + 
                     EDUCATION + INCOME +
                     LEFTRIGHT + 
                     DistrictMagnitude + ENP + EthnicFractionalization + ClassFractionalization,
                   random = ~ SATISFACTION + IDENTIFICATION + CONTACT + EDUCATION | STATE, data = ESEBLOCALNEW, method = "ML")
summary(EqualChangeLocal)
R2ES <- r2beta(EqualChangeLocal, partial = TRUE, method = "nsj", data = ESEBLOCALNEW)





























