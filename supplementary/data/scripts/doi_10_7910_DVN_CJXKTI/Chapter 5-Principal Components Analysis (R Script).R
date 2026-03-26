### Chapter 5

### In this chapter, based on the technique of principal components analysis, 
### I will examine how associated the political preferences of Brazilian voters as I did for elite preferences in Chapter 3. 
### I have compiled the national dataset with the Brazilian Electoral Studies (2002 and 2014).
### The following dataset includes all issue dimensions that were demonstrated as significant in the principal components analysis of the BLSs:
### political, eocnomic, traditional, and left-right issues. For this purpose, I will work with the 2002 and 2014 waves of the BES.

### TASK 1: Data Preparation
### TASK 2: Principal Components Analysis

### TASK 1
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
BESBA14$Party[ BESBA14$Q5CDA == "2110"] <- "[DEM"
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

BES02CLEAN<- subset(BES2002NATIONAL, select = c(         #Voting Intention
                                                         #Electoral District
                                                p19,     #Satisfaction to the functioning of democracy in Brazil
                                                p20,     #Who governs the country makes a difference
                                                p21,     #Your vote influences whay will happen in the country
                                                p22New,  #The best form of government
                                                p31New,  #Is there a political party that represents you?
                                                p35New,  #Is there a political party that you like?
                                                p108b,   #The government must say everything that companies have to do, like how many bathrooms they have to have.
                                                p04New,  #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                                                #p159New, #Up to which grade did you study?
                                                #p176New, #Adding the income of all the people who live in your house, what is the family income?
                                                p50v1    #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
))

BES14CLEAN <- subset(BES2014NATIONAL, select = c(                #Voting Intention
                                                                 #Electoral District
                                                 Q15,            #Satisfaction to the functioning of democracy in Brazil
                                                 Q7,             #Who governs the country makes a difference
                                                 Q8,             #Your vote influences whay will happen in the country
                                                 PC6New,         #The best form of government
                                                 Q16,            #Is there a political party that represents you?
                                                 Q16A,           #Is there a political party that you like?
                                                 PC12BNew,       #The government must say everything that companies have to do, like how many bathrooms they have to have.
                                                 Q17,            #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                                                 #D3_ESCOLANew,   #Up to which grade did you study?
                                                 #D20A_FXRENDFAM, #Adding the income of all the people who live in your house, what is the family income?
                                                 Q12             #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
))

colnames(BES02CLEAN) <- c(                   #Voting Intention
                                             #Electoral District
                          "SATISFACTION",    #Satisfaction to the functioning of democracy in Brazil
                          "DIFFERENCE",      #Who governs the country makes a difference
                          "INFLUENCE",       #Your vote influences whay will happen in the country
                          "GOVERNMENT",      #The best form of government
                          "IDENTIFICATION",  #Is there a political party that represents you?
                          "PREFERENCE",      #Is there a political party that you like?
                          "ECONOMY",         #The government must say everything that companies have to do, like how many bathrooms they have to have.
                          "CONTACT",         #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                          #"EDUCATION",       #Up to which grade did you study?
                          #"INCOME",          #Adding the income of all the people who live in your house, what is the family income?
                          "LEFT RIGHT"        #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
)

colnames(BES14CLEAN) <- c(                   #Voting Intention
                                             #Electoral District
                          "SATISFACTION",    #Satisfaction to the functioning of democracy in Brazil
                          "DIFFERENCE",      #Who governs the country makes a difference
                          "INFLUENCE",       #Your vote influences whay will happen in the country
                          "GOVERNMENT",      #The best form of government
                          "IDENTIFICATION",  #Is there a political party that represents you?
                          "PREFERENCE",      #Is there a political party that you like?
                          "ECONOMY",         #The government must say everything that companies have to do, like how many bathrooms they have to have.
                          "CONTACT",         #During the election campaign a candidate or person from any party contacted with you to ask for your vote?
                          #"EDUCATION",      #Up to which grade did you study?
                          #"INCOME",         #Adding the income of all the people who live in your house, what is the family income?
                          "LEFT RIGHT"        #What do you consider yourself to be? Zero means that you are left and 10 that you are right.
)

ESEB <- rbind(BES02CLEAN, BES14CLEAN)

ESEBNATIONAL <- na.omit(ESEB)



### TASK 2
library(psych)
library(psychTools)
library(factoextra)
library(FactoMineR)
library(Hmisc)
library(ggplot2)
library(sjPlot)
library(ggpubr)

res.mfa <- MFA(ESEBNATIONAL, 
               group = c(6, 1, 1, 1),
               type = c("s", "s", "s", "s"),
               name.group = c("Political", "Economic",
                              "Traditional", "Left-Right"))

# The Size of Eigenvalues
eig.val <- get_eigenvalue(res.mfa)

#Scree Test (Figure 3.2)
p <- fviz_screeplot(res.mfa, geom = "line")
p + labs(title = "",
         x = "Factors", y = "% of variances") +
  theme_classic() +
  theme(text = element_text(family = "serif"))

# Factor Loadings (Figure 3.1)
r <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 20,
                  palette = "jco")
rr <- r + labs(title = "Factor 1",
               x = "") +
  set_theme(base = theme_classic(),
            axis.angle.x = 90,
            axis.textsize.x = 0.8
  ) +
  theme(text = element_text(family = "serif"))

s <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 20,
                  palette = "jco")
ss <- s + labs(title = "Factor 2",
               x = "") +
  set_theme(base = theme_classic(),
            axis.angle.x = 90,
            axis.textsize.x = 0.8
  ) +
  theme(text = element_text(family = "serif"))

t <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 3, top = 20,
                  palette = "jco")
tt <- t + labs(title = "Factor 3",
               x = "") +
  set_theme(base = theme_classic(),
            axis.angle.x = 90,
            axis.textsize.x = 0.8
  ) +
  theme(text = element_text(family = "serif")) 

u <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 4, top = 20,
                  palette = "jco")
uu <- u + labs(title = "Factor 4",
               x = "") +
  set_theme(base = theme_classic(),
            axis.angle.x = 90,
            axis.textsize.x = 0.8
  ) +
  theme(text = element_text(family = "serif")) 

v <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 5, top = 20,
                  palette = "jco")
vv <- v + labs(title = "Factor 5",
               x = "") +
  set_theme(base = theme_classic(),
            axis.angle.x = 90,
            axis.textsize.x = 0.8
  ) +
  theme(text = element_text(family = "serif")) 

ggarrange(rr, ss, tt, uu, vv,
          ncol = 2, nrow = 3) 
