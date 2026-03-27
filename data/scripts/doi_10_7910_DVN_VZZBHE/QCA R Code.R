##R Code for Payments and Penalties for Democracy: Gendered Electoral Financing in Action Worldwide
install.packages("QCA", dependencies = TRUE)
library(QCA)

##Calibration not done in R (done by researchers prior to analysis)
##Except where indicated data analysis coding taken from Dusa (2019), QCA with R: A Comprehensive Resource

##Necessity QCA tests Configurational
GEFQCANR<-superSubset(GEFQCA, outcome = "Success", neg.out = FALSE, conditions = c("Quota", "PartyDirected", "RegulatoryApproachSD", "PublicFunding", "CandidateSelectionSystemCentralized", "WMPsPrior", "PresenceofPR"), relation="necessity", incl.cut=0.9, cov.cut=0.52, use.tilde=FALSE, use.letters = FALSE)
GEFQCANR

##Necessity 0 Outcome
GEFQCANR0<-superSubset(GEFQCA, outcome = "Success", neg.out = TRUE, conditions = c("Quota", "PartyDirected", "RegulatoryApproachSD", "PublicFunding", "CandidateSelectionSystemCentralized", "WMPsPrior", "PresenceofPR"), relation="necessity", incl.cut=0.9, cov.cut=0.52, use.tilde=FALSE, use.letters = FALSE)
GEFQCANR0

##Sufficiency Tests
GEFQCASR<-superSubset(GEFQCA, outcome = "Success", neg.out = FALSE, conditions = c("Quota", "PartyDirected", "RegulatoryApproachSD", "PublicFunding", "CandidateSelectionSystemCentralized", "WMPsPrior", "PresenceofPR"), relation="sufficiency", use.tilde=FALSE, use.letters = FALSE)
GEFQCASR

##Sufficiency 0 outcome
GEFQCASR0<-superSubset(GEFQCA, outcome = "Success", neg.out = TRUE, conditions = c("Quota", "PartyDirected", "RegulatoryApproachSD", "PublicFunding", "CandidateSelectionSystemCentralized", "WMPsPrior", "PresenceofPR"), relation="sufficiency", use.tilde=FALSE, use.letters = FALSE)
GEFQCASR0

##Returned no cases that met incl or coverage

##Truthtable with 6 conditions 
GEFQCATT<- truthTable(GEFQCA, outcome = "Success", 
                      conditions = c("Quota", "RegulatoryApproachSD", "PresenceofPR", "PublicFunding", "CandidateSelectionSystemCentralized", "WMPsPrior"),
                      complete=FALSE, show.cases=TRUE, sort.by = c("incl,n"))
GEFQCATT

##Boolean Minimization with fewest PIs on [1] Outcome  ##All minimal sums of solutions produced same results
GEFQCACS1<-minimize(GEFQCATT, explain = "1", details = TRUE, show.cases = TRUE)
GEFQCACS1

##Boolean Minimization with fewest PIs on [0] Outcome  ##All minimal sums of solutions produced same results
GEFQCACS0<-minimize(GEFQCATT, explain = "0", details = TRUE, show.cases = TRUE)
GEFQCACS0

##[1] Outcome with logical remainders
##Create complete truth table for analysis
GEFQCATTFull<- truthTable(GEFQCA, outcome = "Success", 
                          conditions = c("Quota", "RegulatoryApproachSD", "PresenceofPR", "PublicFunding", "CandidateSelectionSystemCentralized", "WMPsPrior"),
                          complete=TRUE, show.cases=TRUE, sort.by = c("incl,n"))
GEFQCATTFull

##Parsimonious solution of [1] outcome
GEFQCA1PS<-minimize(GEFQCATTFull, include = "?", rowdom = TRUE, details=TRUE, neg.out=FALSE)
GEFQCA1PS
library(venn)
venn(GEFQCA1PS)

##PI chart
GEFQCA1PS[["PIchart"]]

##Simplifying Assumptions
GEFQCA1PS[["SA"]]

##Parsimonious solution of [0] outcome no row dominance
##0 TruthTable
GEFQCAtt0<-truthTable(GEFQCA, "~Success", 
                      conditions = c("Quota", "RegulatoryApproachSD", "PresenceofPR", "PublicFunding", "CandidateSelectionSystemCentralized", "WMPsPrior"),
                      complete=FALSE, show.cases=TRUE, sort.by = c("incl,n"))
GEFQCAtt0

GEFQCA0PS<-minimize(GEFQCAtt0, include = "?", rowdom = TRUE, details = TRUE)
GEFQCA0PS

##PI Chart
GEFQCA0PS[["PIchart"]]
##Simplifying Assumptions
GEFQCA0PS[["SA"]]
