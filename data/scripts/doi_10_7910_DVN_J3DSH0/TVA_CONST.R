# This gives the mapping between the names of threats, vulnerabilities, and assets and their number-coded values
library(cyra4cm)

CTRL_HARDWARE <- c("Automatic Teller Machine (ATM)", "Laptop", "Desktop", "Point of Sale (POS)", "Smartphone, Tablet")
CTRL_SOFTWARE <- c("Server", "Website", "Software")
CTRL_COMMUNIC <- c("Email", "Telephone Communication or Fax Transmissions", "Social Media")
CTRL_DATA <- c("Privacy Laws/Act Violation (State or Federal)", "Printed Records", "Hard Drive (portable)", "Tape", "CD-ROM", "Thumb Drive", "Cloud derived data")

THREAT_MAP = data.frame(
  name = c("Data Breach",  "Privacy Violation", "Extortion/Fraud", "IT Error", "Other"),
  code = as.character(1:5)
)
VULNERABILITY_MAP = data.frame(
  name = c("communication", "data", "software", "hardware"),
  code = as.character(1:4)
)
ASSET_MAP = data.frame(
  name = c(
    "Personal Financial Identity (PFI)",
    "Personal Identity Information (PII)", 
    "Corporate Loss of Business Income/Services",
    "Personal Health Information (PHI)",
    "Corporate Loss of Digital Assets",
    "Corporate Loss of Financial Assets",
    "Protected Critical Infrastructure Information (PCII)",
    "Other",                                               
    "Privacy Laws/Act Violation (State or Federal)",
    "Cloud derived data"),
  code = as.character(1:10)
)

DEFAULT_TH = 1

INV_OPT_DEFAULT <- rbind(
  c(idx = 1, invest = 0, theta = 1),
  c(idx = 1, invest = 2E6, theta = 0.2),
  c(idx = 2, invest = 0, theta = 1),
  c(idx = 2, invest = 8E6, theta = 0.2),
  c(idx = 3, invest = 0, theta = 1),
  c(idx = 3, invest = 1E6, theta = 0.2),
  c(idx = 4, invest = 0, theta = 1),
  c(idx = 4, invest = 4E5, theta = 0.2)
)

DEFAULT_BUDGET_PCT = 0.5
DEFAULT_DEDUCTIBLE_PCT = DEFAULT_BUDGET_PCT/40

DEFAULT_NU = 0.05
DEFAULT_OM = 1
DEFAULT_ETA = 0.05
DEFAULT_GA = 0.05
DEFAULT_LOADING = 0.5

DEFAULT_NORMALIZER = "cte"
DEFAULT_PENALTY_THRES = 0.9

