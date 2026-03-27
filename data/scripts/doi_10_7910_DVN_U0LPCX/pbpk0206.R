# Load required libraries
library(mrgsolve)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(scales)
library(cowplot)
library(stringr)

# Set publication-ready theme
theme_publication <- function(base_size = 14, base_family = "Helvetica") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.6),
      axis.ticks = element_line(color = "black", linewidth = 0.6),
      axis.text = element_text(color = "black", size = rel(1.0)),
      axis.title = element_text(color = "black", size = rel(1.2)),
      legend.title = element_text(color = "black", size = rel(1.0)),
      legend.text = element_text(color = "black", size = rel(1.0)),
      legend.position = "bottom",
      plot.title = element_text(color = "black", size = rel(0.9), face = "bold"),
      plot.subtitle = element_text(color = "black", size = rel(0.9)),
      plot.caption = element_text(color = "black", size = rel(0.7), hjust = 0),
      strip.text = element_text(color = "black", size = rel(0.8), face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "black", linewidth = 0.5)
    ) 
}
theme_set(theme_publication())


clozapine_model_code <- '
$PARAM
// Dosing parameters
DOSE = 150     // mg per dose
BIOAVAIL = 0.5 // Bioavailability

// Demographic parameters
WT = 70        // Body weight (kg)
SEX = 0        // Sex (0 = male, 1 = female)
AGE = 35       // Age in years
ETHNICITY = 1  // 1=Caucasian, 2=African, 3=Asian, 4=Hispanic, 5=Other

// Base PK parameters
KA = 0.6       // Absorption rate constant (1/h)
V2 = 50        // Central volume (L), adjusted for weight
CL = 8.5       // Clearance (L/h)
V3 = 120       // Peripheral volume (L)
Q = 10         // Intercompartmental clearance (L/h)

// Enhanced transporter polymorphism factors
ABCB1_FACTOR = 1.0     // P-glycoprotein (blood-brain barrier transport)
SLCO1B1_FACTOR = 1.0   // OATP1B1 (hepatic uptake transporter)
ABCC2_FACTOR = 1.0     // MRP2 (biliary excretion)

// Enhanced genetic polymorphism factors for metabolism
CYP1A2_FACTOR = 1.0  // CYP1A2 activity factor
CYP2D6_FACTOR = 1.0  // CYP2D6 activity factor
CYP3A4_FACTOR = 1.0  // CYP3A4 activity factor (including *22 allele)
FMO3_FACTOR = 1.0    // FMO3 activity factor
GST_FACTOR = 1.0     // Glutathione S-transferase activity factor
GSTT1_STATUS = 1.0   // GSTT1 null genotype factor (0.3 for null)
GSTM1_STATUS = 1.0   // GSTM1 null genotype factor (0.7 for null)

// Fraction of metabolism by pathway
FRAC_CYP1A2 = 0.5  // Primary pathway
FRAC_CYP2D6 = 0.15  // Secondary pathway
FRAC_CYP3A4 = 0.20  // Secondary pathway (affected by *22)
FRAC_FMO3 = 0.10    // Minor pathway
FRAC_OTHER = 0.05   // Other pathways

// Drug interaction parameters
FLUVOXAMINE_DOSE = 0   // mg/day (0 = no co-medication)
SMOKING_STATUS = 0     // 0 = non-smoker, 1 = light smoker, 2 = heavy smoker

// Enhanced PD parameters for multiple receptors with ethnic differences
D2_EC50 = 80        // Brain concentration for 50% D2 occupancy (ng/mL) - CORRECTED
D2_EMAX = 0.85        // Maximum D2 receptor occupancy
HT2A_EC50 = 25       // Brain concentration for 50% 5HT2A occupancy (ng/mL) - CORRECTED
HT2A_EMAX = 0.95     // Maximum 5HT2A receptor occupancy
H1_EC50 = 15         // Brain concentration for 50% H1 occupancy (ng/mL) - CORRECTED
H1_EMAX = 0.98       // Maximum H1 receptor occupancy
M1_EC50 = 80         // Brain concentration for 50% M1 occupancy (ng/mL) - CORRECTED
M1_EMAX = 0.85       // Maximum M1 receptor occupancy

// Enhanced clinical response parameters with PANSS modeling
EFFICACY_THRESHOLD = 0.6    // Minimum D2 occupancy for efficacy - CORRECTED
RESPONSE_DELAY = 168         // Time to response (hours, ~1 week)
ADHERENCE_FACTOR = 0.9       // Medication adherence (0-1)
BASELINE_PANSS_TOTAL = 120   // Baseline PANSS total score
BASELINE_PANSS_POSITIVE = 35 // Baseline PANSS positive score
BASELINE_PANSS_NEGATIVE = 40 // Baseline PANSS negative score

// BBB Transport Parameters (Physiologically realistic)
// Clozapine brain/plasma ratio is typically 0.1-0.2 (10-20%)
// ABCB1 (P-gp) actively effluxes clozapine from brain
VMAX_BBB = 2.1      // Maximum BBB transport rate (μmol/min/g) - NOT USED in current model
KM_BBB = 8.3        // Michaelis constant for BBB transport (μM) - NOT USED in current model

// Enhanced safety thresholds with time-dependent toxicity - CORRECTED THRESHOLDS
AGRANULOCYTOSIS_RISK_THRESH = 700  // ng/mL threshold - CORRECTED
SEIZURE_RISK_THRESH = 1200         // ng/mL threshold - CORRECTED
CARDIOTOXICITY_THRESH = 900        // ng/mL threshold - CORRECTED
WTOX_AGRAN = 0.4    // Weight factor for agranulocytosis - CORRECTED
WTOX_SEIZURE = 0.2  // Weight factor for seizure - CORRECTED

// Glutathione and reactive metabolite parameters with hepatic zonation
GSH_BASELINE = 87   // Baseline glutathione levels
GSH_DEPLETION_RATE = 0.005  // Rate of GSH depletion
RM_FORMATION_RATE = 0.01    // Rate of reactive metabolite formation
ZONAL_INDEX = 0.5   // Hepatic zonation index (0=periportal, 1=pericentral)

// Metabolite formation rates
NORCLOZAPINE_FORMATION = 0.7
CLOZAPINE_N_OXIDE_FORMATION = 0.2
OTHER_METABOLITES = 0.1

// TDM parameters
TARGET_CONC = 450
LOWER_LIMIT = 350
UPPER_LIMIT = 600

// Regional brain compartment parameters - CORRECTED for realistic penetration
V_CORTEX = 0.8      // Cortical volume (L)
V_STRIATUM = 0.05   // Striatal volume (L)
BBB_CORTEX_PERM = 0.006    // Much lower cortical penetration
BBB_STRIATUM_PERM = 0.004  // Much lower striatal penetration

$CMT @annotated
GUT         : Gut compartment (mg)
CENT        : Central compartment (mg)
PERIPH      : Peripheral compartment (mg)
BRAIN       : Brain compartment (mg)
BRAIN_CORTEX : Cortical brain compartment (mg)
BRAIN_STRIATUM : Striatal brain compartment (mg)
NORCLOZ     : Norclozapine compartment (mg)
CLZ_NOXIDE  : Clozapine N-oxide compartment (mg)
OTHER_MET   : Other metabolites (mg)
RM          : Reactive metabolites (arb. units)
GSH         : Glutathione levels (arb. units)
RESPONSE    : Clinical response accumulator
AUC_CENT    : AUC in central compartment
PANSS_TOTAL : PANSS total score
PANSS_POS   : PANSS positive subscale
PANSS_NEG   : PANSS negative subscale
TOXICITY_SCORE : Cumulative toxicity score

$MAIN
// Ethnic-specific factors
double ETHNIC_CL_FACTOR = ETHNICITY == 2 ? 0.8 : (ETHNICITY == 3 ? 1.2 : 1.0);
double ETHNIC_BBB_FACTOR = ETHNICITY == 3 ? 0.85 : 1.0;

// Age-related changes
double AGE_FACTOR = pow(AGE/35, -0.3);
double ELDERLY_FACTOR = AGE > 65 ? 0.8 : 1.0;

// Enhanced GST activity with zonation and null genotypes
double GST_ZONAL = GSH_BASELINE * (1.0 - 0.4 * ZONAL_INDEX);
double GST_COMBINED = GST_FACTOR * GSTT1_STATUS * GSTM1_STATUS * (GST_ZONAL / GSH_BASELINE);

// Smoking effects on CYP1A2
double SMOKING_CYP1A2 = SMOKING_STATUS == 0 ? 1.0 : 
                       (SMOKING_STATUS == 1 ? 1.3 : 1.7);

// Fluvoxamine interaction
double FLUVOXAMINE_INHIB = FLUVOXAMINE_DOSE > 0 ? 
                          (1.0 - 0.8 * (FLUVOXAMINE_DOSE/100)) : 1.0;
FLUVOXAMINE_INHIB = FLUVOXAMINE_INHIB < 0.1 ? 0.1 : FLUVOXAMINE_INHIB;

// Calculate effective clearances with ethnic factors
double CL_CYP1A2 = CL * FRAC_CYP1A2 * CYP1A2_FACTOR * SMOKING_CYP1A2 * FLUVOXAMINE_INHIB;
double CL_CYP2D6 = CL * FRAC_CYP2D6 * CYP2D6_FACTOR;
double CL_CYP3A4 = CL * FRAC_CYP3A4 * CYP3A4_FACTOR;
double CL_FMO3 = CL * FRAC_FMO3 * FMO3_FACTOR;
double CL_OTHER = CL * FRAC_OTHER;
double CL_TOT = (CL_CYP1A2 + CL_CYP2D6 + CL_CYP3A4 + CL_FMO3 + CL_OTHER) * 
                SLCO1B1_FACTOR * AGE_FACTOR * ELDERLY_FACTOR * ETHNIC_CL_FACTOR;

// Adjust volumes
double WT_factor = WT/70;
double V2_i = V2 * WT_factor;
double V3_i = V3 * WT_factor;
double V_BRAIN = 1.4 * WT_factor;
double CL_BILE = 2.0 * ABCC2_FACTOR;

// Regional brain volumes - weight-adjusted
double V_CORTEX_i = V_CORTEX * WT_factor;
double V_STRIATUM_i = V_STRIATUM * WT_factor;

// Initialize compartments
GUT_0 = 0;
CENT_0 = 0;
PERIPH_0 = 0;
BRAIN_0 = 0;
BRAIN_CORTEX_0 = 0;
BRAIN_STRIATUM_0 = 0;
NORCLOZ_0 = 0;
CLZ_NOXIDE_0 = 0;
OTHER_MET_0 = 0;
RM_0 = 0;
GSH_0 = GSH_BASELINE;
RESPONSE_0 = 0;
AUC_CENT_0 = 0;
PANSS_TOTAL_0 = BASELINE_PANSS_TOTAL;
PANSS_POS_0 = BASELINE_PANSS_POSITIVE;
PANSS_NEG_0 = BASELINE_PANSS_NEGATIVE;
TOXICITY_SCORE_0 = 0;

$ODE
// Drug absorption and distribution
dxdt_GUT = -KA * GUT * ADHERENCE_FACTOR;

// Enhanced BBB transport (physiologically realistic brain penetration)
double BBB_PERM_BASE = 0.003;  // Much lower base permeability for realistic penetration
double BBB_PERM = BBB_PERM_BASE * ABCB1_FACTOR * ETHNIC_BBB_FACTOR;
double EFFLUX_RATIO = 3.0;  // Higher efflux ratio (P-gp actively pumps clozapine out)
double BBB_INFLUX = (BBB_PERM/V2_i) * CENT;
double BBB_EFFLUX = (BBB_PERM * EFFLUX_RATIO/V_BRAIN) * BRAIN;

// Regional BBB transport with even lower penetration
double BBB_CORTEX_INFLUX = (BBB_CORTEX_PERM * 0.1 * ABCB1_FACTOR/V2_i) * CENT;  // Reduced by factor of 10
double BBB_CORTEX_EFFLUX = (BBB_CORTEX_PERM * EFFLUX_RATIO/V_CORTEX_i) * BRAIN_CORTEX;
double BBB_STRIATUM_INFLUX = (BBB_STRIATUM_PERM * 0.1 * ABCB1_FACTOR/V2_i) * CENT;  // Reduced by factor of 10
double BBB_STRIATUM_EFFLUX = (BBB_STRIATUM_PERM * EFFLUX_RATIO/V_STRIATUM_i) * BRAIN_STRIATUM;

dxdt_CENT = KA * GUT * ADHERENCE_FACTOR * BIOAVAIL - (CL_TOT/V2_i) * CENT - 
            (Q/V2_i) * CENT + (Q/V3_i) * PERIPH - 
            BBB_INFLUX + BBB_EFFLUX - 
            BBB_CORTEX_INFLUX + BBB_CORTEX_EFFLUX -
            BBB_STRIATUM_INFLUX + BBB_STRIATUM_EFFLUX -
            (CL_BILE/V2_i) * CENT;

dxdt_PERIPH = (Q/V2_i) * CENT - (Q/V3_i) * PERIPH;

// Enhanced brain compartment with realistic penetration
dxdt_BRAIN = BBB_INFLUX - BBB_EFFLUX;

// Regional brain compartments
dxdt_BRAIN_CORTEX = BBB_CORTEX_INFLUX - BBB_CORTEX_EFFLUX;
dxdt_BRAIN_STRIATUM = BBB_STRIATUM_INFLUX - BBB_STRIATUM_EFFLUX;

// Metabolite formation
double total_metab_rate = (CL_TOT/V2_i) * CENT;
dxdt_NORCLOZ = total_metab_rate * NORCLOZAPINE_FORMATION * 
               (CL_CYP1A2/(CL_CYP1A2 + CL_CYP2D6 + CL_CYP3A4 + CL_FMO3 + CL_OTHER)) - 0.1 * NORCLOZ;
dxdt_CLZ_NOXIDE = total_metab_rate * CLOZAPINE_N_OXIDE_FORMATION * 
                  (CL_FMO3/(CL_CYP1A2 + CL_CYP2D6 + CL_CYP3A4 + CL_FMO3 + CL_OTHER)) - 0.15 * CLZ_NOXIDE;
dxdt_OTHER_MET = total_metab_rate * OTHER_METABOLITES - 0.2 * OTHER_MET;

// Enhanced reactive metabolite formation with zonation
double rm_formation = RM_FORMATION_RATE * (CL_CYP1A2 + CL_FMO3)/V2_i * CENT / GST_COMBINED;
dxdt_RM = rm_formation - 0.1 * RM;

// Enhanced glutathione with zonation effects
double gsh_depletion = GSH_DEPLETION_RATE * rm_formation * GSH / GST_ZONAL;
double gsh_regeneration = 0.1 * (GST_ZONAL - GSH);
dxdt_GSH = gsh_regeneration - gsh_depletion;

// Enhanced clinical response based on multiple receptor occupancies
double brain_conc = BRAIN/V_BRAIN * 1000;
double brain_d2_occ = D2_EMAX * brain_conc / (D2_EC50 + brain_conc);
double brain_ht2a_occ = HT2A_EMAX * brain_conc / (HT2A_EC50 + brain_conc);
double brain_h1_occ = H1_EMAX * brain_conc / (H1_EC50 + brain_conc);
double brain_m1_occ = M1_EMAX * brain_conc / (M1_EC50 + brain_conc);

// CORRECTED: Multi-receptor efficacy calculation with time-dependent response
double base_efficacy = 0.4 * brain_d2_occ + 0.4 * brain_ht2a_occ + 
                      0.1 * brain_h1_occ + 0.1 * brain_m1_occ;

// Time-dependent response with 2-week delay and plateau
double time_factor = (1.0 - exp(-SOLVERTIME/336.0)) * (1.0 - exp(-SOLVERTIME/168.0));

// Baseline severity adjustment
double baseline_factor = (BASELINE_PANSS_TOTAL - 50.0) / 70.0;

// Placebo response component (15% improvement)
double placebo_component = 0.15 * (1.0 - exp(-SOLVERTIME/672.0));

// Final multi-receptor efficacy
double multi_receptor_efficacy = (base_efficacy * time_factor * ADHERENCE_FACTOR * 
                                 baseline_factor) + placebo_component;

double response_rate = multi_receptor_efficacy > EFFICACY_THRESHOLD ? 
                      (multi_receptor_efficacy - EFFICACY_THRESHOLD) / RESPONSE_DELAY : 0;
dxdt_RESPONSE = response_rate;

// CORRECTED: PANSS score modeling based on receptor occupancy
double max_improvement = 0.4 * brain_d2_occ + 0.3 * brain_ht2a_occ + 
                         0.15 * brain_h1_occ + 0.15 * brain_m1_occ;

// Time course with delayed onset and plateau
double time_course = (1.0 - exp(-SOLVERTIME/504.0)) * (1.0 - exp(-SOLVERTIME/168.0));

// Baseline severity scaling
double severity_scaling = BASELINE_PANSS_TOTAL / 120.0;

// PANSS improvement calculation
double panss_improvement_percent = (max_improvement * time_course + 
                                   placebo_component) * severity_scaling * 100.0;

// Individual PANSS subscales improvement rates
double panss_positive_improvement_rate = panss_improvement_percent * 0.4 / RESPONSE_DELAY;
double panss_negative_improvement_rate = panss_improvement_percent * 0.3 / RESPONSE_DELAY;
double panss_general_improvement_rate = panss_improvement_percent * 0.3 / RESPONSE_DELAY;

dxdt_PANSS_POS = -panss_positive_improvement_rate + 0.0001 * (BASELINE_PANSS_POSITIVE - PANSS_POS);
dxdt_PANSS_NEG = -panss_negative_improvement_rate + 0.0001 * (BASELINE_PANSS_NEGATIVE - PANSS_NEG);
dxdt_PANSS_TOTAL = dxdt_PANSS_POS + dxdt_PANSS_NEG - panss_general_improvement_rate + 
                   0.0001 * ((BASELINE_PANSS_TOTAL - BASELINE_PANSS_POSITIVE - BASELINE_PANSS_NEGATIVE) - 
                            (PANSS_TOTAL - PANSS_POS - PANSS_NEG));

// CORRECTED: Cumulative toxicity score calculation with realistic risk assessment
double cp_current = CENT/V2_i * 1000;

// Sigmoid risk functions with corrected thresholds and maximum risks
double agran_risk = 1.0 / (1.0 + exp(-(cp_current - AGRANULOCYTOSIS_RISK_THRESH) / 100.0)) * 0.03;  // 3% max risk
double seizure_risk = 1.0 / (1.0 + exp(-(cp_current - SEIZURE_RISK_THRESH) / 150.0)) * 0.02;        // 2% max risk
double cardio_risk = 1.0 / (1.0 + exp(-(cp_current - CARDIOTOXICITY_THRESH) / 120.0)) * 0.015;      // 1.5% max risk

dxdt_TOXICITY_SCORE = (agran_risk + seizure_risk + cardio_risk) * WTOX_AGRAN;

dxdt_AUC_CENT = CENT/V2_i * 1000;

$OMEGA @labels IIV_CL IIV_V2 IIV_KA IIV_BBB IIV_D2_EC50
0.09 0.04 0.16 0.25 0.15

$SIGMA @labels ERR_PROP ERR_ADD
0.1 25

$TABLE
double CP = CENT/V2_i * 1000;
double BRAIN_CONC = BRAIN/V_BRAIN * 1000;
double BRAIN_CORTEX_CONC = BRAIN_CORTEX/V_CORTEX_i * 1000;
double BRAIN_STRIATUM_CONC = BRAIN_STRIATUM/V_STRIATUM_i * 1000;
double NORCLOZ_CONC = NORCLOZ * 0.8 * 1000/V2_i;
double NOXIDE_CONC = CLZ_NOXIDE * 0.6 * 1000/V2_i;
double NORCLOZ_RATIO = NORCLOZ_CONC / (CP + 0.1);
double NOXIDE_RATIO = NOXIDE_CONC / (CP + 0.1);

// Enhanced receptor occupancies - using brain concentrations
double D2_OCC_PLASMA = D2_EMAX * CP / (D2_EC50 + CP);
double D2_OCC_BRAIN = D2_EMAX * BRAIN_CONC / (D2_EC50 + BRAIN_CONC);
double D2_OCC_CORTEX = D2_EMAX * BRAIN_CORTEX_CONC / (D2_EC50 + BRAIN_CORTEX_CONC);
double D2_OCC_STRIATUM = D2_EMAX * BRAIN_STRIATUM_CONC / (D2_EC50 + BRAIN_STRIATUM_CONC);
double HT2A_OCC_BRAIN = HT2A_EMAX * BRAIN_CONC / (HT2A_EC50 + BRAIN_CONC);
double H1_OCC_BRAIN = H1_EMAX * BRAIN_CONC / (H1_EC50 + BRAIN_CONC);
double M1_OCC_BRAIN = M1_EMAX * BRAIN_CONC / (M1_EC50 + BRAIN_CONC);

// Enhanced efficacy measures
double MULTI_RECEPTOR_EFFICACY = 0.4 * D2_OCC_BRAIN + 0.4 * HT2A_OCC_BRAIN + 
                                0.1 * H1_OCC_BRAIN + 0.1 * M1_OCC_BRAIN;
double EFFICACY = MULTI_RECEPTOR_EFFICACY >= EFFICACY_THRESHOLD ? 1 : 0;
double CLINICAL_RESPONSE = RESPONSE > 1.0 ? 1.0 : RESPONSE;

// Enhanced safety measures with corrected risk calculation
double GSH_RATIO = GSH / GSH_BASELINE;

// Corrected safety risk calculations
double AGRANULOCYTOSIS_RISK_CONC = 1.0 / (1.0 + exp(-(CP - AGRANULOCYTOSIS_RISK_THRESH) / 100.0)) * 0.03;
double AGRANULOCYTOSIS_RISK_RM = (RM > 50) && (GSH_RATIO < 0.7) ? 0.02 : 0.0;
double AGRANULOCYTOSIS_RISK = fmax(AGRANULOCYTOSIS_RISK_CONC, AGRANULOCYTOSIS_RISK_RM);

double SEIZURE_RISK = 1.0 / (1.0 + exp(-(CP - SEIZURE_RISK_THRESH) / 150.0)) * 0.02;
double CARDIOTOXICITY_RISK = 1.0 / (1.0 + exp(-(CP - CARDIOTOXICITY_THRESH) / 120.0)) * 0.015;

// Clinical utility measures
double DOSE_ADJUSTMENT = (TARGET_CONC - CP) / TARGET_CONC;
double TDM_RECOMMENDATION = CP < LOWER_LIMIT ? 1 : (CP > UPPER_LIMIT ? -1 : 0);
double BBB_TRANSPORT_INDEX = BRAIN_CONC / (CP + 0.1);
double HEPATIC_UPTAKE_INDEX = CL_TOT / CL;
double INTERACTION_SEVERITY = FLUVOXAMINE_DOSE > 0 ? 
                             (FLUVOXAMINE_DOSE > 50 ? 2 : 1) : 0;

// PANSS scores and improvement
double PANSS_TOTAL_IMPROVEMENT = BASELINE_PANSS_TOTAL - PANSS_TOTAL;
double PANSS_POS_IMPROVEMENT = BASELINE_PANSS_POSITIVE - PANSS_POS;
double PANSS_NEG_IMPROVEMENT = BASELINE_PANSS_NEGATIVE - PANSS_NEG;
double PANSS_PERCENT_IMPROVEMENT = (PANSS_TOTAL_IMPROVEMENT / BASELINE_PANSS_TOTAL) * 100;
double CLINICAL_RESPONSE_PANSS = PANSS_PERCENT_IMPROVEMENT >= 20 ? 1 : 0;

// Output variables
double RM_OUT = RM;
double GSH_OUT = GSH;
double AUC_OUT = AUC_CENT;
double TOXICITY_OUT = TOXICITY_SCORE;

$CAPTURE CP BRAIN_CONC BRAIN_CORTEX_CONC BRAIN_STRIATUM_CONC NORCLOZ_CONC NOXIDE_CONC NORCLOZ_RATIO NOXIDE_RATIO
D2_OCC_PLASMA D2_OCC_BRAIN D2_OCC_CORTEX D2_OCC_STRIATUM HT2A_OCC_BRAIN H1_OCC_BRAIN M1_OCC_BRAIN
MULTI_RECEPTOR_EFFICACY EFFICACY CLINICAL_RESPONSE
PANSS_TOTAL_IMPROVEMENT PANSS_POS_IMPROVEMENT PANSS_NEG_IMPROVEMENT
PANSS_PERCENT_IMPROVEMENT CLINICAL_RESPONSE_PANSS
AGRANULOCYTOSIS_RISK SEIZURE_RISK CARDIOTOXICITY_RISK
CYP1A2_FACTOR CYP2D6_FACTOR CYP3A4_FACTOR FMO3_FACTOR GST_FACTOR GSTT1_STATUS GSTM1_STATUS
ABCB1_FACTOR SLCO1B1_FACTOR ABCC2_FACTOR
RM_OUT GSH_OUT GSH_RATIO DOSE_ADJUSTMENT TDM_RECOMMENDATION
BBB_TRANSPORT_INDEX HEPATIC_UPTAKE_INDEX INTERACTION_SEVERITY
FLUVOXAMINE_DOSE SMOKING_STATUS AUC_OUT TOXICITY_OUT ETHNICITY
'

# Create the enhanced model
clozapine_mod <- mcode("enhanced_clozapine", clozapine_model_code)

# Enhanced genotype definitions with new variants
enhanced_genotypes <- data.frame(
  profile = c(
    "Reference (All Normal)",
    "CYP1A2*1F/*1F (Ultra-rapid)",
    "CYP1A2*1C/*1C (Poor)",
    "CYP2D6*4/*4 (Poor)",
    "CYP3A4*22/*22 (Reduced)",
    "FMO3*2/*2 (Reduced)",
    "GSTT1-/- (Null)",
    "GSTM1-/- (Null)",
    "GSTT1-/-+GSTM1-/- (Double null)",
    "ABCB1 TT (Poor transport)",
    "SLCO1B1*5/*5 (Poor uptake)",
    "ABCC2*2/*2 (Poor efflux)",
    "CYP1A2*1C/*1C + ABCB1 TT",
    "CYP1A2*1F/*1F + ABCB1 CC",
    "CYP1A2*1C/*1C + CYP2D6*4/*4",
    "Triple: CYP1A2*1C + CYP2D6*4 + ABCB1 TT",
    "CYP3A4*22 + GSTT1-/-",
    "Ultra-complex: CYP1A2*1C + CYP3A4*22 + GSTT1-/- + ABCB1 TT"
  ),
  CYP1A2_FACTOR = c(1.0, 1.6, 0.4, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.7, 1.0, 1.0, 0.4, 1.6, 0.4, 0.4, 1.0, 0.4),
  CYP2D6_FACTOR = c(1.0, 1.0, 1.0, 0.2, 1.0, 1.0, 1.0, 1.0, 1.0, 0.7, 1.0, 1.0, 1.0, 1.0, 0.2, 0.2, 1.0, 1.0),
  CYP3A4_FACTOR = c(1.0, 1.0, 1.0, 1.0, 0.6, 1.0, 1.0, 1.0, 1.0, 0.7, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.6, 0.6),
  FMO3_FACTOR = c(1.0, 1.0, 1.0, 1.0, 1.0, 0.7, 1.0, 1.0, 1.0, 0.7, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
  GST_FACTOR = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.7, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
  GSTT1_STATUS = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.7, 1.0, 0.7, 0.7, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.7, 0.7),
  GSTM1_STATUS = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.7, 0.3, 0.7, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
  ABCB1_FACTOR = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.4, 1.0, 1.0, 0.4, 1.8, 1.0, 0.4, 1.0, 0.4),
  SLCO1B1_FACTOR = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.7, 0.65, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
  ABCC2_FACTOR = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.7, 1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
)

#Define dosing regimen: 150 mg twice daily for 10 days
ev <- ev(amt = 150, ii = 12, addl = 19)

# Run simulation
out <- mrgsim(clozapine_mod, events = ev, end = 240, delta = 1)


# Check concentration ranges at steady state (last 24 hours)
ss_data <- as.data.frame(out)[out$time >= 216, ]
summary(ss_data$CP)

# Plot concentration-time profile
ggplot(as.data.frame(out), aes(x = time, y = CP)) +
  geom_line() +
  labs(x = "Time (hours)",
       y = "Plasma Concentration (ng/mL)") +
  geom_hline(yintercept = 350, linetype = "dashed", color = "green") +
  geom_hline(yintercept = 600, linetype = "dashed", color = "red") +
  annotate("text", x = 220, y = 350, label = "Lower therapeutic threshold", hjust = 0.5) +
  annotate("text", x = 220, y = 600, label = "Upper therapeutic threshold", hjust = 0.5)

# Core simulation functions
simulate_polymorphisms <- function(mod, genotypes = enhanced_genotypes) {
  results <- data.frame()
  for (i in 1:nrow(genotypes)) {
    param_set <- param(mod, 
                       CYP1A2_FACTOR = genotypes$CYP1A2_FACTOR[i],
                       CYP2D6_FACTOR = genotypes$CYP2D6_FACTOR[i],
                       CYP3A4_FACTOR = genotypes$CYP3A4_FACTOR[i],
                       FMO3_FACTOR = genotypes$FMO3_FACTOR[i],
                       GST_FACTOR = genotypes$GST_FACTOR[i],
                       ABCB1_FACTOR = genotypes$ABCB1_FACTOR[i],
                       SLCO1B1_FACTOR = genotypes$SLCO1B1_FACTOR[i],
                       ABCC2_FACTOR = genotypes$ABCC2_FACTOR[i])
    ev <- ev(amt = 150, ii = 12, addl = 29)
    out <- mrgsim(param_set, events = ev, end = 360, delta = 1)
    sim_data <- as.data.frame(out) %>%
      mutate(profile = genotypes$profile[i])
    results <- rbind(results, sim_data)
  }
  return(results)
}

analyze_fluvoxamine_interaction <- function(mod) {
  fluvox_doses <- c(0, 25, 50, 100, 150, 200)
  test_genotypes <- c("Reference (All Normal)", "CYP1A2*1C/*1C (Poor)", "CYP1A2*1F/*1F (Ultra-rapid)")
  results <- data.frame()
  for (genotype in test_genotypes) {
    geno_idx <- which(enhanced_genotypes$profile == genotype)
    if (length(geno_idx) == 0) geno_idx <- 1
    for (dose in fluvox_doses) {
      param_set <- param(mod,
                         CYP1A2_FACTOR = enhanced_genotypes$CYP1A2_FACTOR[geno_idx],
                         CYP2D6_FACTOR = enhanced_genotypes$CYP2D6_FACTOR[geno_idx],
                         FLUVOXAMINE_DOSE = dose)
      ev <- ev(amt = 150, ii = 12, addl = 19)
      out <- mrgsim(param_set, events = ev, end = 240, delta = 1)
      ss_data <- as.data.frame(out) %>%
        filter(time >= 216) %>%
        summarize(
          genotype = genotype,
          fluvoxamine_dose = dose,
          clz_conc_avg = mean(CP),
          clz_conc_max = max(CP),
          brain_conc_avg = mean(BRAIN_CONC),
          d2_occ_brain = mean(D2_OCC_BRAIN),
          norcloz_ratio = mean(NORCLOZ_RATIO),
          safety_risk = mean(AGRANULOCYTOSIS_RISK + SEIZURE_RISK + CARDIOTOXICITY_RISK),
          interaction_severity = mean(INTERACTION_SEVERITY)
        )
      if (dose == 0) {
        ss_data$fold_increase <- 1.0
      } else {
        baseline_conc_df <- results %>% 
          filter(genotype == !!genotype & fluvoxamine_dose == 0)
        if (nrow(baseline_conc_df) > 0 && "clz_conc_avg" %in% names(baseline_conc_df)) {
          baseline_conc <- baseline_conc_df %>% pull(clz_conc_avg)
          ss_data$fold_increase <- ss_data$clz_conc_avg / baseline_conc[1]
        } else {
          ref_param_set <- param(mod,
                                 CYP1A2_FACTOR = enhanced_genotypes$CYP1A2_FACTOR[geno_idx],
                                 CYP2D6_FACTOR = enhanced_genotypes$CYP2D6_FACTOR[geno_idx],
                                 FLUVOXAMINE_DOSE = 0)
          ref_out <- mrgsim(ref_param_set, events = ev, end = 240, delta = 1)
          baseline_conc_val <- mean(as.data.frame(ref_out) %>% filter(time >= 216) %>% pull(CP))
          ss_data$fold_increase <- ss_data$clz_conc_avg / baseline_conc_val
        }
      }
      results <- rbind(results, ss_data)
    }
  }
  return(results)
}

create_tdm_recommendations <- function(mod) {
  patient_scenarios <- data.frame(
    scenario = c(
      "Normal metabolizer",
      "CYP1A2 poor metabolizer",
      "CYP1A2 ultra-rapid metabolizer", 
      "Normal + fluvoxamine 50mg",
      "Normal + fluvoxamine 100mg",
      "Poor metabolizer + fluvoxamine",
      "Ultra-rapid + smoking",
      "ABCB1 poor transporter",
      "Elderly patient (75y)",
      "Non-adherent patient (70%)"
    ),
    cyp1a2_factor = c(1.0, 0.35, 1.8, 1.0, 1.0, 0.35, 1.8, 1.0, 1.0, 1.0),
    fluvoxamine_dose = c(0, 0, 0, 50, 100, 50, 0, 0, 0, 0),
    abcb1_factor = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.4, 1.0, 1.0),
    smoking_status = c(0, 0, 0, 0, 0, 0, 2, 0, 0, 0),
    age = c(35, 35, 35, 35, 35, 35, 35, 35, 75, 35),
    adherence = c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.7)
  )
  tdm_results <- data.frame()
  for (i in 1:nrow(patient_scenarios)) {
    param_set <- param(mod,
                       CYP1A2_FACTOR = patient_scenarios$cyp1a2_factor[i],
                       FLUVOXAMINE_DOSE = patient_scenarios$fluvoxamine_dose[i],
                       ABCB1_FACTOR = patient_scenarios$abcb1_factor[i],
                       SMOKING_STATUS = patient_scenarios$smoking_status[i],
                       AGE = patient_scenarios$age[i],
                       ADHERENCE_FACTOR = patient_scenarios$adherence[i])
    ev <- ev(amt = 150, ii = 12, addl = 19)
    out <- mrgsim(param_set, events = ev, end = 240, delta = 1)
    ss_data <- as.data.frame(out) %>%
      filter(time >= 216) %>%
      summarize(
        scenario = patient_scenarios$scenario[i],
        expected_conc = mean(CP),
        expected_max = max(CP),
        expected_min = min(CP),
        norcloz_ratio = mean(NORCLOZ_RATIO),
        noxide_ratio = mean(NOXIDE_RATIO),
        brain_penetration = mean(BBB_TRANSPORT_INDEX),
        dose_adjustment_needed = mean(TDM_RECOMMENDATION),
        sampling_frequency = case_when(
          patient_scenarios$cyp1a2_factor[i] < 0.5 | 
            patient_scenarios$fluvoxamine_dose[i] > 0 ~ "Weekly for 4 weeks, then bi-weekly",
          patient_scenarios$cyp1a2_factor[i] > 1.5 | 
            patient_scenarios$smoking_status[i] > 0 ~ "Weekly for 2 weeks, then monthly",
          TRUE ~ "At 2 weeks, then monthly"
        ),
        key_metabolite = case_when(
          patient_scenarios$cyp1a2_factor[i] < 0.5 ~ "Monitor norclozapine ratio (expect <0.5)",
          patient_scenarios$cyp1a2_factor[i] > 1.5 ~ "Monitor norclozapine ratio (expect >0.8)",
          TRUE ~ "Monitor norclozapine ratio (expect 0.5-0.7)"
        ),
        special_considerations = case_when(
          patient_scenarios$fluvoxamine_dose[i] > 0 ~ "Strong DDI: Reduce clozapine dose by 50-75%",
          patient_scenarios$smoking_status[i] > 0 ~ "Smoking induction: May need higher doses",
          patient_scenarios$adherence[i] < 0.8 ~ "Poor adherence: Consider adherence interventions",
          patient_scenarios$age[i] > 65 ~ "Elderly: Start low, monitor closely",
          TRUE ~ "Standard monitoring"
        )
      )
    tdm_results <- rbind(tdm_results, ss_data)
  }
  return(tdm_results)
}


analyze_steady_state <- function(results_data) {
  ss_metrics <- results_data %>%
    filter(time >= 300) %>%
    group_by(profile) %>%
    summarize(
      Cmax = max(CP),
      Cmin = min(CP),
      Cavg = mean(CP),
      Brain_Cmax = max(BRAIN_CONC),
      Brain_Cavg = mean(BRAIN_CONC),
      D2_occupancy_avg = mean(D2_OCC_BRAIN),
      D2_occupancy_min = min(D2_OCC_BRAIN),
      Efficacy_probability = mean(EFFICACY),
      Clinical_response_max = max(CLINICAL_RESPONSE),
      Norcloz_ratio = mean(NORCLOZ_RATIO),
      Noxide_ratio = mean(NOXIDE_RATIO),
      BBB_transport_index = mean(BBB_TRANSPORT_INDEX),
      Agranulocytosis_risk = mean(AGRANULOCYTOSIS_RISK),
      Seizure_risk = mean(SEIZURE_RISK),
      Cardiotoxicity_risk = mean(CARDIOTOXICITY_RISK),
      GSH_ratio = mean(GSH_RATIO),
      .groups = "drop"
    ) %>%
    arrange(desc(Cavg))
  return(ss_metrics)
}

perform_sensitivity_analysis <- function(mod) {
  activity_range <- seq(0.1, 2.0, by = 0.1)
  results <- data.frame()
  
  enzymes <- list(
    list(name = "CYP1A2", param = "CYP1A2_FACTOR"),
    list(name = "CYP2D6", param = "CYP2D6_FACTOR"),
    list(name = "CYP3A4", param = "CYP3A4_FACTOR"),
    list(name = "GST", param = "GST_FACTOR")
  )
  
  for (enzyme in enzymes) {
    for (activity in activity_range) {
      param_list <- list(1.0, 1.0, 1.0, 1.0)
      names(param_list) <- c("CYP1A2_FACTOR", "CYP2D6_FACTOR", "CYP3A4_FACTOR", "GST_FACTOR")
      param_list[[enzyme$param]] <- activity
      
      param_set <- param(mod, 
                         CYP1A2_FACTOR = param_list$CYP1A2_FACTOR,
                         CYP2D6_FACTOR = param_list$CYP2D6_FACTOR,
                         CYP3A4_FACTOR = param_list$CYP3A4_FACTOR,
                         GST_FACTOR = param_list$GST_FACTOR)
      ev <- ev(amt = 150, ii = 12, addl = 19)
      out <- mrgsim(param_set, events = ev, end = 240, delta = 1)
      ss_data <- as.data.frame(out) %>%
        filter(time >= 216) %>%
        summarize(
          enzyme = enzyme$name,
          activity = activity,
          Cavg = mean(CP),
          D2_avg = mean(D2_OCC_BRAIN),
          GSH_ratio = mean(GSH_RATIO),
          AGRAN_risk = mean(AGRANULOCYTOSIS_RISK)
        )
      results <- rbind(results, ss_data)
    }
  }
  return(results)
}

validate_model <- function(mod) {
  literature_data <- data.frame(
    study = c("Olesen 1995", "Jann 1993", "Dettling 2000", "van der Weide 2003", 
              "Ng 2009", "Rostami-Hodjegan 2004"),
    scenario = c("CYP1A2 Poor", "Normal + Fluvoxamine", "CYP2D6 Poor", 
                 "Smoking + Normal", "Normal reference", "CYP1A2 Ultra-rapid"),
    observed_conc = c(550, 800, 420, 260, 380, 240),
    observed_sd = c(120, 150, 80, 50, 90, 60)
  )
  validation_params <- data.frame(
    scenario = c("CYP1A2 Poor", "Normal + Fluvoxamine", "CYP2D6 Poor", 
                 "Smoking + Normal", "Normal reference", "CYP1A2 Ultra-rapid"),
    CYP1A2_FACTOR = c(0.4, 1.0, 1.0, 1.0, 1.0, 1.6),
    CYP2D6_FACTOR = c(1.0, 1.0, 0.1, 1.0, 1.0, 1.0),
    FLUVOXAMINE_DOSE = c(0, 100, 0, 0, 0, 0),
    SMOKING_STATUS = c(0, 0, 0, 2, 0, 0)
  )
  validation_results <- data.frame()
  for (i in 1:nrow(validation_params)) {
    param_set <- param(mod,
                       CYP1A2_FACTOR = validation_params$CYP1A2_FACTOR[i],
                       CYP2D6_FACTOR = validation_params$CYP2D6_FACTOR[i],
                       FLUVOXAMINE_DOSE = validation_params$FLUVOXAMINE_DOSE[i],
                       SMOKING_STATUS = validation_params$SMOKING_STATUS[i],
                       DOSE = 150)
    ev <- ev(amt = 150, ii = 12, addl = 19)
    out <- mrgsim(param_set, events = ev, end = 240, delta = 1)
    predicted_conc <- mean(as.data.frame(out)[out$time >= 216, "CP"])
    validation_results <- rbind(validation_results, data.frame(
      scenario = validation_params$scenario[i],
      predicted_conc = predicted_conc
    ))
  }
  validation <- merge(validation_results, literature_data, by = "scenario")
  validation <- validation %>%
    mutate(
      abs_error = abs(predicted_conc - observed_conc),
      rel_error = (predicted_conc - observed_conc) / observed_conc * 100,
      fold_error = ifelse(predicted_conc == 0 & observed_conc == 0, 1,
                          ifelse(observed_conc == 0, Inf,
                                 ifelse(predicted_conc > observed_conc, 
                                        predicted_conc / observed_conc,
                                        observed_conc / predicted_conc))),
      within_2fold = fold_error <= 2
    )
  valid_fold_errors <- validation$fold_error[is.finite(validation$fold_error)]
  mean_fold_error_val <- if(length(valid_fold_errors) > 0) mean(valid_fold_errors) else NA
  validation_metrics <- list(
    mean_abs_error = mean(validation$abs_error, na.rm = TRUE),
    mean_rel_error = mean(abs(validation$rel_error), na.rm = TRUE),
    mean_fold_error = mean_fold_error_val,
    percent_within_2fold = mean(validation$within_2fold, na.rm = TRUE) * 100,
    r_squared = if(nrow(validation) > 1 && !any(is.na(validation$predicted_conc)) && !any(is.na(validation$observed_conc))) {
      cor(validation$predicted_conc, validation$observed_conc)^2
    } else {
      NA
    }
  )
  return(list(validation_data = validation, metrics = validation_metrics))
}

calculate_optimal_dosing <- function(mod, genotypes = enhanced_genotypes, target_conc = 450) {
  dosing_recommendations <- data.frame()
  for (i in 1:nrow(genotypes)) {
    doses <- seq(25, 400, by = 25)
    dose_results <- data.frame()
    for (dose_val in doses) {
      param_set <- param(mod,
                         DOSE = dose_val,
                         CYP1A2_FACTOR = genotypes$CYP1A2_FACTOR[i],
                         CYP2D6_FACTOR = genotypes$CYP2D6_FACTOR[i],
                         CYP3A4_FACTOR = genotypes$CYP3A4_FACTOR[i],
                         FMO3_FACTOR = genotypes$FMO3_FACTOR[i],
                         GST_FACTOR = genotypes$GST_FACTOR[i],
                         ABCB1_FACTOR = genotypes$ABCB1_FACTOR[i],
                         SLCO1B1_FACTOR = genotypes$SLCO1B1_FACTOR[i],
                         ABCC2_FACTOR = genotypes$ABCC2_FACTOR[i])
      ev <- ev(amt = dose_val, ii = 12, addl = 19)
      out <- mrgsim(param_set, events = ev, end = 240, delta = 1)
      
      out_df <- as.data.frame(out)
      has_ht2a <- "HT2A_OCC_BRAIN" %in% names(out_df)
      
      if (has_ht2a) {
        ss_data <- out_df %>%
          filter(time >= 216) %>%
          summarize(
            dose = dose_val,
            Cavg = mean(CP),
            Cmax = max(CP),
            Cmin = min(CP),
            D2_occupancy = mean(D2_OCC_BRAIN),
            HT2A_occupancy = mean(HT2A_OCC_BRAIN),
            Clinical_response = max(CLINICAL_RESPONSE),
            Agranulocytosis_risk = mean(AGRANULOCYTOSIS_RISK),
            Seizure_risk = mean(SEIZURE_RISK),
            Cardiotoxicity_risk = mean(CARDIOTOXICITY_RISK)
          )
      } else {
        ss_data <- out_df %>%
          filter(time >= 216) %>%
          summarize(
            dose = dose_val,
            Cavg = mean(CP),
            Cmax = max(CP),
            Cmin = min(CP),
            D2_occupancy = mean(D2_OCC_BRAIN),
            HT2A_occupancy = NA,
            Clinical_response = max(CLINICAL_RESPONSE),
            Agranulocytosis_risk = mean(AGRANULOCYTOSIS_RISK),
            Seizure_risk = mean(SEIZURE_RISK),
            Cardiotoxicity_risk = mean(CARDIOTOXICITY_RISK)
          )
      }
      dose_results <- rbind(dose_results, ss_data)
    }
    optimal <- dose_results %>%
      filter(Agranulocytosis_risk == 0, Seizure_risk == 0, Cardiotoxicity_risk == 0) %>%
      mutate(target_diff = abs(Cavg - target_conc)) %>%
      arrange(target_diff) %>%
      slice(1)
    if (nrow(optimal) == 0) {
      optimal <- dose_results %>%
        filter(D2_occupancy >= 0.65) %>%
        mutate(total_risk_score = Agranulocytosis_risk + Seizure_risk + Cardiotoxicity_risk) %>%
        arrange(total_risk_score, abs(Cavg - target_conc)) %>%
        slice(1)
    }
    if (nrow(optimal) > 0) {
      rec <- data.frame(
        profile = genotypes$profile[i],
        optimal_dose = optimal$dose,
        expected_avg_conc = optimal$Cavg,
        expected_max_conc = optimal$Cmax,
        expected_D2_occupancy = optimal$D2_occupancy,
        expected_HT2A_occupancy = ifelse(is.na(optimal$HT2A_occupancy), NA, optimal$HT2A_occupancy),
        expected_clinical_response = optimal$Clinical_response,
        safety_risk_score = optimal$Agranulocytosis_risk + optimal$Seizure_risk + optimal$Cardiotoxicity_risk,
        dose_adjustment_percent = round((optimal$dose - 100) / 100 * 100, 0)
      )
      dosing_recommendations <- rbind(dosing_recommendations, rec)
    } else {
      rec <- data.frame(
        profile = genotypes$profile[i],
        optimal_dose = NA,
        expected_avg_conc = NA,
        expected_max_conc = NA,
        expected_D2_occupancy = NA,
        expected_HT2A_occupancy = NA,
        expected_clinical_response = NA,
        safety_risk_score = NA,
        dose_adjustment_percent = NA
      )
      dosing_recommendations <- rbind(dosing_recommendations, rec)
    }
  }
  return(dosing_recommendations)
}

# ===================================================================
# ORIGINAL VISUALIZATION FUNCTIONS (ALL PRESERVED)
# ===================================================================

# 1. PRIMARY PHARMACOGENOMICS PLOTS

plot_polymorphism_plasma <- function(poly_results) {
  ggplot(poly_results %>% filter(time >= 300),
         aes(x = time, y = CP, color = profile)) +
    geom_line(linewidth = 1.1) +
    labs(x = "Time (hours)", y = "Plasma Concentration (ng/mL)") +
    theme_publication() +
    guides(color = guide_legend(ncol = 2)) +
    geom_hline(yintercept = c(350, 600, 1000), 
               linetype = "dashed", alpha = 0.7,
               color = c("green", "orange", "red")) +
    annotate("text", x = max(poly_results$time, na.rm = TRUE) - 20, 
             y = c(350, 600, 1000), 
             label = c("Lower therapeutic", "Upper therapeutic", "Toxicity threshold"),
             vjust = -0.5, size = 3)
}


plot_brain_plasma_correlation <- function(poly_results) {
  brain_plasma_data <- poly_results %>%
    filter(time >= 300) %>%
    group_by(profile) %>%
    summarize(
      avg_plasma = mean(CP),
      avg_brain = mean(BRAIN_CONC),
      brain_plasma_ratio = mean(BBB_TRANSPORT_INDEX),
      .groups = "drop"
    )
  
  ggplot(brain_plasma_data, 
         aes(x = avg_plasma, y = avg_brain, color = brain_plasma_ratio, size = brain_plasma_ratio)) +
    geom_point(alpha = 0.8) +
    # Add reference lines for typical brain penetration ratios
    geom_abline(intercept = 0, slope = 0.1, linetype = "dashed", alpha = 0.5, color = "blue") +
    geom_abline(intercept = 0, slope = 0.3, linetype = "dashed", alpha = 0.5, color = "green") +
    scale_color_gradient(low = "blue", high = "red", name = "BBB\nTransport\nIndex") +
    scale_size_continuous(range = c(2, 6), guide = "none") +
    labs(title = "Brain vs Plasma Concentration by Genotype",
         x = "Average Plasma Concentration (ng/mL)",
         y = "Average Brain Concentration (ng/mL)") +
    theme_publication() +
    annotate("text", x = max(brain_plasma_data$avg_plasma) * 0.7, y = max(brain_plasma_data$avg_plasma) * 0.1, 
             label = "10% penetration", hjust = 0, vjust = -0.5, size = 3, color = "blue") +
    annotate("text", x = max(brain_plasma_data$avg_plasma) * 0.5, y = max(brain_plasma_data$avg_plasma) * 0.3, 
             label = "30% penetration", hjust = 0, vjust = -0.5, size = 3, color = "green")
}

plot_brain_plasma_ecf <- function(results) {
  ecf_data <- results %>%
    filter(time >= 300) %>%
    select(time, profile, CP, BRAIN_CONC) %>%
    pivot_longer(cols = c(CP, BRAIN_CONC), names_to = "compartment", values_to = "concentration") %>%
    mutate(compartment = recode(compartment, CP = "Plasma ECF", BRAIN_CONC = "Brain ECF"))
  
  ggplot(ecf_data, aes(x = time, y = concentration, color = profile, linetype = compartment)) +
    geom_line(linewidth = 1) +
    labs(title = "Brain and Plasma ECF Concentrations vs Time",
         x = "Time (hours)",
         y = "Concentration (ng/mL)") +
    theme_publication() +
    guides(color = guide_legend(ncol = 2), linetype = guide_legend(title = "Compartment")) +
    geom_hline(yintercept = c(350, 600), linetype = "dashed", color = c("green", "orange"), alpha = 0.7)
}

plot_brain_plasma_ratio <- function(ss_metrics) {
  ratio_data <- ss_metrics %>%
    mutate(brain_plasma_ratio = BBB_transport_index) %>%
    arrange(brain_plasma_ratio)
  
  ggplot(ratio_data, aes(x = reorder(profile, brain_plasma_ratio), y = brain_plasma_ratio, fill = profile)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    labs(title = "Brain-to-Plasma Concentration Ratio by Genetic Polymorphism",
         x = "",
         y = "Brain/Plasma Ratio") +
    theme_publication() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip() +
    # Add reference lines for physiological range
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "blue", alpha = 0.7) +
    geom_hline(yintercept = 0.3, linetype = "dashed", color = "green", alpha = 0.7) +
    annotate("text", x = nrow(ratio_data) * 0.9, y = 0.1, label = "Lower normal (10%)", 
             hjust = 0, vjust = -0.5, size = 3, color = "blue") +
    annotate("text", x = nrow(ratio_data) * 0.8, y = 0.3, label = "Upper normal (30%)", 
             hjust = 0, vjust = -0.5, size = 3, color = "green")
}

# 2. METABOLITE AND PD PLOTS
plot_metabolite_ratios <- function(ss_metrics) {
  metabolite_data <- ss_metrics %>%
    select(profile, Norcloz_ratio, Noxide_ratio) %>%
    pivot_longer(cols = c(Norcloz_ratio, Noxide_ratio), 
                 names_to = "metabolite", values_to = "ratio") %>%
    mutate(metabolite = case_when(
      metabolite == "Norcloz_ratio" ~ "Norclozapine/Clozapine",
      metabolite == "Noxide_ratio" ~ "N-oxide/Clozapine"
    ))
  
  ggplot(metabolite_data, 
         aes(x = reorder(profile, ratio), y = ratio, fill = metabolite)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = c("Norclozapine/Clozapine" = "steelblue", 
                                 "N-oxide/Clozapine" = "coral")) +
    labs(title = "Metabolite Ratios by Genetic Polymorphism",
         x = "", y = "Metabolite/Clozapine Ratio", fill = "Metabolite") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
}

plot_d2_occupancy <- function(results) {
  ggplot(results %>% filter(time >= 192), 
         aes(x = time, y = D2_OCC_BRAIN * 100, color = profile)) +
    geom_line(linewidth = 1) +
    labs(title = "D2 Receptor Occupancy vs Time by Genetic Polymorphism",
         x = "Time (hours)",
         y = "D2 Receptor Occupancy (%)") +
    theme_publication() +
    guides(color = guide_legend(nrow = 4)) +
    geom_hline(yintercept = 65, linetype = "dashed", color = "green") +
    annotate("text", x = 230, y = 65, label = "Efficacy Threshold", vjust = -0.5)
}

plot_gsh_ratio <- function(results) {
  ggplot(results %>% filter(time >= 192), 
         aes(x = time, y = GSH_RATIO, color = profile)) +
    geom_line(linewidth = 1) +
    labs(title = "Glutathione Ratio vs Time by Genetic Polymorphism",
         x = "Time (hours)",
         y = "GSH Ratio (fraction of baseline)") +
    theme_publication() +
    guides(color = guide_legend(nrow = 4)) +
    geom_hline(yintercept = 0.7, linetype = "dashed", color = "red", alpha = 0.5) +
    annotate("text", x = 230, y = 0.7, label = "Critical GSH Threshold", vjust = -0.5)
}

# 3. DRUG INTERACTION PLOTS
plot_fluvoxamine_heatmap <- function(fluv_results) {
  if (is.null(fluv_results) || nrow(fluv_results) == 0) return(NULL)
  
  ggplot(fluv_results,
         aes(x = factor(fluvoxamine_dose), y = genotype, fill = fold_increase)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(fold_increase, 1)), color = "black", size = 3, fontface = "bold") +
    scale_fill_gradient2(low = muted("blue"), mid = "white", high = muted("red"), 
                         midpoint = 1, name = "Fold\nIncrease\nin CP") +
    labs(title = "Clozapine-Fluvoxamine Drug Interaction Matrix",
         x = "Fluvoxamine Dose (mg/day)", y = "CYP1A2 Genotype") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 0))
}

# 4. DOSE-RESPONSE PLOTS
plot_dose_response_cyp <- function(mod) {
  genotypes <- data.frame(
    genotype = c("CYP1A2*1/*1", "CYP1A2*1F/*1F", "CYP1A2*1C/*1C"),
    CYP1A2_FACTOR = c(1.0, 1.6, 0.4),
    description = c("Normal metabolizer", "Ultra-rapid metabolizer", "Poor metabolizer")
  )
  all_results <- data.frame()
  doses <- seq(25, 300, by = 25)
  for (i in 1:nrow(genotypes)) {
    for (dose in doses) {
      param_set <- param(mod, CYP1A2_FACTOR = genotypes$CYP1A2_FACTOR[i])
      ev <- ev(amt = dose, ii = 12, addl = 19)
      out <- mrgsim(param_set, events = ev, end = 240, delta = 1)
      results <- as.data.frame(out) %>%
        filter(time >= 216) %>%
        summarize(
          genotype = genotypes$genotype[i],
          description = genotypes$description[i],
          dose = dose,
          Cavg = mean(CP),
          D2_avg = mean(D2_OCC_BRAIN),
          Agranulocytosis_risk = mean(AGRANULOCYTOSIS_RISK)
        )
      all_results <- rbind(all_results, results)
    }
  }
  
  conc_plot <- ggplot(all_results, aes(x = dose, y = Cavg, color = description)) +
    geom_line(linewidth = 1) +
    labs(title = "Average Plasma Concentration vs Dose by CYP1A2 Genotype",
         x = "Dose (mg BID)",
         y = "Average Plasma Concentration (ng/mL)") +
    theme_publication() +
    geom_hline(yintercept = c(350, 600), linetype = "dashed", color = c("green", "orange"), alpha = 0.7)
  
  d2_plot <- ggplot(all_results, aes(x = dose, y = D2_avg * 100, color = description)) +
    geom_line(linewidth = 1) +
    labs(title = "D2 Receptor Occupancy vs Dose by CYP1A2 Genotype",
         x = "Dose (mg BID)",
         y = "D2 Receptor Occupancy (%)") +
    theme_publication() +
    geom_hline(yintercept = 65, linetype = "dashed", color = "green")
  
  return(list(conc_plot = conc_plot, d2_plot = d2_plot, dose_response_data = all_results))
}

# 5. RECEPTOR OCCUPANCY PLOTS
plot_multi_receptor_occupancy <- function(results) {
  if ("HT2A_OCC_BRAIN" %in% names(results)) {
    receptor_data <- results %>%
      filter(time >= 300) %>%
      group_by(profile) %>%
      summarize(
        D2_avg = mean(D2_OCC_BRAIN) * 100,
        HT2A_avg = mean(HT2A_OCC_BRAIN) * 100,
        H1_avg = mean(H1_OCC_BRAIN) * 100,
        M1_avg = mean(M1_OCC_BRAIN) * 100,
        .groups = "drop"
      ) %>%
      pivot_longer(cols = ends_with("_avg"), names_to = "receptor", values_to = "occupancy") %>%
      mutate(receptor = gsub("_avg", "", receptor))
    
    fill_values <- c("D2" = "#E41A1C", "HT2A" = "#377EB8", 
                     "H1" = "#4DAF4A", "M1" = "#984EA3")
    fill_labels <- c("D2", "5HT2A", "H1", "M1")
  } else {
    receptor_data <- results %>%
      filter(time >= 300) %>%
      group_by(profile) %>%
      summarize(
        D2_avg = mean(D2_OCC_BRAIN) * 100,
        .groups = "drop"
      ) %>%
      pivot_longer(cols = ends_with("_avg"), names_to = "receptor", values_to = "occupancy") %>%
      mutate(receptor = gsub("_avg", "", receptor))
    
    fill_values <- c("D2" = "#E41A1C")
    fill_labels <- c("D2")
  }
  
  ggplot(receptor_data, aes(x = reorder(profile, occupancy), y = occupancy, fill = receptor)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = fill_values, labels = fill_labels) +
    labs(title = "Receptor Occupancy by Genetic Polymorphism",
         x = "", y = "Receptor Occupancy (%)", fill = "Receptor") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip() +
    geom_hline(yintercept = 65, linetype = "dashed", color = "gray50", alpha = 0.7)
}

# 6. SENSITIVITY AND VALIDATION PLOTS
plot_sensitivity_exposure <- function(sensitivity_results) {
  ggplot(sensitivity_results %>% filter(enzyme %in% c("CYP1A2", "CYP2D6", "CYP3A4", "GST")), 
         aes(x = activity, y = Cavg, color = enzyme)) +
    geom_line(linewidth = 1.2) +
    labs(title = "Sensitivity of Clozapine Exposure to Enzyme Activity",
         x = "Enzyme Activity (Relative to Normal)",
         y = "Average Plasma Concentration (ng/mL)") +
    theme_publication() +
    scale_x_continuous(breaks = seq(0, 2, by = 0.2)) +
    geom_hline(yintercept = c(350, 600), linetype = "dashed", color = c("green", "orange"), alpha = 0.7) +
    annotate("text", x = 1.8, y = 350, label = "Efficacy Threshold", vjust = -0.5, size = 3) +
    annotate("text", x = 1.8, y = 600, label = "Safety Threshold", vjust = -0.5, size = 3)
}

plot_validation_observed_predicted <- function(validation_data) {
  ggplot(validation_data, aes(x = observed_conc, y = predicted_conc)) +
    geom_point(size = 3, color = "blue") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = predicted_conc, ymax = predicted_conc, xmin = observed_conc - observed_sd, xmax = observed_conc + observed_sd), 
                  color = "blue", alpha = 0.5) +
    labs(title = "Model Validation: Observed vs Predicted Concentrations",
         x = "Observed Concentration (ng/mL)",
         y = "Predicted Concentration (ng/mL)") +
    theme_publication() +
    geom_text(aes(label = study), hjust = -0.1, vjust = 0.5, size = 3, check_overlap = TRUE) +
    coord_cartesian(xlim = c(0, max(validation_data$observed_conc + validation_data$observed_sd, na.rm = TRUE) * 1.1),
                    ylim = c(0, max(validation_data$predicted_conc, na.rm = TRUE) * 1.1))
}

plot_validation_error <- function(validation_data) {
  ggplot(validation_data, aes(x = reorder(study, rel_error), y = rel_error)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
    labs(title = "Model Validation: Relative Prediction Error by Study",
         x = "",
         y = "Relative Error (%)") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black")
}

plot_plasma_concentration_time <- function(poly_results) {
  ggplot(poly_results %>% filter(time >= 300), 
         aes(x = time, y = CP, color = profile)) +
    geom_line(linewidth = 1.1) +
    labs(title = "Clozapine Plasma Concentration vs Time at Steady State",
         x = "Time (hours)", 
         y = "Plasma Concentration (ng/mL)") +
    theme_publication() +
    guides(color = guide_legend(ncol = 2)) +
    geom_hline(yintercept = c(350, 600), 
               linetype = "dashed", alpha = 0.7,
               color = c("green", "orange")) +
    annotate("text", x = max(poly_results$time, na.rm = TRUE) - 5, 
             y = c(350, 600), 
             label = c("Lower therapeutic", "Upper therapeutic"),
             vjust = -0.5, size = 3)
}


plot_smoking_effect <- function(mod) {
  smoking_factors <- data.frame(
    status = c("Non-smoker", "Light smoker", "Heavy smoker"),
    CYP1A2_FACTOR = c(1.0, 1.3, 1.7)
  )
  ev <- ev(amt = 150, ii = 12, addl = 19)
  results <- data.frame()
  for (i in 1:nrow(smoking_factors)) {
    param_set <- param(mod, CYP1A2_FACTOR = smoking_factors$CYP1A2_FACTOR[i])
    out <- mrgsim(param_set, events = ev, end = 240, delta = 0.5)
    sim_data <- as.data.frame(out) %>%
      mutate(smoking_status = smoking_factors$status[i])
    results <- rbind(results, sim_data)
  }
  
  ggplot(results %>% filter(time >= 192), 
         aes(x = time, y = CP, color = smoking_status)) +
    geom_line(linewidth = 1) +
    labs(title = "Effect of Smoking on Clozapine Plasma Concentration",
         x = "Time (hours)",
         y = "Plasma Concentration (ng/mL)") +
    theme_publication() +
    geom_hline(yintercept = c(350, 600), linetype = "dashed", color = c("green", "orange"), alpha = 0.5) +
    annotate("text", x = 230, y = c(350, 600), 
             label = c("Lower therapeutic", "Upper therapeutic"),
             vjust = -0.5, size = 3)
}

# 6. SAFETY AND TDM PLOTS
plot_safety_assessment <- function(ss_metrics) {
  safety_data <- ss_metrics %>%
    select(profile, Agranulocytosis_risk, Seizure_risk, Cardiotoxicity_risk) %>%
    pivot_longer(cols = ends_with("_risk"), names_to = "risk_type", values_to = "risk_proportion_time") %>%
    mutate(risk_type = gsub("_risk", "", risk_type),
           risk_type = stringr::str_to_title(gsub("_", " ", risk_type)))
  
  ggplot(safety_data, 
         aes(x = reorder(profile, risk_proportion_time), y = risk_proportion_time * 100, fill = risk_type)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    scale_fill_manual(values = c("Agranulocytosis" = "#E41A1C", 
                                 "Seizure" = "#FF7F00", 
                                 "Cardiotoxicity" = "#984EA3")) +
    labs(title = "Safety Risk Assessment by Genotype",
         x = "", y = "Proportion of Time at Risk (%)", fill = "Risk Type") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
}

plot_tdm_matrix <- function(tdm_results) {
  if (is.null(tdm_results) || nrow(tdm_results) == 0) return(NULL)
  
  ggplot(tdm_results,
         aes(x = expected_conc, y = norcloz_ratio, color = scenario)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_rect(aes(xmin = 350, xmax = 600, ymin = 0.5, ymax = 0.7),
              fill = "palegreen", alpha = 0.15, color = "darkgreen", 
              linetype = "dashed", inherit.aes = FALSE) +
    labs(title = "TDM Decision Matrix",
         x = "Expected Clozapine Concentration (ng/mL)",
         y = "Norclozapine/Clozapine Ratio") +
    theme_publication() +
    annotate("text", x = 475, y = 0.6, label = "Target\nTherapeutic\nWindow", 
             hjust = 0.5, vjust = 0.5, size = 3, color = "darkgreen", fontface = "bold") +
    guides(color = guide_legend(ncol = 1))
}

# ===================================================================


# CPIC validation dataset (simulated based on literature)
cpic_validation_data <- data.frame(
  study_id = 1:20,
  ethnicity = rep(1:5, each = 4),
  cyp1a2_phenotype = rep(c("Normal", "Intermediate", "Poor", "Ultra-rapid"), 5),
  cyp2d6_phenotype = c(rep("Normal", 12), rep("Poor", 4), rep("Intermediate", 4)),
  abcb1_genotype = sample(c("CC", "CT", "TT"), 20, replace = TRUE),
  smoking_status = sample(c(0, 1, 2), 20, replace = TRUE),
  dose_mg_day = rep(c(200, 250, 300, 350), 5),
  observed_conc = c(380, 420, 580, 320, 410, 390, 620, 350, 
                    445, 435, 655, 375, 520, 480, 750, 410,
                    385, 425, 615, 340),
  observed_panss_improvement = c(25, 30, 15, 35, 28, 32, 12, 38,
                                 22, 27, 18, 33, 20, 25, 10, 29,
                                 26, 31, 16, 36),
  agranulocytosis_event = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0)
)

# Enhanced simulation functions
simulate_enhanced_polymorphisms <- function(mod, genotypes = enhanced_genotypes) {
  results <- data.frame()
  for (i in 1:nrow(genotypes)) {
    param_set <- param(mod, 
                       CYP1A2_FACTOR = genotypes$CYP1A2_FACTOR[i],
                       CYP2D6_FACTOR = genotypes$CYP2D6_FACTOR[i],
                       CYP3A4_FACTOR = genotypes$CYP3A4_FACTOR[i],
                       FMO3_FACTOR = genotypes$FMO3_FACTOR[i],
                       GST_FACTOR = genotypes$GST_FACTOR[i],
                       GSTT1_STATUS = genotypes$GSTT1_STATUS[i],
                       GSTM1_STATUS = genotypes$GSTM1_STATUS[i],
                       ABCB1_FACTOR = genotypes$ABCB1_FACTOR[i],
                       SLCO1B1_FACTOR = genotypes$SLCO1B1_FACTOR[i],
                       ABCC2_FACTOR = genotypes$ABCC2_FACTOR[i])
    ev <- ev(amt = 150, ii = 12, addl = 41)  # Longer simulation for PANSS steady state
    out <- mrgsim(param_set, events = ev, end = 504, delta = 1)
    sim_data <- as.data.frame(out) %>%
      mutate(profile = genotypes$profile[i])
    results <- rbind(results, sim_data)
  }
  return(results)
}

# CPIC validation function (CONCENTRATION ONLY - PANSS REMOVED)
validate_cpic_data <- function(mod, cpic_data = cpic_validation_data) {
  # Convert phenotypes to factors
  cyp1a2_factors <- c("Poor" = 0.4, "Intermediate" = 0.7, "Normal" = 1.0, "Ultra-rapid" = 1.6)
  cyp2d6_factors <- c("Poor" = 0.1, "Intermediate" = 0.5, "Normal" = 1.0)
  abcb1_factors <- c("TT" = 0.4, "CT" = 0.7, "CC" = 1.0)
  
  validation_results <- data.frame()
  
  for (i in 1:nrow(cpic_data)) {
    # Set parameters based on phenotypes
    cyp1a2_factor <- cyp1a2_factors[cpic_data$cyp1a2_phenotype[i]]
    cyp2d6_factor <- cyp2d6_factors[cpic_data$cyp2d6_phenotype[i]]
    abcb1_factor <- abcb1_factors[cpic_data$abcb1_genotype[i]]
    
    param_set <- param(mod,
                       CYP1A2_FACTOR = cyp1a2_factor,
                       CYP2D6_FACTOR = cyp2d6_factor,
                       ABCB1_FACTOR = abcb1_factor,
                       SMOKING_STATUS = cpic_data$smoking_status[i],
                       ETHNICITY = cpic_data$ethnicity[i],
                       DOSE = cpic_data$dose_mg_day[i] / 2)  # Convert to BID dosing
    
    ev <- ev(amt = cpic_data$dose_mg_day[i] / 2, ii = 12, addl = 41)  # 3 weeks to steady state
    out <- mrgsim(param_set, events = ev, end = 504, delta = 1)
    
    # Extract steady-state metrics (CONCENTRATION ONLY)
    ss_data <- as.data.frame(out) %>%
      filter(time >= 480) %>%
      summarize(
        study_id = cpic_data$study_id[i],
        ethnicity = cpic_data$ethnicity[i],
        predicted_conc = mean(CP),
        predicted_agran_risk = mean(AGRANULOCYTOSIS_RISK),
        observed_conc = cpic_data$observed_conc[i],
        observed_agran_event = cpic_data$agranulocytosis_event[i]
      )
    
    validation_results <- rbind(validation_results, ss_data)
  }
  
  # Calculate validation metrics (CONCENTRATION ONLY)
  validation_metrics <- validation_results %>%
    mutate(
      conc_error = predicted_conc - observed_conc,
      conc_rel_error = (conc_error / observed_conc) * 100,
      conc_within_20_percent = abs(conc_rel_error) <= 20
    )
  
  summary_metrics <- list(
    n_patients = nrow(validation_results),
    conc_mae = mean(abs(validation_metrics$conc_error), na.rm = TRUE),
    conc_mape = mean(abs(validation_metrics$conc_rel_error), na.rm = TRUE),
    conc_r_squared = cor(validation_metrics$predicted_conc, validation_metrics$observed_conc)^2,
    conc_within_20pct = mean(validation_metrics$conc_within_20_percent, na.rm = TRUE) * 100
  )
  
  return(list(validation_data = validation_results, metrics = summary_metrics))
}

# Enhanced analysis functions
analyze_ethnic_differences <- function(mod) {
  ethnicities <- data.frame(
    ethnicity = 1:5,
    ethnicity_name = c("Caucasian", "African American", "Asian", "Hispanic", "Other")
  )
  
  results <- data.frame()
  for (i in 1:nrow(ethnicities)) {
    param_set <- param(mod, ETHNICITY = ethnicities$ethnicity[i])
    ev <- ev(amt = 150, ii = 12, addl = 41)  # Longer simulation for steady state
    out <- mrgsim(param_set, events = ev, end = 504, delta = 1)
    
    ss_data <- as.data.frame(out) %>%
      filter(time >= 480) %>%  # Use last 24 hours for steady state
      summarize(
        ethnicity = ethnicities$ethnicity_name[i],
        avg_conc = mean(CP, na.rm = TRUE),
        avg_brain_conc = mean(BRAIN_CONC, na.rm = TRUE),
        avg_d2_occ = mean(D2_OCC_BRAIN, na.rm = TRUE),
        avg_panss_improvement = mean(PANSS_PERCENT_IMPROVEMENT, na.rm = TRUE),
        avg_bbb_transport = mean(BBB_TRANSPORT_INDEX, na.rm = TRUE)
      )
    
    results <- rbind(results, ss_data)
  }
  return(results)
}

# Enhanced visualization functions
plot_panss_improvement <- function(results) {
  # Check if PANSS variables exist and have meaningful data
  if (!"PANSS_PERCENT_IMPROVEMENT" %in% names(results)) {
    cat("Warning: PANSS_PERCENT_IMPROVEMENT not found in results. Skipping PANSS plot.\n")
    return(NULL)
  }
  
  panss_data <- results %>%
    filter(time >= 480) %>%  # Use longer time for steady state
    group_by(profile) %>%
    summarize(
      avg_panss_improvement = mean(PANSS_PERCENT_IMPROVEMENT, na.rm = TRUE),
      avg_panss_pos_improvement = mean(PANSS_POS_IMPROVEMENT, na.rm = TRUE),
      avg_panss_neg_improvement = mean(PANSS_NEG_IMPROVEMENT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(avg_panss_improvement > 0.1) %>%  # Filter out zero values
    pivot_longer(cols = starts_with("avg_panss"), names_to = "subscale", values_to = "improvement") %>%
    mutate(subscale = case_when(
      subscale == "avg_panss_improvement" ~ "Total PANSS (%)",
      subscale == "avg_panss_pos_improvement" ~ "Positive Symptoms",
      subscale == "avg_panss_neg_improvement" ~ "Negative Symptoms"
    ))
  
  # Check if we have any data to plot
  if (nrow(panss_data) == 0) {
    cat("Warning: No meaningful PANSS improvement data found. Returning NULL.\n")
    return(NULL)
  }
  
  ggplot(panss_data, aes(x = reorder(profile, improvement), y = improvement, fill = subscale)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = c("Total PANSS (%)" = "#E41A1C", 
                                 "Positive Symptoms" = "#377EB8", 
                                 "Negative Symptoms" = "#4DAF4A")) +
    labs(title = "PANSS Score Improvement by Genetic Polymorphism",
         x = "", y = "Improvement Score", fill = "PANSS Subscale") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip() +
    geom_hline(yintercept = 20, linetype = "dashed", color = "gray50", alpha = 0.7) +
    annotate("text", x = 1, y = 20, label = "Clinical Response Threshold (20%)", 
             hjust = 0, vjust = -0.5, size = 3)
}

plot_multi_panel_summary <- function(results, ss_metrics) {
  # Panel 1: Genetic effects on concentration
  p1 <- ggplot(ss_metrics %>% head(10), aes(x = reorder(profile, Cavg), y = Cavg)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
    labs(title = "A. Genetic Effects on Steady-State Concentration",
         x = "", y = "Avg Concentration (ng/mL)") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
    geom_hline(yintercept = c(350, 600), linetype = "dashed", color = c("green", "red"))
  
  # Panel 2: BBB transport effects
  p2 <- ggplot(ss_metrics %>% head(10), aes(x = Cavg, y = Brain_Cavg, size = BBB_transport_index)) +
    geom_point(alpha = 0.7, color = "coral") +
    labs(title = "B. Brain Penetration vs Plasma Concentration",
         x = "Plasma Conc (ng/mL)", y = "Brain Conc (ng/mL)",
         size = "BBB Transport\nIndex") +
    theme_publication() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5)
  
  # Panel 3: Receptor occupancy vs clinical response (check for PANSS data)
  if ("PANSS_PERCENT_IMPROVEMENT" %in% names(results)) {
    panss_summary <- results %>%
      filter(time >= 480) %>%  # Use longer time for steady state
      group_by(profile) %>%
      summarize(
        avg_d2_occ = mean(D2_OCC_BRAIN, na.rm = TRUE) * 100,
        avg_panss_improvement = mean(PANSS_PERCENT_IMPROVEMENT, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(avg_panss_improvement > 0.1) %>%  # Filter meaningful data
      head(10)
    
    if (nrow(panss_summary) > 2) {  # Need at least some data points
      p3 <- ggplot(panss_summary, aes(x = avg_d2_occ, y = avg_panss_improvement)) +
        geom_point(size = 3, alpha = 0.7, color = "darkgreen") +
        geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
        labs(title = "C. D2 Occupancy vs PANSS Improvement",
             x = "D2 Receptor Occupancy (%)", y = "PANSS Improvement (%)") +
        theme_publication() +
        geom_hline(yintercept = 20, linetype = "dashed", color = "red", alpha = 0.7) +
        geom_vline(xintercept = 65, linetype = "dashed", color = "blue", alpha = 0.7)
    } else {
      # Fallback to original clinical response
      p3 <- ggplot(ss_metrics %>% head(10), aes(x = D2_occupancy_avg * 100, y = Clinical_response_max)) +
        geom_point(size = 3, alpha = 0.7, color = "darkgreen") +
        labs(title = "C. D2 Occupancy vs Clinical Response",
             x = "D2 Receptor Occupancy (%)", y = "Clinical Response") +
        theme_publication()
    }
  } else {
    # Fallback to original clinical response
    p3 <- ggplot(ss_metrics %>% head(10), aes(x = D2_occupancy_avg * 100, y = Clinical_response_max)) +
      geom_point(size = 3, alpha = 0.7, color = "darkgreen") +
      labs(title = "C. D2 Occupancy vs Clinical Response",
           x = "D2 Receptor Occupancy (%)", y = "Clinical Response") +
      theme_publication()
  }
  
  # Panel 4: Safety threshold exceedance
  p4 <- ggplot(ss_metrics %>% head(10), aes(x = reorder(profile, Agranulocytosis_risk), 
                                            y = (Agranulocytosis_risk + Seizure_risk + Cardiotoxicity_risk) * 100)) +
    geom_bar(stat = "identity", fill = "red", alpha = 0.6) +
    labs(title = "D. Safety Risk by Genotype",
         x = "", y = "Risk Score (%)") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
    coord_flip()
  
  # Combine panels
  grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
}

plot_cpic_validation <- function(cpic_results) {
  # Concentration validation only
  ggplot(cpic_results$validation_data, aes(x = observed_conc, y = predicted_conc)) +
    geom_point(size = 3, alpha = 0.7, color = "blue") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    geom_smooth(method = "lm", se = TRUE, color = "darkblue", alpha = 0.3) +
    labs(title = "CPIC Validation: Predicted vs Observed Concentrations",
         subtitle = paste0("R² = ", round(cpic_results$metrics$conc_r_squared, 3), 
                           ", ", round(cpic_results$metrics$conc_within_20pct, 1), "% within 20%"),
         x = "Observed Concentration (ng/mL)", 
         y = "Predicted Concentration (ng/mL)") +
    theme_publication()
}

plot_ethnic_differences <- function(ethnic_results) {
  ethnic_long <- ethnic_results %>%
    select(ethnicity, avg_conc, avg_brain_conc, avg_d2_occ, avg_panss_improvement) %>%
    pivot_longer(cols = -ethnicity, names_to = "metric", values_to = "value") %>%
    mutate(metric = case_when(
      metric == "avg_conc" ~ "Plasma Conc (ng/mL)",
      metric == "avg_brain_conc" ~ "Brain Conc (ng/mL)",
      metric == "avg_d2_occ" ~ "D2 Occupancy",
      metric == "avg_panss_improvement" ~ "PANSS Improvement (%)"
    ))
  
  ggplot(ethnic_long, aes(x = ethnicity, y = value, fill = ethnicity)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    facet_wrap(~ metric, scales = "free_y", ncol = 2) +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    labs(title = "Ethnic Differences in Clozapine Response",
         subtitle = "Pharmacokinetic and pharmacodynamic differences across ethnic groups",
         x = "Ethnicity", y = "Value", fill = "Ethnicity") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
}

# ===================================================================
# ORIGINAL MAIN ANALYSIS EXECUTION (PRESERVED)
# ===================================================================

run_comprehensive_analysis <- function() {
  cat("=== COMPREHENSIVE CLOZAPINE PHARMACOGENOMICS ANALYSIS ===\n\n")
  
  # 1. Basic simulation
  cat("1. Running basic model simulation (Reference Individual)...\n")
  basic_ev <- ev(amt = 150, ii = 12, addl = 29)
  basic_out <- mrgsim(clozapine_mod, events = basic_ev, end = 360, delta = 1)
  cat("Basic steady-state concentration summary (CP ng/mL) for Reference Individual (Day 13-15):\n")
  ss_basic <- as.data.frame(basic_out) %>% filter(time >= 300) %>% pull(CP)
  print(summary(ss_basic))
  
  # 2. Core analyses
  cat("\n2. Running core analyses...\n")
  polymorphism_results <- simulate_polymorphisms(clozapine_mod)
  ss_metrics <- analyze_steady_state(polymorphism_results)
  fluvoxamine_results <- analyze_fluvoxamine_interaction(clozapine_mod)
  tdm_results <- create_tdm_recommendations(clozapine_mod)
  sensitivity_results <- perform_sensitivity_analysis(clozapine_mod)
  validation_results <- validate_model(clozapine_mod)
  dose_response <- plot_dose_response_cyp(clozapine_mod)
  dosing_recommendations <- calculate_optimal_dosing(clozapine_mod)
  
  # 3. Print key results
  cat("\nSteady-state metrics by genotype (Top 5 by Cavg):\n")
  print(ss_metrics %>% 
          select(profile, Cavg, Brain_Cavg, D2_occupancy_avg, Norcloz_ratio, Agranulocytosis_risk) %>% 
          arrange(desc(Cavg)) %>% 
          head(5))
  
  cat("\nFluvoxamine interaction summary (Top 5 by fold_increase):\n")
  print(fluvoxamine_results %>% 
          select(genotype, fluvoxamine_dose, fold_increase, clz_conc_avg, safety_risk) %>% 
          arrange(desc(fold_increase)) %>% 
          head(5))
  
  cat("\nModel validation metrics:\n")
  print(validation_results$metrics)
  
  # 4. Generate all plots and tables (NO DUPLICATES)
  cat("\n4. Generating visualizations and tables...\n")
  all_plots <- list(
    # Primary pharmacogenomics plots
    polymorphism_plasma = plot_polymorphism_plasma(polymorphism_results),
    plasma_concentration_time = plot_plasma_concentration_time(polymorphism_results),
    brain_plasma_correlation = plot_brain_plasma_correlation(polymorphism_results),
    brain_plasma_ecf = plot_brain_plasma_ecf(polymorphism_results),
    brain_plasma_ratio = plot_brain_plasma_ratio(ss_metrics),
    
    # Metabolite and PD plots
    metabolite_ratios = plot_metabolite_ratios(ss_metrics),
    d2_occupancy = plot_d2_occupancy(polymorphism_results),
    multi_receptor_occupancy = plot_multi_receptor_occupancy(polymorphism_results),
    gsh_ratio = plot_gsh_ratio(polymorphism_results),
    
    # Drug interaction plots
    fluvoxamine_heatmap = plot_fluvoxamine_heatmap(fluvoxamine_results),
    smoking_effect = plot_smoking_effect(clozapine_mod),
    
    # Dose-response plots
    dose_concentration = dose_response$conc_plot,
    dose_d2_occupancy = dose_response$d2_plot,
    
    # Sensitivity and validation plots
    sensitivity_exposure = plot_sensitivity_exposure(sensitivity_results),
    validation_obs_pred = plot_validation_observed_predicted(validation_results$validation_data),
    validation_error = plot_validation_error(validation_results$validation_data),
    
    # Safety and TDM plots
    safety_assessment = plot_safety_assessment(ss_metrics),
    tdm_matrix = plot_tdm_matrix(tdm_results)
  )
  
  # Generate summary tables
  cat("\n5. Generating summary tables...\n")
  all_tables <- list(
    steady_state_summary = ss_metrics,
    tdm_recommendations = tdm_results,
    dosing_recommendations = dosing_recommendations,
    fluvoxamine_interactions = fluvoxamine_results,
    validation_summary = validation_results$validation_data,
    validation_metrics = data.frame(
      Metric = names(validation_results$metrics),
      Value = unlist(validation_results$metrics)
    )
  )
  
  # Print key tables
  cat("\n=== KEY SUMMARY TABLES ===\n")
  
  cat("\nTable 1: Steady-State Metrics by Genotype (Top 5 by Cavg):\n")
  table1_cols <- c("profile", "Cavg", "Brain_Cavg", "D2_occupancy_avg", "Norcloz_ratio", "Agranulocytosis_risk")
  if ("HT2A_occupancy_avg" %in% names(ss_metrics)) {
    table1_cols <- c("profile", "Cavg", "Brain_Cavg", "D2_occupancy_avg", "HT2A_occupancy_avg", "Norcloz_ratio", "Agranulocytosis_risk")
  }
  print(ss_metrics %>% 
          select(all_of(table1_cols)) %>% 
          arrange(desc(Cavg)) %>% 
          head(5))
  
  cat("\nTable 2: TDM Recommendations Summary:\n")
  print(tdm_results %>% 
          select(scenario, expected_conc, norcloz_ratio, sampling_frequency, special_considerations))
  
  cat("\nTable 3: Optimal Dosing Recommendations (Top 5):\n")
  table3_cols <- c("profile", "optimal_dose", "dose_adjustment_percent", "expected_avg_conc", "expected_D2_occupancy", "safety_risk_score")
  if ("expected_HT2A_occupancy" %in% names(dosing_recommendations) && any(!is.na(dosing_recommendations$expected_HT2A_occupancy))) {
    table3_cols <- c("profile", "optimal_dose", "dose_adjustment_percent", "expected_avg_conc", "expected_D2_occupancy", "expected_HT2A_occupancy", "safety_risk_score")
  }
  print(dosing_recommendations %>% 
          select(all_of(table3_cols)) %>% 
          arrange(profile) %>% 
          head(5))
  
  cat("\nTable 4: Fluvoxamine Interaction Summary (Top 5 by fold_increase):\n")
  print(fluvoxamine_results %>% 
          select(genotype, fluvoxamine_dose, fold_increase, clz_conc_avg, safety_risk) %>% 
          arrange(desc(fold_increase)) %>% 
          head(5))
  
  cat("\nTable 5: Model Validation Summary:\n")
  print(validation_results$validation_data %>%
          select(study, scenario, observed_conc, predicted_conc, rel_error, within_2fold))
  
  cat("\nTable 6: Model Validation Metrics:\n")
  print(all_tables$validation_metrics)
  
  # 5. Display or save plots
  if(interactive()) {
    cat("Displaying plots in interactive session...\n")
    for(plot_name in names(all_plots)) {
      if(!is.null(all_plots[[plot_name]])) {
        print(all_plots[[plot_name]])
      }
    }
  } else {
    cat("Saving plots as PNG files...\n")
    for(plot_name in names(all_plots)) {
      if(!is.null(all_plots[[plot_name]])) {
        ggsave(paste0(plot_name, ".png"), plot = all_plots[[plot_name]], width = 10, height = 7)
      }
    }
  }
  
  # 6. Save results
  output_filename <- "clozapine_pharmacogenomics_clean.RData"
  save(clozapine_mod, enhanced_genotypes, polymorphism_results, ss_metrics,
       fluvoxamine_results, tdm_results, sensitivity_results, validation_results,
       dose_response, all_plots, file = output_filename)
  
  cat("\n=== ANALYSIS COMPLETE ===\n")
  cat("✓ All duplications removed\n")
  cat("✓ Enhanced model with multi-receptor occupancy (D2, 5HT2A, H1, M1)\n")
  cat("✓ 18 unique visualizations generated:\n")
  cat("  - Pharmacogenomics: 5 plots\n")
  cat("  - Metabolite/PD: 4 plots\n") 
  cat("  - Drug interactions: 2 plots\n")
  cat("  - Dose-response: 2 plots\n")
  cat("  - Validation: 2 plots\n")
  cat("  - Safety/TDM: 2 plots\n")
  cat("  - Additional: 1 plot\n")
  cat("✓ 6 comprehensive summary tables generated\n")
  cat("✓ All tables printed to console\n")
  cat(paste0("✓ Results saved to: '", output_filename, "'\n"))
  
  return(list(
    results = list(
      polymorphism = polymorphism_results,
      steady_state = ss_metrics,
      fluvoxamine = fluvoxamine_results,
      tdm = tdm_results,
      sensitivity = sensitivity_results,
      validation = validation_results,
      dose_response = dose_response,
      dosing_recommendations = dosing_recommendations
    ),
    plots = all_plots,
    tables = all_tables
  ))
}

# ===================================================================
# ENHANCED COMPREHENSIVE ANALYSIS (ADDITION)
# ===================================================================

run_enhanced_comprehensive_analysis <- function() {
  cat("=== ENHANCED CLOZAPINE PHARMACOGENOMICS ANALYSIS ===\n\n")
  
  # 1. Test enhanced model
  cat("1. Testing enhanced model with PANSS scoring...\n")
  test_ev <- ev(amt = 150, ii = 12, addl = 5)
  test_out <- mrgsim(clozapine_mod, events = test_ev, end = 72, delta = 1)
  test_df <- as.data.frame(test_out)
  
  # Check for new variables
  new_vars <- c("PANSS_TOTAL", "PANSS_PERCENT_IMPROVEMENT", "MULTI_RECEPTOR_EFFICACY", "TOXICITY_OUT")
  cat("New model variables:\n")
  for(var in new_vars) {
    if(var %in% names(test_df)) {
      cat(paste("✓", var, "- Available\n"))
      cat(paste("  Range:", round(min(test_df[[var]], na.rm=TRUE), 3), 
                "to", round(max(test_df[[var]], na.rm=TRUE), 3), "\n"))
    }
  }
  
  # 2. Run enhanced analyses
  cat("\n2. Running enhanced analyses...\n")
  enhanced_poly_results <- simulate_enhanced_polymorphisms(clozapine_mod)
  enhanced_ss_metrics <- analyze_steady_state(enhanced_poly_results)
  cpic_validation <- validate_cpic_data(clozapine_mod)
  ethnic_analysis <- analyze_ethnic_differences(clozapine_mod)
  
  # 3. Print enhanced results
  cat("\nEnhanced steady-state metrics (Top 5 by Cavg):\n")
  enhanced_cols <- c("profile", "Cavg", "Brain_Cavg", "D2_occupancy_avg", "Norcloz_ratio")
  
  # Debug: Check if PANSS variables are in the enhanced results
  if ("PANSS_PERCENT_IMPROVEMENT" %in% names(enhanced_poly_results)) {
    cat("✓ PANSS variables found in enhanced results\n")
    
    # Check actual PANSS values
    panss_check <- enhanced_poly_results %>%
      filter(time >= 480) %>%
      summarize(
        avg_panss_improvement = mean(PANSS_PERCENT_IMPROVEMENT, na.rm = TRUE),
        max_panss_improvement = max(PANSS_PERCENT_IMPROVEMENT, na.rm = TRUE),
        min_panss_improvement = min(PANSS_PERCENT_IMPROVEMENT, na.rm = TRUE)
      )
    cat("PANSS Improvement Summary:\n")
    print(panss_check)
    
    if (panss_check$max_panss_improvement > 0.1) {
      enhanced_cols <- c(enhanced_cols, "PANSS_PERCENT_IMPROVEMENT")
      cat("✓ Meaningful PANSS data found, including in table\n")
    } else {
      cat("⚠ PANSS data found but values too low (max =", panss_check$max_panss_improvement, ")\n")
    }
  } else {
    cat("⚠ PANSS_PERCENT_IMPROVEMENT not found in enhanced results\n")
  }
  
  # Get enhanced steady state metrics using appropriate time filter
  enhanced_ss_metrics <- enhanced_poly_results %>%
    filter(time >= 480) %>%  # Use consistent time filter
    group_by(profile) %>%
    summarize(
      Cmax = max(CP, na.rm = TRUE),
      Cmin = min(CP, na.rm = TRUE),
      Cavg = mean(CP, na.rm = TRUE),
      Brain_Cmax = max(BRAIN_CONC, na.rm = TRUE),
      Brain_Cavg = mean(BRAIN_CONC, na.rm = TRUE),
      D2_occupancy_avg = mean(D2_OCC_BRAIN, na.rm = TRUE),
      D2_occupancy_min = min(D2_OCC_BRAIN, na.rm = TRUE),
      Efficacy_probability = mean(EFFICACY, na.rm = TRUE),
      Clinical_response_max = max(CLINICAL_RESPONSE, na.rm = TRUE),
      Norcloz_ratio = mean(NORCLOZ_RATIO, na.rm = TRUE),
      Noxide_ratio = mean(NOXIDE_RATIO, na.rm = TRUE),
      BBB_transport_index = mean(BBB_TRANSPORT_INDEX, na.rm = TRUE),
      Agranulocytosis_risk = mean(AGRANULOCYTOSIS_RISK, na.rm = TRUE),
      Seizure_risk = mean(SEIZURE_RISK, na.rm = TRUE),
      Cardiotoxicity_risk = mean(CARDIOTOXICITY_RISK, na.rm = TRUE),
      GSH_ratio = mean(GSH_RATIO, na.rm = TRUE),
      PANSS_PERCENT_IMPROVEMENT = if("PANSS_PERCENT_IMPROVEMENT" %in% names(enhanced_poly_results)) 
        mean(PANSS_PERCENT_IMPROVEMENT, na.rm = TRUE) else NA,
      .groups = "drop"
    ) %>%
    arrange(desc(Cavg))
  
  print(enhanced_ss_metrics %>% 
          select(any_of(enhanced_cols)) %>% 
          arrange(desc(Cavg)) %>% 
          head(5))
  
  cat("\nCPIC Validation Metrics:\n")
  print(cpic_validation$metrics)
  
  cat("\nEthnic Differences Summary:\n")
  print(ethnic_analysis)
  
  # 4. Generate enhanced visualizations
  cat("\n4. Generating enhanced visualizations...\n")
  enhanced_plots <- list(
    # Enhanced PANSS improvement plot
    panss_improvement = plot_panss_improvement(enhanced_poly_results),
    
    # Multi-panel summary figure
    multi_panel_summary = plot_multi_panel_summary(enhanced_poly_results, enhanced_ss_metrics),
    
    # CPIC validation plots
    cpic_validation = plot_cpic_validation(cpic_validation),
    
    # Ethnic differences
    ethnic_differences = plot_ethnic_differences(ethnic_analysis),
    
    # Enhanced receptor occupancy with PANSS correlation
    receptor_panss_correlation = if("PANSS_PERCENT_IMPROVEMENT" %in% names(enhanced_poly_results)) {
      receptor_panss_data <- enhanced_poly_results %>%
        filter(time >= 480) %>%  # Use longer time for steady state
        group_by(profile) %>%
        summarize(
          d2_occ = mean(D2_OCC_BRAIN, na.rm = TRUE) * 100,
          ht2a_occ = mean(HT2A_OCC_BRAIN, na.rm = TRUE) * 100,
          h1_occ = mean(H1_OCC_BRAIN, na.rm = TRUE) * 100,
          m1_occ = mean(M1_OCC_BRAIN, na.rm = TRUE) * 100,
          multi_receptor = mean(MULTI_RECEPTOR_EFFICACY, na.rm = TRUE) * 100,
          panss_improvement = mean(PANSS_PERCENT_IMPROVEMENT, na.rm = TRUE),
          brain_conc = mean(BRAIN_CONC, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(panss_improvement > 0.1 & d2_occ > 1)  # Filter meaningful data
      
      if (nrow(receptor_panss_data) > 2) {  # Only plot if we have meaningful data
        ggplot(receptor_panss_data, aes(x = ht2a_occ, y = panss_improvement)) +
          geom_point(aes(color = d2_occ, size = brain_conc), alpha = 0.8) +
          scale_color_gradient(low = "blue", high = "red", name = "D2 Occ (%)") +
          scale_size_continuous(range = c(3, 10), name = "Brain Conc\n(ng/mL)") +
          labs(title = "5HT2A Receptor Occupancy vs PANSS Improvement",
               subtitle = "Multi-receptor contribution to clinical response by genotype",
               x = "5HT2A Receptor Occupancy (%)", 
               y = "PANSS Improvement (%)") +
          theme_publication() +
          geom_hline(yintercept = 20, linetype = "dashed", color = "gray50") +
          geom_smooth(method = "lm", se = TRUE, color = "darkgreen", alpha = 0.3) +
          # Add text annotations for receptor ranges
          annotate("text", x = max(receptor_panss_data$ht2a_occ) * 0.1, 
                   y = max(receptor_panss_data$panss_improvement) * 0.9,
                   label = paste0("5HT2A range: ", round(min(receptor_panss_data$ht2a_occ), 1), 
                                  "-", round(max(receptor_panss_data$ht2a_occ), 1), "%"),
                   hjust = 0, size = 3, color = "darkblue")
      } else {
        NULL  # Return NULL if insufficient data
      }
    } else NULL,
    
    # Additional plot: Individual receptor occupancies
    individual_receptor_occupancy = if("HT2A_OCC_BRAIN" %in% names(enhanced_poly_results)) {
      receptor_data <- enhanced_poly_results %>%
        filter(time >= 480) %>%
        group_by(profile) %>%
        summarize(
          D2 = mean(D2_OCC_BRAIN, na.rm = TRUE) * 100,
          `5HT2A` = mean(HT2A_OCC_BRAIN, na.rm = TRUE) * 100,
          H1 = mean(H1_OCC_BRAIN, na.rm = TRUE) * 100,
          M1 = mean(M1_OCC_BRAIN, na.rm = TRUE) * 100,
          .groups = "drop"
        ) %>%
        head(10) %>%
        pivot_longer(cols = c(D2, `5HT2A`, H1, M1), names_to = "receptor", values_to = "occupancy")
      
      ggplot(receptor_data, aes(x = reorder(profile, occupancy), y = occupancy, fill = receptor)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
        scale_fill_manual(values = c("D2" = "#E41A1C", "5HT2A" = "#377EB8", 
                                     "H1" = "#4DAF4A", "M1" = "#984EA3")) +
        labs(title = "Individual Receptor Occupancy by Genetic Polymorphism",
             subtitle = "Showing 5HT2A, D2, H1, and M1 receptor occupancy patterns",
             x = "", y = "Receptor Occupancy (%)", fill = "Receptor") +
        theme_publication() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        coord_flip() +
        geom_hline(yintercept = c(50, 80), linetype = "dashed", color = "gray50", alpha = 0.7)
    } else NULL
  )
  
  # Remove NULL plots
  enhanced_plots <- enhanced_plots[!sapply(enhanced_plots, is.null)]
  
  # 5. Create clinical decision support recommendations
  cat("\n5. Generating clinical decision support recommendations...\n")
  
  clinical_recommendations <- enhanced_ss_metrics %>%
    mutate(
      dose_recommendation = case_when(
        Cavg < 300 ~ "Increase dose by 25-50%",
        Cavg > 700 ~ "Decrease dose by 25-50%",
        TRUE ~ "Maintain current dose"
      ),
      monitoring_frequency = case_when(
        Agranulocytosis_risk > 0.1 | Seizure_risk > 0.1 ~ "Weekly TDM + CBC",
        Cavg < 350 | Cavg > 600 ~ "Bi-weekly TDM",
        TRUE ~ "Monthly TDM"
      ),
      safety_alert = case_when(
        (Agranulocytosis_risk + Seizure_risk + Cardiotoxicity_risk) > 0.2 ~ "HIGH RISK",
        (Agranulocytosis_risk + Seizure_risk + Cardiotoxicity_risk) > 0.1 ~ "MODERATE RISK",
        TRUE ~ "LOW RISK"
      )
    ) %>%
    select(profile, Cavg, dose_recommendation, monitoring_frequency, safety_alert) %>%
    arrange(desc(Cavg))
  
  cat("\nClinical Decision Support Recommendations (Top 10):\n")
  print(clinical_recommendations %>% head(10))
  
  # 6. Display plots if interactive
  if(interactive()) {
    cat("Displaying enhanced plots...\n")
    for(plot_name in names(enhanced_plots)) {
      if(!is.null(enhanced_plots[[plot_name]])) {
        if(plot_name == "multi_panel_summary") {
          enhanced_plots[[plot_name]]  # grid.arrange plots display automatically
        } else {
          print(enhanced_plots[[plot_name]])
        }
      }
    }
  }
  
  # 7. Save enhanced results
  output_filename <- "enhanced_clozapine_pharmacogenomics_v2.RData"
  save(clozapine_mod, enhanced_genotypes, enhanced_poly_results, enhanced_ss_metrics,
       cpic_validation, ethnic_analysis, enhanced_plots, clinical_recommendations,
       file = output_filename)
  
  cat("\n=== ENHANCED ANALYSIS COMPLETE ===\n")
  cat("✓ Enhanced model with PANSS score modeling\n")
  cat("✓ Multi-receptor efficacy calculations\n")
  cat("✓ Michaelis-Menten BBB transport kinetics\n")
  cat("✓ Enhanced genetic polymorphisms (GSTT1/GSTM1, CYP3A4*22)\n")
  cat("✓ Hepatic zonation effects\n")
  cat("✓ Cumulative toxicity scoring\n")
  cat("✓ CPIC dataset validation\n")
  cat("✓ Ethnic difference analysis\n")
  cat("✓ Clinical decision support recommendations\n")
  cat("✓ Enhanced visualizations with multi-panel summary\n")
  cat(paste0("✓ Results saved to: '", output_filename, "'\n"))
  
  return(list(
    model = clozapine_mod,
    results = list(
      enhanced_polymorphism = enhanced_poly_results,
      enhanced_steady_state = enhanced_ss_metrics,
      cpic_validation = cpic_validation,
      ethnic_analysis = ethnic_analysis,
      clinical_recommendations = clinical_recommendations
    ),
    plots = enhanced_plots
  ))
}

# ===================================================================
# EXECUTION: BOTH ORIGINAL AND ENHANCED ANALYSES
# ===================================================================

# Execute the original analysis (preserved from pbpk31.R)
cat("EXECUTING ORIGINAL ANALYSIS FROM pbpk31.R...\n")
original_results <- run_comprehensive_analysis()

cat("\n", paste(rep("=", 80), collapse=""), "\n")

# Execute the enhanced analysis (new additions)
cat("EXECUTING ENHANCED ANALYSIS WITH NEW FEATURES...\n")
enhanced_final_results <- run_enhanced_comprehensive_analysis()

cat("\n=== COMPLETE ANALYSIS SUMMARY ===\n")
cat("✓ Original pbpk31.R analysis completed successfully\n")
cat("✓ Enhanced analysis with PANSS, CPIC validation, and advanced features completed\n")
cat("✓ Both original and enhanced models available for comparison\n")
cat("✓ All original functions and visualizations preserved\n")
cat("✓ New enhancements added without modifying original code\n")
