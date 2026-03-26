# Load estimating functions ------------------------------------------------

source(here("scripts/programs", "estimate_models.R"))

# Read processed data ------------------------------------------------------

data_specific_prompts_items <- readRDS(here("data/processed", "data_specific_prompts_items.rds"))
data_general_rumors_items <- readRDS(here("data/processed", "data_general_rumors_items.rds"))
data_polarization_attitudes_1_items <- readRDS(here("data/processed", "data_polarization_attitudes_1_items.rds"))
data_polarization_attitudes_2_items <- readRDS(here("data/processed", "data_polarization_attitudes_2_items.rds"))

# Belief in specific prompts

dep_vars <- c("Q17", "Q19", "Q21", "Q23", "Q25", "Q27")

dataFull <- data_specific_prompts_items
dataPetistas <- data_specific_prompts_items %>% filter(ideo_group == "Petistas")
dataAntipetistas <- data_specific_prompts_items %>% filter(ideo_group == "Antipetistas")
dataNonPartisans <- data_specific_prompts_items %>% filter(ideo_group == "Non-Partisans")

datas <- list(
  Full = dataFull,
  Petistas = dataPetistas,
  Antipetistas = dataAntipetistas,
  NonPartisans = dataNonPartisans
)

for (group_name in names(datas)) {
  data <- datas[[group_name]]
  models <- list()
  
  for (var in dep_vars) {
    formula <- reformulate("treatment", response = var)
    
    if(group_name == "Full"){
      model <- lm_robust(
        formula,
        se_type = 'HC2',
        weights = weight,
        data = data
      ) 
    }else{
      model <- lm_robust(
        formula,
        se_type = 'HC2',
        data = data
      )
    }
    
    models[[var]] <- model
  }
  
  plot <- plot_summs(models, 
             inner_ci_level = .9,
             coefs = c("Journalistic" = "treatmentJournalistic", "Crude" = "treatmentCrude"),
             main = paste("Models for", group_name)
  )
  
  ggsave(filename = here(paste0("output/figures/individual_items/belief_prompts/", group_name, ".png")), plot = plot, width = 8, height = 6)
}





# Belief in rumors
dep_vars <- c("Q28_1", "Q28_2", "Q28_3", "Q28_4", "Q28_5", "Q28_6")

dataFull <- data_general_rumors_items
dataPetistas <- data_general_rumors_items %>% filter(ideo_group == "Petistas")
dataAntipetistas <- data_general_rumors_items %>% filter(ideo_group == "Antipetistas")
dataNonPartisans <- data_general_rumors_items %>% filter(ideo_group == "Non-Partisans")

datas <- list(
  Full = dataFull,
  Petistas = dataPetistas,
  Antipetistas = dataAntipetistas,
  NonPartisans = dataNonPartisans
)
for (group_name in names(datas)) {
  data <- datas[[group_name]]
  models <- list()
  
  for (var in dep_vars) {
    formula <- reformulate("treatment", response = var)
    
    if(group_name == "Full"){
      model <- lm_robust(
        formula,
        se_type = 'HC2',
        weights = weight,
        data = data
      ) 
    }else{
      model <- lm_robust(
        formula,
        se_type = 'HC2',
        data = data
      )
    }
    
    models[[var]] <- model
  }
  
  plot <- plot_summs(models, 
                     inner_ci_level = .9,
                     coefs = c("Journalistic" = "treatmentJournalistic", "Crude" = "treatmentCrude"),
                     main = paste("Models for", group_name)
  )
  
  ggsave(filename = here(paste0("output/figures/individual_items/belief_rumors/", group_name, ".png")), plot = plot, width = 8, height = 6)
}
