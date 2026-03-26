export_reg_table <- function(models, file, title, coef_names = c("Journalistic", "Crude"), gof_names = c(NA, NA, NA, "N"), omitted = "Intercept", header, label,coef_map){
  return(
    texreg(
      file = here("output/tables", file),
      models,
      include.ci=FALSE,
      custom.model.names = c("All", "Petistas", "Antipetistas", "Non Partisans"),
      custom.coef.names = coef_names,
      caption = title,
      float.pos = "h!",
      booktabs = TRUE,
      custom.header = header,
      custom.gof.names = gof_names,
      use.packages=FALSE,
      label = label,
      omit.coef = omitted,
      custom.coef.map=coef_map
      
    )
  )
}

export_reg_table_items <- function(models, file, title, coef_names = c("Journalistic", "Crude"), gof_names = c(NA, NA, "NA", NA), header, label){
  return(
    texreg(
      file = here("output/tables", file),
      models,
      include.ci=FALSE,
      custom.model.names = c("Prompt 1", "Prompt 2", "Prompt 3", "Prompt 4", "Prompt 5", "Prompt 6"),
      custom.coef.names = coef_names,
      caption = title,
      float.pos = "h!",
      booktabs = TRUE,
      custom.header = header,
      custom.gof.names = gof_names,
      use.packages=FALSE,
      label = label
    )
  )
}



export_reg_table2 <- function(models, 
                              file, 
                              title, 
                              gof_names = c(NA, NA, NA, "N"), 
                              omitted = "Intercept", 
                              header, 
                              label) {
  return(
    texreg(
      file = here("output/tables", file),
      models,
      include.ci = FALSE,
      custom.model.names = c("Antipetistas","Petistas", "Non Partisans"),
      custom.coef.map = list(
        "treatmentJournalistic" = "Journalistic",
        "treatmentCrude"        = "Crude",
        "social_media_consumer" = "Social Media"
      ),
      caption = title,
      float.pos = "h!",
      booktabs = TRUE,
      custom.gof.names = gof_names,
      use.packages = FALSE,
      label = label,
      omit.coef = omitted,
      digits = 3
    )
  )
}


export_reg_table3 <- function(models, 
                              file, 
                              title, 
                              gof_names = c(NA, NA, NA, "N"), 
                              omitted = "Intercept", 
                              header, 
                              label) {
  return(
    texreg(
      file = here("output/tables", file),
      models,
      include.ci = FALSE,
      custom.model.names = c("Antipetistas","Petistas", "Combined"),
      custom.coef.map = list(
        "treatmentJournalistic" = "Journalistic",
        "treatmentCrude"        = "Crude"
      ),
      caption = title,
      float.pos = "h!",
      booktabs = TRUE,
      custom.gof.names = gof_names,
      use.packages = FALSE,
      label = label,
      omit.coef = omitted,
      digits = 3
    )
  )
}



export_reg_table4 <- function(models, 
                              file, 
                              title, 
                              gof_names = c(NA, NA, NA, "N"), 
                              omitted = "Intercept", 
                              header, 
                              label) {
  return(
    texreg(
      file = here("output/tables", file),
      models,
      include.ci = FALSE,
      custom.model.names = c("Antipetistas","Petistas"),
      custom.coef.map = list(
        "treatmentJournalistic" = "Journalistic",
        "treatmentCrude"        = "Crude"
      ),
      caption = title,
      float.pos = "h!",
      booktabs = TRUE,
      custom.gof.names = gof_names,
      use.packages = FALSE,
      label = label,
      omit.coef = omitted,
      digits = 3
    )
  )
}


get_baseline_results <- function(data, name, .caption){
  reg.believe.petistas<-lm_robust(dv~treatment, se_type = 'HC2', data = data%>%filter(ideo_group=="Petistas"))
  reg.believe.antipetistas<-lm_robust(dv~treatment, se_type = 'HC2', data= data%>%filter(ideo_group=="Antipetistas"))
  reg.believe.np<-lm_robust(dv~treatment, se_type = 'HC2', data= data%>%filter(ideo_group=="Non-partisans"))
  
  petistas <- tidy(reg.believe.petistas) %>% filter(term == "(Intercept)") %>% mutate(group = "Petistas")
  antipetistas <- tidy(reg.believe.antipetistas) %>% filter(term == "(Intercept)")  %>% mutate(group = "Antipetistas")
  np <- tidy(reg.believe.np) %>% filter(term == "(Intercept)")  %>% mutate(group = "Non-Partisans")
  
  test <- bind_rows(petistas, antipetistas, np) %>% 
    select(
      group,
      estimate,
      std.error,
      p.value,
    )
  
  table <- gt(test) %>% 
    cols_align(
      align = "center",
      columns = c(estimate, std.error, p.value)
    ) %>% 
    fmt_number(
      columns = c(estimate, std.error),
      decimals = 2
    ) %>% 
    fmt_number(
      columns = c(p.value),
      decimals = 3
    ) %>% 
    cols_label(
      group = "Ideological group",
      estimate = "Estimate",   
      std.error = "Standard Error",
      p.value = "p-value"
    )
  table <- table %>% 
    tab_header(title = .caption) %>% 
    tab_options(latex.use_longtable = FALSE,
                latex.tbl.pos = "h!")
  
  latex_code <- as_latex(table)
  latex_code
  
  latexx <- as.character(latex_code)
  
  tabla_latex <- gsub("\\\\caption\\*\\{", "\\\\caption{", latexx)
  cat(tabla_latex)
  
  writeLines(as.character(tabla_latex), here("output/tables/baseline_results", name))
}

get_data <- function(data, items){
  items = enquo(items)
  return(
    data %>% 
      dplyr::select(
        id,
        all_of(!!items),
        ideo_group,
        attention_level,
        ideo_group_leaning,
        np_pt,
        np_antipt,
        weight,
        treatment,
        excluded,
        partido_jugador2,
        gender,
        income,
        age,
        religion,
        intr_pol_dummy,
        social_media_consumer
      )%>% 
      pivot_longer(
        cols = starts_with("Q"),
        names_to = "prompt",
        values_to = "dv",
        values_drop_na = TRUE
      )
  )
}

set_treatment_level_reference <- function(data, reference){
  data <- data %>%
    mutate(
      treatment = relevel(treatment, ref = reference)
    )
  return(data)
}


estimate_models <- function(data, outcome, treatment, controls, filters) {
  
  models <- list()
  
  for (i in seq_along(filters)) {
    
    if (is.null(filters[[i]])) {
      
      filtered_data <- data
    } else {
      
      filtered_data <- data %>% filter(!!rlang::parse_expr(filters[[i]]))
    }
    
    if(is.null(controls)){
      formula_model <- reformulate(termlabels = treatment, response = outcome)
    }else{
      
      formula_model <- reformulate(termlabels = c(treatment, controls), response = outcome)
      
    }
    
    models[[paste0("Model_", i)]] <- lm_robust(formula = formula_model, data = filtered_data, se_type = 'HC2', weights = weight)
  }
  
  return(models)
}

