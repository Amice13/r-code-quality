# This script builds a sample dataset based on the provide company id

buildSample = function(company_id, ind_data){
  ind_data$if_company = ind_data$company == company_id
  sample_data = data.frame(
    "X" = ind_data$loss,
    "T" = as.numeric(ind_data$threat),
    "V" = as.numeric(ind_data$vulnerability),
    "A" = as.numeric(ind_data$asset),
    "year" = ind_data$year,
    "rev" = ind_data$rev,
    "company" = ind_data$if_company
  )
  return(sample_data)
}
