amce_replace_dim_names <- function(amce_obj, dimension_names, attribute_names) {
  amce_attr <- amce_obj$attributes
  amce_esti <- amce_obj$estimates
  if(all(str_sub(names(amce_attr), 4) %in% gsub(" ", "", dimension_names))) {
    for(i in 1:length(amce_attr)) {
      for(k in 1:length(colnames(amce_esti[[i]]))) {
        if(str_sub(colnames(amce_esti[[i]]), nchar(names(amce_esti)[i])+1)[k]%in%gsub(" ", "", attribute_names)) {
          colnames(amce_esti[[i]])[k] <- attribute_names[which(str_sub(colnames(amce_esti[[i]]), nchar(names(amce_esti)[i])+1)[k]==gsub(" ", "", attribute_names))]
        } else {
          colnames(amce_esti[[i]])[k] <- str_sub(colnames(amce_esti[[i]]), nchar(names(amce_esti)[i])+1)[k]
        }
      }
      names(amce_esti)[i] <- dimension_names[which(str_sub(names(amce_esti), 4)[i]==gsub(" ", "", dimension_names))]
      names(amce_attr)[i] <- dimension_names[which(str_sub(names(amce_attr), 4)[i]==gsub(" ", "", dimension_names))]
      for(j in 1:length(amce_attr[[i]])) {
        if(any(amce_attr[[i]][j]==gsub(" ", "", attribute_names))) {
          amce_attr[[i]][j] <- attribute_names[which(amce_attr[[i]][j]==gsub(" ", "", attribute_names))]
        }
      } 
    }
    amce_obj$attributes <- amce_attr
    amce_obj$estimates <- amce_esti
  }
  return(amce_obj)
}
