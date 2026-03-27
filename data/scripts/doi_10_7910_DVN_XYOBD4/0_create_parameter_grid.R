
### Script to generate command line arguments for "randomise_traits.R"

nreps = 1000 # Number of replicates for each trait

d = read.csv("Comprehensive_traits.csv", header = T)
d$Trait = factor(d$Trait)
# Make a list of all the traits  
list_traits = levels(d$Trait)

# Remove the names of fitness data.
list_traits = list_traits[grep("Fitness", list_traits, invert = T)]

param_matrix_full = c() 

for (trait in list_traits){
  param_matrix = matrix(NA, nrow = nreps, ncol = 2)
  param_matrix[,1] = trait
  param_matrix[,2] = 1:nreps
  param_matrix_full = rbind(param_matrix_full, param_matrix)
  
}

write.table(noquote(param_matrix_full), file = "000_parameter_grid.txt", sep = " ", col.names = FALSE, row.names = FALSE, quote = FALSE)
