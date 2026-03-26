
### Script to generate command line arguments for "Simulations.R"

# Col 1 = nrep_t
# Col 2 = vA_w
# Col 3 = vE_w
# Col 4 = vA_t
# Col 5 = COV_tw

nsteps = 200 # Number of steps over which COV_tw is to be varied for every combinations of (vA_w&vE_w)*vA_t*nrep_t

# Read actual output for sex and sex ratio specific fitness variation to populate possible_vA_w and possible_vE_w
data_va = read.csv("Output/fitness_vA_output.csv", header=T)
# Remove additive genetic covariances
data_va = data_va[data_va$Sex=="Female Vw"|data_va$Sex=="Male Vw",]

# Possible values of various parameters

possible_nrep_t = c(3, 10, 17, 24, 31)
possible_vA_w = data_va[data_va$Sex.Ratio=="Female Biased"|data_va$Sex.Ratio=="Male Biased",]$Va
possible_vE_w = data_va[data_va$Sex.Ratio=="Female Biased"|data_va$Sex.Ratio=="Male Biased",]$Vr
possible_vA_t = c(0.03,0.13, 0.23, 0.33, 0.43)

# Since the possible_COV_tw depends on th values of vA_w and vA_t, use a function

calculate_possible_COV_tw = function(vA_w, vA_t, nsteps){
  return(seq(0, sqrt(vA_t*vA_w), length = nsteps))
}

# Empty matrix to be populated
param_matrix = c()

# Four loops (outside to inside): loop over possible_vA_t, loop over possible_vA_w and possible_vE_w simultaneously, loop over possible_nrep_t, and loop over possible_COV_tw

for(vA_t in possible_vA_t){
  
  for(vA_w in possible_vA_w){
    
    # Select the corresponding vE_w
    vE_w = possible_vE_w[which(possible_vA_w == vA_w)]
    
    # Calculate the possible_COV_tw for this combination of vA_w and vA_t
    possible_COV_tw = calculate_possible_COV_tw(vA_w = vA_w,
                                                vA_t = vA_t,
                                                nsteps = nsteps)
    
    for(nrep_t in possible_nrep_t){
      
      for(COV_tw in possible_COV_tw){
        param_matrix_current = c(nrep_t, vA_w, vE_w, vA_t, COV_tw)
        param_matrix = rbind(param_matrix, param_matrix_current)
      }
      
      
    }
  }
  
}

write.table(noquote(param_matrix), file = "000_parameter_grid_simulations.txt", sep = " ", col.names = FALSE, row.names = FALSE, quote = FALSE)

