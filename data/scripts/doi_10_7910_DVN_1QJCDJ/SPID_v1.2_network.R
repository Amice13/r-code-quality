#### "Generating Latent Networks"
#### Code to Infer Latent Networks from SPID v1.2
### Sample code for setting network parameters for inferring network
### "June 21, 2021"

### Load Required Packages

if (!require("NetworkInference")) install.packages("Network Inference")
if (!require("reader")) install.packages("reader")
if (!require("readr")) install.packages("readr")
library(NetworkInference) 
library(reader)
library(readr)
library(haven)


### Set Parameters for Window, Edges, Decay Parameter, and Years

WINDOW_SIZE = 100
N_EDGES = 800
LAMBDA = 7.75
BASE_YEARS = 1960:2017


### Create Function for inferring network

infer_network <- function(time, time_window, cascades, params, n_edges) {
  
  casc = subset_cascade_time(cascades, (time - time_window), time)
  
  network = netinf(cascades = casc, trans_mod = "exponential",
                   n_edges = n_edges, params = params, quiet = TRUE)
  
  network = network[, -c(3, 4)]
  network$time = time
  cat('Processed', time, '.\n')
  return(network)
}


### Load policy adoption data from current SPID release and transform to cascade
policies <- read_dta("SPID_v1.2.dta")

policy_cascades = as_cascade_long(policies, cascade_node_name = 'statenam',
                                  cascade_id = 'policy', 
                                  event_time = 'adopt_year')


### Infer the networks for each base year
networks = lapply(BASE_YEARS, infer_network, time_window = WINDOW_SIZE,
                  cascades = policy_cascades, params = LAMBDA, 
                  n_edges = N_EDGES)


### Cast into single dataframe (origin_node, destination_node, time)
networks = do.call(rbind, networks)

#Rename Variables 

colnames(networks) <- c("origin_statenam","destination_statenam", "year")

# Save to file

write_csv(networks, 'SPID_v1.2_network.csv')

### End Session 
rm(list = ls())
