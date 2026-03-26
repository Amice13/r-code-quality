



# Graph
graph <- make_graph(raster.path = popr.path,
                    factor = FACTOR,
                    extent = raster::extent(rshp),
                    noisy = T)

# Crop
graph <- crop_graph(graph, 
                    gUnaryUnion(cshp, id = rep(1, length(cshp))))

# Set min population
vertex_attr(graph, "layer.1" )[vertex_attr(graph, "layer.1" ) %in% c(0, NA)] <- 1

# Create weight attribute where missing
E(graph)$length <- E(graph)$length/1000
E(graph)$weight = E(graph)$length * 1/6 ## 6km/h
E(graph)$name = c(1:length(E(graph)))
E(graph)$rail <- FALSE
E(graph)$railyear <- NA
E(graph)$railseq <- NA

# Save base graph
saveRDS(graph, file.path(out.path, "graph_base.rds"))

# Deflate budget
rshp.length$length <- rshp.length$length / BUDGET_DEFLATE 

# Export network builder
clusterExport(cl, c("roadbuilder"))

# For each year, build roads
for(year in YRS){
  print(Sys.time())
  print(year)
  
  # Export graph
  clusterExport_fast(cl, c("graph"))
  
  # These cshp shapes
  t.cshp <- which(cshp$to >= year &
                    cshp$from <= year)
  
  # Budget
  budget <- rshp.length[rshp.length$cshp.id %in% t.cshp &
                          rshp.length$year == year,]
  budget <- budget[order(budget$cshp.id), "length"]
  
  # Drop 0s
  t.cshp <- t.cshp[budget > 0]
  budget <- budget[budget > 0]
  
  # Check
  stopifnot(length(t.cshp) == length(budget))
  
  # Export
  clusterExport(cl, c("t.cshp", "budget", "roadbuilder"))
  
  # Build roads in parallel
  built.eids <- foreach(c = seq_along(t.cshp),
                        .options.multicore = list(preschedule = FALSE)) %dopar% {
                          print(paste(Sys.time(), ": Start ", cshp$name_uni[t.cshp[c]]))
                          
                          # Crop graph
                          t.graph <- crop_graph(graph, cshp[t.cshp[c],], delete.islands = TRUE)
                          
                          
                          # Return NULL if no graph
                          if(is.null(t.graph)){
                            return(c())
                          }
                          if(length(E(t.graph)) == 0){
                            return(c())
                          }
                          
                          # Fixed edges
                          fix_edges <- which(E(t.graph)$rail)
                          if(length(E(t.graph)) == length(fix_edges)){
                            return(c())
                          }
                          
                          # Check budget
                          if(budget[c] > sum(E(t.graph)$length[!seq_along(E(t.graph)) %in% fix_edges])){
                            return(E(t.graph)$name[!seq_along(E(t.graph)) %in% fix_edges])
                          }
                          
                          # Seed edges
                          start_eid <- get_seed_edges(t.graph, 
                                                      vertex_attr(t.graph, "layer.1"), 
                                                      fix_edges = fix_edges, size = 10, 
                                                      seed = 250589)
                          
                          # Make network
                          node.roadbuilder <- roadbuilder$new(g = t.graph, 
                                                              length.var = "length", 
                                                              weight.var = "weight", 
                                                              pop.var =  "layer.1",
                                                              start_eid = start_eid, 
                                                              fixed.edges = fix_edges,
                                                              speedup_vec = SPEEDUP, 
                                                              budget_vec = budget[c])
                          
                          #sum(E(t.graph)$length[built]) + 
                          node.roadbuilder$top_N <- 10
                          node.roadbuilder$build_steps <- 10
                          node.roadbuilder$adjoining.only <- TRUE
                          
                          # Initialize round
                          node.roadbuilder$make_one_round(noisy = FALSE)
                          
                          # Message
                          print(paste(Sys.time(), ": End ", cshp$name_uni[t.cshp[c]]))
                          
                          # Return IDs
                          return(E(t.graph)$name[node.roadbuilder$built_eids])
                        }
  
  # Transfer to graph
  for(i in seq_along(built.eids)){
    E(graph)$weight[built.eids[[i]]] <- E(graph)$weight[built.eids[[i]]] / SPEEDUP
    E(graph)$rail[built.eids[[i]]] <- TRUE
    E(graph)$railyear[built.eids[[i]]] <- year
    E(graph)$railseq[built.eids[[i]]] <- seq_along(built.eids[[i]])
  }
  
  # Save
  saveRDS(graph, file.path(out.path, "graph_temp.rds"))
}

# Save final graph
saveRDS(graph, file.path(out.path, "graph_fin.rds"))

# TO SPATIAL ####

## Lines
linemat <- edges2lines(graph)[E(graph)$rail,]
e.df <- data.frame(lapply(edge_attr_names(graph), function(v){
  edge_attr(graph, v)
}))[E(graph)$rail,]
colnames(e.df) <- edge_attr_names(graph)

## Spatial Dataframe
lines.ls <- lapply(c(1:nrow(linemat)), function(x){Lines(Line(rbind(linemat[x, 1:2], linemat[x, 3:4])), ID = x)})
railsim.sp <- SpatialLines(lines.ls)
railsim.spdf <- SpatialLinesDataFrame(railsim.sp, 
                                      data = e.df,
                                      match.ID = F)

## Save
writeOGR(railsim.spdf, file.path(out.path, "lines_sim.json"),
         "lines_sim", driver="GeoJSON", overwrite_layer = TRUE)

