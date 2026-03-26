


########################################
# Fast cluster Export via disk
########################################
clusterExport_fast <- function(cl, varlist, envir = .GlobalEnv){
  # Save var.list to tempfile
  tmp.file <- tempfile()
  save(list = varlist, file = tmp.file, envir = envir)
  clusterExport(cl, list("tmp.file"), envir = environment())
  
  # Load data
  load <- parLapply(cl, seq_along(cl), function(i){
    load(tmp.file, envir = .GlobalEnv)
    TRUE
  })
  
  # Cleanup
  clusterEvalQ(cl = cl, expr = {
    rm(tmp.file)
    TRUE
  })
  unlink(tmp.file)
  
  # Return
  return(TRUE)
}

######################
# EMPTY NETWORK
######################

#' Prepares an empty network fo foot-pathes that covers this.shp
make_empty_nw <- function(this.shp, pop.raster.path, 
                          mosaic.n, frame.size, agg.factor = 1, prep.path,
                          random.seed = 250589){
  
  # set random.seed
  set.seed(random.seed)
  
  # ... population grids
  raster.years <- c(1880, 1960, 1970, 1980, 1990, 2000)
  if(agg.factor <= 1 | agg.factor > 2){
    raster <- raster::crop(stack(paste0(pop.raster.path, "popc_",raster.years, "AD.asc")),
                           extent(this.shp))
    if(agg.factor < 1){
      raster <- raster::disaggregate(raster,fact = round(1/agg.factor), na.rm = T )
      raster <- raster / (round(1/agg.factor)^2)
    } else if(agg.factor > 2){
      raster <- raster::aggregate(raster,fact = agg.factor, fun = sum, na.rm = T )
    }
  } else {
    raster <- raster::crop(stack(paste0(pop.raster.path, "popc_",raster.years, "AD_1arcmin.asc")),
                           extent(this.shp))
  }
  names(raster) <- paste0("pop", raster.years)
  raster <- stack(raster)
  
  # ... set population values outside shape to missing
  this.shp$inside <- 1
  shp.raster <- raster::rasterize(this.shp, raster, field = "inside")
  raster[is.na(shp.raster)] <- NA
  
  # Mosaic ID
  
  # ... make mosaics as spatial voronoi polygons
  # mosaic.grid <- spcosa::stratify(gBuffer(this.shp, width = res(raster)[1]), nStrata = mosaic.n, nTry = 5, 
  #                               cellSize = res(raster)[1])
  # mosaic.sp <- as(mosaic.grid@cells, "SpatialPolygons")
  # mosaic.sp <- gUnaryUnion(mosaic.sp, id = mosaic.grid@stratumId+1)
  # mosaic.sp <- SpatialPolygonsDataFrame(mosaic.sp,
  #                                       data = data.frame(mosaic.id = c(1:length(mosaic.sp))),
  #                                       match.ID = F)
  # 
  # 
  # # ... addt to rater
  # vtess.vx <- velox(raster[[1]])
  # vtess.vx$rasterize(mosaic.sp, 
  #                    field="mosaic.id", band=1, background = NA)  
  # vtess.rs <- vtess.vx$as.RasterStack()
  # names(vtess.rs) <- "mosaic.id"
  # raster <- stack(raster, vtess.rs)
  # rm(vtess.vx)
  
  # ... make (empty) network for entire region
  spl.nw <- grid_spnetwork(raster = raster, spldf.ls = list(), 
                           obstacles.ls = list(), 
                           na.band = "pop1960", 
                           noisy = T, ngb = 8)
  
  # ... make unique edge and vertex.id
  V(spl.nw)$vertex.id.full <- c(1:length(V(spl.nw)))
  E(spl.nw)$edge.id.full <- c(1:length(E(spl.nw)))
  
  # ... mosaic.ids
  print("Make Mosaics")
  v.coords <- cbind(V(spl.nw)$x, V(spl.nw)$y)[!is.na(V(spl.nw)$pop1960),]
  v.kmean.cl <- kmeans(v.coords, mosaic.n, iter.max = 1000, algorithm="Lloyd") 
  V(spl.nw)$mosaic.id[!is.na(V(spl.nw)$pop1960)] <- v.kmean.cl$cluster
  
  # ... mosaic polygons
  mosaic.sp <- gUnaryUnion(gBuffer(SpatialPoints(v.coords), width = res(raster)[1] * .5 + .00001, capStyle = "SQUARE", byid = T),
                           id =  v.kmean.cl$cluster)
  mosaic.sp <- SpatialPolygonsDataFrame(mosaic.sp, data = data.frame(mosaic.id = as.numeric(row.names(mosaic.sp))), match.ID = F)
  
  # ... add resolution
  spl.nw$resolution <- res(raster)
  
  # ... cut into frames
  mosaics <- na.omit(unique(mosaic.sp$mosaic.id))
  frame.sp <- gBuffer(mosaic.sp, byid = T, width = frame.size) 
  mos.nw.ls <- lapply(mosaics, function(m){
    return(crop_graph(spl.nw, frame.sp[m,]))
  })
  names(mos.nw.ls) <- paste0("mosaic.",mosaics)
  
  # ... save
  unlink(paste0(prep.path, "networks/"), recursive = T)
  dir.create(paste0(prep.path))
  dir.create(paste0(prep.path,"networks"))
  
  save(spl.nw, file = paste0(prep.path, "full.empty.Rdata"))
  save(mosaic.sp, file = paste0(prep.path, "mosaic.sp.Rdata"))
  
  print(paste0("Saving ",length(mosaics), " mosaics"))
  for(m in mosaics){
    g = mos.nw.ls[[paste0("mosaic.",m)]]
    save(g, file = paste0(prep.path, "networks/","mosaic.", m, ".Rdata"))
  }
}

###########################
# SIMULATOR CLASS
###########################

# Initialize class
roadbuilder.initialize <- function(g, length.var, weight.var, pop.var, speedup_vec, budget_vec,
                                   start_eid = NULL, fixed.edges = NULL,
                                   build_steps = 10, top_N = 10){
  # BoostGraph
  self$g <- g
  self$bg <- BoostGraph(self$g)
  
  # Population matrix
  p.vec <- vertex_attr(g, pop.var) 
  p.vec[is.na(p.vec)] <- 0
  self$pmat <- p.vec %o% p.vec
  
  # Edge weigth and length
  self$lengths <- edge_attr(g, length.var)
  self$weights <- edge_attr(g, weight.var)
  
  # Budget and Speedup
  self$budget_vec <- budget_vec
  self$speedup_vec <- speedup_vec
  
  # Others (not necessary)
  self$start_eid <- start_eid
  self$fixed.edges <- fixed.edges
  self$build_steps <- build_steps
  self$top_N <- top_N
  
}

# Get LOSS Function
roadbuilder.get_loss <- function(){
  mean((self$dmat * self$pmat)[lower.tri(self$dmat)])
}

# Initialize round
roadbuilder.initround <- function(round){
  # Get budget, etc for this round
  self$speedup <- self$speedup_vec[round] # how much to divide the old weights by when upgrading
  self$budget <- self$budget_vec[round] # how many roads of the new type are available
  self$round <- round
  self$step <- 0
  
  # Only adjoining roads or not?
  if(round == 1){
    if(is.null(self$adjoining.only)){
      self$adjoining.only <- T
    }
    self$base_eids <- c(1:length(weights))[!c(1:length(weights)) %in% fixed.edges]
  } else {
    self$adjoining.only <- F
    self$start_eid <-  NULL
    self$base_eids <- which(self$roads_ls[[round - 1]])
  }
  
  # Update weights and built vectors for start_eid
  self$built_bool_vec <- rep(FALSE, length(E(g)))
  if (FALSE) {# !is.null(self$start_eid)) {
    # Build start edge
    self$built_bool_vec[self$start_eid] <- TRUE
    self$weights[self$built_bool_vec] <- weights[self$built_bool_vec]/self$speedup
    self$built_eids <- c(start_eid)
  } else {
    self$built_eids <- c()
  }
  
  # .... allowed IDs
  self$allowed_eids <- self$base_eids[!self$base_eids %in% self$built_eids]
  
  # ... loss vector and built lenght
  self$loss_vec <- c()
  self$built.length = sum(self$lengths[self$built_eids])
  
  # ... dmat and loss value
  self$dmat <- johnson(x = self$bg, weight = self$weights, TRUE)
  self$loss <- get_loss()
}


# Road building

# Get candidates
roadbuilder.get_cand <- function(){
  if (self$step %% self$build_steps == 0 | self$step == 1) {
    # All possible candidates
    if(self$step != 1){
      self$allowed_eids <- self$allowed_eids[self$allowed_eids != self$built_eids[length(self$built_eids)]]
    }
    if(self$adjoining.only){
      # Adjoining
      self$candidate_eids <- hlc:::get_candidates(self$g, 
                                                  unique(c(self$built_eids, self$fixed.edges)), 
                                                  allowed_eids = self$allowed_eids)
      
      # Plus unbuilt start edges
      self$candidate_eids <- unique(c(self$candidate_eids, 
                                      self$start_eid[!self$start_eid %in% self$built_eids]))
    } else {
      self$candidate_eids <- self$allowed_eids[!self$allowed_eids %in% self$built_eids]
    }
    self$all_evaluated <- TRUE
  } else {
    # Only continue yesterday's construction + top N candidates
    self$allowed_eids <- self$allowed_eids[self$allowed_eids != self$built_eids[length(self$built_eids)]]
    self$candidate_eids <- hlc:::get_candidates(self$g, self$built_eids[length(self$built_eids)], self$allowed_eids)
    self$topn_eids <- topn_eids[!(self$topn_eids %in% self$built_eids)] # remove already built top-N candidates
    self$candidate_eids <- unique(c(self$topn_eids, self$candidate_eids))
    self$all_evaluated <- FALSE
  }
}
# Probe Candidates Fast // from hlc packages
roadbuilder.probe_fast <- function(){
  candidate_weights <- self$weights[self$candidate_eids]/self$speedup
  candidate_lengths <- self$lengths[self$candidate_eids]
  start_end_mat <- matrix(NA, length(self$candidate_eids), 2)
  for (i in 1:length(self$candidate_eids)) {
    vids_cpp <- match(as.vector(ends(self$g, self$candidate_eids[i])), 
                      V(self$g)$name) - 1
    start_end_mat[i, ] <- vids_cpp
  }
  candidate_loss_vec <- .Call("_hlc_get_loss_dem_cpp", PACKAGE = "hlc", self$dmat, 
                              self$pmat, candidate_weights, start_end_mat)
  #candidate_loss_vec <- hlc:::get_loss_dem_cpp(dist_mat = self$dmat, 
  #                                       p_mat = self$pmat, candidate_weights = candidate_weights, 
  #                                       start_end_mat = start_end_mat)
  candidate_improvement_vec <- self$loss - candidate_loss_vec
  candidate_improvement_vec <- candidate_improvement_vec/candidate_lengths
  return(list(candidate_improvement_vec = candidate_improvement_vec, 
              candidate_loss_vec = candidate_loss_vec))
}

# Evaluate Candidates
roadbuilder.eval_cand <- function(){
  if(length(self$candidate_eids) == 0) {
    if(self$all_evaluated){
      # If in last round all potential candidates have been checked
      print('Stop: No more candidates.')
      self$best_eid <- NULL
      self$stop.round <- T
    } else {
      # If not, check all, if still no improvement, break loop
      self$best_eid <- NULL
    }
  } else {
    # ... Evaluate
    cand_ls <- try(self$probe_fast())
    
    # ... Catch Error if so happens
    if(class(cand_ls) == "try-error"){
      print("Error in probe_fast() (probably igraph)")
      best_eid <- NULL
      
      ## Save best road and update top_N
    } else {
      candidate_improvement_vec <- cand_ls$candidate_improvement_vec
      self$best_eid <- candidate_eids[which.max(candidate_improvement_vec)]
      best_improvement <- max(candidate_improvement_vec)
      self$best_loss <- cand_ls$candidate_loss_vec[which.max(candidate_improvement_vec)]
      
      # Update topn_eids
      if(self$all_evaluated){
        this_top_N <- min(top_N, length(candidate_improvement_vec))
        self$topn_eids <- (self$candidate_eids[order(-candidate_improvement_vec)])[1:this_top_N]
      }
      
      if (self$best_loss >= self$loss | self$best_loss == 0) {
        self$best_eid <- NULL
        self$best_loss <- NULL
      }
    }
  }
}

# Update loss
roadbuilder.update_loss <- function(){
  self$loss <- get_loss()
}

# Build Roads
roadbuilder.build_road <- function(this.eid, this.speedup){
  
  # Update dmat and loss
  for(e in this.eid){
    self$dmat <- BoostGraph:::dmat_update(self$bg, self$dmat, e, 
                                          self$weights[e]/this.speedup)
  }
  
}

# Build Best Candidate
roadbuilder.build_best_eid <- function(){
  if(!is.null(self$best_eid)){
    # Build road
    build_road(this.eid = self$best_eid, this.speedup = self$speedup)
    
    # Safe trace
    self$built_bool_vec[self$best_eid] <- TRUE
    self$weights[self$best_eid] <- self$weights[self$best_eid]/self$speedup
    self$loss_vec <- c(self$loss_vec, self$loss)
    self$built_eids <- c(self$built_eids, self$best_eid)
    self$built.length <- self$built.length + self$lengths[self$best_eid]
    
    # Update loss
    self$loss <- self$best_loss
    
  }
}

# One step: Wrapper around one round-building step
roadbuilder.make_one_step <- function(){
  if(self$budget <= self$built.length){
    self$best_eid <- NULL
    self$best_loss <- NULL
  } else {
    # Update counter
    self$step <- self$step + 1
    
    # Get Candidates
    get_cand()
    
    # Probe Candidates
    probe_fast()
    
    # Evaluate Candidates
    eval_cand()
    
    # Build Roads
    build_best_eid()
  }
}

# Save information on roads built during one round
roadbuilder.save_round <- function(){
  self$roads_ls[[self$round]] <- self$built_bool_vec
  self$weights_ls[[self$round]] <- self$weights
  self$loss_ls[[self$round]] <- self$built_ls$loss_vec
}

# Wrapper around one road building round
roadbuilder.make_one_round <- function(noisy = T){
  # Update counter
  self$round <- self$round + 1
  
  # Initialize round
  initround(round = self$round)
  
  # Build roads
  save.loss <- self$loss
  while(self$budget > self$built.length){
    if(noisy){print(self$loss)}
    # Stop if loss == 0
    if(self$loss == 0){
      break
    }
    
    # Make a step
    make_one_step()
    
    # Stop if no more improvement
    if(save.loss == self$loss){
      break
    }
    save.loss <- self$loss
  }
  
  # Save result
  save_round()
}


# Wrapper around all rounds
roadbuilder.build_all_roads <- function(){
  for(r in c(1:length(self$budget_vec))){
    # Make round
    make_one_round()
  }
}


# Update igraph of roadbuilder
roadbuilder.update_igraph <- function(){
  # Save results to graph
  E(self$g)$rtype <- 0
  E(self$g)$weight <- E(self$g)$length * 1/6
  budget.spent <- c()
  for(t in c(1:self$round)){
    E(self$g)$rtype[self$roads_ls[[t]]] <- t
    E(self$g)$weight[self$roads_ls[[t]]] <- E(self$g)$weight[self$roads_ls[[t]]] / self$speedup_vec[t]
  }
}

# Save results sofar
roadbuilder.save_disk <- function(path.stub, type.color){
  # Post-process
  out_ls <- lapply(c(1:self$round), function(r){
    if(length(self$loss_ls) >= r){
      list(roads=self$roads_ls[[r]], weights=self$weights_ls[[r]], loss=self$loss_ls[[r]])
    } else {
      list(roads=self$roads_ls[[r]], weights=self$weights_ls[[r]], loss=NULL)
    }
  })
  loss_vec <- unlist(loss_ls)
  
  # Update igraph
  update_igraph()
  
  # Save optimal network
  g <- self$g
  save(g, file = paste0(path.stub, "_optnw.RData"))
  rm(g)
  
  # Save loss curve
  save(out_ls, file = paste0(path.stub,  "_outls.RData"))
  
  # Plot
  png(paste0(path.stub, "_plot.png"), width = abs(min(V(self$g)$x) - max(V(self$g)$x)) + 2, 
      height = abs(min(V(self$g)$y) - max(V(self$g)$y)) + 2,
      res = 200, unit = "in")
  par(mar = c(1,1,1,1))
  self$plot_disk(type.color)
  dev.off()
}

# Plot roadbuilder
roadbuilder.plot_disk <- function(type.color){
  # ... Plot Graph
  E(self$g)$color <- "lightgrey"
  E(self$g)$color[E(self$g)$rtype != 0 & !is.na(E(self$g)$rtype)] <- rev(type.color)[E(self$g)$rtype[E(self$g)$rtype != 0 & !is.na(E(self$g)$rtype)]]
  plot(self$g, vertex.size = 0.1, vertex.color = "black", vertex.label = NA, 
       edge.width = ifelse(E(self$g)$color == "lightgrey",1,2),
       layout = as.matrix(cbind(vertex_attr(self$g, "x"),
                                vertex_attr(self$g, "y"))))
}

roadbuilder <- R6Class(
  classname  = "roadbuilder", cloneable = F, portable = F,
  public = list(
    g = NULL,
    bg = NULL,
    pmat = NULL,
    dmat = NULL,
    loss = NULL,
    lengths = NULL,
    weights = NULL,
    fixed.edges = NULL,
    build_steps = NULL,
    top_N = NULL,
    # Global Objects
    speedup_vec = NULL,
    budget_vec = NULL,
    roads_ls = vector('list', 0),
    weights_ls = vector('list', 0),
    loss_ls = vector('list', 0),
    round = 0,
    # Round Objects
    speedup = NULL,
    budget = NULL,
    built_bool_vec = c(),
    start_eid = NULL,
    built_eids = NULL,
    base_eids = c(),
    loss_vec = NULL,
    allowed_eids = c(),
    adjoining.only = T,
    built.length = 0,
    step = 0,
    stop.round = F,
    # Step objects
    candidate_eids = c(),
    topn_eids = c(),
    all_evaluated = NULL,
    best_eid = NULL,
    best_loss = NULL,
    # Functions
    initialize = roadbuilder.initialize,
    initround = roadbuilder.initround,
    get_loss = roadbuilder.get_loss,
    get_cand = roadbuilder.get_cand,
    probe_fast = roadbuilder.probe_fast,
    eval_cand = roadbuilder.eval_cand,
    build_best_eid = roadbuilder.build_best_eid,
    build_road = roadbuilder.build_road,
    make_one_step = roadbuilder.make_one_step,
    save_round = roadbuilder.save_round,
    make_one_round = roadbuilder.make_one_round,
    build_all_roads = roadbuilder.build_all_roads,
    update_loss = roadbuilder.update_loss,
    update_igraph = roadbuilder.update_igraph,
    save_disk = roadbuilder.save_disk,
    plot_disk = roadbuilder.plot_disk
  )
  
)




##########################
# PREPARE SIMULATION
##########################

# (function) Get seed edges 
get_seed_edges <- function(g, p.vec, fix_edges, size = 10, seed = 250589){
  p.vec[is.na(p.vec)] <- 0
  set.seed(seed)
  these.v <- ends(g, c(1:length(E(g)))[!c(1:length(E(g))) %in% fix_edges], names = F)
  v.sample <- sample(these.v, min(c(size, sum(p.vec[these.v] > 0))), 
                     prob = p.vec[these.v] / sum(p.vec[these.v]))
  e.sample <- unlist(lapply(v.sample,function(v){sample(E(g)[from(v) & !c(1:length(E(g))) %in% fix_edges],1)}))
  # Return
  return(e.sample)
}

#' Initialize graph for road building
init_nodes <- function(cl){
  clusterEvalQ(cl, {
    
    # Load graph
    load(paste0(prep.path,"networks/mosaic.", mosaic.id, ".Rdata"))
    
    # Delete islands on graph
    g.components <- components(g)$membership
    g <- induced_subgraph(g,g.components==which.max(table(g.components)))
    
    # Check for consistency
    if(length(E(g)) == 0 | length(V(g)) == 0){
      g <- NULL
    }
    
    # Create weight attribute where missing
    E(g)$weight = E(g)$length * 1/6
    E(g)$name = c(1:length(E(g)))
    
    # Prepare Budget
    this.budget.df <- budget.df[budget.df$mosaic.id == mosaic.id,c("rtype","length")]
    budget_vec <- unlist(lapply(c(6:1), function(x){sum(this.budget.df$length[this.budget.df$rtype <= x])}))
    
    # Select roads to build on
    
    # ... label edges
    E(g)$mosaic.id <- NA
    all.m <- na.omit(unique(V(g)$mosaic.id))
    all.m <- all.m[order(all.m)]
    for(m in all.m){
      E(g)[from(which(V(g)$mosaic.id == m))]$mosaic.id <- m
    }
    
    # ... select
    fix_edges <- which(E(g)$mosaic.id != mosaic.id)
    build_edges <- which(E(g)$mosaic.id == mosaic.id)
    
    
    
    # Sample edge for starting
    start_eid  <- get_seed_edges(g, vertex_attr(g, paste0("pop", pop.year)), fix_edges, size = 3, seed = 250589)
    
    # Make roadbuilding objects
    node.roadbuilder <- roadbuilder$new(g = g, length.var = "length", weight.var = "weight", pop.var = paste0("pop", pop.year),
                                        start_eid = start_eid, speedup_vec = speedup_vec, budget_vec = budget_vec)
    
    # Remove some things
    rm(g)
    
    # Exit message
    print("initialized")
  })
}