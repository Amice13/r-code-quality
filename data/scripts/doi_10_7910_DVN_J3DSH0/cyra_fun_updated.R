createLambdaikTable <- function(path_c, la_vec){
  df <- unique(path_c[, c("T", "A")])
  colnames(df) = c("i", "k")
  df['la'] <- la_vec
  return(df)
}

generateQuantiles <- function(ub, interval){
  q <- seq(interval, ub, interval)
  return(q)
}

prepareCyraParameters = function (ind.data, loss.dist.name = "lnorm", grid.ub.prob = 0.9999, 
          grid.num = 1000L) 
{
  data_c <- ind.data[ind.data[, "company"], ] 
  # |> 
  #   split(f = ~T+V+A) |> 
  #   lapply(FUN = function(x){if(sum(x$X)==0){x = NULL} else {x}}) |> 
  #   lapply(FUN = function(x){if(length(unique(x$X)) < 2){x = NULL} else {x}}) |>
  #   do.call(what = "rbind")
  rownames(data_c) = c()
  
  if(is.null(data_c)){
    return(NULL)
  }
  
  path_c <- unique(data_c[c("T", "V", "A")])
  all_path = apply(data_c[,c("T", "V", "A")], 1, paste0)
  class_probs = table(all_path)/length(all_path)
  ent = -sum(class_probs*log(class_probs))
  l <- unlist(unique(data_c["T"]))
  m <- unlist(unique(data_c["V"]))
  n <- unlist(unique(data_c["A"]))
  year_min <- min(as.numeric(data_c[, "year"]))
  year_max <- max(as.numeric(data_c[, "year"]))
  lambda <- sum(table(data_c["year"]))/(year_max - year_min + 1)
  loss_params <- NULL
  q_ub <- 0
  for (r in 1:nrow(path_c)) {
    i <- path_c[r, "T"]
    j <- path_c[r, "V"]
    k <- path_c[r, "A"]
    loss_data <- ind.data[(ind.data["T"] == i) & (ind.data["V"] == j) & (ind.data["A"] == k), "X"]
    zero_mass <- sum(loss_data == 0)/length(loss_data)
    truncated_data <- unique(loss_data[which(loss_data != 0)])
    params <- fitdistrplus::fitdist(truncated_data, loss.dist.name)
    q_temp <- do.call(paste0("q", loss.dist.name), c(list(p = grid.ub.prob), 
                                                     params$estimate))
    if (q_temp > q_ub) {
      q_ub <- q_temp
    }
    loss_params <- as.data.frame(rbind(c(i = i, j = j, k = k, 
                                         p0 = zero_mass, params$estimate), loss_params))
  }
  
  lambda_ik <- unique(path_c[, c("T", "A")])
  colnames(lambda_ik) = c("i", "k")
  lambda_ik["la"] = NA
  for(row in 1:nrow(lambda_ik)){
    data_for_lambda = data_c[(data_c$T == lambda_ik[row,]$i) & (data_c$A == lambda_ik[row,]$k), ]
    lambda_ik[row, "la"] = sum(table(data_for_lambda["year"]))/(year_max - year_min + 1)
  }
  
  # lambda_ik <- (function(x) createLambdaikTable(path_c, x))((function(x) {
  #   x[is.na(x)] <- 0
  #   x
  # })(sapply(split(data_c, f = c(data_c["T"], data_c["A"])), 
  #           function(x) sum(table(x["year"]))/(year_max - year_min + 1))))
  q_interval <- q_ub/grid.num
  q <- generateQuantiles(q_ub, q_interval)
  p_i <- (function(x) {
    colnames(x) <- c("i", "p")
    x
  })(as.data.frame((table(data_c["T"])/nrow(data_c))))
  params <- list(l = l, m = m, n = n, lambda = lambda, lambda_ik = lambda_ik, 
                 dist_name = loss.dist.name, dist_params_names = names(params$estimate), 
                 loss_params = loss_params, q = q, p_i = p_i, path_c = path_c, ent=ent)
  class(params) <- "cyraParams"
  return(params)
}

generateInsurTable = function(ik_pairs){
  comb <- lapply(1:nrow(ik_pairs), function(x) return(c(0,1)))
  tb <- expand.grid(comb)
  colnames(tb) = paste0('c', ik_pairs[,1], ik_pairs[,2], sep = "")
  return(tb)
}

createWeightsTables <- function(cyra.params, nu.ik, om.ik, eta.j, nu, om, eta, path_c) {
  l <- cyra.params$l
  m <- cyra.params$m
  n <- cyra.params$n
  nu_om_ik <- createNuOmegaikTable(path_c, nu.ik, om.ik)
  nu_om <- createNuOmegaTable(nu, om)
  etas <- createEtaTable(m, eta.j, eta)
  weights <- list(
    nu_om_ik = nu_om_ik,
    nu_om = nu_om,
    etas = etas
  )
  class(weights) <- "cyraWeights"
  return(
    weights
  )
}

createNuOmegaikTable <- function(path_c, nu.ik, om.ik) {
  df <- unique(path_c[, c("T", "A")])
  colnames(df) = c("i", "k")
  df["nu"] <- nu.ik
  df["om"] <- om.ik
  return(df)
}

createEtaTable <- function(m, eta.j, eta) {
  df <- data.frame(j = c(m, 0), eta = c(eta.j, eta))
  return(df)
}

createNuOmegaTable <- function(nu, om) {
  return(
    data.frame(
      nu = nu,
      om = om
    )
  )
}

prepXParams <- function(l,m,n, loss.params, dist.params.names){
  params_df <- expand.grid(l, m, n)
  colnames(params_df) <- c('i', 'j', 'k')
  params_df['p0'] <- NA
  params_df[dist.params.names] <- NA
  params_df <- merge(params_df, loss.params, by = c('i', 'j', 'k'), all = TRUE, suffixes = c('drop',''))
  params_df[paste0(c(dist.params.names,'p0'), 'drop')] <- NULL
  params_df = params_df[which(!is.na(params_df$p0)), ]
  return(params_df)
}

generateLossDists <- function(th, cyra.params, scale.fun = scaleLogNormal) {
  if(class(cyra.params) != "cyraParams"){
    stop("cyra.params has to be a list of class cyraParams")
  }
  params_df <- prepXParams(cyra.params$l, cyra.params$m, cyra.params$n, cyra.params$loss_params, cyra.params$dist_params_names)
  x_mass <- generateXMassTable(th, params_df, cyra.params, scale.fun)
  zik_mass <- generateZikMassTable(cyra.params$q, th, x_mass)
  zi_mass <- generateZiMassTable(cyra.params$q, zik_mass)
  l_mass <- generateLMassTable(cyra.params$p_i, zi_mass)
  s_mass <- generateSMassTable(cyra.params$q, cyra.params$lambda, l_mass)
  sik_mass <- generateSikMassTable(cyra.params$q, cyra.params$lambda_ik, zik_mass)
  
  out <- list(
    X_dist = x_mass,
    Zik_dist = zik_mass,
    Zi_dist = zi_mass,
    L_dist = l_mass,
    S_dist = s_mass,
    Sik_dist = sik_mass
  )
}

generatePanjerMass <- function(lambda, mass_vec){
  if(length(mass_vec)>1){
    p_0 <- dpois(0, lambda)
    ## The (a, b, 0) class
    a <- 0
    b <- lambda
    ## Recursion
    len <- length(mass_vec)
    s <- numeric(len)
    s[1] = p_0 * exp(mass_vec[1] * b)
    tmp = mass_vec[2:len]
    
    for (k in 1:(len-1)) {
      coef_1 <- a + b*seq(1, k, 1) / k
      coef_2 <- tmp[1:k]
      coef_3 <- s[k:1]
      s[k+1] <- sum(coef_1 * coef_2 * coef_3)
    }
  }else {
    s = mass_vec
  }
  
  return(s)
}

generateSMassTable <- function(q, lambda, l_mass_table){
  s_mass <- generatePanjerMass(lambda, unlist(l_mass_table))
  names(s_mass) <- paste0("p", c(0, q))
  s_mass <- t(as.data.frame(s_mass)) |> as.data.frame()
  return(s_mass)
}

generateSikMassTable <- function(q, lambda_ik, zik_mass_table){
  df <- merge(zik_mass_table, lambda_ik, by = c('i', 'k'), all = TRUE)
  df <- df[order(df$k),]
  sik_mass <- data.frame()
  for(r in 1:nrow(df)){
    sik_mass[r, c('i', 'k')] <- df[r, c('i', 'k')]
    sik_mass[r, paste0('p', c(0,q))] <- generatePanjerMass(df$la[r], unlist(zik_mass_table[r, paste0('p', c(0,q))]))
  }
  return(sik_mass)
}


generateXMassTable <- function(th, params.df, cyra.params, scale.fun){
  p_names <- paste0('p', cyra.params$q)
  params.df[p_names] <- NA
  for(i in 1:nrow(params.df)){
    if((!is.na(params.df[i,5]) & (th[as.character(params.df[i,'j'])] != 0))){
      X <- unlist(params.df[i,])
      new_params <- do.call(scale.fun, c(list(th = th[as.character(params.df[i,'j'])]), params.df[i,cyra.params$dist_params_names]))
      params.df[i, p_names] <- (1-X['p0'])*do.call(generateDiscretizedProbs, c(list(q = cyra.params$q, pdist = paste0("p", cyra.params$dist_name), new_params)))
    } else {
      params.df[i, p_names] <- rep(0, length(cyra.params$q))
      params.df[i, 'p0'] <- 1
    }
  }
  
  return(params.df)
}

convolveDiscreteRV <- function(q, p1, p2){
  q <- c(0,q)
  n <- length(q)
  p <- numeric(n)
  for (i in 1:n){
    p[i] <- sum(p1[1:i] * p2[i:1])
  }
  return(p)
}

generateEachZikMass <- function(q, pair_table){
  pair_p <- pair_table[, paste0('p', c(0,q))]
  pair_p_each <- split(as.matrix(pair_p), f=1:nrow(pair_p))
  z_mass <- Reduce(function(x, y) convolveDiscreteRV(q,x,y), pair_p_each)
  names(z_mass) <- paste0('p', c(0,q))
  z_mass <- data.frame(t(z_mass))
  z_mass['i'] <- pair_table[1, 'i']
  z_mass['k'] <- pair_table[1, 'k']
  return(z_mass)
}

generateZikMassTable <- function(q, th, x_mass_table){
  full_ctrl_idx <- which(th==0) # this needs to be changed if control takes values other than 0 and 1
  x_mass_w_ctrl <- x_mass_table[! x_mass_table$j %in% full_ctrl_idx, ]
  if(nrow(x_mass_w_ctrl) == 0 ){
    zik_table <- unique(x_mass_table[,c('i', 'k')])
    zik_table['p0'] <- 1
    zik_table[paste0('p', q)] <- 0
    return(zik_table)
  }
  
  ik_pairs <- split(x_mass_w_ctrl, f=list(x_mass_w_ctrl$i, x_mass_w_ctrl$k), drop = TRUE)
  zik_list <- lapply(ik_pairs, generateEachZikMass, q=q)
  zik_table <- do.call('rbind', zik_list)
  return(zik_table)
}

generateZiMassTable <- function(q, z_mass_table){
  zis <- split(z_mass_table, f=z_mass_table$i)
  zi_list <- lapply(zis, function(zi){
    p <- zi[, paste0('p', c(0, q))]
    if(length(p)>1){
      p_each <- split(as.matrix(p), f=1:nrow(p))
    } else {
      p_each = as.matrix(p)
    }
    zi_mass <- Reduce(function(x,y) convolveDiscreteRV(q,x,y), p_each)
    names(zi_mass) <- paste0('p', c(0,q))
    zi_mass <- data.frame(t(zi_mass))
    zi_mass['i'] <- zi[1, 'i']
    return(zi_mass)
  })
  zi_table <- do.call('rbind', zi_list)
  return(zi_table)
}

generateLMassTable <- function(p_i, zi_table){
  all_i <- p_i$i
  l <- p_i[p_i$i==all_i[1],]$p * zi_table[zi_table$i==all_i[1],]
  if(length(all_i) > 1){
    for(i in all_i[2:length(all_i)]){
      l <- l+p_i[p_i$i==i,]$p * zi_table[zi_table$i==i,]
    }
    l <- subset(l, select = -i)
  } else{
    l <- subset(l, select = -i)
  }
  return(l)
}

generateCikMassTable <- function(insur, zik_mass, q, ded=0){
  cik_mass <- zik_mass
  for(i in 1:nrow(zik_mass)){
    if(insur[i] == 1){
      q_0 <- c(0, q)
      retained_vec <- which(q_0<ded)
      retained_prob <- sum(cik_mass[i,retained_vec])
      if(length(retained_vec) == 0) retained_vec <- 0
      if(length(retained_vec) != length(q_0)){
        cik_mass[i, retained_vec[length(retained_vec)]+1] <-  1-retained_prob
        zero_prob_q <- q[(retained_vec[length(retained_vec)]+1):length(q)]
        cik_mass[i, c(paste0('p', zero_prob_q))] <- 0 
      }
    }
  }
  return(cik_mass)
}
#subclaim, claim on each threat-asset pair
generateSubClaimMassTable = function(insur, zik_mass, q, ded=0){
  insur_mass <- zik_mass[, c('i', 'k')]
  insur_mass[, c("p0", paste0("p", (q-ded)[which((q-ded)>0)]))] <- 0
  for(i in 1:nrow(zik_mass)){
    if(insur[i] == 1){
      q_0 <- c(0,q)
      retained_vec <- which(q_0<=ded)
      retained_prob <- sum(cik_mass[i,retained_vec])
      insur_mass[i,'p0'] <- retained_prob
      insur_mass[i, paste0("p", (q-ded)[which((q-ded)>0)])] <- zik_mass[i, paste0("p", q[q>ded])]
    }
  }
  return(
    list(
      q = (q-ded)[which((q-ded)>0)],
      mass = insur_mass)
  )
}

calcPremiumTable <- function(insur, sik_mass, q, profit_loading){
  tot_prem <- sik_mass[c('i', 'k')]
  tot_prem['prem'] <- 0
  for(i in 1:nrow(sik_mass)){
    if(insur[i] == 1){
      prem_ik <- sik_mass[i,paste0('p', q)] * q
      tot_prem[i, 'prem'] <- sum(prem_ik) * (1+profit_loading)
    }
  }
  return(tot_prem)
}

# generate a matrix for all possible constrained cases for each pair of ik
generateIMat <- function(ik_pairs) {
  comb_list <- list()
  for (i in 1:nrow(ik_pairs)) {
    comb_list[[i]] <- c(0, 1)
  }
  comb_grid <- t(expand.grid(comb_list))
  I_mat <- cbind(ik_pairs, comb_grid)
  return(I_mat)
}

# calculate the holistic Kik in constrained case
calcConstrainedHolisticKik <- function(I_mat, nu_om, nu_om_ik, Kik_bar, K_bar) {
  feasible_Iik <- calcFeasibleIik(I_mat, nu_om, nu_om_ik, Kik_bar, K_bar)
  holistic_Iik <- feasible_Iik
  holistic_Iik["Kik"] <- 0
  for (r in 1:nrow(feasible_Iik)) {
    if (feasible_Iik[r, ]$Iik != 0) {
      Kik <- Kik_bar[Kik_bar$i == feasible_Iik[r, ]$i & Kik_bar$k == feasible_Iik[r, ]$k, ]$Kik
      Bik <- calcConstrainedBik(feasible_Iik[r, ]$i, feasible_Iik[r, ]$k, feasible_Iik, nu_om, nu_om_ik)
      ik_other <- feasible_Iik[feasible_Iik$i != feasible_Iik[r, ]$i | feasible_Iik$k != feasible_Iik[r, ]$k, c("i", "k")]
      Tik_sum <- 0
      if(nrow(ik_other)){for (s in 1:nrow(ik_other)) {
        Tik_sum <- Tik_sum + calcConstrainedTik(ik_other[s, ]$i, ik_other[s, ]$k, feasible_Iik, Kik_bar)
      }}
      holistic_Iik$Kik[r] <- unlist(Kik - Bik * (Tik_sum + Kik - K_bar))
    }
  }
  return(holistic_Iik)
}

# find the feasible ik pair that meets all constraints
calcFeasibleIik <- function(I_mat, nu_om, nu_om_ik, Kik_bar, K_bar) {
  ik <- I_mat[, c("i", "k")]
  Iik_sets <- list()
  for (r in 1:nrow(ik)) {
    Iik_sets[[r]] <- calcConstrainedIikIndex(ik[r, ]$i, ik[r, ]$k, I_mat, nu_om, nu_om_ik, Kik_bar, K_bar)
  }
  feasible_Iik_idx <- Reduce(intersect, Iik_sets)
  I_mat_noik <- subset(I_mat, select = -c(i, k))
  feasible_Iik <- cbind(ik, Iik = I_mat_noik[, feasible_Iik_idx])
  return(feasible_Iik)
}

# find the possible cases for each ik pair
calcConstrainedIikIndex <- function(i, k, I_mat, nu_om, nu_om_ik, Kik_bar, K_bar) {
  Iik <- unlist(subset(I_mat[I_mat$i == i & I_mat$k == k, ], select = -c(i, k)))
  Kik <- Kik_bar[Kik_bar$i == i & Kik_bar$k == k, ]$Kik
  Bik <- calcConstrainedBik(i, k, I_mat, nu_om, nu_om_ik)
  ik_other <- I_mat[I_mat$i != i | I_mat$k != k, c("i", "k")]
  Tik_sum <- 0
  if(nrow(ik_other)){
    for (r in 1:nrow(ik_other)) {
      Tik_sum <- Tik_sum + calcConstrainedTik(ik_other[r, ]$i, ik_other[r, ]$k, I_mat, Kik_bar)
    } 
  }
  condition_val <- Kik - Bik * (Tik_sum + Kik - K_bar)
  condition_bool <- as.numeric(condition_val >= 0)
  return(which(condition_bool == Iik))
}

# calculate Bik in constrained case
calcConstrainedBik <- function(i, k, I_mat, nu_om, nu_om_ik) {
  Iother <- I_mat[I_mat$i != i | I_mat$k != k, ]
  Iother <- merge(Iother, nu_om_ik, by = c("i", "k"), all.x = TRUE)
  Tother <- subset(Iother, select = -c(i, k, nu, om)) * (1 / Iother$om)
  T_sum <- colSums(Tother)
  omik <- nu_om_ik[nu_om_ik$i == i & nu_om_ik$k == k, ]$om
  om <- nu_om$om
  denominator <- T_sum + 1 / omik + 1 / om
  Bik <- (1 / omik) / denominator
  return(Bik)
}

# calculate T2 for condition checking and holistic Kik in constrained case
calcConstrainedTik <- function(i, k, I_mat, Kik_bar) {
  Iik <- subset(I_mat[I_mat$i == i & I_mat$k == k, ], select = -c(i, k))
  Kik <- Kik_bar[Kik_bar$i == i & Kik_bar$k == k, ]$Kik
  Tik <- Iik * Kik
  return(Tik)
}

calcPenaltyValue<- function(x, p, q) {
  if ((1 - sum(p[q <= x])) == 0) {
    return(0)
  }
  return(as.numeric(q > x) / (1 - sum(p[q <= x])))
}

calcObjValue <- function(penalty.thres, q, Kik_holistic, M, nu_om, nu_om_ik, etas, ga, ga_ik, sik_mass, s_mass, premium_table) {
  part1 <- sum(etas[etas$j != 0, "eta", drop = TRUE] * M) + etas[etas$j == 0, "eta", drop = TRUE] * sum(M)
  part2 <- sum(Kik_holistic$Kik * nu_om_ik$nu) + nu_om$nu * sum(Kik_holistic$Kik)
  nu_om_ik["E"] <- 0
  for (r in 1:nrow(sik_mass)) {
    p <- sik_mass[r, paste0("p", c(0, q))]
    h <- calcPenaltyValue(calcVaR(alpha = penalty.thres, p = unlist(p), q = q), p, c(0, q))
    quad <- (c(0, q) - Kik_holistic$Kik[which(Kik_holistic$i == sik_mass$i[r] & Kik_holistic$k == sik_mass$k[r])])^2
    e <- sum(p * h * quad)
    nu_om_ik[which(nu_om_ik$i == sik_mass$i[r] & nu_om_ik$k == sik_mass$k[r]), "E"] <- e
  }
  part3 <- sum(nu_om_ik$om * nu_om_ik$E)
  p <- unlist(s_mass)
  h <- calcPenaltyValue(calcVaR(alpha = penalty.thres, p = p, q = q), p, c(0, q))
  quad <- (c(0, q) - sum(Kik_holistic$Kik))^2
  part3 <- part3 + nu_om["om"]*sum(p * h * quad)
  # print(part2)
  # print(part3)
  
  part4 <- sum(premium_table$prem * (ga_ik$ga+ga))
  return(
    c(
      cost_c = part1,
      cost_i = part4,
      cost_r = part2 + part3,
      cost_tot = part1 + part2 + part3 + part4
    )
  )
}

extractFromInvestTable = function(invest.table, field){
  th_table <- data.frame()
  for (i in 1:nrow(invest.table)) {
    th_row <- sapply(1:ncol(invest_table), function(x) invest_table[[i, x]][[field]])
    th_table <- rbind(th_table, th_row)
  }
  rownames(th_table) <- NULL
  colnames(th_table) <- paste0(field, names(invest_table))
  return(th_table)
}
