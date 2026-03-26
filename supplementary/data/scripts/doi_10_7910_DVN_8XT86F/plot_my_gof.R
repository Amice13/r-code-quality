
# multiple join function
multi_join <- function(list_of_loaded_data, join_func, ...){
    require("dplyr")
    output <- Reduce(function(x, y) {join_func(x, y, ...)}, list_of_loaded_data)
    return(output)
}

# function to compute gof statistics
statistic.dist <- function(statistic.dist.list, g, metric, max.dig = NULL) {
    
    n <- length(statistic.dist.list)
    
    min.statistic.dist <- min(do.call("c", lapply(statistic.dist.list, min)))
    max.statistic.dist <- max(do.call("c", lapply(statistic.dist.list, max)))
    
    if (metric == "eigen"){
        seq_statistic <- seq(min.statistic.dist, max.statistic.dist, 1/(10^max.dig))
    } else {
        seq_statistic <- c(min.statistic.dist : max.statistic.dist)    
    }
    
    table.statistic.dist <- lapply(statistic.dist.list, table)
    table.statistic.dist <- lapply(table.statistic.dist, function(x) data.frame(statistic = names(x), obs = as.numeric(x), stringsAsFactors = F))
    table.statistic.dist <- multi_join(table.statistic.dist, full_join, by = "statistic")
    table.statistic.dist <- merge(data.frame(statistic = seq_statistic), table.statistic.dist,
                                  by = "statistic", all.x = T)
    table.statistic.dist[is.na(table.statistic.dist)] <- 0
    
    table.prob.statistic.dist <- apply(table.statistic.dist[, - 1], 2, function(x) x/sum(x[!is.infinite(x)]))
    table.prob.statistic.dist <- data.frame(table.prob.statistic.dist)
    table.prob.statistic.dist[] <- lapply(table.prob.statistic.dist,
                                          function(x) x/(1-x))
    table.prob.statistic.dist[] <- lapply(table.prob.statistic.dist,
                                          function(x) log(x))
    table.prob.statistic.dist <- cbind(table.statistic.dist[, 1], table.prob.statistic.dist)
    
    if (metric == "degree" | metric == "between") {
        table.obs.statistic <- table(degree(g))  
    } else if (metric == "geo.dist") {
        table.obs.statistic <- table(c(igraph::distances(intergraph::asIgraph(g))))
    } else if (metric == "eigen") {
        table.obs.statistic <- table(round(evcent(g), max.dig))
    }
    
    table.obs.statistic <- data.frame(statistic = names(table.obs.statistic),
                                      obs = as.numeric(table.obs.statistic))
    table.obs.statistic <- merge(data.frame(statistic = seq_statistic), table.obs.statistic, by = "statistic", all.x = T)
    table.obs.statistic[is.na(table.obs.statistic)] <- 0
    table.obs.statistic[ , 2] <- table.obs.statistic[ , 2] / (sum(table.obs.statistic[ , 2]))
    table.obs.statistic[ , 2] <- table.obs.statistic[ , 2] / (1 - table.obs.statistic[ , 2])
    table.obs.statistic[ , 2] <- log(table.obs.statistic[ , 2])
    table.obs.statistic <- table.obs.statistic[!is.infinite(table.obs.statistic[ , 2]), ]
    colnames(table.prob.statistic.dist) <- c("seq_statistic", paste0("sim_", 2:ncol(table.prob.statistic.dist)))
    
    table.statistic.dist$seq_statistic <- as.factor(table.prob.statistic.dist$seq_statistic)
    
    data_long <- tidyr::gather(table.prob.statistic.dist, seq_statistic, factor_key = TRUE)
    data_long$statistic <- rep(seq_statistic, n)
    if (metric == "geo.dist" | metric == "eigen") {
        data_long <- data_long[data_long$statistic %in% table.obs.statistic$statistic, ]
    } else {
        data_long <- data_long[data_long$statistic <= max(table.obs.statistic$statistic), ]    
    }
    data_long <- data_long[!is.infinite(data_long[,2]), ]
    
    if (metric == "eigen") {
        data_long <- data_long[data_long$statistic %in% table.obs.statistic$statistic, ]  
        table.obs.statistic <- table.obs.statistic[table.obs.statistic$statistic %in% data_long$statistic, ]
        
        data_long$statistic <- factor(data_long$statistic * 10)
        data_long$statistic <- factor(data_long$statistic,levels(data_long$statistic)[order(as.numeric(as.character(levels(data_long$statistic))))])
        
        table.obs.statistic$statistic <- factor(table.obs.statistic$statistic * 10)
        table.obs.statistic$statistic <- factor(table.obs.statistic$statistic, levels(table.obs.statistic$statistic)[c(order(as.numeric(as.character(levels(table.obs.statistic$statistic)))))])
        
    } else {
        data_long <- data_long[data_long$statistic %in% table.obs.statistic$statistic, ]  
        table.obs.statistic <- table.obs.statistic[table.obs.statistic$statistic %in% data_long$statistic, ]
        
        data_long$statistic <- as.factor(data_long$statistic)
    }
    
    if (metric == "geo.dist") {
        data_long <- data_long[which(data_long$statistic != 0), ]
        table.obs.statistic <- table.obs.statistic[which(table.obs.statistic$statistic != 0), ]
    }
    return(list(table.obs.statistic, data_long))
}

# function to plot gof
my.gof <- function(g, fit, rescale = T) {
    # load required libraries
    require(tidyr); require(ggplot2); require(purrr); require(sna)
    
    # simulate networks
    foo <- simulate(fit, 100)
    
    #---------------------------------------------------------------------------
    # Degree distribution plot
    #---------------------------------------------------------------------------
    
    # Degree distribution simulated
    deg.dist.list <- lapply(foo, sna::degree)
    
    # Store statistics
    tmp <- statistic.dist(statistic.dist.list = deg.dist.list, g = g, metric = "degree")
    data_long <- tmp[[2]]
    table.obs.deg <- tmp[[1]]
    
    # X axis: settings
    x.lab.lev <- levels(data_long$statistic)
    sel <- seq(1, length(x.lab.lev), 20)
    x.lab.lev <- x.lab.lev[sel]
    x.lab.lab <- as.numeric(as.character(x.lab.lev))
    
    # Plot
    p_degree <- ggplot(data_long, 
                       aes(x = statistic, y = value)) + 
        geom_boxplot() +
        stat_summary(fun.y = max, geom="line", aes(group=1), linetype="dotted", size = 1.2) + 
        stat_summary(fun.y = min, geom="line", aes(group=1), linetype="dotted", size = 1.2) +  
        geom_line(data = table.obs.deg,
                  aes(x = as.factor(statistic), y = obs, group = 1), size = 1.25) +
        labs(title = "", x = "degree", y = "log-odds for a node") +
        theme_bw() + 
        theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), axis.text = element_text(family="Times New Roman", size = 12),
              axis.title = element_text(family="Times New Roman", size = 14),
              axis.text.x = element_text(family="Times New Roman", angle = 90, hjust = 1, size = 10),
              
              panel.grid.major.x = element_blank(),
              plot.title = element_text(size = 14, hjust = 0.5)) + 
        scale_x_discrete(breaks = x.lab.lev, labels = x.lab.lab)
    
    #---------------------------------------------------------------------------
    # Eigenvector distribution plot
    #---------------------------------------------------------------------------
    
    # Eigenvector distribution simulated
    evc.dist.list <- lapply(foo, sna::evcent)
    evc.dist.list <- lapply(evc.dist.list, round, digits = 4)
    
    # Store statistics
    tmp <- statistic.dist(statistic.dist.list = evc.dist.list, g = g, metric = "eigen", max.dig = 4)
    data_long <- tmp[[2]]
    table.obs.evc <- tmp[[1]]
    
    # X and Y axis: settings
    k <- 2
    x <- as.numeric(as.character(levels(data_long$statistic)))
    y <- order(x)
    z <- seq(min(x), max(x), min(x[-1]))#(max(x) - min(x)) / (length(x) + k)
    z <- sort(c(z,x))
    z <- z[!duplicated(z)]
    z[which(z %in% x == F)] <- "skip"
    m <- length(levels(data_long$statistic))
    sel <- rep(F, m)
    sel[seq(1, m, 5)[- 2]] <- T
    sel[m] <- T
    
    # Plot
    p_evc <- ggplot(data_long, 
                    aes(x = statistic, y = value)) + 
        geom_boxplot() +
        stat_summary(fun.y = max, geom="line", aes(group=1), linetype="dotted", size = 0.6) + 
        stat_summary(fun.y = min, geom="line", aes(group=1), linetype="dotted", size = 0.6) +  
        geom_line(data = table.obs.evc,
                  aes(x = statistic, y = obs, group = 1), size = 0.7) +
        labs(title = "", x = "eigenvector", y = "log-odds for a node") +
        theme_bw() + 
        theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), axis.text = element_text(family="Times New Roman", size = 12),
              #          axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 100, l = 0)),
              axis.text.x = element_text(family="Times New Roman", angle = 90, hjust = 1, size = 10),
              
              axis.title = element_text(family="Times New Roman", size = 14),
              plot.title = element_text(family="Times New Roman", size = 14, hjust = 0.5),
              panel.grid.major.x = element_blank()) 
    if(rescale == T){
        x.lab.lev <- levels(data_long$statistic)[sel]
        x.lab.lab <- format(as.numeric(as.character(x.lab.lev)), digits = 4, nsmall = 4)
        reduce_sel <- round(seq(1, length(x.lab.lev), length.out = 10))
        p_evc <- p_evc + #theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
            scale_x_discrete(breaks = x.lab.lev[reduce_sel],
                             #                   limits = z,
                             labels = x.lab.lab[reduce_sel])
    } else {
        x.lab.lev <- levels(data_long$statistic)
        x.lab.lab <- format(as.numeric(as.character(x.lab.lev)), digits = 4, nsmall = 4)
        p_evc <- p_evc + scale_x_discrete(labels = x.lab.lab)
    }
    
    #---------------------------------------------------------------------------
    # Betweenness distribution plot
    #---------------------------------------------------------------------------
    
    # Betweenness distribution simulated
    btw.dist.list <- lapply(foo, sna::betweenness)
    btw.dist.list <- lapply(btw.dist.list, round)
    
    # Store statistics
    tmp <- statistic.dist(statistic.dist.list = btw.dist.list, g = g, metric = "between")
    data_long <- tmp[[2]]
    table.obs.deg <- tmp[[1]]
    
    # X axis settings
    x.lab.lev <- levels(data_long$statistic)
    sel <- seq(1, length(x.lab.lev), 20)
    x.lab.lev <- x.lab.lev[sel]
    x.lab.lab <- as.numeric(as.character(x.lab.lev))
    
    # Plot
    p_between <- ggplot(data_long, 
                        aes(x = statistic, y = value)) + 
        geom_boxplot() +
        stat_summary(fun.y = max, geom="line", aes(group=1), linetype="dotted", size = 1.2) + 
        stat_summary(fun.y = min, geom="line", aes(group=1), linetype="dotted", size = 1.2) +  
        geom_line(data = table.obs.deg,
                  aes(x = as.factor(statistic), y = obs, group = 1), size = 1.25) +
        labs(title = "", x = "betweenness", y = "log-odds for a node") +
        theme_bw() + 
        theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), axis.text = element_text(family="Times New Roman", size = 12),
              axis.title = element_text(family="Times New Roman", size = 14),
              axis.text.x = element_text(family="Times New Roman", angle = 90, hjust = 1, size = 10),
              
              panel.grid.major.x = element_blank(),
              plot.title = element_text(size = 14, hjust = 0.5)) + 
        scale_x_discrete(breaks = x.lab.lev, labels = x.lab.lab)
    
    return(list(p_degree, p_evc, p_between))
}

