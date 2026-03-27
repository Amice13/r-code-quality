custom_mm_plot <- function(cj_mm_object, dim_attr_list, dim_names, order_of_dimensions=NULL, offset = 0.15, show_legend = FALSE, legend_text = NULL, legend_inset = c(-0.35,.4), xlim=c(0,1), plot_title="", my_col=NULL, increase_ylim=0) {
  
  cj_mm_object$level <- unlist(dim_attr_list) # Replace changed factor labels with original ones
  numb_of_dim <- length(dim_attr_list)
  numb_of_attributes_total <- length(unlist(dim_attr_list))
  
  if("BY"%in%colnames(cj_mm_object)){
    n_comp <- length(unique(cj_mm_object$BY))
  } else {
    n_comp <- 1
  }
  if(is.null(my_col)){
    if(n_comp >2 ){
      my_col <- RColorBrewer::brewer.pal(n_comp, "Set2")
    } else {
      my_col <- RColorBrewer::brewer.pal(3, "Set2") [1:2]
    }
  }
  
  my_pch <- c(16, 17, 15, 18, 21, 24, 22, 23)[1:n_comp]
  offset <- c(0, -offset, offset, -offset*2, offset*2, -offset*3, offset*3, -offset*4, offset*4)[1:n_comp]
  
  par(mar=c(6,14,2,1))
  plot(-2,-2, xlim=xlim, ylim=c(1-increase_ylim,numb_of_dim+numb_of_attributes_total), 
       type="n", axes=F, ylab="", xlab="Marginal Mean", main=plot_title)
  abline(v=0.5, col="grey70")
  
  # add custom order
  if(!is.null(order_of_dimensions)) {
    dim_attr_list <- dim_attr_list[order_of_dimensions]
    dim_names <- dim_names[order_of_dimensions]
  }
  
  dim_and_attr <- character()  
  for (i in 1:numb_of_dim) {
    dim_and_attr <- c(dim_and_attr, names(dim_attr_list)[i], dim_attr_list[[i]])
  }
  
  # estimates with ci for all comparison groups   
  for(k in 1:n_comp) {
    if(n_comp>1) {
      estim_and_ci_group_k <- cj_mm_object[cj_mm_object$BY==unique(cj_mm_object$BY)[k],]      
    } else {
      estim_and_ci_group_k <- cj_mm_object
    }
    estim <- numeric()
    lower <- numeric()
    upper <- numeric()
    for (i in 1:numb_of_dim) {
      estim <- c(estim, NA)
      lower <- c(lower, NA)
      upper <- c(upper, NA)
      for (j in 1:length(dim_attr_list[[i]])){
        estim <- c(estim, estim_and_ci_group_k$estimate[estim_and_ci_group_k$feature==(names(dim_attr_list)[i]) & estim_and_ci_group_k$level==dim_attr_list[[i]][j]])
        lower <- c(lower, estim_and_ci_group_k$lower[estim_and_ci_group_k$feature==(names(dim_attr_list)[i]) & estim_and_ci_group_k$level==dim_attr_list[[i]][j]])
        upper <- c(upper, estim_and_ci_group_k$upper[estim_and_ci_group_k$feature==(names(dim_attr_list)[i]) & estim_and_ci_group_k$level==dim_attr_list[[i]][j]])
      }
    }  
    
    points(estim, length(dim_and_attr):1+offset[k], pch=my_pch[k], col = my_col[k]) #my_col_vec)
    
    for(j in 1:length(lower)) {
      lines(x=c(lower[j],upper[j]),
            y=c(length(lower)-j+1+offset[k], length(lower)-j+1+offset[k]), col=my_col[k])
    }
  }
  
  dim_and_attr <- character()  
  for (i in 1:numb_of_dim) {
    dim_and_attr <- c(dim_and_attr, dim_names[i], dim_attr_list[[i]])
  }
  
  axis(2, at = rev(length(dim_and_attr)+1-rev(which(!dim_and_attr%in%dim_names))),
       labels=dim_and_attr[!dim_and_attr%in%dim_names], las=1)
  axis(2, at = rev(length(dim_and_attr)+1-rev(which(dim_and_attr%in%dim_names))),
       labels=dim_names, las=1, font=2)
  axis(1)
  
  # Add a legend at the bottom
  if(n_comp>1) { 
    if(show_legend){
      if(is.null(legend_text)) {legend_text <- c(unique(cj_mm_object$BY))}
      legend("bottomleft", legend=legend_text, pch=my_pch, col=my_col,
             inset=c(legend_inset), xpd=TRUE, horiz=TRUE, bty="n")
    }
  }
}  