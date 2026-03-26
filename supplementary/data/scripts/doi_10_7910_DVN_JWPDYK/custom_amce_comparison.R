custom_amce_comparison <- function(amce_attributes_list, amce_estimates_list, df_with_dim_names_and_order=NULL, offset = 0.15, legend_text = "", legend_inset = c(-0.35,.4), xlim=c(-0.2,0.1), plot_title="", my_col=NULL, increase_ylim=0) {
  numb_of_dim <- length(amce_attributes_list[[1]])
  numb_of_attributes_total <- length(unlist(amce_attributes_list[[1]]))
  
  n_comp <- length(amce_attributes_list)
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
       type="n", axes=F, ylab="", xlab="Estimated AMCE", main=plot_title)
  abline(v=0, col="grey70")
  
  for(k in 1:length(amce_attributes_list)) {
    if(!is.null(df_with_dim_names_and_order)){
      amce_attributes_unordered <- amce_attributes_list[[k]]
      amce_attributes <- amce_attributes_unordered[order(match(names(amce_attributes_unordered),
                                                               df_with_dim_names_and_order$dim_names))]
    } else {
      amce_attributes <- amce_attributes_list[[k]]
    }
    
    amce_estimates <- amce_estimates_list[[k]]
    if(!is.null(df_with_dim_names_and_order)){
      amce_estimates <- amce_estimates[order(match(names(amce_attributes_unordered),
                                                   df_with_dim_names_and_order$dim_names))]
    }
    
    dim_and_attr <- character()  
    estim <- numeric()
    std_err <- numeric()
    for (i in 1:numb_of_dim) {
      dim_and_attr <- c(dim_and_attr, names(amce_attributes)[i], amce_attributes[[i]])
      estim <- c(estim, NA, 0, amce_estimates[[i]][1,])
      std_err <- c(std_err, NA, 0, amce_estimates[[i]][2,])
    }  
    
    points(estim, length(dim_and_attr):1+offset[k], pch=my_pch[k], col = my_col[k]) #my_col_vec)
    
    for(j in 1:length(std_err)) {
      lines(x=c(estim[j]-std_err[j]*1.96,estim[j]+std_err[j]*1.96),
            y=c(length(std_err)-j+1+offset[k], length(std_err)-j+1+offset[k]), col=my_col[k])
    }
  }
  
  axis(2, at = rev(length(dim_and_attr)+1-rev(which(!dim_and_attr%in%names(amce_attributes)))),
       labels=dim_and_attr[!dim_and_attr%in%names(amce_attributes)], las=1)
  axis(2, at = rev(length(dim_and_attr)+1-rev(which(dim_and_attr%in%names(amce_attributes)))),
       labels=names(amce_attributes), las=1, font=2)
  axis(1, line=1)
  
  # Add a legend at the bottom
  if(length(legend_text)>1) {
    legend("bottomleft", legend_text, pch=my_pch, col=my_col,
           inset=c(legend_inset), xpd=TRUE, horiz=TRUE, bty="n")
  }
}