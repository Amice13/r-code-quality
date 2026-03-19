custom_amce_plot <- function(amce_obj, df_with_dim_names_and_order, show_cat_in_plot=TRUE, add_legend=TRUE, plot_title="", legend_inset = -0.22, legend_inset_x=0, show_yaxis=T, xlim=c(-0.3,0.11), col_override = "", alpha=0.05, bonf_corr=FALSE) {
  numb_of_dim <- length(amce_obj$attributes)
  numb_of_attributes_total <- length(unlist(amce_obj$attributes))
  
  amce_attributes <- amce_obj$attributes[order(match(names(amce_obj$attributes),df_with_dim_names_and_order$dim_names))]
  amce_estimates <- amce_obj$estimates[order(match(names(amce_obj$attributes),df_with_dim_names_and_order$dim_names))]
  
  my_col <- RColorBrewer::brewer.pal(length(unique(df_with_dim_names_and_order$ord2)), "Set2")
  if(!col_override=="") {my_col <- rep(col_override, length(unique(df_with_dim_names_and_order$ord2)))}
  df_with_dim_names_and_order$ord2 <- as.factor(df_with_dim_names_and_order$ord2)
  levels(df_with_dim_names_and_order$ord2) <- sort(unique(df_with_dim_names_and_order$ord2))
  my_col_vec <- character()
  
  my_pch <- c(16, 17, 15)
  if(!col_override=="") {my_pch <- rep(16, length(my_pch))}
  my_pch_vec <- numeric()
  
  vector_order1 <- character()
  dim_and_attr <- character()  
  estim <- numeric()
  std_err <- numeric()
  for (i in 1:numb_of_dim) {
    dim_and_attr <- c(dim_and_attr, names(amce_attributes)[i], amce_attributes[[i]])
    estim <- c(estim, NA, 0, amce_estimates[[i]][1,])
    std_err <- c(std_err, NA, 0, amce_estimates[[i]][2,])
    my_col_vec <- c(my_col_vec, NA, rep(my_col[df_with_dim_names_and_order$ord2[i]], length(amce_estimates[[i]][1,])+1))
    my_pch_vec <- c(my_pch_vec, NA, rep(my_pch[df_with_dim_names_and_order$ord2[i]], length(amce_estimates[[i]][1,])+1))
    vector_order1 <- c(vector_order1, NA, rep(df_with_dim_names_and_order$ord1[i], length(amce_estimates[[i]][1,])+1))
  }
  
  if(show_yaxis) {
    par(mar=c(6,14,2,1))
  } else {
    par(mar=c(6,0,2,1))
  }
  plot(-2,-2, xlim=xlim, ylim= c(1,numb_of_dim+numb_of_attributes_total), type="n", axes=F, ylab="", xlab="Estimated AMCE", main = plot_title)
  abline(v=0, col="grey70")
  points(estim, length(dim_and_attr):1, pch=my_pch_vec, col = my_col_vec)
  #axis(2, at = length(dim_and_attr):1, labels=dim_and_attr, las=1)
  if(show_yaxis) {
    axis(2, at = rev(length(dim_and_attr)+1-rev(which(!dim_and_attr%in%names(amce_attributes)))), labels=dim_and_attr[!dim_and_attr%in%names(amce_attributes)], las=1)
    axis(2, at = rev(length(dim_and_attr)+1-rev(which(dim_and_attr%in%names(amce_attributes)))), labels=names(amce_attributes), las=1, font=2)
  }
  axis(1, at=c(-0.3,-0.2,-0.1,0,0.1), line=1)
  
  for(i in 1:length(std_err)) {
    lines(x=c(estim[i]-std_err[i]*qnorm(1-alpha/2), estim[i]+std_err[i]*qnorm(1-alpha/2)), y=c(length(std_err)-i+1, length(std_err)-i+1), col=my_col_vec[i])
  }
  
  # Add estimates with bonferroni corrected confidence intervals
  if(bonf_corr) {
    test_num <- sum(!is.na(std_err))
    for(i in 1:length(std_err)) {
      lines(x=c(estim[i]-std_err[i]*qnorm(1-alpha/(2*test_num)), estim[i]+std_err[i]*qnorm(1-alpha/(2*test_num))), 
            y=c(length(std_err)-i+1-0.3, length(std_err)-i+1-0.3), col=my_col_vec[i])
    }
    points(estim, (length(dim_and_attr):1)-0.3, pch=1, col = my_col_vec)
    legend("bottomleft", legend = c("No corr.", paste0("Bonf. corr. (Number of tests = ", test_num, ")")), pch=c(my_pch[1], 1), col=my_col[1], 
           inset=c(legend_inset_x,legend_inset), xpd=TRUE, horiz=TRUE, bty="n",
           text.width=c(.1,.1)
    )
    add_legend <- FALSE
  }
  
  if(show_cat_in_plot==TRUE){
    # Addition of order 1 labels
    for ( i in 2:length(unique(df_with_dim_names_and_order$ord1))) {
      abline(h=numb_of_dim+numb_of_attributes_total-which(vector_order1==unique(df_with_dim_names_and_order$ord1)[i])[1]+2.5, lty="dashed")
      text(x=-0.3, y=numb_of_dim+numb_of_attributes_total+1-which(vector_order1==unique(df_with_dim_names_and_order$ord1)[i])[length(which(vector_order1==unique(df_with_dim_names_and_order$ord1)[i]))],
           labels=unique(df_with_dim_names_and_order$ord1)[i], pos=4, srt=90)
    }
  }
  
  # Add a legend at the bottom
  if(add_legend==TRUE) {
    legend("bottom", c(levels(df_with_dim_names_and_order$ord2)), pch=my_pch, col=my_col,
           inset=c(legend_inset_x,legend_inset), xpd=TRUE, horiz=TRUE, bty="n"
    )
  }
}