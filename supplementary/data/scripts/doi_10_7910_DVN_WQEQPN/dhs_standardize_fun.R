standardize_dhs <- function(dhs.subset=NULL){
  
  
  # drop previously defined standardize variables
  out.df <- dhs.subset[,-which(names(dhs.subset)%in%yvars_std)]

  
  # code standardized exogamy dummies
  yvars<- paste0("exogamy_l",c(1:11,15,16)) 
  
  exogamy_std <- lapply(yvars,function(y){
    out <- scale(out.df[,y])
  })
  
  exogamy_std_df <- data.frame(do.call(cbind,exogamy_std))
  names(exogamy_std_df) <- paste0(yvars,"_std")
  out.df <- cbind(out.df,exogamy_std_df)
  
  # code standardized predictors
  out.df$hance_crops5_sum_15km_std <- scale(out.df$hance_crops5_sum_15km)[,1]
  
  
  out.df$pubspc.23_poly_std <- scale(out.df$pubspc.23_poly)[,1]
  out.df$pubspc.soas_poly_std <- scale(out.df$pubspc.soas_poly)[,1]
  
  
  out.df$hance_crops5_sqkm_ethn_std.m <- scale(out.df$hance_crops5_sqkm_ethn_std.m)[,1]
  out.df$hance_crops5_sqkm_ethn_std <- scale(out.df$hance_crops5_sqkm_ethn_std)[,1]
  
  
  out.df$pubspc.23_ethn_std.m <- scale(out.df$pubspc.23_ethn_std.m)[,1]
  out.df$pubspc.23_ethn_std <- scale(out.df$pubspc.23_ethn_std)[,1]
  
  
  # output
  return(out.df)
}

