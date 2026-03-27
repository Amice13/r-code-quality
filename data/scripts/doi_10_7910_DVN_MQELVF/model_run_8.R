 setwd(pathData)
 load("unGridAll.rda")


#Model Frame

model.data <- model.frame(un.yes~nlights_max+nlights_mean+nlights_calib_mean_orig+nlights_calib_mean.l1_orig+duringUN+afterUN+country_name+year,data=un.data.final[un.data.final$year<=2012 & un.data.final$year>1994,])
	model.data <- model.data %>%ungroup()


