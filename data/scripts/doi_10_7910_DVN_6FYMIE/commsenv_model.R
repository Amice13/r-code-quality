## Linear Model Fit and Validation
## Ian A Smith (iasmith@bu.edu)

library(grDevices) # For validation figure legend
library(viridis) # For visualization
#################
##  Functions  ##
#################

rmse <- function(error) { sqrt(mean(error^2)) }

plot_colorByDensity = function(x1,x2,
                               ylim=c(min(x2),max(x2)),
                               xlim=c(min(x1),max(x1)),
                               xlab="",ylab="",main="", yaxt = 'n') {
     
    df <- data.frame(x1,x2)
    x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
    df$dens <- col2rgb(x)[1,] + 1L
    cols <-  rev(magma(n = 255))
    df$col <- cols[df$dens]
    plot(x2~x1, data=df[order(df$dens),], 
         ylim=ylim,xlim=xlim,pch=16,col=col,
         cex=0.755,xlab=xlab,ylab=ylab,
         main=main, cex.axis=1.5,cex.lab=1.5, yaxt = 'n')
}

###############################
##  IMPORT DATA & FIT MODEL  ##
###############################

## Import data
station_data <- read.csv('/projectnb/buultra/iasmith/heat_model/manuscript/Comms_EandE/weather_station_data.csv')

## Fit model to MADIS station data
fit <- lm(temp_obs_C ~ tree_fraction + albedo + wind_m_s + wtr_dist_m + solar_w_m2 + max_temp_daymet_C + hod + I(hod^2), data = station_data[station_data$source == 'madis',])

## View model fit & coefficients
summary(fit)

########################
##  MODEL VALIDATION  ##
########################

## Predict weather underground (wu) station temperature from MADIS station linear model
preds_wu <- predict(fit, station_data[station_data$source == 'wu',])

## Add predictions to dataframe
station_data$prediction <- NA
station_data[station_data$source == 'wu','prediction'] <- preds_wu

## Fit linear model to compare observed vs prediction wu station temp
fit_validation <- lm(temp_obs_C ~ prediction, data = station_data)

## Summarize validation model fit
summary(fit_validation)

## Legend for validation plot
density_legend <- as.raster(matrix(magma(200), ncol=1))

## Validation plot
par(mar = c(5,5,5,5), pty = 's', bg = 'white')
plot_colorByDensity(station_data$prediction, station_data$temp_obs_C, xlim = c(0,40), ylim = c(0,40), ylab = expression(paste('Observed Air Temperature (',degree,'C)')), xlab = expression(paste('Predicted Air Temperature (',degree,'C)')), main = 'JJA: 14:00 - 16:00 (2021 - 2022)')
axis(side = 2, at = seq(0, 40, 10), las = 2, cex.axis = 1.5)
rasterImage(density_legend, 1.7, 17, 6.7, 27)
text(4.2,29.5,'Point\nDensity', font = 2)
text(8.7, 26.25, 'High')
text(8.7, 17.75, 'Low')
abline(0,1,col = 'slategrey', lty = 3, lwd = 2)
legend('topleft', c('Regression Line', '1:1 line'), col = c('red', 'slategrey'), lty = c(1,3), bty = 'n', cex = 1.25)
legend('bottomright', c(paste('Slope:',round(coef(fit_validation)[2],2)),paste('Intercept:',round(coef(fit_validation)[1],2)),paste('R-Squared:',round(summary(fit_validation)$r.square,2)),paste('RMSE:',round(rmse(fit_validation $residuals),2),'°C')), bty = 'n', text.font = 3, cex = 1.25)
clip(10.55557, 39.68833, 10.55557, 39.68833)
abline(lm(fit_validation), col = 'red', lwd = 4)
