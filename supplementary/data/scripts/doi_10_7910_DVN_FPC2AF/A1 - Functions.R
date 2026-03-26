## Copyright 2016, 2025 Elizabeth Rigby
##
## This program is free software: you can redistribute it and/or modify it 
## under the terms of the GNU General Public License as published by the Free 
## Software Foundation, either version 3 of the License, or (at your option) 
## any later version.
##
## This program is distributed in the hope that it will be useful, but WITHOUT 
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
## more details.
##
## A copy of the GNU General Public License is stored in the COPYING.txt file 
## along with this program. See also <https://www.gnu.org/licenses/>.
##
###############################################################################

## Functions for simulation of detection of birds during point count surveys

## load necessary packages
library(MASS)

#########################
## Distributions
#########################

## Alternate parameterization of Beta Distribution 
## parameters = mean (mu) and concentration parameter (theta) instead of a,b
## See Link and Barker (2010) p. 319
rbetaAlt <- function(n, MEAN, theta){
  a <- MEAN * theta        
  b <- theta - MEAN * theta  
  rbeta(n, a, b)
}

## Alternate parameterization of Beta Distribution for Vectorized inputs
## parameters = mean (mu) and concentration parameter (theta) instead of a,b
## See Link and Barker (2010) p. 319
rbetaAlt2 <- function(MEAN, theta){
  if(length(MEAN) == length(theta)){
    a <- MEAN * theta        
    b <- theta - MEAN * theta  
    return(rbeta(length(MEAN), a, b))
  } else {
    return("rbetaAlt2 error: Mean and theta vectors are of unequal length")}
}

## Alternate parameterization of LogNormal distribution: input N, arithmetic 
##  mean and arithmetic SD of desired final distribution.
rlnormAlt <- function(N, DesiredMean, DesiredSD){
  mu <- log(DesiredMean) - 0.5 * log(1 + (DesiredSD^2) / (DesiredMean^2))
  sigma <- sqrt(log(1 + (DesiredSD^2) / (DesiredMean^2)))
  rlnorm(N, mu, sigma)
}

#########################
## Spatial
#########################

## PolarSolve is a function to add 2 (r,theta) vectors 
##  Add 2 vectors: a distance and angle from origin + a movement (distance and 
##  angle from 1st position)
##      Vector 1 = Distance and Angle (degrees) of bird from observer at time i
##      Vector 2 = Distance and Angle (degrees) of bird's position at time i+1 
##                 from position at time i

PolarSolve<- function(r1, theta1, rmove, thetamove){
  radtheta1 <- theta1*(2*pi/360)
  radthetamove <- thetamove*(2*pi/360)
  x1 <- r1*cos(radtheta1)
  y1 <- r1*sin(radtheta1)
  xmove <- rmove*cos(radthetamove)
  ymove <- rmove*sin(radthetamove)
  x2 <- x1+xmove
  y2 <- y1+ymove
  r2 <- sqrt(x2^2+y2^2)
  radtheta2 <- atan(y2/x2) 
  ## Note: atan alone moves everything to right half of unit circle 
  
  if(x2<0) {theta2 <- (radtheta2+pi)*(360/(2*pi))}
  if(x2>=0) {theta2 <- (radtheta2)*(360/(2*pi))}
  PolarSolveResults <- c(r2, theta2)
  return(PolarSolveResults)
  ## PolarSolve(r1, theta1, rmove,thetamove)[1] = New Distance of bird from Obs 
  ## PolarSolve(r1, theta1, rmove,thetamove)[2] = New Angle of bird from Obs
}  

## Calculate the distance between an observer at the origin and a bird with 
##  location (x,y).
ObsDistance <- function(x,y){
  Distance.yijrk <- sqrt(x^2+y^2)  ##radius
  BirdAnglePolar.yijrk <-atan2(y,x)    ##polar coordinates angle
  if(BirdAnglePolar.yijrk<0){ BirdAnglePolar.yijrk <- BirdAnglePolar.yijrk + 2*pi}
  ## BirdAngleCompass.yijrk = Compass rose angle of bird in relation to obs
  BirdAngleCompass.yijrk <- (-1*BirdAnglePolar.yijrk + (pi/2))  
  if(BirdAngleCompass.yijrk<0){ BirdAngleCompass.yijrk <- BirdAngleCompass.yijrk + 2*pi}
  return(list("Distance.yijrk" = Distance.yijrk,
              "BirdAnglePolar.yijrk" = BirdAnglePolar.yijrk,
              "BirdAngleCompass.yijrk" = BirdAngleCompass.yijrk))
}

## Calculate the distance between the center of a single GenSpatialParameters
## object and the centers of a list of GenSpatialParameters objects.
DistanceCenters <- function(SpatialCandidate, SpatialList){
  xc <- SpatialCandidate$CenterX.yij
  yc <- SpatialCandidate$CenterY.yij
  
  xs <- unname(unlist(SpatialList)[names(unlist(SpatialList)) == "CenterX.yij"])
  ys <- unname(unlist(SpatialList)[names(unlist(SpatialList)) == "CenterY.yij"])
  
  Distances <- sqrt((xc-xs)^2 + (yc-ys)^2)
  return(Distances)  
}

## Rotate X and Y coordinates around THE ORIGIN by an angle, theta.
##  Input angle as degrees.
RotatePoints <- function(X1, Y1, theta){
  radtheta <- theta*(2*pi/360)
  Xnew <- X1*cos(radtheta) - Y1*sin(radtheta)
  Ynew <- X1*sin(radtheta) + Y1*cos(radtheta)
  RotateResults <- cbind(Xnew, Ynew)
  return(RotateResults)  
  #RotatePoints(X2, Y2, Theta)[,1] #will return column of x coordinates
  #RotatePoints(X2, Y2, Theta)[,2] #will return column of y coordinates
}

## Generate parameters needed to generate Bivariate normal bird territories.
##  Input: Area (mean & SD) of territory and PercentUD.yij (the percent of 
##       locations contained by a HR that large)
##  Output: CenterXyij, CenterY.yij, Theta.yij, SDy.yij, SDx.yij, Area.yij, 
##        a.yij, b.yij, Ecc.yij
GenSpatialParameters <- function(
  HRAreamean, ## mean territory/HR size
  HRAreaSD,   ## SD territory/HR size
  PercentUD.yij = 0.95,  ## Percent of locations contained by a territory of mean size
  ylim = c(-1000,1000),  ## meters wide of area modeled
  xlim = c(-1000,1000),  ## meters tall of area modeled
  OverlapUD = 0.8){  ## Percent Utilization ellipse used for overlap comparisons
  CenterX.yij <- runif(1, xlim[1], xlim[2])  ##x-coordinate for center of ellipse
  CenterY.yij <- runif(1, ylim[1], ylim[2])  ##y-coordinate for center of ellipse
  Theta.yij <- runif(1, 0, 360)   ##Angle at which ellipse will be rotated
  
  ##Area of a 95% Home Range Ellipse (Jennrich and Turner 1969, eq. 13):
  ## Area = 6*pi*sqrt(determinent of var/covar matrix)
  ##      = 6*pi*SDx*SDy)  (assumes covariance = 0)
  Area.yij <- rlnormAlt(1, HRAreamean, HRAreaSD) 
  ## Area.yij = Area of territory/HR for bird j in survey site i
  ## Area.yij generated from a lognormal distribution to prevent negative areas (nonsensical)
  
  #Ecc.yij <- rbetaAlt(1, 0.8, 20)  ## Determine desired eccentricity of HR ij with beta distribution
  Ecc.yij <- runif(1, 0, 0.95) ## Determine desired eccentricity of HR ij with uniform distribution
  a.yij <- ((Area.yij^2)/((1-Ecc.yij^2)*pi^2))^(1/4) ## intercept on major axis of similar ellipse centered at origin
  b.yij <- Area.yij/(pi*a.yij)                     ## intercept on minor axis of similar ellipse centered at origin
  C <- sqrt(log((1-PercentUD.yij)^-2))  ## (C^2)*pi*sqrt(det(Sigma)) = Area of ellipse 
  
  SDx.yij <- sqrt((a.yij*Area.yij)/(C^2*pi*b.yij)) ##for 95% UD, C^2=6
  SDy.yij <- b.yij*SDx.yij/a.yij
  
  radtheta.yij <- Theta.yij*(2*pi/360)
  Xmeanrotated <- CenterX.yij*cos(radtheta.yij) - CenterY.yij*sin(radtheta.yij)
  Ymeanrotated <- CenterX.yij*sin(radtheta.yij) + CenterY.yij*cos(radtheta.yij)
  Xdif.yij <- CenterX.yij - Xmeanrotated
  Ydif.yij <- CenterY.yij - Ymeanrotated
  Sigma <- matrix(c(SDx.yij^2, 0,0,SDy.yij^2),2,2)
  eSigma <- eigen(Sigma)
  U <- c(CenterX.yij, CenterY.yij)
  
  ##Unrotated Axes
  UnRMajorAxis1 <- U + C*(sqrt(eSigma$values[1])*eSigma$vectors[,1])
  UnRMajorAxis2 <- U - C*(sqrt(eSigma$values[1])*eSigma$vectors[,1])
  UnRMinorAxis1 <- U + C*(sqrt(eSigma$values[2])*eSigma$vectors[,2])
  UnRMinorAxis2 <- U - C*(sqrt(eSigma$values[2])*eSigma$vectors[,2])
  
  ##Rotated, unshifted Axes
  #RotatePoints(UnRMajorAxis1[1], UnRMajorAxis1[2], Theta.yij)
  
  RMajorAxis1 <- RotatePoints(UnRMajorAxis1[1], UnRMajorAxis1[2], Theta.yij)
  RMajorAxis2 <- RotatePoints(UnRMajorAxis2[1], UnRMajorAxis2[2], Theta.yij)
  RMinorAxis1 <- RotatePoints(UnRMinorAxis1[1], UnRMinorAxis1[2], Theta.yij)
  RMinorAxis2 <- RotatePoints(UnRMinorAxis2[1], UnRMinorAxis2[2], Theta.yij)
  
  ##Rotated, shifted Axes
  RSMajorAxis1.yij <- RMajorAxis1 + c(Xdif.yij,Ydif.yij)
  RSMajorAxis2.yij <- RMajorAxis2 + c(Xdif.yij,Ydif.yij) 
  RSMinorAxis1.yij <- RMinorAxis1 + c(Xdif.yij,Ydif.yij)
  RSMinorAxis2.yij <- RMinorAxis2 + c(Xdif.yij,Ydif.yij)
  
  #####
  ##parameters for OverlapEllipse % UD ellipse, all other parameters same - useful for overlap calculation
  ## NOTE: OE stands for "Overlap Ellipse" in all variable names
  AreaOE.yij <- Area.yij*((log((1-OverlapUD)^-2))/(log((1-PercentUD.yij)^-2))) ##Area of an OverlapEllipse% UD, other parameters same
  aOE.yij <- ((AreaOE.yij^2)/((1-Ecc.yij^2)*pi^2))^(1/4)  ##intercept on major axis of OverlapEllipse% UD ellipse centered at origin
  bOE.yij <- AreaOE.yij/(pi*aOE.yij) ##intercept on minor axis of OE% UD ellipse centered at origin
  COE <- sqrt(log((1-OverlapUD)^-2)) ## constant C for an OverlapEllipse% UD ellipse
  
  SDOEx.yij <- sqrt((aOE.yij*AreaOE.yij)/(COE^2*pi*bOE.yij)) ##for 95% UD, C^2=6
  SDOEy.yij <- bOE.yij*SDOEx.yij/aOE.yij
  SigmaOE <- matrix(c(SDOEx.yij^2, 0, 0, SDOEy.yij^2), 2, 2)
  eSigmaOE <- eigen(SigmaOE)
  
  ##Unrotated Axes - OverlapEllipse% UD ellipse
  UnROEMajorAxis1 <- U + COE*(sqrt(eSigmaOE$values[1])*eSigmaOE$vectors[,1])
  UnROEMajorAxis2 <- U - COE*(sqrt(eSigmaOE$values[1])*eSigmaOE$vectors[,1])
  UnROEMinorAxis1 <- U + COE*(sqrt(eSigmaOE$values[2])*eSigmaOE$vectors[,2])
  UnROEMinorAxis2 <- U - COE*(sqrt(eSigmaOE$values[2])*eSigmaOE$vectors[,2])
  
  ##Rotated, unshifted Axes - OverlapEllipse% UD ellipse
  #RotatePoints(UnROverlapEllipseMajorAxis1[1], UnROverlapEllipseMajorAxis1[2], Theta.yij)
  
  ROEMajorAxis1 <- RotatePoints(UnROEMajorAxis1[1], UnROEMajorAxis1[2], Theta.yij)
  ROEMajorAxis2 <- RotatePoints(UnROEMajorAxis2[1], UnROEMajorAxis2[2], Theta.yij)
  ROEMinorAxis1 <- RotatePoints(UnROEMinorAxis1[1], UnROEMinorAxis1[2], Theta.yij)
  ROEMinorAxis2 <- RotatePoints(UnROEMinorAxis2[1], UnROEMinorAxis2[2], Theta.yij)
  
  ##Rotated, shifted Axes - OverlapEllipse% UD ellipse
  RSOEMajorAxis1.yij <- ROEMajorAxis1 + c(Xdif.yij,Ydif.yij)
  RSOEMajorAxis2.yij <- ROEMajorAxis2 + c(Xdif.yij,Ydif.yij) 
  RSOEMinorAxis1.yij <- ROEMinorAxis1 + c(Xdif.yij,Ydif.yij)
  RSOEMinorAxis2.yij <- ROEMinorAxis2 + c(Xdif.yij,Ydif.yij)
  
  ##Verify with plot that the rotation is correct
  #   par(mfrow=c(1,3))
  #   plot(c(CenterXij,UnRMajorAxis1[1],UnRMajorAxis2[1],UnRMinorAxis1[1],UnRMinorAxis2[1]),
  #        c(CenterYij,UnRMajorAxis1[2],UnRMajorAxis2[2],UnRMinorAxis1[2],UnRMinorAxis2[2]), pch=16, xlim=c(-15,15), ylim=c(-10,20))
  #  
  #   plot(c(CenterXij,RMajorAxis1[1],RMajorAxis2[1],RMinorAxis1[1],RMinorAxis2[1]),
  #        c(CenterYij,RMajorAxis1[2],RMajorAxis2[2],RMinorAxis1[2],RMinorAxis2[2]), pch=16, xlim=c(-15,15), ylim=c(-10,20))
  #   
  #   plot(c(CenterXij,RSMajorAxis1[1],RSMajorAxis2[1],RSMinorAxis1[1],RSMinorAxis2[1]),
  #        c(CenterYij,RSMajorAxis1[2],RSMajorAxis2[2],RSMinorAxis1[2],RSMinorAxis2[2]), pch=16, xlim=c(-15,15), ylim=c(-10,20))
  #   
  return(
    list(
      "CenterX.yij" = CenterX.yij, 
      "CenterY.yij" = CenterY.yij,
      "Area.yij" = Area.yij,
      "Theta.yij" = Theta.yij,
      "a.yij" = a.yij,
      "b.yij" = b.yij,
      "SDx.yij" = SDx.yij,
      "SDy.yij" = SDy.yij,
      "Ecc.yij" = Ecc.yij,
      "PercentUD.yij" = PercentUD.yij,
      "Xdif.yij" = Xdif.yij,
      "Ydif.yij" = Ydif.yij,
      "RSMajorAxis1.yij" = RSMajorAxis1.yij,
      "RSMajorAxis2.yij" = RSMajorAxis2.yij,
      "RSMinorAxis1.yij" = RSMinorAxis1.yij,
      "RSMinorAxis2.yij" = RSMinorAxis2.yij,
      "RSOEMajorAxis1.yij" = RSOEMajorAxis1.yij,
      "RSOEMajorAxis2.yij" = RSOEMajorAxis2.yij,
      "RSOEMinorAxis1.yij" = RSOEMinorAxis1.yij,
      "RSOEMinorAxis2.yij" = RSOEMinorAxis2.yij,
      "AreaOE.yij" = AreaOE.yij,
      "aOE.yij" = aOE.yij,
      "bOE.yij" = bOE.yij,
      "SDOEx.yij" = SDOEx.yij,
      "SDOEy.yij" = SDOEy.yij
    ))
}

## Generate locations for a bird with territory parameters produced by GenSpatialParameters().
GenLocations <- function(NPoints, EllipseParm){ 
  ##EllipseParm includes CenterX.yij,CenterY.yij,Theta.yij,SDx.yij,SDy.yij)
  require(MASS)
  EllipseParm <- unlist(EllipseParm)
  CenterX.yij <- EllipseParm["CenterX.yij"]
  CenterY.yij <- EllipseParm["CenterY.yij"]
  Theta.yij <- EllipseParm["Theta.yij"]
  SDx.yij <- EllipseParm["SDx.yij"]
  SDy.yij <- EllipseParm["SDy.yij"]
  
  ##Generate Bivariate normal points for ellipse
  Sigma.yij <- matrix(c(SDx.yij^2, 0, 0, SDy.yij^2), 2, 2) #Covariance matrix
  UnrotatedLocations.yijrk <- mvrnorm(NPoints, 
                                      c(CenterX.yij, CenterY.yij),
                                      Sigma.yij)
  if(NPoints == 1){UnrotatedLocations.yijrk <- matrix(UnrotatedLocations.yijrk, 1, 2)}
  dimnames(UnrotatedLocations.yijrk) <- list(NULL, c("X.yijrk", "Y.yijrk"))
  #return(UnrotatedLocations.yijrk)
  
  ##Rotate Points 
  RotatedLocations.yijrk <- RotatePoints(UnrotatedLocations.yijrk[,"X.yijrk"],
                                         UnrotatedLocations.yijrk[,"Y.yijrk"], 
                                         Theta.yij) 
  #return(RotatedLocations.yijrk)
  
  ##Shift points back in place
  radtheta <- Theta.yij*(2*pi/360)
  Xmeanrotated <- CenterX.yij*cos(radtheta) - CenterY.yij*sin(radtheta)
  Ymeanrotated <- CenterX.yij*sin(radtheta) + CenterY.yij*cos(radtheta)
  Xdif.yij <- CenterX.yij - Xmeanrotated
  Ydif.yij <- CenterY.yij - Ymeanrotated
  RotatedAndShiftedLocations.yijrk <- cbind("XRS" = (RotatedLocations.yijrk[,"Xnew"] + Xdif.yij),
                                            "YRS" = (RotatedLocations.yijrk[,"Ynew"] + Ydif.yij))
  
  ##Plot unrotated, rotated, and rotated&shifted points
  # par(mfrow=c(1,3))
  # plot(UnrotatedLocationsijk, ylim=c(-20,20), xlim=c(-20,20))
  # lines(CenterXij, CenterYij, col="red", pch=16, type="p")
  # plot(RotatedLocationsijk, ylim=c(-20,20), xlim=c(-20,20))
  # lines(CenterXij, CenterYij, col="red", pch=16, type="p")
  # plot(RotatedAndShiftedLocationsijk, ylim=c(-20,20), xlim=c(-20,20))
  # lines(CenterXij, CenterYij, col="red", pch=16, type="p")
  
  ##revise dimnames for NPoints=1 to avoid irritating false row name
  if(NPoints==1) {
    RSLdimnames <- dimnames(RotatedAndShiftedLocations.yijrk)
    dimnames(RotatedAndShiftedLocations.yijrk) <- list(NULL,RSLdimnames[[2]])}
  return(RotatedAndShiftedLocations.yijrk)
}

## Generate locations of ellipses for 2 stages of GenLocations() (Before, Rotated, Rotated&Shifted)
GenEllipseAll <- function(EllipseParm){ 
  ## EllipseParm includes CenterXij, CenterYij, aij, bij, Thetaij)
  EllipseParm <- unlist(EllipseParm)
  CenterX.yij <- EllipseParm["CenterX.yij"]
  CenterY.yij <- EllipseParm["CenterY.yij"]
  a.yij <- EllipseParm["a.yij"]
  b.yij <- EllipseParm["b.yij"]
  Theta.yij <- EllipseParm["Theta.yij"]
  
  EllipseXs <- (CenterX.yij-a.yij):(CenterX.yij+a.yij) ##Range of x-coordinates for plotting Ellipse
  OriginXs <- -a.yij:a.yij                        ##Range of x-coordinates if ellipse were plotted at origin instead of centerpoint
  OriginYs <- sqrt((b.yij^2)*abs(1-(OriginXs^2/a.yij^2)))  ##y-coordinates if ellipse were plotted at origin instead of centerpoint
  
  radtheta <- Theta.yij*(2*pi/360)
  Xmeanrotated <- CenterX.yij*cos(radtheta) - CenterY.yij*sin(radtheta)
  Ymeanrotated <- CenterX.yij*sin(radtheta) + CenterY.yij*cos(radtheta)
  Xdif.yij <- CenterX.yij - Xmeanrotated
  Ydif.yij <- CenterY.yij - Ymeanrotated
  
  ##Ellipse before rotation
  EllipseBefore1 <- cbind("EllipseBefore1X" = EllipseXs, 
                          "EllipseBefore1Y" = CenterY.yij+OriginYs)
  EllipseBefore2 <- cbind("EllipseBefore2X" = EllipseXs[order(-EllipseXs)], 
                          "EllipseBefore2Y" = CenterY.yij-OriginYs)
  
  ##Ellipse rotated (around origin)
  EllipseR1 <- RotatePoints(EllipseBefore1[,1], EllipseBefore1[,2], Theta.yij)
  EllipseR2 <- RotatePoints(EllipseBefore2[,1], EllipseBefore2[,2], Theta.yij)
  
  ##Ellipse rotated and shifted
  EllipseRS1 <- cbind("EllipseRS1X" = EllipseR1[,1] + Xdif.yij, 
                      "EllipseRS1Y" = EllipseR1[,2] + Ydif.yij)
  EllipseRS2 <- cbind("EllipseRS2X" = EllipseR2[,1] + Xdif.yij, 
                      "EllipseRS2Y" = EllipseR2[,2] + Ydif.yij)
  
  HH <- data.frame(cbind(EllipseBefore1,
                         EllipseBefore2,
                         EllipseR1,
                         EllipseR2,
                         EllipseRS1,
                         EllipseRS2))
  return(HH)
}

## Generate locations of ellipses for plotting ONLY rotated and shifted Ellipses)
GenEllipsePlot <- function(EllipseParm){  
  ## EllipseParm includes CenterXij, CenterYij, aij, bij, Thetaij)
  EllipseParm <- unlist(EllipseParm)
  CenterX.yij <- EllipseParm["CenterX.yij"]
  CenterY.yij <- EllipseParm["CenterY.yij"]
  a.yij <- EllipseParm["a.yij"]
  b.yij <- EllipseParm["b.yij"]
  Theta.yij <- EllipseParm["Theta.yij"]
  
  EllipseXs <- (CenterX.yij-a.yij):(CenterX.yij+a.yij) ##Range of x-coordinates for plotting Ellipse
  OriginXs <- -a.yij:a.yij                        ##Range of x-coordinates if ellipse were plotted at origin instead of centerpoint
  OriginYs <- sqrt((b.yij^2)*abs(1-(OriginXs^2/a.yij^2)))  ##y-coordinates if ellipse were plotted at origin instead of centerpoint
  
  radtheta <- Theta.yij*(2*pi/360)
  Xmeanrotated <- CenterX.yij*cos(radtheta) - CenterY.yij*sin(radtheta)
  Ymeanrotated <- CenterX.yij*sin(radtheta) + CenterY.yij*cos(radtheta)
  Xdif.yij <- CenterX.yij - Xmeanrotated
  Ydif.yij <- CenterY.yij - Ymeanrotated
  
  ##Ellipse before rotation
  EllipseBefore1 <- cbind("EllipseBefore1X" = EllipseXs, 
                          "EllipseBefore1Y" = CenterY.yij + OriginYs)
  EllipseBefore2 <- cbind("EllipseBefore2X" = EllipseXs[order(-EllipseXs)], 
                          "EllipseBefore2Y" = CenterY.yij - OriginYs)
  
  ##Ellipse rotated (around origin)
  EllipseR1 <- RotatePoints(EllipseBefore1[,1], EllipseBefore1[,2], Theta.yij)
  EllipseR2 <- RotatePoints(EllipseBefore2[,1], EllipseBefore2[,2], Theta.yij)
  
  ##Ellipse rotated and shifted
  EllipseRS1 <-cbind("EllipseRS1X" = EllipseR1[,1] + Xdif.yij, 
                     "EllipseRS1Y" = EllipseR1[,2] + Ydif.yij)
  EllipseRS2 <-cbind("EllipseRS2X" = EllipseR2[,1] + Xdif.yij, 
                     "EllipseRS2Y" = EllipseR2[,2] + Ydif.yij)
  
  EllipseRSAll <- rbind(EllipseRS1, EllipseRS2)
  return(EllipseRSAll)
}

## Generate locations of Overlap Ellipse % UD ellipses for plotting ONLY rotated 
##  and shifted Ellipses.
## EllipseParm includes CenterXij, CenterYij, aij, bij, Thetaij
GenOEEllipsePlot <- function(EllipseParm){ 
  EllipseParm <- unlist(EllipseParm)
  CenterX.yij <- EllipseParm["CenterX.yij"]
  CenterY.yij <- EllipseParm["CenterY.yij"]
  aOE.yij <- EllipseParm["aOE.yij"]
  bOE.yij <- EllipseParm["bOE.yij"]
  Theta.yij <- EllipseParm["Theta.yij"]
  
  EllipseXs <- (CenterX.yij - aOE.yij):(CenterX.yij + aOE.yij) ##Range of x-coordinates for plotting Ellipse
  OriginXs <- -aOE.yij:aOE.yij                        ##Range of x-coordinates if ellipse were plotted at origin instead of centerpoint
  OriginYs <- sqrt((bOE.yij^2)*abs(1-(OriginXs^2/aOE.yij^2)))  ##y-coordinates if ellipse were plotted at origin instead of centerpoint
  
  radtheta <- Theta.yij*(2*pi/360)
  Xmeanrotated <- CenterX.yij*cos(radtheta) - CenterY.yij*sin(radtheta)
  Ymeanrotated <- CenterX.yij*sin(radtheta) + CenterY.yij*cos(radtheta)
  Xdif.yij <- CenterX.yij - Xmeanrotated
  Ydif.yij <- CenterY.yij - Ymeanrotated
  
  ##Ellipse before rotation
  EllipseBefore1 <- cbind("EllipseBefore1X" = EllipseXs, 
                          "EllipseBefore1Y" = CenterY.yij + OriginYs)
  EllipseBefore2 <- cbind("EllipseBefore2X" = EllipseXs[order(-EllipseXs)], 
                          "EllipseBefore2Y" = CenterY.yij - OriginYs)
  
  ##Ellipse rotated (around origin)
  EllipseR1 <- RotatePoints(EllipseBefore1[,1], EllipseBefore1[,2], Theta.yij)
  EllipseR2 <- RotatePoints(EllipseBefore2[,1], EllipseBefore2[,2], Theta.yij)
  
  ##Ellipse rotated and shifted
  EllipseRS1 <-cbind("EllipseRS1X" = EllipseR1[,1] + Xdif.yij, 
                     "EllipseRS1Y" = EllipseR1[,2] + Ydif.yij)
  EllipseRS2 <-cbind("EllipseRS2X" = EllipseR2[,1] + Xdif.yij, 
                     "EllipseRS2Y" = EllipseR2[,2] + Ydif.yij)
  
  EllipseRSAll <- rbind(EllipseRS1, EllipseRS2)
  
  #HH <- data.frame(cbind(EllipseBefore1,EllipseBefore2,EllipseR1,EllipseR2,EllipseRS1,EllipseRS2))
  return(EllipseRSAll)
}

##Check if point (x,y) is inside an ellipse.  Returns TRUE / FALSE
XYCheckInsideEllipse <- function(x, y, ExistingEllipseParm){
  ExistingEllipseParm <- unlist(ExistingEllipseParm)
  CenterX.yij <- ExistingEllipseParm["CenterX.yij"]
  CenterY.yij <- ExistingEllipseParm["CenterY.yij"]
  a.yij <- ExistingEllipseParm["a.yij"]
  b.yij <- ExistingEllipseParm["b.yij"]
  Theta.yij <- ExistingEllipseParm["Theta.yij"]
  
  radtheta <- Theta.yij*(2*pi/360)
  A <- (cos(radtheta)^2)/a.yij^2 + (sin(radtheta)^2)/b.yij^2
  B <- 2*cos(radtheta)*sin(radtheta)*(1/a.yij^2 - 1/b.yij^2)
  C <- (sin(radtheta)^2)/a.yij^2 + (cos(radtheta)^2)/b.yij^2
  Verify <- A*x^2+ B*x*y+ C*y^2- (2*A*CenterX.yij + CenterY.yij*B)*x- (2*C*CenterY.yij+B*CenterX.yij)*y+ (A*CenterX.yij^2+B*CenterX.yij*CenterY.yij+C*CenterY.yij^2)
  ifelse(Verify<=1, return(TRUE), return(FALSE))
}


##Check if any of 4 axes points OR center of NewEllipse are inside 1 Existing Ellipse.  
##Returns TRUE / FALSE
AxesCheckInsideEllipse <- function(NewEllipseParm,ExistingEllipseParm){
  Major1 <- unlist(NewEllipseParm[["RSMajorAxis1.yij"]])
  Major2 <- unlist(NewEllipseParm[["RSMajorAxis2.yij"]])
  Minor1 <- unlist(NewEllipseParm[["RSMinorAxis1.yij"]])
  Minor2 <- unlist(NewEllipseParm[["RSMinorAxis2.yij"]])
  CenterA <- c(NewEllipseParm[["CenterX.yij"]], 
               NewEllipseParm[["CenterY.yij"]])
  
  Axes <- (rbind(Major1, Major2, Minor1, Minor2, CenterA))
  
  ExistingEllipseParm <- unlist(ExistingEllipseParm)
  CenterX.yij <- ExistingEllipseParm["CenterX.yij"]
  CenterY.yij <- ExistingEllipseParm["CenterY.yij"]
  a.yij <- ExistingEllipseParm["a.yij"]
  b.yij <- ExistingEllipseParm["b.yij"]
  Theta.yij <- ExistingEllipseParm["Theta.yij"]
  Inside <- rep(NA,nrow(Axes))
  
  for(i in 1:nrow(Axes)){
    x <-Axes[i,1]
    y <-Axes[i,2]
    
    radtheta <- Theta.yij*(2*pi/360)
    A <- (cos(radtheta)^2)/a.yij^2 + (sin(radtheta)^2)/b.yij^2
    B <- 2*cos(radtheta)*sin(radtheta)*(1/a.yij^2 - 1/b.yij^2)
    C <- (sin(radtheta)^2)/a.yij^2 + (cos(radtheta)^2)/b.yij^2
    Verify <- A*x^2 + B*x*y + C*y^2 - 
      (2*A*CenterX.yij + CenterY.yij*B)*x - 
      (2*C*CenterY.yij+B*CenterX.yij)*y + 
      (A*CenterX.yij^2 + B*CenterX.yij*CenterY.yij + C*CenterY.yij^2)
    ifelse(Verify<=1, Inside[i] <-1, Inside[i] <-0)
    ## Above equations are basic ellipse geometry
    ## see also http://www.maa.org/external_archive/joma/Volume8/Kalman/General.html
  }
  ifelse(sum(Inside)==0, return(FALSE), return(TRUE))
}

##Check if any of 4 axes points OR center of NewEllipse are inside multiple Existing Ellipses.  
##Returns TRUE / FALSE
AxesCheckInsideEllipsesVectorized <- function(NewEllipseParm, ExistingEllipseParm){
  ##NewEllipseParm is a GenSpatialParameters generated list
  ##ExistingEllipseParm is a list of GenSpatialParameters lists
  
  ##Create matrix of Existing Ellipse parameters
  ExistingEllipses <- rbind(unlist(ExistingEllipseParm[[1]]))
  if(length(ExistingEllipseParm)>=2){
    for(ii in 2:length(ExistingEllipseParm)){
      ExistingEllipses <- rbind(ExistingEllipses,
                                rbind(unlist(ExistingEllipseParm[[ii]])))
    }
  }
  Major1 <- unlist(NewEllipseParm[["RSMajorAxis1.yij"]])
  Major2 <- unlist(NewEllipseParm[["RSMajorAxis2.yij"]])
  Minor1 <- unlist(NewEllipseParm[["RSMinorAxis1.yij"]])
  Minor2 <- unlist(NewEllipseParm[["RSMinorAxis2.yij"]])
  CenterA <- c(NewEllipseParm[["CenterX.yij"]], NewEllipseParm[["CenterY.yij"]])
  
  Axes <- (rbind(Major1,Major2, Minor1, Minor2, CenterA))
  
  CenterX.yij <- ExistingEllipses[,"CenterX.yij"]
  CenterY.yij <- ExistingEllipses[,"CenterY.yij"]
  a.yij <- ExistingEllipses[,"a.yij"]
  b.yij <- ExistingEllipses[,"b.yij"]
  Theta.yij <- ExistingEllipses[,"Theta.yij"]
  Inside <-list()
  
  for(i in 1:nrow(Axes)){
    x <-Axes[i,1]
    y <-Axes[i,2]
    
    radtheta <- Theta.yij*(2*pi/360)
    A <- (cos(radtheta)^2)/a.yij^2 + (sin(radtheta)^2)/b.yij^2
    B <- 2*cos(radtheta)*sin(radtheta)*(1/a.yij^2 - 1/b.yij^2)
    C <- (sin(radtheta)^2)/a.yij^2 + (cos(radtheta)^2)/b.yij^2
    Verify <- {A*x^2 + B*x*y + C*y^2 - 
        (2*A*CenterX.yij + CenterY.yij*B)*x - 
        (2*C*CenterY.yij + B*CenterX.yij)*y + 
        (A*CenterX.yij^2 + B*CenterX.yij*CenterY.yij + C*CenterY.yij^2)}
    ## if Verify<=1, an axis point from the new ellipse is inside an existing ellipse
    ## Above equations are basic ellipse geometry
    ## see http://www.maa.org/external_archive/joma/Volume8/Kalman/General.html
    Inside[[i]] <-sum(Verify<=1)
  }
  ifelse(sum(unlist(Inside))==0, return(FALSE), return(TRUE))
}

## Check if any of 4 axes points OR center of Overlap Ellipse % UD NewEllipse  
## are inside 1 Existing Ellipse (Overlap Ellipse % UD too).  
## Returns TRUE / FALSE
AxesCheckInsideEllipseOE <- function(NewEllipseParm, ExistingEllipseParm){
  Major1 <- unlist(NewEllipseParm[["RSOEMajorAxis1.yij"]])
  Major2 <- unlist(NewEllipseParm[["RSOEMajorAxis2.yij"]])
  Minor1 <- unlist(NewEllipseParm[["RSOEMinorAxis1.yij"]])
  Minor2 <- unlist(NewEllipseParm[["RSOEMinorAxis2.yij"]])
  CenterA <- c(NewEllipseParm[["CenterX.yij"]], 
               NewEllipseParm[["CenterY.yij"]])
  
  Axes <- (rbind(Major1, Major2, Minor1, Minor2, CenterA))
  
  ExistingEllipseParm <- unlist(ExistingEllipseParm)
  CenterX.yij <- ExistingEllipseParm["CenterX.yij"]
  CenterY.yij <- ExistingEllipseParm["CenterY.yij"]
  a.yij <- ExistingEllipseParm["aOE.yij"]
  b.yij <- ExistingEllipseParm["bOE.yij"]
  Theta.yij <- ExistingEllipseParm["Theta.yij"]
  Inside <- rep(NA,nrow(Axes))
  
  for(i in 1:nrow(Axes)){
    x <-Axes[i,1]
    y <-Axes[i,2]
    
    radtheta <- Theta.yij*(2*pi/360)
    A <- (cos(radtheta)^2)/a.yij^2 + (sin(radtheta)^2)/b.yij^2
    B <- 2*cos(radtheta)*sin(radtheta)*(1/a.yij^2 - 1/b.yij^2)
    C <- (sin(radtheta)^2)/a.yij^2 + (cos(radtheta)^2)/b.yij^2
    Verify <- {A*x^2+ B*x*y+ C*y^2 - 
        (2*A*CenterX.yij + CenterY.yij*B)*x - 
        (2*C*CenterY.yij + B*CenterX.yij)*y + 
        (A*CenterX.yij^2 + B*CenterX.yij*CenterY.yij + C*CenterY.yij^2)}
    ifelse(Verify <= 1, Inside[i] <-1, Inside[i] <-0)
    ## Above equations are basic ellipse geometry
    ## see http://www.maa.org/external_archive/joma/Volume8/Kalman/General.html
  }
  ifelse(sum(Inside)==0, return(FALSE), return(TRUE))
}

## Check if any of 4 axes points OR center of Overlap Ellipse % UD NewEllipse are inside 
## multiple Existing Overlap Ellipse % UD Ellipses.  
## Returns TRUE / FALSE
AxesCheckInsideEllipsesVectorizedOE <- function(NewEllipseParm,ExistingEllipseParm){
  ## NewEllipseParm is a GenSpatialParameters generated list
  ## ExistingEllipseParm is a list of GenSpatialParameters lists
  
  ## Create matrix of Existing Ellipse parameters
  ExistingEllipses <- rbind(unlist(ExistingEllipseParm[[1]]))
  if(length(ExistingEllipseParm)>=2){
    for(ii in 2:length(ExistingEllipseParm)){
      ExistingEllipses <- rbind(ExistingEllipses,
                                rbind(unlist(ExistingEllipseParm[[ii]])))
    }
  }
  Major1 <- unlist(NewEllipseParm[["RSOEMajorAxis1.yij"]])
  Major2 <- unlist(NewEllipseParm[["RSOEMajorAxis2.yij"]])
  Minor1 <- unlist(NewEllipseParm[["RSOEMinorAxis1.yij"]])
  Minor2 <- unlist(NewEllipseParm[["RSOEMinorAxis2.yij"]])
  CenterA <-c(NewEllipseParm[["CenterX.yij"]], 
              NewEllipseParm[["CenterY.yij"]])
  
  Axes <- (rbind(Major1,Major2, Minor1, Minor2, CenterA))
  
  CenterX.yij <- ExistingEllipses[,"CenterX.yij"]
  CenterY.yij <- ExistingEllipses[,"CenterY.yij"]
  a.yij <- ExistingEllipses[,"aOE.yij"]
  b.yij <- ExistingEllipses[,"bOE.yij"]
  Theta.yij <- ExistingEllipses[,"Theta.yij"]
  Inside <- list()
  
  for(i in 1:nrow(Axes)){
    x <- Axes[i,1]
    y <- Axes[i,2]
    
    radtheta <- Theta.yij*(2*pi/360)
    A <- (cos(radtheta)^2)/a.yij^2 + (sin(radtheta)^2)/b.yij^2
    B <- 2*cos(radtheta)*sin(radtheta)*(1/a.yij^2 - 1/b.yij^2)
    C <- (sin(radtheta)^2)/a.yij^2 + (cos(radtheta)^2)/b.yij^2
    Verify <- {A*x^2 + B*x*y + C*y^2 - 
        (2*A*CenterX.yij + CenterY.yij*B)*x - 
        (2*C*CenterY.yij + B*CenterX.yij)*y + 
        (A*CenterX.yij^2 + B*CenterX.yij*CenterY.yij + C*CenterY.yij^2)}
    ## if Verify<=1, an axis point from the new ellipse is inside an existing ellipse
    ## Above equations are basic ellipse geometry
    Inside[[i]] <-sum(Verify <= 1)
  }
  ifelse(sum(unlist(Inside)) == 0, return(FALSE), return(TRUE))
}

## Determine closest axis point to the observer.
## Useful for determining if a territory overlaps survey radius.
## Note: will use axes for PercentUD.yij used to create territory ExistingEllipseParm
## (Default = 95% UD)
AxesClosestObs <- function(ExistingEllipseParm){
  AxesDistances <-c(unname(unlist(ObsDistance(ExistingEllipseParm[["RSMajorAxis1.yij"]][1],
                                              ExistingEllipseParm[["RSMajorAxis1.yij"]][2])["Distance.yijrk"])),
                    unname(unlist(ObsDistance(ExistingEllipseParm[["RSMajorAxis2.yij"]][1],
                                              ExistingEllipseParm[["RSMajorAxis2.yij"]][2])["Distance.yijrk"])),
                    unname(unlist(ObsDistance(ExistingEllipseParm[["RSMinorAxis1.yij"]][1],
                                              ExistingEllipseParm[["RSMinorAxis1.yij"]][2])["Distance.yijrk"])),
                    unname(unlist(ObsDistance(ExistingEllipseParm[["RSMinorAxis2.yij"]][1],
                                              ExistingEllipseParm[["RSMinorAxis2.yij"]][2])["Distance.yijrk"])))
  return(min(AxesDistances))
}


#########################
### Availability
#########################

## Generate event DidBirdSing for a single interval with a Markov Process
AutoCInstant <- function(DidBirdSingLastTime, PSS, PSNS){
  ifelse(DidBirdSingLastTime == 1, 
         X <- rbinom(1, 1, PSS), 
         X <- rbinom(1, 1, PSNS))
  return(X)
}

## Determine steady state vector [q1 q2] for Markov process
##  As t->infinity, Pr(Singing Mode=1)=q1, Pr(Singing Mode=0)=q2
##  Also, q1 = proportion of birds in Singing Mode at any given time
##  PSS= Pr(Singing Mode=1, given that it was 1 at t-1)
##  PSNS= Pr(Singing Mode=1, given that it was 0 at t-1)
MarkovSS <- function(PSS, PSNS){
  pmatrix <- rbind(c(PSS, 1-PSS), 
                   c(PSNS, 1-PSNS))  ## transition matrix
  
  q1 <- (-1*pmatrix[2,1])/(pmatrix[1,1]-pmatrix[2,1]-1)
  q2 <- (-1*pmatrix[1,2])/(pmatrix[2,2]-pmatrix[1,2]-1)
  
  return(c(q1, q2))
}

## Determine transition matrix value P(S|NS), given desired steady state vector & P(S|S)
MarkovTM <- function(q1, PSS){
  q2 <- 1-q1
  PSNS <- (-1*q1*(PSS-1))/q2
  return(PSNS)
}

## Function for finding PSNS via optimization
##  NOT a standalone function
SolvePSNS <- function(Par, PSNS, PrSing, PSS, DataIntervals){
  PSNS <- Par[1]
  qq <- {(PSNS/(1-PSS+PSNS) + (1-(PSNS/(1-PSS+PSNS)))*(1-((1-PSNS)^DataIntervals))) - PrSing}
  return(abs(qq))
}
## Example:
# optimize(SolvePSNS, interval=c(0,0.05), PrSing=0.6, PSS=0.92, DataIntervals=200, maximum=F)
## NOTE: **Starting values** are VERY important here
## For PSS=0.99 and PSS=0.92, all optimal values of P(S|NS) < 0.02
## Suggest using interval=c(0,0.05) for all
## DataIntervals refers to number of intervals in X minutes in 
##  Pr(bird sings at least once in X minutes), NOT NIntervals


## Determine the Pr(sing) within a survey, 
##  given NIntervals, P(S|S), and P(S|NS).
##  2 methods used, both should return equal results.
ZNIntervals1 <- function(
  PSS1 = 0.98, 
  PSNS1 = 0.00264495, 
  NIntervals = 90){
  -1*PSNS1/(PSS1-PSNS1-1) + 
    (1-(-1*PSNS1/(PSS1-PSNS1-1))) * (1-(1-PSNS1)^NIntervals)
}

ZNIntervals2 <- function(
  q1 = 0.1168009, 
  PSNS1 = 0.00264495, 
  NIntervals = 90){
  ## q1 = steady-state proportion of birds in Singing Mode at any given time
  q1 + (1-q1)*(1-(1-PSNS1)^NIntervals)
}
# ZNIntervals1()
# ZNIntervals2()



#########################
###Perceptibility
#########################

## Produce an observer-estimated distance to bird j, given a known 
## true distance between the observer and bird j.
## To output mean distance estimate, use Output="meanonly" (probabilistic answer).
## To output SD for error estimate, use Output="sdonly" (for plotting).
## To stochastically generate distance, use Output="stochastic".
ObserverEstDistance <- function(TrueDistance, 
                                DistanceCategories, 
                                Output="stochastic"){
  if(TrueDistance < 0) stop("ObserverEstDistance() ERROR: Distance input cannot be negative")
  if(TrueDistance < max(DistanceCategories$Distance)){
    Row <- sum(DistanceCategories$Distance <= TrueDistance)
    
    ## Determine mean error for x=TrueDistance assuming a straight line
    ## between points (x1,y1) & (x2,y2)
    x1 <- DistanceCategories[Row, "Distance"]
    x2 <- DistanceCategories[Row+1, "Distance"]
    y1 <- DistanceCategories[Row, "meanerror"]
    y2 <- DistanceCategories[Row+1, "meanerror"]
    
    ## Equation for a straight line between points (x1,y1) & (x2,y2), for point x=TrueDistance
    MeanError <- y1 + ((y2-y1)/(x2-x1))*(TrueDistance-x1)
    
    ## Determine SD error for x=TrueDistance assuming a straight line
    ## between points (x1,y1) & (x2,y2)
    x3 <- DistanceCategories[Row, "Distance"]
    x4 <- DistanceCategories[Row+1, "Distance"]
    y3 <- DistanceCategories[Row, "sderror"]
    y4 <- DistanceCategories[Row+1, "sderror"]
    
    ## Equation for a straight line between points (x1,y1) & (x2,y2), for point x=TrueDistance
    SDError <-y3 + ((y4-y3)/(x4-x3))*(TrueDistance-x3)
  }
  
  ## to extrapolate beyond existing distance data, use values for greatest data point
  if(TrueDistance >= max(DistanceCategories$Distance)){
    MeanError <- DistanceCategories[DistanceCategories$Distance == max(DistanceCategories$Distance), "meanerror"]
    SDError <- DistanceCategories[DistanceCategories$Distance == max(DistanceCategories$Distance), "sderror"]
    
  }
  ## Mean observer-estimated distance for TrueDistance (probabilistic estimate)
  MeanEstDistance <- TrueDistance + MeanError
  
  ## Stochastic observer-estimated distance for TrueDistance
  StoEstDistance <- TrueDistance + rnorm(1, MeanError, SDError)
  
  ## Output the mean distance estimated for point x=TrueDistance
  if(Output == "meanonly") return(MeanEstDistance) ##Output the mean distance estimated for point x=TrueDistance
  if(Output == "stochastic") return(StoEstDistance)   ##Output a stochastic value for distance estimated for point x=TrueDistance
  if(Output == "sdonly") return(SDError)
  
}


#########################
### Estimators & Likelihoods
#########################

## ML Estimator for Multiple observer survey (Nichols 2000)
## This is a closed-form estimator: 
##  it produces estimates without optim() .

MultObs.Nichols <- function(x11, x21, x22, x12){
  ## x11 = seen by obs 1 on stops when obs 1 was primary
  ## x21 = seen by obs 2 on stops when obs 1 was primary
  ## x22 = seen by obs 2 on stops when obs 2 was primary
  ## x12 = seen by obs 1 on stops when obs 2 was primary
  x11 <-sum(x11)
  x22 <-sum(x22)
  x12 <-sum(x12)
  x21 <-sum(x21)
  
  if(x11 == 0 | x22 == 0) print("N & p not calculable: One or more observers had Count=0 when primary observer.")
  
  p1hat <- (x11*x22 - x12*x21)/(x11*x22 + x22*x21)
  p2hat <- (x11*x22 - x12*x21)/(x11*x22 + x11*x12)
  phat <- 1-(x12*x21)/(x22*x11)
  xdotdot <- x11 + x21 + x22 + x12
  Nhat <- xdotdot/phat
  return(list("p1hat" = p1hat, 
              "p2hat" = p2hat, 
              "phat" = phat, 
              "xdotdot" = xdotdot, 
              "Nhat" = Nhat))
}

## Optimizable estimator for Farnsworth Removal method.
## Not a standalone function, designed to estimate parameters via optim() or optimx()
##  3 intervals: 3 min, 2 min, 5 min (10 min total)
FarnsworthRemoval10min.negLL <- function(Param,xes){
  #function(cc,qq,xes){#(Param,xes){ ##for mle2
  cc=Param[1]
  qq=Param[2]
  #     cc <-0.3
  #     qq <-0.7
  
  x1 <-xes[1]
  x2 <-xes[2]
  x3 <-xes[3]
  
  #     xes <-II.data[,c("FarnsRemovalPeriod1.yi",
  #                "FarnsRemovalPeriod2.yi",
  #                "FarnsRemovalPeriod3.yi")]
  
  NLL.A <- log(1-cc*qq^3) - log(1-cc*qq^10)
  NLL.B <- log(cc*qq^3) + log(1-qq^2) - log(1-cc*qq^10)
  NLL.C <- log(cc*qq^5) + log(1-qq^5) - log(1-cc*qq^10)
  NLL.All <- -1*(x1*NLL.A + x2*NLL.B + x3*NLL.C)
  
  return(sum(NLL.All))
}

##########################
## References
##########################

# Link, W. A., and R. J. Barker (2010). 
# Bayesian Inference with Ecological Applications. 
# Academic Press, New York, USA.

##### END FUNCTIONS ##########################################################
