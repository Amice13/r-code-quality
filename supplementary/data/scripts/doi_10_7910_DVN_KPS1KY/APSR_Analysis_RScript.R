###################################################################/
# Populist Attitudes: More than the sum of its parts?             #
# Last updated: November 17, 2019                                 #
# Authors: Wuttke, Alexander; Schimpf, Christian, Schoen Harald   #
# Harvard Dataverse:		https://doi.org/10.7910/DVN/KPS1KY        # 
# Shiny Web Application:	http://populism.alexander-wuttke.de/    #
# R-Version: 3.5.1                                                #
# Interface: R-Studio                                             #
##################################################################/

## SETTING WORKING DIRECTORY etc.: Start by creating a main folder.
## To run the R-Script, place the data in the subfolder "data" and the 
## syntaxes in the subfolder "syntax". Also create a subfolder called 
## "Plots" in the main directory along with the following sub-subfolder 
## structures:
## "Plots/Appendix"
## "Plots/TIFF/Appendix TIFF"
## All instances in which a path needs to be exchanged can be searched by
## searching for ##PATH##.


#Clear the working space
rm(list=ls()) 


#///////////////////////////////#
#### Step 0.A: load packages ####
#///////////////////////////////#

# Note: the renv_Wuttkeetal2019.lock file contains information on 
# the versions of all packages used in this R-Script.

library("readstata13")
library("lavaan")
library("foreign")
library("ggplot2")
library("reshape2")
library("GGally")
library("dotwhisker")
library("broom")
library("dplyr")
library("purrr")
library("tidyr")
library("cocor")


#//////////////////////////////////////#
#### Step 0.B: Additional Functions ####
#//////////////////////////////////////#

# The function below is a modified version of the common R Function "mycor." It is modified
# in a way that it allows to specify the number of decimals for the resulting correlation
# coefficients. This function was copied from a stackoverflow entry, available here:
# https://stackoverflow.com/questions/47496364/rounding-digits-in-ggpairs

# Update (November 17, 2019): Because ggplot2 changed the way it handles data, the 
# function below required updating. There is a link in the original stackoverflow 
# thread that leads to the following website: https://github.com/ggobi/ggally/issues/294
# The mycor function was updated accordingly to allow for replication of the 
# original analyses (please be aware that with future updates, there may be further
# updates to this function required to replicate the data. To ensure smooth functioning,
# the version and packages as outlined at the begining of this script should be used.)

mycor <- function(data, mapping, alignPercent = 0.6, method = "pearson", 
                  use = "complete.obs", corAlignPercent = NULL, corMethod = NULL, 
                  corUse = NULL, sgnf=3, ...) {
  
  if (! is.null(corAlignPercent)) {
    stop("'corAlignPercent' is deprecated.  Please use argument 'alignPercent'")
  }
  if (! is.null(corMethod)) {
    stop("'corMethod' is deprecated.  Please use argument 'method'")
  }
  if (! is.null(corUse)) {
    stop("'corUse' is deprecated.  Please use argument 'use'")
  }
  
  useOptions <- c(
    "all.obs",
    "complete.obs",
    "pairwise.complete.obs",
    "everything",
    "na.or.complete"
  )
  use <- pmatch(use, useOptions)
  if (is.na(use)) {
    warning("correlation 'use' not found.  Using default value of 'all.obs'")
    use <- useOptions[1]
  } else {
    use <- useOptions[use]
  }
  
  cor_fn <- function(x, y) {
    # also do ddply below if fn is altered
    cor(x, y, method = method, use = use)
  }
  
  # xVar <- data[[as.character(mapping$x)]]
  # yVar <- data[[as.character(mapping$y)]]
  # x_bad_rows <- is.na(xVar)
  # y_bad_rows <- is.na(yVar)
  # bad_rows <- x_bad_rows | y_bad_rows
  # if (any(bad_rows)) {
  #   total <- sum(bad_rows)
  #   if (total > 1) {
  #     warning("Removed ", total, " rows containing missing values")
  #   } else if (total == 1) {
  #     warning("Removing 1 row that contained a missing value")
  #   }
  #
  #   xVar <- xVar[!bad_rows]
  #   yVar <- yVar[!bad_rows]
  # }
  
  # mapping$x <- mapping$y <- NULL
  
  xData <- eval_data_col(data, mapping$x)
  yData <- eval_data_col(data, mapping$y)
  
  if (GGally:::is_date(xData)) {
    xData <- as.numeric(xData)
  }
  if (GGally:::is_date(yData)) {
    yData <- as.numeric(yData)
  }
  colorData <- eval_data_col(data, mapping$colour)
  if (is.numeric(colorData)) {
    stop("ggally_cor: mapping color column must be categorical, not numeric")
  }
  
  if (use %in% c("complete.obs", "pairwise.complete.obs", "na.or.complete")) {
    if (!is.null(colorData) && (length(colorData) == length(xData))) {
      rows <- complete.cases(xData, yData, colorData)
    } else {
      rows <- complete.cases(xData, yData)
    }
    
    if (any(!rows)) {
      total <- sum(!rows)
      if (total > 1) {
        warning("Removed ", total, " rows containing missing values")
      } else if (total == 1) {
        warning("Removing 1 row that contained a missing value")
      }
    }
    
    if (!is.null(colorData) && (length(colorData) == length(xData))) {
      colorData <- colorData[rows]
    }
    xData <- xData[rows]
    yData <- yData[rows]
  }
  
  xVal <- xData
  yVal <- yData
  
  # if the mapping has to deal with the data, remove it
  if (packageVersion("ggplot2") > "2.2.1") {
    for (mappingName in names(mapping)) {
      itemData <- eval_data_col(data, mapping[[mappingName]])
      if (!inherits(itemData, "AsIs")) {
        mapping[[mappingName]] <- NULL
      }
    }
  } else {
    if (length(names(mapping)) > 0){
      for (i in length(names(mapping)):1){
        # find the last value of the aes, such as cyl of as.factor(cyl)
        tmp_map_val <- deparse(mapping[names(mapping)[i]][[1]])
        if (tmp_map_val[length(tmp_map_val)] %in% colnames(data))
          mapping[[names(mapping)[i]]] <- NULL
        
        if (length(names(mapping)) < 1){
          mapping <- NULL
          break;
        }
      }
    }
  }
  
  if (
    !is.null(colorData) &&
    !inherits(colorData, "AsIs")
  ) {
    
    cord <- plyr::ddply(
      data.frame(x = xData, y = yData, color = colorData),
      "color",
      function(dt) {
        cor_fn(dt$x, dt$y)
      }
    )
    colnames(cord)[2] <- "correlation"
    
    cord$correlation <- signif(as.numeric(cord$correlation), sgnf)
    
    # put in correct order
    lev <- levels(as.factor(colorData))
    ord <- rep(-1, nrow(cord))
    for (i in 1:nrow(cord)) {
      for (j in seq_along(lev)){
        if (identical(as.character(cord$color[i]), as.character(lev[j]))) {
          ord[i] <- j
        }
      }
    }
    
    # print(order(ord[ord >= 0]))
    # print(lev)
    cord <- cord[order(ord[ord >= 0]), ]
    cord$label <- GGally:::str_c(cord$color, ": ", cord$correlation)
    
    # calculate variable ranges so the gridlines line up
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * (xmax - xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * (ymax - ymin))
    
    
    # print(cord)
    p <- ggally_text(
      label   = GGally:::str_c("Cor : ", signif(cor_fn(xVal, yVal), sgnf)),
      mapping = mapping,
      xP      = 0.5,
      yP      = 0.9,
      xrange  = xrange,
      yrange  = yrange,
      color   = "black",
      ...
    ) +
      #element_bw() +
      theme(legend.position = "none")
    
    xPos <- rep(alignPercent, nrow(cord)) * diff(xrange) + min(xrange, na.rm = TRUE)
    yPos <- seq(
      from = 0.9,
      to = 0.2,
      length.out = nrow(cord) + 1)
    yPos <- yPos * diff(yrange) + min(yrange, na.rm = TRUE)
    yPos <- yPos[-1]
    # print(range(yVal))
    # print(yPos)
    
    cordf <- data.frame(xPos = xPos, yPos = yPos, labelp = cord$label)
    cordf$labelp <- factor(cordf$labelp, levels = cordf$labelp)
    # print(cordf)
    # print(str(cordf))
    
    p <- p + geom_text(
      data = cordf,
      aes(
        x = xPos,
        y = yPos,
        label = labelp,
        color = labelp
      ),
      hjust = 1,
      ...
      
    )
    
    p
  } else {
    # calculate variable ranges so the gridlines line up
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * (xmax - xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * (ymax - ymin))
    
    p <- ggally_text(
      label = paste(
        "Corr:\n",
        signif(
          cor_fn(xVal, yVal),
          sgnf
        ),
        sep = "", collapse = ""
      ),
      mapping,
      xP = 0.5,
      yP = 0.5,
      xrange = xrange,
      yrange = yrange,
      ...
    ) +
      #element_bw() +
      theme(legend.position = "none")
    
    p
  }
}

#### >>> Set working directory ####

setwd("##PATH##/Data")

##############################################################################################/
#### 1 - Analyzing the GLES Data ####
##############################################################################################/


#////////////////////////////////////#
#### Step 1A: load data set - GLES ####
#////////////////////////////////////#

# >>> Change path to location in which dataset is stored. 

dat <- read.dta13("##PATH##/Data/populism_touched.dta")


#//////////////////////////////////////#
#### Step 1B: Fit CFA Models - GLES ####
#/////////////////////////////////////#

## Add a running ID - make identification of respondents within the Lavaan models easier:
dat$ID <- seq.int(nrow(dat))

## Model - Standard CFA WITH multi-level structure (a la Schulz et al 2017):

# https://jonathantemplin.com/files/sem/sem15pre906/sem15pre906_lecture09.pdf

popmodel <- '
  # measurement model
Antielite =~    pop_antielite_talk	+   pop_antielite_differences +	pop_antielite_contact
Homogenous =~   pop_hom_character +	    pop_hom_unity		+		        pop_hom_values
Sovereignty =~  pop_sov_referendum	+   pop_sov_decisions	+		      pop_sov_delegate
Populism =~     Antielite           +   Homogenous        +         Sovereignty
'
fit1 <- lavaan::cfa(popmodel, data=dat)
summary(fit1,fit.measures=TRUE)

# Now predict the populism score from the multidimensional CFA model ("Popmodel") for all respondents
# who have NO system missing value for any of the populist item (Note: Because the CFA deletes incomplete
# cases, predicting without specifying complete cases results in an error because R will argue that one 
# tries to replace data for less respondents (i.e., only the complete cases) than there are respondents
# in total in the dataset, i.e. all respondents regardless of whether they have a missing for any of the 
# 9 populist items or not) 

## Step 1. Extract ID used in lavaan model (note: Lavaan uses a running number. Cases which were deleted
## because of an NA in one of items were skipped. Because we have added a running ID to the original 
## dataset before, the factor scores can then easily be merged with the original dataset, using the ID.):
ID <- inspect(fit1, "case.idx") 
#(Note - see here for further information: https://groups.google.com/forum/#!msg/lavaan/UPrU8qG5nOs/70OyCU-1u4EJ,
# last accessed: September 6, 2018)


## Step 2. predict the scores and add the ID variable used in CFA Lavaan model:
scores <- cbind(ID, lavPredict(fit1))

## Step 3. Convert into data frame:
scoredf <- as.data.frame(scores)

## Step 4. Rename the new variables:
colnames(scoredf)[2] <- "AntiElitism_3D_SEM_Schulz_LowerFactor"
colnames(scoredf)[3] <- "Homogeneity_3D_SEM_Schulz_LowerFactor"
colnames(scoredf)[4] <- "Sovereignty_3D_SEM_Schulz_LowerFactor"
colnames(scoredf)[5] <- "Populism_3D_SEM_Schulz_HigherFactor"

## Step 5. Merge Schulz Factor score dataframe with original dataset. Note that for cases with an 
## NA (i.e., system missing) in one of the populist items, the factor scores are also NA:

df_Final <- merge(dat, scoredf, by = "ID", all.x = T)

## Save as ID and New Populism Score as .dta file:

myvars <- c("lfdn", "ID", "AntiElitism_3D_SEM_Schulz_LowerFactor", "Homogeneity_3D_SEM_Schulz_LowerFactor",
            "Sovereignty_3D_SEM_Schulz_LowerFactor", "Populism_3D_SEM_Schulz_HigherFactor")
newdata <- df_Final[myvars]

# >>> change path to folder in which dataset is to be stored:

write.dta(newdata, "##PATH##/Data/populism_touched_PLUS_SCHULZ.dta")

## Through the Stata Syntax, the newly generated variables will be added on to the GLES dataset. For the purpose
## of running this R-Scrip, no further steps need to be taken in STATA. 

#####################################################################################/
#### Step 2 - Main Analyses - Figures 2-5 in the Main Text.                      ####
#####################################################################################/

## In the following section, we generate the four figures (Figures 2 through 4)
## presented in the main text. Figure 2 is based on the GLES data, Figures 3-5 
## are based on the Silva Dataset described in the main text. Before proceeding,
## we provide some general information on the Silva Dataset as well as we load
## and prepare the dataset to allow working simultanousely with both the GLES 
## and the Silva data.

# Information about Silva dataset: 
# To ease analyses, variables referring to single dimensions are referred to
# in some way as "anti-elitism", "sovereignty", and "homogeneity" (as used in
# Schulz scale). However, for two scales (Oliver/Rahn & Silva), these variables 
# have slightly different meanings and names in the original dataset/publications:
#
#          Silva Scale:   Anti-Elitism (Schulz) = Anti-Elitism (Silva)
#                         Sovereignty (Schulz)	= People-Centrism (Silva)
#                         Homogeneity (Schulz)	= Manichean Outlook (Silva)



#///////////////////////////////////////////////////#
#### Step 2A: load Silva Full Dataset            ####
#///////////////////////////////////////////////////#

# >>> Change path to folder in which Full Dataset ("silva_recoded.dta") is stored before running commands:

dat_Silva <- read.dta13("##PATH##/Data/Silva/silva_recoded.dta")


#///////////////////////////////////////#
#### Step 2B: Data Preparation - Silva####
#///////////////////////////////////////#

## Assign levels to Country Identifier:
dat_Silva$country <- as.factor(dat_Silva$country)
levels(dat_Silva$country) <- c("US", "Brazil", "Mexico",
                               "Spain", "Italy", "UK", 
                               "France", "Greece", "Ireland")


#////////////////////////////////////////////////////////////////////////#
#### Step 2C: FIGURE 2 - Correlation Matrix Schulz et al (Germany)      #### 
#### -- Dimensions and Scales Correlations  (GLES Panel Data)          ###/
#////////////////////////////////////////////////////////////////////////#

# Note: this figure is also shown in the supplementary material (Figure S3-2)

## The following code generates Figure 2 in the paper:

#create quick subset containing only the variables to be included in the Figure:
plotsub1 <- c("antielite_sum_z", "homogenous_sum_z", "sovereignty_sum_z",
              "pop_3D_man_goertzian_z", "pop_3D_man_bollen_z", "pop_3D_man_sartori75_z")
df_plotsub1 <- df_Final[plotsub1]

#Ensure that Sartortori Variable is factor (because it is a dichotomy):
df_plotsub1$pop_3D_man_sartori75_z <- as.factor(df_plotsub1$pop_3D_man_sartori75_z)

#Add levels (value labels) to Sartori Variable:
# 0. "No Pop." - Respondent is not a populist according to Sartori aggregation
# 1. "Pop." - Respondent is a populist according to Sartori aggregation
levels(df_plotsub1$pop_3D_man_sartori75_z) <- c("No Pop.", "Pop.")

#############################################/
# ggpairs solution to plot:
#Link: https://stackoverflow.com/questions/45873483/ggpairs-plot-with-heatmap-of-correlation-values
#(Last accessed: Sept.28, 2018)

#ggpairs(df_plotsub1)

#Change names of columns: 
colnames(df_plotsub1) <- c("Anti-Elitism", "Homogeneity", "Sovereignty",
                           "Goertzian", "Bollen", "Sartorian")

# Matrix of plots
p1 <- ggpairs(df_plotsub1[!is.na(df_plotsub1$Sartorian),], 
              lower = list(continuous = wrap("points", alpha = 0.01 )),
              upper = list(continuous = wrap (mycor, sgnf=2)))

# Correlation matrix plot
p2 <- ggcorr(df_plotsub1, label = TRUE, label_round = 2, 
             low = "#998ec3", mid="#CCCCCC", high = "#f1a340") 

# https://www.rdocumentation.org/packages/GGally/versions/1.4.0/topics/ggcorr
# last accessed: November 20, 2018

# Extract the list of colors from the correlation matrix plot:
g2 <- ggplotGrob(p2)
colors <- g2$grobs[[6]]$children[[3]]$gp$fill
colors

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colors2 <- c("#D9C3AFFF", "#E8B47CFF", "#EAB173FF", "#EDAC63FF",
             "#CCCCCC", "#E1BC98FF", "#EBAF6DFF", "#EBAF6DFF" , "#CCCCCC",
             "#EBAF6BFF", "#EEA956FF", "#CCCCCC", "#EFA852FF", "#CCCCCC",
             "#CCCCCC")

# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1,k1,k2) +
      theme(panel.background = element_rect(fill = colors2[idx], color="white"),
            panel.grid.major = element_line(color=colors2[idx]))
    p1 <- putPlot(p1,plt,k1,k2) 
    idx <- idx+1
  }
}
#print(p1)

#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/
# >>> Change path to Plots Folder:

png("##PATH##/Plots/FIGURE_2_GLES_Schulz_Scale_Relationship_Aggregations_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=300)
p1 
dev.off()

#TIF Version
tiff("##PATH##/Plots/TIFF/FIGURE_2_TIFF_GLES_Schulz_Scale_Relationship_Aggregations_SingleDimensions.tiff",
    units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1
dev.off()

#////////////////////////////////////////////////////////////////////////#
#### Step 2D: FIGURE 3 - Correlation Matrix Castanho Silva et al. (US)  #### 
#### -- Dimensions and Scales Correlations  (Castanho Silva et al data) ###/
#////////////////////////////////////////////////////////////////////////#

## The following code generates Figure 3 in the main text. 

## Step one is to subset the relevant variables for the plots:

#create subset for respondents from US only:
dat_Silva_MTURK_Only <- dat_Silva[which(dat_Silva$country=="US"),]

#Select variables relevant for plot
plotsub1b <- c("antielite_sum_z", "homogenous_sum_z", "sovereignty_sum_z",
               "pop_3D_man_goertzian_z", "pop_3D_man_bollen_z", "pop_3D_man_sartori75_z")
df_plotsub1b <- dat_Silva_MTURK_Only[plotsub1b]

#Change names of columns: 
colnames(df_plotsub1b) <- c("Anti-Elitism", "Manichean", "People-Centrism",
                            "Goertzian", "Bollen", "Sartorian")

#Change Sartori to factor type variable:
df_plotsub1b$Sartorian <- as.factor(df_plotsub1b$Sartorian)

#Add levels (value labels) to Sartori Variable:
levels(df_plotsub1b$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1b <- ggpairs(df_plotsub1b, lower = list(continuous = wrap("points", alpha = 0.2 )),
               upper = list(continuous = wrap (mycor, sgnf=2))) 
# Correlation matrix plot
p2b <- ggcorr(df_plotsub1b, label = TRUE, label_round = 2,
              low = "#998ec3", mid="#CCCCCC", high = "#f1a340") 

# Extract the list of colors from the correlation matrix plot:
g2b <- ggplotGrob(p2b)
colorsb <- g2b$grobs[[6]]$children[[3]]$gp$fill
colorsb

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsb2 <- c("#D1C9C2FF", "#DCC0A5FF", "#DDBFA3FF", "#EBB06EFF" ,
              "#CCCCCC", "#D5C6B9FF", "#EEAA5AFF", "#E9B278FF", "#CCCCCC",
              "#E0BC99FF", "#EBB06EFF", "#CCCCCC", "#ECAD64FF", "#CCCCCC",
              "#CCCCCC")



# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1b,k1,k2) +
      theme(panel.background = element_rect(fill = colorsb2[idx], color="white"),
            panel.grid.major = element_line(color=colorsb2[idx]))
    p1b <- putPlot(p1b,plt,k1,k2)
    idx <- idx+1
  }
}

# Save as PNG
# >>> Change path to folder for PNG figures:
png("##PATH##/Plots/FIGURE_3_MTURK_Silva_Scale_Relationship_Aggregations_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=300)
p1b +  theme(text = element_text(size=9.75))
dev.off()

#TIF Version
# >>> Change path to folder for TIFF figures:
tiff("##PATH##/Plots/TIFF/FIGURE_3_TIFF_MTURK_Silva_Scale_Relationship_Aggregations_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1b +  theme(text = element_text(size=9.75))
dev.off()


#////////////////////////////////////////////////////////////////////////#
#### Step 2E: FIGURE 4 - Correlation Matrix Akkerman et al (Germany)    #### 
#### -- Dimensions and Scales Correlations  (2017 GLES CrossSec Data)   ###/
#////////////////////////////////////////////////////////////////////////#

# The following code generates Figure 4 which depicts the correlations between
# the idmensions and aggregates of the Akkerman et al scale. Data basis is the
# 2017 Post-Election Cross Sectional GLES study. 

#create quick subset for German data only and, including just the variables needed for the plot:
# >>> load recoded GLES Panel Data
df_plotsub1.i_Akkerm_DEU <- read.dta13("##PATH##/Data/gles_touched.dta")

plotsub1.i_Akkerm <- c("antielite_sum_akker_z", "homogenous_sum_akker_z", "sovereignty_sum_akker_z",
                       "pop_3D_goertzian_akker_z", "pop_3D_bollen_akker_z", "pop_3D_man_sartori75_akker")
df_plotsub1.i_Akkerm_DEU <- df_plotsub1.i_Akkerm_DEU[plotsub1.i_Akkerm]

#Change names of columns: 
colnames(df_plotsub1.i_Akkerm_DEU) <- c("Anti-Elitism", "Manichean", "Sovereignty",
                                        "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Akkerm_DEU$Sartorian <- as.factor(df_plotsub1.i_Akkerm_DEU$Sartorian)
levels(df_plotsub1.i_Akkerm_DEU$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Akkerm_DEU <- ggpairs(df_plotsub1.i_Akkerm_DEU[!is.na(df_plotsub1.i_Akkerm_DEU$Sartorian),], 
                          lower = list(continuous = wrap("points", alpha = 0.05 )),
                          upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Akkerm_DEU <- ggcorr(df_plotsub1.i_Akkerm_DEU[!is.na(df_plotsub1.i_Akkerm_DEU$Sartorian),], 
                         label = TRUE, label_round = 2,
                         low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Akkerm_DEU <- ggplotGrob(p2i_Akkerm_DEU)
colorsi_Akkerm_DEU <- g2i_Akkerm_DEU$grobs[[6]]$children[[3]]$gp$fill
colorsi_Akkerm_DEU

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Akkerm_DEU2 <- c("#E6B682FF", "#E6B684FF", "#E8B379FF", "#EEAA5CFF",
                         "#CCCCCC", "#E5B787FF", "#EFA74FFF", "#EFA855FF", "#CCCCCC",
                         "#EBB06EFF", "#EDAC60FF", "#CCCCCC", "#EFA74FFF",
                         "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Akkerm_DEU,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Akkerm_DEU2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Akkerm_DEU2[idx]))
    p1i_Akkerm_DEU <- putPlot(p1i_Akkerm_DEU,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/FIGURE_4_GLES_CS_Akkerman_Scale_Relationship_Aggregations_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Akkerm_DEU
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/FIGURE_4_GLES_CS_Akkerman_Scale_Relationship_Aggregations_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Akkerm_DEU
dev.off()



#////////////////////////////////////////////////////////////////////////////////#
#### Step 2F: FIGURE 5 - Correlations (Goertz and Bollen) - Castanho Silva Datset      ####
#////////////////////////////////////////////////////////////////////////////////#

# This step produces correlations for Figure 4 in the
# paper. It shows the correlation between two different aggregations types,
# Bollen and Goertz for the Schulz Scale, the Castanho Silva Scale, and the 
# Akkerman Scale (all three included in Castanho Silva et al Dataset)

#Link: https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
#(Last accessed: October 7, 2018)

# Correlations by country: Goertz and Bollenin Castanho Silva et al Scale (Silva Dataset)
by_country1 <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_man_goertzian_z,
                                .x$pop_3D_man_bollen_z)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)

# Correlations by country: Goertz and Bollen in Schulz et al Scale (Silva Dataset)
by_country2 <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_goertzian_schulz_z,
                                .x$pop_3D_bollen_schulz_z)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 2)

# Correlations by country: Goertz and Bollen in Akkerman et al scale (Silva Dataset)
by_country3 <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_goertzian_akker_z,
                                .x$pop_3D_bollen_akker_z)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 3)

##Bind the data frames with the correlation results together:

correlation_results1_Silva <- rbind(by_country1, 
                                    by_country2,
                                    by_country3)


#Ensure that Scale Variable is factor and has levels:
correlation_results1_Silva$Scale<- as.factor(correlation_results1_Silva$Scale)
levels(correlation_results1_Silva$Scale) <- c(" Castanho \n Silva et al.", "Schulz et al.", "Akkerman et al.")

## Plot correlations (with 95% CIs) by Scale and Country
p1a <- ggplot(correlation_results1_Silva,
              aes(x=Scale, y=estimate)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high), size=0.7) +
  facet_wrap( ~ country)+
  coord_flip()+
  ylim(0.2, 1) +
  xlab("") +
  ylab("Correlation between Bollen and Goertz \n Concept Structure (Pearson's R, 95 CI%)")

# Save plot:
png("##PATH##/Plots/FIGURE_5_SILVA_DATASETS_CorrelationsBtwGoertzAndBollenByCountry.png", units="in", width=7.5, height=5.8, res=300)
p1a
dev.off()

#TIF Version
tiff("##PATH##/Plots/TIFF/FIGURE_5_TIFF_SILVA_DATASETS_CorrelationsBtwGoertzAndBollenByCountry.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1a
dev.off()

#////////////////////////////////////////////////////////////////////////////////////////////////////////#
#### Step 2G: FIGURE 6 - Analysis Step 3 (Bollen and Goertz) - Correlation with trust_institutions_silva  ####
#////////////////////////////////////////////////////////////////////////////////////////////////////////#

# Comparing correlation between populist attitudes and its dimensions to political
# trust

#### --- Silva Data - Correlation with Trust Variable (all items)

# Correlations by country: Goertz (1A) / Bollen (1B) / Antielitism (1C)/
#                          Manichean Outlook (1D) / People Centrism (1E) &
#                          Institutional Trust (derived from
#                         Silva Scale) (Silva Dataset)
by_country1A_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_man_goertzian_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1) %>%
  mutate(Aggregate = 1) %>%
  mutate(Type = 2)

by_country1B_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_man_bollen_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 2)%>%
  mutate(Type = 2)

by_country1C_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$antielite_sum_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 3)%>%
  mutate(Type = 1)

by_country1D_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$homogenous_sum_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 4)%>%
  mutate(Type = 1)

by_country1E_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$sovereignty_sum_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 5)%>%
  mutate(Type = 1)

## Note: for the Silva scale, variables referred to as "homogeneity" measure the manichean outlook. 
##       Variables labeled as "sovreignty" refer to the original scale dimension "people-centrism."

### Bind Dataset

dat_Silva_Plot_TrustSilvaCor <- rbind(by_country1A_trust_silva, by_country1B_trust_silva,
                                      by_country1C_trust_silva, by_country1D_trust_silva,
                                      by_country1E_trust_silva)

### Assign Levels to Scale & Aggregate Variables & Type:
dat_Silva_Plot_TrustSilvaCor$Scale <- as.factor(dat_Silva_Plot_TrustSilvaCor$Scale)
levels(dat_Silva_Plot_TrustSilvaCor$Scale) <- c("Silva")

dat_Silva_Plot_TrustSilvaCor$Aggregate <- as.factor(dat_Silva_Plot_TrustSilvaCor$Aggregate)
levels(dat_Silva_Plot_TrustSilvaCor$Aggregate) <- c("Goertz",
                                                    "Bollen",
                                                    "Anti-Elitism",
                                                    "Manichean Outlook",
                                                    "People-Centrism")

dat_Silva_Plot_TrustSilvaCor$Type <- as.factor(dat_Silva_Plot_TrustSilvaCor$Type)
levels(dat_Silva_Plot_TrustSilvaCor$Type) <- c("Single Dimension", "Aggregation")

#Plot (Silva Scale):
p3a_Silva <- ggplot(dat_Silva_Plot_TrustSilvaCor[dat_Silva_Plot_TrustSilvaCor$Scale=="Silva",],
                    aes(x=Aggregate, y=estimate, color = Type)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high), size=0.7) +
  geom_hline(yintercept=0, color='red', size=0.5)+
  facet_wrap( ~ country)+
  coord_flip()+
  xlab("")+
  scale_color_manual(values=c("#666666", "#000000")) +
  theme(legend.position="bottom")

## Save the plots:
png("##PATH##/Plots/FIGURE_6_Correlations_Trust_Silva_Items_MTURK_Silva_Scale.png", units="in", width=7.5, height=5.8, res=300)
p3a_Silva
dev.off()

#TIF Version
tiff("##PATH##/Plots/TIFF/FIGURE_6_TIFF_Correlations_Trust_Silva_Items_MTURK_Silva_Scale.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p3a_Silva
dev.off()


#### Calculate if differences between correlations are statistically different from each other,
##   using the cocor package:
# Diedenhofen, B. & Musch, J. (2015). cocor: A Comprehensive Solution for the Statistical Comparison
# of Correlations. PLoS ONE, 10(4): e0121945. doi:10.1371/journal.pone.0121945

#-> Note: the purpose of this test is to see if the correlation coefficients between Bollen-Trust
#         and Goertz-Trust are different from each other in each of the nine samples. Thus, the test
#         is based on two depedent groups (because they are from the same sample) with overlapping
#         correlations. This is done only for the Silva Scale (Figure 5 in the paper)

# Step 1: generate datasets which include the correlations for each of the country samples (takes as 
#         the basis the dataset which includes the correlation between Goertz and Trust)
cocor_1_silvatrust <- by_country1A_trust_silva
# Add Bollen-Trust Cor:
cocor_1_silvatrust$estimate2 <- by_country1B_trust_silva$estimate
# Add variable to indicate the number of cases per country:
cocor_1_silvatrust$N <- NA
cocor_1_silvatrust$N[cocor_1_silvatrust$country=="US"] <- 505
cocor_1_silvatrust$N[cocor_1_silvatrust$country=="Brazil"] <- 285
cocor_1_silvatrust$N[cocor_1_silvatrust$country=="Mexico"] <- 281
cocor_1_silvatrust$N[cocor_1_silvatrust$country=="Spain"] <- 278
cocor_1_silvatrust$N[cocor_1_silvatrust$country=="Italy"] <- 270
cocor_1_silvatrust$N[cocor_1_silvatrust$country=="UK"] <- 219
cocor_1_silvatrust$N[cocor_1_silvatrust$country=="France"] <- 222
cocor_1_silvatrust$N[cocor_1_silvatrust$country=="Greece"] <- 310
cocor_1_silvatrust$N[cocor_1_silvatrust$country=="Ireland"] <- 186
#Add correlation between Goertz and Bollen var:
cocor_1_silvatrust$GBcor <- tidy(cor.test(dat_Silva$pop_3D_man_goertzian_z, dat_Silva$pop_3D_man_bollen_z))$estimate

# Apply the test separately for each country sample:
#Brazil
cocor.dep.groups.overlap(r.jk=-0.0198, r.jh=-0.159, r.kh=+0.7028052, n=285, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)
#Mexico
cocor.dep.groups.overlap(r.jk=+0.0248, r.jh=-0.180, r.kh=+0.7028052, n=281, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)
#Spain
cocor.dep.groups.overlap(r.jk=-0.0885, r.jh=-0.305, r.kh=+0.7028052, n=278, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)
#Italy
cocor.dep.groups.overlap(r.jk=-0.130, r.jh=-0.323, r.kh=+0.7028052, n=270, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)
#UK
cocor.dep.groups.overlap(r.jk=+0.0157, r.jh=-0.176, r.kh=+0.7028052, n=219, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)
#France
cocor.dep.groups.overlap(r.jk=+0.0833, r.jh=-0.284, r.kh=+0.7028052, n=222, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)
#Greece
cocor.dep.groups.overlap(r.jk=+0.0554, r.jh=-0.146, r.kh=+0.7028052, n=310, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)
#US
cocor.dep.groups.overlap(r.jk=-0.0369, r.jh=-0.127, r.kh=+0.7028052, n=505, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)
#Ireland
cocor.dep.groups.overlap(r.jk=+0.0318, r.jh=-0.240, r.kh=+0.7028052, n=186, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

# Note:
# r.jk= Correlation between Trust and Goertz Aggregation of Populism
# r.jh= Correlation between Trust and Bollen Aggregation of Populism
# r.kh= Correlation between Bollen and Goertz Aggregation of Populism

#####################################################################################/
#### Step 3 - Supplementary Analyses and Figures (inc. for Shiny App)             ####
#####################################################################################/

## In this section of the R-Script, we generate the figures presented in the supplementary
## material and the Shiny Web App. The steps follow the numbering of supplementary sections and figures in the
## material. 

#### Step 3A - Figures for Supplement 3 (Discrepancies between original and adopted Schulz et al Scale) ####

## Note: Figure S3-1 in the supplementarys material identical to Figure 2 in the main text. For 
##       the R-Code, see code for FIGURE 2. 

#### <<>> Figure S3-10 - Brazil Sample (Silva data - Schulz Scale) ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SchulzH_Brazil <- dat_Silva[which(dat_Silva$country=="Brazil"),]

plotsub1.i_SilvaSchulz <- c("antielite_sum_schulz_z", "homogenous_sum_schulz_z", "sovereignty_sum_schulz_z",
                            "pop_3D_goertzian_schulz_z", "pop_3D_bollen_schulz_z", "pop_3D_sartori75_schulz_z")
df_plotsub1.i_SilvaSchulz_Brazil <- df_plotsub1.i_SchulzH_Brazil[plotsub1.i_SilvaSchulz]

#Change names of columns: 
colnames(df_plotsub1.i_SilvaSchulz_Brazil) <- c("Anti-Elitism", "Homogeneity", "Sovereignty",
                                                "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_SilvaSchulz_Brazil$Sartorian <- as.factor(df_plotsub1.i_SilvaSchulz_Brazil$Sartorian)
levels(df_plotsub1.i_SilvaSchulz_Brazil$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_SilvaSchulz_Brazil <- ggpairs(df_plotsub1.i_SilvaSchulz_Brazil[!is.na(df_plotsub1.i_SilvaSchulz_Brazil$Sartorian),], 
                                  lower = list(continuous = wrap("points", alpha = 0.1 )),
                                  upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_SilvaSchulz_Brazil <- ggcorr(df_plotsub1.i_SilvaSchulz_Brazil[!is.na(df_plotsub1.i_SilvaSchulz_Brazil$Sartorian),], 
                                 label = TRUE, label_round = 2,
                                 low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
p2i_SilvaSchulz_Brazil <- ggplotGrob(p2i_SilvaSchulz_Brazil)
colorsi_SilvaSchulz_Brazil <- p2i_SilvaSchulz_Brazil$grobs[[6]]$children[[3]]$gp$fill
colorsi_SilvaSchulz_Brazil

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_SilvaSchulz_Brazil2 <- c("#DCC0A6FF", "#E3BA91FF", "#E5B787FF", "#EDAC63FF"  ,
                                 "#CCCCCC", "#DCC1A7FF", "#EEA956FF", "#EBB06EFF", "#CCCCCC",
                                 "#E4B88AFF", "#EDAC62FF", "#CCCCCC", "#EEAA5BFF",
                                 "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_SilvaSchulz_Brazil,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_SilvaSchulz_Brazil2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_SilvaSchulz_Brazil2[idx]))
    p1i_SilvaSchulz_Brazil <- putPlot(p1i_SilvaSchulz_Brazil,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_3_Figure2_Brazil_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_Brazil
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_3_Figure2_Brazil_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_Brazil
dev.off()


#### <<>> Figure S3-9 - France Sample (Silva data - Schulz Scale) ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SchulzH_France <- dat_Silva[which(dat_Silva$country=="France"),]

plotsub1.i_SilvaSchulz <- c("antielite_sum_schulz_z", "homogenous_sum_schulz_z", "sovereignty_sum_schulz_z",
                            "pop_3D_goertzian_schulz_z", "pop_3D_bollen_schulz_z", "pop_3D_sartori75_schulz_z")
df_plotsub1.i_SilvaSchulz_France <- df_plotsub1.i_SchulzH_France[plotsub1.i_SilvaSchulz]

#Change names of columns: 
colnames(df_plotsub1.i_SilvaSchulz_France) <- c("Anti-Elitism", "Homogeneity", "Sovereignty",
                                                "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_SilvaSchulz_France$Sartorian <- as.factor(df_plotsub1.i_SilvaSchulz_France$Sartorian)
levels(df_plotsub1.i_SilvaSchulz_France$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_SilvaSchulz_France <- ggpairs(df_plotsub1.i_SilvaSchulz_France[!is.na(df_plotsub1.i_SilvaSchulz_France$Sartorian),], 
                                  lower = list(continuous = wrap("points", alpha = 0.1 )),
                                  upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_SilvaSchulz_France <- ggcorr(df_plotsub1.i_SilvaSchulz_France[!is.na(df_plotsub1.i_SilvaSchulz_France$Sartorian),], 
                                 label = TRUE, label_round = 2,
                                 low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
p2i_SilvaSchulz_France <- ggplotGrob(p2i_SilvaSchulz_France)
colorsi_SilvaSchulz_France <- p2i_SilvaSchulz_France$grobs[[6]]$children[[3]]$gp$fill
colorsi_SilvaSchulz_France

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_SilvaSchulz_France2 <- c("#D4C7BCFF", "#E5B788FF", "#E4B98DFF", "#ECAD64FF"  ,
                                 "#CCCCCC", "#DCC0A4FF", "#EEAA5CFF", "#E8B37AFF" , "#CCCCCC",
                                 "#E6B684FF", "#EEAA5AFF", "#CCCCCC", "#EDAB5EFF",
                                 "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_SilvaSchulz_France,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_SilvaSchulz_France2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_SilvaSchulz_France2[idx]))
    p1i_SilvaSchulz_France <- putPlot(p1i_SilvaSchulz_France,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_3_Figure3_France_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_France
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_3_Figure3_France_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_France
dev.off()


#### <<>> Figure S3-6 - Greece Sample (Silva data - Schulz Scale) ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SchulzH_Greece <- dat_Silva[which(dat_Silva$country=="Greece"),]

plotsub1.i_SilvaSchulz <- c("antielite_sum_schulz_z", "homogenous_sum_schulz_z", "sovereignty_sum_schulz_z",
                            "pop_3D_goertzian_schulz_z", "pop_3D_bollen_schulz_z", "pop_3D_sartori75_schulz_z")
df_plotsub1.i_SilvaSchulz_Greece <- df_plotsub1.i_SchulzH_Greece[plotsub1.i_SilvaSchulz]

#Change names of columns: 
colnames(df_plotsub1.i_SilvaSchulz_Greece) <- c("Anti-Elitism", "Homogeneity", "Sovereignty",
                                                "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_SilvaSchulz_Greece$Sartorian <- as.factor(df_plotsub1.i_SilvaSchulz_Greece$Sartorian)
levels(df_plotsub1.i_SilvaSchulz_Greece$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_SilvaSchulz_Greece <- ggpairs(df_plotsub1.i_SilvaSchulz_Greece[!is.na(df_plotsub1.i_SilvaSchulz_Greece$Sartorian),], 
                                  lower = list(continuous = wrap("points", alpha = 0.1 )),
                                  upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_SilvaSchulz_Greece <- ggcorr(df_plotsub1.i_SilvaSchulz_Greece[!is.na(df_plotsub1.i_SilvaSchulz_Greece$Sartorian),], 
                                 label = TRUE, label_round = 2,
                                 low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
p2i_SilvaSchulz_Greece <- ggplotGrob(p2i_SilvaSchulz_Greece)
colorsi_SilvaSchulz_Greece <- p2i_SilvaSchulz_Greece$grobs[[6]]$children[[3]]$gp$fill
colorsi_SilvaSchulz_Greece

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_SilvaSchulz_Greece2 <- c("#DBC1A8FF", "#E8B47DFF", "#E4B88BFF", "#EDAB5EFF"  ,
                                 "#CCCCCC", "#DDC0A4FF", "#EFA751FF", "#EAB070FF" , "#CCCCCC",
                                 "#E4B88BFF", "#EEAA5CFF", "#CCCCCC", "#EDAB5EFF",
                                 "#CCCCCC", "#CCCCCC")

# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_SilvaSchulz_Greece,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_SilvaSchulz_Greece2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_SilvaSchulz_Greece2[idx]))
    p1i_SilvaSchulz_Greece <- putPlot(p1i_SilvaSchulz_Greece,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_3_FigureA4_Greece_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_Greece
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_3_FigureA4_Greece_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_Greece
dev.off()


#### <<>> Figure S3-3 - Ireland Sample (Silva data - Schulz Scale) ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SchulzH_Ireland <- dat_Silva[which(dat_Silva$country=="Ireland"),]

plotsub1.i_SilvaSchulz <- c("antielite_sum_schulz_z", "homogenous_sum_schulz_z", "sovereignty_sum_schulz_z",
                            "pop_3D_goertzian_schulz_z", "pop_3D_bollen_schulz_z", "pop_3D_sartori75_schulz_z")
df_plotsub1.i_SilvaSchulz_Ireland <- df_plotsub1.i_SchulzH_Ireland[plotsub1.i_SilvaSchulz]

#Change names of columns: 
colnames(df_plotsub1.i_SilvaSchulz_Ireland) <- c("Anti-Elitism", "Homogeneity", "Sovereignty",
                                                 "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_SilvaSchulz_Ireland$Sartorian <- as.factor(df_plotsub1.i_SilvaSchulz_Ireland$Sartorian)
levels(df_plotsub1.i_SilvaSchulz_Ireland$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_SilvaSchulz_Ireland <- ggpairs(df_plotsub1.i_SilvaSchulz_Ireland[!is.na(df_plotsub1.i_SilvaSchulz_Ireland$Sartorian),], 
                                   lower = list(continuous = wrap("points", alpha = 0.1 )),
                                   upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_SilvaSchulz_Ireland <- ggcorr(df_plotsub1.i_SilvaSchulz_Ireland[!is.na(df_plotsub1.i_SilvaSchulz_Ireland$Sartorian),], 
                                  label = TRUE, label_round = 2,
                                  low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
p2i_SilvaSchulz_Ireland <- ggplotGrob(p2i_SilvaSchulz_Ireland)
colorsi_SilvaSchulz_Ireland <- p2i_SilvaSchulz_Ireland$grobs[[6]]$children[[3]]$gp$fill
colorsi_SilvaSchulz_Ireland

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_SilvaSchulz_Ireland2 <- c("#DFBD9CFF", "#E4B98DFF", "#E8B37AFF", "#EDAB5EFF"  ,
                                  "#CCCCCC", "#E0BD99FF", "#EEAA5BFF", "#EBAF6BFF", "#CCCCCC",
                                  "#E6B785FF", "#EDAB5EFF", "#CCCCCC", "#EEAA5BFF",
                                  "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_SilvaSchulz_Ireland,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_SilvaSchulz_Ireland2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_SilvaSchulz_Ireland2[idx]))
    p1i_SilvaSchulz_Ireland <- putPlot(p1i_SilvaSchulz_Ireland,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_3_Figure5_Ireland_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_Ireland
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_3_Figure5_Ireland_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_Ireland
dev.off()



#### <<>> Figure S3-4 - Italy Sample (Silva data - Schulz Scale) ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SchulzH_Italy <- dat_Silva[which(dat_Silva$country=="Italy"),]

plotsub1.i_SilvaSchulz <- c("antielite_sum_schulz_z", "homogenous_sum_schulz_z", "sovereignty_sum_schulz_z",
                            "pop_3D_goertzian_schulz_z", "pop_3D_bollen_schulz_z", "pop_3D_sartori75_schulz_z")
df_plotsub1.i_SilvaSchulz_Italy <- df_plotsub1.i_SchulzH_Italy[plotsub1.i_SilvaSchulz]

#Change names of columns: 
colnames(df_plotsub1.i_SilvaSchulz_Italy) <- c("Anti-Elitism", "Homogeneity", "Sovereignty",
                                               "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_SilvaSchulz_Italy$Sartorian <- as.factor(df_plotsub1.i_SilvaSchulz_Italy$Sartorian)
levels(df_plotsub1.i_SilvaSchulz_Italy$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_SilvaSchulz_Italy <- ggpairs(df_plotsub1.i_SilvaSchulz_Italy[!is.na(df_plotsub1.i_SilvaSchulz_Italy$Sartorian),], 
                                 lower = list(continuous = wrap("points", alpha = 0.1 )),
                                 upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_SilvaSchulz_Italy <- ggcorr(df_plotsub1.i_SilvaSchulz_Italy[!is.na(df_plotsub1.i_SilvaSchulz_Italy$Sartorian),], 
                                label = TRUE, label_round = 2,
                                low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
p2i_SilvaSchulz_Italy <- ggplotGrob(p2i_SilvaSchulz_Italy)
colorsi_SilvaSchulz_Italy <- p2i_SilvaSchulz_Italy$grobs[[6]]$children[[3]]$gp$fill
colorsi_SilvaSchulz_Italy

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_SilvaSchulz_Italy2 <- c("#DCC0A5FF", "#E2BA92FF", "#E5B786FF", "#ECAD66FF",
                                "#CCCCCC", "#E2BA91FF", "#EFA955FF", "#ECAD66FF", "#CCCCCC",
                                "#E8B47DFF", "#EDAB5DFF", "#CCCCCC", "#EEA957FF",
                                "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_SilvaSchulz_Italy,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_SilvaSchulz_Italy2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_SilvaSchulz_Italy2[idx]))
    p1i_SilvaSchulz_Italy <- putPlot(p1i_SilvaSchulz_Italy,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_3_Figure6_Italy_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_Italy
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_3_Figure6_Italy_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_Italy
dev.off()


#### <<>> Figure S3-5 - Mexico Sample (Silva data - Schulz Scale) ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SchulzH_Mexico <- dat_Silva[which(dat_Silva$country=="Mexico"),]

plotsub1.i_SilvaSchulz <- c("antielite_sum_schulz_z", "homogenous_sum_schulz_z", "sovereignty_sum_schulz_z",
                            "pop_3D_goertzian_schulz_z", "pop_3D_bollen_schulz_z", "pop_3D_sartori75_schulz_z")
df_plotsub1.i_SilvaSchulz_Mexico <- df_plotsub1.i_SchulzH_Mexico[plotsub1.i_SilvaSchulz]

#Change names of columns: 
colnames(df_plotsub1.i_SilvaSchulz_Mexico) <- c("Anti-Elitism", "Homogeneity", "Sovereignty",
                                                "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_SilvaSchulz_Mexico$Sartorian <- as.factor(df_plotsub1.i_SilvaSchulz_Mexico$Sartorian)
levels(df_plotsub1.i_SilvaSchulz_Mexico$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_SilvaSchulz_Mexico <- ggpairs(df_plotsub1.i_SilvaSchulz_Mexico[!is.na(df_plotsub1.i_SilvaSchulz_Mexico$Sartorian),], 
                                  lower = list(continuous = wrap("points", alpha = 0.1 )),
                                  upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_SilvaSchulz_Mexico <- ggcorr(df_plotsub1.i_SilvaSchulz_Mexico[!is.na(df_plotsub1.i_SilvaSchulz_Mexico$Sartorian),], 
                                 label = TRUE, label_round = 2,
                                 low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
p2i_SilvaSchulz_Mexico <- ggplotGrob(p2i_SilvaSchulz_Mexico)
colorsi_SilvaSchulz_Mexico <- p2i_SilvaSchulz_Mexico$grobs[[6]]$children[[3]]$gp$fill
colorsi_SilvaSchulz_Mexico

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_SilvaSchulz_Mexico2 <- c("#DAC2ABFF", "#E4B98CFF", "#E6B785FF", "#ECAD65FF",
                                 "#CCCCCC", "#E0BC98FF", "#EFA855FF", "#EBAF6BFF", "#CCCCCC",
                                 "#E6B683FF", "#EEAB5CFF", "#CCCCCC", "#EEA958FF",
                                 "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_SilvaSchulz_Mexico,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_SilvaSchulz_Mexico2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_SilvaSchulz_Mexico2[idx]))
    p1i_SilvaSchulz_Mexico <- putPlot(p1i_SilvaSchulz_Mexico,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_3_Figure7_Mexico_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_Mexico
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_3_Figure7_Mexico_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_Mexico
dev.off()


#### <<>> Figure S3-7 - Spain Sample (Silva data - Schulz Scale) ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SchulzH_Spain <- dat_Silva[which(dat_Silva$country=="Spain"),]

plotsub1.i_SilvaSchulz <- c("antielite_sum_schulz_z", "homogenous_sum_schulz_z", "sovereignty_sum_schulz_z",
                            "pop_3D_goertzian_schulz_z", "pop_3D_bollen_schulz_z", "pop_3D_sartori75_schulz_z")
df_plotsub1.i_SilvaSchulz_Spain <- df_plotsub1.i_SchulzH_Spain[plotsub1.i_SilvaSchulz]

#Change names of columns: 
colnames(df_plotsub1.i_SilvaSchulz_Spain) <- c("Anti-Elitism", "Homogeneity", "Sovereignty",
                                               "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_SilvaSchulz_Spain$Sartorian <- as.factor(df_plotsub1.i_SilvaSchulz_Spain$Sartorian)
levels(df_plotsub1.i_SilvaSchulz_Spain$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_SilvaSchulz_Spain <- ggpairs(df_plotsub1.i_SilvaSchulz_Spain[!is.na(df_plotsub1.i_SilvaSchulz_Spain$Sartorian),], 
                                 lower = list(continuous = wrap("points", alpha = 0.1 )),
                                 upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_SilvaSchulz_Spain <- ggcorr(df_plotsub1.i_SilvaSchulz_Spain[!is.na(df_plotsub1.i_SilvaSchulz_Spain$Sartorian),], 
                                label = TRUE, label_round = 2,
                                low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
p2i_SilvaSchulz_Spain <- ggplotGrob(p2i_SilvaSchulz_Spain)
colorsi_SilvaSchulz_Spain <- p2i_SilvaSchulz_Spain$grobs[[6]]$children[[3]]$gp$fill
colorsi_SilvaSchulz_Spain

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_SilvaSchulz_Spain2 <- c("#DDC0A4FF", "#E3BA91FF", "#E6B685FF", "#ECAD64FF",
                                "#CCCCCC", "#DEBFA1FF", "#EDAB5DFF", "#EBAF6DFF", "#CCCCCC",
                                "#E6B684FF", "#EDAB5FFF" , "#CCCCCC", "#EEAA5CFF",
                                "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_SilvaSchulz_Spain,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_SilvaSchulz_Spain2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_SilvaSchulz_Spain2[idx]))
    p1i_SilvaSchulz_Spain <- putPlot(p1i_SilvaSchulz_Spain,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_3_Figure8_Spain_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_Spain
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_3_Figure8_Spain_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_Spain
dev.off()



#### <<>> Figure S3-8 - UK Sample (Silva data - Schulz Scale) ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SchulzH_UK <- dat_Silva[which(dat_Silva$country=="UK"),]

plotsub1.i_SilvaSchulz <- c("antielite_sum_schulz_z", "homogenous_sum_schulz_z", "sovereignty_sum_schulz_z",
                            "pop_3D_goertzian_schulz_z", "pop_3D_bollen_schulz_z", "pop_3D_sartori75_schulz_z")
df_plotsub1.i_SilvaSchulz_UK <- df_plotsub1.i_SchulzH_UK[plotsub1.i_SilvaSchulz]

#Change names of columns: 
colnames(df_plotsub1.i_SilvaSchulz_UK) <- c("Anti-Elitism", "Homogeneity", "Sovereignty",
                                            "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_SilvaSchulz_UK$Sartorian <- as.factor(df_plotsub1.i_SilvaSchulz_UK$Sartorian)
levels(df_plotsub1.i_SilvaSchulz_UK$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_SilvaSchulz_UK <- ggpairs(df_plotsub1.i_SilvaSchulz_UK[!is.na(df_plotsub1.i_SilvaSchulz_UK$Sartorian),], 
                              lower = list(continuous = wrap("points", alpha = 0.1 )),
                              upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_SilvaSchulz_UK <- ggcorr(df_plotsub1.i_SilvaSchulz_UK[!is.na(df_plotsub1.i_SilvaSchulz_UK$Sartorian),], 
                             label = TRUE, label_round = 2,
                             low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
p2i_SilvaSchulz_UK <- ggplotGrob(p2i_SilvaSchulz_UK)
colorsi_SilvaSchulz_UK <- p2i_SilvaSchulz_UK$grobs[[6]]$children[[3]]$gp$fill
colorsi_SilvaSchulz_UK

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_SilvaSchulz_UK2 <- c("#D7C5B4FF", "#DFBE9DFF", "#E3BA8FFF", "#ECAE68FF",
                             "#CCCCCC", "#DDBFA1FF", "#EDAC62FF", "#EAB173FF", "#CCCCCC",
                             "#E5B787FF", "#EDAC62FF" , "#CCCCCC", "#EDAC60FF",
                             "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_SilvaSchulz_UK,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_SilvaSchulz_UK2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_SilvaSchulz_UK2[idx]))
    p1i_SilvaSchulz_UK <- putPlot(p1i_SilvaSchulz_UK,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_3_Figure9_UK_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_UK
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_3_Figure9_UK_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_UK
dev.off()


#### <<>> Figure S3-2 - US Sample (Silva data - Schulz Scale) ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SchulzH_US <- dat_Silva[which(dat_Silva$country=="US"),]

plotsub1.i_SilvaSchulz <- c("antielite_sum_schulz_z", "homogenous_sum_schulz_z", "sovereignty_sum_schulz_z",
                            "pop_3D_goertzian_schulz_z", "pop_3D_bollen_schulz_z", "pop_3D_sartori75_schulz_z")
df_plotsub1.i_SilvaSchulz_US <- df_plotsub1.i_SchulzH_US[plotsub1.i_SilvaSchulz]

#Change names of columns: 
colnames(df_plotsub1.i_SilvaSchulz_US) <- c("Anti-Elitism", "Homogeneity", "Sovereignty",
                                            "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_SilvaSchulz_US$Sartorian <- as.factor(df_plotsub1.i_SilvaSchulz_US$Sartorian)
levels(df_plotsub1.i_SilvaSchulz_US$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_SilvaSchulz_US <- ggpairs(df_plotsub1.i_SilvaSchulz_US[!is.na(df_plotsub1.i_SilvaSchulz_US$Sartorian),], 
                              lower = list(continuous = wrap("points", alpha = 0.1 )),
                              upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_SilvaSchulz_US <- ggcorr(df_plotsub1.i_SilvaSchulz_US[!is.na(df_plotsub1.i_SilvaSchulz_US$Sartorian),], 
                             label = TRUE, label_round = 2,
                             low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
p2i_SilvaSchulz_US <- ggplotGrob(p2i_SilvaSchulz_US)
colorsi_SilvaSchulz_US <- p2i_SilvaSchulz_US$grobs[[6]]$children[[3]]$gp$fill
colorsi_SilvaSchulz_US

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_SilvaSchulz_US2 <- c("#CFCAC5FF", "#DDC0A3FF", "#E0BD99FF", "#EAB06FFF" ,
                             "#CCCCCC", "#D9C3ADFF", "#ECAD65FF", "#E8B47BFF", "#CCCCCC",
                             "#E3BA8FFF", "#ECAD64FF", "#CCCCCC", "#ECAD64FF",
                             "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_SilvaSchulz_US,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_SilvaSchulz_US2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_SilvaSchulz_US2[idx]))
    p1i_SilvaSchulz_US <- putPlot(p1i_SilvaSchulz_US,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_3_Figure10_US_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_US
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_3_Figure10_US_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_US
dev.off()

#### Step 3B - Supplement 8 (Non-probabiltiy and probability based survey data) #### 

## Note: we also saved these figues below with headers and captions for the shiny app.

#### <<>> Figure S8-1 - Germany (GLES 2017 Cross Section Post Election Study; Supplement 8 Figure 1) ####

#create quick subset for German data only and, including just the variables needed for the plot:
# >>> load recoded GLES Panel Data
df_plotsub1.i_Akkerm_DEU <- read.dta13("D:/Dropbox (Privat)/populism_concept and measurement/Data/gles_touched.dta")

plotsub1.i_Akkerm <- c("antielite_sum_akker_z", "homogenous_sum_akker_z", "sovereignty_sum_akker_z",
                       "pop_3D_goertzian_akker_z", "pop_3D_bollen_akker_z", "pop_3D_man_sartori75_akker")
df_plotsub1.i_Akkerm_DEU <- df_plotsub1.i_Akkerm_DEU[plotsub1.i_Akkerm]

#Change names of columns: 
colnames(df_plotsub1.i_Akkerm_DEU) <- c("Anti-Elitism", "Manichean", "Sovereignty",
                                        "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Akkerm_DEU$Sartorian <- as.factor(df_plotsub1.i_Akkerm_DEU$Sartorian)
levels(df_plotsub1.i_Akkerm_DEU$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Akkerm_DEU <- ggpairs(df_plotsub1.i_Akkerm_DEU[!is.na(df_plotsub1.i_Akkerm_DEU$Sartorian),], 
                          lower = list(continuous = wrap("points", alpha = 0.05 )),
                          upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Akkerm_DEU <- ggcorr(df_plotsub1.i_Akkerm_DEU[!is.na(df_plotsub1.i_Akkerm_DEU$Sartorian),], 
                         label = TRUE, label_round = 2,
                         low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Akkerm_DEU <- ggplotGrob(p2i_Akkerm_DEU)
colorsi_Akkerm_DEU <- g2i_Akkerm_DEU$grobs[[6]]$children[[3]]$gp$fill
colorsi_Akkerm_DEU

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Akkerm_DEU2 <- c("#E6B682FF", "#E6B684FF", "#E8B379FF", "#EEAA5CFF",
                         "#CCCCCC", "#E5B787FF", "#EFA74FFF", "#EFA855FF", "#CCCCCC",
                         "#EBB06EFF", "#EDAC60FF", "#CCCCCC", "#EFA74FFF",
                         "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Akkerm_DEU,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Akkerm_DEU2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Akkerm_DEU2[idx]))
    p1i_Akkerm_DEU <- putPlot(p1i_Akkerm_DEU,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_8_Figure1_DEU_Relationship_Akkerm_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Akkerm_DEU
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_8_TIFF_Figure1_DEU_Relationship_Akkerm_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Akkerm_DEU
dev.off()



#### <<>> Figure S8-2 - Netherlands (LISS 2016 Panel Data, Study Number 151.3; Supplement 8 Figure 2 ) ####

#create quick subset for Dutch data only and, including just the variables needed for the plot:
df_plotsub1.i_Akkerm_NLD <- read.dta13("D:/Dropbox (Privat)/populism_concept and measurement/Data/Liss_touched.dta")

plotsub1.i_Akkerm <- c("antielite_sum_akker_z", "homogenous_sum_akker_z", "sovereignty_sum_akker_z",
                       "pop_3D_goertzian_akker_z", "pop_3D_bollen_akker_z", "pop_3D_man_sartori75_akker")
df_plotsub1.i_Akkerm_NLD <- df_plotsub1.i_Akkerm_NLD[plotsub1.i_Akkerm]

#Change names of columns: 
colnames(df_plotsub1.i_Akkerm_NLD) <- c("Anti-Elitism", "Manichean", "Sovereignty",
                                        "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Akkerm_NLD$Sartorian <- as.factor(df_plotsub1.i_Akkerm_NLD$Sartorian)
levels(df_plotsub1.i_Akkerm_NLD$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Akkerm_NLD <- ggpairs(df_plotsub1.i_Akkerm_NLD[!is.na(df_plotsub1.i_Akkerm_NLD$Sartorian),], 
                          lower = list(continuous = wrap("points", alpha = 0.05 )),
                          upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Akkerm_NLD <- ggcorr(df_plotsub1.i_Akkerm_NLD[!is.na(df_plotsub1.i_Akkerm_NLD$Sartorian),], 
                         label = TRUE, label_round = 2,
                         low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Akkerm_NLD <- ggplotGrob(p2i_Akkerm_NLD)
colorsi_Akkerm_NLD <- g2i_Akkerm_NLD$grobs[[6]]$children[[3]]$gp$fill
colorsi_Akkerm_NLD

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Akkerm_NLD2 <- c("#E9B275FF", "#E9B378FF", "#EBB06EFF", "#EFA955FF",
                         "#CCCCCC", "#E7B57FFF", "#EEAA59FF", "#EFA751FF", "#CCCCCC",
                         "#EEAA5CFF", "#EEAB5CFF", "#CCCCCC", "#F0A74EFF",
                         "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Akkerm_NLD,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Akkerm_NLD2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Akkerm_NLD2[idx]))
    p1i_Akkerm_NLD <- putPlot(p1i_Akkerm_NLD,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_8_Figure2_NLD_Relationship_Akkerm_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Akkerm_NLD
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_8_TIFF_Figure2_NLD_Relationship_Akkerm_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Akkerm_NLD
dev.off()


#### <<>> Figure S8-3 - UK Sample  (Silva et al Dataset - Akkerman et al Scale; Supplement 8, Figure 3) ####

#create quick subset for UK data only and, including just the variables needed for the plot:
df_plotsub1.i_Akkerm_UK <- dat_Silva[which(dat_Silva$country=="UK"),]

plotsub1.i_Akkerm <- c("antielite_sum_akker_z", "homogenous_sum_akker_z", "sovereignty_sum_akker_z",
                       "pop_3D_goertzian_akker_z", "pop_3D_bollen_akker_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Akkerm_UK <- df_plotsub1.i_Akkerm_UK[plotsub1.i_Akkerm]

#Change names of columns: 
colnames(df_plotsub1.i_Akkerm_UK) <- c("Anti-Elitism", "Manichean", "Sovereignty",
                                       "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Akkerm_UK$Sartorian <- as.factor(df_plotsub1.i_Akkerm_UK$Sartorian)
levels(df_plotsub1.i_Akkerm_UK$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Akkerm_UK <- ggpairs(df_plotsub1.i_Akkerm_UK[!is.na(df_plotsub1.i_Akkerm_UK$Sartorian),], 
                         lower = list(continuous = wrap("points", alpha = 0.1 )),
                         upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Akkerm_UK <- ggcorr(df_plotsub1.i_Akkerm_UK[!is.na(df_plotsub1.i_Akkerm_UK$Sartorian),], 
                        label = TRUE, label_round = 2,
                        low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Akkerm_UK <- ggplotGrob(p2i_Akkerm_UK)
colorsi_Akkerm_UK <- g2i_Akkerm_UK$grobs[[6]]$children[[3]]$gp$fill
colorsi_Akkerm_UK

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Akkerm_UK2 <- c("#D9C3AFFF", "#E4B88AFF", "#E4B98DFF", "#ECAE69FF" ,
                        "#CCCCCC", "#D9C3AFFF", "#EEAB5CFF", "#ECAD66FF", "#CCCCCC",
                        "#E4B98DFF", "#EBAF6BFF", "#CCCCCC", "#EEAA59FF",
                        "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Akkerm_UK,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Akkerm_UK2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Akkerm_UK2[idx]))
    p1i_Akkerm_UK <- putPlot(p1i_Akkerm_UK,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_8_Figure3_UK_Relationship_Akkerm_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Akkerm_UK
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_8_TIFF_Figure3_UK_Relationship_Akkerm_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Akkerm_UK
dev.off()

#### <<>> Figure S8-4 - United States Sample  (Silva et al Dataset; Supplement 8, Figure 4) ####

#create quick subset for US data only and, including just the variables needed for the plot:
df_plotsub1.i_Akkerm_US <- dat_Silva[which(dat_Silva$country=="US"),]

plotsub1.i_Akkerm <- c("antielite_sum_akker_z", "homogenous_sum_akker_z", "sovereignty_sum_akker_z",
                       "pop_3D_goertzian_akker_z", "pop_3D_bollen_akker_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Akkerm_US <- df_plotsub1.i_Akkerm_US[plotsub1.i_Akkerm]

#Change names of columns: 
colnames(df_plotsub1.i_Akkerm_US) <- c("Anti-Elitism", "Manichean", "Sovereignty",
                                       "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Akkerm_US$Sartorian <- as.factor(df_plotsub1.i_Akkerm_US$Sartorian)
levels(df_plotsub1.i_Akkerm_US$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Akkerm_US <- ggpairs(df_plotsub1.i_Akkerm_US[!is.na(df_plotsub1.i_Akkerm_US$Sartorian),], 
                         lower = list(continuous = wrap("points", alpha = 0.1 )),
                         upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Akkerm_US <- ggcorr(df_plotsub1.i_Akkerm_US[!is.na(df_plotsub1.i_Akkerm_US$Sartorian),], 
                        label = TRUE, label_round = 2,
                        low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Akkerm_US <- ggplotGrob(p2i_Akkerm_US)
colorsi_Akkerm_US <- g2i_Akkerm_US$grobs[[6]]$children[[3]]$gp$fill
colorsi_Akkerm_US

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Akkerm_US2 <- c("#DDBFA3FF", "#E6B683FF", "#E7B57FFF", "#EDAC62FF",
                        "#CCCCCC", "#DCC0A4FF", "#EDAB5FFF", "#ECAD65FF", "#CCCCCC",
                        "#E9B275FF", "#ECAD64FF", "#CCCCCC", "#EFA751FF",
                        "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Akkerm_US,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Akkerm_US2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Akkerm_US2[idx]))
    p1i_Akkerm_US <- putPlot(p1i_Akkerm_US,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/SUPPLEMENT_8_Figure4_US_Relationship_Akkerm_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Akkerm_US
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_8_TIFF_Figure4_US_Relationship_Akkerm_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Akkerm_US
dev.off()



#////////////////////////////////////////////////////////////////////////////////////////////////////////#
#### Step 3C - Supplement 9 - Re-Analysis for Schulz Scale with GLES Data                            ####
#### Figure S9-1
#////////////////////////////////////////////////////////////////////////////////////////////////////////#

# In this section, we replicate Figure 2 in the paper. However, rather than using the average sum score
# for the Populist Aggregation (Bollen), we use the higher order factor scores. This also implies that
# the single dimensions to arrive at this score are not aggregated by average sum scores but on the basis
# of weighted factor scores (See Step 1C). The final figure is Figure A5-1 in Supplement 5. 

#create quick subset:
plotsub1_sup8 <- c("antielite_sum_z", "homogenous_sum_z", "sovereignty_sum_z",
                   "pop_3D_man_goertzian_z", "Populism_3D_SEM_Schulz_HigherFactor", "pop_3D_man_sartori75_z")
df_plotsub1_sup8 <- df_Final[plotsub1_sup8]

#Ensure that Sartortori Variable is factor:
df_plotsub1_sup8$pop_3D_man_sartori75_z <- as.factor(df_plotsub1_sup8$pop_3D_man_sartori75_z)

#Add levels (value labels) to Sartori Variable:
levels(df_plotsub1_sup8$pop_3D_man_sartori75_z) <- c("No Pop.", "Pop.")

#############################################/
# Ggparis solution to plot:
#Link: https://stackoverflow.com/questions/45873483/ggpairs-plot-with-heatmap-of-correlation-values
#(Last accessed: Sept.28, 2018)

#ggpairs(df_plotsub1)

#Change names of columns: 
colnames(df_plotsub1_sup8) <- c("Anti-Elitism", "Homogeneity", "Sovereignty",
                                "Goertzian", "Bollen (HO)", "Sartorian")

# Matrix of plots
p1_sup8 <- ggpairs(df_plotsub1_sup8[!is.na(df_plotsub1_sup8$Sartorian),], 
                   lower = list(continuous = wrap("points", alpha = 0.01 )),
                   upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2_sup8 <- ggcorr(df_plotsub1_sup8, label = TRUE, label_round = 2,
                  low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2_sup8 <- ggplotGrob(p2_sup8)
colors_sup8 <- g2_sup8$grobs[[6]]$children[[3]]$gp$fill
colors_sup8

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colors2_sup8 <- c("#D9C3AFFF", "#E8B47CFF", "#EAB173FF", "#E5B787FF",
                  "#CCCCCC", "#E1BC98FF", "#EBAF6DFF", "#DFBE9EFF", "#CCCCCC",
                  "#EBAF6BFF", "#F1A444FF", "#CCCCCC", "#EAB173FF", "#CCCCCC",
                  "#CCCCCC")

# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1_sup8,k1,k2) +
      theme(panel.background = element_rect(fill = colors2_sup8[idx], color="white"),
            panel.grid.major = element_line(color=colors2_sup8[idx]))
    p1_sup8 <- putPlot(p1_sup8,plt,k1,k2)
    idx <- idx+1
  }
}
#print(p1)

#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/
png("##PATH##/Plots/Appendix/SUPPLEMENT_9_Figure1_GLES_PANEL_Schulz_Scale_Relationship_Aggregations_SingleDimensions.png",
    units="in", width=11.5, height=8.8, res=300)
p1_sup8 
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_9_TIFF_Figure1_GLES_PANEL_Schulz_Scale_Relationship_Aggregations_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1_sup8
dev.off()




#////////////////////////////////////////////////////////////////////////////////////////////////////////#
#### Step 3D - Supplement 12 Part I - Correlation with conspirational thinking                         ####
#### Figures 1-3
#////////////////////////////////////////////////////////////////////////////////////////////////////////#

# Comparing correlation between populist attitudes and its dimensions to conspirational thinking [Figures
# A12-1 to A12-3]

#### Silva Data - Correlation with Conspirational Thinking (CT) 

# Correlations by country: Goertz (1A) / Bollen (1B) / Antielitism (1C)/
#                          Manichean Outlook (1D) / People-Centrism (1E) &
#                          conspiracy thinking (derived from Silva Scale) (Silva Dataset)
by_country1A_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_man_goertzian_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1) %>%
  mutate(Aggregate = 1) %>%
  mutate(Type = 2)

by_country1B_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_man_bollen_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 2)%>%
  mutate(Type = 2)

by_country1C_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$antielite_sum_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 3)%>%
  mutate(Type = 1)

by_country1D_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$homogenous_sum_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 6)%>%
  mutate(Type = 1)


by_country1E_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$sovereignty_sum_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 7)%>%
  mutate(Type = 1)

#Note: All variables referring to "homogeneity" for the Silva Scale measure the "Manichean Outlook".
#      All items referring to "sovereignty" are called "people-centrism" in the original Silva scale.


# Correlations by country: Goertz (2A) / Bollen (2B)/ Anti-Elitism (2C) /
#                           National Affiliation (3D) / Mistrust in Expertise (3E)
#                          conspiracy (derived from
#                         Rahn Scale) (Silva Dataset)
by_country2A_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_goertzian_rahn_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 2)%>%
  mutate(Aggregate = 1)%>%
  mutate(Type = 2)

by_country2B_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_bollen_rahn_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 2)%>%
  mutate(Aggregate = 2)%>%
  mutate(Type = 2)

by_country2C_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$antielite_sum_rahn_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 2)%>%
  mutate(Aggregate = 3)%>%
  mutate(Type = 1)

by_country2D_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$homogenous_sum_rahn_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 2)%>%
  mutate(Aggregate = 9)%>%
  mutate(Type = 1)

by_country2E_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$sovereignty_sum_rahn_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 2)%>%
  mutate(Aggregate = 8)%>%
  mutate(Type = 1)


#Note: All variables referring to "homogeneity" for the Rahn Scale measure the "National Affiliation"
#      dimension in the original scale.
#      All items referring to "sovereignty" are called "Mistrust in Expertise" in the original Oliver/Rahn scale.

# Correlations by country: Goertz (3A) / Bollen (3B) / Anti-Elitism (3C) /
#                           Homogeneity (3D) & Sovereignty (3E) & conspiracy
#                          (derived from Schulz Scale) (Silva Dataset)
by_country3A_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_goertzian_schulz_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 3)%>%
  mutate(Aggregate = 1)%>%
  mutate(Type = 2)

by_country3B_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_bollen_schulz_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 3)%>%
  mutate(Aggregate = 2)%>%
  mutate(Type = 2)

by_country3C_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$antielite_sum_schulz_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 3)%>%
  mutate(Aggregate = 3)%>%
  mutate(Type = 1)

by_country3D_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$homogenous_sum_schulz_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 3)%>%
  mutate(Aggregate = 4)%>%
  mutate(Type = 1)

by_country3E_CT_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$sovereignty_sum_schulz_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 3)%>%
  mutate(Aggregate = 5)%>%
  mutate(Type = 1)

### Bind Dataset

dat_Silva_Plot_conspiracy_SilvaCor <- rbind(by_country1A_CT_silva, by_country1B_CT_silva,
                                            by_country1C_CT_silva, by_country1D_CT_silva,
                                            by_country1E_CT_silva, 
                                            by_country2A_CT_silva, by_country2B_CT_silva,
                                            by_country2C_CT_silva, by_country2D_CT_silva,
                                            by_country2E_CT_silva,
                                            by_country3A_CT_silva, by_country3B_CT_silva,
                                            by_country3C_CT_silva, by_country3D_CT_silva,
                                            by_country3E_CT_silva)

### Assign Levels to Scale & Aggregate Variables & Type:
dat_Silva_Plot_conspiracy_SilvaCor$Scale <- as.factor(dat_Silva_Plot_conspiracy_SilvaCor$Scale)
levels(dat_Silva_Plot_conspiracy_SilvaCor$Scale) <- c("Silva",
                                                      "Rahn",
                                                      "Schulz")

dat_Silva_Plot_conspiracy_SilvaCor$Aggregate <- as.factor(dat_Silva_Plot_conspiracy_SilvaCor$Aggregate)
levels(dat_Silva_Plot_conspiracy_SilvaCor$Aggregate) <- c("Goertz",
                                                          "Bollen",
                                                          "Anti-Elitism",
                                                          "Homogeneity",
                                                          "Sovereignty",
                                                          "Manichean Outlook",
                                                          "People-Centrism",
                                                          "Mistrust of Expertise",
                                                          "National Affiliation")

dat_Silva_Plot_conspiracy_SilvaCor$Type <- as.factor(dat_Silva_Plot_conspiracy_SilvaCor$Type)
levels(dat_Silva_Plot_conspiracy_SilvaCor$Type) <- c("Single Dimension", "Aggregation")

#### <<<>>> Plot S12-1 (Silva Scale) ####
p3a_Silva_sup <- ggplot(dat_Silva_Plot_conspiracy_SilvaCor[dat_Silva_Plot_conspiracy_SilvaCor$Scale=="Silva",],
                        aes(x=Aggregate, y=estimate, color = Type)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high), size=0.7) +
  geom_hline(yintercept=0, color='red', size=0.5)+
  facet_wrap( ~ country)+
  coord_flip()+
  xlab("")+
  scale_color_manual(values=c("#666666", "#000000"))+
  theme(legend.position="bottom")

#### <<<>>> Plot S12-2 (Oliver/Rahn Scale) ####
p3b_Silva_sup <- ggplot(dat_Silva_Plot_conspiracy_SilvaCor[dat_Silva_Plot_conspiracy_SilvaCor$Scale=="Rahn",],
                        aes(x=Aggregate, y=estimate, color = Type)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high), size=0.7) +
  geom_hline(yintercept=0, color='red', size=0.5)+
  facet_wrap( ~ country)+
  coord_flip()+
  xlab("")+
  scale_color_manual(values=c("#666666", "#000000"))+
  theme(legend.position="bottom")

#### <<<>>> Plot S12-3 (Schulz Scale) ####
p3c_Silva_sup <- ggplot(dat_Silva_Plot_conspiracy_SilvaCor[dat_Silva_Plot_conspiracy_SilvaCor$Scale=="Schulz",],
                        aes(x=Aggregate, y=estimate, color = Type)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high), size=0.7) +
  geom_hline(yintercept=0, color='red', size=0.5)+
  facet_wrap( ~ country)+
  coord_flip()+
  xlab("")+
  scale_color_manual(values=c("#666666", "#000000"))+
  theme(legend.position="bottom")

## Save the plots:
png("##PATH##/Plots/Appendix/SUPPLEMENT_12_Figure1_Correlations_Conspiracy_Silva_Data_Silva_Scale.png", units="in", width=7.5, height=5.8, res=300)
p3a_Silva_sup
dev.off()

png("##PATH##/Plots/Appendix/SUPPLEMENT_12_Figure2_Correlations_Conspiracy_Silva_Data_Rahn_Scale.png", units="in", width=7.5, height=5.8, res=300)
p3b_Silva_sup
dev.off()

png("##PATH##/Plots/Appendix/SUPPLEMENT_12_Figure3_Correlations_Conspiracy_Silva_Data_Schulz_Scale.png", units="in", width=7.5, height=5.8, res=300)
p3c_Silva_sup
dev.off()

#Tif Versions:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_12_Figure1_Correlations_Conspiracy_Silva_Data_Silva_Scale.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p3a_Silva_sup
dev.off()

tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_12_Figure2_Correlations_Conspiracy_Silva_Data_Rahn_Scale.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p3b_Silva_sup
dev.off()

tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_12_Figure3_Correlations_Conspiracy_Silva_Data_Schulz_Scale.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p3c_Silva_sup
dev.off()

#////////////////////////////////////////////////////////////////////////////////////////////////////////#
#### Step 3E - Supplement 12 Part II - Correlation with conspirational thinking (Akkermann Scale)      ####
#### Figures S12-4
#////////////////////////////////////////////////////////////////////////////////////////////////////////#

# Comparing correlation between populist attitudes and its dimensions to conspirational thinking [Figure A9-4]
# for the Akkermann Scale

#### --- Silva Data - Correlation with Conspirational Thinking (CT) 

# Correlations by country: Goertz (1A) / Bollen (1B) / Antielitism (1C)/
#                          Manichean Outlook (1D) / Sovereignty (1E) &
#                          Institutional Trust (derived from
#                         Akkerman et al Scale) (Silva Dataset)
by_country1A_CT_Akkerm <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_goertzian_akker_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Aggregate = 1) %>%
  mutate(Type = 2)

by_country1B_CT_Akkerm <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_bollen_akker_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Aggregate = 2)%>%
  mutate(Type = 2)

by_country1C_CT_Akkerm <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$antielite_sum_akker_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Aggregate = 3)%>%
  mutate(Type = 1)

by_country1D_CT_Akkerm <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$homogenous_sum_akker_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Aggregate = 4)%>%
  mutate(Type = 1)

by_country1E_CT_Akkerm <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$sovereignty_sum_akker_z,
                                .x$conspiracy)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Aggregate = 5)%>%
  mutate(Type = 1)


### Bind Dataset

dat_Silva_Plot_CTSilvaCor_Akkerm <- rbind(by_country1A_CT_Akkerm, by_country1B_CT_Akkerm,
                                          by_country1C_CT_Akkerm, by_country1D_CT_Akkerm,
                                          by_country1E_CT_Akkerm)

dat_Silva_Plot_CTSilvaCor_Akkerm$Aggregate <- as.factor(dat_Silva_Plot_CTSilvaCor_Akkerm$Aggregate)
levels(dat_Silva_Plot_CTSilvaCor_Akkerm$Aggregate) <- c("Goertz",
                                                        "Bollen",
                                                        "Anti-Elitism",
                                                        "Manichean Outlook",
                                                        "Sovereignty")

dat_Silva_Plot_CTSilvaCor_Akkerm$Type <- as.factor(dat_Silva_Plot_CTSilvaCor_Akkerm$Type)
levels(dat_Silva_Plot_CTSilvaCor_Akkerm$Type) <- c("Single Dimension", "Aggregation")

#### <<<>>> Plot S12-4 ####
p3a_Akkerm_CT <- ggplot(dat_Silva_Plot_CTSilvaCor_Akkerm,
                        aes(x=Aggregate, y=estimate, color = Type)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high), size=0.7) +
  geom_hline(yintercept=0, color='red', size=0.5)+
  facet_wrap( ~ country)+
  coord_flip()+
  xlab("")+
  scale_color_manual(values=c("#666666", "#000000")) +
  theme(legend.position="bottom")


## Save the plots:
png("##PATH##/Plots/Appendix/SUPPLEMENT_12_Figure4_Correlations_Conspiracy_byCountry_Akkerm_Scale.png", units="in", width=7.5, height=5.8, res=300)
p3a_Akkerm_CT
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_12_Figure4_Correlations_Conspiracy_byCountry_Akkerm_Scale.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p3a_Akkerm_CT
dev.off()


#////////////////////////////////////////////////////////////////////////////////////////////////////////#
#### Step 3F - Supplement 13 - (Bollen and Goertz) - Correlation with trust_institutions_silva        ####
#### Figures 1/2
#////////////////////////////////////////////////////////////////////////////////////////////////////////#

# Comparing correlation between populist attitudes and its dimensions to political
# trust for the Oliver/Rahn Scale and the Schulz Scale (Silva dataset)

#### --- Silva Data - Correlation with Trust Variable (all items)

## Note: for the Silva scale, variables referred to as "homogeneity" measure the manichean outlook. 
##       Variables labeled as "sovreignty" refer to the original scale dimension "people-centrism."

# Correlations by country: Goertz (2A) / Bollen (2B) / Antielitism (2C)/
#                          National Affiliation (1D) / Mistrust in Expertise (1E) &
#                          Institutional Trust (derived from
#                         Rahn Scale) (Silva Dataset)
by_country2A_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_goertzian_rahn_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 1)%>%
  mutate(Type = 2)

by_country2B_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_bollen_rahn_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 2)%>%
  mutate(Type = 2)

by_country2C_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$antielite_sum_rahn_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 3)%>%
  mutate(Type = 1)

by_country2D_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$homogenous_sum_rahn_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 4)%>%
  mutate(Type = 1)

by_country2E_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$sovereignty_sum_rahn_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 1)%>%
  mutate(Aggregate = 5)%>%
  mutate(Type = 1)

## Note: for the Rahn scale, variables referred to as "homogeneity" measure the national affiliation. 
##       Variables labeled as "sovereignty" refer to the original scale dimension "mistrust of expertise."


# Correlations by country: Goertz (3A) / Bollen (3B) & Institutional
#                          Trust (derived from Schulz Scale) (Silva Dataset)
by_country3A_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_goertzian_schulz_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 2)%>%
  mutate(Aggregate = 1)%>%
  mutate(Type = 2)

by_country3B_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$pop_3D_bollen_schulz_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 2)%>%
  mutate(Aggregate = 2)%>%
  mutate(Type = 2)

by_country3C_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$antielite_sum_schulz_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 2)%>%
  mutate(Aggregate = 3)%>%
  mutate(Type = 1)

by_country3D_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$homogenous_sum_schulz_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 2)%>%
  mutate(Aggregate = 6)%>%
  mutate(Type = 1)

by_country3E_trust_silva <- dat_Silva %>% 
  nest(-country) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$sovereignty_sum_schulz_z,
                                .x$trust_institutions_silva)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)%>%
  mutate(Scale = 2)%>%
  mutate(Aggregate = 7)%>%
  mutate(Type = 1)

### Bind Dataset

dat_Silva_Plot_TrustSilvaCor <- rbind(by_country2A_trust_silva, by_country2B_trust_silva,
                                      by_country2C_trust_silva, by_country2D_trust_silva,
                                      by_country2E_trust_silva,
                                      by_country3A_trust_silva, by_country3B_trust_silva,
                                      by_country3C_trust_silva, by_country3D_trust_silva,
                                      by_country3E_trust_silva)

### Assign Levels to Scale & Aggregate Variables & Type:
dat_Silva_Plot_TrustSilvaCor$Scale <- as.factor(dat_Silva_Plot_TrustSilvaCor$Scale)
levels(dat_Silva_Plot_TrustSilvaCor$Scale) <- c("Rahn",
                                                "Schulz")

dat_Silva_Plot_TrustSilvaCor$Aggregate <- as.factor(dat_Silva_Plot_TrustSilvaCor$Aggregate)
levels(dat_Silva_Plot_TrustSilvaCor$Aggregate) <- c("Goertz",
                                                    "Bollen",
                                                    "Anti-Elitism",
                                                    "National Affiliation",
                                                    "Mistrust of Expertise",
                                                    "Homogeneity",
                                                    "Sovereignty")

dat_Silva_Plot_TrustSilvaCor$Type <- as.factor(dat_Silva_Plot_TrustSilvaCor$Type)
levels(dat_Silva_Plot_TrustSilvaCor$Type) <- c("Single Dimension", "Aggregation")

#### <<<>>> Plot S13.1 (Oliver/Rahn Scale) ####
p3b_Silva <- ggplot(dat_Silva_Plot_TrustSilvaCor[dat_Silva_Plot_TrustSilvaCor$Scale=="Rahn",],
                    aes(x=Aggregate, y=estimate, color = Type)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high), size=0.7) +
  geom_hline(yintercept=0, color='red', size=0.5)+
  facet_wrap( ~ country)+
  coord_flip()+
  xlab("")+
  scale_color_manual(values=c("#666666", "#000000")) +
  theme(legend.position="bottom")

#### <<<>>> Plot S13.2 (Schulz et al. Scale) ####
p3c_Silva <- ggplot(dat_Silva_Plot_TrustSilvaCor[dat_Silva_Plot_TrustSilvaCor$Scale=="Schulz",],
                    aes(x=Aggregate, y=estimate, color = Type)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high), size=0.7) +
  geom_hline(yintercept=0, color='red', size=0.5)+
  facet_wrap( ~ country)+
  coord_flip()+
  xlab("")+
  scale_color_manual(values=c("#666666", "#000000")) +
  theme(legend.position="bottom")

## Save the plots:

png("##PATH##/Plots/Appendix/SUPPLEMENT_13_Figure1_Correlations_Trust_Silva_Items_MTURK_Rahn_Scale.png", units="in", width=7.5, height=5.8, res=300)
p3b_Silva
dev.off()


png("##PATH##t/Plots/Appendix/SUPPLEMENT_13_Figure2_Correlations_Trust_Silva_Items_MTURK_Schulz_Scale.png", units="in", width=7.5, height=5.8, res=300)
p3c_Silva
dev.off()

#Tif Versions:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_13_Figure1_Correlations_Trust_Silva_Items_MTURK_Rahn_Scale.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p3b_Silva
dev.off()

tiff("##PATH##/Plots/TIFF/Appendix TIFF/SUPPLEMENT_13_Figure2_Correlations_Trust_Silva_Items_MTURK_Schulz_Scale.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p3c_Silva
dev.off()


################################################/
### Step 3G - Additional Plots for Shiny App ####
################################################/

## This section contains all the additional plots added to the Shiny App (focusing all on 
## the internal structure of the scale). 

#### ))) 3F-a - Shiny Plots - Internal Structure of Akkerman Scale ####


#### <<>>  Figure Shiny1 - Brazil Sample (Silva et al Dataset - Akkerman et al Scale) ####

#create quick subset for Brazil data only and, including just the variables needed for the plot:
df_plotsub1.i_Akkerm_Brazil <- dat_Silva[which(dat_Silva$country=="Brazil"),]

plotsub1.i_Akkerm <- c("antielite_sum_akker_z", "homogenous_sum_akker_z", "sovereignty_sum_akker_z",
                       "pop_3D_goertzian_akker_z", "pop_3D_bollen_akker_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Akkerm_Brazil <- df_plotsub1.i_Akkerm_Brazil[plotsub1.i_Akkerm]

#Change names of columns: 
colnames(df_plotsub1.i_Akkerm_Brazil) <- c("Anti-Elitism", "Manichean", "Sovereignty",
                                           "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Akkerm_Brazil$Sartorian <- as.factor(df_plotsub1.i_Akkerm_Brazil$Sartorian)
levels(df_plotsub1.i_Akkerm_Brazil$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Akkerm_Brazil <- ggpairs(df_plotsub1.i_Akkerm_Brazil[!is.na(df_plotsub1.i_Akkerm_Brazil$Sartorian),], 
                             lower = list(continuous = wrap("points", alpha = 0.1 )),
                             upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Akkerm_Brazil <- ggcorr(df_plotsub1.i_Akkerm_Brazil[!is.na(df_plotsub1.i_Akkerm_Brazil$Sartorian),], 
                            label = TRUE, label_round = 2,
                            low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Akkerm_Brazil <- ggplotGrob(p2i_Akkerm_Brazil)
colorsi_Akkerm_Brazil <- g2i_Akkerm_Brazil$grobs[[6]]$children[[3]]$gp$fill
colorsi_Akkerm_Brazil

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Akkerm_Brazil2 <- c("#E3BA90FF", "#E6B684FF", "#E8B47DFF", "#EEAB5DFF" ,
                            "#CCCCCC", "#E1BC97FF", "#EEA957FF", "#EDAB5DFF", "#CCCCCC",
                            "#EAB174FF", "#EDAC62FF", "#CCCCCC", "#EFA852FF",
                            "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Akkerm_Brazil,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Akkerm_Brazil2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Akkerm_Brazil2[idx]))
    p1i_Akkerm_Brazil <- putPlot(p1i_Akkerm_Brazil,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny1_Brazil_Relationship_Akkerm_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Akkerm_Brazil + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (Brazil)",
                         caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#TIF Version
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny1_Brazil_Relationship_Akkerm_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Akkerm_Brazil + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (Brazil)",
                         caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>>  Figure Shiny2 - France Sample (Silva et al Dataset - Akkerman et al Scale) ####

#create quick subset for France data only and, including just the variables needed for the plot:
df_plotsub1.i_Akkerm_France <- dat_Silva[which(dat_Silva$country=="France"),]

plotsub1.i_Akkerm <- c("antielite_sum_akker_z", "homogenous_sum_akker_z", "sovereignty_sum_akker_z",
                       "pop_3D_goertzian_akker_z", "pop_3D_bollen_akker_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Akkerm_France <- df_plotsub1.i_Akkerm_France[plotsub1.i_Akkerm]

#Change names of columns: 
colnames(df_plotsub1.i_Akkerm_France) <- c("Anti-Elitism", "Manichean", "Sovereignty",
                                           "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Akkerm_France$Sartorian <- as.factor(df_plotsub1.i_Akkerm_France$Sartorian)
levels(df_plotsub1.i_Akkerm_France$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Akkerm_France <- ggpairs(df_plotsub1.i_Akkerm_France[!is.na(df_plotsub1.i_Akkerm_France$Sartorian),], 
                             lower = list(continuous = wrap("points", alpha = 0.1 )),
                             upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Akkerm_France <- ggcorr(df_plotsub1.i_Akkerm_France[!is.na(df_plotsub1.i_Akkerm_France$Sartorian),], 
                            label = TRUE, label_round = 2,
                            low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Akkerm_France <- ggplotGrob(p2i_Akkerm_France)
colorsi_Akkerm_France <- g2i_Akkerm_France$grobs[[6]]$children[[3]]$gp$fill
colorsi_Akkerm_France

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Akkerm_France2 <- c("#DDBFA2FF", "#E9B275FF", "#E7B580FF", "#EDAC61FF" ,
                            "#CCCCCC", "#DDBFA2FF", "#EDAB5DFF", "#EDAD63FF", "#CCCCCC",
                            "#E9B378FF", "#EDAC61FF", "#CCCCCC", "#EFA854FF",
                            "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Akkerm_France,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Akkerm_France2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Akkerm_France2[idx]))
    p1i_Akkerm_France <- putPlot(p1i_Akkerm_France,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny2_France_Relationship_Akkerm_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Akkerm_France + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (France)",
                         caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

# TIF Version
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny2_France_Relationship_Akkerm_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Akkerm_France + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (France)",
                         caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>> Figure Shiny 3 - Greece Sample (Silva et al Dataset - Akkerman et al Scale) ####

#create quick subset for Greece data only and, including just the variables needed for the plot:
df_plotsub1.i_Akkerm_Greece <- dat_Silva[which(dat_Silva$country=="Greece"),]

plotsub1.i_Akkerm <- c("antielite_sum_akker_z", "homogenous_sum_akker_z", "sovereignty_sum_akker_z",
                       "pop_3D_goertzian_akker_z", "pop_3D_bollen_akker_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Akkerm_Greece <- df_plotsub1.i_Akkerm_Greece[plotsub1.i_Akkerm]

#Change names of columns: 
colnames(df_plotsub1.i_Akkerm_Greece) <- c("Anti-Elitism", "Manichean", "Sovereignty",
                                           "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Akkerm_Greece$Sartorian <- as.factor(df_plotsub1.i_Akkerm_Greece$Sartorian)
levels(df_plotsub1.i_Akkerm_Greece$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Akkerm_Greece <- ggpairs(df_plotsub1.i_Akkerm_Greece[!is.na(df_plotsub1.i_Akkerm_Greece$Sartorian),], 
                             lower = list(continuous = wrap("points", alpha = 0.1 )),
                             upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Akkerm_Greece <- ggcorr(df_plotsub1.i_Akkerm_Greece[!is.na(df_plotsub1.i_Akkerm_Greece$Sartorian),], 
                            label = TRUE, label_round = 2,
                            low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Akkerm_Greece <- ggplotGrob(p2i_Akkerm_Greece)
colorsi_Akkerm_Greece <- g2i_Akkerm_Greece$grobs[[6]]$children[[3]]$gp$fill
colorsi_Akkerm_Greece

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Akkerm_Greece2 <- c("#E3BA90FF", "#E3B98EFF", "#E7B47DFF", "#EDAC61FF" ,
                            "#CCCCCC", "#E3BA90FF", "#EEA958FF", "#EEAA5AFF", "#CCCCCC",
                            "#EAB174FF", "#EDAC63FF", "#CCCCCC", "#EFA852FF",
                            "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Akkerm_Greece,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Akkerm_Greece2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Akkerm_Greece2[idx]))
    p1i_Akkerm_Greece <- putPlot(p1i_Akkerm_Greece,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny3_Greece_Relationship_Akkerm_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Akkerm_Greece + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (Greece)",
                         caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

# TIF Version
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny3_Greece_Relationship_Akkerm_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Akkerm_Greece + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (Greece)",
                         caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>> Figure Shiny 4 - Ireland Sample (Silva et al Dataset - Akkerman et al Scale) ####

#create quick subset for Ireland data only and, including just the variables needed for the plot:
df_plotsub1.i_Akkerm_Ireland <- dat_Silva[which(dat_Silva$country=="Ireland"),]

plotsub1.i_Akkerm <- c("antielite_sum_akker_z", "homogenous_sum_akker_z", "sovereignty_sum_akker_z",
                       "pop_3D_goertzian_akker_z", "pop_3D_bollen_akker_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Akkerm_Ireland <- df_plotsub1.i_Akkerm_Ireland[plotsub1.i_Akkerm]

#Change names of columns: 
colnames(df_plotsub1.i_Akkerm_Ireland) <- c("Anti-Elitism", "Manichean", "Sovereignty",
                                            "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Akkerm_Ireland$Sartorian <- as.factor(df_plotsub1.i_Akkerm_Ireland$Sartorian)
levels(df_plotsub1.i_Akkerm_Ireland$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Akkerm_Ireland <- ggpairs(df_plotsub1.i_Akkerm_Ireland[!is.na(df_plotsub1.i_Akkerm_Ireland$Sartorian),], 
                              lower = list(continuous = wrap("points", alpha = 0.1 )),
                              upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Akkerm_Ireland <- ggcorr(df_plotsub1.i_Akkerm_Ireland[!is.na(df_plotsub1.i_Akkerm_Ireland$Sartorian),], 
                             label = TRUE, label_round = 2,
                             low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Akkerm_Ireland <- ggplotGrob(p2i_Akkerm_Ireland)
colorsi_Akkerm_Ireland <- g2i_Akkerm_Ireland$grobs[[6]]$children[[3]]$gp$fill
colorsi_Akkerm_Ireland

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Akkerm_Ireland2 <- c("#E2BB92FF", "#E6B684FF", "#E8B47DFF", "#EEAB5DFF"  ,
                             "#CCCCCC", "#DDC0A3FF", "#EDAB5EFF", "#EDAC60FF", "#CCCCCC",
                             "#E9B378FF", "#ECAE67FF", "#CCCCCC", "#EFA854FF",
                             "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Akkerm_Ireland,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Akkerm_Ireland2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Akkerm_Ireland2[idx]))
    p1i_Akkerm_Ireland <- putPlot(p1i_Akkerm_Ireland,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny4_Ireland_Relationship_Akkerm_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Akkerm_Ireland + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (Ireland)",
                          caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

# TIF Version
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny4_Ireland_Relationship_Akkerm_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Akkerm_Ireland + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (Ireland)",
                          caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>> Figure Shiny 5 - Italy Sample (Silva et al Dataset - Akkerman et al Scale) ####

#create quick subset for Italy data only and, including just the variables needed for the plot:
df_plotsub1.i_Akkerm_Italy <- dat_Silva[which(dat_Silva$country=="Italy"),]

plotsub1.i_Akkerm <- c("antielite_sum_akker_z", "homogenous_sum_akker_z", "sovereignty_sum_akker_z",
                       "pop_3D_goertzian_akker_z", "pop_3D_bollen_akker_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Akkerm_Italy <- df_plotsub1.i_Akkerm_Italy[plotsub1.i_Akkerm]

#Change names of columns: 
colnames(df_plotsub1.i_Akkerm_Italy) <- c("Anti-Elitism", "Manichean", "Sovereignty",
                                          "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Akkerm_Italy$Sartorian <- as.factor(df_plotsub1.i_Akkerm_Italy$Sartorian)
levels(df_plotsub1.i_Akkerm_Italy$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Akkerm_Italy <- ggpairs(df_plotsub1.i_Akkerm_Italy[!is.na(df_plotsub1.i_Akkerm_Italy$Sartorian),], 
                            lower = list(continuous = wrap("points", alpha = 0.1 )),
                            upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Akkerm_Italy <- ggcorr(df_plotsub1.i_Akkerm_Italy[!is.na(df_plotsub1.i_Akkerm_Italy$Sartorian),], 
                           label = TRUE, label_round = 2,
                           low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Akkerm_Italy <- ggplotGrob(p2i_Akkerm_Italy)
colorsi_Akkerm_Italy <- g2i_Akkerm_Italy$grobs[[6]]$children[[3]]$gp$fill
colorsi_Akkerm_Italy

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Akkerm_Italy2 <- c("#E4B88BFF", "#E6B684FF", "#E6B682FF", "#EEAB5CFF" ,
                           "#CCCCCC", "#E1BB95FF", "#EEA957FF", "#EEAA5CFF", "#CCCCCC",
                           "#EAB172FF", "#EDAC62FF", "#CCCCCC", "#EFA854FF",
                           "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Akkerm_Italy,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Akkerm_Italy2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Akkerm_Italy2[idx]))
    p1i_Akkerm_Italy <- putPlot(p1i_Akkerm_Italy,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny5_Italy_Relationship_Akkerm_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Akkerm_Italy + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (Italy)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

# TIF Version
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny5_Italy_Relationship_Akkerm_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Akkerm_Italy + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (Italy)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>> Figure Shiny 6 - Mexican Sample (Silva et al Dataset - Akkerman et al Scale) ####

#create quick subset for Mexico data only and, including just the variables needed for the plot:
df_plotsub1.i_Akkerm_Mexico <- dat_Silva[which(dat_Silva$country=="Mexico"),]

plotsub1.i_Akkerm <- c("antielite_sum_akker_z", "homogenous_sum_akker_z", "sovereignty_sum_akker_z",
                       "pop_3D_goertzian_akker_z", "pop_3D_bollen_akker_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Akkerm_Mexico <- df_plotsub1.i_Akkerm_Mexico[plotsub1.i_Akkerm]

#Change names of columns: 
colnames(df_plotsub1.i_Akkerm_Mexico) <- c("Anti-Elitism", "Manichean", "Sovereignty",
                                           "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Akkerm_Mexico$Sartorian <- as.factor(df_plotsub1.i_Akkerm_Mexico$Sartorian)
levels(df_plotsub1.i_Akkerm_Mexico$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Akkerm_Mexico <- ggpairs(df_plotsub1.i_Akkerm_Mexico[!is.na(df_plotsub1.i_Akkerm_Mexico$Sartorian),], 
                             lower = list(continuous = wrap("points", alpha = 0.1 )),
                             upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Akkerm_Mexico <- ggcorr(df_plotsub1.i_Akkerm_Mexico[!is.na(df_plotsub1.i_Akkerm_Mexico$Sartorian),], 
                            label = TRUE, label_round = 2,
                            low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Akkerm_Mexico <- ggplotGrob(p2i_Akkerm_Mexico)
colorsi_Akkerm_Mexico <- g2i_Akkerm_Mexico$grobs[[6]]$children[[3]]$gp$fill
colorsi_Akkerm_Mexico

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Akkerm_Mexico2 <- c("#DBC1A7FF", "#E7B57FFF", "#E4B88BFF", "#ECAD65FF" ,
                            "#CCCCCC", "#DFBD9BFF", "#EFA956FF", "#ECAD64FF", "#CCCCCC",
                            "#E8B47BFF", "#EDAB5EFF", "#CCCCCC", "#EFA955FF",
                            "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Akkerm_Mexico,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Akkerm_Mexico2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Akkerm_Mexico2[idx]))
    p1i_Akkerm_Mexico <- putPlot(p1i_Akkerm_Mexico,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny6_Mexico_Relationship_Akkerm_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Akkerm_Mexico + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (Mexico)",
                         caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny6_Mexico_Relationship_Akkerm_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Akkerm_Mexico + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (Mexico)",
                         caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 7 - Spain Sample  (Silva et al Dataset - Akkerman et al Scale) ####

#create quick subset for Spain data only and, including just the variables needed for the plot:
df_plotsub1.i_Akkerm_Spain <- dat_Silva[which(dat_Silva$country=="Spain"),]

plotsub1.i_Akkerm <- c("antielite_sum_akker_z", "homogenous_sum_akker_z", "sovereignty_sum_akker_z",
                       "pop_3D_goertzian_akker_z", "pop_3D_bollen_akker_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Akkerm_Spain <- df_plotsub1.i_Akkerm_Spain[plotsub1.i_Akkerm]

#Change names of columns: 
colnames(df_plotsub1.i_Akkerm_Spain) <- c("Anti-Elitism", "Manichean", "Sovereignty",
                                          "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Akkerm_Spain$Sartorian <- as.factor(df_plotsub1.i_Akkerm_Spain$Sartorian)
levels(df_plotsub1.i_Akkerm_Spain$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Akkerm_Spain <- ggpairs(df_plotsub1.i_Akkerm_Spain[!is.na(df_plotsub1.i_Akkerm_Spain$Sartorian),], 
                            lower = list(continuous = wrap("points", alpha = 0.1 )),
                            upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Akkerm_Spain <- ggcorr(df_plotsub1.i_Akkerm_Spain[!is.na(df_plotsub1.i_Akkerm_Spain$Sartorian),],
                           label = TRUE, label_round = 2,
                           low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Akkerm_Spain <- ggplotGrob(p2i_Akkerm_Spain)
colorsi_Akkerm_Spain <- g2i_Akkerm_Spain$grobs[[6]]$children[[3]]$gp$fill
colorsi_Akkerm_Spain

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Akkerm_Spain2 <- c("#D1C9C1FF", "#E5B787FF", "#E1BC97FF", "#EAB06FFF" ,
                           "#CCCCCC", "#DCC1A6FF", "#EEAB5DFF", "#EBAF6CFF", "#CCCCCC",
                           "#E5B788FF", "#EDAC61FF", "#CCCCCC", "#EEAA5BFF",
                           "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Akkerm_Spain,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Akkerm_Spain2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Akkerm_Spain2[idx]))
    p1i_Akkerm_Spain <- putPlot(p1i_Akkerm_Spain,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny7_Spain_Relationship_Akkerm_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Akkerm_Spain + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (Spain)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off() 

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny7_Spain_Relationship_Akkerm_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Akkerm_Spain + labs(title = "Akkerman et al. Scale - Dimensions and Aggregates (Spain)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 8 - United States  (ANES Dataset - CSES Populism Scale) ####

## >>> Load dataset:

ANES_2017PostElection <- read.dta13("##PATH##/Data/anes_touched.dta") 

## The following code generates Figure 2 in the paper:

#create quick subset containing only the variables to be included in the Figure:
plotsub3gd <- c("antielite_sum_cses_z", "sovereignty_sum_cses_z",
                "pop_3D_goertzian_cses_z", "pop_3D_bollen_cses_z", 
                "pop_3D_sartori75_cses_z")
df_plotsub3gd <- ANES_2017PostElection[plotsub3gd]

#Ensure that Sartortori Variable is factor (because it is a dichotomy):
df_plotsub3gd$pop_3D_sartori75_cses_z <- as.factor(df_plotsub3gd$pop_3D_sartori75_cses_z)

#Add levels (value labels) to Sartori Variable:
# 0. "No Pop." - Respondent is not a populist according to Sartori aggregation
# 1. "Pop." - Respondent is a populist according to Sartori aggregation
levels(df_plotsub3gd$pop_3D_sartori75_cses_z) <- c("No Pop.", "Pop.")

#############################################/
# Ggparis solution to plot:
#Link: https://stackoverflow.com/questions/45873483/ggpairs-plot-with-heatmap-of-correlation-values
#(Last accessed: Sept.28, 2018)

#ggpairs(df_plotsub1)

#Change names of columns: 
colnames(df_plotsub3gd) <- c("Anti-Elitism", "Challenges to repr. democ.",
                             "Goertzian", "Bollen", "Sartorian")

# Matrix of plots
p1_3gd <- ggpairs(df_plotsub3gd[!is.na(df_plotsub3gd$Sartorian),], 
                  lower = list(continuous = wrap("points", alpha = 0.1 )),
                  upper = list(continuous = wrap (mycor, sgnf=2)))

# Correlation matrix plot
p2_3gd <- ggcorr(df_plotsub3gd, label = TRUE, label_round = 2, 
                 low = "#998ec3", mid="#CCCCCC", high = "#f1a340") 
# https://www.rdocumentation.org/packages/GGally/versions/1.4.0/topics/ggcorr
# last accessed: November 20, 2018

# Extract the list of colors from the correlation matrix plot:
g2_3gd <- ggplotGrob(p2_3gd)
colors <- g2_3gd$grobs[[6]]$children[[3]]$gp$fill
colors

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colors2_3Gd <- c("#E0BD9AFF", "#EBAF6CFF", "#EEAA5AFF" ,
                 "#CCCCCC", "#EEAA5CFF", "#EEAA5BFF" , "#CCCCCC",
                 "#F0A64CFF", "#CCCCCC", "#CCCCCC")

# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 5
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1_3gd,k1,k2)  +
      theme(panel.background = element_rect(fill = colors2_3Gd[idx], color="white"),
            panel.grid.major = element_line(color=colors2_3Gd[idx]))
    p1_3gd <- putPlot(p1_3gd,plt,k1,k2) 
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny8_ANES_POSTELECTION_CSES_Scale_Relationship_Aggregations_SingleDimensions.png",
    units="in", width=11.5, height=8.8, res=300)
p1_3gd  +  labs(title = "CSES Scale - Dimensions and Aggregates (US 2016)",
       caption = "Data source: ANES 2016")
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny8_ANES_POSTELECTION_CSES_Scale_Relationship_Aggregations_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1_3gd  + labs(title = "CSES Scale - Dimensions and Aggregates (US 2016)",
               caption = "Data source: ANES 2016")
dev.off()


#### <<>> Figure Shiny 9 - Austria  (AUTNES 2017 Dataset - CSES Populism Scale) ####

## >>> Load dataset:

AUTNES_2017PostElection <- read.dta13("##PATH##/Data/autnes_touched.dta") 

## The following code generates Figure 2 in the paper:

#create quick subset containing only the variables to be included in the Figure:
plotsub3gc <- c("antielite_sum_cses_z", "sovereignty_sum_cses_z",
                "pop_3D_goertzian_cses_z", "pop_3D_bollen_cses_z", 
                "pop_3D_sartori75_cses_z")
df_plotsub3gc <- AUTNES_2017PostElection[plotsub3gc]

#Ensure that Sartortori Variable is factor (because it is a dichotomy):
df_plotsub3gc$pop_3D_sartori75_cses_z <- as.factor(df_plotsub3gc$pop_3D_sartori75_cses_z)

#Add levels (value labels) to Sartori Variable:
# 0. "No Pop." - Respondent is not a populist according to Sartori aggregation
# 1. "Pop." - Respondent is a populist according to Sartori aggregation
levels(df_plotsub3gc$pop_3D_sartori75_cses_z) <- c("No Pop.", "Pop.")

#############################################/
# Ggparis solution to plot:
#Link: https://stackoverflow.com/questions/45873483/ggpairs-plot-with-heatmap-of-correlation-values
#(Last accessed: Sept.28, 2018)

#ggpairs(df_plotsub1)

#Change names of columns: 
colnames(df_plotsub3gc) <- c("Anti-Elitism", "Challenges to repr. democ.",
                             "Goertzian", "Bollen", "Sartorian")

# Matrix of plots
p1_3gc <- ggpairs(df_plotsub3gc[!is.na(df_plotsub3gc$Sartorian),], 
                  lower = list(continuous = wrap("points", alpha = 0.1 )),
                  upper = list(continuous = wrap (mycor, sgnf=2)))

# Correlation matrix plot
p2_3gc <- ggcorr(df_plotsub3gc, label = TRUE, label_round = 2, 
                 low = "#998ec3", mid="#CCCCCC", high = "#f1a340") 
# https://www.rdocumentation.org/packages/GGally/versions/1.4.0/topics/ggcorr
# last accessed: November 20, 2018

# Extract the list of colors from the correlation matrix plot:
g2_3gc <- ggplotGrob(p2_3gc)
colors <- g2_3gc$grobs[[6]]$children[[3]]$gp$fill
colors

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colors2_3Gc <- c("#E7B581FF", "#EFA852FF", "#EFA750FF",
                 "#CCCCCC", "#EDAC61FF", "#EFA956FF" , "#CCCCCC",
                 "#F0A548FF", "#CCCCCC", "#CCCCCC")

# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 5
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1_3gc,k1,k2) +
      theme(panel.background = element_rect(fill = colors2_3Gc[idx], color="white"),
            panel.grid.major = element_line(color=colors2_3Gc[idx]))
    p1_3gc <- putPlot(p1_3gc,plt,k1,k2) 
    idx <- idx+1
  }
}
#print(p1_3ga)

#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny9_AUTNES_POSTELECTION_CSES_Scale_Relationship_Aggregations_SingleDimensions.png",
    units="in", width=11.5, height=8.8, res=300)
p1_3gc  + labs(title = "CSES Scale - Dimensions and Aggregates (Austria 2017)",
                caption = "Data source: AUTNES 2017")
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny9_AUTNES_POSTELECTION_CSES_Scale_Relationship_Aggregations_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1_3gc + labs(title = "CSES Scale - Dimensions and Aggregates (Austria 2017)",
                caption = "Data source: AUTNES 2017")
dev.off()


#### <<>> Figure Shiny 10 - United Kingdom  (BES 2017 Dataset - CSES Populism Scale) ####

## >>> Load dataset:

BES_2017PostElection <- read.dta13("##PATH##/Data/bes_touched.dta") 

## The following code generates Figure 2 in the paper:

#create quick subset containing only the variables to be included in the Figure:
plotsub3gb <- c("antielite_sum_cses_z", "sovereignty_sum_cses_z",
                "pop_3D_goertzian_cses_z", "pop_3D_bollen_cses_z", 
                "pop_3D_sartori75_cses_z")
df_plotsub3gb <- BES_2017PostElection[plotsub3gb]

#Ensure that Sartortori Variable is factor (because it is a dichotomy):
df_plotsub3gb$pop_3D_sartori75_cses_z <- as.factor(df_plotsub3gb$pop_3D_sartori75_cses_z)

#Add levels (value labels) to Sartori Variable:
# 0. "No Pop." - Respondent is not a populist according to Sartori aggregation
# 1. "Pop." - Respondent is a populist according to Sartori aggregation
levels(df_plotsub3gb$pop_3D_sartori75_cses_z) <- c("No Pop.", "Pop.")

#############################################/
# Ggparis solution to plot:
#Link: https://stackoverflow.com/questions/45873483/ggpairs-plot-with-heatmap-of-correlation-values
#(Last accessed: Sept.28, 2018)

#ggpairs(df_plotsub1)

#Change names of columns: 
colnames(df_plotsub3gb) <- c("Anti-Elitism", "Challenges to repr. democ.",
                             "Goertzian", "Bollen", "Sartorian")

# Matrix of plots
p1_3gb <- ggpairs(df_plotsub3gb[!is.na(df_plotsub3gb$Sartorian),], 
                  lower = list(continuous = wrap("points", alpha = 0.1 )),
                  upper = list(continuous = wrap (mycor, sgnf=2)))

# Correlation matrix plot
p2_3gb <- ggcorr(df_plotsub3gb, label = TRUE, label_round = 2, 
                 low = "#998ec3", mid="#CCCCCC", high = "#f1a340") 
# https://www.rdocumentation.org/packages/GGally/versions/1.4.0/topics/ggcorr
# last accessed: November 20, 2018

# Extract the list of colors from the correlation matrix plot:
g2_3gb <- ggplotGrob(p2_3gb)
colors <- g2_3gb$grobs[[6]]$children[[3]]$gp$fill
colors

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colors2_3Gb <- c("#E5B786FF", "#EDAC63FF", "#EFA956FF",
                 "#CCCCCC", "#EFA854FF", "#EFA853FF", "#CCCCCC",
                 "#F0A549FF", "#CCCCCC", "#CCCCCC")

# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 5
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1_3gb,k1,k2) +
      theme(panel.background = element_rect(fill = colors2_3Gb[idx], color="white"),
            panel.grid.major = element_line(color=colors2_3Gb[idx]))
    p1_3gb <- putPlot(p1_3gb,plt,k1,k2) 
    idx <- idx+1
  }
}
#print(p1_3ga)

#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/
png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny10_BES_POSTELECTION_CSES_Scale_Relationship_Aggregations_SingleDimensions.png",
    units="in", width=11.5, height=8.8, res=300)
p1_3gb + labs(title = "CSES Scale - Dimensions and Aggregates (UK 2017)",
                caption = "Data source: BES 2017")
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny10_BES_POSTELECTION_CSES_Scale_Relationship_Aggregations_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1_3gb + labs(title = "CSES Scale - Dimensions and Aggregates (UK 2017)",
              caption = "Data source: BES 2017")
dev.off()


#### <<>> Figure Shiny 11 - Germany  (GLES 2017 Dataset - CSES Populism Scale) ####


## >>> Load dataset:

gles2017PostElection <- read.dta13("##PATH##/Data/gles_touched.dta") 

## The following code generates Figure XYZ in the paper:

#create quick subset containing only the variables to be included in the Figure:
plotsub3ga <- c("antielite_sum_cses_z", "sovereignty_sum_cses_z",
                "pop_3D_goertzian_cses_z", "pop_3D_bollen_cses_z", 
                "pop_3D_sartori75_cses_z")
df_plotsub3ga <- gles2017PostElection[plotsub3ga]

#Ensure that Sartortori Variable is factor (because it is a dichotomy):
df_plotsub3ga$pop_3D_sartori75_cses_z <- as.factor(df_plotsub3ga$pop_3D_sartori75_cses_z)

#Add levels (value labels) to Sartori Variable:
# 0. "No Pop." - Respondent is not a populist according to Sartori aggregation
# 1. "Pop." - Respondent is a populist according to Sartori aggregation
levels(df_plotsub3ga$pop_3D_sartori75_cses_z) <- c("No Pop.", "Pop.")

#############################################/
# Ggparis solution to plot:
#Link: https://stackoverflow.com/questions/45873483/ggpairs-plot-with-heatmap-of-correlation-values
#(Last accessed: Sept.28, 2018)

#ggpairs(df_plotsub1)

#Change names of columns: 
colnames(df_plotsub3ga) <- c("Anti-Elitism", "Challenges to repr. democ.",
                             "Goertzian", "Bollen", "Sartorian")

# Matrix of plots
p1_3ga <- ggpairs(df_plotsub3ga[!is.na(df_plotsub3ga$Sartorian),], 
                  lower = list(continuous = wrap("points", alpha = 0.1 )),
                  upper = list(continuous = wrap (mycor, sgnf=2)))

# Correlation matrix plot
p2_3ga <- ggcorr(df_plotsub3ga, label = TRUE, label_round = 2, 
                 low = "#998ec3", mid="#CCCCCC", high = "#f1a340") 
# https://www.rdocumentation.org/packages/GGally/versions/1.4.0/topics/ggcorr
# last accessed: November 20, 2018

# Extract the list of colors from the correlation matrix plot:
g2_3ga <- ggplotGrob(p2_3ga)
colors <- g2_3ga$grobs[[6]]$children[[3]]$gp$fill
colors

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colors2_3Ga <- c("#E8B379FF", "#EFA750FF", "#EFA852FF",
                 "#CCCCCC", "#EEAB5DFF", "#EFA74FFF" , "#CCCCCC",
                 "#F0A548FF", "#CCCCCC", "#CCCCCC")

# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 5
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1_3ga,k1,k2) +
      theme(panel.background = element_rect(fill = colors2_3Ga[idx], color="white"),
            panel.grid.major = element_line(color=colors2_3Ga[idx]))
    p1_3ga <- putPlot(p1_3ga,plt,k1,k2) 
    idx <- idx+1
  }
}
#print(p1_3ga)

#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/
png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny11_GLES_POSTELECTION_CSES_Scale_Relationship_Aggregations_SingleDimensions.png",
    units="in", width=11.5, height=8.8, res=300)
p1_3ga + labs(title = "CSES Scale - Dimensions and Aggregates (Germany 2017)",
              caption = "Data source: GLES 2017 (Cross-Section)") 
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny11_GLES_POSTELECTION_CSES_Scale_Relationship_Aggregations_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1_3ga + labs(title = "CSES Scale - Dimensions and Aggregates (Germany 2017)",
              caption = "Data source: GLES 2017 (Cross-Section)") 
dev.off()


#### <<>>  Figure Shiny 12 - Brazil Sample (Silva et al Dataset - Oliver/Rahn Scale) ####

# Information about Silva dataset: 
# To ease analyses, variables referring to single dimensions are referred to
# in some way as "anti-elitism", "sovereignty", and "homogeneity" (as used in
# Schulz scale). However, for two scales (Oliver/Rahn & Silva), these variables 
# have slightly different meanings and names in the original dataset/publications:
#
#    Oliver/Rahn Scale:   Anti-Elitism (Schulz) = Anti-Elitism (Oliver/Rahn)
#                         Homogeneity (Schulz)	= National Affiliation (Oliver/Rahn)
#                         Sovereignty (Schulz)	= Mistrust in Expertise (Oliver/Rahn)


#create quick subset for Brazil data only and, including just the variables needed for the plot:
df_plotsub1.i_Rahn_Brazil <- dat_Silva[which(dat_Silva$country=="Brazil"),]

plotsub1.i_Rahn <- c("antielite_sum_rahn_z", "homogenous_sum_rahn_z", "sovereignty_sum_rahn_z",
                     "pop_3D_goertzian_rahn_z", "pop_3D_bollen_rahn_z", "pop_3D_sartori75_rahn_z")
df_plotsub1.i_Rahn_Brazil <- df_plotsub1.i_Rahn_Brazil[plotsub1.i_Rahn]

#Change names of columns: 
colnames(df_plotsub1.i_Rahn_Brazil) <- c("Anti-Elitism", "National Aff.", "Mistrust in Exp.",
                                           "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Rahn_Brazil$Sartorian <- as.factor(df_plotsub1.i_Rahn_Brazil$Sartorian)
levels(df_plotsub1.i_Rahn_Brazil$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Rahn_Brazil <- ggpairs(df_plotsub1.i_Rahn_Brazil[!is.na(df_plotsub1.i_Rahn_Brazil$Sartorian),], 
                             lower = list(continuous = wrap("points", alpha = 0.3 )),
                             upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Rahn_Brazil <- ggcorr(df_plotsub1.i_Rahn_Brazil[!is.na(df_plotsub1.i_Rahn_Brazil$Sartorian),], 
                            label = TRUE, label_round = 2,
                            low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Rahn_Brazil <- ggplotGrob(p2i_Rahn_Brazil)
colorsi_Rahn_Brazil <- g2i_Rahn_Brazil$grobs[[6]]$children[[3]]$gp$fill
colorsi_Rahn_Brazil

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Rahn_Brazil2 <- c("#CBCBCCFF", "#DEBFA1FF", "#E6B684FF", "#EAB06FFF" ,
                            "#CCCCCC", "#D1C9C2FF", "#DAC2ADFF", "#E7B57FFF", "#CCCCCC",
                          "#EEAA5BFF", "#EBB06EFF", "#CCCCCC", "#EDAB5EFF",
                            "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Rahn_Brazil,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Rahn_Brazil2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Rahn_Brazil2[idx]))
    p1i_Rahn_Brazil <- putPlot(p1i_Rahn_Brazil,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny12_Brazil_Relationship_Rahn_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Rahn_Brazil + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (Brazil)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#TIF Version
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny12_Brazil_Relationship_Rahn_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Rahn_Brazil + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (Brazil)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>>  Figure Shiny 13 - France Sample (Silva et al Dataset - Oliver/Rahn Scale) ####

#create quick subset for France data only and, including just the variables needed for the plot:
df_plotsub1.i_Rahn_France <- dat_Silva[which(dat_Silva$country=="France"),]

plotsub1.i_Rahn <- c("antielite_sum_rahn_z", "homogenous_sum_rahn_z", "sovereignty_sum_rahn_z",
                       "pop_3D_goertzian_rahn_z", "pop_3D_bollen_rahn_z", "pop_3D_sartori75_rahn_z")
df_plotsub1.i_Rahn_France <- df_plotsub1.i_Rahn_France[plotsub1.i_Rahn]

#Change names of columns: 
colnames(df_plotsub1.i_Rahn_France) <- c("Anti-Elitism", "National Aff.", "Mistrust in Exp.",
                                         "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Rahn_France$Sartorian <- as.factor(df_plotsub1.i_Rahn_France$Sartorian)
levels(df_plotsub1.i_Rahn_France$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Rahn_France <- ggpairs(df_plotsub1.i_Rahn_France[!is.na(df_plotsub1.i_Rahn_France$Sartorian),], 
                             lower = list(continuous = wrap("points", alpha = 0.3 )),
                             upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Rahn_France <- ggcorr(df_plotsub1.i_Rahn_France[!is.na(df_plotsub1.i_Rahn_France$Sartorian),], 
                            label = TRUE, label_round = 2,
                            low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Rahn_France <- ggplotGrob(p2i_Rahn_France)
colorsi_Rahn_France <- g2i_Rahn_France$grobs[[6]]$children[[3]]$gp$fill
colorsi_Rahn_France

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Rahn_France2 <- c("#CBCBCCFF", "#DAC2ACFF", "#E5B888FF", "#EAB06FFF" ,
                            "#CCCCCC", "#D5C6B9FF", "#DDC0A3FF", "#E7B581FF", "#CCCCCC",
                          "#EDAC60FF", "#EBAF6DFF", "#CCCCCC", "#EEAA5CFF",
                            "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Rahn_France,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Rahn_France2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Rahn_France2[idx]))
    p1i_Rahn_France <- putPlot(p1i_Rahn_France,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny13_France_Relationship_Rahn_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Rahn_France + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (France)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

# TIF Version
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny13_France_Relationship_Rahn_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Rahn_France + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (France)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 14 - Greece Sample (Silva et al Dataset - Oliver/Rahn Scale) ####

#create quick subset for Greece data only and, including just the variables needed for the plot:
df_plotsub1.i_Rahn_Greece <- dat_Silva[which(dat_Silva$country=="Greece"),]

plotsub1.i_Rahn <- c("antielite_sum_rahn_z", "homogenous_sum_rahn_z", "sovereignty_sum_rahn_z",
                       "pop_3D_goertzian_rahn_z", "pop_3D_bollen_rahn_z", "pop_3D_sartori75_rahn_z")
df_plotsub1.i_Rahn_Greece <- df_plotsub1.i_Rahn_Greece[plotsub1.i_Rahn]

#Change names of columns: 
colnames(df_plotsub1.i_Rahn_Greece) <- c("Anti-Elitism", "National Aff.", "Mistrust in Exp.",
                                         "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Rahn_Greece$Sartorian <- as.factor(df_plotsub1.i_Rahn_Greece$Sartorian)
levels(df_plotsub1.i_Rahn_Greece$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Rahn_Greece <- ggpairs(df_plotsub1.i_Rahn_Greece[!is.na(df_plotsub1.i_Rahn_Greece$Sartorian),], 
                             lower = list(continuous = wrap("points", alpha = 0.1 )),
                             upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Rahn_Greece <- ggcorr(df_plotsub1.i_Rahn_Greece[!is.na(df_plotsub1.i_Rahn_Greece$Sartorian),], 
                            label = TRUE, label_round = 2,
                            low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Rahn_Greece <- ggplotGrob(p2i_Rahn_Greece)
colorsi_Rahn_Greece <- g2i_Rahn_Greece$grobs[[6]]$children[[3]]$gp$fill
colorsi_Rahn_Greece

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Rahn_Greece2 <- c("#D0CAC4FF", "#D9C3AEFF", "#E7B57FFF", "#EBAF6BFF" ,
                            "#CCCCCC", "#CFCAC5FF", "#D5C6B8FF", "#E7B580FF", "#CCCCCC",
                          "#EEAA5CFF", "#E9B275FF", "#CCCCCC", "#EDAB5FFF",
                            "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Rahn_Greece,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Rahn_Greece2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Rahn_Greece2[idx]))
    p1i_Rahn_Greece <- putPlot(p1i_Rahn_Greece,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny14_Greece_Relationship_Rahn_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Rahn_Greece + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (Greece)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

# TIF Version
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny14_Greece_Relationship_Rahn_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Rahn_Greece + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (Greece)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>> Figure Shiny 15 - Ireland Sample (Silva et al Dataset - Oliver/Rahn Scale) ####

#create quick subset for Ireland data only and, including just the variables needed for the plot:
df_plotsub1.i_Rahn_Ireland <- dat_Silva[which(dat_Silva$country=="Ireland"),]

plotsub1.i_Rahn <- c("antielite_sum_rahn_z", "homogenous_sum_rahn_z", "sovereignty_sum_rahn_z",
                       "pop_3D_goertzian_rahn_z", "pop_3D_bollen_rahn_z", "pop_3D_sartori75_rahn_z")
df_plotsub1.i_Rahn_Ireland <- df_plotsub1.i_Rahn_Ireland[plotsub1.i_Rahn]

#Change names of columns: 
colnames(df_plotsub1.i_Rahn_Ireland) <- c("Anti-Elitism", "National Aff.", "Mistrust in Exp.",
                                          "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Rahn_Ireland$Sartorian <- as.factor(df_plotsub1.i_Rahn_Ireland$Sartorian)
levels(df_plotsub1.i_Rahn_Ireland$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Rahn_Ireland <- ggpairs(df_plotsub1.i_Rahn_Ireland[!is.na(df_plotsub1.i_Rahn_Ireland$Sartorian),], 
                              lower = list(continuous = wrap("points", alpha = 0.1 )),
                              upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Rahn_Ireland <- ggcorr(df_plotsub1.i_Rahn_Ireland[!is.na(df_plotsub1.i_Rahn_Ireland$Sartorian),], 
                             label = TRUE, label_round = 2,
                             low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Rahn_Ireland <- ggplotGrob(p2i_Rahn_Ireland)
colorsi_Rahn_Ireland <- g2i_Rahn_Ireland$grobs[[6]]$children[[3]]$gp$fill
colorsi_Rahn_Ireland

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Rahn_Ireland2 <- c("#D6C5B7FF", "#D7C5B4FF", "#E3B98FFF", "#EBAF6DFF",
                             "#CCCCCC", "#D2C8BFFF", "#DBC1A8FF", "#E9B378FF", "#CCCCCC",
                           "#EEAA5CFF", "#E9B274FF", "#CCCCCC", "#EDAC61FF",
                             "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Rahn_Ireland,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Rahn_Ireland2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Rahn_Ireland2[idx]))
    p1i_Rahn_Ireland <- putPlot(p1i_Rahn_Ireland,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny15_Ireland_Relationship_Rahn_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Rahn_Ireland + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (Ireland)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

# TIF Version
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny15_Ireland_Relationship_Rahn_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Rahn_Ireland + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (Ireland)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 16 - Italy Sample (Silva et al Dataset - Oliver/Rahn Scale) ####

#create quick subset for Italy data only and, including just the variables needed for the plot:
df_plotsub1.i_Rahn_Italy <- dat_Silva[which(dat_Silva$country=="Italy"),]

plotsub1.i_Rahn <- c("antielite_sum_rahn_z", "homogenous_sum_rahn_z", "sovereignty_sum_rahn_z",
                     "pop_3D_goertzian_rahn_z", "pop_3D_bollen_rahn_z", "pop_3D_sartori75_rahn_z")
df_plotsub1.i_Rahn_Italy <- df_plotsub1.i_Rahn_Italy[plotsub1.i_Rahn]

#Change names of columns: 
colnames(df_plotsub1.i_Rahn_Italy) <- c("Anti-Elitism", "National Aff.", "Mistrust in Exp.",
                                        "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Rahn_Italy$Sartorian <- as.factor(df_plotsub1.i_Rahn_Italy$Sartorian)
levels(df_plotsub1.i_Rahn_Italy$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Rahn_Italy <- ggpairs(df_plotsub1.i_Rahn_Italy[!is.na(df_plotsub1.i_Rahn_Italy$Sartorian),], 
                            lower = list(continuous = wrap("points", alpha = 0.1 )),
                            upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Rahn_Italy <- ggcorr(df_plotsub1.i_Rahn_Italy[!is.na(df_plotsub1.i_Rahn_Italy$Sartorian),], 
                           label = TRUE, label_round = 2,
                           low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Rahn_Italy <- ggplotGrob(p2i_Rahn_Italy)
colorsi_Rahn_Italy <- g2i_Rahn_Italy$grobs[[6]]$children[[3]]$gp$fill
colorsi_Rahn_Italy

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Rahn_Italy2 <- c("#D7C4B2FF", "#E2BB93FF", "#EAB071FF", "#EDAC60FF" ,
                           "#CCCCCC", "#D9C3AFFF", "#DEBE9EFF", "#E9B276FF", "#CCCCCC",
                         "#EEA957FF", "#ECAE68FF", "#CCCCCC", "#EEA957FF",
                           "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Rahn_Italy,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Rahn_Italy2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Rahn_Italy2[idx]))
    p1i_Rahn_Italy <- putPlot(p1i_Rahn_Italy,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny16_Italy_Relationship_Rahn_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Rahn_Italy + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (Italy)",
                      caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

# TIF Version
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny16_Italy_Relationship_Rahn_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Rahn_Italy + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (Italy)",
                      caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 17 - Mexican Sample (Silva et al Dataset - Oliver/Rahn Scale) ####

#create quick subset for Mexico data only and, including just the variables needed for the plot:
df_plotsub1.i_Rahn_Mexico <- dat_Silva[which(dat_Silva$country=="Mexico"),]

plotsub1.i_Rahn <- c("antielite_sum_rahn_z", "homogenous_sum_rahn_z", "sovereignty_sum_rahn_z",
                     "pop_3D_goertzian_rahn_z", "pop_3D_bollen_rahn_z", "pop_3D_sartori75_rahn_z")
df_plotsub1.i_Rahn_Mexico <- df_plotsub1.i_Rahn_Mexico[plotsub1.i_Rahn]

#Change names of columns: 
colnames(df_plotsub1.i_Rahn_Mexico) <- c("Anti-Elitism", "National Aff.", "Mistrust in Exp.",
                                         "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Rahn_Mexico$Sartorian <- as.factor(df_plotsub1.i_Rahn_Mexico$Sartorian)
levels(df_plotsub1.i_Rahn_Mexico$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Rahn_Mexico <- ggpairs(df_plotsub1.i_Rahn_Mexico[!is.na(df_plotsub1.i_Rahn_Mexico$Sartorian),], 
                             lower = list(continuous = wrap("points", alpha = 0.1 )),
                             upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Rahn_Mexico <- ggcorr(df_plotsub1.i_Rahn_Mexico[!is.na(df_plotsub1.i_Rahn_Mexico$Sartorian),], 
                            label = TRUE, label_round = 2,
                            low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Rahn_Mexico <- ggplotGrob(p2i_Rahn_Mexico)
colorsi_Rahn_Mexico <- g2i_Rahn_Mexico$grobs[[6]]$children[[3]]$gp$fill
colorsi_Rahn_Mexico

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Rahn_Mexico2 <- c("#D6C5B6FF", "#D5C6B8FF", "#E4B98DFF", "#EAB070FF" ,
                            "#CCCCCC", "#D8C4B2FF", "#DFBE9EFF", "#EAB070FF", "#CCCCCC",
                          "#EDAC61FF", "#EAB174FF", "#CCCCCC", "#EDAC60FF",
                            "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Rahn_Mexico,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Rahn_Mexico2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Rahn_Mexico2[idx]))
    p1i_Rahn_Mexico <- putPlot(p1i_Rahn_Mexico,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny17_Mexico_Relationship_Rahn_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Rahn_Mexico + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (Mexico)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny17_Mexico_Relationship_Rahn_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Rahn_Mexico + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (Mexico)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 18 - Spain Sample  (Silva et al Dataset - Oliver/Rahn Scale) ####

#create quick subset for Spain data only and, including just the variables needed for the plot:
df_plotsub1.i_Rahn_Spain <- dat_Silva[which(dat_Silva$country=="Spain"),]

plotsub1.i_Rahn <- c("antielite_sum_rahn_z", "homogenous_sum_rahn_z", "sovereignty_sum_rahn_z",
                     "pop_3D_goertzian_rahn_z", "pop_3D_bollen_rahn_z", "pop_3D_sartori75_rahn_z")
df_plotsub1.i_Rahn_Spain <- df_plotsub1.i_Rahn_Spain[plotsub1.i_Rahn]

#Change names of columns: 
colnames(df_plotsub1.i_Rahn_Spain) <- c("Anti-Elitism", "National Aff.", "Mistrust in Exp.",
                                        "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Rahn_Spain$Sartorian <- as.factor(df_plotsub1.i_Rahn_Spain$Sartorian)
levels(df_plotsub1.i_Rahn_Spain$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Rahn_Spain <- ggpairs(df_plotsub1.i_Rahn_Spain[!is.na(df_plotsub1.i_Rahn_Spain$Sartorian),], 
                            lower = list(continuous = wrap("points", alpha = 0.1 )),
                            upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Rahn_Spain <- ggcorr(df_plotsub1.i_Rahn_Spain[!is.na(df_plotsub1.i_Rahn_Spain$Sartorian),],
                           label = TRUE, label_round = 2,
                           low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Rahn_Spain <- ggplotGrob(p2i_Rahn_Spain)
colorsi_Rahn_Spain <- g2i_Rahn_Spain$grobs[[6]]$children[[3]]$gp$fill
colorsi_Rahn_Spain

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Rahn_Spain2 <- c("#C2BFCAFF", "#DAC2ACFF", "#E6B684FF", "#E8B37BFF" ,
                           "#CCCCCC", "#D2C8BFFF", "#D2C8BEFF", "#E5B889FF", "#CCCCCC",
                         "#EDAC60FF", "#EBAF6BFF" , "#CCCCCC", "#EDAC62FF",
                           "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Rahn_Spain,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Rahn_Spain2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Rahn_Spain2[idx]))
    p1i_Rahn_Spain <- putPlot(p1i_Rahn_Spain,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny18_Spain_Relationship_Rahn_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Rahn_Spain + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (Spain)",
                      caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny18_Spain_Relationship_Rahn_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Rahn_Spain + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (Spain)",
                      caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 19 - UK Sample  (Silva et al Dataset - Oliver/Rahn Scale) ####

#create quick subset for UK data only and, including just the variables needed for the plot:
df_plotsub1.i_Rahn_UK <- dat_Silva[which(dat_Silva$country=="UK"),]

plotsub1.i_Rahn <- c("antielite_sum_rahn_z", "homogenous_sum_rahn_z", "sovereignty_sum_rahn_z",
                     "pop_3D_goertzian_rahn_z", "pop_3D_bollen_rahn_z", "pop_3D_sartori75_rahn_z")
df_plotsub1.i_Rahn_UK <- df_plotsub1.i_Rahn_UK[plotsub1.i_Rahn]

#Change names of columns: 
colnames(df_plotsub1.i_Rahn_UK) <- c("Anti-Elitism", "National Aff.", "Mistrust in Exp.",
                                     "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Rahn_UK$Sartorian <- as.factor(df_plotsub1.i_Rahn_UK$Sartorian)
levels(df_plotsub1.i_Rahn_UK$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Rahn_UK <- ggpairs(df_plotsub1.i_Rahn_UK[!is.na(df_plotsub1.i_Rahn_UK$Sartorian),], 
                         lower = list(continuous = wrap("points", alpha = 0.1 )),
                         upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Rahn_UK <- ggcorr(df_plotsub1.i_Rahn_UK[!is.na(df_plotsub1.i_Rahn_UK$Sartorian),], 
                        label = TRUE, label_round = 2,
                        low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Rahn_UK <- ggplotGrob(p2i_Rahn_UK)
colorsi_Rahn_UK <- g2i_Rahn_UK$grobs[[6]]$children[[3]]$gp$fill
colorsi_Rahn_UK

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Rahn_UK2 <- c("#C8C7CBFF", "#D1C9C2FF", "#E2BA91FF", "#E7B57FFF" ,
                        "#CCCCCC", "#DBC1A7FF", "#DEBFA0FF", "#E8B37AFF", "#CCCCCC",
                        "#ECAD67FF", "#EBAF6DFF", "#CCCCCC", "#EDAB5EFF",
                        "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Rahn_UK,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Rahn_UK2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Rahn_UK2[idx]))
    p1i_Rahn_UK <- putPlot(p1i_Rahn_UK,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny19_UK_Relationship_Rahn_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Rahn_UK + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (UK)",
                   caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny19_UK_Relationship_Rahn_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Rahn_UK + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (UK)",
                   caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 20 - United States Sample  (Silva et al Dataset - Oliver/Rahn Scale) ####

#create quick subset for US data only and, including just the variables needed for the plot:
df_plotsub1.i_Rahn_US <- dat_Silva[which(dat_Silva$country=="US"),]

plotsub1.i_Rahn <-  c("antielite_sum_rahn_z", "homogenous_sum_rahn_z", "sovereignty_sum_rahn_z",
                      "pop_3D_goertzian_rahn_z", "pop_3D_bollen_rahn_z", "pop_3D_sartori75_rahn_z")
df_plotsub1.i_Rahn_US <- df_plotsub1.i_Rahn_US[plotsub1.i_Rahn]

#Change names of columns: 
colnames(df_plotsub1.i_Rahn_US) <- c("Anti-Elitism", "National Aff.", "Mistrust in Exp.",
                                     "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Rahn_US$Sartorian <- as.factor(df_plotsub1.i_Rahn_US$Sartorian)
levels(df_plotsub1.i_Rahn_US$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Rahn_US <- ggpairs(df_plotsub1.i_Rahn_US[!is.na(df_plotsub1.i_Rahn_US$Sartorian),], 
                         lower = list(continuous = wrap("points", alpha = 0.1 )),
                         upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Rahn_US <- ggcorr(df_plotsub1.i_Rahn_US[!is.na(df_plotsub1.i_Rahn_US$Sartorian),], 
                        label = TRUE, label_round = 2,
                        low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Rahn_US <- ggplotGrob(p2i_Rahn_US)
colorsi_Rahn_US <- g2i_Rahn_US$grobs[[6]]$children[[3]]$gp$fill
colorsi_Rahn_US

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Rahn_US2 <- c("#C5C3CBFF", "#DDC0A4FF", "#E7B581FF", "#EBAF6CFF",
                        "#CCCCCC", "#CFCAC7FF", "#D7C4B3FF", "#E0BD9AFF", "#CCCCCC",
                      "#EDAB5DFF", "#ECAD66FF", "#CCCCCC", "#EEA956FF",
                        "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Rahn_US,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Rahn_US2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Rahn_US2[idx]))
    p1i_Rahn_US <- putPlot(p1i_Rahn_US,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny20_US_Relationship_Rahn_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Rahn_US + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (US)",
                   caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny20_US_Relationship_Rahn_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Rahn_US + labs(title = "Oliver and Rahn Scale - Dimensions and Aggregates (US)",
                   caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>> Figure Shiny 21 -  Brazil Sample (Castanho Silva et al Scale) ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SilvaH_Brazil <- dat_Silva[which(dat_Silva$country=="Brazil"),]

plotsub1.i_Silva <- c("antielite_sum_z", "homogenous_sum_z", "sovereignty_sum_z",
                      "pop_3D_man_goertzian_z", "pop_3D_man_bollen_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Silva_Brazil <- df_plotsub1.i_SilvaH_Brazil[plotsub1.i_Silva]

#Change names of columns: 
colnames(df_plotsub1.i_Silva_Brazil) <- c("Anti-Elitism", "Manichean", "People-Centrism",
                                          "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Silva_Brazil$Sartorian <- as.factor(df_plotsub1.i_Silva_Brazil$Sartorian)
levels(df_plotsub1.i_Silva_Brazil$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Silva_Brazil <- ggpairs(df_plotsub1.i_Silva_Brazil[!is.na(df_plotsub1.i_Silva_Brazil$Sartorian),], 
                            lower = list(continuous = wrap("points", alpha = 0.1 )),
                            upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Silva_Brazil <- ggcorr(df_plotsub1.i_Silva_Brazil[!is.na(df_plotsub1.i_Silva_Brazil$Sartorian),], 
                           label = TRUE, label_round = 2,
                           low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Silva_Brazil <- ggplotGrob(p2i_Silva_Brazil)
colorsi_Silva_Brazil <- g2i_Silva_Brazil$grobs[[6]]$children[[3]]$gp$fill
colorsi_Silva_Brazil

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Silva_Brazil2 <- c("#C2C0CAFF", "#DEBFA1FF", "#D3C7BCFF", "#E9B378FF" ,
                           "#CCCCCC", "#D8C4B2FF", "#EEA957FF", "#E5B786FF", "#CCCCCC",
                           "#E2BB94FF", "#EDAC61FF", "#CCCCCC", "#EBAE6AFF",
                           "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Silva_Brazil,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Silva_Brazil2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Silva_Brazil2[idx]))
    p1i_Silva_Brazil <- putPlot(p1i_Silva_Brazil,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny21_BRAZIL_Relationship_Silva_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Silva_Brazil + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (Brazil)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny21_BRAZIL_Relationship_Silva_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Silva_Brazil + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (Brazil)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 22 -  France Sample (Castanho Silva et al Scale)  ####

#create quick subset for France data only and, including just the variables needed for the plot:
df_plotsub1.i_SilvaH_France <- dat_Silva[which(dat_Silva$country=="France"),]

plotsub1.i_Silva <- c("antielite_sum_z", "homogenous_sum_z", "sovereignty_sum_z",
                      "pop_3D_man_goertzian_z", "pop_3D_man_bollen_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Silva_France <- df_plotsub1.i_SilvaH_France[plotsub1.i_Silva]

#Change names of columns: 
colnames(df_plotsub1.i_Silva_France) <- c("Anti-Elitism", "Manichean", "People-Centrism",
                                          "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Silva_France$Sartorian <- as.factor(df_plotsub1.i_Silva_France$Sartorian)
levels(df_plotsub1.i_Silva_France$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Silva_France <- ggpairs(df_plotsub1.i_Silva_France[!is.na(df_plotsub1.i_Silva_France$Sartorian),], 
                            lower = list(continuous = wrap("points", alpha = 0.1 )),
                            upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Silva_France <- ggcorr(df_plotsub1.i_Silva_France[!is.na(df_plotsub1.i_Silva_France$Sartorian),], 
                           label = TRUE, label_round = 2,
                           low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Silva_France <- ggplotGrob(p2i_Silva_France)
colorsi_Silva_France <- g2i_Silva_France$grobs[[6]]$children[[3]]$gp$fill
colorsi_Silva_France

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Silva_France2 <- c("#BEBACAFF", "#E0BD9AFF", "#CAC9CCFF", "#E8B47BFF"  ,
                           "#CCCCCC", "#CFCAC5FF", "#EEA957FF", "#E4B98CFF", "#CCCCCC",
                           "#D9C3ADFF", "#ECAE67FF", "#CCCCCC", "#E8B37AFF",
                           "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Silva_France,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Silva_France2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Silva_France2[idx]))
    p1i_Silva_France <- putPlot(p1i_Silva_France,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny22_France_Relationship_Silva_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Silva_France + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (France)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny22_France_Relationship_Silva_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Silva_France + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (France)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>> Figure Shiny 23 - Greece Sample (Castanho Silva et al Scale)  ####

#create quick subset for Greece data only and, including just the variables needed for the plot:
df_plotsub1.i_SilvaH_Greece <- dat_Silva[which(dat_Silva$country=="Greece"),]

plotsub1.i_Silva <- c("antielite_sum_z", "homogenous_sum_z", "sovereignty_sum_z",
                      "pop_3D_man_goertzian_z", "pop_3D_man_bollen_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Silva_Greece <- df_plotsub1.i_SilvaH_Greece[plotsub1.i_Silva]

#Change names of columns: 
colnames(df_plotsub1.i_Silva_Greece) <- c("Anti-Elitism", "Manichean", "People-Centrism",
                                          "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Silva_Greece$Sartorian <- as.factor(df_plotsub1.i_Silva_Greece$Sartorian)
levels(df_plotsub1.i_Silva_Greece$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Silva_Greece <- ggpairs(df_plotsub1.i_Silva_Greece[!is.na(df_plotsub1.i_Silva_Greece$Sartorian),], 
                            lower = list(continuous = wrap("points", alpha = 0.1)),
                            upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Silva_Greece <- ggcorr(df_plotsub1.i_Silva_Greece[!is.na(df_plotsub1.i_Silva_Greece$Sartorian),], 
                           label = TRUE, label_round = 2,
                           low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Silva_Greece <- ggplotGrob(p2i_Silva_Greece)
colorsi_Silva_Greece <- g2i_Silva_Greece$grobs[[6]]$children[[3]]$gp$fill
colorsi_Silva_Greece

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:

colorsi_Silva_Greece2 <- c("#C1BECAFF", "#DFBE9DFF", "#CBCACCFF", "#E8B37AFF" ,
                           "#CCCCCC", "#D3C8BEFF", "#EFA74EFF", "#E5B889FF", "#CCCCCC",
                           "#DBC1A8FF", "#EDAD63FF", "#CCCCCC", "#E9B276FF",
                           "#CCCCCC", "#CCCCCC")

# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Silva_Greece,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Silva_Greece2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Silva_Greece2[idx]))
    p1i_Silva_Greece <- putPlot(p1i_Silva_Greece,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny23_Greece_Relationship_Silva_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Silva_Greece + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (Greece)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny23_Greece_Relationship_Silva_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Silva_Greece + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (Greece)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>> Figure Shiny 24 - Ireland Sample (Castanho Silva et al Scale)  ####

#create quick subset for Ireland data only and, including just the variables needed for the plot:
df_plotsub1.i_SilvaH_Ireland <- dat_Silva[which(dat_Silva$country=="Ireland"),]

plotsub1.i_Silva <- c("antielite_sum_z", "homogenous_sum_z", "sovereignty_sum_z",
                      "pop_3D_man_goertzian_z", "pop_3D_man_bollen_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Silva_Ireland <- df_plotsub1.i_SilvaH_Ireland[plotsub1.i_Silva]

#Change names of columns: 
colnames(df_plotsub1.i_Silva_Ireland) <- c("Anti-Elitism", "Manichean", "People-Centrism",
                                           "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Silva_Ireland$Sartorian <- as.factor(df_plotsub1.i_Silva_Ireland$Sartorian)
levels(df_plotsub1.i_Silva_Ireland$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Silva_Ireland <- ggpairs(df_plotsub1.i_Silva_Ireland[!is.na(df_plotsub1.i_Silva_Ireland$Sartorian),], 
                             lower = list(continuous = wrap("points", alpha = 0.1 )),
                             upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Silva_Ireland <- ggcorr(df_plotsub1.i_Silva_Ireland[!is.na(df_plotsub1.i_Silva_Ireland$Sartorian),], 
                            label = TRUE, label_round = 2,
                            low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Silva_Ireland <- ggplotGrob(p2i_Silva_Ireland)
colorsi_Silva_Ireland <- g2i_Silva_Ireland$grobs[[6]]$children[[3]]$gp$fill
colorsi_Silva_Ireland

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Silva_Ireland2 <- c("#CFCAC6FF", "#E1BC97FF", "#D3C7BCFF", "#EBAF6DFF" ,
                            "#CCCCCC", "#D2C8BFFF", "#EFA750FF", "#E8B47CFF", "#CCCCCC",
                            "#DDBFA3FF", "#ECAE68FF", "#CCCCCC", "#EAB071FF",
                            "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Silva_Ireland,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Silva_Ireland2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Silva_Ireland2[idx]))
    p1i_Silva_Ireland <- putPlot(p1i_Silva_Ireland,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny24_Ireland_Relationship_Silva_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Silva_Ireland + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (Ireland)",
                         caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny24_Ireland_Relationship_Silva_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Silva_Ireland + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (Ireland)",
                         caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>> Figure Shiny 25 - Italy Sample (Castanho Silva et al Scale)  ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SilvaH_Italy <- dat_Silva[which(dat_Silva$country=="Italy"),]

plotsub1.i_Silva <- c("antielite_sum_z", "homogenous_sum_z", "sovereignty_sum_z",
                      "pop_3D_man_goertzian_z", "pop_3D_man_bollen_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Silva_Italy <- df_plotsub1.i_SilvaH_Italy[plotsub1.i_Silva]

#Change names of columns: 
colnames(df_plotsub1.i_Silva_Italy) <- c("Anti-Elitism", "Manichean", "People-Centrism",
                                         "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Silva_Italy$Sartorian <- as.factor(df_plotsub1.i_Silva_Italy$Sartorian)
levels(df_plotsub1.i_Silva_Italy$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Silva_Italy <- ggpairs(df_plotsub1.i_Silva_Italy[!is.na(df_plotsub1.i_Silva_Italy$Sartorian),], 
                           lower = list(continuous = wrap("points", alpha = 0.1 )),
                           upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Silva_Italy <- ggcorr(df_plotsub1.i_Silva_Italy[!is.na(df_plotsub1.i_Silva_Italy$Sartorian),], 
                          label = TRUE, label_round = 2,
                          low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Silva_Italy <- ggplotGrob(p2i_Silva_Italy)
colorsi_Silva_Italy <- g2i_Silva_Italy$grobs[[6]]$children[[3]]$gp$fill
colorsi_Silva_Italy

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Silva_Italy2 <- c("#C9C8CCFF", "#E3B98FFF", "#DBC1A7FF", "#EBAE6AFF" ,
                          "#CCCCCC", "#D4C6BAFF", "#EFA955FF", "#E6B684FF", "#CCCCCC",
                          "#E0BD9AFF", "#ECAD65FF", "#CCCCCC", "#ECAD66FF",
                          "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Silva_Italy,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Silva_Italy2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Silva_Italy2[idx]))
    p1i_Silva_Italy <- putPlot(p1i_Silva_Italy,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny25_Italy_Relationship_Silva_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Silva_Italy + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (Italy)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny25_Italy_Relationship_Silva_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Silva_Italy + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (Italy)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 26 - Mexico Sample (Castanho Silva et al Scale)  ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SilvaH_Mexico <- dat_Silva[which(dat_Silva$country=="Mexico"),]

plotsub1.i_Silva <- c("antielite_sum_z", "homogenous_sum_z", "sovereignty_sum_z",
                      "pop_3D_man_goertzian_z", "pop_3D_man_bollen_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Silva_Mexico <- df_plotsub1.i_SilvaH_Mexico[plotsub1.i_Silva]

#Change names of columns: 
colnames(df_plotsub1.i_Silva_Mexico) <- c("Anti-Elitism", "Manichean", "People-Centrism",
                                          "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Silva_Mexico$Sartorian <- as.factor(df_plotsub1.i_Silva_Mexico$Sartorian)
levels(df_plotsub1.i_Silva_Mexico$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Silva_Mexico <- ggpairs(df_plotsub1.i_Silva_Mexico[!is.na(df_plotsub1.i_Silva_Mexico$Sartorian),], 
                            lower = list(continuous = wrap("points", alpha = 0.1 )),
                            upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Silva_Mexico <- ggcorr(df_plotsub1.i_Silva_Mexico, label = TRUE, label_round = 2,
                           low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Silva_Mexico <- ggplotGrob(p2i_Silva_Mexico)
colorsi_Silva_Mexico <- g2i_Silva_Mexico$grobs[[6]]$children[[3]]$gp$fill
colorsi_Silva_Mexico

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Silva_Mexico2 <- c("#C9C8CBFF", "#DEBFA1FF", "#D4C7BAFF", "#EAB071FF" ,
                           "#CCCCCC", "#D6C5B5FF", "#EFA750FF", "#E6B683FF", "#CCCCCC",
                           "#E0BD9AFF", "#ECAD66FF", "#CCCCCC", "#EBAF6DFF",
                           "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Silva_Mexico,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Silva_Mexico2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Silva_Mexico2[idx]))
    p1i_Silva_Mexico <- putPlot(p1i_Silva_Mexico,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny26_Mexico_Relationship_Silva_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Silva_Mexico + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (Mexico)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny26_Mexico_Relationship_Silva_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Silva_Mexico + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (Mexico)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>> Figure Shiny 27 - Spain Sample (Castanho Silva et al Scale)  ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SilvaH_Spain <- dat_Silva[which(dat_Silva$country=="Spain"),]

plotsub1.i_Silva <- c("antielite_sum_z", "homogenous_sum_z", "sovereignty_sum_z",
                      "pop_3D_man_goertzian_z", "pop_3D_man_bollen_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Silva_Spain <- df_plotsub1.i_SilvaH_Spain[plotsub1.i_Silva]

#Change names of columns: 
colnames(df_plotsub1.i_Silva_Spain) <- c("Anti-Elitism", "Manichean", "People-Centrism",
                                         "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Silva_Spain$Sartorian <- as.factor(df_plotsub1.i_Silva_Spain$Sartorian)
levels(df_plotsub1.i_Silva_Spain$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Silva_Spain <- ggpairs(df_plotsub1.i_Silva_Spain[!is.na(df_plotsub1.i_Silva_Spain$Sartorian),], 
                           lower = list(continuous = wrap("points", alpha = 0.1 )),
                           upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Silva_Spain <- ggcorr(df_plotsub1.i_Silva_Spain[!is.na(df_plotsub1.i_Silva_Spain$Sartorian),], 
                          label = TRUE, label_round = 2,
                          low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Silva_Spain <- ggplotGrob(p2i_Silva_Spain)
colorsi_Silva_Spain <- g2i_Silva_Spain$grobs[[6]]$children[[3]]$gp$fill
colorsi_Silva_Spain

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Silva_Spain2 <- c("#C0BCCAFF", "#E1BC97FF", "#D2C8BFFF", "#E9B378FF" ,
                          "#CCCCCC", "#D1C9C2FF", "#EEA957FF", "#E4B88AFF", "#CCCCCC",
                          "#DBC1A8FF", "#ECAD66FF", "#CCCCCC", "#EBB06EFF",
                          "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Silva_Spain,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Silva_Spain2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Silva_Spain2[idx]))
    p1i_Silva_Spain <- putPlot(p1i_Silva_Spain,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny27_Spain_Relationship_Silva_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Silva_Spain + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (Spain)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny27_Spain_Relationship_Silva_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Silva_Spain + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (Spain)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 28 - UK Sample (Castanho Silva et al Scale)  ####

#create quick subset for MTRUK data only and, including just the variables needed for the plot:
df_plotsub1.i_SilvaH_UK <- dat_Silva[which(dat_Silva$country=="UK"),]

plotsub1.i_Silva <- c("antielite_sum_z", "homogenous_sum_z", "sovereignty_sum_z",
                      "pop_3D_man_goertzian_z", "pop_3D_man_bollen_z", "pop_3D_man_sartori75_z")
df_plotsub1.i_Silva_UK <- df_plotsub1.i_SilvaH_UK[plotsub1.i_Silva]

#Change names of columns: 
colnames(df_plotsub1.i_Silva_UK) <- c("Anti-Elitism", "Manichean", "People-Centrism",
                                      "Goertzian", "Bollen", "Sartorian")

#Sartori as dichotomous variable (i.e. Factor):
df_plotsub1.i_Silva_UK$Sartorian <- as.factor(df_plotsub1.i_Silva_UK$Sartorian)
levels(df_plotsub1.i_Silva_UK$Sartorian) <- c("No Pop.", "Pop.")

# Matrix of plots
p1i_Silva_UK <- ggpairs(df_plotsub1.i_Silva_UK[!is.na(df_plotsub1.i_Silva_UK$Sartorian),], 
                        lower = list(continuous = wrap("points", alpha = 0.1 )),
                        upper = list(continuous = wrap (mycor, sgnf=2)))  

# Correlation matrix plot
p2i_Silva_UK <- ggcorr(df_plotsub1.i_Silva_UK[!is.na(df_plotsub1.i_Silva_UK$Sartorian),], 
                       label = TRUE, label_round = 2,
                       low = "#998ec3", mid="#CCCCCC", high = "#f1a340")

# Extract the list of colors from the correlation matrix plot:
g2i_Silva_UK <- ggplotGrob(p2i_Silva_UK)
colorsi_Silva_UK <- g2i_Silva_UK$grobs[[6]]$children[[3]]$gp$fill
colorsi_Silva_UK

#To identify what the color codes mean, the following webtool was used:
#https://color.adobe.com/create/color-wheel/?base=2&rule=Analogous&selected=2&name=My%20Color%20Theme&mode=rgb&rgbvalues=1,0.6912128435850279,0.37745098039215685,0.91,0.5463862448519609,0.3434803921568628,1,0.5490196078431373,0.42745098039215684,0.91,0.38115135923113175,0.3434803921568628,1,0.37745098039215685,0.546794060088599&swatchOrder=0,1,2,3,4
#(last accessed: October 7, 2018)

#Note: because correlations are not estimated between the Sartorian and
#      the other concepts in correlation matrix plot (p2, heat map),
#      it was necessary to manually add a grey filling
#      ("#CCCCCC") at the end of every line from the heat map colors. 
#      This was to ensure that all graphs would be colored and that the
#      corresponding colors from the correlations would go onto the 
#      correlation plots for relationships between the continous variables
#      (i.e., the non-Sartorian variables) only. The colors were added
#      manually to those extracted from the heat map:
colorsi_Silva_UK2 <- c("#CDCBC9FF", "#DEBFA0FF", "#DAC2ACFF", "#EAB172FF" ,
                       "#CCCCCC", "#D3C7BDFF", "#EEAB5DFF", "#E9B378FF", "#CCCCCC",
                       "#DEBFA0FF", "#EBAF6DFF", "#CCCCCC", "#EBAE6AFF",
                       "#CCCCCC", "#CCCCCC")


# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
p <- 6
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1i_Silva_UK,k1,k2) +
      theme(panel.background = element_rect(fill = colorsi_Silva_UK2[idx], color="white"),
            panel.grid.major = element_line(color=colorsi_Silva_UK2[idx]))
    p1i_Silva_UK <- putPlot(p1i_Silva_UK,plt,k1,k2)
    idx <- idx+1
  }
}


#change resolution. https://www.r-bloggers.com/high-resolution-figures-in-r/

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny28_UK_Relationship_Silva_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_Silva_UK + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (UK)",
                    caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny28_UK_Relationship_Silva_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_Silva_UK + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (UK)",
                    caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 29 - US Sample (Castanho Silva et al Scale)  ####

## This is the same Figure as FIGURE 3 (cf. STEP 2D), except that a title and caption is already added
## to the figure for the Shiny App:

# Save as PNG
# >>> Change path to folder for PNG figures:
png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny29_US_Relationship_Silva_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=300)
p1b + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (US)",
           caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#TIF Version
# >>> Change path to folder for TIFF figures:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny29_US_Relationship_Silva_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1b + labs(title = "Castanho Silva et al. Scale - Dimensions and Aggregates (US)",
           caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 30 - Germany 2017 Short Term Panel (Schulz et al Scale)  ####

## This is the same Figure as FIGURE 2 (cf. STEP 2C), except that a title and caption is already added
## to the figure for the Shiny App:

# Save as PNG
# >>> Change path to folder for PNG figures:
png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny30_DEU_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=300)
p1 + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Germany 2017)",
           caption = "Data source: 2017 GLES Short-Term Campaign Panel")
dev.off()

#TIF Version
# >>> Change path to folder for TIFF figures:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny30_DEU_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1 + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Germany 2017)",
           caption = "Data source: 2017 GLES Short-Term Campaign Panel")
dev.off()


#### <<>> Figure Shiny 31 - Brazil Sample (Schulz et al Scale)  ####
# This Figure was already generated for Supplement 3 (see STEP 3A).
# The figures are only saved again with a header and caption for the 
# Shiny App.
png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny31_Brazil_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_Brazil + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Brazil)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#TIF Version
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny31_Brazil_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_Brazil + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Brazil)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#### <<>> Figure Shiny 32 - France Sample (Schulz et al Scale)  ####
# This Figure was already generated for Supplement 3 (see STEP 3A).
# The figures are only saved again with a header and caption for the 
# Shiny App.

png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny32_France_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_France + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (France)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny32_France_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_France + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (France)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 33 - Greece Sample (Schulz et al Scale)  ####
# This Figure was already generated for Supplement 3 (see STEP 3A).
# The figures are only saved again with a header and caption for the 
# Shiny App.
png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny33_Greece_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_Greece + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Greece)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny33_Greece_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_Greece + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Greece)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 34 - Ireland Sample (Schulz et al Scale)  ####
# This Figure was already generated for Supplement 3 (see STEP 3A).
# The figures are only saved again with a header and caption for the 
# Shiny App.
png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny34_Ireland_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_Ireland + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Ireland)",
                         caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny34_Ireland_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_Ireland + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Ireland)",
                         caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 35 - Italy Sample (Schulz et al Scale)  ####
# This Figure was already generated for Supplement 3 (see STEP 3A).
# The figures are only saved again with a header and caption for the 
# Shiny App.
png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny35_Italy_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_Italy + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Italy)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tif Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny35_Italy_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_Italy + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Italy)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 36 - Mexico Sample (Schulz et al Scale)  ####
# This Figure was already generated for Supplement 3 (see STEP 3A).
# The figures are only saved again with a header and caption for the 
# Shiny App.
png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny36_Mexico_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_Mexico + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Mexico)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny36_Mexico_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_Mexico + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Mexico)",
                        caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 37 - Spain Sample (Schulz et al Scale)  ####
# This Figure was already generated for Supplement 3 (see STEP 3A).
# The figures are only saved again with a header and caption for the 
# Shiny App.
png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny37_Spain_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_Spain + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Spain)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny37_Spain_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_Spain + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (Spain)",
                       caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 38 - UK Sample (Schulz et al Scale)  ####
# This Figure was already generated for Supplement 3 (see STEP 3A).
# The figures are only saved again with a header and caption for the 
# Shiny App.
png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny38_UK_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=800)
p1i_SilvaSchulz_UK + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (UK)",
                    caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#Tiff Version:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny3_UK_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_UK + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (UK)",
                    caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()


#### <<>> Figure Shiny 39 - US Sample (Schulz et al Scale)  ####
# This Figure was already generated for Supplement 3 (see STEP 3A).
# The figures are only saved again with a header and caption for the 
# Shiny App.
png("##PATH##/Plots/Appendix/ShinyApp_Figure_Shiny39_US_Relationship_Schulz_Scale_SingleDimensions.png",
    units="in", width=7.5, height=5.8, res=300)
p1i_SilvaSchulz_US + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (US)",
           caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()

#TIF Version
# >>> Change path to folder for TIFF figures:
tiff("##PATH##/Plots/TIFF/Appendix TIFF/ShinyApp_Figure_Shiny39_US_Relationship_Schulz_Scale_SingleDimensions.tiff",
     units="in", width=7.5, height=5.8, res=600, compression = "lzw")
p1i_SilvaSchulz_US + labs(title = "Schulz et al. Scale - Dimensions and Aggregates (US)",
           caption = "Data source: Castanho Silva et al. 2018 Replication Dataset")
dev.off()




#/////////////////////////////////////////////////////////#
#### Sources and links to useful tips used         ####
#/////////////////////////////////////////////////////////#

# Identifying the right colors: We used a color scheme with 3 classes, diverging nature, and 
# colorblind safe, print friendly, and photocopy safe properties, choosen from here:
# http://colorbrewer2.org/#type=diverging&scheme=PuOr&n=3 (last accessed: November 22, 2018)

# Rounding correlation coefficients in ggpairs:
# https://stackoverflow.com/questions/47496364/rounding-digits-in-ggpairs (last accessed: November 22, 2018)

#### END OF R-SCRIPT####