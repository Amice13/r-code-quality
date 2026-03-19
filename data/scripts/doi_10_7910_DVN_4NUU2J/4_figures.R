# !diagnostics off


rm( list = ls ( ) )
############################################# Tables and Figures for ##################################################
###### "The European Union Emissions Trading Scheme and Fuel Efficiency of Fossil Fuel Power Plants in Germany" #######
################################ by Robert Germeshausen ##############################################################
#######################################################################################################################

######## Preparation #######
# Set working directory
setwd( "..." )

# Set results directory 
results_wd <- "..."


# Load necessary packages (install if neccessary)
library( dplyr ); library( ggplot2 ); library( xtable ); library( data.table ); library( scales ); library( stringr ); library( readr );
library( magrittr)

# Penalize scientific writing of numbers and show only 4 digits
options( scipen = 999, digits = 4 )

####### Multiplot function #########
# Load multiplot function, source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



####### 85 percent of fossil fuel electricity generation in Germany on average (2003-2012) mentioned in Introduction #######
coverage_fossil <- readr::read_delim( "tab_gen_sample_conv_share.csv", delim = "\t", locale = locale( decimal_mark = "." ), trim_ws = TRUE, na = c( "", "NA", "." ) )
round( coverage_fossil$da_share_generation_conv[ 11 ], digits = 2 )

rm( coverage_fossil )


####### P90 Values ETS Intensity mentioned in Introduction #######
# P90 values - all plants
ETS_total <- readr::read_delim( "tab_tr95_ti_eff_level_tr95.csv", delim = "\t", locale = locale( decimal_mark = "." ), trim_ws = TRUE, na = c( "", "NA", "." ) )
round( ETS_total$p90[ 11 ], digits = 0 )

# P90 values - hard coal plants
ETS_coal <- readr::read_delim( "tab_tr95_ti_eff_1_tr95.csv", delim = "\t", locale = locale( decimal_mark = "." ), trim_ws = TRUE, na = c( "", "NA", "." ) )
round( ETS_coal$p90[ 11 ], digits = 0 )

# P90 values - lignite plants
ETS_lignite <- readr::read_delim( "tab_tr95_ti_eff_2_tr95.csv", delim = "\t", locale = locale( decimal_mark = "." ), trim_ws = TRUE, na = c( "", "NA", "." ) )
round( ETS_lignite$p90[ 11 ], digits = 0 )

# P90 values - natural gas plants
ETS_gas <- readr::read_delim( "tab_tr95_ti_eff_3_tr95.csv", delim = "\t", locale = locale( decimal_mark = "." ), trim_ws = TRUE, na = c( "", "NA", "." ) )
round( ETS_gas$p90[ 11 ], digits = 0 )


####### Average carbon cost in 2005-2012 in sample mentioned in Introduction #######
round( weighted.mean( ETS_total$mean[ 3:10 ], ETS_total$N[ 3:10 ] ), digits = 0 )


######### Within variation of ETS intensity based on coefficient of variation mentioned in Introduction ############
cv_yearly <- function( x, variable ){
  b <- ( x$sd / x$mean )  
  return( b )
}

cv_total_yearly <- as.data.frame(
                    cbind( year = c( seq( from = 2003, to = 2012, by = 1 ), "Total" ), 
                          total = cv_yearly( x = ETS_total ),
                          coal = cv_yearly( x = ETS_coal ),
                          lignite = cv_yearly( x = ETS_lignite ),
                          gas = cv_yearly( x = ETS_gas ) ) )


# CV Total
round( mean( as.numeric( cv_total_yearly[ 3:10, 2 ] ) ) / as.numeric( cv_total_yearly[ 11, 2 ] ), digits = 1 )
# CV within hard coal
round( mean( as.numeric( as.character( cv_total_yearly[ 3:10, 3 ] ) ) ) / as.numeric( as.character( cv_total_yearly[ 11, 3 ] ) ), digits = 1 )
# CV within lignite
round( mean( as.numeric( as.character( cv_total_yearly[ 3:10, 4 ] ) ) ) / as.numeric( as.character( cv_total_yearly[ 11, 4 ] ) ), digits = 1 )
# CV within natural gas
round( mean( as.numeric( as.character( cv_total_yearly[ 3:10, 5 ] ) ) ) / as.numeric( as.character( cv_total_yearly[ 11, 5 ] ) ), digits = 1 )



### Figure 1: Boxplot per Fuel Type: ETS Intensity TR95 ##############
h <- function( dat ){
  a <- as.data.frame( do.call( "rbind", stringr::str_split( dat$X1, pattern = ":" ) ) )
  x <- cbind( a, dat[ , 2:10 ] )
  colnames( x ) <- c( "year", "variable", "N", "mean", "sd", "range", "p10", "p25", "p50", "p75", "p90" )
  if( sum( is.na( x$p10 ) ) >= 1 ){
    x$ab1 <- 0
    x$ab2 <- as.numeric( x$p25 )
    x$ab3 <- x$p50 - x$p25
    x$ab4 <- x$p75 - x$p50  
    x$ab5 <- 0
  } else{
    x$ab1 <- x$p10
    x$ab2 <- x$p25 - x$p10
    x$ab3 <- x$p50 - x$p25
    x$ab4 <- x$p75 - x$p50  
    x$ab5 <- x$p90 - x$p75  
  }  
  
  return( x )
}

tr95_ti_total <- h( dat = readr::read_delim( "tab_tr95_ti_eff_level_tr95.csv", delim = "\t", na = c( "-", "." ), locale = locale( decimal_mark = "." ), trim_ws = TRUE  ) )
tr95_ti_coal <- h( dat = readr::read_delim( "tab_tr95_ti_eff_1_tr95.csv", delim = "\t", na = c( "-", "." ), locale = locale( decimal_mark = "." ), trim_ws = TRUE  ) )
tr95_ti_lignite <- h( dat = readr::read_delim( "tab_tr95_ti_eff_2_tr95.csv", delim = "\t", na = c( "-", "." ), locale = locale( decimal_mark = "." ), trim_ws = TRUE  ) )
tr95_ti_lignite$ab2 <- tr95_ti_lignite$ab1 + tr95_ti_lignite$ab2
tr95_ti_lignite$ab1 <- 0
tr95_ti_lignite$ab5 <- 0
tr95_ti_gas <- h( dat = readr::read_delim( "tab_tr95_ti_eff_3_tr95.csv", delim = "\t", na = c( "-", "." ), locale = locale( decimal_mark = "." ), trim_ws = TRUE  ) )

sum( tr95_ti_total$mean[ 3:10 ] * tr95_ti_total$N[ 3:10 ] ) / sum( tr95_ti_total$N[ 3:10 ] ) 


box_plot_f <- function( dat, y_lab, y_down, y_up ){
  x = melt( dat[ dat$year != "Total", c( "year", "ab1", "ab2", "ab3", "ab4", "ab5" ) ], id.vars = "year" )
  ggplot( data = x[ order( x$variable, decreasing = TRUE ), ], aes( x = year ) ) +
    theme_bw( ) +
    geom_bar( aes( x = year,  y = value, fill = factor( variable, levels = c( "ab5", "ab4", "ab3", "ab2", "ab1" ) ), 
                   colour = factor( variable, levels = c( "ab5", "ab4", "ab3", "ab2", "ab1" ) ) ,
                   linetype = factor( variable, levels = c( "ab5", "ab4", "ab3", "ab2", "ab1" ) ) ),
              stat = "identity", position = "stack" ) +
    labs( x = "Year", y = y_lab) + 
    theme( aspect.ratio = ( 2 / 4 ) ) +
    theme( panel.grid = element_blank( ) ) + 
    scale_y_continuous( expand = c( 0, 0 ), limits = c( y_down, y_up ) ) + 
    theme( legend.position = "None") +
    scale_fill_manual( values = c( "White", "White", "White", "White", "White" ) ) +
    scale_colour_manual( values = c( "Black", "Black", "Black", "Black", "White" ) ) +
    scale_linetype_manual( values = c( "dashed", "solid", "solid", "dashed", "solid" ) ) +
    theme( axis.text.x = element_text( size = 10 ), axis.text.y = element_text( size = 10 ), 
           axis.title.y = element_text( size = 10, vjust = 1 ),  axis.title.x = element_text( size = 10, vjust = -0.5 ) ) 
}


plot_ti_total <- box_plot_f( dat = tr95_ti_total, y_lab = "EUR / MWh" , y_down = 0, y_up = 25 ) + ggtitle( "A Total" )
plot_ti_coal <- box_plot_f( dat = tr95_ti_coal, y_lab = "EUR / MWh" , y_down = 0, y_up = 25 ) + ggtitle( "C Hard Coal" )

y <- tr95_ti_lignite[ tr95_ti_lignite$year != "Total", c( "year", "ab2", "ab3", "ab4" ) ]   
x <- data.table::melt( y , id.vars = "year" )
plot_ti_lignite <- ggplot( data = x[ order( x$variable, decreasing = TRUE ), ], aes( x = year ) ) +
  theme_bw( ) +
  geom_bar( aes( x = year,  y = value, fill = factor( variable, levels = c( "ab4", "ab3", "ab2" ) ), 
                 colour = factor( variable, levels = c( "ab4", "ab3", "ab2" ) ) ,
                 linetype = factor( variable, levels = c( "ab4", "ab3", "ab2" ) ) ),
            stat = "identity", position = "stack" ) +
  labs( x = "Year", y = "EUR / MWh") + 
  theme( aspect.ratio = ( 2 / 4 ) ) +
  theme( panel.grid = element_blank( ) ) + 
  scale_y_continuous( expand = c( 0, 0 ), limits = c( 0, 25 ) ) + 
  theme( legend.position = "None" ) +
  scale_fill_manual( values = c( "White", "White", "White" ) ) +
  scale_colour_manual( values = c( "Black", "Black", "White" ) ) +
  scale_linetype_manual( values = c( "solid", "solid", "solid" ) ) +
  theme( axis.text.x = element_text( size = 10 ), axis.text.y = element_text( size = 10 ), 
         axis.title.y = element_text( size = 10, vjust = 1 ),  axis.title.x = element_text( size = 10, vjust = -0.5 ) ) + 
  ggtitle( "B Lignite" )

plot_ti_gas <- box_plot_f( dat = tr95_ti_gas, y_lab = "EUR / MWh" , y_down = 0, y_up = 25 ) + ggtitle( "D Natural Gas" )

# Print plot
pdf( paste( results_wd, "intensity_el_tr95.pdf", sep = "" ), width = 12, height = 6 )
multiplot( plot_ti_total, plot_ti_coal, plot_ti_lignite, plot_ti_gas , cols = 2 )
dev.off( )


############# Figure B-1: Average Fuel and Carbon Cost in Power Generation ###########
CO2_Cost <- readr::read_delim( "CO2_Cost.csv", locale = locale( decimal_mark = "." ), trim_ws = TRUE, delim = ";" )

dat.m <- data.table::melt( CO2_Cost, id.vars = "Year" )

dat.m$type <- stringr::str_split_fixed( as.character( dat.m$variable ), pattern = "_" , n = 4 )[ , 4 ]
dat.m$fueltype <- stringr::str_split_fixed( as.character( dat.m$variable ), pattern = "_" , n = 4 )[ , 1 ]
dat.m$fueltype[ dat.m$fueltype == "Gas" ] <- "Natural Gas"
dat.m$fueltype[ dat.m$fueltype == "Coal" ] <- "Hard Coal"
dat.m$variable <- factor( dat.m$variable, levels = levels( factor( dat.m$variable ) )[ length( levels( factor( dat.m$variable ) ) ):1 ] )

ets_cost_plot <- function( type, fueltype, quelle, title ){
  ggplot( data = dat.m[ dat.m$type == type & dat.m$fueltype == fueltype, ], aes( x = as.factor( Year ) ) ) +
    theme_bw( ) +
    geom_bar( aes( x = as.factor( Year ),  y = value, fill = variable ), stat = "identity", position = "stack" ) +
    scale_fill_manual( values = c( "#999999", "#333333" ), labels = c( "Carbon", "Fuel" ) )  + 
    labs( x = "Year", y = "Euros per MWh") + 
    theme( aspect.ratio = ( 2 / 4 ) ) +
    scale_y_continuous( expand = c( 0, 0 ), limits = c( 0, 80 ) ) +
    theme( panel.grid = element_blank( ) ) + 
    theme( axis.line = element_line( color = "black" ) ) +
    theme( axis.text.x = element_text( size = 10 ), axis.text.y = element_text( size = 10 ) ) +
    theme( axis.title.y = element_text( size = 10, vjust = 1 ),  axis.title.x = element_text( size = 10, vjust = -0.5 ) ) +
    theme( legend.position = "bottom", legend.title = element_blank( ), legend.direction = "horizontal" ) +
    theme( legend.text = element_text( size = 10 ) ) +
    theme( legend.key = element_blank( ), legend.key.height = unit( 1, "line" ), legend.key.width = unit( 1, "line" ) ) +
    ggtitle( title ) +  theme( plot.title = element_text( hjust = 0 ) )
} 


coal_el <- ets_cost_plot( type = "El", fueltype = "Hard Coal", title = "A Hard Coal" )  
gas_el <- ets_cost_plot( type = "El", fueltype = "Natural Gas", title = "B Natural Gas" )  
coal_f <- ets_cost_plot( type = "Fuel", fueltype = "Hard Coal" , title = "A Hard Coal" )  
gas_f <- ets_cost_plot( type = "Fuel", fueltype = "Natural Gas", title = "B Natural Gas" )  


# Print plot
pdf( paste( results_wd, "avg_intensity_el.pdf", sep = "" ), width = 12, height = 6 )
multiplot( coal_el, gas_el, cols = 2 )
dev.off( ) 




############# Figure C-1: Total Aggregate Investments in Machinery over Time ###########
# Read in sample data
inv_fuel_jahr_sum <- read.csv( "bi_Code3001_fuel_jahr_sum.txt", sep = "\t" )
inv_jahr_sum <- aggregate( bi_Code3001 ~ jahr, data = inv_fuel_jahr_sum, FUN = "sum" )
inv_jahr_sum$index_2005 <- ( inv_jahr_sum$bi_Code3001 / inv_jahr_sum$bi_Code3001[ inv_jahr_sum$jahr == 2005 ] ) * 100
colnames( inv_jahr_sum ) <- c("Year", "invest", "index_2005")
inv_jahr_sum$type <- "Sample"

# Read in aggregate data
investment <- read.csv( "investment.csv", sep = ";" )
investment$X <- NULL


# index in invest machinery on 2005 base year
investment$index_2005 <- ( investment$Invest_Machinery / investment$Invest_Machinery[ investment$Year == 2005 ] ) * 100

# combine with afid sample
invest_index <- investment[ , c( "Year", "index_2005" ) ]
invest_index$type <- "Aggregate" 

invest_index <- rbind( invest_index, inv_jahr_sum[, c( 1, 3:4 ) ] )
invest_index$type <- as.factor( invest_index$type )


index_figure <- ggplot( data = invest_index[ invest_index$Year > 1992, ] ) +
  theme_bw( ) +
  geom_line( aes( x = Year, y = index_2005, linetype = type ) ) + 
  geom_hline( aes( yintercept = 100 ) ) + 
  scale_y_continuous( limits = c( 0, 250 ), expand = c( 0, 0 ) ) +
  labs( x = "Year", y = "Investment in Machinery (Index: 2005 = 100)" ) + 
  theme( aspect.ratio = ( 2 / 4 ) ) +
  theme( panel.grid = element_blank( ) ) + 
  theme( axis.line = element_line( color = "black" ) ) +
  theme( axis.text.x = element_text( size = 10 ), axis.text.y = element_text( size = 10 ) ) +
  theme( axis.title.y = element_text( size = 10, vjust = 1 ),  axis.title.x = element_text( size = 10, vjust = -0.5 ) ) +
  theme( legend.position = "bottom", legend.title = element_blank( ), legend.direction = "horizontal" ) +
  theme( legend.text = element_text( size = 10 ) ) +
  theme( legend.key = element_blank( ), legend.key.height = unit( 1, "line" ), legend.key.width = unit( 1, "line" ) )


# Print plot
pdf( paste( results_wd, "invest_index.pdf", sep = "" ), width = 12, height = 6 )
index_figure
dev.off( ) 




############ Figure C-2: Investments in Machinery Index 2005 by efficiency quantiles ###########
### All investments:
# Read in 
inv_eff_bin_file <- c( paste( "tr95_investments_by_efficiency_bin", seq( 1, 4, 1 ) ,".csv", sep = "" ) )

# Calculate index (2005 = 100) for investments
inv_eff_bin <- lapply( inv_eff_bin_file, function( x ){
  y <- read.csv( x, sep = "\t" )
  y$sum <- y$N * y$mean
  a <- as.data.frame( do.call( "rbind", strsplit( as.character( y$X ), split = ":") ) )
  a$V1 <- as.character( a$V1 )
  y <- as.data.frame( cbind( Year = a[ 1:10, 1 ], inv_mach_mean = y[ 1:10 , 3 ], 
                             inv_mach_sum = y[ 1:10, 5 ]  ) )
  y$Year <- as.integer( as.character( y$Year ) ) 
  y$inv_mach_mean <- as.numeric( as.character( y$inv_mach_mean ) ) 
  y$inv_mach_sum <- as.numeric( as.character( y$inv_mach_sum ) ) 
  y$index_2005_mean <- ( y$inv_mach_mean / y$inv_mach_mean[ y$Year == 2005 ] ) * 100
  y$index_2005_sum <- ( y$inv_mach_sum / y$inv_mach_sum[ y$Year == 2005 ] ) * 100
  y$type <- "Sample"
  return( y )
})

# Set names for different panels
names( inv_eff_bin ) <- c( "<= P25", ">P25 - <= P50", ">P50 - <= P75", ">P75" )
panel <- c( "A", "B", "C", "D" )

# Generate plots for all four efficency classes
for( i in 1:length( inv_eff_bin ) ){
  x <-  ggplot( data = inv_eff_bin[[ i ]] ) +
    theme_bw( ) +
    geom_line( aes( x = as.Date( as.character( Year ), "%Y" ), y = index_2005_sum ) ) + 
    geom_hline( aes( yintercept = 100 ) ) + 
    scale_y_continuous( limits = c( 0, 160 ), expand = c( 0, 0 ) ) + 
    scale_x_date( labels = date_format( "%Y"), breaks = date_breaks( "years" ) ) + 
    labs( x = "Year", y = "Investment in Machinery \n (Index: 2005 = 100)" )  + 
    ggtitle( paste( panel[ i ], names( inv_eff_bin )[ i ], "- efficiency bin ", sep = " " ) ) +
    theme( aspect.ratio = ( 2 / 4 ) ) +
    theme( panel.grid = element_blank( ) ) + 
    theme( plot.title = element_text( size = 14 ) ) +
    theme( axis.line = element_line( color = "black" ) ) +
    theme( axis.text.x = element_text( size = 12 ), axis.text.y = element_text( size = 12 ) ) +
    theme( axis.title.y = element_text( size = 12, vjust = 1 ),  
           axis.title.x = element_text( size = 12, vjust = -0.5 ) ) 
  assign( paste( "plot_inv_eff_", i, sep = "" ), x )
}

# Print plot
pdf( paste( results_wd, "invest_eff.pdf",  sep = "" ), width = 12, height = 6 )
multiplot( plot_inv_eff_1, plot_inv_eff_2, plot_inv_eff_3, plot_inv_eff_4, layout = matrix( 1:4, ncol = 2, byrow = TRUE ) )
dev.off( )



############## Figure C-3: Investments in Machinery Index 2005 by fuel type ###########

### Read-in average investments in machinery of all plants and differentiated by fuel types for TR95 sample
# Define file names
inv_file <- c( # Alle Plants
                "tab_inv_mach_level_tr95.csv", 
                # Hard Coal Plants
                "tab_inv_mach_level_1_tr95.csv",
                # Lignite Plants
                "tab_inv_mach_level_2_tr95.csv", 
                # Natural Gas Plants
                "tab_inv_mach_level_3_tr95.csv" )

# Function to read in and modify data frames of investments in machinery
inv_jahr_mean <- lapply( inv_file, function( x ){
  y <- read.csv( x, sep = "\t" )
  a <- as.data.frame( do.call( "rbind", strsplit( as.character( y$X ), split = ":") ) )
  a$V1 <- as.character( a$V1 )
  y <- as.data.frame( cbind( Year = a[ 1:10, 1 ], inv_mach = y[ 1:10 , 3 ] ) )
  y$Year <- as.integer( as.character( y$Year ) ) 
  y$inv_mach <- as.numeric( as.character( y$inv_mach ) ) 
  y$index_2005 <- ( y$inv_mach / y$inv_mach[ y$Year == 2005 ] ) * 100
  y$type <- "Sample"
  return( y )
} ) 

inv_jahr_mean[[ 1 ]] <- NULL
names( inv_jahr_mean ) <- c( "Hard Coal", "Lignite", "Natural Gas" )
panel <- c( "A", "C", "B" )


for( i in 1:length( inv_jahr_mean ) ){
  x <-  ggplot( data = inv_jahr_mean[[ i ]] ) +
          theme_bw( ) +
          geom_line( aes( x = as.Date( as.character( Year ), "%Y" ), y = index_2005 ) ) + 
          geom_hline( aes( yintercept = 100 ) ) + 
          scale_y_continuous( limits = c( 0, 500 ), expand = c( 0, 0 ) ) + 
          scale_x_date( labels = date_format( "%Y"), breaks = date_breaks( "years" ) ) + 
          labs( x = "Year", y = "Investment in Machinery \n (Index: 2005 = 100)" )  + 
          ggtitle( paste( panel[ i ], names( inv_jahr_mean )[ i ], sep = " " ) ) +
          theme( aspect.ratio = ( 2 / 4 ) ) +
          theme( panel.grid = element_blank( ) ) + 
          theme( plot.title = element_text( size = 14 ) ) +
          theme( axis.line = element_line( color = "black" ) ) +
          theme( axis.text.x = element_text( size = 12 ), axis.text.y = element_text( size = 12 ) ) +
          theme( axis.title.y = element_text( size = 12, vjust = 1 ),  
                 axis.title.x = element_text( size = 12, vjust = -0.5 ) ) 
  assign( paste( "plot_inv_type_", i, sep = "" ), x )
}


pdf( paste( results_wd, "invest_fuel.pdf",  sep = "" ), width = 12, height = 6 )
multiplot( plot_inv_type_1, plot_inv_type_3, plot_inv_type_2, layout = matrix( 1:4, ncol = 2, byrow = TRUE ) )
dev.off( )







####### Tables D-1 - D-3: Summary Statistics (Total, Hard Coal, Lignite, Natural Gas) #########
files_sum_total <- c( # Bottleneck Capacity level
                      "tab_k_level_tr95.csv", 
                      # Labor Input level
                      "tab_l_levels_tr95.csv",    
                      # Fuel Input level
                      "tab_m_level_tr95.csv", 
                      # Output level
                      "tab_y_level_tr95.csv",  
                      # Total Investments level
                      "tab_inv_level_tr95.csv", 
                      # Investments in Machinery level
                      "tab_inv_mach_level_tr95.csv", 
                      # Investments in Machinery (large) binary
                      "tab_inv_mach_large_binary_tr95.csv", 
                      # Capacity Factor
                      "tab_10_tr95.csv",
                      # ETS Ex-Ante Exposure
                      "tab_40.csv",
                      # ETS Intensity
                      "tab_tr95_ti_eff_level_tr95.csv",
                      # Fuel Price Ex-Ante Exposure
                      "tab_50.csv",
                      # Fuel Price Intensity
                      "tab_46.csv" )

files_sum_coal <- c(  # Bottleneck Capacity level
                      "tab_k_level_1_tr95.csv",  
                      # Labour Input level
                      "tab_l_levels_1_tr95.csv", 
                      # Fuel Input level
                      "tab_m_level_1_tr95.csv", 
                      # Output level
                      "tab_y_level_1_tr95.csv",  
                      # Total Investments level
                      "tab_inv_level_1_tr95.csv", 
                      # Investments in Machinery level
                      "tab_inv_mach_level_1_tr95.csv", 
                      # Investments in Machinery (large) binary
                      "tab_inv_mach_large_binary_1_tr95.csv", 
                      # Capacity Factor
                      "tab_10_1_tr95.csv", 
                      # ETS Ex-Ante Exposure
                      "tab_40_1.csv", 
                      # ETS Intensity
                      "tab_tr95_ti_eff_1_tr95.csv",
                      # Fuel Price Ex-Ante Exposure
                      "tab_50_1.csv",
                      # Fuel Price Intensity
                      "tab_46_1.csv" )
                      

files_sum_lignite <- c( # Bottleneck Capacity level
                        "tab_k_level_2_tr95.csv", 
                        # Labor Input level
                        "tab_l_levels_2_tr95.csv", 
                        # Fuel Input level
                        "tab_m_level_2_tr95.csv", 
                        # Output level
                        "tab_y_level_2_tr95.csv", 
                        # Total investment level
                        "tab_inv_level_2_tr95.csv", 
                        # Investments in Machinery level
                        "tab_inv_mach_level_2_tr95.csv", 
                        # Investments in Machinery (large) binary
                        "tab_inv_mach_large_binary_2_tr95.csv", 
                        # Capacity Factor
                        "tab_10_2_tr95.csv", 
                        # ETS Ex-Ante Exposure
                        "tab_40_2.csv", 
                        # ETS Intensity
                        "tab_tr95_ti_eff_2_tr95.csv",
                        # Fuel Price Ex-Ante Exposure
                        "tab_50_2.csv",
                        # Fuel Price Intensity
                        "tab_46_2.csv" )
                        

files_sum_gas <- c( # Bottleneck Capacity level
                    "tab_k_level_3_tr95.csv", 
                    # Labour Input level
                    "tab_l_levels_3_tr95.csv", 
                    # Fuel Input level
                    "tab_m_level_3_tr95.csv", 
                    # Output level
                    "tab_y_level_3_tr95.csv", 
                    # Total investment level
                    "tab_inv_level_3_tr95.csv", 
                    # Investments in Machinery level
                    "tab_inv_mach_level_3_tr95.csv", 
                    # Investments in Machinery (large) binary
                    "tab_inv_mach_large_binary_3_tr95.csv", 
                    # Capacity Factor
                    "tab_10_3_tr95.csv", 
                    # ETS Ex-Ante Exposure
                    "tab_40_3.csv", 
                    # ETS Intensity
                    "tab_tr95_ti_eff_3_tr95.csv",
                    # Fuel Price Ex-Ante Exposure
                    "tab_50_3.csv",
                    # Fuel Price Intensity
                    "tab_46_3.csv" )

ldf_sum_total <- lapply( files_sum_total, function( x ){ read.delim(x,  sep = "\t", colClasses = "character" ) } ) 
ldf_sum_coal <- lapply( files_sum_coal, function( x ){ read.delim(x,  sep = "\t", colClasses = "character" ) } ) 
ldf_sum_lignite <- lapply( files_sum_lignite, function( x ){ read.delim(x,  sep = "\t", colClasses = "character" ) } ) 
ldf_sum_gas <- lapply( files_sum_gas, function( x ){ read.delim(x,  sep = "\t", colClasses = "character" ) } ) 



# summary statistics function
g_total <- function( x ){
  a <- as.data.frame( do.call( "rbind", strsplit( as.character( x$X ), split = ":") ) )
  x <- cbind( a, x[, 2:10 ] )
  colnames( x ) <- c( "year", "variable", "N", "mean", "sd", "range", "p10", "p25", "p50", "p75", "p90" )
  b <- x[ 11, c( "variable", "N", "mean", "sd", "p10", "p25", "p50", "p75", "p90" ) ]
  b$N <- as.integer( b$N )
  b$mean <- as.numeric( b$mean )
  b$sd <- as.numeric( b$sd )
  b$p10 <- as.numeric( b$p10 )
  b$p25 <- as.numeric( b$p25 )
  b$p50 <- as.numeric( b$p50 )
  b$p75 <- as.numeric( b$p75 )
  b$p90 <- as.numeric( b$p90 )
  return( b )
}


sum_variable <- c( "Bottleneck capacity (MW)", "Labor (hours)", "Fuel use (GJ)", "Output (MWh)", 
                   "Total investment (EUR)", "Investment machinery (EUR)", "Investment machinery large (Yes/no)", 
                   "Capacity factor", "ETS ex-ante exposure (EUR/MWh)",
                   "ETS intensity (EUR/MWh)", "Fuel price ex-ante exposure (EUR/MWh)", 
                   "Fuel price intensity (EUR/MWh)" )


g_year <- function( x, jahr ){
  a <- as.data.frame( do.call( "rbind", strsplit( as.character( x$X ), split = ":") ) )
  x <- cbind( a, x[, 2:10 ] )
  colnames( x ) <- c( "year", "variable", "N", "mean", "sd", "range", "p10", "p25", "p50", "p75", "p90" )
  b <- x[ x$year == jahr, c( "variable", "N", "mean", "sd", "p10", "p25", "p50", "p75", "p90" ) ]
  b$N <- as.integer( b$N )
  b$mean <- as.numeric( b$mean )
  b$sd <- as.numeric( b$sd )
  b$p10 <- as.numeric( b$p10 )
  b$p25 <- as.numeric( b$p25 )
  b$p50 <- as.numeric( b$p50 )
  b$p75 <- as.numeric( b$p75 )
  b$p90 <- as.numeric( b$p90 )
  return( b )
}


### 2003
ldf_sum_total_2003 <- lapply( ldf_sum_total, FUN = function( x ) g_year( x, jahr = 2003 ) )
df_sum_total_2003 <- do.call( "rbind", ldf_sum_total_2003 )
df_sum_total_2003 <- cbind( sum_variable, df_sum_total_2003[, 2:9 ] )
df_sum_total_2003 <- xtable( format( df_sum_total_2003, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( df_sum_total_2003, include.rownames = FALSE )

ldf_sum_coal_2003 <- lapply( ldf_sum_coal, FUN = function( x ) g_year( x, jahr = 2003 ) )
df_sum_coal_2003 <- do.call( "rbind", ldf_sum_coal_2003 )
df_sum_coal_2003 <- cbind( sum_variable, df_sum_coal_2003[, 2:9 ] )
df_sum_coal_2003 <- xtable( format( df_sum_coal_2003, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( df_sum_coal_2003, include.rownames = FALSE )

ldf_sum_lignite_2003 <- lapply( ldf_sum_lignite, FUN = function( x ) g_year( x, jahr = 2003 ) )
df_sum_lignite_2003 <- do.call( "rbind", ldf_sum_lignite_2003 )
df_sum_lignite_2003 <- cbind( sum_variable, df_sum_lignite_2003[, 2:9 ] )
df_sum_lignite_2003 <- xtable( format( df_sum_lignite_2003, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( df_sum_lignite_2003, include.rownames = FALSE )

ldf_sum_gas_2003 <- lapply( ldf_sum_gas, FUN = function( x ) g_year( x, jahr = 2003 ) )
df_sum_gas_2003 <- do.call( "rbind", ldf_sum_gas_2003 )
df_sum_gas_2003 <- cbind( sum_variable, df_sum_gas_2003[, 2:9 ] )
df_sum_gas_2003 <- xtable( format( df_sum_gas_2003, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( df_sum_gas_2003, include.rownames = FALSE )


### 2008
ldf_sum_total_2008 <- lapply( ldf_sum_total, FUN = function( x ) g_year( x, jahr = 2008 ) )
df_sum_total_2008 <- do.call( "rbind", ldf_sum_total_2008 )
df_sum_total_2008 <- cbind( sum_variable, df_sum_total_2008[, 2:9 ] )
df_sum_total_2008 <- xtable( format( df_sum_total_2008, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( df_sum_total_2008, include.rownames = FALSE )

ldf_sum_coal_2008 <- lapply( ldf_sum_coal, FUN = function( x ) g_year( x, jahr = 2008 ) )
df_sum_coal_2008 <- do.call( "rbind", ldf_sum_coal_2008 )
df_sum_coal_2008 <- cbind( sum_variable, df_sum_coal_2008[, 2:9 ] )
df_sum_coal_2008 <- xtable( format( df_sum_coal_2008, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( df_sum_coal_2008, include.rownames = FALSE )

ldf_sum_lignite_2008 <- lapply( ldf_sum_lignite, FUN = function( x ) g_year( x, jahr = 2008 ) )
df_sum_lignite_2008 <- do.call( "rbind", ldf_sum_lignite_2008 )
df_sum_lignite_2008 <- cbind( sum_variable, df_sum_lignite_2008[, 2:9 ] )
df_sum_lignite_2008 <- xtable( format( df_sum_lignite_2008, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( df_sum_lignite_2008, include.rownames = FALSE )

ldf_sum_gas_2008 <- lapply( ldf_sum_gas, FUN = function( x ) g_year( x, jahr = 2008 ) )
df_sum_gas_2008 <- do.call( "rbind", ldf_sum_gas_2008 )
df_sum_gas_2008 <- cbind( sum_variable, df_sum_gas_2008[, 2:9 ] )
df_sum_gas_2008 <- xtable( format( df_sum_gas_2008, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( df_sum_gas_2008, include.rownames = FALSE )


### 2012
ldf_sum_total_2012 <- lapply( ldf_sum_total, FUN = function( x ) g_year( x, jahr = 2012 ) )
df_sum_total_2012 <- do.call( "rbind", ldf_sum_total_2012 )
df_sum_total_2012 <- cbind( sum_variable, df_sum_total_2012[, 2:9 ] )
df_sum_total_2012 <- xtable( format( df_sum_total_2012, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( df_sum_total_2012, include.rownames = FALSE )

ldf_sum_coal_2012 <- lapply( ldf_sum_coal, FUN = function( x ) g_year( x, jahr = 2012 ) )
df_sum_coal_2012 <- do.call( "rbind", ldf_sum_coal_2012 )
df_sum_coal_2012 <- cbind( sum_variable, df_sum_coal_2012[, 2:9 ] )
df_sum_coal_2012 <- xtable( format( df_sum_coal_2012, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( df_sum_coal_2012, include.rownames = FALSE )

ldf_sum_lignite_2012 <- lapply( ldf_sum_lignite, FUN = function( x ) g_year( x, jahr = 2012 ) )
df_sum_lignite_2012 <- do.call( "rbind", ldf_sum_lignite_2012 )
df_sum_lignite_2012 <- cbind( sum_variable, df_sum_lignite_2012[, 2:9 ] )
df_sum_lignite_2012 <- xtable( format( df_sum_lignite_2012, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( df_sum_lignite_2012, include.rownames = FALSE )

ldf_sum_gas_2012 <- lapply( ldf_sum_gas, FUN = function( x ) g_year( x, jahr = 2012 ) )
df_sum_gas_2012 <- do.call( "rbind", ldf_sum_gas_2012 )
df_sum_gas_2012 <- cbind( sum_variable, df_sum_gas_2012[, 2:9 ] )
df_sum_gas_2012 <- xtable( format( df_sum_gas_2012, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( df_sum_gas_2012, include.rownames = FALSE )




###### Table D-4: Summary Statistics: Total (2003 - 2012) for regression specification based on Table 1 ############
sum_total_reg <- readr::read_delim( "tab_fuel_eq_col45_tr95.csv", delim = "\t", locale = locale( decimal_mark = "." ), 
                                    trim_ws = TRUE, na = c( "", "NA", "." ) )

sum_total_reg <- t( sum_total_reg )
var <- as.character( row.names( sum_total_reg ) )
colnames( sum_total_reg ) <- sum_total_reg[ 1, ]
sum_total_reg <- dplyr::as_tibble( sum_total_reg )

sum_total_reg <- cbind( var, sum_total_reg )
sum_total_reg$var <- as.character( sum_total_reg$var )
sum_total_reg <- sum_total_reg[ -c( 1, nrow( sum_total_reg ) ), ]


sum_variable_label <- dplyr::as_tibble( cbind( 
                                      label = c( "Bottleneck capacity (MW)", "Labor (hours)", "Wages (EUR/hours)", 
                                                 "Fuel use (GJ)", "Output (MWh)", 
                                                 "Total investment (EUR)","Investment machinery (EUR)", "Investment machinery large (Yes/no)", 
                                                 "Capacity factor", "ETS ex-ante exposure (EUR/MWh)",
                                                 "ETS intensity (EUR/MWh)", "Fuel price ex-ante exposure (EUR/MWh)", 
                                                 "Fuel price intensity (EUR/MWh)" ), 
                                      var = c(   "k_level", "dn_mbe_EF24_sum_mean", "wage_level", 
                                                 "m_level", "y_level", 
                                                 "inv_level", "inv_mach_level", "inv_mach_large", 
                                                 "dn_cf_gr_elec_mon", "tr95_z_ti_eff", 
                                                 "tr95_ti_eff", "tr95_fp_eff_z", 
                                                 "tr95_fp_eff" ) ) )


sum_total_reg <- dplyr::left_join( sum_total_reg, sum_variable_label, by = "var" )
sum_total_reg <- sum_total_reg[ !is.na( sum_total_reg$label ), ]
sum_total_reg <- sum_total_reg[ order( match( sum_total_reg$label, sum_variable_label$label ) ), ]
sum_total_reg <- sum_total_reg[ , c( "label", "N", "mean", "sd", "p10", "p25", "p50", "p75", "p90" ) ]
colnames( sum_total_reg ) <- c( "Variable", "N", "Mean", "S.D.", "p10", "p25", "p50", "p75", "p90" )

sum_total_reg[ , 2:9 ] <- as.numeric( as.character( unlist( sum_total_reg[ , 2:9 ] ) ) )
sum_total_reg$N <- as.integer( sum_total_reg$N )

sum_total_reg <- xtable( format( sum_total_reg, big.mark = ",", trim = TRUE, scientific = FALSE, digits = 2, nsmall = 2 ) )
print( sum_total_reg, include.rownames = FALSE )



### Figure D-1: Boxplot per Fuel Type: ETS Ex-Ante Exposure TR95 ##############
h <- function( dat ){
  a <- as.data.frame( do.call( "rbind", stringr::str_split( dat$X1, pattern = ":" ) ) ) 
  x <- cbind( a, dat[ , 2:10 ] )
  colnames( x ) <- c( "year", "variable", "N", "mean", "sd", "range", "p10", "p25", "p50", "p75", "p90" )
  if( sum( is.na( x$p10 ) ) >= 1 ){
    x$ab1 <- 0
    x$ab2 <- as.numeric( x$p25 )
    x$ab3 <- x$p50 - x$p25
    x$ab4 <- x$p75 - x$p50  
    x$ab5 <- 0
  } else{
    x$ab1 <- x$p10
    x$ab2 <- x$p25 - x$p10
    x$ab3 <- x$p50 - x$p25
    x$ab4 <- x$p75 - x$p50  
    x$ab5 <- x$p90 - x$p75  
  }  
  
  return( x )
}

tr95_eae_total <- h( dat = readr::read_delim( "tab_40.csv", delim = "\t", na = c( "-", "." ), locale = locale( decimal_mark = "." ), trim_ws = TRUE  ) )
tr95_eae_coal <- h( dat = readr::read_delim( "tab_40_1.csv", delim = "\t", na = c( "-", "." ), locale = locale( decimal_mark = "." ), trim_ws = TRUE  ) )
tr95_eae_lignite <- h( dat = readr::read_delim( "tab_40_2.csv", delim = "\t", na = c( "-", "." ), locale = locale( decimal_mark = "." ), trim_ws = TRUE  ) )
tr95_eae_lignite$ab2 <- tr95_eae_lignite$ab1 + tr95_eae_lignite$ab2
tr95_eae_lignite$ab1 <- 0
tr95_eae_lignite$ab5 <- 0
tr95_eae_gas <- h( dat = readr::read_delim( "tab_40_3.csv", delim = "\t", na = c( "-", "." ), locale = locale( decimal_mark = "." ), trim_ws = TRUE  ) )

box_plot_f <- function( dat, y_lab, y_down, y_up ){
  x = melt( dat[ dat$year != "Total", c( "year", "ab1", "ab2", "ab3", "ab4", "ab5" ) ], id.vars = "year" )
  ggplot( data = x[ order( x$variable, decreasing = TRUE ), ], aes( x = year ) ) +
    theme_bw( ) +
    geom_bar( aes( x = year,  y = value, fill = factor( variable, levels = c( "ab5", "ab4", "ab3", "ab2", "ab1" ) ), 
                   colour = factor( variable, levels = c( "ab5", "ab4", "ab3", "ab2", "ab1" ) ) ,
                   linetype = factor( variable, levels = c( "ab5", "ab4", "ab3", "ab2", "ab1" ) ) ),
              stat = "identity", position = "stack" ) +
    labs( x = "Year", y = y_lab) + 
    theme( aspect.ratio = ( 2 / 4 ) ) +
    theme( panel.grid = element_blank( ) ) + 
    scale_y_continuous( expand = c( 0, 0 ), limits = c( y_down, y_up ) ) + 
    theme( legend.position = "None") +
    scale_fill_manual( values = c( "White", "White", "White", "White", "White" ) ) +
    scale_colour_manual( values = c( "Black", "Black", "Black", "Black", "White" ) ) +
    scale_linetype_manual( values = c( "dashed", "solid", "solid", "dashed", "solid" ) ) +
    theme( axis.text.x = element_text( size = 10 ), axis.text.y = element_text( size = 10 ), 
           axis.title.y = element_text( size = 10, vjust = 1 ),  axis.title.x = element_text( size = 10, vjust = -0.5 ) ) 
}


plot_eae_total <- box_plot_f( dat = tr95_eae_total, y_lab = "EUR / MWh" , y_down = 0, y_up = 25 ) + ggtitle( "A Total" )
plot_eae_coal <- box_plot_f( dat = tr95_eae_coal, y_lab = "EUR / MWh" , y_down = 0, y_up = 25 ) + ggtitle( "C Hard Coal" )

y <- tr95_eae_lignite[ tr95_eae_lignite$year != "Total", c( "year", "ab2", "ab3", "ab4" ) ]   
x <- melt( y , id.vars = "year" )
plot_eae_lignite <- ggplot( data = x[ order( x$variable, decreasing = TRUE ), ], aes( x = year ) ) +
  theme_bw( ) +
  geom_bar( aes( x = year,  y = value, fill = factor( variable, levels = c( "ab4", "ab3", "ab2" ) ), 
                 colour = factor( variable, levels = c( "ab4", "ab3", "ab2" ) ) ,
                 linetype = factor( variable, levels = c( "ab4", "ab3", "ab2" ) ) ),
            stat = "identity", position = "stack" ) +
  labs( x = "Year", y = "EUR / MWh") + 
  theme( aspect.ratio = ( 2 / 4 ) ) +
  theme( panel.grid = element_blank( ) ) + 
  scale_y_continuous( expand = c( 0, 0 ), limits = c( 0, 25 ) ) + 
  theme( legend.position = "None" ) +
  scale_fill_manual( values = c( "White", "White", "White" ) ) +
  scale_colour_manual( values = c( "Black", "Black", "White" ) ) +
  scale_linetype_manual( values = c( "solid", "solid", "solid" ) ) +
  theme( axis.text.x = element_text( size = 10 ), axis.text.y = element_text( size = 10 ), 
         axis.title.y = element_text( size = 10, vjust = 1 ),  axis.title.x = element_text( size = 10, vjust = -0.5 ) ) + 
  ggtitle( "B Lignite" )

plot_eae_gas <- box_plot_f( dat = tr95_eae_gas, y_lab = "EUR / MWh" , y_down = 0, y_up = 25 ) + ggtitle( "D Natural Gas" )

pdf( paste( results_wd, "intensity_eae_tr95.pdf", sep = "" ), width = 12, height = 6 )
multiplot( plot_eae_total, plot_eae_coal, plot_eae_lignite, plot_eae_gas , cols = 2 )
dev.off( )




############## Figure D-2: EUA Spot vs. Forward ########
eua_prices <- read.delim( "eua_prices.txt" )

eua_prices_melt <- melt( eua_prices, id.vars = "Year" )
eua_prices_melt$variable_new <- "EUA Spot Price"
eua_prices_melt$variable_new[ eua_prices_melt$variable == "EUA_forwards" ] <- "EUA Forward Price"
eua_prices_melt$value[ eua_prices_melt$Year < 2005 ] <- NA

eua_prices_plot <- ggplot( data = eua_prices_melt, aes( x = as.factor( Year ), y = value, linetype = variable_new ) ) +
  theme_bw( ) +
  geom_line( aes( x = as.factor( Year ), y = value, group = variable_new ) ) +
  labs( x = "Year", y = "Euro per ton CO2") + 
  theme( aspect.ratio = ( 2 / 4 ) ) +
  scale_y_continuous( expand = c( 0, 0 ), limits = c( 0, 30 ) ) +
  theme( panel.grid = element_blank( ) ) + 
  theme( axis.line = element_line( color = "black" ) ) +
  theme( axis.text.x = element_text( size = 10 ), axis.text.y = element_text( size = 10 ) ) +
  theme( axis.title.y = element_text( size = 10, vjust = 1 ),  axis.title.x = element_text( size = 10, vjust = -0.5 ) ) +
  theme( legend.position = "bottom", legend.title = element_blank( ), legend.direction = "horizontal" ) +
  theme( legend.text = element_text( size = 10 ) ) +
  theme( legend.key = element_blank( ), legend.key.height = unit( 1, "line" ), legend.key.width = unit( 1, "line" ) )

pdf( paste( results_wd, "eua_prices.pdf", sep = "" ), width = 12, height = 6 )
eua_prices_plot
dev.off( )




###### Figure D-3: Boxplot per Fuel Type: Fuel Efficiency and Heat Rates 2003 - 2004 TR95 #############

fuel_fun <- function( dat ){
  colnames( dat )[1] <- "X1"
  a <- as.data.frame( do.call( "rbind", stringr::str_split( dat$X1, pattern = ":" ) ) )
  x <- cbind( a, dat[ , 2:10 ] )
  colnames( x ) <- c( "year", "variable", "N", "mean", "sd", "range", "p10", "p25", "p50", "p75", "p90" )
  if( sum( is.na( x$p10 ) ) >= 1 ){
    x$ab1 <- 0
    x$ab2 <- as.numeric( x$p25 )
    x$ab3 <- x$p50 - x$p25
    x$ab4 <- x$p75 - x$p50  
    x$ab5 <- 0
  } else{
    x$ab1 <- x$p10
    x$ab2 <- x$p25 - x$p10
    x$ab3 <- x$p50 - x$p25
    x$ab4 <- x$p75 - x$p50  
    x$ab5 <- x$p90 - x$p75  
  }  
  
  return( x )
}

fuel_eff_0304_total <- fuel_fun( dat = readr::read_delim( "tab_39.csv", delim = "\t", na = c( "-", "." ), locale = locale( decimal_mark = "." ), trim_ws = TRUE ) )
fuel_eff_0304_coal <- fuel_fun( dat = readr::read_delim( "tab_39_1.csv", delim = "\t", na = c( "-", "." ), locale = locale( decimal_mark = "." ), trim_ws = TRUE ) )  
fuel_eff_0304_lignite <- fuel_fun( dat = readr::read_delim( "tab_39_2.csv", delim = "\t", na = c( "-", "." ), locale = locale( decimal_mark = "." ), trim_ws = TRUE ) )
fuel_eff_0304_gas <- fuel_fun( dat = readr::read_delim( "tab_39_3.csv", delim = "\t", na = c( "-", "." ), locale = locale( decimal_mark = "." ), trim_ws = TRUE ) ) 

# Heat rate (inverse of fuel efficiency, valid for distribution but not for mean or standard deviation)
heat_rate <- function( dat ){
  x <- dat[ , c("year", "p10", "p25", "p50", "p75", "p90" ) ]
  x$ab1 <- 1 / x$p90
  x$ab2 <- ( 1 / x$p75) - ( 1 / x$p90 )
  x$ab3 <- ( 1 / x$p50) - ( 1 / x$p75 )
  x$ab4 <- ( 1 / x$p25) - ( 1 / x$p50 )
  x$ab5 <- ( 1 / x$p10) - ( 1 / x$p25 )
  
  return( x )
}

heat_rate_0304_total <- heat_rate( fuel_eff_0304_total )
heat_rate_0304_coal <- heat_rate( fuel_eff_0304_coal )
heat_rate_0304_lignite <- heat_rate( fuel_eff_0304_lignite )
heat_rate_0304_gas <- heat_rate( fuel_eff_0304_gas )

heat_rate_0304 <- rbind( heat_rate_0304_total[ 11, ], heat_rate_0304_coal[ 11, ], heat_rate_0304_lignite[ 11, ], heat_rate_0304_gas[ 11, ] )
heat_rate_0304$tech <- c( "Total", "Hard Coal", "Lignite", "Natural Gas")

heat_rate_0304_plot <- melt( heat_rate_0304[ , c( "ab1", "ab2", "ab3", "ab4", "ab5", "tech" ) ], id.vars = c( "tech" ) )

pdf( paste( results_wd, "ht_0304.pdf",  sep = "" ), width = 12, height = 6 )
ggplot( data = heat_rate_0304_plot, aes( x = tech ) ) +
    theme_bw( ) +
    geom_bar( aes( x = tech,  y = value, fill = factor( variable, levels = c( "ab5", "ab4", "ab3", "ab2", "ab1" ) ), 
                   colour = factor( variable, levels = c( "ab5", "ab4", "ab3", "ab2", "ab1" ) ) ,
                   linetype = factor( variable, levels = c( "ab5", "ab4", "ab3", "ab2", "ab1" ) ) ),
              stat = "identity", position = "stack" ) +
    labs( x = "Main Fuel Type", y = "Heat Rate") + 
    theme( aspect.ratio = ( 2 / 4 ) ) +
    theme( panel.grid = element_blank( ) ) + 
    scale_y_continuous( expand = c( 0, 0 ), limits = c( 0, 3 ) ) + 
    theme( legend.position = "None" ) +
    scale_fill_manual( values = c( "White", "White", "White", "White", "White" ) ) +
    scale_colour_manual( values = c( "Black", "Black", "Black", "Black", "White" ) ) +
    scale_linetype_manual( values = c( "dashed", "solid", "solid", "dashed", "solid" ) ) +
    theme( axis.text.x = element_text( size = 10 ), axis.text.y = element_text( size = 10 ), 
           axis.title.y = element_text( size = 10, vjust = 1 ),  axis.title.x = element_text( size = 10, vjust = -0.5 ) ) 
dev.off( )




####### Figure F-1: Density of Investments in Machinery per MW Capacity (log) ########
inv_mach_large <- read.delim( file = "tr95_ln_inv_mach_mw.txt" )

pdf( paste( results_wd, "ln_inv_mach_large_kdensity.pdf", sep = "" ), width = 12, height = 6 )
ggplot( data = inv_mach_large, aes( x = X__000001, y = X__000000 ) ) + 
  theme_bw( ) + 
  geom_line(  ) + 
  scale_x_continuous( expand = c( 0, 0 ), limits = c( 0, 20 ) ) + 
  scale_y_continuous( expand = c( 0, 0 ), limits = c( 0, 0.18 ) ) + 
  labs( x = "ln(Investments per MW)", y = "Density" ) + 
  theme( panel.grid = element_blank( ) ) + 
  theme( axis.line = element_line( color = "black" ) ) +
  theme( axis.text.x = element_text( size = 10 ), axis.text.y = element_text( size = 10 ) ) +
  theme( axis.title.y = element_text( size = 10, vjust = 1 ),  axis.title.x = element_text( size = 10, vjust = -0.5 ) )
dev.off( )
