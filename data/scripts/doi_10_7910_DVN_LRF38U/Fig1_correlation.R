#----------------------------------------------
## Creating Figure 1
## Author: Ken Mwai
#----------------------------------------------
#----------------------------------------------
# 0 - Load libraries and set seed 
#----------------------------------------------
library(tidyverse)
library(ggpubr)
set.seed(1504769)
##
#----------------------------------------------
## Load the data set
#----------------------------------------------
df_inhouse <- haven::read_dta("preCOVID KNBTS_2020_2021_cor.dta")

#----------------------------------------------
## My theme for plotting
#----------------------------------------------
mytheme <-  theme(axis.text=element_text(size=18,  color="black"),
                  axis.title=element_text(size=20,  color="black") ,
                  plot.title = element_text( size=22 , hjust = 0, face= "bold"),
                  text = element_text(size = 18) ,
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())

#----------------------------------------------
## Create the correlation plot
#----------------------------------------------
p <- ggplot(df_inhouse ,aes(x=log10(ODratio2021),y=log10(odratio2020))) +
  geom_point(alpha=0.5) +
  geom_smooth(method = lm)+
  stat_cor(method = "pearson", label.x = -0.3, label.y = 0.5, size = 6) +
  labs(x="Log OD ratio 2021", y="Log OD ratio 2020")+
  theme_bw() +
  geom_vline(xintercept = 0.30103, linetype="dotted") +
  geom_hline(yintercept = 0.30103, linetype="dotted") +
  mytheme

p

#----------------------------------------------
## Save the graph
#----------------------------------------------
ggexport(p ,
         filename ="images/figure1.png" ,
         width = 4000 , height = 2400 , limitsize = F,
         res=400)


#----------------------------------------------
## Proportion tables
#----------------------------------------------
prop.table(table(df_inhouse$seropos2021,df_inhouse$seropos2020))

## confidence interval
#' Title
#'
#' @param tbl 
#' @param margin 
#'
#' @return
#' @export
#'
#' @examples
ci.table <- function(tbl, margin = NULL) {
  binom_ci <- function(x, n) {
    paste(round(binom.test(x, n)$conf.int, 3), collapse = " - ")
  }
  sweep_ci <- function(xx, nn) { mapply(FUN = binom_ci, xx, nn) }
  
  if (length(margin))
    result <- sweep(tbl, margin, margin.table(tt, margin),
                    "sweep_ci", check.margin = FALSE)       
  else
    result <- sweep_ci(tbl, sum(tbl))
  
  dim(result) <- dim(tbl)
  as.table(result)
}
ci.table(table(df_inhouse$seropos2021,df_inhouse$seropos2021))


##End
