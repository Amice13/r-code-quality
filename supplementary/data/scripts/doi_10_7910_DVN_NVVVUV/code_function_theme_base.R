############################################################################
############################################################################
#### 
#### Constantine Boussalis, Travis G. Coan, Mirya R. Holman, Stefan Müller
#### Gender, Candidate Emotional Expression,
#### and Voter Reactions During Televised Debates 
####
#### American Political Science Review 
####
#### code_function_theme_base.R
#### Note: function to create ggplot2 graphs with an adjusted layout
#### For details on all datasets, R scripts, and instructions 
#### please consult the file "0000_replication_instructions.pdf" in the 
#### Dataverse of this project.
############################################################################
############################################################################


theme_baser <- function (){
    theme_minimal()  %+replace%
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.border = element_rect(fill=NA,color="black", size=0.5,
                                          linetype="solid"),
              legend.title = element_text(size = 15),
              title = element_text(size = 15, vjust = 1.5, hjust = 0),
              legend.position = "bottom",
              axis.ticks = element_line(size = 0.3),
              axis.ticks.length = unit(0.3,"cm"),
              legend.text=element_text(size = 13),
              strip.text.y = element_text(size = 15, hjust = 0.5, face = "bold", angle = 270,
                                          margin=margin(b=5, r = 5, l = 5, t = 5)),
              strip.text.x = element_text(size = 15, hjust = 0.5, face = "bold",
                                        margin=margin(b=5, r = 5, l = 5, t = 5)),
              axis.text = element_text(colour="black", size = 14),
              axis.title = element_text(size = 15, hjust = 0.5))
}

theme_set(theme_baser())
