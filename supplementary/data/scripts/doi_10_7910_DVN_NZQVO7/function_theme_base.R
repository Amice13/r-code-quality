theme_baser <- function (){
    theme_minimal()  %+replace%
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.border = element_rect(fill=NA,color="black", size=0.5,
                                          linetype="solid"),
              #panel.border = element_rect(color = "black", fill = NA, size = 1),
              legend.title = element_text(size = 15),
              title = element_text(size = 15, vjust = 1.5, hjust = 0),
              legend.position = "bottom",
              axis.ticks = element_line(size = 0.3),
              axis.ticks.length = unit(0.3,"cm"),
              legend.text=element_text(size = 13),
              strip.text = element_text(size = 12, hjust = 0.5,
                                        margin = margin( b = 2, t = 2, l = 2, r = 2)),
              #,# margin(b=5, r = 5, l = 5, t = 5)),
              axis.text = element_text(colour="black", size = 13),
              axis.title = element_text(size = 13, hjust = 0.5))
}

theme_set(theme_baser())
