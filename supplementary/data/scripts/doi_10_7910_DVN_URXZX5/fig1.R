# Code for building Figure 1 of the Paper
# Note that this does not depend on the data


library(ggplot2)
theme_set(theme_bw())

### Figure 1
dat <- data.frame(
	Type = c("Competitor", "Target", "Decoy"),
	Attribute1 = c(14, 24, 20),
	Attribute2 = c(4.0, 2.0, 2.0)
	)


# Ok, this is pretty hacky way of getting the contour thing.
lev <- function(x,y){
	#return( -.02*(x-20)^2 + -.05*(y-3)^2 )
	x <- x-9.2
	z <- -1 * (exp(.255*x)-3.5*x-1)-2*y^2
	return(z)
}
x <- seq(10, 25, by=.1)
y <- seq(1,5, by=.05)
xy <- expand.grid(x,y)
z <- lev(xy[1], xy[2])
xy <- cbind(xy,z)
names(xy) <- c("x","y", "z")
#xy[xy$x<11,] <- NA
#xy[xy$x>24,] <- NA
#xy[xy$y>3.4,] <- NA
# ggplot(aes(x,y, fill=z), data=xy) + geom_tile() 


# Basic idea for Figure 1 / but needs different data and perhaps differnt colours for the underlying tile.
p <- ggplot() +
	geom_tile(data=xy, aes(x=x, y=y, fill=z), alpha=.3) +
	scale_fill_gradientn(colours = c("white", "white", "white", "white", "red")) + # very ugly hack ;)
	geom_vline(xintercept=20, linetype=2, color="red") +
	geom_abline(intercept = 6.8, slope= -.2, color="grey", linetype=5) + 
	ylab( expression( A[2]:~RAM ) ) +
	xlab( expression( A[1]:~Battery ) ) +
  scale_x_continuous(limits=c(10,40), expand = c(0, 0)) +
  scale_y_continuous(limits=c(0,5), expand = c(0, 0)) +
	#ylim(c(1,5)) +
	#xlim(c(10,25)) +	
	geom_point(data = dat, aes(x=Attribute1, y=Attribute2, color=Type, label=Type), color=c("black", "black", "red")) +
	geom_text(data = dat, aes(x=Attribute1, y=Attribute2, color=Type, label=Type), color="black", vjust=-1) +
	theme(legend.position="none") +
	theme(axis.title.x=element_text(hjust=.9)) +
	theme(axis.title.y=element_text(hjust=.9)) 
	
print(p)

ggsave("figs/Figure1.pdf", p)
	

