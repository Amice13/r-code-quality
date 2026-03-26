## seed
set.seed(20181002)

## packages
if (!("plot3D") %in% installed.packages()) install.packages("plot3D") 

## Define quantities
N <- 10000L # individuals
J <- 3L #items
K <- 5L # categories per item

## items is an NxJ matrix
items <- t(replicate(N, sample(1:5, J, replace = T)))

## these are length N vectors
items.min <- apply(items, 1, min) # intra-individual item mean
items.mean <- apply(items, 1, mean) # intra-individual item mean
items.disc <- items.mean - items.min # discrepancy between measures
items.var <- apply(items, 1, var) # intra-individual item var

## Visualizations
par(oma = c(0, 0, 0, 0), 
    mar = c(5, 5, 3, 1))
plot(items.min, items.mean,
     main = "Joint distribution of both measures",
     xlab = "Goertz",
     ylab = "Bollen")
text(3, 1.5, paste0(expression(rho), " = ", round(cor(items.min, items.mean), 2)))


plot(items.var, items.disc,
     main = "Joint distribution of Discrepancy and Variance",
     xlab = "Variance",
     ylab = "Discrepancy")
text(4, 0.5, paste0(expression(rho), " = ", round(cor(items.min, items.mean), 2)))


library(plot3D)

## Min, Mean, Variance
par(oma = c(0, 0, 1, 0), 
    mar = c(0, 3, 0, 1))
perspbox(z = NULL, 
         zlim = c(0, K),
         zlab = "Variance",
         y = NULL, ylim = c(1, K),
         ylab = "Bollen",
         x = NULL, xlim = c(1, K),
         xlab = "Goertz",
         main = "",
         bty = 'u', 
         col.grid = "white",
         col.panel = "gray95",
         col.axis = "gray80",
         lwd.grid = 2,
         lwd.panel = 2,
         ticktype = "detailed",
         ntick = 6,
         expand = 0.8,
         theta = 45,
         phi = 33.75,
         r = sqrt(5),
         d = 1)

## jitter
x.jit <- items.min + rnorm(N, 0, .05)
y.jit <- items.mean + rnorm(N, 0, .05)
z.jit <- items.var + rnorm(N, 0, .05)

scatter3D(x.jit, y.jit, z.jit,
          pch = 20, cex = .5,
          add = T)


## Min, Discrepancy, Variance
perspbox(z = NULL, 
         zlim = c(0, K),
         zlab = "Variance",
         y = NULL, ylim = c(0, K-1),
         ylab = "Discrepancy",
         x = NULL, xlim = c(1, K),
         xlab = "Min",
         main = "",
         bty = 'u', 
         col.grid = "white",
         col.panel = "gray95",
         col.axis = "gray80",
         lwd.grid = 2,
         lwd.panel = 2,
         ticktype = "detailed",
         ntick = 6,
         expand = 0.8,
         theta = 67.5,
         phi = 22.5,
         r = sqrt(5),
         d = 1)

## jitter
y.jit <- items.disc + rnorm(N, 0, .05)

scatter3D(x.jit, y.jit, z.jit,
          pch = 20, cex = .5,
          add = T)