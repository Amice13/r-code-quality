library(plspm)
library(ggplot2)

# data
data <- read.csv("DataSurvey.csv")

#### 1. pls model ####
# inner Matrix
el  = c(0,0,0,0)
m   = c(1,0,0,0)
se  = c(1,0,0,0)
ap  = c(0,1,1,0)
Path_g = rbind(el, m, se, ap)
innerplot(Path_g, box.size = 0.08, curve = 0.1, box.cex =1.6)

# outer Matrix
g_blocks = list(1:4,5:6,7:8,9:12)
g_modes = rep("A", 4)

# running PLS-PM
# general pls
Pls_g = plspm(data, Path_g, g_blocks, modes = g_modes, boot.val = TRUE, br = 5000)
plot(Pls_g, box.size = 0.05, curve = 0.1, arr.pos = 0.4, cex.txt = 1.1, box.cex =1.2)

# confirmatory analalysis results
# outer model results
Pls_g$unidim
Pls_g$outer_model
#ggplot(data = Pls_g$outer_model,aes(x = name, y = loading, fill = block)) +
#        geom_bar(stat = 'identity', position = 'dodge') +
#        scale_fill_manual(values=c("green3","cyan4","darkblue","brown2","deeppink3","chocolate3")) +
#        geom_hline(yintercept = 0.7, color = 'gray50') +
#        theme(axis.text.x = element_text(angle = 90,size =15),axis.text.y = element_text(size =15))
Pls_g$crossloadings

# inner model results
# Pls_g$inner_model
Pls_g$inner_summary
Pls_g$boot$paths

#### 2. ScoresRescaled #####
scoresRescaled = rescale(Pls_g) #scoresRescaled is an R-Object as scores
write.csv(scoresRescaled, file="ScoresRescaled.csv", row.names = F)

