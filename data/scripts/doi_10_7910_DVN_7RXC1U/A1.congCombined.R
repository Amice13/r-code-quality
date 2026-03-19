#################### APPENDIX: KRIGING MEASURES OF IDEOLOGY ####################
# Clean up
#rm(list=ls())

# Load packages
library(krige)

summary(congCombined)

# Descriptive Statistics
summary(congCombined$krige.cong)

# Correlate House Members' DW-NOMINATE Scores with Public Opinion Ideology
cor(congCombined$dwnom1,congCombined$krige.cong)

# Code output page 23
summary(congCombined$krige.cong)

# Figure A.1.4
# Plot House Members' DW-NOMINATE Scores against Public Opinion Ideology
#postscript('congDist.eps',width=5,height=5)
pdf('output/fig/congDist.pdf',width=5,height=5)
plot(y=congCombined$dwnom1,x=congCombined$krige.cong, xlim = c(40,65),
     xlab="District Ideology (Kriging)", ylab="Legislator Ideology (1st Dim., Common Space)", 
     main="U.S. House of Representatives", type="n")
points(y=congCombined$dwnom1[congCombined$party==200],
       x=congCombined$krige.cong[congCombined$party==200],pch="R",col="red")
points(y=congCombined$dwnom1[congCombined$party==100],
       x=congCombined$krige.cong[congCombined$party==100],pch="D",col="blue")
dev.off()