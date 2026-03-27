# READ DATA
d <- read.table("cache/d.csv", header=TRUE, sep=",",
	stringsAsFactors=F)

# READ DATA
d <- read.table("cache/d.csv", header=TRUE, sep=",",
	stringsAsFactors=F)

mat <- as.matrix(d[,c("immigrant.adapt", "politics.abstain", "measures.environment", 
"samesexmarriage", "menwomen.equal", "stiffer.sentences", "social.security", 
"income.redist", "immigrant.good", "war.terrorism", "ind.rights", 
"open.globalmarkets", "id")])
mat <- na.omit(mat)
ids <- mat[,"id"]
mat <- mat[,-ncol(mat)]

fit <- princomp(mat, cor=TRUE)
summary(fit) # print variance accounted for 
plot(fit, type="lines") # scree plot == 3 dimensions with eigenvalues>=1
round(loadings(fit, cutoff=.20)[,1:3],3) # pc loadings 

#                      Comp.1 Comp.2 Comp.3
# immigrant.adapt      -0.381  0.329  0.122
# politics.abstain     -0.111  0.183  0.343
# measures.environment -0.208 -0.354 -0.028
# samesexmarriage      -0.006 -0.469 -0.169
# menwomen.equal       -0.204 -0.324 -0.246
# stiffer.sentences    -0.434  0.243  0.077
# social.security      -0.490 -0.078 -0.086
# income.redist        -0.446 -0.109 -0.192
# immigrant.good        0.184 -0.449  0.317
# war.terrorism         0.010 -0.093  0.586
# ind.rights           -0.214 -0.290  0.064
# open.globalmarkets   -0.223 -0.194  0.528

# looks like dimension 2 is left/right positions
est <- data.frame(
	id = ids,
	fa_x1 = fit$scores[,1],
	fa_x2 = fit$scores[,2])

d <- merge(d, est, all.x=TRUE)

cor(d$leftright, d$fa_x1, use='complete.obs')
# [1] 0.04898835

cor(d$leftright, d$fa_x2, use='complete.obs')
# [1] 0.2756916






