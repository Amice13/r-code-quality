# libraries: 'psych' (MAIN), 'factoextra', 'FactoMiner' ‘zip’, ‘openxlsx’, ‘carData’, ‘pbkrtest’, ‘rio’, ‘car’, ‘flashClust’, ‘leaps’, ‘scatterplot3d’, ‘FactoMineR’, ‘ca’, ‘igraph’
# ЧАСТЬ 1 создаем датафрейм
# шаг-1. вчитываем таблицу. делаем из нее датафрейм.
MDepths <- read.csv("MorphologyFA.csv", header=TRUE, sep = ",")
MDF<- na.omit(MDepths) 
row.has.na <- apply(MDF, 1, function(x){any(is.na(x))}) 
sum(row.has.na)
# ЧАСТЬ 2 Факторный анализ
	# шаг-2. Create Correlation Matrix // создаем корреляционную матрицу из датафрейма
corMat <- cor(MDF) # create correlation matrix
corMat
	# шаг-3. Factor Analysis (method: Principal Axis) // делаем факторный анализ по методу главных осей
faM <- fa(corMat, nfactors = 2, rotate = "oblimin", scores = "regression", residuals = FALSE, SMC=TRUE, covar=FALSE, missing = FALSE,impute = "median", min.err = 0.001,  max.iter = 50,symmetric = TRUE, warnings = TRUE, fm = "pa", alpha = .1, p = .05, oblique.scores = FALSE, np.obs = NULL,  use = "pairwise", cor = "cor",  correct = .5, weight = NULL)
print(faM,sort=TRUE) # print the results from a factor analysis
plot(faM) # plot the loadings from a factor analysis или вот так: 
fa.plot(faM) # plot the loadings from a factor, principal components, or cluster analysis 
	# шаг-4. Parallel Factor Analysis compares the observed eigen values of a correlation matrix with those from random data // параллельный анализ (сравниваем значения айгенвекторов корр. матрицы с рандомными значениями)
fa.parallel(corMat) 
	# шаг-5. Weighted Least Squares Factor Analysis // метод взвешенных наименьших квадратов
faWls <- fa(corMat, nfactors = 2, rotate = "oblimin", scores = "regression", residuals = FALSE, SMC=TRUE, covar=FALSE, missing = FALSE,impute = "median", min.err = 0.001,  max.iter = 50,symmetric = TRUE, warnings = TRUE, fm = "wls", alpha = .1, p = .05, oblique.scores = FALSE, np.obs = NULL,  use = "pairwise", cor = "cor",  correct = .5, weight = NULL)
print(faWls,sort=TRUE) # print the results from a factor analysis
plot(faWls)
	# шаг-6.Item Cluster Analysis: iclust // айтемный кластерный анализ по корреляционной матрице
iclust(corMat)
ic <- iclust(corMat)
summary(ic)
	# шаг-7. Hierarchical and bi-factor solutions using the omega function // бифакторные иерархические решения 
om.h <- omega(corMat, n.obs=25, sl=FALSE)
om <- omega(corMat, n.obs=25)
	# Alpha Factor Analysis // Факторный анализ методом альфа 
faMal <- fa(corMat, nfactors = 2, rotate = "oblimin", scores = "regression", residuals = FALSE, SMC=TRUE, covar=FALSE, missing = FALSE,impute = "median", min.err = 0.001,  max.iter = 50,symmetric = TRUE, warnings = TRUE, fm = "alpha", alpha = .1, p = .05, oblique.scores = FALSE, np.obs = NULL,  use = "pairwise", cor = "cor",  correct = .5, weight = NULL)
print(faM,sort=TRUE) # print the results from a factor analysis
plot(faMal)






