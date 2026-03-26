
library(MCMCpack)

getwd()


##### Estimate a one dimensional factor analysis for eight questions associated with intellectual property rights (IPR)
##### A postive response on the question 4.01.07.01  is constrained to be positive
##### Very hight burnins and simulations were used to ensure convergance.  Following the advice of Heidelberger and Welch (1981)
#####     length of runs were increased by 50% each time convergance was tested and failed.  Although not necessary, all estimations were
#####     run with the largest number of iterations need for any model.


post.f1.IPR.scores <- MCMCmixfactanal(~X4.01.02.01 + X4.01.07.01 + X4.01.07.02 + X4.01.08 + X4.01.09 +
				X4.01.10 + X4.01.11 + X4.01.12,
                        factors=1, data=pharma.factors,
                        lambda.constraints = list(X4.01.07.01=list(1,"+")),
                        burnin=100000, mcmc=50000000, thin=100,
                        verbose=100000, L0=0.25, store.lambda=TRUE,
                        store.scores=TRUE, tune=3.0)
save(post.f1.IPR.scores, file="post.f1.IPR.scores.R")


##### Table 4
##### Estimate a one dimenational feactoral analysis for questions associated with the monitoring and regulationof the private market
#####    A dummy variable for whether regular inspections are conducted is included.

reg.and.monitor.private.regular.inspections <- MCMCmixfactanal(~
			X5.02.08 + X5.02.12S + X5.05.02 + X5.05.06 +
			X5.07.01 + X5.05.01 + X5.05.05 + X5.05.08 + X5.06.02 +
			X5.03.05.01 + X5.03.05.02 + X5.03.05.03 + X5.03.05.05 + 
			X5.05.04 + X5.04.02 + X5.04.03 + X5.04.04 +
			regular.inspection +
			X5.10.01 + X5.10.02 + X5.10.03 + X5.10.05 + X5.10.06 +
			X5.10.10 + X5.10.16S + X5.10.22S,
                        factors=1, data=pharma.factors,
                        lambda.constraints = list(X5.03.05.01=list(2,"+")),
                        burnin=100000, mcmc=50000000, thin=100,
                        verbose=100000, L0=0.25, store.lambda=TRUE,
                        store.scores=TRUE, tune=3.0)
save(reg.and.monitor.private.regular.inspections, file="reg.and.monitor.private.regular.inspections.R")



##### Estimate a one dimenational factorial analysis for questions associated with the monitoring and regulationof the private market
#####    A dummy variable for whether anual inspections are conducted is included.

reg.and.monitor.private.annual.inspections <- MCMCmixfactanal(~
			X5.02.08 + X5.02.12S + X5.05.02 + X5.05.06 +
			X5.07.01 + X5.05.01 + X5.05.05 + X5.05.08 + X5.06.02 +
			X5.03.05.01 + X5.03.05.02 + X5.03.05.03 + X5.03.05.05 + 
			X5.05.04 + X5.04.02 + X5.04.03 + X5.04.04 +
			regular.annual.inspection +
			X5.10.01 + X5.10.02 + X5.10.03 + X5.10.05 + X5.10.06 +
			X5.10.10 + X5.10.16S + X5.10.22S,
                        factors=1, data=pharma.factors,
                        lambda.constraints = list(X5.03.05.01=list(2,"+")),
                        burnin=100000, mcmc=50000000, thin=100,
                        verbose=100000, L0=0.25, store.lambda=TRUE,
                        store.scores=TRUE, tune=3.0)
save(reg.and.monitor.private.annual.inspections, file="reg.and.monitor.annual.private.inspections.R")


##### Table 6  Regulatory Infrastructure and Good Practices
##### Estimate a one dimenational factorial analysis for questions associated with the presence of basic institutions
#####    A dummy variable for whether anual inspections are conducted is included.

pharma.factors$X5.05.11  <- as.ordered(pharma.factors$X5.05.11 )
basic.intitutions <- MCMCmixfactanal(~X3.01.04 + X3.01.12 +
			X5.01.01 + X5.01.02 + X5.01.04.02 + X5.01.10 + X5.01.11 + X5.01.15 +
			X5.05.03 + X5.05.07 + X5.05.11 + X3.01.14 + X3.01.16 + X5.02.15S,
                        factors=1, data=pharma.factors,
                        lambda.constraints = list(X5.01.02=list(2,"+")),
                        burnin=100000, mcmc=50000000, thin=100,
                        verbose=100000, L0=0.25, store.lambda=TRUE,
                        store.scores=TRUE, tune=3.0)
save(basic.intitutions, file="basic.intitutions.R")



##### Table 5 - all countries

public.quality.control <- MCMCmixfactanal(~X5.03.05.04 + X5.03.05.05 + X5.06.04.01 + X5.06.04.06 +
			X7.01.05 + X7.01.07S + X7.01.10S + X7.01.11S + X7.02.03 +
			X5.05.08 + X5.05.10 + X7.01.03 + X7.01.04 + 
			X7.01.12.01S + X7.01.12.02S,
                        factors=1, data=pharma.factors,
                        lambda.constraints = list(X5.06.04.01=list(2,"+")),
                        burnin=100000, mcmc=50000000, thin=100,
                        verbose=100000, L0=0.25, store.lambda=TRUE,
                        store.scores=TRUE, tune=3.0)
save(public.quality.control, file="public.quality.control.R")



##### Table 5 - countries with Central Medical Stores

central.medical.store.data <- subset(pharma.factors, X7.02.01 == 1) 
f1.central.medical.stores <- MCMCmixfactanal(~X5.03.05.04 + X5.03.05.05 + X5.06.04.01 + X5.06.04.06 +
			X7.01.05 + X7.01.07S + X7.01.10S + X7.01.11S + X7.02.03 +
			X5.05.08 + X5.05.10 + X7.01.03 + X7.01.04 + 
			X7.01.12.01S + X7.01.12.02S +
			X7.01.10.01S + X7.01.10.02S + X7.01.10.03S +
			X7.02.08.06S + X7.02.08.07S,
                        factors=1, data=central.medical.store.data,
                        lambda.constraints = list(X5.06.04.01=list(2,"+")),
                        burnin=100000, mcmc=50000000, thin=100,
                        verbose=100000, L0=0.25, store.lambda=TRUE,
                        store.scores=TRUE, tune=3.0)
save(f1.central.medical.stores, file="f1.central.medical.stores.R")


##### Estimate a two dimenational factorial analysis for questions associated Public Quality Control for countries with Central Medical Stores

f2.central.medical.stores <- MCMCmixfactanal(~X5.03.05.04 + X5.03.05.05 + X5.06.04.01 + X5.06.04.06 +
			X7.01.05 + X7.01.07S + X7.01.10S + X7.01.11S + X7.02.03 +
			X5.05.08 + X5.05.10 + X7.01.03 + X7.01.04 + 
			X7.01.12.01S + X7.01.12.02S +
			X7.01.10.01S + X7.01.10.02S + X7.01.10.03S +
			X7.02.08.06S + X7.02.08.07S,
                        factors=2, data=central.medical.store.data,
                        lambda.constraints = list(X5.06.04.01=list(2,"+"), X7.01.10.01S=list(3,"+")),
                        burnin=100000, mcmc=50000000, thin=100,
                        verbose=100000, L0=0.25, store.lambda=TRUE,
                        store.scores=TRUE, tune=3.0)
save(f2.central.medical.stores, file="f2.central.medical.stores.R")


##### Estimate a one dimenational factorial analysis for questions associated Central Medical Stores for countries with Central Medical Stores

only.central.medical.stores <- MCMCmixfactanal(~X7.01.10.01S + X7.01.10.02S + X7.01.10.03S +
			X7.02.08.06S + X7.02.08.07S,
                        factors=1, data=central.medical.store.data,
                        lambda.constraints = list(X7.01.10.01S=(2,"+")),
                        burnin=100000, mcmc=50000000, thin=100,
                        verbose=100000, L0=0.25, store.lambda=TRUE,
                        store.scores=TRUE, tune=3.0)
save(only.central.medical.stores, file="only.central.medical.stores.R")


####  Heidelberger and Welch's convergence diagnostic

heidel.diag(post.f1.IPR.scores)
heidel.diag(reg.and.monitor.private.reg.inspections)
heidel.diag(reg.and.monitor.private.annual.inspections)
heidel.diag(basic.intitutions)
heidel.diag(public.quality.control)
heidel.diag(f1.central.medical.stores)
heidel.diag(f2.central.medical.stores)
heidel.diag(only.central.medical.stores)




