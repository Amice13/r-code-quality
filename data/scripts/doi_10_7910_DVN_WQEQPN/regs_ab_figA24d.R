
## prelims
source("regression_functions.R")
tab.path <- "results/"
fig.path <- "results/"
med.fig <- paste0(fig.path, "ab_figA24d.pdf")


#####
#####
 ## focus on ethnic identity atm

ab$higheduc <- as.numeric(ab$educ > 2 )
ab$lowincome <- as.numeric(ab$econ_income > 2)


yvars1 <- c( "identity_more_ethnic")
yvars <- paste0(yvars1, "_sc")
cropvar <- c( "crop_lang_sc")
pubvar <- c("pubspc.23_lang_sc")
controls.ind <- c("sex", "age", "age2" )

k <- 1
j <- 1
i <- 1

X <- c(controls.ind, "lpopc_hyde_meanprecol_lang")
Z <- c( "f.health", "f.water", "f.food")

## Mediation à la Acharya, Blackwell, Sen: Average controlled direct effects (ACDE)
## suitability Blackwell

# define number of iterations (takes about 10 minutes for 50 iterations)
boots <- 150

# define outcome
yvar <- yvars[i]

# define mediator
mediators <- c("higheduc" , "lowincome" )
medlabs <- c("Education", "Income"  )
nmed <- length(mediators)

# define pretreatment confounders
conf.pre <- X
# define intermediate confounders
conf.int <- Z

##
cl_var <- "loc.id"

# define analysis data
 ## add education average in location, born b/f 1960
abdf <- ab[,c(cropvar[k],pubvar[j], yvars[i], X, Z,  mediators, cl_var)]
abdf <- na.omit(abdf)
dat <- abdf

clusters <- unique(abdf[,cl_var])

# set seed
set.seed(2021)

## base regressions

#### Fun with felm
####My mediation function with felm with unlim nb of mediators
####
Val.mediation <- function(mediators , Xconf =conf.pre,  Zconf=conf.int,  data ){
 base <- felm(as.formula(RegFor( y = yvar , x = c(cropvar[k] , pubvar[j] , Xconf) ,
                    FE = "loc.id" , IV="0", clust = "loc.id")),
                    data=data)

 m.stage1 <- felm(as.formula(RegFor( y = yvar , x = c(cropvar[k] , pubvar[j] ,mediators, Xconf, Zconf) ,
                 FE = "loc.id" , IV="0", clust = "0")),
                 data=data)
betas <- coef(m.stage1)[mediators]
betamat <- diag(betas, ncol = length(betas), nrow = length(betas))
correctterm <- data.matrix(data[,mediators]) %*% data.matrix(betamat)
data[["ytild"]] <- data[[yvar]]  + (correctterm %*% matrix(-1*rep(1,length(betas))))
m.stage2 <- felm(as.formula(RegFor( y = "ytild" , x = c(cropvar[k] , pubvar[j], conf.pre) ,
                                   FE = "loc.id" , IV="0", clust = "0")),
                data=data)
 out <- coef(m.stage2)[c(cropvar[k] , pubvar[j])]
# out <- list(m.stage1,m.stage2)
# names(out) <- c("stage1","stage2")
return(out)
}

### Compute coeffs
m.base <- felm(as.formula(RegFor( y = yvar , x = c(cropvar[k] , pubvar[j] , conf.pre) ,
                   FE = "loc.id" , IV="0", clust = "loc.id")),
                   data=abdf)
ate <- coef(m.base)[c(cropvar[k] , pubvar[j])]
med.sep <- sapply(mediators, function(x) Val.mediation( x , conf.pre, conf.int, abdf) )
combined <- Val.mediation( mediators , conf.pre, conf.int, abdf)
acde <- data.frame(cbind(ate, med.sep, combined)) %>%
  rownames_to_column("treatment")
acde$treatment <- ifelse(grepl("crop", acde$treatment), "Cash Crops", "Publications")

### Bootstrap se

print("Bootstrapping for ACDE... ")
t0 <- Sys.time()
 mediation.coefs <- lapply(seq(boots),function(i){
  print(paste(i, "out of", boots, "..." ))

  units <- sample(clusters, size = length(clusters), replace=TRUE)
  # create bootstap sample with sapply
  df.bs <- sapply(units, function(x) which(dat[,cl_var]==x))
  dat.boot <- dat[unlist(df.bs),]

  m.base <- felm(as.formula(RegFor( y = yvar , x = c(cropvar[k] , pubvar[j] , conf.pre) ,
                                         FE = "loc.id" , IV="0", clust = "loc.id")),
                      data=dat.boot)

 med.sep <- sapply(mediators, function(x) Val.mediation( x , conf.pre, conf.int, dat.boot) )
 combined <- Val.mediation( mediators , conf.pre, conf.int, dat.boot)
 ate <- coef(m.base)[c(cropvar[k] , pubvar[j])]
 acde <- cbind(ate, med.sep, combined)

 t0.1 <- Sys.time()
 print("has been going on for....")
 print(t0.1-t0)
return(acde)
})

t1 <- Sys.time()
print("total time to bootstrap")
print(t1-t0)

## Make plots

med.coefs <- as.data.frame(do.call(rbind,mediation.coefs)) %>%
  rownames_to_column("treatment")
med.coefs$treatment <- ifelse(grepl("crop", med.coefs$treatment), "Cash Crops", "Publications")

q2 <- function(x){quantile(x, 0.025, na.rm = TRUE)}
q9 <- function(x){quantile(x, 0.975, na.rm = TRUE)}

med.coefs.ub <- med.coefs %>%
  group_by(treatment) %>%
  summarize_all(q9)

med.coefs.lb <- med.coefs %>%
  group_by(treatment) %>%
  summarize_all(q2)

outs <- c("ate", mediators, "combined")
med.plot.df <- data.frame(beta=c(t(acde[acde$treatment == "Cash Crops", outs]),
                                 t(acde[acde$treatment == "Publications", outs])),
                        lb_boot= c(t(med.coefs.lb[med.coefs.lb$treatment == "Cash Crops", outs]),
                                 t(med.coefs.lb[med.coefs.lb$treatment == "Publications", outs])),
                          ub_boot= c(t(med.coefs.ub[med.coefs.ub$treatment == "Cash Crops", outs]),
                                 t(med.coefs.ub[med.coefs.ub$treatment == "Publications",  outs])),
                          effect=c("Total Effect Crops",
                                  paste0("ACDE Crops - ", c(medlabs , "Combined")),
                                  "Total Effect Publications",
                                  paste0("ACDE Publications - ", c(medlabs, "Combined") )))
#
med.plot.df$treat <- ifelse(grepl("Crop", med.plot.df$effect), "Cash Crops", "Publications")
med.plot.df$order <- rev(seq(from = 0.1, length = dim(med.plot.df)[1], by =0.2))
med.plot.df$order <- ifelse(grepl("Crop", med.plot.df$effect), med.plot.df$order + 0.2 , med.plot.df$order)
med.plot.df$beta.short<- as.character(round(med.plot.df$beta, 4))

# # do the plot
cols <- wes_palette("Rushmore", 5)[c(4,5)]

p <- ggplot(med.plot.df, aes(x=beta,y=order, col=treat, shape= treat))
p <- p + geom_point() +
  geom_text(aes(label = beta.short), hjust = 1 ,vjust = -1 ) +
  geom_pointrange(aes(xmin = lb_boot, xmax = ub_boot)) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_color_manual(values=c("Cash Crops" = cols[1], "Publications" = cols[2]), name="Treatment")+
  scale_shape_manual(values=c("Cash Crops" = 24, "Publications" = 25), name="Treatment") +
  labs(x = "Estimates and 95% Confidence Intervals (Cluster Bootstrap)", y = "Total and Demediated Effects",
       title="Causal Mediation Analysis - Ethnic level",
       subtitle="Total and demediated direct effects (ACDE)") +
  theme_minimal(base_size = 16 )+
  theme(axis.text.y = element_text(size=16),  legend.position = "bottom") +
  scale_y_continuous(breaks=med.plot.df$order, labels=med.plot.df$effect) +
  ggsave(med.fig , height = 7 , width = 10)
#
