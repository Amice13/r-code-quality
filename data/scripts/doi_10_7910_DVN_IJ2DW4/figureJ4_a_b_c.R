rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(tidyr)
library(dplyr)
library(rdrobust)
library(ggplot2)

## load data
sentiment <- readRDS('./data/media/media.RDS')
covars_media <- readRDS('./data/media/covars_media.RDS')


### Figure J4: Sensitivity to bandwidth size and order of polynomial
## Figure a: sensitivity to bandwidth size (MSE)
i <- 3
j <- 'prop_negative'
nnmatch <- 3

mse <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), get(j)],
                sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), victory_margin],
                covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), covars_media, with = FALSE],
                p = 1,
                kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), cluster])

data_mse <- cbind('MSE' ,mse$bws[1], mse$coef[1], mse$ci[3,1], mse$ci[3,2])

mse1.25 <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), get(j)],
                    sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), victory_margin],
                    covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), covars_media, with = FALSE],
                    p = 1,
                    kernel = "triangular", h=1.25*mse$bws[1], nnmatch = nnmatch,
                    cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), cluster])

data_mse1.25 <- cbind('1.25*MSE', mse1.25$bws[1], mse1.25$coef[1], mse1.25$ci[3,1], mse1.25$ci[3,2])

mse1.5 <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), get(j)],
                   sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), victory_margin],
                   covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), covars_media, with = FALSE],
                   p = 1,
                   kernel = "triangular", h=1.5*mse$bws[1], nnmatch = nnmatch,
                   cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), cluster])

data_mse1.5 <- cbind('1.5*MSE', mse1.5$bws[1], mse1.5$coef[1], mse1.5$ci[3,1], mse1.5$ci[3,2])

mse.75 <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), get(j)],
                   sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), victory_margin],
                   covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), covars_media, with = FALSE],
                   p = 1,
                   kernel = "triangular", h=0.75*mse$bws[1], nnmatch = nnmatch,
                   cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), cluster])

data_mse.75 <- cbind('0.75*MSE', mse.75$bws[1], mse.75$coef[1], mse.75$ci[3,1], mse.75$ci[3,2])

mse.5 <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), get(j)],
                  sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), victory_margin],
                  covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), covars_media, with = FALSE],
                  p = 1,
                  kernel = "triangular", h=0.5*mse$bws[1], nnmatch = nnmatch,
                  cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), cluster])

data_mse.5 <- cbind('0.5*MSE', mse.5$bws[1], mse.5$coef[1], mse.5$ci[3,1], mse.5$ci[3,2])

data_mse <- as.data.frame(rbind(data_mse.5, data_mse.75, data_mse, data_mse1.25, data_mse1.5))
colnames(data_mse) <- c('type', 'bandwidth', 'coef', 'ci_l', 'ci_u')

# graph sensitivity to choice of bandwidth
ggplot(data_mse) +
  aes(x=as.numeric(as.character(bandwidth)), y=as.numeric(as.character(coef))) +
  geom_point() +
  geom_errorbar(aes(ymin=as.numeric(as.character(ci_l)), ymax=as.numeric(as.character(ci_u))), alpha=0.4) +
  scale_x_continuous(breaks=c(mse.5$bws[1], mse.75$bws[1], mse$bws[1], mse1.25$bws[1], mse1.5$bws[1]),
                     labels = c(paste('0.5*MSE',round(mse.5$bws[1],1),sep='\n'),
                                paste('0.75*MSE',round(mse.75$bws[1],1),sep='\n'),
                                paste('MSE',round(mse$bws[1],1),sep='\n'),
                                paste('1.25*MSE',round(mse1.25$bws[1],1),sep='\n'),
                                paste('1.5*MSE',round(mse1.5$bws[1],1),sep='\n'))) +
  geom_hline(aes(yintercept=0),
             linetype='dashed', colour='#666666') +
  ggtitle("MSE Bandwidth") +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=10, angle = 90), axis.text.y = element_text(size = 10)) +
  xlab(NULL) +
  ylab(paste('RD effect on negative mentions', '3 months after election', sep = '\n'))
ggsave('./output/figures/figureJ4a.pdf', width=6, height=4.85)


### Figure b: sensitivity to bandwidth size (CER)
cer <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), get(j)],
                sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), victory_margin],
                covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), covars_media, with = FALSE],
                p = 1,
                kernel = "triangular", bwselect = "cerrd", nnmatch = nnmatch,
                cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), cluster])
data_cer <- cbind('CER', cer$bws[1], cer$coef[1], cer$ci[3,1], cer$ci[3,2])

cer1.25 <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), get(j)],
                    sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), victory_margin],
                    covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), covars_media, with = FALSE],
                    p = 1,
                    kernel = "triangular", h=1.25*cer$bws[1], nnmatch = nnmatch,
                    cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), cluster])
data_cer1.25 <- cbind('1.25*CER', cer1.25$bws[1], cer1.25$coef[1], cer1.25$ci[3,1], cer1.25$ci[3,2])

cer1.5 <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), get(j)],
                   sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), victory_margin],
                   covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), covars_media, with = FALSE],
                   p = 1,
                   kernel = "triangular", h=1.5*cer$bws[1], nnmatch = nnmatch,
                   cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), cluster])
data_cer1.5 <- cbind('1.5*CER', cer1.5$bws[1], cer1.5$coef[1], cer1.5$ci[3,1], cer1.5$ci[3,2])

cer.5 <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), get(j)],
                  sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), victory_margin],
                  covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i),
                                   setdiff(covars_media, c('qualification_level3','tenure_social_rented')), with = FALSE],
                  p = 1,
                  kernel = "triangular", h=0.5*cer$bws[1], nnmatch = nnmatch,
                  cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), cluster])

data_cer.5 <- cbind('0.5*CER', cer.5$bws[1], cer.5$coef[1], cer.5$ci[3,1], cer.5$ci[3,2])

cer.75 <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), get(j)],
                   sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), victory_margin],
                   covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i),
                                    setdiff(covars_media, c('qualification_level3','tenure_social_rented')), with = FALSE],
                   p = 1,
                   kernel = "triangular", h=0.75*cer$bws[1], nnmatch = nnmatch,
                   cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i), cluster])

data_cer.75 <- cbind('0.75*CER', cer.75$bws[1], cer.75$coef[1], cer.75$ci[3,1], cer.75$ci[3,2])

data_cer <- as.data.frame(rbind(data_cer.5, data_cer.75, data_cer, data_cer1.25, data_cer1.5))
colnames(data_cer) <- c('type', 'bandwidth', 'coef', 'ci_l', 'ci_u')

# graph sensitivity to choice of bandwidth
ggplot(data_cer) +
  aes(x=as.numeric(as.character(bandwidth)), y=as.numeric(as.character(coef))) +
  geom_point() +
  geom_errorbar(aes(ymin=as.numeric(as.character(ci_l)), ymax=as.numeric(as.character(ci_u))), alpha=0.4) +
  scale_x_continuous(breaks=c(cer.5$bws[1], cer.75$bws[1], cer$bws[1], cer1.25$bws[1], cer1.5$bws[1]),
                     labels = c(paste('0.5*CER',round(cer.5$bws[1],1),sep='\n'),
                                paste('0.75*CER',round(cer.75$bws[1],1),sep='\n'),
                                paste('CER',round(cer$bws[1],1),sep='\n'),
                                paste('1.25*CER',round(cer1.25$bws[1],1),sep='\n'),
                                paste('1.5*CER',round(cer1.5$bws[1],1),sep='\n'))) +
  geom_hline(aes(yintercept=0),
             linetype='dashed', colour='#666666') +
  ggtitle("CER Bandwidth") +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=10, angle = 90), axis.text.y = element_text(size = 10)) +
  xlab(NULL) +
  ylab(paste('RD effect on negative mentions', '3 months after election', sep = '\n'))
ggsave('./output/figures/figureJ4b.pdf', width=6, height=4.85)


### Figure c: sensitivity to polynomial order
# estimate RD effects with quadratic polynomial
outcomes <- c('prop_negative', 'num_negative', 'prop_positive', 'num_positive')
estimates <- vector(mode = 'list', length = 10)
for (i in seq(1,10,1)) {
  
  coef <- c()
  se <- c()
  p_value <- c()
  cil <- c()
  cir <- c()
  mean_control <- c()
  std_effect <- c()
  bw <-  c()
  nl <- c()
  nr <- c()
  Nl <- c()
  Nr <- c()
  
  for (j in outcomes) {
    
    if (i < 3) {
      nnmatch = 3
    } else {
      nnmatch = i
    }
    main <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), get(j)],
                     sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), victory_margin],
                     p = 2,
                     kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                     cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), cluster])
    
    main_covar <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), get(j)],
                           sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), victory_margin],
                           covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), covars_media, with = FALSE],
                           p = 2,
                           kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                           cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), cluster])
    
    # estimates
    coef <- c(coef, main$coef[1,1])
    coef <- c(coef, main_covar$coef[1,1])
    se <- c(se, main$se[3,1])
    se <- c(se, main_covar$se[3,1])
    p_value <- c(p_value, main$pv[3,1])
    p_value <- c(p_value, main_covar$pv[3,1])
    cil <- c(cil, main$ci[3,1])
    cil <- c(cil, main_covar$ci[3,1])
    cir <- c(cir, main$ci[3,2])
    cir <- c(cir, main_covar$ci[3,2])
    mean_control <- c(mean_control, main$beta_p_l[1])
    mean_control <- c(mean_control, main_covar$beta_p_l[1])
    std_effect <- c(std_effect, main$coef[1,1]/(sentiment[(victory_margin >= -main$bws[1,1] & victory_margin < 0 &
                                                             region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                             !is.na(first_generation)),
                                                          sd(get(j), na.rm = TRUE)]))
    std_effect <- c(std_effect, main_covar$coef[1,1]/(sentiment[(victory_margin >= -main_covar$bws[1,1] & victory_margin < 0 &
                                                                   region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                   !is.na(first_generation)),
                                                                sd(get(j), na.rm = TRUE)]))
    bw <-  c(bw, main$bws[1])
    bw <-  c(bw, main_covar$bws[1])
    nl <- c(nl, main$N_h[1])
    nl <- c(nl, main_covar$N_h[1])
    nr <- c(nr, main$N_h[2])
    nr <- c(nr, main_covar$N_h[2])
    Nl <- c(Nl, main$N[1])
    Nl <- c(Nl, main_covar$N[1])
    Nr <- c(Nr, main$N[2])
    Nr <- c(Nr, main_covar$N[2])
    
  }
  
  estimates[[i]] <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,mean_control=mean_control,
                               std_effect=std_effect,bw=bw,nl=nl,nr=nr,Nl=Nl,Nr=Nr,
                               cov = rep(c('no', 'yes'), length(outcomes)),
                               outcome = rep(outcomes, each=2),
                               month = rep(i, length(coef)))
}  

estimates_quadratic <- data.table(dplyr::bind_rows(estimates))
estimates_quadratic[,type:='quadratic polynomial']

## plot linear and quadratic sentiment effects by month
# load estimates from Figure 3b
estimates_linear <- readRDS('./output/estimates/media_estimates.RDS')
estimates_linear[, type:='linear polynomial']

estimates_all <- rbind(estimates_linear, estimates_quadratic)

ggplot(estimates_all[outcome %in% c('prop_negative') & cov=='yes'],
       aes(x=month, y=coef, color=type)) + geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=cil, ymax=cir), width=.1, position=position_dodge(0.2)) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  scale_x_continuous(breaks = c(1:10)) +
  scale_color_manual(values=c("#0000FF", '#666666')) +
  xlab('Number of months after election') +
  ylab(paste('Minority victory effects on',
             "proportion of negative mentions",
             " about candidate's ethnic group",
             sep='\n')) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave(file='./output/figures/figureJ4c.pdf', width=6, height=4.85)
