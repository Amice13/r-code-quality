############################################################################################################################################
## Replication Data for:                                                                                                                   #
## Wawro, Gregory, and Ira Katznelson. Time Counts: Quantitative Analysis for Historical Social Science. Princeton University Press, 2022. #
############################################################################################################################################

rm(list = ls())
library(ggplot2)
library(gridExtra)
library(grid)

dta      <- read.table("../Data/LaborSenateRollCalls.txt", header=T)
n.period <- max(dta$period)

b.ds     <- array(NA, c(5,8))
se.ds    <- array(NA, c(5,8))

for (j in 1:n.period){
  dta.ds    <- subset(dta, region==1 & period==j)
  out.ds    <- glm(prolabor ~ unionpop + urbanpct + aapct + laborcomm, family=binomial(link="probit"), data = dta.ds)
  b.ds[,j]  <- coef(out.ds)
  se.ds[,j] <- sqrt(diag(vcov(out.ds)))  
}

b.bs  <- array(NA, c(5,8))
se.bs <- array(NA, c(5,8))

for (j in 1:n.period) {
  dta.bs    <- subset(dta, region==2 & period==j)
  out.bs    <- glm(prolabor ~ unionpop + urbanpct + aapct + laborcomm, family=binomial(link="probit"), data=dta.bs)
  b.bs[,j]  <- coef(out.bs)
  se.bs[,j] <- sqrt(diag(vcov(out.bs)))  
}

b.ns  <- array(NA,c(6,8))
se.ns <- array(NA,c(6,8))

for (j in 1:n.period) {
  dta.ns    <- subset(dta,region==3 & period==j)
  out.ns    <- glm(prolabor ~ unionpop + urbanpct + aapct + laborcomm + dem, family=binomial(link="probit"), data=dta.ns)
  b.ns[,j]  <- coef(out.ns)
  se.ns[,j] <- sqrt(diag(vcov(out.ns)))  
}

congnum <- seq(73,80,1)

## Probit regression analysis figure
alldf <- list(b.ds = b.ds, b.bs = b.bs, b.ns = b.ns, se.ds = se.ds, se.bs = se.bs, se.ns = se.ns)
grp   <- c('Region-Period Constant', 'Union Pct', 'Urban Pct', 'African American Pct')
df    <- NULL
for (i in c('ds', 'bs', 'ns')){
  region <- ifelse(i == 'ds', 'Deep South', ifelse(i == 'bs', 'Border South', 'Non-South'))
  for (j in 1:4){
    df <- dplyr::bind_rows(df, 
                           data.frame(x   = congnum,
                                      est = alldf[[paste0('b.',i)]][j,], 
                                      se  = alldf[[paste0('se.',i)]][j,],
                                      grp = grp[j],
                                      reg = region)
    )
  }
}

df$grp.f <- factor(df$grp, levels = c('Region-Period Constant', 'Union Pct', 'African American Pct', 'Urban Pct'))
df$reg.f <- factor(df$reg, levels = c('Deep South', 'Border South', 'Non-South'))
plt <- list()
for (r in unique(df$reg)){
  for (i in unique(df$grp)){
    t = paste0(substr(r, 1, 1), substr(i, 1, 3))
    plt[[t]] <- 
      ggplot(data = df[df$grp == i & df$reg == r,], aes(x = x, y = est)) +
      geom_hline(yintercept = 0, lty = 3, color = 'gray75') +
      geom_point() + 
      geom_errorbar(aes(ymin = est - 1.64 * se, ymax = est + 1.64 * se), width = 0) + 
      scale_x_continuous(breaks = seq(min(df$x), max(df$x), 1)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
      theme_bw() +
      labs(y = '', x = '') + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.text.x = element_blank(), 
            strip.background = element_blank(),
            axis.text.y = element_text(size = 10, color="black"),
            axis.text.x = element_text(size = 10, color="black")
      )
    if (t %in% c('BAfr', 'DAfr', 'BUrb', 'NUrb')){
      plt[[t]] <- plt[[t]] + 
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
        theme(axis.text.y = element_text(size = 10, color="black"),
              axis.text.x = element_text(size = 10, color="black"))
    }
  }
}

pdf('../Figures/Figure 4-8.pdf', width = 7, height = 9)
grid.arrange(arrangeGrob(plt[['DReg']], top=textGrob("Deep South",   hjust=0.2, gp=gpar(fontsize=11, fontface=1))),
             arrangeGrob(plt[['BReg']], top=textGrob("Border South", hjust=0.27, gp=gpar(fontsize=11, fontface=1))),
             arrangeGrob(plt[['NReg']], top=textGrob("Non-South",    hjust=0.2, gp=gpar(fontsize=11, fontface=1)),
                         right=textGrob(unique(df$grp)[1], hjust=.55, rot=270, gp=gpar(fontsize=11, fontface=1))),
             plt[['DUni']],
             plt[['BUni']], 
             arrangeGrob(plt[['NUni']], right=textGrob(unique(df$grp)[2], hjust=.9, rot=270, gp=gpar(fontsize=11, fontface=1))),
             plt[['DAfr']],
             plt[['BAfr']],
             arrangeGrob(plt[['NAfr']], right=textGrob(unique(df$grp)[4], hjust=0.625, rot=270, gp=gpar(fontsize=11, fontface=1))),
             plt[['DUrb']], 
             plt[['BUrb']], 
             arrangeGrob(plt[['NUrb']], right=textGrob(unique(df$grp)[3], hjust=.85,rot=270, gp=gpar(fontsize=11, fontface=1))),
             ncol=3, nrow=4, heights = c(4.45,4,4,4), widths = c(4,4,4.3),
             left=textGrob("Estimates", rot=90, gp=gpar(fontsize=14, fontface=1)),
             bottom=textGrob("Congress", gp=gpar(fontsize=14, fontface=1)))
dev.off()


## Probit regression analysis table
a.names     <- c("73rd,Deep South","73rd,Border South","73rd,Non-South","74th,Deep South","74th,Border South","74th,Non-South","75th,Deep South",
                 "75th,Border South","75th,Non-South","76th,Deep South","76th,Border South","76th,Non-South","77th,Deep South","77th,Border South",
                 "77th,Non-South","78th,Deep South","78th,Border South","78th,Non-South","79th,Deep South","79th,Border South","79th,Non-South",
                 "80th,Deep South","80th,Border South","80th,Non-South")
a.names.alt <- t(matrix(a.names,3,8))

par(mfrow=c(1,2))
rnames   <- a.names.alt
texfile  <- "../Tables/Table 4-1.tex"
roundval <- 3

cat("\\begin{table}\\centering \\small \\ssp \n ",file=texfile,append=FALSE)
cat("\\begin{threeparttable}\n ",file=texfile,append=TRUE)
cat("\\caption{Results from congess-by-congress regressions analysis of labor roll call votes in the Senate (73rd--80th Congresses)} \n \\label{tab:rc_probits} \n",file=texfile,append=TRUE)
cat("\\begin{tabular}{ld{-1}d{-1}d{-1}d{-1}d{-1}d{-1}} \n \\toprule \n \\midrule \n",file=texfile,append=TRUE)
cat("& \\multicolumn{1}{c}{Intercept} & \\multicolumn{1}{c}{Union} & \\multicolumn{1}{c}{AA\\%} & \\multicolumn{1}{c}{Urban \\%} & \\multicolumn{1}{c}{Labor Comm.} &  \\multicolumn{1}{c}{Democrat} \\\\ \n",file=texfile,append=TRUE)

names.ds <- rnames[,1]
for (t in 1:length(names.ds)){
  cat(names.ds[t],file=texfile,append=TRUE)
  for (k in 1:nrow(b.ds)){
    est <- b.ds[k,t]
    se <-  se.ds[k,t]
    if ( !is.na(est) ) {
      pval <- pnorm(abs(est/se), lower.tail=F)*1.64
      siglevel <- ifelse(pval <= 0.1,"^{*}","")
      cat (paste(" & ", round(est,roundval),siglevel),file=texfile,append=TRUE)
    }  else {
      cat (" & \\cdot ",file=texfile,append=TRUE)
    }
  }
  cat (" & \\cdot \\\\\n ",file=texfile,append=TRUE)
}

cat("\n \\midrule \n",file=texfile,append=TRUE)
names.bs <- rnames[,2]
for (t in 1:length(names.bs)){
  cat(names.bs[t],file=texfile,append=TRUE)
  for (k in 1:nrow(b.bs)){
    est <- b.bs[k,t]
    se <-  se.bs[k,t]
    if ( !is.na(est) ) {
      pval <- pnorm(abs(est/se), lower.tail=F)*1.64
      siglevel <- ifelse(pval <= 0.1,"^{*}","")

      cat (paste(" & ", round(est,roundval),siglevel),file=texfile,append=TRUE)
    }   else {
      cat (" & \\cdot ",file=texfile,append=TRUE)
    }
  }
  cat (" & \\cdot \\\\\n ",file=texfile,append=TRUE)
}
cat("\n \\midrule \n",file=texfile,append=TRUE)

names.ns <- rnames[,3]
for (t in 1:length(names.ns)){
  cat(names.ns[t],file=texfile,append=TRUE)
  for (k in 1:(nrow(b.ns))){
    est <- b.ns[k,t]
    se <-  se.ns[k,t]
    if ( !is.na(est) ) {
      pval <- pnorm(abs(est/se), lower.tail=F)*1.64
      siglevel <- ifelse(pval <= 0.1,"^{*}","")

      cat (paste(" & ", round(est,roundval),siglevel),file=texfile,append=TRUE)
    }
  }
  cat (" \\\\\n ",file=texfile,append=TRUE)
}
cat("\\\\ \\bottomrule \n \\end{tabular}\n \\begin{tablenotes} \n \\item \n {\\em Notes}: \\ssp $^* p \\leq .1$. Table entries are probit maximum likelihood coefficient estimates. \\textit{Union} indicates union density, \\textit{AA} indicates percent African-American, \\textit{Urban} indicates percent urbanization, and \\textit{Labor Committee} and \\textit{Democrat} refer to dummy variables indicating membership on the labor committee and in the Democratic party, respectively. \n \\end{tablenotes} \n \\end{threeparttable} \n \\hspace{\\fill} \n \\end{table}\n \n \n",file=texfile,append=TRUE)
