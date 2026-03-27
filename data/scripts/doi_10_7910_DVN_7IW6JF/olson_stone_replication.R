###################################################################################################
# Replication Code for "The Incumbency Advantage in Judicial Elections" ###########################
# Michael Olson and Andrew Stone ##################################################################
###################################################################################################

###################################################################################################
# set working directory and load packages #########################################################
###################################################################################################

  setwd("C:/Users/michael.p.olson/Dropbox/Research/Judicial RD/replication_package")

  require(rdrobust) # for local linear RD estimation
  require(rdd) # for flexible bandwidths in local linear estimation
  require(lfe) # for panel OLS models
  require(ggplot2) # for visualizations
  require(multiwayvcov) # for clustering SEs
  require(stargazer) # for visualizing tabular results
  require(rdlocrand) # for randomization inference estimates of rd design

###################################################################################################
# load the data ###################################################################################
###################################################################################################

  dat <- read.csv("olson_stone_replication_data.csv")

###################################################################################################
# make descriptive plots, in-text references to values, and summary statistics tables #############
###################################################################################################

# summary statistics table ########################################################################

  states <- as.data.frame(model.matrix(data=dat,~full_state-1))
  colnames(states) <- gsub("full_state","State: ",fixed=T,colnames(states))
  
  sumdat <- cbind(dat[,c("dem_share_lead","dem_share","dem_share_lag","dem_win","inc_share",
                         "rep_running_lead","dem_running_lead","inc")],states)
  
  stargazer(sumdat,summary=T,
            covariate.labels = c("Democratic Vote Share (t+1)",
                                 "Democratic Vote Share (t)",
                                 "Democratic Vote Share (t-1)",
                                 "Democrat Wins (t)",
                                 "Incumbent Party Vote Share (t+1)",
                                 "Republican Running (t+1)",
                                 "Democrat Running (t+1)",
                                 "Incumbent Running (t)"),
            summary.stat = c("mean","median","sd","min","max","n"),
            title="Summary Statistics",
            label="sumstats",
            out="sumstats.tex")

# how many races in sample ########################################################################

  fileConn<-file("races.tex")
  writeLines(format(nrow(dat[!is.na(dat$dem_share),]),format="d",big.mark=','), fileConn)
  close(fileConn)

# number and share of contested races #############################################################

  fileConn<-file("contested_number.tex")
  writeLines(format(nrow(dat[!is.na(dat$dem_share) &
                               dat$dem_share>0 & dat$dem_share<1,]),format="d",big.mark=','), fileConn)
  close(fileConn)

  fileConn<-file("contested_share.tex")
  writeLines(as.character(100*round(nrow(dat[!is.na(dat$dem_share) &
                                               dat$dem_share>0 & dat$dem_share<1,])/
                                      nrow(dat[!is.na(dat$dem_share),]),2)), fileConn)
  close(fileConn)

# histogram of democratic vote share (Figure 1 in the text) ##############################################################

  cairo_ps("dem_vote_hist.eps",fallback_resolution=1200,width=8,height=5)
  print(
  ggplot(data=dat[!is.na(dat$deminc) & !is.na(dat$dem_share_lead),],aes(x=dem_share_lead,fill=deminc,group=deminc))+
    geom_histogram(alpha=0.5,bins=20)+
    geom_vline(aes(xintercept=0.5),colour="black",linetype=2,size=2)+
    xlab("Democratic Vote Share")+
    ylab("Number of Races")+
    labs(fill = "Democrat Won Previous:")+
    scale_fill_grey(start=0.05,end=0.5)+
    theme_minimal()+ 
    theme(legend.position="bottom",
          legend.title = element_text(face = "bold", size = 10))
  )
  dev.off()

# histogram of incumbent party vote share #########################################################
# this is for the appendix, Figure A.1 ############################################################
  
  cairo_ps("incumbent_vote_share_zoomed.eps",fallback_resolution=1200,width=8,height=5)
  print(
  ggplot(data=dat,aes(x=inc_share))+
    geom_histogram(alpha=0.5,bins=30)+
    geom_vline(aes(xintercept=0.5),colour="black",linetype=2,size=2)+
    xlim(0.4,0.6)+
    xlab("Incumbent Party Vote Share")+
    ylab("Count")+
    theme_minimal()
  )
  dev.off()
  
###################################################################################################
# visualize regression discontinuity design #######################################################
###################################################################################################

# visualize the base regression discontinuity design (Figure 3 in the text) ##############################################
  
  cairo_ps("actual_outcome.eps",fallback_resolution=1200,width=8,height=5)
  print(
  ggplot(data=dat,aes(x=dem_share,y=dem_share_lead))+
    geom_point(alpha=0.1)+
    geom_smooth(data=dat[dat$dem_share<0.5,],se=F,colour="black",size=2,method="loess")+
    geom_smooth(data=dat[dat$dem_share>=0.5,],se=F,colour="black",size=2,method="loess")+
    geom_vline(xintercept=0.5,size=1.5,colour="grey50",linetype=2)+
    xlab("Democratic Vote Share, Time = T")+
    ylab("Democratic Vote Share, Time = T+1")+
    xlim(0.4,0.6)+
    ylim(0,1)+
    theme_minimal()
  )
  dev.off()

# visualize the regression discontinuity design with the lagged outcome (Figure 2 in the text) ###########################

  cairo_ps("lagged_outcome.eps",fallback_resolution=1200,width=8,height=5)
  print(
  ggplot(data=dat,aes(x=dem_share,y=dem_share_lag))+
    geom_point(alpha=0.1)+
    geom_smooth(data=dat[dat$dem_share<0.5,],se=F,colour="black",size=2,method="loess")+
    geom_smooth(data=dat[dat$dem_share>=0.5,],se=F,colour="black",size=2,method="loess")+
    geom_vline(xintercept=0.5,size=1.5,colour="grey50",linetype=2)+
    xlab("Democratic Vote Share, Time = T")+
    ylab("Democratic Vote Share, Time = T-1")+
    xlim(0.4,0.6)+
    theme_minimal()
  )
  dev.off()

###################################################################################################
# estimate effects of incumbency using local linear regression ####################################
###################################################################################################
  
# first Use rdrobust at optimal bandwidth #########################################################
  
  # incumbency advantage estimates
  
    lead <- rdrobust(x=dat$dem_share,y=dat$dem_share_lead,
                     cluster=dat$office,c=0.5,all=TRUE)
    
    lead_vec <- c("Dem. Vote Share (t+1)",lead$coef[3],lead$se[3],lead$bws[1,1],sum(lead$N_h))
    
    fileConn<-file("rdrobust_coef.tex")
    writeLines(format(round(lead$coef[3],3),format="d",big.mark=','), fileConn)
    close(fileConn)
    
  # validity check (Y as lagged outcome not lead outcome)
  
    lag <- rdrobust(x=dat$dem_share,y=dat$dem_share_lag,
                    cluster=dat$office,c=0.5,all=TRUE)
    
    lag_vec <- c("Dem. Vote Share (t-1)",lag$coef[3],lag$se[3],lag$bws[1,1],sum(lag$N_h))
    
  # combine estimates and make table (Table B.2 in the appendix)
    
    rdrobust_dat <- as.data.frame(rbind(lead_vec,lag_vec))
    colnames(rdrobust_dat) <- c("Outcome","Estimate","Standard Error","Bandwidth","Effective Observations")
    rownames(rdrobust_dat) <- NULL
    rdrobust_dat$Estimate <- as.numeric(as.character(rdrobust_dat$Estimate))
    rdrobust_dat$`Standard Error` <- as.numeric(as.character(rdrobust_dat$`Standard Error`))
    rdrobust_dat$`Bandwidth` <- as.numeric(as.character(rdrobust_dat$`Bandwidth`))
    
    rd_sg <- stargazer(rdrobust_dat,summary=F,
                       title="Regression Discontinuity Estimate of Incumbency Advantage",
                       label="rdrobust",
                       rownames=F,
                       notes="\\parbox[t]{0.975\\textwidth}{\\footnotesize \\textit{Note}: Table presents local linear regression
                       estimates produced using \\textsc{rdrobust} package in \\textsc{R}. We present bias-corrected coefficients
                       with robust standard errors, clustered by seat. Optimal bandwidth is selected using \\textsc{rdbwselect}. Running
                       variable is Democratic Vote Share in time t, and the cutpoint is 0.5.}")
    
    cat(rd_sg, sep = '\n', file  = paste("rdrobust.tex",sep=""))

# now estimate at a variety of bandwidths #########################################################

  # incumbency advantage (plot of estimates by bandwidth) - Figure 4 in the main text
    
    bw_vec <- seq(0.01,0.2,by=0.01)
    
    rdbw_out <- RDestimate(dem_share_lead~dem_share,cutpoint=0.5,
                           bw=bw_vec,cluster=dat$office,data=dat)
    
    rdbw_out <- as.data.frame(cbind(rdbw_out$est,rdbw_out$bw,rdbw_out$se,rdbw_out$obs))
    colnames(rdbw_out) <- c("coef","bw","se","obs")
    
    cairo_ps("main_result.eps",fallback_resolution=1200,width=8,height=5)
    print(
    ggplot(data=rdbw_out,aes(x=bw,y=coef))+
      geom_point()+
      geom_errorbar(aes(ymin=coef-2*se,ymax=coef+2*se),width=0,size=0.5)+
      geom_errorbar(aes(ymin=coef-1.65*se,ymax=coef+1.65*se),width=0,size=1)+
      geom_hline(yintercept = 0, linetype=2, colour="grey50")+
      geom_text(aes(x=bw,y=rdbw_out$coef-2*rdbw_out$se-0.02,label=obs),size=3)+
      ylab("Estimate of Incumbency Advantage")+
      xlab("Bandwidth")+
      theme_minimal()
    )
    dev.off()
  
  # validity check (same plot but with *lagged* vote share as the outcome) - Figure A.3 in the appendix
    
    rdbw_out_lag <- RDestimate(dem_share_lag~dem_share,cutpoint=0.5,
                           bw=bw_vec,cluster=dat$office,data=dat)
    
    rdbw_out_lag <- as.data.frame(cbind(rdbw_out_lag$est,rdbw_out_lag$bw,rdbw_out_lag$se,rdbw_out_lag$obs))
    colnames(rdbw_out_lag) <- c("coef","bw","se","obs")
    
    cairo_ps("placebo_test.eps",fallback_resolution=1200,width=8,height=5)
    print(
    ggplot(data=rdbw_out_lag,aes(x=bw,y=coef))+
      geom_point()+
      geom_errorbar(aes(ymin=coef-2*se,ymax=coef+2*se),width=0,size=0.5)+
      geom_errorbar(aes(ymin=coef-1.65*se,ymax=coef+1.65*se),width=0,size=1)+
      geom_hline(yintercept = 0, linetype=2, colour="grey50")+
      geom_text(aes(x=bw,y=rdbw_out_lag$coef-2*rdbw_out_lag$se-0.02,label=obs),size=3)+
      ylab("Estimate")+
      xlab("Bandwidth")+
      theme_minimal()
    )
    dev.off()
    
  # democrat running outcome - for the scare-off analysis - Figure 5a in the main text
    
    rdbw_out_dem <- RDestimate(dem_running_lead~dem_share,cutpoint=0.5,
                           bw=bw_vec,cluster=dat$office,data=dat)
    
    rdbw_out_dem <- as.data.frame(cbind(rdbw_out_dem$est,rdbw_out_dem$bw,rdbw_out_dem$se,rdbw_out_dem$obs))
    colnames(rdbw_out_dem) <- c("coef","bw","se","obs")
    
    cairo_ps("dem_running_result.eps",fallback_resolution=1200,width=8,height=5)
    print(
      ggplot(data=rdbw_out_dem,aes(x=bw,y=coef))+
        geom_point()+
        geom_errorbar(aes(ymin=coef-2*se,ymax=coef+2*se),width=0,size=0.5)+
        geom_errorbar(aes(ymin=coef-1.65*se,ymax=coef+1.65*se),width=0,size=1)+
        geom_hline(yintercept = 0, linetype=2, colour="grey50")+
        geom_text(aes(x=bw,y=rdbw_out_dem$coef-2*rdbw_out_dem$se-0.02,label=obs),size=3)+
        ylab("Estimated Change in Probability of Democrat Running")+
        xlab("Bandwidth")+
        theme_minimal()
    )
    dev.off()
    
  # republican running outcome - for the scare-off analysis - Figure 5b in the main text
    
    rdbw_out_rep <- RDestimate(rep_running_lead~dem_share,cutpoint=0.5,
                           bw=bw_vec,cluster=dat$office,data=dat)
    
    rdbw_out_rep <- as.data.frame(cbind(rdbw_out_rep$est,rdbw_out_rep$bw,rdbw_out_rep$se,rdbw_out_rep$obs))
    colnames(rdbw_out_rep) <- c("coef","bw","se","obs")
    
    cairo_ps("rep_running_result.eps",fallback_resolution=1200,width=8,height=5)
    print(
      ggplot(data=rdbw_out_rep,aes(x=bw,y=coef))+
        geom_point()+
        geom_errorbar(aes(ymin=coef-2*se,ymax=coef+2*se),width=0,size=0.5)+
        geom_errorbar(aes(ymin=coef-1.65*se,ymax=coef+1.65*se),width=0,size=1)+
        geom_hline(yintercept = 0, linetype=2, colour="grey50")+
        geom_text(aes(x=bw,y=rdbw_out_rep$coef-2*rdbw_out_rep$se-0.02,label=obs),size=3)+
        ylab("Estimated Change in Probability of Republican Running")+
        xlab("Bandwidth")+
        theme_minimal()
    )
    dev.off()
    
###################################################################################################
# robustness checks ###############################################################################
###################################################################################################
    
# re-estimate before and after 2000 (Figure B.5 in the appendix) ###############################################################
  
  rdbw_out_2000 <- RDestimate(dem_share_lead~dem_share,cutpoint=0.5,
                              bw=bw_vec,cluster=dat$office[dat$year>=2002],data=dat[dat$year>=2002,])
  
  rdbw_out_2000 <- as.data.frame(cbind(rdbw_out_2000$est,rdbw_out_2000$bw,rdbw_out_2000$se,rdbw_out_2000$obs))
  colnames(rdbw_out_2000) <- c("coef","bw","se","obs")
  rdbw_out_2000$`Time Period` <- "After and Including 2002"
  
  rdbw_out_1990 <- RDestimate(dem_share_lead~dem_share,cutpoint=0.5,
                              bw=bw_vec,cluster=dat$office[dat$year<2002],data=dat[dat$year<2002,])
  
  rdbw_out_1990 <- as.data.frame(cbind(rdbw_out_1990$est,rdbw_out_1990$bw,rdbw_out_1990$se,rdbw_out_1990$obs))
  colnames(rdbw_out_1990) <- c("coef","bw","se","obs")
  rdbw_out_1990$`Time Period` <- "Before 2002"
  
  rdbw_out_2000 <- rbind(rdbw_out_2000,rdbw_out_1990)
  
  cairo_ps("main_result_2000.eps",fallback_resolution=1200,width=9,height=5)
  print(
  ggplot(data=rdbw_out_2000,aes(x=bw,y=coef,group=`Time Period`,colour=`Time Period`))+
    geom_point(position=position_dodge(width=0.006))+
    geom_errorbar(aes(ymin=coef-2*se,ymax=coef+2*se),width=0,size=0.5,position=position_dodge(width=0.006))+
    geom_errorbar(aes(ymin=coef-1.65*se,ymax=coef+1.65*se),width=0,size=1,position=position_dodge(width=0.006))+
    geom_hline(yintercept = 0, linetype=2, colour="grey50")+
    geom_text(show.legend=F,aes(x=bw,y=rdbw_out_2000$coef-2*rdbw_out_2000$se-0.02,label=obs),size=3,position=position_dodge(width=0.006))+
    ylab("Estimate")+
    xlab("Bandwidth")+
    scale_colour_grey(start=0,end=0.4)+
    theme_minimal()
  )
  dev.off()
  
# re-estimate with a demeaned outcome (Figure B.3 in the appendix) #############################################################

  rdbw_out_demean <- RDestimate(dm_outcome~dem_share,cutpoint=0.5,
                         bw=bw_vec,cluster=dat$office,data=dat)
  
  rdbw_out_demean <- as.data.frame(cbind(rdbw_out_demean$est,rdbw_out_demean$bw,rdbw_out_demean$se,rdbw_out_demean$obs))
  colnames(rdbw_out_demean) <- c("coef","bw","se","obs")
  
  cairo_ps("demeaned_result.eps",fallback_resolution=1200,width=8,height=5)
  print(
  ggplot(data=rdbw_out_demean,aes(x=bw,y=coef))+
    geom_point()+
    geom_errorbar(aes(ymin=coef-2*se,ymax=coef+2*se),width=0,size=0.5)+
    geom_errorbar(aes(ymin=coef-1.65*se,ymax=coef+1.65*se),width=0,size=1)+
    geom_hline(yintercept = 0, linetype=2, colour="grey50")+
    geom_text(aes(x=bw,y=rdbw_out_demean$coef-2*rdbw_out_demean$se-0.02,label=obs),size=3)+
    ylab("Estimate")+
    xlab("Bandwidth")+
    theme_minimal()
  )
  dev.off()

# re-estimate using a democrat running lag (placebo check, for Figure A.4 in the appendix) ########################################################

  rdbw_out_demlag <- RDestimate(dem_running_lag~dem_share,cutpoint=0.5,
                         bw=bw_vec,cluster=dat$office,data=dat)
  
  rdbw_out_demlag <- as.data.frame(cbind(rdbw_out_demlag$est,rdbw_out_demlag$bw,rdbw_out_demlag$se,rdbw_out_demlag$obs))
  colnames(rdbw_out_demlag) <- c("coef","bw","se","obs")
  
  cairo_ps("dem_running_result_lag.eps",fallback_resolution=1200,width=8,height=5)
  print(
  ggplot(data=rdbw_out_demlag,aes(x=bw,y=coef))+
    geom_point()+
    geom_errorbar(aes(ymin=coef-2*se,ymax=coef+2*se),width=0,size=0.5)+
    geom_errorbar(aes(ymin=coef-1.65*se,ymax=coef+1.65*se),width=0,size=1)+
    geom_hline(yintercept = 0, linetype=2, colour="grey50")+
    geom_text(aes(x=bw,y=rdbw_out_demlag$coef-2*rdbw_out_demlag$se-0.02,label=obs),size=3)+
    ylab("Estimated Change in Probability of Democrat Running, Lagged")+
    xlab("Bandwidth")+
    theme_minimal()
  )
  dev.off()
  
# re-estimate using a republican running lag (placebo check, for Figure A.4 in the appendix) ######################################################
  
  rdbw_out_replag <- RDestimate(rep_running_lag~dem_share,cutpoint=0.5,
                         bw=bw_vec,cluster=dat$office,data=dat)
  
  rdbw_out_replag <- as.data.frame(cbind(rdbw_out_replag$est,rdbw_out_replag$bw,rdbw_out_replag$se,rdbw_out_replag$obs))
  colnames(rdbw_out_replag) <- c("coef","bw","se","obs")
  
  cairo_ps("rep_running_result_lag.eps",fallback_resolution=1200,width=8,height=5)
  print(
  ggplot(data=rdbw_out_replag,aes(x=bw,y=coef))+
    geom_point()+
    geom_errorbar(aes(ymin=coef-2*se,ymax=coef+2*se),width=0,size=0.5)+
    geom_errorbar(aes(ymin=coef-1.65*se,ymax=coef+1.65*se),width=0,size=1)+
    geom_hline(yintercept = 0, linetype=2, colour="grey50")+
    geom_text(aes(x=bw,y=rdbw_out_replag$coef-2*rdbw_out_replag$se-0.02,label=obs),size=3)+
    ylab("Estimated Change in Probability of Republican Running, Lagged")+
    xlab("Bandwidth")+
    theme_minimal()
  )
  dev.off()

# test across different placebo cutpoints (Figure A.2 in the appendix) #########################################################

  cutpoint_vec <- seq(0.4,0.6,by=0.02)
  coef_vec <- vector()
  se_vec   <- vector()
  
  for(i in 1:length(cutpoint_vec)){
    
    rdout <- RDestimate(dem_share_lead~dem_share,cutpoint=cutpoint_vec[i],
                        bw=0.05,cluster=dat$office,data=dat)
    
    coef_vec[i] <- rdout$est[1]
    
    se_vec[i]   <- rdout$se[1]
    
  }
  
  rdbw_out <- as.data.frame(cbind(coef_vec,cutpoint_vec,se_vec))
  colnames(rdbw_out) <- c("coef","cutpoint","se")
  
  cairo_ps("cutpoint_placebo.eps",fallback_resolution=1200,width=8,height=5)
  print(
  ggplot(data=rdbw_out,aes(x=cutpoint,y=coef))+
    geom_point()+
    geom_errorbar(aes(ymin=coef-2*se,ymax=coef+2*se),width=0,size=0.5)+
    geom_errorbar(aes(ymin=coef-1.65*se,ymax=coef+1.65*se),width=0,size=1)+
    geom_hline(yintercept = 0, linetype=2, colour="grey50")+
    ylab("Estimate")+
    xlab("Cutpoint")+
    theme_minimal()
  )
  dev.off()

# OLS models (Figure B.4 in the Appendix) ######################################################################################
  
  # create running variable and polynomials
  
    dat$running <- dat$dem_share-0.5
    dat$running2 <- dat$running^2
    dat$running3 <- dat$running^3
    dat$running4 <- dat$running^4

  # we want to run a series of regressions across polynomial fits and bandwidths
  
  # create bw vector 
  
    bw_vec <- seq(0.01,0.2,by=0.01)

  # create formula vector
    
    lin <- as.formula("dem_share_lead~dem_win*running")
    quad <- as.formula("dem_share_lead~dem_win*(running+running2)")
    cube <- as.formula("dem_share_lead~dem_win*(running+running2+running3)")
    quart <- as.formula("dem_share_lead~dem_win*(running+running2+running3+running4)")
    
    form_vec <- c(lin,quad,cube,quart)
    poly_order_vec <- c("One","Two","Three","Four")
    
    coef_list <- list()
    se_list   <- list()
    bw_list   <- list()
    form_list <- list()
    
    for(j in 1:length(form_vec)){
      
      coef <- vector()
      se   <- vector()
      
      for(i in 1:length(bw_vec)){
        
        coef[i] <- coef(lm(form_vec[j][[1]],data=dat[abs(dat$running)<bw_vec[i],]))["dem_win"]
        se[i]   <- sqrt(cluster.vcov(lm(form_vec[j][[1]],data=dat[abs(dat$running)<bw_vec[i],]),
                                     dat[abs(dat$running)<bw_vec[i],c("office")])["dem_win","dem_win"])
      }
      
      coef_list[[j]] <- coef
      se_list[[j]] <- se
      bw_list[[j]] <- bw_vec
      form_list[[j]] <- rep(poly_order_vec[j],length(bw_vec))
      
    }
    
    olsdat <- as.data.frame(cbind(unlist(coef_list),unlist(se_list),unlist(bw_list),unlist(form_list)))
    colnames(olsdat) <- c("coef","se","bw","poly")
    olsdat$poly <- factor(olsdat$poly,levels=c("One","Two","Three","Four"))
    olsdat$z <- as.numeric(as.character(olsdat$coef))/as.numeric(as.character(olsdat$se))
    olsdat$coef2 <- format(round(as.numeric(as.character(olsdat$coef)),2))
    olsdat$se2 <- format(round(as.numeric(as.character(olsdat$se)),2))
    olsdat$use <- ifelse(olsdat$z<1.65,
                         paste(olsdat$coef2," (",olsdat$se2,")",sep=""),
                         ifelse(olsdat$z<1.96,
                                paste(olsdat$coef2,"* (",olsdat$se2,")",sep=""),
                                paste(olsdat$coef2,"** (",olsdat$se2,")",sep="")))
    olsdat$`p-value` <- ">0.10"
    olsdat$`p-value`[olsdat$z>1.65] <- "<0.10"
    olsdat$`p-value`[olsdat$z>2] <- "<0.05"
    olsdat$`p-value` <- factor(olsdat$`p-value`)
  
    cairo_ps("ols_results.eps",fallback_resolution=1200,width=8,height=8)
    print(
    ggplot(data=olsdat,aes(x=poly,y=bw))+
      geom_tile(fill="white",color="black")+
      geom_text(aes(label=use),size=5)+
      xlab("Polynomial Order")+
      ylab("Bandwidth")+
      theme_minimal()
    )
    dev.off()

# randomization inference (Table B.3 in the Appendix) #########################################################################
  
  windows <- seq(0.002,0.05,by=0.002)
  locrand_out_list <- list()
  
  for(j in 1:length(windows)){
    
    locrand_out <-  rdrandinf(Y=dat$dem_share_lead,
                              R=dat$dem_share,
                              cutoff=0.5,
                              wl=(0.50-windows[j]),wr=(0.50+windows[j]))
    
    locrand_out_list[[j]] <- c(locrand_out$obs.stat,
                               locrand_out$p.value,
                               locrand_out$sumstats[5,],
                               locrand_out$sumstats[2,])
    
  }
  
  locrand_present <- as.data.frame(do.call("rbind",locrand_out_list))
  colnames(locrand_present) <- c("Estimate","P-Value","Left Edge","Right Edge","Left Obs.","Right Obs.")
  
  locrand_tab_sg <- stargazer(locrand_present,summary=FALSE,
                              rownames = FALSE,
                              #font.size="footnotesize",
                              title="RD Estimates, Randomization Inference",
                              label = "locrand_est",
                              #digits=2,
                              notes="\\parbox[t]{0.725\\textwidth}{\\footnotesize \\textit{Note}: Regression discontinuity estimates with randomization inference p-values.
                           Estimation undertaken using \\textsc{rdrandinf} function from \\textsc{rdlocrand} package in \\textsc{R}.}")
  
  cat(locrand_tab_sg, sep = '\n', file = "locrand_out.tex")
  
# dropping states one at a time (Figure 6 in the main text) ###################################################################
  
  dropstate <- list()
  
  for(j in 1:length(unique(dat$full_state))){
    
    bit <- dat[dat$full_state!=sort(unique(dat$full_state))[j],]
    
    
    lead_loop <- rdrobust(x=bit$dem_share,y=bit$dem_share_lead,
                     cluster=bit$office,c=0.5,all=TRUE)
    
    lead_vec <- c(sort(unique(dat$full_state))[j],lead_loop$coef[3],lead_loop$se[3],lead_loop$bws[1,1],sum(lead_loop$N_h))
    
    dropstate[[j]] <- lead_vec
    
    
  }
  
  ds_tab <- as.data.frame(do.call(rbind,dropstate))
  colnames(ds_tab) <- c("state","est","se","bw","obs")
  ds_tab$bw <- paste("BW = ",round(as.numeric(as.character(ds_tab$bw)),3),sep="")
  ds_tab$obs <- paste("N = ",ds_tab$obs,sep="")
  ds_tab$est <- as.numeric(as.character(ds_tab$est))
  ds_tab$se  <- as.numeric(as.character(ds_tab$se))
  
  cairo_ps("dropstate.eps",fallback_resolution=1200,width=8,height=5)
  print(
  ggplot(data=ds_tab,aes(x=state,y=est))+
    geom_point(size=3)+
    geom_linerange(aes(ymin=est-2*se,ymax=est+2*se),size=2)+
    geom_text(aes(x=state,y=est-2*se-0.02,label=bw),size=3)+
    geom_text(aes(x=state,y=est-2*se-0.04,label=obs),size=3)+
    geom_hline(size=2,linetype=2,yintercept = 0,colour="grey50")+
    xlab("Omitted State")+
    ylab("Estimate of Incumbency Advantage")+
    theme_minimal()
  )
  dev.off()

# only competitive races (Figure B.2 in the Appendix) ##########################################################################

  dat_comp <- dat[dat$dem_share_lead<1 & dat$dem_share_lead>0,]
  
  bw_vec <- seq(0.01,0.2,by=0.01)
  
  rdbw_out_comp <- RDestimate(dem_share_lead~dem_share,cutpoint=0.5,
                         bw=bw_vec,cluster=dat_comp$office,data=dat_comp)
  
  rdbw_out_comp <- as.data.frame(cbind(rdbw_out_comp$est,rdbw_out_comp$bw,rdbw_out_comp$se,rdbw_out_comp$obs))
  colnames(rdbw_out_comp) <- c("coef","bw","se","obs")
  
  cairo_ps("contested_result.eps",fallback_resolution=1200,width=8,height=5)
  print(
  ggplot(data=rdbw_out_comp,aes(x=bw,y=coef))+
    geom_point()+
    geom_errorbar(aes(ymin=coef-2*se,ymax=coef+2*se),width=0,size=0.5)+
    geom_errorbar(aes(ymin=coef-1.65*se,ymax=coef+1.65*se),width=0,size=1)+
    geom_hline(yintercept = 0, linetype=2, colour="grey50")+
    geom_text(aes(x=bw,y=rdbw_out_comp$coef-2*rdbw_out_comp$se-0.003,label=obs),size=3)+
    ylab("Estimate of Incumbency Advantage")+
    xlab("Bandwidth")+
    theme_minimal()
  )
  dev.off()
  
###################################################################################################
# personal incumbency advantage models ############################################################
###################################################################################################

# personal incumbency descriptives (Figure B.1 in the Appendix) ################################################################

  fileConn<-file("prop_contested.tex")
  writeLines(paste(100*round( mean(dat$inc,na.rm=T),3),"\\%",sep=""), fileConn)
  close(fileConn)

  dat$folded <- abs(dat$dem_share_lag-0.5)
  
  cairo_ps("prob_reelection.eps",fallback_resolution=1200,width=8,height=5)
  print(
  ggplot(data=dat,aes(x=folded,y=inc))+
    geom_jitter(colour="grey40",width=0.01,height=0.01)+
    geom_smooth(size=2,se=F,colour="black")+
    xlab("Previous Election Margin")+
    ylab("Incumbent Running")+
    theme_minimal()
  )
  dev.off()

# individual-level models (Table B.4 in the Appendix) #########################################################################

  ind <- felm(dem_share~inc3|office+year|0|office,data=dat)
  
  ind_ldv <- felm(dem_share~inc3+dem_share_lag|year|0|office,data=dat)
  
  pers_sg <- stargazer(ind,ind_ldv,
                       dep.var.labels = c("Democratic Vote Share"),
                       title="Personal Incumbency Advantage Estimates",
                       label="personal",
                       covariate.labels = c("Incumbent",
                                            "Lagged Dem. Vote Share"),
                       order=c(1,3,4,2),
                       add.lines = list(c("Office Fixed Effects","\\checkmark","","\\checkmark",""),
                                        c("Year Fixed Effects","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
                       keep.stat = c("n"),
                       star.cutoffs = c(0.10,0.05),star.char = c("*","**"),table.placement="!ht",
                       notes.append = FALSE,notes.label = "",column.sep.width="0pt",
                       table.layout ="-ld-#-t-as-n",
                       notes="\\parbox[t]{0.5\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                       standard errors clustered on seat in parentheses. ``Incumbent'' is a 3-way indicator taking a value of 1 if a Democratic
                       incumbent was running, -1 if a Republican incumbent was running, and 0 if no incumbent was running.
                       Observations are at the seat-year level.
                        $^{*}$p$<$0.10, $^{**}$p$<$0.05 (two-tailed test).}")
  
  cat(pers_sg, sep = '\n', file = "personal.tex")   

# texas appointments (Table B.5 in the Appendix) ##############################################################################
  
  tx_appoint1 <- felm(dem_share~inc3|office+year|0|office,data=dat[!is.na(dat$appoint),])
  
  tx_appoint2 <- felm(dem_share~inc3+dem_share_lag|year|0|office,data=dat[!is.na(dat$appoint),])
  
  tx_appoint3 <- felm(dem_share~inc3+appoint_inc3|office+year|0|office,data=dat[!is.na(dat$appoint),])
  
  tx_appoint4 <- felm(dem_share~inc3+appoint_inc3+dem_share_lag|year|0|office,data=dat[!is.na(dat$appoint),])
  
  appoint_sg <- stargazer(tx_appoint1,tx_appoint2,tx_appoint3,tx_appoint4,
                          dep.var.labels = c("Democratic Vote Share"),
                          title="Incumbency Advantage for Appointed Judges in Texas (2006-2016)",
                          label="appointed",order=c(1,3,2),
                          covariate.labels = c("Incumbent","Incumbent $\\times$ Appointed","Lagged Dem. Vote Share"),
                          add.lines = list(c("Office Fixed Effects","\\checkmark","","\\checkmark",""),
                                           c("Year Fixed Effects","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
                          keep.stat = c("n"),
                          star.cutoffs = c(0.10,0.05),star.char = c("*","**"),table.placement="!ht",
                          notes.append = FALSE,notes.label = "",column.sep.width="0pt",
                          table.layout ="-ld-#-t-as-n",
                          notes="\\parbox[t]{0.725\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                       standard errors clustered on seat in parentheses. Observations are at the seat-year level.
                        $^{*}$p$<$0.10, $^{**}$p$<$0.05 (two-tailed test).}")
  
  cat(appoint_sg, sep = '\n', file = "appointed.tex")  

  
  
 