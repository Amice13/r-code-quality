#Fake Fake News Warnings: Misinformation Warnings and the Tainted Truth Effect 

#An online survey experiment conducted through Amazon Mechanical Turk
#by Melanie Freeze and the POSC 226 (Political Psychology) at Carleton College.

#NOTE: Original survey data was first cleaned in Stata using "cleaningTTEdata.do"


###
# INSTALL Packages AND LOAD LIBRARIES HERE
###

  
    #install.packages("stargazer")
    library(stargazer)
    
    #install.packages("tidyverse")
    library(tidyverse)
    
    #install.packages("multcomp")
    library(multcomp)
    

###
    #README Note: you will need to change this working directory to point to the folder where you have downloaded the data and script files
    setwd("")
    
    tte <-  read.csv("tte_cleanedfopreplication.csv", stringsAsFactors=FALSE) 
    
    
###
#variable Cleaning here
###
    
    tte<- tte %>%
      mutate(warnmain.f=factor(warnmain, labels=c("not warned", "warned")), 
             misinfomain.f=factor(misinfomain, labels=c("misinformed", "not informed", "informed")),
             condition.f=factor(condition, labels=c("Not Warned-Misinformed", "Not Warned-Control", "Not Warned-Informed", "Warned-Misinformed", "Warned-Control", "Warned-Informed")),
             cond.f=factor(cond, labels=c("No Warning, Misinformation" , "Warning, Misinformation"  ,  "No Warning, Control"  , "Warning, Control" , "No Warning, Information" , "Warning, Information")))
    
  #subset dataset to include only individuals who were exposed to critical experimental manipulations/materials
    #I also select only variables of interest to make it easier to see the data. 
    #If you want to look at other variables...just add to the list below

    ttesub<- tte %>%
      filter(drop2==0) %>%
      dplyr::select(memcrit, membuf, memmisinfo, dkcrit, dkbuf, 
                    crednews_clean, credvid_clean,
                    warnmain, misinformed, notinformed, informed, warned_misinformed, 
                    warned_notinformed, warned_informed, cond.f)
    
    
    #dkcrit, dkbuf, cond, condition, condition.f, cond.f, misinfomain.f, warnmain.f,  
    #misinfomain, id, drop2, warnmain, misinformed, notinformed, informed, warned_misinformed, 
    #warned_notinformed, warned_informed, memcrit, memall, membuf, memmisinfo, crednews_clean, credvid_clean
    #female, attention, age, married, inc, collegegrad, over50k, unemp, reg, citizen, 
    #mobile, race, white, lowcog, partisan, know, ideological, newsdays, firstday)

###    
# Memory Score and Misinformation Score 
###
    #Figure 1 is not created in R: (Figure 1: Overview of Szpitalak and Polczyk's (2011) Experimental Approach)
    
    #Figure 2: Average Memory Score by Condition
      #Calculate means
      ttesum<-ttesub %>%
      group_by(cond.f)  %>%
      summarize(memcritavg=mean(memcrit, na.rm=T), 
                memcritsd=sd(memcrit, na.rm=T),
                membufavg=round( mean(membuf, na.rm=T), 3),
                membufsd=round( sd(membuf), 3),
                memmisinfoavg=round( mean(memmisinfo, na.rm=T), 3),
                memmisinfosd=round( sd(memmisinfo), 3),
                n=n(),
                memcritse=memcritsd/sqrt(n),
                lower.ci.crit = memcritavg - qt(1 - (0.05 / 2), n - 1) * memcritse,
                upper.ci.crit = memcritavg + qt(1 - (0.05 / 2), n - 1) * memcritse,
                membufse=membufsd/sqrt(n),
                lower.ci.buf = membufavg - qt(1 - (0.05 / 2), n - 1) * membufse,
                upper.ci.buf = membufavg + qt(1 - (0.05 / 2), n - 1) * membufse,
                memmisinfose=memmisinfosd/sqrt(n),
                lower.ci.misinfo = memmisinfoavg - qt(1 - (0.05 / 2), n - 1) * memmisinfose,
                upper.ci.misinfo = memmisinfoavg + qt(1 - (0.05 / 2), n - 1) * memmisinfose)
    
                ttesum
                ttesum$order<-c(1:6)
                ttesum$info<-ifelse(ttesum$order==1:2, c(1), ifelse(ttesum$order==3:4, c(2), c(3)))
                ttesum
                ttesum$info.f<-factor(ttesum$info, labels=c("Misinformation", "Control", "Information"))
    
          #for graphs' x labels                
           abbrev_x <- c("No", "Yes","No", "Yes","No", "Yes")
           
      #Panel a: Fixed Facts
           tiff("Figure2a.tiff", units="in", width=3.66, height=3.82, res=300)   
      ggplot(ttesum, aes(order, membufavg, fill=info.f))+
        geom_bar(stat="identity")+
        geom_errorbar(aes(ymin=lower.ci.buf, ymax=upper.ci.buf), width=.1)+
        scale_x_continuous(breaks = seq(1,6,by=1), labels = abbrev_x) +
        labs(x="Warning Condition", y="Average Memory Score %", fill="Description Conditions", subtitle="Score Subset: Fixed Items")+
        coord_cartesian(ylim=c(0,100))+
        scale_fill_grey()+
        theme_bw()+
        theme(aspect.ratio =1.25)+
        theme(legend.position = "none")
      dev.off()
      
      #Panel b: Experimental Facts
      tiff("Figure2b.tiff", units="in", width=5.03, height=3.82, res=300)
      ggplot(ttesum, aes(order, memcritavg, fill=info.f))+
        geom_bar(stat="identity")+
        geom_errorbar(aes(ymin=lower.ci.crit, ymax=upper.ci.crit), width=.1)+
        scale_x_continuous(breaks = seq(1,6,by=1), labels = abbrev_x) +
        labs(x="Warning Condition", y="Average Memory Score %", fill="Description Conditions", subtitle="Score Subset: Experimental Items")+
        coord_cartesian(ylim=c(0,100))+
        scale_fill_grey()+
        theme_bw()+
        theme(aspect.ratio =1.25)
         dev.off()
      
    #Figure 3: Average Misinformation Score by Condition, Experimental Subset
         tiff("Figure3.tiff", units="in", width=5.37, height=3.82, res=300)
      ggplot(ttesum, aes(order, memmisinfoavg, fill=info.f))+
        geom_bar(stat="identity")+
        geom_errorbar(aes(ymin=lower.ci.misinfo, ymax=upper.ci.misinfo), width=.1)+
        scale_x_continuous(breaks = seq(1,6,by=1), labels = abbrev_x) +
        labs(x="Warning Condition", y="Average Misinformation Score %", fill="Description Conditions", subtitle="Score Subset: Experimental Items")+
        coord_cartesian(ylim=c(0,100))+
        scale_fill_grey()+
        theme_bw()+
        theme(aspect.ratio =1.25)
      dev.off()
      
    #Table 1, Model1: Memory Score, experimental subset 
      m1<-lm(memcrit~warnmain  + misinformed + informed + warned_misinformed + warned_informed, data=ttesub)
    
    #Table 1, Model2: Memory Score, fixed subset 
      m2<-lm(membuf~warnmain  + misinformed + informed + warned_misinformed + warned_informed, data=ttesub)
      
    #Table 1, Model3: Misinformation Score, experimental subset 
      m3<-lm(memmisinfo~warnmain  + misinformed + informed + warned_misinformed + warned_informed, data=ttesub)
    
    #TABLE 1 OUTPUT (latex code and text version so you can check it here)      
        stargazer(m1, m2, m3, title="The Effects of  Information and Warning on Correct and Incorrect Memories",
                  align=TRUE, dep.var.labels=c("Memory Score: exp.", "Memory Score: fixed", "Misinfo. Score: exp."),
                  covariate.labels=c("Warning", "Misinformation", "Information", "Misinformation X Warning", "Information X Warning"),
                  star.cutoffs = c(0.05, 0.01, 0.001), digits = 2,
                  omit.stat = c("ser", "f"))
        
        stargazer(m1, m2, m3, title="The Effects of  Information and Warning on Correct and Incorrect Memories",
                  align=TRUE, dep.var.labels=c("Memory Score: exp.", "Memory Score: fixed", "Misinfo. Score: exp."),
                  covariate.labels=c("Warning", "Misinformation", "Information", "Misinformation X Warning", "Information X Warning"),
                  star.cutoffs = c(0.05, 0.01, 0.001),digits = 2,
                  omit.stat = c("ser", "f"), type= "text")
    
    #Marginal Effects of Warning corresponding to Table1:M1 (memory score, exp.) 
      a<-confint(glht(m1, linfct = c("warnmain + warned_informed = 0")))
      a2<-summary(glht(m1, linfct = c("warnmain + warned_informed = 0")))
      b<-confint(glht(m1, linfct = c("warnmain  = 0")))
      b2<-summary(glht(m1, linfct = c("warnmain  = 0")))
      c<-confint(glht(m1, linfct = c("warnmain + warned_misinformed = 0")))
      c2<-summary(glht(m1, linfct = c("warnmain + warned_misinformed = 0")))
      
      newframe<-data.frame(info=factor(c(1,2,3), labels=c("information", "no information", "misinformation")), est=c(a$confint[1,1], b$confint[1,1], c$confint[1,1]), lwr=c(a$confint[1,2], b$confint[1,2], c$confint[1,2]), upr=c(a$confint[1,3], b$confint[1,3], c$confint[1,3]), pvalue=c(a2$test$pvalues[1], b2$test$pvalues[1], c2$test$pvalues[1]))
      newframe 
      newframe$fact=1
      contrast1<-newframe
      contrast1
    
    #Examining if the marginal effects of warning significantly differ between the Misinformation and Information Conditions
      #Note: We already know if they are significantly different from the Control condition by the interaction terms' significance levels in the full model.
      summary(glht(m1, linfct = c("warned_informed - warned_misinformed = 0")))
      
    #Marginal Effects of Warning corresponding to Table1:M2 (memory score, fixed)
      a<-confint(glht(m2, linfct = c("warnmain + warned_informed = 0")))
      a2<-summary(glht(m2, linfct = c("warnmain + warned_informed = 0")))
      b<-confint(glht(m2, linfct = c("warnmain  = 0")))
      b2<-summary(glht(m2, linfct = c("warnmain  = 0")))
      c<-confint(glht(m2, linfct = c("warnmain + warned_misinformed = 0")))
      c2<-summary(glht(m2, linfct = c("warnmain + warned_misinformed = 0")))
      
      newframe<-data.frame(info=factor(c(1,2,3), labels=c("information", "no information", "misinformation")), est=c(a$confint[1,1], b$confint[1,1], c$confint[1,1]), lwr=c(a$confint[1,2], b$confint[1,2], c$confint[1,2]), upr=c(a$confint[1,3], b$confint[1,3], c$confint[1,3]), pvalue=c(a2$test$pvalues[1], b2$test$pvalues[1], c2$test$pvalues[1]))
      newframe 
      newframe$fact=2
      contrast2<-newframe
      contrast2
      
      #Examining if the marginal effects of warning significantly differ between the Misinformation and Information Conditions
      #Note: We already know if they are significantly different from the Control condition by the interaction terms' significance levels in the full model.
      #We do not expect to see a difference between the misinformation and information memory for the fixed fact subset.
      summary(glht(m2, linfct = c("warned_informed - warned_misinformed = 0")))
      
    #Marginal Effects of Warning corresponding to Table1:M3 (misinfo score, exp.)
      a<-confint(glht(m3, linfct = c("warnmain + warned_informed = 0")))
      a2<-summary(glht(m3, linfct = c("warnmain + warned_informed = 0")))
      b<-confint(glht(m3, linfct = c("warnmain  = 0")))
      b2<-summary(glht(m3, linfct = c("warnmain  = 0")))
      c<-confint(glht(m3, linfct = c("warnmain + warned_misinformed = 0")))
      c2<-summary(glht(m3, linfct = c("warnmain + warned_misinformed = 0")))
      
      newframe<-data.frame(info=factor(c(1,2,3), labels=c("information", "no information", "misinformation")), est=c(a$confint[1,1], b$confint[1,1], c$confint[1,1]), lwr=c(a$confint[1,2], b$confint[1,2], c$confint[1,2]), upr=c(a$confint[1,3], b$confint[1,3], c$confint[1,3]), pvalue=c(a2$test$pvalues[1], b2$test$pvalues[1], c2$test$pvalues[1]))
      newframe 
      newframe$fact=3
      contrast3<-newframe
      contrast3
      
      #Examining if the marginal effects of warning significantly differ between the Misinformation and Information Conditions
      #Note: We already know if they are significantly different from the Control condition by the interaction terms' significance levels in the full model.
      summary(glht(m3, linfct = c("warned_informed - warned_misinformed = 0")))
      
    #Figure 4: Marginal Effect of Warning on Memory by Description Condition
      contrast<-bind_rows(contrast1, contrast2, contrast3)
      contrast$fact.f<-factor(contrast$fact, labels=c("MemoryScore:exp", "MemoryScore:fixed", "Misinfo.Score:exp"))
      contrast
      contrast$info2<-car::Recode(as.numeric(contrast$info), "1=3; 3=1")
      contrast$info2<-factor(contrast$info2, labels = c("misinfo", "ctrl", "info"))
      contrast$sig<-factor(ifelse(contrast$pvalue<0.1, c(1), c(0)), labels = c("pvalue>0.10","pvalue<0.10"))
      contrast$order<-c(1:9)
      contrast$validwarning<-0
      contrast$validwarning[contrast$order==3 |contrast$order==9  ]<-1
      contrast$validwarning[contrast$order==1 |contrast$order== 4|contrast$order== 6]<-2
      contrast$validwarning.f<-factor(contrast$validwarning, labels=c("NA/Control", "Valid", "Invalid"))
      contrast
      
      tiff("Figure4.tiff", units="in", width=5.37, height=3.82, res=300)
      ggplot(contrast, aes(info2, est, shape=validwarning.f, color=validwarning.f))+
        geom_point(size=3)+
        geom_linerange(aes(ymin=lwr, ymax=upr))+
        geom_hline(yintercept = 0, color="red")+
        facet_wrap(~fact.f)+
        labs(x="Description Conditions", y="Marginal Effect of Warning (%)", colour="Warning Validity", shape="Warning Validity" ) +
        theme_bw() +
        theme(aspect.ratio = 2.1)
      dev.off()
  
  ###    
  # Memory Uncertainty 
  ###
      
      #Figure 5: Average Memory Uncertainty Score by Condition
      #Calculate means
      ttesum2<-ttesub %>%
        group_by(cond.f)  %>%
        summarize(dkcritavg=mean(dkcrit, na.rm=T), 
                  dkcritsd=sd(dkcrit, na.rm=T),
                  dkbufavg=round( mean(dkbuf, na.rm=T), 3),
                  dkbufsd=round( sd(dkbuf), 3),
                  n=n(),
                  dkcritse=dkcritsd/sqrt(n),
                  lower.ci.crit = dkcritavg - qt(1 - (0.05 / 2), n - 1) * dkcritse,
                  upper.ci.crit = dkcritavg + qt(1 - (0.05 / 2), n - 1) * dkcritse,
                  dkbufse=dkbufsd/sqrt(n),
                  lower.ci.buf = dkbufavg - qt(1 - (0.05 / 2), n - 1) * dkbufse,
                  upper.ci.buf = dkbufavg + qt(1 - (0.05 / 2), n - 1) * dkbufse)
      
      
        ttesum2
        ttesum2$order<-c(1:6)
        ttesum2$info<-ifelse(ttesum2$order==1:2, c(1), ifelse(ttesum2$order==3:4, c(2), c(3)))
        ttesum2
        ttesum2$info.f<-factor(ttesum2$info, labels=c("Misinformation", "Control", "Information"))
      
        #labels for table x axis
        abbrev_x <- c("No", "Yes","No", "Yes","No", "Yes")
     
      
        #Figure 5: Average Memory Uncertainty Score by Condition, Panel A: Fixed Subset
        tiff("Figure5a.tiff", units="in", width=3.53, height=3.82, res=300)
        ggplot(ttesum2, aes(order, dkbufavg, fill=info.f))+
        geom_bar(stat="identity")+
        geom_errorbar(aes(ymin=lower.ci.buf, ymax=upper.ci.buf), width=.1)+
        scale_x_continuous(breaks = seq(1,6,by=1), labels = abbrev_x) +
        labs(x="Warning Condition", y="Average Uncertainty Score %", fill="Description Conditions", subtitle="Score Subset: Fixed Items")+
        coord_cartesian(ylim=c(0,100))+
        scale_fill_grey()+
        theme_bw()+
        theme(aspect.ratio =1.25)+
        theme(legend.position = "none")
        dev.off()
      
        #Figure 5: Average Memory Uncertainty Score by Condition, Panel B: Experimental Subset
        tiff("Figure5b.tiff", units="in", width=5.37, height=3.82, res=300)
        ggplot(ttesum2, aes(order, dkcritavg, fill=info.f))+
          geom_bar(stat="identity")+
          geom_errorbar(aes(ymin=lower.ci.crit, ymax=upper.ci.crit), width=.1)+
          scale_x_continuous(breaks = seq(1,6,by=1), labels = abbrev_x) +
          labs(x="Warning Condition", y="Average Uncertainty Score %", fill="Description Conditions", subtitle="Score Subset: Experimental Items")+
          coord_cartesian(ylim=c(0,100))+
          scale_fill_grey()+
          theme_bw()+
          theme(aspect.ratio =1.25)
        dev.off()
        
      #Table 2: Effects of Information and Warning on Memory Uncertainty
      
        #Table 2, Model 1: experimental facts subset
        m4<-lm(dkcrit~warnmain  + misinformed + informed + warned_misinformed + warned_informed, data=ttesub)
        
        #Table 2, Model 2: fixed facts subset
        m5<-lm(dkbuf~warnmain  + misinformed + informed + warned_misinformed + warned_informed, data=ttesub)
      
        #Table 2 Output (latex code and text so you can check it here)
        stargazer(m4, m5, title="The Effects of  Information and Warning on Memory Uncertainty",
                  align=TRUE, dep.var.labels=c("Uncertainty Score: exp.", "Uncertainty Score: fixed"),
                  covariate.labels=c("Warning", "Misinformation", "Information", "Misinformation X Warning", "Information X Warning"),
                  star.cutoffs = c(0.05, 0.01, 0.001), digits = 2,
                  omit.stat = c("ser", "f"))
        
        
        stargazer(m4, m5, title="The Effects of  Information and Warning on Memory Uncertainty",
                  align=TRUE, dep.var.labels=c("Uncertainty Score: exp.", "Uncertainty Score: fixed"),
                  covariate.labels=c("Warning", "Misinformation", "Information", "Misinformation X Warning", "Information X Warning"),
                  star.cutoffs = c(0.05, 0.01, 0.001), digits = 2,
                  omit.stat = c("ser", "f"), type="text")
      
      
        #Marginal Effects of Warning corresponding to Table2:M1 (uncertainty score, exp.)
        a<-confint(glht(m4, linfct = c("warnmain + warned_informed = 0")))
        a2<-summary(glht(m4, linfct = c("warnmain + warned_informed = 0")))
        b<-confint(glht(m4, linfct = c("warnmain  = 0")))
        b2<-summary(glht(m4, linfct = c("warnmain  = 0")))
        c<-confint(glht(m4, linfct = c("warnmain + warned_misinformed = 0")))
        c2<-summary(glht(m4, linfct = c("warnmain + warned_misinformed = 0")))
        
        newframe<-data.frame(info=factor(c(1,2,3), labels=c("information", "no information", "misinformation")), est=c(a$confint[1,1], b$confint[1,1], c$confint[1,1]), lwr=c(a$confint[1,2], b$confint[1,2], c$confint[1,2]), upr=c(a$confint[1,3], b$confint[1,3], c$confint[1,3]), pvalue=c(a2$test$pvalues[1], b2$test$pvalues[1], c2$test$pvalues[1]))
        newframe 
        newframe$fact=1
        contrast1c<-newframe
        contrast1c
        
        #Examining if the marginal effects of warning significantly differ between the Misinformation and Information Conditions
        #Note: We already know if they are significantly different from the Control condition by the interaction terms' significance levels in the full model.
        summary(glht(m4, linfct = c("warned_informed - warned_misinformed = 0")))
        
        #Marginal Effects of Warning corresponding to Table2:M2 (uncertainty score, fixed)
        a<-confint(glht(m5, linfct = c("warnmain + warned_informed = 0")))
        a2<-summary(glht(m5, linfct = c("warnmain + warned_informed = 0")))
        b<-confint(glht(m5, linfct = c("warnmain  = 0")))
        b2<-summary(glht(m5, linfct = c("warnmain  = 0")))
        c<-confint(glht(m5, linfct = c("warnmain + warned_misinformed = 0")))
        c2<-summary(glht(m5, linfct = c("warnmain + warned_misinformed = 0")))
        
        newframe<-data.frame(info=factor(c(1,2,3), labels=c("information", "no information", "misinformation")), est=c(a$confint[1,1], b$confint[1,1], c$confint[1,1]), lwr=c(a$confint[1,2], b$confint[1,2], c$confint[1,2]), upr=c(a$confint[1,3], b$confint[1,3], c$confint[1,3]), pvalue=c(a2$test$pvalues[1], b2$test$pvalues[1], c2$test$pvalues[1]))
        newframe 
        newframe$fact=2
        contrast2c<-newframe
        contrast2c
        
        #Examining if the marginal effects of warning significantly differ between the Misinformation and Information Conditions
        #Note: We already know if they are significantly different from the Control condition by the interaction terms' significance levels in the full model.
        summary(glht(m5, linfct = c("warned_informed - warned_misinformed = 0")))
        
        contrastc<-bind_rows(contrast1c, contrast2c)
        contrastc$fact.f<-factor(contrastc$fact, labels=c("Uncertainty Score: exp.", "Uncertainty Score: fixed"))
        contrastc
        contrastc$info2<-car::Recode(as.numeric(contrastc$info), "1=3; 3=1")
        contrastc$info2<-factor(contrastc$info2, labels = c("misinfo", "ctrl", "info"))
        contrastc$sig<-factor(ifelse(contrastc$pvalue<0.1, c(1), c(0)), labels = c("pvalue>0.10","pvalue<0.10"))
        contrastc$order<-c(1:6)
        contrastc$validwarning<-0
        contrastc$validwarning[contrastc$order==3 ]<-1
        contrastc$validwarning[contrastc$order==1 |contrastc$order== 4|contrastc$order== 6]<-2
        contrastc$validwarning.f<-factor(contrastc$validwarning, labels=c("NA/Control", "Valid", "Invalid"))
        contrastc
        
        #Figure 6: Marginal Effect of Warning for Memory Uncertainty by Description Condition
        tiff("Figure6.tiff", units="in", width=6.23, height=3.82, res=300)
        ggplot(contrastc, aes(info2, est, shape=validwarning.f, color=validwarning.f))+
          geom_point(size=3)+
          geom_linerange(aes(ymin=lwr, ymax=upr))+
          geom_hline(yintercept = 0, color="red")+
          facet_wrap(~fact.f)+
          labs(x="Description Conditions", y="Marginal Effect of Warning (%)", colour="Warning Validity", shape="Warning Validity" ) +
          theme_bw() +
          theme(aspect.ratio = 2.1)
        dev.off()
        

###
# Credibility
###
        
    #Figure 7: Average Credibility of Original Event (Video) and Post-event Description (Article) by Condition
        ttesum<-ttesub %>%
          group_by(cond.f)  %>%
          summarize(credvid_cleanavg=mean(credvid_clean, na.rm=T), 
                    credvid_cleansd=sd(credvid_clean, na.rm=T),
                    crednews_cleanavg=round( mean(crednews_clean, na.rm=T), 3),
                    crednews_cleansd=round( sd(crednews_clean, na.rm=T), 3),
                    n=n(),
                    credvid_cleanse=credvid_cleansd/sqrt(n),
                    lower.ci.crit = credvid_cleanavg - qt(1 - (0.05 / 2), n - 1) * credvid_cleanse,
                    upper.ci.crit = credvid_cleanavg + qt(1 - (0.05 / 2), n - 1) * credvid_cleanse,
                    crednews_cleanse=crednews_cleansd/sqrt(n),
                    lower.ci.buf = crednews_cleanavg - qt(1 - (0.05 / 2), n - 1) * crednews_cleanse,
                    upper.ci.buf = crednews_cleanavg + qt(1 - (0.05 / 2), n - 1) * crednews_cleanse)
        
        ttesum
        ttesum$order<-c(1:6)
        ttesum$info<-ifelse(ttesum$order==1:2, c(1), ifelse(ttesum$order==3:4, c(2), c(3)))
        ttesum
        ttesum$info.f<-factor(ttesum$info, labels=c("Misinformation", "Control", "Information"))
        
        abbrev_x <- c("No", "Yes","No", "Yes","No", "Yes")
        #Figure 7: Average Credibility of Original Event (Video) and Post-event Description (Article) by Condition, Panel A: Original Event
        tiff("Figure7a.tiff", units="in", width=3.47, height=3.82, res=300)
        ggplot(ttesum, aes(order, credvid_cleanavg, fill=info.f))+
          geom_bar(stat="identity")+
          geom_errorbar(aes(ymin=lower.ci.crit, ymax=upper.ci.crit), width=.1)+
          scale_x_continuous(breaks = seq(1,6,by=1), labels = abbrev_x) +
          labs(x="Warning Condition", y="Average Video Credibility", fill="Description Conditions")+
          coord_cartesian(ylim=c(1,5))+
          scale_fill_grey()+
          theme_bw()+
          theme(aspect.ratio =1.25)+
          theme(legend.position = "none")
        dev.off()
        
        #Figure 7: Average Credibility of Original Event (Video) and Post-event Description (Article) by Condition, Panel B: Post-Decription
        tiff("Figure7b.tiff", units="in", width=5.08, height=3.82, res=300)
        ggplot(ttesum, aes(order, crednews_cleanavg, fill=info.f))+
          geom_bar(stat="identity")+
          geom_errorbar(aes(ymin=lower.ci.buf, ymax=upper.ci.buf), width=.1)+
          scale_x_continuous(breaks = seq(1,6,by=1), labels = abbrev_x) +
          labs(x="Warning Condition", y="Average News Article Credibility", fill="Description Conditions")+
          coord_cartesian(ylim=c(1,5))+
          scale_fill_grey()+
          theme_bw()+
          theme(aspect.ratio =1.25)
        dev.off()
        
  #Table 3: Credibility
        #Table 3: Model 1 Video Credibility
        m6<-lm(credvid_clean~warnmain  + misinformed + informed + warned_misinformed + warned_informed, data=ttesub)
        
        #Table 3: Model 2 Article Credibility
        m7<-lm(crednews_clean~warnmain  + misinformed + informed + warned_misinformed + warned_informed, data=ttesub)
        
        #Table 3 Output: Credibility (latex code and text to view output easily here)
        stargazer(m6, m7, title="The Effects of Information and Warning on Original Event and Post-event Description Credibility",
                  align=TRUE, dep.var.labels=c("Video", "Article"),
                  covariate.labels=c("Warning", "Misinformation", "Information", "Misinformation X Warning", "Information X Warning"),
                  star.cutoffs = c(0.05, 0.01, 0.001), digits = 2,
                  omit.stat = c("ser", "f"))
        
        stargazer(m6, m7, title="The Effects of Information and Warning on Original Event and Post-event Description Credibility",
                  align=TRUE, dep.var.labels=c("Video", "Article"),
                  covariate.labels=c("Warning", "Misinformation", "Information", "Misinformation X Warning", "Information X Warning"),
                  star.cutoffs = c(0.05, 0.01, 0.001),
                  omit.stat = c("ser", "f"), type= "text")
        
        #for Figure 8: Marginal Effects of Warning on Video  Credibility
          a<-confint(glht(m6, linfct = c("warnmain + warned_informed = 0")))
          a2<-summary(glht(m6, linfct = c("warnmain + warned_informed = 0")))
          b<-confint(glht(m6, linfct = c("warnmain  = 0")))
          b2<-summary(glht(m6, linfct = c("warnmain  = 0")))
          c<-confint(glht(m6, linfct = c("warnmain + warned_misinformed = 0")))
          c2<-summary(glht(m6, linfct = c("warnmain + warned_misinformed = 0")))
          
          newframe<-data.frame(info=factor(c(1,2,3), labels=c("information", "no information", "misinformation")), est=c(a$confint[1,1], b$confint[1,1], c$confint[1,1]), lwr=c(a$confint[1,2], b$confint[1,2], c$confint[1,2]), upr=c(a$confint[1,3], b$confint[1,3], c$confint[1,3]), pvalue=c(a2$test$pvalues[1], b2$test$pvalues[1], c2$test$pvalues[1]))
          newframe 
          newframe$fact<-1
          contrast1b<-newframe
          contrast1b
          
          #Examining if the marginal effects of warning significantly differ between the Misinformation and Information Conditions
          #Note: We already know if they are significantly different from the Control condition by the interaction terms' significance levels in the full model.
          summary(glht(m6, linfct = c("warned_informed - warned_misinformed = 0")))
          
        #for Figure 8: Marginal Effects of Warning on Article  Credibility
          a<-confint(glht(m7, linfct = c("warnmain + warned_informed = 0")))
          a2<-summary(glht(m7, linfct = c("warnmain + warned_informed = 0")))
          b<-confint(glht(m7, linfct = c("warnmain  = 0")))
          b2<-summary(glht(m7, linfct = c("warnmain  = 0")))
          c<-confint(glht(m7, linfct = c("warnmain + warned_misinformed = 0")))
          c2<-summary(glht(m7, linfct = c("warnmain + warned_misinformed = 0")))
          
          newframe<-data.frame(info=factor(c(1,2,3), labels=c("information", "no information", "misinformation")), est=c(a$confint[1,1], b$confint[1,1], c$confint[1,1]), lwr=c(a$confint[1,2], b$confint[1,2], c$confint[1,2]), upr=c(a$confint[1,3], b$confint[1,3], c$confint[1,3]), pvalue=c(a2$test$pvalues[1], b2$test$pvalues[1], c2$test$pvalues[1]))
          newframe 
          newframe$fact<-2
          contrast2b<-newframe
          contrast2b
          
          #Examining if the marginal effects of warning significantly differ between the Misinformation and Information Conditions
          #Note: We already know if they are significantly different from the Control condition by the interaction terms' significance levels in the full model.
          summary(glht(m7, linfct = c("warned_informed - warned_misinformed = 0")))
          
          contrastb<-bind_rows(contrast1b, contrast2b)
          contrastb$fact.f<-factor(contrastb$fact, labels=c("Video Credibility", "Article Credibility"))
          contrastb
          contrastb$info2<-car::Recode(as.numeric(contrastb$info), "1=3; 3=1")
          contrastb$info2<-factor(contrastb$info2, labels = c("misinfo", "ctrl", "info"))
          contrastb$sig<-factor(ifelse(contrastb$pvalue<0.1, c(1), c(0)), labels = c("pvalue>0.10","pvalue<0.10"))
          contrastb$order<-c(1:6)
          contrastb$validwarning<-0
          contrastb$validwarning[contrastb$order==3 |contrastb$order==6  ]<-1
          contrastb$validwarning[contrastb$order==1 |contrastb$order== 4]<-2
          contrastb$validwarning.f<-factor(contrastb$validwarning, labels=c("NA/Control", "Valid", "Invalid"))
          contrastb
          
          #produce Figure 8 Marginal Effects of Warning on Source Credibility by Description Conditions
          tiff("Figure8.tiff", units="in", width=6.23, height=3.82, res=300)
          ggplot(contrastb, aes(info2, est, shape=validwarning.f, color=validwarning.f))+
            geom_point(size=3)+
            geom_linerange(aes(ymin=lwr, ymax=upr))+
            geom_hline(yintercept = 0, color="red")+
            facet_wrap(~fact.f)+
            labs(x="Description Conditions", y="Marginal Effect of Warning (5-pt scale)", colour="Warning Validity", shape="Warning Validity" ) +
            theme_bw() +
            theme(aspect.ratio = 1.2)
          dev.off()
          
          
          #Appendix tables calculated in Stata see "cleaningTTEdata.do"