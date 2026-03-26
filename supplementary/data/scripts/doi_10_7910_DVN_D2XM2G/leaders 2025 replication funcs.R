qu <- function (x,p=.025) {
  return(quantile(x,probs=p,na.rm=T))
}

r<-function(x, dig=2) { return (round(x,dig)) }

ggs <- function (filename,plottype="Density",width=8,height=5,types=c("png","pdf"),save=T,subfolder=F,dropbox=NA) {
  
  if (save) {
    for (filetype in types) {
      #dpi=ifelse(filetype=="png",72,300)
      if (subfolder) { folderpath = str_c(filetype,"/") } else { folderpath="" }
      
      subdir = str_c("Plots/",toupper(folderpath))
      
      ifelse(!dir.exists(subdir), dir.create(subdir), FALSE)
      
      subdir = str_c(subdir,plottype,"/")
      
      ifelse(!dir.exists(subdir), dir.create(subdir), FALSE)
      
      filen = str_c(subdir,filename,".",filetype)
      
      print(filen)
      
      ggsave(file=filen, width=width, height=height) #,dpi=dpi)
      
      if (!is.na(dropbox)) {
        filen = str_c("../../../Dropbox/",dropbox,"/Plots/",folderpath,filename,".",filetype)
        print(filen)
        ggsave(file=filen, width=width, height=height) #,dpi=dpi)
      }
    }    
  }
  
}

do_descriptive_plots <- function() {
  
  load("Objects/leaderz 1989-2023.Rdata")
  cols = c("blue4","red4"); 
  
  library(lubridate)
  library(dplyr)
  
  # histograms
  
  leader.hist = topleaderz %>%
    group_by(party) %>%
    summarize(diff=mean(diff,na.rm=T))
  
  ggplot(topleaderz, aes(x=diff, fill=party)) +
    scale_color_manual(values=cols) +
    scale_fill_manual(values=cols) +
    xlim(-.8,.8) +
    geom_vline(data=leader.hist, aes(xintercept = diff, color=party), linetype="dashed", linewidth=1) + # from chatgpt 050323
    geom_density(alpha=0.3) +
    theme_bw() + theme(legend.position="none") + labs(x="Distance between Leaders and Caucus",y=NULL)
  
  ggs("overall_diff_party",plottype="Density",subfolder=T,width=10,height=5)
  
  leader.hist = topleaderz %>%
    filter(chamber.leader==1) %>%
    group_by(party,chamber) %>%
    summarize(diff.chamber=mean(diff.chamber,na.rm=T))
  
  ggplot(topleaderz, aes(x=diff.chamber, fill=party)) +
    scale_color_manual(values=cols) +
    scale_fill_manual(values=cols) +
    xlim(-2.5,2.5) +
    geom_density(alpha=0.3,position="identity") +
    geom_vline(data=leader.hist, aes(xintercept = diff.chamber, color=party), linetype="dashed") + # from chatgpt 050323
    geom_text(data=leader.hist,aes(label=round(diff.chamber,2), x=diff.chamber, y=Inf, color=party), size=3, hjust=1.25, vjust=1.9, check_overlap = T) +
    theme_bw() + theme(legend.position="none") + labs(x="Distance between Leaders and Chambers",y=NULL) +
    facet_wrap(~chamber, scales="free_x")
  
  ggs("overall_diff_chamber",plottype="Density",subfolder=T,width=10,height=5)
  
  leader.summary = topleaderz %>%
    filter(year>=yearz[1]) %>%
    mutate(year=make_date(year)) %>%
    group_by(party,year) %>%
    summarize(p10=qu(pred.np,p=.10),
              p90=qu(pred.np,p=.90),
              pred.np=mean(pred.np,na.rm=T),
              median=mean(median,na.rm=T),
              diff=mean(diff,na.rm=T),
              chamb.median=mean(chamb.median,na.rm=T),
              diff.chamber=mean(diff.chamber,na.rm=T)) %>%
    mutate(ymin=ifelse(party=="D",-1.75,0),
           ymax=ifelse(party=="D",0,1.75))
  
  ggplot(leader.summary, aes(x=year)) + 
    scale_x_date(date_breaks="4 years", date_labels="%Y") +
    geom_line(aes(y=pred.np, color=party)) + 
    geom_line(aes(y=median, color=party),linetype="dashed") + 
    geom_ribbon(aes(x=year,ymin=p10,ymax=p90, group=party, fill=party),alpha=.3) +
    scale_color_manual(values=cols) +
    scale_fill_manual(values=cols) +
    theme_bw() + theme(legend.position="none") + labs(y="Leader and Caucus Position",x=NULL) +
    facet_wrap(~party, scales="free_y") +
    geom_blank(aes(y=ymin)) +
    geom_blank(aes(y=ymax))
  
  ggs("leadership_party_variance",plottype="Trends",subfolder=T,width=10,height=5)
  
  
  leader.summary = topleaderz %>%
    filter(year>=yearz[1]) %>%
    mutate(year=make_date(year), leadertype=str_to_title(leadertype)) %>%
    group_by(party,leadertype,chamber,year) %>%
    summarize(p10=qu(pred.np,p=.10),
              p90=qu(pred.np,p=.90),
              pred.np=mean(pred.np,na.rm=T),
              median=mean(median,na.rm=T),
              diff=mean(diff,na.rm=T),
              chamb.median=mean(chamb.median,na.rm=T),
              diff.chamber=mean(diff.chamber,na.rm=T))
  
  cols = c("blue4","red4")
  
  # draw smoothed lines for each party
  
  ggplot(leader.summary, aes(x=year, y=diff, color=party)) + 
    scale_x_date(date_breaks="4 years", date_labels="%Y") +
    #geom_line(aes(y=diff, color=party)) + 
    ylim(c(-.10,.10)) +
    scale_color_manual(values=cols) +
    theme_bw() + labs(y="Difference between Leader and Caucus",x=NULL) +
    geom_hline(yintercept = 0, linetype="dashed") +
    geom_smooth(method='loess',se=F) +
    theme(legend.title=element_blank()) +
    facet_wrap(~chamber+leadertype)
  
  
  ggs("leadership_diff_party_chamber_type",plottype="Trends",subfolder=T,width=10,height=10)
  
  # states, averaging chambers, differences
  
  cols = c("blue4","red4")
  
  library(plyr)
  leader.summary.noch = ddply(topleaderz,.(year,st,party),summarize,diff=median(diff,na.rm=T))
  leader.m.noch = melt(leader.summary.noch,id=c("party","st","year"),c("diff"))
  leader.m.noch$variable=car::recode(leader.m.noch$variable,"'diff'='Difference'")
  leader.m.noch$variable=as.character(leader.m.noch$variable)
  leader.m.noch$party.type = str_c(leader.m.noch$party,"-",str_sub(leader.m.noch$variable,1,1))
  leader.m.noch$year = make_date(leader.m.noch$year)
  detach(package:plyr)
  
  ggplot(leader.m.noch, aes(x=year, y=value, color=party, linetype=variable, group = party.type)) + 
    geom_line() + #+  geom_point(size=1) +
    #geom_smooth(method='loess',se=F) +
    theme(axis.text.x=element_text(angle=45, size=6))  +  scale_color_manual(values=cols) + 
    scale_x_date(date_breaks="8 years", date_labels="%Y") +
    scale_y_continuous(breaks=c(-1.0:1.0)) +
    theme_bw()+ theme(legend.position = "none") + labs(y="Difference between Leader and Caucus Ideology",x=NULL) +
    theme(strip.text.x = element_text(size = 10, face="bold")) +
    facet_wrap(~st, ncol=5)
  
  ggs("leadership_diff_party_state",plottype="Trends",subfolder=T,width=10,height=11)
  
  
}

gen_averages <- function() {
  # calculate averages
  
  library(plyr)
  overall=plyr::ddply(topleaderz,.(party,chamber),summarize,
                      pred.np=round(mean(pred.np,na.rm=T),3),
                      median=round(mean(median,na.rm=T),3),
                      diff=round(mean(diff,na.rm=T),3),
                      abs.diff = round(mean(abs,na.rm=T),3),
                      sd.diff = round(sd(abs,na.rm=T),3),
                      diff.chamber=round(mean(diff.chamber,na.rm=T),3),
                      #count.r = mean(pred.np>median),
                      n=length(party))
  overall$type = str_c(tolower(str_sub(overall$chamber,1,1)),".",tolower(overall$party))
  overall
  
  prop.table(table(leaders[["h.r"]]$diff>=0)) 
  prop.table(table(leaders[["h.d"]]$diff<=0)) 
  prop.table(table(leaders[["s.r"]]$diff>=0)) 
  prop.table(table(leaders[["s.d"]]$diff<=0)) 
  
  overall.p=plyr::ddply(subset(topleaderz,leadertype=="party"),.(party,chamber),summarize,
                        pred.np=round(mean(pred.np,na.rm=T),3),
                        median=round(mean(median,na.rm=T),3),
                        diff=round(mean(diff,na.rm=T),3),
                        abs.diff = round(mean(abs,na.rm=T),3),
                        diff.chamber=round(mean(diff.chamber,na.rm=T),3),
                        n=length(party))
  
  overall.p$type = str_c(tolower(str_sub(overall.p$chamber,1,1)),".",tolower(overall.p$party),".p")
  overall.p
  
  overall.c=plyr::ddply(subset(topleaderz,leadertype=="chamber"),.(party,chamber),summarize,
                        pred.np=round(mean(pred.np,na.rm=T),3),
                        median=round(mean(median,na.rm=T),3),
                        diff=round(mean(diff,na.rm=T),3),
                        abs.diff = round(mean(abs,na.rm=T),3),
                        diff.chamber=round(mean(diff.chamber,na.rm=T),3),
                        n=length(party))
  
  overall.c$type = str_c(tolower(str_sub(overall.c$chamber,1,1)),".",tolower(overall.c$party),".c")
  overall.c
  
  save(overall,overall.c,overall.p,file="Objects/overall averages.Rdata")
  
  detach(package:plyr)  
}

gen_tables <- function () {
  
  load(file="Objects/overall averages.Rdata")
  
  #fs::file_info("Objects/pvalues absolute 5000.Rdata")
  load("Objects/pvalues absolute 5000.Rdata")
  
  #fs::file_info("Objects/pvalues directional 5000.Rdata")
  load("Objects/pvalues directional 5000.Rdata")
  
  # generate tables
  
  library(xtable)
  
  overall.out <- overall %>% select(party, chamber,diff,abs.diff,sd.diff)
  names(overall.out)=c("Party","Chamber","Mean Distance","Abs Distance","SD Distance")
  xout <- xtable(overall.out,caption="All Legislative Leaders and their Caucuses",label="overall",digits=3)
  print(xout,file="Tables/overall.tex")
  
  overall.out <- overall %>% select(party, chamber,diff,abs.diff)
  names(overall.out)=c("Party","Chamber","Mean Distance","Absolute Distance")
  xout <- xtable(overall.out,caption="All Legislative Leaders and their Caucuses",label="overall",digits=3)
  print(xout,file="Tables/overall_simple.tex")
  
  
  overall.out <- overall.p %>% select(party, chamber,diff,abs.diff)
  names(overall.out)=c("Party","Chamber","Mean Distance","Abs Distance")
  xout <- xtable(overall.out,caption="Party Leaders and their Caucuses",label="overall_p",digits=3)
  print(xout,file="Tables/overall_p.tex")

  overall.out <- overall.c %>% select(party, chamber,diff,abs.diff)
  names(overall.out)=c("Party","Chamber","Mean Distance","Abs Distance")
  xout<-xtable(overall.out,caption="Chamber Leaders and their Caucuses",label="overall_c",digits=3)
  print(xout,file="Tables/overall_c.tex")
  
  combined.p <- overall.p %>% select(party,chamber,diff)
  combined.c <- overall.c %>% select(diff)
  
  overall.out <- data.frame(combined.p,combined.c)
  names(overall.out)=c("Party","Chamber","Party Leader Distance","Chamber Leader Distance")
  xout<-xtable(overall.out,caption="Ideological distance between leaders and their caucuses",label="combined",digits=3)
  print(xout,file="Tables/combined.tex")
  
  overall.out <- overall.c %>% select(party,chamber,diff.chamber)
  names(overall.out)=c("Party","Chamber","Chamber Leader Distance")
  xout<-xtable(overall.out,caption="Ideological distance between leaders and their chambers",label="overall_chamber",digits=3)
  print(xout,file="Tables/overall_chamber.tex")
  
  
  data.frame(party=overall$party,chamber=overall$chamber,overall$abs.diff,overall.p$abs.diff,overall.c$abs.diff)
  df.d = t(data.frame(overall$abs.diff,overall.p$abs.diff,overall.c$abs.diff))
  colnames(df.d)=c("h.d","s.d","h.r","s.r")
  df.d = data.frame(type=c("All Leaders","Party Leaders","Chamber Leaders"),stat=c("Abs Distance","Abs Distance","Abs Distance"),df.d)
  
  # p value tables
  
  # absolute
  
  data.frame(pv.a=unlist(pv.a),pv.p=unlist(pv.p),pv.c=unlist(pv.c))
  df.pv = t(data.frame(pv.a=unlist(pv.a),pv.p=unlist(pv.p),pv.c=unlist(pv.c)))
  df.pv = data.frame(type=c("All Leaders","Party Leaders","Chamber Leaders"),stat=rep("p-value",3),df.pv)
  
  # directional
  
  # left
  

  df.pv.left = t(data.frame(pv.a=unlist(pv.a.left),
                            pv.p=c(pv.p.left[["h.r.p"]],pv.p.left[["h.d.p"]],pv.p.left[["s.r.p"]],pv.p.left[["s.d.p"]]),
                            pv.c=c(pv.c.left[["h.r.c"]],pv.c.left[["h.d.c"]],pv.c.left[["s.r.c"]],pv.c.left[["s.d.c"]])))
  df.pv.left = data.frame(type=c("All Leaders","Party Leaders","Chamber Leaders"),stat=rep("p-value",3),df.pv.left)
  df.pv.left
  
  # right

  df.pv.right = t(data.frame(pv.a=unlist(pv.a.right),
                             pv.p=c(pv.p.right[["h.r.p"]],pv.p.right[["h.d.p"]],pv.p.right[["s.r.p"]],pv.p.right[["s.d.p"]]),
                             pv.c=c(pv.c.right[["h.r.c"]],pv.c.right[["h.d.c"]],pv.c.right[["s.r.c"]],pv.c.right[["s.d.c"]])))
  
  df.pv.right = data.frame(type=c("All Leaders","Party Leaders","Chamber Leaders"),stat=rep("p-value",3),df.pv.right)
  df.pv.right
  
  # get rid of all leaders
  pv.abs = df.pv[-1,]
  pv.left = df.pv.left[-1,]
  pv.right = df.pv.right[-1,]
  
  for (i in c("pv.abs","pv.left","pv.right")) {
    
    df = get(i)
    
    filename = str_c("pvalues_",str_sub(i,4),".tex")
    print(i)
    print(filename)
    
    strCaption = "Monte Carlo p-values"
    if (str_detect(i,"left")) { print("Left!"); strCaption = str_c(strCaption, " (extreme left hypothesis)")  }
    if (str_detect(i,"right")) { print("Right!"); strCaption = str_c(strCaption, " (extreme right hypothesis)")  }
    
    texlabel = i
    
    require(xtable)
    
    # only needed if first column consists of numbers
    df[[1]] <- as.character(df[[1]])
    
    rle.lengths <- rle(df[[1]])$lengths
    first <- !duplicated(df[[1]])
    df[[1]][!first] <- ""
    
    # define appearance of \multirow
    df[[1]][first] <-
      paste0("\\midrule\\multirow{", rle.lengths, "}{*}{\\textbf{", df[[1]][first], "}}")
    
    # set up xtable output
    sink(str_c("Tables/",filename))
    print(xtable(df, digits = c(0, 0, 0, 3, 3, 3, 3), # first zero "represents" row numbers which we skip later
                 align = "lllrr|rr",  # align and put a vertical line (first "l" again represents column of row numbers)
                 caption = strCaption, label = texlabel),
          size = "normalsize", #Change size; useful for bigger tables "normalsize" "footnotesize"
          include.rownames = FALSE, #Don't print rownames
          include.colnames = FALSE, #We create them ourselves
          caption.placement = "top", #"top", NULL
          hline.after=NULL, #We don't need hline; we use booktabs
          floating=TRUE, # whether \begin{Table} should be created (TRUE) or not (FALSE)
          sanitize.text.function = force, # Important to treat content of first column as latex function
          add.to.row = list(pos = list(-1,
                                       
                                       nrow(df)),
                            command = c(paste("\\toprule \n",  # NEW row
                                              "\\multicolumn{2}{c}{} & \\multicolumn{2}{c}{House} & \\multicolumn{2}{c}{Senate} \\\\\n",
                                              "\\cmidrule(l){3-4} \\cmidrule(l){5-6}\n",
                                              " & & R & D & R & D \\\\\n", # NEW row 
                                              "\\midrule \n"
                            ),
                            paste("\\bottomrule \n"  # paste is used as it is more flexible regarding adding lines
                            )
                            )
          )
    )
    sink()
  }
  
}

leader_sims <- function () {
  
  load(file="Objects/overall averages.Rdata")
  
  #fs::file_info("Objects/pvalues absolute 5000.Rdata")
  load("Objects/pvalues absolute 5000.Rdata")

  #fs::file_info("Objects/pvalues directional 5000.Rdata")
  load("Objects/pvalues directional 5000.Rdata")
  
  #fs::file_info("Objects/leader draws 5000.Rdata")
  load("Objects/leader draws 5000.Rdata")
  
  #####
  
  library(reshape2)
  library(ggplot2)
  

  d.a=melt(draws.a,varnames=c("leader"),value.name="distance")
  names(d.a)[2]="type"
  #d.a$abs = abs(d.a$distance)
  d.a$chamber = str_sub(d.a$type,1,1)
  
  d.p=melt(pdraws.a,varnames=c("leader"),value.name="distance")
  names(d.p)[2]="type"
  
  d.c=melt(cdraws.a,varnames=c("leader"),value.name="distance")
  names(d.c)[2]="type"
  
  
  cols4 = c("red4","blue4","red4","blue4")
  

  ###########
  # via the absolute draws
  ############
  
  facet_label <- c('h.r'='House Rs','h.d'='House Ds','s.r'='Senate Rs','s.d'='Senate Ds')
  
  #cols4 = c("red","blue","red","blue")
  
  ggplot(d.a,aes(x=distance)) + geom_histogram(fill="white", col="gray10") + theme_bw() + #xlim(0.15,0.25)  +
    geom_vline(aes(xintercept = abs.diff, color = cols4), overall) +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title = element_blank(), legend.position="none") + labs(y="Count",x="Absolute Distance to Median") +
    facet_wrap(~type, labeller = as_labeller(facet_label))
  
  ggs("montecarlo_middlep_overall",plottype="Density",subfolder=T,width=7,height=7)
  
  facet_label <- c('h.r.p'='House Rs (Party)','h.d.p'='House Ds (Party)','s.r.p'='Senate Rs (Party)','s.d.p'='Senate Ds (Party)')
  pvals <- data.frame(type=names(pv.p), pval=sprintf("%.3f",as.numeric(pv.p)))
  
  ggplot(d.p,aes(x=distance)) + geom_density(fill="grey", col="gray10") + theme_bw() + #xlim(0.15,0.25)  +
    geom_vline(aes(xintercept = abs.diff), overall.p, linetype="dashed") +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title = element_blank(), legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    labs(y="",x="Absolute Distance to Median") +
    facet_wrap(~type, labeller = as_labeller(facet_label)) +
    geom_text(data=pvals, aes(label=pval,x=(min(d.p$distance)+max(d.p$distance))/2, y=Inf), size=3, hjust=2,vjust=2, check_overlap = T)
  
  ggs("montecarlo_middlep_party",plottype="Density",subfolder=T,width=7,height=7)
  
  facet_label <- c('h.r.c'='House Rs (Chamber)','h.d.c'='House Ds (Chamber)','s.r.c'='Senate Rs (Chamber)','s.d.c'='Senate Ds (Chamber)')
  pvals <- data.frame(facet_label=facet_label, pval=sprintf("%.3f",as.numeric(pv.c)))
  
  ggplot(d.c,aes(x=distance)) + geom_density(fill="grey", col="gray10") + theme_bw() + #xlim(0.15,0.25)  +
    geom_vline(aes(xintercept = abs.diff), overall.c, linetype="dashed") +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title = element_blank(), legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    labs(y="",x="Absolute Distance to Median") +
    facet_wrap(~type, labeller = as_labeller(facet_label)) +
    geom_text(data=pvals, aes(label=pval,x=(min(d.c$distance)+max(d.c$distance))/2, y=Inf), size=3, hjust=1,vjust=2, check_overlap = T)
  
  ggs("montecarlo_middlep_chamber",plottype="Density",subfolder=T,width=7,height=7)
  
  
  
  ###########
  # directional 
  ###########
  
  
  dd.a=melt(draws.d,varnames=c("leader"),value.name="distance")
  names(dd.a)[2]="type"
  dd.a$chamber = str_sub(dd.a$type,1,1)
  
  dd.p=melt(pdraws.d,varnames=c("leader"),value.name="distance")
  names(dd.p)[2]="type"
  
  dd.c=melt(cdraws.d,varnames=c("leader"),value.name="distance")
  names(dd.c)[2]="type"
  
  
  facet_label <- c('h.r'='House Rs','h.d'='House Ds','s.r'='Senate Rs','s.d'='Senate Ds')
  
  ggplot(dd.a,aes(x=distance)) + geom_histogram(fill="white", col="gray10") + theme_bw() + #xlim(0.15,0.25)  +
    geom_vline(aes(xintercept = diff, color = cols4), overall) +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title = element_blank(), legend.position="none") + labs(y="Count",x="Average Signed Distance to Median") +
    facet_wrap(~type, labeller = as_labeller(facet_label))
  
  ggs("montecarlo_direction_overall",plottype="Density",subfolder=T,width=7,height=7)
  
  
  facet_label <- c('h.r.p'='House Rs (Party)','h.d.p'='House Ds (Party)','s.r.p'='Senate Rs (Party)','s.d.p'='Senate Ds (Party)')
  pvals <- data.frame(type=names(pv.p.left),
                      pval.left=sprintf("%.3f",as.numeric(pv.p.left)),
                      pval.right=sprintf("%.3f",as.numeric(pv.p.right))) %>% arrange(type)
  
  ggplot(dd.p,aes(x=distance)) + geom_density(fill="grey", col="gray10") + theme_bw() + #xlim(0.15,0.25)  +
    geom_vline(aes(xintercept = diff), overall.p, linetype="dashed") +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title = element_blank(), legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    labs(y="",x="Signed Distance to Median") +
    facet_wrap(~type, labeller = as_labeller(facet_label)) +
    geom_text(data=pvals, aes(label=pval.right,x=max(dd.p$distance), y=Inf), color="red4", vjust=2, hjust=1, size=3, check_overlap = T) +
    geom_text(data=pvals, aes(label=pval.left,x=min(dd.p$distance), y=Inf), color="blue4", vjust=2, hjust=1.5, size=3, check_overlap = T)
  
  ggs("montecarlo_direction_party",plottype="Density",subfolder=T,width=7,height=7)
  
  facet_label <- c('h.r.c'='House Rs (Chamber)','h.d.c'='House Ds (Chamber)','s.r.c'='Senate Rs (Chamber)','s.d.c'='Senate Ds (Chamber)')
  pvals <- data.frame(type=names(pv.c.left),
                      pval.left=sprintf("%.3f",as.numeric(pv.c.left)),
                      pval.right=sprintf("%.3f",as.numeric(pv.c.right))) %>% arrange(type)
  
  ggplot(dd.c,aes(x=distance)) + geom_density(fill="grey", col="gray10") + theme_bw() + #xlim(0.15,0.25)  +
    geom_vline(aes(xintercept = diff), overall.c, linetype="dashed") +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title = element_blank(), legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    labs(y="",x="Signed Distance to Median") +
    facet_wrap(~type, labeller = as_labeller(facet_label)) +
    geom_text(data=pvals, aes(label=pval.right,x=max(dd.c$distance), y=Inf), color="red4", vjust=2, hjust=1, size=3, check_overlap = T) +
    geom_text(data=pvals, aes(label=pval.left,x=min(dd.c$distance), y=Inf), color="blue4", vjust=2, hjust=0, size=3, check_overlap = T)
  
  ggs("montecarlo_direction_chamber",plottype="Density",subfolder=T,width=7,height=7)
  
  
}

leader_models <- function (dots=T) {

  library(dplyr)
  
  library(ggplot2)
  library(cspp)
  library(tidylog)
  library(dplyr)
  library(tidyr)
  library(stargazer)
  library(modelsummary)
  library(lme4)
  library(ggdist)
  library(marginaleffects)
  library(patchwork)
  
  
  load("Objects/leaderz 1989-2023.Rdata")
  load("Objects/leader model data.Rdata")
  
    
  
  # maj/min directional plus CPG
  
  
  library(fixest)
  
  leader.data %>% group_by(majparty) %>% summarize(cpg.pca=mean(cpg.pca,na.rm=T))
  
  fit.diff.maj.cpg.ols = list()
  
  fit.diff.maj.cpg.ols[["d.maj"]]=feols(diff~leadertype+cpg.pca+party.margin+incumbent+termlim+samepartygov | st+year+k.id,data=subset(leader.data,majority == T & party=="D"))
  fit.diff.maj.cpg.ols[["r.maj"]]=feols(diff~leadertype+cpg.pca+party.margin+incumbent+termlim+samepartygov | st+year+k.id,data=subset(leader.data,majority == T & party=="R"))
  
  modelsummary(fit.diff.maj.cpg.ols,output="markdown",stars=T)

  # This option avoids LaTeX conflicts by creating a simpler table
  options(modelsummary_factory_latex = "kableExtra")
  
  # Define the goodness-of-fit statistics 
  gof_custom <- list(
    list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
    list("raw" = "adj.r.squared", "clean" = "R2 Adj.", "fmt" = 3),
    list("raw" = "FE: st", "clean" = "State FE", "fmt" = "X"),
    list("raw" = "FE: year", "clean" = "Year FE", "fmt" = "X"),
    list("raw" = "FE: k.id", "clean" = "Leader FE", "fmt" = "X")
  )
  
  # Use the CORRECT raw variable names from the summary output
  coef_correct <- c(
    "leadertypeparty"  = "Party Leader",
    "cpg.pca"          = "Conditional Party Government (latent)",
    "party.margin"     = "Party Margin",
    "incumbent"        = "Incumbent Leader",
    "termlim"          = "Legislative Term Limits",
    "samepartygovTRUE" = "Same Party Governor"
  )
  
  # Re-run the command with the corrected coef_map
  modelsummary(
    fit.diff.maj.cpg.ols,
    output = "Tables/cpg.maj.ols.tex",
    title = "OLS Model of Leader-Caucus Signed Divergence",
    fmt = 3,
    #notes = "The dependent variable is Signed Divergence.",
    escape = FALSE,
    coef_map = coef_correct,
    gof_map = gof_custom,
    stars = c('*' = .1, '**' = .05, '***' = .01)
  )  
  
  fit.diff.maj.cpg = list()
  
  fit.diff.maj.cpg[["d"]]=lmer(diff~(1|st)+(1|year)+(1|k.id)+leadertype+cpg.pca+party.margin+incumbent+termlim+samepartygov,data=subset(leader.data,majority == T & party=="D"))
  fit.diff.maj.cpg[["r"]]=lmer(diff~(1|st)+(1|year)+(1|k.id)+leadertype+cpg.pca+party.margin+incumbent+termlim+samepartygov,data=subset(leader.data,majority == T & party=="R"))
  stargazer(fit.diff.maj.cpg,digits = 3, type="text",column.labels = c("D Majority","R Majority"))

  stargazer(fit.diff.maj.cpg,digits = 3,
            title = "Models of Leader-Caucus Signed Divergence",
            covariate.labels = c("Party Leader","Conditional Party Government (latent)","Party Margin","Incumbent Leader","Legislative Term Limits","Same Party Governor","Constant"),
            column.labels = c("D Majority","R Majority"),
            dep.var.labels=c("Signed Divergence"),  label = "table.cpg",
            out = "Tables/cpg.maj.tex")
  
  
  # do plot_predictions over a range of 0.6 to 0.95
  
  round(qu(leader.data$cpg.pca,c(.05,.95)),2)
  round(qu(leader.data$diff,c(.05,.95)),2)
  round(sd(leader.data$cpg.pca, na.rm=T),1)
  mean(leader.data$cpg.pca, na.rm=T)
  summary(leader.data$cpg.pca, na.rm=T)
  
  dat<-fit.diff.maj.cpg[["d"]]@frame
  d.cpg <- plot_predictions(fit.diff.maj.cpg[["d"]],condition=c("cpg.pca"), re.form=NA) + theme_bw() +
    geom_dots(alpha = .8,scale = .3,pch = 18,data = dat, aes(x = cpg.pca,y = -.25)) +
    labs(title="CPG for Democratic Majority Leaders",y="Leader Divergence",x="CPG Score") +
    coord_cartesian(xlim=c(0.65,0.95), ylim=c(-.25,.25))
  
  dat<-fit.diff.maj.cpg[["r"]]@frame
  r.cpg <-plot_predictions(fit.diff.maj.cpg[["r"]],condition=c("cpg.pca"), re.form=NA) + theme_bw() +
    geom_dots(alpha = .8, scale = .3,pch = 18,data = dat, aes(x = cpg.pca,y=0.25, side="bottom")) +
    labs(title="CPG for Republican Majority Leaders",y="Leader Divergence",x="CPG Score") +
    coord_cartesian(xlim=c(0.65,0.95), ylim=c(-.25,.25))

  p<-d.cpg + r.cpg
  p
    
  ggsave("Plots/PNG/Models/diff_cpg.png",plot=p,width=8,height=4)
  ggsave("Plots/PDF/Models/diff_cpg.pdf",plot=p,width=8,height=4)
  
  # prediction
  pred.d<-predictions(fit.diff.maj.cpg[["d"]],newdata=datagrid(cpg.pca=qu(leader.data$cpg.pca[leader.data$party=="D"],c(0.025,0.975))), re.form=NA)
  pred.r<-predictions(fit.diff.maj.cpg[["r"]],newdata=datagrid(cpg.pca=qu(leader.data$cpg.pca[leader.data$party=="R"],c(0.025,0.975))), re.form=NA)
  
  # difference between 95th and 5th percentile

  diff.d=r(pred.d$estimate[2]-pred.d$estimate[1],2)
  diff.r=r(pred.r$estimate[2]-pred.r$estimate[1],2)

  diff.d # 0.12
  diff.r # -0.15
  

  ### robustness checks
  
  fit.r = fit.d = list()
  
  fit.d[[1]]=lmer(diff~(1|st)+(1|year)+(1|k.id)+cpg.pca,data=subset(leader.data,majority == T & party=="D"))
  fit.d[[2]]=lmer(diff~(1|st)+(1|year)+(1|k.id)+cpg.pca+party.margin,data=subset(leader.data,majority == T & party=="D"))
  fit.d[[3]]=lmer(diff~(1|st)+(1|year)+(1|k.id)+cpg.pca+leadertype+party.margin+bowen1+bowen2,data=subset(leader.data,majority == T & party=="D"))
  fit.d[[4]]=lmer(diff~(1|st)+(1|year)+(1|k.id)+cpg.pca+leadertype+party.margin+incumbent+termlim+samepartygov,data=subset(leader.data,majority == T & party=="D"))
  
  fit.r[[1]]=lmer(diff~(1|st)+(1|year)+(1|k.id)+cpg.pca,data=subset(leader.data,majority == T & party=="R"))
  fit.r[[2]]=lmer(diff~(1|st)+(1|year)+(1|k.id)+cpg.pca+party.margin,data=subset(leader.data,majority == T & party=="R"))
  fit.r[[3]]=lmer(diff~(1|st)+(1|year)+(1|k.id)+cpg.pca+leadertype+party.margin+bowen1+bowen2,data=subset(leader.data,majority == T & party=="R"))
  fit.r[[4]]=lmer(diff~(1|st)+(1|year)+(1|k.id)+cpg.pca+leadertype+party.margin+incumbent+termlim+samepartygov,data=subset(leader.data,majority == T & party=="R"))
  
  
  stargazer(fit.d,digits = 3, type="text",column.labels = c("CPG","CPG+Margin","Profession.","Full"))
  stargazer(fit.r,digits = 3, type="text",column.labels = c("CPG","CPG+Margin","Profession.","Full"))
  
  stargazer(fit.d,digits = 3,
            title = "Models of Leader-Caucus Signed Divergence: Democrats",
            covariate.labels = c("Conditional Party Government (latent)","Party Leader","Party Margin","Professionalization 1D", "Professionalization 2D","Incumbent Leader","Legislative Term Limits","Same Party Governor","Constant"),
            column.labels = c("CPG","CPG+Margin","Profession.","Full"),
            dep.var.labels=c("Signed Divergence"),  label = "table.diff.d",
            out = "Tables/cpg.d.tex")
  
  stargazer(fit.r,digits = 3,
            title = "Models of Leader-Caucus Signed Divergence: Republicans",
            covariate.labels = c("Conditional Party Government (latent)","Party Leader","Party Margin","Professionalization 1D", "Professionalization 2D","Incumbent Leader","Legislative Term Limits","Same Party Governor","Constant"),
            column.labels = c("CPG","CPG+Margin","Profession.","Full"),
            dep.var.labels=c("Signed Divergence"),  label = "table.diff.r",
            out = "Tables/cpg.r.tex")
  
}

model_rolls <- function () {
  
  library(lme4)
  library(stargazer)
  library(modelsummary)
  library(ggplot2)
  library(marginaleffects)
  library(patchwork)
  library(ggdist)
  
  library(ggeffects)
  library(ggplot2)
  library(scales)
  
  
  load("Objects/leader model data.Rdata")
  
  
  fit.roll.2model.styr <-list()
  fit.roll.2model.styr[["d"]] <- lmer(majparty.roll~(1|st)+(1|year)+(1|k.id)+leadertype+diff+cpg.pca+party.margin+incumbent+termlim+samepartygov,data=leader.data%>% filter(majparty=="D"))
  fit.roll.2model.styr[["r"]] <- lmer(majparty.roll~(1|st)+(1|year)+(1|k.id)+leadertype+diff+cpg.pca+party.margin+incumbent+termlim+samepartygov,data=leader.data%>% filter(majparty=="R"))
  stargazer(fit.roll.2model.styr,type="text")
  
  stargazer(fit.roll.2model.styr,digits = 3,
            title = "Models of Party Rolls at Leader-Year Level",
            covariate.labels = c("Party Leader","Divergence from Caucus","Conditional Party Government (latent)","Party Margin","Incumbent Leader","Legislative Term Limits","Same Party Governor","Constant"),
            column.labels = c("D Chamber","R Chamber","D Party","R Party"),
            dep.var.labels=c("Leader Divergence"), label = "model.rolls.yr",
            out="Tables/diff.rolls.2model.tex")

  qu(leader.data$majparty.roll,c(.05,.95))
  
  qu(leader.data$diff,c(.05,.95))
  qu(leader.data$diff[leader.data$majparty=="D"],c(.05,.95))
  qu(leader.data$diff[leader.data$majparty=="R"],c(.05,.95))
  
  
  # do plot_predictions over a range of -0.5 to 0.5
  
  dat<-fit.roll.2model.styr[["d"]]@frame
  p.d <- plot_predictions(fit.roll.2model.styr[["d"]],condition=c("diff"), re.form=NA) + theme_bw() +
    geom_dots(alpha = .8, scale = .05,pch = 18,data = dat, aes(x = diff,y = -.01)) +
    labs(title="Roll Rate for Democratic Majority Leaders",y="Roll Rate",x="Leader-Caucus Divergence") +
    scale_y_continuous(labels=scales::label_percent())+coord_cartesian(xlim=c(-0.4,0.4), ylim=c(0,.04))
  
  dat<-fit.roll.2model.styr[["r"]]@frame
  r.d <-plot_predictions(fit.roll.2model.styr[["r"]],condition=c("diff"), re.form=NA) + theme_bw() +
    geom_dots(alpha = .8, scale = .05,pch = 18,data = dat, aes(x = diff,y = -.01)) +
    labs(title="Roll Rate for Republican Majority Leaders",y="Roll Rate",x="Leader-Caucus Divergence") +
    #geom_hline(yintercept=0, linetype="dashed",color="grey") +
    scale_y_continuous(labels=scales::label_percent())+coord_cartesian(xlim=c(-0.4,0.4), ylim=c(0,.04))
  
  #r.d

  p.roll.me<-p.d + r.d
  
  p.roll.me
  
  ggsave("Plots/PNG/Models/rolls_diffs.png",plot=p.roll.me,width=8,height=4)
  ggsave("Plots/PDF/Models/rolls_diffs.pdf",plot=p.roll.me,width=8,height=4)
  
  pred.d = predictions(fit.roll.2model.styr[["d"]],newdata=datagrid(diff=qu(leader.data$diff[leader.data$party=="D"],c(0.025,.975))),re.form=NA)
  pred.r = predictions(fit.roll.2model.styr[["r"]],newdata=datagrid(diff=qu(leader.data$diff[leader.data$party=="D"],c(0.025,.975))),re.form=NA)
  
  pred.d
  pred.r
  
  print(str_c("Republican roll estimates for moderate leadership is ",r(pred.r$estimate[1],3)))
  print(str_c("Republican roll estimates for conservative leadership is ",r(pred.r$estimate[2],3)))
  
  print(str_c("The difference between the moderate and conservative leadership is ",r(pred.r$estimate[1]/pred.r$estimate[2],1)))
  
  ## replication
  
  fit.r = fit.d = list()
  
  fit.d[[1]]=lmer(majparty.roll~(1|st)+(1|year)+(1|k.id)+diff,data=subset(leader.data,majority == T & party=="D"))
  fit.d[[2]]=lmer(majparty.roll~(1|st)+(1|year)+(1|k.id)+diff+cpg.pca+party.margin,data=subset(leader.data,majority == T & party=="D"))
  fit.d[[3]]=lmer(majparty.roll~(1|st)+(1|year)+(1|k.id)+diff+cpg.pca+leadertype+party.margin+bowen1+bowen2,data=subset(leader.data,majority == T & party=="D"))
  fit.d[[4]]=lmer(majparty.roll~(1|st)+(1|year)+(1|k.id)+diff+cpg.pca+leadertype+party.margin+incumbent+samepartygov,data=subset(leader.data,majority == T & party=="D"))
  
  fit.r[[1]]=lmer(majparty.roll~(1|st)+(1|year)+(1|k.id)+diff,data=subset(leader.data,majority == T & party=="R"))
  fit.r[[2]]=lmer(majparty.roll~(1|st)+(1|year)+(1|k.id)+diff+cpg.pca+party.margin,data=subset(leader.data,majority == T & party=="R"))
  fit.r[[3]]=lmer(majparty.roll~(1|st)+(1|year)+(1|k.id)+diff+cpg.pca+leadertype+party.margin+bowen1+bowen2,data=subset(leader.data,majority == T & party=="R"))
  fit.r[[4]]=lmer(majparty.roll~(1|st)+(1|year)+(1|k.id)+diff+cpg.pca+leadertype+party.margin+incumbent+samepartygov,data=subset(leader.data,majority == T & party=="R"))
  
  
  #modelsummary(fit.d,output="markdown",coef_omit = "SD", stars=T)
  
  stargazer(fit.d,digits = 3, type="text",column.labels = c("Divergence","CPG+Margin","Profession.","Full"))
  stargazer(fit.r,digits = 3, type="text",column.labels = c("Divergence","CPG+Margin","Profession.","Full"))
  
  stargazer(fit.d,digits = 3,
            title = "Models of Leader-Caucus Signed Divergence: Democrats",
            covariate.labels = c("Divergence from Caucus","Conditional Party Government (latent)","Party Leader","Party Margin","Professionalization 1D", "Professionalization 2D","Incumbent Leader","Same Party Governor","Constant"),
            column.labels = c("CPG","CPG+Margin","Profession.","Full"),
            dep.var.labels=c("Roll Rate"),  label = "table.diff.d",
            out = "Tables/rolls.d.tex")
  
  stargazer(fit.r,digits = 3,
            title = "Models of Leader-Caucus Signed Divergence: Republicans",
            covariate.labels = c("Divergence from Caucus","Conditional Party Government (latent)","Party Leader","Party Margin","Professionalization 1D", "Professionalization 2D","Incumbent Leader","Legislative Term Limits","Same Party Governor","Constant"),
            column.labels = c("CPG","CPG+Margin","Profession.","Full"),
            dep.var.labels=c("Roll Rate"),  label = "table.diff.r",
            out = "Tables/rolls.r.tex")
  
  
  # FE models
  
  library(fixest)
  
  fit.roll.2model.ols = list()
  
  fit.roll.2model.ols[["d"]]=feols(majparty.roll~leadertype+diff+cpg.pca+party.margin+incumbent+samepartygov | st+year+k.id,data=subset(leader.data,majority == T & party=="D"))
  fit.roll.2model.ols[["r"]]=feols(majparty.roll~leadertype+diff+cpg.pca+party.margin+incumbent+samepartygov | st+year+k.id,data=subset(leader.data,majority == T & party=="R"))
  
  #collinearity(fit.roll.2model.ols$r)
  #The variable 'termlim' is collinear with the fixed-effects `st`.
  
  modelsummary(fit.roll.2model.ols,output="markdown",stars=T)
  
  # This option avoids LaTeX conflicts by creating a simpler table
  options(modelsummary_factory_latex = "kableExtra")
  
  # Define the goodness-of-fit statistics 
  gof_custom <- list(
    list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
    list("raw" = "adj.r.squared", "clean" = "R2 Adj.", "fmt" = 3),
    list("raw" = "FE: st", "clean" = "State FE", "fmt" = "X"),
    list("raw" = "FE: year", "clean" = "Year FE", "fmt" = "X"),
    list("raw" = "FE: k.id", "clean" = "Leader FE", "fmt" = "X")
  )
  
  # Use the CORRECT raw variable names from the summary output
  coef_correct <- c(
    "leadertypeparty"  = "Party Leader",
    "diff"             = "Leader-Caucus Divergence",
    "cpg.pca"          = "Conditional Party Government (latent)",
    "party.margin"     = "Party Margin",
    "incumbent"        = "Incumbent Leader",
    "termlim"          = "Legislative Term Limits",
    "samepartygovTRUE" = "Same Party Governor"
  )
  
  names(fit.roll.2model.ols) <- c("D Chamber", "R Chamber")
  
  # Re-run the command with the corrected coef_map
  modelsummary(
    fit.roll.2model.ols, 
    output = "Tables/diff.rolls.2model.ols.tex",
     title = "OLS Model of Party Rolls at Leader-Year Level",
    fmt = 3,
    #notes = "The dependent variable is Signed Divergence.",
    escape = FALSE,
    coef_map = coef_correct,
    gof_map = gof_custom,
    stars = c('*' = .1, '**' = .05, '***' = .01)
  )  
  
   
}

leader_selection <- function () {
  
  library(dplyr)
  library(janitor)
  
  options(tibble.width = Inf)
  
  load("Objects/leader candidates.Rdata")
  

  ggplot(cands.m, mapping=aes(x=diff,fill=win)) + geom_density(alpha=0.2) + 
    facet_wrap(~party) + 
    xlim(-.75,.75) + 
    labs(x=NULL,y=NULL,fill="") +
    geom_vline(xintercept=0, linetype="dashed", alpha=0.5) +
    theme_bw()
  
  ggs("candidates_winlose",plottype="Density",subfolder=T)
  
  # competition


  ncands = table(cands.m$stsession)
  singles.name = names(ncands[which(ncands==1)])
  cands.dual <- cands.m %>%
    filter(!(stsession %in% singles.name))

  ###
  # models
  ####
  
  fit.chamber <- list()
  
  fit.chamber[["d caucus"]] <-  lmer(winner ~ (1|st) + diff.abs + cpg.pca+party.margin, data = subset(cands.m,party=="D" & type=="Chamber"))
  fit.chamber[["r caucus"]] <-  lmer(winner ~ (1|st) + diff.abs + cpg.pca+party.margin, data = subset(cands.m,party=="R" & type=="Chamber"))
  
  stargazer(fit.chamber,type="text",
            title = "Models of Leader-Caucus Divergence Divergence",
            covariate.labels = c("Leader-Caucus Divergence","Conditional Party Government (latent)","Party Margin","Constant"),
            column.labels = c("Democratic Candidates","Republican Candidates"))
  
  stargazer(fit.chamber,
            title = "Models of Winning Candidates",
            dep.var.labels = "Leadership Election Winner",
            covariate.labels = c("Leader-Caucus Divergence","Conditional Party Government (latent)","Party Margin","Constant"),
            label = "model.cands",
            out = "Tables/cands_model.tex")
}
