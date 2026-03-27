############################################
############################################
# Replication code:                       ##
# Depolarization, Repolarization          ##
# and  Redistributive Ideological Change  ##
# in Britain, 1983-2016                   ##
# Cohen and Cohen (2019)                  ##
############################################
############################################

recode_attitude_nicenames<-function(x){
  case_when(
    x=="rrichn"~ "rich",
    x=="rwealthn" ~ "wealth",
    x=="rrd" ~ "redist",
    x=="unempjobn"~"unemp.job",
    x=="rbigbusnnn"~"big.busns",
    x=="rdolen"~"dole",
    x=="morewelf2n"~"more.wlf",
    x=="rind4n"~"boss.exploit",
    x=="welffeetn"~"wlf.feet",
    x=="damlives2n"~"wlf.dam.lives",
    x=="sochelpn"~"soc.help",
    x=="dolefidln"~"dole.fidl",
    x=="rincomgapn"~"income.gap",
    x=="rtaxspendn"~"tax.spend",
    x=="proudwlf2n"~"proud.wlf",
    x=="failclm2n"~"fail.clm",
    x=="welfhelpn"~"wlf.help",
    
    TRUE ~ x
  )
}



correlate.attitudes<-function(dat, group.col, cols=c(bsa.lrvars), which_fun="cor"){
  egroup.cols<-enquo(group.col)
  factor.name<-(paste0(quo_name(egroup.cols), "f"))
  
  if(which_fun=="cov"){
    print("using covariance")
    mycorrelate<-function (dat){
      dat[,cols]<-apply(dat[,cols], 2, function(x) as.numeric(as.character(x)))
      #cor(data[,cols.1], data[,cols.2], use="pairwise.complete.obs")
      y<-data.frame(cov(dat[, cols], use="pairwise.complete.obs"))
      y$var1<-cols
      return(data.frame(y))
    }
  } else {
    print("using correlation")
    mycorrelate<-function (dat){
      dat[,cols]<-apply(dat[,cols], 2, function(x) as.numeric(as.character(x)))
      y<-data.frame(cor(dat[, cols], use="pairwise.complete.obs"))
      y$var1<-cols
      return(data.frame(y))
    }
  }
  
  aa<-dat %>%
    as_tibble %>%
    group_by(year, !!egroup.cols) %>%
    #group_by(year) %>%
    nest() %>%
    mutate(correl=map(data, mycorrelate)) %>%
    dplyr::select(-data) %>%
    unnest()
  
  aa<-gather(aa, var2, value, -year, -!!egroup.cols, -var1) %>%
    drop_na(!!egroup.cols, value) %>%
    filter(var1!=var2)
  
  aab <- aa %>%
    drop_na(value, !!egroup.cols) %>%
    mutate(year=as.numeric(as.character(year))) %>%
    mutate(nyear=year-1983) %>%
    mutate(ndecade=nyear/10) %>%
    mutate(ndecade07=(year-2008)/10) %>%
    mutate(!!factor.name:=factor(!!egroup.cols)) %>%
    drop_na(!!egroup.cols)
  
  varorder<-as.numeric(factor(aab$var1))<as.numeric(factor(aab$var2))
  aab$firstvar[varorder]<-aab$var1[varorder]
  aab$firstvar[!varorder]<-aab$var2[!varorder]
  aab$secondvar[varorder]<-aab$var2[varorder]
  aab$secondvar[!varorder]<-aab$var1[!varorder]
  aab<-unite(aab, col=mylab, c(!!egroup.cols, year, firstvar, secondvar), remove=FALSE, sep="")
  
  aab<-aab[!duplicated(aab$mylab),]
  aab<-aab[!is.na(aab$value),]
  
  aab<-unite(aab, col=bothvars, c(firstvar, secondvar), remove=FALSE, sep="")
  
  areaorder<-as.numeric(factor(aab$area1))<as.numeric(factor(aab$area2))

  return(aab)
}



correlate.polchoice<-function(data, group.col, pid.cols=c("pid_labcon"), cols=c(bsa.lrvars), wave.col=year){
  egroup.cols<-enquo(group.col)
  ewave.col<-enquo(wave.col)
  factor.name<-(paste0(quo_name(egroup.cols), "f"))
  
  mycorrelate<-function (data, cols.1=cols, cols.2=pid.cols){
    cor(data[,cols.1], data[,cols.2], use="pairwise.complete.obs")
  }
  
  aa<-data %>%
    as_tibble %>%
    dplyr::group_by(!!ewave.col, !!egroup.cols, year) %>%
    nest() %>%
    mutate(correl=map(data, mycorrelate)) %>%
    mutate(correl=map(correl, tidy)) %>%
    dplyr::select(-data) %>%
    unnest()
  names(aa)[names(aa)==".rownames"]<-"attitude"
  aa<-gather(aa, party, value, -!!ewave.col, -!!egroup.cols, -attitude, -year)
  
  aa <- aa %>%
    # drop_na(value, !!egroup.cols) %>%
    mutate(year=as.numeric(as.character(year))) %>%
    mutate(att=as.character(attitude)) %>%
    mutate(!!factor.name:=factor(!!egroup.cols)) %>%
    mutate(year=as.numeric(as.character(year))) %>%
    mutate(nyear=year-1983) %>%
    mutate(ndecade=nyear/10) %>%
    mutate(ndecade07=(year-2008)/10)
    # drop_na(!!egroup.cols)
    
  
  return(aa)
}

trend.attitudes<-function(dat, group.col, cols=c(bsa.lrvars), myfuns=c("mean", "sd", "prop_extremism"), min_scale_length=5){
  rescale_0_1<-function(x){
    x<- (x-min(x, na.rm=TRUE))/(1 + max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
    x
  }
  prop_extremism<-function(x, na.rm=TRUE){
    if(na.rm){
      x<-x[!is.na(x)]
    }
    highest_val<-max(x)
    lowest_val<-min(x)
    second_highest<-sort(unique(x), decreasing=TRUE)[2]
    second_lowest<-sort(unique(x))[2]
    if (length(unique(x))>9){
      high_vals<-c(highest_val, second_highest)
      low_vals<-c(lowest_val, second_lowest)
    } else {
      high_vals<-c(highest_val)
      low_vals<-c(lowest_val)
    }
    (sum(x %in% high_vals)+sum(x %in% low_vals))/length(x)
  }
  scale_length<-function(x, na.rm=TRUE){
    if(na.rm){
      x<-x[!is.na(x)]
    }
    (length(unique(x)))
  }
  egroup.cols<-enquo(group.col)
  factor.name<-(paste0(quo_name(egroup.cols), "f"))
  dat[,"none"]<-"no group"
  
  meets_min_scale<- dat %>%
    summarize_at(cols, .funs=scale_length) %>%
    gather(attitude, scale_length) %>%
    filter(scale_length>=min_scale_length)
  
  aa<-dat %>%
    as_tibble %>%
    group_by(year, !!egroup.cols) %>%
    mutate_at(cols, rescale_0_1) %>%
    summarize_at(cols, funs_(myfuns), na.rm=TRUE)
  
  aa<-gather(aa, variable, value, -year, -!!egroup.cols) %>%
    drop_na(!!egroup.cols, value) %>%
    separate(variable, sep="_", into=c("attitude", "fun")) %>%
    inner_join(meets_min_scale, by="attitude")

  
  aab <- aa %>%
    ungroup %>%
    drop_na(value, !!egroup.cols) %>%
    mutate(year=as.numeric(as.character(year))) %>%
    mutate(nyear=year-1983) %>%
    mutate(ndecade=nyear/10) %>%
    mutate(area = "redistribution") %>%
    mutate(!!factor.name:=factor(!!egroup.cols)) %>%
    drop_na(!!egroup.cols) %>%
    mutate(dimension=case_when(area %in% c("redistribution", "welfarist") ~ "redistribution",
                               area %in% c("values", "environment", "nationalism") ~ "culture"))
  return(aab)
}


split.dat<-function(data, group.col, short.factor.levels, balance.no=3, core.vars=c(bsa.scale.vars), pid.cols=c("pid_labcon"), party.contrast.labels=c("lc")) {
  egroup.cols<-rlang::sym(group.col)
  ecore<-rlang::syms(core.vars)
  eparty.contrasts<-rlang::syms(pid.cols)
  funlist<-list()
  data<-data.frame(data)
  levs<-levels(factor(data[,group.col]))
  lev1<-levs[1]
  
  before<-data %>%
    dplyr::filter(party==!!pid.cols[1]) %>%
    filter(UQ(egroup.cols)==(!!lev1)) %>%
    dplyr::filter(year<2008) %>%
    dplyr::group_by(attitude) %>%
    dplyr::summarize(appears=length(year)) %>%
    dplyr::filter(appears>=balance.no)
  after<-data %>%
    dplyr::filter(party==!!pid.cols[1]) %>%
    filter(UQ(egroup.cols)==(!!lev1)) %>%
    dplyr::filter(year>2006) %>%
    dplyr::group_by(attitude) %>%
    dplyr::summarize(appears=length(year)) %>%
    dplyr::filter(appears>=balance.no)
  
  
  #balanced.vars<-unique(data$attitude[data$attitude %in% before$attitude & data$attitude %in% after$attitude]) 
  balanced.vars<-intersect(unique(before$attitude), unique(after$attitude))
  
  for (pid in 1:length(pid.cols)){
    for (n in 1:length(levs)){
      lev<-levs[n]
      this.83.name<-paste0("corp", group.col, "_", party.contrast.labels[pid], "_83_07_", short.factor.levels[n])
      this.83.name.core<-paste0(this.83.name, "_core")
      this.83.name.b<-paste0(this.83.name, "_b")
      this.07.name<-paste0("corp", group.col, "_",  party.contrast.labels[pid], "_07_16_", short.factor.levels[n])
      this.07.name.core<-paste0(this.07.name, "_core")
      this.07.name.b<-paste0(this.07.name, "_b")
      
      d<-data %>%
        filter(party==!!pid.cols[pid]) %>%
        filter(UQ(egroup.cols)==(!!lev))
      
      d83<- d %>%
        filter(year<2008)
      d07<- d %>%
        filter(year>2006)
      
      
      funlist[[this.83.name]]<-d83
      
      funlist[[this.83.name.core]] <-d83 %>%
        filter(attitude %in% c(!!!core.vars))
      
      funlist[[this.83.name.b]] <-d83 %>%
        filter(attitude %in% c(balanced.vars))
      
      funlist[[this.07.name]]<-d07
      funlist[[this.07.name.core]] <-d07 %>%
        filter(attitude %in% c(!!!core.vars))
      funlist[[this.07.name.b]] <-d07 %>%
        filter(attitude %in% c(balanced.vars))
    }
  }
  return(funlist)
}


split.fun.dat<-function(data, group.col, short.factor.levels, core.vars=c(bsa.scale.vars), balance.no=3, fun_col="fun") {
  egroup.cols<-rlang::sym(group.col)
  ecore<-rlang::syms(core.vars)
  efun_col<-rlang::sym(fun_col)
  funlist<-list()
  data<-data.frame(data)
  levs<-levels(factor(data[,group.col]))
  
  before<-data %>%
    dplyr::filter(year<2008) %>%
    group_by(attitude, !!efun_col) %>%
    dplyr::summarize(appears=length(year)) %>%
    filter(appears>=balance.no)
  after<-data %>%
    dplyr::filter(year>2006) %>%
    group_by(attitude, !!efun_col) %>%
    dplyr::summarize(appears=length(year)) %>%
    filter(appears>=balance.no)
  
  balanced.vars<-unique(data$attitude[data$attitude %in% before$attitude & data$attitude %in% after$attitude]) 
  
  
  for (n in 1:length(levs)){
    for(this_fun in unique(data[,fun_col])){
      lev<-levs[n]
      this.83.name<-paste0(this_fun, "_", group.col, "_83_07_", short.factor.levels[n])
      this.83.name.core<-paste0(this.83.name, "_core")
      this.83.name.b<-paste0(this.83.name, "_b")
      this.07.name<-paste0(this_fun, "_", group.col, "_07_16_", short.factor.levels[n])
      this.07.name.core<-paste0(this.07.name, "_core")
      this.07.name.b<-paste0(this.07.name, "_b")
      
      d<-data %>%
        filter(UQ(efun_col)==(!!this_fun)) %>%
        filter(UQ(egroup.cols)==(!!lev))
      
      d83<- d %>%
        filter(year<2008)
      d07<- d %>%
        filter(year>2006)
      
      
      funlist[[this.83.name]]<-d83
      
      funlist[[this.83.name.core]] <-d83 %>%
        filter(attitude %in% c(!!!core.vars))
      
      funlist[[this.83.name.b]] <-d83 %>%
        filter(attitude %in% c(!!!balanced.vars))
      
      funlist[[this.07.name]]<-d07
      funlist[[this.07.name.core]] <-d07 %>%
        filter(attitude %in% c(!!!core.vars))
      funlist[[this.07.name.b]] <-d07 %>%
        filter(attitude %in% c(!!!balanced.vars))
      
      
    }
  }
  return(funlist)
}


split.constraint.dat<-function(data, group.col, short.factor.levels, balance.no=3, core.vars=c(r.idx3, l.idx), stan.label="att") {
  
  # find balanced var - vars with at least balance no before and after 2007 
  vars<-data[,c("year", "var1")]
  vars2<-data[,c("year", "var2")]
  names(vars2)<-names(vars)
  vars<-rbind(vars, vars2)
  vars<-unique(vars)
  before<-vars %>%
    dplyr::filter(year<2008) %>%
    group_by(var1) %>%
    dplyr::summarize(appears=length(year)) %>%
    filter(appears>=balance.no)
  after<-vars %>%
    dplyr::filter(year>2006) %>%
    group_by(var1) %>%
    dplyr::summarize(appears=length(year)) %>%
    filter(appears>=balance.no)
  
  balanced.vars<-unique(vars$var1[vars$var1 %in% before$var1 & vars$var1 %in% after$var1]) 
  
  egroup.cols<-rlang::sym(group.col)
  ebalanced<-rlang::syms(balanced.vars)
  ecore<-rlang::syms(core.vars)
  funlist<-list()
  data<-data.frame(data)
  levs<-levels(factor(data[,group.col]))
  
  for (n in 1:length(levs)){
    lev<-levs[n]
    this.83.name<-paste0("s_", stan.label, "_", group.col, "_83_07_", short.factor.levels[n])
    this.83.name.core<-paste0(this.83.name, "_core")
    this.83.name.b<-paste0(this.83.name, "_b")
    this.07.name<-paste0("s_", stan.label, "_", group.col, "_07_16_", short.factor.levels[n])
    this.07.name.core<-paste0(this.07.name, "_core")
    this.07.name.b<-paste0(this.07.name, "_b")
    
    d<-data %>%
      filter(UQ(egroup.cols)==(!!lev))
    
    d83<- d %>%
      filter(year<2008)
    d07<- d %>%
      filter(year>2006)
    
    
    funlist[[this.83.name]]<-d83
    
    funlist[[this.83.name.core]] <-d83 %>%
      filter(var1 %in% c(!!!core.vars) & var2  %in% c(!!!core.vars))
    
    funlist[[this.83.name.b]] <-d83 %>%
      filter(var1 %in% c(!!!balanced.vars) & var2 %in% c(!!!balanced.vars))
    
    funlist[[this.07.name]]<-d07
    
    funlist[[this.07.name.core]] <-d07 %>%
      filter(var1 %in% c(!!!core.vars) & var2  %in% c(!!!core.vars))
    
    funlist[[this.07.name.b]] <-d07 %>%
      filter(var1 %in% c(!!!balanced.vars) & var2 %in% c(!!!balanced.vars))
  }
  return(funlist)
}



myregtable<-function(y, digits=2, covariate.relabels=c("Intercept", "Time (decades)", "Residual SD:", "\\hskip .5cm Intercepts", "\\hskip .5cm Trends", "\\hskip .5cm Data", "N", "Groups"), model.names=c("1983-2007", "2007-2016"), extraheader1=c("$\\rho =$ attitude  $\\times $ attitude"), extraheader1colspans=c(0), extraheader2=c(""), extraheader2colspans=c(0), error.labels="default", sd.break="Residual SD:", caption="caption", label=NULL, gsub_coef_pattern="ndecade07", gsub_coef_replace="ndecade", ordermodel=1, nsmall=2){
  if (exists("fullouttable")){
    rm("fullouttable")
  }
  
  if (class(y)[[1]]=="list"){
    
    n.goes<-length(y)
    
  } else {
    #y<-list(y, y)
    y<-list(y)
    n.goes<-1
  }
  for (n in 1:n.goes){
    current.mod.names<-paste0("mod", n)
    x<-y[[n]]
    fixef<-tidy(x)
    my_round <- function(x, digits, nsmall){
      format(round(x, digits = digits), nsmall=nsmall)
    }
    h<-tidy(x, "hierarchical")
    #fixef[,-1] <- round(fixef[,-1], digits) #format(round(fixef[,-1], digits), nsmall=digits)
    fixef<-fixef %>% 
      mutate_at(2:ncol(.), my_round, digits=digits, nsmall=nsmall) 
    #h[,ncol(h)]<-round(h[,ncol(h)], digits=digits)  #format(round(h[,ncol(h)], digits=digits), nsmall=digits)
    h<- h %>% 
      mutate_at(3, my_round, digits=digits, nsmall=nsmall) 
    outtable<-data.frame(matrix(ncol=2, nrow=0))
    outtable[1:nrow(fixef), 1]<-fixef$term
    outtable[1:nrow(fixef), 2]<-paste0(fixef$estimate, " (", fixef$std.error, ")")
    #print(xtable(outtable), digits = c(0,0, 1, 2))
    
    h<-separate(h, "term", c("A", "B"), "_", remove=FALSE)%>%
      separate("B", c("C", "D"), "\\.", extra="merge", remove=FALSE)
    brk<-data.frame(sd.break, NA)
    names(brk)<-names(outtable)
    outtable<-rbind(outtable, brk)
    h<-h[h$A=="sd",c("C", "estimate")]
    h[,1]<-paste("\\hskip .5cm ", xtable::sanitize(h[,1, drop=TRUE]))
    
    names(h)<-names(outtable)
    
    outtable<-rbind(outtable, h)
    
    outtable<-rbind(outtable,c("N", attr(summary(x), "nobs")))
    outtable<-rbind(outtable,c("Groups", attr(summary(x), "ngrps")))
    
    outtable[,1]<-str_replace(outtable[,1], gsub_coef_pattern, gsub_coef_replace)
    
    #a<-xtable(outtable)
    #a<-print(xtable(outtable), type="latex", sanitize.text.function=function(x){x})
    names(outtable)[names(outtable)!="X1"]<-paste0(current.mod.names, names(outtable)[names(outtable)!="X1"])
    
    if (!exists("fullouttable")){
      fullouttable<-outtable
      fullouttable$roworder<-1:nrow(fullouttable)
      #print(ncol(fullouttable))
    } else {
      outtable$roworder<-1:nrow(outtable)
      if(ordermodel==n){
        fullouttable<-fullouttable%>%
          dplyr::select(-roworder)
      } else {
        outtable<-outtable%>%
          dplyr::select(-roworder) 
      }
      #print(n)
      #print(ncol(fullouttable))
      fullouttable<-full_join(fullouttable, outtable, by="X1", suffix=c("", n)) %>% 
        arrange(roworder)
    }
    
    
  }
  
  ncols<-ncol(fullouttable)-1
  
  #align1<-c("l",rep("p{2in}",ncols))
  align1<-c("l",rep("l",ncols))
  a<-fullouttable %>%
    dplyr::select(-roworder) %>%
    xtable::xtable(align=align1, caption=caption, label=label)
  
  a$X1[1:length(covariate.relabels)]<-covariate.relabels
  if(length(model.names)==(ncol(a)-1))
    names(a)<-c("", model.names)
  addtorow <- list()
  addtorow$pos <- list(-1,-1)
  cmd1<-NULL
  for (pp in 1:length(extraheader1)){
    cmd1<-paste0(cmd1, "& \\multicolumn{", extraheader1colspans[pp], "}{c}{",extraheader1[pp], "}")
  }
  cmd1<-paste0(cmd1, " \\\\")
  
  cmd2<-NULL
  for (pp in 1:length(extraheader2)){
    cmd2<-paste0(cmd2, "& \\multicolumn{", extraheader2colspans[pp], "}{c}{",extraheader2[pp], "}")
  }
  cmd2<-paste0(cmd2, " \\\\")
  
  addtorow$command <- c(cmd1, cmd2)
  a<-(print(a, type="latex", add.to.row=addtorow, comment=FALSE, sanitize.text.function=function(x){gsub("//", "// /n /n", x)}, file="clipboard-128", include.rownames=FALSE, floating=TRUE, hline.after=c(0)))
  b<-(paste0("cat(", a, ")"))
  b<-(print(b, type="latex", sanitize.text.function=function(x){gsub("//", "// /n /n", x)}, include.rownames=FALSE))
}


ranef_scatterplot_timecombine<-function(sf1, sf2, pdf.output=FALSE , graph.name="stangraph.pdf", i.path=paste0(dropbox.path, "/SarahGidon/images"), with.col=FALSE, with.time.col=FALSE, with.slope.col=TRUE, palette_name="Set1", gwidth=7, gheight=5, zero.line=TRUE, times=c("1983-2007", "2007-2016"), x.min=c(0, 2.5), x.max=c(2.4, 3.5), xshift_t1=0, xshift_t2=0, facet.type="wrap", legend.pos="bottom", selectvars="all", ncol=NULL, facet.on.domain=FALSE, facet.on.source=FALSE, fix_interact="dimension", ylimmin="default", ylimmax="default", slopevar_t1="ndecade", slopevar_t2="ndecade", source.order="none", recode.attitude.function="none", xlab="decades after 1983", ylab="value"){
  dat<-NULL
  re<-NULL
  for (t in 1:2){
    if(t==1){
      this_slopevar<-slopevar_t1
    } else {
      this_slopevar<-slopevar_t2
    }
    sf<-list(sf1, sf2)[[t]]
    global.int<-fixef(sf)[[1]]
    if (length(fixef(sf)>1)){
      global.slope<-fixef(sf)[[2]] 
    } else {
      global.slope=0
    }
    
    if(is.null(dat)){
      dat1<-sf$data
      dat1$time<-times[t]
      dat<-dat1
    } else {
      dat1<-sf$data
      dat1$time<-times[t]
      dat<-bind_rows(dat, dat1)
    }
    #if (is.null(ranef(sf)$attitude)){
    #  re1<-ranef(sf)$bothvars
    #} else {
    #  re1<-ranef(sf)$attitude
    #}
    
    
    #re1$attitude<-rownames(re1)
    #re1$attitude<-rownames(re1)
    #re1$int<-re1[,"(Intercept)"]+global.int
    #re1$ndecade<-re1$ndecade+global.slope
    re1<-ranef_extract_slopes(sf, slopevar = this_slopevar, fix_interact = fix_interact)
    re1$time<-times[t]
    if (t==1){
      re1$xstart<-x.min[t] +xshift_t1
      re1$xend<-x.max[t] +xshift_t1
    }
    if (t==2){
      re1$xstart<-x.min[t] +xshift_t2
      re1$xend<-x.max[t] +xshift_t2
    }
    re1$ystart<-re1$int+x.min[t]*re1$slope
    re1$yend<-re1$int+x.max[t]*re1$slope
    
    if (is.null(re)){
      re<-re1
    } else {
      re<-bind_rows(re, re1)
    }
  }
  re.levs1<-re[re$time==times[1],c("attitude", "slope")]
  re.levs2<-re[re$time==times[2],c("attitude", "slope")]
  add.1<-re.levs2$attitude[!(re.levs2$attitude %in% re.levs1$attitude)]
  add.2<-re.levs1$attitude[!(re.levs1$attitude %in% re.levs2$attitude)]
  add.1<-data.frame("attitude"=add.1, "slope"=rep(0, length(add.1)))
  add.2<-data.frame("attitude"=add.2, "slope"=rep(0, length(add.2)))
  
  re.levs1<-bind_rows(re.levs1, add.1)
  re.levs2<-bind_rows(re.levs2, add.2)
  
  slope.change.order<-order(re.levs1$slope - re.levs2$slope[order(match(re.levs2$attitude, re.levs1$attitude))])
  
  re$attitude<-factor(re$attitude, levels=re.levs1$attitude[slope.change.order])
  #re$slope<-re$ndecade
  
  # names of col of interest different in different regressions
  if(!"area" %in% names(dat)){
    dat$area<-""
    dat$bothareas<-""
  }
  
  if(!"attitude" %in% names(dat)){
    dat$attitude<-dat$bothvars
    dat$area<-dat$bothareas
  }
  if (is.null(dat$source)){
    dat$source<-"commonsrc"
  } else {
    if(!identical(source.order, "none")){
      dat$source<-factor(dat$source, levels=source.order)
    }
  }
  mre<-dat %>% 
    dplyr::group_by(attitude, area, source) %>%
    dplyr::summarize(n=length(attitude))
  
  dat$attitude<-factor(dat$attitude, levels=re.levs1$attitude[slope.change.order])
  
  dat<-merge(dat, re[,names(re)[names(re)!="ndecade"]], by=c("attitude", "time", "area"), all.x=TRUE)
  re<-merge(re, mre, by=c("attitude", "area"), all.x=TRUE)
  
  short.recodes<-'"redistribution"="rd";
  "welfarist"="wlf";
  "values"="val";
  "environment"="env";
  "nationalism"="nat";
  "europe"="eu";
  "defence"="def"'
  
  dat$shortarea<-car::recode(dat$area,
                             short.recodes)
  re$shortarea<-car::recode(re$area,
                            short.recodes)
  dat$slopetype<-ifelse(dat$slope<0, "negative trend", "positive trend")
  re$slopetype<-ifelse(re$slope<0, "negative trend", "positive trend")
  
  if (!identical(selectvars, "all")){
    dat<-dat[dat$attitude %in% selectvars,]
    re<-re[re$attitude %in% selectvars,]
  } 
  
  if(!identical(recode.attitude.function, "none")){
    dat<-dat %>%
      mutate(attitude=fct_relabel(attitude, recode_attitude_nicenames))
    re<-re %>%
      mutate(attitude=fct_relabel(attitude, recode_attitude_nicenames))
    
  }
  
  plt<-ggplot(dat, aes(y=value, x=ndecade))
  if(facet.type=="grid"){
    plt<- plt + facet_grid(.~attitude)
  } else if (facet.on.domain){
    plt<-plt + facet_wrap(~shortarea+attitude, ncol=ncol)
  } else if (facet.on.source){
    plt<-plt +  facet_wrap(~source+attitude, ncol=ncol)
  }  else {
    plt<-plt + facet_wrap(~attitude, ncol=ncol)
  }
  
  if (with.time.col){
    plt<-plt+
      geom_point(aes(colour=time))+
      scale_color_brewer(palette = palette_name)
  } else if (with.col){
    plt<-plt+
      geom_point(aes(colour=area))+
      scale_color_brewer(palette = palette_name)
  } else if (with.slope.col){
    if(palette_name=="my_bw"){
      plt<-plt+
        geom_point(aes(colour=slopetype))+
        scale_color_manual(values = c("black", "grey"))
    } else if(palette_name=="notcol"){
      plt<-plt+
        geom_point(aes(shape=slopetype))+
        scale_color_manual(values = c("black", "black"))
    } else {
      plt<-plt+
        geom_point(aes(colour=slopetype))+
        scale_color_brewer(palette = palette_name)
    }
  } else {
    plt<-plt+
      geom_point()
  } 
  plt<-plt+
    #geom_abline(data=re, aes(intercept=int, slope=ndecade))+
    geom_segment(data=re, aes(x=xstart, y=ystart, xend=xend, yend=yend, colour=slopetype))+
    geom_hline(yintercept = 0)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), panel.grid.major=element_blank(), panel.grid.minor = element_blank(),
          legend.position=legend.pos, legend.title=element_blank())+
    xlab(xlab)+
    ylab(ylab)
  if (zero.line){
    plt<-plt+
      geom_hline(yintercept=0)
  }
  if(is.numeric(ylimmin)|is.numeric(ylimmax)){
    as.numeric("default")
    plt<-plt+
      ylim(as.numeric(ylimmin), as.numeric(ylimmax))
  }
  if(pdf.output){
    pdf(paste0(i.path, "/", graph.name), width = gwidth, height = gheight)
    print(plt)
    dev.off()
  } else {
    print(plt)
  }
  
}

ranef_extract_slopes<-function(sf, slopevar="ndecade", fix_interact="dimension"){
  dat<-NULL
  re<-NULL
  if(fix_interact=="none"){
    sf$data[,"none"]<-0
  }
  if(fix_interact!="none"){
    levs<-levels(factor(sf$data[,fix_interact, drop=TRUE]))[-1]
    #fi<-enquo(fix_interact)
    
  }
  fixdf<-tidy(sf)
  global.int<-fixef(sf)[[1]]
  if (length(fixef(sf)>1)){
    global.slope<-fixef(sf)[[2]] 
  } else {
    global.slope=0
  }
  
  if(is.null(dat)){
    dat1<-sf$data
    dat<-dat1
  } else {
    dat1<-sf$data
    dat<-rbind(dat, dat1)
  }
  if (is.null(ranef(sf)$attitude)&is.null(ranef(sf)$attdata)){
    re1<-ranef(sf)$bothvars
    re1$bothvars<-rownames(re1)
    
    datsum<-unique(dat1[, c("bothvars", fix_interact)])
    re1<-left_join(re1, datsum, by="bothvars")
    
    re1$joinint<-paste0(fix_interact, re1[,fix_interact])
    re1$joinslope<-paste0(slopevar, ":", fix_interact, re1[,fix_interact])
    re1$attitude<-re1$bothvars 
  } else if (is.null(ranef(sf)$attitude)){
    re1<-ranef(sf)$attdata
    re1$attdata<-rownames(re1)
    
    datsum<-unique(dat1[, c("attdata", fix_interact)])
    re1<-left_join(re1, datsum, by="attdata")
    
    re1$joinint<-paste0(fix_interact, re1[,fix_interact])
    re1$joinslope<-paste0(slopevar, ":", fix_interact, re1[,fix_interact])
    #re1$attitude<-re1$attdata
  } else {
    re1<-ranef(sf)$attitude
    re1$attitude<-rownames(re1)
    if(fix_interact=="none"){
      dat1$none<-0
    }
    datsum<-unique(dat1[, c("attitude", fix_interact)])
    re1<-left_join(re1, datsum, by="attitude")
    
    re1$joinint<-paste0(fix_interact, re1[,fix_interact])
    re1$joinslope<-paste0(slopevar, ":", fix_interact, re1[,fix_interact])
    
    
    
  }
  
  #re1$attitude<-rownames(re1)
  #re1$attitude<-rownames(re1)
  
  fixdf[,c("fixslope", "fixint", "globalint", "globalslope")]<-fixdf$estimate
  
  re1$globalint<-fixdf[fixdf$term=="(Intercept)", "globalint", drop=TRUE]
  
  re1$globalslope<-ifelse(length(fixdf[fixdf$term==slopevar, "globalslope", drop=TRUE])==0,
                          0,
                          fixdf[fixdf$term==slopevar, "globalslope", drop=TRUE])
  re1<-left_join(re1, fixdf[,c("term", "fixint")], by=c("joinint"="term"))
  re1<-left_join(re1, fixdf[,c("term", "fixslope")], by=c("joinslope"="term"))
  re1$fixint[is.na(re1$fixint)]<-0
  re1$fixslope[is.na(re1$fixslope)]<-0
  
  re1$int<-rowSums(re1[,c("(Intercept)", "fixint", "globalint")])
  re1$slope<-rowSums(re1[,c(slopevar, "fixslope", "globalslope")]) #$ndecade+global.slope
  
  
  if (is.null(re)){
    re<-re1
  } else {
    re<-rbind(re, re1)
  }
  
  # names of col of interest different in different regressions
  if(!"area" %in% names(dat)){
    dat$area<-""
    dat$bothareas<-""
  }
  
  if(!"attitude" %in% names(dat)){
    dat$attitude<-dat$bothvars
    dat$area<-dat$bothareas
  }
  mre<-dat %>% 
    dplyr::group_by(attitude, area) %>%
    dplyr::summarize(n=length(attitude))
  
  
  dat<-merge(dat, re[,names(re)[!names(re) %in% c("ndecade", "area")]], by=c("attitude"), all.x=TRUE)
  if ("area" %in% names(re)){
    re<-merge(re, mre[,names(mre)[!names(mre) %in% c("area")]], by="attitude")
  } else {
    re<-merge(re, mre, by="attitude")
  }
  
  short.recodes<-'"redistribution"="rd";
  "welfarist"="wlf";
  "values"="val";
  "environment"="env";
  "nationalism"="nat";
  "europe"="eu";
  "defence"="def"'
  
  dat$shortarea<-car::recode(dat$area,
                             short.recodes)
  re$shortarea<-car::recode(re$area,
                            short.recodes)
  dat$slopetype<-ifelse(dat$slope<0, "negative trend", "positive trend")
  re$slopetype<-ifelse(re$slope<0, "negative trend", "positive trend")
  
  return(re)
}

ranef_scatterplot_dataset<-function(sf, pdf.output=FALSE , graph.name="stangraph.pdf", i.path=paste0(dropbox.path, "/SarahGidon/images"), with.col=FALSE, with.time.col=FALSE, with.slope.col=TRUE, palette_name="Set1", gwidth=7, gheight=5, zero.line=TRUE, times=c("1983-2007", "2007-2016"), slopevar="ndecade", fix_interact="dimension", ncol=NULL, legend.position="none", text_size=NULL, ylab="correlation", xlab="decades after 1991", relabeller=relabel_fix_facet){
  global.int<-fixef(sf)[[1]]
  if (length(fixef(sf)>1)){
    global.slope<-fixef(sf)[[2]] 
  } else {
    global.slope=0
  }
  
  re<-ranef_extract_slopes(sf, slopevar=slopevar, fix_interact = fix_interact)
  dat<-sf$data
  dat$dataset2<-dat$dataset
  dat$dataset2[dat$dataset=="observed"]<-NA
  dat_features<- dat %>%
    dplyr::group_by(attitude, variable, dataset, dataset2) %>%
    summarize(lengthx=length(attitude))
  re<- left_join(re, dat_features)
  
  if(!"attitude" %in% names(dat)){
    dat$attitude<-dat$bothvars
  }
  
  #re$attitude<-factor(re$attitude, levels=re$attitude[order(re$slope)])
  #dat$attitude<-factor(dat$attitude, levels=re$attitude[order(re$slope)])
  #dat[,"slope"]<-dat[,slopevar]
  
  dat<-merge(dat, re[,names(re)[names(re)!="ndecade"]], by=c("attitude", "dataset", "dataset2", "variable"), all.x=TRUE)
  
  dat$slopetype<-ifelse(dat$slope<0, "negative trend", "positive trend")
  re$slopetype<-ifelse(re$slope<0, "negative trend", "positive trend")
  
  foregroundpoints <-filter(dat, !is.na(dataset2))
  backgroundpoints <-filter(dat, is.na(dataset2)) %>%
    dplyr::select(-dataset2)
  
  foregroundre <-filter(re, !is.na(dataset2))
  backgroundre <-filter(re, is.na(dataset2)) %>%
    dplyr::select(-dataset2)
  
  plt<-ggplot(foregroundpoints, aes(y=value, x=ndecade, colour="fixed value"))+
    facet_grid(dataset2~variable, labeller= relabeller)
  #facet_grid(dataset2~variable)
  
  
  plt<-plt+
    geom_point(aes(colour="fixed value")) +
    geom_point(data=backgroundpoints, aes(colour="observed"))
  
  plt<-plt+
    geom_abline(data=foregroundre, size=1, aes(intercept=int, slope=slope, colour="fixed value"))+
    geom_abline(data=backgroundre, size=1, aes(intercept=int, slope=slope, colour="observed"))+
    geom_hline(yintercept = 0)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), panel.grid.major=element_blank(), panel.grid.minor = element_blank(),
          legend.position=legend.position, legend.title=element_blank())+
    scale_colour_manual(name="",
                        values=c("fixed value"="black", "observed"="grey")) +
    
    xlab(xlab)+
    ylab(ylab)
  
  if (zero.line){
    plt<-plt+
      geom_hline(yintercept=0)
  }
  if(pdf.output){
    pdf(paste0(i.path, "/", graph.name), width = gwidth, height = gheight)
    print(plt)
    dev.off()
  } else {
    print(plt)
  }
  
}


ranef_scatterplot_dataset2<-function(sf, pdf.output=FALSE , graph.name="stangraph.pdf", i.path=paste0(dropbox.path, "/SarahGidon/images"), with.col=FALSE, with.time.col=FALSE, with.slope.col=TRUE, palette_name="Set1", gwidth=7, gheight=5, zero.line=TRUE, times=c("1983-2007", "2007-2016"), slopevar="ndecade", fix_interact="dimension", ncol=NULL, legend.position="none", text_size=NULL, ylab="correlation", xlab="decades after 1991", relabeller=relabel_fix_facet){
  global.int<-fixef(sf)[[1]]
  if (length(fixef(sf)>1)){
    global.slope<-fixef(sf)[[2]] 
  } else {
    global.slope=0
  }
  
  re<-ranef_extract_slopes(sf, slopevar=slopevar, fix_interact = fix_interact)
  dat<-sf$data
  dat$dataset2<-dat$dataset
  dat$dataset2[dat$dataset=="partisans"]<-NA
  dat_features<- dat %>%
    dplyr::group_by(attitude, variable, dataset, dataset2) %>%
    summarize(lengthx=length(attitude))
  re<- left_join(re, dat_features)
  
  if(!"attitude" %in% names(dat)){
    dat$attitude<-dat$bothvars
  }
  
  #re$attitude<-factor(re$attitude, levels=re$attitude[order(re$slope)])
  #dat$attitude<-factor(dat$attitude, levels=re$attitude[order(re$slope)])
  #dat[,"slope"]<-dat[,slopevar]
  
  dat<-merge(dat, re[,names(re)[names(re)!="ndecade"]], by=c("attitude", "dataset", "dataset2", "variable"), all.x=TRUE)
  
  dat$slopetype<-ifelse(dat$slope<0, "negative trend", "positive trend")
  re$slopetype<-ifelse(re$slope<0, "negative trend", "positive trend")
  
  foregroundpoints <-filter(dat, !is.na(dataset2))
  backgroundpoints <-filter(dat, is.na(dataset2)) %>%
    dplyr::select(-dataset2)
  
  foregroundre <-filter(re, !is.na(dataset2))
  backgroundre <-filter(re, is.na(dataset2)) %>%
    dplyr::select(-dataset2)
  
  plt<-ggplot(foregroundpoints, aes(y=value, x=ndecade))+
    facet_grid(.~variable)
  
  
  plt<-plt+
    geom_point(aes(shape="matches")) +
    geom_point(data=backgroundpoints, aes(shape="partisans"))
  
  plt<-plt+
    geom_abline(data=foregroundre, size=1, aes(intercept=int, slope=slope, linetype="matches"))+
    geom_abline(data=backgroundre, size=1, aes(intercept=int, slope=slope, linetype="partisans"))+
    geom_hline(yintercept = 0)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), panel.grid.major=element_blank(), panel.grid.minor = element_blank(),
          legend.position=legend.position, legend.title=element_blank())+
    scale_linetype_manual(name="",
                          values=c("matches"="solid", "partisans"="dashed")) +
    scale_shape_manual(name="",
                       values=c("matches"="circle", "partisans"="square")) +
    
    xlab(xlab)+
    ylab(ylab)
  
  if (zero.line){
    plt<-plt+
      geom_hline(yintercept=0)
  }
  if(pdf.output){
    pdf(paste0(i.path, "/", graph.name), width = gwidth, height = gheight)
    print(plt)
    dev.off()
  } else {
    print(plt)
  }
  
}

rescale_0_1<-function(x){
  x<- (x-min(x, na.rm=TRUE))/(1 + max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
  x
}

