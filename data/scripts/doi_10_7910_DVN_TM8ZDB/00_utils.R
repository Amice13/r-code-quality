## The main function post-cleaning
get_ds = function(stateabb, elec, save=TRUE){ # also needs: nm_df, returns, ensemble

  ## Starting with the point data
  load("../data_clean/all_firms.RData")
  firms1 = firms1 %>% filter(state == stateabb)
  firms1 = sf::st_as_sf(x = firms1, coords = c("lon", "lat"), crs="NAD83")
  firms2 = sp::over(as(firms1, "Spatial"), as(nm_sf, "Spatial"))
  firms1$NAME = firms2$NAME
  firms2 = firms1 %>% group_by(NAME) %>%
    summarize(firms = sum(type == "firm"),
              walmarts = sum(type == "walmart"),
              hospitals = sum(type == "hospital"),
              airports = sum(type == "airport"),
              pacfirms = sum(type == "pacfirm"))
  
  
  load("../data_clean/all_donors.RData")
  donors1 = donor %>% filter(state == stateabb)
  donors1 = sf::st_as_sf(x = donors1, coords = c("lon", "lat"), crs="NAD83")
  donors2 = sp::over(as(donors1, "Spatial"), as(nm_sf, "Spatial"))
  donors1$NAME = donors2$NAME
  donors2 = donors1 %>% group_by(NAME) %>%
    summarize(donors100k = sum(type == "donors100k"),
              donors200k = sum(type == "donors200k"),
              donors250k = sum(type == "donors250k"),
              donors500k = sum(type == "donors500k"),
              donorsleft = sum(type == "donorsleft"),
              donorsright = sum(type == "donorsright"),
              donorsswing = sum(type == "donorsswing"),
              dollars_ideo = weighted.mean(ideo[type=="donors200k"], w = dollars[type=="donors200k"]))
  
  firms2 = firms2 %>% full_join(sf::st_drop_geometry(donors2), by="NAME") %>% 
    mutate_if(is.numeric,coalesce,0)
  
  firms3 = nm_df %>% select(NAME, white,black,pop) %>% left_join(firms2, by="NAME")  %>%
    mutate_if(is.numeric,coalesce,0) %>%
    select(-geometry)
  
  rm(firms1)
  rm(firms2)
  
  ## Next, merge in the census data
  load("../data_clean/allprecincts_census_covars.RData")
  out = out %>% filter(state==stateabb)
  
  firms3 = merge(firms3, out, by="NAME") %>%
    select(-college,-homeowner,-unemp,-income_gini,-state)
  
  rm(out)
  
  ## Finally, get the dollar amounts
  load("../data_clean/donors_clean.RData")
  contribs2 = contribs2 %>% filter(most.recent.contributor.state ==stateabb)
  contribs2 = sf::st_as_sf(x = contribs2,
                           coords = c("most.recent.contributor.longitude",
                                      "most.recent.contributor.latitude"),
                           crs="NAD83")
  contribs3 = sp::over(as(contribs2, "Spatial"), as(nm_sf, "Spatial"))
  contribs2$NAME = contribs3$NAME
  contribs3 = contribs2 %>% group_by(NAME)%>%
    summarize(dollars = sum(cumul2012),
              dollars_weighted = sum(cumul2012*contributor.cfscore)) %>%
    data.table::data.table() %>%  
    select(-geometry)
  
  firms3 = firms3 %>% left_join(contribs3, by="NAME") %>%
    mutate_if(is.numeric,coalesce,0)
  
  rm(contribs2)
  rm(contribs3)
  
  print("Covariates merged")
  
  ## Combine precinct-level results into their relevant districts by partition
  
  test0 = merge(returns %>% filter(election == elec), firms3, by="NAME")
  test1 = ensembles %>% 
    left_join(test0, by="NAME", multiple = "all")
  
  maxdist = length(unique(test1$district))
  
  x2 = list()
  for(i in 1:maxdist){
    x2[[i]] =  test1 %>% filter(district==i) %>%
      group_by(election,partition, district, party) %>%
      summarize(firms = sum(firms),
                walmarts = sum(walmarts),
                airports = sum(airports),
                hospitals = sum(hospitals),
                pacfirms = sum(pacfirms),
                value = sum(value),
                donors100k = sum(donors100k),
                donors200k = sum(donors200k),
                donors250k = sum(donors250k),
                donors500k = sum(donors500k),
                donorsleft = sum(donorsleft),
                donorsright = sum(donorsright),
                donorsswing = sum(donorsswing),
                donors_nofirms = sum(donors200k[firms==0]),
                firms_nodonors = sum(firms[donors200k==0]),
                dollars = sum(dollars),
                dollars_weighted = sum(dollars_weighted),
                medincome =mean(medincome),
                topincome = sum(topincome),
                urban = mean(urban),
                white = sum(white),
                black = sum(black),
                pop = sum(pop))
    #print(i)
  }
  x2 = do.call(rbind, x2)
  
  print("X2 generated")

  
  ## Quantify the results
  
  comp <- x2 %>%
    filter(!is.na(election)) %>%
    group_by(election, partition, district) %>%
    filter(party == party[which.max(value)]) %>%
    group_by(election, partition, party) %>%
    summarize(seats = n(),
              firms = sum(firms),
              walmarts = sum(walmarts),
              hospitals = sum(hospitals),
              airports = sum(airports),
              pacfirms = sum(pacfirms),
              donors100k = sum(donors100k),
              donors200k = sum(donors200k),
              donors250k = sum(donors250k),
              donors500k = sum(donors500k),
              donorsleft = sum(donorsleft),
              donorsright = sum(donorsright),
              donorsswing = sum(donorsswing),
              donors_nofirms = sum(donors_nofirms),
              firms_nodonors = sum(firms_nodonors),
              dollars = sum(dollars),
              dollars_weighted = sum(dollars_weighted),
              medincome =mean(medincome),
              topincome = sum(topincome),
              urban = mean(urban),
              whitepct = sum(white)/sum(pop),
              blackpct = sum(black)/sum(pop)) %>%
    ungroup()
  
  print("Comp generated")
  
  ## Seat or firm distribution
  
  
  ds <- comp %>% filter(party == 'D') %>% filter(partition != "CD")
  
  x2 <<- x2
  comp <<- comp
  firms3 <<- firms3
  ds <<- ds
  
  if(save==TRUE){
    save.image(paste0("../data_clean/state_results/", stateabb, ".RData"))
  }
}


## How to pass dplyr column name??
get_hist = function(partit, elec, ds, var, xaxs_label,
                    conditional = FALSE,
                    partit2 = NULL,
                    save = TRUE){
  
  xlab_denom = as.numeric(firms3 %>% summarize(sum(.data[[var]])))

  ds2 =ds %>% 
    mutate(seats = seats,
           firms = .data[[var]]/xlab_denom) %>%
    filter(election == elec)
  
  if(conditional) {
    ds2 = ds2 %>% filter(seats == ds$seats[ds$partition==partit & ds$election == elec])
  }
  
  p3 = ds2 %>%
    ggplot() +
    geom_histogram(aes(firms, fill = election),binwidth=0.025,boundary=1) +
    xlim(c(0.1,1))+
    
    geom_vline(xintercept = ds2$firms[ds2$partition==partit & ds2$election == elec], 
               linetype = 3, 
               color = "grey",
               size = 1) +
    ggthemes::scale_fill_economist()+
    theme_minimal()+
    theme(legend.position = 'none',
          axis.text.y = element_blank())  + xlab("") + ylab(xaxs_label) 
  
  if(!is.null(partit2)){
    p3 = p3 + 
      geom_vline(xintercept = ds2$firms[ds2$partition==partit2 & ds2$election == elec], 
                 linetype = 2, 
                 color = "grey",
                 size = 1)
  }
  
  
  if(save == TRUE){
    fn = paste0("../output/bottom_hists/", stateabb, "_", var, ".rds")
    if(conditional==TRUE){
      fn = paste0("../output/bottom_hists/", stateabb, "_", var,"_cond.rds")
    }
    saveRDS(p3, file=fn)
  }
  
 return(p3)
  
}


## Function to generate bin2d plots
get_bin2d = function(partit, elec, var, xaxs_label,
                     ds, statename, save = TRUE,
                     partit2 = NULL){
  
  maxfirm = as.numeric(firms3 %>% summarize(sum(.data[[var]])))
  maxdist = length(unique(nm_df$CD))

  ds = ds %>% filter(election == elec) %>%
    mutate(outcome = !!sym(var))
  
  p5 = ds %>% ggplot(aes(x = outcome, y=seats+1)) +
    geom_bin2d(bins=c(maxfirm,maxdist+1)) +
    scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10") +
    scale_y_continuous(breaks=c(0:maxdist)+0.5,
                       labels=c(0:maxdist),
                       limits=c(0,maxdist+1),
                       expand = c(0,0))+
    scale_x_continuous(breaks=floor(seq(0,maxfirm, length.out = maxdist+1)),
                       limits=c(0,maxfirm),
                       expand=c(0,0))+
    geom_vline(xintercept = maxfirm/2, lwd=0.5, col="darkgrey") + 
    geom_hline(yintercept = ((maxdist+1)/2)+0.5, lwd=0.5, col="darkgrey") + 
    geom_abline(intercept=0.5,
                slope = (maxdist+1)/maxfirm,
                lwd=0.5, col="darkgrey")+
    labs(title = statename,
         x = xaxs_label,
         y = "Dem Seats",
         fill = "Simulated Plans") +
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank()) +
    annotate("text", y = ds$seats[ds$partition==partit] + 0.5,
             x = ds$outcome[ds$partition==partit], label="O", col="black")
  
  if(!is.null(partit2)){
    p5 = p5 + 
      annotate("text", y = ds$seats[ds$partition==partit2] + 0.5,
               x = ds$outcome[ds$partition==partit2], label="X", col="black")
  }
  
  if(save == TRUE){
    fn = paste0("../output/plots/", stateabb, "_", var,"_bin2d.pdf")
    ggsave(p5, filename= fn)
  }
  
  return(p5)
  
}

## Helper function to calculate the p-values for an arbitrary metric
pvals_fun = function(x, seats){
  ## Find the realized value of whatever metric indicated in x
  col_id = which(colnames(ds)==x)
  realized_val = unlist(ds[ds$partition == partit, col_id])
  ## How many plans have the same or fewer than the realized plan?
  samefewer = sum(ds[,col_id] <= realized_val)
  ## How many plans have the same as realized plan?
  samecount = sum(ds[,col_id] == realized_val)
  
  #Count of Simulated Plans with as many or Fewer as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  samefewer_sameseats = sum(ds[ds$seats == seats,col_id] <= realized_val) -1
  
  # Count of Simulated Plans with exact same # of PAC firms 
  # AND exact same # of seats as in the Enacted Plan
  samecount_sameseats = sum(ds$seats == seats & ds[,col_id] == realized_val) -1
  
  col_id2 = which(colnames(firms3)==x)
  totalcount = sum(firms3[,..col_id2])
  
  out = c(realized_val, samefewer,samecount,samefewer_sameseats,samecount_sameseats,totalcount)
  names(out) = paste0(x, c("_realized", "_samefewer", "_samecount", "_samefewer_sameseats", "_samecount_sameseats", "_totalcount"))
  return(out)
}


## Function to collect summary statistics for calculating pvalues
get_pvals_old = function(elec, partit, firms3, ds, stateabb, save = FALSE){
  
  ds = ds %>% filter(election == elec)
  
  seats = ds$seats[ds$partition==partit]
  firms = ds$firms[ds$partition==partit]
  
  # Max firms
  a = sum(firms3$firms)
  
  # Count of Simulated Plans with as many or Fewer Firms than Enacted Plan
  b = sum(ds$firms <= firms) -1
  
  # Count of Simulated Plans with Same # of Firms as Enacted Plan
  c = sum(ds$firms == firms)
  
  #Count of Simulated Plans with as many or Fewer Firms as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  d = sum(ds$firms[ds$seats == seats] <= firms) -1
  
  # Count of Simulated Plans with # of Seats in Enacted Plan
  e = sum(ds$seats == seats)
  
  # Count of Simulated Plans with exact same # of Firms 
  # AND exact same # of seats as in the Enacted Plan
  f = sum(ds$seats == seats & ds$firms == firms) - 1
  
  # Count of Simulated Plans with as many or Fewer Firms 
  # AND as many or Fewer Seats as Enacted Plans (i.e. visually below and to the left)
  g = sum(ds$seats <= seats & ds$firms <= firms) - 1
  
  # Count of Simulated Plans with as many or More Firms 
  # AND as many or More Seats as Enacted Plans (i.e. visually above and to the right)
  h = sum(ds$seats >= seats & ds$firms >= firms) - 1
  
  # Count of Simulated plans with either Exact Seats as enacted plan and fewer firms
  # OR Exact Firms as enacted plan and fewer seats
  i = sum(ds$seats == seats & ds$firms <= firms) + sum(ds$seats <= seats & ds$firms == firms) - 2
  
  # Count of Simulated plans with either Exact Seats as enacted plan and more firms
  # OR Exact Firms as enacted plan and more seats
  j = sum(ds$seats == seats & ds$firms >= firms) + sum(ds$seats >= seats & ds$firms == firms) - 2
  
  # Count of Simulated Plans with as many or Fewer Seats than Enacted Plan
  k = sum(ds$seats <= seats) - 1
  
  
  # Median simulated firms
  l = median(ds$firms)
  firms
  
  ## Max/min firms to dems
  m = max(ds$firms[ds$partition!= partit])
  n = min(ds$firms[ds$partition!= partit])
  
  
  ## Walmarts
  # Count of Simulated Plans with as many or Fewer Walmarts than Enacted Plan
  wals = ds$walmarts[ds$partition == partit]
  o = sum(ds$walmarts <= wals) -1
  
  # Count of Simulated Plans with Same # of Walmarts as Enacted Plan
  p = sum(ds$walmarts == wals)
  
  #Count of Simulated Plans with as many or Fewer Walmarts as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  q = sum(ds$walmarts[ds$seats == seats] <= wals) -1
  
  # Count of Simulated Plans with exact same # of Walmarts 
  # AND exact same # of seats as in the Enacted Plan
  r = sum(ds$seats == seats & ds$walmarts == wals) - 1
  
  ## Hospitals
  # Count of Simulated Plans with as many or Fewer Hospitals than Enacted Plan
  hosps = ds$hospitals[ds$partition == partit]
  s = sum(ds$hospitals <= hosps) -1
  
  # Count of Simulated Plans with Same # of Hospitals as Enacted Plan
  t = sum(ds$hospitals == hosps)
  
  #Count of Simulated Plans with as many or Fewer Hospitals as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  u = sum(ds$hospitals[ds$seats == seats] <= hosps) -1
  
  # Count of Simulated Plans with exact same # of Hospitals 
  # AND exact same # of seats as in the Enacted Plan
  v = sum(ds$seats == seats & ds$hospitals == hosps) - 1
  
  ## Airports
  # Count of Simulated Plans with as many or Fewer Airports than Enacted Plan
  airps = ds$airports[ds$partition == partit]
  w = sum(ds$airports <= airps) -1
  
  # Count of Simulated Plans with Same # of Airports as Enacted Plan
  x = sum(ds$airports == airps)
  
  #Count of Simulated Plans with as many or Fewer Airports as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  y = sum(ds$airports[ds$seats == seats] <= airps) -1
  
  # Count of Simulated Plans with exact same # of Airports 
  # AND exact same # of seats as in the Enacted Plan
  z = sum(ds$seats == seats & ds$airports == airps) - 1
  
  
  ## Pacfirms
  # Count of Simulated Plans with as many or Fewer PAC firms than Enacted Plan
  pacs = ds$pacfirms[ds$partition == partit]
  ww = sum(ds$pacfirms <= pacs) -1
  
  # Count of Simulated Plans with Same # of PAC firms as Enacted Plan
  xx = sum(ds$pacfirms == pacs)
  
  #Count of Simulated Plans with as many or Fewer PAC firms as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  yy = sum(ds$pacfirms[ds$seats == seats] <= pacs) -1
  
  # Count of Simulated Plans with exact same # of PAC firms 
  # AND exact same # of seats as in the Enacted Plan
  zz = sum(ds$seats == seats & ds$pacfirms == pacs) - 1
  
  pacfirms = sum(firms3$pacfirms)
  
  
  
  ## Donors
  # Count of Simulated Plans with as many or Fewer donors than Enacted Plan
  donors100k = ds$donors100k[ds$partition == partit]
  aa = sum(ds$donors100k <= donors100k)
  
  # Count of simulated plans with the same # of donors as enacted
  bb = sum(ds$donors100k == donors100k)
  
  #Count of Simulated Plans with as many or Fewer PAC firms as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  cc = sum(ds$donors100k[ds$seats == seats] <= donors100k) -1
  
  # Count of Simulated Plans with exact same # of PAC firms 
  # AND exact same # of seats as in the Enacted Plan
  dd = sum(ds$seats == seats & ds$donors100k == donors100k) - 1
  
  alldonors100k = sum(firms3$donors100k)
  
  
  donors250k = ds$donors250k[ds$partition == partit]
  aa2 = sum(ds$donors250k <= donors250k)
  
  # Count of simulated plans with the same # of donors as enacted
  bb2 = sum(ds$donors250k == donors250k)
  
  #Count of Simulated Plans with as many or Fewer PAC firms as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  cc2 = sum(ds$donors250k[ds$seats == seats] <= donors250k) -1
  
  # Count of Simulated Plans with exact same # of PAC firms 
  # AND exact same # of seats as in the Enacted Plan
  dd2 = sum(ds$seats == seats & ds$donors250k == donors250k) - 1
  
  alldonors250k = sum(firms3$donors250k)
  
  
  donors500k = ds$donors500k[ds$partition == partit]
  aa3 = sum(ds$donors500k <= donors500k)
  
  # Count of simulated plans with the same # of donors as enacted
  bb3 = sum(ds$donors500k == donors500k)
  
  #Count of Simulated Plans with as many or Fewer PAC firms as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  cc3 = sum(ds$donors500k[ds$seats == seats] <= donors500k) -1
  
  # Count of Simulated Plans with exact same # of PAC firms 
  # AND exact same # of seats as in the Enacted Plan
  dd3 = sum(ds$seats == seats & ds$donors500k == donors500k) - 1
  
  alldonors500k = sum(firms3$donors500k)
  
  
  ### This is the set of big donors who have donated in the last 2 cycles
  donors200k = ds$donors200k[ds$partition == partit]
  aa4 = sum(ds$donors200k <= donors200k)
  
  # Count of simulated plans with the same # of donors as enacted
  bb4 = sum(ds$donors200k == donors200k)
  
  #Count of Simulated Plans with as many or Fewer PAC firms as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  cc4 = sum(ds$donors200k[ds$seats == seats] <= donors200k) -1
  
  # Count of Simulated Plans with exact same # of PAC firms 
  # AND exact same # of seats as in the Enacted Plan
  dd4 = sum(ds$seats == seats & ds$donors200k == donors200k) - 1
  
  alldonors200k = sum(firms3$donors200k)
  
  
  
  donors_nofirms = ds$donors_nofirms[ds$partition == partit]
  aa5 = sum(ds$donors_nofirms <= donors_nofirms)
  
  # Count of simulated plans with the same # of donors as enacted
  bb5 = sum(ds$donors_nofirms == donors_nofirms)
  
  #Count of Simulated Plans with as many or Fewer PAC firms as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  cc5 = sum(ds$donors_nofirms[ds$seats == seats] <= donors_nofirms) -1

  # Count of Simulated Plans with exact same # of PAC firms 
  # AND exact same # of seats as in the Enacted Plan
  dd5 = sum(ds$seats == seats & ds$donors_nofirms == donors_nofirms) - 1
  
  alldonors_nofirms = sum(firms3$donors_nofirms)
  
  
  
  firms_nodonors = ds$firms_nodonors[ds$partition == partit]
  aa6 = sum(ds$firms_nodonors <= firms_nodonors)
  
  # Count of simulated plans with the same # of donors as enacted
  bb6 = sum(ds$firms_nodonors == firms_nodonors)
  
  #Count of Simulated Plans with as many or Fewer PAC firms as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  cc6 = sum(ds$firms_nodonors[ds$seats == seats] <= firms_nodonors) -1
  
  # Count of Simulated Plans with exact same # of PAC firms 
  # AND exact same # of seats as in the Enacted Plan
  dd6 = sum(ds$seats == seats & ds$firms_nodonors == firms_nodonors) - 1
  
  allfirms_nodonors = sum(firms3$firms_nodonors)
  
  
  
  
  ## Donor dollars
  # Count of Simulated Plans with as many or Fewer donor dollars than Enacted Plan
  dollars = ds$dollars[ds$partition == partit]
  ee = sum(ds$dollars <= dollars)
  
  # Count of simulated plans with the same # of donors as enacted
  ff = sum(ds$dollars == dollars)
  
  #Count of Simulated Plans with as many or Fewer PAC firms as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  gg = sum(ds$dollars[ds$seats == seats] <= dollars) -1
  
  # Count of Simulated Plans with exact same # of PAC firms 
  # AND exact same # of seats as in the Enacted Plan
  hh = sum(ds$seats == seats & ds$dollars == dollars) - 1
  
  alldollars = sum(firms3$dollars)
  
  
  ## Confounders
  # Count of Simulated Plans with as many or Fewer donor dollars than Enacted Plan
  medincome = ds$medincome[ds$partition == partit]
  topincome = ds$topincome[ds$partition == partit]
  urban = ds$urban[ds$partition==partit]
  whitepct = ds$whitepct[ds$partition==partit]
  blackpct = ds$blackpct[ds$partition==partit]
  
  c1a = sum(ds$medincome <= medincome)
  c2a = sum(ds$urban <= urban)
  c3a = sum(ds$whitepct <= whitepct)
  c4a = sum(ds$blackpct <= blackpct)
  c5a = sum(ds$topincome <= topincome)
  
  # Count of simulated plans with the same # of donors as enacted
  c1b = sum(ds$medincome == medincome)
  c2b = sum(ds$urban == urban)
  c3b = sum(ds$whitepct == whitepct)
  c4b = sum(ds$blackpct == blackpct)
  c5b = sum(ds$topincome == topincome)
  
  #Count of Simulated Plans with as many or Fewer PAC firms as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  c1c = sum(ds$medincome[ds$seats == seats] <= medincome) -1
  c2c = sum(ds$urban[ds$seats == seats] <= urban) -1
  c3c = sum(ds$whitepct[ds$seats == seats] <= whitepct) -1
  c4c = sum(ds$blackpct[ds$seats == seats] <= blackpct) -1
  c5c = sum(ds$topincome[ds$seats == seats] <= topincome) -1
  
  # Count of Simulated Plans with exact same # of PAC firms 
  # AND exact same # of seats as in the Enacted Plan
  c1d = sum(ds$seats == seats & ds$medincome == medincome) - 1
  c2d = sum(ds$seats == seats & ds$urban == urban) - 1
  c3d = sum(ds$seats == seats & ds$whitepct == whitepct) - 1
  c4d = sum(ds$seats == seats & ds$blackpct == blackpct) - 1
  c5d = sum(ds$seats == seats & ds$topincome == topincome) - 1
  
  medinc = mean(firms3$medincome)
  urban = mean(firms3$urban)
  whitepct = sum(firms3$white)/sum(firms3$pop)
  blackpct = sum(firms3$black)/sum(firms3$pop)
  topincome = sum(firms3$topincome)/sum(firms3$pop)
  
  results = c(a,b,c,d,e,f,g,h,i,j,k,firms,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,pacfirms,ww,xx,yy,zz, # stuff results
              aa,bb,cc,dd,alldonors100k,aa2,bb2,cc2,dd2,alldonors250k,
              aa3,bb3,cc3,dd3,alldonors500k,ee,ff,gg,hh,alldollars, # donor results
              aa4,bb4,cc4,dd4,alldonors200k, aa5,bb5,cc5,dd5,alldonors_nofirms,
              aa6,bb6,cc6,dd6,allfirms_nodonors, # donor results 2
              c1a,c1b,c1c,c1d,medinc,c2a,c2b,c2c,c3d,urban,
              c3a,c3b,c3c,c3d,whitepct,c4a,c4b,c4c,c4d,blackpct,
              c5a,c5b,c5c,c5d,topincome) # confounder results
  
  if(save == TRUE){
    save(results, file=paste0("../output/results/", stateabb, "_results.RData"))
  }
  
  return(results)
  
}


## Function to collect summary statistics for calculating pvalues
get_pvals = function(elec, partit, firms3, ds, stateabb, save = FALSE){
  
  ds = ds %>% filter(election == elec)
  
  seats = ds$seats[ds$partition==partit]
  firms = ds$firms[ds$partition==partit]
  
  # Max firms
  a = sum(firms3$firms)
  
  # Count of Simulated Plans with as many or Fewer Firms than Enacted Plan
  b = sum(ds$firms <= firms) -1
  
  # Count of Simulated Plans with Same # of Firms as Enacted Plan
  c = sum(ds$firms == firms)
  
  #Count of Simulated Plans with as many or Fewer Firms as Enacted Plans
  # fixing Seats to the exact count in the Enacted Plan
  d = sum(ds$firms[ds$seats == seats] <= firms) -1
  
  # Count of Simulated Plans with # of Seats in Enacted Plan
  e = sum(ds$seats == seats)
  
  # Count of Simulated Plans with exact same # of Firms 
  # AND exact same # of seats as in the Enacted Plan
  f = sum(ds$seats == seats & ds$firms == firms) - 1
  
  # Count of Simulated Plans with as many or Fewer Firms 
  # AND as many or Fewer Seats as Enacted Plans (i.e. visually below and to the left)
  g = sum(ds$seats <= seats & ds$firms <= firms) - 1
  
  # Count of Simulated Plans with as many or More Firms 
  # AND as many or More Seats as Enacted Plans (i.e. visually above and to the right)
  h = sum(ds$seats >= seats & ds$firms >= firms) - 1
  
  # Count of Simulated plans with either Exact Seats as enacted plan and fewer firms
  # OR Exact Firms as enacted plan and fewer seats
  i = sum(ds$seats == seats & ds$firms <= firms) + sum(ds$seats <= seats & ds$firms == firms) - 2
  
  # Count of Simulated plans with either Exact Seats as enacted plan and more firms
  # OR Exact Firms as enacted plan and more seats
  j = sum(ds$seats == seats & ds$firms >= firms) + sum(ds$seats >= seats & ds$firms == firms) - 2
  
  # Count of Simulated Plans with as many or Fewer Seats than Enacted Plan
  k = sum(ds$seats <= seats) - 1
  
  
  # Median simulated firms
  l = median(ds$firms)
  firms
  
  ## Max/min firms to dems
  m = max(ds$firms[ds$partition!= partit])
  n = min(ds$firms[ds$partition!= partit])
  
  firmvec = c(a,b,c,d,e,f,g,h,i,j,k,firms,l,m,n)
  names(firmvec) = c("maxfirms", "fewerfirms", "samefirms",
                     "fewerfirms_sameseats", "sameseats",
                     "samefirms_sameseats", "fewerfirms_fewerseats",
                     "morefirms_moreseats", "below_or_left",
                     "above_or_right", "fewerseats", "enactedfirms",
                     "medianfirms", "dem_maxfirms", "dem_minfirms")
  
  wals = pvals_fun("walmarts", seats )
  hosps = pvals_fun("hospitals", seats)
  airps = pvals_fun("airports", seats)
  pacs = pvals_fun("pacfirms", seats)
  donors200k = pvals_fun("donors200k", seats)
  donors_nofirms = pvals_fun("donors_nofirms", seats)
  firms_nodonors = pvals_fun("firms_nodonors", seats)
  donorsleft = pvals_fun("donorsleft", seats)
  donorsright = pvals_fun("donorsright", seats)
  donorsswing = pvals_fun("donorsswing", seats)
  dollars = pvals_fun("dollars", seats)
  dollars_weighted = pvals_fun("dollars_weighted", seats)
  medincome = pvals_fun("medincome", seats)
  topincome = pvals_fun("topincome", seats)
  urban = pvals_fun("urban", seats)
  whitepct = pvals_fun("whitepct", seats)
  blackpct = pvals_fun("blackpct", seats)

  
  results = c(firmvec, #firms
              wals,hosps,airps,pacs, #other infra
              donors200k,donorsleft,donorsright,donorsswing, #donors
              donors_nofirms,firms_nodonors, #separating donors and firms
              dollars, dollars_weighted, # dollars
              medincome,topincome,urban,whitepct,blackpct) # confounder results
  
  results["donors_nofirms_totalcount"] = sum(firms3$donors200k[firms3$firms==0])
  results["firms_nodonors_totalcount"] = sum(firms3$firms[firms3$donors==0])
  
  if(save == TRUE){
    save(results, file=paste0("../output/results/", stateabb, "_results2.RData"))
  }
  
  return(results)
  
}


## Function to get the precinct-level unusualness stats
get_unusualness = function(elec, partit, stateabb, save = TRUE){
  
  sf::sf_use_s2(FALSE)
  ## I think I need the x2 and ensembles objects
  
  ens1 <- x2 %>%
    filter(election == elec) %>%
    group_by(partition, district) %>%
    mutate(margin = max(value)/(max(value)+min(value))) %>%
    filter(party == party[which.max(value)])%>% 
    select(partition, district, party) %>%
    right_join(ensembles)
  
  ens2012 <- ens1 %>% filter(partition == partit)
  
  ens2 <- ens1 %>%
    group_by(NAME) %>%
    summarize(prop.d = mean(party == "D")) %>%
    left_join(ens2012)
  
  ## Merge the "unusualness" outcome with precinct data
  ens2b <- ens2 %>%
    left_join(nm_df, by="NAME")
  
  dat2 = ens2b %>% left_join(firms3, by = "NAME")
  if(save){
    write.csv(dat2, file=paste0("../output/unusualness/", stateabb, "_unusualness.csv"))
  }

  return(dat2)
}
