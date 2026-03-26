## Preamble --------------------------------------------------------------------
library(tidyverse)
library(foreign)
library(stargazer)
library(janitor)
library(xtable)

## create subfolder for council results:
if(!dir.exists("councils")){
  dir.create("councils")
}

# lead and lag functions that account for missing years
lag.new <- function(x, n = 1L, along_with){
  index <- match(along_with - n, along_with, incomparable = NA)
  out <- x[index]
  attributes(out) <- attributes(x)
  out
}

lead.new <- function(x, n = 1L, along_with){
  index <- match(along_with + n, along_with, incomparable = NA)
  out <- x[index]
  attributes(out) <- attributes(x)
  out
}


## Read data -------------------------------------------------------------------
data_indfin <- read_rds("elecs_councils_allDVs_230919.rds")

## add CBPS delta 2/3 avg outcomes:
data_indfin <- data_indfin %>%
  rowwise() %>%
  mutate(
    total_bldgs_delta23avg = mean(c(total_bldgs_delta2, total_bldgs_delta3), na.rm=T),
    total_bldgs_multi_delta23avg = mean(c(total_bldgs_multi_delta2, total_bldgs_multi_delta3), na.rm=T),
    total_bldgs_single_delta23avg = mean(c(total_bldgs_single_delta2, total_bldgs_single_delta3), na.rm=T),
    
    total_units_delta23avg = mean(c(total_units_delta2, total_units_delta3), na.rm=T),
    total_units_multi_delta23avg = mean(c(total_units_multi_delta2, total_units_multi_delta3), na.rm=T),
    total_units_single_delta23avg = mean(c(total_units_single_delta2, total_units_single_delta3), na.rm=T),
    
    total_bldgs_ln_delta23avg = mean(c(total_bldgs_ln_delta2, total_bldgs_ln_delta3), na.rm=T),
    total_bldgs_multi_ln_delta23avg = mean(c(total_bldgs_multi_ln_delta2, total_bldgs_multi_ln_delta3), na.rm=T),
    total_bldgs_single_ln_delta23avg = mean(c(total_bldgs_single_ln_delta2, total_bldgs_single_ln_delta3), na.rm=T),
    
    total_units_delta23avg = mean(c(total_units_delta2, total_units_delta3), na.rm=T),
    total_units_multi_delta23avg = mean(c(total_units_multi_delta2, total_units_multi_delta3), na.rm=T),
    total_units_single_delta23avg = mean(c(total_units_single_delta2, total_units_single_delta3), na.rm=T),
    
    total_units_ln_delta23avg = mean(c(total_units_ln_delta2, total_units_ln_delta3), na.rm=T),
    total_units_multi_ln_delta23avg = mean(c(total_units_multi_ln_delta2, total_units_multi_ln_delta3), na.rm=T),
    total_units_single_ln_delta23avg = mean(c(total_units_single_ln_delta2, total_units_single_ln_delta3), na.rm=T),
    
    total_units_ln_delta234avg = mean(c(total_units_ln_delta2, total_units_ln_delta3, total_units_ln_delta4), na.rm=T),
    total_units_multi_ln_delta234avg = mean(c(total_units_multi_ln_delta2, total_units_multi_ln_delta3, total_units_multi_ln_delta4), na.rm=T),
    total_units_single_ln_delta234avg = mean(c(total_units_single_ln_delta2, total_units_single_ln_delta3, total_units_single_ln_delta4), na.rm=T),
    
    
    total_units_ln_delta1234avg = mean(c(total_units_ln_delta1,total_units_ln_delta2, total_units_ln_delta3, total_units_ln_delta4), na.rm=T),
    total_units_multi_ln_delta1234avg = mean(c(total_units_multi_ln_delta1, total_units_multi_ln_delta2, total_units_multi_ln_delta3, total_units_multi_ln_delta4), na.rm=T),
    total_units_single_ln_delta1234avg = mean(c(total_units_single_ln_delta1,total_units_single_ln_delta2, total_units_single_ln_delta3, total_units_single_ln_delta4), na.rm=T),
    
    total_units_ln_lead234avg = mean(c(total_units_ln_lead2, total_units_ln_lead3, total_units_ln_lead4), na.rm=T),
    total_units_multi_ln_lead234avg = mean(c(total_units_multi_ln_lead2, total_units_multi_ln_lead3, total_units_multi_ln_lead4), na.rm=T),
    total_units_single_ln_lead234avg = mean(c(total_units_single_ln_lead2, total_units_single_ln_lead3, total_units_single_ln_lead4), na.rm=T),
    
    
    total_units_ln_deltaterm234avg = (total_units_ln_lead234avg-total_units_ln_lagterm4avg),
    total_units_multi_ln_deltaterm234avg = (total_units_multi_ln_lead234avg-total_units_multi_ln_lagterm4avg),
    total_units_single_ln_deltaterm234avg = (total_units_single_ln_lead234avg-total_units_single_ln_lagterm4avg),
    
    ratio_bldgs_multisingle_delta23avg = mean(c(ratio_bldgs_multisingle_delta2, ratio_bldgs_multisingle_delta3), na.rm=T),
    ratio_units_multisingle_delta23avg = mean(c(ratio_units_multisingle_delta2, ratio_units_multisingle_delta3), na.rm=T),
    ratio_units_multisingle_lead234avg = mean(c(ratio_units_multisingle_lead2, ratio_units_multisingle_lead3, ratio_units_multisingle_lead4), na.rm=T),
    ratio_units_multisingle_delta234avg = mean(c(ratio_units_multisingle_delta2, ratio_units_multisingle_delta3, ratio_units_multisingle_delta4), na.rm=T),
    ratio_units_multisingle_delta1234avg = mean(c(ratio_units_multisingle_delta1, ratio_units_multisingle_delta2, ratio_units_multisingle_delta3, ratio_units_multisingle_delta4), na.rm=T),
    ratio_units_multisingle_deltaterm234avg = ratio_units_multisingle_lead234avg-ratio_units_multisingle_lagterm4avg,
    ratio_bldgs_multisingle_lead23avg = mean(c(ratio_bldgs_multisingle_lead2, ratio_bldgs_multisingle_lead3), na.rm=T),
    ratio_units_multisingle_lead23avg = mean(c(ratio_units_multisingle_lead2, ratio_units_multisingle_lead3), na.rm=T)
  )

## add CBPS delta term 4-year avgs:
data_indfin <- data_indfin %>%
  rowwise() %>%
  mutate(
    total_bldgs_ln_delta14avg = mean(c(total_bldgs_ln_delta1,total_bldgs_ln_delta2, total_bldgs_ln_delta3,total_bldgs_ln_delta4), na.rm=T),
    total_bldgs_multi_ln_delta14avg = mean(c(total_bldgs_multi_ln_delta1,total_bldgs_multi_ln_delta2, total_bldgs_multi_ln_delta3,total_bldgs_multi_ln_delta4), na.rm=T),
    total_bldgs_single_ln_delta14avg = mean(c(total_bldgs_single_ln_delta1,total_bldgs_single_ln_delta2, total_bldgs_single_ln_delta3,total_bldgs_single_ln_delta4), na.rm=T),
    total_units_ln_delta14avg = mean(c(total_units_ln_delta1,total_units_ln_delta2, total_units_ln_delta3,total_units_ln_delta4), na.rm=T),
    total_units_multi_ln_delta14avg = mean(c(total_units_multi_ln_delta1,total_units_multi_ln_delta2, total_units_multi_ln_delta3,total_units_multi_ln_delta4), na.rm=T),
    total_units_single_ln_delta14avg = mean(c(total_units_single_ln_delta1,total_units_single_ln_delta2, total_units_single_ln_delta3,total_units_single_ln_delta4), na.rm=T),
    ratio_bldgs_multisingle_delta14avg = mean(c(ratio_bldgs_multisingle_delta1,ratio_bldgs_multisingle_delta2, ratio_bldgs_multisingle_delta3,ratio_bldgs_multisingle_delta4), na.rm=T),
    ratio_units_multisingle_delta14avg = mean(c(ratio_units_multisingle_delta1,ratio_units_multisingle_delta2, ratio_units_multisingle_delta3,ratio_units_multisingle_delta4), na.rm=T),
    ratio_bldgs_multisingle_lead23avg = mean(c(ratio_bldgs_multisingle_lead1,ratio_bldgs_multisingle_lead2, ratio_bldgs_multisingle_lead3,ratio_bldgs_multisingle_lead4), na.rm=T),
    ratio_units_multisingle_lead23avg = mean(c(ratio_units_multisingle_lead1,ratio_units_multisingle_lead2, ratio_units_multisingle_lead3,ratio_units_multisingle_lead4), na.rm=T)
  )

## add per capita measures:
data_indfin <- data_indfin %>%
  mutate(total_units_pc = (total_units/(population_est/100000)),
         total_units_single_pc = (total_units_single/(population_est/100000)),
         total_units_multi_pc = (total_units_multi/(population_est/100000)),
         total_bldgs_pc = (total_bldgs/(population_est/100000)),
         total_bldgs_single_pc = (total_bldgs_single/(population_est/100000)),
         total_bldgs_multi_pc = (total_bldgs_multi/(population_est/100000)),
         
         total_units_pc_lead2 = (total_units_lead2/(population_est/100000)),
         total_units_single_pc_lead2 = (total_units_single_lead2/(population_est/100000)),
         total_units_multi_pc_lead2 = (total_units_multi_lead2/(population_est/100000)),
         total_bldgs_pc_lead2 = (total_bldgs_lead2/(population_est/100000)),
         total_bldgs_single_pc_lead2 = (total_bldgs_single_lead2/(population_est/100000)),
         total_bldgs_multi_pc_lead2 = (total_bldgs_multi_lead2/(population_est/100000)),
         
         total_units_pc_lead3 = (total_units_lead3/(population_est/100000)),
         total_units_single_pc_lead3 = (total_units_single_lead3/(population_est/100000)),
         total_units_multi_pc_lead3 = (total_units_multi_lead3/(population_est/100000)),
         total_bldgs_pc_lead3 = (total_bldgs_lead3/(population_est/100000)),
         total_bldgs_single_pc_lead3 = (total_bldgs_single_lead3/(population_est/100000)),
         total_bldgs_multi_pc_lead3 = (total_bldgs_multi_lead3/(population_est/100000)),
         
         total_units_pc_delta2 = (total_units_pc_lead2 - total_units_pc),
         total_units_single_pc_delta2 = (total_units_single_pc_lead2-total_units_single_pc),
         total_units_multi_pc_delta2 = (total_units_multi_pc_lead2 - total_units_multi_pc),
         total_bldgs_pc_delta2 = (total_bldgs_pc_lead2-total_bldgs_pc),
         total_bldgs_single_pc_delta2 = (total_bldgs_single_pc_lead2-total_bldgs_single_pc),
         total_bldgs_multi_pc_delta2 = (total_bldgs_multi_pc_lead2-total_bldgs_multi_pc),
         
         total_units_pc_delta3 = (total_units_pc_lead3 - total_units_pc),
         total_units_single_pc_delta3 = (total_units_single_pc_lead3-total_units_single_pc),
         total_units_multi_pc_delta3 = (total_units_multi_pc_lead3 - total_units_multi_pc),
         total_bldgs_pc_delta3 = (total_bldgs_pc_lead3-total_bldgs_pc),
         total_bldgs_single_pc_delta3 = (total_bldgs_single_pc_lead3-total_bldgs_single_pc),
         total_bldgs_multi_pc_delta3 = (total_bldgs_multi_pc_lead3-total_bldgs_multi_pc)
  )

data_indfin <- data_indfin %>%
  rowwise() %>%
  mutate(total_units_pc_delta23avg = mean(c(total_units_pc_delta2, total_units_pc_delta3), na.rm=T),
         total_units_single_pc_delta23avg = mean(c(total_units_single_pc_delta2, total_units_single_pc_delta3), na.rm=T),
         total_units_multi_pc_delta23avg = mean(c(total_units_multi_pc_delta2, total_units_multi_pc_delta3), na.rm=T),
         total_bldgs_pc_delta23avg = mean(c(total_bldgs_pc_delta2, total_bldgs_pc_delta3), na.rm=T),
         total_bldgs_single_pc_delta23avg = mean(c(total_bldgs_single_pc_delta2, total_bldgs_single_pc_delta3), na.rm=T),
         total_bldgs_multi_pc_delta23avg = mean(c(total_bldgs_multi_pc_delta2, total_bldgs_multi_pc_delta3), na.rm=T)
  )

## alternative ln base values:
data_indfin <- data_indfin %>%
  mutate(total_units_ln01 = log(total_units+0.1),
         total_units_single_ln01 = log(total_units_single+0.1),
         total_units_multi_ln01 = log(total_units_multi+0.1),
         total_bldgs_ln01 = log(total_bldgs+0.1),
         total_bldgs_single_ln01 = log(total_bldgs_single+0.1),
         total_bldgs_multi_ln01 = log(total_bldgs_multi+0.1),
         
         total_units_ln01_lead2 = log(total_units_lead2+0.1),
         total_units_single_ln01_lead2 = log(total_units_single_lead2+0.1),
         total_units_multi_ln01_lead2 = log(total_units_multi_lead2+0.1),
         total_bldgs_ln01_lead2 = log(total_bldgs_lead2+0.1),
         total_bldgs_single_ln01_lead2 = log(total_bldgs_single_lead2+0.1),
         total_bldgs_multi_ln01_lead2 = log(total_bldgs_multi_lead2+0.1),
         
         total_units_ln01_lead3 = log(total_units_lead3+0.1),
         total_units_single_ln01_lead3 = log(total_units_single_lead3+0.1),
         total_units_multi_ln01_lead3 = log(total_units_multi_lead3+0.1),
         total_bldgs_ln01_lead3 = log(total_bldgs_lead3+0.1),
         total_bldgs_single_ln01_lead3 = log(total_bldgs_single_lead3+0.1),
         total_bldgs_multi_ln01_lead3 = log(total_bldgs_multi_lead3+0.1),
         
         total_units_ln01_delta2 = (total_units_ln01_lead2 - total_units_ln01),
         total_units_single_ln01_delta2 = (total_units_single_ln01_lead2-total_units_single_ln01),
         total_units_multi_ln01_delta2 = (total_units_multi_ln01_lead2 - total_units_multi_ln01),
         total_bldgs_ln01_delta2 = (total_bldgs_ln01_lead2-total_bldgs_ln01),
         total_bldgs_single_ln01_delta2 = (total_bldgs_single_ln01_lead2-total_bldgs_single_ln01),
         total_bldgs_multi_ln01_delta2 = (total_bldgs_multi_ln01_lead2-total_bldgs_multi_ln01),
         
         total_units_ln01_delta3 = (total_units_ln01_lead3 - total_units_ln01),
         total_units_single_ln01_delta3 = (total_units_single_ln01_lead3-total_units_single_ln01),
         total_units_multi_ln01_delta3 = (total_units_multi_ln01_lead3 - total_units_multi_ln01),
         total_bldgs_ln01_delta3 = (total_bldgs_ln01_lead3-total_bldgs_ln01),
         total_bldgs_single_ln01_delta3 = (total_bldgs_single_ln01_lead3-total_bldgs_single_ln01),
         total_bldgs_multi_ln01_delta3 = (total_bldgs_multi_ln01_lead3-total_bldgs_multi_ln01)
  )

data_indfin <- data_indfin %>%
  rowwise() %>%
  mutate(total_units_ln01_delta23avg = mean(c(total_units_ln01_delta2, total_units_ln01_delta3), na.rm=T),
         total_units_single_ln01_delta23avg = mean(c(total_units_single_ln01_delta2, total_units_single_ln01_delta3), na.rm=T),
         total_units_multi_ln01_delta23avg = mean(c(total_units_multi_ln01_delta2, total_units_multi_ln01_delta3), na.rm=T),
         total_bldgs_ln01_delta23avg = mean(c(total_bldgs_ln01_delta2, total_bldgs_ln01_delta3), na.rm=T),
         total_bldgs_single_ln01_delta23avg = mean(c(total_bldgs_single_ln01_delta2, total_bldgs_single_ln01_delta3), na.rm=T),
         total_bldgs_multi_ln01_delta23avg = mean(c(total_bldgs_multi_ln01_delta2, total_bldgs_multi_ln01_delta3), na.rm=T)
  )


## Select relevant data --------------------------------------------------------

# create coverage of cities table:
pop <- read_csv("places_2020_population.csv")

data_allcities <- pop %>%
  summarize(n_cities = n(),
            min_population_2020 = min(population_2020,na.rm=T),
            max_population_2020 = max(population_2020,na.rm=T),
            avg_population_2020 = mean(population_2020,na.rm=T),
            total_population = sum(population_2020,na.rm=T)
  )
table_allcities <- data.frame(subset="All cities",data_allcities)

data_universe <- pop %>%
  filter(population_2020>=75000) %>%
  summarize(n_cities = n(),
            min_population_2020 = min(population_2020,na.rm=T),
            max_population_2020 = max(population_2020,na.rm=T),
            avg_population_2020 = mean(population_2020,na.rm=T),
            total_population = sum(population_2020,na.rm=T)
  )
table_bigcities <- data.frame(subset="Medium and large cities (target universe)",data_universe)


data_indfin_unique <- data_indfin %>%
  filter(year>=1990) %>%
  group_by(place_fips,population_2020) %>%  
  summarize(n=n()) %>%
  ungroup() %>%
  summarize(n_elecs=sum(n),
            n_cities = n(),
            min_population_2020 = min(population_2020,na.rm=T),
            max_population_2020 = max(population_2020,na.rm=T),
            avg_population_2020 = mean(population_2020,na.rm=T),
            total_population = sum(population_2020,na.rm=T)
  )
table_elecs_collected_post1990 <- data.frame(subset="All data collected",data_indfin_unique)



## output for DID:
data_housing <- data_indfin %>%
  select(place_fips,abb,city, district, year, n_winners,votes_win,pid_final_win,lastname_win,firstname_win,votes_lose,pid_final_lose,lastname_lose,firstname_lose,demshare,
         population_est,population_2020,
         fog, initiative, partisan, referendum,
         contains("bldgs"),contains("units"))

write_rds(data_housing,file = "data_councils_housing_fordid.rds",compress = "gz")


data_indfin <- data_indfin %>%
  filter(population_2020>=75000)

nrow(data_indfin) # 16902 elections
length(unique(data_indfin$place_fips)) # in 380 cities

writeLines(prettyNum(nrow(data_indfin[data_indfin$year>=1990,]),big.mark = ","),"councils/n_elecs_post1990.tex",sep = "%")
writeLines(prettyNum(length(unique(data_indfin$place_fips[data_indfin$year>=1990])),big.mark = ","),"councils/n_cities_post1990.tex",sep = "%")


data_indfin_unique <- data_indfin %>%
  filter(year>=1990) %>%
  group_by(place_fips,population_2020) %>%  
  summarize(n=n()) %>%
  ungroup() %>%
  summarize(n_elecs=sum(n),
            n_cities = n(),
            min_population_2020 = min(population_2020,na.rm=T),
            max_population_2020 = max(population_2020,na.rm=T),
            avg_population_2020 = mean(population_2020,na.rm=T),
            total_population = sum(population_2020,na.rm=T)
  )
table_elecs_bigcities_post1990 <- data.frame(subset="Medium and large cities in elections dataset",data_indfin_unique)



## Create weighting variable:
data_indfin_summ <- data_indfin %>%
  group_by(place_fips) %>%
  distinct(district,.keep_all = T) %>%
  summarize(seats_tot = sum(n_winners)) # this should deal with SMDs and MMDs

mean_citycouncil_size <- mean(data_indfin_summ$seats_tot)
(median_citycouncil_size <- median(data_indfin_summ$seats_tot)) # 8
writeLines(as.character(median_citycouncil_size),"councils/median_councilsize.tex",sep = "%")
writeLines(as.character(max(data_indfin_summ$seats_tot)),"councils/max_councilsize.tex",sep = "%")
data_indfin_summ %>% filter(seats_tot == max(data_indfin_summ$seats_tot)) %>% select(place_fips)

# merge back to elections data:
data_indfin$seats_tot <- data_indfin_summ$seats_tot[match(data_indfin$place_fips,data_indfin_summ$place_fips)]

data_indfin <- data_indfin %>%
  mutate(weight = mean_citycouncil_size/seats_tot)

## create cluster variable:
data_indfin <- data_indfin %>%
  mutate(cluster = paste(place_fips,year,sep="_"))


# select only dem-rep races:
data_2p <- data_indfin %>%
  filter(!is.na(demshare))
dim(data_2p) # 4043 two-party elecs
length(unique(data_2p$place_fips)) # in 348 cities


# to enable better plots:
data_2p <- data_2p %>%
  mutate(demshare_bin = cut(demshare,breaks=seq(0,1,by=0.01)),
         mid = as.numeric(demshare_bin)/100-0.005)
table(data_2p$demshare_bin)
table(data_2p$mid)

# write_rds(data_2p,file = "data_2p.rds")

data_2p_post1990 <- data_2p %>%
  filter(year>=1990)

write_rds(data_2p_post1990,file = "data_councils_2p_post1990.rds",compress = "gz")

# save for table later
data_2p_post1990_unique <- data_2p_post1990 %>%
  group_by(place_fips,population_2020) %>%  
  summarize(n=n()) %>%
  ungroup() %>%
  summarize(n_elecs=sum(n),
            n_cities = n(),
            min_population_2020 = min(population_2020,na.rm=T),
            max_population_2020 = max(population_2020,na.rm=T),
            avg_population_2020 = mean(population_2020,na.rm=T),
            total_population = sum(population_2020,na.rm=T)
  )
table_elecs_2p_post1990 <- data.frame(subset="Two-party contested elections in dataset",data_2p_post1990_unique)

data_2p_close_post1990_unique <- data_2p_post1990 %>%
  filter(demshare>=0.4 & demshare<=0.6) %>%
  group_by(place_fips,population_2020) %>%  
  summarize(n=n()) %>%
  ungroup() %>%
  summarize(n_elecs=sum(n),
            n_cities = n(),
            min_population_2020 = min(population_2020,na.rm=T),
            max_population_2020 = max(population_2020,na.rm=T),
            avg_population_2020 = mean(population_2020,na.rm=T),
            total_population = sum(population_2020,na.rm=T)
  )
table_elecs_close_2p_post1990 <- data.frame(subset="Two-party close elections in dataset",data_2p_close_post1990_unique)


## data coverage table:
coverage_table <- bind_rows(table_allcities,table_bigcities) %>%
  bind_rows(table_elecs_bigcities_post1990) %>%
  bind_rows(table_elecs_2p_post1990) %>%
  bind_rows(table_elecs_close_2p_post1990) %>%
  mutate(perc_target_uni = round(total_population/table_bigcities$total_population*100,0),
         perc_target_uni = ifelse(perc_target_uni>100,NA,perc_target_uni)) %>%
  mutate_at(vars(-subset),
            .funs = function(x)prettyNum(round(x,digits = 0),big.mark=",")) %>%
  mutate_at(vars(-subset),
            .funs = function(x)ifelse(x=="NA","",x)) %>%
  select(Subset = subset,`N Cities` = n_cities,`N Elections` = n_elecs,
         `Min Pop.`=min_population_2020,`Max Pop.`=max_population_2020,`Avg. Pop.`=avg_population_2020,`Total Pop.`=total_population,`% of Target Uni. Pop.`=perc_target_uni)

## Table A1
print(xtable(coverage_table,digits = 0),
      include.rownames = F,
      floating = F,
      file = "councils/tab_coverage.tex")

## ------------------------ ##
#### Descriptive plots ---------------------------------------------------------
## ------------------------ ##
council_comp <- read_rds("council_comp_wcbps.rds")

council_comp <- council_comp %>%
  mutate(dem_seatshare = total_dem/seats_total,
         rep_seatshare = total_rep/seats_total,
         unk_seatshare = total_oth/seats_total)

## Figure A2:
(hist_councilsize <- ggplot(data_indfin_summ) + 
   geom_bar(aes(x=seats_tot),width = 1) + 
   scale_x_continuous("Total Council Seats",breaks=seq(0,50,5)) + 
   scale_y_continuous("Count") + 
   theme_minimal()
)
ggsave("councils/councilsize_histogram.pdf",hist_councilsize,height=3,width=6)

## Figure A1a:
(hist_timeplot_post1990 <- ggplot(data_indfin) + 
    geom_bar(aes(x=year),width = 1) + 
    scale_x_continuous("Year",limits=c(1989.5,2020.5),breaks=seq(1992,2020,4)) + 
    scale_y_continuous("Count of council elections") + 
    theme_minimal()
)
ggsave("councils/timeplot_histogram_post1990.pdf",hist_timeplot_post1990,height=3,width=6)


council_comp_summ <- council_comp %>%
  group_by(year) %>%
  summarize(dem_seatshare = mean(dem_seatshare,na.rm=T),
            rep_seatshare = mean(rep_seatshare,na.rm=T),
            unk_seatshare = mean(unk_seatshare,na.rm=T)
  )

## Figure 2a:
(hist_timeplot_councilcomp3 <- ggplot(council_comp_summ) + 
    # geom_line(aes(x=year,col=city,y=dem_seatshare),alpha=0.1) + 
    geom_smooth(aes(x=year,y=dem_seatshare),span=0.3,col="blue") + 
    geom_smooth(aes(x=year,y=rep_seatshare),span=0.3,col="red",lty="dashed") + 
    geom_smooth(aes(x=year,y=unk_seatshare),span=0.3,col="black",lty="dotted") +
    annotate("text",x=2014,y=0.60,label="Democratic",col="blue") + 
    annotate("text",x=2015,y=0.36,label="Republican",col="red") + 
    annotate("text",x=2014,y=0.1,label="Other/Unknown",col="black") + 
    scale_x_continuous("",limits=c(1989.9,2017.1),breaks=seq(1992,2020,4)) + 
    scale_y_continuous("Share of council seats",labels=scales::percent_format(),limits=c(0,1)) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(panel.grid = element_blank(),axis.line = element_line(),axis.ticks = element_line())
)
ggsave("councils/timeplot_councilcomp3.pdf",hist_timeplot_councilcomp3,height=3,width=6)
