# TSMO Data Script to set up main analysis. Main analysis in tsmo_analysis markdown file.

packs <- c("tidyverse","countrycode","readxl","lmtest","multiwayvcov","stargazer",
           "readstata13", "magrittr","parallel") # Load Packages
lapply(packs,library,character.only = T)

cnames <- read_delim("data/tsmod_cnames.tab", delim = "\t") # Pair COW codes with country-names used in TSMOD data
cnames <- cnames %>% mutate(ccode = countrycode(cname,'country.name','cown'),
                            ccode = replace(ccode,cname == 'botswan',571),
                            ccode = replace(ccode,cname == 'burkfas',439),
                            ccode = replace(ccode,cname == 'capeverd',402),
                            ccode = replace(ccode,cname == 'centafre',482),
                            ccode = replace(ccode,cname == 'guin_bis',404),
                            ccode = replace(ccode,cname == 'argent',160),
                            ccode = replace(ccode,cname == 'australi',900),
                            ccode = replace(ccode,cname == 'azerbaij',373),
                            ccode = replace(ccode,cname == 'banglade',771),
                            ccode = replace(ccode,cname == 'brun_dar',835),
                            ccode = replace(ccode,cname == 'cost_ric',94),
                            ccode = replace(ccode,cname == 'czech_re',316),
                            ccode = replace(ccode,cname == 'dom_rep',42),
                            ccode = replace(ccode,cname == 'east_tim',860),
                            ccode = replace(ccode,cname == 'el_salv',92),
                            ccode = replace(ccode,cname == 'guatemal',90),
                            ccode = replace(ccode,cname == 'indonesi',850),
                            ccode = replace(ccode,cname == 'ivcoast',437),
                            ccode = replace(ccode,cname == 'kor_rep',732),
                            ccode = replace(ccode,cname == 'lithuani',368),
                            ccode = replace(ccode,cname == 'madagasc',580),
                            ccode = replace(ccode,cname == 'mauritan',435),
                            ccode = replace(ccode,cname == 'mauritiu',590),
                            ccode = replace(ccode,cname == 'mozamb',541),
                            ccode = replace(ccode,cname == 'n_korea',731),
                            ccode = replace(ccode,cname == 'netherla',210),
                            ccode = replace(ccode,cname == 'new_zeal',920),
                            ccode = replace(ccode,cname == 'nicaragu',93),
                            ccode = replace(ccode,cname == 'philippi',840),
                            ccode = replace(ccode,cname == 'saudi_ar',670),
                            ccode = replace(ccode,cname == 'serbia',345),
                            ccode = replace(ccode,cname == 'seychel',591),
                            ccode = replace(ccode,cname == 'sier_le',451),
                            ccode = replace(ccode,cname == 'sri_lank',780),
                            ccode = replace(ccode,cname == 'st_lucia',56),
                            ccode = replace(ccode,cname == 'st_vince',57),
                            ccode = replace(ccode,cname == 'sth_afr',560),
                            ccode = replace(ccode,cname == 'swazilan',572),
                            ccode = replace(ccode,cname == 'u_ar_em',696),
                            ccode = replace(ccode,cname == 'venezuel',101),
                            ccode = replace(ccode,cname == 'yugo_serb',345), # Combined Serbia and Yugoslavia, both of which are represented in inconsistent ways in the TSMO data
                            ccode = replace(ccode,cname == 'congo_dr',490),
                            ccode = replace(ccode,cname == 'german_dr',265),
                            ccode = replace(ccode,cname == 'yemen_pr',680))
  
  

vdem.main <- vdem::VDem_plus %>% # Get Main V-Dem Indictators
  select(vdem_country_name,GWn,year,
         v2x_polyarchy,v2x_liberal,v2x_partip,v2xdl_delib,v2x_egal, # High-Level Democracy Indexes
         v2x_elecoff,v2xel_frefair,v2x_frassoc_thick,v2x_suffr,v2x_freexp_altinf,# Components of Polyarchy Index
         e_regionpol) %>% # Controls
  rename(ccode = GWn, polyarchy = v2x_polyarchy,liberal = v2x_liberal, partip = v2x_partip, delib = v2xdl_delib, egal = v2x_egal,
         elec = v2x_elecoff, frefair = v2xel_frefair, fr.assoc = v2x_frassoc_thick, fre.exp = v2x_freexp_altinf, suffr = v2x_suffr) %>%
  filter(year > 1952 & year < 2015) %>% 
  group_by(e_regionpol,year) %>% 
  mutate(region_dem = mean(polyarchy,na.rm = T))

# Create IO Score Control Variable from Pevehouse data

ioscores <- read.dta13("data/ioscore_stateunit_1945-2016.dta") %>% 
  filter(year > 1952 & year < 2015)

io.org.cnty.yrs <- ioscores %>% # Dataset of all organization-country-years
  select(ccode,year,acc:cdc) %>%
  gather(key = "orgname", value = value,-year,-ccode) 
io.org.cnty.yrs <- io.org.cnty.yrs %>% mutate(value = replace(value,is.na(value),0)) %>%
  filter(value != 0) 
io.org.cnty.yrs <- left_join(io.org.cnty.yrs,vdem.main, by = c("ccode","year")) %>% 
  select(ccode:fre.exp)

cowcodes <- unique(io.org.cnty.yrs$ccode)

system.time({
  cl <- makeCluster(7)
  clusterExport(cl,list("io.org.cnty.yrs","cowcodes"))
  clusterEvalQ(cl, {library(dplyr)})
  io.cnty.yrs.max <- parLapply(cl,cowcodes, function(y){
    single.cnty <- io.org.cnty.yrs %>% mutate_at(vars(polyarchy:fre.exp), ~ replace(.,ccode == y,NA_real_)) %>% # Remove the country being analyzed from average
      group_by(orgname,year) %>% 
      summarize_at(vars(polyarchy:fre.exp), ~ mean(.,na.rm = T)) %>% 
      rename_at(vars(polyarchy:fre.exp), ~ paste("ioscore",.,sep = "."))
    
    country <- io.org.cnty.yrs %>% filter(ccode == y)  
    country <- left_join(country,single.cnty, by = c("orgname","year")) %>% 
      group_by(year) %>% 
      summarize_at(vars(ioscore.polyarchy:ioscore.fre.exp), ~ max(.,na.rm = T)) %>% 
      mutate(ccode = y)
  })
  stopCluster(cl)
  io.cnty.yrs.max <- bind_rows(io.cnty.yrs.max)
  rm (cl,io.org.cnty.yrs)
})

io.cnty.yrs.max <- right_join(io.cnty.yrs.max,ioscores,by = c("ccode","year")) %>% 
  mutate_at(vars(ioscore.polyarchy:ioscore.fre.exp), ~ replace(.,is.na(.),0)) %>% 
  select(year:ccode)

# Get John's GDP and democracy data

dem.econ.perf.df <- read.dta13("data/Master_DemDev_1953-2014.dta",nonint.factors = T)
dem.econ.perf.df %<>% 
  select(ccode,year,lrgdpopc,litpop,log_fuelincomepc07,island_contig400,avgrgdpopc_5yrch_corrpolyarchy,avgrgdpopc_5yrch_corrlibdem,
         avgrgdpopc_5yrch_corrpartipdem,avgrgdpopc_5yrch_corrdelibdem,avgrgdpopc_5yrch_corregaldem,
         avgrgdpopc_5yrch_corrfreexp,avgrgdpopc_5yrch_corrfrassoc,avgrgdpopc_5yrch_corrsuffr,
         avgrgdpopc_5yrch_correlfrefair,avgrgdpopc_5yrch_correlecoff,v2x_polyarchy_samecolnetwork,
         v2x_libdem_samecolnetwork,v2x_partipdem_samecolnetwork,v2x_delibdem_samecolnetwork,
         v2x_egaldem_samecolnetwork,v2x_polyarchy_neighbors400,v2x_libdem_neighbors400,v2x_partipdem_neighbors400,
         v2x_delibdem_neighbors400,v2x_egaldem_neighbors400,v2x_freexp_altinf_neighbors400,
         v2x_frassoc_thick_neighbors400,v2x_suffr_neighbors400,v2xel_frefair_neighbors400,v2x_elecoff_neighbors400) %>% 
  rename(dem.perf.poly = avgrgdpopc_5yrch_corrpolyarchy,
         dem.perf.liberal = avgrgdpopc_5yrch_corrlibdem,
         dem.perf.partip = avgrgdpopc_5yrch_corrpartipdem,
         dem.perf.delib = avgrgdpopc_5yrch_corrdelibdem,
         dem.perf.egal = avgrgdpopc_5yrch_corregaldem,
         dem.perf.elec = avgrgdpopc_5yrch_correlecoff,
         dem.perf.frefair = avgrgdpopc_5yrch_correlfrefair,
         dem.perf.fr.assoc = avgrgdpopc_5yrch_corrfrassoc,
         dem.perf.fre.exp = avgrgdpopc_5yrch_corrfreexp,
         dem.perf.suffr = avgrgdpopc_5yrch_corrsuffr,
         colonial.poly = v2x_polyarchy_samecolnetwork,
         colonial.liberal = v2x_libdem_samecolnetwork,
         colonial.partip = v2x_partipdem_samecolnetwork,
         colonial.delib = v2x_delibdem_samecolnetwork,
         colonial.egal = v2x_egaldem_samecolnetwork,
         neighbors400.poly = v2x_polyarchy_neighbors400,
         neighbors400.liberal = v2x_libdem_neighbors400,
         neighbors400.partip = v2x_partipdem_neighbors400,
         neighbors400.delib = v2x_delibdem_neighbors400,
         neighbors400.egal = v2x_egaldem_neighbors400,
         neighbors400.elec = v2x_elecoff_neighbors400,
         neighbors400.frefair = v2xel_frefair_neighbors400,
         neighbors400.suffr = v2x_suffr_neighbors400,
         neighbors400.fr.assoc = v2x_frassoc_thick_neighbors400,
         neighbors400.fre.exp = v2x_freexp_altinf_neighbors400,
         log_gdppc = lrgdpopc,
         log_pop = litpop,
         log_fuel = log_fuelincomepc07)

# Get TSMOs Data

tsmos.work <- read_csv("data/2018-9-13_TSMOD_ver3 (no miss v.115).csv", # Import TSMOD
                  col_types = cols(entryyear = col_character(),
                                   members_5 = col_number())) %>%  
  select(orgname,seriesid,year,foundyr,hrights,women,algeria:othercountry) %>% 
  group_by(seriesid) %>%
  mutate(minyear = min(year),
         maxyear = max(year)) %>%
  ungroup() %>% 
  rename(yugo_serb = yugoslav) %>% # Create membership variable combining memberships recorded for Serbia and Yugoslavia
  mutate(yugo_serb = replace(yugo_serb,serbia == 1,1)) %>% 
  select(-serbia)

# Change "other country" variable into new country membership variables

other.countries <- unlist(strsplit(unique(tsmos.work$othercountry),",")) # Split entries separated by comma
other.countries <- as.data.frame(unique(trimws(unlist(strsplit(other.countries,";")),which = "both"))) #Trim white space and split entries separated by semicolon
colnames(other.countries) <- "country"
other.countries$country <- as.character(gsub("(","",other.countries$country, fixed = TRUE)) #Get rid of parentheses to avoid breaking later code
other.countries %<>% mutate(ccode = countrycode(country,"country.name","cown"), # Match all strings (including typos) to country codes
                            ccode = replace(ccode,country == 'Djbouti',522),
                            ccode = replace(ccode,country == "Mya",775),
                            ccode = replace(ccode,country == "Timor-" |
                                              country == "Timor-Le" |
                                              country == "Timor-Lete",860),
                            ccode = replace(ccode,country == "Serbia-" | 
                                              country == "Serbia" |
                                              country == "Serbia-Montenegro" |
                                              country == "Serbia-Mo" |
                                              country == "Serbia-Monte" |
                                              country == "Serbia-Montene" |
                                              country == "serb-mont" |
                                              country == "Serbia-Mon" |
                                              country == "Serbi" |
                                              country == "serb-m" |
                                              country == "SRB-MONT",345),
                            ccode = replace(ccode,country == "Monten",341),
                            ccode = replace(ccode,country == "Kyrgystan" |
                                              country == "Kyrgyszstan" |
                                              country == "Kyr" |
                                              country == "Krygyzstan",703),
                            ccode = replace(ccode,country == "Lichtenstein" |
                                              country == "Liechtens" |
                                              country == "Liechtenstien",223),
                            ccode = replace(ccode,country == "Erit",531),
                            ccode = replace(ccode,country == "Clovakia" |
                                              country == "Slova",317),
                            ccode = replace(ccode,country == "Equatorial" |
                                              country == "EQUATORIAL" |
                                              country == "Equatoria" |
                                              country == "Equatorial Guiena" |
                                              country == "equatorial",411),
                            ccode = replace(ccode,country == "S Rhodes" |
                                              country == "Rhodeisa",552),
                            ccode = replace(ccode,country == "Northern Ireland" |
                                              country == "Scot" |
                                              country == "England" |
                                              country == "Scotland" |
                                              country == "northern ireland" |
                                              country == "Cornwall" |
                                              country == "England (" |
                                              country == "England and Scotland" |
                                              country == "Northern Irel" |
                                              country == "ENGLAND" |
                                              country == "NORTHERN IRELAND" |
                                              country == "Northen Irelan" |
                                              country == "SCOTLAND",200),
                            ccode = replace(ccode,country == "Maced" |
                                              country == "Macedon" |
                                              country == "MACEDON" |
                                              country == "macedoni",343),
                            ccode = replace(ccode,country == "Quebec" |
                                              country == "QUEBEC",20),
                            ccode = replace(ccode,country == "canton of geneva",225),
                            ccode = replace(ccode,country == "Catalonia" |
                                              country == "catalunya" |
                                              country == "Catalunya" |
                                              country == "Andalusia" |
                                              country == "Basque Cou",230),
                            ccode = replace(ccode,country == "Bos-Herz" |
                                              country == "bos-herz" |
                                              country == "BOS-HERZ",346),
                            ccode = replace(ccode,country == "Dubai" |
                                              country == "Abu Dhabi listed separ" |
                                              country == "abu dhabi" |
                                              country == "dubai listed separ",696),
                            ccode = replace(ccode,country == "greenland" |
                                              country == "Greenland",390),
                            ccode = replace(ccode,country == "Hawaii" |
                                              country == "HAWAII" |
                                              country == "SAMOA USA" |
                                              country == "Samoa USA" |
                                              country == "Guam" |
                                              country == "GUAM",2),
                            ccode = replace(ccode,country == "Sao Tom",403),
                            ccode = replace(ccode,country == "German DR (coded as Germany)" |
                                              country == "Germany DR (marked as Germany" |
                                              country == "German DR (coded as" |
                                              country == "German DR (C0ded as Germany" |
                                              country == "German DR" |
                                              country == "Germany DR",265),
                            ccode = replace(ccode,country == "UAR",651)
) %>% 
  filter(!is.na(ccode))

tsmos.work %<>% # Add connection where country was recorded in "other country" variable but not main country variable.
  mutate(bosnia = replace(bosnia,grepl(paste(other.countries$country[other.countries$ccode == 346],collapse = "|"),othercountry),1),
         djibouti = replace(djibouti,grepl(paste(other.countries$country[other.countries$ccode == 522],collapse = "|"),othercountry),1),
         myanmar = replace(myanmar,grepl(paste(other.countries$country[other.countries$ccode == 775],collapse = "|"),othercountry),1),
         east_tim = replace(east_tim,grepl(paste(other.countries$country[other.countries$ccode == 860],collapse = "|"),othercountry),1),
         yugo_serb = replace(yugo_serb,grepl(paste(other.countries$country[other.countries$ccode == 345],collapse = "|"),othercountry),1),
         montenegro = replace(montenegro,grepl(paste(other.countries$country[other.countries$ccode == 341],collapse = "|"),othercountry),1),
         kyrgyzstan = replace(kyrgyzstan,grepl(paste(other.countries$country[other.countries$ccode == 703],collapse = "|"),othercountry),1),
         liechtenstein = replace(liechtenstein,grepl(paste(other.countries$country[other.countries$ccode == 223],collapse = "|"),othercountry),1),
         eritrea = replace(eritrea,grepl(paste(other.countries$country[other.countries$ccode == 531],collapse = "|"),othercountry),1),
         slovakia = replace(slovakia,grepl(paste(other.countries$country[other.countries$ccode == 317],collapse = "|"),othercountry),1),
         guinea_equat = replace(guinea_equat,grepl(paste(other.countries$country[other.countries$ccode == 411],collapse = "|"),othercountry),1),
         zimbabwe = replace(zimbabwe,grepl(paste(other.countries$country[other.countries$ccode == 552],collapse = "|"),othercountry),1),
         uk = replace(uk,grepl(paste(other.countries$country[other.countries$ccode == 200],collapse = "|"),othercountry),1),
         macedonia = replace(macedonia,grepl(paste(other.countries$country[other.countries$ccode == 343],collapse = "|"),othercountry),1),
         canada = replace(canada,grepl(paste(other.countries$country[other.countries$ccode == 20],collapse = "|"),othercountry),1),
         switzerl = replace(switzerl,grepl(paste(other.countries$country[other.countries$ccode == 225],collapse = "|"),othercountry),1),
         spain = replace(spain,grepl(paste(other.countries$country[other.countries$ccode == 230],collapse = "|"),othercountry),1),
         u_ar_em = replace(u_ar_em,grepl(paste(other.countries$country[other.countries$ccode == 696],collapse = "|"),othercountry),1),
         denmark = replace(denmark,grepl(paste(other.countries$country[other.countries$ccode == 390],collapse = "|"),othercountry),1),
         usa = replace(usa,grepl(paste(other.countries$country[other.countries$ccode == 2],collapse = "|"),othercountry),1),
         saotomeprincipe = replace(saotomeprincipe,grepl(paste(other.countries$country[other.countries$ccode == 403],collapse = "|"),othercountry),1),
         egypt = replace(egypt,grepl(paste(other.countries$country[other.countries$ccode == 651],collapse = "|"),othercountry),1),
         laos = replace(laos,grepl(paste(other.countries$country[other.countries$ccode == 812],collapse = "|"),othercountry),1),
         bhutan = replace(bhutan,grepl(paste(other.countries$country[other.countries$ccode == 760],collapse = "|"),othercountry),1),
         taiwan = replace(taiwan,grepl(paste(other.countries$country[other.countries$ccode == 713],collapse = "|"),othercountry),1),
         comoros = replace(comoros,grepl(paste(other.countries$country[other.countries$ccode == 581],collapse = "|"),othercountry),1),
         solomon_is = replace(solomon_is,grepl(paste(other.countries$country[other.countries$ccode == 940],collapse = "|"),othercountry),1),
         moldova = replace(moldova,grepl(paste(other.countries$country[other.countries$ccode == 359],collapse = "|"),othercountry),1),
         germany = replace(germany,grepl(paste(other.countries$country[other.countries$ccode == 255],collapse = "|"),othercountry),1),
         malawi = replace(malawi,grepl(paste(other.countries$country[other.countries$ccode == 553],collapse = "|"),othercountry),1),
         burkfas =replace(burkfas,grepl(paste(other.countries$country[other.countries$ccode == 439],collapse = "|"),othercountry),1),
         madagasc = replace(madagasc,grepl(paste(other.countries$country[other.countries$ccode == 580],collapse = "|"),othercountry),1),
         congo_dr = replace(congo_dr,grepl(paste(other.countries$country[other.countries$ccode == 490],collapse = "|"),othercountry),1),
         tanzania = replace(tanzania,grepl(paste(other.countries$country[other.countries$ccode == 511],collapse = "|"),othercountry),1),
         russia = replace(russia,grepl(paste(other.countries$country[other.countries$ccode == 365],collapse = "|"),othercountry),1),
         azerbaij = replace(azerbaij,grepl(paste(other.countries$country[other.countries$ccode == 373],collapse = "|"),othercountry),1),
         ireland = replace(ireland,grepl(paste(other.countries$country[other.countries$ccode == 205],collapse = "|"),othercountry),1),
         jamaica = replace(jamaica,grepl(paste(other.countries$country[other.countries$ccode == 51],collapse = "|"),othercountry),1),
         cyprus = replace(cyprus,grepl(paste(other.countries$country[other.countries$ccode == 352],collapse = "|"),othercountry),1),
         turkmenistan = replace(turkmenistan,grepl(paste(other.countries$country[other.countries$ccode == 701],collapse = "|"),othercountry),1),
         n_korea = replace(n_korea,grepl(paste(other.countries$country[other.countries$ccode == 731],collapse = "|"),othercountry),1),
         sri_lank = replace(sri_lank,grepl(paste(other.countries$country[other.countries$ccode == 780],collapse = "|"),othercountry),1),
         sth_afr = replace(sth_afr,grepl(paste(other.countries$country[other.countries$ccode == 560],collapse = "|"),othercountry),1),
         papua_ng = replace(papua_ng,grepl(paste(other.countries$country[other.countries$ccode == 910],collapse = "|"),othercountry),1),
         capeverd = replace(capeverd,grepl(paste(other.countries$country[other.countries$ccode == 402],collapse = "|"),othercountry),1),
         guinea = replace(guinea,grepl(paste(other.countries$country[other.countries$ccode == 438],collapse = "|"),othercountry),1),
         tajiksta = replace(tajiksta,grepl(paste(other.countries$country[other.countries$ccode == 702],collapse = "|"),othercountry),1),
         
         # Final three lines below create new variables for these countries not included in original data
         
         german_dr = if_else(grepl(paste(other.countries$country[other.countries$ccode == 265],collapse = "|"),othercountry),1,0),
         kosovo = if_else(grepl(paste(other.countries$country[other.countries$ccode == 347],collapse = "|"),othercountry),1,0),
         yemen_pr = if_else(grepl(paste(other.countries$country[other.countries$ccode == 680],collapse = "|"),othercountry),1,0)
         ) 


#Interpolate all variables into new years based on previous observation

all.years <- data.frame(c(1953:2013))
colnames(all.years) <- 'year'

tsmos.allyears <- map(as.character(unique(tsmos.work$seriesid)), function(x) {
  tsmos.work %>% filter(seriesid == x) %>% 
    left_join(all.years,.,by = "year") %>% 
    filter(year >= mean(na.omit(minyear)) & year <= mean(na.omit(maxyear)))
}) %>% 
  bind_rows() %>% 
  fill(.,1:length(.)) 

# Add Country Memberships in years where all members drop to zero


org.mem.counts <- tsmos.allyears %>% 
  select(-othercountry,-minyear,-maxyear) %>%  
  mutate(allmems = rowSums(.[6:length(.)],na.rm = T)) %>% # Create var summing number of country memberships
  filter(allmems < 200) %>%   # Dropping twelve observations where membership coded non-binary, likely coding error.
# Interpolate Country memberships for years where number drops to zero (country memberships not reported that year)
  group_by(seriesid) %>% 
  arrange(year) %>% 
  mutate(zerodrop = if_else(allmems == 0 & lag(allmems) > 0,1,0)) %>%  # Create var measuring drop to zero
  mutate_at(vars(algeria:yugo_serb),funs(if_else(allmems == 0 & cumsum(zerodrop) > 0,NA_real_,as.double(.)))) %>% 
  ungroup() %>% 
  fill_(.,names(.)) %>% # This line fills forward individual country memberships in TSMOS 
  # during periods where memberships are not reported.
  mutate(allmems.new = rowSums(.[6:length(.)],na.rm = T)) # Create new var summing number of country memberships to check that code executed properly


# Dataset of all organization-country-years

org.cnty.yrs <- org.mem.counts %>% 
  select(-orgname, -foundyr) %>%
  gather(key = "cname", value = value,-year,-seriesid,-hrights,-women) %>% 
  filter(value != '0') %>% 
  left_join(.,cnames, by = "cname") %>% 
  left_join(.,vdem.main, by = c("ccode","year"))

# Create Dataset with number of TSMO memberships per country-year

tsmo.members <- org.cnty.yrs %>% 
  group_by(ccode,year) %>% 
  summarize(num.tsmo.mems = n(),
            num.hr.mems = sum(hrights),
            num.women.mems = sum(women)) %>% 
  ungroup() 

# Create dataset with number of IGO memberships per country-year

io.members.data <- ioscores %>% 
  mutate_at(vars(acc:cdc),funs(replace(.,. < 0,0))) %>% 
  mutate(num.igo.mems = rowSums(.[7:87],na.rm = T)) %>% 
  select(ccode,year,num.igo.mems)

all.membership.numbers <- full_join(tsmo.members,io.members.data,by = c("ccode","year")) %>% 
  group_by(year) %>% 
  mutate(log_xxxtsmo = log(num.tsmo.mems + 1) - log(median(num.tsmo.mems,na.rm = T)))


# Create TSMO Score Variable (Alternative IVs, replicating Pevehouse method) Using Parallel Processing for speed
# Creating Three Different Versions based on HR TSMOs, Women's TSMOs and All TSMOS
# Function takes as Input the organization-country-year dataset appropriately filtered 
# to get the kind of TSMO that we want

create.tsmoscore <- function(tsmo.dataset) {
  cowcodes <- unique(tsmo.dataset$ccode)
  cl <- makeCluster(7)
  clusterEvalQ(cl, {library(dplyr)})
  tsmoscores <- parLapply(cl,cowcodes, function(y){
    organizations <- tsmo.dataset %>% 
      mutate_at(vars(polyarchy:fre.exp), ~ replace(.,ccode == y,NA_real_)) %>% 
      group_by(seriesid,year) %>% 
      summarize_at(vars(polyarchy:fre.exp),funs(mean(.,na.rm = T))) %>% 
      rename_at(vars(polyarchy:fre.exp), ~ paste("tsmoscore",.,sep = "."))
      
    country <- tsmo.dataset %>% filter(ccode == y) %>% 
      left_join(.,organizations, by = c("seriesid","year")) %>% 
      group_by(year) %>% 
      summarize_at(vars(tsmoscore.polyarchy:tsmoscore.fre.exp), ~ max(.,na.rm = T)) %>% 
      mutate(ccode = y)
  })
  stopCluster(cl)
  remove(cl)
  tsmoscores <- bind_rows(tsmoscores)
  return(tsmoscores)
}

tsmoscore.all <- create.tsmoscore(org.cnty.yrs)

tsmoscore.hr.only <- create.tsmoscore(filter(org.cnty.yrs,hrights == 1))

tsmoscore.women.only <- create.tsmoscore(filter(org.cnty.yrs,women == 1))


### Creating Spatial Lag Variable ###

wt.tsmo.var <- function(var,total,single) { # Function to create spatial lag based on proportion of links
  var * (single/total)
}

make.spatial.lag <- function(df) { 

# Create data-frame to merge on right-hand-side
    
cnty.members <- df %>% 
  select(year,seriesid,ccode) %>% 
  rename(member = ccode)

cntys.by.year <- df %>% 
  select(ccode,year,seriesid) %>% 
  left_join(.,cnty.members, by = c("year","seriesid")) %>% # Combine ccode on LHS with all cntys connected through TSMOlink in the same year on RHS
  filter(ccode != member) %>% # Drop observation linking country to itself
  group_by(ccode,year,member) %>% 
  mutate(links = n()) %>% # Create var measuring number of links between LHS and RHS country
  left_join(vdem.main,by = c("member" = "ccode","year")) %>% # Import V-Dem vars for RHS country
  ungroup() %>% 
  group_by(ccode,year) %>% 
  mutate(total_links = sum(links)) %>% # Create var measuring total number of links for LHS country-year
  mutate_at(vars(polyarchy:fre.exp), ~ wt.tsmo.var(.,total_links,links)) %>% # Multiple RHS country's V-dem var by its proportion of links to LHS country
  summarize_at(vars(polyarchy:fre.exp), ~ sum(.,na.rm = T)) %>%  # Sum all weighted V-Dem vars for LHS country-year
  rename_at(vars(polyarchy:fre.exp), ~ paste("tsmo.wtd",.,sep = ".")) # Rename weighted vars
}

tsmo.wtd.all <- make.spatial.lag(org.cnty.yrs)

tsmo.wtd.hr.only <- make.spatial.lag(filter(org.cnty.yrs,hrights == 1))

tsmo.wtd.women.only <- make.spatial.lag(filter(org.cnty.yrs,women == 1))


# Join Dataframe of Weighted Scores (functionized to create separate datasets for All TSMOS, limited to HR TSMOs, and limited to women TSMOs)

create.working.data <- function(wtd.data,tsmoscore.data) {
  working.dataset <- left_join(vdem.main,wtd.data, by = c("ccode","year")) %>% 
    mutate_at(vars(tsmo.wtd.polyarchy:tsmo.wtd.fre.exp), ~ if_else(is.na(.) & year < 2014,0,.)) %>% 
    group_by(ccode) %>% 
    arrange(year) %>% 
    mutate_at(vars(polyarchy:fre.exp), list(lead = ~ lead(.),
                                            diff = ~ lead(.) - .)) %>% 
    mutate(cw = if_else(year < 1991,1,0),
           tsmo.island = if_else(tsmo.wtd.polyarchy == 0,0,1)) %>% 
    filter(year %in% 1953:2013 & ccode != 99999 & !(is.na(ccode))) %>% 
    left_join(.,io.cnty.yrs.max, by = c("ccode","year")) %>% 
    left_join(.,dem.econ.perf.df, by = c("ccode","year")) %>% 
    left_join(.,all.membership.numbers, by = c("ccode","year")) %>%
    left_join(.,tsmoscore.data, by = c("ccode","year")) %>% 
    mutate(igo.island = if_else(num.igo.mems == 0,1,0),
           log.tsmo.mems = log1p(num.tsmo.mems))
}

working.data.all <- create.working.data(tsmo.wtd.all,tsmoscore.all)

working.data.hr.only <- create.working.data(tsmo.wtd.hr.only,tsmoscore.hr.only)

working.data.women.only <- create.working.data(tsmo.wtd.women.only,tsmoscore.women.only)

# Save Working Datasets

all.working.datasets <- list(working.data.all,
                             working.data.hr.only,
                             working.data.women.only)


saveRDS(all.working.datasets, file = "data/all_working_datasets.rds")

#############################
######## Old Code ###########
#############################

# Create TSMOScore variables from replicated data

# cowcodes <- unique(org.cnty.yrs$ccode)
# 
# system.time({
#   cl <- makeCluster(7)
#   clusterExport(cl,list("org.cnty.yrs","cowcodes"))
#   clusterEvalQ(cl, {library(dplyr)})
#   foo <- parLapply(cl,cowcodes, function(y){
#     foo <- org.cnty.yrs %>% mutate_at(vars(polyarchy:fre.exp),funs(replace(.,ccode == y,NA_real_))) %>% 
#       group_by(seriesid,year) %>% 
#       summarize_at(vars(polyarchy:fre.exp),funs(mean(.,na.rm = T))) %>% 
#       rename_at(vars(polyarchy:fre.exp),funs(paste("tsmoscore",.,sep = ".")))
#     
#     country <- org.cnty.yrs %>% filter(ccode == y)  
#     country <- left_join(country,foo, by = c("seriesid","year")) %>% 
#       group_by(year) %>% 
#       summarize_at(vars(tsmoscore.polyarchy:tsmoscore.fre.exp),funs(max(.,na.rm = T))) %>% 
#       mutate(ccode = y)
#   })
#   stopCluster(cl)
#   tsmoscores.new <- bind_rows(foo) %>% 
#     mutate_all(funs(replace(.,. == -Inf,NA)))
#   remove(cl,foo)
# })
# 
# # Create Weighted Variables
# 
# network.new <- lapply(years,netdata) # Create Adjacency Matrix from replicated data
# 
# network.work.new <-lapply(network.new,function(x) bind_rows(x)) # Combine each annual list of observations into a single dataframe
# 
# network.work.new <- lapply(network.work.new, function(x) left_join(cnames,x,by = "ccode") %>% mutate(year = max(na.omit(year)))) # Add missing names
# 
# network.work.new <- bind_rows(network.work.new) %>%  mutate_at(vars(afghanis:algeria),funs(replace(.,is.na(.),0))) # Replace is.na with 0 observations
# 
# network.work.new <- network.work.new[,c(1,2,197,3:196,198)] # Set year variable to third column
# 
# weighted.vars <- network.work.new %>% 
#   gather(key = country, value = value, -ccode,-cname,-year) %>% # Create dataset of each dyad-year with number of connections between each.
#   rename(ccode.home = ccode,cname.home = cname)                                                 
# weighted.vars <- left_join(weighted.vars,cnames,by = c("country" = "cname")) # Add COW codes for V-Dem join
# weighted.vars <- left_join(weighted.vars,vdem.main,by = c("ccode","year")) %>% # Join dyads to V-Dem data (vars connected to left-hand side of dyad)
#   select(-country_name) %>%
#   mutate_at(vars(poly:fre.exp),funs(.*value)) # Multiply all IVs by number of connections between dyad
# 
# weighted.vars.1 <- weighted.vars %>%# Creat Cnty-Year dataset with all IVs and number of connections summed.
#   group_by(ccode.home,year) %>% 
#   summarise_at(vars(value,poly:fre.exp),funs(sum(na.omit(.)))) %>% # Sum number of connections and all IVs
#   filter(!is.na(ccode.home)) %>%                                                                                        
#   mutate_at(vars(poly:fre.exp),funs(./value)) %>% # Divide summed DVs by total number of connections
#   mutate_all(funs(replace(.,is.nan(.),0))) # Replace 0/0 observations with 0
# 
# names(weighted.vars.1)[4:13] <- paste("tsmo.",names(weighted.vars.1)[4:13],sep ="") # Rename independent variables
# 
# weighted.cnty.yrs.new <- full_join(vdem.main,weighted.vars.1, by = c("ccode" = "ccode.home","year" = "year")) %>% # Merge with VDem for all DVs
#   group_by(ccode) %>%
#   arrange(year) %>%
#   mutate_at(vars(poly:fre.exp),funs(lead = lead(.))) %>% #Lead all DVs one year
#   ungroup() %>%
#   mutate(cw = if_else(year<1991,1,0), # Create CW dummy variable
#          tsmo.island = if_else(value == 0,1,0)) %>% # Create TSMO Island dummy variable
#   mutate(poly_diff = poly_lead - poly,
#          liberal_diff = liberal_lead - liberal,
#          partip_diff = partip_lead - partip,
#          delib_diff = delib_lead - delib,
#          egal_diff = egal_lead - egal,
#          elec_diff = elec_lead - elec,
#          frefair_diff = frefair_lead - frefair,
#          fr.assoc_diff = fr.assoc_lead - fr.assoc,
#          fre.exp_diff = fre.exp_lead - fre.exp,
#          suffr_diff = suffr_lead - suffr) %>% 
#   filter(year > 1952 & year < 2014 & ccode != 99999) %>% 
#   rename(polyarchy = poly)
# 
# weighted.cnty.yrs.new <- left_join(weighted.cnty.yrs.new,io.cnty.yrs.max.2,by = c("ccode","year")) # Join to IO score dataset
# weighted.cnty.yrs.new <- left_join(weighted.cnty.yrs.new,dem.econ.perf.df,by = c("ccode","year")) # Join to Democracy economic performance indicator
# weighted.cnty.yrs.new <- left_join(weighted.cnty.yrs.new,tsmoscores.new,by = c("ccode","year")) # Join to TSMO Scores Dataset
# weighted.cnty.yrs.new <- left_join(weighted.cnty.yrs.new,all.membership.numbers.new,by = c("ccode","year")) %>% # Join to Democracy economic performance indicator
#   mutate(igo.island = if_else(num.igo.mems == 0,1,0)) %>%  # Ad IGO island variable
#   mutate_at(vars(tsmoscore.polyarchy:tsmoscore.fre.exp),funs(replace(.,. == -Inf,NA_real_))) # Drop -Inf Observations
# 
# write.csv(weighted.cnty.yrs.new,"tsmo_paper_data_wtd_new.csv",row.names = F)
# 
# rm(weighted.vars,weighted.vars.1,tsmos.allyears) # Remove unneeded objects
# 
# save.image("tsmopaper_imputed.Rdata") # Save workspace and move to analysis markdown
# 
# beepr::beep(sound = 3)

# foo <- org.cnty.yrs %>% # Max and mean of vdem measures for each TSMO-year
#   group_by(.dots = c("seriesid","year")) %>%
#   summarize_at(vars(poly:fre.exp),funs(max,mean), na.rm = T)
# 
# cnty.yrs.max <- left_join(org.cnty.yrs,foo, by = c("seriesid","year")) %>% # Country-Year dataset with maximum of each Vdem measure in all countries connected by a TSMO
#   group_by(.dots = c("ccode","year")) %>%
#   summarize_at(vars(poly_max:fre.exp_max),funs(max),na.rm = T) %>%
#   filter(!is.na(ccode)) %>%
#   rename(tsmo.poly.max = poly_max, tsmo.lib.max = liberal_max, tsmo.partip.max = partip_max, tsmo.delib.max = delib_max, tsmo.egal.max = egal_max,
#          tsmo.elec.max = elec_max,tsmo.frefair.max = frefair_max,tsmo.fr.assoc.max = fr.assoc_max,tsmo.suffr.max = suffr_max,tsmo.freexp.max = fre.exp_max)
# 
# cnty.yrs.mean <- left_join(org.cnty.yrs,foo, by = c("seriesid","year")) %>% # Country-Year dataset with mean of each Vdem measure in all countries connected by a TSMO
#   group_by(.dots = c("ccode","year")) %>%
#   summarize_at(vars(poly_mean:fre.exp_mean),funs(mean),na.rm = T) %>%
#   filter(!is.na(ccode)) %>%
#   rename(tsmo.poly.mean = poly_mean, tsmo.lib.mean = liberal_mean, tsmo.partip.mean = partip_mean, tsmo.delib.mean = delib_mean, tsmo.egal.mean = egal_mean,
#          tsmo.elec.mean = elec_mean,tsmo.frefair.mean = frefair_mean,tsmo.fr.assoc.mean = fr.assoc_mean,tsmo.suffr.mean = suffr_mean,tsmo.freexp.mean = fre.exp_mean)
# 
# 
# cnty.yrs.all <- left_join(cnty.yrs.max,cnty.yrs.mean, by = c("ccode","year"))
# cnty.yrs.all <- left_join(vdem.main,cnty.yrs.all,by = c("ccode","year"))
# cnty.yrs.all <- left_join(cnty.yrs.all,io.cnty.yrs.max, by = c("ccode","year"))
# cnty.yrs.all <- left_join(cnty.yrs.all,dem.econ.perf.df, by = c("ccode","year"))
# 
# cnty.yrs.all <- cnty.yrs.all %>% 
#   group_by(ccode) %>%
#   arrange(year) %>%
#   mutate_at(vars(poly:fre.exp),funs(lead = lead(.))) %>% # Lead dependent variables
#   ungroup() %>%
#   filter(year > 1953 & ccode != 99999) %>%
#   mutate_at(vars(tsmo.poly.max:tsmo.freexp.mean),funs(replace(.,is.na(.),0))) %>%
#   mutate(cw = if_else(year < 1990,1,0)) # Cold War dummy
# 
# write.csv(cnty.yrs.all,"tsmo_paper_data_unw.csv",row.names = F)

# Function to Create Adjacency Matrix (Number of TSMO connections between each country in each year)

# netdata <- function(tid,df) { # Outer-level function defined on basis of year
#   cntys.thisyear <- df %>% filter(year == tid) %>% distinct(ccode) %>% filter(!is.na(ccode)) #List of countries with any connections this year
#   intermediate <- apply(cntys.thisyear[1],1,function(cow) { #Inner-level function defined on basis of countries with any connections this year
#     orgs <- df %>% filter(ccode == cow & year == tid) %>% distinct(seriesid) # List of org-cnty.years connected to this 
#     
#     test <- apply(orgs[1],1,function(x) unique(df$ccode[df$seriesid==x & df$year == tid])) #Get list of every country code
#     if(length(test)==0) {                                                                                                # connected to this TSMO in this year
#       test3 <- cnames %>% # Generate list of 0 connections if 0 connections
#         filter(!is.na(ccode) & ccode != cow) %>%
#         select(-ccode) %>%
#         mutate(Freq = 0) %>%
#         spread(key = cname,value = Freq) %>%
#         mutate(ccode = cow,year = tid)
#     } else {
#       test2 <- reduce(test,c) # Combine into single list
#       
#       test3 <- as.data.frame(table(test2)) %>% # Set up frequency table for how often each other country code appears
#         rename(ccode = test2) %>% 
#         mutate(ccode = as.numeric(levels(ccode))[ccode])
#       
#       test3 <- left_join(cnames,test3,by = "ccode") %>% # Join with TSMO country names
#         filter(!is.na(ccode) & ccode != cow) %>% #Drop row with missing ccodes and if ccode is the country currently being iterated over
#         mutate(Freq = replace(Freq,is.na(Freq),0)) %>% # Replace countries that did not appear in above code (those with no connections)
#         select(-ccode)                                 # as 0 connections rather than missing data
#       
#       test3 <- test3 %>% # Spread out to make single row for each country-year with a column for number of connections to each other country
#         spread(key = cname,value = Freq) %>% 
#         mutate(ccode = cow,year = tid)
#     }
#   }
#   )
# } 
# 
# 
# 
# years <- seq(1953,2013,1)
# network <- lapply(years,netdata) # Apply above function over all years in TSMO data
# 
# network.work <-lapply(network,function(x) bind_rows(x)) # Combine each annual list of observations into a single dataframe
# 
# network.work <- lapply(network.work, function(x) left_join(cnames,x,by = "ccode") %>% mutate(year = max(na.omit(year)))) # Add missing names
# 
# network.work <- bind_rows(network.work) %>%  mutate_at(vars(afghanis:algeria),funs(replace(.,is.na(.),0))) # Replace is.na with 0 observations
# 
# network.work <- network.work[,c(1,2,197,3:196,198)] # Set year variable to third column
# 
# weighted.vars <- network.work %>% 
#   gather(key = country, value = value, -ccode,-cname,-year) %>% # Create dataset of each dyad-year with number of connections between each.
#   rename(ccode.home = ccode,cname.home = cname)                                                 
# weighted.vars <- left_join(weighted.vars,cnames,by = c("country" = "cname")) # Add COW codes for V-Dem join
# weighted.vars <- left_join(weighted.vars,vdem.main,by = c("ccode","year")) %>% # Join dyads to V-Dem data (vars connected to left-hand side of dyad)
#   select(-country_name) %>%
#   mutate_at(vars(poly:fre.exp),funs(.*value)) # Multiply all IVs by number of connections between dyad
# 
# weighted.vars.1 <- weighted.vars %>%# Creat Cnty-Year dataset with all IVs and number of connections summed.
#   group_by(ccode.home,year) %>% 
#   summarise_at(vars(value,poly:fre.exp),funs(sum(na.omit(.)))) %>% # Sum number of connections and all IVs
#   filter(!is.na(ccode.home)) %>%                                                                                        
#   mutate_at(vars(poly:fre.exp),funs(./value)) %>% # Divide summed DVs by total number of connections
#   mutate_all(funs(replace(.,is.nan(.),0))) # Replace 0/0 observations with 0
# 
# names(weighted.vars.1)[4:13] <- paste("tsmo.",names(weighted.vars.1)[4:13],sep ="") # Rename independent variables
# 
# weighted.cnty.yrs <- full_join(vdem.main,weighted.vars.1, by = c("ccode" = "ccode.home","year" = "year")) %>% # Merge with VDem for all DVs
#   group_by(ccode) %>%
#   arrange(year) %>%
#   mutate_at(vars(poly:fre.exp),funs(lead = lead(.))) %>% #Lead all DVs one year
#   ungroup() %>%
#   mutate(cw = if_else(year<1991,1,0), # Create CW dummy variable
#          tsmo.island = if_else(value == 0,1,0)) %>% # Create TSMO Island dummy variable 
#   mutate(poly_diff = poly_lead - poly,
#          liberal_diff = liberal_lead - liberal,
#          partip_diff = partip_lead - partip,
#          delib_diff = delib_lead - delib,
#          egal_diff = egal_lead - egal,
#          elec_diff = elec_lead - elec,
#          frefair_diff = frefair_lead - frefair,
#          fr.assoc_diff = fr.assoc_lead - fr.assoc,
#          fre.exp_diff = fre.exp_lead - fre.exp,
#          suffr_diff = suffr_lead - suffr) %>% 
#   filter(year > 1952 & year < 2014 & ccode != 99999)
# 
# weighted.cnty.yrs <- left_join(weighted.cnty.yrs,io.cnty.yrs.max,by = c("ccode","year")) # Join to IO score dataset
# weighted.cnty.yrs <- left_join(weighted.cnty.yrs,dem.econ.perf.df,by = c("ccode","year")) # Join to Democracy economic performance indicator
# weighted.cnty.yrs <- left_join(weighted.cnty.yrs,tsmoscores,by = c("ccode","year")) # Join to TSMOscore variable
# weighted.cnty.yrs <- left_join(weighted.cnty.yrs,all.membership.numbers,by = c("ccode","year")) %>% # Join to Membership Counts
#   mutate(igo.island = if_else(num.igo.mems == 0,1,0))

