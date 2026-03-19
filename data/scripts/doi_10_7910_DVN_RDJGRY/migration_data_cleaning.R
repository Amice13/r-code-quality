# Import relevant packages


source('scripts/cleaning_and_management/libraries.R') 


##################
#### EUROSTAT ####

# extract data from Eurostat - Immigration by age group, sex and country of previous residence. EU and non-EU citizens that were residing outside 
# and migrated to the EU are counted.

# Data downloaded on August 2020 from Eurostat, and if downloaded again, some figures, especially from the  
# most recent years may have been updated. 

# Eurostat package to load migration data. 

# Total inflows

mig_res <- get_eurostat("migr_imm5prv",
                        type = "label",
                        time_format = "num")

mig_res$partner_cha <- as.character(mig_res$partner)
mig_res$geo_cha <- as.character(mig_res$geo)

#identify eu28 countries of destinations (geo) with countrycode package
mig_res$eu28 <- countrycode::countrycode(mig_res$geo_cha,"country.name", "eu28",warn = T)

# filter age definition, all ages, all sex, and only eu-28 destination countries
mig_res <- mig_res %>% 
  filter(age=="Total",agedef=="Age reached during the year",sex=="Total", eu28=="EU")

eu_filter <- c("Non-EU28 countries (2013-2020) nor reporting country","European Free Trade Association except reporting country","Non-EU27 countries (2007-2013) nor reporting country")

non_eu <- mig_res %>% 
  dplyr::filter(time %in% 2008:2017, partner_cha %in% eu_filter) %>% 
  dplyr::group_by(time,partner_cha) %>% 
  dplyr::summarise(immigration=sum(values, na.rm = T)) %>% 
  tidyr::spread(partner_cha,immigration) %>% 
  dplyr::mutate(immigration1= `Non-EU28 countries (2013-2020) nor reporting country`-`European Free Trade Association except reporting country`,
                immigration2= `Non-EU27 countries (2007-2013) nor reporting country`-`European Free Trade Association except reporting country`) %>%  
  ungroup() %>% 
  mutate(val = select(., contains("immigration")) %>% rowSums(na.rm = T)) %>% 
  select(year=time, val) %>% 
  add_row(year = 2020, val = NA) 

rm(mig_res)

# Asylum 

# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=migr_asyappctza&lang=en 

mig_res_asylum <- get_eurostat("migr_asyappctza",
                               type = "label",
                               time_format = "num")

mig_res_asylum <- mig_res_asylum %>% 
  filter(
    age=="Total",
         asyl_app=="First time applicant",
         sex=="Total",
         geo=="European Union - 28 countries (2013-2020)", 
         citizen=="Extra-EU28 (2013-2020)",
         time<2019) %>% 
  group_by(time) %>% 
  summarise(first=sum(values,na.rm = T)) 


# Labor
mig_res_labor <- get_eurostat("migr_resocc",
                              type = "label",
                              time_format = "num")

mig_res_labor <- mig_res_labor %>% 
  filter(duration=="Total", geo=="European Union - 28 countries (2013-2020)", 
         citizen=="Total",
         time <= 2018) %>% 
  group_by(time,reason) %>% 
  summarise(first=sum(values,na.rm = T)) %>% 
  spread(reason,first) 

mig_res_labor$high_rese  <- (mig_res_labor$"Remunerated activities reasons: Highly skilled workers" + 
                               mig_res_labor$"Remunerated activities reasons: Researchers")

mig_res_labor <- mig_res_labor %>% 
  rename("labour"="Remunerated activities reasons") 


# High
mig_res_labor$high_rese

#################
#### FRONTEX ####

# Data from https://frontex.europa.eu/along-eu-borders/migratory-map/ 

irregular <- read_xlsx("data/Migration Data/Frontex/Detections_of_IBC_2019_09_04.xlsx", "Detections_of_IBC")

# Tidy irregular data
irregular <- irregular %>% 
  tidyr::gather(key=time,value=total,4:130) %>% 
  tidyr::separate(time, c("month", "year"), sep = 3) %>% 
  dplyr::select(-"Border location",-Route) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(total_ir=sum(total,na.rm = T)) %>% 
  mutate(year=as.numeric(year)) %>% 
  filter(year<2019)


########################
#### Flows together ####

# data relevant for forecast
# total flows
total <- non_eu %>% 
  mutate(flow = "total") %>% 
  drop_na()%>% 
  as.data.frame()

# labour flows

labour <- mig_res_labor %>%
  select(val=labour, year=time) %>% 
  mutate(flow = "labour")%>% 
  as.data.frame()

# high skilled flows

high <- mig_res_labor %>% 
  select(year=time, val=high_rese) %>% 
  mutate(flow="high") %>% 
  as.data.frame()

# asylum 

asylum <- mig_res_asylum %>% 
  select(year=time, val=first) %>% 
  mutate(flow="asylum")%>% 
  as.data.frame()

# irregular
irregular <- irregular %>%
  select(year, val=total_ir) %>% 
  mutate(flow="irregular") %>% 
  as.data.frame()

# all flows together

flows_forecast <- rbind(total, labour, high, irregular, asylum)

saveRDS(flows_forecast, "data/Migration data/clean(Frontex+Eurostat)/forecast.rds")


