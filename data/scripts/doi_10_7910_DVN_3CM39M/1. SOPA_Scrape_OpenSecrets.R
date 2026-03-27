# load the packages
library(rvest, dplyr, writexl, readxl)

############ Identify the interest groups active on specific topics ##########
scrape_os_topics =  function(link, table_no = 1, odd_links = "T") {
  
  content = read_html(link) #scrape
  
  tables = content %>% 
    html_table(fill = TRUE) # extract tables
  
  df = tables[[table_no]] # extract first table
  
  #get links from table 
  links = content %>%
    html_nodes("table") %>%
    html_nodes("tr") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  #some topics link to reports, some not. Links to reports must be skipped.
  if(odd_links == "T"){
    df$regid = links[c(F,T)]
  } else {
    df$regid = links
  }
  
  # extract the Open Secrets ID from the link
  if(odd_links == "T"){
    df$regid = gsub(".*client_id=(.*)&cycle.*", "\\1", df$regid)
  } else {
    df$regid = gsub(".*&id=(.*)", "\\1", df$regid)
  }
  
  return(df)
}

# Apply function to COICA
IG_COICA = scrape_os_topics("https://www.opensecrets.org/federal-lobbying/bills/summary?id=s3804-111&start=1&page_length=100")

# Apply function to SOPA
IG_SOPA = scrape_os_topics("https://www.opensecrets.org/federal-lobbying/bills/summary?id=hr3261-112&page_length=300")

# Apply function to Copyright, Trademarks, Patents
# 2010
IG_CTP10 = scrape_os_topics("https://www.opensecrets.org/federal-lobbying/issues/summary?cycle=2010&id=CPT&page_length=500&start=25", odd_links = "F")
# 2011
IG_CTP11 = scrape_os_topics("https://www.opensecrets.org/federal-lobbying/issues/summary?cycle=2011&id=CPT&start=25&page_length=500", odd_links = "F")
# 2012
IG_CTP12 = scrape_os_topics("https://www.opensecrets.org/federal-lobbying/issues/summary?cycle=2012&id=CPT&page_length=500&start=25", odd_links = "F")

IG_COICA = select(IG_COICA, c(Client, regid))
IG_SOPA = select(IG_SOPA, c(Client, regid))
IG_CTP10 = select(IG_CTP10, c(Client, regid))
IG_CTP11 = select(IG_CTP11, c(Client, regid))
IG_CTP12 = select(IG_CTP12, c(Client, regid))

IG_OS = unique(rbind(IG_COICA, IG_SOPA, IG_CTP10, IG_CTP11, IG_CTP12))

############### scrape lobbying expenditure, lobbyists and revolvers for the IGs active on Copyright, SOPA, COICA ##################
scrape_OS_lobby = function(data, year){
  
  exp_list = vector(mode = "list")
  lobbyists_list = vector(mode = "list")
  revolvers_list = vector(mode = "list")
  regid_list = vector(mode = "list")
  
  for(i in 1:nrow(data)){
    
    print(i)
    
    link = paste0("https://www.opensecrets.org/federal-lobbying/clients/lobbyists?cycle=",year,"&id=",data$regid[i])
    
    content = read_html(link)
    
    #get links from table 
    OS_data = content %>%
      html_nodes("h3")
    
    if(length(OS_data) == 5){
      exp_list[[i]] = gsub(".*>(.*)</h3>", "\\1", OS_data[1])
      lobbyists_list[[i]] = gsub(".*>(.*)</h3>", "\\1", OS_data[3])
      revolvers_list[[i]] = gsub(".*>(.*)</h3>", "\\1", OS_data[4])
      
      exp_list[[i]] = as.integer(gsub("\\$|,", "", exp_list[[i]]))
      lobbyists_list[[i]] = as.integer(lobbyists_list[[i]])
      revolvers_list[[i]] = as.integer(gsub("(.*) \\(.*", "\\1", revolvers_list[[i]]))
      
      regid_list[[i]] = data$regid[i]
    } else if(length(OS_data) == 3){
        exp_list[[i]] = 0
        lobbyists_list[[i]] = gsub(".*>(.*)</h3>", "\\1", OS_data[1])
        revolvers_list[[i]] = gsub(".*>(.*)</h3>", "\\1", OS_data[2])
        
        lobbyists_list[[i]] = as.integer(lobbyists_list[[i]])
        revolvers_list[[i]] = as.integer(gsub("(.*) \\(.*", "\\1", revolvers_list[[i]]))
        
        regid_list[[i]] = data$regid[i]
    } else {
        exp_list[[i]] = NA
        lobbyists_list[[i]] = NA
        revolvers_list[[i]] = NA
        
        regid_list[[i]] = data$regid[i]
        print(paste0("weird",i))
    }
  }
  
  out = cbind(data.frame(do.call(rbind, exp_list)),
              data.frame(do.call(rbind, lobbyists_list)),
              data.frame(do.call(rbind, revolvers_list)),
              data.frame(do.call(rbind, regid_list)))
  
  # name the columns in the dataframe
  names(out)[1] <- "lobby_exp"
  names(out)[2] <- "lobbyists"
  names(out)[3] <- "revolvers"
  names(out)[4] <- "regid"
  
  return(out)
}

lob_10 = scrape_OS_lobby(IG_OS, year = 2010)
lob_11 = scrape_OS_lobby(IG_OS, year = 2011)
lob_12 = scrape_OS_lobby(IG_OS, year = 2012)

names(lob_10)[1] <- "lobby_exp_10"
names(lob_11)[1] <- "lobby_exp_11"
names(lob_12)[1] <- "lobby_exp_12"

names(lob_10)[2] <- "lobbyists_10"
names(lob_11)[2] <- "lobbyists_11"
names(lob_12)[2] <- "lobbyists_12"

names(lob_10)[3] <- "revolvers_10"
names(lob_11)[3] <- "revolvers_11"
names(lob_12)[3] <- "revolvers_12"

lob = full_join(lob_10, lob_11, by = "regid")
lob = full_join(lob, lob_12, by = "regid")

IG_OS_full = full_join(IG_OS, lob, by = "regid")

IG_OS_full = unique(IG_OS_full)

######### Scrape info on the industries of IGs ######
# Open Secrets has information on the industries in which IGs are active on the summary site. I scrape this info as a first indicator on the industry of
# a firm.

# define a function to scrape Open Secrets website
scrape_os_industry = function(data, year){
  
  # initiate empty lists to store results
  regid_list = vector(mode = "list")
  industries_list = vector(mode = "list")
  year_list = vector(mode = "list")
  
  # loop over the ids of the IGs (which appear at the end of each summary page's link)
  for(i in 1:nrow(data)){
    print(i)
    
    link = paste0("https://www.opensecrets.org/federal-lobbying/clients/summary?cycle=",year,"&id=",data$regid[i]) #generate link
    
    content = read_html(link) # read link
    
    tables = content %>% 
    html_table(fill = TRUE) # extract tables
    
    regid_list[[i]] = data$regid[i] # fill in results
    year_list[[i]] = year
    
    if(length(tables) == 1){ # if there is only one table on a page it is the table with the industry info
      industries_list[[i]] = paste(tables[[1]]$Industry, collapse = ", ")
    } else if (length(tables) == 2) { # if there are two tables, the second one has the info on industries
      industries_list[[i]] = paste(tables[[2]]$Industry, collapse = ", ")
    } else if (length(tables) == 0) { # if there is no table, industry is unknown
      industries_list[[i]] = NA
    } else { # warn if there are annormalities
      industries_list[[i]] = NA
      print(paste("weird", i))
    }
  } 
  
  # put results into dataframe
  out = cbind(data.frame(do.call(rbind, regid_list)),
              data.frame(do.call(rbind, year_list)),
              data.frame(do.call(rbind, industries_list)))
  
  # name the columns in the dataframe
  names(out)[1] <- "regid"
  names(out)[2] <- "year"
  names(out)[3] <- "industry"
  
  return(out)
}

# apply function
industry_10 = scrape_os_industry(IG_OS, year = 2010)
industry_11 = scrape_os_industry(IG_OS, year = 2011)
industry_12 = scrape_os_industry(IG_OS, year = 2012)

# bind results together
industry = rbind(industry_10, industry_11, industry_12)

# delete info on 2011 and 2012 if industry stayed the same
industry = industry[!duplicated(industry[,c("regid","industry")]),]

# convert from long to wide format
industry = spread(industry, year, industry)

# rename columns
names(industry)[2] = "industry_10"
names(industry)[3] = "industry_11"
names(industry)[4] = "industry_12"

# merge data to IG data
IG_OS_full = full_join(IG_OS_full, industry, by = "regid")

################## Scrape industry Info and lobbying data for manually identified IGs (from SOPA_interest Groups file) ###################
SOPA_IGs = read_xlsx("SOPA_Interest Groups.xlsx")

# lobbying data
man_lob_10 = scrape_OS_lobby(SOPA_IGs, year = 2010)
man_lob_11 = scrape_OS_lobby(SOPA_IGs, year = 2011)
man_lob_12 = scrape_OS_lobby(SOPA_IGs, year = 2012)

man_lob_10 = subset(man_lob_10, !is.na(regid))
man_lob_11 = subset(man_lob_11, !is.na(regid))
man_lob_12 = subset(man_lob_12, !is.na(regid))

names(man_lob_10)[1] <- "lobby_exp_10"
names(man_lob_11)[1] <- "lobby_exp_11"
names(man_lob_12)[1] <- "lobby_exp_12"

names(man_lob_10)[2] <- "lobbyists_10"
names(man_lob_11)[2] <- "lobbyists_11"
names(man_lob_12)[2] <- "lobbyists_12"

names(man_lob_10)[3] <- "revolvers_10"
names(man_lob_11)[3] <- "revolvers_11"
names(man_lob_12)[3] <- "revolvers_12"

man_lob = full_join(man_lob_10, man_lob_11, by = "regid")
man_lob = full_join(man_lob, man_lob_12, by = "regid")

man_lob = unique(man_lob)

SOPA_IGs_full = full_join(SOPA_IGs, man_lob, by = "regid")

# industry data
man_industry_10 = scrape_os_industry(SOPA_IGs, year = 2010)
man_industry_11 = scrape_os_industry(SOPA_IGs, year = 2011)
man_industry_12 = scrape_os_industry(SOPA_IGs, year = 2012)

# bind results together
man_industry = rbind(man_industry_10, man_industry_11, man_industry_12)

# delete info on 2011 and 2012 if industry stayed the same
man_industry = man_industry[!duplicated(man_industry[,c("regid","industry")]),]

# convert from long to wide format
man_industry = spread(man_industry, year, industry)

# rename columns
names(man_industry)[2] = "industry_10"
names(man_industry)[3] = "industry_11"
names(man_industry)[4] = "industry_12"

# merge data to IG data
SOPA_IGs_full = full_join(SOPA_IGs_full, man_industry, by = "regid")

IG_full = full_join(SOPA_IGs_full, IG_OS_full, by = intersect(colnames(SOPA_IGs_full), colnames(IG_OS_full)))

# export final dataset
write_xlsx(IG_full, "OpenSecrets_IGs_lobbying_on_Copyright.xlsx")


