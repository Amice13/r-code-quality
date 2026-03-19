setwd("~/Data")

library(haven)
library(dplyr)
library(magrittr)

# Data comes from sweep 2 BCS 1970
# DOI: 10.5255/UKDA-SN-2699-4
# Study Number SN: 2699
df5 <- read_dta("f699a.dta")

df5age <- df5 %>% 
			select(bcsid, d025, d026, d027, d028, d029, d030, d031, d032, d033, d034, d035, d036, d037, d038, d039, d040, d041, d042, d043)

# Data comes from sweep 3 BCS 1970
# DOI: 10.5255/UKDA-SN-3723-7
# Study Number SN: 	3723
df10 <- read_dta("sn3723.dta")

df10age <- df10 %>% 
			select(bcsid, m43, m44, m45, m46, m47, m48, m49, m50, m51, m52, m53, m54, m55, m56, m57, m58, m59, m60, m61)

# Data comes from sweep 4 BCS 1970
# http://nesstar.ukdataservice.ac.uk/webview/velocity?v=2&mode=documentation&submode=abstract&study=http%3A%2F%2Fnesstar.ukdataservice.ac.uk%3A80%2Fobj%2FfStudy%2F3535
# UKDA-3535-stata11_se 
# file: bcs7016x.dta 
df16 <- read_dta("bcs7016x.dta")

df16age <- df16 %>%
			select(bcsid, pa5_1, 
				pa5_2, 
				pa5_3, 
				pa5_4, 
				pa5_5, 
				pa5_6, 
				pa5_7, 
				pa5_8, 
				pa5_9, 
				pa5_10, 
				pa5_11, 
				pa5_12, 
				pa5_13, 
				pa5_14, 
				pa5_15, 
				pa5_16, 
				pa5_17, 
				pa5_18, 
				pa5_19)

df510 <- merge(df5age, df10age, by = "bcsid", all = TRUE)
df <- merge(df510, df16age, by = "bcsid", all = TRUE)

library(car)

df$d025[df$d025 == -3] <- NA         
df$d026[df$d026 == -3] <- NA         
df$d027[df$d027 == -3] <- NA         
df$d028[df$d028 == -3] <- NA        
df$d029[df$d029 == -3] <- NA         
df$d030[df$d030 == -3] <- NA         
df$d031[df$d031 == -3] <- NA         
df$d032[df$d032 == -3] <- NA         
df$d033[df$d033 == -3] <- NA        
df$d034[df$d034 == -3] <- NA         
df$d035[df$d035 == -3] <- NA         
df$d036[df$d036 == -3] <- NA         
df$d037[df$d037 == -3] <- NA         
df$d038[df$d038 == -3] <- NA        
df$d039[df$d039 == -3] <- NA         
df$d040[df$d040 == -3] <- NA         
df$d041[df$d041 == -3] <- NA         
df$d042[df$d042 == -3] <- NA         
df$d043[df$d043 == -3] <- NA

#	df$m43[df$m43 < 0] <- NA          
#	df$m44[df$m44 < 0] <- NA          
#	df$m45[df$m45 < 0] <- NA          
#	df$m46[df$m46 < 0] <- NA          
#	df$m47[df$m47 < 0] <- NA         
#	df$m48[df$m48 < 0] <- NA          
#	df$m49[df$m49 < 0] <- NA          
#	df$m50[df$m50 < 0] <- NA          
#	df$m51[df$m51 < 0] <- NA          
#	df$m52[df$m52 < 0] <- NA         
#	df$m53[df$m53 < 0] <- NA          
#	df$m54[df$m54 < 0] <- NA          
#	df$m55[df$m55 < 0] <- NA          
#	df$m56[df$m56 < 0] <- NA          
#	df$m57[df$m57 < 0] <- NA         
#	df$m58[df$m58 < 0] <- NA          
#	df$m59[df$m59 < 0] <- NA          
#	df$m60[df$m60 < 0] <- NA          
#	df$m61[df$m61 < 0] <- NA   



df$m43a <- car::recode(df$m43, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m43 <- as.integer(df$m43a)
df$m43a <- NULL
df$m44a <- car::recode(df$m44, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m44 <- as.integer(df$m44a)
df$m44a <- NULL
df$m45a <- car::recode(df$m45, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m45 <- as.integer(df$m45a)
df$m45a <- NULL
df$m46a <- car::recode(df$m46, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m46 <- as.integer(df$m46a)
df$m46a <- NULL
df$m47a <- car::recode(df$m47, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m47 <- as.integer(df$m47a)
df$m47a <- NULL
df$m48a <- car::recode(df$m48, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m48 <- as.integer(df$m48a)
df$m48a <- NULL
df$m49a <- car::recode(df$m49, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m49 <- as.integer(df$m49a)
df$m49a <- NULL
df$m50a <- car::recode(df$m50, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m50 <- as.integer(df$m50a)
df$m50a <- NULL
df$m51a <- car::recode(df$m51, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m51 <- as.integer(df$m51a)
df$m51a <- NULL
df$m52a <- car::recode(df$m52, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m52 <- as.integer(df$m52a)
df$m52a <- NULL
df$m53a <- car::recode(df$m53, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m53 <- as.integer(df$m53a)
df$m53a <- NULL
df$m54a <- car::recode(df$m54, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m54 <- as.integer(df$m54a)
df$m54a <- NULL
df$m55a <- car::recode(df$m55, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m55 <- as.integer(df$m55a)
df$m55a <- NULL
df$m56a <- car::recode(df$m56, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m56 <- as.integer(df$m56a)
df$m56a <- NULL
df$m57a <- car::recode(df$m57, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m57 <- as.integer(df$m57a)
df$m57a <- NULL
df$m58a <- car::recode(df$m58, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m58 <- as.integer(df$m58a)
df$m58a <- NULL
df$m59a <- car::recode(df$m59, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m59 <- as.integer(df$m59a)
df$m59a <- NULL
df$m60a <- car::recode(df$m60, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m60 <- as.integer(df$m60a)
df$m60a <- NULL
df$m61a <- car::recode(df$m61, "0:20 = 1; 21:80 = 2; 81:100 = 3; -7:-1 = NA")
df$m61 <- as.integer(df$m61a)
df$m61a <- NULL


df$pa5_1a <- car::recode(df$pa5_1, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_1 <- as.integer(df$pa5_1a)
df$pa5_1a <- NULL
df$pa5_2a <- car::recode(df$pa5_2, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_2 <- as.integer(df$pa5_2a)
df$pa5_2a <- NULL
df$pa5_3a <- car::recode(df$pa5_3, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_3 <- as.integer(df$pa5_3a)
df$pa5_3a <- NULL
df$pa5_4a <- car::recode(df$pa5_4, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_4 <- as.integer(df$pa5_4a)
df$pa5_4a <- NULL
df$pa5_5a <- car::recode(df$pa5_5, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_5 <- as.integer(df$pa5_5a)
df$pa5_5a <- NULL
df$pa5_6a <- car::recode(df$pa5_6, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_6 <- as.integer(df$pa5_6a)
df$pa5_6a <- NULL
df$pa5_7a <- car::recode(df$pa5_7, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_7 <- as.integer(df$pa5_7a)
df$pa5_7a <- NULL
df$pa5_8a <- car::recode(df$pa5_8, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_8 <- as.integer(df$pa5_8a)
df$pa5_8a <- NULL
df$pa5_9a <- car::recode(df$pa5_9, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_9 <- as.integer(df$pa5_9a)
df$pa5_9a <- NULL
df$pa5_10a <- car::recode(df$pa5_10, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_10 <- as.integer(df$pa5_10a)
df$pa5_10a <- NULL
df$pa5_11a <- car::recode(df$pa5_11, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_11 <- as.integer(df$pa5_11a)
df$pa5_11a <- NULL
df$pa5_12a <- car::recode(df$pa5_12, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_12 <- as.integer(df$pa5_12a)
df$pa5_12a <- NULL
df$pa5_13a <- car::recode(df$pa5_13, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_13 <- as.integer(df$pa5_13a)
df$pa5_13a <- NULL
df$pa5_14a <- car::recode(df$pa5_14, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_14 <- as.integer(df$pa5_14a)
df$pa5_14a <- NULL
df$pa5_15a <- car::recode(df$pa5_15, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_15 <- as.integer(df$pa5_15a)
df$pa5_15a <- NULL
df$pa5_16a <- car::recode(df$pa5_16, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_16 <- as.integer(df$pa5_16a)
df$pa5_16a <- NULL
df$pa5_17a <- car::recode(df$pa5_17, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_17 <- as.integer(df$pa5_17a)
df$pa5_17a <- NULL
df$pa5_18a <- car::recode(df$pa5_18, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_18 <- as.integer(df$pa5_18a)
df$pa5_18a <- NULL
df$pa5_19a <- car::recode(df$pa5_19, "3 = 1; 1 = 3; -1 = NA; -2 = NA")
df$pa5_19 <- as.integer(df$pa5_19a)
df$pa5_19a <- NULL


### Composite score for conduct problems

df$dCon <- (df$d027 + df$d028 + df$d034 + df$d038 + df$d042 + df$d043)/6

df$mCon <- (df$m45 + df$m46 + df$m52 + df$m56 + df$m60 + df$m61)/6

df$paCon <- (df$pa5_3 + df$pa5_4 + df$pa5_10 + df$pa5_14 + df$pa5_18 + df$pa5_19)/6



setwd("~/Replication files")

write_dta(df, "df_traj_rep.dta")


