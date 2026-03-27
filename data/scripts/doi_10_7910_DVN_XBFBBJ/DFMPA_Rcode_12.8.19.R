obs = read.csv("SCI_20122013_all_FINAL_150723.csv") #read obs data
nav = read.csv("NAV_COMBINED_SCMPA_130624.csv") #read nav data

head(obs) #check obs data
head(nav) #check nav data

#########################################################################################

#calculate mean depths of each transect from nav data

depth_SCF01 = mean(nav$Depth[nav$Tran == "SCF01"])
depth_SCF02 = mean(nav$Depth[nav$Tran == "SCF02"])
depth_SCF03 = mean(nav$Depth[nav$Tran == "SCF03"])
depth_SCF04 = mean(nav$Depth[nav$Tran == "SCF04"])
depth_SCF05 = mean(nav$Depth[nav$Tran == "SCF05"])
depth_SCF06 = mean(nav$Depth[nav$Tran == "SCF06"])
depth_SCF50 = mean(nav$Depth[nav$Tran == "SCF50"])
depth_SCF51 = mean(nav$Depth[nav$Tran == "SCF51"])
depth_SCF52 = mean(nav$Depth[nav$Tran == "SCF52"])
depth_SCF53 = mean(nav$Depth[nav$Tran == "SCF53"])
depth_SCF54 = mean(nav$Depth[nav$Tran == "SCF54"])
depth_SCF55 = mean(nav$Depth[nav$Tran == "SCF55"])
depth_SCF60a = mean(nav$Depth[nav$Tran == "SCF60a"])
depth_SCF60b = mean(nav$Depth[nav$Tran == "SCF60b"])
depth_SCF60c = mean(nav$Depth[nav$Tran == "SCF60c"])

depth_SCG01 = mean(nav$Depth[nav$Tran == "SCG01"])
depth_SCG02 = mean(nav$Depth[nav$Tran == "SCG02"])
depth_SCG03 = mean(nav$Depth[nav$Tran == "SCG03"])
depth_SCG04 = mean(nav$Depth[nav$Tran == "SCG04"])
depth_SCG05 = mean(nav$Depth[nav$Tran == "SCG05"])
depth_SCG06 = mean(nav$Depth[nav$Tran == "SCG06"])
depth_SCG07 = mean(nav$Depth[nav$Tran == "SCG07"])
depth_SCG50 = mean(nav$Depth[nav$Tran == "SCG50"])
depth_SCG51 = mean(nav$Depth[nav$Tran == "SCG51"])
depth_SCG52a = mean(nav$Depth[nav$Tran == "SCG52a"])
depth_SCG52b = mean(nav$Depth[nav$Tran == "SCG52b"])
depth_SCG53a = mean(nav$Depth[nav$Tran == "SCG53a"])
depth_SCG53b = mean(nav$Depth[nav$Tran == "SCG53b"])
depth_SCG55 = mean(nav$Depth[nav$Tran == "SCG55"])
depth_SCG56 = mean(nav$Depth[nav$Tran == "SCG56"])
depth_SCG57 = mean(nav$Depth[nav$Tran == "SCG57"])
depth_SCG58 = mean(nav$Depth[nav$Tran == "SCG58"])
depth_SCG60 = mean(nav$Depth[nav$Tran == "SCG60"])
depth_SCG62 = mean(nav$Depth[nav$Tran == "SCG62"])

depth_SCF01 
depth_SCF02 
depth_SCF03 
depth_SCF04 
depth_SCF05 
depth_SCF06 
depth_SCF50 
depth_SCF51 
depth_SCF52 
depth_SCF53 
depth_SCF54 
depth_SCF55 
depth_SCF60a 
depth_SCF60b
depth_SCF60c

depth_SCG01 
depth_SCG02 
depth_SCG03 
depth_SCG04 
depth_SCG05 
depth_SCG06 
depth_SCG07 
depth_SCG50 
depth_SCG51
depth_SCG52a 
depth_SCG52b 
depth_SCG53a 
depth_SCG53b 
depth_SCG55 
depth_SCG56 
depth_SCG57 
depth_SCG58 
depth_SCG60 
depth_SCG62 

#########################################################################################

#Determine predominant substrate on each transect based on habitat patch data

#rule: if >50% of the transect was over hard or mixed habitat, call transect "rock", if
# >50% of transect was over soft habitat, call transect "sand"

HabitatPatch_2012 = read.csv("SCI_HabitatPatch_2012.csv") #read in 2012 hab patch data
head(HabitatPatch_2012) #check

hab_SCF01 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCF01", ]
count_rock_SCF01 = length(hab_SCF01$Habitat[hab_SCF01$Habitat == "HARD" | hab_SCF01$Habitat == "MIXED"])
count_total_SCF01 = length(hab_SCF01$Habitat[hab_SCF01$Habitat == "HARD" | hab_SCF01$Habitat == "MIXED" | hab_SCF01$Habitat == "SOFT"])
percentrock_SCF01 = count_rock_SCF01 / count_total_SCF01 *100

hab_SCF02 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCF02", ]
count_rock_SCF02 = length(hab_SCF02$Habitat[hab_SCF02$Habitat == "HARD" | hab_SCF02$Habitat == "MIXED"])
count_total_SCF02 = length(hab_SCF02$Habitat[hab_SCF02$Habitat == "HARD" | hab_SCF02$Habitat == "MIXED" | hab_SCF02$Habitat == "SOFT"]) 
percentrock_SCF02 = count_rock_SCF02 / count_total_SCF02 *100

hab_SCF03 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCF03", ]
count_rock_SCF03 = length(hab_SCF03$Habitat[hab_SCF03$Habitat == "HARD" | hab_SCF03$Habitat == "MIXED"])
count_total_SCF03 = length(hab_SCF03$Habitat[hab_SCF03$Habitat == "HARD" | hab_SCF03$Habitat == "MIXED" | hab_SCF03$Habitat == "SOFT"]) 
percentrock_SCF03 = count_rock_SCF03 / count_total_SCF03 *100

hab_SCF04 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCF04", ]
count_rock_SCF04 = length(hab_SCF04$Habitat[hab_SCF04$Habitat == "HARD" | hab_SCF04$Habitat == "MIXED"])
count_total_SCF04 = length(hab_SCF04$Habitat[hab_SCF04$Habitat == "HARD" | hab_SCF04$Habitat == "MIXED" | hab_SCF04$Habitat == "SOFT"]) 
percentrock_SCF04 = count_rock_SCF04 / count_total_SCF04 *100

hab_SCF05 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCF05", ]
count_rock_SCF05 = length(hab_SCF05$Habitat[hab_SCF05$Habitat == "HARD" | hab_SCF05$Habitat == "MIXED"])
count_total_SCF05 = length(hab_SCF05$Habitat[hab_SCF05$Habitat == "HARD" | hab_SCF05$Habitat == "MIXED" | hab_SCF05$Habitat == "SOFT"]) 
percentrock_SCF05 = count_rock_SCF05 / count_total_SCF05 *100

hab_SCF06 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCF06", ]
count_rock_SCF06 = length(hab_SCF06$Habitat[hab_SCF06$Habitat == "HARD" | hab_SCF06$Habitat == "MIXED"])
count_total_SCF06 = length(hab_SCF06$Habitat[hab_SCF06$Habitat == "HARD" | hab_SCF06$Habitat == "MIXED" | hab_SCF06$Habitat == "SOFT"]) 
percentrock_SCF06 = count_rock_SCF06 / count_total_SCF06 *100

hab_SCG01 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCG01", ]
count_rock_SCG01 = length(hab_SCG01$Habitat[hab_SCG01$Habitat == "HARD" | hab_SCG01$Habitat == "MIXED"])
count_total_SCG01 = length(hab_SCG01$Habitat[hab_SCG01$Habitat == "HARD" | hab_SCG01$Habitat == "MIXED" | hab_SCG01$Habitat == "SOFT"]) 
percentrock_SCG01 = count_rock_SCG01 / count_total_SCG01 *100

hab_SCG02 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCG02", ]
count_rock_SCG02 = length(hab_SCG02$Habitat[hab_SCG02$Habitat == "HARD" | hab_SCG02$Habitat == "MIXED"])
count_total_SCG02 = length(hab_SCG02$Habitat[hab_SCG02$Habitat == "HARD" | hab_SCG02$Habitat == "MIXED" | hab_SCG02$Habitat == "SOFT"]) 
percentrock_SCG02 = count_rock_SCG02 / count_total_SCG02 *100

hab_SCG03 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCG03", ]
count_rock_SCG03 = length(hab_SCG03$Habitat[hab_SCG03$Habitat == "HARD" | hab_SCG03$Habitat == "MIXED"])
count_total_SCG03 = length(hab_SCG03$Habitat[hab_SCG03$Habitat == "HARD" | hab_SCG03$Habitat == "MIXED" | hab_SCG03$Habitat == "SOFT"]) 
percentrock_SCG03 = count_rock_SCG03 / count_total_SCG03 *100

hab_SCG04 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCG04", ]
count_rock_SCG04 = length(hab_SCG04$Habitat[hab_SCG04$Habitat == "HARD" | hab_SCG04$Habitat == "MIXED"])
count_total_SCG04 = length(hab_SCG04$Habitat[hab_SCG04$Habitat == "HARD" | hab_SCG04$Habitat == "MIXED" | hab_SCG04$Habitat == "SOFT"]) 
percentrock_SCG04 = count_rock_SCG04 / count_total_SCG04 *100

hab_SCG05 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCG05", ]
count_rock_SCG05 = length(hab_SCG05$Habitat[hab_SCG05$Habitat == "HARD" | hab_SCG05$Habitat == "MIXED"])
count_total_SCG05 = length(hab_SCG05$Habitat[hab_SCG05$Habitat == "HARD" | hab_SCG05$Habitat == "MIXED" | hab_SCG05$Habitat == "SOFT"]) 
percentrock_SCG05 = count_rock_SCG05 / count_total_SCG05 *100

hab_SCG06 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCG06", ]
count_rock_SCG06 = length(hab_SCG06$Habitat[hab_SCG06$Habitat == "HARD" | hab_SCG06$Habitat == "MIXED"])
count_total_SCG06 = length(hab_SCG06$Habitat[hab_SCG06$Habitat == "HARD" | hab_SCG06$Habitat == "MIXED" | hab_SCG06$Habitat == "SOFT"]) 
percentrock_SCG06 = count_rock_SCG06 / count_total_SCG06 *100

hab_SCG07 = HabitatPatch_2012[HabitatPatch_2012$TransectID == "SCG07", ]
count_rock_SCG07 = length(hab_SCG07$Habitat[hab_SCG07$Habitat == "HARD" | hab_SCG07$Habitat == "MIXED"])
count_total_SCG07 = length(hab_SCG07$Habitat[hab_SCG07$Habitat == "HARD" | hab_SCG07$Habitat == "MIXED" | hab_SCG07$Habitat == "SOFT"]) 
percentrock_SCG07 = count_rock_SCG07 / count_total_SCG07 *100

hab_SCF50 = read.csv("SCI_HabitatPatch_2013_SCF50.csv")
count_rock_SCF50 = length(hab_SCF50$Habitat[hab_SCF50$Habitat == "HARD" | hab_SCF50$Habitat == "MIXED"])
count_total_SCF50 = length(hab_SCF50$Habitat[hab_SCF50$Habitat == "HARD" | hab_SCF50$Habitat == "MIXED" | hab_SCF50$Habitat == "SOFT"]) 
percentrock_SCF50 = count_rock_SCF50 / count_total_SCF50 *100

hab_SCF51 = read.csv("SCI_HabitatPatch_2013_SCF51.csv")
count_rock_SCF51 = length(hab_SCF51$Habitat[hab_SCF51$Habitat == "HARD" | hab_SCF51$Habitat == "MIXED"])
count_total_SCF51 = length(hab_SCF51$Habitat[hab_SCF51$Habitat == "HARD" | hab_SCF51$Habitat == "MIXED" | hab_SCF51$Habitat == "SOFT"]) 
percentrock_SCF51 = count_rock_SCF51 / count_total_SCF51 *100

hab_SCF52 = read.csv("SCI_HabitatPatch_2013_SCF52.csv")
count_rock_SCF52 = length(hab_SCF52$Habitat[hab_SCF52$Habitat == "HARD" | hab_SCF52$Habitat == "MIXED"])
count_total_SCF52 = length(hab_SCF52$Habitat[hab_SCF52$Habitat == "HARD" | hab_SCF52$Habitat == "MIXED" | hab_SCF52$Habitat == "SOFT"]) 
percentrock_SCF52 = count_rock_SCF52 / count_total_SCF52 *100

hab_SCF53 = read.csv("SCI_HabitatPatch_2013_SCF53.csv")
count_rock_SCF53 = length(hab_SCF53$Habitat[hab_SCF53$Habitat == "HARD" | hab_SCF53$Habitat == "MIXED"])
count_total_SCF53 = length(hab_SCF53$Habitat[hab_SCF53$Habitat == "HARD" | hab_SCF53$Habitat == "MIXED" | hab_SCF53$Habitat == "SOFT"]) 
percentrock_SCF53 = count_rock_SCF53 / count_total_SCF53 *100

hab_SCF54 = read.csv("SCI_HabitatPatch_2013_SCF54.csv")
count_rock_SCF54 = length(hab_SCF54$Habitat[hab_SCF54$Habitat == "HARD" | hab_SCF54$Habitat == "MIXED"])
count_total_SCF54 = length(hab_SCF54$Habitat[hab_SCF54$Habitat == "HARD" | hab_SCF54$Habitat == "MIXED" | hab_SCF54$Habitat == "SOFT"]) 
percentrock_SCF54 = count_rock_SCF54 / count_total_SCF54 *100

hab_SCF55 = read.csv("SCI_HabitatPatch_2013_SCF55.csv")
count_rock_SCF55 = length(hab_SCF55$Habitat[hab_SCF55$Habitat == "HARD" | hab_SCF55$Habitat == "MIXED"])
count_total_SCF55 = length(hab_SCF55$Habitat[hab_SCF55$Habitat == "HARD" | hab_SCF55$Habitat == "MIXED" | hab_SCF55$Habitat == "SOFT"]) 
percentrock_SCF55 = count_rock_SCF55 / count_total_SCF55 *100

hab_SCF60a = read.csv("SCI_HabitatPatch_2013_SCF60a.csv")
count_rock_SCF60a = length(hab_SCF60a$Habitat[hab_SCF60a$Habitat == "HARD" | hab_SCF60a$Habitat == "MIXED"])
count_total_SCF60a = length(hab_SCF60a$Habitat[hab_SCF60a$Habitat == "HARD" | hab_SCF60a$Habitat == "MIXED" | hab_SCF60a$Habitat == "SOFT"]) 
percentrock_SCF60a = count_rock_SCF60a / count_total_SCF60a *100

hab_SCF60b = read.csv("SCI_HabitatPatch_2013_SCF60b.csv")
count_rock_SCF60b = length(hab_SCF60b$Habitat[hab_SCF60b$Habitat == "HARD" | hab_SCF60b$Habitat == "MIXED"])
count_total_SCF60b = length(hab_SCF60b$Habitat[hab_SCF60b$Habitat == "HARD" | hab_SCF60b$Habitat == "MIXED" | hab_SCF60b$Habitat == "SOFT"]) 
percentrock_SCF60b = count_rock_SCF60b / count_total_SCF60b *100

hab_SCF60c = read.csv("SCI_HabitatPatch_2013_SCF60c.csv")
count_rock_SCF60c = length(hab_SCF60c$Habitat[hab_SCF60c$Habitat == "HARD" | hab_SCF60c$Habitat == "MIXED"])
count_total_SCF60c = length(hab_SCF60c$Habitat[hab_SCF60c$Habitat == "HARD" | hab_SCF60c$Habitat == "MIXED" | hab_SCF60c$Habitat == "SOFT"]) 
percentrock_SCF60c = count_rock_SCF60c / count_total_SCF60c *100

hab_SCG50 = read.csv("SCI_HabitatPatch_2013_SCG50.csv")
count_rock_SCG50 = length(hab_SCG50$Habitat[hab_SCG50$Habitat == "HARD" | hab_SCG50$Habitat == "MIXED"])
count_total_SCG50 = length(hab_SCG50$Habitat[hab_SCG50$Habitat == "HARD" | hab_SCG50$Habitat == "MIXED" | hab_SCG50$Habitat == "SOFT"]) 
percentrock_SCG50 = count_rock_SCG50 / count_total_SCG50 *100

hab_SCG51 = read.csv("SCI_HabitatPatch_2013_SCG51.csv")
count_rock_SCG51 = length(hab_SCG51$Habitat[hab_SCG51$Habitat == "HARD" | hab_SCG51$Habitat == "MIXED"])
count_total_SCG51 = length(hab_SCG51$Habitat[hab_SCG51$Habitat == "HARD" | hab_SCG51$Habitat == "MIXED" | hab_SCG51$Habitat == "SOFT"]) 
percentrock_SCG51 = count_rock_SCG51 / count_total_SCG51 *100

hab_SCG52a = read.csv("SCI_HabitatPatch_2013_SCG52a.csv")
count_rock_SCG52a = length(hab_SCG52a$Habitat[hab_SCG52a$Habitat == "HARD" | hab_SCG52a$Habitat == "MIXED"])
count_total_SCG52a = length(hab_SCG52a$Habitat[hab_SCG52a$Habitat == "HARD" | hab_SCG52a$Habitat == "MIXED" | hab_SCG52a$Habitat == "SOFT"]) 
percentrock_SCG52a = count_rock_SCG52a / count_total_SCG52a *100

hab_SCG52b = read.csv("SCI_HabitatPatch_2013_SCG52b.csv")
count_rock_SCG52b = length(hab_SCG52b$Habitat[hab_SCG52b$Habitat == "HARD" | hab_SCG52b$Habitat == "MIXED"])
count_total_SCG52b = length(hab_SCG52b$Habitat[hab_SCG52b$Habitat == "HARD" | hab_SCG52b$Habitat == "MIXED" | hab_SCG52b$Habitat == "SOFT"]) 
percentrock_SCG52b = count_rock_SCG52b / count_total_SCG52b *100

hab_SCG53a = read.csv("SCI_HabitatPatch_2013_SCG53a.csv")
count_rock_SCG53a = length(hab_SCG53a$Habitat[hab_SCG53a$Habitat == "HARD" | hab_SCG53a$Habitat == "MIXED"])
count_total_SCG53a = length(hab_SCG53a$Habitat[hab_SCG53a$Habitat == "HARD" | hab_SCG53a$Habitat == "MIXED" | hab_SCG53a$Habitat == "SOFT"]) 
percentrock_SCG53a = count_rock_SCG53a / count_total_SCG53a *100

hab_SCG53b = read.csv("SCI_HabitatPatch_2013_SCG53b.csv")
count_rock_SCG53b = length(hab_SCG53b$Habitat[hab_SCG53b$Habitat == "HARD" | hab_SCG53b$Habitat == "MIXED"])
count_total_SCG53b = length(hab_SCG53b$Habitat[hab_SCG53b$Habitat == "HARD" | hab_SCG53b$Habitat == "MIXED" | hab_SCG53b$Habitat == "SOFT"]) 
percentrock_SCG53b = count_rock_SCG53b / count_total_SCG53b *100

hab_SCG55 = read.csv("SCI_HabitatPatch_2013_SCG55.csv")
count_rock_SCG55 = length(hab_SCG55$Habitat[hab_SCG55$Habitat == "HARD" | hab_SCG55$Habitat == "MIXED"])
count_total_SCG55 = length(hab_SCG55$Habitat[hab_SCG55$Habitat == "HARD" | hab_SCG55$Habitat == "MIXED" | hab_SCG55$Habitat == "SOFT"]) 
percentrock_SCG55 = count_rock_SCG55 / count_total_SCG55 *100

hab_SCG56 = read.csv("SCI_HabitatPatch_2013_SCG56.csv")
count_rock_SCG56 = length(hab_SCG56$Habitat[hab_SCG56$Habitat == "HARD" | hab_SCG56$Habitat == "MIXED"])
count_total_SCG56 = length(hab_SCG56$Habitat[hab_SCG56$Habitat == "HARD" | hab_SCG56$Habitat == "MIXED" | hab_SCG56$Habitat == "SOFT"]) 
percentrock_SCG56 = count_rock_SCG56 / count_total_SCG56 *100

hab_SCG57 = read.csv("SCI_HabitatPatch_2013_SCG57.csv")
count_rock_SCG57 = length(hab_SCG57$Habitat[hab_SCG57$Habitat == "HARD" | hab_SCG57$Habitat == "MIXED"])
count_total_SCG57 = length(hab_SCG57$Habitat[hab_SCG57$Habitat == "HARD" | hab_SCG57$Habitat == "MIXED" | hab_SCG57$Habitat == "SOFT"]) 
percentrock_SCG57 = count_rock_SCG57 / count_total_SCG57 *100

hab_SCG58 = read.csv("SCI_HabitatPatch_2013_SCG58.csv")
count_rock_SCG58 = length(hab_SCG58$Habitat[hab_SCG58$Habitat == "HARD" | hab_SCG58$Habitat == "MIXED"])
count_total_SCG58 = length(hab_SCG58$Habitat[hab_SCG58$Habitat == "HARD" | hab_SCG58$Habitat == "MIXED" | hab_SCG58$Habitat == "SOFT"]) 
percentrock_SCG58 = count_rock_SCG58 / count_total_SCG58 *100

hab_SCG60 = read.csv("SCI_HabitatPatch_2013_SCG60.csv")
count_rock_SCG60 = length(hab_SCG60$Habitat[hab_SCG60$Habitat == "HARD" | hab_SCG60$Habitat == "MIXED"])
count_total_SCG60 = length(hab_SCG60$Habitat[hab_SCG60$Habitat == "HARD" | hab_SCG60$Habitat == "MIXED" | hab_SCG60$Habitat == "SOFT"]) 
percentrock_SCG60 = count_rock_SCG60 / count_total_SCG60 *100

hab_SCG62 = read.csv("SCI_HabitatPatch_2013_SCG62.csv")
count_rock_SCG62 = length(hab_SCG62$Habitat[hab_SCG62$Habitat == "HARD" | hab_SCG62$Habitat == "MIXED"])
count_total_SCG62 = length(hab_SCG62$Habitat[hab_SCG62$Habitat == "HARD" | hab_SCG62$Habitat == "MIXED" | hab_SCG62$Habitat == "SOFT"]) 
percentrock_SCG62 = count_rock_SCG62 / count_total_SCG62 *100

percentrock_SCF01 
percentrock_SCF02 
percentrock_SCF03 
percentrock_SCF04 
percentrock_SCF05 
percentrock_SCF06 
percentrock_SCF50 
percentrock_SCF51 
percentrock_SCF52 
percentrock_SCF53 
percentrock_SCF54 
percentrock_SCF55 
percentrock_SCF60a 
percentrock_SCF60b
percentrock_SCF60c

percentrock_SCG01 
percentrock_SCG02 
percentrock_SCG03 
percentrock_SCG04 
percentrock_SCG05 
percentrock_SCG06 
percentrock_SCG07 
percentrock_SCG50 
percentrock_SCG51
percentrock_SCG52a 
percentrock_SCG52b 
percentrock_SCG53a 
percentrock_SCG53b 
percentrock_SCG55 
percentrock_SCG56 
percentrock_SCG57 
percentrock_SCG58 
percentrock_SCG60 
percentrock_SCG62 

#########################################################################################

#Determine predominant primary substrate on each transect based on animal observations

#rule: if >50% of observations were made over primary substrate reef (rock) or boulder 
#(lg rock), call transect "rock", if >50% of observations were made over sand, call #transect "sand"

subst_SCF01 = table(obs$Prim_sub[obs$tran_ID == 110]) #make a table of categories
subst_SCF01 #print that table
length(obs$Prim_sub[obs$tran_ID == 110]) / 2  #50% cutoff value

subst_SCF02 = table(obs$Prim_sub[obs$tran_ID == 111])
subst_SCF02
length(obs$Prim_sub[obs$tran_ID == 111]) / 2 

subst_SCF03 = table(obs$Prim_sub[obs$tran_ID == 119])
subst_SCF03
length(obs$Prim_sub[obs$tran_ID == 119]) / 2 

subst_SCF04 = table(obs$Prim_sub[obs$tran_ID == 120])
subst_SCF04
length(obs$Prim_sub[obs$tran_ID == 120]) / 2 

subst_SCF05 = table(obs$Prim_sub[obs$tran_ID == 140])
subst_SCF05
length(obs$Prim_sub[obs$tran_ID == 140]) / 2 

subst_SCF06 = table(obs$Prim_sub[obs$tran_ID == 121])
subst_SCF06
length(obs$Prim_sub[obs$tran_ID == 121]) / 2 

subst_SCF50 = table(obs$Prim_sub[obs$tran_ID == 149])
subst_SCF50
length(obs$Prim_sub[obs$tran_ID == 149]) / 2 

subst_SCF51 = table(obs$Prim_sub[obs$tran_ID == 150])
subst_SCF51
length(obs$Prim_sub[obs$tran_ID == 150]) / 2 

subst_SCF52 = table(obs$Prim_sub[obs$tran_ID == 151])
subst_SCF52
length(obs$Prim_sub[obs$tran_ID == 151]) / 2 

subst_SCF53 = table(obs$Prim_sub[obs$tran_ID == 156])
subst_SCF53
length(obs$Prim_sub[obs$tran_ID == 156]) / 2 

subst_SCF54 = table(obs$Prim_sub[obs$tran_ID == 157])
subst_SCF54
length(obs$Prim_sub[obs$tran_ID == 157]) / 2 

subst_SCF55 = table(obs$Prim_sub[obs$tran_ID == 152])
subst_SCF55
length(obs$Prim_sub[obs$tran_ID == 152]) / 2 

subst_SCF60a = table(obs$Prim_sub[obs$tran_ID == 153])
subst_SCF60a
length(obs$Prim_sub[obs$tran_ID == 153]) / 2 

subst_SCF60b = table(obs$Prim_sub[obs$tran_ID == 154])
subst_SCF60b
length(obs$Prim_sub[obs$tran_ID == 154]) / 2 

subst_SCF60c = table(obs$Prim_sub[obs$tran_ID == 155])
subst_SCF60c
length(obs$Prim_sub[obs$tran_ID == 155]) / 2 

subst_SCG01 = table(obs$Prim_sub[obs$tran_ID == 105])
subst_SCG01
length(obs$Prim_sub[obs$tran_ID == 105]) / 2 

subst_SCG02 = table(obs$Prim_sub[obs$tran_ID == 106])
subst_SCG02
length(obs$Prim_sub[obs$tran_ID == 106]) / 2 

subst_SCG03 = table(obs$Prim_sub[obs$tran_ID == 107])
subst_SCG03
length(obs$Prim_sub[obs$tran_ID == 107]) / 2 

subst_SCG04 = table(obs$Prim_sub[obs$tran_ID == 108])
subst_SCG04
length(obs$Prim_sub[obs$tran_ID == 108]) / 2 

subst_SCG05 = table(obs$Prim_sub[obs$tran_ID == 109])
subst_SCG05
length(obs$Prim_sub[obs$tran_ID == 109]) / 2 

subst_SCG06 = table(obs$Prim_sub[obs$tran_ID == 112])
subst_SCG06
length(obs$Prim_sub[obs$tran_ID == 112]) / 2 

subst_SCG07 = table(obs$Prim_sub[obs$tran_ID == 113])
subst_SCG07
length(obs$Prim_sub[obs$tran_ID == 113]) / 2 

subst_SCG50 = table(obs$Prim_sub[obs$tran_ID == 158])
subst_SCG50
length(obs$Prim_sub[obs$tran_ID == 158]) / 2 

subst_SCG51 = table(obs$Prim_sub[obs$tran_ID == 159])
subst_SCG51
length(obs$Prim_sub[obs$tran_ID == 159]) / 2 

subst_SCG52a = table(obs$Prim_sub[obs$tran_ID == 161])
subst_SCG52a
length(obs$Prim_sub[obs$tran_ID == 161]) / 2 

subst_SCG52b = table(obs$Prim_sub[obs$tran_ID == 162])
subst_SCG52b
length(obs$Prim_sub[obs$tran_ID == 162]) / 2 

subst_SCG53a = table(obs$Prim_sub[obs$tran_ID == 169])
subst_SCG53a
length(obs$Prim_sub[obs$tran_ID == 169]) / 2 

subst_SCG53b = table(obs$Prim_sub[obs$tran_ID == 170])
subst_SCG53b
length(obs$Prim_sub[obs$tran_ID == 170]) / 2 

subst_SCG55 = table(obs$Prim_sub[obs$tran_ID == 167])
subst_SCG55
length(obs$Prim_sub[obs$tran_ID == 167]) / 2 

subst_SCG56 = table(obs$Prim_sub[obs$tran_ID == 168])
subst_SCG56
length(obs$Prim_sub[obs$tran_ID == 168]) / 2 

subst_SCG57 = table(obs$Prim_sub[obs$tran_ID == 165])
subst_SCG57
length(obs$Prim_sub[obs$tran_ID == 165]) / 2 

subst_SCG58 = table(obs$Prim_sub[obs$tran_ID == 166])
subst_SCG58
length(obs$Prim_sub[obs$tran_ID == 166]) / 2 

subst_SCG60 = table(obs$Prim_sub[obs$tran_ID == 163])
subst_SCG60
length(obs$Prim_sub[obs$tran_ID == 166]) / 2 

subst_SCG62 = table(obs$Prim_sub[obs$tran_ID == 164])
subst_SCG62
length(obs$Prim_sub[obs$tran_ID == 166]) / 2 

#########################################################################################

#some summary/exploratory analysis

#subset obs so that we're only dealing with transects from control & impact sites
obs_sites = subset(obs, (obs$tran_ID == 110 |
obs$tran_ID == 111 |
obs$tran_ID == 119 |
obs$tran_ID == 120 |
obs$tran_ID == 140 |
obs$tran_ID == 121 |
obs$tran_ID == 149 |
obs$tran_ID == 150 |
obs$tran_ID == 151 |
obs$tran_ID == 156 |
obs$tran_ID == 157 |
obs$tran_ID == 152 |
obs$tran_ID == 153 |
obs$tran_ID == 154 |
obs$tran_ID == 155 |
obs$tran_ID == 105 |
obs$tran_ID == 106 |
obs$tran_ID == 107 |
obs$tran_ID == 108 |
obs$tran_ID == 109 |
obs$tran_ID == 112 |
obs$tran_ID == 113 |
obs$tran_ID == 158 |
obs$tran_ID == 159 |
obs$tran_ID == 161 |
obs$tran_ID == 162 |
obs$tran_ID == 169 |
obs$tran_ID == 170 |
obs$tran_ID == 167 |
obs$tran_ID == 168 |
obs$tran_ID == 165 |
obs$tran_ID == 166 |
obs$tran_ID == 163 |
obs$tran_ID == 164 ))

head(obs_sites) #check

#subset zone F obs
obs_F = subset(obs, (obs$tran_ID == 110 |
obs$tran_ID == 111 |
obs$tran_ID == 119 |
obs$tran_ID == 120 |
obs$tran_ID == 140 |
obs$tran_ID == 121 |
obs$tran_ID == 149 |
obs$tran_ID == 150 |
obs$tran_ID == 151 |
obs$tran_ID == 156 |
obs$tran_ID == 157 |
obs$tran_ID == 152 |
obs$tran_ID == 153 |
obs$tran_ID == 154 |
obs$tran_ID == 155 ))
head(obs_F) #check

#subset zone G obs
obs_G = subset(obs, (obs$tran_ID == 105 |
obs$tran_ID == 106 |
obs$tran_ID == 107 |
obs$tran_ID == 108 |
obs$tran_ID == 109 |
obs$tran_ID == 112 |
obs$tran_ID == 113 |
obs$tran_ID == 158 |
obs$tran_ID == 159 |
obs$tran_ID == 161 |
obs$tran_ID == 162 |
obs$tran_ID == 169 |
obs$tran_ID == 170 |
obs$tran_ID == 167 |
obs$tran_ID == 168 |
obs$tran_ID == 165 |
obs$tran_ID == 166 |
obs$tran_ID == 163 |
obs$tran_ID == 164 ))
head(obs_G) #check

#count all fish observed
total_fish = sum(obs_sites$Count[is.na(obs_sites$Fish) == FALSE], na.rm = TRUE)

#count unique fish species/species groups
fish_sp = length(unique(obs_sites$Fish))

#count all mob inverts observed
total_mob_inv = sum(obs_sites$Count[is.na(obs_sites$Mob_Inv) == FALSE], na.rm = TRUE)

#count unique mob invert species/species groups
mob_inv_sp = length(unique(obs_sites$Mob_Inv))

#explore relative fish abundances
table(obs_sites$Fish[obs_sites$ID_quality >= 3])
table(obs_F$Fish[obs_F$ID_quality >= 3])
table(obs_G$Fish[obs_G$ID_quality >= 3])

#explore relative mob invert abundances
table(obs_sites$Mob_Inv[obs_sites$ID_quality >= 3])
table(obs_F$Mob_Inv[obs_F$ID_quality >= 3])
table(obs_G$Mob_Inv[obs_G$ID_quality >= 3])

#########################################################################################

#extract abundance data (raw counts) from obs

#Lingcod abundance control site
count_lcod_SCF01 = sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF02 = sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF03 = sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF04 = sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF05 = sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF06 = sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF50 = sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF51 = sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF52 = sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF53 = sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF54 = sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF55 = sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF60a = sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF60b = sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCF60c = sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)

count_lcod_SCF01 
count_lcod_SCF02 
count_lcod_SCF03 
count_lcod_SCF04 
count_lcod_SCF05 
count_lcod_SCF06 
count_lcod_SCF50 
count_lcod_SCF51 
count_lcod_SCF52 
count_lcod_SCF53
count_lcod_SCF54 
count_lcod_SCF55 
count_lcod_SCF60a
count_lcod_SCF60b 
count_lcod_SCF60c

#Lingcod abundance DFMPA site
count_lcod_SCG01 = sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG02 = sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG03 = sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG04 = sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG05 = sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG06 = sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG07 = sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG50 = sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG51 = sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG52a = sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG52b = sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG53a = sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG53b = sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG55 = sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG56 = sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG57 = sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG58 = sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG60 = sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
count_lcod_SCG62 = sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)

count_lcod_SCG01
count_lcod_SCG02 
count_lcod_SCG03
count_lcod_SCG04 
count_lcod_SCG05 
count_lcod_SCG06
count_lcod_SCG07 
count_lcod_SCG50
count_lcod_SCG51 
count_lcod_SCG52a 
count_lcod_SCG52b 
count_lcod_SCG53a 
count_lcod_SCG53b 
count_lcod_SCG55 
count_lcod_SCG56 
count_lcod_SCG57 
count_lcod_SCG58 
count_lcod_SCG60 
count_lcod_SCG62

#########################################################################################

#California Sheephead abundance control site
count_cash_SCF01 = sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF02 = sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF03 = sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF04 = sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF05 = sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF06 = sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF50 = sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF51 = sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF52 = sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF53 = sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF54 = sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF55 = sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF60a = sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF60b = sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCF60c = sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)

count_cash_SCF01 
count_cash_SCF02 
count_cash_SCF03 
count_cash_SCF04 
count_cash_SCF05 
count_cash_SCF06 
count_cash_SCF50
count_cash_SCF51 
count_cash_SCF52 
count_cash_SCF53 
count_cash_SCF54 
count_cash_SCF55
count_cash_SCF60a 
count_cash_SCF60b 
count_cash_SCF60c 

#California Sheephead abundance DFMPA site
count_cash_SCG01 = sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG02 = sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG03 = sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG04 = sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG05 = sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG06 = sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG07 = sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG50 = sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG51 = sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG52a = sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG52b = sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG53a = sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG53b = sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG55 = sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG56 = sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG57 = sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG58 = sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG60 = sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
count_cash_SCG62 = sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)

count_cash_SCG01 
count_cash_SCG02 
count_cash_SCG03 
count_cash_SCG04 
count_cash_SCG05 
count_cash_SCG06 
count_cash_SCG07 
count_cash_SCG50 
count_cash_SCG51 
count_cash_SCG52a
count_cash_SCG52b 
count_cash_SCG53a 
count_cash_SCG53b 
count_cash_SCG55 
count_cash_SCG56 
count_cash_SCG57 
count_cash_SCG58 
count_cash_SCG60 
count_cash_SCG62

#########################################################################################

#California Scorpionfish abundance control site
count_casc_SCF01 = sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF02 = sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF03 = sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF04 = sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF05 = sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF06 = sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF50 = sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF51 = sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF52 = sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF53 = sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF54 = sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF55 = sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF60a = sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF60b = sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCF60c = sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)

count_casc_SCF01 
count_casc_SCF02 
count_casc_SCF03 
count_casc_SCF04 
count_casc_SCF05 
count_casc_SCF06 
count_casc_SCF50 
count_casc_SCF51 
count_casc_SCF52 
count_casc_SCF53 
count_casc_SCF54 
count_casc_SCF55 
count_casc_SCF60a 
count_casc_SCF60b 
count_casc_SCF60c

#California Scorpionfish abundance DFMPA site
count_casc_SCG01 = sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG02 = sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG03 = sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG04 = sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG05 = sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG06 = sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG07 = sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG50 = sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG51 = sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG52a = sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG52b = sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG53a = sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG53b = sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG55 = sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG56 = sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG57 = sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG58 = sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG60 = sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
count_casc_SCG62 = sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)

count_casc_SCG01 
count_casc_SCG02
count_casc_SCG03 
count_casc_SCG04
count_casc_SCG05 
count_casc_SCG06 
count_casc_SCG07 
count_casc_SCG50 
count_casc_SCG51 
count_casc_SCG52a 
count_casc_SCG52b 
count_casc_SCG53a 
count_casc_SCG53b 
count_casc_SCG55 
count_casc_SCG56 
count_casc_SCG57 
count_casc_SCG58 
count_casc_SCG60 
count_casc_SCG62

#########################################################################################

#Ocean Whitefish abundance control site
count_ocwf_SCF01 = sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF02 = sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF03 = sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF04 = sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF05 = sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF06 = sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF50 = sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF51 = sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF52 = sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF53 = sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF54 = sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF55 = sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF60a = sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF60b = sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCF60c = sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)

count_ocwf_SCF01 
count_ocwf_SCF02 
count_ocwf_SCF03 
count_ocwf_SCF04 
count_ocwf_SCF05 
count_ocwf_SCF06 
count_ocwf_SCF50 
count_ocwf_SCF51 
count_ocwf_SCF52 
count_ocwf_SCF53 
count_ocwf_SCF54 
count_ocwf_SCF55 
count_ocwf_SCF60a
count_ocwf_SCF60b 
count_ocwf_SCF60c

#Ocean Whitefish abundance DFMPA site
count_ocwf_SCG01 = sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG02 = sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG03 = sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG04 = sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG05 = sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG06 = sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG07 = sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG50 = sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG51 = sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG52a = sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG52b = sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG53a = sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG53b = sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG55 = sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG56 = sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG57 = sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG58 = sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG60 = sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
count_ocwf_SCG62 = sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)

count_ocwf_SCG01 
count_ocwf_SCG02 
count_ocwf_SCG03 
count_ocwf_SCG04 
count_ocwf_SCG05 
count_ocwf_SCG06 
count_ocwf_SCG07 
count_ocwf_SCG50 
count_ocwf_SCG51 
count_ocwf_SCG52a 
count_ocwf_SCG52b 
count_ocwf_SCG53a 
count_ocwf_SCG53b 
count_ocwf_SCG55 
count_ocwf_SCG56
count_ocwf_SCG57 
count_ocwf_SCG58 
count_ocwf_SCG60 
count_ocwf_SCG62 

#########################################################################################

#Bocaccio abundance control site
count_bcac_SCF01 = sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF02 = sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF03 = sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF04 = sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF05 = sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF06 = sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF50 = sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF51 = sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF52 = sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF53 = sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF54 = sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF55 = sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF60a = sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF60b = sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCF60c = sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)

count_bcac_SCF01 
count_bcac_SCF02 
count_bcac_SCF03 
count_bcac_SCF04 
count_bcac_SCF05 
count_bcac_SCF06 
count_bcac_SCF50 
count_bcac_SCF51 
count_bcac_SCF52 
count_bcac_SCF53 
count_bcac_SCF54 
count_bcac_SCF55 
count_bcac_SCF60a
count_bcac_SCF60b 
count_bcac_SCF60c

#Bocaccio abundance DFMPA site
count_bcac_SCG01 = sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG02 = sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG03 = sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG04 = sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG05 = sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG06 = sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG07 = sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG50 = sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG51 = sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG52a = sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG52b = sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG53a = sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG53b = sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG55 = sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG56 = sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG57 = sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG58 = sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG60 = sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_bcac_SCG62 = sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)

count_bcac_SCG01 
count_bcac_SCG02 
count_bcac_SCG03 
count_bcac_SCG04 
count_bcac_SCG05 
count_bcac_SCG06 
count_bcac_SCG07 
count_bcac_SCG50 
count_bcac_SCG51 
count_bcac_SCG52a 
count_bcac_SCG52b 
count_bcac_SCG53a 
count_bcac_SCG53b 
count_bcac_SCG55 
count_bcac_SCG56
count_bcac_SCG57 
count_bcac_SCG58 
count_bcac_SCG60 
count_bcac_SCG62 

#########################################################################################

#Copper abundance control site
count_copp_SCF01 = sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF02 = sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF03 = sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF04 = sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF05 = sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF06 = sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF50 = sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF51 = sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF52 = sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF53 = sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF54 = sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF55 = sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF60a = sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF60b = sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCF60c = sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)

count_copp_SCF01 
count_copp_SCF02 
count_copp_SCF03 
count_copp_SCF04 
count_copp_SCF05 
count_copp_SCF06 
count_copp_SCF50 
count_copp_SCF51 
count_copp_SCF52 
count_copp_SCF53 
count_copp_SCF54 
count_copp_SCF55 
count_copp_SCF60a
count_copp_SCF60b 
count_copp_SCF60c

#Copper abundance DFMPA site
count_copp_SCG01 = sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG02 = sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG03 = sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG04 = sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG05 = sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG06 = sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG07 = sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG50 = sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG51 = sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG52a = sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG52b = sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG53a = sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG53b = sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG55 = sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG56 = sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG57 = sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG58 = sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG60 = sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
count_copp_SCG62 = sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)

count_copp_SCG01 
count_copp_SCG02 
count_copp_SCG03 
count_copp_SCG04 
count_copp_SCG05 
count_copp_SCG06 
count_copp_SCG07 
count_copp_SCG50 
count_copp_SCG51 
count_copp_SCG52a 
count_copp_SCG52b 
count_copp_SCG53a 
count_copp_SCG53b 
count_copp_SCG55 
count_copp_SCG56
count_copp_SCG57 
count_copp_SCG58 
count_copp_SCG60 
count_copp_SCG62 

#########################################################################################

#Olive/Yellowtail abundance control site
count_olyt_SCF01 = sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF02 = sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF03 = sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF04 = sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF05 = sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF06 = sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF50 = sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF51 = sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF52 = sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF53 = sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF54 = sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF55 = sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF60a = sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF60b = sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCF60c = sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)

count_olyt_SCF01 
count_olyt_SCF02 
count_olyt_SCF03 
count_olyt_SCF04 
count_olyt_SCF05 
count_olyt_SCF06 
count_olyt_SCF50 
count_olyt_SCF51 
count_olyt_SCF52 
count_olyt_SCF53 
count_olyt_SCF54 
count_olyt_SCF55 
count_olyt_SCF60a
count_olyt_SCF60b 
count_olyt_SCF60c

#Olive/Yellowtail abundance DFMPA site
count_olyt_SCG01 = sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG02 = sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG03 = sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG04 = sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG05 = sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG06 = sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG07 = sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG50 = sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG51 = sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG52a = sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG52b = sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG53a = sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG53b = sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG55 = sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG56 = sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG57 = sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG58 = sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG60 = sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
count_olyt_SCG62 = sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)

count_olyt_SCG01 
count_olyt_SCG02 
count_olyt_SCG03 
count_olyt_SCG04 
count_olyt_SCG05 
count_olyt_SCG06 
count_olyt_SCG07 
count_olyt_SCG50 
count_olyt_SCG51 
count_olyt_SCG52a 
count_olyt_SCG52b 
count_olyt_SCG53a 
count_olyt_SCG53b 
count_olyt_SCG55 
count_olyt_SCG56
count_olyt_SCG57 
count_olyt_SCG58 
count_olyt_SCG60 
count_olyt_SCG62 

#########################################################################################

#Vermilion/Canary abundance control site
count_vrmlcnry_SCF01 = sum(obs$Count[obs$tran_ID == 110 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF02 = sum(obs$Count[obs$tran_ID == 111 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF03 = sum(obs$Count[obs$tran_ID == 119 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF04 = sum(obs$Count[obs$tran_ID == 120 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF05 = sum(obs$Count[obs$tran_ID == 140 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF06 = sum(obs$Count[obs$tran_ID == 121 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF50 = sum(obs$Count[obs$tran_ID == 149 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF51 = sum(obs$Count[obs$tran_ID == 150 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF52 = sum(obs$Count[obs$tran_ID == 151 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF53 = sum(obs$Count[obs$tran_ID == 156 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF54 = sum(obs$Count[obs$tran_ID == 157 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF55 = sum(obs$Count[obs$tran_ID == 152 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF60a = sum(obs$Count[obs$tran_ID == 153 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF60b = sum(obs$Count[obs$tran_ID == 154 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCF60c = sum(obs$Count[obs$tran_ID == 155 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)

count_vrmlcnry_SCF01 
count_vrmlcnry_SCF02 
count_vrmlcnry_SCF03 
count_vrmlcnry_SCF04 
count_vrmlcnry_SCF05 
count_vrmlcnry_SCF06 
count_vrmlcnry_SCF50 
count_vrmlcnry_SCF51 
count_vrmlcnry_SCF52 
count_vrmlcnry_SCF53 
count_vrmlcnry_SCF54 
count_vrmlcnry_SCF55 
count_vrmlcnry_SCF60a
count_vrmlcnry_SCF60b 
count_vrmlcnry_SCF60c

#Vermilion/Canary abundance DFMPA site
count_vrmlcnry_SCG01 = sum(obs$Count[obs$tran_ID == 105 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG02 = sum(obs$Count[obs$tran_ID == 106 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG03 = sum(obs$Count[obs$tran_ID == 107 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG04 = sum(obs$Count[obs$tran_ID == 108 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG05 = sum(obs$Count[obs$tran_ID == 109 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG06 = sum(obs$Count[obs$tran_ID == 112 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG07 = sum(obs$Count[obs$tran_ID == 113 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG50 = sum(obs$Count[obs$tran_ID == 158 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG51 = sum(obs$Count[obs$tran_ID == 159 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG52a = sum(obs$Count[obs$tran_ID == 161 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG52b = sum(obs$Count[obs$tran_ID == 162 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG53a = sum(obs$Count[obs$tran_ID == 169 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG53b = sum(obs$Count[obs$tran_ID == 170 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG55 = sum(obs$Count[obs$tran_ID == 167 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG56 = sum(obs$Count[obs$tran_ID == 168 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG57 = sum(obs$Count[obs$tran_ID == 165 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG58 = sum(obs$Count[obs$tran_ID == 166 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG60 = sum(obs$Count[obs$tran_ID == 163 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
count_vrmlcnry_SCG62 = sum(obs$Count[obs$tran_ID == 164 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)

count_vrmlcnry_SCG01 
count_vrmlcnry_SCG02 
count_vrmlcnry_SCG03 
count_vrmlcnry_SCG04 
count_vrmlcnry_SCG05 
count_vrmlcnry_SCG06 
count_vrmlcnry_SCG07 
count_vrmlcnry_SCG50 
count_vrmlcnry_SCG51 
count_vrmlcnry_SCG52a 
count_vrmlcnry_SCG52b 
count_vrmlcnry_SCG53a 
count_vrmlcnry_SCG53b 
count_vrmlcnry_SCG55 
count_vrmlcnry_SCG56
count_vrmlcnry_SCG57 
count_vrmlcnry_SCG58 
count_vrmlcnry_SCG60 
count_vrmlcnry_SCG62

#########################################################################################

#Dwarf-red abundance control site
count_drrf_SCF01 = sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF02 = sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF03 = sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF04 = sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF05 = sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF06 = sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF50 = sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF51 = sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF52 = sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF53 = sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF54 = sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF55 = sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF60a = sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF60b = sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCF60c = sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)

count_drrf_SCF01 
count_drrf_SCF02 
count_drrf_SCF03 
count_drrf_SCF04 
count_drrf_SCF05 
count_drrf_SCF06 
count_drrf_SCF50 
count_drrf_SCF51 
count_drrf_SCF52 
count_drrf_SCF53 
count_drrf_SCF54 
count_drrf_SCF55 
count_drrf_SCF60a
count_drrf_SCF60b 
count_drrf_SCF60c

#Dwarf-red abundance DFMPA site
count_drrf_SCG01 = sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG02 = sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG03 = sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG04 = sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG05 = sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG06 = sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG07 = sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG50 = sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG51 = sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG52a = sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG52b = sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG53a = sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG53b = sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG55 = sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG56 = sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG57 = sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG58 = sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG60 = sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
count_drrf_SCG62 = sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)

count_drrf_SCG01 
count_drrf_SCG02 
count_drrf_SCG03 
count_drrf_SCG04 
count_drrf_SCG05 
count_drrf_SCG06 
count_drrf_SCG07 
count_drrf_SCG50 
count_drrf_SCG51 
count_drrf_SCG52a 
count_drrf_SCG52b 
count_drrf_SCG53a 
count_drrf_SCG53b 
count_drrf_SCG55 
count_drrf_SCG56
count_drrf_SCG57 
count_drrf_SCG58 
count_drrf_SCG60 
count_drrf_SCG62 

#########################################################################################

#Halfbanded abundance control site
count_hfbd_SCF01 = sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF02 = sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF03 = sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF04 = sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF05 = sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF06 = sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF50 = sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF51 = sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF52 = sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF53 = sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF54 = sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF55 = sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF60a = sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF60b = sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCF60c = sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)

count_hfbd_SCF01 
count_hfbd_SCF02 
count_hfbd_SCF03 
count_hfbd_SCF04 
count_hfbd_SCF05 
count_hfbd_SCF06 
count_hfbd_SCF50 
count_hfbd_SCF51 
count_hfbd_SCF52 
count_hfbd_SCF53 
count_hfbd_SCF54 
count_hfbd_SCF55 
count_hfbd_SCF60a
count_hfbd_SCF60b 
count_hfbd_SCF60c

#Halfbanded abundance DFMPA site
count_hfbd_SCG01 = sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG02 = sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG03 = sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG04 = sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG05 = sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG06 = sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG07 = sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG50 = sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG51 = sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG52a = sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG52b = sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG53a = sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG53b = sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG55 = sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG56 = sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG57 = sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG58 = sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG60 = sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
count_hfbd_SCG62 = sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)

count_hfbd_SCG01 
count_hfbd_SCG02 
count_hfbd_SCG03 
count_hfbd_SCG04 
count_hfbd_SCG05 
count_hfbd_SCG06 
count_hfbd_SCG07 
count_hfbd_SCG50 
count_hfbd_SCG51 
count_hfbd_SCG52a 
count_hfbd_SCG52b 
count_hfbd_SCG53a 
count_hfbd_SCG53b 
count_hfbd_SCG55 
count_hfbd_SCG56
count_hfbd_SCG57 
count_hfbd_SCG58 
count_hfbd_SCG60 
count_hfbd_SCG62 

#########################################################################################

#Squarespot abundance control site
count_sqsp_SCF01 = sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF02 = sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF03 = sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF04 = sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF05 = sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF06 = sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF50 = sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF51 = sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF52 = sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF53 = sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF54 = sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF55 = sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF60a = sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF60b = sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCF60c = sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)

count_sqsp_SCF01 
count_sqsp_SCF02 
count_sqsp_SCF03 
count_sqsp_SCF04 
count_sqsp_SCF05 
count_sqsp_SCF06 
count_sqsp_SCF50 
count_sqsp_SCF51 
count_sqsp_SCF52 
count_sqsp_SCF53 
count_sqsp_SCF54 
count_sqsp_SCF55 
count_sqsp_SCF60a
count_sqsp_SCF60b 
count_sqsp_SCF60c

#Squarespot abundance DFMPA site
count_sqsp_SCG01 = sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 57 & obs$ID_quality >= 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG02 = sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG03 = sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG04 = sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG05 = sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG06 = sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG07 = sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG50 = sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG51 = sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG52a = sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG52b = sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG53a = sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG53b = sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG55 = sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG56 = sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG57 = sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG58 = sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG60 = sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
count_sqsp_SCG62 = sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)

count_sqsp_SCG01 
count_sqsp_SCG02 
count_sqsp_SCG03 
count_sqsp_SCG04 
count_sqsp_SCG05 
count_sqsp_SCG06 
count_sqsp_SCG07 
count_sqsp_SCG50 
count_sqsp_SCG51 
count_sqsp_SCG52a 
count_sqsp_SCG52b 
count_sqsp_SCG53a 
count_sqsp_SCG53b 
count_sqsp_SCG55 
count_sqsp_SCG56
count_sqsp_SCG57 
count_sqsp_SCG58 
count_sqsp_SCG60 
count_sqsp_SCG62

#########################################################################################

#Sanddab abundance control site
count_sdb_SCF01 = sum(obs$Count[obs$tran_ID == 110 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF02 = sum(obs$Count[obs$tran_ID == 111 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF03 = sum(obs$Count[obs$tran_ID == 119 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF04 = sum(obs$Count[obs$tran_ID == 120 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF05 = sum(obs$Count[obs$tran_ID == 140 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF06 = sum(obs$Count[obs$tran_ID == 121 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF50 = sum(obs$Count[obs$tran_ID == 149 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF51 = sum(obs$Count[obs$tran_ID == 150 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF52 = sum(obs$Count[obs$tran_ID == 151 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF53 = sum(obs$Count[obs$tran_ID == 156 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF54 = sum(obs$Count[obs$tran_ID == 157 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF55 = sum(obs$Count[obs$tran_ID == 152 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF60a = sum(obs$Count[obs$tran_ID == 153 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF60b = sum(obs$Count[obs$tran_ID == 154 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCF60c = sum(obs$Count[obs$tran_ID == 155 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)

count_sdb_SCF01 
count_sdb_SCF02 
count_sdb_SCF03 
count_sdb_SCF04 
count_sdb_SCF05 
count_sdb_SCF06 
count_sdb_SCF50 
count_sdb_SCF51 
count_sdb_SCF52 
count_sdb_SCF53 
count_sdb_SCF54 
count_sdb_SCF55 
count_sdb_SCF60a
count_sdb_SCF60b 
count_sdb_SCF60c

#Sanddab abundance DFMPA site
count_sdb_SCG01 = sum(obs$Count[obs$tran_ID == 105 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG02 = sum(obs$Count[obs$tran_ID == 106 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG03 = sum(obs$Count[obs$tran_ID == 107 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG04 = sum(obs$Count[obs$tran_ID == 108 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG05 = sum(obs$Count[obs$tran_ID == 109 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG06 = sum(obs$Count[obs$tran_ID == 112 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG07 = sum(obs$Count[obs$tran_ID == 113 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG50 = sum(obs$Count[obs$tran_ID == 158 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG51 = sum(obs$Count[obs$tran_ID == 159 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG52a = sum(obs$Count[obs$tran_ID == 161 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG52b = sum(obs$Count[obs$tran_ID == 162 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG53a = sum(obs$Count[obs$tran_ID == 169 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG53b = sum(obs$Count[obs$tran_ID == 170 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG55 = sum(obs$Count[obs$tran_ID == 167 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG56 = sum(obs$Count[obs$tran_ID == 168 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG57 = sum(obs$Count[obs$tran_ID == 165 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG58 = sum(obs$Count[obs$tran_ID == 166 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG60 = sum(obs$Count[obs$tran_ID == 163 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
count_sdb_SCG62 = sum(obs$Count[obs$tran_ID == 164 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)

count_sdb_SCG01 
count_sdb_SCG02 
count_sdb_SCG03 
count_sdb_SCG04 
count_sdb_SCG05 
count_sdb_SCG06 
count_sdb_SCG07 
count_sdb_SCG50 
count_sdb_SCG51 
count_sdb_SCG52a 
count_sdb_SCG52b 
count_sdb_SCG53a 
count_sdb_SCG53b 
count_sdb_SCG55 
count_sdb_SCG56
count_sdb_SCG57 
count_sdb_SCG58 
count_sdb_SCG60 
count_sdb_SCG62

#########################################################################################

#Perch abundance control site
count_prch_SCF01 = sum(obs$Count[obs$tran_ID == 110 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF02 = sum(obs$Count[obs$tran_ID == 111 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF03 = sum(obs$Count[obs$tran_ID == 119 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF04 = sum(obs$Count[obs$tran_ID == 120 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF05 = sum(obs$Count[obs$tran_ID == 140 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF06 = sum(obs$Count[obs$tran_ID == 121 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF50 = sum(obs$Count[obs$tran_ID == 149 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF51 = sum(obs$Count[obs$tran_ID == 150 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF52 = sum(obs$Count[obs$tran_ID == 151 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF53 = sum(obs$Count[obs$tran_ID == 156 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF54 = sum(obs$Count[obs$tran_ID == 157 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF55 = sum(obs$Count[obs$tran_ID == 152 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF60a = sum(obs$Count[obs$tran_ID == 153 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF60b = sum(obs$Count[obs$tran_ID == 154 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCF60c = sum(obs$Count[obs$tran_ID == 155 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)

count_prch_SCF01 
count_prch_SCF02 
count_prch_SCF03 
count_prch_SCF04 
count_prch_SCF05 
count_prch_SCF06 
count_prch_SCF50 
count_prch_SCF51 
count_prch_SCF52 
count_prch_SCF53 
count_prch_SCF54 
count_prch_SCF55 
count_prch_SCF60a
count_prch_SCF60b 
count_prch_SCF60c

#Perch abundance DFMPA site
count_prch_SCG01 = sum(obs$Count[obs$tran_ID == 105 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG02 = sum(obs$Count[obs$tran_ID == 106 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG03 = sum(obs$Count[obs$tran_ID == 107 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG04 = sum(obs$Count[obs$tran_ID == 108 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG05 = sum(obs$Count[obs$tran_ID == 109 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG06 = sum(obs$Count[obs$tran_ID == 112 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG07 = sum(obs$Count[obs$tran_ID == 113 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG50 = sum(obs$Count[obs$tran_ID == 158 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG51 = sum(obs$Count[obs$tran_ID == 159 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG52a = sum(obs$Count[obs$tran_ID == 161 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG52b = sum(obs$Count[obs$tran_ID == 162 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG53a = sum(obs$Count[obs$tran_ID == 169 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG53b = sum(obs$Count[obs$tran_ID == 170 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG55 = sum(obs$Count[obs$tran_ID == 167 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG56 = sum(obs$Count[obs$tran_ID == 168 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG57 = sum(obs$Count[obs$tran_ID == 165 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG58 = sum(obs$Count[obs$tran_ID == 166 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG60 = sum(obs$Count[obs$tran_ID == 163 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
count_prch_SCG62 = sum(obs$Count[obs$tran_ID == 164 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)

count_prch_SCG01 
count_prch_SCG02 
count_prch_SCG03 
count_prch_SCG04 
count_prch_SCG05 
count_prch_SCG06 
count_prch_SCG07 
count_prch_SCG50 
count_prch_SCG51 
count_prch_SCG52a 
count_prch_SCG52b 
count_prch_SCG53a 
count_prch_SCG53b 
count_prch_SCG55 
count_prch_SCG56
count_prch_SCG57 
count_prch_SCG58 
count_prch_SCG60 
count_prch_SCG62

#########################################################################################

#CA Spiny Lobster abundance control site
count_lbstr_SCF01 = sum(obs$Count[obs$tran_ID == 110 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF02 = sum(obs$Count[obs$tran_ID == 111 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF03 = sum(obs$Count[obs$tran_ID == 119 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF04 = sum(obs$Count[obs$tran_ID == 120 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF05 = sum(obs$Count[obs$tran_ID == 140 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF06 = sum(obs$Count[obs$tran_ID == 121 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF50 = sum(obs$Count[obs$tran_ID == 149 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF51 = sum(obs$Count[obs$tran_ID == 150 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF52 = sum(obs$Count[obs$tran_ID == 151 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF53 = sum(obs$Count[obs$tran_ID == 156 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF54 = sum(obs$Count[obs$tran_ID == 157 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF55 = sum(obs$Count[obs$tran_ID == 152 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF60a = sum(obs$Count[obs$tran_ID == 153 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF60b = sum(obs$Count[obs$tran_ID == 154 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCF60c = sum(obs$Count[obs$tran_ID == 155 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)

count_lbstr_SCF01 
count_lbstr_SCF02 
count_lbstr_SCF03 
count_lbstr_SCF04 
count_lbstr_SCF05 
count_lbstr_SCF06 
count_lbstr_SCF50 
count_lbstr_SCF51 
count_lbstr_SCF52 
count_lbstr_SCF53 
count_lbstr_SCF54 
count_lbstr_SCF55 
count_lbstr_SCF60a
count_lbstr_SCF60b 
count_lbstr_SCF60c

#CA Spiny Lobster abundance DFMPA site
count_lbstr_SCG01 = sum(obs$Count[obs$tran_ID == 105 & obs$Mob_Inv == 46 & obs$ID_quality >= 3 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG02 = sum(obs$Count[obs$tran_ID == 106 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG03 = sum(obs$Count[obs$tran_ID == 107 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG04 = sum(obs$Count[obs$tran_ID == 108 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG05 = sum(obs$Count[obs$tran_ID == 109 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG06 = sum(obs$Count[obs$tran_ID == 112 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG07 = sum(obs$Count[obs$tran_ID == 113 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG50 = sum(obs$Count[obs$tran_ID == 158 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG51 = sum(obs$Count[obs$tran_ID == 159 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG52a = sum(obs$Count[obs$tran_ID == 161 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG52b = sum(obs$Count[obs$tran_ID == 162 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG53a = sum(obs$Count[obs$tran_ID == 169 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG53b = sum(obs$Count[obs$tran_ID == 170 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG55 = sum(obs$Count[obs$tran_ID == 167 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG56 = sum(obs$Count[obs$tran_ID == 168 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG57 = sum(obs$Count[obs$tran_ID == 165 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG58 = sum(obs$Count[obs$tran_ID == 166 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG60 = sum(obs$Count[obs$tran_ID == 163 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)
count_lbstr_SCG62 = sum(obs$Count[obs$tran_ID == 164 & obs$Mob_Inv == 46 & obs$ID_quality >= 3], na.rm = TRUE)

count_lbstr_SCG01 
count_lbstr_SCG02 
count_lbstr_SCG03 
count_lbstr_SCG04 
count_lbstr_SCG05 
count_lbstr_SCG06 
count_lbstr_SCG07 
count_lbstr_SCG50 
count_lbstr_SCG51 
count_lbstr_SCG52a 
count_lbstr_SCG52b 
count_lbstr_SCG53a 
count_lbstr_SCG53b 
count_lbstr_SCG55 
count_lbstr_SCG56
count_lbstr_SCG57 
count_lbstr_SCG58 
count_lbstr_SCG60 
count_lbstr_SCG62

#########################################################################################

#Sea cucumber (Warty, CA, or other) abundance control site
count_cuke_SCF01 = sum(obs$Count[obs$tran_ID == 110 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF02 = sum(obs$Count[obs$tran_ID == 111 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF03 = sum(obs$Count[obs$tran_ID == 119 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF04 = sum(obs$Count[obs$tran_ID == 120 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF05 = sum(obs$Count[obs$tran_ID == 140 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF06 = sum(obs$Count[obs$tran_ID == 121 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF50 = sum(obs$Count[obs$tran_ID == 149 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF51 = sum(obs$Count[obs$tran_ID == 150 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF52 = sum(obs$Count[obs$tran_ID == 151 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF53 = sum(obs$Count[obs$tran_ID == 156 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF54 = sum(obs$Count[obs$tran_ID == 157 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF55 = sum(obs$Count[obs$tran_ID == 152 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF60a = sum(obs$Count[obs$tran_ID == 153 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF60b = sum(obs$Count[obs$tran_ID == 154 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCF60c = sum(obs$Count[obs$tran_ID == 155 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)

count_cuke_SCF01 
count_cuke_SCF02 
count_cuke_SCF03 
count_cuke_SCF04 
count_cuke_SCF05 
count_cuke_SCF06 
count_cuke_SCF50 
count_cuke_SCF51 
count_cuke_SCF52 
count_cuke_SCF53 
count_cuke_SCF54 
count_cuke_SCF55 
count_cuke_SCF60a
count_cuke_SCF60b 
count_cuke_SCF60c

#Sea cucumber (Warty, CA, or other) abundance DFMPA site
count_cuke_SCG01 = sum(obs$Count[obs$tran_ID == 105 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) & obs$ID_quality >= 3 ], na.rm = TRUE)
count_cuke_SCG02 = sum(obs$Count[obs$tran_ID == 106 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG03 = sum(obs$Count[obs$tran_ID == 107 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG04 = sum(obs$Count[obs$tran_ID == 108 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG05 = sum(obs$Count[obs$tran_ID == 109 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG06 = sum(obs$Count[obs$tran_ID == 112 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG07 = sum(obs$Count[obs$tran_ID == 113 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG50 = sum(obs$Count[obs$tran_ID == 158 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG51 = sum(obs$Count[obs$tran_ID == 159 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG52a = sum(obs$Count[obs$tran_ID == 161 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG52b = sum(obs$Count[obs$tran_ID == 162 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG53a = sum(obs$Count[obs$tran_ID == 169 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG53b = sum(obs$Count[obs$tran_ID == 170 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG55 = sum(obs$Count[obs$tran_ID == 167 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG56 = sum(obs$Count[obs$tran_ID == 168 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG57 = sum(obs$Count[obs$tran_ID == 165 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG58 = sum(obs$Count[obs$tran_ID == 166 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG60 = sum(obs$Count[obs$tran_ID == 163 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)
count_cuke_SCG62 = sum(obs$Count[obs$tran_ID == 164 & (obs$Mob_Inv == 2 | obs$Mob_Inv == 47 | obs$Mob_Inv ==48) ], na.rm = TRUE)

count_cuke_SCG01 
count_cuke_SCG02 
count_cuke_SCG03 
count_cuke_SCG04 
count_cuke_SCG05 
count_cuke_SCG06 
count_cuke_SCG07 
count_cuke_SCG50 
count_cuke_SCG51 
count_cuke_SCG52a 
count_cuke_SCG52b 
count_cuke_SCG53a 
count_cuke_SCG53b 
count_cuke_SCG55 
count_cuke_SCG56
count_cuke_SCG57 
count_cuke_SCG58 
count_cuke_SCG60 
count_cuke_SCG62

#########################################################################################

#########################################################################################

#Size distributions by species and transect for biomass calculations

#calculate length (in cm) of each fish (midpoints of size classes)
obs$mdpt_size_class = NA
obs$mdpt_size_class = 
ifelse(obs$Size_class == "<5", 2.5,
ifelse(obs$Size_class == "5-10", 7.5,
ifelse(obs$Size_class == "10-15", 12.5,
ifelse(obs$Size_class == "15-20", 17.5,
ifelse(obs$Size_class == "20-25", 22.5,
ifelse(obs$Size_class == "25-30", 27.5, 
ifelse(obs$Size_class == "30-35", 32.5,
ifelse(obs$Size_class == "35-40", 37.5, 
ifelse(obs$Size_class == "40-45", 42.5, 
ifelse(obs$Size_class == "45-50", 47.5, 
ifelse(obs$Size_class == "50+", 50, NA)))))))))))

#calculate weight (in grams) based on length using fish LWRs

obs$weight = NA
obs$weight = 
ifelse(obs$Fish == 15, 0.01330*obs$mdpt_size_class^3,
ifelse(obs$Fish == 65, 0.02890*obs$mdpt_size_class^3,
ifelse(obs$Fish == 64, 0.03300*obs$mdpt_size_class^2.996,
ifelse(obs$Fish == 88, 0.02390*obs$mdpt_size_class^3,
ifelse(obs$Fish == 3, 0.01321*obs$mdpt_size_class^3,
ifelse(obs$Fish == 8, 0.01746*obs$mdpt_size_class^3,
ifelse(obs$Fish == 19, 0.01080*obs$mdpt_size_class^2.968,
ifelse(obs$Fish == 34 | obs$Fish == 44, 0.03270*obs$mdpt_size_class^3,
ifelse(obs$Fish == 86, 0.014640*obs$mdpt_size_class^2.984000,
ifelse(obs$Fish == 39, 0.01900*obs$mdpt_size_class^2.810, 
ifelse(obs$Fish == 57, 0.014640*obs$mdpt_size_class^2.984000,
ifelse(obs$Fish == 22 | obs$Fish == 30, 0.00776*obs$mdpt_size_class^3.0757,
ifelse(obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123, 0.0616*obs$mdpt_size_class^2.864, NA)))))))))))))

#calculate total biomass, weight*count
obs$biomass_g = NA
obs$biomass_g = obs$weight * obs$Count #biomass in grams
obs$biomass = NA
obs$biomass = obs$biomass_g / 1000 #biomass in kilograms

#########################################################################################

#extract biomass data from obs

#Lingcod biomass control site
biomass_lcod_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_lcod_SCF01 
biomass_lcod_SCF02 
biomass_lcod_SCF03 
biomass_lcod_SCF04 
biomass_lcod_SCF05 
biomass_lcod_SCF06 
biomass_lcod_SCF50 
biomass_lcod_SCF51 
biomass_lcod_SCF52 
biomass_lcod_SCF53
biomass_lcod_SCF54 
biomass_lcod_SCF55 
biomass_lcod_SCF60a
biomass_lcod_SCF60b 
biomass_lcod_SCF60c

#Lingcod biomass DFMPA site
biomass_lcod_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_lcod_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_lcod_SCG01
biomass_lcod_SCG02 
biomass_lcod_SCG03
biomass_lcod_SCG04 
biomass_lcod_SCG05 
biomass_lcod_SCG06
biomass_lcod_SCG07 
biomass_lcod_SCG50
biomass_lcod_SCG51 
biomass_lcod_SCG52a 
biomass_lcod_SCG52b 
biomass_lcod_SCG53a 
biomass_lcod_SCG53b 
biomass_lcod_SCG55 
biomass_lcod_SCG56 
biomass_lcod_SCG57 
biomass_lcod_SCG58 
biomass_lcod_SCG60 
biomass_lcod_SCG62

#########################################################################################

#California Sheephead biomass control site
biomass_cash_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_cash_SCF01 
biomass_cash_SCF02 
biomass_cash_SCF03 
biomass_cash_SCF04 
biomass_cash_SCF05 
biomass_cash_SCF06 
biomass_cash_SCF50
biomass_cash_SCF51 
biomass_cash_SCF52 
biomass_cash_SCF53 
biomass_cash_SCF54 
biomass_cash_SCF55
biomass_cash_SCF60a 
biomass_cash_SCF60b 
biomass_cash_SCF60c 

#California Sheephead biomass DFMPA site
biomass_cash_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_cash_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_cash_SCG01 
biomass_cash_SCG02 
biomass_cash_SCG03 
biomass_cash_SCG04 
biomass_cash_SCG05 
biomass_cash_SCG06 
biomass_cash_SCG07 
biomass_cash_SCG50 
biomass_cash_SCG51 
biomass_cash_SCG52a
biomass_cash_SCG52b 
biomass_cash_SCG53a 
biomass_cash_SCG53b 
biomass_cash_SCG55 
biomass_cash_SCG56 
biomass_cash_SCG57 
biomass_cash_SCG58 
biomass_cash_SCG60 
biomass_cash_SCG62

#########################################################################################

#California Scorpionfish biomass control site
biomass_casc_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_casc_SCF01 
biomass_casc_SCF02 
biomass_casc_SCF03 
biomass_casc_SCF04 
biomass_casc_SCF05 
biomass_casc_SCF06 
biomass_casc_SCF50 
biomass_casc_SCF51 
biomass_casc_SCF52 
biomass_casc_SCF53 
biomass_casc_SCF54 
biomass_casc_SCF55 
biomass_casc_SCF60a 
biomass_casc_SCF60b 
biomass_casc_SCF60c

#California Scorpionfish biomass DFMPA site
biomass_casc_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_casc_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_casc_SCG01 
biomass_casc_SCG02
biomass_casc_SCG03 
biomass_casc_SCG04
biomass_casc_SCG05 
biomass_casc_SCG06 
biomass_casc_SCG07 
biomass_casc_SCG50 
biomass_casc_SCG51 
biomass_casc_SCG52a 
biomass_casc_SCG52b 
biomass_casc_SCG53a 
biomass_casc_SCG53b 
biomass_casc_SCG55 
biomass_casc_SCG56 
biomass_casc_SCG57 
biomass_casc_SCG58 
biomass_casc_SCG60 
biomass_casc_SCG62

#########################################################################################

#Ocean Whitefish biomass control site
biomass_ocwf_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_ocwf_SCF01 
biomass_ocwf_SCF02 
biomass_ocwf_SCF03 
biomass_ocwf_SCF04 
biomass_ocwf_SCF05 
biomass_ocwf_SCF06 
biomass_ocwf_SCF50 
biomass_ocwf_SCF51 
biomass_ocwf_SCF52 
biomass_ocwf_SCF53 
biomass_ocwf_SCF54 
biomass_ocwf_SCF55 
biomass_ocwf_SCF60a
biomass_ocwf_SCF60b 
biomass_ocwf_SCF60c

#Ocean Whitefish biomass DFMPA site
biomass_ocwf_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_ocwf_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_ocwf_SCG01 
biomass_ocwf_SCG02 
biomass_ocwf_SCG03 
biomass_ocwf_SCG04 
biomass_ocwf_SCG05 
biomass_ocwf_SCG06 
biomass_ocwf_SCG07 
biomass_ocwf_SCG50 
biomass_ocwf_SCG51 
biomass_ocwf_SCG52a 
biomass_ocwf_SCG52b 
biomass_ocwf_SCG53a 
biomass_ocwf_SCG53b 
biomass_ocwf_SCG55 
biomass_ocwf_SCG56
biomass_ocwf_SCG57 
biomass_ocwf_SCG58 
biomass_ocwf_SCG60 
biomass_ocwf_SCG62 

#########################################################################################

#Bocaccio biomass control site
biomass_bcac_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_bcac_SCF01 
biomass_bcac_SCF02 
biomass_bcac_SCF03 
biomass_bcac_SCF04 
biomass_bcac_SCF05 
biomass_bcac_SCF06 
biomass_bcac_SCF50 
biomass_bcac_SCF51 
biomass_bcac_SCF52 
biomass_bcac_SCF53 
biomass_bcac_SCF54 
biomass_bcac_SCF55 
biomass_bcac_SCF60a
biomass_bcac_SCF60b 
biomass_bcac_SCF60c

#Bocaccio biomass DFMPA site
biomass_bcac_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_bcac_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_bcac_SCG01 
biomass_bcac_SCG02 
biomass_bcac_SCG03 
biomass_bcac_SCG04 
biomass_bcac_SCG05 
biomass_bcac_SCG06 
biomass_bcac_SCG07 
biomass_bcac_SCG50 
biomass_bcac_SCG51 
biomass_bcac_SCG52a 
biomass_bcac_SCG52b 
biomass_bcac_SCG53a 
biomass_bcac_SCG53b 
biomass_bcac_SCG55 
biomass_bcac_SCG56
biomass_bcac_SCG57 
biomass_bcac_SCG58 
biomass_bcac_SCG60 
biomass_bcac_SCG62 

#########################################################################################

#Copper biomass control site
biomass_copp_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_copp_SCF01 
biomass_copp_SCF02 
biomass_copp_SCF03 
biomass_copp_SCF04 
biomass_copp_SCF05 
biomass_copp_SCF06 
biomass_copp_SCF50 
biomass_copp_SCF51 
biomass_copp_SCF52 
biomass_copp_SCF53 
biomass_copp_SCF54 
biomass_copp_SCF55 
biomass_copp_SCF60a
biomass_copp_SCF60b 
biomass_copp_SCF60c

#Copper biomass DFMPA site
biomass_copp_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_copp_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_copp_SCG01 
biomass_copp_SCG02 
biomass_copp_SCG03 
biomass_copp_SCG04 
biomass_copp_SCG05 
biomass_copp_SCG06 
biomass_copp_SCG07 
biomass_copp_SCG50 
biomass_copp_SCG51 
biomass_copp_SCG52a 
biomass_copp_SCG52b 
biomass_copp_SCG53a 
biomass_copp_SCG53b 
biomass_copp_SCG55 
biomass_copp_SCG56
biomass_copp_SCG57 
biomass_copp_SCG58 
biomass_copp_SCG60 
biomass_copp_SCG62 

#########################################################################################

#Olive/Yellowtail biomass control site
biomass_olyt_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_olyt_SCF01 
biomass_olyt_SCF02 
biomass_olyt_SCF03 
biomass_olyt_SCF04 
biomass_olyt_SCF05 
biomass_olyt_SCF06 
biomass_olyt_SCF50 
biomass_olyt_SCF51 
biomass_olyt_SCF52 
biomass_olyt_SCF53 
biomass_olyt_SCF54 
biomass_olyt_SCF55 
biomass_olyt_SCF60a
biomass_olyt_SCF60b 
biomass_olyt_SCF60c

#Olive/Yellowtail biomass DFMPA site
biomass_olyt_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_olyt_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_olyt_SCG01 
biomass_olyt_SCG02 
biomass_olyt_SCG03 
biomass_olyt_SCG04 
biomass_olyt_SCG05 
biomass_olyt_SCG06 
biomass_olyt_SCG07 
biomass_olyt_SCG50 
biomass_olyt_SCG51 
biomass_olyt_SCG52a 
biomass_olyt_SCG52b 
biomass_olyt_SCG53a 
biomass_olyt_SCG53b 
biomass_olyt_SCG55 
biomass_olyt_SCG56
biomass_olyt_SCG57 
biomass_olyt_SCG58 
biomass_olyt_SCG60 
biomass_olyt_SCG62 

#########################################################################################

#Vermilion/Canary biomass control site
biomass_vrmlcnry_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)

biomass_vrmlcnry_SCF01 
biomass_vrmlcnry_SCF02 
biomass_vrmlcnry_SCF03 
biomass_vrmlcnry_SCF04 
biomass_vrmlcnry_SCF05 
biomass_vrmlcnry_SCF06 
biomass_vrmlcnry_SCF50 
biomass_vrmlcnry_SCF51 
biomass_vrmlcnry_SCF52 
biomass_vrmlcnry_SCF53 
biomass_vrmlcnry_SCF54 
biomass_vrmlcnry_SCF55 
biomass_vrmlcnry_SCF60a
biomass_vrmlcnry_SCF60b 
biomass_vrmlcnry_SCF60c

#Vermilion/Canary biomass DFMPA site
biomass_vrmlcnry_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_vrmlcnry_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & (obs$Fish == 44 | obs$Fish == 34) & obs$ID_quality >= 3], na.rm = TRUE)

biomass_vrmlcnry_SCG01 
biomass_vrmlcnry_SCG02 
biomass_vrmlcnry_SCG03 
biomass_vrmlcnry_SCG04 
biomass_vrmlcnry_SCG05 
biomass_vrmlcnry_SCG06 
biomass_vrmlcnry_SCG07 
biomass_vrmlcnry_SCG50 
biomass_vrmlcnry_SCG51 
biomass_vrmlcnry_SCG52a 
biomass_vrmlcnry_SCG52b 
biomass_vrmlcnry_SCG53a 
biomass_vrmlcnry_SCG53b 
biomass_vrmlcnry_SCG55 
biomass_vrmlcnry_SCG56
biomass_vrmlcnry_SCG57 
biomass_vrmlcnry_SCG58 
biomass_vrmlcnry_SCG60 
biomass_vrmlcnry_SCG62

#########################################################################################

#Dwarf-red biomass control site
biomass_drrf_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_drrf_SCF01 
biomass_drrf_SCF02 
biomass_drrf_SCF03 
biomass_drrf_SCF04 
biomass_drrf_SCF05 
biomass_drrf_SCF06 
biomass_drrf_SCF50 
biomass_drrf_SCF51 
biomass_drrf_SCF52 
biomass_drrf_SCF53
biomass_drrf_SCF54 
biomass_drrf_SCF55 
biomass_drrf_SCF60a
biomass_drrf_SCF60b 
biomass_drrf_SCF60c

#Dwarf-red biomass DFMPA site
biomass_drrf_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_drrf_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_drrf_SCG01
biomass_drrf_SCG02 
biomass_drrf_SCG03
biomass_drrf_SCG04 
biomass_drrf_SCG05 
biomass_drrf_SCG06
biomass_drrf_SCG07 
biomass_drrf_SCG50
biomass_drrf_SCG51 
biomass_drrf_SCG52a 
biomass_drrf_SCG52b 
biomass_drrf_SCG53a 
biomass_drrf_SCG53b 
biomass_drrf_SCG55 
biomass_drrf_SCG56 
biomass_drrf_SCG57 
biomass_drrf_SCG58 
biomass_drrf_SCG60 
biomass_drrf_SCG62

#########################################################################################

#Halfbanded biomass control site
biomass_hfbd_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_hfbd_SCF01 
biomass_hfbd_SCF02 
biomass_hfbd_SCF03 
biomass_hfbd_SCF04 
biomass_hfbd_SCF05 
biomass_hfbd_SCF06 
biomass_hfbd_SCF50 
biomass_hfbd_SCF51 
biomass_hfbd_SCF52 
biomass_hfbd_SCF53 
biomass_hfbd_SCF54 
biomass_hfbd_SCF55 
biomass_hfbd_SCF60a
biomass_hfbd_SCF60b 
biomass_hfbd_SCF60c

#Halfbanded biomass DFMPA site
biomass_hfbd_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_hfbd_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_hfbd_SCG01 
biomass_hfbd_SCG02 
biomass_hfbd_SCG03 
biomass_hfbd_SCG04 
biomass_hfbd_SCG05 
biomass_hfbd_SCG06 
biomass_hfbd_SCG07 
biomass_hfbd_SCG50 
biomass_hfbd_SCG51 
biomass_hfbd_SCG52a 
biomass_hfbd_SCG52b 
biomass_hfbd_SCG53a 
biomass_hfbd_SCG53b 
biomass_hfbd_SCG55 
biomass_hfbd_SCG56
biomass_hfbd_SCG57 
biomass_hfbd_SCG58 
biomass_hfbd_SCG60 
biomass_hfbd_SCG62 

#########################################################################################

#Squarespot biomass control site
biomass_sqsp_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_sqsp_SCF01 
biomass_sqsp_SCF02 
biomass_sqsp_SCF03 
biomass_sqsp_SCF04 
biomass_sqsp_SCF05 
biomass_sqsp_SCF06 
biomass_sqsp_SCF50 
biomass_sqsp_SCF51 
biomass_sqsp_SCF52 
biomass_sqsp_SCF53
biomass_sqsp_SCF54 
biomass_sqsp_SCF55 
biomass_sqsp_SCF60a
biomass_sqsp_SCF60b 
biomass_sqsp_SCF60c

#Squarespot biomass DFMPA site
biomass_sqsp_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sqsp_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE)

biomass_sqsp_SCG01
biomass_sqsp_SCG02 
biomass_sqsp_SCG03
biomass_sqsp_SCG04 
biomass_sqsp_SCG05 
biomass_sqsp_SCG06
biomass_sqsp_SCG07 
biomass_sqsp_SCG50
biomass_sqsp_SCG51 
biomass_sqsp_SCG52a 
biomass_sqsp_SCG52b 
biomass_sqsp_SCG53a 
biomass_sqsp_SCG53b 
biomass_sqsp_SCG55 
biomass_sqsp_SCG56 
biomass_sqsp_SCG57 
biomass_sqsp_SCG58 
biomass_sqsp_SCG60 
biomass_sqsp_SCG62

#########################################################################################

#Sanddab biomass control site
biomass_sdb_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)

biomass_sdb_SCF01 
biomass_sdb_SCF02 
biomass_sdb_SCF03 
biomass_sdb_SCF04 
biomass_sdb_SCF05 
biomass_sdb_SCF06 
biomass_sdb_SCF50 
biomass_sdb_SCF51 
biomass_sdb_SCF52 
biomass_sdb_SCF53
biomass_sdb_SCF54 
biomass_sdb_SCF55 
biomass_sdb_SCF60a
biomass_sdb_SCF60b 
biomass_sdb_SCF60c

#Sanddab biomass DFMPA site
biomass_sdb_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_sdb_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & (obs$Fish == 22 | obs$Fish == 30) & obs$ID_quality >= 3], na.rm = TRUE)

biomass_sdb_SCG01
biomass_sdb_SCG02 
biomass_sdb_SCG03
biomass_sdb_SCG04 
biomass_sdb_SCG05 
biomass_sdb_SCG06
biomass_sdb_SCG07 
biomass_sdb_SCG50
biomass_sdb_SCG51 
biomass_sdb_SCG52a 
biomass_sdb_SCG52b 
biomass_sdb_SCG53a 
biomass_sdb_SCG53b 
biomass_sdb_SCG55 
biomass_sdb_SCG56 
biomass_sdb_SCG57 
biomass_sdb_SCG58 
biomass_sdb_SCG60 
biomass_sdb_SCG62

#########################################################################################

#Perch biomass control site
biomass_prch_SCF01 = sum(obs$biomass[obs$tran_ID == 110 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF02 = sum(obs$biomass[obs$tran_ID == 111 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF03 = sum(obs$biomass[obs$tran_ID == 119 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF04 = sum(obs$biomass[obs$tran_ID == 120 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF05 = sum(obs$biomass[obs$tran_ID == 140 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF06 = sum(obs$biomass[obs$tran_ID == 121 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF50 = sum(obs$biomass[obs$tran_ID == 149 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF51 = sum(obs$biomass[obs$tran_ID == 150 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF52 = sum(obs$biomass[obs$tran_ID == 151 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF53 = sum(obs$biomass[obs$tran_ID == 156 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF54 = sum(obs$biomass[obs$tran_ID == 157 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF55 = sum(obs$biomass[obs$tran_ID == 152 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF60a = sum(obs$biomass[obs$tran_ID == 153 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF60b = sum(obs$biomass[obs$tran_ID == 154 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCF60c = sum(obs$biomass[obs$tran_ID == 155 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)

biomass_prch_SCF01 
biomass_prch_SCF02 
biomass_prch_SCF03 
biomass_prch_SCF04 
biomass_prch_SCF05 
biomass_prch_SCF06 
biomass_prch_SCF50 
biomass_prch_SCF51 
biomass_prch_SCF52 
biomass_prch_SCF53
biomass_prch_SCF54 
biomass_prch_SCF55 
biomass_prch_SCF60a
biomass_prch_SCF60b 
biomass_prch_SCF60c

#Perch biomass DFMPA site
biomass_prch_SCG01 = sum(obs$biomass[obs$tran_ID == 105 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG02 = sum(obs$biomass[obs$tran_ID == 106 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG03 = sum(obs$biomass[obs$tran_ID == 107 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG04 = sum(obs$biomass[obs$tran_ID == 108 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG05 = sum(obs$biomass[obs$tran_ID == 109 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG06 = sum(obs$biomass[obs$tran_ID == 112 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG07 = sum(obs$biomass[obs$tran_ID == 113 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG50 = sum(obs$biomass[obs$tran_ID == 158 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG51 = sum(obs$biomass[obs$tran_ID == 159 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG52a = sum(obs$biomass[obs$tran_ID == 161 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG52b = sum(obs$biomass[obs$tran_ID == 162 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG53a = sum(obs$biomass[obs$tran_ID == 169 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG53b = sum(obs$biomass[obs$tran_ID == 170 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG55 = sum(obs$biomass[obs$tran_ID == 167 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG56 = sum(obs$biomass[obs$tran_ID == 168 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG57 = sum(obs$biomass[obs$tran_ID == 165 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG58 = sum(obs$biomass[obs$tran_ID == 166 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG60 = sum(obs$biomass[obs$tran_ID == 163 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)
biomass_prch_SCG62 = sum(obs$biomass[obs$tran_ID == 164 & (obs$Fish == 40 | obs$Fish == 70 | obs$Fish == 79 | obs$Fish == 91 | obs$Fish == 92 | obs$Fish == 123) & obs$ID_quality >= 3], na.rm = TRUE)

biomass_prch_SCG01
biomass_prch_SCG02 
biomass_prch_SCG03
biomass_prch_SCG04 
biomass_prch_SCG05 
biomass_prch_SCG06
biomass_prch_SCG07 
biomass_prch_SCG50
biomass_prch_SCG51 
biomass_prch_SCG52a 
biomass_prch_SCG52b 
biomass_prch_SCG53a 
biomass_prch_SCG53b 
biomass_prch_SCG55 
biomass_prch_SCG56 
biomass_prch_SCG57 
biomass_prch_SCG58 
biomass_prch_SCG60 
biomass_prch_SCG62

#########################################################################################
#########################################################################################

#Species richness (number of species per transect) (fish only)

richness_SCF01 = length(unique(obs$Fish[obs$tran_ID == 110]))
richness_SCF01
unique(obs$Fish[obs$tran_ID == 110]) 
#check for NAs, if an NA is present, subtract 1 from richness value

richness_SCF02 = length(unique(obs$Fish[obs$tran_ID == 111]))
richness_SCF02
unique(obs$Fish[obs$tran_ID == 111])

richness_SCF03 = length(unique(obs$Fish[obs$tran_ID == 119]))
richness_SCF03
unique(obs$Fish[obs$tran_ID == 119])

richness_SCF04 = length(unique(obs$Fish[obs$tran_ID == 120]))
richness_SCF04
unique(obs$Fish[obs$tran_ID == 120])

richness_SCF05 = length(unique(obs$Fish[obs$tran_ID == 140]))
richness_SCF05
unique(obs$Fish[obs$tran_ID == 140])

richness_SCF06 = length(unique(obs$Fish[obs$tran_ID == 121]))
richness_SCF06
unique(obs$Fish[obs$tran_ID == 121])

richness_SCF50 = length(unique(obs$Fish[obs$tran_ID == 149]))
richness_SCF50
unique(obs$Fish[obs$tran_ID == 149])

richness_SCF51 = length(unique(obs$Fish[obs$tran_ID == 150]))
richness_SCF51
unique(obs$Fish[obs$tran_ID == 150])

richness_SCF52 = length(unique(obs$Fish[obs$tran_ID == 151]))
richness_SCF52
unique(obs$Fish[obs$tran_ID == 151])

richness_SCF53 = length(unique(obs$Fish[obs$tran_ID == 156]))
richness_SCF53
unique(obs$Fish[obs$tran_ID == 156])

richness_SCF54 = length(unique(obs$Fish[obs$tran_ID == 157]))
richness_SCF54
unique(obs$Fish[obs$tran_ID == 157])

richness_SCF55 = length(unique(obs$Fish[obs$tran_ID == 152]))
richness_SCF55
unique(obs$Fish[obs$tran_ID == 152])

richness_SCF60a = length(unique(obs$Fish[obs$tran_ID == 153]))
richness_SCF60a
unique(obs$Fish[obs$tran_ID == 153])

richness_SCF60b = length(unique(obs$Fish[obs$tran_ID == 154]))
richness_SCF60b
unique(obs$Fish[obs$tran_ID == 154])

richness_SCF60c = length(unique(obs$Fish[obs$tran_ID == 155]))
richness_SCF60c
unique(obs$Fish[obs$tran_ID == 155])

richness_SCG01 = length(unique(obs$Fish[obs$tran_ID == 105]))
richness_SCG01
unique(obs$Fish[obs$tran_ID == 105])

richness_SCG02 = length(unique(obs$Fish[obs$tran_ID == 106]))
richness_SCG02
unique(obs$Fish[obs$tran_ID == 106])

richness_SCG03 = length(unique(obs$Fish[obs$tran_ID == 107]))
richness_SCG03
unique(obs$Fish[obs$tran_ID == 107])

richness_SCG04 = length(unique(obs$Fish[obs$tran_ID == 108]))
richness_SCG04
unique(obs$Fish[obs$tran_ID == 108])

richness_SCG05 = length(unique(obs$Fish[obs$tran_ID == 109]))
richness_SCG05
unique(obs$Fish[obs$tran_ID == 109])

richness_SCG06 = length(unique(obs$Fish[obs$tran_ID == 112]))
richness_SCG06
unique(obs$Fish[obs$tran_ID == 112])

richness_SCG07 = length(unique(obs$Fish[obs$tran_ID == 113]))
richness_SCG07
unique(obs$Fish[obs$tran_ID == 113])

richness_SCG50 = length(unique(obs$Fish[obs$tran_ID == 158]))
richness_SCG50
unique(obs$Fish[obs$tran_ID == 158])

richness_SCG51 = length(unique(obs$Fish[obs$tran_ID == 159]))
richness_SCG51
unique(obs$Fish[obs$tran_ID == 159])

richness_SCG52a = length(unique(obs$Fish[obs$tran_ID == 161]))
richness_SCG52a
unique(obs$Fish[obs$tran_ID == 161])

richness_SCG52b = length(unique(obs$Fish[obs$tran_ID == 162]))
richness_SCG52b
unique(obs$Fish[obs$tran_ID == 162])

richness_SCG53a = length(unique(obs$Fish[obs$tran_ID == 169]))
richness_SCG53a
unique(obs$Fish[obs$tran_ID == 169])

richness_SCG53b = length(unique(obs$Fish[obs$tran_ID == 170]))
richness_SCG53b
unique(obs$Fish[obs$tran_ID == 170])

richness_SCG55 = length(unique(obs$Fish[obs$tran_ID == 167]))
richness_SCG55
unique(obs$Fish[obs$tran_ID == 167])

richness_SCG56 = length(unique(obs$Fish[obs$tran_ID == 168]))
richness_SCG56
unique(obs$Fish[obs$tran_ID == 168])

richness_SCG57 = length(unique(obs$Fish[obs$tran_ID == 165]))
richness_SCG57
unique(obs$Fish[obs$tran_ID == 165])

richness_SCG58 = length(unique(obs$Fish[obs$tran_ID == 166]))
richness_SCG58
unique(obs$Fish[obs$tran_ID == 166])

richness_SCG60 = length(unique(obs$Fish[obs$tran_ID == 163]))
richness_SCG60
unique(obs$Fish[obs$tran_ID == 163])

richness_SCG62 = length(unique(obs$Fish[obs$tran_ID == 164]))
richness_SCG62
unique(obs$Fish[obs$tran_ID == 164])

#########################################################################################
#########################################################################################

#Analysis

#read in data for each species (abundance + biomass for fish, abundance only for inverts)

lcod = read.csv("lcod.csv")
lcod <- lcod[-c(35), ] #remove NA from last row of lcod
cash = read.csv("cash.csv")
casc = read.csv("casc.csv")
ocwf = read.csv("ocwf.csv")
bcac = read.csv("bcac.csv")
copp = read.csv("copp.csv")
olyt = read.csv("olyt.csv")
vrmlcnry = read.csv("vrmlcnry.csv")
drrf = read.csv("drrf.csv")
hfbd = read.csv("hfbd.csv")
sqsp = read.csv("sqsp.csv")
lbstr = read.csv("lbstr.csv")
sdb = read.csv("sdb.csv")
prch = read.csv("prch.csv")
cuke = read.csv("cuke.csv")

#########################################################################################

#analyze how predictor variables are related
#Factor analysis of mixed data... essentially PCA, but includes categorical variables

library(FactoMineR)
predictors = read.csv("predictors.csv")
head(predictors) #check

#FAMD
predictors$Year = NULL
predictors$Transect = NULL
famd = AFDM(predictors, graph = TRUE)
plot(famd)


#check for significant differences in depth and substrate between sites

#depth: one-way ANOVA
depth_aov = aov(mean_depth ~ Treatment, predictors)
plot(depth_aov)
summary(depth_aov)

#percent rock: one-way ANOVA
rock_aov = aov(percent_rock ~ Treatment, predictors)
plot(rock_aov)
summary(rock_aov)

########################################################################################
########################################################################################
#focal spp comparisons

#calculate mean densities and biomasses, +/- standard error

se <- function(x) sd(x)/sqrt(length(x)) #define function to calculate standard error

mean_density_lcod_control = mean(lcod$density[lcod$Treatment == "control"])
mean_density_lcod_control
SE_density_lcod_control = se(lcod$density[lcod$Treatment == "control"])
SE_density_lcod_control

mean_density_lcod_DFMPA = mean(lcod$density[lcod$Treatment == "DFMPA"])
mean_density_lcod_DFMPA
SE_density_lcod_DFMPA = se(lcod$density[lcod$Treatment == "control"])
SE_density_lcod_DFMPA

mean_density_cash_control = mean(cash$density[cash$Treatment == "control"])
mean_density_cash_control
SE_density_cash_control = se(cash$density[cash$Treatment == "control"])
SE_density_cash_control

mean_density_cash_DFMPA = mean(cash$density[cash$Treatment == "DFMPA"])
mean_density_cash_DFMPA
SE_density_cash_DFMPA = se(cash$density[cash$Treatment == "DFMPA"])
SE_density_cash_DFMPA

mean_density_casc_control = mean(casc$density[casc$Treatment == "control"])
mean_density_casc_control
SE_density_casc_control = se(casc$density[casc$Treatment == "control"])
SE_density_casc_control

mean_density_casc_DFMPA = mean(casc$density[casc$Treatment == "DFMPA"])
mean_density_casc_DFMPA
SE_density_casc_DFMPA = se(casc$density[casc$Treatment == "DFMPA"])
SE_density_casc_DFMPA

mean_density_ocwf_control = mean(ocwf$density[ocwf$Treatment == "control"])
mean_density_ocwf_control
SE_density_ocwf_control = se(ocwf$density[ocwf$Treatment == "control"])
SE_density_ocwf_control

mean_density_ocwf_DFMPA = mean(ocwf$density[ocwf$Treatment == "DFMPA"])
mean_density_ocwf_DFMPA
SE_density_ocwf_DFMPA = se(ocwf$density[ocwf$Treatment == "DFMPA"])
SE_density_ocwf_DFMPA

mean_density_bcac_control = mean(bcac$density[bcac$Treatment == "control"])
mean_density_bcac_control
SE_density_bcac_control = se(bcac$density[bcac$Treatment == "control"])
SE_density_bcac_control

mean_density_bcac_DFMPA = mean(bcac$density[bcac$Treatment == "DFMPA"])
mean_density_bcac_DFMPA
SE_density_bcac_DFMPA = se(bcac$density[bcac$Treatment == "DFMPA"])
SE_density_bcac_DFMPA

mean_density_copp_control = mean(copp$density[copp$Treatment == "control"])
mean_density_copp_control
SE_density_copp_control = se(copp$density[copp$Treatment == "control"])
SE_density_copp_control

mean_density_copp_DFMPA = mean(copp$density[copp$Treatment == "DFMPA"])
mean_density_copp_DFMPA
SE_density_copp_DFMPA = se(copp$density[copp$Treatment == "DFMPA"])
SE_density_copp_DFMPA

mean_density_olyt_control = mean(olyt$density[olyt$Treatment == "control"])
mean_density_olyt_control
SE_density_olyt_control = se(olyt$density[olyt$Treatment == "control"])
SE_density_olyt_control

mean_density_olyt_DFMPA = mean(olyt$density[olyt$Treatment == "DFMPA"])
mean_density_olyt_DFMPA
SE_density_olyt_DFMPA = se(olyt$density[olyt$Treatment == "DFMPA"])
SE_density_olyt_DFMPA

mean_density_vrmlcnry_control = mean(vrmlcnry$density[vrmlcnry$Treatment == "control"])
mean_density_vrmlcnry_control
SE_density_vrmlcnry_control = se(vrmlcnry$density[vrmlcnry$Treatment == "control"])
SE_density_vrmlcnry_control

mean_density_vrmlcnry_DFMPA = mean(vrmlcnry$density[vrmlcnry$Treatment == "DFMPA"])
mean_density_vrmlcnry_DFMPA
SE_density_vrmlcnry_DFMPA = se(vrmlcnry$density[vrmlcnry$Treatment == "DFMPA"])
SE_density_vrmlcnry_DFMPA

mean_density_drrf_control = mean(drrf$density[drrf$Treatment == "control"])
mean_density_drrf_control
SE_density_drrf_control = se(drrf$density[drrf$Treatment == "control"])
SE_density_drrf_control

mean_density_drrf_DFMPA = mean(drrf$density[drrf$Treatment == "DFMPA"])
mean_density_drrf_DFMPA
SE_density_drrf_DFMPA = se(drrf$density[drrf$Treatment == "DFMPA"])
SE_density_drrf_DFMPA

mean_density_hfbd_control = mean(hfbd$density[hfbd$Treatment == "control"])
mean_density_hfbd_control
SE_density_hfbd_control = se(hfbd$density[hfbd$Treatment == "control"])
SE_density_hfbd_control

mean_density_hfbd_DFMPA = mean(hfbd$density[hfbd$Treatment == "DFMPA"])
mean_density_hfbd_DFMPA
SE_density_hfbd_DFMPA = se(hfbd$density[hfbd$Treatment == "DFMPA"])
SE_density_hfbd_DFMPA

mean_density_sqsp_control = mean(sqsp$density[sqsp$Treatment == "control"])
mean_density_sqsp_control
SE_density_sqsp_control = se(sqsp$density[sqsp$Treatment == "control"])
SE_density_sqsp_control

mean_density_sqsp_DFMPA = mean(sqsp$density[sqsp$Treatment == "DFMPA"])
mean_density_sqsp_DFMPA
SE_density_sqsp_DFMPA = se(sqsp$density[sqsp$Treatment == "DFMPA"])
SE_density_sqsp_DFMPA

mean_density_lbstr_control = mean(lbstr$density[lbstr$Treatment == "control"])
mean_density_lbstr_control
SE_density_lbstr_control = se(lbstr$density[lbstr$Treatment == "control"])
SE_density_lbstr_control

mean_density_lbstr_DFMPA = mean(lbstr$density[lbstr$Treatment == "DFMPA"])
mean_density_lbstr_DFMPA
SE_density_lbstr_DFMPA = se(lbstr$density[lbstr$Treatment == "DFMPA"])
SE_density_lbstr_DFMPA

mean_density_sdb_control = mean(sdb$density[sdb$Treatment == "control"])
mean_density_sdb_control
SE_density_sdb_control = se(sdb$density[sdb$Treatment == "control"])
SE_density_sdb_control

mean_density_sdb_DFMPA = mean(sdb$density[sdb$Treatment == "DFMPA"])
mean_density_sdb_DFMPA
SE_density_sdb_DFMPA = se(sdb$density[sdb$Treatment == "DFMPA"])
SE_density_sdb_DFMPA

mean_density_prch_control = mean(prch$density[prch$Treatment == "control"])
mean_density_prch_control
SE_density_prch_control = se(prch$density[prch$Treatment == "control"])
SE_density_prch_control

mean_density_prch_DFMPA = mean(prch$density[prch$Treatment == "DFMPA"])
mean_density_prch_DFMPA
SE_density_prch_DFMPA = se(prch$density[prch$Treatment == "DFMPA"])
SE_density_prch_DFMPA

mean_density_cuke_control = mean(cuke$density[cuke$Treatment == "control"])
mean_density_cuke_control
SE_density_cuke_control = se(cuke$density[cuke$Treatment == "control"])
SE_density_cuke_control

mean_density_cuke_DFMPA = mean(cuke$density[cuke$Treatment == "DFMPA"])
mean_density_cuke_DFMPA
SE_density_cuke_DFMPA = se(cuke$density[cuke$Treatment == "DFMPA"])
SE_density_cuke_DFMPA

mean_biomass_lcod_control = mean(lcod$biomass_kg_per_m2[lcod$Treatment == "control"])
mean_biomass_lcod_control
SE_biomass_lcod_control = se(lcod$biomass_kg_per_m2[lcod$Treatment == "control"])
SE_biomass_lcod_control

mean_biomass_lcod_DFMPA = mean(lcod$biomass_kg_per_m2[lcod$Treatment == "DFMPA"])
mean_biomass_lcod_DFMPA
SE_biomass_lcod_DFMPA = se(lcod$biomass_kg_per_m2[lcod$Treatment == "control"])
SE_biomass_lcod_DFMPA

mean_biomass_cash_control = mean(cash$biomass_kg_per_m2[cash$Treatment == "control"])
mean_biomass_cash_control
SE_biomass_cash_control = se(cash$biomass_kg_per_m2[cash$Treatment == "control"])
SE_biomass_cash_control

mean_biomass_cash_DFMPA = mean(cash$biomass_kg_per_m2[cash$Treatment == "DFMPA"])
mean_biomass_cash_DFMPA
SE_biomass_cash_DFMPA = se(cash$biomass_kg_per_m2[cash$Treatment == "DFMPA"])
SE_biomass_cash_DFMPA

mean_biomass_casc_control = mean(casc$biomass_kg_per_m2[casc$Treatment == "control"])
mean_biomass_casc_control
SE_biomass_casc_control = se(casc$biomass_kg_per_m2[casc$Treatment == "control"])
SE_biomass_casc_control

mean_biomass_casc_DFMPA = mean(casc$biomass_kg_per_m2[casc$Treatment == "DFMPA"])
mean_biomass_casc_DFMPA
SE_biomass_casc_DFMPA = se(casc$biomass_kg_per_m2[casc$Treatment == "DFMPA"])
SE_biomass_casc_DFMPA

mean_biomass_ocwf_control = mean(ocwf$biomass_kg_per_m2[ocwf$Treatment == "control"])
mean_biomass_ocwf_control
SE_biomass_ocwf_control = se(ocwf$biomass_kg_per_m2[ocwf$Treatment == "control"])
SE_biomass_ocwf_control

mean_biomass_ocwf_DFMPA = mean(ocwf$biomass_kg_per_m2[ocwf$Treatment == "DFMPA"])
mean_biomass_ocwf_DFMPA
SE_biomass_ocwf_DFMPA = se(ocwf$biomass_kg_per_m2[ocwf$Treatment == "DFMPA"])
SE_biomass_ocwf_DFMPA

mean_biomass_bcac_control = mean(bcac$biomass_kg_per_m2[bcac$Treatment == "control"])
mean_biomass_bcac_control
SE_biomass_bcac_control = se(bcac$biomass_kg_per_m2[bcac$Treatment == "control"])
SE_biomass_bcac_control

mean_biomass_bcac_DFMPA = mean(bcac$biomass_kg_per_m2[bcac$Treatment == "DFMPA"])
mean_biomass_bcac_DFMPA
SE_biomass_bcac_DFMPA = se(bcac$biomass_kg_per_m2[bcac$Treatment == "DFMPA"])
SE_biomass_bcac_DFMPA

mean_biomass_copp_control = mean(copp$biomass_kg_per_m2[copp$Treatment == "control"])
mean_biomass_copp_control
SE_biomass_copp_control = se(copp$biomass_kg_per_m2[copp$Treatment == "control"])
SE_biomass_copp_control

mean_biomass_copp_DFMPA = mean(copp$biomass_kg_per_m2[copp$Treatment == "DFMPA"])
mean_biomass_copp_DFMPA
SE_biomass_copp_DFMPA = se(copp$biomass_kg_per_m2[copp$Treatment == "DFMPA"])
SE_biomass_copp_DFMPA

mean_biomass_olyt_control = mean(olyt$biomass_kg_per_m2[olyt$Treatment == "control"])
mean_biomass_olyt_control
SE_biomass_olyt_control = se(olyt$biomass_kg_per_m2[olyt$Treatment == "control"])
SE_biomass_olyt_control

mean_biomass_olyt_DFMPA = mean(olyt$biomass_kg_per_m2[olyt$Treatment == "DFMPA"])
mean_biomass_olyt_DFMPA
SE_biomass_olyt_DFMPA = se(olyt$biomass_kg_per_m2[olyt$Treatment == "DFMPA"])
SE_biomass_olyt_DFMPA

mean_biomass_vrmlcnry_control = mean(vrmlcnry$biomass_kg_per_m2[vrmlcnry$Treatment == "control"])
mean_biomass_vrmlcnry_control
SE_biomass_vrmlcnry_control = se(vrmlcnry$biomass_kg_per_m2[vrmlcnry$Treatment == "control"])
SE_biomass_vrmlcnry_control

mean_biomass_vrmlcnry_DFMPA = mean(vrmlcnry$biomass_kg_per_m2[vrmlcnry$Treatment == "DFMPA"])
mean_biomass_vrmlcnry_DFMPA
SE_biomass_vrmlcnry_DFMPA = se(vrmlcnry$biomass_kg_per_m2[vrmlcnry$Treatment == "DFMPA"])
SE_biomass_vrmlcnry_DFMPA

mean_biomass_drrf_control = mean(drrf$biomass_kg_per_m2[drrf$Treatment == "control"])
mean_biomass_drrf_control
SE_biomass_drrf_control = se(drrf$biomass_kg_per_m2[drrf$Treatment == "control"])
SE_biomass_drrf_control

mean_biomass_drrf_DFMPA = mean(drrf$biomass_kg_per_m2[drrf$Treatment == "DFMPA"])
mean_biomass_drrf_DFMPA
SE_biomass_drrf_DFMPA = se(drrf$biomass_kg_per_m2[drrf$Treatment == "DFMPA"])
SE_biomass_drrf_DFMPA

mean_biomass_hfbd_control = mean(hfbd$biomass_kg_per_m2[hfbd$Treatment == "control"])
mean_biomass_hfbd_control
SE_biomass_hfbd_control = se(hfbd$biomass_kg_per_m2[hfbd$Treatment == "control"])
SE_biomass_hfbd_control

mean_biomass_hfbd_DFMPA = mean(hfbd$biomass_kg_per_m2[hfbd$Treatment == "DFMPA"])
mean_biomass_hfbd_DFMPA
SE_biomass_hfbd_DFMPA = se(hfbd$biomass_kg_per_m2[hfbd$Treatment == "DFMPA"])
SE_biomass_hfbd_DFMPA

mean_biomass_sqsp_control = mean(sqsp$biomass_kg_per_m2[sqsp$Treatment == "control"])
mean_biomass_sqsp_control
SE_biomass_sqsp_control = se(sqsp$biomass_kg_per_m2[sqsp$Treatment == "control"])
SE_biomass_sqsp_control

mean_biomass_sqsp_DFMPA = mean(sqsp$biomass_kg_per_m2[sqsp$Treatment == "DFMPA"])
mean_biomass_sqsp_DFMPA
SE_biomass_sqsp_DFMPA = se(sqsp$biomass_kg_per_m2[sqsp$Treatment == "DFMPA"])
SE_biomass_sqsp_DFMPA

mean_biomass_sdb_control = mean(sdb$biomass_kg_per_m2[sdb$Treatment == "control"])
mean_biomass_sdb_control
SE_biomass_sdb_control = se(sdb$biomass_kg_per_m2[sdb$Treatment == "control"])
SE_biomass_sdb_control

mean_biomass_sdb_DFMPA = mean(sdb$biomass_kg_per_m2[sdb$Treatment == "DFMPA"])
mean_biomass_sdb_DFMPA
SE_biomass_sdb_DFMPA = se(sdb$biomass_kg_per_m2[sdb$Treatment == "DFMPA"])
SE_biomass_sdb_DFMPA

mean_biomass_prch_control = mean(prch$biomass_kg_per_m2[prch$Treatment == "control"])
mean_biomass_prch_control
SE_biomass_prch_control = se(prch$biomass_kg_per_m2[prch$Treatment == "control"])
SE_biomass_prch_control

mean_biomass_prch_DFMPA = mean(prch$biomass_kg_per_m2[prch$Treatment == "DFMPA"])
mean_biomass_prch_DFMPA
SE_biomass_prch_DFMPA = se(prch$biomass_kg_per_m2[prch$Treatment == "DFMPA"])
SE_biomass_prch_DFMPA

#########################################################################################
#read in individual species csv's

lcod = read.csv("lcod.csv")
cash = read.csv("cash.csv")
casc = read.csv("casc.csv")
ocwf = read.csv("ocwf.csv")
bcac = read.csv("bcac.csv")
copp = read.csv("copp.csv")
olyt = read.csv("olyt.csv")
vrmlcnry = read.csv("vrmlcnry.csv")
drrf = read.csv("drrf.csv")
hfbd = read.csv("hfbd.csv")
sqsp = read.csv("sqsp.csv")
sdb = read.csv("sdb.csv")
prch = read.csv("prch.csv")
lbstr = read.csv("lbstr.csv")
cuke = read.csv("cuke.csv")

#########################################################################################
#construct GLMs for abundance and biomass, and compare w AIC

null = lm(lcod$density ~ 1, lcod)
m_T = lm(lcod$density ~ Treatment, lcod)
m_R = lm(lcod$density ~ percent_rock, lcod)
m_D = lm(lcod$density ~ mean_depth, lcod)
m_T_R = lm (lcod$density ~ Treatment + percent_rock, lcod)
m_T_D = lm (lcod$density ~ Treatment + mean_depth, lcod)
m_R_D = lm (lcod$density ~ percent_rock + mean_depth, lcod)
m_T_R_D = lm (lcod$density ~ Treatment + percent_rock + mean_depth, lcod)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(lcod[,1]))
aict

null = lm(cash$density ~ 1, cash)
m_T = lm(cash$density ~ Treatment, cash)
m_R = lm(cash$density ~ percent_rock, cash)
m_D = lm(cash$density ~ mean_depth, cash)
m_T_R = lm (cash$density ~ Treatment + percent_rock, cash)
m_T_D = lm (cash$density ~ Treatment + mean_depth, cash)
m_R_D = lm (cash$density ~ percent_rock + mean_depth, cash)
m_T_R_D = lm (cash$density ~ Treatment + percent_rock + mean_depth, cash)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(cash[,1]))
aict

null = lm(casc$density ~ 1, casc)
m_T = lm(casc$density ~ Treatment, casc)
m_R = lm(casc$density ~ percent_rock, casc)
m_D = lm(casc$density ~ mean_depth, casc)
m_T_R = lm (casc$density ~ Treatment + percent_rock, casc)
m_T_D = lm (casc$density ~ Treatment + mean_depth, casc)
m_R_D = lm (casc$density ~ percent_rock + mean_depth, casc)
m_T_R_D = lm (casc$density ~ Treatment + percent_rock + mean_depth, casc)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(casc[,1]))
aict

null = lm(ocwf$density ~ 1, ocwf)
m_T = lm(ocwf$density ~ Treatment, ocwf)
m_R = lm(ocwf$density ~ percent_rock, ocwf)
m_D = lm(ocwf$density ~ mean_depth, ocwf)
m_T_R = lm (ocwf$density ~ Treatment + percent_rock, ocwf)
m_T_D = lm (ocwf$density ~ Treatment + mean_depth, ocwf)
m_R_D = lm (ocwf$density ~ percent_rock + mean_depth, ocwf)
m_T_R_D = lm (ocwf$density ~ Treatment + percent_rock + mean_depth, ocwf)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(ocwf[,1]))
aict

null = lm(bcac$density ~ 1, bcac)
m_T = lm(bcac$density ~ Treatment, bcac)
m_R = lm(bcac$density ~ percent_rock, bcac)
m_D = lm(bcac$density ~ mean_depth, bcac)
m_T_R = lm (bcac$density ~ Treatment + percent_rock, bcac)
m_T_D = lm (bcac$density ~ Treatment + mean_depth, bcac)
m_R_D = lm (bcac$density ~ percent_rock + mean_depth, bcac)
m_T_R_D = lm (bcac$density ~ Treatment + percent_rock + mean_depth, bcac)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(bcac[,1]))
aict

null = lm(copp$density ~ 1, copp)
m_T = lm(copp$density ~ Treatment, copp)
m_R = lm(copp$density ~ percent_rock, copp)
m_D = lm(copp$density ~ mean_depth, copp)
m_T_R = lm (copp$density ~ Treatment + percent_rock, copp)
m_T_D = lm (copp$density ~ Treatment + mean_depth, copp)
m_R_D = lm (copp$density ~ percent_rock + mean_depth, copp)
m_T_R_D = lm (copp$density ~ Treatment + percent_rock + mean_depth, copp)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(copp[,1]))
aict

null = lm(olyt$density ~ 1, olyt)
m_T = lm(olyt$density ~ Treatment, olyt)
m_R = lm(olyt$density ~ percent_rock, olyt)
m_D = lm(olyt$density ~ mean_depth, olyt)
m_T_R = lm (olyt$density ~ Treatment + percent_rock, olyt)
m_T_D = lm (olyt$density ~ Treatment + mean_depth, olyt)
m_R_D = lm (olyt$density ~ percent_rock + mean_depth, olyt)
m_T_R_D = lm (olyt$density ~ Treatment + percent_rock + mean_depth, olyt)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(olyt[,1]))
aict

null = lm(vrmlcnry$density ~ 1, vrmlcnry)
m_T = lm(vrmlcnry$density ~ Treatment, vrmlcnry)
m_R = lm(vrmlcnry$density ~ percent_rock, vrmlcnry)
m_D = lm(vrmlcnry$density ~ mean_depth, vrmlcnry)
m_T_R = lm (vrmlcnry$density ~ Treatment + percent_rock, vrmlcnry)
m_T_D = lm (vrmlcnry$density ~ Treatment + mean_depth, vrmlcnry)
m_R_D = lm (vrmlcnry$density ~ percent_rock + mean_depth, vrmlcnry)
m_T_R_D = lm (vrmlcnry$density ~ Treatment + percent_rock + mean_depth, vrmlcnry)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(vrmlcnry[,1]))
aict

null = lm(drrf$density ~ 1, drrf)
m_T = lm(drrf$density ~ Treatment, drrf)
m_R = lm(drrf$density ~ percent_rock, drrf)
m_D = lm(drrf$density ~ mean_depth, drrf)
m_T_R = lm (drrf$density ~ Treatment + percent_rock, drrf)
m_T_D = lm (drrf$density ~ Treatment + mean_depth, drrf)
m_R_D = lm (drrf$density ~ percent_rock + mean_depth, drrf)
m_T_R_D = lm (drrf$density ~ Treatment + percent_rock + mean_depth, drrf)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(drrf[,1]))
aict

null = lm(hfbd$density ~ 1, hfbd)
m_T = lm(hfbd$density ~ Treatment, hfbd)
m_R = lm(hfbd$density ~ percent_rock, hfbd)
m_D = lm(hfbd$density ~ mean_depth, hfbd)
m_T_R = lm (hfbd$density ~ Treatment + percent_rock, hfbd)
m_T_D = lm (hfbd$density ~ Treatment + mean_depth, hfbd)
m_R_D = lm (hfbd$density ~ percent_rock + mean_depth, hfbd)
m_T_R_D = lm (hfbd$density ~ Treatment + percent_rock + mean_depth, hfbd)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(hfbd[,1]))
aict

null = lm(sqsp$density ~ 1, sqsp)
m_T = lm(sqsp$density ~ Treatment, sqsp)
m_R = lm(sqsp$density ~ percent_rock, sqsp)
m_D = lm(sqsp$density ~ mean_depth, sqsp)
m_T_R = lm (sqsp$density ~ Treatment + percent_rock, sqsp)
m_T_D = lm (sqsp$density ~ Treatment + mean_depth, sqsp)
m_R_D = lm (sqsp$density ~ percent_rock + mean_depth, sqsp)
m_T_R_D = lm (sqsp$density ~ Treatment + percent_rock + mean_depth, sqsp)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(sqsp[,1]))
aict

null = lm(lbstr$density ~ 1, lbstr)
m_T = lm(lbstr$density ~ Treatment, lbstr)
m_R = lm(lbstr$density ~ percent_rock, lbstr)
m_D = lm(lbstr$density ~ mean_depth, lbstr)
m_T_R = lm (lbstr$density ~ Treatment + percent_rock, lbstr)
m_T_D = lm (lbstr$density ~ Treatment + mean_depth, lbstr)
m_R_D = lm (lbstr$density ~ percent_rock + mean_depth, lbstr)
m_T_R_D = lm (lbstr$density ~ Treatment + percent_rock + mean_depth, lbstr)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(lbstr[,1]))
aict

null = lm(sdb$density ~ 1, sdb)
m_T = lm(sdb$density ~ Treatment, sdb)
m_R = lm(sdb$density ~ percent_rock, sdb)
m_D = lm(sdb$density ~ mean_depth, sdb)
m_T_R = lm (sdb$density ~ Treatment + percent_rock, sdb)
m_T_D = lm (sdb$density ~ Treatment + mean_depth, sdb)
m_R_D = lm (sdb$density ~ percent_rock + mean_depth, sdb)
m_T_R_D = lm (sdb$density ~ Treatment + percent_rock + mean_depth, sdb)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(sdb[,1]))
aict

null = lm(prch$density ~ 1, prch)
m_T = lm(prch$density ~ Treatment, prch)
m_R = lm(prch$density ~ percent_rock, prch)
m_D = lm(prch$density ~ mean_depth, prch)
m_T_R = lm (prch$density ~ Treatment + percent_rock, prch)
m_T_D = lm (prch$density ~ Treatment + mean_depth, prch)
m_R_D = lm (prch$density ~ percent_rock + mean_depth, prch)
m_T_R_D = lm (prch$density ~ Treatment + percent_rock + mean_depth, prch)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(prch[,1]))
aict

null = lm(cuke$density ~ 1, cuke)
m_T = lm(cuke$density ~ Treatment, cuke)
m_R = lm(cuke$density ~ percent_rock, cuke)
m_D = lm(cuke$density ~ mean_depth, cuke)
m_T_R = lm (cuke$density ~ Treatment + percent_rock, cuke)
m_T_D = lm (cuke$density ~ Treatment + mean_depth, cuke)
m_R_D = lm (cuke$density ~ percent_rock + mean_depth, cuke)
m_T_R_D = lm (cuke$density ~ Treatment + percent_rock + mean_depth, cuke)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(cuke[,1]))
aict

null = lm(lcod$biomass_kg_per_m2 ~ 1, lcod)
m_T = lm(lcod$biomass_kg_per_m2 ~ Treatment, lcod)
m_R = lm(lcod$biomass_kg_per_m2 ~ percent_rock, lcod)
m_D = lm(lcod$biomass_kg_per_m2 ~ mean_depth, lcod)
m_T_R = lm (lcod$biomass_kg_per_m2 ~ Treatment + percent_rock, lcod)
m_T_D = lm (lcod$biomass_kg_per_m2 ~ Treatment + mean_depth, lcod)
m_R_D = lm (lcod$biomass_kg_per_m2 ~ percent_rock + mean_depth, lcod)
m_T_R_D = lm (lcod$biomass_kg_per_m2 ~ Treatment + percent_rock + mean_depth, lcod)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(lcod[,1]))
aict

null = lm(cash$biomass_kg_per_m2 ~ 1, cash)
m_T = lm(cash$biomass_kg_per_m2 ~ Treatment, cash)
m_R = lm(cash$biomass_kg_per_m2 ~ percent_rock, cash)
m_D = lm(cash$biomass_kg_per_m2 ~ mean_depth, cash)
m_T_R = lm (cash$biomass_kg_per_m2 ~ Treatment + percent_rock, cash)
m_T_D = lm (cash$biomass_kg_per_m2 ~ Treatment + mean_depth, cash)
m_R_D = lm (cash$biomass_kg_per_m2 ~ percent_rock + mean_depth, cash)
m_T_R_D = lm (cash$biomass_kg_per_m2 ~ Treatment + percent_rock + mean_depth, cash)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(cash[,1]))
aict

null = lm(casc$biomass_kg_per_m2 ~ 1, casc)
m_T = lm(casc$biomass_kg_per_m2 ~ Treatment, casc)
m_R = lm(casc$biomass_kg_per_m2 ~ percent_rock, casc)
m_D = lm(casc$biomass_kg_per_m2 ~ mean_depth, casc)
m_T_R = lm (casc$biomass_kg_per_m2 ~ Treatment + percent_rock, casc)
m_T_D = lm (casc$biomass_kg_per_m2 ~ Treatment + mean_depth, casc)
m_R_D = lm (casc$biomass_kg_per_m2 ~ percent_rock + mean_depth, casc)
m_T_R_D = lm (casc$biomass_kg_per_m2 ~ Treatment + percent_rock + mean_depth, casc)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(casc[,1]))
aict

null = lm(ocwf$biomass_kg_per_m2 ~ 1, ocwf)
m_T = lm(ocwf$biomass_kg_per_m2 ~ Treatment, ocwf)
m_R = lm(ocwf$biomass_kg_per_m2 ~ percent_rock, ocwf)
m_D = lm(ocwf$biomass_kg_per_m2 ~ mean_depth, ocwf)
m_T_R = lm (ocwf$biomass_kg_per_m2 ~ Treatment + percent_rock, ocwf)
m_T_D = lm (ocwf$biomass_kg_per_m2 ~ Treatment + mean_depth, ocwf)
m_R_D = lm (ocwf$biomass_kg_per_m2 ~ percent_rock + mean_depth, ocwf)
m_T_R_D = lm (ocwf$biomass_kg_per_m2 ~ Treatment + percent_rock + mean_depth, ocwf)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(ocwf[,1]))
aict

null = lm(bcac$biomass_kg_per_m2 ~ 1, bcac)
m_T = lm(bcac$biomass_kg_per_m2 ~ Treatment, bcac)
m_R = lm(bcac$biomass_kg_per_m2 ~ percent_rock, bcac)
m_D = lm(bcac$biomass_kg_per_m2 ~ mean_depth, bcac)
m_T_R = lm (bcac$biomass_kg_per_m2 ~ Treatment + percent_rock, bcac)
m_T_D = lm (bcac$biomass_kg_per_m2 ~ Treatment + mean_depth, bcac)
m_R_D = lm (bcac$biomass_kg_per_m2 ~ percent_rock + mean_depth, bcac)
m_T_R_D = lm (bcac$biomass_kg_per_m2 ~ Treatment + percent_rock + mean_depth, bcac)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(bcac[,1]))
aict

null = lm(copp$biomass_kg_per_m2 ~ 1, copp)
m_T = lm(copp$biomass_kg_per_m2 ~ Treatment, copp)
m_R = lm(copp$biomass_kg_per_m2 ~ percent_rock, copp)
m_D = lm(copp$biomass_kg_per_m2 ~ mean_depth, copp)
m_T_R = lm (copp$biomass_kg_per_m2 ~ Treatment + percent_rock, copp)
m_T_D = lm (copp$biomass_kg_per_m2 ~ Treatment + mean_depth, copp)
m_R_D = lm (copp$biomass_kg_per_m2 ~ percent_rock + mean_depth, copp)
m_T_R_D = lm (copp$biomass_kg_per_m2 ~ Treatment + percent_rock + mean_depth, copp)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(copp[,1]))
aict

null = lm(olyt$biomass_kg_per_m2 ~ 1, olyt)
m_T = lm(olyt$biomass_kg_per_m2 ~ Treatment, olyt)
m_R = lm(olyt$biomass_kg_per_m2 ~ percent_rock, olyt)
m_D = lm(olyt$biomass_kg_per_m2 ~ mean_depth, olyt)
m_T_R = lm (olyt$biomass_kg_per_m2 ~ Treatment + percent_rock, olyt)
m_T_D = lm (olyt$biomass_kg_per_m2 ~ Treatment + mean_depth, olyt)
m_R_D = lm (olyt$biomass_kg_per_m2 ~ percent_rock + mean_depth, olyt)
m_T_R_D = lm (olyt$biomass_kg_per_m2 ~ Treatment + percent_rock + mean_depth, olyt)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(olyt[,1]))
aict

null = lm(vrmlcnry$biomass_kg_per_m2 ~ 1, vrmlcnry)
m_T = lm(vrmlcnry$biomass_kg_per_m2 ~ Treatment, vrmlcnry)
m_R = lm(vrmlcnry$biomass_kg_per_m2 ~ percent_rock, vrmlcnry)
m_D = lm(vrmlcnry$biomass_kg_per_m2 ~ mean_depth, vrmlcnry)
m_T_R = lm (vrmlcnry$biomass_kg_per_m2 ~ Treatment + percent_rock, vrmlcnry)
m_T_D = lm (vrmlcnry$biomass_kg_per_m2 ~ Treatment + mean_depth, vrmlcnry)
m_R_D = lm (vrmlcnry$biomass_kg_per_m2 ~ percent_rock + mean_depth, vrmlcnry)
m_T_R_D = lm (vrmlcnry$biomass_kg_per_m2 ~ Treatment + percent_rock + mean_depth, vrmlcnry)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(vrmlcnry[,1]))
aict

null = lm(hfbd$biomass_kg_per_m2 ~ 1, hfbd)
m_T = lm(hfbd$biomass_kg_per_m2 ~ Treatment, hfbd)
m_R = lm(hfbd$biomass_kg_per_m2 ~ percent_rock, hfbd)
m_D = lm(hfbd$biomass_kg_per_m2 ~ mean_depth, hfbd)
m_T_R = lm (hfbd$biomass_kg_per_m2 ~ Treatment + percent_rock, hfbd)
m_T_D = lm (hfbd$biomass_kg_per_m2 ~ Treatment + mean_depth, hfbd)
m_R_D = lm (hfbd$biomass_kg_per_m2 ~ percent_rock + mean_depth, hfbd)
m_T_R_D = lm (hfbd$biomass_kg_per_m2 ~ Treatment + percent_rock + mean_depth, hfbd)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(hfbd[,1]))
aict

null = lm(sqsp$biomass_kg_per_m2 ~ 1, sqsp)
m_T = lm(sqsp$biomass_kg_per_m2 ~ Treatment, sqsp)
m_R = lm(sqsp$biomass_kg_per_m2 ~ percent_rock, sqsp)
m_D = lm(sqsp$biomass_kg_per_m2 ~ mean_depth, sqsp)
m_T_R = lm (sqsp$biomass_kg_per_m2 ~ Treatment + percent_rock, sqsp)
m_T_D = lm (sqsp$biomass_kg_per_m2 ~ Treatment + mean_depth, sqsp)
m_R_D = lm (sqsp$biomass_kg_per_m2 ~ percent_rock + mean_depth, sqsp)
m_T_R_D = lm (sqsp$biomass_kg_per_m2 ~ Treatment + percent_rock + mean_depth, sqsp)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(sqsp[,1]))
aict

null = lm(sdb$biomass_kg_per_m2 ~ 1, sdb)
m_T = lm(sdb$biomass_kg_per_m2 ~ Treatment, sdb)
m_R = lm(sdb$biomass_kg_per_m2 ~ percent_rock, sdb)
m_D = lm(sdb$biomass_kg_per_m2 ~ mean_depth, sdb)
m_T_R = lm (sdb$biomass_kg_per_m2 ~ Treatment + percent_rock, sdb)
m_T_D = lm (sdb$biomass_kg_per_m2 ~ Treatment + mean_depth, sdb)
m_R_D = lm (sdb$biomass_kg_per_m2 ~ percent_rock + mean_depth, sdb)
m_T_R_D = lm (sdb$biomass_kg_per_m2 ~ Treatment + percent_rock + mean_depth, sdb)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(sdb[,1]))
aict

null = lm(prch$biomass_kg_per_m2 ~ 1, prch)
m_T = lm(prch$biomass_kg_per_m2 ~ Treatment, prch)
m_R = lm(prch$biomass_kg_per_m2 ~ percent_rock, prch)
m_D = lm(prch$biomass_kg_per_m2 ~ mean_depth, prch)
m_T_R = lm (prch$biomass_kg_per_m2 ~ Treatment + percent_rock, prch)
m_T_D = lm (prch$biomass_kg_per_m2 ~ Treatment + mean_depth, prch)
m_R_D = lm (prch$biomass_kg_per_m2 ~ percent_rock + mean_depth, prch)
m_T_R_D = lm (prch$biomass_kg_per_m2 ~ Treatment + percent_rock + mean_depth, prch)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(prch[,1]))
aict

########################################################################################
#density and biomass side-by-side barplots


#density barplots

#abbreviate organism names so easier to read on barplots
lcod$org = "LCOD"
cash$org = "CASH"
casc$org = "CASC"
ocwf$org = "OCWF"
bcac$org = "BCAC"
copp$org = "COPP"
olyt$org = "OLYT"
vrmlcnry$org = "VRCN"
drrf$org = "DRRF"
hfbd$org = "HFBD"
sqsp$org = "SQSP"
sdb$org = "SDB"
prch$org = "PRCH"
lbstr$org = "LBSTR"
cuke$org = "CUKE"

########################################################################################
#density and biomass figures

#fishes, rock-associated

predatory_fishes_rock = rbind(lcod, cash, casc, ocwf, bcac, copp, olyt, vrmlcnry)
melted = melt(predatory_fishes_rock, id.vars = c("org", "Treatment"), measure.vars = c("density"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("LCOD", "CASH", "CASC", "OCWF", "BCAC", "COPP", "OLYT", "VRMLCNRY"))
p_density = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("blue", "red")) + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab("density (organisms per square meter)")

melted = melt(predatory_fishes_rock, id.vars = c("org", "Treatment"), measure.vars = c("biomass_kg_per_m2"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("LCOD", "CASH", "CASC", "OCWF", "BCAC", "COPP", "OLYT", "VRMLCNRY"))
p_biomass = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("blue", "red")) + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab("biomass (kg per square meter)")

multiplot(p_density, p_biomass, cols = 1) #export plot as 8" x 8" pdf



dwarf_rockfishes_rock = rbind(drrf, hfbd, sqsp)
melted = melt(dwarf_rockfishes_rock, id.vars = c("org", "Treatment"), measure.vars = c("density"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("DRRF", "HFBD", "SQSP"))
p_density = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("blue", "red")) + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab("density (organisms per square meter)")

melted = melt(dwarf_rockfishes_rock, id.vars = c("org", "Treatment"), measure.vars = c("biomass_kg_per_m2"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("DRRF", "HFBD", "SQSP"))
p_biomass = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("blue", "red")) + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab("biomass (kg per square meter)")

multiplot(p_density, p_biomass, cols = 1) #export plot as 8" x 8" pdf


#fishes, sand-associated

predatory_fishes_sand = rbind(sdb, prch)
melted = melt(predatory_fishes_sand, id.vars = c("org", "Treatment"), measure.vars = c("density"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("SDB", "PRCH"))
p_density = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("blue", "red")) + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab("density (organisms per square meter)")

melted = melt(predatory_fishes_sand, id.vars = c("org", "Treatment"), measure.vars = c("biomass_kg_per_m2"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("SDB", "PRCH"))
p_biomass = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("blue", "red")) + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab("biomass (kg per square meter)")

multiplot(p_density, p_biomass, cols = 1) #export plot as 8" x 8" pdf



#mobile inverts, rock-associated

mobile_inverts_rock = lbstr
melted = melt(mobile_inverts_rock, id.vars = c("org", "Treatment"), measure.vars = c("density"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("blue", "red")) + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab("density (organisms per square meter)")
#export plot as 8" x 4" pdf



#mobile inverts, sand-associated

mobile_inverts_sand = cuke
melted = melt(mobile_inverts_sand, id.vars = c("org", "Treatment"), measure.vars = c("density"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("blue", "red")) + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab("density (organisms per square meter)")
#export plot as 8" x 4" pdf

#########################################################################################
#modified figures for publication

predatory_fishes_rock = rbind(lcod, cash, casc, ocwf, bcac, copp, olyt, vrmlcnry)
melted = melt(predatory_fishes_rock, id.vars = c("org", "Treatment"), measure.vars = c("density"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("LCOD", "CASH", "CASC", "OCWF", "BCAC", "COPP", "OLYT", "VRCN"))
means_SE$Treatment <- gsub('control', 'fished', means_SE$Treatment)
p_density = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment, levels=c("fished", "DFMPA")))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("gray80", "gray50")) + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = c(0.85, 0.85)) + xlab(NULL) + ylab(expression(paste("organisms / m"^{2})))
p_density + opts(title = "a") #export 4.75x3.5

melted = melt(predatory_fishes_rock, id.vars = c("org", "Treatment"), measure.vars = c("biomass_kg_per_m2"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("LCOD", "CASH", "CASC", "OCWF", "BCAC", "COPP", "OLYT", "VRCN"))
p_biomass = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("gray80", "gray50"), guide = "none") + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab(expression(paste("kg / m"^{2})))
p_biomass + opts(title = "b") #export

dwarf_rockfishes_rock = rbind(drrf, hfbd, sqsp)
melted = melt(dwarf_rockfishes_rock, id.vars = c("org", "Treatment"), measure.vars = c("density"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("DRRF", "HFBD", "SQSP"))
p_density = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("gray80", "gray50"), guide = "none") + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab(expression(paste("organisms / m"^{2})))
p_density + opts(title = "c") #export

melted = melt(dwarf_rockfishes_rock, id.vars = c("org", "Treatment"), measure.vars = c("biomass_kg_per_m2"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("DRRF", "HFBD", "SQSP"))
p_biomass = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("gray80", "gray50"), guide = "none") + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab(expression(paste("kg / m"^{2})))
p_biomass + opts(title = "d") #export

predatory_fishes_sand = rbind(sdb, prch)
melted = melt(predatory_fishes_sand, id.vars = c("org", "Treatment"), measure.vars = c("density"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("SDB", "PRCH"))
p_density = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("gray80", "gray50"), guide = "none") + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab(expression(paste("organisms / m"^{2})))
p_density + opts(title = "e") #export

melted = melt(predatory_fishes_sand, id.vars = c("org", "Treatment"), measure.vars = c("biomass_kg_per_m2"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("SDB", "PRCH"))
p_biomass = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("gray80", "gray50"), guide = "none") + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab(expression(paste("kg / m"^{2})))
p_biomass + opts(title = "f") #export

mobile_inverts = rbind(lbstr, cuke)
melted = melt(mobile_inverts, id.vars = c("org", "Treatment"), measure.vars = c("density"))
means = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("org", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$org = factor(means_SE$org, levels = c("LBSTR", "CUKE"))
p_inverts = ggplot(means_SE, aes(x = org, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("gray80", "gray50"), guide = "none") + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab(expression(paste("organisms / m"^{2})))
p_inverts + opts(title = "g") #export

#########################################################################################
#comapre size frequency distributions for focal spp

obs$sizebin = 
  ifelse(obs$Size_class == "<5", "<5",
         ifelse(obs$Size_class == "5-10", "05-10",
                ifelse(obs$Size_class == "10-15", "10-15",
                       ifelse(obs$Size_class == "15-20", "15-20",
                              ifelse(obs$Size_class == "20-25", "20-25",
                                     ifelse(obs$Size_class == "25-30", "25-30", 
                                            ifelse(obs$Size_class == "30-35", "30-35",
                                                   ifelse(obs$Size_class == "35-40", "35-40", 
                                                          ifelse(obs$Size_class == "40-45", "40-45", 
                                                                 ifelse(obs$Size_class == "45-50", "45-50", 
                                                                        ifelse(obs$Size_class == "50+", "50+", NA)))))))))))
obs$sizebin = as.factor(obs$sizebin)
plot(obs$sizebin)

#subset obs so that we're only dealing with transects from control & impact sites

#subset zone F obs
obs_F = subset(obs, (obs$tran_ID == 110 |
                       obs$tran_ID == 111 |
                       obs$tran_ID == 119 |
                       obs$tran_ID == 120 |
                       obs$tran_ID == 140 |
                       obs$tran_ID == 121 |
                       obs$tran_ID == 149 |
                       obs$tran_ID == 150 |
                       obs$tran_ID == 151 |
                       obs$tran_ID == 156 |
                       obs$tran_ID == 157 |
                       obs$tran_ID == 152 |
                       obs$tran_ID == 153 |
                       obs$tran_ID == 154 |
                       obs$tran_ID == 155 ))
head(obs_F) #check

#subset zone G obs
obs_G = subset(obs, (obs$tran_ID == 105 |
                       obs$tran_ID == 106 |
                       obs$tran_ID == 107 |
                       obs$tran_ID == 108 |
                       obs$tran_ID == 109 |
                       obs$tran_ID == 112 |
                       obs$tran_ID == 113 |
                       obs$tran_ID == 158 |
                       obs$tran_ID == 159 |
                       obs$tran_ID == 161 |
                       obs$tran_ID == 162 |
                       obs$tran_ID == 169 |
                       obs$tran_ID == 170 |
                       obs$tran_ID == 167 |
                       obs$tran_ID == 168 |
                       obs$tran_ID == 165 |
                       obs$tran_ID == 166 |
                       obs$tran_ID == 163 |
                       obs$tran_ID == 164 ))
head(obs_G)#check

#lingcod
plot(obs_F$sizebin[obs_F$Fish == 15 & obs_F$ID_quality >= 3], xlab = "Size class (cm)", main = "Lingcod fished")
plot(obs_G$sizebin[obs_G$Fish == 15 & obs_G$ID_quality >= 3], xlab = "Size class (cm)", main = "Lingcod DFMPA")
table(obs_F$sizebin[obs_F$Fish == 15 & obs_F$ID_quality >= 3])
table(obs_G$sizebin[obs_G$Fish == 15 & obs_G$ID_quality >= 3])
fished = c(0,0,0,0,0,1,1,4,2,0,1) / 13593.39
DFMPA = c(0,0,0,2,0,1,2,2,2,0,6) / 25264.01
sizebin = c("<5", "05-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50+")
df1 = data.frame(fished, DFMPA, sizebin)
df2 = melt(df1, id.vars='sizebin')
plot = ggplot(df2, aes(x=sizebin, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_fill_manual(values = c("gray80", "gray50")) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = c(0.85, 0.85)) + xlab("size class (cm)") + ylab(expression(paste("organisms / m"^{2})))
plot + opts(title = "Lingcod")

#CA sheephead
plot(obs_F$sizebin[obs_F$Fish == 65 & obs_F$ID_quality >= 3], xlab = "Size class (cm)", main = "Sheephead fished")
plot(obs_G$sizebin[obs_G$Fish == 65 & obs_G$ID_quality >= 3], xlab = "Size class (cm)", main = "Sheephead DFMPA") 
table(obs_F$sizebin[obs_F$Fish == 65 & obs_F$ID_quality >= 3])
table(obs_G$sizebin[obs_G$Fish == 65 & obs_G$ID_quality >= 3])
fished = c(0,0,0,10,18,8,5,1,0,0,0) / 13593.39
DFMPA = c(0,3,33,55,71,73,42,25,6,2,4) / 25264.01
sizebin = c("<5", "05-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50+")
df1 = data.frame(fished, DFMPA, sizebin)
df2 = melt(df1, id.vars='sizebin')
plot = ggplot(df2, aes(x=sizebin, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_fill_manual(values = c("gray80", "gray50")) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = c(0.85, 0.85)) + xlab("size class (cm)") + ylab(expression(paste("organisms / m"^{2})))
plot + opts(title = "a) California sheephead")
#Kolmogorov-Smirnov test to look for differences in size freq dists
ks.test(fished, DFMPA)

#CA scorpionfish
plot(obs_F$sizebin[obs_F$Fish == 64 & obs_F$ID_quality >= 3], xlab = "Size class (cm)", main = "CA scorpionfish fished")
plot(obs_G$sizebin[obs_G$Fish == 64 & obs_G$ID_quality >= 3], xlab = "Size class (cm)", main = "CA scorpionfish DFMPA")
table(obs_F$sizebin[obs_F$Fish == 64 & obs_F$ID_quality >= 3])
table(obs_G$sizebin[obs_G$Fish == 64 & obs_G$ID_quality >= 3])
fished = c(0,0,0,3,2,0,2,0,0,0,0) / 13593.39
DFMPA = c(0,0,0,0,1,0,1,0,0,0,0) / 25264.01
sizebin = c("<5", "05-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50+")
df1 = data.frame(fished, DFMPA, sizebin)
df2 = melt(df1, id.vars='sizebin')
plot = ggplot(df2, aes(x=sizebin, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_fill_manual(values = c("gray80", "gray50")) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = c(0.85, 0.85)) + xlab("size class (cm)") + ylab(expression(paste("organisms / m"^{2})))
plot + opts(title = "California Scorpionfish")

#ocean whitefish
plot(obs_F$sizebin[obs_F$Fish == 88 & obs_F$ID_quality >= 3], xlab = "Size class (cm)", main = "Ocean whitefish fished")
plot(obs_G$sizebin[obs_G$Fish == 88 & obs_G$ID_quality >= 3], xlab = "Size class (cm)", main = "Ocean whitefish DFMPA")
table(obs_F$sizebin[obs_F$Fish == 88 & obs_F$ID_quality >= 3])
table(obs_G$sizebin[obs_G$Fish == 88 & obs_G$ID_quality >= 3])
fished = c(0,0,1,1,0,0,1,0,0,0,0) / 13593.39
DFMPA = c(0,0,1,2,1,12,4,8,0,0,1) / 25264.01
sizebin = c("<5", "05-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50+")
df1 = data.frame(fished, DFMPA, sizebin)
df2 = melt(df1, id.vars='sizebin')
plot = ggplot(df2, aes(x=sizebin, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_fill_manual(values = c("gray80", "gray50")) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = "none") + xlab("size class (cm)") + ylab(expression(paste("organisms / m"^{2})))
plot + opts(title = "b) Ocean Whitefish")
#Kolmogorov-Smirnov test to look for differences in size freq dists
ks.test(fished, DFMPA)

#copper rckf
plot(obs_F$sizebin[obs_F$Fish == 8 & obs_F$ID_quality >= 3], xlab = "Size class (cm)", main = "Copper rckf fished")
plot(obs_G$sizebin[obs_G$Fish == 8 & obs_G$ID_quality >= 3], xlab = "Size class (cm)", main = "Copper rckf DFMPA")
table(obs_F$sizebin[obs_F$Fish == 8 & obs_F$ID_quality >= 3])
table(obs_G$sizebin[obs_G$Fish == 8 & obs_G$ID_quality >= 3])
fished = c(0,0,0,1,4,5,2,4,0,0,0) / 13593.39
DFMPA = c(0,0,0,3,9,7,2,5,1,1,0) / 25264.01
sizebin = c("<5", "05-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50+")
df1 = data.frame(fished, DFMPA, sizebin)
df2 = melt(df1, id.vars='sizebin')
plot = ggplot(df2, aes(x=sizebin, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_fill_manual(values = c("gray80", "gray50")) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = c(0.85, 0.85)) + xlab("size class (cm)") + ylab(expression(paste("organisms / m"^{2})))
plot + opts(title = "Copper Rockfish")

#olive/yellowtail rckf
plot(obs_F$sizebin[obs_F$Fish == 19 & obs_F$ID_quality >= 3], xlab = "Size class (cm)", main = "olyt rckf fished")
plot(obs_G$sizebin[obs_G$Fish == 19 & obs_G$ID_quality >= 3], xlab = "Size class (cm)", main = "olyt rckf DFMPA")
table(obs_F$sizebin[obs_F$Fish == 19 & obs_F$ID_quality >= 3])
table(obs_G$sizebin[obs_G$Fish == 19 & obs_G$ID_quality >= 3])
fished = c(0,0,0,0,0,4,1,2,0,0,1) / 13593.39
DFMPA = c(0,0,0,0,2,14,10,19,2,7,10) / 25264.01
sizebin = c("<5", "05-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50+")
df1 = data.frame(fished, DFMPA, sizebin)
df2 = melt(df1, id.vars='sizebin')
plot = ggplot(df2, aes(x=sizebin, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_fill_manual(values = c("gray80", "gray50")) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = c(0.85, 0.85)) + xlab("size class (cm)") + ylab(expression(paste("organisms / m"^{2})))
plot + opts(title = "Olive/Yellowtail Rockfish")

#vermilion/canary rckf
plot(obs_F$sizebin[obs_F$Fish == 44 & obs_F$ID_quality >= 3], xlab = "Size class (cm)", main = "vrcn rckf fished")
plot(obs_G$sizebin[obs_G$Fish == 44 & obs_G$ID_quality >= 3], xlab = "Size class (cm)", main = "vrcn rckf DFMPA")
table(obs_F$sizebin[obs_F$Fish == 44 & obs_F$ID_quality >= 3])
table(obs_G$sizebin[obs_G$Fish == 44 & obs_G$ID_quality >= 3])
fished = c(0,0,0,0,2,3,0,5,1,0,0) / 13593.39
DFMPA = c(0,0,0,0,1,4,4,14,0,1,2) / 25264.01
sizebin = c("<5", "05-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50+")
df1 = data.frame(fished, DFMPA, sizebin)
df2 = melt(df1, id.vars='sizebin')
plot = ggplot(df2, aes(x=sizebin, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_fill_manual(values = c("gray80", "gray50")) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = c(0.85, 0.85)) + xlab("size class (cm)") + ylab(expression(paste("organisms / m"^{2})))
plot + opts(title = "Vermilion/Canary Rockfish")

#dwarf-red rckf
plot(obs_F$sizebin[obs_F$Fish == 86 & obs_F$ID_quality >= 3], xlab = "Size class (cm)", main = "drrf rckf fished")
plot(obs_G$sizebin[obs_G$Fish == 86 & obs_G$ID_quality >= 3], xlab = "Size class (cm)", main = "drrf rckf DFMPA")
table(obs_F$sizebin[obs_F$Fish == 86 & obs_F$ID_quality >= 3])
table(obs_G$sizebin[obs_G$Fish == 86 & obs_G$ID_quality >= 3])
fished = c(0,14,5,0,0,0,0,0,0,0,0) / 13593.39
DFMPA = c(0,11,1,1,0,0,0,0,0,0,0) / 25264.01
sizebin = c("<5", "05-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50+")
df1 = data.frame(fished, DFMPA, sizebin)
df2 = melt(df1, id.vars='sizebin')
plot = ggplot(df2, aes(x=sizebin, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_fill_manual(values = c("gray80", "gray50")) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = c(0.85, 0.85)) + xlab("size class (cm)") + ylab(expression(paste("organisms / m"^{2})))
plot + opts(title = "Dwarf-red Rockfish")

#halfbanded rckf
plot(obs_F$sizebin[obs_F$Fish == 39 & obs_F$ID_quality >= 3], xlab = "Size class (cm)", main = "hfbd rckf fished")
plot(obs_G$sizebin[obs_G$Fish == 39 & obs_G$ID_quality >= 3], xlab = "Size class (cm)", main = "hfbd rckf DFMPA")
table(obs_F$sizebin[obs_F$Fish == 39 & obs_F$ID_quality >= 3])
table(obs_G$sizebin[obs_G$Fish == 39 & obs_G$ID_quality >= 3])
fished = c(0,103,97,2,0,0,0,0,0,0,0) / 13593.39
DFMPA = c(0,46,82,0,0,0,0,0,0,0,0) / 25264.01
sizebin = c("<5", "05-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50+")
df1 = data.frame(fished, DFMPA, sizebin)
df2 = melt(df1, id.vars='sizebin')
plot = ggplot(df2, aes(x=sizebin, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_fill_manual(values = c("gray80", "gray50")) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = c(0.85, 0.85)) + xlab("size class (cm)") + ylab(expression(paste("organisms / m"^{2})))
plot + opts(title = "Halfbanded Rockfish")

#squarespot rckf
plot(obs_F$sizebin[obs_F$Fish == 57 & obs_F$ID_quality >= 3], xlab = "Size class (cm)", main = "sqsp rckf fished")
plot(obs_G$sizebin[obs_G$Fish == 57 & obs_G$ID_quality >= 3], xlab = "Size class (cm)", main = "sqsp rckf DFMPA")
table(obs_F$sizebin[obs_F$Fish == 57 & obs_F$ID_quality >= 3])
table(obs_G$sizebin[obs_G$Fish == 57 & obs_G$ID_quality >= 3])
fished = c(0,21,84,20,2,0,0,0,0,0,0) / 13593.39
DFMPA = c(0,52,307,98,5,2,0,0,0,0,0) / 25264.01
sizebin = c("<5", "05-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50+")
df1 = data.frame(fished, DFMPA, sizebin)
df2 = melt(df1, id.vars='sizebin')
plot = ggplot(df2, aes(x=sizebin, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_fill_manual(values = c("gray80", "gray50")) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = c(0.85, 0.85)) + xlab("size class (cm)") + ylab(expression(paste("organisms / m"^{2})))
plot + opts(title = "Squarespot Rockfish")

#sanddab
plot(obs_F$sizebin[(obs_F$Fish == 22 | obs_F$Fish == 30) & obs_F$ID_quality >= 3], xlab = "Size class (cm)", main = "sdb fished")
plot(obs_G$sizebin[(obs_G$Fish == 22 | obs_G$Fish == 30) & obs_G$ID_quality >= 3], xlab = "Size class (cm)", main = "sdb DFMPA")
table(obs_F$sizebin[(obs_F$Fish == 22 | obs_F$Fish == 30) & obs_F$ID_quality >= 3])
table(obs_G$sizebin[(obs_G$Fish == 22 | obs_G$Fish == 30) & obs_G$ID_quality >= 3])
fished = c(0,3,2,1,1,0,0,0,0,0,0) / 13593.39
DFMPA = c(0,0,3,2,1,2,0,0,0,0,0) / 25264.01
sizebin = c("<5", "05-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50+")
df1 = data.frame(fished, DFMPA, sizebin)
df2 = melt(df1, id.vars='sizebin')
plot = ggplot(df2, aes(x=sizebin, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_fill_manual(values = c("gray80", "gray50")) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = c(0.85, 0.85)) + xlab("size class (cm)") + ylab(expression(paste("organisms / m"^{2})))
plot + opts(title = "Sanddab")

#perch
plot(obs_F$sizebin[(obs_F$Fish == 40 | obs_F$Fish == 70 | obs_F$Fish == 79 | obs_F$Fish == 91 | obs_F$Fish == 92 | obs_F$Fish == 123) & obs_F$ID_quality >= 3], xlab = "Size class (cm)", main = "prch fished")
plot(obs_G$sizebin[(obs_G$Fish == 40 | obs_G$Fish == 70 | obs_G$Fish == 79 | obs_G$Fish == 91 | obs_G$Fish == 92 | obs_G$Fish == 123) & obs_G$ID_quality >= 3], xlab = "Size class (cm)", main = "prch DFMPA")
table(obs_F$sizebin[(obs_F$Fish == 40 | obs_F$Fish == 70 | obs_F$Fish == 79 | obs_F$Fish == 91 | obs_F$Fish == 92 | obs_F$Fish == 123) & obs_F$ID_quality >= 3])
table(obs_G$sizebin[(obs_G$Fish == 40 | obs_G$Fish == 70 | obs_G$Fish == 79 | obs_G$Fish == 91 | obs_G$Fish == 92 | obs_G$Fish == 123) & obs_G$ID_quality >= 3])
fished = c(0,11,20,2,1,0,0,0,0,0,0) / 13593.39
DFMPA = c(0,12,30,6,0,0,1,0,0,0,0) / 25264.01
sizebin = c("<5", "05-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50+")
df1 = data.frame(fished, DFMPA, sizebin)
df2 = melt(df1, id.vars='sizebin')
plot = ggplot(df2, aes(x=sizebin, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_fill_manual(values = c("gray80", "gray50")) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), legend.position = c(0.85, 0.85)) + xlab("size class (cm)") + ylab(expression(paste("organisms / m"^{2})))
plot + opts(title = "Perch")

#########################################################################################
#########################################################################################
#community-level comparisons: richness and diversity

#richness

richness = read.csv("richness.csv")

#calculate means +/- SE

mean_richness_control = mean(richness$richness_std[richness$Treatment == "control"])
mean_richness_control
SE_richness_control = se(richness$richness_std[richness$Treatment == "control"])
SE_richness_control

mean_richness_DFMPA = mean(richness$richness_std[richness$Treatment == "DFMPA"])
mean_richness_DFMPA
SE_richness_control = se(richness$richness_std[richness$Treatment == "DFMPA"])
SE_richness_control

#GLMs
null = lm(richness$richness_std ~ 1, richness)
m_T = lm(richness$richness_std ~ Treatment, richness)
m_R = lm(richness$richness_std ~ percent_rock, richness)
m_D = lm(richness$richness_std ~ mean_depth, richness)
m_T_R = lm (richness$richness_std ~ Treatment + percent_rock, richness)
m_T_D = lm (richness$richness_std ~ Treatment + mean_depth, richness)
m_R_D = lm (richness$richness_std ~ percent_rock + mean_depth, richness)
m_T_R_D = lm (richness$richness_std ~ Treatment + percent_rock + mean_depth, richness)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(richness[,1]))
aict

#########################################################################################
#diversity

diversity = read.csv("diversity.csv")

#calculate means +/- SE

mean_diversity_control = mean(diversity$Shannon_std[diversity$Treatment == "control"])
mean_diversity_control
SE_diversity_control = se(diversity$Shannon_std[diversity$Treatment == "control"])
SE_diversity_control

mean_diversity_DFMPA = mean(diversity$Shannon_std[diversity$Treatment == "DFMPA"])
mean_diversity_DFMPA
SE_diversity_control = se(diversity$Shannon_std[diversity$Treatment == "DFMPA"])
SE_diversity_control

null = lm(diversity$Shannon_std ~ 1, diversity)
m_T = lm(diversity$Shannon_std ~ Treatment, diversity)
m_R = lm(diversity$Shannon_std ~ percent_rock, diversity)
m_D = lm(diversity$Shannon_std ~ mean_depth, diversity)
m_T_R = lm (diversity$Shannon_std ~ Treatment + percent_rock, diversity)
m_T_D = lm (diversity$Shannon_std ~ Treatment + mean_depth, diversity)
m_R_D = lm (diversity$Shannon_std ~ percent_rock + mean_depth, diversity)
m_T_R_D = lm (diversity$Shannon_std ~ Treatment + percent_rock + mean_depth, diversity)

aic = AIC(null, m_T, m_R, m_D, m_T_R, m_T_D, m_R_D, m_T_R_D)

fredsAICtable <- function( aic, n) { 
K <- aic$df                                     
AICc <- aic$AIC + 2 * K * (K+1) / ( n - K - 1 ) 
delAIC<- AICc - min( AICc )
AICw <- exp(-0.5*delAIC) / sum( exp(-0.5*delAIC)) 
data.frame( aic, AICc, delAIC , AICw)
                                     
};
aict=fredsAICtable(aic, n=length(diversity[,1]))
aict


#richness and diversity side-by-side barplots

richness_diversity = read.csv("richness_diversity.csv")
melted = melt(richness_diversity, id.vars = c("metric", "Treatment"), measure.vars = c("value"))
means = ddply(melted, c("metric", "Treatment", "variable"), summarize, mean = mean(value))
means_SE = ddply(melted, c("metric", "Treatment", "variable"), summarize, mean = mean(value), SE = sd(value)/sqrt(length(value)))
means_SE = transform(means_SE, lower = mean - SE, upper = mean + SE)
means_SE$metric = factor(means_SE$metric, levels = c("richness", "diversity"))
ggplot(means_SE, aes(x = metric, y = mean, fill=factor(Treatment))) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("blue", "red")) + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), width = 0.25, data=means_SE) + labs(fill = "Site") + theme_bw() + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + xlab(NULL) + ylab("value")

#########################################################################################
#########################################################################################
#community-level comparisons: analysis of similarity

#calculate raw counts for all unique fish species observed on each transect

fish39= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 39 & obs$ID_quality >= 3], na.rm = TRUE))

fish40= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 40 & obs$ID_quality >= 3], na.rm = TRUE))

fish65= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 65 & obs$ID_quality >= 3], na.rm = TRUE))

fish2= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 2 & obs$ID_quality >= 3], na.rm = TRUE))

fish74= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 74 & obs$ID_quality >= 3], na.rm = TRUE))

fish8= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 8 & obs$ID_quality >= 3], na.rm = TRUE))

fish52= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 52 & obs$ID_quality >= 3], na.rm = TRUE))

fish13= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 13 & obs$ID_quality >= 3], na.rm = TRUE))

fish72= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 72 & obs$ID_quality >= 3], na.rm = TRUE))

fish48= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 48 & obs$ID_quality >= 3], na.rm = TRUE))

fish49= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 49 & obs$ID_quality >= 3], na.rm = TRUE))

fish15= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 15 & obs$ID_quality >= 3], na.rm = TRUE))

fish41= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 41 & obs$ID_quality >= 3], na.rm = TRUE))

fish83= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 83 & obs$ID_quality >= 3], na.rm = TRUE))

fish28= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 28 & obs$ID_quality >= 3], na.rm = TRUE))

fish53= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 53 & obs$ID_quality >= 3], na.rm = TRUE))

fish97= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 97 & obs$ID_quality >= 3], na.rm = TRUE))

fish107= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 107 & obs$ID_quality >= 3], na.rm = TRUE))

fish106= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 106 & obs$ID_quality >= 3], na.rm = TRUE))

fish114= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 114 & obs$ID_quality >= 3], na.rm = TRUE))

fish26= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 26 & obs$ID_quality >= 3], na.rm = TRUE))

fish3= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 3 & obs$ID_quality >= 3], na.rm = TRUE))

fish45= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 45 & obs$ID_quality >= 3], na.rm = TRUE))

fish61= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 61 & obs$ID_quality >= 3], na.rm = TRUE))

fish64= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 64 & obs$ID_quality >= 3], na.rm = TRUE))

fish56= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 56 & obs$ID_quality >= 3], na.rm = TRUE))

fish57= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 57 & obs$ID_quality >= 3], na.rm = TRUE))

fish86= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 86 & obs$ID_quality >= 3], na.rm = TRUE))

fish84= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 84 & obs$ID_quality >= 3], na.rm = TRUE))

fish43= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 43 & obs$ID_quality >= 3], na.rm = TRUE))

fish11= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 11 & obs$ID_quality >= 3], na.rm = TRUE))

fish51= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 51 & obs$ID_quality >= 3], na.rm = TRUE))

fish44= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 44 & obs$ID_quality >= 3], na.rm = TRUE))

fish34= c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 34 & obs$ID_quality >= 3], na.rm = TRUE))

fish55 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 55 & obs$ID_quality >= 3], na.rm = TRUE))

fish37 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 37 & obs$ID_quality >= 3], na.rm = TRUE))

fish47 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 47 & obs$ID_quality >= 3], na.rm = TRUE))

fish22 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 22 & obs$ID_quality >= 3], na.rm = TRUE))

fish91 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 91 & obs$ID_quality >= 3], na.rm = TRUE))

fish101 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 101 & obs$ID_quality >= 3], na.rm = TRUE))

fish92 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 92 & obs$ID_quality >= 3], na.rm = TRUE))

fish23 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 23 & obs$ID_quality >= 3], na.rm = TRUE))

fish87 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 87 & obs$ID_quality >= 3], na.rm = TRUE))

fish88 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 88 & obs$ID_quality >= 3], na.rm = TRUE))

fish5 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 5 & obs$ID_quality >= 3], na.rm = TRUE))

fish30 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 30 & obs$ID_quality >= 3], na.rm = TRUE))

fish75 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 75 & obs$ID_quality >= 3], na.rm = TRUE))

fish9 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 9 & obs$ID_quality >= 3], na.rm = TRUE))

fish125 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 125 & obs$ID_quality >= 3], na.rm = TRUE))

fish19 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 19 & obs$ID_quality >= 3], na.rm = TRUE))

fish25 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 25 & obs$ID_quality >= 3], na.rm = TRUE))

fish113 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 113 & obs$ID_quality >= 3], na.rm = TRUE))

fish16 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 16 & obs$ID_quality >= 3], na.rm = TRUE))

fish94 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 94 & obs$ID_quality >= 3], na.rm = TRUE))

fish80 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 80 & obs$ID_quality >= 3], na.rm = TRUE))

fish54 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 54 & obs$ID_quality >= 3], na.rm = TRUE))

fish38 = c(
sum(obs$Count[obs$tran_ID == 110 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 111 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 119 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 120 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 140 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 121 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 149 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 150 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 151 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 156 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 157 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 152 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 153 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 154 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 155 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 105 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 106 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 107 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 108 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 109 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 112 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 113 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 158 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 159 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 161 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 162 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 169 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 170 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 167 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 168 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 165 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 166 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 163 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE),
sum(obs$Count[obs$tran_ID == 164 & obs$Fish == 38 & obs$ID_quality >= 3], na.rm = TRUE))

#construct assemblage matrix -- raw counts first
assemblage = data.frame(fish39, fish40, fish65, fish2, fish74, fish8, fish52, fish13, fish72, fish48, fish49, fish15, fish41, fish83, fish28, fish53, fish97, fish107, fish106, fish114, fish26, fish3, fish45, fish61, fish64, fish56, fish57, fish86, fish84, fish43, fish11, fish51, fish44, fish34, fish55, fish37, fish47, fish22, fish91, fish101, fish92, fish23, fish87, fish88, fish5, fish30, fish75, fish9, fish125, fish19, fish25, fish113, fish16, fish94, fish80, fish54, fish38)

transect = c(1:34)
treatment = c(rep("control", 15), rep("DFMPA", 19))

#standardize all counts by transect area (m2)
assemblage[1,] = assemblage[1,] / 926 #SCF01
assemblage[2,] = assemblage[2,] / 912 #SCF02
assemblage[3,] = assemblage[3,] / 895 #SCF03
assemblage[4,] = assemblage[4,] / 917 #SCF04
assemblage[5,] = assemblage[5,] / 1045 #SCF05
assemblage[6,] = assemblage[6,] / 792 #SCF06
assemblage[7,] = assemblage[7,] / 856.999 #SCF50
assemblage[8,] = assemblage[8,] / 1276.452 #SCF51
assemblage[9,] = assemblage[9,] / 1279.918 #SCF52
assemblage[10,] = assemblage[10,] / 1205.806 #SCF53
assemblage[11,] = assemblage[11,] / 977.65 #SCF54
assemblage[12,] = assemblage[12,] / 1244.873 #SCF55
assemblage[13,] = assemblage[13,] / 371.124 #SCF60a
assemblage[14,] = assemblage[14,] / 411.314 #SCF60b
assemblage[15,] = assemblage[15,] / 482.251 #SCF60c
assemblage[16,] = assemblage[16,] / 1697 #SCG01
assemblage[17,] = assemblage[17,] / 1019 #SCG02
assemblage[18,] = assemblage[18,] / 763 #SCG03
assemblage[19,] = assemblage[19,] / 2393 #SCG04
assemblage[20,] = assemblage[20,] / 1926 #SCG05
assemblage[21,] = assemblage[21,] / 762 #SCG06
assemblage[22,] = assemblage[22,] / 775 #SCG07
assemblage[23,] = assemblage[23,] / 1350.028 #SCG50
assemblage[24,] = assemblage[24,] / 2274.015 #SCG51
assemblage[25,] = assemblage[25,] / 924.697 #SCG52a
assemblage[26,] = assemblage[26,] / 1638.549 #SCG52b
assemblage[27,] = assemblage[27,] / 1288.852 #SCG53a
assemblage[28,] = assemblage[28,] / 751.094 #SCG53b
assemblage[29,] = assemblage[29,] / 1423.699 #SCG55
assemblage[30,] = assemblage[30,] / 1189.343 #SCG56
assemblage[31,] = assemblage[31,] / 1942.821 #SCG57
assemblage[32,] = assemblage[32,] / 696.726 #SCG58
assemblage[33,] = assemblage[33,] / 1674.684 #SCG60
assemblage[34,] = assemblage[34,] / 774.502 #SCG62

assemblage = data.matrix(assemblage)

#calculate Bray-Curtis dissimilarity indices
bray = vegdist(assemblage, method = "bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = T)

#conduct analysis of similarity based on Bray-Curtis dissimilarity indices
anosim = anosim(bray, grouping = treatment, distance = "bray")
summary(anosim)

#########################################################################################
#cluster analysis and associated dendrogram

clust = hclust(bray)

data = data.frame(transect, treatment)

colLab = function(n){
if(is.leaf(n)){
a = attributes(n)
line = match(attributes(n)$label, data[, 1])
treat = data[line, 2];
if(treat == "control"){col_treat = "blue"}; if(treat == "DFMPA"){col_treat="red"}
attr(n,"nodePar") = c(a$nodePar,list(cex=1.5, lab.cex=1, pch=19, col=col_treat))
}
return(n)
}

clust = as.dendrogram(clust)
dL = dendrapply(clust, colLab)
plot(dL, ylab = "dissimilarity", leaflab = "none")
 
#########################################################################################
#Non-metric multidimensional scaling plot

NMDS = metaMDS(assemblage, distance = "bray",  k = 2, trymax = 100)

pchvec = c(16,2)

#plot
plot(NMDS, type = "n")
points(NMDS, pch = pchvec)
ordiellipse(NMDS, predictors$Treatment)
legend('bottomright', legend = levels(predictors$Treatment), pch = pchvec, bty = "n", title = "Site")

#modified plot for publication using ggplot2
scores = as.data.frame(scores(NMDS))
scores$Transect = rownames(scores)
scores$Site = factor(predictors$Treatment, levels = c("fished", "DFMPA"))
Site = scores$Site
grp_fished = scores[scores$Site == "fished", ][chull(scores[scores$Site == 
    "fished", c("NMDS1", "NMDS2")]), ]
grp_DFMPA = scores[scores$Site == "DFMPA", ][chull(scores[scores$Site == 
    "DFMPA", c("NMDS1", "NMDS2")]), ]
hull = rbind(grp_fished, grp_DFMPA)
nmds_plot = ggplot() + geom_polygon(data = hull, aes(x = NMDS1, y = NMDS2, shape = Site), alpha = 0.30) + geom_point(data = scores, aes(x = NMDS1, y = NMDS2, shape = Site)) + theme_bw() + opts(panel.grid.major = theme_blank()) + opts(panel.grid.minor = theme_blank())

#########################################################################################
#########################################################################################
#########################################################################################

