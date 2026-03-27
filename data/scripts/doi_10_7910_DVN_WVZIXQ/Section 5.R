# Title -------------------------------------------------------------------

# Replication script for "How to distinguish human error from election fraud: Evidence from the 2019 Malawi election"   
# Authors Johan Ahlback and Ryan Jablonski               


# Description -------------------------------------------------------------

# Use this script to replicate tables and figures presented in section 5 and the online appendix

# Section 5 includes the following analyses:

# How edits are related to candidate votes, presented in figures 3, S9, S10 and S12 and in tables S8, S9, and S21 
# how edits are related to differences in results recorded by MEC and NICE, presented in figure 4, S11, S13 and S14 and in tables S4, S10 and S11
# how edits are related to differences in candidate votes between the 2019 and 2020 elections, presented in figure S5
# How edits are related to candidate votes at polling stream level analysis presented in figures S6, S7 and S8 and in table S7


# Prerequisites -----------------------------------------------------------
# run codes in data_preparation.R prior to running the codes below

################################################################################


# Figure 3: Tallying irregularities and candidate votes -------------------
# This code generates the regression models presented in figure 3

# Regression models (equation 1)
out.m1 <- felm(mec_mutharika ~ pres_alt + registered | 0 | 0 | constituency, data=edits)
out.m2 <- felm(mec_mutharika ~ pres_alt + registered | constituency | 0 | constituency, data=edits)
out.m3 <- felm(mec_mutharika ~ cand_alt + registered | 0 | 0 | constituency, data=edits)
out.m4 <- felm(mec_mutharika ~ cand_alt + registered | constituency | 0 | constituency, data=edits)

out.c1 <- felm(mec_chakwera ~ pres_alt + registered | 0 | 0 | constituency, data=edits)
out.c2 <- felm(mec_chakwera ~ pres_alt + registered | constituency | 0 | constituency, data=edits)
out.c3 <- felm(mec_chakwera ~ cand_alt + registered | 0 | 0 | constituency, data=edits)
out.c4 <- felm(mec_chakwera ~ cand_alt + registered | constituency | 0 | constituency, data=edits)

out.cc1 <- felm(mec_chilima ~ pres_alt + registered | 0 | 0 | constituency, data=edits)
out.cc2 <- felm(mec_chilima ~ pres_alt + registered | constituency | 0 | constituency, data=edits)
out.cc3 <- felm(mec_chilima ~ cand_alt + registered | 0 | 0 | constituency, data=edits)
out.cc4 <- felm(mec_chilima ~ cand_alt + registered | constituency | 0 | constituency, data=edits)

# Result-sheet with any edits
pdata1x <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.m1$coefficients[2,1], out.c1$coefficients[2,1], out.cc1$coefficients[2,1])
)

pdata1x$conf.low <- c(pdata1x$estimate - qnorm(0.975)*
                        c(summary(out.m1)$coefficients[2,2], summary(out.c1)$coefficients[2,2], summary(out.cc1)$coefficients[2,2]))
pdata1x$conf.high <- c(pdata1x$estimate + qnorm(0.975)*
                         c(summary(out.m1)$coefficients[2,2], summary(out.c1)$coefficients[2,2], summary(out.cc1)$coefficients[2,2]))
pdata1x$model <- "Result-sheet with any edits"

# Result-sheet with any edits (fixed effects)
pdata2x <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.m2$coefficients[1,1], out.c2$coefficients[1,1], out.cc2$coefficients[1,1])
)

pdata2x$conf.low <- c(pdata2x$estimate - qnorm(0.975)*
                        c(summary(out.m2)$coefficients[1,2], summary(out.c2)$coefficients[1,2], summary(out.cc2)$coefficients[1,2]))
pdata2x$conf.high <- c(pdata2x$estimate + qnorm(0.975)*
                         c(summary(out.m2)$coefficients[1,2], summary(out.c2)$coefficients[1,2], summary(out.cc2)$coefficients[1,2]))
pdata2x$model <- "Result-sheet with any edits (fixed effects)"

# Edits in candidate rows
pdata3x <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.m3$coefficients[2,1], out.c3$coefficients[2,1], out.cc3$coefficients[2,1])
)

pdata3x$conf.low <- c(pdata3x$estimate - qnorm(0.975)*
                        c(summary(out.m3)$coefficients[2,2], summary(out.c3)$coefficients[2,2], summary(out.cc3)$coefficients[2,2]))
pdata3x$conf.high <- c(pdata3x$estimate + qnorm(0.975)*
                         c(summary(out.m3)$coefficients[2,2], summary(out.c3)$coefficients[2,2], summary(out.cc3)$coefficients[2,2]))
pdata3x$model <- "Edits in candidate rows"

# Edits in candidate rows (fixed effects)
pdata4x <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.m4$coefficients[1,1], out.c4$coefficients[1,1], out.cc4$coefficients[1,1])
)

pdata4x$conf.low <- c(pdata4x$estimate - qnorm(0.975)*
                        c(summary(out.m4)$coefficients[1,2], summary(out.c4)$coefficients[1,2], summary(out.cc4)$coefficients[1,2]))
pdata4x$conf.high <- c(pdata4x$estimate + qnorm(0.975)*
                         c(summary(out.m4)$coefficients[1,2], summary(out.c4)$coefficients[1,2], summary(out.cc4)$coefficients[1,2]))
pdata4x$model <- "Edits in candidate rows (fixed effects)"

pdata.all.x <- rbind(pdata1x, pdata2x, pdata3x, pdata4x)
pdata.all.x[order(pdata.all.x$model, decreasing = F),]

figure3 <-dwplot(pdata.all.x, 
                 dot_args = list(size = 1.5),
                 whisker_args = list(size = 0.75),
                 vline = geom_vline(xintercept=0, colour="black", linetype=2, linewidth=0.5))+
  theme_bw()+xlab("Coefficient (95% confindence)") +
  scale_colour_discrete(name = "Model")+
  #  coord_cartesian(xlim=c(-1,1)) +
  theme(text = element_text(size=12),  
        #        legend.position = c(0.007, 0.01),
        legend.justification = c(0, 0)) 

figure3



# Table S8: Result-sheet edits and candidate votes ------------------------
# This code generates table S8 examining the relation between any-edits and candidate votes

stargazer(out.m1, out.m2, out.c1, out.c2, out.cc1, out.cc2, 
          covariate.labels = c("Result-sheet with any edit", "Registered voters at ps"), 
          dep.var.labels = c("Mutharika votes", "Chakwera votes", "Chilima votes"), 
          title = "Result-sheet edits and candidate votes") 



# Table S9: Result-sheet edits in candidate rows and candidate vot --------
# This code generates table S9 examining the relation between edits in candidate rows and candidate votes

stargazer(out.m3, out.m4, out.c3, out.c4, out.cc3, out.cc4, 
          covariate.labels = c("Edits in candidate rows", "Registered voters at ps"), 
          dep.var.labels = c("Mutharika votes", "Chakwera votes", "Chilima votes"), 
          title = "Result-sheet edits in candidate rows and candidate votes") 



# Figure S9: Fixed effects comparison -------------------------------------
# This codes generates figure S9 comparing fixed effects

# ward FE
out.mutharikia1 <- felm(mec_mutharika ~ pres_alt + registered | ward_code | 0 | constituency, data=edits)
out.mutharikia2 <- felm(mec_mutharika ~ cand_alt + registered | ward_code | 0 | constituency, data=edits)

out.chakwera1 <- felm(mec_chakwera ~ pres_alt + registered | ward_code | 0 | constituency, data=edits)
out.chakwera2 <- felm(mec_chakwera ~ cand_alt + registered | ward_code | 0 | constituency, data=edits)

out.chilima1 <- felm(mec_chilima ~ pres_alt + registered | ward_code | 0 | constituency, data=edits)
out.chilima2 <- felm(mec_chilima ~ cand_alt + registered | ward_code | 0 | constituency, data=edits)


# Any edits (ward FE)
pdata1x <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1$coefficients[1,1], out.chakwera1$coefficients[1,1], out.chilima1$coefficients[1,1])
)

pdata1x$conf.low <- c(pdata1x$estimate - qnorm(0.975)*
                        c(summary(out.mutharikia1)$coefficients[1,2], summary(out.chakwera1)$coefficients[1,2], summary(out.chilima1)$coefficients[1,2]))
pdata1x$conf.high <- c(pdata1x$estimate + qnorm(0.975)*
                         c(summary(out.mutharikia1)$coefficients[1,2], summary(out.chakwera1)$coefficients[1,2], summary(out.chilima1)$coefficients[1,2]))
pdata1x$model <- "Any edits (ward FE)"

# Candidate rows (ward FE)
pdata2x <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2$coefficients[1,1], out.chakwera2$coefficients[1,1], out.chilima2$coefficients[1,1])
)

pdata2x$conf.low <- c(pdata2x$estimate - qnorm(0.975)*
                        c(summary(out.mutharikia2)$coefficients[1,2], summary(out.chakwera2)$coefficients[1,2], summary(out.chilima2)$coefficients[1,2]))
pdata2x$conf.high <- c(pdata2x$estimate + qnorm(0.975)*
                         c(summary(out.mutharikia2)$coefficients[1,2], summary(out.chakwera2)$coefficients[1,2], summary(out.chilima2)$coefficients[1,2]))
pdata2x$model <- "Candidate rows (ward FE)"



# constituency FE
out.mutharikia1 <- felm(mec_mutharika ~ pres_alt + registered | constituency | 0 | constituency, data=edits)
out.mutharikia2 <- felm(mec_mutharika ~ cand_alt + registered | constituency | 0 | constituency, data=edits)

out.chakwera1 <- felm(mec_chakwera ~ pres_alt + registered | constituency | 0 | constituency, data=edits)
out.chakwera2 <- felm(mec_chakwera ~ cand_alt + registered | constituency | 0 | constituency, data=edits)

out.chilima1 <- felm(mec_chilima ~ pres_alt + registered | constituency | 0 | constituency, data=edits)
out.chilima2 <- felm(mec_chilima ~ cand_alt + registered | constituency | 0 | constituency, data=edits)


# Any edits (constituency FE)
pdata3x <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1$coefficients[1,1], out.chakwera1$coefficients[1,1], out.chilima1$coefficients[1,1])
)

pdata3x$conf.low <- c(pdata3x$estimate - qnorm(0.975)*
                        c(summary(out.mutharikia1)$coefficients[1,2], summary(out.chakwera1)$coefficients[1,2], summary(out.chilima1)$coefficients[1,2]))
pdata3x$conf.high <- c(pdata3x$estimate + qnorm(0.975)*
                         c(summary(out.mutharikia1)$coefficients[1,2], summary(out.chakwera1)$coefficients[1,2], summary(out.chilima1)$coefficients[1,2]))
pdata3x$model <- "Any edits (constituency FE)"

# Candidate rows (constituency FE)
pdata4x <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2$coefficients[1,1], out.chakwera2$coefficients[1,1], out.chilima2$coefficients[1,1])
)

pdata4x$conf.low <- c(pdata4x$estimate - qnorm(0.975)*
                        c(summary(out.mutharikia2)$coefficients[1,2], summary(out.chakwera2)$coefficients[1,2], summary(out.chilima2)$coefficients[1,2]))
pdata4x$conf.high <- c(pdata4x$estimate + qnorm(0.975)*
                         c(summary(out.mutharikia2)$coefficients[1,2], summary(out.chakwera2)$coefficients[1,2], summary(out.chilima2)$coefficients[1,2]))
pdata4x$model <- "Candidate rows (constituency FE)"



pdata.all.x <- rbind(pdata1x, pdata2x, pdata3x, pdata4x)
pdata.all.x[order(pdata.all.x$model, decreasing = F),]

figure.s9 <-dwplot(pdata.all.x, 
                  dot_args = list(size = 1.5),
                  whisker_args = list(size = 0.75),
                  vline = geom_vline(xintercept=0, colour="black", linetype=2, linewidth=0.5))+
  theme_bw()+xlab("Coefficient (95% confindence)") +
  scale_colour_discrete(name = "Model")+
  #  coord_cartesian(xlim=c(-1,1)) +
  theme(text = element_text(size=12),  
        #        legend.position = c(0.007, 0.01),
        legend.justification = c(0, 0)) 

figure.s9



# Figure S10: Result-sheet edits and candidate votes by party --------------
# This codes generates figure S10 comparing result-sheet edits and candidate votes in different areas depending on party dominance

# DPP Area
out.mutharikia1.dpp <- felm(mec_mutharika ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="DPP",])
out.mutharikia2.dpp <- felm(mec_mutharika ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="DPP",])

out.chakwera1.dpp <- felm(mec_chakwera ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="DPP",])
out.chakwera2.dpp <- felm(mec_chakwera ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="DPP",])

out.chilima1.dpp <- felm(mec_chilima ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="DPP",])
out.chilima2.dpp <- felm(mec_chilima ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="DPP",])


# MCP Area
out.mutharikia1.mcp <- felm(mec_mutharika ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="MCP",])
out.mutharikia2.mcp <- felm(mec_mutharika ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="MCP",])

out.chakwera1.mcp <- felm(mec_chakwera ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="MCP",])
out.chakwera2.mcp <- felm(mec_chakwera ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="MCP",])

out.chilima1.mcp <- felm(mec_chilima ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="MCP",])
out.chilima2.mcp <- felm(mec_chilima ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="MCP",])


# PP Area
out.mutharikia1.pp <- felm(mec_mutharika ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="PP",])
out.mutharikia2.pp <- felm(mec_mutharika ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="PP",])

out.chakwera1.pp <- felm(mec_chakwera ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="PP",])
out.chakwera2.pp <- felm(mec_chakwera ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="PP",])

out.chilima1.pp <- felm(mec_chilima ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="PP",])
out.chilima2.pp <- felm(mec_chilima ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$district_2014_winner=="PP",])


# Any edits (DPP districts)
pdata.dpp1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.dpp$coefficients[1,1], out.chakwera1.dpp$coefficients[1,1], out.chilima1.dpp$coefficients[1,1])
)

pdata.dpp1$conf.low <- c(pdata.dpp1$estimate - qnorm(0.975)*
                           c(summary(out.mutharikia1.dpp)$coefficients[1,2], summary(out.chakwera1.dpp)$coefficients[1,2], summary(out.chilima1.dpp)$coefficients[1,2]))
pdata.dpp1$conf.high <- c(pdata.dpp1$estimate + qnorm(0.975)*
                            c(summary(out.mutharikia1.dpp)$coefficients[1,2], summary(out.chakwera1.dpp)$coefficients[1,2], summary(out.chilima1.dpp)$coefficients[1,2]))
pdata.dpp1$model <- "Any Edits (DPP districts)"

# Candidate rows (DPP districts)
pdata.dpp2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.dpp$coefficients[1,1], out.chakwera2.dpp$coefficients[1,1], out.chilima2.dpp$coefficients[1,1])
)

pdata.dpp2$conf.low <- c(pdata.dpp2$estimate - qnorm(0.975)*
                           c(summary(out.mutharikia2.dpp)$coefficients[1,2], summary(out.chakwera2.dpp)$coefficients[1,2], summary(out.chilima2.dpp)$coefficients[1,2]))
pdata.dpp2$conf.high <- c(pdata.dpp2$estimate + qnorm(0.975)*
                            c(summary(out.mutharikia2.dpp)$coefficients[1,2], summary(out.chakwera2.dpp)$coefficients[1,2], summary(out.chilima2.dpp)$coefficients[1,2]))
pdata.dpp2$model <- "Candidate rows (DPP districts)"


# Any Edits (MCP districts)
pdata.mcp1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.mcp$coefficients[1,1], out.chakwera1.mcp$coefficients[1,1], out.chilima1.mcp$coefficients[1,1])
)

pdata.mcp1$conf.low <- c(pdata.mcp1$estimate - qnorm(0.975)*
                           c(summary(out.mutharikia1.mcp)$coefficients[1,2], summary(out.chakwera1.mcp)$coefficients[1,2], summary(out.chilima1.mcp)$coefficients[1,2]))
pdata.mcp1$conf.high <- c(pdata.mcp1$estimate + qnorm(0.975)*
                            c(summary(out.mutharikia1.mcp)$coefficients[1,2], summary(out.chakwera1.mcp)$coefficients[1,2], summary(out.chilima1.mcp)$coefficients[1,2]))
pdata.mcp1$model <- "Any Edits (MCP districts)"

# Candidate rows (MCP districts)
pdata.mcp2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.mcp$coefficients[1,1], out.chakwera2.mcp$coefficients[1,1], out.chilima2.mcp$coefficients[1,1])
)

pdata.mcp2$conf.low <- c(pdata.mcp2$estimate - qnorm(0.975)*
                           c(summary(out.mutharikia2.mcp)$coefficients[1,2], summary(out.chakwera2.mcp)$coefficients[1,2], summary(out.chilima2.mcp)$coefficients[1,2]))
pdata.mcp2$conf.high <- c(pdata.mcp2$estimate + qnorm(0.975)*
                            c(summary(out.mutharikia2.mcp)$coefficients[1,2], summary(out.chakwera2.mcp)$coefficients[1,2], summary(out.chilima2.mcp)$coefficients[1,2]))
pdata.mcp2$model <- "Candidate rows (MCP districts)"


# Any Edits (PP districts)
pdata.pp1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.pp$coefficients[1,1], out.chakwera1.pp$coefficients[1,1], out.chilima1.pp$coefficients[1,1])
)

pdata.pp1$conf.low <- c(pdata.pp1$estimate - qnorm(0.975)*
                          c(summary(out.mutharikia1.pp)$coefficients[1,2], summary(out.chakwera1.pp)$coefficients[1,2], summary(out.chilima1.pp)$coefficients[1,2]))
pdata.pp1$conf.high <- c(pdata.pp1$estimate + qnorm(0.975)*
                           c(summary(out.mutharikia1.pp)$coefficients[1,2], summary(out.chakwera1.pp)$coefficients[1,2], summary(out.chilima1.pp)$coefficients[1,2]))
pdata.pp1$model <- "Any Edits (PP districts)"

# Candidate rows (PP districts)
pdata.pp2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.pp$coefficients[1,1], out.chakwera2.pp$coefficients[1,1], out.chilima2.pp$coefficients[1,1])
)

pdata.pp2$conf.low <- c(pdata.pp2$estimate - qnorm(0.975)*
                          c(summary(out.mutharikia2.pp)$coefficients[1,2], summary(out.chakwera2.pp)$coefficients[1,2], summary(out.chilima2.pp)$coefficients[1,2]))
pdata.pp2$conf.high <- c(pdata.pp2$estimate + qnorm(0.975)*
                           c(summary(out.mutharikia2.pp)$coefficients[1,2], summary(out.chakwera2.pp)$coefficients[1,2], summary(out.chilima2.pp)$coefficients[1,2]))
pdata.pp2$model <- "Candidate rows (PP districts)"



pdata.all.x <- rbind(pdata.dpp1, pdata.dpp2, pdata.mcp1, pdata.mcp2, pdata.pp1, pdata.pp2)
pdata.all.x[order(pdata.all.x$model, decreasing = F),]

figure.s10 <-dwplot(pdata.all.x, 
                       dot_args = list(size = 1.5),
                       whisker_args = list(size = 0.75),
                       vline = geom_vline(xintercept=0, colour="black", linetype=2, linewidth=0.5))+
  theme_bw()+xlab("Coefficient (95% confindence)") +
  scale_colour_discrete(name = "Model")+
  #  coord_cartesian(xlim=c(-1,1)) +
  theme(text = element_text(size=12),  
        #        legend.position = c(0.007, 0.01),
        legend.justification = c(0, 0)) 

figure.s10


# Figure S12: Result-sheet edits and candidate votes by ethnic gro --------
# This codes generates figure S12 comparing result-sheet edits and candidate votes in different areas depending on majority ethnic group

# Chewa
out.mutharikia1.chewa <- felm(mec_mutharika ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Chewa",])
out.mutharikia2.chewa <- felm(mec_mutharika ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Chewa",])
out.chakwera1.chewa <- felm(mec_chakwera ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Chewa",])
out.chakwera2.chewa <- felm(mec_chakwera ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Chewa",])
out.chilima1.chewa <- felm(mec_chilima ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Chewa",])
out.chilima2.chewa <- felm(mec_chilima ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Chewa",])

# Lomwe
out.mutharikia1.lomwe <- felm(mec_mutharika ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Lomwe",])
out.mutharikia2.lomwe <- felm(mec_mutharika ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Lomwe",])
out.chakwera1.lomwe <- felm(mec_chakwera ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Lomwe",])
out.chakwera2.lomwe <- felm(mec_chakwera ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Lomwe",])
out.chilima1.lomwe <- felm(mec_chilima ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Lomwe",])
out.chilima2.lomwe <- felm(mec_chilima ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Lomwe",])


# Ngoni
out.mutharikia1.ngoni <- felm(mec_mutharika ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Ngoni",])
out.mutharikia2.ngoni <- felm(mec_mutharika ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Ngoni",])
out.chakwera1.ngoni <- felm(mec_chakwera ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Ngoni",])
out.chakwera2.ngoni <- felm(mec_chakwera ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Ngoni",])
out.chilima1.ngoni <- felm(mec_chilima ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Ngoni",])
out.chilima2.ngoni <- felm(mec_chilima ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Ngoni",])


# Tumbuka
out.mutharikia1.tumbuka <- felm(mec_mutharika ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Tumbuka",])
out.mutharikia2.tumbuka <- felm(mec_mutharika ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Tumbuka",])
out.chakwera1.tumbuka <- felm(mec_chakwera ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Tumbuka",])
out.chakwera2.tumbuka <- felm(mec_chakwera ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Tumbuka",])
out.chilima1.tumbuka <- felm(mec_chilima ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Tumbuka",])
out.chilima2.tumbuka <- felm(mec_chilima ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Tumbuka",])

# Any Edits (Chewa districts)
pdata.chewa1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.chewa$coefficients[1,1], out.chakwera1.chewa$coefficients[1,1], out.chilima1.chewa$coefficients[1,1])
)

pdata.chewa1$conf.low <- c(pdata.chewa1$estimate - qnorm(0.975)*
                             c(summary(out.mutharikia1.chewa)$coefficients[1,2], summary(out.chakwera1.chewa)$coefficients[1,2], summary(out.chilima1.chewa)$coefficients[1,2]))
pdata.chewa1$conf.high <- c(pdata.chewa1$estimate + qnorm(0.975)*
                              c(summary(out.mutharikia1.chewa)$coefficients[1,2], summary(out.chakwera1.chewa)$coefficients[1,2], summary(out.chilima1.chewa)$coefficients[1,2]))
pdata.chewa1$model <- "Any Edits (Chewa districts)"


# Candidate rows (Chewa districts)
pdata.chewa2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.chewa$coefficients[1,1], out.chakwera2.chewa$coefficients[1,1], out.chilima2.chewa$coefficients[1,1])
)

pdata.chewa2$conf.low <- c(pdata.chewa2$estimate - qnorm(0.975)*
                             c(summary(out.mutharikia2.chewa)$coefficients[1,2], summary(out.chakwera2.chewa)$coefficients[1,2], summary(out.chilima2.chewa)$coefficients[1,2]))
pdata.chewa2$conf.high <- c(pdata.chewa2$estimate + qnorm(0.975)*
                              c(summary(out.mutharikia2.chewa)$coefficients[1,2], summary(out.chakwera2.chewa)$coefficients[1,2], summary(out.chilima2.chewa)$coefficients[1,2]))
pdata.chewa2$model <- "Candidate rows (Chewa districts)"



# Any Edits (Lomwe districts)
pdata.lomwe1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.lomwe$coefficients[1,1], out.chakwera1.lomwe$coefficients[1,1], out.chilima1.lomwe$coefficients[1,1])
)

pdata.lomwe1$conf.low <- c(pdata.lomwe1$estimate - qnorm(0.975)*
                             c(summary(out.mutharikia1.lomwe)$coefficients[1,2], summary(out.chakwera1.lomwe)$coefficients[1,2], summary(out.chilima1.lomwe)$coefficients[1,2]))
pdata.lomwe1$conf.high <- c(pdata.lomwe1$estimate + qnorm(0.975)*
                              c(summary(out.mutharikia1.lomwe)$coefficients[1,2], summary(out.chakwera1.lomwe)$coefficients[1,2], summary(out.chilima1.lomwe)$coefficients[1,2]))
pdata.lomwe1$model <- "Any Edits (Lomwe districts)"


# Candidate rows (Lomwe districts)
pdata.lomwe2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.lomwe$coefficients[1,1], out.chakwera2.lomwe$coefficients[1,1], out.chilima2.lomwe$coefficients[1,1])
)

pdata.lomwe2$conf.low <- c(pdata.lomwe2$estimate - qnorm(0.975)*
                             c(summary(out.mutharikia2.lomwe)$coefficients[1,2], summary(out.chakwera2.lomwe)$coefficients[1,2], summary(out.chilima2.lomwe)$coefficients[1,2]))
pdata.lomwe2$conf.high <- c(pdata.lomwe2$estimate + qnorm(0.975)*
                              c(summary(out.mutharikia2.lomwe)$coefficients[1,2], summary(out.chakwera2.lomwe)$coefficients[1,2], summary(out.chilima2.lomwe)$coefficients[1,2]))
pdata.lomwe2$model <- "Candidate rows (Lomwe districts)"


# Any Edits (Ngoni districts)
pdata.ngoni1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.ngoni$coefficients[1,1], out.chakwera1.ngoni$coefficients[1,1], out.chilima1.ngoni$coefficients[1,1])
)

pdata.ngoni1$conf.low <- c(pdata.ngoni1$estimate - qnorm(0.975)*
                             c(summary(out.mutharikia1.ngoni)$coefficients[1,2], summary(out.chakwera1.ngoni)$coefficients[1,2], summary(out.chilima1.ngoni)$coefficients[1,2]))
pdata.ngoni1$conf.high <- c(pdata.ngoni1$estimate + qnorm(0.975)*
                              c(summary(out.mutharikia1.ngoni)$coefficients[1,2], summary(out.chakwera1.ngoni)$coefficients[1,2], summary(out.chilima1.ngoni)$coefficients[1,2]))
pdata.ngoni1$model <- "Any Edits (Ngoni districts)"


# Candidate rows (Ngoni districts)
pdata.ngoni2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.ngoni$coefficients[1,1], out.chakwera2.ngoni$coefficients[1,1], out.chilima2.ngoni$coefficients[1,1])
)

pdata.ngoni2$conf.low <- c(pdata.ngoni2$estimate - qnorm(0.975)*
                             c(summary(out.mutharikia2.ngoni)$coefficients[1,2], summary(out.chakwera2.ngoni)$coefficients[1,2], summary(out.chilima2.ngoni)$coefficients[1,2]))
pdata.ngoni2$conf.high <- c(pdata.ngoni2$estimate + qnorm(0.975)*
                              c(summary(out.mutharikia2.ngoni)$coefficients[1,2], summary(out.chakwera2.ngoni)$coefficients[1,2], summary(out.chilima2.ngoni)$coefficients[1,2]))
pdata.ngoni2$model <- "Candidate rows (Ngoni districts)"


# Any Edits (Tumbuka districts)
pdata.tumbuka1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.tumbuka$coefficients[1,1], out.chakwera1.tumbuka$coefficients[1,1], out.chilima1.tumbuka$coefficients[1,1])
)

pdata.tumbuka1$conf.low <- c(pdata.tumbuka1$estimate - qnorm(0.975)*
                               c(summary(out.mutharikia1.tumbuka)$coefficients[1,2], summary(out.chakwera1.tumbuka)$coefficients[1,2], summary(out.chilima1.tumbuka)$coefficients[1,2]))
pdata.tumbuka1$conf.high <- c(pdata.tumbuka1$estimate + qnorm(0.975)*
                                c(summary(out.mutharikia1.tumbuka)$coefficients[1,2], summary(out.chakwera1.tumbuka)$coefficients[1,2], summary(out.chilima1.tumbuka)$coefficients[1,2]))
pdata.tumbuka1$model <- "Any Edits (Tumbuka districts)"


# Candidate rows (Tumbuka districts)
pdata.tumbuka2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.tumbuka$coefficients[1,1], out.chakwera2.tumbuka$coefficients[1,1], out.chilima2.tumbuka$coefficients[1,1])
)

pdata.tumbuka2$conf.low <- c(pdata.tumbuka2$estimate - qnorm(0.975)*
                               c(summary(out.mutharikia2.tumbuka)$coefficients[1,2], summary(out.chakwera2.tumbuka)$coefficients[1,2], summary(out.chilima2.tumbuka)$coefficients[1,2]))
pdata.tumbuka2$conf.high <- c(pdata.tumbuka2$estimate + qnorm(0.975)*
                                c(summary(out.mutharikia2.tumbuka)$coefficients[1,2], summary(out.chakwera2.tumbuka)$coefficients[1,2], summary(out.chilima2.tumbuka)$coefficients[1,2]))
pdata.tumbuka2$model <- "Candidate rows (Tumbuka districts)"



pdata.all.x <- rbind(pdata.chewa1, pdata.chewa2, pdata.lomwe1, pdata.lomwe2, pdata.ngoni1, pdata.ngoni2,  pdata.tumbuka1, pdata.tumbuka2)
pdata.all.x[order(pdata.all.x$model, decreasing = F),]

figure.s12 <-dwplot(pdata.all.x, 
                     dot_args = list(size = 1.5),
                     whisker_args = list(size = 0.75),
                     vline = geom_vline(xintercept=0, colour="black", linetype=2, linewidth=0.5))+
  theme_bw()+xlab("Coefficient (95% confindence)") +
  scale_colour_discrete(name = "Model")+
  #  coord_cartesian(xlim=c(-1,1)) +
  theme(text = element_text(size=12),  
        #        legend.position = c(0.007, 0.01),
        legend.justification = c(0, 0)) 

figure.s12


# Correct NICE figures ----------------------------------------------------
# In some cases the NICE data showed implausible figures with candidate votes exceeding the number of registered voters
# We remove these for the main analyses, but include them in robustness tests (see footnote 16 in main text)

edits$nice_error <- 0
edits$nice_error[(edits$nice_chakwera +edits$nice_chilima + edits$nice_chisi + edits$nice_kaliya + edits$nice_kuwani + edits$nice_muluzi + edits$nice_mutharika)>edits$registered] <- 1

table(edits$nice_error)
edit_data1e <- subset(edits, edits$nice_error==0)


# Table S4: Descriptive statistics: Post-aggregation - pre-aggrega --------
# This code generates table S4 showing the differences in raw votes between MEC and NICE vote figures

mydata <- data.frame(edit_data1e$mutharika_difference, edit_data1e$chakwera_difference, edit_data1e$chilima_difference)
names(mydata) <- c("mutharika_difference", "chakwera_difference", "chilima_difference")
stargazer(mydata, type = "latex", title="Descriptive statistics: Post-aggregation - pre-aggregation votes", digits=1, 
          covariate.labels = c("Differences in Mutharika votes", "Differences in Chakwera votes", "Differences in Chilima votes"))


# Figure 4: Tallying irregularities and changes in candidate votes --------
# This code generates figure 4 comparing edits and changes in candidate votes during aggregation

# Regression models (equation 2)
out.m1d <- felm(mutharika_difference ~ pres_alt + registered | 0 | 0 | constituency, data=edit_data1e)
out.m2d <- felm(mutharika_difference ~ pres_alt + registered | constituency | 0 | 0, data=edit_data1e)
out.m3d <- felm(mutharika_difference ~ cand_alt + registered | 0 | 0 | constituency, data=edit_data1e)
out.m4d <- felm(mutharika_difference ~ cand_alt + registered | constituency | 0 | 0, data=edit_data1e)

out.c1d <- felm(chakwera_difference ~ pres_alt + registered | 0 | 0 | constituency, data=edit_data1e)
out.c2d <- felm(chakwera_difference ~ pres_alt + registered | constituency | 0 | 0, data=edit_data1e)
out.c3d <- felm(chakwera_difference ~ cand_alt + registered | 0 | 0 | constituency, data=edit_data1e)
out.c4d <- felm(chakwera_difference ~ cand_alt + registered | constituency | 0 | 0, data=edit_data1e)

out.cc1d <- felm(chilima_difference ~ pres_alt + registered | 0 | 0 | constituency, data=edit_data1e)
out.cc2d <- felm(chilima_difference ~ pres_alt + registered | constituency | 0 | 0, data=edit_data1e)
out.cc3d <- felm(chilima_difference ~ cand_alt + registered | 0 | 0 | constituency, data=edit_data1e)
out.cc4d <- felm(chilima_difference ~ cand_alt + registered | constituency | 0 | 0, data=edit_data1e)

# Result-sheet with any edits
pdata1xd <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.m1d$coefficients[2,1], out.c1d$coefficients[2,1], out.cc1d$coefficients[2,1])
)

pdata1xd$conf.low <- c(pdata1xd$estimate - qnorm(0.975)*
                         c(summary(out.m1d)$coefficients[2,2], summary(out.c1d)$coefficients[2,2], summary(out.cc1d)$coefficients[2,2]))
pdata1xd$conf.high <- c(pdata1xd$estimate + qnorm(0.975)*
                          c(summary(out.m1d)$coefficients[2,2], summary(out.c1d)$coefficients[2,2], summary(out.cc1d)$coefficients[2,2]))
pdata1xd$model <- "Result-sheet with any edits"

# Result-sheet with any edits (fixed effects)
pdata2xd <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.m2d$coefficients[1,1], out.c2d$coefficients[1,1], out.cc2d$coefficients[1,1])
)
pdata2xd$conf.low <- c(pdata2xd$estimate - qnorm(0.975)*
                         c(summary(out.m2d)$coefficients[1,2], summary(out.c2d)$coefficients[1,2], summary(out.cc2d)$coefficients[1,2]))
pdata2xd$conf.high <- c(pdata2xd$estimate + qnorm(0.975)*
                          c(summary(out.m2d)$coefficients[1,2], summary(out.c2d)$coefficients[1,2], summary(out.cc2d)$coefficients[1,2]))
pdata2xd$model <- "Result-sheet with any edits (fixed effects)"

# Edits in candidate rows
pdata3xd <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.m3d$coefficients[2,1], out.c3d$coefficients[2,1], out.cc3d$coefficients[2,1])
)
pdata3xd$conf.low <- c(pdata3xd$estimate - qnorm(0.975)*
                         c(summary(out.m3d)$coefficients[2,2], summary(out.c3d)$coefficients[2,2], summary(out.cc3d)$coefficients[2,2]))
pdata3xd$conf.high <- c(pdata3xd$estimate + qnorm(0.975)*
                          c(summary(out.m3d)$coefficients[2,2], summary(out.c3d)$coefficients[2,2], summary(out.cc3d)$coefficients[2,2]))
pdata3xd$model <- "Edits in candidate rows"

# Edits in candidate rows (fixed effects)
pdata4xd <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.m4d$coefficients[1,1], out.c4d$coefficients[1,1], out.cc4d$coefficients[1,1])
)
pdata4xd$conf.low <- c(pdata4xd$estimate - qnorm(0.975)*
                         c(summary(out.m4d)$coefficients[1,2], summary(out.c4d)$coefficients[1,2], summary(out.cc4d)$coefficients[1,2]))
pdata4xd$conf.high <- c(pdata4xd$estimate + qnorm(0.975)*
                          c(summary(out.m4d)$coefficients[1,2], summary(out.c4d)$coefficients[1,2], summary(out.cc4d)$coefficients[1,2]))
pdata4xd$model <- "Edits in candidate rows (fixed effects)"

pdata.all.xd <- rbind(pdata1xd, pdata2xd, pdata3xd, pdata4xd)
pdata.all.xd[order(pdata.all.xd$model, decreasing = F),]

figure4 <-dwplot(pdata.all.xd, 
               dot_args = list(size = 1.5),
               whisker_args = list(size = 0.75),
               vline = geom_vline(xintercept=0, colour="black", linetype=2, linewidth=0.5))+
  theme_bw()+xlab("Coefficient (95% confindence)") +
  scale_colour_discrete(name = "Model")+
  coord_cartesian(xlim=c(-25,25)) +
  theme(text = element_text(size=12),  
        #        legend.position = c(0.007, 0.01),
        legend.justification = c(0, 0)) 

figure4



# Table S10: Result-sheet edits and changes in candidate votes dur --------
# This code generates table S10 comparing edits and changes in candidate votes

stargazer(out.m1d, out.m2d, out.c1d, out.c2d, out.cc1d, out.cc2d, 
          covariate.labels = c("Result-sheet with any edits", "Registered voters at ps"), 
          dep.var.labels = c("Mutharika votes", "Chakwera votes", "Chilima votes"), 
          title = "Result-sheet edits and changes in candidate votes during aggregation") 



# Table S11: Result-sheet edits in candidate rows and changes in c --------
# This code generates table S10 comparing edits in candidate rows and changes in candidate votes

stargazer(out.m3d, out.m4d, out.c3d, out.c4d, out.cc3d, out.cc4d, 
          covariate.labels = c("Edits in candidate rows", "Registered voters at ps"), 
          dep.var.labels = c("Mutharika votes", "Chakwera votes", "Chilima votes"), 
          title = "Result-sheet edits in candidate rows and changes in candidate votes during aggregation") 



# Figure S11: Result-sheet edits and candidate votes by party -------------
# This code generates figure S11 comparing edits and changes in candidates votes in different areas depending on party dominance

edit_data1e$district_2014_winner=edits[match(edit_data1e$district_code, edits$district_code), "district_2014_winner"]

# DPP Area
out.mutharikia1.dpp <- felm(mutharika_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="DPP",])
out.mutharikia2.dpp <- felm(mutharika_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="DPP",])

out.chakwera1.dpp <- felm(chakwera_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="DPP",])
out.chakwera2.dpp <- felm(chakwera_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="DPP",])

out.chilima1.dpp <- felm(chilima_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="DPP",])
out.chilima2.dpp <- felm(chilima_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="DPP",])


# MCP Area
out.mutharikia1.mcp <- felm(mutharika_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="MCP",])
out.mutharikia2.mcp <- felm(mutharika_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="MCP",])

out.chakwera1.mcp <- felm(chakwera_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="MCP",])
out.chakwera2.mcp <- felm(chakwera_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="MCP",])

out.chilima1.mcp <- felm(chilima_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="MCP",])
out.chilima2.mcp <- felm(chilima_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="MCP",])


# PP Area
out.mutharikia1.pp <- felm(mutharika_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="PP",])
out.mutharikia2.pp <- felm(mutharika_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="PP",])

out.chakwera1.pp <- felm(chakwera_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="PP",])
out.chakwera2.pp <- felm(chakwera_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="PP",])

out.chilima1.pp <- felm(chilima_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="PP",])
out.chilima2.pp <- felm(chilima_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edit_data1e[edit_data1e$district_2014_winner=="PP",])


# Any Edits (DPP districts)
pdata.dpp1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.dpp$coefficients[1,1], out.chakwera1.dpp$coefficients[1,1], out.chilima1.dpp$coefficients[1,1])
)

pdata.dpp1$conf.low <- c(pdata.dpp1$estimate - qnorm(0.975)*
                           c(summary(out.mutharikia1.dpp)$coefficients[1,2], summary(out.chakwera1.dpp)$coefficients[1,2], summary(out.chilima1.dpp)$coefficients[1,2]))
pdata.dpp1$conf.high <- c(pdata.dpp1$estimate + qnorm(0.975)*
                            c(summary(out.mutharikia1.dpp)$coefficients[1,2], summary(out.chakwera1.dpp)$coefficients[1,2], summary(out.chilima1.dpp)$coefficients[1,2]))
pdata.dpp1$model <- "Any Edits (DPP districts)"


# Candidate rows (DPP districts)
pdata.dpp2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.dpp$coefficients[1,1], out.chakwera2.dpp$coefficients[1,1], out.chilima2.dpp$coefficients[1,1])
)

pdata.dpp2$conf.low <- c(pdata.dpp2$estimate - qnorm(0.975)*
                           c(summary(out.mutharikia2.dpp)$coefficients[1,2], summary(out.chakwera2.dpp)$coefficients[1,2], summary(out.chilima2.dpp)$coefficients[1,2]))
pdata.dpp2$conf.high <- c(pdata.dpp2$estimate + qnorm(0.975)*
                            c(summary(out.mutharikia2.dpp)$coefficients[1,2], summary(out.chakwera2.dpp)$coefficients[1,2], summary(out.chilima2.dpp)$coefficients[1,2]))
pdata.dpp2$model <- "Candidate rows (DPP districts)"



# Any Edits (MCP districts)
pdata.mcp1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.mcp$coefficients[1,1], out.chakwera1.mcp$coefficients[1,1], out.chilima1.mcp$coefficients[1,1])
)

pdata.mcp1$conf.low <- c(pdata.mcp1$estimate - qnorm(0.975)*
                           c(summary(out.mutharikia1.mcp)$coefficients[1,2], summary(out.chakwera1.mcp)$coefficients[1,2], summary(out.chilima1.mcp)$coefficients[1,2]))
pdata.mcp1$conf.high <- c(pdata.mcp1$estimate + qnorm(0.975)*
                            c(summary(out.mutharikia1.mcp)$coefficients[1,2], summary(out.chakwera1.mcp)$coefficients[1,2], summary(out.chilima1.mcp)$coefficients[1,2]))
pdata.mcp1$model <- "Any Edits (MCP districts)"


# Candidate rows (MCP districts)
pdata.mcp2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.mcp$coefficients[1,1], out.chakwera2.mcp$coefficients[1,1], out.chilima2.mcp$coefficients[1,1])
)

pdata.mcp2$conf.low <- c(pdata.mcp2$estimate - qnorm(0.975)*
                           c(summary(out.mutharikia2.mcp)$coefficients[1,2], summary(out.chakwera2.mcp)$coefficients[1,2], summary(out.chilima2.mcp)$coefficients[1,2]))
pdata.mcp2$conf.high <- c(pdata.mcp2$estimate + qnorm(0.975)*
                            c(summary(out.mutharikia2.mcp)$coefficients[1,2], summary(out.chakwera2.mcp)$coefficients[1,2], summary(out.chilima2.mcp)$coefficients[1,2]))
pdata.mcp2$model <- "Candidate rows (MCP districts)"

# Any Edits (PP districts)
pdata.pp1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.pp$coefficients[1,1], out.chakwera1.pp$coefficients[1,1], out.chilima1.pp$coefficients[1,1])
)

pdata.pp1$conf.low <- c(pdata.pp1$estimate - qnorm(0.975)*
                          c(summary(out.mutharikia1.pp)$coefficients[1,2], summary(out.chakwera1.pp)$coefficients[1,2], summary(out.chilima1.pp)$coefficients[1,2]))
pdata.pp1$conf.high <- c(pdata.pp1$estimate + qnorm(0.975)*
                           c(summary(out.mutharikia1.pp)$coefficients[1,2], summary(out.chakwera1.pp)$coefficients[1,2], summary(out.chilima1.pp)$coefficients[1,2]))
pdata.pp1$model <- "Any Edits (PP districts)"


# Candidate rows (PP districts)
pdata.pp2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.pp$coefficients[1,1], out.chakwera2.pp$coefficients[1,1], out.chilima2.pp$coefficients[1,1])
)

pdata.pp2$conf.low <- c(pdata.pp2$estimate - qnorm(0.975)*
                          c(summary(out.mutharikia2.pp)$coefficients[1,2], summary(out.chakwera2.pp)$coefficients[1,2], summary(out.chilima2.pp)$coefficients[1,2]))
pdata.pp2$conf.high <- c(pdata.pp2$estimate + qnorm(0.975)*
                           c(summary(out.mutharikia2.pp)$coefficients[1,2], summary(out.chakwera2.pp)$coefficients[1,2], summary(out.chilima2.pp)$coefficients[1,2]))
pdata.pp2$model <- "Candidate rows (PP districts)"



pdata.all.x <- rbind(pdata.dpp1, pdata.dpp2, pdata.mcp1, pdata.mcp2, pdata.pp1, pdata.pp2)
pdata.all.x[order(pdata.all.x$model, decreasing = F),]


figure.S11 <-dwplot(pdata.all.x, 
                       dot_args = list(size = 1.5),
                       whisker_args = list(size = 0.75),
                       vline = geom_vline(xintercept=0, colour="black", linetype=2, linewidth=0.5))+
  theme_bw()+xlab("Coefficient (95% confindence)") +
  scale_colour_discrete(name = "Model")+
  #  coord_cartesian(xlim=c(-1,1)) +
  theme(text = element_text(size=12),  
        #        legend.position = c(0.007, 0.01),
        legend.justification = c(0, 0)) 

figure.S11



# Figure S13: Result-sheet edits and changes in candidate votes du --------
# This codes generates figure S13 comparing edits and changes candidate votes during aggregation in different areas depending on majority ethnic group

edit_data1e$largest_ethnic_group=edits[match(edit_data1e$district_code, edits$district_code), "largest_ethnic_group"]

# Chewa
out.mutharikia1.chewa <- felm(mutharika_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Chewa",])
out.mutharikia2.chewa <- felm(mutharika_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Chewa",])

out.chakwera1.chewa <- felm(chakwera_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Chewa",])
out.chakwera2.chewa <- felm(chakwera_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Chewa",])

out.chilima1.chewa <- felm(chilima_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Chewa",])
out.chilima2.chewa <- felm(chilima_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Chewa",])


# Lomwe
out.mutharikia1.lomwe <- felm(mutharika_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Lomwe",])
out.mutharikia2.lomwe <- felm(mutharika_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Lomwe",])

out.chakwera1.lomwe <- felm(chakwera_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Lomwe",])
out.chakwera2.lomwe <- felm(chakwera_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Lomwe",])

out.chilima1.lomwe <- felm(chilima_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Lomwe",])
out.chilima2.lomwe <- felm(chilima_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Lomwe",])


# Ngoni
out.mutharikia1.ngoni <- felm(mutharika_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Ngoni",])
out.mutharikia2.ngoni <- felm(mutharika_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Ngoni",])

out.chakwera1.ngoni <- felm(chakwera_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Ngoni",])
out.chakwera2.ngoni <- felm(chakwera_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Ngoni",])

out.chilima1.ngoni <- felm(chilima_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Ngoni",])
out.chilima2.ngoni <- felm(chilima_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Ngoni",])



# Tumbuka
out.mutharikia1.tumbuka <- felm(mutharika_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Tumbuka",])
out.mutharikia2.tumbuka <- felm(mutharika_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Tumbuka",])

out.chakwera1.tumbuka <- felm(chakwera_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Tumbuka",])
out.chakwera2.tumbuka <- felm(chakwera_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Tumbuka",])

out.chilima1.tumbuka <- felm(chilima_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Tumbuka",])
out.chilima2.tumbuka <- felm(chilima_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits[edits$largest_ethnic_group=="Tumbuka",])


# Any Edits (Chewa districts)
pdata.chewa1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.chewa$coefficients[1,1], out.chakwera1.chewa$coefficients[1,1], out.chilima1.chewa$coefficients[1,1])
)

pdata.chewa1$conf.low <- c(pdata.chewa1$estimate - qnorm(0.975)*
                             c(summary(out.mutharikia1.chewa)$coefficients[1,2], summary(out.chakwera1.chewa)$coefficients[1,2], summary(out.chilima1.chewa)$coefficients[1,2]))
pdata.chewa1$conf.high <- c(pdata.chewa1$estimate + qnorm(0.975)*
                              c(summary(out.mutharikia1.chewa)$coefficients[1,2], summary(out.chakwera1.chewa)$coefficients[1,2], summary(out.chilima1.chewa)$coefficients[1,2]))
pdata.chewa1$model <- "Any Edits (Chewa districts)"


# Candidate rows (Chewa districts)
pdata.chewa2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.chewa$coefficients[1,1], out.chakwera2.chewa$coefficients[1,1], out.chilima2.chewa$coefficients[1,1])
)

pdata.chewa2$conf.low <- c(pdata.chewa2$estimate - qnorm(0.975)*
                             c(summary(out.mutharikia2.chewa)$coefficients[1,2], summary(out.chakwera2.chewa)$coefficients[1,2], summary(out.chilima2.chewa)$coefficients[1,2]))
pdata.chewa2$conf.high <- c(pdata.chewa2$estimate + qnorm(0.975)*
                              c(summary(out.mutharikia2.chewa)$coefficients[1,2], summary(out.chakwera2.chewa)$coefficients[1,2], summary(out.chilima2.chewa)$coefficients[1,2]))
pdata.chewa2$model <- "Candidate rows (Chewa districts)"


# Any Edits (Lomwe districts)
pdata.lomwe1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.lomwe$coefficients[1,1], out.chakwera1.lomwe$coefficients[1,1], out.chilima1.lomwe$coefficients[1,1])
)

pdata.lomwe1$conf.low <- c(pdata.lomwe1$estimate - qnorm(0.975)*
                             c(summary(out.mutharikia1.lomwe)$coefficients[1,2], summary(out.chakwera1.lomwe)$coefficients[1,2], summary(out.chilima1.lomwe)$coefficients[1,2]))
pdata.lomwe1$conf.high <- c(pdata.lomwe1$estimate + qnorm(0.975)*
                              c(summary(out.mutharikia1.lomwe)$coefficients[1,2], summary(out.chakwera1.lomwe)$coefficients[1,2], summary(out.chilima1.lomwe)$coefficients[1,2]))
pdata.lomwe1$model <- "Any Edits (Lomwe districts)"


# Candidate rows (Lomwe districts)
pdata.lomwe2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.lomwe$coefficients[1,1], out.chakwera2.lomwe$coefficients[1,1], out.chilima2.lomwe$coefficients[1,1])
)

pdata.lomwe2$conf.low <- c(pdata.lomwe2$estimate - qnorm(0.975)*
                             c(summary(out.mutharikia2.lomwe)$coefficients[1,2], summary(out.chakwera2.lomwe)$coefficients[1,2], summary(out.chilima2.lomwe)$coefficients[1,2]))
pdata.lomwe2$conf.high <- c(pdata.lomwe2$estimate + qnorm(0.975)*
                              c(summary(out.mutharikia2.lomwe)$coefficients[1,2], summary(out.chakwera2.lomwe)$coefficients[1,2], summary(out.chilima2.lomwe)$coefficients[1,2]))
pdata.lomwe2$model <- "Candidate rows (Lomwe districts)"


# Any Edits (Ngoni districts)
pdata.ngoni1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.ngoni$coefficients[1,1], out.chakwera1.ngoni$coefficients[1,1], out.chilima1.ngoni$coefficients[1,1])
)

pdata.ngoni1$conf.low <- c(pdata.ngoni1$estimate - qnorm(0.975)*
                             c(summary(out.mutharikia1.ngoni)$coefficients[1,2], summary(out.chakwera1.ngoni)$coefficients[1,2], summary(out.chilima1.ngoni)$coefficients[1,2]))
pdata.ngoni1$conf.high <- c(pdata.ngoni1$estimate + qnorm(0.975)*
                              c(summary(out.mutharikia1.ngoni)$coefficients[1,2], summary(out.chakwera1.ngoni)$coefficients[1,2], summary(out.chilima1.ngoni)$coefficients[1,2]))
pdata.ngoni1$model <- "Any Edits (Ngoni districts)"


# Candidate rows (Ngoni districts)
pdata.ngoni2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.ngoni$coefficients[1,1], out.chakwera2.ngoni$coefficients[1,1], out.chilima2.ngoni$coefficients[1,1])
)

pdata.ngoni2$conf.low <- c(pdata.ngoni2$estimate - qnorm(0.975)*
                             c(summary(out.mutharikia2.ngoni)$coefficients[1,2], summary(out.chakwera2.ngoni)$coefficients[1,2], summary(out.chilima2.ngoni)$coefficients[1,2]))
pdata.ngoni2$conf.high <- c(pdata.ngoni2$estimate + qnorm(0.975)*
                              c(summary(out.mutharikia2.ngoni)$coefficients[1,2], summary(out.chakwera2.ngoni)$coefficients[1,2], summary(out.chilima2.ngoni)$coefficients[1,2]))
pdata.ngoni2$model <- "Candidate rows (Ngoni districts)"


# Any Edits (Tumbuka districts)
pdata.tumbuka1 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia1.tumbuka$coefficients[1,1], out.chakwera1.tumbuka$coefficients[1,1], out.chilima1.tumbuka$coefficients[1,1])
)

pdata.tumbuka1$conf.low <- c(pdata.tumbuka1$estimate - qnorm(0.975)*
                               c(summary(out.mutharikia1.tumbuka)$coefficients[1,2], summary(out.chakwera1.tumbuka)$coefficients[1,2], summary(out.chilima1.tumbuka)$coefficients[1,2]))
pdata.tumbuka1$conf.high <- c(pdata.tumbuka1$estimate + qnorm(0.975)*
                                c(summary(out.mutharikia1.tumbuka)$coefficients[1,2], summary(out.chakwera1.tumbuka)$coefficients[1,2], summary(out.chilima1.tumbuka)$coefficients[1,2]))
pdata.tumbuka1$model <- "Any Edits (Tumbuka districts)"


# Candidate rows (Tumbuka districts)
pdata.tumbuka2 <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.mutharikia2.tumbuka$coefficients[1,1], out.chakwera2.tumbuka$coefficients[1,1], out.chilima2.tumbuka$coefficients[1,1])
)

pdata.tumbuka2$conf.low <- c(pdata.tumbuka2$estimate - qnorm(0.975)*
                               c(summary(out.mutharikia2.tumbuka)$coefficients[1,2], summary(out.chakwera2.tumbuka)$coefficients[1,2], summary(out.chilima2.tumbuka)$coefficients[1,2]))
pdata.tumbuka2$conf.high <- c(pdata.tumbuka2$estimate + qnorm(0.975)*
                                c(summary(out.mutharikia2.tumbuka)$coefficients[1,2], summary(out.chakwera2.tumbuka)$coefficients[1,2], summary(out.chilima2.tumbuka)$coefficients[1,2]))
pdata.tumbuka2$model <- "Candidate rows (Tumbuka districts)"



pdata.all.x <- rbind(pdata.chewa1, pdata.chewa2, pdata.lomwe1, pdata.lomwe2, pdata.ngoni1, pdata.ngoni2,  pdata.tumbuka1, pdata.tumbuka2)
pdata.all.x[order(pdata.all.x$model, decreasing = F),]

figure.s13 <-dwplot(pdata.all.x, 
                     dot_args = list(size = 1.5),
                     whisker_args = list(size = 0.75),
                     vline = geom_vline(xintercept=0, colour="black", linetype=2, linewidth=0.5))+
  theme_bw()+xlab("Coefficient (95% confindence)") +
  scale_colour_discrete(name = "Model")+
  #  coord_cartesian(xlim=c(-1,1)) +
  theme(text = element_text(size=12),  
        #        legend.position = c(0.007, 0.01),
        legend.justification = c(0, 0)) 

figure.s13



# Figure S14: Result-sheet edits and changes in candidate percent  --------
# This code generates figure S14 comparing edits and changes candidate vote-shares (%) during aggregation

out.m1d <- felm(mutharika_difference_percent ~ pres_alt  | constituency | 0 | constituency, data=edits)
out.m2d <- felm(mutharika_difference_percent ~ pres_alt  | constituency | 0 | constituency, data=edit_data1e)
out.m3d <- felm(mutharika_difference_percent ~ cand_alt  | constituency | 0 | constituency, data=edit_data1e)
out.m4d <- felm(mutharika_difference_percent ~ cand_alt  | constituency | 0 | constituency, data=edit_data1e)

out.c1d <- felm(chakwera_difference_percent ~ pres_alt  | constituency | 0 | constituency, data=edit_data1e)
out.c2d <- felm(chakwera_difference_percent ~ pres_alt  | constituency | 0 | constituency, data=edit_data1e)
out.c3d <- felm(chakwera_difference_percent ~ cand_alt  | constituency | 0 | constituency, data=edit_data1e)
out.c4d <- felm(chakwera_difference_percent ~ cand_alt  | constituency | 0 | constituency, data=edit_data1e)

out.cc1d <- felm(chilima_difference_percent ~ pres_alt  | constituency | 0 | constituency, data=edit_data1e)
out.cc2d <- felm(chilima_difference_percent ~ pres_alt  | constituency | 0 | constituency, data=edit_data1e)
out.cc3d <- felm(chilima_difference_percent ~ cand_alt  | constituency | 0 | constituency, data=edit_data1e)
out.cc4d <- felm(chilima_difference_percent ~ cand_alt  | constituency | 0 | constituency, data=edit_data1e)


# Result-sheet with any edits
pdata1xd <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.m1d$coefficients[1,1], out.c1d$coefficients[1,1], out.cc1d$coefficients[1,1])
)

pdata1xd$conf.low <- c(pdata1xd$estimate - qnorm(0.975)*
                         c(summary(out.m1d)$coefficients[1,2], summary(out.c1d)$coefficients[1,2], summary(out.cc1d)$coefficients[1,2]))
pdata1xd$conf.high <- c(pdata1xd$estimate + qnorm(0.975)*
                          c(summary(out.m1d)$coefficients[1,2], summary(out.c1d)$coefficients[1,2], summary(out.cc1d)$coefficients[1,2]))
pdata1xd$model <- "Result-sheet with any edits"

# Result-sheet with any edits (fixed effects)
pdata2xd <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.m2d$coefficients[1,1], out.c2d$coefficients[1,1], out.cc2d$coefficients[1,1])
)
pdata2xd$conf.low <- c(pdata2xd$estimate - qnorm(0.975)*
                         c(summary(out.m2d)$coefficients[1,2], summary(out.c2d)$coefficients[1,2], summary(out.cc2d)$coefficients[1,2]))
pdata2xd$conf.high <- c(pdata2xd$estimate + qnorm(0.975)*
                          c(summary(out.m2d)$coefficients[1,2], summary(out.c2d)$coefficients[1,2], summary(out.cc2d)$coefficients[1,2]))
pdata2xd$model <- "Result-sheet with any edits (fixed effects)"

# Edits in candidate rows
pdata3xd <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.m3d$coefficients[1,1], out.c3d$coefficients[1,1], out.cc3d$coefficients[1,1])
)
pdata3xd$conf.low <- c(pdata3xd$estimate - qnorm(0.975)*
                         c(summary(out.m3d)$coefficients[1,2], summary(out.c3d)$coefficients[1,2], summary(out.cc3d)$coefficients[1,2]))
pdata3xd$conf.high <- c(pdata3xd$estimate + qnorm(0.975)*
                          c(summary(out.m3d)$coefficients[1,2], summary(out.c3d)$coefficients[1,2], summary(out.cc3d)$coefficients[1,2]))
pdata3xd$model <- "Edits in candidate rows"

# Edits in candidate rows (fixed effects)
pdata4xd <- data.frame(
  term = c(c('Mutharika', 'Chakwera', 'Chilima')), 
  estimate = c(out.m4d$coefficients[1,1], out.c4d$coefficients[1,1], out.cc4d$coefficients[1,1])
)
pdata4xd$conf.low <- c(pdata4xd$estimate - qnorm(0.975)*
                         c(summary(out.m4d)$coefficients[1,2], summary(out.c4d)$coefficients[1,2], summary(out.cc4d)$coefficients[1,2]))
pdata4xd$conf.high <- c(pdata4xd$estimate + qnorm(0.975)*
                          c(summary(out.m4d)$coefficients[1,2], summary(out.c4d)$coefficients[1,2], summary(out.cc4d)$coefficients[1,2]))
pdata4xd$model <- "Edits in candidate rows (fixed effects)"


pdata.all.xd <- rbind(pdata1xd, pdata2xd, pdata3xd, pdata4xd)
pdata.all.xd[order(pdata.all.xd$model, decreasing = F),]

figure.s14 <-dwplot(pdata.all.xd, 
                           dot_args = list(size = 1.5),
                           whisker_args = list(size = 0.75),
                           vline = geom_vline(xintercept=0, colour="black", linetype=2, linewidth=0.5))+
  theme_bw()+xlab("Coefficient (95% confindence)") +
  scale_colour_discrete(name = "Model")+
  coord_cartesian(xlim=c(-10,10)) +
  theme(text = element_text(size=12),  
        #        legend.position = c(0.007, 0.01),
        legend.justification = c(0, 0)) 


figure.s14


# Figure S5: Result-sheet edits and changes in candidate votes bet --------
# This code generates figure S5 comparing edits and changes in candidate votes between 2019 and 2020 elections

out.2020.m1d <- felm(mutharika_2020_difference ~ pres_alt + registered | 0 | 0 | constituency, data=edits)
out.2020.m2d <- felm(mutharika_2020_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits)
out.2020.m3d <- felm(mutharika_2020_difference ~ cand_alt + registered | 0 | 0 | constituency, data=edits)
out.2020.m4d <- felm(mutharika_2020_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits)
 
out.2020.c1d <- felm(chakwera_2020_difference ~ pres_alt + registered | 0 | 0 | constituency, data=edits)
out.2020.c2d <- felm(chakwera_2020_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits)
out.2020.c3d <- felm(chakwera_2020_difference ~ cand_alt + registered | 0 | 0 | constituency, data=edits)
out.2020.c4d <- felm(chakwera_2020_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits)

out.2020.cc1d <- felm(joint_2020_difference ~ pres_alt + registered | 0 | 0 | constituency, data=edits)
out.2020.cc2d <- felm(joint_2020_difference ~ pres_alt + registered | constituency | 0 | constituency, data=edits)
out.2020.cc3d <- felm(joint_2020_difference ~ cand_alt + registered | 0 | 0 | constituency, data=edits)
out.2020.cc4d <- felm(joint_2020_difference ~ cand_alt + registered | constituency | 0 | constituency, data=edits)

# Result-sheet with any edits
pdata1xd <- data.frame(
       term = c(c('Mutharika', 'Chakwera', 'Chakwera + Chilima')), 
       estimate = c(out.2020.m1d$coefficients[2,1], out.2020.c1d$coefficients[2,1], out.2020.cc1d$coefficients[2,1])
    )
 
pdata1xd$conf.low <- c(pdata1xd$estimate - qnorm(0.975)*
                      c(summary(out.2020.m1d)$coefficients[2,2], summary(out.2020.c1d)$coefficients[2,2], summary(out.2020.cc1d)$coefficients[2,2]))
pdata1xd$conf.high <- c(pdata1xd$estimate + qnorm(0.975)*
                      c(summary(out.2020.m1d)$coefficients[2,2], summary(out.2020.c1d)$coefficients[2,2], summary(out.2020.cc1d)$coefficients[2,2]))
pdata1xd$model <- "Result-sheet with any edits"
 
# Result-sheet with any edits (fixed effects)
pdata2xd <- data.frame(
       term = c(c('Mutharika', 'Chakwera', 'Chakwera + Chilima')), 
       estimate = c(out.2020.m2d$coefficients[1,1], out.2020.c2d$coefficients[1,1], out.2020.cc2d$coefficients[1,1])
    )
pdata2xd$conf.low <- c(pdata2xd$estimate - qnorm(0.975)*
                           c(summary(out.2020.m2d)$coefficients[1,2], summary(out.2020.c2d)$coefficients[1,2], summary(out.2020.cc2d)$coefficients[1,2]))
pdata2xd$conf.high <- c(pdata2xd$estimate + qnorm(0.975)*
                           c(summary(out.2020.m2d)$coefficients[1,2], summary(out.2020.c2d)$coefficients[1,2], summary(out.2020.cc2d)$coefficients[1,2]))
pdata2xd$model <- "Result-sheet with any edits (fixed effects)"
 
# Edits in candidate rows
pdata3xd <- data.frame(
    term = c(c('Mutharika', 'Chakwera', 'Chakwera + Chilima')), 
    estimate = c(out.2020.m3d$coefficients[2,1], out.2020.c3d$coefficients[2,1], out.2020.cc3d$coefficients[2,1])
    )
pdata3xd$conf.low <- c(pdata3xd$estimate - qnorm(0.975)*
                           c(summary(out.2020.m3d)$coefficients[2,2], summary(out.2020.c3d)$coefficients[2,2], summary(out.2020.cc3d)$coefficients[2,2]))
pdata3xd$conf.high <- c(pdata3xd$estimate + qnorm(0.975)*
                            c(summary(out.2020.m3d)$coefficients[2,2], summary(out.2020.c3d)$coefficients[2,2], summary(out.2020.cc3d)$coefficients[2,2]))
pdata3xd$model <- "Edits in candidate rows"

# Edits in candidate rows (fixed effects)
pdata4xd <- data.frame(
    term = c(c('Mutharika', 'Chakwera', 'Chakwera + Chilima')), 
    estimate = c(out.2020.m4d$coefficients[1,1], out.2020.c4d$coefficients[1,1], out.2020.cc4d$coefficients[1,1])
    )
pdata4xd$conf.low <- c(pdata4xd$estimate - qnorm(0.975)*
                           c(summary(out.2020.m4d)$coefficients[1,2], summary(out.2020.c4d)$coefficients[1,2], summary(out.2020.cc4d)$coefficients[1,2]))
pdata4xd$conf.high <- c(pdata4xd$estimate + qnorm(0.975)*
                            c(summary(out.2020.m4d)$coefficients[1,2], summary(out.2020.c4d)$coefficients[1,2], summary(out.2020.cc4d)$coefficients[1,2]))
pdata4xd$model <- "Edits in candidate rows (fixed effects)"
 
pdata.all.xd <- rbind(pdata1xd, pdata2xd, pdata3xd, pdata4xd)
pdata.all.xd[order(pdata.all.xd$model, decreasing = F),]

figure.s5 <-dwplot(pdata.all.xd, 
                     dot_args = list(size = 1.5),
                     whisker_args = list(size = 0.75),
                     vline = geom_vline(xintercept=0, colour="black", linetype=2, linewidth=0.5))+
  theme_bw()+xlab("Coefficient (95% confindence)") +
  scale_colour_discrete(name = "Model")+
  coord_cartesian(xlim=c(-50,50)) +
  theme(text = element_text(size=12),  
            #        legend.position = c(0.007, 0.01),
            legend.justification = c(0, 0)) 
figure.s5



# Supplementary 6.2 Polling Stream analysis -------------------------------
# The sets of codes below generate the figures and graphs presented in the supplementary material, section 6.2



# Figure S6: Expected and observed votes in polling streams for Mu --------

dat1 <- subset(pstream2, pstream2$sedit == 0 | pstream2$sedit==1)
dat1$groups <- ave(dat1$sedit, dat1$ps_pres, FUN = mean)
# control group
dat1_a <- subset(dat1, dat1$groups==0)
set.seed(2222)
dat1_a$placebo <- rbinom(nrow(dat1_a),1,0.5) # placebo treatment
dat1_a$placebo_mean <- ave(dat1_a$placebo, dat1_a$ps_pres, FUN = mean)
dat1_a <- subset(dat1_a, dat1_a$placebo_mean>0 & dat1_a$placebo_mean<1) # remove polling stations with no or all placebo
# expected and observed
dat1_a$valid2 <- dat1_a$valid
dat1_a$valid2[dat1_a$placebo==1]<-0  
dat1_a$valid_m <- ave(dat1_a$valid2, dat1_a$ps_pres, FUN = sum)
dat1_a$mutharika2 <- dat1_a$mutharika  
dat1_a$mutharika2[dat1_a$placebo==1]<-0
dat1_a$mutharika_m <- ave(dat1_a$mutharika2, dat1_a$ps_pres, FUN = sum)
dat1_a$expected1.1 <- (dat1_a$mutharika_m / dat1_a$valid_m) * dat1_a$valid
dat1_a$observed1.1 <- dat1_a$mutharika  
dat1_a <- subset(dat1_a, dat1_a$placebo==1)
dat1_a1 <- data.frame(dat1_a$expected1.1, dat1_a$observed1.1)
names(dat1_a1) <- c("expected1.1", "observed1.1")
dat1_a1$alteration <-0
## treatment group
dat1_b <- subset(dat1, dat1$groups>0 & dat1$groups<1)
# expected and observed
dat1_b$valid2 <- dat1_b$valid
dat1_b$valid2[dat1_b$sedit==1]<-0
dat1_b$valid_m <- ave(dat1_b$valid2, dat1_b$ps_pres, FUN = sum)
dat1_b$mutharika2 <- dat1_b$mutharika
dat1_b$mutharika2[dat1_b$sedit==1]<- 0
dat1_b$mutharika_m <- ave(dat1_b$mutharika2, dat1_b$ps_pres, FUN = sum)
dat1_b$expected1.1 <- (dat1_b$mutharika_m / dat1_b$valid_m) * dat1_b$valid
dat1_b$observed1.1 <- dat1_b$mutharika
dat1_b <- subset(dat1_b, dat1_b$sedit==1)
dat1_b1 <- data.frame(dat1_b$expected1.1, dat1_b$observed1.1)
names(dat1_b1) <- c("expected1.1", "observed1.1")
dat1_b1$alteration <-1
data1.1 <- rbind(dat1_a1, dat1_b1)
# Mutharika plot
Z1 <- subset(data1.1, data1.1$alteration==0)
Z1$x<-Z1$expected1.1
Z1$y<-Z1$observed1.1
B1a<-MCMCquantreg(y~x, data = Z1, tau=0.975, burnin = 1000, mcmc = 10000, thin = 1, verbose = 0, seed = sample(1:1000000,1), beta.start = NA, b0 = 0, B0 = 0)
B1aX0<-mean(B1a[,1])
B1aX1<-mean(B1a[,2])
B1b<-MCMCquantreg(y~x, data = Z1, tau=0.025, burnin = 1000, mcmc = 10000, thin = 1, verbose = 0, seed = sample(1:1000000,1), beta.start = NA, b0 = 0, B0 = 0)
B1bX0<-mean(B1b[,1])
B1bX1<-mean(B1b[,2])
A <- data1.1
A$x<-A$expected1.1
A$y<-A$observed1.1
A$g<-A$alteration
A$g[A$g==1]<-"2. Edit"
A$g[A$g==0]<-"1. Placebo"
p2 <- ggplot(A, aes(x, y)) 

figure.s6 <- p2 + geom_point() +
  theme_bw()+
  geom_abline(intercept = 0, slope = 1, colour = "green", linetype=2)+
  geom_abline(intercept = B1bX0, slope = B1bX1, colour = "blue", linetype=2, lwd=1)+
  geom_abline(intercept = B1aX0, slope = B1aX1, colour = "blue", linetype=2, lwd=1)+
  facet_wrap(~ g)+
  theme(legend.position = "none") +
  xlab("Expected Mutharika Votes")+
  ylab("Observed Mutharika Votes") +
  ylim(0,800)+
  xlim(0,800)+
  theme(title = element_text(size=10),
        axis.title = element_text(size=10), 
        aspect.ratio = 0.75) 

figure.s6

# obtain bayes factors
m0 <- MCMCregress(observed1.1 ~ expected1.1, 
                  data=data1.1, 
                  b0=c(1,10), 
                  B0=c(1,0.5),
                  marginal.likelihood="Chib95",
                  mcmc=10000
)
m1 <- MCMCregress(observed1.1 ~ expected1.1 + alteration, 
                  data=data1.1, 
                  b0=c(1,10), 
                  B0=c(1,0.5),
                  marginal.likelihood="Chib95",
                  mcmc=10000
)

BF1.1 <- BayesFactor(m0, m1)
ts7.mutharika1 <- BF1.1$BF.logmarglike
ts7.mutharika2 <- exp(1)^(BF1.1$BF.logmarglike[1,2]-BF1.1$BF.logmarglike[1,1])


# Figure s7: Expected and observed votes in polling streams for Ch --------

dat1 <- subset(pstream2, pstream2$sedit==0 | pstream2$sedit==1)
dat1$sedit <- as.numeric(dat1$sedit)
dat1$groups <- ave(dat1$sedit, dat1$ps_pres, FUN = mean)
## control group
dat1_a <- subset(dat1, dat1$groups==0)
set.seed(2222)
dat1_a$placebo <- rbinom(nrow(dat1_a),1,0.5) # placebo treatment
dat1_a$placebo_mean <- ave(dat1_a$placebo, dat1_a$ps_pres, FUN = mean)
dat1_a <- subset(dat1_a, dat1_a$placebo_mean>0 & dat1_a$placebo_mean<1) # remove precincts with no or all placebo
# expected and observed
dat1_a$valid2 <- dat1_a$valid
dat1_a$valid2[dat1_a$placebo==1]<-0
dat1_a$valid_m <- ave(dat1_a$valid2, dat1_a$ps_pres, FUN = sum)
dat1_a$chakwera2 <- dat1_a$chakwera
dat1_a$chakwera2[dat1_a$placebo==1]<-0
dat1_a$chakwera_m <- ave(dat1_a$chakwera2, dat1_a$ps_pres, FUN = sum)
dat1_a$expected2.1 <- (dat1_a$chakwera_m / dat1_a$valid_m) * dat1_a$valid
dat1_a$observed2.1 <- dat1_a$chakwera
dat1_a <- subset(dat1_a, dat1_a$placebo==1)
dat1_a1 <- data.frame(dat1_a$expected2.1, dat1_a$observed2.1)
names(dat1_a1) <- c("expected2.1", "observed2.1")
dat1_a1$alteration <-0
## treatment group
dat1_b <- subset(dat1, dat1$groups>0 & dat1$groups<1)
# expected and observed
dat1_b$valid2 <- dat1_b$valid
dat1_b$valid2[dat1_b$sedit==1]<-0
dat1_b$valid_m <- ave(dat1_b$valid2, dat1_b$ps_pres, FUN = sum)
dat1_b$chakwera2 <- dat1_b$chakwera
dat1_b$chakwera2[dat1_b$sedit==1]<-0
dat1_b$chakwera_m <- ave(dat1_b$chakwera2, dat1_b$ps_pres, FUN = sum)
dat1_b$expected2.1 <- (dat1_b$chakwera_m / dat1_b$valid_m) * dat1_b$valid
dat1_b$observed2.1 <- dat1_b$chakwera
dat1_b <- subset(dat1_b, dat1_b$sedit==1)
dat1_b1 <- data.frame(dat1_b$expected2.1, dat1_b$observed2.1)
names(dat1_b1) <- c("expected2.1", "observed2.1")
dat1_b1$alteration <-1
# rbind
data2.1 <- rbind(dat1_a1, dat1_b1)
# Chakwera plot
Z1 <- subset(data2.1, data2.1$alteration==0)
Z1$x<-Z1$expected2.1
Z1$y<-Z1$observed2.1
B1a<-MCMCquantreg(y~x, data = Z1, tau=0.975, burnin = 1000, mcmc = 10000, thin = 1, verbose = 0, seed = sample(1:1000000,1), beta.start = NA, b0 = 0, B0 = 0)
B1aX0<-mean(B1a[,1])
B1aX1<-mean(B1a[,2])
B1b<-MCMCquantreg(y~x, data = Z1, tau=0.025, burnin = 1000, mcmc = 10000, thin = 1, verbose = 0, seed = sample(1:1000000,1), beta.start = NA, b0 = 0, B0 = 0)
B1bX0<-mean(B1b[,1])
B1bX1<-mean(B1b[,2])
A <- data2.1
A$x<-A$expected2.1
A$y<-A$observed2.1
A$g<-A$alteration
A$g[A$g==1]<-"2. Edit"
A$g[A$g==0]<-"1. Placebo"
p <- ggplot(A, aes(x, y)) 

figure.s7 <- p + geom_point() +
  theme_bw()+
  geom_abline(intercept = 0, slope = 1, colour = "green", linetype=2)+
  geom_abline(intercept = B1bX0, slope = B1bX1, colour = "blue", linetype=2, lwd=1)+
  geom_abline(intercept = B1aX0, slope = B1aX1, colour = "blue", linetype=2, lwd=1)+
  facet_wrap(~ g)+
  theme(legend.position = "none") +
  xlab("Expected Chakwera Votes")+
  ylab("Observed Chakwera Votes") +
  ylim(0,800)+
  xlim(0,800)+
  theme(title = element_text(size=10),
        axis.title = element_text(size=10), 
        aspect.ratio = 0.75) 
figure.s7

# obtain bayes factors
m0 <- MCMCregress(observed2.1 ~ expected2.1, 
                  data=data2.1, 
                  b0=c(1,10), 
                  B0=c(1,0.5),
                  marginal.likelihood="Chib95",
                  mcmc=10000
)
m1 <- MCMCregress(observed2.1 ~ expected2.1 + alteration, 
                  data=data2.1, 
                  b0=c(1,10), 
                  B0=c(1,0.5),
                  marginal.likelihood="Chib95",
                  mcmc=10000
)

BF2.1 <- BayesFactor(m0, m1)
ts7.chakwera1 <- BF2.1$BF.logmarglike
ts7.chakwera2 <- exp(1)^(BF2.1$BF.logmarglike[1,2]-BF2.1$BF.logmarglike[1,1])


# Figure S8: Expected and observed votes in polling streams for Ch --------

dat1 <- subset(pstream2, pstream2$sedit==0 | pstream2$sedit==1)
dat1$sedit <- as.numeric(dat1$sedit)
dat1$groups <- ave(dat1$sedit, dat1$ps_pres, FUN = mean)
## control group
dat1_a <- subset(dat1, dat1$groups==0)
set.seed(1111)
dat1_a$placebo <- rbinom(nrow(dat1_a),1,0.5) # placebo treatment
dat1_a$placebo_mean <- ave(dat1_a$placebo, dat1_a$ps_pres, FUN = mean)
dat1_a <- subset(dat1_a, dat1_a$placebo_mean>0 & dat1_a$placebo_mean<1) # remove precincts with no or all placebo
# expected and observed
dat1_a$valid2 <- dat1_a$valid
dat1_a$valid2[dat1_a$placebo==1]<-0
dat1_a$valid_m <- ave(dat1_a$valid2, dat1_a$ps_pres, FUN = sum)
dat1_a$chilima2 <- dat1_a$chilima
dat1_a$chilima2[dat1_a$placebo==1]<-0
dat1_a$chilima_m <- ave(dat1_a$chilima2, dat1_a$ps_pres, FUN = sum)
dat1_a$expected3.1 <- (dat1_a$chilima_m / dat1_a$valid_m) * dat1_a$valid
dat1_a$observed3.1 <- dat1_a$chilima
dat1_a <- subset(dat1_a, dat1_a$placebo==1)
dat1_a1 <- data.frame(dat1_a$expected3.1, dat1_a$observed3.1)
names(dat1_a1) <- c("expected3.1", "observed3.1")
dat1_a1$alteration <-0
## treatment group
dat1_b <- subset(dat1, dat1$groups>0 & dat1$groups<1)
# expected and observed
dat1_b$valid2 <- dat1_b$valid
dat1_b$valid2[dat1_b$sedit==1]<-0
dat1_b$valid_m <- ave(dat1_b$valid2, dat1_b$ps_pres, FUN = sum)
dat1_b$chilima2 <- dat1_b$chilima
dat1_b$chilima2[dat1_b$sedit==1]<-0
dat1_b$chilima_m <- ave(dat1_b$chilima2, dat1_b$ps_pres, FUN = sum)
dat1_b$expected3.1 <- (dat1_b$chilima_m / dat1_b$valid_m) * dat1_b$valid
dat1_b$observed3.1 <- dat1_b$chilima
dat1_b <- subset(dat1_b, dat1_b$sedit==1)
dat1_b1 <- data.frame(dat1_b$expected3.1, dat1_b$observed3.1)
names(dat1_b1) <- c("expected3.1", "observed3.1")
dat1_b1$alteration <-1
#rbind
data3.1 <- rbind(dat1_a1, dat1_b1)
# Chilima plot
Z1 <- subset(data3.1, data3.1$alteration==0)
Z1$x<-Z1$expected3.1
Z1$y<-Z1$observed3.1
B1a<-MCMCquantreg(y~x, data = Z1, tau=0.975, burnin = 1000, mcmc = 10000, thin = 1, verbose = 0, seed = sample(1:1000000,1), beta.start = NA, b0 = 0, B0 = 0)
B1aX0<-mean(B1a[,1])
B1aX1<-mean(B1a[,2])
B1b<-MCMCquantreg(y~x, data = Z1, tau=0.025, burnin = 1000, mcmc = 10000, thin = 1, verbose = 0, seed = sample(1:1000000,1), beta.start = NA, b0 = 0, B0 = 0)
B1bX0<-mean(B1b[,1])
B1bX1<-mean(B1b[,2])
A <- data3.1
A$x<-A$expected3.1
A$y<-A$observed3.1
A$g<-A$alteration
A$g[A$g==1]<-"2. Edit"
A$g[A$g==0]<-"1. Placebo"
p <- ggplot(A, aes(x, y)) 

figure.s8 <- p + geom_point() +
  theme_bw()+
  geom_abline(intercept = 0, slope = 1, colour = "green", linetype=2)+
  geom_abline(intercept = B1bX0, slope = B1bX1, colour = "blue", linetype=2, lwd=1)+
  geom_abline(intercept = B1aX0, slope = B1aX1, colour = "blue", linetype=2, lwd=1)+
  facet_wrap(~ g)+
  theme(legend.position = "none") +
  xlab("Expected Chilima Votes")+
  ylab("Observed Chilima Votes") +
  ylim(0,800)+
  xlim(0,800)+
  theme(title = element_text(size=10),
        axis.title = element_text(size=10), 
        aspect.ratio = 0.75)

figure.s8

# obtain bayes factors
m0 <- MCMCregress(observed3.1 ~ expected3.1, 
                  data=data3.1, 
                  b0=c(1,10), 
                  B0=c(1,0.5),
                  marginal.likelihood="Chib95",
                  mcmc=10000
)
m1 <- MCMCregress(observed3.1 ~ expected3.1 + alteration, 
                  data=data3.1, 
                  b0=c(1,10), 
                  B0=c(1,0.5),
                  marginal.likelihood="Chib95",
                  mcmc=10000
)

BF3.1 <- BayesFactor(m0, m1)
ts7.chilima1 <- BF3.1$BF.logmarglike
ts7.chilima2 <- exp(1)^(BF3.1$BF.logmarglike[1,2]-BF3.1$BF.logmarglike[1,1])


# Table S7: Bayes factors for candidate votes -----------------------------
# this code generates the values for table s7

candidates <- c('Mutharika', 'Chakwera', 'Chilima')
M0 <- c(ts7.mutharika1[1,1], ts7.chakwera1[1,1], ts7.chilima1[1,1])
M1 <- c(ts7.mutharika1[1,2], ts7.chakwera1[1,2], ts7.chilima1[1,2])
Bayes_factor <- c(round(ts7.mutharika2,13), round(ts7.chakwera2,13), round(ts7.chilima2,13))
Bayes_factor <- as.character(Bayes_factor)
Interpretation <- c('In favour of M0', 'In favour of M0', 'In favour of M0')

tab7 <- data.frame(candidates, M0, M1, Bayes_factor, Interpretation)
colnames(tab7) <- c('Candidate Votes', 'M0', 'M1', 'Bayes Factor','Interpretation')
tab7x <- xtable(tab7)

print.xtable(tab7x, type = 'latex', 
             include.rownames = F, include.colnames = T, table.placement = '!!h',
             hline.after = c(-1, -1, 0, nrow(tab7x), nrow(tab7x)))
