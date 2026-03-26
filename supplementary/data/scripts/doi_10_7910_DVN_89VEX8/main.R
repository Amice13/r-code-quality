rm(list=ls())
#Set working directory to replication folder
library(tidyverse)
library(readxl)
library(gridExtra)
library(xtable)

#GRAPH PARAMETERS
title.size <- 20
nom.label.size <- 10
x.axis.tick.size <- 14
y.axis.tick.size <- 14
x.axis.label.size <- 18
y.axis.label.size <- 18
strip.text.size <- 14
line.width <- 1.5
axis.text <- 1.2
label.text <- 1.2
y.offset <- .5
text.size <- 1.4
text.size2 <- 1.2
arrow.width <- 1.5
width <-2
text.size <- .7
y.axis.max <- 70
transparency = .2

theme_set(
  theme_bw() +
    theme(
      axis.text        = element_text(color = "black"),
      axis.text.x      = element_text(color = "black"),
      axis.text.y      = element_text(color = "black")
    )
)

options(scipen=10)

cda <- read_csv('Data/shadow_docket_database_v2-0.csv')

#turn action_class to factor
cda$action_class <- factor(cda$action_class)

sort(table(cda$action_class), decreasing=TRUE)

table(cda$relief)
###############################################################
#Create Table 1
foo.names <- levels(cda$action_class)
foo.counts <- as.numeric(table(cda$action_class))


foo.description <- c(
  "Motion to request abeyance or decision to hold action in abeyance",
  "Admission to the bar",
  "Judgment affirmed",
  "Judgment aﬃrmed for some claims and reversed for others",
  "Request to amend petition or briefing schedule",
  "Requests to participate as amicus curiae, including, invitations to third-parties other than the Solicitor General participate as amicus curiae",
  "Request for appointment of counsel",
  "Deals with the suspension or disbarment of an attorney or other attorney discipline",
  "Application for bail",
  "Motion for leave to file Bill of Complaint",
  "Decisions involving the briefing schedule",
  "Request for a certificate of appealability",
  "Grants, denies, or dismisses a petition for certiorari",
  "Request to consolidate by petitioners or the court deciding to consolidate cases",
  "Requests for costs and fees", 
  "Requests for damages",
  "Request by one or both of the parties to defer an action",
  "Court dismisses certiorari as improvidently granted",
  "Direction by the court to brief an argument or question",
  "Direction by the court to file briefs",
  "Request to dismiss appeal parties or dismissal of appeal by court",
  "Request to add materials to the record",
  "Request to expedite action",		
  "Request to submit (or resubmit) a filing under seal",
  "Grants, vacates, and remands a case to the lower court in light of a related opinion of the court",
  "Petition for writ of habeas corpus", 
  "Petition to proceed in forma pauperis",
  "Request for injunctive relief",
  "Court postpones discussing questions of jurisdiction until deciding case on the merits",
  "Judgment is affirmed due to lack of quorum by the court", 
  "Request leave to file a petition not captured by other categories",
  "Petitioner asks for leave to intervene in a case",
  "Petition for writ of mandamus",
  "Miscellaneous actions not fitting into another category",
  "Asks for divided argument, sets schedule for oral argument, adds or removes case to argument calendar, sets report from special master for oral argument",
  "Request to file out of time", 
  "Request involving printing requirements",
  "Request to proceed pro hac vice", 
  "Court announces it has probable jurisdiction for a case",
  "Request for writ of prohibition",
  'Request to reconsider a ruling on filing IFP',
  "Asks the court for a rehearing", 
  "Requests the court for release pending appeal",
  "Motion to remand the case", 
  "Request by the court to file briefs",
  "Motion to retax costs", 
  "Reversal of decision by lower court", 
  "Motions of or concerning the River Master",
  "Motion for sanctions", 
  "Request to proceed as seaman",
  "Actions involving the Solicitor General participating as amicus and/or in oral argument", 
  "Actions relating to report or actions of a special master, not including oral argument",
  "Request of the court to stay an action", 
  "Motion to strike filing from record", 
  "Motion for substitution",
  "Motion to vacate or decision by the court to vacate order", 
  "Motion to vacate and remand or decision to vacate and remand",
  "Request to vacate a stay",
  "Request to proceed as a veteran"
)

foo.df <- data.frame(cbind(foo.names, foo.description, foo.counts), row.names=NULL)

# Ensure foo.counts is numeric
foo.df$foo.counts <- as.numeric(foo.df$foo.counts)

sum(foo.df$foo.counts)

# Sort descending by foo.counts
foo.df <- foo.df[order(-foo.df$foo.counts), ]
names(foo.df) <- c("Action class", "Description", "Total counts") 
head(foo.df)

#MAKE TEX TABLE

# Format last column as integers with commas
foo.df[[ncol(foo.df)]] <- as.numeric(foo.df[[ncol(foo.df)]])
foo.df[[ncol(foo.df)]] <- formatC(foo.df[[ncol(foo.df)]], format = "d", big.mark = ",")

# Create xtable
xtab <- xtable(foo.df, caption = "A Summary of the Action Classes in the Dataset. The last
              column depicts the total number of times each class appears in the data.", label = "tab:action_class_table")

# Alignment: 2nd column = 3.5in with hanging indent
alignment <- c("l", "l", ">{\\hangindent=1em\\hangafter=1}p{4in}")
if (ncol(foo.df) > 2) {
  alignment <- c(alignment, rep("l", ncol(foo.df) - 2))
}
align(xtab) <- alignment

# Bold header row
bold_headers <- paste0("\\textbf{", names(foo.df), "}", collapse = " & ")
bold_headers <- paste0(bold_headers, " \\\\")
add <- list(pos = list(0), command = bold_headers)

# Extract tabular content with bold headers and no duplicated header
tabular_body <- capture.output(print(xtab,
                                     include.rownames = FALSE,
                                     include.colnames = FALSE,
                                     sanitize.text.function = identity,
                                     only.contents = TRUE,
                                     add.to.row = add,
                                     caption.placement = "bottom",
                                     comment = FALSE))

# Build final LaTeX code
latex_code <- c(
  "\\begin{table}[t!]",
  "\\centering",
  "\\tiny",
  paste0("\\begin{tabular}{", paste(align(xtab)[-1], collapse = ""), "}"),  # exclude rownames
  tabular_body,
  "\\end{tabular}",
  "\\caption{A Summary of the Action Classes in the Dataset. The last
              column depicts the total number of times each class appears in the data.}",
  "\\label{tab:action_class_table}",
  "\\end{table}"
)

# Save to file
dir.create("Plots", showWarnings = FALSE)
writeLines(latex_code, "Tables/table1.tex")

################################################################################################

#calculate counts by year for all action classes
term.data <- cda %>%  group_by(term, action_class) %>% 
  summarise(
    counts = n()
  )


##################################################################################################################################
#Create Figure 3
foo.sd <- cda %>%
  filter(non_case==0)%>%
  mutate(cert_action = ifelse(action_class=='Certiorari','Cert','Non-cert')) %>%
  group_by(term, cert_action) %>%
  summarise(counts = n()) 

ggplot(foo.sd, aes(term, counts, linetype = cert_action)) + theme_bw()+
  geom_line() + geom_point() +  labs(x="Term", y="Number of actions") +
  coord_cartesian(ylim=(c(0, max(foo.sd$counts)+100))) + 
  scale_linetype_manual(values = c("Cert" = "solid", "Non-cert" = "longdash"))  +  # Longer dashes
  scale_x_continuous(breaks=seq(1992, 2024,4)) +
  scale_y_continuous(breaks=seq(0, 9000,2000), expand=c(0,0)) +
  theme(legend.position="none",
        axis.text.x = element_text(size=x.axis.tick.size, color = "black"),  
        axis.title.x=element_text(size=y.axis.label.size, color = "black"), 
        axis.text.y = element_text(size=y.axis.tick.size, color = "black"),  
        axis.title.y = element_text(size=y.axis.label.size, color = "black")
  ) +
  annotate("text", x = 2016, y = 7000, label = "Certiorari", color = "black", cex=6) +
  annotate("text", x = 2008, y = 2500, label = "Other actions", color = "black", cex=6) 

ggsave('Plots/Figure_3.pdf', width = 9, height = 6)

##################################################################################################################################
#Create Figure 4

###################
#Cert over time
foo.cert <- cda %>% filter(!is.na(cert_granted),!is.na(cert_type))%>% 
  group_by(term, cert_type) %>%
  summarise(counts=n()) 

cert.levels.plot <- ggplot(foo.cert, aes(term, counts, group = cert_type, linetype=cert_type)) + theme_bw() +
  geom_line() + geom_point() + labs(x="Term", y="Number of cert petitions", title="A) Numbers of petitions") + 
  coord_cartesian(ylim=(c(0, max(foo.cert$counts)+500))) + 
  scale_linetype_manual(values = c("IFP" = "solid", "Paid" = "longdash"))  +  # Longer dashes
  scale_x_continuous(breaks=seq(1992, 2024,4)) +
  scale_y_continuous(breaks=seq(0, 8000,1000), expand=c(0,0)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = x.axis.tick.size, color = "black"),
    axis.title.x = element_text(size = y.axis.label.size, color = "black"),
    axis.text.y = element_text(size = y.axis.tick.size, color = "black"),
    axis.title.y = element_text(size = y.axis.label.size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.margin = unit(c(1, 1, 1, 1), "lines")
  ) +
  annotate("text", x = 1995, y = 5100, label = "IFP", color = "black", size=6) +
  annotate("text", x = 1997, y = 2400, label = "Paid", color = "black", size=6) 

###########################
#Grant rate by cert type

foo.cert_paid <- cda %>%
  filter(!is.na(cert_granted),!is.na(cert_type))%>% 
  group_by(term, cert_type) %>%
  mutate(denom = n(),
         grant_rate = sum(cert_granted)) %>%
  select(action_class, term, denom, grant_rate, cert_type) %>%
  mutate(grant_rate=100*grant_rate/denom) %>% unique()

cert.rate.plot <- ggplot(foo.cert_paid, aes(term, grant_rate, linetype=cert_type)) + theme_bw() +
  geom_line() + geom_point() + labs(x="Term", y="% granted", title="B) Grant Rate") + 
  scale_linetype_manual(values = c("IFP" = "solid", "Paid" = "longdash"))  +  # Longer dashes
  coord_cartesian(ylim=(c(0, 15)))+
  scale_x_continuous(breaks=seq(1992, 2024,4)) +
  scale_y_continuous(breaks=seq(0, 15,3), expand=c(0,0)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = x.axis.tick.size, color = "black"),
    axis.title.x = element_text(size = y.axis.label.size, color = "black"),
    axis.text.y = element_text(size = y.axis.tick.size, color = "black"),
    axis.title.y = element_text(size = y.axis.label.size, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.margin = unit(c(1, 1, 1, 2), "lines")
  ) +
  annotate("text", x = 2016.5, y = 9, label = "Paid", color = "black", cex=6) +
  annotate("text", x = 2002, y = 2, label = "IFP", color = "black", cex=6) #+


pdf("Plots/Figure_4.pdf", height=12, width=10)

grid.arrange( cert.levels.plot, cert.rate.plot, ncol=1)

dev.off()

##################################################################################################################################
#Create Figure 5
foo.grantrate <- cda %>%
  filter(!is.na(relief_granted)) %>%
  mutate(execution = ifelse(str_detect(action_text,'execution'),'Executions','Others'))%>%  
  group_by(year, action_class) %>%
  mutate(denom = n(),
         granted = ifelse(relief=='Granted',1,0)) %>% 
  mutate(grant_rate = sum(granted)) %>%
  dplyr::select(action_class, year, denom, grant_rate) %>%
  mutate(grant_rate=100*grant_rate/denom) %>% unique()

ggplot(filter(foo.grantrate, action_class  %in% c('Stay', 'Certiorari', 'Habeas Corpus')), aes(year, grant_rate, color = action_class, linetype = action_class)) + theme_bw()+
  geom_line() + geom_point() +  labs(x="Year", y="Percent granted") +
  coord_cartesian(ylim=(c(-2.5, 30)))+
  scale_linetype_manual(values = c("Certiorari" = "solid", "Stay" = "longdash", 'Habeas Corpus'='dashed'))  +  # Longer dashes
  scale_color_manual(values = c("Certiorari" = "#006CD1", "Stay" = "#994F00", 'Habeas Corpus'='black'))  +  # Longer dashes
  scale_x_continuous(breaks=seq(1992, 2024,4)) +
  scale_y_continuous(breaks=seq(0, 40,10), expand=c(0,0)) +
  theme(legend.position="none",
        axis.text.x = element_text(size=x.axis.tick.size),  axis.title.x=element_text(size=y.axis.label.size), 
        axis.text.y = element_text(size=y.axis.tick.size),  axis.title.y = element_text(size=y.axis.label.size))+
  annotate("text", x = 2019, y = 5, label ="Certiorari", color = "#006CD1", cex=6) +
  annotate("text", x = 2018, y = -1, label = "Habeas Corpus", color = "black", cex=6) +
  annotate("text", x = 2018.5, y = 27.2, label = "Stay", color = "#994F00", cex=6) 

ggsave('Plots/Figure_5.pdf', width = 9, height = 6)


########################################################################################################################
#Figure 6
foo.emergency_petitioner <- cda %>%  filter(emergency_application==TRUE) %>%
  mutate(execution = ifelse(str_detect(action_text,'execution'),'Executions','Others'))%>%  
  #2004 is the first year with full coverage 
  filter(year>=2004) %>%
  mutate(gov_petitioner = case_when(execution=='Executions'~'Death Penalty',
                                    gov_petitioner~'President',
                                    .default='other'),
         adjusted_year = ifelse(str_split_i(date,'-',2)=='01'&as.integer(str_split_i(date,'-',3)<20),year-1,year)) %>%
  filter(adjusted_year>=2004) %>%
  group_by(adjusted_year, gov_petitioner) %>%
  summarise(counts=n()) %>% ungroup() %>%
  complete(adjusted_year,gov_petitioner) %>% mutate(counts = ifelse(is.na(counts),0,counts))

ggplot(foo.emergency_petitioner, aes(adjusted_year, counts, color = gov_petitioner, linetype=gov_petitioner)) + theme_bw() +
  geom_line() + geom_point() + labs(x="Year", y="Number of emergency applications") + 
  coord_cartesian(ylim=(c(-1, max(foo.emergency_petitioner$counts)+5))) + 
  scale_linetype_manual(values = c("other" = "solid", "President" = "longdash", 'Death Penalty'='dashed'))  +  # Longer dashes
  scale_color_manual(values = c("other" = "#006CD1", "President" = "#994F00", 'Death Penalty'='black'))  +  # Longer dashes
  scale_x_continuous(breaks=seq(1992, 2024,4)) +
  scale_y_continuous(breaks=seq(0, 90,10), expand=c(0,0)) +
  theme(legend.position="none",
        axis.text.x = element_text(size=x.axis.tick.size, color = "black"),  
        axis.title.x=element_text(size=y.axis.label.size, color = "black"), 
        axis.text.y = element_text(size=y.axis.tick.size, color = "black"),  
        axis.title.y = element_text(size=y.axis.label.size, color = "black"))+
  annotate("text", x = 2008.6, y = 80, label ="Death Penalty", color = "black", cex=6) +
  annotate("text", x = 2021.2, y = 67.5, label = "Other Petitioners", color = "#006CD1", cex=6) +
  annotate("text", x = 2016, y = 7.5, label = "President", color = "#994F00", cex=6) 

ggsave('Plots/Figure_6.pdf', width = 9, height = 6)




########################################################################################################################
#Figure 9

#pull out summary reversals
sr <-  subset(term.data, action_class=="Reversal")
#plot all class by year
ggplot(sr, aes(term, counts)) + theme_bw()+
  geom_line() + geom_point() +
  labs(x="Term", y="Number of summary reversals") +
  scale_x_continuous(breaks=seq(1995, 2020,5)) +
  scale_y_continuous(breaks=seq(0, max(sr$counts),1),  limits = c(0, max(sr$counts)), expand=c(0,.1)) +
  theme(
    axis.text.x = element_text(size=x.axis.tick.size, color = "black"),  
    axis.title.x=element_text(size=y.axis.label.size, color = "black"), 
    axis.text.y = element_text(size=y.axis.tick.size, color = "black"),  
    axis.title.y = element_text(size=y.axis.label.size, color = "black"),
    panel.grid.minor.y = element_blank()
  )

ggsave('Plots/Figure_9.pdf', width = 9, height = 6)


########################################################################################################################
#Figure 7

#dissents
foo.dissents <- cda %>% 
  filter(dissent==1) %>%
  select(docket_number,term, date) %>% unique() %>%
  group_by(term) %>% summarise(counts = n()) %>% mutate(no_IFP = 'False')

foo.dissents.noIFP <- cda %>% ungroup() %>%
  mutate(
    dissent = ifelse(str_detect(action_text, 'Stevens')& (str_detect(action_text,'Herald Co')|str_detect(action_text,'See id'))&term<2020,0,dissent)) %>%
  filter(dissent==1) %>%
  select(docket_number,term, date) %>% unique() %>%
  group_by(term) %>% summarise(counts = n()) %>% mutate(no_IFP ='True')

foo.all.dissents <- foo.dissents %>% bind_rows(foo.dissents.noIFP)

dissent_plot <- ggplot(foo.all.dissents, aes(term, counts, linetype=no_IFP)) + theme_bw()+
  geom_line() + geom_point() +  labs(x="Term", y="Dissents", title = 'A) Dissent')+
  scale_linetype_manual(values = c('True'='solid','False'='longdash'))+
  coord_cartesian(ylim=(c(-1, max(foo.dissents$counts)+1)))+
  scale_x_continuous(breaks=seq(1992, 2024,4)) +
  scale_y_continuous(breaks=seq(0, 90,10), expand=c(0,0)) +
  theme(legend.position="none",
        strip.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = x.axis.tick.size, color = "black"),
        axis.title.x = element_text(size = y.axis.label.size, color = "black"),
        axis.text.y = element_text(size = y.axis.tick.size, color = "black"),
        axis.title.y = element_text(size = y.axis.label.size, color = "black"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.margin = unit(c(1, 1, 1, 2), "lines"))+
  annotate('text', y = 50, x=2003.5, label='Includes dissents about\nIFP procedure by\nJustice Stevens', cex = 6)

#disagreements
foo.disagreements <- cda %>% 
  filter(disagreement==1) %>%
  select(docket_number,term, date) %>% unique() %>%
  group_by(term) %>% summarise(counts = n()) %>% mutate(no_IFP = 'False')

foo.disagreements.noIFP <- cda %>% 
  mutate(
    disagreement = ifelse(str_detect(action_text, 'Stevens')& #Remove Stevens IFP dissents.
                            (str_detect(action_text,'Herald Co')|str_detect(action_text,'See id'))&term<2020,0,disagreement)) %>%
  filter(disagreement==1) %>%
  select(docket_number,term, date) %>% unique() %>%
  group_by(term) %>% summarise(counts = n()) %>% mutate(no_IFP='True')

foo.all.disagreements <- foo.disagreements %>% bind_rows(foo.disagreements.noIFP)


disagreement_plot <- ggplot(foo.all.disagreements, aes(term, counts, linetype = no_IFP)) + theme_bw()+
  geom_line() + geom_point() +  labs(x="Term", y="Noted disagreements", title='B) Disagreement')+
  coord_cartesian(ylim=(c(-1, max(foo.disagreements$counts)+1)))+
  scale_linetype_manual(values = c('True'='solid','False'='longdash'))+
  scale_x_continuous(breaks=seq(1992, 2024,4)) +
  scale_y_continuous(breaks=seq(0, 90,10), expand=c(0,0)) +
  theme(legend.position="none",
        strip.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = x.axis.tick.size, color = "black"),
        axis.title.x = element_text(size = y.axis.label.size, color = "black"),
        axis.text.y = element_text(size = y.axis.tick.size, color = "black"),
        axis.title.y = element_text(size = y.axis.label.size, color = "black"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.margin = unit(c(1, 1, 1, 2), "lines"))+
  annotate('text', y = 67, x=2004, label='Includes dissents about\nIFP procedure by\nJustice Stevens', cex = 6) 


pdf("Plots/Figure_7.pdf", height=12, width=10)

grid.arrange( dissent_plot, disagreement_plot, ncol=1)

dev.off()


########################################################################################################################
#Figure 8
library(MCMCpack)
disagreements <- read_excel('Data/disagreements.xlsx')
disagreements <- disagreements %>%
  mutate(docket_number = str_replace(docket_number,'–','-')) %>%
  left_join(dplyr::select(cda, c(docket_number, action_text, term, date_unix)), by = c('docket_number', 'action_text')) %>%
  unique()
disagreements <- disagreements %>% 
  mutate(across(everything(),~ifelse(.x == -9,NA,.x))) %>%
  filter(!(str_detect(action_text, 'Stevens')& #Remove Stevens IFP dissents.
             (str_detect(action_text,'Herald Co')|str_detect(action_text,'See id'))&term<2020))

disagreements <- disagreements %>%
  mutate( blackmun = ifelse(date_unix>775890000, NA, blackmun),
          rehnquist = ifelse(date_unix>1125806399, NA, rehnquist),
          oconnor = ifelse(date_unix>1138687200, NA, oconnor),
          souter = ifelse(date_unix>1246334399, NA, souter),
          stevens = ifelse(date_unix>1277870399, NA, stevens),
          scalia = ifelse(date_unix>1455425999, NA, scalia),
          kennedy = ifelse(date_unix>1533095999, NA, kennedy),
          ginsburg = ifelse(date_unix>1600487999, NA, ginsburg),
          breyer = ifelse(date_unix>1656565200, NA, breyer),
          breyer = ifelse(date_unix<775890000, NA, breyer),
          roberts = ifelse(date_unix<1127970000, NA, roberts),
          alito = ifelse(date_unix<1138687200, NA, alito),
          sotomayor = ifelse(date_unix<1249707600, NA, sotomayor),
          kagan = ifelse(date_unix<1281157200, NA, kagan),
          gorsuch = ifelse(date_unix<1491800400, NA, gorsuch),
          kavanaugh = ifelse(date_unix<1538802000, NA, kavanaugh),
          barrett = ifelse(date_unix<1603774800, NA, barrett),
          jackson = ifelse(date_unix<1656565200, NA, jackson),
          natural_court = case_when(
            date_unix>1656565200~10,
            date_unix>1603774800~9,
            date_unix>1538802000~8,
            date_unix>1491800400~7,
            date_unix>1281157200~6,
            date_unix>1249707600~5,
            date_unix>1138687200~4,
            date_unix>1127970000~3,
            date_unix>775890000~2,
            .default=1
          )
  )

rehnquist <- filter(disagreements,natural_court==2&term>=1994)




votes <-rehnquist[,c(6,13:20)]
votes <- as.matrix(votes)
votes <- t(votes)
terms <- rehnquist$term-min(rehnquist$term)+1

theta.start <- rep(0, 9)
theta.start[5] <- -3 ## stevens
theta.start[1] <- 2  ## Thomas

set.seed(78705)
model <- MCMCdynamicIRT1d(votes, terms,mcmc=50000, burnin=20000, thin=5,
                          verbose=10000, tau2.start=rep(0.1, 9),
                          e0=0, E0=1,
                          a0=0, A0=1,
                          b0=0, B0=1, c0=-1, d0=-1,
                          store.item=FALSE,
                          theta.constraints=list(stevens="-", thomas="+"))


ms <- summary(model[,1:99])
ms <-data.frame(estimate = ms$statistics[,1], se = ms$statistics[,2])
ms$justice <- rownames(ms)

ms <- ms %>%
  mutate(name =strsplit(justice,'\\.')) %>% rowwise() %>%
  mutate(period = name[[3]],
         name = name[[2]],
         period = as.integer(substr(period,2,nchar(period)))+min(rehnquist$term)-1)



ms <- ms %>% group_by(period) %>%
  mutate(ranking = rank(estimate)+.1, estimator = 'sd') 

mq <- read_excel('Data/justices.xlsx')
mq <- mq %>%
  filter(term>=1994&term<2005) %>%
  mutate(name = case_when(justice == 102~'rehnquist',
                          justice == 103~'stevens',
                          justice == 104~'oconnor',
                          justice == 105~'scalia',
                          justice == 106~'kennedy',
                          justice == 107~'souter',
                          justice == 108~'thomas',
                          justice == 109~'ginsburg',
                          justice == 110~'breyer',
                          justice == 111~'roberts',
                          justice == 112~'alito',
                          justice == 113~'sotomayor',
                          justice == 114~'kagan',
                          justice == 115~'gorsuch',
                          justice == 116~'kavanaugh',
                          justice == 117~'barrett',
                          justice == 118~'jackson'),
         period = term) %>%
  group_by(period) %>%
  mutate(ranking = rank(post_mn)-.1, estimator = 'mq', estimate = post_mn)

ms <- ms %>% bind_rows(dplyr::select(mq, c(estimator,name,ranking,period, estimate))) %>%
  mutate(initials = case_when(name=='scalia'~'AS',
                              name=='rehnquist'~'WR',
                              name == 'thomas'~'CT',
                              name == 'oconnor'~'SDO',
                              name == 'souter'~'DS',
                              name == 'kennedy'~'AK',
                              name == 'breyer'~'SB',
                              name == 'ginsburg'~'RBG',
                              name == 'stevens' ~'JPS'))


merits_plot <- ggplot(filter(ms, estimator=='mq'), aes(x=period, y = estimate, label=initials, group = initials))+geom_text()+
  scale_x_continuous('Term', breaks = seq(1994,2004,2))+ labs(title='A) Merits Docket')+
  scale_y_continuous('Conservatism')+
  theme_bw()+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"),
        axis.text.y = element_text(size=x.axis.tick.size, angle = 0),  axis.title.x=element_text(size=x.axis.label.size), 
        axis.text.x = element_text(size=y.axis.tick.size),  axis.title.y = element_text(size=y.axis.label.size))

sd_plot <-ggplot(filter(ms, estimator=='sd'), aes(x=period, y = estimate, label=initials, group = initials))+geom_text()+
  scale_x_continuous('Term', breaks = seq(1994,2004,2))+labs(title = 'B) Shadow Docket')+
  scale_y_continuous('Conservatism')+
  theme_bw()+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"),
        axis.text.y = element_text(size=x.axis.tick.size, angle = 0),  axis.title.x=element_text(size=x.axis.label.size), 
        axis.text.x = element_text(size=y.axis.tick.size),  axis.title.y = element_text(size=y.axis.label.size))



ms_wide <- ms %>% dplyr::select(estimator, estimate, name,period, initials) %>%
  pivot_wider(names_from = 'estimator', values_from = 'estimate')


cor(ms_wide$sd,ms_wide$mq)

cor_plot<-ggplot(ms_wide, aes(x=sd, y = mq))+geom_point()+geom_smooth(method = 'lm', se = F)+
  labs(x = 'Shadow Docket', y = 'Martin-Quinn', title = 'C) Correlation')+
  theme_bw()+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"),
        axis.text.y = element_text(size=x.axis.tick.size, angle = 0),  axis.title.x=element_text(size=x.axis.label.size), 
        axis.text.x = element_text(size=y.axis.tick.size),  axis.title.y = element_text(size=y.axis.label.size))+
  annotate('text', x = -1.5, y = 3, label = 'Correlation: 0.89', cex = 6)


rank_plot <-ggplot(filter(ms, period<2005), aes(x=period,y=ranking,shape = as.factor(estimator), color = as.factor(name), linewidth = name))+
  geom_line(aes(linetype = estimator))+ labs(title = 'D) Ranking')+
  scale_x_continuous('Term', breaks = seq(1994,2004,2))+
  scale_y_continuous('Ranking', breaks = c(1:9))+
  theme_bw()+
  scale_color_manual(values = c('ginsburg'='#00008B', 'oconnor'='purple','scalia'='#C41E3A'), na.value = 'white')+
  scale_linetype_manual(values = c('sd'='solid', 'mq'='dashed'))+
  scale_linewidth_manual(values = c('ginsburg'=1.5, 'oconnor'=1.5,'scalia'=1.5), na.value = 0)+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"),
        axis.text.y = element_text(size=x.axis.tick.size, angle = 0),  axis.title.x=element_text(size=x.axis.label.size), 
        axis.text.x = element_text(size=y.axis.tick.size),  axis.title.y = element_text(size=y.axis.label.size)) +
  annotate(geom = 'text', x = 1997, y = 1.75, label = 'Ginsburg', color = '#00008B', cex = 6)+
  annotate(geom = 'text', x = 1995, y = 8.5, label = 'Scalia', color = '#C41E3A', cex = 6)+
  annotate(geom = 'text', x = 2002, y = 5.5, label = "O'Connor", color = 'purple', cex = 6)+
  annotate(geom = 'text', x = 1999, y = 6.5, label = 'Shadow docket', color = 'black', cex = 6)+
  annotate(geom = 'text', x = 2000, y = 4.5, label = 'Merits docket (MQ)', color = 'black', cex = 6)

pdf("Plots/Figure_8.pdf", height=12, width=12)

grid.arrange( merits_plot, sd_plot,cor_plot, rank_plot, ncol=2)

dev.off()


########################################################################################################################
#Summary stats referenced in paper

#Number of case-based actions
nrow(filter(cda, non_case==0))

#Number of unique dockets
foo <- filter(cda, non_case==0)
length(unique(foo$docket_number))

#Percent cert
nrow(filter(foo, action_class=='Certiorari'))/nrow(cda)

#Number of actions in 2024
nrow(filter(foo, term==2024))

#Percent cert, cased-based dockets
nrow(filter(foo, action_class=='Certiorari'))/nrow(foo)

#IFP in 2006
foo <- filter(foo.cert, term ==2006, cert_type=='IFP')
foo$counts

#IFP in 2006
foo <- filter(foo.cert, term ==2024, cert_type=='IFP')
foo$counts

#Mean IFP grant rate
mean(filter(foo.cert_paid, cert_type=='IFP')$grant_rate)


#Excluding gvrs
foo.no_gvr<- cda %>%
  mutate(gvr = ifelse(action_class=='GVR',1,0)) %>%
  group_by(docket_number) %>%
  mutate(gvr = sum(gvr)) %>% ungroup() %>%
  filter(!is.na(cert_granted),!is.na(cert_type), gvr<1)%>% 
  group_by(term, cert_type) %>%
  mutate(denom = n(),
         grant_rate = sum(cert_granted)) %>%
  dplyr::select(action_class, term, denom, grant_rate, cert_type) %>%
  mutate(grant_rate=100*grant_rate/denom) %>% unique()


mean(filter(foo.no_gvr, cert_type=='IFP')$grant_rate)


# Percentage of cases that come from IFP petitions (excluding GVRs) 2020-2024
foo.share_no_gvr <- cda %>%
  mutate(gvr = ifelse(action_class=='GVR',1,0)) %>%
  group_by(docket_number) %>%
  mutate(gvr = sum(gvr)) %>% ungroup() %>%
  filter(cert_granted==1,!is.na(cert_type), gvr<1)%>% 
  group_by(term) %>%
  mutate(denom = n()) %>% filter(cert_type=='IFP') %>% 
  mutate( IFPs = n(),IFP_share = 100*IFPs/denom) %>% 
  dplyr::select(term, IFP_share) %>% unique() %>% filter(term>2019)
mean(foo.share_no_gvr$IFP_share)

#Years where habeas is granted
nrow(filter(foo.grantrate, action_class == 'Habeas Corpus', grant_rate!=0))


#Number of emergency petitions president (2025)
filter(foo.emergency_petitioner, gov_petitioner =='President',adjusted_year==2025)

#Number of emergency petitions president (2025)
filter(foo.emergency_petitioner, gov_petitioner =='President',adjusted_year==2020)

#Year of max dissents
max(foo.dissents.noIFP$counts)
n <-which.max(foo.dissents.noIFP$counts)
foo.dissents.noIFP$term[n]

