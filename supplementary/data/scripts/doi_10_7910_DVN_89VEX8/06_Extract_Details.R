rm(list=ls())

#Set working directory to the Replications folder

library(tidyverse)
library(stringr)
library(readxl)
library(writexl)



#Load docket_day actions
dockets <- read_excel('Data/Processed_Data/Docket_Day_Action.xlsx')

#Separately load admissions to the bar
admissions <- read_excel('Data/Processed_Data/SC_Bar_Admissions.xlsx')

#Merge datasets
dockets <- dockets %>%
  mutate(term = case_when(
    term>90~term+1900,
    .default = term+2000)) %>%
  filter(action !='Admissions to the Bar') %>%
  bind_rows(admissions) %>%
  group_by(cleaned_docket,date) %>%
  mutate(num_actions = n()) %>% ungroup()


#Coding of actions

dockets <- dockets %>%
  mutate(action_class = case_when(
    action == 'Admission'&cleaned_docket=='AttB'~'Admission',
    action == 'Certiorari Denied'&num_actions==1&!(str_detect(cleaned_docket,'A'))~'Certiorari',
    action == 'Certiorari Granted'&num_actions==1~'Certiorari',
    action == 'Habeas Corpus'&num_actions==1~'Habeas Corpus',
    action == 'Mandamus Denied'&num_actions==1~'Mandamus',
    action == 'Rehearings Denied'&num_actions==1~'Rehearings',
    str_detect(cleaned_docket,'D')&num_actions==1~'Attorney Discipline',
    action == 'Prohibition Denied'&num_actions==1~'Prohibition',
    action == 'Oral Argument'&specific_action=='Missing'~'Oral Argument',
    action == 'Oral Argument'&str_detect(specific_action,'divided argument')~'Oral Argument',
    action %in% c('Orders in Pending Cases','Prohibition Denied','Rehearings Denied','Mandamus Denied','Habeas Corpus','Certiorari Denied','Certiorari Granted','Admission')~case_when(
      str_detect(specific_action,'directed to file')~'Directed to File',
      str_detect(specific_action,'amend the')~'Amend',
      str_detect(specific_action,'directed to brief')~'Directed to Brief',
      str_detect(specific_action,'out of time')~'Out of time',
      str_detect(specific_action,'habeas corpus')~'Habeas Corpus',
      str_detect(specific_action,'mandamus')~'Mandamus',
      str_detect(specific_action, 'defer consideration')~'Defer action',
      str_detect(specific_action,' leave to file a petition for rehearing')~'Leave to File',
      str_detect(specific_action,'rehearing')~'Rehearings',
      str_detect(specific_action,'prohibition')~'Prohibition',
      str_detect(specific_action,'abeyance')~'Abeyance',
      str_detect(specific_action, 'in forma pauperis')&str_detect(specific_action, 'reconsider')~'Reconsider IFP',
      str_detect(specific_action, 'in forma pauperis')~'In forma pauperis',
      str_detect(specific_action, 'briefs{0,1} as amic[a-z]{1,2} curia')~'Amicus',
      str_detect(specific_action, 'views of the United States')~'Amicus',
      str_detect(specific_action, 'appointment\\s*of\\s*counsel')~'Appointment of Counsel',
      str_detect(specific_action, 'dismiss the writ of certiorari as improvidently granted')~'DIG',
      str_detect(specific_action, 'directed to submit')~'Directed to File',
      str_detect(specific_action, 'printing')~'Printing',
      str_detect(specific_action, 'paper')~'Printing',
      str_detect(specific_action, 'certiorari under seal')~'File Under Seal',  
      str_detect(specific_action, 'treated as a petition for writ of cert')~'Certiorari',
      str_detect(specific_action,'stay of execution')~'Stay',
      str_detect(specific_action, 'stay pending appeal')~'Stay',
      str_detect(specific_action, '[Aa]pplication for\\s*a{0,1}\\s*stay')~'Stay',
      str_detect(specific_action, 'treated as a petition')~'Stay',
      str_detect(specific_action, 'leave\\s*to\\s*intervene\\s*to\\s*file\\s*a\\s*petition\\s*for\\s*writ\\s*of\\s*cert')~'Leave to Intervene',
      str_detect(specific_action, '[Pp]etitions{0,1}\\s{0,1}for\\s{0,1}a{0,1}\\s{0,1}writs{0,1}\\s{0,1}of\\s{0,1}certiorari')~'Certiorari',
      str_detect(specific_action, 'vacate\\s{0,1}the\\s{0,1}injunction')~'Vacate',
      str_detect(specific_action, 'vacate\\s{0,1}injunction')~'Vacate',
      str_detect(specific_action, 'vacate\\s{0,1}the\\s{0,1}stay')~'Vacate Stay',
      str_detect(specific_action, 'stay')~'Stay',
      str_detect(specific_action, 'injunct')~'Injunction',
      str_detect(specific_action, 'Certiorari Granted')~'Certiorari',
      str_detect(specific_action, 'bail')~'Bail',
      str_detect(specific_action, 'seal')~'File Under Seal',
      str_detect(specific_action, 'leave to proceed as a veteran')~'Veteran',
      str_detect(specific_action, 'leave to proceed as a seaman')~'Seaman',
      str_detect(specific_action, 'River\\s{0,1}Master')~'River Master',
      str_detect(specific_action, 'Special\\s{0,1}Master')~'Special Master',
      str_detect(specific_action, 'expedite')~'Expedite Action',
      str_detect(specific_action, 'divided argument')~'Oral Argument',
      str_detect(specific_action, 'certiﬁcate of appealability')~'Certiﬁcate of Appealability',
      str_detect(specific_action, 'retax')~'Retax',
      str_detect(specific_action, 'fees')~'Costs and Fees',
      str_detect(specific_action, '[Bbill]s{0,1}\\s{0,1}of\\s{0,1}[cC]omplaint')~'Bill of Complaint',
      str_detect(specific_action, 'leave to file')~'Leave to File',
      str_detect(specific_action, 'defer')~'Defer action',
      str_detect(specific_action, 'briefing schedule')~'Briefing',
      str_detect(specific_action, 'briefing proposal')~'Briefing',
      str_detect(specific_action, 'ﬁle a brief')~'Briefing',
      str_detect(specific_action, 'strike')~'Strike',
      str_detect(specific_action, 'leave to intervene')~'Leave to Intervene',
      str_detect(specific_action,'sanctions')~'Sanctions',
      str_detect(specific_action,'damage')~'Damages',
      str_detect(specific_action,'certificate of appealability')~'Certificate of Appealability',
      str_detect(specific_action,'divided\\s*argument')~'Divided Argument',
      str_detect(specific_action,'pro\\s{0,1}hac\\s{0,1}vice')~'Pro Hac Vice',
      str_detect(specific_action,'substitut')~'Substitution',
      str_detect(specific_action, 'consolidate')~'Consolidate',
      str_detect(specific_action, 'oral argument')~'Oral Argument',
      str_detect(action_text,'invited to brief and argue')~'Amicus',
      str_detect(action_text,'probable jurisdiction\\s{0,1}[a-z]{2}\\s{0,1}noted')~'Probable Jurisdiction',
      str_detect(action_text, 'allotted for oral argument')~'Oral Argument',
      str_detect(action_text, 'time for oral argument')~'Oral Argument',
      str_detect(action_text, 'and\\s{0,1}[a-z]{0,3}\\s{0,1}petitions{0,1}\\s{0,1}[a-z]{0,3}\\s{0,1}submitted')~'DUPLICATE',
      .default = 'Other'
    ),
    action %in% c('Opinions Per Curiam','Summary Disposition')~case_when(
      str_detect(specific_action, 'denying the petition for a writ of certiorari is vacated')~'Vacate',
      str_detect(specific_action, 'in forma pauperis')~'In forma pauperis',
      str_detect(specific_action,'amend the')~'Amend',
      str_detect(specific_action, 'Judgments{0,1} vacated and cases{0,1} remanded')&str_detect(specific_action, 'U. S.')~'GVR',
      str_detect(specific_action, 'The judgments{0,1} [a-z]{2,3} vacated and the cases{0,1} [a-z]{2,3} remanded')&
        str_detect(specific_action, 'U. S.')~'GVR',
      str_detect(specific_action,'out of time')~'Out of time',
      str_detect(specific_action,'affirmed in part')~'Affirmed in Part',
      str_detect(specific_action,'stay of execution')~'Stay',
      str_detect(specific_action,'affirmed\\s{0,1}with\\s{0,1}respect')~'Affirmed in Part',
      str_detect(specific_action,'Judgment reversed')~'Reversal',
      str_detect(specific_action, 'The judgment is affirmed')~'Affirmed',
      str_detect(specific_action, 'Judgments{0,1} vacated and cases{0,1} remanded')~'Reversal',
      str_detect(specific_action, 'The judgment is vacated and the case is remanded')~'Reversal',
      str_detect(specific_action, 'The judgment is vacated and the cases are remanded')~'Reversal',
      str_detect(specific_action, '[Jj]udgment')&str_detect(specific_action, 'vacate')&str_detect(specific_action, 'remand')~'Reversal',
      str_detect(specific_action,'Judgment affirmed')~'Affirmed',
      str_detect(specific_action,'oral argument')~'Oral Argument',
      str_detect(specific_action,'divided argument')~'Oral Argument',
      str_detect(specific_action,'Petitions{0,1} for rehearing')~'Rehearings',
      str_detect(specific_action, 'treated as a petition for writ of cert')~'Certiorari',
      str_detect(specific_action,'stay of execution')~'Stay',
      str_detect(specific_action, 'stay pending appeal')~'Stay',
      str_detect(specific_action, '[Aa]pplication for\\s*a{0,1}\\s*stay')~'Stay',
      str_detect(specific_action, 'treated as a petition')~'Stay',
      str_detect(specific_action,'Petitions{0,1} for writs{0,1} of certiorari')~'Certiorari',
      str_detect(specific_action, 'petitions{0,1} for\\s{0,1}a{0,1} writs{0,1} of certiorari')~'Certiorari',
      str_detect(specific_action, 'printing')~'Printing',
      str_detect(specific_action, 'appointment\\s*of\\s*counsel')~'Appointment of Counsel',
      str_detect(specific_action, 'vacate\\s{0,1}the\\s{0,1}injunction')~'Vacate',
      str_detect(specific_action, 'vacate\\s{0,1}injunction')~'Vacate',
      str_detect(specific_action, 'vacate\\s{0,1}the\\s{0,1}stay')~'Vacate Stay',
      str_detect(specific_action, 'stay')~'Stay',
      str_detect(specific_action, 'injunct')~'Injunction',
      str_detect(specific_action, 'brief as amic[a-z]{1,2} curiae')~'Amicus',
      str_detect(specific_action, 'views of the United States')~'Amicus',
      str_detect(specific_action,'habeas corpus')~'Habeas Corpus',
      .default = 'Other'
    ),
    .default = 'Other'
  ), #Coding relief
  relief = case_when(
    str_detect(action,'Denied')&num_actions==1~'Denied',
    str_detect(action,'Granted')&num_actions==1~'Granted',
    str_detect(action,'Granted')&num_actions==1~'Granted',
    action_class=='Amicus'&str_detect(specific_action,'Solicitor')~'SG',
    action_class=='Oral Argument'&str_detect(specific_action,'Solicitor')~'SG',
    action_class=='Habeas Corpus'& str_detect(action_text,'dismiss')~'Dismissed',
    action_class=='Mandamus'& str_detect(action_text,'dismiss')~'Dismissed',
    action_class=='Prohibition'& str_detect(action_text,'dismiss')~'Dismissed',
    action_class=='Certiorari'& str_detect(action_text,'dismiss')~'Dismissed',
    action_class=='Amicus'& str_detect(action_text,'invited to brief and argue')~NA,
    action_class=='Consolidate'& str_detect(specific_action,'[Cc]ases\\s{0,1}are\\s{0,1}consolidated')~NA,
    action_class=='Consolidate'& str_detect(specific_action,' applications\\s{0,1}are\\s{0,1}consolidated')~NA,
    action_class=='Oral Argument'&specific_action=='Missing'~NA,
    str_detect(cleaned_docket,'D')~case_when(
      str_detect(action_text,'requested to resign')~'Resigned',
      str_detect(action_text,'suspend')~'Suspended',
      str_detect(specific_action,'reconsider')|str_detect(specific_action,'vacate')~'Challenge',
      str_detect(action_text,'Disbarment\\s{0,1}order\\s{0,1}entered')~'Disbarred',
      .default='Other'),
    action_class %in% c('Reversal','Affirmed','GVR','River Master','Special Master','Admission')~NA,
    .default=case_when(
      str_detect(action_text, 'pauperis\\s{0,1}and\\s{0,1}the\\s{0,1}petition\\s{0,1}for\\s{0,1}writ\\s{0,1}of\\s{0,1}certiorari')&str_detect(action_text,'granted')~'Granted',
      str_detect(action_text, 'pauperis\\s{0,1}and\\s{0,1}petition\\s{0,1}for\\s{0,1}writ\\s{0,1}of\\s{0,1}certiorari')&str_detect(action_text,'granted')~'Granted',
      str_detect(specific_action,' granted in part and denied in part')~'Granted/Denied',
      str_detect(specific_action,'granted is denied')~'Denied',
      str_detect(specific_action,'granted\\s{0,1}denied')~'Denied',
      str_detect(specific_action,'granted')~'Granted',
      str_detect(specific_action,'Granted')~'Granted',
      str_detect(specific_action,'denied')~'Denied',
      .default = 'Other'
    )
  )) %>%
  filter(action!='Opinion') %>%
  filter(cleaned_docket!='nan')

#Docket-Day_actions missing an action_class
action_class_errors <- dockets %>%
  filter(action_class=='Other') 
write_xlsx(action_class_errors,'Data/Processed_Data/Coding_Errors_Action_Class.xlsx')

#Hand coded fixes to errors
fixed_errors <- read_excel('Data/Processed_Data/Coding_Errors_Action_Class_Fixed.xlsx')


#Add in fixed action classes and remove some found errors
completed_dockets <- dockets %>%
  mutate(action_class=case_when(
    cleaned_docket=='12–5401'&term==2014~'ERROR',
    cleaned_docket=='20–7796'&action_class=='In forma pauperis'~'DUPLICATE',
    cleaned_docket=='21–1484'&action_class=='Strike'~'Amicus',
    cleaned_docket=='22–51'&action_class=='Strike'~'Amicus',
    cleaned_docket=='15A48'&action_class=='In forma pauperis'~'Certiorari',
    cleaned_docket=='21A85'&action_class=='Injunction'~'DUPLICATE',
    cleaned_docket=='21A375'&action_class=='Stay'&relief=='Granted'~'Certiorari',
    cleaned_docket=='21A376'&action_class=='Stay'&relief=='Granted'~'Certiorari',
    cleaned_docket=='14A1065'~'Stay',
    cleaned_docket=='24A949'&action_class=='Vacate'~'DUPLICATE',
    cleaned_docket=='24A949'&action_class=='Stay'~'Vacate',
    cleaned_docket=='13-9085'&action_class == 'GVR'~'Reversal',
    cleaned_docket=='00–8452'&action_class=='Certiorari'~'DUPLICATE',
    cleaned_docket=='00–8452'&action_class=='In forma pauperis'~'DUPLICATE',
    cleaned_docket=='20A55'&action_class=='Certiorari'&date=='2020-10-05'~'ERROR',
    cleaned_docket=='15–6551'&specific_action=='Petition for writ of certiorarito the Supreme Court of Florida denied. Justice Breyer dissents from denial of the application for stay of execution'~'Certiorari',
    .default=action_class)) %>%
  filter(action_class!='Other') %>%
  bind_rows(fixed_errors) %>%
  filter(!action_class %in% c('DUPLICATE','ERROR','Opinion')) %>%
  mutate(relief=ifelse(action_class %in% c('Reversal','Affirmed','Affirmed in Part','GVR','River Master',
                                           'Special Master','Admission','Directed to Brief','Directed to File',
                                           'Probable Jurisdiction', 'Jurisdiction Postponed','Lack of Quorum'),
                NA,relief))



#sort(unique(completed_dockets$action_class))

#Docket-Day-actions missing relief coding
relief_errors <- completed_dockets %>%
  filter(relief=='Other') 
write_xlsx(relief_errors,'Data/Processed_Data/Coding_Errors_Relief.xlsx')


#Load in fixed relief coding
fixed_relief <- read_excel('Data/Processed_Data/Coding_Errors_Relief_Fixed.xlsx')


#Merge in fixed relief coding
completed_dockets <- completed_dockets %>%
  filter(relief!='Other'|is.na(relief)) %>%
  bind_rows(fixed_relief) %>%
  filter(!action_class %in% c('DUPLICATE','ERROR')) %>%
  mutate(relief=ifelse(action_class %in% c('Reversal','Affirmed','Affirmed in Part','GVR','River Master',
                                           'Special Master','Admission','Directed to Brief','Directed to File',
                                           'Probable Jurisdiction', 'Jurisdiction Postponed','Lack of Quorum'),
                       NA,relief)) %>%
  mutate(relief = ifelse(is.na(relief),'NA',relief),
         relief = ifelse(action_class=="Dismiss"&relief=='Dismissed','NA',relief )) %>% 
  dplyr::select(cleaned_docket,term,date,text,action_text,action_class,relief,specific_action) %>% unique()

#Relabel solicitor general actions
completed_dockets <- completed_dockets %>%
  mutate(action_class=case_when(
    action_class == 'Amicus'&relief=='SG'~'Solicitor General',
    action_class == 'Oral Argument' & relief=='SG'~'Solicitor General',
    action_class %in% c('Oral Argument','Amicus')&relief %in% c('Granted','Denied')&str_detect(specific_action,'[sS]olicitor')~'Solicitor General',
    .default = action_class),
    relief = ifelse(action_class=='Solicitor General'&relief=='SG','NA',relief))

#Relabel Cert improvidently granted as DIG
completed_dockets <- completed_dockets %>%
  mutate(action_class=case_when(
    action_class == 'Certiorari'&str_detect(action_text,'improvidently granted')~'DIG',
    .default = action_class),
    relief = ifelse(action_class=='DIG','NA',relief))


#Change summary reversal coding
completed_dockets <- completed_dockets %>%
  mutate(action_class=case_when(
    action_class == 'Reversal'&cleaned_docket == '13-9085' ~'Reversal',
    action_class == 'Reversal'&str_detect(action_text,'U.\\s*S.')~'GVR',
    action_class == 'Reversal'&str_detect(action_text,'[pP]er\\s*curiam')~'Reversal',
    action_class == 'Reversal'&str_detect(action_text,'opinion of this Court')~'Reversal',
    action_class == 'Reversal'~'Vacate and Remand',
    .default = action_class))









#Remove state SGs from Solicitor General action Class
states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
  "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
  "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
  "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
  "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
  "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
  "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
)

sg_errors <- completed_dockets %>%
  filter(action_class=='Solicitor General') %>%
  mutate(state_sg=F)

for (state in states){
  sg_errors <-mutate(sg_errors,state_sg=ifelse(str_detect(action_text,paste('Solicitor General of',state,sep = ' ')),T,state_sg))
}

sg_errors <-mutate(sg_errors,state_sg=ifelse(str_detect(action_text,'for Texas')&date!="2020-08-24",T,state_sg)) %>%
  filter(state_sg==T)

completed_dockets <- completed_dockets %>%
  left_join(dplyr::select(sg_errors,c('cleaned_docket','date','action_class','state_sg')), by = c('cleaned_docket','date','action_class')) %>%
  mutate(action_class = ifelse(!is.na(state_sg),'Oral Argument',action_class)) %>%
  dplyr::select(-state_sg)


#Remove oral arguments
completed_dockets <- completed_dockets %>%
  mutate(action_class = ifelse((action_class=='Oral Argument'|action_class=='Solicitor General')&str_detect(action_text,'[aA]rgued by'),'Arguments',action_class))%>%
  filter(action_class!='Arguments')


#Non-case, cert, cert-type, year, ea 
completed_dockets <- completed_dockets %>%
  mutate(non_case = ifelse(action_class %in% c('Admission','Attorney Discipline'),1,0),
         relief_granted = case_when(
           relief=='Granted'~1,
           relief== 'Denied'~0,
           .default = NA
         ) ,
         cert_granted = ifelse(action_class=='Certiorari',relief_granted, NA),
         year = as.numeric(substring(completed_dockets$date,1,4)),
         number = as.integer(substring(cleaned_docket,4)),
         cert_type=case_when(
          str_detect(cleaned_docket,'–')&number<5000&action_class=='Certiorari'~'Paid',
          str_detect(cleaned_docket,'–')& number>=5000&action_class=='Certiorari'~'IFP',
           .default=NA
         ),
         emergency_application = case_when(
           term<2003~NA,
           non_case==1~NA, 
           action_class %in% c('Stay', 'Injunction', 'Vacate Stay', 'Vacate')&str_detect(action_text,'\\d\\dA')~1,
           .default=0),
         discipline_action = ifelse(action_class=='Attorney Discipline', relief, NA),
         relief = case_when(
           action_class == 'Attorney Discipline'~NA,
           relief == 'Misc'~NA,
           relief == 'Other'~NA,
           relief == 'Schedule'~NA,
           relief == 'Separate'~NA,
           relief == 'SG'~NA,
           relief == 'Third-Party'~NA,
           relief == 'NA'~NA,
           relief == 'Dismiss'~'Dismissed',
           .default=relief)) %>%
  dplyr::select(-number)



#Fix some leave to file errors 
completed_dockets <- completed_dockets %>% 
  mutate(action_class = case_when(
    cleaned_docket=='90–1397'&action_class=='Leave to File'~'Rehearings',
    cleaned_docket=='97–369'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='99–1224'&action_class=='Leave to File'~'Rehearings',
    cleaned_docket=='07–421'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='07–533'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='07–538'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='07–538'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='07–538'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='07–585'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='07–636'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='08–751'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='08–1375'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='08–1375'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='08–1375'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='08–1375'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='12–112'&action_class=='Leave to File'~'File Under Seal',
    cleaned_docket=='12–1342'&action_class=='Leave to File'~'Amicus',
    cleaned_docket=='13–6739'&action_class=='Leave to File'~'File Under Seal',
    cleaned_docket=='14–10470'&action_class=='Leave to File'~'File Under Seal',
    cleaned_docket=='02–1433'&action_class=='Leave to File'~'Rehearings',
    cleaned_docket=='92–8908'&action_class=='Leave to File'~'Rehearings',
    cleaned_docket=='04–7271'&action_class=='Leave to File'~'Rehearings',
    cleaned_docket=='09–7870'&action_class=='Leave to File'~'Rehearings',
    .default = action_class),
    action_text = str_replace(action_text, 'executi on','execution'))


##########################################
completed_dockets <-completed_dockets %>%
  mutate(date_unix =as.numeric(as.POSIXct(date,tz='UTC')),
        action_class_numeric = as.numeric(as.factor(action_class)),
        relief_numeric = as.numeric(as.factor(relief)),
        discipline_action_numeric =  as.numeric(as.factor(discipline_action)), 
        emergency_application = ifelse(emergency_application,1,0),
        death_penalty = ifelse(str_detect(action_text,'execution'),1,0)) %>%
  group_by(cleaned_docket) %>%
  mutate(death_penalty =max(death_penalty)) %>% ungroup() %>%
  mutate(death_penalty = ifelse(non_case==1,NA,death_penalty),
         dissent = ifelse(str_detect(action_text,'dissent'),1,0),
         disagreement = case_when(str_detect(action_text,'would grant')~1,
                                   str_detect(action_text,'would deny')~1,
                                   dissent==1~1,
                                   .default=0))

###########
#Parse Petitioner Names

completed_dockets <- completed_dockets %>%
  mutate(petitioner = str_match(text, '[–\\d\\)]\\. (.*?), Petitioner')[,2],
         applicant =  str_match(text, '[-\\d\\)]\\. (.*?), Applicant')[,2],
         original = str_match(text, 'Original\\.(.*?)v\\.')[,2],
         petitioner = case_when(is.na(petitioner)&!is.na(applicant)~applicant,
                                is.na(petitioner)&!is.na(original)~original,
                                is.na(petitioner)~str_match(text, '[-\\d\\)]\\s{0,1}\\. (.*?)v\\.')[,2],
                                .default = petitioner),
         respondent = str_match(text, 'Petitioners{0,1}\\s{0,1}v\\.(.*?)[\\.;]')[,2],
         applicant = str_match(text, 'Applicants{0,1}\\s{0,1}v\\.(.*?)[\\.;]')[,2],
         petition = str_match(text, 'Petitioners{0,1}\\s{0,1}v\\.(.*?)[\\.;] Petition')[,2],
         motion = str_match(text, 'Petitioners{0,1}\\s{0,1}v\\.(.*?)[\\.;] The motion')[,2],
         motion1 = str_match(text, 'Petitioners{0,1}\\s{0,1}v\\.(.*?)[\\.;] Motion')[,2],
         naive = str_match(text,'v\\.(.{10}.*?)[\\.;]')[,2],
         respondent = case_when(is.na(respondent)&!is.na(applicant)~applicant,
                                nchar(respondent)<15&!is.na(petition)&nchar(petition)<75~petition,
                                nchar(respondent)<15&!is.na(motion)&nchar(motion)<75~motion,
                                nchar(respondent)<15&!is.na(motion1)&nchar(motion1)<75~motion1,
                                (is.na(respondent)|nchar(respondent)<15)&nchar(naive)<75~naive,
                                       .default = respondent)
  )
       
### Select relevant columns
completed_dockets <- completed_dockets %>%
  dplyr::select(cleaned_docket,term,date,date_unix,year,text,action_text,action_class,action_class_numeric,relief,relief_numeric,
         relief_granted,emergency_application,cert_granted,cert_type,death_penalty, dissent, disagreement, non_case, discipline_action,discipline_action_numeric, petitioner, respondent)

colnames(completed_dockets)[1]<- 'docket_number'


#Write data to file
write_xlsx(completed_dockets,'Data/Processed_Data/Categorized_Docket_actions.xlsx')




