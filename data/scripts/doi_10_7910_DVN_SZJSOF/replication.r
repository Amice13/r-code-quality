# load up libraries which will be needed for the plot

library(ggplot2)
library(scales)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(stargazer) 
library(MASS)

setwd("Z:/projects/covid19/nurses/code")

nhs <- read.csv("strike_data.csv", header=TRUE)

# create doctors and nurses frames

nurses <- subset(nhs, !is.na(strikesNurses))

doctors <- subset(nhs, !is.na(strikesJuniorDoctors))

# how many respondents for each? 

length(nurses$id)

length(doctors$id)

# rename and recode
# we will flip the scale
# important that we do this here, 
# after separating doctors and nurses, as we
# have removed the NAs; otherwise, it would
# try to subtract NA from 8

nurses$supportNurses <- 8 - nurses$strikesNurses
doctors$supportDoctors <- 8 - doctors$strikesJuniorDoctors

# make tables 

nursesSurveys <- c( 
	round(length(nurses$supportNurses[nurses$supportNurses > 4]) / length(nurses$supportNurses)*100, 2),
	round(mean(nurses$age), 2), 
	round(length(nurses$profile_gender[nurses$profile_gender == 1]) / length(nurses$profile_gender)*100, 2),
	round(length(nurses$profile_gender[nurses$profile_gender == 2]) / length(nurses$profile_gender)*100, 2),
	round(length(nurses$ownHealth[nurses$ownHealth > 3]) / length(nurses$ownHealth)*100, 2),
	round(length(nurses$disabled[nurses$disabled == 1 & !is.na(nurses$disabled)]) / length(nurses$disabled[!is.na(nurses$disabled)])*100, 2), 
	round(length(nurses$ethnicMinority[nurses$ethnicMinority == 1 & !is.na(nurses$ethnicMinority)]) / length(nurses$ethnicMinority[!is.na(nurses$ethnicMinority)])*100, 2), 
	round(length(nurses$leftRight[nurses$leftRight < 5 & !is.na(nurses$leftRight)]) / length(nurses$leftRight[!is.na(nurses$leftRight)])*100, 2), 
	round(length(nurses$leftRight[nurses$leftRight == 5 & !is.na(nurses$leftRight)]) / length(nurses$leftRight[!is.na(nurses$leftRight)])*100, 2), 
	round(length(nurses$leftRight[nurses$leftRight > 5 & !is.na(nurses$leftRight)]) / length(nurses$leftRight[!is.na(nurses$leftRight)])*100, 2), 
	round(length(nurses$conVote19[nurses$conVote19 == 1 & !is.na(nurses$conVote19)]) / length(nurses$conVote19[!is.na(nurses$conVote19)])*100, 2), 
	round(length(nurses$labVote19[nurses$labVote19 == 1 & !is.na(nurses$labVote19)]) / length(nurses$labVote19[!is.na(nurses$labVote19)])*100, 2), 
	round(length(nurses$ldVote19[nurses$ldVote19 == 1 & !is.na(nurses$ldVote19)]) / length(nurses$ldVote19[!is.na(nurses$ldVote19)])*100, 2), 
	round(length(nurses$trustNHS[nurses$trustNHS == 1]) / length(nurses$trustNHS)*100, 2), 
	length(nurses$id)
	)
	
doctorsSurveys <- c( 
	round(length(doctors$supportDoctors[doctors$supportDoctors > 4]) / length(doctors$supportDoctors)*100, 2),
	round(mean(doctors$age), 2), 
	round(length(doctors$profile_gender[doctors$profile_gender == 1]) / length(doctors$profile_gender)*100, 2),
	round(length(doctors$profile_gender[doctors$profile_gender == 2]) / length(doctors$profile_gender)*100, 2),
	round(length(doctors$ownHealth[doctors$ownHealth > 3]) / length(doctors$ownHealth)*100, 2),
	round(length(doctors$disabled[doctors$disabled == 1 & !is.na(doctors$disabled)]) / length(doctors$disabled[!is.na(doctors$disabled)])*100, 2), 
	round(length(doctors$ethnicMinority[doctors$ethnicMinority == 1 & !is.na(doctors$ethnicMinority)]) / length(doctors$ethnicMinority[!is.na(doctors$ethnicMinority)])*100, 2), 
	round(length(doctors$leftRight[doctors$leftRight < 5 & !is.na(doctors$leftRight)]) / length(doctors$leftRight[!is.na(doctors$leftRight)])*100, 2), 
	round(length(doctors$leftRight[doctors$leftRight == 5 & !is.na(doctors$leftRight)]) / length(doctors$leftRight[!is.na(doctors$leftRight)])*100, 2), 
	round(length(doctors$leftRight[doctors$leftRight > 5 & !is.na(doctors$leftRight)]) / length(doctors$leftRight[!is.na(doctors$leftRight)])*100, 2), 
	round(length(doctors$conVote19[doctors$conVote19 == 1 & !is.na(doctors$conVote19)]) / length(doctors$conVote19[!is.na(doctors$conVote19)])*100, 2), 
	round(length(doctors$labVote19[doctors$labVote19 == 1 & !is.na(doctors$labVote19)]) / length(doctors$labVote19[!is.na(doctors$labVote19)])*100, 2), 
	round(length(doctors$ldVote19[doctors$ldVote19 == 1 & !is.na(doctors$ldVote19)]) / length(doctors$ldVote19[!is.na(doctors$ldVote19)])*100, 2), 
	round(length(doctors$trustNHS[doctors$trustNHS == 1]) / length(doctors$trustNHS)*100, 2), 
	length(doctors$id)
	)

categories <- c(
	"Support strikes (%)", 
	"Mean age",
	"Men (%)", 
	"Women (%)", 
	"In good health (%)", 
	"Disabled (%)", 
	"Ethnic minority (%)", 
	"Left wing (%)", 
	"Middle (%)", 
	"Right wing (%)", 
	"Voted Conservative 2019 (%)", 
	"Voted Labour 2019 (%)", 
	"Voted Liberal Democrat 2019 (%)", 
	"Trust the NHS (%)", 
	"N"
	)
	

	
descriptives <- data.frame(categories, nursesSurveys, doctorsSurveys)

descriptives

railWorkers <- c(
	length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 4 & nhs$wave==6]) / length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 8 & nhs$wave==6]), 
	length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 4 & nhs$wave==7]) / length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 8 & nhs$wave==7]), 
	length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 4 & nhs$wave==8]) / length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 8 & nhs$wave==8]), 
	length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 4 & nhs$wave==9]) / length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 8 & nhs$wave==9]), 
	length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 4 & nhs$wave==10]) / length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 8 & nhs$wave==10]), 
	length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 4 & nhs$wave==11]) / length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 8 & nhs$wave==11]), 
	length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 4 & nhs$wave==16]) / length(nhs$strikesRailWorkers[!is.na(nhs$strikesRailWorkers) & nhs$strikesRailWorkers < 8 & nhs$wave==16])
	)
	
nurses <- c(
	length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 4 & nhs$wave==6]) / length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 8 & nhs$wave==6]), 
	length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 4 & nhs$wave==7]) / length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 8 & nhs$wave==7]), 
	length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 4 & nhs$wave==8]) / length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 8 & nhs$wave==8]), 
	length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 4 & nhs$wave==9]) / length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 8 & nhs$wave==9]), 
	length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 4 & nhs$wave==10]) / length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 8 & nhs$wave==10]), 
	length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 4 & nhs$wave==11]) / length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 8 & nhs$wave==11]), 
	length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 4 & nhs$wave==16]) / length(nhs$strikesNurses[!is.na(nhs$strikesNurses) & nhs$strikesNurses < 8 & nhs$wave==16])
	)
	
postalWorkers <- c(
	length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 4 & nhs$wave==6]) / length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 8 & nhs$wave==6]), 
	length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 4 & nhs$wave==7]) / length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 8 & nhs$wave==7]), 
	length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 4 & nhs$wave==8]) / length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 8 & nhs$wave==8]), 
	length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 4 & nhs$wave==9]) / length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 8 & nhs$wave==9]), 
	length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 4 & nhs$wave==10]) / length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 8 & nhs$wave==10]), 
	length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 4 & nhs$wave==11]) / length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 8 & nhs$wave==11]), 
	length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 4 & nhs$wave==16]) / length(nhs$strikesPostalWorkers[!is.na(nhs$strikesPostalWorkers) & nhs$strikesPostalWorkers < 8 & nhs$wave==16])
	)
	
airportWorkers <- c(
	length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 4 & nhs$wave==6]) / length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 8 & nhs$wave==6]), 
	length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 4 & nhs$wave==7]) / length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 8 & nhs$wave==7]), 
	length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 4 & nhs$wave==8]) / length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 8 & nhs$wave==8]), 
	length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 4 & nhs$wave==9]) / length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 8 & nhs$wave==9]), 
	length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 4 & nhs$wave==10]) / length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 8 & nhs$wave==10]), 
	length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 4 & nhs$wave==11]) / length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 8 & nhs$wave==11]), 
	length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 4 & nhs$wave==16]) / length(nhs$strikesAirportWorkers[!is.na(nhs$strikesAirportWorkers) & nhs$strikesAirportWorkers < 8 & nhs$wave==16])
	)
	
teachers <- c(
	length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 4 & nhs$wave==6]) / length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 8 & nhs$wave==6]), 
	length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 4 & nhs$wave==7]) / length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 8 & nhs$wave==7]), 
	length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 4 & nhs$wave==8]) / length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 8 & nhs$wave==8]), 
	length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 4 & nhs$wave==9]) / length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 8 & nhs$wave==9]), 
	length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 4 & nhs$wave==10]) / length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 8 & nhs$wave==10]), 
	length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 4 & nhs$wave==11]) / length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 8 & nhs$wave==11]), 
	length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 4 & nhs$wave==16]) / length(nhs$strikesTeachers[!is.na(nhs$strikesTeachers) & nhs$strikesTeachers < 8 & nhs$wave==16])
	)
	
civilServants <- c(
	length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 4 & nhs$wave==6]) / length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 8 & nhs$wave==6]), 
	length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 4 & nhs$wave==7]) / length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 8 & nhs$wave==7]), 
	length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 4 & nhs$wave==8]) / length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 8 & nhs$wave==8]), 
	length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 4 & nhs$wave==9]) / length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 8 & nhs$wave==9]), 
	length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 4 & nhs$wave==10]) / length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 8 & nhs$wave==10]), 
	length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 4 & nhs$wave==11]) / length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 8 & nhs$wave==11]), 
	length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 4 & nhs$wave==16]) / length(nhs$strikesCivilServants[!is.na(nhs$strikesCivilServants) & nhs$strikesCivilServants < 8 & nhs$wave==16])
	)
	
uniLecturers <- c(
	length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 4 & nhs$wave==6]) / length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 8 & nhs$wave==6]), 
	length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 4 & nhs$wave==7]) / length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 8 & nhs$wave==7]), 
	length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 4 & nhs$wave==8]) / length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 8 & nhs$wave==8]), 
	length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 4 & nhs$wave==9]) / length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 8 & nhs$wave==9]), 
	length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 4 & nhs$wave==10]) / length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 8 & nhs$wave==10]), 
	length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 4 & nhs$wave==11]) / length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 8 & nhs$wave==11]), 
	length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 4 & nhs$wave==16]) / length(nhs$strikesUniversityLecturers[!is.na(nhs$strikesUniversityLecturers) & nhs$strikesUniversityLecturers < 8 & nhs$wave==16])
	)
	
juniorDoctors <- c(
	length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 4 & nhs$wave==6]) / length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 8 & nhs$wave==6]), 
	length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 4 & nhs$wave==7]) / length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 8 & nhs$wave==7]), 
	length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 4 & nhs$wave==8]) / length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 8 & nhs$wave==8]), 
	length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 4 & nhs$wave==9]) / length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 8 & nhs$wave==9]), 
	length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 4 & nhs$wave==10]) / length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 8 & nhs$wave==10]), 
	length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 4 & nhs$wave==11]) / length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 8 & nhs$wave==11]), 
	length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 4 & nhs$wave==16]) / length(nhs$strikesJuniorDoctors[!is.na(nhs$strikesJuniorDoctors) & nhs$strikesJuniorDoctors < 8 & nhs$wave==16])
	)
	
date <- c("Dec 2022", "Jan 2023", "Feb 2023", "Mar 2023", "Apr 2023", "May 2023", "Oct 2023") 

strikeSupport <- data.frame(date, railWorkers, nurses, postalWorkers, airportWorkers, teachers, civilServants, uniLecturers, juniorDoctors) 

strikeSupport$date <- my(strikeSupport$date)

# Reshape the data for plotting
strike_support_data_long <- pivot_longer(strikeSupport, cols = -date, names_to = "group", values_to = "support")

# Determine the order of the groups for the legend based on December 2022 values, ensuring juniorDoctors is second
support_levels <- strikeSupport %>%
  filter(date == min(date)) %>%
  pivot_longer(cols = -date, names_to = "group", values_to = "support") %>%
  arrange(desc(support))

# Extract the group names in descending order of support, but place 'juniorDoctors' right after 'nurses'
legend_order <- support_levels$group
# Move 'juniorDoctors' to the second position in the legend order
legend_order <- c("nurses", "juniorDoctors", setdiff(legend_order, c("nurses", "juniorDoctors")))

# Map current group names to formatted names with capital letters and spaces
formatted_names <- setNames(c("Nurses", "Junior Doctors", "Postal Workers", "Teachers", "Rail Workers", "Airport Workers", "Civil Servants", "University Lecturers"), 
                            c("nurses", "juniorDoctors", "postalWorkers", "teachers", "railWorkers", "airportWorkers", "civilServants", "uniLecturers"))

# Apply the formatted names to the legend order and the color palette
legend_order_formatted <- formatted_names[legend_order]
color_palette <- setNames(brewer.pal(n = min(8, length(legend_order_formatted)), name = "Set2"), legend_order_formatted)

# Convert the 'group' column to a factor with levels in the order we want and labels as formatted names
strike_support_data_long$group <- factor(strike_support_data_long$group, levels = legend_order, labels = legend_order_formatted)



# Create the plot with smoothed lines only
plot <- ggplot(strike_support_data_long, aes(x = date, y = support, group = group, color = group)) +
  geom_rect(aes(xmin = as.Date('2023-06-01'), xmax = as.Date('2023-08-31'), ymin = -Inf, ymax = +Inf), fill = "#dddddd", alpha = 0.3, inherit.aes = FALSE) +
  geom_smooth(size = 1, method = "loess", span = 1, se = FALSE) +  # Smooth lines, with loess by default
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = color_palette) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  labs(x = "Date", y = "Level of Support (%)", title = "Public Support for Strike", color = "Groups") +
  theme_minimal() +
  theme(legend.key = element_blank())

# Display the plot
plot

# Save the plot in both PNG and PDF formats
png_file_path <- 'smoothing_loess_1_no_points.png'
pdf_file_path <- 'smoothing_loess_1_no_points.pdf'
ggsave(png_file_path, plot, width = 15, height = 8, units = "in", bg="white")
ggsave(pdf_file_path, plot, width = 15, height = 8, units = "in")

# models 

nhs$ageSquared <- nhs$age^2

# flip the scales on support for strikes
# so that higher number means higher support

nhs$strikesRailWorkers <- 8 - nhs$strikesRailWorkers
nhs$strikesNurses <- 8 - nhs$strikesNurses
nhs$strikesPostalWorkers <- 8 - nhs$strikesPostalWorkers 
nhs$strikesAirportWorkers <- 8 - nhs$strikesAirportWorkers
nhs$strikesTeachers <- 8 - nhs$strikesTeachers
nhs$strikesCivilServants <- 8 - nhs$strikesCivilServants
nhs$strikesUniversityLecturers <- 8 - nhs$strikesUniversityLecturers
nhs$strikesJuniorDoctors <- 8 - nhs$strikesJuniorDoctors

# linear regression models

nurses1 <- lm(strikesNurses ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority, data=nhs) 
doctors1 <- lm(strikesJuniorDoctors ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority, data=nhs) 

nurses2 <- lm(strikesNurses ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS, data=nhs) 
doctors2 <- lm(strikesJuniorDoctors ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS, data=nhs) 

nurses3 <- lm(strikesNurses ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS + conVote19, data=nhs) 
doctors3 <- lm(strikesJuniorDoctors ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS + conVote19, data=nhs) 

stargazer(nurses1, nurses2, nurses3, doctors1, doctors2, doctors3, type = "text", omit.stat = c("f", "ser"))

stargazer(nurses1, nurses2, nurses3, doctors1, doctors2, doctors3, type = "html", omit.stat = c("f", "ser"))
		  



# ordered logistic regression models 

nurses1_ord <- polr(as.ordered(strikesNurses) ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority, data = nhs, method = "logistic")
doctors1_ord <- polr(as.ordered(strikesJuniorDoctors ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority, data=nhs, method = "logistic") 

nurses2_ord <- polr(as.ordered(strikesNurses ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS, data=nhs, method = "logistic") 
doctors2_ord <- polr(as.ordered(strikesJuniorDoctors ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS, data=nhs, method = "logistic") 

nurses3_ord <- polr(as.ordered(strikesNurses ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS + conVote19, data=nhs, method = "logistic") 
doctors3_ord <- polr(as.ordered(strikesJuniorDoctors ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS + conVote19, data=nhs, method = "logistic") 

stargazer(nurses1_ord, doctors1_ord, nurses2_ord, doctors2_ord, nurses3_ord, doctors3_ord, type="text", omit.stat = c("f", "ser")) 


# now with weights 

# weighted linear regression models

nurses1w <- lm(strikesNurses ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority, data=nhs, weights=W8) 
doctors1w <- lm(strikesJuniorDoctors ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority, data=nhs, weights=W8)

nurses2w <- lm(strikesNurses ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS, data=nhs, weights=W8) 
doctors2w <- lm(strikesJuniorDoctors ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS, data=nhs, weights=W8)

nurses3w <- lm(strikesNurses ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS + conVote19, data=nhs, weights=W8) 
doctors3w <- lm(strikesJuniorDoctors ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS + conVote19, data=nhs, weights=W8)

# compare all weighted models 

stargazer(nurses1w, doctors1w, nurses2w, doctors2w, nurses3w, doctors3w, type="text", omit.stat = c("f", "ser")) 

# compare weighted with non-weighted

stargazer(nurses1, nurses1w, nurses2, nurses2w, nurses3, nurses3w, type="text", omit.stat = c("f", "ser")) 
stargazer(doctors1, doctors1w, doctors2, doctors2w, doctors3, doctors3w, type="text", omit.stat = c("f", "ser")) 

# weighted ordered logistic regression models

nurses1_ord_wt <- polr(as.ordered(strikesNurses) ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority, data = nhs, weights = nhs$W8, method = "logistic")
doctors1_ord_wt <- polr(as.ordered(strikesJuniorDoctors) ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority, data = nhs, weights = nhs$W8, method = "logistic")

nurses2_ord_wt <- polr(as.ordered(strikesNurses) ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS, data = nhs, weights = nhs$W8, method = "logistic")
doctors2_ord_wt <- polr(as.ordered(strikesJuniorDoctors) ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS, data = nhs, weights = nhs$W8, method = "logistic")

nurses3_ord_wt <- polr(as.ordered(strikesNurses) ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS + conVote19, data = nhs, weights = nhs$W8, method = "logistic")
doctors3_ord_wt <- polr(as.ordered(strikesJuniorDoctors) ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS + conVote19, data = nhs, weights = nhs$W8, method = "logistic")

# compare all weighted ordered logistic regression models 

stargazer(nurses1_ord_wt, doctors1_ord_wt, nurses2_ord_wt, doctors2_ord_wt, nurses3_ord_wt, doctors3_ord_wt, type="text", omit.stat = c("f", "ser")) 


# time sensitive 

nhs611 <- nhs[nhs$wave >= 6 & nhs$wave <= 11, ]
nhs16 <- nhs[nhs$wave == 16, ]
nhs911 <- nhs[nhs$wave >= 9 & nhs$wave <= 11, ]

nurses611 <- lm(strikesNurses ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS + conVote19, data=nhs611) 
doctors911 <- lm(strikesJuniorDoctors ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS + conVote19, data=nhs911) 

nurses16 <- lm(strikesNurses ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS + conVote19, data=nhs16) 
doctors16 <- lm(strikesJuniorDoctors ~ age + ageSquared + women + university + ownHealth + disabled + ethnicMinority + leftRight + trustNHS + conVote19, data=nhs16) 

stargazer(nurses611, nurses16, doctors911, doctors16, type="text", omit.stat = c("f", "ser"))
stargazer(nurses611, nurses16, doctors911, doctors16, type="html", omit.stat = c("f", "ser"))


