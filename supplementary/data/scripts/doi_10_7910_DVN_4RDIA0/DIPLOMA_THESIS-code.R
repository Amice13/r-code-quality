
rm(list = ls())

getwd()

list.files()

## RUN ONLY IF NEEDED ##
#install.packages("foreign")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("ggcorrplot")
#install.packages("car")
#install.packages("stargazer")
#install.packages("MASS")
#install.packages("dplyr")
#install.packages("dotwhisker")
#install.packages("ggeffects")
#install.packages("knitr")
#install.packages("tidyr")
#install.packages("gridExtra")
#install.packages("jtools")

library(foreign)
library(readxl)
library(ggplot2)
library(ggcorrplot)
library(car)
library(stargazer)
library(MASS)
library(dplyr)
library(dotwhisker)
library(ggeffects)
library(knitr)
library(tidyr)
library(gridExtra)
library(jtools)

############### IMPORTING DATA ###############################


dataset <- read.csv("results-survey185359.csv")


############### DATA TRANSFORMATION ###########################


dataset$Are.you....[dataset$Are.you.... == "A1"] <- "M"
dataset$Are.you....[dataset$Are.you.... == "A2"] <- "F"
dataset$Are.you....[dataset$Are.you.... == "A3"] <- "O"
dataset$Are.you....[dataset$Are.you.... == "A4"] <- "X"

dataset$Are.you.currently.enrolled.in.a.university.programme.[dataset$Are.you.currently.enrolled.in.a.university.programme. == "A1"] <- "Y"
dataset$Are.you.currently.enrolled.in.a.university.programme.[dataset$Are.you.currently.enrolled.in.a.university.programme. == "A2"] <- "N"

dataset$What.is.the.highest.educational.qualification.that.you.have.achieved..[dataset$What.is.the.highest.educational.qualification.that.you.have.achieved.. == "A1"] <- 1
dataset$What.is.the.highest.educational.qualification.that.you.have.achieved..[dataset$What.is.the.highest.educational.qualification.that.you.have.achieved.. == "A2"] <- 2
dataset$What.is.the.highest.educational.qualification.that.you.have.achieved..[dataset$What.is.the.highest.educational.qualification.that.you.have.achieved.. == "A3"] <- 3
dataset$What.is.the.highest.educational.qualification.that.you.have.achieved..[dataset$What.is.the.highest.educational.qualification.that.you.have.achieved.. == "A4"] <- 4
dataset$What.is.the.highest.educational.qualification.that.you.have.achieved..[dataset$What.is.the.highest.educational.qualification.that.you.have.achieved.. == "A5"] <- 5
dataset$What.is.the.highest.educational.qualification.that.you.have.achieved..[dataset$What.is.the.highest.educational.qualification.that.you.have.achieved.. == "A6"] <- 6

dataset$Are.you.a.student.of.political.science.or.any.related.subfield.of.political.science..international.relations..security.studies.etc...[dataset$Are.you.a.student.of.political.science.or.any.related.subfield.of.political.science..international.relations..security.studies.etc... == "A1"] <- "Y"
dataset$Are.you.a.student.of.political.science.or.any.related.subfield.of.political.science..international.relations..security.studies.etc...[dataset$Are.you.a.student.of.political.science.or.any.related.subfield.of.political.science..international.relations..security.studies.etc... == "A2"] <- "N"

dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself.[dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself. == "A1"] <- 1
dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself.[dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself. == "A2"] <- 2
dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself.[dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself. == "A3"] <- 3
dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself.[dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself. == "A4"] <- 4
dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself.[dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself. == "A5"] <- 5
dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself.[dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself. == "A6"] <- 6
dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself.[dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself. == "A7"] <- 7

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction. == "A6"] <- 6

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut. == "A1"] <- 1
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut. == "A2"] <- 2
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut. == "A3"] <- 3
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut. == "A4"] <- 4
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut. == "A5"] <- 5
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut.[dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut. == "A6"] <- 6

dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer.[dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer. == "A1"] <- 1
dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer.[dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer. == "A2"] <- 2
dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer.[dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer. == "A3"] <- 3
dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer.[dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer. == "A4"] <- 4

dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion.[dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion. == "A1"] <- 1
dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion.[dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion. == "A2"] <- 2
dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion.[dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion. == "A3"] <- 3
dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion.[dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion. == "A4"] <- 4

dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..1[dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..1 == "A1"] <- 1
dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..1[dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..1 == "A2"] <- 2
dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..1[dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..1 == "A3"] <- 3
dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..1[dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..1 == "A4"] <- 4

dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..1[dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..1 == "A1"] <- 1
dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..1[dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..1 == "A2"] <- 2
dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..1[dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..1 == "A3"] <- 3
dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..1[dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..1 == "A4"] <- 4

dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..[dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer.. == "A1"] <- 1
dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..[dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer.. == "A2"] <- 2
dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..[dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer.. == "A3"] <- 3
dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..[dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer.. == "A4"] <- 4

dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..2[dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..2 == "A1"] <- 1
dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..2[dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..2 == "A2"] <- 2
dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..2[dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..2 == "A3"] <- 3
dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..2[dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..2 == "A4"] <- 4

dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon.[dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon. == "A1"] <- 1
dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon.[dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon. == "A2"] <- 2
dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon.[dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon. == "A3"] <- 3
dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon.[dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon. == "A4"] <- 4
dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon.[dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon. == "A5"] <- 5
dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon.[dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon. == "A6"] <- 6

dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons.[dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons. == "A1"] <- 1
dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons.[dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons. == "A2"] <- 2
dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons.[dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons. == "A3"] <- 3
dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons.[dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons. == "A4"] <- 4
dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons.[dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons. == "A5"] <- 5
dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons.[dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons. == "A6"] <- 6

dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons. == "A1"] <- 1
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons. == "A2"] <- 2
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons. == "A3"] <- 3
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons. == "A4"] <- 4
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons. == "A5"] <- 5
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons. == "A6"] <- 6

dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons. == "A1"] <- 1
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons. == "A2"] <- 2
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons. == "A3"] <- 3
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons. == "A4"] <- 4
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons. == "A5"] <- 5
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons. == "A6"] <- 6

dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons. == "A1"] <- 1
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons. == "A2"] <- 2
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons. == "A3"] <- 3
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons. == "A4"] <- 4
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons. == "A5"] <- 5
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons. == "A6"] <- 6

dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles. == "A1"] <- 1
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles. == "A2"] <- 2
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles. == "A3"] <- 3
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles. == "A4"] <- 4
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles. == "A5"] <- 5
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles. == "A6"] <- 6

dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns. == "A1"] <- 1
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns. == "A2"] <- 2
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns. == "A3"] <- 3
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns. == "A4"] <- 4
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns. == "A5"] <- 5
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns. == "A6"] <- 6

dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm. == "A1"] <- 1
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm. == "A2"] <- 2
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm. == "A3"] <- 3
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm. == "A4"] <- 4
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm. == "A5"] <- 5
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm. == "A6"] <- 6

dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines. == "A1"] <- 1
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines. == "A2"] <- 2
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines. == "A3"] <- 3
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines. == "A4"] <- 4
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines. == "A5"] <- 5
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines. == "A6"] <- 6

dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade. == "A1"] <- 1
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade. == "A2"] <- 2
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade. == "A3"] <- 3
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade. == "A4"] <- 4
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade. == "A5"] <- 5
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade. == "A6"] <- 6

dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition. == "A1"] <- 1
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition. == "A2"] <- 2
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition. == "A3"] <- 3
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition. == "A4"] <- 4
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition. == "A5"] <- 5
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition.[dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition. == "A6"] <- 6

dataset$What.is.the.highest.educational.qualification.that.you.have.achieved.. <- as.integer(dataset$What.is.the.highest.educational.qualification.that.you.have.achieved..)

dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself. <- as.integer(dataset$Where.on.the.following.scale.of.political.orientation.would.you.in.general.place.yourself.)

dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shoplifting.a.candy.bar.from.a.convenience.store.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stepping.on.dog.poop.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Stealing.from.a.neighbor.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Sitting.next.to.someone.who.has.red.sores.on.their.arm.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...A.student.cheating.to.get.good.grades.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Shaking.hands.with.a.stranger.who.has.sweaty.palms.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Deceiving.a.friend.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.some.mold.on.old.leftovers.in.your.refrigerator.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Forging.someone.s.signature.on.a.legal.document.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Standing.close.to.a.person.who.has.body.odor.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Cutting.to.the.front.of.a.line.to.purchase.the.last.few.tickets.to.a.show.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Seeing.a.cockroach.run.across.the.floor.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Intentionally.lying.during.a.business.transaction.)
dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut. <- as.integer(dataset$Please.indicate.how.disgusting.would.you.perceive.the.following...Accidentally.touching.a.person.s.bloody.cut.)

dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer. <- as.integer(dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer.)
dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion. <- as.integer(dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion.)

dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..1 <- as.integer(dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..1)
dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..1 <- as.integer(dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..1)

dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer.. <- as.integer(dataset$Being.the.decision.maker.in.this.situation.selecting.between.the.use.of.a.single.nuclear.weapon.or.an.airstrike.using.chemical.weapons..which.do.you.prefer..)
dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..2 <- as.integer(dataset$Some.people.find.chemical.weapons.more.disgusting.than.nuclear.weapons..some.find.nuclear.weapons.more.disgusting.than.chemical.weapons..and.others.see.them.as.equally.disgusting..Which.of.the.following.statements.best.describes.your.opinion..2)

dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon. <- as.integer(dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.a.single.nuclear.weapon.)
dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons. <- as.integer(dataset$Regardless.of.which.option.you.preferred..how.ethical.or.unethical.do.you.think.it.would.be.if.the.coalition.used.an.airstrike.using.chemical.weapons.)

dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons. <- as.integer(dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Chemical.weapons.)
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons. <- as.integer(dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Nuclear.weapons.)
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons. <- as.integer(dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Biological.weapons.)
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles. <- as.integer(dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Conventional.missiles.)
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns. <- as.integer(dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Machine.guns.)
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm. <- as.integer(dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Napalm.)
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines. <- as.integer(dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Anti.personnel..landmines.)
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade. <- as.integer(dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Grenade.)
dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition. <- as.integer(dataset$Please.rate.the.following.weapon.systems.from.least.to.most.disgusting...Cluster.munition.)

dataset$Are.you.currently.enrolled.in.a.university.programme.[dataset$Are.you.currently.enrolled.in.a.university.programme. == "Y"] <- 1
dataset$Are.you.currently.enrolled.in.a.university.programme.[dataset$Are.you.currently.enrolled.in.a.university.programme. == "N"] <- 0
dataset$Are.you.currently.enrolled.in.a.university.programme. <- as.integer(dataset$Are.you.currently.enrolled.in.a.university.programme.)

dataset$Are.you.a.student.of.political.science.or.any.related.subfield.of.political.science..international.relations..security.studies.etc...[dataset$Are.you.a.student.of.political.science.or.any.related.subfield.of.political.science..international.relations..security.studies.etc... == "Y"] <- 1
dataset$Are.you.a.student.of.political.science.or.any.related.subfield.of.political.science..international.relations..security.studies.etc...[dataset$Are.you.a.student.of.political.science.or.any.related.subfield.of.political.science..international.relations..security.studies.etc... == "N"] <- 0
dataset$Are.you.a.student.of.political.science.or.any.related.subfield.of.political.science..international.relations..security.studies.etc... <- as.integer(dataset$Are.you.a.student.of.political.science.or.any.related.subfield.of.political.science..international.relations..security.studies.etc...)

table(dataset$What.is.your.country.of.origin.)
dataset$NATO <- NA
dataset$NATO[dataset$What.is.your.country.of.origin. == "canada"|dataset$What.is.your.country.of.origin. == "Canada"|dataset$What.is.your.country.of.origin. == "Česká republika"|dataset$What.is.your.country.of.origin. == "Česko"|dataset$What.is.your.country.of.origin. == "ČR"|dataset$What.is.your.country.of.origin. == "cz"|dataset$What.is.your.country.of.origin. == "Cz"|dataset$What.is.your.country.of.origin. == "CZ"|dataset$What.is.your.country.of.origin. == "CZE"|dataset$What.is.your.country.of.origin. == "czech"|dataset$What.is.your.country.of.origin. == "Czech"|dataset$What.is.your.country.of.origin. == "czech "|dataset$What.is.your.country.of.origin. == "Czech "|dataset$What.is.your.country.of.origin. == "Czech Republi "|dataset$What.is.your.country.of.origin. == "czech republic"|dataset$What.is.your.country.of.origin. == "Czech republic"|dataset$What.is.your.country.of.origin. == "Czech Republic"|dataset$What.is.your.country.of.origin. == "Czech republic "|dataset$What.is.your.country.of.origin. == "Czech Republic "|dataset$What.is.your.country.of.origin. == "Czech republice, i'm conswrvativ libartarien, no liberal"|dataset$What.is.your.country.of.origin. == "czech reublic"|dataset$What.is.your.country.of.origin. == "czechia"|dataset$What.is.your.country.of.origin. == "Czechia"|dataset$What.is.your.country.of.origin. == "Czechia "|dataset$What.is.your.country.of.origin. == "Denmark"|dataset$What.is.your.country.of.origin. == "Finland"|dataset$What.is.your.country.of.origin. == "France"|dataset$What.is.your.country.of.origin. == "France "|dataset$What.is.your.country.of.origin. == "German"|dataset$What.is.your.country.of.origin. == "Germany"|dataset$What.is.your.country.of.origin. == "Germany "|dataset$What.is.your.country.of.origin. == "Hungary"|dataset$What.is.your.country.of.origin. == "italy"|dataset$What.is.your.country.of.origin. == "Italy"|dataset$What.is.your.country.of.origin. == "Norway"|dataset$What.is.your.country.of.origin. == "Poland"|dataset$What.is.your.country.of.origin. == "Slovak"|dataset$What.is.your.country.of.origin. == "Slovak Republic"|dataset$What.is.your.country.of.origin. == "slovakia"|dataset$What.is.your.country.of.origin. == "Slovakia"|dataset$What.is.your.country.of.origin. == "slovakia "|dataset$What.is.your.country.of.origin. == "Slovakia "|dataset$What.is.your.country.of.origin. == "Slovenia"|dataset$What.is.your.country.of.origin. == "Spain"|dataset$What.is.your.country.of.origin. == "the Czech republic"|dataset$What.is.your.country.of.origin. == "the Czech Republic"|dataset$What.is.your.country.of.origin. == "The Czech republic"|dataset$What.is.your.country.of.origin. == "The Czech Republic"|dataset$What.is.your.country.of.origin. == "Turkey"|dataset$What.is.your.country.of.origin. == "UK"|dataset$What.is.your.country.of.origin. == "United States"|dataset$What.is.your.country.of.origin. == "United States of America "|dataset$What.is.your.country.of.origin. == "United States/United Kingdom "|dataset$What.is.your.country.of.origin. == "US"|dataset$What.is.your.country.of.origin. == "USA"] <- 1
dataset$NATO[dataset$What.is.your.country.of.origin. == "Austria"|dataset$What.is.your.country.of.origin. == "Brazil"|dataset$What.is.your.country.of.origin. == "El Salvador"|dataset$What.is.your.country.of.origin. == "India"|dataset$What.is.your.country.of.origin. == "India "|dataset$What.is.your.country.of.origin. == "Indonesia"|dataset$What.is.your.country.of.origin. == "japan"|dataset$What.is.your.country.of.origin. == "Japan"|dataset$What.is.your.country.of.origin. == "Japan "|dataset$What.is.your.country.of.origin. == "Malaysia"|dataset$What.is.your.country.of.origin. == "Mexico"|dataset$What.is.your.country.of.origin. == "Mongolia"|dataset$What.is.your.country.of.origin. == "Myanmar "|dataset$What.is.your.country.of.origin. == "Philippines"|dataset$What.is.your.country.of.origin. == "Russia "|dataset$What.is.your.country.of.origin. == "RUssia-Ukraine"|dataset$What.is.your.country.of.origin. == "Russian Federation"|dataset$What.is.your.country.of.origin. == "South Africa"|dataset$What.is.your.country.of.origin. == "Tanzania"|dataset$What.is.your.country.of.origin. == "Timor Leste"|dataset$What.is.your.country.of.origin. == "Uganda "|dataset$What.is.your.country.of.origin. == "Ukraine"|dataset$What.is.your.country.of.origin. == "Ukraine "] <- 0

dataset$disgust_score <- rowSums(dataset[,14:27])
dataset$disgust_score_mean <- rowMeans(dataset[,14:27])

colnames(dataset) <- c("response_id","date","last_page","language","seed","experimental_group","gender","age","student","education","political_science","ideology","country","disgust_scale1","disgust_scale2","disgust_scale3","disgust_scale4","disgust_scale5","disgust_scale6","disgust_scale7","disgust_scale8","disgust_scale9","disgust_scale10","disgust_scale11","disgust_scale12","disgust_scale13","disgust_scale14","attention_check_control","attention_check_chemical","attention_check_nuclear","preference_control","disgust_control","preference_chemical","disgust_chemical","preference_nuclear","disgust_nuclear","ethicality_nw","ethicality_cw","chemical_weapons","nuclear_weapons","biological_weapons","conventional_missiles","machine_guns","napalm","antipersonnel_landmines","grenade","cluster_munition","vranka_code","vranka_course","nato","disgust_score","disgust_score_mean")

table(dataset$preference_control) ## 0 = 76, 1 = 44
dataset$preference_control_binary[dataset$preference_control == 1 | dataset$preference_control == 2] <- 0
dataset$preference_control_binary[dataset$preference_control == 3 | dataset$preference_control == 4] <- 1
table(dataset$preference_control_binary)

table(dataset$preference_chemical) ## 0 = 56, 1 = 63
dataset$preference_chemical_binary[dataset$preference_chemical == 1 | dataset$preference_chemical == 2] <- 0
dataset$preference_chemical_binary[dataset$preference_chemical == 3 | dataset$preference_chemical == 4] <- 1
table(dataset$preference_chemical_binary)

table(dataset$preference_nuclear) ## 0 = 92, 1 = 53
dataset$preference_nuclear_binary[dataset$preference_nuclear == 1 | dataset$preference_nuclear == 2] <- 0
dataset$preference_nuclear_binary[dataset$preference_nuclear == 3 | dataset$preference_nuclear == 4] <- 1
table(dataset$preference_nuclear_binary)

table(dataset$ethicality_nw) ## 1 = 72, 0 = 310
dataset$ethicality_nw_binary[dataset$ethicality_nw == 1 | dataset$ethicality_nw == 2 | dataset$ethicality_nw == 3] <- 1
dataset$ethicality_nw_binary[dataset$ethicality_nw == 4 | dataset$ethicality_nw == 5 | dataset$ethicality_nw == 6] <- 0
table(dataset$ethicality_nw_binary)

table(dataset$ethicality_cw) ## 1 = 44, 0 = 338
dataset$ethicality_cw_binary[dataset$ethicality_cw == 1 | dataset$ethicality_cw == 2 | dataset$ethicality_cw == 3] <- 1
dataset$ethicality_cw_binary[dataset$ethicality_cw == 4 | dataset$ethicality_cw == 5 | dataset$ethicality_cw == 6] <- 0
table(dataset$ethicality_cw_binary)

table(dataset$gender)
dataset$gender_numeric <- NA
dataset$gender_numeric[dataset$gender == "F"] <- 1
dataset$gender_numeric[dataset$gender == "M"] <- 0
table(dataset$gender_numeric)

dataset$preference_binary <- do.call(pmax, c(dataset[,53:55], na.rm=TRUE))

dataset$experimental_group <- as.factor(dataset$experimental_group)

dataset$pathogen_disgust_score <- rowSums(dataset[,c("disgust_scale2","disgust_scale4","disgust_scale6","disgust_scale8","disgust_scale10","disgust_scale12","disgust_scale14")])
dataset$pathogen_disgust_score_mean <-rowMeans(dataset[,c("disgust_scale2","disgust_scale4","disgust_scale6","disgust_scale8","disgust_scale10","disgust_scale12","disgust_scale14")])

dataset$moral_disgust_score <- rowSums(dataset[,c("disgust_scale1","disgust_scale3","disgust_scale5","disgust_scale7","disgust_scale9","disgust_scale11","disgust_scale13")])
dataset$moral_disgust_score_mean <- rowMeans(dataset[,c("disgust_scale1","disgust_scale3","disgust_scale5","disgust_scale7","disgust_scale9","disgust_scale11","disgust_scale13")])

dataset$disgust <- do.call(pmax, c(dataset[,c("disgust_control","disgust_chemical","disgust_nuclear")], na.rm=TRUE))

table(dataset$ideology) ## 1 = 90, 2 = 86, 3 = 289
dataset$ideology_ternary[dataset$ideology == 1 | dataset$ideology == 2 | dataset$ideology == 3] <- 1
dataset$ideology_ternary[dataset$ideology == 4] <- 2
dataset$ideology_ternary[dataset$ideology == 5 | dataset$ideology == 6 | dataset$ideology == 7] <- 3
table(dataset$ideology_ternary)

dataset$gender_numeric <- as.factor(dataset$gender_numeric)
dataset$ideology_ternary <- as.factor(dataset$ideology_ternary)
dataset$nato <- as.factor(dataset$nato)
dataset$political_science <- as.factor(dataset$political_science)

write.csv(dataset, file="dataset.csv")

################## ANALYSIS ###########################

# Logistic regression

reg_aversion_m1 <- glm(formula = preference_binary ~ experimental_group, data = dataset, family = binomial(link = logit))
summary(reg_aversion_m1)

reg_disgust_m1 <- glm(formula = preference_control_binary ~ disgust_score_mean, data = dataset, family = binomial(link = logit))
summary(reg_disgust_m1)

reg_disgust_m2 <- glm(formula = preference_control_binary ~ pathogen_disgust_score_mean, data = dataset, family = binomial(link = logit))
summary(reg_disgust_m2)

reg_disgust_m3 <- glm(formula = preference_control_binary ~ moral_disgust_score_mean, data = dataset, family = binomial(link = logit))
summary(reg_disgust_m3)

# Multivariate logistic regression

reg_aversion_m2 <- glm(formula = preference_binary ~ experimental_group + disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset, family = binomial(link = logit))
summary(reg_aversion_m2)

reg_aversion_m3 <- glm(formula = preference_binary ~ experimental_group + pathogen_disgust_score_mean + moral_disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset, family = binomial(link = logit))
summary(reg_aversion_m3)

reg_aversion_m4 <- glm(formula = preference_binary ~ experimental_group*disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset, family = binomial(link = logit))
summary(reg_aversion_m4)

reg_aversion_m5 <- glm(formula = preference_binary ~ experimental_group*pathogen_disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset, family = binomial(link = logit))
summary(reg_aversion_m5)

# Robustness check

reg_ethicality_cw_m1 <- glm(formula = ethicality_cw_binary ~ experimental_group, data = dataset, family = binomial(link = logit))
summary(reg_ethicality_cw_m1)

reg_ethicality_nw_m1 <- glm(formula = ethicality_nw_binary ~ experimental_group, data = dataset, family = binomial(link = logit))
summary(reg_ethicality_nw_m1)

reg_ethicality_cw_m2 <- glm(formula = ethicality_cw_binary ~ experimental_group + disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset, family = binomial(link = logit))
summary(reg_ethicality_cw_m2)

reg_ethicality_nw_m2 <- glm(formula = ethicality_nw_binary ~ experimental_group + disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset, family = binomial(link = logit))
summary(reg_ethicality_nw_m2)

# Exploration

reg_cw_m1 <- lm(formula = chemical_weapons ~ pathogen_disgust_score_mean + moral_disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset)
summary(reg_cw_m1)

reg_nw_m1 <- lm(formula = nuclear_weapons ~ pathogen_disgust_score_mean + moral_disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset)
summary(reg_nw_m1)

reg_bw_m1 <- lm(formula = biological_weapons ~ pathogen_disgust_score_mean + moral_disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset)
summary(reg_bw_m1)

reg_conv_miss_m1 <- lm(formula = conventional_missiles ~ pathogen_disgust_score_mean + moral_disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset)
summary(reg_conv_miss_m1)

reg_machine_guns_m1 <- lm(formula = machine_guns ~ pathogen_disgust_score_mean + moral_disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset)
summary(reg_machine_guns_m1)

reg_napalm_m1 <- lm(formula = napalm ~ pathogen_disgust_score_mean + moral_disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset)
summary(reg_napalm_m1)

reg_mines_m1 <- lm(formula = antipersonnel_landmines ~ pathogen_disgust_score_mean + moral_disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset)
summary(reg_mines_m1)

reg_grenade_m1 <- lm(formula = grenade ~ gender_numeric + pathogen_disgust_score_mean + moral_disgust_score_mean + age + education + ideology_ternary + nato + political_science, data = dataset)
summary(reg_grenade_m1)

reg_cluster_m1 <- lm(formula = cluster_munition ~ pathogen_disgust_score_mean + moral_disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset)
summary(reg_cluster_m1)



################## PLOTS ##############################

## Barplot - preference X experimental group

table(dataset$preference_binary)/sum(table(dataset$preference_binary))*100

table(dataset[,c("experimental_group","preference_binary")])

df <- data.frame(table(dataset[,c("experimental_group","preference_binary")]))
df$perc <- c(76/120,56/119,92/145,44/120,63/119,53/145)

ggplot(df, aes(x = experimental_group, y = perc, fill = as.factor(preference_binary))) + geom_col(color = "black") + labs(x = "Experimental Group", y = "Preference in %", fill = NULL) + scale_fill_manual(values = c("gray", "black"), labels = c("Airstrike using chemical weapons", "Single nuclear weapon")) + scale_x_discrete(labels = c("Control","Chemical","Nuclear")) + scale_y_continuous(labels = scales::percent) + theme_classic() + theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.5)) + geom_text(aes(label=round(perc*100, 1)), position = position_stack(1.08))
ggsave("preference_cw_nw_experimental_group.png", dpi = 300)

binom <- data.frame(Column1 = rep(NA, 6), Column2 = rep(NA, 6), Column3 = df$Freq)
colnames(binom) <- c("lower","upper","Freq")
binom$lower[1] <- binom.test(76, 120, 1/2)$conf.int[1]
binom$upper[1] <- binom.test(76, 120, 1/2)$conf.int[2]
binom$lower[2] <- binom.test(56, 119, 1/2)$conf.int[1]
binom$upper[2] <- binom.test(56, 119, 1/2)$conf.int[2]
binom$lower[3] <- binom.test(92, 145, 1/2)$conf.int[1]
binom$upper[3] <- binom.test(92, 145, 1/2)$conf.int[2]
binom$lower[4] <- binom.test(44, 120, 1/2)$conf.int[1]
binom$upper[4] <- binom.test(44, 120, 1/2)$conf.int[2]
binom$lower[5] <- binom.test(63, 119, 1/2)$conf.int[1]
binom$upper[5] <- binom.test(63, 119, 1/2)$conf.int[2]
binom$lower[6] <- binom.test(53, 145, 1/2)$conf.int[1]
binom$upper[6] <- binom.test(53, 145, 1/2)$conf.int[2]

df <- full_join(df, binom)

##ggplot(data = subset(df, preference_binary == 1), aes(x = experimental_group, y = perc, fill = as.factor(preference_binary))) + geom_col(color = "black") + labs(x = "Experimental Group", y = "% prefer NW", fill = NULL) + scale_fill_manual(values = c("gray")) + scale_x_discrete(labels = c("Control","Chemical","Nuclear")) + scale_y_continuous(labels = scales::percent) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_text(aes(label=round(perc*100, 1)), position = position_stack(0.5)) + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3)
##ggsave("preference_nw_experimental_group.png", dpi = 300)

ggplot(data = subset(df, preference_binary == 0), aes(x = experimental_group, y = perc, fill = as.factor(preference_binary))) + geom_col(color = "black") + labs(x = "Experimental Group", y = "% prefer CW", fill = NULL) + scale_fill_manual(values = c("gray")) + scale_x_discrete(labels = c("Control","Chemical","Nuclear")) + scale_y_continuous(labels = scales::percent) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_text(aes(label=round(perc*100, 1)), position = position_stack(0.5)) + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3)
ggsave("preference_cw_experimental_group.png", dpi = 300)

## Coefficient plot - aversion models

summary(reg_aversion_m1)
levels(dataset$experimental_group)[1]
p = dwplot(reg_aversion_m1)
dwplot(reg_aversion_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Nuclear (to Control)","Chemical (to Control)")) + labs(y = "Experimental Group", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_hline(yintercept = 1.5) + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c("*"," ")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_aversion_m1.png", dpi = 300)

summary(reg_aversion_m1)
levels(dataset$experimental_group)[1]
summary(reg_aversion_m2)
levels(dataset$gender_numeric)[1]
levels(dataset$ideology_ternary)[1]
levels(dataset$nato)[1]
levels(dataset$political_science)[1]
summary(reg_aversion_m3)
p = dwplot(list(reg_aversion_m1, reg_aversion_m2, reg_aversion_m3), vars_order = c("experimental_group2","experimental_group3","disgust_score_mean","pathogen_disgust_score_mean","moral_disgust_score_mean","gender_numeric1","age","education","ideology_ternary2","ideology_ternary3","nato1","political_science1"))
dwplot(list(reg_aversion_m1, reg_aversion_m2, reg_aversion_m3), dot_args = aes(size = 2), dodge_size = 0.9, vars_order = c("experimental_group2","experimental_group3","disgust_score_mean","pathogen_disgust_score_mean","moral_disgust_score_mean","gender_numeric1","age","education","ideology_ternary2","ideology_ternary3","nato1","political_science1")) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "bottom", legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.5)) + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score","Mean of disgust score","Nuclear (to Control)","Chemical (to Control)")) + labs(caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_manual(values = c("black","darkgrey","lightgray"), name = NULL) + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c("*"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ")), hjust = -0.8, vjust = -2) + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c("*"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ","+"," "," "," ","*","+"," ")), hjust = -0.8, vjust = -0.3) + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c("*"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ","+"," "," "," ","*","+"," ")), hjust = -0.8, vjust = 1.8)
ggsave("coeffplot_reg_aversion_m1+2+3.png", dpi = 300)

## Coefficient plot - disgust models

summary(reg_disgust_m1)
summary(reg_disgust_m2)
summary(reg_disgust_m3)
p = dwplot(list(reg_disgust_m1, reg_disgust_m2, reg_disgust_m3))
dwplot(list(reg_disgust_m1, reg_disgust_m2, reg_disgust_m3), dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Moral disgust only","Pathogen disgust only","Disgust score")) + labs(y = "Mean of disgust score", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_manual(values = c("black","black","black")) + geom_hline(yintercept = c(1.5,2.5)) ##+ geom_text(x = p$data$estimate[c(1,5,9)], y = p$data$term[c(1,5,9)], aes(label = c(" "," "," ")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_disgust_m1-3.png", dpi = 300)

## Coefficient plot - ethicality models (robustness check)

summary(reg_ethicality_cw_m1)
p = dwplot(reg_ethicality_cw_m1)
dwplot(reg_ethicality_cw_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Nuclear (to Control)","Chemical (to Control)")) + labs(y = "Experimental Group", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_hline(yintercept = 1.5) ##+ geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" "," ")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_ethicality_cw_m1.png", dpi = 300)

summary(reg_ethicality_nw_m1)
p = dwplot(reg_ethicality_nw_m1)
dwplot(reg_ethicality_nw_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Nuclear (to Control)","Chemical (to Control)")) + labs(y = "Experimental Group", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_hline(yintercept = 1.5) ##+ geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" "," ")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_ethicality_nw_m1.png", dpi = 300)

summary(reg_ethicality_cw_m1)
summary(reg_ethicality_cw_m2)
p = dwplot(list(reg_ethicality_cw_m1, reg_ethicality_cw_m2))
dwplot(list(reg_ethicality_cw_m1, reg_ethicality_cw_m2), dot_args = aes(size = 2), dodge_size = 1) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "bottom", legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.5)) + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of disgust score","Nuclear (to Control)","Chemical (to Control)")) + labs(caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_manual(values = c("black","grey"), name = NULL) + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ")), hjust = -0.5, vjust = -0.7) + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" "," "," "," "," "," "," "," "," "," "," ","+"," ","+"," ","+"," "," ","**"," ")), hjust = -0.5, vjust = 0.7)
ggsave("coeffplot_reg_ethicality_cw_m1+2.png", dpi = 300)

summary(reg_ethicality_nw_m1)
summary(reg_ethicality_nw_m2)
p = dwplot(list(reg_ethicality_nw_m1, reg_ethicality_nw_m2))
dwplot(list(reg_ethicality_nw_m1, reg_ethicality_nw_m2), dot_args = aes(size = 2), dodge_size = 1) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "bottom", legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.5)) + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of disgust score","Nuclear (to Control)","Chemical (to Control)")) + labs(caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_manual(values = c("black","grey"), name = NULL) + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ")), hjust = -0.5, vjust = -0.7) + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" "," "," "," "," "," "," "," "," "," "," "," "," ","**"," ","+"," ","+"," ","+")), hjust = -0.5, vjust = 0.7)
ggsave("coeffplot_reg_ethicality_nw_m1+2.png", dpi = 300)

## Interaction plot - aversion model

inter_plot <- ggpredict(reg_aversion_m4, terms=c("disgust_score_mean", "experimental_group"))
plot(inter_plot, colors = "bw") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "bottom", legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.5)) + labs(linetype = NULL, title = NULL, x = "Mean of disgust sensitivity score", y = "Preference of weapon system (binary)") + scale_linetype_manual(values = 1:3, labels = c("Control", "Chemical", "Nuclear")) + scale_y_continuous(labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1))
ggsave("interplot_reg_aversion_m4.png", dpi = 300)
p2 <- plot(inter_plot)
summary(p2)

inter_plot <- ggpredict(reg_aversion_m5, terms=c("pathogen_disgust_score_mean", "experimental_group"))
plot(inter_plot, colors = "bw") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "bottom", legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.5)) + labs(linetype = NULL, title = NULL, x = "Mean of pathogen disgust sensitivity score", y = "Preference of weapon system (binary)") + scale_linetype_manual(values = 1:3, labels = c("Control", "Chemical", "Nuclear")) + scale_y_continuous(labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1))
ggsave("interplot_reg_aversion_m5.png", dpi = 300)
p2 <- plot(inter_plot)
summary(p2)

## Coefficient plots - weapon system matrix

summary(reg_cw_m1)
p = dwplot(reg_cw_m1)
dwplot(reg_cw_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Chemical weapons", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","*"," "," "," "," "," ","*"," ")), hjust = -0.5, vjust = -0.5)
cw <- dwplot(reg_cw_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Chemical weapons", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","*"," "," "," "," "," ","*"," ")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_cw_m1.tiff", dpi = 320)

summary(reg_nw_m1)
p = dwplot(reg_nw_m1)
dwplot(reg_nw_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Nuclear weapons", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","**","**"," "," "," ","+"," "," ")), hjust = -0.5, vjust = -0.5)
nw <- dwplot(reg_nw_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Nuclear weapons", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","**","**"," "," "," ","+"," "," ")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_nw_m1.tiff", dpi = 320)

summary(reg_bw_m1)
p = dwplot(reg_bw_m1)
dwplot(reg_bw_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Biological weapons", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","+"," "," ","**"," "," ","*"," ")), hjust = -0.5, vjust = -0.5)
bw <- dwplot(reg_bw_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Biological weapons", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","+"," "," ","**"," "," ","*"," ")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_bw_m1.tiff", dpi = 320)

summary(reg_conv_miss_m1)
p = dwplot(reg_conv_miss_m1)
dwplot(reg_conv_miss_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Conventional missiles", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","+","**"," ","+","*","**"," "," ")), hjust = -0.5, vjust = -0.5)
conv_miss <- dwplot(reg_conv_miss_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Conventional missiles", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","+","**"," ","+","*","**"," "," ")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_conv_miss_m1.tiff", dpi = 320)

summary(reg_machine_guns_m1)
p = dwplot(reg_machine_guns_m1)
dwplot(reg_machine_guns_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Machine guns", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","+","**"," ","+"," ","*"," "," ")), hjust = -0.5, vjust = -0.5)
machine_guns <- dwplot(reg_machine_guns_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Machine guns", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","+","**"," ","+"," ","*"," "," ")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_machine_guns_m1.tiff", dpi = 320)

summary(reg_napalm_m1)
p = dwplot(reg_napalm_m1)
dwplot(reg_napalm_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Napalm", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","*","**"," "," "," "," "," "," ")), hjust = -0.5, vjust = -0.5)
napalm <- dwplot(reg_napalm_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Napalm", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","*","**"," "," "," "," "," "," ")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_napalm_m1.tiff", dpi = 320)

summary(reg_mines_m1)
p = dwplot(reg_mines_m1)
dwplot(reg_mines_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Antipersonnel landmines", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","+","**"," "," ","*"," "," ","**")), hjust = -0.5, vjust = -0.5)
mines <- dwplot(reg_mines_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Antipersonnel landmines", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","+","**"," "," ","*"," "," ","**")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_mines_m1.tiff", dpi = 320)

summary(reg_grenade_m1)
p = dwplot(reg_grenade_m1)
dwplot(reg_grenade_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Grenade", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","+","**"," ","*"," "," "," "," ")), hjust = -0.5, vjust = -0.5)
grenade <- dwplot(reg_grenade_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Grenade", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","+","**"," ","*"," "," "," "," ")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_grenade_m1.tiff", dpi = 320)

summary(reg_cluster_m1)
p = dwplot(reg_cluster_m1)
dwplot(reg_cluster_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Cluster munition", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","**","**"," "," "," "," "," "," ")), hjust = -0.5, vjust = -0.5)
cluster <- dwplot(reg_cluster_m1, dot_args = aes(color = "black", size = 2)) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + geom_vline(xintercept = 0, lty = 2) + scale_y_discrete(labels = c("Student of political science","NATO country","Liberal","Moderate","Education","Age","Female","Mean of moral disgust score","Mean of pathogen disgust score")) + labs(title = "Cluster munition", caption = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") + scale_color_grey() + geom_text(x = p$data$estimate, y = p$data$term, aes(label = c(" ","**","**"," "," "," "," "," "," ")), hjust = -0.5, vjust = -0.5)
ggsave("coeffplot_reg_cluster_m1.tiff", dpi = 320)

ggsave("coeffplots_weapon_system_matrix.tiff", arrangeGrob(cw, nw, bw, conv_miss, machine_guns, napalm, mines, grenade, cluster), dpi = 320)

## Barplot - weapon systems matrix

selected_columns <- dataset[, 39:47]

column_means <- colMeans(selected_columns, na.rm = TRUE)
column_sds <- apply(selected_columns, 2, sd, na.rm = TRUE)
column_ns <- apply(selected_columns, 2, function(x) sum(!is.na(x)))

z <- 1.96
ci_upper <- column_means + z * (column_sds / sqrt(column_ns))
ci_lower <- column_means - z * (column_sds / sqrt(column_ns))

plot_data <- data.frame(Weapon = names(column_means), MeanValue = column_means, CI_Lower = ci_lower, CI_Upper = ci_upper)

plot_data$Weapon <- factor(plot_data$Weapon, levels = names(selected_columns))

plot_data$MeanValue <- pmin(pmax(plot_data$MeanValue, 1), 6)
plot_data$CI_Lower <- pmin(pmax(plot_data$CI_Lower, 1), 6)
plot_data$CI_Upper <- pmin(pmax(plot_data$CI_Upper, 1), 6)

ggplot(plot_data, aes(x = Weapon, y = MeanValue)) + geom_bar(stat = "identity", fill = "grey", color = "black") + geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "black") + theme_classic() + labs(x = "Weapon system", y = "Mean of disgust rating") + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + geom_text(aes(label = round(MeanValue, 2)), vjust = 10, size = 4, color = "black") + scale_x_discrete(labels = c("Chemical weapons","Nuclear weapons","Biological weapons","Conventional missiles","Machine guns","Napalm","Antipersonnel landmines","Grenade","Cluster munition")) + coord_cartesian(ylim=c(1,6)) + scale_y_continuous(labels = c(1,2,3,4,5,6), breaks = c(1,2,3,4,5,6))
ggsave("weapon_systems_matrix.png", dpi = 300)

## Barplot - preference X political science

table(dataset[,c("political_science","preference_binary")])

agg_data <- aggregate(preference_binary ~ political_science, data = dataset, FUN = function(x) {
  n <- length(x)                     
  p <- mean(x == 0)                  
  ci <- 1.96 * sqrt(p * (1 - p) / n) 
  c(mean = p * 100, lower = (p - ci) * 100, upper = (p + ci) * 100) 
})


agg_data <- do.call(data.frame, agg_data)
colnames(agg_data) <- c("political_science", "percentage_preference_binary_0", "lower", "upper")

ggplot(agg_data, aes(x = factor(political_science), y = percentage_preference_binary_0)) + geom_bar(stat = "identity", fill = "grey", color = "black") + theme_classic() + labs(x = NULL, y = "Percentage of preference for chemical weapon") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) + scale_x_discrete(labels = c("Not students of political science","Students of political science")) + geom_text(aes(label = round(percentage_preference_binary_0, 2)), vjust = 10, size = 4, color = "black") + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black")
ggsave("pol_sci_cw.png", dpi = 300)

## Barplot - preference X nato

table(dataset[,c("nato","preference_binary")])

agg_data <- aggregate(preference_binary ~ nato, data = dataset, FUN = function(x) {
  n <- length(x)                     
  p <- mean(x == 0)                  
  ci <- 1.96 * sqrt(p * (1 - p) / n) 
  c(mean = p * 100, lower = (p - ci) * 100, upper = (p + ci) * 100) 
})


agg_data <- do.call(data.frame, agg_data)
colnames(agg_data) <- c("nato", "preference_binary0", "lower", "upper")

ggplot(agg_data, aes(x = factor(nato), y = preference_binary0)) + geom_bar(stat = "identity", fill = "grey", color = "black") + theme_classic() + labs(x = NULL, y = "Percentage of preference for chemical weapon") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) + scale_x_discrete(labels = c("Non-NATO country","NATO country")) + geom_text(aes(label = round(preference_binary0, 2)), vjust = 10, size = 4, color = "black") + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black")
ggsave("nato_cw.png", dpi = 300)

## Barplot - preference X gender

agg_data <- aggregate(preference_binary ~ gender_numeric, data = dataset, FUN = function(x) {
  n <- length(x)                     
  p <- mean(x == 0)                  
  ci <- 1.96 * sqrt(p * (1 - p) / n) 
  c(mean = p * 100, lower = (p - ci) * 100, upper = (p + ci) * 100) 
})


agg_data <- do.call(data.frame, agg_data)
colnames(agg_data) <- c("gender_numeric", "preference_binary0", "lower", "upper")

ggplot(agg_data, aes(x = factor(gender_numeric), y = preference_binary0)) + geom_bar(stat = "identity", fill = "grey", color = "black") + theme_classic() + labs(x = NULL, y = "Percentage of preference for chemical weapon") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) + scale_x_discrete(labels = c("Male","Female")) + geom_text(aes(label = round(preference_binary0, 2)), vjust = 10, size = 4, color = "black") + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black")
ggsave("gender_cw.png", dpi = 300)

## Barplot - preference X ideology

agg_data <- aggregate(preference_binary ~ ideology_ternary, data = dataset, FUN = function(x) {
  n <- length(x)                     
  p <- mean(x == 0)                  
  ci <- 1.96 * sqrt(p * (1 - p) / n) 
  c(mean = p * 100, lower = (p - ci) * 100, upper = (p + ci) * 100) 
})


agg_data <- do.call(data.frame, agg_data)
colnames(agg_data) <- c("ideology_ternary", "preference_binary0", "lower", "upper")

ggplot(agg_data, aes(x = factor(ideology_ternary), y = preference_binary0)) + geom_bar(stat = "identity", fill = "grey", color = "black") + theme_classic() + labs(x = NULL, y = "Percentage of preference for chemical weapon") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) + scale_x_discrete(labels = c("Conservative","Moderate","Liberal")) + geom_text(aes(label = round(preference_binary0, 2)), vjust = 10, size = 4, color = "black") + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black")
ggsave("ideology_cw.png", dpi = 300)

## Barplot - ethicality_nw X experimental group

agg_data <- aggregate(ethicality_nw_binary ~ experimental_group, data = dataset, FUN = function(x) {
  n <- length(x)                     
  p <- mean(x)                       
  ci <- 1.96 * sqrt(p * (1 - p) / n)
  c(mean = p * 100, lower = (p - ci) * 100, upper = (p + ci) * 100)
})


agg_data <- do.call(data.frame, agg_data)
colnames(agg_data) <- c("experimental_group", "ethicality_nw_binary", "lower", "upper")

ggplot(agg_data, aes(x = factor(experimental_group), y = ethicality_nw_binary)) + geom_bar(stat = "identity", fill = "grey", color = "black") + theme_classic() + labs(x = NULL, y = "Percentage of ethical use of nuclear weapon") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) + scale_x_discrete(labels = c("Control","Chemical","Nuclear")) + geom_text(aes(label = round(ethicality_nw_binary, 2)), vjust = 6, size = 4, color = "black") + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black")
ggsave("ethicality_nw_binary_experimental_group.png", dpi = 300)


#agg_data <- aggregate(ethicality_nw_binary ~ experimental_group, data = dataset, FUN = function(x) {
  #n <- length(x)                     
  #p <- mean(x == 0)                  
  #ci <- 1.96 * sqrt(p * (1 - p) / n) 
  #c(mean = p * 100, lower = (p - ci) * 100, upper = (p + ci) * 100) 
#})


#agg_data <- do.call(data.frame, agg_data)
#colnames(agg_data) <- c("experimental_group", "ethicality_nw_binary0", "lower", "upper")

#ggplot(agg_data, aes(x = factor(experimental_group), y = ethicality_nw_binary0)) + geom_bar(stat = "identity", fill = "grey", color = "black") + theme_classic() + labs(x = NULL, y = "Percentage of unethical use of nuclear weapon") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) + scale_x_discrete(labels = c("Control","Chemical","Nuclear")) + geom_text(aes(label = round(ethicality_nw_binary0, 2)), vjust = 10, size = 4, color = "black") + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black")
#ggsave("ethicality_nw_binary0_experimental_group.png", dpi = 300)

## Barplot - ethicality_cw X experimental group

agg_data <- aggregate(ethicality_cw_binary ~ experimental_group, data = dataset, FUN = function(x) {
  n <- length(x)                     
  p <- mean(x)                       
  ci <- 1.96 * sqrt(p * (1 - p) / n)
  c(mean = p * 100, lower = (p - ci) * 100, upper = (p + ci) * 100)
})


agg_data <- do.call(data.frame, agg_data)
colnames(agg_data) <- c("experimental_group", "ethicality_cw_binary", "lower", "upper")

ggplot(agg_data, aes(x = factor(experimental_group), y = ethicality_cw_binary)) + geom_bar(stat = "identity", fill = "grey", color = "black") + theme_classic() + labs(x = NULL, y = "Percentage of ethical use of chemical weapon") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) + scale_x_discrete(labels = c("Control","Chemical","Nuclear")) + geom_text(aes(label = round(ethicality_cw_binary, 2)), vjust = 5, size = 4, color = "black") + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black")
ggsave("ethicality_cw_binary_experimental_group.png", dpi = 300)


#agg_data <- aggregate(ethicality_cw_binary ~ experimental_group, data = dataset, FUN = function(x) {
  #n <- length(x)                     
  #p <- mean(x == 0)                  
  #ci <- 1.96 * sqrt(p * (1 - p) / n) 
  #c(mean = p * 100, lower = (p - ci) * 100, upper = (p + ci) * 100) 
#})


#agg_data <- do.call(data.frame, agg_data)
#colnames(agg_data) <- c("experimental_group", "ethicality_cw_binary0", "lower", "upper")

#ggplot(agg_data, aes(x = factor(experimental_group), y = ethicality_cw_binary0)) + geom_bar(stat = "identity", fill = "grey", color = "black") + theme_classic() + labs(x = NULL, y = "Percentage of unethical use of chemical weapon") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) + scale_x_discrete(labels = c("Control","Chemical","Nuclear")) + geom_text(aes(label = round(ethicality_cw_binary0, 2)), vjust = 10, size = 4, color = "black") + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black")
#ggsave("ethicality_cw_binary0_experimental_group.png", dpi = 300)

## Barplot - disgust X experimental group

cleaned_data <- dataset %>%
  filter(!is.na(disgust))

# Calculate counts, percentages, and confidence intervals
summary_data <- cleaned_data %>%
  group_by(experimental_group, disgust) %>%
  summarise(
    count = n(),
    total = n_distinct(row_number()), 
    .groups = 'drop'
  ) %>%
  group_by(experimental_group) %>%
  mutate(
    percentage = (count / sum(count)) * 100,
    se = sqrt((percentage / 100) * (1 - percentage / 100) / total) * 100, 
    ci_upper = percentage + 1.96 * se, 
    ci_lower = percentage - 1.96 * se  
  )


ggplot(summary_data, aes(x = as.factor(experimental_group), y = percentage, fill = as.factor(disgust))) + geom_bar(stat = "identity", position = "dodge", color = "black") + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = 0.9), width = 0.2) + labs(x = "Experimental Group", y = "Percentage of Answers", fill = "Disgust", title = NULL) + theme_classic() + scale_fill_manual(values = c("darkgrey","lightgray","white"), name = NULL, labels = c("Chemical weapons are more disgusting","Nuclear weapons are more disgusting","They are equally disgusting")) + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "bottom", legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.5)) + scale_x_discrete(labels = c("Control","Chemical","Nuclear")) + scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-5, 100)) + geom_text(aes(label = round(percentage, 2)), size = 4, color = "black", position = position_dodge(0.9), vjust = -10)
ggsave("disgust_experimental_group.png", dpi = 300)

## Adjusted predictions - ideology vs. predicted probability of preferring chemical weapons

range(dataset$ideology, na.rm = TRUE)

reg_aversion_m2 <- glm(formula = preference_binary ~ experimental_group + disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset, family = binomial(link = logit))
summary(reg_aversion_m2)

predict_model <- glm(formula = preference_binary ~ experimental_group + disgust_score_mean + as.integer(gender_numeric) + age + education + ideology + as.integer(nato) + as.integer(political_science), family = binomial(link = logit), data = dataset)
summary(predict_model)
ideology_sequence <- seq(from = 1, to = 7, by = 1)
ci <- data.frame(ideology = rep(ideology_sequence, each = 3), experimental_group = factor(rep(1:3, times = 7)), disgust_score_mean = mean(dataset$disgust_score_mean, na.rm = TRUE), gender_numeric = median(as.integer(dataset$gender_numeric), na.rm = TRUE), age = median(dataset$age, na.rm = TRUE), education = median(dataset$education, na.rm = TRUE), nato = median(as.integer(dataset$nato), na.rm = TRUE), political_science = median(as.integer(dataset$political_science), na.rm = TRUE))
ci$predict <- predict(predict_model, newdata = ci, type = "response")

ggplot(ci, aes(x = ideology, y = predict)) + geom_smooth(se = TRUE, color = "black") + theme_classic() + labs(x = "Ideology", y = "Preference of weapon system (binary)") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + ylim(0, 1) + xlim(1, 7)
ggsave("adjusted_prediction-ideology.png", dpi = 300)

## Adjusted predictions - gender vs. predicted probability of preferring chemical weapons

reg_aversion_m2 <- glm(formula = preference_binary ~ experimental_group + disgust_score_mean + gender_numeric + age + education + ideology_ternary + nato + political_science, data = dataset, family = binomial(link = logit))
summary(reg_aversion_m2)

effect_plot(reg_aversion_m2, pred = gender_numeric, interval = TRUE, line.thickness = 0.5) + theme_classic() + labs(x = "Gender", y = "Preference of weapon system (binary)") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "none") + ylim(0, 1) + scale_x_discrete(labels = c("Males","Females"))
ggsave("adjusted_prediction-gender.png", dpi = 300)

# Barplot - weapon systems matrix with gender disaggregation

weapons_data <- dataset[, c("gender_numeric", "chemical_weapons", "nuclear_weapons", "biological_weapons", 
                         "conventional_missiles", "machine_guns", "napalm", "antipersonnel_landmines", 
                         "grenade", "cluster_munition")]


weapons_long <- pivot_longer(weapons_data, 
                             cols = -gender_numeric, 
                             names_to = "weapon_type", 
                             values_to = "value") %>%
  filter(!is.na(value) & !is.na(gender_numeric))


weapons_summary <- weapons_long %>%
  group_by(weapon_type, gender_numeric) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE), 
    ci_low = mean_value - qt(0.975, df = n() - 1) * sd(value, na.rm = TRUE) / sqrt(n()),
    ci_high = mean_value + qt(0.975, df = n() - 1) * sd(value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )


weapons_summary <- weapons_summary %>% arrange(factor(weapon_type, levels = c("chemical_weapons", "nuclear_weapons", "biological_weapons", 
                                                           "conventional_missiles", "machine_guns", "napalm", "antipersonnel_landmines", 
                                                           "grenade", "cluster_munition")))

positions <- c("chemical_weapons", "nuclear_weapons", "biological_weapons", 
               "conventional_missiles", "machine_guns", "napalm", "antipersonnel_landmines", 
               "grenade", "cluster_munition")

ggplot(weapons_summary, aes(x = weapon_type, y = mean_value, fill = factor(gender_numeric))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(0.9), width = 0.2) +
  geom_text(aes(label = round(mean_value, 2)), 
            position = position_dodge(0.9), 
            vjust = 10, 
            size = 3) +
  labs(x = "Weapon system", 
       y = "Mean of disgust rating", 
       fill = NULL) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = "bottom", legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.5)) + scale_fill_manual(values = c("grey","white"), labels = c("Male","Female")) + scale_x_discrete(limits = positions, labels = c("Chemical weapons","Nuclear weapons","Biological weapons","Conventional missiles","Machine guns","Napalm","Antipersonnel landmines","Grenade","Cluster munition")) + coord_cartesian(ylim = c(1,6)) + scale_y_continuous(labels = c(1,2,3,4,5,6), breaks = c(1,2,3,4,5,6))
ggsave("weapon_systems_matrix_gender.png", dpi = 300)

################## EXPORT ##############################

ra1 <- reg_aversion_m1
ra2 <- reg_aversion_m2
ra3 <- reg_aversion_m3
ra4 <- reg_aversion_m4
ra5 <- reg_aversion_m5
stargazer(ra1,ra2,ra3,ra4,ra5, type = "html", out = "reg_aversion.html")

stargazer(reg_disgust_m1,reg_disgust_m2,reg_disgust_m3,type = "html", out = "reg_disgust.html")

e_cw_m1 <- reg_ethicality_cw_m1
e_cw_m2 <- reg_ethicality_cw_m2
e_nw_m1 <- reg_ethicality_nw_m1
e_nw_m2 <- reg_ethicality_nw_m2
stargazer(e_cw_m1,e_cw_m2,e_nw_m1,e_nw_m2,type = "html", out = "reg_ethicality_cw+nw.html")

m1 <- reg_cw_m1
m2 <- reg_nw_m1
m3 <- reg_bw_m1
m4 <- reg_conv_miss_m1
m5 <- reg_machine_guns_m1
m6 <- reg_napalm_m1
m7 <- reg_mines_m1
m8 <- reg_grenade_m1
m9 <- reg_cluster_m1
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9,type = "html", out = "reg_weapon_matrix.html")

