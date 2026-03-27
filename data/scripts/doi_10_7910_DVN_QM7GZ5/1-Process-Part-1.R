rm(list=ls())
library(data.table)
library(stringr)

# NOTE: Adjust "./" to your directory

# Load Qualtrics
survey <- fread("./Data-Raw/Qualtrics-Part1.csv")
setnames(survey, "ResponseId", "qid")
question_cols <- paste0("Q", 2:51)
questions <- survey[, (c("qid", question_cols)), with=F]
basics <- survey[, .(duration=as.numeric(`Duration (in seconds)`),
                     qid, color=Q53, movie=Q54,
                     drink=Q55, age=Q56, gender=Q57, subject=Q58,
                     sport=Q59)]
questions <- melt(questions, id.vars="qid", variable.name="qnum", value.name="answer")
questions[, qnum := as.numeric(gsub("Q", "", as.character(qnum))) - 1]
model.answers <- questions[, .(.N), .(qnum, answer)]
model.answers[, nmax := max(N), qnum]
model.answers <- model.answers[N==nmax, .(qnum, correct=answer)]
questions <- merge(questions, model.answers, by="qnum")
scores <- questions[, .(score=sum(correct==answer)), .(qid)]
info <- merge(basics, scores, by="qid")

# Remove duration 1-99 (229 secs and 3270 secs)
cuts <- info[, quantile(duration, c(0.01, 0.99), type=1)]
info <- info[duration %between% cuts]

# Merge M-Turk
d1 <- fread("./Data-Extract/MTurk-US-Part1.csv")[, .(qid=Answer.surveycode, country="United States")]
d2 <- fread("./Data-Extract/MTurk-India-Part1.csv")[, .(qid=Answer.surveycode, country="India")]
dt <- rbind(d1, d2, fill=T)
info <- merge(info, dt, by="qid")

info <- info[, .(qid, duration=as.integer(duration), score, country, gender, age=as.integer(age), 
                 subject=str_to_title(subject), 
                 sport=str_to_title(sport), 
                 color=str_to_title(color), 
                 movie=str_to_title(movie), drink)]

fwrite(info, file="./Data-Extract/Profiles.csv")