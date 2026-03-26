library(tidyverse)
library(sandwich)
rm(list=ls())

data <- read.csv("Judicial Legitimacy- USA_August 29, 2023_07.23.csv")
data$PROLIFIC_PID
data_sever <- data[3:805,]
data_trust <- data_sever %>% select(Q14_3,Q13_3,Q15_3,Q16_3)
data_trust <- data_trust %>% mutate(across(everything(),as.numeric))
summary(data_trust)

colnames(data_trust) <- c("sc_trust","courts_trust","sc_distrust","courts_distrust")


data_trust$sc_distrust = 6-data_trust$sc_distrust
data_trust$courts_distrust=6-data_trust$courts_distrust

t.test(data_trust$sc_trust,data_trust$sc_distrust)
t.test(data_trust$courts_trust,data_trust$courts_distrust)
t.test(data_trust$courts_trust,data_trust$sc_trust)
write.csv(data_trust,"data_trust.csv")





#Confidence vs Trust
data_long <- data_sever %>% 
  gather(key = "question", value = "value", Q17_1, Q18_1) %>% select("question","value")
data_long$value <- as.numeric(data_long$value)
data_long <- data_long %>% na.omit()
t.test(data_long$value[which(data_long$question=="Q17_1",)],data_long$value[which(data_long$question=="Q18_1",)])
# Create the boxplot
png("csvscourts2.png", units="in", width=8, height=4, res=300)
ggpubr::ggboxplot(data_long, x = "question", y = "value",
                  color = "question", palette = c("#00AFBB", "#E7B800"),
                  add = "jitter", shape = "question") +
  scale_x_discrete(labels = c("Q17_1" = "confidence", "Q18_1" = "trust")) +
  labs(x = "Question wording", y = "responses")
dev.off()
####

#Trust v Distrust

data_long <- data_sever %>% 
  gather(key = "question", value = "value", Q14_3, Q15_3) %>% select("question","value")
data_long$value <- as.numeric(data_long$value)
data_long <- data_long %>% na.omit()
t.test(data_long$value[which(data_long$question=="Q14_3",)],data_long$value[which(data_long$question=="Q15_3",)])
# Create the boxplot
png("csvscourts3.png", units="in", width=8, height=4, res=300)
ggpubr::ggboxplot(data_long, x = "question", y = "value",
                  color = "question", palette = c("#00AFBB", "#E7B800"),
                  add = "jitter", shape = "question") +
  scale_x_discrete(labels = c("Q14_3" = "trust", "Q15_3" = "distrust")) +
  labs(x = "Question wording", y = "responses")
dev.off()
####
data_long <- data_sever %>% 
  gather(key = "question", value = "value", Q14_3, Q19_3) %>% select("question","value")
data_long$value <- as.numeric(data_long$value)
data_long <- data_long %>% na.omit()
t.test(data_long$value[which(data_long$question=="Q14_3",)],data_long$value[which(data_long$question=="Q19_3",)])
# Create the boxplot
png("csvscourts4.png", units="in", width=8, height=4, res=300)
ggpubr::ggboxplot(data_long, x = "question", y = "value",
                  color = "question", palette = c("#00AFBB", "#E7B800"),
                  add = "jitter", shape = "question") +
  scale_x_discrete(labels = c("Q14_3" = "Right for country as a whole", "Q19_3" = "You yourself")) +
  labs(x = "Question wording", y = "responses")
dev.off()
####



data_sever$score_courts <-  NA
data_sever$doaway <- NA
data_sever$limitjuris <-  NA
data_sever$trust <- NA
data_sever$mixedpolitics <- NA
data_sever$removejudges<- NA
data_sever$independance<- NA
data_sever$control<- NA
data_sever$judgestrust <- NA


#Supreme Court in the US |Trust
data_gibson_sc <- data_sever %>% select(starts_with("Q14"))
data_gibson_sc <- data_gibson_sc %>% mutate(across(everything(),as.numeric))
data_gibson2011 <- data_gibson_sc %>% select(-Q14_9,-Q14_10)
data_gibson2011$Q14_3 <- 6-data_gibson2011$Q14_3
psych::fa(data_gibson2011)
psych::alpha(data_gibson2011)
data_gibson_sc$court_scores <- psych::fa(data_gibson2011)$scores
data_sever$score_courts <-  data_gibson_sc$court_scores
data_sever$doaway <- ifelse(is.na(data_sever$doaway),data_gibson2011[,1],data_sever$doaway)
data_sever$limitjuris <-  ifelse(is.na(data_sever$limitjuris),data_gibson2011[,2],data_sever$limitjuris)
data_sever$trust <- ifelse(is.na(data_sever$trust),data_gibson2011[,3],data_sever$trust)
data_sever$mixedpolitics <- ifelse(is.na(data_sever$mixedpolitics),data_gibson2011[,4],data_sever$mixedpolitics)
data_sever$removejudges<- ifelse(is.na(data_sever$removejudges),data_gibson2011[,5],data_sever$removejudges)
data_sever$independance<- ifelse(is.na(data_sever$independance),data_gibson2011[,6],data_sever$independance)
data_sever$control<- ifelse(is.na(data_sever$control),data_gibson2011[,7],data_sever$control)
data_sever$judgestrust <- ifelse(is.na(data_sever$judgestrust),data_gibson2011[,8],data_sever$judgestrust)

#Supreme Court in the US | Distrust
data_gibson_sc <- data_sever %>% select(starts_with("Q15"))
data_gibson_sc <- data_gibson_sc %>% mutate(across(everything(),as.numeric))
data_gibson2011 <- data_gibson_sc %>% select(-Q15_9,-Q15_10)
psych::fa(data_gibson2011)
psych::alpha(data_gibson2011)
data_gibson_sc$court_scores <- psych::fa(data_gibson2011,nfactors = 1)$scores
data_sever$score_sc <-  data_gibson_sc$court_scores
data_sever$score_courts <-  data_gibson_sc$court_scores
data_sever$doaway <- ifelse(is.na(data_sever$doaway),data_gibson2011[,1],data_sever$doaway)
data_sever$limitjuris <-  ifelse(is.na(data_sever$limitjuris),data_gibson2011[,2],data_sever$limitjuris)
data_sever$trust <- ifelse(is.na(data_sever$trust),data_gibson2011[,3],data_sever$trust)
data_sever$mixedpolitics <- ifelse(is.na(data_sever$mixedpolitics),data_gibson2011[,4],data_sever$mixedpolitics)
data_sever$removejudges<- ifelse(is.na(data_sever$removejudges),data_gibson2011[,5],data_sever$removejudges)
data_sever$independance<- ifelse(is.na(data_sever$independance),data_gibson2011[,6],data_sever$independance)
data_sever$control<- ifelse(is.na(data_sever$control),data_gibson2011[,7],data_sever$control)
data_sever$judgestrust <- ifelse(is.na(data_sever$judgestrust),data_gibson2011[,8],data_sever$judgestrust)

sc_data_gibson <- cbind(data_sever$doaway,data_sever$limitjuris,data_sever$trust,data_sever$mixedpolitics,data_sever$removejudges,data_sever$independance,data_sever$control,data_sever$judgestrust)
sc_data_gibson <- as.data.frame(sc_data_gibson)
psych::fa(sc_data_gibson,nfactors =1)
data_sever$score_sc <- sc_data_gibson$scores
psych::alpha(sc_data_gibson)
sc_data_gibson <- cbind(data_sever$doaway,data_sever$limitjuris,data_sever$mixedpolitics,data_sever$removejudges,data_sever$independance,data_sever$control,data_sever$judgestrust)
sc_data_gibson <- as.data.frame(sc_data_gibson)
data_sever$score_sc2 <- psych::fa(sc_data_gibson,nfactors =1)$scores
psych::fa(sc_data_gibson,nfactors =1)
psych::alpha(sc_data_gibson)


data_long <- data_sever %>% 
  gather(key = "court_type", value = "diffuse_support", score_courts, score_sc)

# Create the boxplot
png("csvscourts.png", units="in", width=8, height=4, res=300)
ggpubr::ggboxplot(data_long, x = "court_type", y = "diffuse_support",
                  color = "court_type", palette = c("#00AFBB", "#E7B800"),
                  add = "jitter", shape = "court_type") +
  scale_x_discrete(labels = c("score_courts" = "Court Systems", "score_sc" = "Supreme Court")) +
  labs(x = "Court Type", y = "Diffuse Support")
dev.off()

sc_data_gibson <- sc_data_gibson %>% drop_na()
psych::scree(cor(sc_data_gibson))
psych::fac

data_sever$score_sc

data_sever$approval <-NA
data_sever$approval <- ifelse(is.na(data_sever$Q19_2),data_sever$Q20_2,data_sever$Q19_2)
data_sever$approval <- as.numeric(data_sever$approval)
data_sever$follow_courts <- ifelse(is.na(data_sever$Q23_1),as.numeric(data_sever$Q22_1),as.numeric(data_sever$Q23_1))

summary(model1 <- lm(data_sever$Q48_1~data_sever$score_sc))

summary(model2 <- lm(data_sever$follow_courts~data_sever$score_sc ))

summary(model3 <- lm(data_sever$approval~data_sever$score_sc))

summary(model2 <- lm(data_sever$follow_courts~data_sever$score_sc ))

summary(model3 <- lm(data_sever$Q48_1~data_sever$score_sc2  + data_sever$trust ))

summary(model3 <- lm(data_sever$approval~data_sever$score_sc2 + data_sever$trust))

summary(model4 <-lm( data_sever$follow_courts~data_sever$score_sc2 + data_sever$trust ))
##You should do what courts tell you to even if you do not agree 

summary(model5 <- lm(formula = data_sever$Q48_1 ~ data_sever$doaway + data_sever$limitjuris + 
     data_sever$mixedpolitics + data_sever$removejudges + data_sever$independance + 
     data_sever$control + data_sever$judgestrust + data_sever$trust))

summary(model6 <- lm(formula = data_sever$follow_courts ~ data_sever$doaway + data_sever$limitjuris + 
                       data_sever$mixedpolitics + data_sever$removejudges + data_sever$independance + 
                       data_sever$control + data_sever$judgestrust + data_sever$trust))

summary(model6 <- lm(formula = data_sever$approval ~ data_sever$doaway + data_sever$limitjuris + 
                       data_sever$mixedpolitics + data_sever$removejudges + data_sever$independance + 
                       data_sever$control + data_sever$judgestrust + data_sever$trust))

m1 <- texreg::extract(model1)
m2 <- texreg::extract(model2)
m3 <- texreg::extract(model3)
m4 <- texreg::extract(model4)
m5 <- texreg::extract(model5)
m6 <- texreg::extract(model6)

# Combine and display in LaTeX format
texreg::texreg(c(m1,m3,m2,m4,m5,m6), 
               digits = 2)






#Courts in the US | Trust
data_gibson_sc <- data_sever %>% select(starts_with("Q16"))
data_gibson_sc <- data_gibson_sc %>% mutate(across(everything(),as.numeric))
data_gibson2011 <- data_gibson_sc %>% select(-Q16_9,-Q16_10)
data_gibson2011$Q16_3 <- 6-data_gibson2011$Q16_3
psych::fa(data_gibson2011)
psych::alpha(data_gibson2011)
data_gibson_sc$court_scores <- psych::fa(data_gibson2011)$scores
data_sever$score_courts <-  ifelse(!is.na(data_sever$score_courts),data_sever$score_courts,data_gibson_sc$court_scores)
data_sever$score_courts <-  data_gibson_sc$court_scores
data_sever$doaway <- ifelse(is.na(data_sever$doaway),data_gibson2011[,1],data_sever$doaway)
data_sever$limitjuris <-  ifelse(is.na(data_sever$limitjuris),data_gibson2011[,2],data_sever$limitjuris)
data_sever$trust <- ifelse(is.na(data_sever$trust),data_gibson2011[,3],data_sever$trust)
data_sever$mixedpolitics <- ifelse(is.na(data_sever$mixedpolitics),data_gibson2011[,4],data_sever$mixedpolitics)
data_sever$removejudges<- ifelse(is.na(data_sever$removejudges),data_gibson2011[,5],data_sever$removejudges)
data_sever$independance<- ifelse(is.na(data_sever$independance),data_gibson2011[,6],data_sever$independance)
data_sever$control<- ifelse(is.na(data_sever$control),data_gibson2011[,7],data_sever$control)
data_sever$judgestrust <- ifelse(is.na(data_sever$judgestrust),data_gibson2011[,8],data_sever$judgestrust)

#Courts in the US | Distrust
data_gibson_sc <- data_sever %>% select(starts_with("Q13"))
data_gibson_sc <- data_gibson_sc %>% mutate(across(everything(),as.numeric))
data_gibson2011 <- data_gibson_sc %>% select(-Q13_9,-Q13_10)
psych::fa(data_gibson2011)
psych::alpha(data_gibson2011)
data_gibson_sc$court_scores <- psych::fa(data_gibson2011)$scores
data_sever$score_sc <-  ifelse(!is.na(data_sever$score_sc),data_sever$score_sc,data_gibson_sc$court_scores)
data_sever$score_courts <-  data_gibson_sc$court_scores
data_sever$doaway <- ifelse(is.na(data_sever$doaway),data_gibson2011[,1],data_sever$doaway)
data_sever$limitjuris <-  ifelse(is.na(data_sever$limitjuris),data_gibson2011[,2],data_sever$limitjuris)
data_sever$trust <- ifelse(is.na(data_sever$trust),data_gibson2011[,3],data_sever$trust)
data_sever$mixedpolitics <- ifelse(is.na(data_sever$mixedpolitics),data_gibson2011[,4],data_sever$mixedpolitics)
data_sever$removejudges<- ifelse(is.na(data_sever$removejudges),data_gibson2011[,5],data_sever$removejudges)
data_sever$independance<- ifelse(is.na(data_sever$independance),data_gibson2011[,6],data_sever$independance)
data_sever$control<- ifelse(is.na(data_sever$control),data_gibson2011[,7],data_sever$control)
data_sever$judgestrust <- ifelse(is.na(data_sever$judgestrust),data_gibson2011[,8],data_sever$judgestrust)

all_data_gibson <- c(data_sever$doaway,data_sever$limitjuris,data_sever$trust,data_sever$mixedpolitics,data_sever$removejudges,data_sever$independance,data_sever$control,data_sever$judgestrust)


#Gibson and Calideira (1992)
data_gibson1992_sc <- data_sever %>% select(Q14_1,Q15_1,Q14_2,Q15_2,Q70_1,Q70_2,Q70_3,PROLIFIC_PID)
data_gibson1992_sc <- data_gibson1992_sc %>% mutate(across(starts_with("Q"),as.numeric))
data_gibson1992_sc$doaway <- ifelse(is.na(data_gibson1992_sc$Q15_1),data_gibson1992_sc$Q14_1,data_gibson1992_sc$Q15_1)
data_gibson1992_sc$controv <- ifelse(is.na(data_gibson1992_sc$Q15_2),data_gibson1992_sc$Q14_2,data_gibson1992_sc$Q15_2)
data_gibson1992_sc <- data_gibson1992_sc %>% select(Q70_1,Q70_2,Q70_3,doaway,controv,PROLIFIC_PID) %>% na.omit()
data_gibson1992_sc$Q70_2 <- 6-data_gibson1992_sc$Q70_2
model <- psych::fa(data_gibson1992_sc[,-6])
data_gibson1992_sc$score_gc_1992 <- model$scores
data_temp <- data_gibson1992_sc %>% select(PROLIFIC_PID,score_gc_1992)
data_sever <- data_sever %>% left_join(data_temp)

#Gibson and Nelson(2015)
data_gibson2015_sc <- data_sever %>% select(Q14_1,Q15_1,Q14_2,Q15_2,Q14_4,Q15_4,Q14_5,Q15_5,Q14_6,Q15_6,Q14_7,Q15_7,Q14_8,Q15_8,PROLIFIC_PID)
data_gibson2015_sc <- data_gibson2015_sc %>% mutate(across(starts_with("Q"),as.numeric))
data_gibson2015_sc$doaway <- ifelse(is.na(data_gibson2015_sc$Q15_1),data_gibson2015_sc$Q14_1,data_gibson2015_sc$Q15_1)
data_gibson2015_sc$controv <- ifelse(is.na(data_gibson2015_sc$Q15_2),data_gibson2015_sc$Q14_2,data_gibson2015_sc$Q15_2)
data_gibson2015_sc$mixedup <- ifelse(is.na(data_gibson2015_sc$Q15_4),data_gibson2015_sc$Q14_4,data_gibson2015_sc$Q15_4)
data_gibson2015_sc$remove_judges <- ifelse(is.na(data_gibson2015_sc$Q15_5),data_gibson2015_sc$Q14_5,data_gibson2015_sc$Q15_5)
data_gibson2015_sc$less_independent <- ifelse(is.na(data_gibson2015_sc$Q15_6),data_gibson2015_sc$Q14_6,data_gibson2015_sc$Q15_6)
data_gibson2015_sc$controlcourts <- ifelse(is.na(data_gibson2015_sc$Q15_7),data_gibson2015_sc$Q14_7,data_gibson2015_sc$Q15_7)
data_gibson2015_sc$trust_judges <- ifelse(is.na(data_gibson2015_sc$Q15_8),data_gibson2015_sc$Q14_8,data_gibson2015_sc$Q15_8)

data_gibson2015_sc <- data_gibson2015_sc %>% select(-starts_with("Q")) %>% na.omit
model <- psych::fa(data_gibson2015_sc[,-1])
data_gibson2015_sc$score_gn_2015 <- model$scores
data_temp <- data_gibson2015_sc %>% select(PROLIFIC_PID,score_gn_2015)
data_sever <- data_sever %>% left_join(data_temp)



#Bartels and Johnston (2013)
data_bj2013_sc <- data_sever %>% select(Q14_1,Q15_1,Q14_2,Q15_2,Q14_3,Q15_3,Q14_8,Q15_8,Q14_9,Q15_9,Q17_3,Q18_3,Q19_3,Q20_3,PROLIFIC_PID)
data_bj2013_sc  <- data_bj2013_sc  %>% mutate(across(starts_with("Q"),as.numeric))
data_bj2013_sc$Q14_3 <- 6-data_bj2013_sc$Q14_3
data_bj2013_sc$doaway <- ifelse(is.na(data_bj2013_sc$Q15_1),data_bj2013_sc$Q14_1,data_bj2013_sc$Q15_1)
data_bj2013_sc$controv <- ifelse(is.na(data_bj2013_sc$Q15_2),data_bj2013_sc$Q14_2,data_bj2013_sc$Q15_2)
data_bj2013_sc$trust_best <- ifelse(is.na(data_bj2013_sc$Q15_3),data_bj2013_sc$Q14_3,data_bj2013_sc$Q15_3)
data_bj2013_sc$favor <- ifelse(is.na(data_bj2013_sc$Q15_9),data_bj2013_sc$Q14_9,data_bj2013_sc$Q15_9)
data_bj2013_sc$trust <- ifelse(is.na(data_bj2013_sc$Q19_3),data_bj2013_sc$Q20_3,data_bj2013_sc$Q19_3)
data_bj2013_sc$trust <- 6-data_bj2013_sc$trust
data_bj2013_sc <- data_bj2013_sc %>% select(-starts_with("Q")) %>% na.omit
model <- psych::fa(data_bj2013_sc[,-1])
psych::scree(data_bj2013_sc[,-1])
data_bj2013_sc$score_bj2013 <- model$scores
data_temp <- data_bj2013_sc %>% select(PROLIFIC_PID,score_bj2013)
data_sever <- data_sever %>% left_join(data_temp)

library(lavaan)

# Make an ordered dataframe with your 7 battery items (exclude trust_item)
legit7 <- data.frame(
  DOW = as.ordered(data_sever$doaway),
  LIM = as.ordered(data_sever$limitjuris),
  MIX = as.ordered(data_sever$mixedpolitics),
  REM = as.ordered(data_sever$removejudges),
  LES = as.ordered(data_sever$independance),    # rename to independence in data
  CTR = as.ordered(data_sever$control),
  JTR = as.ordered(data_sever$judgestrust)
)
TRU <- as.numeric(data_sever$trust)  # trust single item, numeric 1–5 (reversed correctly)

model <- '
G =~ DOW + LIM + MIX + REM + LES + CTR + JTR
G ~ TRU
'
fit <- cfa(model, data=cbind(legit7, TRU=TRU),
           estimator="WLSMV", ordered=colnames(legit7), std.lv=TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)

# Convergent correlation between latent G scores and trust
Gscores <- lavPredict(fit)  # factor scores for G
cor(Gscores, TRU, use="pairwise.complete.obs")
inspect(fit, "r2")         # should show ~.44 for G

###
# z-indices
idx7 <- scale(rowMeans(legit7_num, na.rm=TRUE))     # 7-item index (no trust)
tru  <- scale(TRU)
dv   <- as.numeric(data_sever$Q48_1)                # “obey even if disagree”

library(rsample); library(caret); set.seed(42)
df <- data.frame(dv, idx7=as.numeric(idx7), tru=as.numeric(tru))
folds <- vfold_cv(df, v=10)
eval <- function(form) map_df(folds$splits, ~{
  tr <- analysis(.x); te <- assessment(.x)
  fit <- lm(as.formula(form), data=tr)
  tibble(rmse = RMSE(predict(fit, te), te$dv),
         r2   = R2(predict(fit, te), te$dv))
})
cv_tru <- eval("dv ~ tru"); cv_7 <- eval("dv ~ idx7")
summarise_all(bind_rows(mutate(cv_tru, model="trust"),
                        mutate(cv_7,   model="battery7")), mean)


####
# Simple FA + base regressions
library(psych)

# ---------- 0) Build item matrix (rename ONE place if your variable names differ) ----------
items <- data.frame(
  DOW = as.numeric(data_sever$doaway),
  LIM = as.numeric(data_sever$limitjuris),
  TRU = as.numeric(data_sever$trust),            # the single trust item
  MIX = as.numeric(data_sever$mixedpolitics),
  REM = as.numeric(data_sever$removejudges),
  LES = as.numeric(data_sever$independance),     # if you've renamed to 'independence', change here
  CTR = as.numeric(data_sever$control),
  JTR = as.numeric(data_sever$judgestrust)
)

# Choose your DV(s)
dv_obey <- as.numeric(data_sever$Q48_1)          # “obey even if you disagree”
dv_approv <- suppressWarnings(as.numeric(data_sever$approval))  # if you created this earlier

# Keep only complete rows for items (+ each DV as used)
cc_items <- complete.cases(items)
items_cc <- items[cc_items, , drop = FALSE]
dv1 <- dv_obey[cc_items]
dv2 <- dv_approv[cc_items]

# ---------- 1) Factor scores: 7-item (no TRU) and 8-item (with TRU) ----------
seven <- items_cc[, c("DOW","LIM","MIX","REM","LES","CTR","JTR")]
eight <- items_cc[, c("DOW","LIM","MIX","REM","LES","CTR","JTR","TRU")]

fa7   <- fa(seven, nfactors = 1, fm = "minres", scores = "regression")
fa8   <- fa(eight, nfactors = 1, fm = "minres", scores = "regression")

score7 <- as.numeric(fa7$scores)   # latent legitimacy (7-item battery)
score8 <- as.numeric(fa8$scores)   # latent legitimacy (8-item battery)
tru    <- as.numeric(items_cc$TRU)  # single trust item

# ---------- 2) Replacement test: how well does TRU recover the 7-item latent? ----------
lat_rec <- lm(score7 ~ tru)
cat("\n[Replace battery] R^2(score7 ~ trust) =", round(summary(lat_rec)$r.squared, 3),
    " | cor =", round(cor(score7, tru, use = "pairwise.complete.obs"), 3), "\n")

# ---------- 3) Reliability: 7 vs 8 items ----------
a7 <- alpha(seven); a8 <- alpha(eight)
cat("\n[Alpha] 7-item alpha =", round(a7$total$raw_alpha, 3),
    " | 8-item alpha =", round(a8$total$raw_alpha, 3), "\n")

# ---------- 4) Simple predictive regressions (DV = obey). Repeat for approval if available ----------
z <- function(x) as.numeric(scale(x))

m_tru_1 <- lm(z(dv1) ~ z(tru))
m_7_1   <- lm(z(dv1) ~ z(score7))
m_8_1   <- lm(z(dv1) ~ z(score8))

cat("\n[DV: obey] Adj.R^2  trust-only =", round(summary(m_tru_1)$adj.r.squared, 3),
    " | 7-item =", round(summary(m_7_1)$adj.r.squared, 3),
    " | 8-item =", round(summary(m_8_1)$adj.r.squared, 3), "\n")

# Optional second DV (approval) if present with enough data
if (sum(!is.na(dv2)) > 100) {
  keep2 <- complete.cases(score7, score8, tru, dv2)
  m_tru_2 <- lm(z(dv2[keep2]) ~ z(tru[keep2]))
  m_7_2   <- lm(z(dv2[keep2]) ~ z(score7[keep2]))
  m_8_2   <- lm(z(dv2[keep2]) ~ z(score8[keep2]))
  cat("[DV: approval] Adj.R^2 trust-only =", round(summary(m_tru_2)$adj.r.squared, 3),
      " | 7-item =", round(summary(m_7_2)$adj.r.squared, 3),
      " | 8-item =", round(summary(m_8_2)$adj.r.squared, 3), "\n")
}

# ---------- 5) Incremental value (nested ANOVA on combined model) ----------
#   (a) Do the 7 items add anything beyond trust?
m_both_1 <- lm(z(dv1) ~ z(tru) + z(score7))
cat("\n[DV: obey] Increment beyond trust (add 7-item):\n"); print(anova(m_tru_1, m_both_1))

#   (b) Does trust add anything beyond the 7 items?
cat("\n[DV: obey] Increment beyond 7-item (add trust):\n"); print(anova(m_7_1, m_both_1))

# Repeat for approval if available
if (sum(!is.na(dv2)) > 100) {
  keep2 <- complete.cases(score7, tru, dv2)
  m_tru_2  <- lm(z(dv2[keep2]) ~ z(tru[keep2]))
  m_7_2    <- lm(z(dv2[keep2]) ~ z(score7[keep2]))
  m_both_2 <- lm(z(dv2[keep2]) ~ z(tru[keep2]) + z(score7[keep2]))
  cat("\n[DV: approval] Increment beyond trust (add 7-item):\n"); print(anova(m_tru_2, m_both_2))
  cat("\n[DV: approval] Increment beyond 7-item (add trust):\n"); print(anova(m_7_2,  m_both_2))
}

# ---------- 6) Quick AIC/BIC for parsimony (lower is better) ----------
ab <- function(f) c(AIC=AIC(f), BIC=BIC(f))
cat("\n[DV: obey] AIC/BIC\n"); print(rbind(trust_only=ab(m_tru_1), battery7=ab(m_7_1), battery8=ab(m_8_1)))


r2   <- function(m) summary(m)$r.squared
adjr <- function(m) summary(m)$adj.r.squared

delta_r2_add7  <- r2(m_both_1) - r2(m_tru_1)   # gain when adding score7 to trust
delta_r2_addTr <- r2(m_both_1) - r2(m_7_1)     # gain when adding trust to score7
delta_r2_add7; delta_r2_addTr

coef(summary(m_both_1))  # standardized betas & SEs for z(tru) and z(score7)

pred_tru <- fitted(m_tru_1)
pred_7   <- fitted(m_7_1)
plot(pred_7, pred_tru, xlab="Predicted (7-item)", ylab="Predicted (trust-only)")
abline(0,1,lty=2); cor(pred_7, pred_tru)  # high = similar predicted values

# partial eta^2 from your already-run ANOVAs
petasq <- function(a) { ss <- a$`Sum of Sq`[2]; rss2 <- a$RSS[2]; ss/(ss + rss2) }
a_add7  <- anova(m_tru_1, m_both_1)
a_addTr <- anova(m_7_1,  m_both_1)
partial_eta2_add7  <- petasq(a_add7)
partial_eta2_addTr <- petasq(a_addTr)
partial_eta2_add7; partial_eta2_addTr

summary(lm(score8 ~ tru))$r.squared
dev.off()
plot(fitted(m_7_1), fitted(m_tru_1),
     xlab="Predicted (7-item index)", ylab="Predicted (trust-only)")
abline(0,1,lty=2)


## ------------------------------
## 0) Helpers + DVs
## ------------------------------
std <- function(x) as.numeric(scale(x))  # z-score
to_num <- function(x) suppressWarnings(as.numeric(x))

# Agreement DV you already use:
agree <- to_num(data_sever$Q48_1)

# Legitimacy DV: try to auto-detect if you named it differently; else set NA
legit_candidates <- intersect(names(data_sever), c("Legitimacy", "legitimacy", "Q49_1", "Q47_1"))
legit <- if (length(legit_candidates)) to_num(data_sever[[legit_candidates[1]]]) else rep(NA_real_, nrow(data_sever))

## Optional robust SE support (comment out if you don't want it)
use_robust <- TRUE
if (use_robust && requireNamespace("lmtest", quietly = TRUE)) {
  library(lmtest)
  rcoefs <- function(m) lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC3"))
} else {
  rcoefs <- function(m) summary(m)$coefficients
}

## ------------------------------
## 1) TRUST vs CONFIDENCE frame
##    Items: Q18_1 (trust) vs Q17_1 (confidence)
## ------------------------------
d## --- TRUST vs CONFIDENCE (AGREEMENT) ---
# Build df_tc exactly as before
df_tc <- tibble::tibble(
  frame = dplyr::case_when(!is.na(data_sever$Q18_1) ~ "trust",
                           !is.na(data_sever$Q17_1) ~ "confidence",
                           TRUE ~ NA_character_),
  x_raw = dplyr::coalesce(suppressWarnings(as.numeric(data_sever$Q18_1)),
                          suppressWarnings(as.numeric(data_sever$Q17_1))),
  agree = suppressWarnings(as.numeric(data_sever$Q48_1))
) |>
  dplyr::filter(!is.na(frame), !is.na(x_raw), !is.na(agree))

# Make higher = more trust and standardize
std <- function(x) as.numeric(scale(x))
df_tc <- dplyr::mutate(df_tc, x = std(x_raw))

# Sanity check: do we actually have both frames?
print(table(df_tc$frame, useNA = "ifany"))
df_tc$frame <- droplevels(factor(df_tc$frame))

if (nlevels(df_tc$frame) >= 2) {
  m_tc_eq  <- lm(std(agree) ~ x + frame, data = df_tc)
  m_tc_int <- lm(std(agree) ~ x * frame, data = df_tc)
  cat("\n[Trust vs Confidence] AGREEMENT: slope-homogeneity test\n")
  print(anova(m_tc_eq, m_tc_int))      # ns => no slope differences
  print(summary(m_tc_int)$coefficients) # see x and x:frameconfidence
} else {
  # Fall back: estimate a single slope; report that only one frame is present
  m_tc_single <- lm(std(agree) ~ x, data = df_tc)
  cat("\n[Trust vs Confidence] Only one frame present (",
      levels(df_tc$frame), "). Running single-slope model.\n", sep = "")
  print(summary(m_tc_single)$coefficients)
}


## ------------------------------
## 2) CAN vs CANNOT (valence)
##    Items: Q14_3 (“can … trusted … country”) vs Q15_3 (“cannot … trusted … country”)
## ------------------------------
# Reverse the negative-valence response so higher = more trust
x_can     <- to_num(data_sever$Q14_3)
x_cannot  <- to_num(data_sever$Q15_3)
x_valence <- dplyr::coalesce(x_can, 6 - x_cannot)  # reverse "cannot"
frame_val <- dplyr::case_when(!is.na(x_can) ~ "can", !is.na(x_cannot) ~ "cannot")

df_val <- tibble::tibble(frame = frame_val, x = std(x_valence),
                         agree = agree, legit = legit) |>
  dplyr::filter(!is.na(frame), !is.na(x), !is.na(agree))

m_val_eq  <- lm(std(agree) ~ x + frame, data = df_val)
m_val_int <- lm(std(agree) ~ x * frame, data = df_val)
cat("\n[Can vs Cannot] AGREEMENT: slope-homogeneity test\n")
print(anova(m_val_eq, m_val_int))
print(rcoefs(m_val_int))

if (sum(!is.na(df_val$legit)) > 100) {
  m_val_eq_L  <- lm(std(legit) ~ x + frame, data = df_val)
  m_val_int_L <- lm(std(legit) ~ x * frame, data = df_val)
  cat("\n[Can vs Cannot] LEGITIMACY: slope-homogeneity test\n")
  print(anova(m_val_eq_L, m_val_int_L))
  print(rcoefs(m_val_int_L))
}
## ------------------------------
## 3) COUNTRY suffix vs PERSONAL (“you yourself”)
##    Items: Q14_3 (country) vs Q19_3 (personal trust)
## ------------------------------
x_country  <- to_num(data_sever$Q14_3)           # agreement with “right for the country …”
x_personal <- to_num(data_sever$Q19_3)           # “you yourself trust …”
# Both are already “higher=more trust” in your survey; standardize
x_suffix   <- dplyr::coalesce(x_country, x_personal)
frame_suf  <- dplyr::case_when(!is.na(x_country) ~ "country", !is.na(x_personal) ~ "personal")

df_suf <- tibble::tibble(frame = frame_suf, x = std(x_suffix),
                         agree = agree, legit = legit) |>
  dplyr::filter(!is.na(frame), !is.na(x), !is.na(agree))

m_suf_eq  <- lm(std(agree) ~ x + frame, data = df_suf)
m_suf_int <- lm(std(agree) ~ x * frame, data = df_suf)
cat("\n[Country vs Personal] AGREEMENT: slope-homogeneity test\n")
print(anova(m_suf_eq, m_suf_int))
print(rcoefs(m_suf_int))

if (sum(!is.na(df_suf$legit)) > 100) {
  m_suf_eq_L  <- lm(std(legit) ~ x + frame, data = df_suf)
  m_suf_int_L <- lm(std(legit) ~ x * frame, data = df_suf)
  cat("\n[Country vs Personal] LEGITIMACY: slope-homogeneity test\n")
  print(anova(m_suf_eq_L, m_suf_int_L))
  print(rcoefs(m_suf_int_L))
}

## ------------------------------
## 4) (Optional) Equivalence check for slope differences
##    CI-based TOST with a small margin (e.g., |Δβ| <= 0.10 SD)
## ------------------------------
equiv_margin <- 0.10

ci_beta_diff <- function(m, term) {
  cf <- coef(summary(m))
  if (!term %in% rownames(cf)) return(c(NA, NA))
  est <- cf[term, "Estimate"]; se <- cf[term, "Std. Error"]
  c(est - 1.645*se, est + 1.645*se)  # 90% CI (TOST)
}

# Example: Trust vs Confidence, AGREEMENT
ci_tc <- ci_beta_diff(m_tc_int, "x:frameconfidence")
cat("\n[Equivalence] Trust vs Confidence (AGREE) 90% CI for Δβ:", round(ci_tc,3),
    " | within ±", equiv_margin, "? ", ifelse(all(abs(ci_tc) <= equiv_margin), "YES", "NO"), "\n")

# Repeat for other contrasts if you like:
ci_val <- ci_beta_diff(m_val_int, "x:framecannot")
cat("[Equivalence] Can vs Cannot (AGREE)   90% CI for Δβ:", round(ci_val,3),
    " | within ±", equiv_margin, "? ", ifelse(all(abs(ci_val) <= equiv_margin), "YES", "NO"), "\n")

ci_suf <- ci_beta_diff(m_suf_int, "x:framepersonal")
cat("[Equivalence] Country vs Personal (AGREE) 90% CI for Δβ:", round(ci_suf,3),
    " | within ±", equiv_margin, "? ", ifelse(all(abs(ci_suf) <= equiv_margin), "YES", "NO"), "\n")

#####
## ---------- TRUST vs CONFIDENCE: predictive-slope equivalence ----------
std    <- function(x) as.numeric(scale(x))
to_num <- function(x) suppressWarnings(as.numeric(x))

# Predictor arms: Q18_1 = trust, Q17_1 = confidence
x_trust  <- to_num(data_sever$Q18_1)
x_conf   <- to_num(data_sever$Q17_1)
frame_tc <- dplyr::case_when(!is.na(x_trust) ~ "trust",
                             !is.na(x_conf)  ~ "confidence",
                             TRUE ~ NA_character_)
x_pred   <- dplyr::coalesce(x_trust, x_conf)   # higher = more (trust/confidence)

# Outcomes
agree <- to_num(data_sever$follow_courts)      # AGREEMENT
legit <- to_num(data_sever$Q48_1)              # LEGITIMACY

# Build analysis frames
df_tc_A <- tibble::tibble(frame = frame_tc, x = std(x_pred), y = std(agree)) |>
  dplyr::filter(!is.na(frame), !is.na(x), !is.na(y))
df_tc_L <- tibble::tibble(frame = frame_tc, x = std(x_pred), y = std(legit)) |>
  dplyr::filter(!is.na(frame), !is.na(x), !is.na(y))

# Force 'trust' as reference (so the interaction is the slope difference vs confidence)
df_tc_A$frame <- stats::relevel(factor(df_tc_A$frame), ref = "trust")
df_tc_L$frame <- stats::relevel(factor(df_tc_L$frame), ref = "trust")

run_slope_test <- function(df, label){
  cat("\n[Trust vs Confidence] ", label, " — counts by frame:\n", sep = "")
  print(table(df$frame))
  if (nlevels(df$frame) < 2L) {
    cat("Only one frame present → running single-slope model.\n")
    print(summary(lm(y ~ x, data = df))$coefficients)
    return(invisible(NULL))
  }
  m_eq  <- lm(y ~ x + frame, data = df)
  m_int <- lm(y ~ x * frame, data = df)
  cat("\nEqual-slope vs interacted-slope (ANOVA):\n")
  print(anova(m_eq, m_int))                 # ns => no slope differences
  cat("\nInteracted model coefficients (x = slope for TRUST; x:frameconfidence = Δslope):\n")
  print(summary(m_int)$coefficients)
  
  # 90% CI for the slope difference (TOST-style equivalence check)
  cf <- summary(m_int)$coefficients
  term <- "x:frameconfidence"
  if (term %in% rownames(cf)) {
    est <- cf[term, "Estimate"]; se <- cf[term, "Std. Error"]
    ci90 <- c(est - 1.645*se, est + 1.645*se)
    cat("\n90% CI for Δβ (confidence − trust):", round(ci90, 3), "\n")
  }
}

run_slope_test(df_tc_A, "AGREEMENT (follow_courts)")
run_slope_test(df_tc_L, "LEGITIMACY (Q48_1)")
####
## Rebuild outcomes after rm(list=ls())
to_num <- function(x) suppressWarnings(as.numeric(x))
std    <- function(x) as.numeric(scale(x))

# Agreement (follow_courts): reconstruct from Q23_1 or Q22_1
data_sever$follow_courts <- to_num(dplyr::coalesce(data_sever$Q23_1, data_sever$Q22_1))
# Legitimacy (self-report): you said it's Q48_1
data_sever$Q48_1 <- to_num(data_sever$Q48_1)

# Predictor arms: Q18_1 = trust, Q17_1 = confidence
x_trust  <- to_num(data_sever$Q18_1)
x_conf   <- to_num(data_sever$Q17_1)
frame_tc <- dplyr::case_when(!is.na(x_trust) ~ "trust",
                             !is.na(x_conf)  ~ "confidence",
                             TRUE ~ NA_character_)
x_pred   <- dplyr::coalesce(x_trust, x_conf)  # higher = more (trust/conf)

# Build analysis frames
df_tc_A <- tibble::tibble(frame = frame_tc, x = std(x_pred), y = std(data_sever$follow_courts)) |>
  dplyr::filter(!is.na(frame), !is.na(x), !is.na(y))
df_tc_L <- tibble::tibble(frame = frame_tc, x = std(x_pred), y = std(data_sever$Q48_1)) |>
  dplyr::filter(!is.na(frame), !is.na(x), !is.na(y))

# Force 'trust' as reference
df_tc_A$frame <- stats::relevel(factor(df_tc_A$frame), ref = "trust")
df_tc_L$frame <- stats::relevel(factor(df_tc_L$frame), ref = "trust")

run_slope_test <- function(df, label){
  cat("\n[Trust vs Confidence] ", label, " — counts by frame:\n", sep = "")
  print(table(df$frame))
  if (nlevels(df$frame) < 2L) {
    cat("Only one frame present → running single-slope model.\n")
    print(summary(lm(y ~ x, data = df))$coefficients)
    return(invisible(NULL))
  }
  m_eq  <- lm(y ~ x + frame, data = df)
  m_int <- lm(y ~ x * frame, data = df)
  cat("\nEqual-slope vs interacted-slope (ANOVA):\n")
  print(anova(m_eq, m_int))
  cat("\nInteracted model coefficients (x = slope for TRUST; x:frameconfidence = Δslope):\n")
  cf <- summary(m_int)$coefficients
  print(cf)
  if ("x:frameconfidence" %in% rownames(cf)) {
    est <- cf["x:frameconfidence","Estimate"]; se <- cf["x:frameconfidence","Std. Error"]
    ci90 <- c(est - 1.645*se, est + 1.645*se)
    cat("\n90% CI for Δβ (confidence − trust):", round(ci90, 3), "\n")
  }
}

run_slope_test(df_tc_A, "AGREEMENT (follow_courts)")
run_slope_test(df_tc_L, "LEGITIMACY (Q48_1)")

################# R&R ############
## =========================
## ROBUSTNESS 1A: Party heterogeneity
## =========================
library(lmtest)

# REPLACE this with your party ID variable (example names):
# party <- factor(data_sever$party_id)  # e.g., 1=Dem, 2=Rep, 3=Ind
party<- data_sever$Q7


# If you have strings already, great:
party <- factor(party)  # <-- change this line

library(lmtest)
library(sandwich)

# Build dfp as you did
party <- factor(data_sever$Q7)
dfp <- data.frame(
  dv     = suppressWarnings(as.numeric(data_sever$Q48_1)),
  score7 = suppressWarnings(as.numeric(data_sever$score_sc2)),
  trust  = suppressWarnings(as.numeric(data_sever$trust)),
  party  = party
)

dfp <- dfp[complete.cases(dfp), ]
dfp$party <- droplevels(dfp$party)     # IMPORTANT

# Standardise ONCE (full sample), so SD is not 0 within groups
dfp$dv_z    <- as.numeric(scale(dfp$dv))
dfp$trust_z <- as.numeric(scale(dfp$trust))

# Quick diagnostics (optional)
table(dfp$party)
tapply(dfp$trust, dfp$party, sd)

# (i) Separate slopes by party, skipping tiny / no-variance groups
by(dfp, dfp$party, function(d){
  if (nrow(d) < 20) return(paste("Too few cases:", nrow(d)))
  if (sd(d$trust, na.rm=TRUE) == 0) return(paste("No variance in trust:", nrow(d)))
  m <- lm(dv_z ~ trust_z, data=d)
  coeftest(m, vcov.=vcovHC(m, type="HC3"))
})

# (ii) Interaction model (preferred robustness): does slope differ by party?
m_int <- lm(dv_z ~ trust_z * party, data=dfp)
coeftest(m_int, vcov.=vcovHC(m_int, type="HC3"))
anova(m_int)

#####
library(forcats)

dfp3 <- dfp %>%
  mutate(pid3 = fct_collapse(
    party,
    Democrat = c("Strong Democrat","Weak Democrat","Independent but Lean Democrat"),
    Republican = c("Strong Republican","Weak Republican","Independent but lean Republican"),
    Independent = c("Independent")
  )) %>%
  filter(!is.na(pid3)) %>%
  droplevels()

m_pid3 <- lm(dv_z ~ trust_z * pid3, data=dfp3)
coeftest(m_pid3, vcov.=vcovHC(m_pid3, type="HC2"))
anova(m_pid3)

####

library(dplyr)
library(forcats)

dfp3 <- dfp %>%
  mutate(pid3 = fct_collapse(
    party,
    Democrat    = c("Strong Democrat","Weak Democrat","Independent but Lean Democrat"),
    Republican  = c("Strong Republican","Weak Republican","Independent but lean Republican"),
    Independent = c("Independent"),
    .other_level = "Other"
  )) %>%
  filter(pid3 != "Other") %>%     # drop stray categories
  droplevels()

table(dfp3$pid3)


# Party ID (Q7): remove blanks before factor()
party_raw <- as.character(data_sever$Q7)
party_raw <- trimws(party_raw)
party_raw[party_raw == ""] <- NA   # drop blank responses

party <- droplevels(factor(party_raw))
dfp <- data.frame(
  dv     = suppressWarnings(as.numeric(data_sever$Q48_1)),
  score7 = suppressWarnings(as.numeric(data_sever$score_sc2)),
  trust  = suppressWarnings(as.numeric(data_sever$trust)),
  party  = party
)

dfp <- dfp[complete.cases(dfp), ]
dfp$party <- droplevels(dfp$party)

library(forcats)
dfp3 <- dfp %>%
  mutate(pid3 = fct_collapse(
    party,
    Democrat    = c("Strong Democrat","Weak Democrat","Independent but Lean Democrat"),
    Republican  = c("Strong Republican","Weak Republican","Independent but lean Republican"),
    Independent = c("Independent")
  )) %>%
  filter(!is.na(pid3)) %>%
  droplevels()
m_pid3 <- lm(dv_z ~ trust_z * pid3, data=dfp3)
coeftest(m_pid3, vcov.=vcovHC(m_pid3, type="HC1"))
anova(m_pid3)


######
library(dplyr)
library(forcats)

# Starting from dfp (which has dv, trust, party)
dfp3 <- dfp %>%
  mutate(
    pid3 = fct_collapse(
      party,
      Democrat    = c("Strong Democrat","Weak Democrat","Independent but Lean Democrat"),
      Republican  = c("Strong Republican","Weak Republican","Independent but lean Republican"),
      Independent = c("Independent"),
      .other_level = NA_character_
    )
  ) %>%
  filter(!is.na(pid3)) %>%
  mutate(
    dv_z    = as.numeric(scale(dv)),
    trust_z = as.numeric(scale(trust))
  ) %>%
  droplevels()
m_pid3 <- lm(dv_z ~ trust_z * pid3, data=dfp3)
anova(m_pid3)


