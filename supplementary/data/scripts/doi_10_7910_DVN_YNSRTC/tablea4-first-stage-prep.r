### Generate first stage probabilities by a leave-one-out (actually
### leave-municipality-out) process, where the first stage prediction for
### observation i of municipality m comes from a model whose training data
### contained everything but municipality m

date()
library("tidyverse")
library("caret")
library("foreach")
library("haven")
library("iterators")
library("randomForest")
sessionInfo()

raw_vict <- read_stata("violence_t_export.dta")


## Clean data, including factor outcome to play nice with caret
data_vict <- raw_vict %>%
    select(muni_code, victim_farc, victim_paras, nbi_t, royalties_t, coca_t,
           share_left, lpobl_tot_t, time, army_dist, gini_i, dmr, evlp, gcaribe,
           gpacifica, gorinoquia, gamazonia, farc_dist, paras_dist) %>%
    rename(victim_para = victim_paras,
           para_dist = paras_dist) %>%
    na.omit() %>%
    mutate(victim_farc_f = factor(victim_farc,
                                  levels = 0:1,
                                  labels = c("No", "Yes")),
           victim_para_f = factor(victim_para,
                                  levels = 0:1,
                                  labels = c("No", "Yes")))

## Variables for the FARC and paramilitary models
formula_farc <- victim_farc_f ~ nbi_t + royalties_t + coca_t + share_left +
    lpobl_tot_t + factor(time) + army_dist + gini_i + dmr +
    evlp + gcaribe + gpacifica + gorinoquia + gamazonia +
    farc_dist
formula_para <- update(formula_farc, victim_para_f ~ . - farc_dist + para_dist)

set.seed(59350)

muni_codes <- unique(data_vict$muni_code)
tab_out <- foreach (muni_out = muni_codes, i = icount(), .combine = "rbind") %do% {
    cat("municipality", i, "/", length(muni_codes), date(), "\n")

    data_train <- filter(data_vict, muni_code != muni_out)
    data_test <- filter(data_vict, muni_code == muni_out)

    ## Model each group's probability of victimizing civilians
    fit_farc <- train(formula_farc,
                      data = data_train,
                      method = "rf",
                      metric = "logLoss",
                      trControl = trainControl(
                          method = "cv",
                          number = 10,
                          summaryFunction = mnLogLoss,
                          classProbs = TRUE,
                          verboseIter = FALSE,
                          allowParallel = FALSE,
                          savePredictions = FALSE
                      ),
                      tuneLength = 5,
                      ntree = 1000,
                      importance = FALSE)
    fit_para <- train(formula_para,
                      data = data_train,
                      method = "rf",
                      metric = "logLoss",
                      trControl = trainControl(
                          method = "cv",
                          number = 10,
                          summaryFunction = mnLogLoss,
                          classProbs = TRUE,
                          verboseIter = FALSE,
                          allowParallel = FALSE,
                          savePredictions = FALSE
                      ),
                      tuneLength = 5,
                      ntree = 1000,
                      importance = FALSE)

    ## Extract predicted probabilities
    p_farc <- predict(fit_farc, newdata = data_test, type = "prob")[, "Yes"]
    p_para <- predict(fit_para, newdata = data_test, type = "prob")[, "Yes"]

    ans <- data_test %>%
        add_column(p_farc_rf = !!p_farc) %>%
        add_column(p_para_rf = !!p_para) %>%
        select(muni_code, time, p_farc_rf, p_para_rf)

    ans
}

write_csv(tab_out, file = "results-loo-first-stage.csv")


date()
