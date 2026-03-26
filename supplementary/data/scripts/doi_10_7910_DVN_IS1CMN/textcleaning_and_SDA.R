library(manifestoR)
mp_setapikey("manifesto_apikey.txt")
mpds <- mp_maindataset()
data <- subset(mpds)
data_selected <- data[, c("party", "date", "partyname",
                           "per202", "per305", "per408", "per410",
                           "per413", "per601", "per602", "per603", "per604")]

library(readxl)
my_data <- read_excel("task.xlsx")
library(dplyr)
countries_to_extract <- my_data %>%
  filter(polity2 > 5) %>%
  distinct(countryname) %>%
  pull(countryname)
library(purrr)
translation_lang <- "en"
corpus_list <- map(
  countries_to_extract,
  ~ tryCatch(
    mp_corpus(countryname == .x, translation = translation_lang, as_tibble = TRUE),
    error = function(e) NULL
  )
)
combined_corpus <- bind_rows(corpus_list, .id = "country_id")
combined_corpus

variable_corpus <- combined_corpus %>%
  left_join(
    data_selected,
    by = c("party", "date")
  )
variable_corpus
saveRDS(variable_corpus, "variable_corpus.rds")

variable_corpus <- readRDS("variable_corpus.rds")
my_data_sub <- my_data %>%
  dplyr::select(date, party, authoritarianism)
variable_corpus_new <- variable_corpus %>%
  dplyr::left_join(my_data_sub,
                   by = c("date", "party"))
variable_corpus_new
write.csv(variable_corpus_new, "manifesto_corpus.csv", row.names = FALSE)


library(dplyr)
variable_corpus_unique <- variable_corpus_new %>%
  distinct(manifesto_id, .keep_all = TRUE) %>%
  select(-text, -cmp_code, -eu_code, -pos)
vars_to_summarize <- c("per202", "per305", "per408", "per410", 
                       "per413", "per601", "per602", 
                       "per603", "per604", "authoritarianism")
variable_corpus_descript <- variable_corpus_unique %>%
  summarise(across(all_of(vars_to_summarize),
                   list(min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE),
                        mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))
variable_corpus_descript

###SDA: take economic goals as example###
library(dplyr)
library(tm)
library(caret)
library(sda)
original <- variable_corpus_new %>%
  mutate(
    authoritarianism_label = case_when(
      authoritarianism < 0.309117583 ~ 0,
      authoritarianism > 6.392642917 ~ 1,
      TRUE ~ NA_real_
    ),
    morality_label = case_when(
      per604 == 0 ~ 0,
      per604 > 1.420924858 ~ 1,
      TRUE ~ NA_real_
    ),
    life_label = case_when(
      per601 == 0 ~ 0,
      per601 > 6.351823202 ~ 1,
      TRUE ~ NA_real_
    ),
    eco_label = case_when(
      per408 == 0 ~ 0,
      per408 > 4.00450895 ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  select(text, manifesto_id,
         authoritarianism_label, morality_label,
         life_label, eco_label)

data <- original %>%
  select(text, eco_label) %>%
  filter(!is.na(eco_label))


data$eco_label <- as.factor(data$eco_label)
set.seed(20250607)
data <- data[sample(nrow(data)), ]

# preprocessing
data$text <- gsub("[[:punct:]]", " ", data$text)
data <- data %>%
  mutate(text = tolower(text))
corpus <- Corpus(VectorSource(data$text))
dtm <- DocumentTermMatrix(corpus)
dtm_sparse <- removeSparseTerms(dtm, 0.995)
dtm_matrix <- as.matrix(dtm_sparse)
colnames(dtm_matrix) <- make.names(colnames(dtm_matrix))
sda_data <- cbind(eco_label = data$eco_label, dtm_matrix)


set.seed(20250677)
train_idx <- sample(1:nrow(sda_data), size = 0.7 * nrow(sda_data))
train_data <- sda_data[train_idx, ]
test_data <- sda_data[-train_idx, ]
x_train <- as.matrix(train_data[, -1])
y_train <- train_data[, 1]
x_test <- as.matrix(test_data[, -1])
y_test <- test_data[, 1]

set.seed(42)
subsample_size <- 50000  # Adjust the upper limit based on memory
if (nrow(x_train) > subsample_size) {
  idx <- sample(1:nrow(x_train), subsample_size)
  x_train <- x_train[idx, ]
  y_train <- y_train[idx]
}

# train SDA
model <- sda(x_train, y_train)

predictions <- predict(model, x_test)$class

# evaluation
accuracy <- mean(predictions == y_test)
cat("accuracy：", accuracy, "\n")

summary(model)

# sda beta
beta_values <- model$beta
dim(beta_values)
beta_vector <- beta_values[1, ]
beta_table <- data.frame(Feature = names(beta_vector), Beta = beta_vector)

beta_vector2 <- beta_values[2, ]
beta_table2 <- data.frame(Feature = names(beta_vector2), Beta = beta_vector2)

write.csv(beta_table2, "eco_shrinkage_beta.csv", row.names = FALSE)
