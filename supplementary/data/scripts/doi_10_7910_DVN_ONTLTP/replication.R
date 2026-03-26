

library(tidyverse)
library(quanteda)
library(quanteda.corpora) #devtools::install_github("quanteda/quanteda.corpora")
library(quanteda.textmodels)
library(tm)
library(ggjoy)
library(gridExtra)
library(readxl)
library(patchwork)
library(wnominate)
library(speech)
library(hrbrthemes)
library(rio)
library(puy) # https://nicolas-schmidt.github.io/puy/

source("R/functions.R")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## WNOMINATE -------------------------------------------------------------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat <- rio::import("data-raw/rollcall.xlsx") #'rollcall.xlsx'
dat2 <- wnuy(data = dat, legislature = 42:46, run = c(1, rep(3, 3), 15))
l42 <- run_wn(data = dat2[[1]], polar = 'Rovira, Nelson Lorenzo', legislature = 42)
l43 <- run_wn(data = dat2[[2]], polar = 'Cores, Hugo', legislature = 43)
l44 <- run_wn(data = dat2[[3]], polar = 'Sarthou, Helios', legislature = 44)
l45 <- run_wn(data = dat2[[4]], polar = 'Mujica, José', legislature = 45)
#l46 <- run_wn(data = dat2[[5]], polar = 'Roque Arregui', legislature = 46)
scores_wn <- rbind(l42, l43, l44, l45)
nom <- filter(scores_wn,  party %in% c("FA", "PN", "PC"))
nom$legis2 <- paste('Legistature', nom$legislature)
nom$legis3 <- header(nom$legislature)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## WORDSCORES ------------------------------------------------------------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

speech_uy <- rio::import("data-raw/speech_uy.rds") # 'speech_uy.rds'

vec_legis <- c(42:45)
salidas <- list()
for(i in 1:length(vec_legis)){
        salidas[[i]] <- ws_uy(data = speech_uy, legislature = vec_legis[i])
}
fin <- do.call(rbind, salidas)
fin <- filter(fin, partido %in% c("FA", "PN", "PC"))
fin$legislatura2 <- paste('Legistature', fin$legislatura)
fin$legislatura3 <- header(fin$legislatura)

#export(nom, "data/scoresNominates.xlsx")
#export(fin, "data/scoresWordscores.xlsx")



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## WORDFISH --------------------------------------------------------------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## legislatura 42
speech2 <-
     speech_uy %>%
     filter(chamber == "ASAMBLEA GENERAL", legislature==42) %>%
     speech_recompiler(compiler_by = c("legislator", "party", "legislature", "chamber"))
speech2$palabras  = speech_word_count(speech2$speech)
speech2$ref_score = NA
speech2$mediana   = median(speech2$palabras)
## SE PREPARA CORPUS -----------------------------------------------------------
corpus_speech <-  corpus(speech2$speech)
docvars(corpus_speech, "Party")       <- speech2$party
docvars(corpus_speech, "legislador")  <- speech2$legislator
## SE INICIA ESCALAMIENTO ------------------------------------------------------
dfm_tot <- dfm(corpus_speech,
               stem = TRUE,
               tolower = TRUE,
               remove = c(stopwords("spanish"),"Uruguay","uru*","senador","ley","artículo","proyecto"),
               remove_punct = TRUE,
               remove_numbers = TRUE,
               verbose = TRUE)
fish <- textmodel_wordfish(dfm_tot, sparse=TRUE)
fish$docs <- speech2$legislator
datos_42 <- data.frame(legislador  = fish$docs,
                       score       = fish$theta,
                       error       = fish$se.theta,
                       partido     = speech2$party)
datos_42$legislatura <- 42


## legislatura 43
speech2 <-
     speech_uy %>%
     filter(chamber == "ASAMBLEA GENERAL", legislature==43) %>%
     speech_recompiler(compiler_by = c("legislator", "party", "legislature", "chamber"))
speech2$palabras  = speech_word_count(speech2$speech)
speech2$ref_score = NA
speech2$mediana   = median(speech2$palabras)
## SE PREPARA CORPUS -----------------------------------------------------------
corpus_speech <-  corpus(speech2$speech)
docvars(corpus_speech, "Party")       <- speech2$party
docvars(corpus_speech, "legislador")  <- speech2$legislator
## SE INICIA ESCALAMIENTO ------------------------------------------------------
dfm_tot <- dfm(corpus_speech,
               stem = TRUE,
               tolower = TRUE,
               remove = c(stopwords("spanish"),"Uruguay","uru*","senador","ley","artículo","proyecto"),
               remove_punct = TRUE,
               remove_numbers = TRUE,
               verbose = TRUE)
fish <- textmodel_wordfish(dfm_tot, sparse=TRUE)
fish$docs <- speech2$legislator
datos_43 <- data.frame(legislador  = fish$docs,
                       score       = fish$theta,
                       error       = fish$se.theta,
                       partido     = speech2$party)
datos_43$legislatura <- 43


## legislatura 44
speech2 <-
     speech_uy %>%
     filter(chamber == "ASAMBLEA GENERAL", legislature==44) %>%
     speech_recompiler(compiler_by = c("legislator", "party", "legislature", "chamber"))
speech2$palabras  = speech_word_count(speech2$speech)
speech2$ref_score = NA
speech2$mediana   = median(speech2$palabras)
## SE PREPARA CORPUS -----------------------------------------------------------
corpus_speech <-  corpus(speech2$speech)
docvars(corpus_speech, "Party")       <- speech2$party
docvars(corpus_speech, "legislador")  <- speech2$legislator
## SE INICIA ESCALAMIENTO ------------------------------------------------------
dfm_tot <- dfm(corpus_speech,
               stem = TRUE,
               tolower = TRUE,
               remove = c(stopwords("spanish"),"Uruguay","uru*","senador","ley","artículo","proyecto"),
               remove_punct = TRUE,
               remove_numbers = TRUE,
               verbose = TRUE)
fish <- textmodel_wordfish(dfm_tot, sparse=TRUE)
fish$docs <- speech2$legislator
datos_44 <- data.frame(legislador  = fish$docs,
                       score       = fish$theta,
                       error       = fish$se.theta,
                       partido     = speech2$party)
datos_44$legislatura <- 44


## legislatura 45
speech2 <-
     speech_uy %>%
     filter(chamber == "ASAMBLEA GENERAL", legislature==45) %>%
     speech_recompiler(compiler_by = c("legislator", "party", "legislature", "chamber"))
speech2$palabras  = speech_word_count(speech2$speech)
speech2$ref_score = NA
speech2$mediana   = median(speech2$palabras)
## SE PREPARA CORPUS -----------------------------------------------------------
corpus_speech <-  corpus(speech2$speech)
docvars(corpus_speech, "Party")       <- speech2$party
docvars(corpus_speech, "legislador")  <- speech2$legislator
## SE INICIA ESCALAMIENTO ------------------------------------------------------
dfm_tot <- dfm(corpus_speech,
               stem = TRUE,
               tolower = TRUE,
               remove = c(stopwords("spanish"),"Uruguay","uru*","senador","ley","artículo","proyecto"),
               remove_punct = TRUE,
               remove_numbers = TRUE,
               verbose = TRUE)
fish <- textmodel_wordfish(dfm_tot, sparse=TRUE)
fish$docs <- speech2$legislator
datos_45 <- data.frame(legislador  = fish$docs,
                       score       = fish$theta,
                       error       = fish$se.theta,
                       partido     = speech2$party)
datos_45$legislatura <- 45



datos_total <- rbind.data.frame(datos_42, datos_43, datos_44, datos_45)
datos_total <- datos_total %>% filter(partido=="PN" | partido=="PC" | partido=="FA")
# datos_total$legislatura <- header(datos_total$legislatura)

graph_wf <-
     ggplot(datos_total) +
     xlim(-1.2, 1.2) +
     geom_joy( aes(x = score, y = partido, fill = partido), alpha = 0.4) +
     theme_joy(grid = TRUE ) +
     scale_y_discrete(breaks=c("FA", "PC", "PN"), expand = c(0.50, 0.05)) +
     scale_fill_grey(start = 0, end = .9, guide = FALSE) +
     facet_wrap(~legislatura, nrow = 1) +
     labs(title = "",
          x = "wordfish",
          y = "",
          caption = "PN: Partido Nacional; PC: Partido Colorado; FA: Frente Amplio"
     ) +
     theme_ipsum() +
     theme(panel.grid = element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
           panel.grid.minor.y = element_blank(),
           #axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
           #axis.text.y = element_text(size = rel(0.9)),
           axis.text.x=element_text(size=rel(0.9), angle=0),
           axis.line.x = element_line(),
           plot.title  = element_text(hjust = 0.5, face = "bold"),
           plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
           plot.caption = element_text(colour = "grey50"),
           text = element_text(family = "Arial Narrow"),
           #legend.title = element_blank(),
           legend.position = "bottom",
           legend.justification = "left",
           legend.direction = "vertical"
     )

graph_wf

ggsave("figures/wfish.png", width=8, height=5, dpi=700)



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## GRAPH NOM AND WS
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ROLLCALL
datos_total$legislatura <- header(datos_total$legislatura)

graph_nom <-
        ggplot(nom) +
        xlim(-1.2, 1.2) +
        geom_joy( aes(x = coord1D, y = party, fill = party), alpha = 0.4) +
        theme_joy(grid = TRUE ) +
        scale_y_discrete(breaks=c("FA", "PC", "PN"), expand = c(0.50, 0.05)) +
        scale_fill_grey(start = 0, end = .9, guide = FALSE) +
        facet_wrap(~substring(legis3, 13, nchar(legis3)), nrow = 1) +
        labs(title = "",
             x = "wnominate",
             y = "",
             caption = "PN: Partido Nacional; PC: Partido Colorado; FA: Frente Amplio"
             ) +
        theme_ipsum() +
        theme(panel.grid = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
          panel.grid.minor.y = element_blank(),
          #axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
          #axis.text.y = element_text(size = rel(0.9)),
          axis.text.x=element_text(size=rel(0.9), angle=0),
          axis.line.x = element_line(),
          plot.title  = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
          plot.caption = element_text(colour = "grey50"),
          text = element_text(family = "Arial Narrow"),
          #legend.title = element_blank(),
          legend.position = "bottom",
          legend.justification = "left",
          legend.direction = "vertical"
          )

graph_nom

ggsave("figures/nominates.png", width=8, height=5, dpi=700)

# SPEECH
graph_ws <-
        ggplot(fin) +
        xlim(-0.5, 0.5) +
        geom_joy( aes(x = score, y = partido, fill = partido), alpha = 0.4) +
        theme_joy(grid = TRUE ) +
        scale_y_discrete(breaks=c("FA", "PC", "PN"), expand = c(0.50, 0.05)) +
        scale_fill_grey(start = 0, end = .9, guide = FALSE) +
        facet_wrap(~substring(legislatura3, 13, nchar(legislatura3)), nrow = 1) +
        labs(title = "",
             x = "wordscores",
             y = "",
             caption = "PN: Partido Nacional; PC: Partido Colorado; FA: Frente Amplio"
             ) +
        theme_ipsum() +
        theme(panel.grid = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
          panel.grid.minor.y = element_blank(),
          #axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
          #axis.text.y = element_text(size = rel(0.9)),
          axis.text.x=element_text(size=rel(0.9), angle=0),
          axis.line.x = element_line(),
          plot.title  = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
          plot.caption = element_text(colour = "grey50"),
          text = element_text(family = "Arial Narrow"),
          #legend.title = element_blank(),
          legend.position = "bottom",
          legend.justification = "left",
          legend.direction = "vertical"
          )

graph_ws

ggsave("figures/wscores.png", width=8, height=5, dpi=700)



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## GRAPH NOM VS WS
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#nom <- import("data/WN.xlsx")
#fin <- import("data/WS.xlsx")


# ws
w <- fin['score']
names(w)[1] <- 'score_ws'
fin$legislador <- parse_names(fin$legislador)
w$id <- paste(fin$legislador, fin$legislatura, fin$partido, sep = "_")

# rc
n <- nom['coord1D']
names(n)[1] <- 'score_nom'
nom$legislator <- parse_names(nom$legislator)
n$id <- paste(nom$legislator, nom$legislature, nom$party, sep = "_")


# union
two <- full_join(w, n,  "id")
two <- two %>%
    separate(col = 2, into = c('name', 'legislature', 'party'), sep = "_") %>%
    filter(party %in% c('PN', 'PC', 'FA'))



two$legislature <- header(two$legislature) #paste0("Legislature ", two$legislature, "°")
two$ptyname <- ifelse(two$party == "FA", "Frente Amplio",
                      ifelse(two$party == "PN", "Partido Nacional", "Partido Colorado"))


ggplot(two, aes(x = score_nom, y = score_ws, fill = ptyname)) +
    geom_point(shape = 21, size = 3, alpha = 0.8, color = "black") +
    scale_fill_grey(start = 0, end = .9) +
    xlim(-1, 1) +
    ylim(-0.5, 0.5) +
    labs(y = 'wordscores', x = 'wnominate', fill = '') +
    #geom_smooth(method = lm, se = TRUE) +
    facet_wrap(~substring(legislature, 13, nchar(legislature)), nrow = 1) +
    theme_ipsum() +
    theme(panel.grid = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
          panel.grid.minor.y = element_blank(),
          #axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
          #axis.text.y = element_text(size = rel(0.9)),
          axis.text.x=element_text(size=rel(0.9), angle=0),
          axis.line.x = element_line(),
          plot.title  = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
          plot.caption = element_text(colour = "grey50"),
          text = element_text(family = "Arial Narrow"),
          #legend.title = element_blank(),
          legend.position = "bottom",
          legend.justification = "left",
          legend.direction = "vertical"
          )


ggsave("figures/corrWSWN.png", width=8, height=6, dpi=700)

#export(two, "data/WSWN.xlsx")
#two <- import("data/WSWN.xlsx")

two2 <- na.omit(two)
two2$id <- paste(two2$legislature, two2$party)
cor.test(two2[,1], two2[,5]) # correlation = 0.22

by(data = two2[,c(1, 5)],INDICES = factor(two2$legislature), cor)

cor.test(two2[two2$legislature == "Legislature: 1985 - 1990", 1],
         two2[two2$legislature == "Legislature: 1985 - 1990", 5])

cor.test(two2[two2$legislature == "Legislature: 1990 - 1995", 1],
         two2[two2$legislature == "Legislature: 1990 - 1995", 5])

cor.test(two2[two2$legislature == "Legislature: 1995 - 2000", 1],
         two2[two2$legislature == "Legislature: 1995 - 2000", 5])

cor.test(two2[two2$legislature == "Legislature: 2000 - 2005", 1],
         two2[two2$legislature == "Legislature: 2000 - 2005", 5])


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## GRAPH NOM VS WF
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#nom <- import("data/WN.xlsx")

# wf
wf <- datos_total['score']
names(wf)[1] <- 'score_wf'
datos_total$legislador <- parse_names(datos_total$legislador)
wf$id <- paste(datos_total$legislador, datos_total$legislatura, datos_total$partido, sep = "_")

# rc
n <- nom['coord1D']
names(n)[1] <- 'score_nom'
nom$legislator <- parse_names(nom$legislator)
n$id <- paste(nom$legislator, nom$legislature, nom$party, sep = "_")


# union
twolo <- full_join(wf, n,  "id")
twolo <- twolo %>%
    separate(col = 2, into = c('name', 'legislature', 'party'), sep = "_") %>%
    filter(party %in% c('PN', 'PC', 'FA'))



twolo$legislature <- header(twolo$legislature) #paste0("Legislature ", two$legislature, "°")
twolo$ptyname <- ifelse(twolo$party == "FA", "Frente Amplio",
                      ifelse(twolo$party == "PN", "Partido Nacional", "Partido Colorado"))


ggplot(twolo, aes(x = score_nom, y = score_wf, fill = ptyname)) +
    geom_point(shape = 21, size = 3, alpha = 0.8, color = "black") +
    scale_fill_grey(start = 0, end = .9) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    labs(y = 'wordfish', x = 'wnominate', fill = '') +
    #geom_smooth(method = lm, se = TRUE) +
    facet_wrap(~substring(legislature, 13, nchar(legislature)), nrow = 1) +
    theme_ipsum() +
    theme(panel.grid = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
          panel.grid.minor.y = element_blank(),
          #axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
          #axis.text.y = element_text(size = rel(0.9)),
          axis.text.x=element_text(size=rel(0.9), angle=0),
          axis.line.x = element_line(),
          plot.title  = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
          plot.caption = element_text(colour = "grey50"),
          text = element_text(family = "Arial Narrow"),
          #legend.title = element_blank(),
          legend.position = "bottom",
          legend.justification = "left",
          legend.direction = "vertical"
          )


ggsave("figures/corrWFWN.png", width=8, height=6, dpi=700)



#export(two, "data/WSWN.xlsx")
#two <- import("data/WSWN.xlsx")

two3 <- na.omit(twolo)
two3$id <- paste(two3$legislature, two3$party)
cor.test(two3[,1], two3[,5]) # correlation = 0.22

by(data = two3[,c(1, 5)],INDICES = factor(two3$legislature), cor)

cor.test(two3[two3$legislature == "Legislature: 1985 - 1990", 1],
         two3[two3$legislature == "Legislature: 1985 - 1990", 5])

cor.test(two3[two3$legislature == "Legislature: 1990 - 1995", 1],
         two3[two3$legislature == "Legislature: 1990 - 1995", 5])

cor.test(two3[two3$legislature == "Legislature: 1995 - 2000", 1],
         two3[two3$legislature == "Legislature: 1995 - 2000", 5])

cor.test(two3[two3$legislature == "Legislature: 2000 - 2005", 1],
         two3[two3$legislature == "Legislature: 2000 - 2005", 5])



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CONTROL SPEECH VETOS
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


vetos <- rio::import("data-raw/fecha_vetos.xlsx")
dv <- as.character(vetos$fecha)
speech_uy <- rio::import("data-raw/speech_uy.rds") # 'speech_uy.rds'
ubic_vetos <- which(speech_uy$date %in% dv)
speech_uy <- speech_uy[ubic_vetos, ]




# ------------------
# WF vs WS
# ------------------
# union
three <- full_join(wf, w,  "id")
three <- three %>%
    separate(col = 2, into = c('name', 'legislature', 'party'), sep = "_") %>%
    filter(party %in% c('PN', 'PC', 'FA'))

three$legislature <- header(three$legislature) #paste0("Legislature ", two$legislature, "°")
three$ptyname <- ifelse(three$party == "FA", "Frente Amplio",
                      ifelse(two$party == "PN", "Partido Nacional", "Partido Colorado"))


ggplot(three, aes(x = score_ws, y = score_wf, fill = ptyname)) +
     geom_point(shape = 21, size = 3, alpha = 0.8, color = "black") +
     scale_fill_grey(start = 0, end = .9) +
     xlim(-1, 1) +
     ylim(-1, 1) +
     labs(y = 'wordfish', x = 'wordscores', fill = '') +
     #geom_smooth(method = lm, se = TRUE) +
     facet_wrap(~substring(legislature, 13, nchar(legislature)), nrow = 1) +
     theme_ipsum() +
     theme(panel.grid = element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
           panel.grid.minor.y = element_blank(),
           #axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
           #axis.text.y = element_text(size = rel(0.9)),
           axis.text.x=element_text(size=rel(0.9), angle=0),
           axis.line.x = element_line(),
           plot.title  = element_text(hjust = 0.5, face = "bold"),
           plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
           plot.caption = element_text(colour = "grey50"),
           text = element_text(family = "Arial Narrow"),
           #legend.title = element_blank(),
           legend.position = "bottom",
           legend.justification = "left",
           legend.direction = "vertical"
     )

ggsave("figures/corrWFWS.png", width=8, height=6, dpi=700)


two4<-na.omit(three)

cor.test(two4[,1], two4[,5])

cor.test(two4[two4$legislature == "Legislature: 1985 - 1990", 1],
         two4[two4$legislature == "Legislature: 1985 - 1990", 5])

cor.test(two4[two4$legislature == "Legislature: 1990 - 1995", 1],
         two4[two4$legislature == "Legislature: 1990 - 1995", 5])

cor.test(two4[two4$legislature == "Legislature: 1995 - 2000", 1],
         two4[two4$legislature == "Legislature: 1995 - 2000", 5])

cor.test(two4[two4$legislature == "Legislature: 2000 - 2005", 1],
         two4[two4$legislature == "Legislature: 2000 - 2005", 5])


## -----------------------------------------------------------------------------
## OC
## -----------------------------------------------------------------------------
devtools::install_github("https://github.com/cran/oc")


out <- out[[1]]
base <- out
legis_vector <- character()
for(i in 1:nrow(base)){legis_vector[i] <- paste("Legislador", 99+i, sep = "")}
names <- paste(legis_vector, as.character(base[,1]))
base <- as.matrix(base)
label_party <- matrix(base[, 2], nrow = length(base[, 2]), ncol = 1)
colnames(label_party) <- "party"
base <- base[, -c(1,2,3,4)]


rc <- rollcall(base,
               yea = 1,
               nay = 4,
               notInLegis = 0,
               legis.names = names,
               legis.data = label_party,
               desc = "base Votes")



ocl <- oc(rc, dims = 1, polarity = c(207))
summary(ocl)

## -----------------------------------------------------------------------------
## ideal
## -----------------------------------------------------------------------------
dat <- rio::import(file.choose()) #'rollcall.xlsx'


legislature <- 45
base2 <- data
base2[is.na(base2)] <- 0L
out <- list()
for(i in 1:length(legislature)){
     legis  <- legislature[i]
     dataL  <- base2[base2$legislatura==legis, c(1:7,grep(paste("^a", legis, sep = ""), names(base2)))]
     dataL2 <- data.frame("NOMBRE"  = dataL[,2],
                          "LEMA"    = dataL[,6],
                          "SUBLEMA" = dataL[,7],
                          "LISTA"   = "list", stringsAsFactors = FALSE)
     dataL2 <- cbind(dataL2, dataL[,8:ncol(dataL)])
     x    <- dataL2
     out[[i]] <- dataL2

}
out


out <- out[[1]]
base <- out
legis_vector <- character()
for(i in 1:nrow(base)){legis_vector[i] <- paste("Legislador", 99+i, sep = "")}
names <- paste(legis_vector, as.character(base[,1]))
base <- as.matrix(base)
label_party <- matrix(base[, 2], nrow = length(base[, 2]), ncol = 1)
colnames(label_party) <- "party"
base <- base[, -c(1,2,3,4)]


rc <- rollcall(base,
               yea = 1,
               nay = 4,
               notInLegis = 0,
               legis.names = names,
               legis.data = label_party,
               desc = "base Votes")

iculo <- ideal(rc)

id <- as.data.frame(iculo$xbar)
id$NOMBRE <- substring(rownames(id), 15, 100)
rownames(id) <- NULL
id <- merge(id, out[, 1:2], by = "NOMBRE")


#l42 <- id %>% mutate(legis3 = "1985 - 1990")
#l43 <- id %>% mutate(legis3 = "1990 - 1995")
#l44 <- id %>% mutate(legis3 = "1995 - 2000", D1 = D1*-1)
#l45 <- id %>% mutate(legis3 = "2000 - 2005", D1 = D1*-1)


idl <- rbind(l42, l43, l44, l45)


nom <- idl %>% filter(LEMA %in% c("PC", "FA", "PN"))


## ROLLCALL
graph_nom <-
     ggplot(nom) +
     xlim(-1.2, 1.2) +
     geom_joy( aes(x = D1, y = LEMA, fill = LEMA), alpha = 0.4) +
     theme_joy(grid = TRUE ) +
     scale_y_discrete(breaks=c("FA", "PC", "PN"), expand = c(0.50, 0.05)) +
     scale_fill_grey(start = 0, end = .9, guide = FALSE) +
     facet_wrap(~legis3, nrow = 1) +
     labs(title = "",
          x = "Ideal point",
          y = "",
          caption = "PN: Partido Nacional; PC: Partido Colorado; FA: Frente Amplio"
     ) +
     theme_ipsum() +
     theme(panel.grid = element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
           panel.grid.minor.y = element_blank(),
           #axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
           #axis.text.y = element_text(size = rel(0.9)),
           axis.text.x=element_text(size=rel(0.9), angle=0),
           axis.line.x = element_line(),
           plot.title  = element_text(hjust = 0.5, face = "bold"),
           plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
           plot.caption = element_text(colour = "grey50"),
           text = element_text(family = "Arial Narrow"),
           #legend.title = element_blank(),
           legend.position = "bottom",
           legend.justification = "left",
           legend.direction = "vertical"
     )

graph_nom



ggsave("figures/ideal.png", width=8, height=5, dpi=700)



## -----------------------------------------------------------------------------
## Exploratory data analysis
## -----------------------------------------------------------------------------


ss <- rio::import(file.choose()) # 'speech_total_word.csv'
s2 <- as_tibble(ss)

out <- s2 %>%
     group_by(legislature) %>%
     summarise(legiladores     = length(unique(legislator)),
               sesiones        = length(unique(id)),
               speech          = length(speech),
               #partidos        = paste(unique(party), collapse = " - "),
               speech_promedio = ceiling(mean(word)),
               #speech_mediana = ceiling(median(word))
     ) %>% ungroup()

out <- merge(puy::legislaturas[, c("legislatura", "periodo")],
             out,
             by.y = "legislature",
             by.x = "legislatura")

out

#rio::export(s2, "speech_total_word.csv") # C:/Users/usuario/Documents
#rio::export(out, "Table1.xlsx") # C:/Users/usuario/Documents


# boxplot ------

speech <- s2
df <- speech %>% filter(party %in% c("FA", "PN", "PC"))
df$col <- ifelse(df$party == "FA", 2, ifelse(df$party == "PC", 1, 3))

theme_set(theme_minimal() + theme(legend.position = 'none',
                                  strip.text = element_text(size = 13),
                                  axis.text.x = element_text( size = 11 ),
                                  axis.text.y = element_text( size = 11)))

df <- merge(puy::legislaturas[, c("legislatura", "periodo")],
             df,
             by.y = "legislature",
             by.x = "legislatura")

df$periodo <- stringr::str_replace_all(df$periodo, pattern = "-", " - ")

ggplot(df, aes(party, word, fill = factor(col))) +
     stat_boxplot(geom ='errorbar',width= 0.25) +
     ylim(0,10000) +
     scale_fill_manual(values = c("gray88", "gray88", "gray88")) +
     geom_jitter(size=1, alpha=0.5) +
     geom_boxplot(alpha = 0.7,  width = 0.5, outlier.size = 0,
                  outlier.color  =  "black", outlier.alpha  =  0.1) +
     facet_wrap(~periodo, ncol = 6) +
     labs(title = "",
          x = "",
          y = "",
          fill = "",
          col = "",
          caption = "PN: Partido Nacional; PC: Partido Colorado; FA: Frente Amplio"
     ) +
     theme_ipsum() +
     theme(panel.grid = element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
           panel.grid.minor.y = element_blank(),
           axis.text.x=element_text(size=rel(0.9), angle=0),
           axis.line.x = element_line(),
           plot.title  = element_text(hjust = 0.5, face = "bold"),
           plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
           plot.caption = element_text(colour = "grey50"),
           text = element_text(family = "Arial Narrow"),
           legend.position = "none"
     )



ggsave("figures/eda.png", width=12, height=6, dpi=700)














