## legislatoR paper --------------------------------
## Sascha Göbel, Simon Munzert ---------------------


## load packages and functions ------------------
source("packages-add-applications")


## download raw HTMLs ---------------------------
options(timeout=5)


# download function
download_articles <- function(url, path){
  filename <- paste0(str_replace_all(basename(url), "/", ""), ".html")
  if(!file.exists(paste0(path, filename))){
    try(download.file(url, destfile = paste0(path, filename)))
    Sys.sleep(runif(1, 0, 1))
  }
}

# create data folders
legislature_names <- names(cld_content())
legislature_folders <- paste0("../data/wikipedia_htmls/", legislature_names)
map(legislature_folders, dir.create)

# download htmls
dat_aut <- right_join(x = get_core(legislature = "aut"),
                      y = get_political(legislature = "aut"), 
                      by = "pageid")
dat_aut$legislature <- "Austria"
aut_urls <- paste0("https://de.wikipedia.org/wiki/", dat_aut$wikititle)
map(aut_urls, download_articles, path = "../data/wikipedia_htmls/aut/")

dat_can <- get_core(legislature = "can")
can_urls <- paste0("https://en.wikipedia.org/wiki/", dat_can$wikititle)
map(can_urls, download_articles, path = "../data/wikipedia_htmls/can/")

dat_ger <- get_core(legislature = "deu")
ger_urls <- paste0("https://de.wikipedia.org/wiki/", dat_ger$wikititle)
map(ger_urls, download_articles, path = "../data/wikipedia_htmls/deu/")

dat_cze <- get_core(legislature = "cze")
cze_urls <- paste0("https://cs.wikipedia.org/wiki/", dat_cze$wikititle)
map(cze_urls, download_articles, path = "../data/wikipedia_htmls/cze/")

dat_esp <- get_core(legislature = "esp")
esp_urls <- paste0("https://es.wikipedia.org/wiki/", dat_esp$wikititle)
map(esp_urls, download_articles, path = "../data/wikipedia_htmls/esp/")

dat_fra <- get_core(legislature = "fra")
fra_urls <- paste0("https://fr.wikipedia.org/wiki/", dat_fra$wikititle)
map(fra_urls, download_articles, path = "../data/wikipedia_htmls/fra/")

dat_irl <- get_core(legislature = "irl")
irl_urls <- paste0("https://en.wikipedia.org/wiki/", dat_irl$wikititle)
map(irl_urls, download_articles, path = "../data/wikipedia_htmls/irl/")

dat_sco <- get_core(legislature = "sco")
sco_urls <- paste0("https://en.wikipedia.org/wiki/", dat_sco$wikititle)
map(sco_urls, download_articles, path = "../data/wikipedia_htmls/sco/")

dat_gbr <- get_core(legislature = "gbr")
gbr_urls <- paste0("https://en.wikipedia.org/wiki/", dat_gbr$wikititle)
map(gbr_urls, download_articles, path = "../data/wikipedia_htmls/gbr/")

dat_usa_house <- get_core(legislature = "usa_house")
usa_house_urls <- paste0("https://en.wikipedia.org/wiki/", dat_usa_house$wikititle)
map(usa_house_urls, download_articles, path = "../data/wikipedia_htmls/usa_house/")

dat_usa_senate <- get_core(legislature = "usa_senate")
usa_senate_urls <- paste0("https://en.wikipedia.org/wiki/", dat_usa_senate$wikititle)
map(usa_senate_urls, download_articles, path = "../data/wikipedia_htmls/usa_senate/")


# create list of dfs
dat_list <- list()
for (i in seq_along(legislature_names)){
  dat_list[[i]] <- right_join(x = get_core(legislature = legislature_names[i]),
             y = get_political(legislature = legislature_names[i]), 
             by = "pageid")
  dat_list[[i]]$legislature <- legislature_names[i]
}


## import HTMLs ---------------------------

filenames_list <- list()
parsed_list <- list()
for (i in seq_along(legislature_names)){
  htmls <- list.files(paste0("../data/wikipedia_htmls/", legislature_names[i]), full.names = TRUE)
  filenames_list[[i]] <- basename(htmls) %>% str_replace(".html$", "") %>% sapply(URLencode)
  parsed_list[[i]] <- lapply(htmls, read_html)
}


## generate graph based on links between articles; paragraphs only -----

# define function
generate_graph <- function(files_parsed, filenames) {
  connections <- data.frame(from = NULL, to = NULL)
  for (i in seq_along(files_parsed)) {
    pslinks <- html_attr(
      html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
      "href")
    links_in_pslinks <- seq_along(files_parsed)[filenames %in% basename(pslinks)]
    links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
    connections <- rbind(
      connections,
      data.frame(
        from = rep(i, length(links_in_pslinks)), # -1 for zero-indexing
        to = links_in_pslinks # here too
      )
    )
    if (i%%1000==0) { print(paste0(i, " cases"))}
  }
  # add artificial edge for last observation to get length of graph right
  connections[nrow(connections+1),] <- c(length(filenames), length(filenames)-1) 
  names(connections) <- c("from", "to")
  # generate graph
  graph_out <- igraph::graph_from_edgelist(as.matrix(connections), directed = TRUE)
  graph_out
}



# run function
graph_list <- list()
for (i in seq_along(legislature_names)){
  print(legislature_names[i])
  graph_list[[i]] <- try(generate_graph(parsed_list[[i]], filenames_list[[i]]))
}
save(graph_list, file = "../data/graph_list.RData")


## compute pageRank ----------

pagerank_df_list <- list()
for (i in seq_along(legislature_names)){
  pagerank_df_list[[i]] <- data.frame(
    wikititle = filenames_list[[i]], 
    pagerank = graph_list[[i]] %>% page.rank(directed = TRUE) %>%  use_series("vector") , 
    stringsAsFactors = FALSE)
  }


## generate sessions dfs ------

sessions_df_list <- list()
for (i in seq_along(legislature_names)){
  sessions_df_list[[i]] <- 
    left_join(dat_list[[i]], pagerank_df_list[[i]], by = "wikititle") %>% 
    mutate(female = (sex == "female")) %>% 
    group_by(session) %>% 
    mutate(pr_rank = rank(-pagerank, ties.method = "min")) %>%
    ungroup() %>% 
    group_by(session) %>% 
    summarize(pagerank_female_mean = mean(pagerank[female == TRUE], na.rm = TRUE),
              pagerank_male_mean = mean(pagerank[female == FALSE], na.rm = TRUE),
              pr_rank_female_mean = mean(pr_rank[female == TRUE], na.rm = TRUE),
              pr_rank_male_mean = mean(pr_rank[female == FALSE], na.rm = TRUE),
              pr_rank_female_median = median(pr_rank[female == TRUE], na.rm = TRUE),
              pr_rank_male_median = median(pr_rank[female == FALSE], na.rm = TRUE),
              pagerank_mean_ratio = pagerank_female_mean/pagerank_male_mean,
              pr_rank_mean_ratio = pr_rank_female_mean/pr_rank_male_mean,
              pr_rank_median_ratio = pr_rank_female_median/pr_rank_male_median,
              female_ratio_top10 = mean(female[pr_rank <= 10], na.rm = TRUE),
              female_ratio_top25 = mean(female[pr_rank <= 25], na.rm = TRUE),
              female_ratio_top50 = mean(female[pr_rank <= 50], na.rm = TRUE),
              female_ratio_top100 = mean(female[pr_rank <= 100], na.rm = TRUE),
              female_ratio_top5perc = mean(female[pr_rank <= .05*n()], na.rm = TRUE),
              female_ratio_top10perc = mean(female[pr_rank <= .10*n()], na.rm = TRUE),
              female_ratio_top25perc = mean(female[pr_rank <= .25*n()], na.rm = TRUE),
              female_ratio_top50perc = mean(female[pr_rank <= .50*n()], na.rm = TRUE),
              female_ratio = mean(female, na.rm = TRUE),
              n_members = n(),
              session_start = ymd(first(session_start)), 
              session_end = ymd(first(session_end)),
              legislature = first(legislature)) %>%
    arrange(as.numeric(session))
}


## plot data ------

sessions_df_long <- bind_rows(sessions_df_list)
sessions_df_long$legislature_label <- recode(sessions_df_long$legislature, 
                                             "aut" = "Austria",
                                             "can" = "Canada",
                                             "cze" = "Czech Republic",
                                             "deu" = "Germany",
                                             "esp" = "Spain",
                                             "fra" = "France",
                                             "gbr" = "United Kingdom",
                                             "irl" = "Ireland",
                                             "sco" = "Scotland",
                                             "usa_house" = "United States (House)",
                                             "usa_senate" = "United States (Senate)")

# plot
pdf(file="../figures/female-ratio-centrality-top25.pdf", height=5, width=8, family="URWTimes")
par(oma=c(0,0,.5,0))
par(mar=c(0,3,0,.5))
par(yaxs="i", xaxs="i", bty="n")
colors <- c("Female ratio" = "black", "Female ratio among\nPageRank top 25%" = "darkgrey")
ggplot(data = filter(sessions_df_long, session_start >= as.Date("1946-01-01")), aes(x = session_start)) + 
  geom_line(aes(y = female_ratio, color = "Female ratio")) + 
  geom_line(aes(y = female_ratio_top25perc, color = "Female ratio among\nPageRank top 25%")) +
  facet_wrap( ~ legislature_label) + 
  theme_bw() + 
  labs(x = "",
      y = "",
      color = "") +
  scale_color_manual(values = colors) + 
  ylim(0, .5) + 
  theme(legend.position = c(1, 0.0),
        legend.justification = c(1, 0))
dev.off()


# plot
pdf(file="../figures/female-ratio-centrality-top10.pdf", height=5, width=8, family="URWTimes")
par(oma=c(0,0,.5,0))
par(mar=c(0,3,0,.5))
par(yaxs="i", xaxs="i", bty="n")
colors <- c("Female ratio" = "black", "Female ratio among\nPageRank top 10%" = "darkgrey")
ggplot(data = filter(sessions_df_long, session_start >= as.Date("1946-01-01")), aes(x = session_start)) + 
  geom_line(aes(y = female_ratio, color = "Female ratio")) + 
  geom_line(aes(y = female_ratio_top10perc, color = "Female ratio among\nPageRank top 10%")) +
  facet_wrap( ~ legislature_label) + 
  theme_bw() + 
  labs(x = "",
       y = "",
       color = "") +
  scale_color_manual(values = colors) + 
  ylim(0, .5) + 
  theme(legend.position = c(1, 0.0),
        legend.justification = c(1, 0))
dev.off()






