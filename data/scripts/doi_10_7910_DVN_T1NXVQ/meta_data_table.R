#############################
# HEG META DATA TABLE
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#############################

################################################################################
## Load data
################################################################################

## Subset to used maps
meta.df <-  read.csv("data/geography/HEG/heg_meta.csv")

## Check for encoding errors
findEncErrors <- function(x){
  x[which(is.na(iconv(x)) | validUTF8(x)==F)]
}

titles <- meta.df$title
findEncErrors(titles)

## Function to fix broken UTF characters
fixUtf8 <- function(x){
  x <- gsub("\x85", "Ö", x)
  x <- gsub("\x9a|<9a>", "ö", x)
  x <- gsub("\x8a", "ä", x)
  x <- gsub("\xfc", "ü", x)
  x <- gsub("\xca", "", x)
  x <- gsub("\xdc", "Ü", x)
  x <- gsub("\xe9", "é", x)
  x <- gsub("\xf6", "ö", x)
  x <- gsub("\xa0e", " ", x)
  x <- gsub("\x9fc|<fc>", "ü", x)
  x <- gsub("\xe4|<e4>", "ä", x)
  x <- gsub("\x8e", "é", x)
  x <- gsub("\xfc\xbe\x99\xa3\xa4\xbc", "ü", x)
  x <- gsub("\xfc\xbe\x8d\xa6\x90\xbc", "Ö", x)
  x <- gsub("\xfc\xbe\x8d\xa3\xa0\xbc", "Ü", x)
  x <- gsub("<9a>", "ö", x)
  x <- gsub("<85>|<d6>", "Ö", x)
  x <- gsub("<86>", "Ü", x)
  x <- gsub("<8a>", "ä", x) 
  x <- gsub("Vˆlk", "Völk", x) 
  return(x)
}

## Prepare output table: Fix encoding errors
meta.df.out <- meta.df %>% 
  ## Subset to files used in ethnic maps database 
  dplyr::select(file_name, title, author, year_data_end, scale, link_map_image) %>%
  setNames(c("file_name", "title", "author", "map_year", "scale", "source")) %>%
  arrange(map_year) %>%
  mutate_all(fixUtf8)

## Add author nationality
t <- read.csv("data/geography/HEG/heg_author_nationalities.csv")

meta.df.out <- meta.df.out %>%
  mutate(author = str_split(author, "\\|")) %>%
  unnest(cols = author) %>%
  mutate(author = trimws(author)) %>%
  left_join(t, by = c("author" = "author_name"))
meta.df.out <- aggregate.data.frame(meta.df.out[, c("author","nationality")],
                                    meta.df.out[, c("file_name", "title", "map_year")],
                                    FUN = function(x){paste(unique(x[trimws(x) != ""]), collapse = " | ")})

################################################################################
## Create metadata table in latex format
################################################################################
meta.table.print <- meta.df.out[, c("title" , "map_year", "author", "nationality")] %>% 
  ## Set column widths for long-text columns
  mutate(title = paste0("\\parbox{7cm}{", title, "}"),
         author = paste0("\\parbox{3cm}{", author, "}")) %>%
  ## Fix problematic latex characters
  rename_all(~gsub("_", "\\_", ., fixed = T)) %>%
  mutate_all(~gsub("_", "\\_", ., fixed = T)) %>%
  mutate_all(~gsub("&", "\\&", ., fixed = T)) %>%
  mutate_all(~gsub(" |", ",", ., fixed = T)) %>%
  mutate(title = gsub("\\.$", "", title))
colnames(meta.table.print) <- c("Title", "Year", "Author", "Nationality")


print(xtable(meta.table.print, 
             caption = paste("List of",nrow(meta.table.print), 
                             "ethnographic maps used as source material"), 
             caption.placement = "top", label = "map_meta_table"),
      sanitize.text = force, 
      sanitize.colnames.function=function(x){paste0("\\multicolumn{1}{c}{",x,"}")},
      booktabs=TRUE,
      tabular.environment = "longtable", 
      include.rownames=F, 
      digits=0,
      add.to.row=list(
        pos=list(as.list(c(0,seq(from=1,to=nrow(meta.table.print)-1,by=2))))[[1]],
        command=c("\\hline \\endhead ",
                  rep("\\rowcolor[gray]{0.8}",length(seq(from=1,to=nrow(meta.table.print)-1,by=2))))),
      size="\\fontsize{8pt}{8pt}\\selectfont",
      file = file.path(tab.path, "map_meta_table.tex"))

