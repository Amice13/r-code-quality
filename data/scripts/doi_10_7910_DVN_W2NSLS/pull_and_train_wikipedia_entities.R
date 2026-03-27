stop("The wikipedia snapshots are not included in the replication materials because they are very large. We are happy to share this old snapshot.")

dates <- "2019-12-01"

languages <- c("zh","fa","it","de","ru")

for (language in languages) {
    for (date in dates) {
        system(paste0("wikipedia2vec train --iteration 10 --negative 15 ",language,"wiki-",gsub("-", "", as.character(date)), "-pages-articles.xml.bz2 ",language,"wiki-",gsub("-", "", as.character(date)), "-pages-articles_embeddings_100d.pkl.bz2"))
        system(paste0("bzip2 -d ",language,"wiki-",gsub("-", "", as.character(date)), "-pages-articles_embeddings_100d.pkl.bz2"))
        system(paste0("wikipedia2vec save-text --out-format word2vec ",language,"wiki-",gsub("-", "", as.character(date)), "-pages-articles_embeddings_100d.pkl ",language,"wiki-",gsub("-", "", as.character(date)), "-pages-articles_embeddings_100d.txt"))
        system(paste0("gzip -f ",language,"wiki-",gsub("-", "", as.character(date)), "-pages-articles_embeddings_100d.txt"))
    }
}
