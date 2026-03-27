# developper: Norihito Kubota 
# developper's affiliation: National Defense Academy, Japan
# developper's email address: norihito@nda.ac.jp

library(tidyverse)
library(readr)
library(rvest)
library(stringr)

dir.create("1968")
dir_name0<-"1968/"
download.file(url="https://www.sipri.org/sites/default/files/SIPRI%20Yearbook%201968%E2%80%9369.pdf",destfile =  str_c(dir_name0, "1968.pdf"))
dir.create("1969")
dir_name0<-"1969/"
download.file(url="https://www.sipri.org/sites/default/files/SIPRI%20Yearbook%201969%E2%80%9370.pdf",destfile =  str_c(dir_name0, "1969.pdf"))
p<-NA
i<-NA
dir_name0<-NA
for (p in 1:46){
  i<-1971+p
  dir.create(as.character(i))
  dir_name0<-paste0(as.character(i),"/")
  html_metameta<-paste0("https://www.sipri.org/yearbook/",i,"/")
  html_data_meta <- read_html(html_metameta)
  bodyArea <- html_data_meta %>% html_nodes(xpath = '//*[@class="content"]')
      if(p<1){
  links <- bodyArea %>%  html_nodes("a") %>% html_attr("href") %>% str_subset("files") %>% unique()
  fn0<-strsplit(links , "/")
    if(is.na(fn0)){
      
    }
  else{
    length(fn0[[1]])
    filenames<-NA
    filenames<-fn0[[1]][[length(fn0[[1]])]]
    filenames
    if(is.na(fn0)){
      
    }
    else{
      download.file(url = links, 
                    destfile =  str_c(dir_name0, filenames), 
                    method = "libcurl") 
      Sys.sleep(1) #1秒あける
    }
    }}
else{
links_meta_relative <- bodyArea %>%  html_nodes("a") %>% html_attr("href") %>% str_subset("yearbook") %>% unique()
links_meta <- paste0("https://www.sipri.org",links_meta_relative)
j<-NA
for (j in 1:length(links_meta)){
html_data<-NA
html_data <- read_html(links_meta[j])
links<-NA
links <- html_data %>%  html_nodes("a") %>% html_attr("href") %>% str_subset(".pdf") %>% unique()
if(length(links)==0){}else{
for (k in 1:length(links)){
  fn0<-strsplit(links[k] , "/")
  if(is.na(fn0)){}else{
filenames<-NA
filenames<-fn0[[1]][[length(fn0[[1]])]]
if(is.na(fn0)){}else{
download.file(url = links[k], 
              destfile =  str_c(dir_name0, filenames), 
              method = "libcurl")
Sys.sleep(1) #1秒あける
}}}
}
}
}
}
