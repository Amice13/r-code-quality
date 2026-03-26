rm(list=ls())
gc()


# Run scripts in correct order
files = list.files('log')
files = files[grepl('\\.R', files)]

files = files[as.numeric(substr(files,1,2))%in%1:99]

for(f in files){
    print(f)
    source(paste0('log/',f))
}


