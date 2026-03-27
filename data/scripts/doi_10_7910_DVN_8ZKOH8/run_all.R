rm(list=ls())
gc()

# Create subdirectories to store output

if(!dir.exists('figures')){
  dir.create(('figures'))
}

if(!dir.exists('tables')){
  dir.create(('tables'))
}

if(!dir.exists('output')){
  dir.create(('output'))
}

if(!dir.exists('results')){
  dir.create(('results'))
}

# Run scripts in correct order
files = list.files('code')

files = files[as.numeric(substr(files,1,2))%in%1:99]

for(f in files){
    print(f)
    source(paste0('code/',f))
}


