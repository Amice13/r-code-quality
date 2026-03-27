## Package checker


if(!require(ranger,quietly = T)){
	cat('Installing ranger...', "\n")
	install.packages("ranger",repos = "https://cloud.r-project.org")
	require(ranger)
	cat('ranger installed...', "\n")
}


if(!require(tidyverse,quietly = T)){
	cat('tidyverse tidyverse', "\n")
	install.packages("tidyverse",repos = "https://cloud.r-project.org")
	require(tidyverse)
	cat('tidyverse installed...', "\n")
}

if(!require(magrittr,quietly = T)){
	cat('magrittr magrittr', "\n")
	install.packages("magrittr",repos = "https://cloud.r-project.org")
	require(magrittr)
	cat('magrittr installed...', "\n")
}

if(!require(arrow,quietly = T)){
	cat('Installing arrow', "\n")
	install.packages("arrow",repos = "https://cloud.r-project.org")
	require(arrow)
	cat('arrow installed...', "\n")
}

if(!require(foreach,quietly = T)){
	cat('Installing foreach', "\n")
	install.packages("foreach",repos = "https://cloud.r-project.org")
	require(foreach)
	cat('foreach installed...', "\n")
}

if(!require(doParallel,quietly = T)){
	cat('Installing doParallel', "\n")
	install.packages("doParallel",repos = "https://cloud.r-project.org")
	require(doParallel)
	cat('doParallel installed...', "\n")
}

if(!require(doRNG,quietly = T)){
	cat('Installing doRNG', "\n")
	install.packages("doRNG",repos = "https://cloud.r-project.org")
	require(doRNG)
	cat('doRNG installed...', "\n")
}

if(!require(pintervals,quietly = T)){
	cat('Installing pintervals', "\n")
	install.packages("pintervals",repos = "https://cloud.r-project.org")
	require(pintervals)
	cat('pintervals installed...', "\n")
}

cat("\n",'All required packages installed',"\n")


