## Function to estimate number of Democratic seats using v0 and uniform partisan swing. 
## It assumes the dataframe, dta, has the following columns:
## v0:  Lag of Democratic Vote Share (two-party) in the district
## v1:  Democratic Vote Share (two-party) in the district
##
## Outputs as a tibble:
## l:   Number of legislative districts in the election
## s1:  True number of seats won in the election
## s1h: Estimated number of seats won in election 1 given v0 and swing

estimate <- function(dta,str){
  l   <- length(dta$v1)               # Number of districts
  d   <- mean(dta$v1) - mean(dta$v0)  # Uniform swing estimate
  m   <- mean(dta$v0) 
   if(str=="ups") {
    di <- d    #  Estimate of district level swing
  }
  else if(str=="prop") {
    di <- dta$v0*d/m
  }
  else if(str=="pw") {
    if (d>0) {di <- d*(1-dta$v0)/(1-m) } 
    else {di <- d*dta$v0/m}
  }

  v1h <- dta$v0 + di   
  s1  <- mean(dta$v1 > 0.50)          # Actual number of seats won in election 1
  s1h <- mean(v1h > 0.50)             # Estimate of number of seats won in election 1 given v0 and swing
  
  dta <- tibble(l = l, d = d, v1h = v1h, v0 = dta$v0, v1 = dta$v1, s1=s1, s1h=s1h)
  return(dta)
}

compute_results <-function(dta, str){
    s <- as.name(str)
    dta %>% 
    group_by(stabb, year, sen) %>%
    nest()  %>%
    mutate(s = map(data, estimate, str = str)) %>%
    select(stabb, year, sen, s) %>%
    unnest(cols = c(s)) %>%
    mutate(error = v1 - v1h, error2 = s1-s1h, error3 = ((sign(v1h - v0) == sign(v1-v0)) || (v1==v0)), error4 = (v1 > 0.5 & v1h >0.5) || (v1<0.5 & v1h<0.5), error5 = (v1h<0 || v1h>1), D = abs(d), dd=sd(v1-v0))

}
print_results <-function(dta){

  print(mean(dta$error4))
  print(mean(dta$error3))
  print(1- mean(dta$error5))
  print(mean(dta$error^2)) 
  print(cor(dta$v1, dta$v1h, method = "pearson"))

}
