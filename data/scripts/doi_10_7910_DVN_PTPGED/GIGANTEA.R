GIGANTEA_R<-function(input,freevec,para){
  
  #Model for GIGANTEA (Nagano et al. 2012)
  #Time in input starts from 01:01 of the first day and ends at 01:00 of the fourth day.
  
  #parameter constrains:
  #0 < pl < 1440
  #0 < os < 1440
  #0 < ol < 1440
  
  Ns <- freevec[1]
  Nt <- freevec[2]
  
  a <- para[1]
  b <- para[2]/1000
  pl <- round(para[3])
  th <- para[4]
  os <- round(para[5]) - 60
  ol <- round(para[6])
  
  os.seq <- seq(os, Nt, 1440)
  open <- NULL
  for(i in 1:length(os.seq)){
    open <- c(open, os.seq[i]:(os.seq[i] + ol))
  }
  open<-open[-which(open>Nt)]
  
  output<-numeric(Ns)
  for(i in 1:Ns){
    t<-input[i]
    trange<-(t-pl):t
    trange<-intersect(trange,open)
    f<-input[Ns+trange]-th
    f<-f[f>0]
    E<-sum(f)
    output[i]<-E
  }
  output<-a+output*b
  
  output
}