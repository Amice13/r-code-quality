###################################
## Primary Study Data Generation 
###################################
dgp <-function(al1, sigh,obs){
  x1=runif(obs, min = 100, max = 200);
  x2=x1+rnorm(obs, mean = 0, sd = 50);
  al2=rnorm(1,mean=0,sd=sigh);
  z = 100 + al1*x1 + al2*x2 + rnorm(obs,mean=0,sd=100);
  return (as.data.frame(cbind(z, x1 ,matrix(al1+al2, nrow=obs, ncol=1))))
}
###################################
## PMeta Analysis Data Generation
###################################
MetaStudy <- function(al1, sigh, ssize, Bias, obsList){
  output =  matrix(0, nrow=ssize, ncol=6);
  colnames(output) <- c("id","y","al_se","Significant","popal","obs");
  
  num_publ=ssize*(Bias/100);
  for(i in 1:ssize) {
    obs<-obsList[i];
    output[i,1]=i;
    output[i,6]=obs;
    if (i<=num_publ){
      while (output[i,4]==0){
        data=dgp(al1,sigh,obs);
        output[i,5]=mean(data[,3]);
        out <- lm(data[,1] ~ data[,2])
        output[i,2]=coefficients(out)[2];		
        output[i,3]=sqrt(diag(vcov(out)))[2];	
        output[i,4]=((summary(out)$coefficients[2,4]<=0.05)*(0<=summary(out)$coefficients[2,1]));
      }	
    } else if (i>num_publ){
      data=dgp(al1,sigh,obs);
      output[i,5]=mean(data[,3]);
      out <- lm(data[,1] ~ data[,2])
      output[i,2]=coefficients(out)[2];		
      output[i,3]=sqrt(diag(vcov(out)))[2];	
      output[i,4]=((summary(out)$coefficients[2,4]<=0.05)*(0<=summary(out)$coefficients[2,1]));
    } else { cat("Publication Bias Error", "\n"); }
  }
  return(output)
}
###################################