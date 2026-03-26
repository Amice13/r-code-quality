
##### For TE analysis
#define functions
#function1
Chr_Average <- function(x, y){
  chr10.skew <- rowMeans(x[ ,c(1:y)], na.rm=T)
  chr11.skew <- rowMeans(x[ ,c((y+1):(y*2))], na.rm=T)
  chr12.skew <- rowMeans(x[ ,c((y*2+1):(y*3))], na.rm=T)
  chr13.skew <- rowMeans(x[ ,c((y*3+1):(y*4))], na.rm=T)
  chr14.skew <- rowMeans(x[ ,c((y*4+1):(y*5))], na.rm=T)
  chr15.skew <- rowMeans(x[ ,c((y*5+1):(y*6))], na.rm=T)
  chr16.skew <- rowMeans(x[ ,c((y*6+1):(y*7))], na.rm=T)
  chr17.skew <- rowMeans(x[ ,c((y*7+1):(y*8))], na.rm=T)
  chr18.skew <- rowMeans(x[ ,c((y*8+1):(y*9))], na.rm=T)
  chr19.skew <- rowMeans(x[ ,c((y*9+1):(y*10))], na.rm=T)
  chr1.skew <- rowMeans(x[ ,c((y*10+1):(y*11))], na.rm=T)
  chr2.skew <- rowMeans(x[ ,c((y*11+1):(y*12))], na.rm=T)
  chr3.skew <- rowMeans(x[ ,c((y*12+1):(y*13))], na.rm=T)
  chr4.skew <- rowMeans(x[ ,c((y*13+1):(y*14))], na.rm=T)
  chr5.skew <- rowMeans(x[ ,c((y*14+1):(y*15))], na.rm=T)
  chr6.skew <- rowMeans(x[ ,c((y*15+1):(y*16))], na.rm=T)
  chr7.skew <- rowMeans(x[ ,c((y*16+1):(y*17))], na.rm=T)
  chr8.skew <- rowMeans(x[ ,c((y*17+1):(y*18))], na.rm=T)
  chr9.skew <- rowMeans(x[ ,c((y*18+1):(y*19))], na.rm=T)
  chrX.skew <- rowMeans(x[ ,c((y*19+1):(y*20))], na.rm=T)
  
  Combined <- cbind(chr1.skew,chr2.skew,chr3.skew,chr4.skew,chr5.skew,chr6.skew,chr7.skew,chr8.skew,chr9.skew,chr10.skew,chr11.skew,chr12.skew,chr13.skew,chr14.skew,chr15.skew,chr16.skew,chr17.skew,chr18.skew,chr19.skew,chrX.skew)
  
  return(Combined)
}

#function2
Auto.X_Average <- function(x, y){
  chr10.skew <- rowMeans(x[ ,c(1:y)], na.rm=T)
  chr11.skew <- rowMeans(x[ ,c((y+1):(y*2))], na.rm=T)
  chr12.skew <- rowMeans(x[ ,c((y*2+1):(y*3))], na.rm=T)
  chr13.skew <- rowMeans(x[ ,c((y*3+1):(y*4))], na.rm=T)
  chr14.skew <- rowMeans(x[ ,c((y*4+1):(y*5))], na.rm=T)
  chr15.skew <- rowMeans(x[ ,c((y*5+1):(y*6))], na.rm=T)
  chr16.skew <- rowMeans(x[ ,c((y*6+1):(y*7))], na.rm=T)
  chr17.skew <- rowMeans(x[ ,c((y*7+1):(y*8))], na.rm=T)
  chr18.skew <- rowMeans(x[ ,c((y*8+1):(y*9))], na.rm=T)
  chr19.skew <- rowMeans(x[ ,c((y*9+1):(y*10))], na.rm=T)
  chr1.skew <- rowMeans(x[ ,c((y*10+1):(y*11))], na.rm=T)
  chr2.skew <- rowMeans(x[ ,c((y*11+1):(y*12))], na.rm=T)
  chr3.skew <- rowMeans(x[ ,c((y*12+1):(y*13))], na.rm=T)
  chr4.skew <- rowMeans(x[ ,c((y*13+1):(y*14))], na.rm=T)
  chr5.skew <- rowMeans(x[ ,c((y*14+1):(y*15))], na.rm=T)
  chr6.skew <- rowMeans(x[ ,c((y*15+1):(y*16))], na.rm=T)
  chr7.skew <- rowMeans(x[ ,c((y*16+1):(y*17))], na.rm=T)
  chr8.skew <- rowMeans(x[ ,c((y*17+1):(y*18))], na.rm=T)
  chr9.skew <- rowMeans(x[ ,c((y*18+1):(y*19))], na.rm=T)
  chrX.skew <- rowMeans(x[ ,c((y*19+1):(y*20))], na.rm=T)
  
  auto.combined <- data.frame(chr="auto", skewing=c(chr1.skew,chr2.skew,chr3.skew,chr4.skew,chr5.skew,chr6.skew,chr7.skew,chr8.skew,chr9.skew,chr10.skew,chr11.skew,chr12.skew,chr13.skew,chr14.skew,chr15.skew,chr16.skew,chr17.skew,chr18.skew,chr19.skew))
  chrX.combined <- data.frame(chr="chrX", skewing=chrX.skew)
  auto.chrX.combined <- rbind(auto.combined, chrX.combined)
  
  return(auto.chrX.combined)
  
}

#function3
X.chr_Average <- function(x, y){
  chr10.skew <- rowMeans(x[ ,c(1:y)], na.rm=T)
  chr11.skew <- rowMeans(x[ ,c((y+1):(y*2))], na.rm=T)
  chr12.skew <- rowMeans(x[ ,c((y*2+1):(y*3))], na.rm=T)
  chr13.skew <- rowMeans(x[ ,c((y*3+1):(y*4))], na.rm=T)
  chr14.skew <- rowMeans(x[ ,c((y*4+1):(y*5))], na.rm=T)
  chr15.skew <- rowMeans(x[ ,c((y*5+1):(y*6))], na.rm=T)
  chr16.skew <- rowMeans(x[ ,c((y*6+1):(y*7))], na.rm=T)
  chr17.skew <- rowMeans(x[ ,c((y*7+1):(y*8))], na.rm=T)
  chr18.skew <- rowMeans(x[ ,c((y*8+1):(y*9))], na.rm=T)
  chr19.skew <- rowMeans(x[ ,c((y*9+1):(y*10))], na.rm=T)
  chr1.skew <- rowMeans(x[ ,c((y*10+1):(y*11))], na.rm=T)
  chr2.skew <- rowMeans(x[ ,c((y*11+1):(y*12))], na.rm=T)
  chr3.skew <- rowMeans(x[ ,c((y*12+1):(y*13))], na.rm=T)
  chr4.skew <- rowMeans(x[ ,c((y*13+1):(y*14))], na.rm=T)
  chr5.skew <- rowMeans(x[ ,c((y*14+1):(y*15))], na.rm=T)
  chr6.skew <- rowMeans(x[ ,c((y*15+1):(y*16))], na.rm=T)
  chr7.skew <- rowMeans(x[ ,c((y*16+1):(y*17))], na.rm=T)
  chr8.skew <- rowMeans(x[ ,c((y*17+1):(y*18))], na.rm=T)
  chr9.skew <- rowMeans(x[ ,c((y*18+1):(y*19))], na.rm=T)
  chrX.skew <- rowMeans(x[ ,c((y*19+1):(y*20))], na.rm=T)
  
  auto.skew <- c(chr1.skew,chr2.skew,chr3.skew,chr4.skew,chr5.skew,chr6.skew,chr7.skew,chr8.skew,chr9.skew,chr10.skew,chr11.skew,chr12.skew,chr13.skew,chr14.skew,chr15.skew,chr16.skew,chr17.skew,chr18.skew,chr19.skew)
  auto.combined <- data.frame(chr="auto", skewing=auto.skew[!is.na(auto.skew)])
  chrX <- data.frame(chr="chrX", skewing=chrX.skew[!is.na(chrX.skew)])
  chr1 <- data.frame(chr="chr1", skewing=chr1.skew[!is.na(chr1.skew)])
  chr13 <- data.frame(chr="chr13", skewing=chr13.skew[!is.na(chr13.skew)])
  
  auto.chrX.combined <- rbind(chrX, chr1, chr13)
  
  return(auto.chrX.combined)
  
}
