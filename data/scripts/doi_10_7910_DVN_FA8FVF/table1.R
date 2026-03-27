load("../results/preds.RData")

library(xtable)
set.seed(02138)

preds_df = finalpreds
preds_df$district = as.character(preds_df$district)
preds_df = preds_df[grepl("^1_L_..._2006", preds_df$district),]

#preds_df = preds_df[grepl("_001_|_037_|_002_|_023_", preds_df$district),]

reock = rank(preds_df$reock)
hull = rank(preds_df$hull)
polsby = rank(preds_df$polsby)
sym = rank(-1*preds_df$sym_x) # changing this to monotonically ranked because it's the nonoverlapping area left over when you take symmetrically projected shape and divide by the area of the original. You want 0 nonoverlap
boyce = rank(-1*preds_df$boyce)
corners = rank(-1*preds_df$corners)
lw = rank(abs(1 - preds_df$lenwid)*-1)

#### IMPORTANT NOTE:
## This nested loop will take a very, very long time.
## We recommend running it for a few minutes then stopping it.
star = Sys.time()
distsets = c(0,0,0,0)
for(i in 1:1){
  for(j in setdiff(1:nrow(preds_df), i)){
    
    if(reock[i] > reock[j] & hull[i] < hull[j]){
      
      for(k in setdiff(1:nrow(preds_df), c(i,j))){
        
        if(reock[j] > reock[k] & hull[j] < hull[k]){
          
          for(l in setdiff(1:nrow(preds_df), c(i,j,k))){
            
            if(reock[k] > reock[l] & hull[k] < hull[l]){
              
              r_polsby = rank(polsby[c(i,j,k,l)])
              r_lw = rank(lw[c(i,j,k,l)])
              r_boyce = rank(boyce[c(i,j,k,l)])
              r_sym= rank(sym[c(i,j,k,l)])
              r_cor = rank(corners[c(i,j,k,l)])
              r_reock = rank(reock[c(i,j,k,l)])
              r_hull = rank(hull[c(i,j,k,l)])
              
              
              if(!any(duplicated(rbind(r_polsby, r_lw, r_boyce, r_sym, r_cor,r_reock, r_hull)))){
                distsets = rbind(distsets, c(i,j,k,l))
                print(paste0("Found number ", nrow(distsets)-1 ))
                
              }
              
            }
            
          }
          
        }
        
      }
      
    }
    
  }
  print(i)
}
sto = Sys.time()

z=2
dat = rbind(rank(hull[distsets[z,]]),
            rank(reock[distsets[z,]]),
            rank(polsby[distsets[z,]]),
            rank(sym[distsets[z,]]),
            rank(boyce[distsets[z,]]),
            rank(corners[distsets[z,]]),
            rank(lw[distsets[z,]]))
any(duplicated(dat))

colnames(dat) = preds_df$district[distsets[z,]]
rownames(dat) = c("Convex Hull", "Reock", "Polsby-Popper", "X-axis Symmetry","Boyce-Clark", "Significant Corners", "Length/Width")
writeLines(print(xtable(dat, digits=0)), "../results/table1.tex")

preds_df$district[distsets[z,]]

# Z = 503 and 5, for instance
# "01_L_1_2000"   "01_L_102_2000" "08_L_17_2000"  "9_C_0_1903"   
# "01_L_1_2000"   "01_L_102_2000" "01_L_87_2000"  "21_C_9_1935"  
