# CFA_scoretest.R

if(!exists("old_items")){old_items <- c()}
ts <- lavaan::lavTestScore(model.estim)$uni
ts <- ts[order(ts$X2, decreasing = TRUE),]
labels <- lavsummary$pe$label
rhss <- lavsummary$pe$rhs
rhss <- rhss[which(labels!="")]
labels <- labels[which(labels!="")]
labels <- labels[which(rhss!="")]
rhss <- rhss[which(rhss!="")]
p_item <- ""
p <- 0
ok <- FALSE
if(length(old_items)<length(unique(rhss)))
{
  while(!ok)
  {
    p <- p+1
    position <- which(labels==ts$lhs[p])
    if(length(position)>0)
    {
      p_item <- unique(rhss[position])
      tsok <- ts[which(ts$lhs==unique(labels[position])),]
      ok <- TRUE
      for (i in old_items)
      {
        if(p_item==i) # already used
        {
          ok <- FALSE
        }
      } 
    } else
    {
      ok <- FALSE
    }
  }
  old_items <- c(old_items,p_item)
  cat("\nItem with highest discrepancy is ",p_item,sep="")
  cat("\nX^2(",tsok$df,") = ",tsok$X2," p = ",tsok$p.value,"\n",sep="")
} else
{
  cat("\nNo more items available\n")
}
