rm(list=ls())
require(dplyr)
require(tibble)
require(readr)
require(tidyr)
library(xtable)

# This script outputs the correlation of the normal vote at the county level with different measures of exposure, as reported in Table S18

nm = read_csv('county-normal-vote-pres-2008-2016.csv')

p = read_csv('partisan-seg-county-summaries.csv')




d = nm %>%
  inner_join(p, by = c('res_state', 'res_countyfips'))


t= c(cov.wt(d%>%select(d.post, dem.normal), wt = d$n, cor = TRUE)$cor[1,2],
     cov.wt(d%>%select(r.post, rep.normal), wt = d$n, cor = TRUE)$cor[1,2],
  
  cov.wt(d%>%select(w.mean.d.post, dem.normal), wt = d$n, cor = TRUE)$cor[1,2],
cov.wt(d%>%select(w.mean.r.post, rep.normal), wt = d$n, cor = TRUE)$cor[1,2],

cov.wt(d%>%select(mean.d.post, dem.normal), wt = d$n, cor = TRUE)$cor[1,2],
cov.wt(d%>%select(mean.r.post, rep.normal), wt = d$n, cor = TRUE)$cor[1,2]

)
party = c('Democratic', 'Republican', 'Democratic', 'Republican', 'Democratic', 'Republican')
stat = c('Imputation', 'Imputation', 'Weighted Exposure', 'Weighted Exposure','Unweighted Exposure', 'Unweighted Exposure')

p = data.frame(Party=party,
               Correlate = stat,
               `Correlation` = t)

out.table = xtable(p,digits=3)

print(out.table,'TabS18.tex', type='latex')
