## load regression functions and set output folder
source("regression_functions.R")
tab.path <- "results/"
fig.path <- "results/"


dhs.ab2wlms.ls <- readRDS("wlms/exogamy_identity_by_wlms.rds")
names(dhs.ab2wlms.ls)

wlms.ls <- c(list(wlms_data),dhs.ab2wlms.ls)

wlms_data <- as.data.frame(c(list(wlms_data),dhs.ab2wlms.ls) %>% reduce(left_join))

# make correlation matrix between group-level political relevance and aggregate DHS inter-marriage and AB identity salience
M <- cor(wlms_data[,c("preg_link","epr_link","preg_link_narrow","epr_link_narrow","identity_more_ethnic_lang","exogamy_l15_ethn","exogamy_l15_ethn.m")],use="c")
rownames(M) <- colnames(M) <- c("PREG Link (Y/N)","EPR Link (Y/N)","Excl. PREG Link (Y/N)","Excl. EPR Link (Y/N)","AB Ethnic > Nat. ID (Y/N)", "DHS Exogamy L15 (f)", "DHS Exogamy L15 (m)")


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

pdf(width=10,height=10,file=paste0(fig.path,"wlms_figA4.pdf"))
corrplot(M, method = "color", col = col(10),  
              type = "upper", 
              #order = "hclust", 
              addCoef.col = "black", # Add coefficient of correlation
              tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
              # Combine with significance level
              #p.mat = p_mat, sig.level = 0.01,  
              # hide correlation coefficient on the principal diagonal
              diag = FALSE 
)
dev.off()

