print("Running predict_districts.R....")

# Setup
library("compactness")
set.seed(02138)

# if you'd like, use this line in RStudio to set your working directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stateleg = "../data/sl_06_16.shp"
cong = "../data/cong_all_new.shp"

## Read the shapefiles
shp_sld = read_shapefiles(shp = stateleg, namecol = "NAME")
shp_cng = read_shapefiles(shp = cong, namecol = "NAME")

## Generate features (this will take a while!)
## Please ignore the following error:
## Error in rgeos::gUnaryUnion(x): TopologyException: Input geom 0 is invalid: Self-intersection at or near point -80.799257019999999 24.67188209 at -80.799257019999999 24.67188209\n"

feat_sld = generate_features(shp=shp_sld)
save(feat_sld, file="../results/feat_sld.RData")
feat_cng = generate_features(shp=shp_cng)
save(feat_cng, file="../results/feat_cng.RData")

## Make colnames the same, then combine
#load("../results/feat_sld.RData")
#load("../results/feat_cng.RData")
colnames(feat_sld) = tolower(colnames(feat_sld))
colnames(feat_cng) = tolower(colnames(feat_cng))
feat_sld = feat_sld[,colnames(feat_sld) %in% colnames(feat_cng)]
feat_cng = feat_cng[,colnames(feat_cng) %in% colnames(feat_sld)]
feat_cng = feat_cng[,colnames(feat_sld)]

feats_all = rbind(feat_sld, feat_cng)

## Generate predictions
load("../results/final_models.RData")
finalpreds = generate_predictions(feats_all, "name", new.models = models)
#finalpreds = merge(feats_all, finalpreds, by.x="name", by.y="district")
finalpreds = cbind(finalpreds, feats_all)
finalpreds = finalpreds[,-c(3,31)]
finalpreds = finalpreds[!duplicated(finalpreds),]

## Save the RData
finalpreds = finalpreds[finalpreds$compactness < 100 & finalpreds$compactness >= 0,]
save(finalpreds, file="../results/preds.RData")
