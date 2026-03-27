# ======================================
# = clear working memory and load data =
# ======================================

# clear working memory

	rm(list=ls())
	
# load evaluations of forecasts

	load("Accuracy.RData")
	
# ===========
# = table 1 =
# ===========	

# all elections

	cbind(cpw.sea.ove.ful, cpw.sea.ele.ful)[order(cpw.sea.ove.ful, decreasing=TRUE),]
	cbind(mae.sea.ove.ful, mae.sea.ele.ful)[order(mae.sea.ove.ful),]
	n.ove.ful

# elections with constant constituency boundaries

	cbind(cpw.sea.ove.uns, cpw.sea.ele.uns)[order(cpw.sea.ove.uns, decreasing=TRUE),]
	cbind(mae.sea.ove.uns, mae.sea.ele.uns)[order(mae.sea.ove.uns),]
	n.ove.uns

# ===========
# = table 2 =
# ===========

# all elections

	cbind(cpw.sea.ove.ful, cpw.sea.qua.ful, cpw.sea.yea.ful[,-1])[order(cpw.sea.ove.ful, decreasing=TRUE),]
	cbind(mae.sea.ove.ful, mae.sea.qua.ful, mae.sea.yea.ful[,-1])[order(mae.sea.ove.ful),]
	n.qua.ful
	n.yea.ful[-2]

# elections with constant constituency boundaries

	cbind(cpw.sea.ove.uns, cpw.sea.qua.uns, cpw.sea.yea.uns[,-1])[order(cpw.sea.ove.uns, decreasing=TRUE),]
	cbind(mae.sea.ove.uns, mae.sea.qua.uns, mae.sea.yea.uns[,-1])[order(mae.sea.ove.uns),]
	n.qua.uns
	n.yea.uns

# ===================
# = end source code =
# ===================